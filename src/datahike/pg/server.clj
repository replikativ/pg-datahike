(ns datahike.pg.server
  "PostgreSQL wire protocol server for Datahike.

   Starts a pgwire-compatible server that accepts SQL queries from standard
   PostgreSQL clients (psql, DBeaver, JDBC, Python/psycopg2, etc.) and
   translates them to Datahike Datalog queries.

   Usage:
     (require '[datahike.api :as d])
     (require '[datahike.pg.server :as pg])

     (def conn (d/connect cfg))
     (def server (pg/start-server conn {:port 5432}))
     ;; ... use any PostgreSQL client ...
     (pg/stop-server server)"
  (:require [clojure.string :as str]
            [clojure.set]
            [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.db.interface :as dbi]
            [datahike.versioning :as versioning]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.errors :as errors]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.catalog :as catalog]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.sql.temporal :as sql-temporal]
            [datahike.pg.types :as types]
            [datahike.pg.window :as window]
            [datahike.pg.jsonb :as jb])
  (:import [datahike.pg PgWireServer PgWireServer$QueryResult PgWireServer$QueryHandler
            PgWireServer$QueryHandlerFactory]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.statement.select PlainSelect Limit Offset]
           [net.sf.jsqlparser.expression LongValue]))

(set! *warn-on-reflection* true)

;; ============================================================================
;; Cancel flag bridge
;; ============================================================================
;;
;; The pgwire layer exposes cancellation via a Java AtomicBoolean stored in
;; PgWireServer/CANCEL_FLAG (ThreadLocal, set at connection startup, flipped
;; by a parallel CancelRequest). The Datahike query engine's `check-cancel!`
;; macro expects an IDeref. This deftype is a cheap one-object bridge —
;; `.deref` compiles to a direct AtomicBoolean.get(), keeping the hot-path
;; call monomorphic per pgwire session.

(deftype AtomicBooleanDeref [^java.util.concurrent.atomic.AtomicBoolean ab]
  clojure.lang.IDeref
  (deref [_] (.get ab)))

(defn- current-cancel
  "Return an IDeref wrapping the current connection's cancel flag, or nil
   when called outside a pgwire connection. Safe to pass as `:cancel` in
   the query map — nil short-circuits the check-cancel! macro."
  []
  (when-let [flag (.get ^ThreadLocal PgWireServer/CANCEL_FLAG)]
    (->AtomicBooleanDeref flag)))

;; ============================================================================
;; Row-lock registry (FOR UPDATE / FOR NO KEY UPDATE / SHARE / KEY SHARE)
;; ============================================================================
;;
;; A server-wide map `{[table id] → session-id}` tracks which session holds a
;; lock on each row. Locks are advisory — we can't actually block other
;; clients from writing Datahike directly — but within the pgwire layer they
;; implement PG's SKIP LOCKED / NOWAIT semantics.
;;
;; Lock modes aren't distinguished yet: FOR UPDATE, FOR NO KEY UPDATE,
;; FOR SHARE and FOR KEY SHARE all acquire the same exclusive-style lock.
;; Upgrading to the full PG compatibility matrix is planned (A3).
;;
;; Locks are released on COMMIT / ROLLBACK via tx-state. Outside a
;; transaction, FOR UPDATE still acquires a "lock" for the lifetime of the
;; statement — PG actually holds it for the implicit transaction, which is
;; statement-length since autocommit.

(defonce ^:private lock-registry (atom {}))

(defn reset-lock-registry!
  "Clear the server-wide row-lock registry. Intended for test fixtures —
   in production, locks are released on COMMIT/ROLLBACK/DISCARD ALL and
   when the handler's TCP connection closes. Do not call this from
   handler code."
  []
  (reset! lock-registry {}))

(defn- release-session-locks!
  "Remove all entries in the global lock registry owned by `session-id`."
  [session-id]
  (swap! lock-registry
         (fn [reg]
           (into {} (remove (fn [[_ sid]] (= sid session-id)) reg)))))

;; ============================================================================
;; Advisory locks (pg_advisory_lock / pg_try_advisory_lock / …).
;;
;; PG semantics we implement:
;;   - Session-level: held until pg_advisory_unlock / pg_advisory_unlock_all /
;;     connection close. Re-entrant for the same session — a second
;;     pg_advisory_lock by the same owner increments a refcount and requires
;;     an equal number of unlocks to release.
;;   - Transaction-level (pg_advisory_xact_lock): auto-released at COMMIT or
;;     ROLLBACK. Not reference-counted in PG (a second call in the same tx
;;     is a no-op); we match that.
;;   - Try variants are non-blocking: return false instead of waiting.
;;   - Blocking variants spin-wait (exponential backoff up to 100ms). Our
;;     entire pgwire server is a single JVM, so conflicts resolve in-process.
;;
;; Crucial: migration tools (Flyway, Alembic, Ecto.Migrator, Rails) take a
;; well-known key at startup and only proceed if pg_try_advisory_lock
;; returns true. Returning anything other than a real boolean (or a dummy
;; "looks truthy" string — our prior behavior) silently breaks
;; mutual-exclusion across concurrent migrate runs.
(defonce ^:private advisory-locks
  ;; {lock-key {:session-id str :count long :xact? bool}}
  (atom {}))

(defn reset-advisory-locks!
  "Clear the advisory-lock registry. Test-fixture helper; not for handler
   code. In production, locks release on unlock, COMMIT/ROLLBACK (xact-
   level), DISCARD ALL, or connection close."
  []
  (reset! advisory-locks {}))

(defn- advisory-lock-try!
  "Acquire a single advisory lock, non-blocking. Returns true on success,
   false if another session holds it. Same-session re-lock increments the
   refcount (session-level only); xact-level re-lock is a no-op."
  [lock-key session-id xact?]
  (let [result (atom :unknown)]
    (swap! advisory-locks
           (fn [m]
             (let [existing (get m lock-key)]
               (cond
                 (nil? existing)
                 (do (reset! result :acquired)
                     (assoc m lock-key {:session-id session-id
                                        :count 1
                                        :xact? xact?}))
                 (not= (:session-id existing) session-id)
                 (do (reset! result :blocked) m)
                 xact?
                 (do (reset! result :acquired) m)
                 :else
                 (do (reset! result :acquired)
                     (update-in m [lock-key :count] inc))))))
    (= @result :acquired)))

(defn- advisory-lock!
  "Blocking acquire. Spin-wait with exponential backoff up to 100ms."
  [lock-key session-id xact?]
  (loop [attempt 0]
    (if (advisory-lock-try! lock-key session-id xact?)
      true
      (do (Thread/sleep ^long (min 100 (bit-shift-left 1 (min 7 attempt))))
          (recur (inc attempt))))))

(defn- advisory-unlock!
  "Decrement refcount on `lock-key` (session-level); remove when it
   reaches zero. Returns true if WE held it and released one, false
   otherwise. xact-level locks cannot be unlocked by hand in PG — they
   release at tx end — but we accept it as a no-op-false for symmetry."
  [lock-key session-id]
  (let [[before after]
        (swap-vals! advisory-locks
                    (fn [m]
                      (let [e (get m lock-key)]
                        (cond
                          (or (nil? e)
                              (not= (:session-id e) session-id)
                              (:xact? e))
                          m
                          (<= (:count e) 1)
                          (dissoc m lock-key)
                          :else
                          (update-in m [lock-key :count] dec)))))]
    (not= before after)))

(defn- release-advisory-locks!
  "Release advisory locks owned by `session-id`. If xact-only? is true,
   only release the tx-level ones (called from COMMIT/ROLLBACK); otherwise
   release all (called from session close and DISCARD ALL)."
  ([session-id] (release-advisory-locks! session-id false))
  ([session-id xact-only?]
   (swap! advisory-locks
          (fn [m]
            (into {}
                  (remove (fn [[_k v]]
                            (and (= (:session-id v) session-id)
                                 (or (not xact-only?) (:xact? v)))))
                  m)))))

(defn- parse-advisory-key
  "Extract the advisory-lock key from a parsed pg_advisory_lock* map.
   parse-sql merges the classifier's :args (a vector of longs) into
   the parsed map; single-arg form returns the long directly, two-arg
   form returns [hi lo] so the two-key namespace is distinct in the
   lock registry."
  [parsed]
  (let [args (:args parsed)]
    (case (count args)
      1 (first args)
      2 (vec args)
      nil)))

(defn- acquire-lock!
  "Record `[table id]` as locked by `session-id`. If already locked by a
   different session, returns `:conflict`; otherwise returns `:acquired`
   (already held by this session also counts as :acquired — lock re-entry
   within the same session is a no-op in PG too)."
  [session-id table id]
  (let [key [table id]
        [old new] (swap-vals! lock-registry
                              (fn [reg]
                                (if-let [holder (get reg key)]
                                  (if (= holder session-id) reg reg)
                                  (assoc reg key session-id))))]
    (let [holder (get new key)]
      (cond
        (= holder session-id) :acquired
        holder                :conflict
        :else                 :acquired))))

;; ============================================================================
;; Value → String conversion for pgwire result rows
;; ============================================================================

(defn- value->string
  "Convert a Datahike value to a PostgreSQL text-format string."
  [v]
  (cond
    (nil? v)           nil
    (= :__null__ v)    nil  ;; LEFT JOIN sentinel → SQL NULL
    ;; PgArray → PG canonical array text format `{…}` (see
    ;; datahike.pg.arrays/to-pg-text). Checked before vector? because
    ;; PgArray is a defrecord and vectors would otherwise intercept.
    (pg-arr/array? v) (pg-arr/to-pg-text v)
    (string? v)  v
    (keyword? v) (if-let [ns (namespace v)]
                   (str ns "/" (name v))
                   (name v))
    (boolean? v) (if v "t" "f")
    (instance? clojure.lang.Ratio v) (str (double v))
    ;; PG float text format emits the shortest round-trip representation,
    ;; so 1.0/-2.0/0.0 come across as "1"/"-2"/"0" (no ".0" suffix).
    ;; Java's Double/Float toString always appends ".0" for whole-valued
    ;; floats, so strip it to match. This also handles the -0.0 → 0
    ;; normalization. pgjdbc's getBoolean on a float column routes
    ;; through string parsing in text protocol and only accepts
    ;; "0"/"1"/"true"/... — never "0.0"/"1.0".
    (and (or (instance? Float v) (instance? Double v))
         (let [d (double v)]
           (and (Double/isFinite d)
                (== d (Math/rint d))
                (< (Math/abs d) 1e15))))
    (let [d (double v)
          ;; Normalize -0.0 to 0.0 for both Float and Double (pgjdbc's
          ;; boolean/number parsers accept "0" but not "-0").
          v (if (zero? d) 0.0 v)
          s (str v)]
      (if (str/ends-with? s ".0") (subs s 0 (- (count s) 2)) s))
    ;; CAST results carry a Java type that encodes the source SQL type,
    ;; so we can emit the PG-correct text form without dragging the
    ;; timestamp through a lossy date-only conversion.
    (instance? java.time.LocalDate v) (str v)       ;; "2017-03-13"
    (instance? java.time.LocalTime v) (str v)       ;; "14:25:48.130861"
    (instance? java.time.LocalDateTime v)           ;; "2017-03-13 14:25:48.130861"
    (-> (str v) (str/replace "T" " "))
    (inst? v)    (let [^java.time.Instant inst
                       (if (instance? java.util.Date v)
                         (.toInstant ^java.util.Date v)
                         (if (instance? java.time.Instant v)
                           v
                           (.toInstant ^java.util.Date v)))
                        ;; ISO-8601 format: 2024-01-15T10:30:00Z → 2024-01-15 10:30:00
                       s (str inst)]
                   (-> s
                       (str/replace "T" " ")
                       (str/replace "Z" "")))
    (uuid? v)    (str v)
    (symbol? v)  (str v)
    (bytes? v)   "<bytes>"
    ;; Maps and vectors (jsonb values) → serialize as JSON
    (map? v)     (jb/serialize-jsonb v)
    (vector? v)  (jb/serialize-jsonb v)
    :else        (str v)))

(defn- infer-oid
  "Infer a PostgreSQL type OID from a Clojure value.
   Delegates to centralized type registry."
  [v]
  (types/infer-oid-from-value v))

;; ============================================================================
;; HAVING post-filter
;; ============================================================================

(defn- having-pred-fn
  "Return a predicate function for a single HAVING clause spec.
   Spec is a map {:op <symbol> :col-idx <int> :value <number>}
   or nested {:op :and/:or :clauses [...]}.
   Returns a predicate (fn [row] -> bool) or nil if unresolvable."
  [having-spec]
  (cond
    ;; Nested AND/OR
    (= (:op having-spec) :and)
    (let [preds (keep having-pred-fn (:clauses having-spec))]
      (when (seq preds)
        (fn [row] (every? #(% row) preds))))

    (= (:op having-spec) :or)
    (let [preds (keep having-pred-fn (:clauses having-spec))]
      (when (seq preds)
        (fn [row] (some #(% row) preds))))

    ;; IS NULL / IS NOT NULL
    (#{:is-null :is-not-null} (:op having-spec))
    (when-let [col-idx (:col-idx having-spec)]
      (let [check-null? (= :is-null (:op having-spec))]
        (fn [row]
          (let [row-vec (if (sequential? row) (vec row) [row])
                cell (nth row-vec col-idx nil)
                is-null? (or (nil? cell) (= :__null__ cell))]
            (if check-null? is-null? (not is-null?))))))

    ;; Leaf comparison. SQL three-valued logic: a NULL cell makes
    ;; `cell op value` evaluate to UNKNOWN, and HAVING treats UNKNOWN
    ;; as FALSE (the group is filtered out). Without this guard,
    ;; `(> :__null__ 15)` throws on the Keyword-vs-Number compare.
    (:col-idx having-spec)
    (let [{:keys [op col-idx value]} having-spec
          op-fn (cond
                  (= op '>) >
                  (= op '>=) >=
                  (= op '<) <
                  (= op '<=) <=
                  (= op '=) =
                  (= op '<>) not=
                  (= op '!=) not=
                  :else =)]
      (fn [row]
        (let [row-vec (if (sequential? row) (vec row) [row])
              cell (nth row-vec col-idx nil)]
          (if (or (nil? cell) (= :__null__ cell))
            false
            (let [cell-num (cond
                             (number? cell) cell
                             (instance? clojure.lang.Ratio cell) (double cell)
                             :else cell)]
              (op-fn cell-num value))))))

    :else nil))

(defn- apply-having
  "Filter result rows by HAVING predicates.
   having-spec is a map {:op ... :col-idx ... :value ...} from the SQL parser."
  [results _find-aliases having-spec]
  (if (nil? having-spec)
    results
    (if-let [pred (having-pred-fn having-spec)]
      (filter pred results)
      results)))

;; ============================================================================
;; Result formatting
;; ============================================================================

(defn- find-var->attr
  "Map each :find symbol of a translated query back to the schema
   attribute keyword that binds it. Shared by compute-schema-oids and
   compute-column-sources."
  [parsed]
  (let [where-clauses (:where (:query parsed))]
    (into {}
          (keep (fn [clause]
                  (cond
                    (and (vector? clause) (= 3 (count clause))
                         (keyword? (second clause))
                         (symbol? (nth clause 2)))
                    [(nth clause 2) (second clause)]
                    (and (vector? clause) (= 2 (count clause))
                         (seq? (first clause))
                         (= 'get-else (ffirst clause))
                         (symbol? (second clause)))
                    [(second clause) (nth (vec (first clause)) 3)])))
          where-clauses)))

(defn- compute-column-sources
  "Per-column [tableOid attnum typmod] arrays for the RowDescription.
   Returns [int[] short[] int[]] when at least one column has a known
   source table; otherwise nil (caller omits the fields — pgjdbc sees
   zeros and short-circuits getBaseColumnName to \"\", which is the
   existing behavior for non-table expressions).

   typmod (PG's per-column type modifier, e.g. NUMERIC(10, 2)
   precision+scale encoded per types/encode-numeric-typmod) is read
   from the attribute's `:pg/typmod` ident-attached marker via the
   live db. Defaults to -1 (unspecified) when no marker is set —
   matches PG's RowDescription default for unconstrained columns.

   pgjdbc uses (tableOid, attnum) as the cache key for its field-
   metadata query, so these values MUST agree with the rows we emit
   in pg_class and pg_attribute — that's why both sides route through
   schema.clj helpers (table-oid, column-attnum)."
  [parsed db]
  (when (and db (:find-aliases parsed) (:query parsed))
    (let [aliases (:find-aliases parsed)
          find-vars (:find (:query parsed))
          var->attr (find-var->attr parsed)
          schema (dbi/-schema db)
          ;; Bulk-fetch typmods for the schema attrs we'll need; falls
          ;; back to -1 per column when the attr has none. One Datalog
          ;; query per RowDescription emission instead of N point lookups.
          typmod-map (when db
                       (into {} (d/q '{:find [?ident ?tm]
                                       :where [[?e :db/ident ?ident]
                                               [?e :pg/typmod ?tm]]}
                                     db)))
          n (count aliases)
          toids (int-array n)
          attnums (short-array n)
          typmods (int-array n -1)
          any? (atom false)]
      (dotimes [i n]
        (let [fvar (when (< i (count find-vars)) (nth find-vars i))
              attr (when (symbol? fvar) (get var->attr fvar))
              tname (when attr (namespace attr))
              cname (when attr (name attr))
              toid (when tname (pgs/table-oid db tname))
              anum (when (and tname cname)
                     (pgs/column-attnum schema tname cname))
              tm (when attr (get typmod-map attr))]
          (when (and toid anum)
            (reset! any? true)
            (aset-int toids i (int toid))
            (aset-short attnums i (short anum)))
          (when tm
            (reset! any? true)
            (aset-int typmods i (int tm)))))
      (when @any? [toids attnums typmods]))))

(defn- compute-schema-oids
  "Resolve the PG OID for each :find element of a parsed SELECT.

   We walk the translated :where clauses to map each find-var back to a
   schema attribute keyword, then look up that attribute's :db/valueType
   (or :pg/type override for jsonb) in the live db schema. A -1 sentinel
   means 'no schema attr bound' (aggregates, expressions, literals) — the
   caller refines these via value-based inference once results are known.

   Used by both the extended-query Describe step (results not yet
   available — we must return typed RowDescription ahead of Execute) and
   the simple-query path (cross-check against value inference)."
  [parsed db]
  (let [find-aliases (:find-aliases parsed)
        query        (:query parsed)
        where-clauses (:where query)
        find-vars    (:find query)
        schema       (dbi/-schema db)
        var->attr (into {}
                        (keep (fn [clause]
                                (cond
                                  (and (vector? clause) (= 3 (count clause))
                                       (keyword? (second clause))
                                       (symbol? (nth clause 2)))
                                  [(nth clause 2) (second clause)]
                                  (and (vector? clause) (= 2 (count clause))
                                       (seq? (first clause))
                                       (= 'get-else (ffirst clause))
                                       (symbol? (second clause)))
                                  [(second clause) (nth (vec (first clause)) 3)])))
                        where-clauses)
        pgtype-map (when db
                     (into {}
                           (d/q '{:find [?ident ?pt]
                                  :where [[?e :db/ident ?ident]
                                          [?e :pg/type ?pt]]}
                                db)))]
    (let [;; Promote cardinality-many props to array OIDs — matches what
          ;; col-var! emits for `:db.cardinality/many :db.type/ref`
          ;; columns (PgArray of target PKs). Without this, describe-
          ;; Result reports int8 for what's actually int8[], and pgjdbc
          ;; calls .toLong on the PgArray.
          oid-for-props (fn [props]
                          (let [base (pgs/oid-for-valuetype (:db/valueType props))]
                            (if (= :db.cardinality/many (:db/cardinality props))
                              (get types/element-oid->array-oid
                                   base
                                   types/oid-text-array)
                              base)))]
      (int-array
       (map-indexed
        (fn [i alias]
          (let [fvar (when (< i (count find-vars)) (nth find-vars i))
                attr (when (symbol? fvar) (get var->attr fvar))
                props (when attr (get schema attr))]
            (if props
              (if-let [pgtype (and pgtype-map (get pgtype-map attr))]
                (case pgtype
                  "jsonb" types/oid-jsonb
                  "json"  types/oid-jsonb
                  ;; Native PG array column: `:pg/type "_T"` resolves
                  ;; via pg-name->oid to the corresponding array OID
                  ;; (`_int4` → 1007 etc.). Fall back to the storage
                  ;; type's OID when not in the array registry.
                  (or (get types/pg-name->oid pgtype)
                      (oid-for-props props)))
                (oid-for-props props))
              (or (some (fn [[attr-kw p]]
                          (when (and (keyword? attr-kw)
                                     (= (name attr-kw) alias)
                                     (not (str/starts-with? (str (namespace attr-kw)) "__")))
                            (oid-for-props p)))
                        schema)
                  -1))))
        find-aliases)))))

(defn- format-query-result
  "Format Datalog query results into a PgWire QueryResult.
   Handles empty result sets by returning proper column metadata with 0 rows.
   Optional schema-oids: int array of OIDs to use when results are empty."
  ([results find-aliases] (format-query-result results find-aliases nil))
  ([results find-aliases schema-oids]
   (let [col-names (into-array String find-aliases)
         result-seq (seq results)
        ;; Determine OIDs: prefer schema-oids, refine unknowns with value inference
         first-row (first result-seq)
         oids (cond
               ;; Schema OIDs provided: use them, but refine -1 sentinel using value inference.
               ;; The sentinel -1 means "no schema attr found" (aggregates, expressions).
               ;; Regular OID_TEXT means "schema says it's a text column" — don't override.
                (and schema-oids first-row)
                (int-array (map-indexed
                            (fn [i _alias]
                              (let [schema-oid (aget ^ints schema-oids i)]
                                (if (= schema-oid -1)
                                 ;; No schema attr found — use value-based inference
                                  (let [v (if (sequential? first-row) (nth first-row i nil) first-row)]
                                    (infer-oid v))
                                  schema-oid)))
                            find-aliases))
                schema-oids
               ;; Replace any -1 sentinels with OID_TEXT for empty results
                (if (some #(= % -1) schema-oids)
                  (int-array (map #(if (= % -1) PgWireServer/OID_TEXT %) schema-oids))
                  schema-oids)
                first-row
                (int-array (map-indexed
                            (fn [i _alias]
                              (let [v (if (sequential? first-row)
                                        (nth first-row i nil)
                                        first-row)]
                                (infer-oid v)))
                            find-aliases))
                :else (int-array (repeat (count find-aliases) PgWireServer/OID_TEXT)))
        ;; Convert rows to String[][]
         rows (if result-seq
                (into-array (Class/forName "[Ljava.lang.String;")
                            (for [row result-seq]
                              (into-array String
                                          (if (sequential? row)
                                            (map value->string row)
                                            [(value->string row)]))))
                (into-array (Class/forName "[Ljava.lang.String;")
                            (make-array String 0 0)))]
     (PgWireServer$QueryResult.
      col-names oids rows
      (str "SELECT " (alength ^"[[Ljava.lang.String;" rows))))))

(defn- empty-result [tag]
  (PgWireServer$QueryResult/empty tag))

(defn- error-result
  "Build an error QueryResult. Optional sqlstate defaults to \"XX000\".

   PostgreSQL clients branch on SQLSTATE (Odoo retries on 40001 serialization
   failures, ORMs map 23505 to unique violations, etc.). Pass the most
   specific code you can determine — see classify-exception for the usual
   mapping from Datahike ex-data."
  ([^String msg] (PgWireServer$QueryResult. msg))
  ([^String msg ^String sqlstate]
   (PgWireServer$QueryResult. msg sqlstate)))

(def ^:private wire-debug?
  (some? (System/getenv "DATAHIKE_WIRE_DEBUG")))

(defn- parse-execute-args
  "Split the argument list of a SQL-level EXECUTE into top-level
   literals. Respects single-quoted strings (with '' escape) and
   parenthesis nesting so commas inside those don't split the list.

   Returns a vector of trimmed raw-text args, preserving quote marks
   on strings so they're valid SQL literals after interpolation."
  [^String args-str]
  (if (or (nil? args-str) (str/blank? args-str))
    []
    (let [len (count args-str)]
      (loop [i 0
             in-str? false
             depth 0
             start 0
             acc []]
        (if (>= i len)
          (let [tail (str/trim (subs args-str start))]
            (if (pos? (count tail)) (conj acc tail) acc))
          (let [c (.charAt args-str i)]
            (cond
              ;; inside a string: only exit on a non-escaped closing quote
              in-str?
              (if (= c \')
                (if (and (< (inc i) len) (= \' (.charAt args-str (inc i))))
                  (recur (+ i 2) true depth start acc)            ; '' → escaped
                  (recur (inc i) false depth start acc))
                (recur (inc i) true depth start acc))
              (= c \')  (recur (inc i) true depth start acc)
              (= c \()  (recur (inc i) false (inc depth) start acc)
              (= c \))  (recur (inc i) false (dec depth) start acc)
              (and (= c \,) (zero? depth))
              (recur (inc i) false 0 (inc i)
                     (conj acc (str/trim (subs args-str start i))))
              :else (recur (inc i) false depth start acc))))))))

(defn- substitute-prepared-params
  "Walk a PREPARE template and replace each placeholder with the
   corresponding arg literal:
     ?        → next positional arg
     $1..$N   → 1-indexed arg

   Placeholders inside single-quoted string literals OR double-quoted
   SQL identifiers are left alone — Metabase's describe-fields query
   uses `\"pk?\"` as a column alias, and substituting that `?` would
   silently turn the alias into the first parameter value.

   Positional (`?`) and numbered (`$N`) placeholders can be mixed;
   `?` consumes from a separate cursor so they don't fight over
   indices with `$N`. When a mix is present, most clients pick one
   style for the whole template — we allow either."
  [^String template args]
  (let [len (count template)
        sb (StringBuilder.)]
    (loop [i 0
           in-str?   false   ; inside '…'
           in-ident? false   ; inside "…"
           pos-idx 0]
      (if (>= i len)
        (.toString sb)
        (let [c (.charAt template i)]
          (cond
            in-str?
            (do (.append sb c)
                (cond
                  (and (= c \') (< (inc i) len) (= \' (.charAt template (inc i))))
                  (do (.append sb \')
                      (recur (+ i 2) true false pos-idx))
                  (= c \') (recur (inc i) false false pos-idx)
                  :else    (recur (inc i) true false pos-idx)))
            in-ident?
            (do (.append sb c)
                (cond
                  ;; "" inside an identifier is the literal " escape.
                  (and (= c \") (< (inc i) len) (= \" (.charAt template (inc i))))
                  (do (.append sb \")
                      (recur (+ i 2) false true pos-idx))
                  (= c \") (recur (inc i) false false pos-idx)
                  :else    (recur (inc i) false true pos-idx)))
            (= c \')
            (do (.append sb c) (recur (inc i) true false pos-idx))
            (= c \")
            (do (.append sb c) (recur (inc i) false true pos-idx))
            (= c \?)
            (do (.append sb (if (< pos-idx (count args))
                              (nth args pos-idx)
                              "NULL"))
                (recur (inc i) false false (inc pos-idx)))
            (= c \$)
            ;; $N — consume digits
            (let [j (long (loop [k (inc i)]
                            (if (and (< k len) (Character/isDigit (.charAt template k)))
                              (recur (inc k)) k)))]
              (if (> j (inc i))
                (let [n (Long/parseLong (subs template (inc i) j))
                      ;; $1 → args[0]
                      arg (nth args (dec n) "NULL")]
                  (.append sb arg)
                  (recur j false false pos-idx))
                (do (.append sb c) (recur (inc i) false false pos-idx))))
            :else (do (.append sb c) (recur (inc i) false false pos-idx))))))))

(defmacro ^:private with-stmt-timeout
  "Schedule a cancel-flag flip after `timeout-ms` (nil or 0 disables);
   execute body; cancel the timer on return.

   The flag is observed by the query engine's check-cancel! macro on
   every scan iteration, so a long-running query is interrupted
   mid-flight and raises a `:datahike/canceled` ex-info — which
   `datahike.pg.errors/classify-exception` then maps to the wire code
   57014 (query_canceled). The wire layer also interrupts the backend
   thread as a safety net for any blocking I/O outside check-cancel!
   coverage (e.g. konserve reads against a disk-backed store)."
  [timeout-ms & body]
  `(let [t# ~timeout-ms
         flag# (.get ^ThreadLocal datahike.pg.PgWireServer/CANCEL_FLAG)
         timer# (when (and t# flag# (pos? t#))
                  (.schedule ^java.util.concurrent.ScheduledExecutorService
                   datahike.pg.PgWireServer/TIMEOUT_SCHED
                             ^Runnable (fn [] (.set ^java.util.concurrent.atomic.AtomicBoolean
                                               flag# true))
                             (long t#)
                             java.util.concurrent.TimeUnit/MILLISECONDS))]
     (try ~@body
          (finally
            (when timer#
              (.cancel ^java.util.concurrent.ScheduledFuture timer# false))))))

(defn- classified-error
  "Build an error QueryResult from a Throwable with auto-detected
   SQLSTATE and ErrorResponse detail fields (constraint name, table,
   column, data type, detail text — depending on what's in ex-data).
   `prefix` is prepended to the exception message.

   Prefer this over `(error-result (str prefix (.getMessage e)))` so
   ORMs get structured diagnostics. See
   `datahike.pg.errors/classify-exception` for the mapping from
   Datahike ex-data to PG error fields.

   Writes the full cause-chain stacktrace to stderr when
   `DATAHIKE_WIRE_DEBUG` is set in the environment — ORMs that get an
   opaque XX000 can then be investigated without re-running the
   client."
  [^String prefix ^Throwable e]
  (when wire-debug?
    (binding [*out* *err*]
      (println (str "[PGWIRE] " prefix "sqlstate="
                    (first (errors/classify-exception e))))
      (.printStackTrace e ^java.io.PrintWriter *err*)))
  (let [[sqlstate msg fields] (errors/classify-exception e)
        ^PgWireServer$QueryResult result (error-result (str prefix msg) sqlstate)]
    (if fields (.withErrorFields result fields) result)))

(defn- rewrite-cursor-page
  "Given a user SELECT and a page offset + optional limit, produce a
   SQL string that paginates the user's query *without* wrapping it
   in a subquery — wrapping loses the inner ORDER BY because our SQL
   translator doesn't propagate row order through nested selects.

   Strategy: parse the user's SQL with JSqlParser, combine the user's
   LIMIT/OFFSET with the cursor's page position on the same
   PlainSelect, emit the rewritten SQL.

   If the user wrote their own LIMIT N (to cap the cursor's total
   result size), we respect it — the cursor cannot fetch past that.
   If they wrote OFFSET M, we add M to our pos so the cursor starts
   where they said. Unparsable SQL falls through to string concat."
  [^String cquery ^long pos ^Long page-limit]
  (try
    (let [stmt (CCJSqlParserUtil/parse cquery)]
      (if-not (instance? PlainSelect stmt)
        (str cquery " LIMIT " (or page-limit 1) " OFFSET " pos)
        (let [^PlainSelect ps stmt
              user-limit (when-let [^Limit l (.getLimit ps)]
                           (when-let [row-count (.getRowCount l)]
                             (when (instance? LongValue row-count)
                               (.getValue ^LongValue row-count))))
              user-offset (when-let [^Offset o (.getOffset ps)]
                            (when-let [ov (.getOffset o)]
                              (when (instance? LongValue ov)
                                (.getValue ^LongValue ov))))
              base-offset (long (or user-offset 0))
              effective-offset (+ base-offset pos)
              remaining (when user-limit (max 0 (- (long user-limit) pos)))
              effective-limit (cond
                                (and page-limit remaining)
                                (min page-limit remaining)
                                page-limit page-limit
                                remaining remaining
                                :else nil)]
          (if (and remaining (zero? remaining))
            (str cquery " LIMIT 0")
            (do (when effective-limit
                  (let [l (Limit.)]
                    (.setRowCount l (LongValue. (long effective-limit)))
                    (.setLimit ps l)))
                (when (pos? effective-offset)
                  (let [o (Offset.)]
                    (.setOffset o (LongValue. effective-offset))
                    (.setOffset ps o)))
                (.toString ps))))))
    (catch Exception _
      (str cquery " LIMIT " (or page-limit 1) " OFFSET " pos))))

(defn- tx-buffer-attrs
  "Return the set of attribute keywords this tx-buffer will write.
   Handles both map-form entities ({:db/id _ :ns/col val}) and
   vector-form ops ([:db/add e a v], [:db/retract e a v],
   [:db.fn/retractEntity e], [:db.fn/cas e a old new]).

   Used by COMMIT's conflict check: if none of these attributes were
   also touched by a concurrent committer, our writes can't conflict."
  [buf]
  (into #{}
        (mapcat (fn [item]
                  (cond
                    (map? item)
                    (keep #(when (and (keyword? %)
                                      (not= :db/id %)
                                      (not= % (keyword (namespace %) "db-row-exists")))
                             %)
                          (keys item))

                    (vector? item)
                    (let [op (first item)]
                      (case op
                        (:db/add :db/retract)
                        [(nth item 2 nil)]
                        :db.fn/cas
                        [(nth item 2 nil)]
                        :db.fn/retractEntity
                        ;; Entity retraction touches all attrs of the
                        ;; target — we don't know which without a db
                        ;; lookup. Conservative: treat as wildcard by
                        ;; returning nil; the caller falls back to the
                        ;; pessimistic "any concurrent write conflicts"
                        ;; check when it sees nil in the set.
                        [::wildcard]
                        nil))

                    :else nil)))
        buf))

(defn- concurrent-write-attrs
  "Query the real DB for attributes written after `begin-tx`. Returns
   a set of attribute keywords, or ::any if the query fails (caller
   falls back to the pessimistic 'any concurrent write conflicts'
   signal).

   Datahike datoms store the attribute as the ident keyword directly
   (not an entity id), so `?a` is already the keyword we want."
  [db begin-tx]
  (try
    (let [q-fn d/q]
      (into #{}
            (map first)
            (q-fn '{:find [?a]
                    :in [$ ?begin]
                    :where [[?e ?a ?v ?tx]
                            [(> ?tx ?begin)]]}
                  db begin-tx)))
    (catch Exception _ ::any)))

(defn- tag-tx-status
  "Set the txStatus field on a QueryResult based on tx-state atom."
  [^PgWireServer$QueryResult result tx-state]
  (.withTxStatus result
                 (cond (:aborted? @tx-state) \E
                       (:in-tx? @tx-state)   \T
                       :else                 \I)))

;; ============================================================================
;; System query handlers
;; ============================================================================

(def ^:private show-settings
  {"server_version"                "15.0"
   "server_encoding"               "UTF8"
   "client_encoding"               "UTF8"
   "search_path"                   "\"$user\", public"
   "standard_conforming_strings"   "on"
   "default_transaction_isolation" "read committed"
   "transaction_isolation"         "read committed"
   "datestyle"                     "ISO, MDY"
   "timezone"                      "UTC"
   "integer_datetimes"             "on"
   "IntervalStyle"                 "postgres"
   "max_connections"               "100"
   "max_identifier_length"         "63"})

(defn- show-setting
  "Return a single-column SHOW QueryResult for setting-name / value."
  [setting-name value]
  (PgWireServer$QueryResult.
   (into-array String [setting-name])
   (int-array [PgWireServer/OID_TEXT])
   (into-array (Class/forName "[Ljava.lang.String;")
               [(into-array String [(or value "")])])
   "SHOW"))

(defn- handle-show
  "Returns the value of a session setting. `setting-name` is already
   lowercased (classify supplies it via :var)."
  [^String setting-name schema session-state]
  (let [setting-name (or setting-name "")]
    (cond
      (= setting-name "tables")
      (let [tables (pgs/table-names schema)
            rows (into-array (Class/forName "[Ljava.lang.String;")
                             (for [t tables]
                               (into-array String ["public" t "datahike"])))]
        (PgWireServer$QueryResult.
         (into-array String ["Schema" "Name" "Owner"])
         (int-array [PgWireServer/OID_TEXT PgWireServer/OID_TEXT PgWireServer/OID_TEXT])
         rows
         (str "SELECT " (count tables))))

      (str/starts-with? setting-name "transaction")
      (show-setting "transaction_isolation" "read committed")

      ;; statement_timeout: report the session-local value (ms) or 0.
      (= setting-name "statement_timeout")
      (show-setting "statement_timeout"
                    (str (or (:statement-timeout @session-state) 0)))

      (contains? show-settings setting-name)
      (show-setting setting-name (get show-settings setting-name))

      :else
      (show-setting setting-name ""))))

;; Catalog and information_schema queries are handled by virtual table
;; materialization in sql.clj (CTE-injected speculative data), not by
;; string-matching handlers. This supports WHERE, JOIN, and projection
;; on catalog tables properly.

(defn- handle-version []
  (PgWireServer$QueryResult.
   (into-array String ["version"])
   (int-array [PgWireServer/OID_TEXT])
   (into-array (Class/forName "[Ljava.lang.String;")
               [(into-array String ["PostgreSQL 15.0 (Datahike PgWire compatibility layer)"])])
   "SELECT 1"))

(defn- handle-current-schema []
  (PgWireServer$QueryResult.
   (into-array String ["current_schema"])
   (int-array [PgWireServer/OID_TEXT])
   (into-array (Class/forName "[Ljava.lang.String;")
               [(into-array String ["public"])])
   "SELECT 1"))

(defn- handle-current-database
  "Return the connection's own database name. In the single-handler
   case the handler was created without a registry and `db-name` is the
   legacy placeholder; with a registry (`start-server {name → conn}`),
   it is the looked-up name from the StartupMessage `database` param."
  [db-name]
  (PgWireServer$QueryResult.
   (into-array String ["current_database"])
   (int-array [PgWireServer/OID_TEXT])
   (into-array (Class/forName "[Ljava.lang.String;")
               [(into-array String [(or db-name "datahike")])])
   "SELECT 1"))

;; ============================================================================
;; DML execution
;; ============================================================================

(defn- build-returning-result
  "Build a QueryResult for RETURNING clause from entity IDs.
   returning: :* for all columns, or [col-name ...] for specific columns.
   db: database to read values from (db-after for INSERT/UPDATE, db-before for DELETE).
   eids: entity IDs to return.
   table-name: the table name (namespace prefix for attributes).
   schema: database schema."
  [returning db eids table-name schema]
  (let [col-names (if (= :* returning)
                    (let [cols (pgs/column-info schema table-name db)]
                      (mapv :name (rest cols)))  ;; skip db_id
                    returning)
        ;; Resolve inherited attrs: for INHERITS, some columns live in parent namespace
        raw-attrs (mapv #(keyword table-name %) col-names)
        attrs (mapv #(#'sql/resolve-inherited-attr % schema db) raw-attrs)
        rows (for [eid eids]
               (let [datoms (d/datoms db :eavt eid)
                     entity-map (into {} (map (fn [^datahike.datom.Datom d]
                                                [(.-a d) (.-v d)])
                                              datoms))]
                 (mapv (fn [attr] (get entity-map attr)) attrs)))
        row-arrays (into-array (Class/forName "[Ljava.lang.String;")
                               (for [row rows]
                                 (into-array String (map value->string row))))
        col-name-array (into-array String col-names)
        ;; Derive OIDs from schema types for proper client-side type mapping
        oids (int-array (map (fn [attr]
                               (if-let [vtype (get-in schema [attr :db/valueType])]
                                 (pgs/oid-for-valuetype vtype)
                                 PgWireServer/OID_TEXT))
                             attrs))]
    (PgWireServer$QueryResult.
     col-name-array oids row-arrays
     (str "INSERT 0 " (count eids)))))

;; ----------------------------------------------------------------------------
;; Per-schema memoisation for constraint metadata.
;;
;; `read-{column,check,fk}-constraints`, `compute-identity-cols`, and
;; `enrich-schema-with-pg-array-meta` are pure functions of the schema —
;; same (schema, table) pair always yields the same result, until DDL
;; transacts a change. The CPU profile of a Pagila pg_dump replay
;; showed these recompute on every INSERT (each ~0.5-0.7 ms/row of
;; wall time on top of d/transact's 1.3 ms baseline). Caching them by
;; schema-map identity drops that overhead to ~zero.
;;
;; Outer: `Collections.synchronizedMap(WeakHashMap)` keyed on the
;; schema map's IDENTITY (System/identityHashCode). Schema maps are
;; interned-by-equality-not-identity from Clojure's perspective; we
;; want identity so equal-but-distinct schemas across test fixtures
;; don't share entries. WeakHashMap reclaims entries when the schema
;; is GC'd (after the last db pinning it goes out of scope).
;;
;; Inner: `ConcurrentHashMap` keyed on the cache-key the caller
;; passed (e.g. `[::col "rental"]`). Concurrent for safety.
;;
;; PG-side metadata (`:pg/not-null`, `:pg/check-*`, `:pg/fk-*`,
;; `:pg/default-*`, `:pg/array-elem`) is stored on the schema-
;; attribute entity but does NOT appear in `(dbi/-schema db)` — only
;; `:db/valueType` / `:db/cardinality` / `:db/unique` do. A DDL
;; that adds NOT NULL to an existing column therefore doesn't change
;; schema-map identity, and the identity-keyed cache would return
;; stale info. We invalidate the cache on every DDL exec branch.
;; DDL is rare; the bust is cheap.
;;
;; `cache-stats` is a hit/miss counter exposed for tests/
;; observability. A verification harness can call
;; `(reset! cache-stats {})`, load some INSERTs, and confirm
;; `(:hit @cache-stats) ≫ (:miss …)` to confirm the cache fires
;; as intended.
;; ----------------------------------------------------------------------------

(def ^:private schema-deriv-cache
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

(def ^:dynamic *schema-cache-enabled?*
  "Bind false to bypass the cache. For perf comparisons only;
   production code should leave this on."
  true)

(defonce ^{:doc "Hit/miss counter for the schema cache. Atom whose
  identity is stable across reloads so external observers can hold
  a reference and watch counts. Mutated by every `schema-cached`
  call; read for inspection / verification harnesses."}
  cache-stats
  (atom {:hit 0 :miss 0}))

(defn invalidate-schema-cache!
  "Clear the per-schema cache. Called from every DDL exec branch."
  []
  (.clear ^java.util.Map schema-deriv-cache))

(defn- schema-cached
  "`(schema-cached db cache-key produce)` — memoise `(produce)`
   (a 0-arg thunk) by `[schema-identity cache-key]`."
  [db cache-key produce]
  (if-not *schema-cache-enabled?*
    (produce)
    (let [schema (dbi/-schema db)
          ^java.util.Map outer schema-deriv-cache
          ^java.util.concurrent.ConcurrentHashMap inner
          (or (.get outer schema)
              (locking outer
                (or (.get outer schema)
                    (let [m (java.util.concurrent.ConcurrentHashMap.)]
                      (.put outer schema m)
                      m))))
          existing (.get inner cache-key)]
      (if (some? existing)
        (do (swap! cache-stats update :hit (fnil inc 0))
            (if (= ::nil existing) nil existing))
        (let [v (produce)]
          (swap! cache-stats update :miss (fnil inc 0))
          (.putIfAbsent inner cache-key (if (nil? v) ::nil v))
          v)))))

(defn- compute-identity-cols
  "Discover IDENTITY-backed columns of `table-name`. Two d/q calls —
   INHERITS lookup + sequences-by-prefix. Pure function of the
   schema; cached per (schema, table)."
  [db table-name]
  (let [parent-table (ffirst (d/q '{:find [?p]
                                    :where [[?e :__inherit__/child ?c]
                                            [?e :__inherit__/parent ?p]]
                                    :in [$ ?c]}
                                  db table-name))
        tables-to-check (if parent-table [table-name parent-table] [table-name])
        schema (dbi/-schema db)]
    (vec (mapcat
          (fn [tbl]
            (let [seq-prefix (str tbl "_")
                  seq-results (d/q '{:find [?name]
                                     :where [[?e :__seq__/name ?name]]
                                     :in [$ ?prefix]}
                                   db seq-prefix)]
              (keep (fn [[sname]]
                      (let [pref-len (count seq-prefix)
                            tail-end (- (count sname) 4)]
                        (when (and (str/starts-with? sname seq-prefix)
                                   (str/ends-with? sname "_seq")
                                   (< pref-len tail-end))
                          (let [col-name (subs sname pref-len tail-end)
                                attr (keyword tbl col-name)]
                            (when (get schema attr)
                              {:col col-name :ns tbl :seq-name sname})))))
                    seq-results)))
          tables-to-check))))

(defn- auto-populate-identity
  "If the table has IDENTITY columns (backed by __seq__ sequences), populate
   any missing identity attributes in the INSERT tx-data using :db.fn/call
   for atomic increment. Also checks parent table sequences for INHERITS.

   The identity-cols set is memoised per (schema, table) — this used
   to fire two d/q calls per INSERT even on tables without identity
   columns."
  [tx-data table-name db]
  (let [identity-cols (schema-cached db [::identity table-name]
                                     #(compute-identity-cols db table-name))
        identity-cols (if (= ::nil identity-cols) [] identity-cols)]
    (if (empty? identity-cols)
      tx-data
      ;; Wrap entire INSERT in :db.fn/call to atomically generate IDs.
      ;; Uses a local atom to track running sequence values across rows
      ;; within the same multi-row INSERT (txdb is immutable, can't see
      ;; prior rows' sequence increments).
      [[:db.fn/call
        (fn [txdb]
          (let [q-fn d/q
                ;; Pre-fetch sequence state for each identity column
                seq-state (atom
                           (into {}
                                 (for [{:keys [col ns seq-name]} identity-cols
                                       :let [seq-eid (ffirst (q-fn '{:find [?e]
                                                                     :where [[?e :__seq__/name ?n]]
                                                                     :in [$ ?n]}
                                                                   txdb seq-name))
                                             curr-val (when seq-eid
                                                        (or (ffirst (q-fn '{:find [?v]
                                                                            :where [[?e :__seq__/value ?v]]
                                                                            :in [$ ?e]}
                                                                          txdb seq-eid))
                                                            0))
                                             increment (or (when seq-eid
                                                             (ffirst (q-fn '{:find [?i]
                                                                             :where [[?e :__seq__/increment ?i]]
                                                                             :in [$ ?e]}
                                                                           txdb seq-eid)))
                                                           1)]
                                       :when seq-eid]
                                   [col {:eid seq-eid :val (or curr-val 0) :inc increment :ns ns}])))]
            (vec (mapcat
                  (fn [entity-map]
                    (if-not (map? entity-map)
                      [entity-map]
                      (let [populated
                            (reduce
                             (fn [m {:keys [col ns]}]
                               ;; Use the column's owning namespace (may be parent for inherited)
                               (let [attr (keyword ns col)]
                                 (if (contains? m attr)
                                   m
                                   ;; Auto-generate: increment local counter
                                   (let [{:keys [val inc]} (get @seq-state col)
                                         new-val (+ val inc)]
                                     (swap! seq-state assoc-in [col :val] new-val)
                                     (assoc m attr new-val)))))
                             entity-map
                             identity-cols)]
                        (let [seq-updates
                              (keep (fn [{:keys [col ns]}]
                                      (let [attr (keyword ns col)]
                                        (when-not (contains? entity-map attr)
                                          (let [{:keys [eid val]} (get @seq-state col)]
                                            (when eid
                                              [:db/add eid :__seq__/value val])))))
                                    identity-cols)]
                          (into [populated] seq-updates)))))
                  tx-data))))]])))

(defn- eval-default
  "Evaluate a :pg/default-* triple at INSERT time. Stateless defaults
   (literal + function) resolve immediately; stateful ones (nextval)
   return a sentinel the tx-fn unwraps against the txdb snapshot so
   sequence bumps are atomic with the row insert.

   Returns either a concrete value or [::nextval seq-name] — the
   caller must handle that sentinel inside :db.fn/call."
  [kind value arg]
  (case kind
    :literal (try (cond
                    (nil? value) nil
                    (#{"true"} value) true
                    (#{"false"} value) false
                    (re-matches #"-?\d+" value) (Long/parseLong value)
                    (re-matches #"-?\d+\.\d+" value) (Double/parseDouble value)
                    :else value)
                  (catch Exception _ value))
    :fn      (case value
               "now"           (java.util.Date.)
               "current_date"  (java.time.LocalDate/now java.time.ZoneOffset/UTC)
               "current_time"  (java.time.LocalTime/now java.time.ZoneOffset/UTC)
               "current_user"  "datahike"
               nil)
    :nextval (when value [::nextval value])
    nil))

(defn- read-check-constraints*
  [db table-name]
  (mapv (fn [{:keys [name expr]}] {:name name :expr expr})
        (d/q '{:find [?n ?x]
               :keys [name expr]
               :in [$ ?tbl]
               :where [[?e :pg/check-name ?n]
                       [?e :pg/check-table ?tbl]
                       [?e :pg/check-expr ?x]]}
             db table-name)))

(defn- read-check-constraints
  "All CHECK constraints for `table-name`. Returns a vector of
   {:name str :expr str} pairs. Empty when the table has none.
   Memoised per (schema, table)."
  [db table-name]
  (let [v (schema-cached db [::check table-name]
                         #(read-check-constraints* db table-name))]
    (if (= ::nil v) [] v)))

(defn- parse-check-expression
  "Re-parse a stored CHECK expression string into a JSqlParser
   Expression AST. We do this at enforcement time (not at CREATE
   TABLE) so the cached serialized form stays simple strings —
   cheap to persist, round-trips across restarts, no ABI ties to
   JSqlParser's Expression class hierarchy."
  [^String expr-text]
  (try
    (net.sf.jsqlparser.parser.CCJSqlParserUtil/parseCondExpression expr-text)
    (catch Exception _
      (net.sf.jsqlparser.parser.CCJSqlParserUtil/parseExpression expr-text))))

(defn- enforce-check-constraints!
  "Evaluate every CHECK expression registered for `table-name` against
   a proposed entity map. Raises 23514 when the expression yields
   literal false. PG's semantics: CHECK returning NULL is NOT a
   violation (unknown → passes), distinct from NOT NULL — the
   tri-state from sql/eval-check-predicate encodes that directly."
  [db table-name ns entity-map]
  (let [checks (seq (read-check-constraints db table-name))]
    (when checks
      (let [schema (dbi/-schema db)]
        (doseq [{:keys [name expr]} checks]
          (let [ast (parse-check-expression expr)
                val (try
                      (sql/eval-check-predicate ast entity-map ns schema)
                      (catch Exception _ ::error))]
            (when (false? val)
              (throw (ex-info "check constraint violation"
                              {:error :check-violation
                               :table table-name
                               :constraint name})))))))))

(defn- read-domain-enum-checks*
  [db table-name]
  ;; Pull every column-attr in this table's namespace that has a
  ;; :datahike.pg/domain-of or :datahike.pg/enum-of hint. For each,
  ;; resolve the registry entity and pre-parse the CHECK expression
  ;; (domains) / freeze the value-set (enums) so per-row enforcement
  ;; is just a pre-computed lookup + AST eval / set membership.
  ;;
  ;; `get-else` doesn't accept nil as default; use the project-wide
  ;; `:__null__` sentinel and unwrap it in Clojure (matches the
  ;; convention in datahike.pg.jsonb / datahike.pg.window).
  (let [unwrap-null (fn [v] (when (not= v :__null__) v))
        domain-rows
        (d/q '{:find [?ident ?dname ?cname ?cexpr ?nn]
               :keys [ident domain-name check-name check-expr not-null]
               :in [$ ?tbl]
               :where [[?col :db/ident ?ident]
                       [?col :datahike.pg/domain-of ?dname]
                       [(namespace ?ident) ?ns]
                       [(= ?ns ?tbl)]
                       [?dom :datahike.pg.domain/name ?dname]
                       [(get-else $ ?dom :datahike.pg.domain/check-name :__null__) ?cname]
                       [(get-else $ ?dom :datahike.pg.domain/check-expr :__null__) ?cexpr]
                       [(get-else $ ?dom :datahike.pg.domain/not-null false) ?nn]]}
             db table-name)
        enum-rows
        (d/q '{:find [?ident ?ename ?vs]
               :keys [ident enum-name values]
               :in [$ ?tbl]
               :where [[?col :db/ident ?ident]
                       [?col :datahike.pg/enum-of ?ename]
                       [(namespace ?ident) ?ns]
                       [(= ?ns ?tbl)]
                       [?en :datahike.pg.enum/name ?ename]
                       [?en :datahike.pg.enum/values ?vs]]}
             db table-name)
        result (java.util.HashMap.)]
    (doseq [{:keys [ident domain-name check-name check-expr not-null]} domain-rows]
      (let [col (name ident)
            check-expr (unwrap-null check-expr)
            check-name (unwrap-null check-name)]
        (.put result col
              {:kind :domain
               :attr ident
               :domain-name domain-name
               :check-name check-name
               :not-null? not-null
               :check-ast (when check-expr
                            (try (parse-check-expression check-expr)
                                 (catch Throwable _ nil)))})))
    (let [enum-map (java.util.HashMap.)]
      (doseq [{:keys [ident enum-name values]} enum-rows]
        (let [col (name ident)
              cur (.get enum-map col)]
          (.put enum-map col
                {:kind :enum
                 :attr ident
                 :enum-name enum-name
                 :values (conj (or (:values cur) #{}) (str values))})))
      (doseq [[col spec] enum-map]
        (.put result col spec)))
    (into {} result)))

(defn- read-domain-enum-checks
  "Cached per (schema, table). Returns
     {col-name <spec>}
   where <spec> is either
     {:kind :domain :attr kw :domain-name str :check-name str
      :not-null? bool :check-ast AST-or-nil}
     {:kind :enum   :attr kw :enum-name str :values #{string ...}}.
   Empty when the table has no domain- or enum-typed columns."
  [db table-name]
  (let [v (schema-cached db [::dom-enum table-name]
                         #(read-domain-enum-checks* db table-name))]
    (if (= ::nil v) {} v)))

(defn- enforce-domain-enum-checks!
  "Per-row column-level domain CHECK + enum membership enforcement.
   Raises 23514 (CHECK violation) for domain failures, 22P02
   (invalid_text_representation) for enum membership failures.
   Both are PG-canonical. Cheap: each row visits only the columns
   that are domain- or enum-typed in this table."
  [db table-name ns entity-maps]
  (let [specs (read-domain-enum-checks db table-name)]
    (when (seq specs)
      (let [schema (dbi/-schema db)]
        (doseq [em entity-maps
                [col-name spec] specs
                :let [attr (:attr spec)
                      ;; Look up the value under either the schema-
                      ;; declared attr or the parsed ns-prefix; INSERT
                      ;; tx-data uses the latter.
                      v (or (get em attr) (get em (keyword ns col-name)))]]
          (cond
            ;; Domain :not-null lives on the domain itself, not the
            ;; column. Column-level :pg/not-null already fired above
            ;; in apply-column-constraints — this is the *domain*'s
            ;; constraint. Both are 23502.
            (and (= :domain (:kind spec))
                 (:not-null? spec)
                 (nil? v))
            (throw (ex-info "domain not-null violation"
                            {:error :not-null-violation
                             :table table-name
                             :column col-name
                             :domain (:domain-name spec)}))

            ;; Domain CHECK with a parsed AST. PG's `VALUE` keyword
            ;; refers to the column's value; bind it under the
            ;; conventional (keyword "" "VALUE") so the existing
            ;; eval-check-predicate / eval-update-expr machinery
            ;; resolves it without a special case.
            (and (= :domain (:kind spec)) (some? v) (:check-ast spec))
            (let [r (try
                      (sql/eval-check-predicate (:check-ast spec)
                                                {(keyword "" "VALUE") v}
                                                "" schema)
                      (catch Throwable _ ::error))]
              (when (false? r)
                (throw (ex-info "domain check constraint violation"
                                {:error :check-violation
                                 :table table-name
                                 :column col-name
                                 :constraint (or (:check-name spec)
                                                 (str (:domain-name spec) "_check"))
                                 :domain (:domain-name spec)
                                 :value v}))))

            ;; Enum membership. Stored values come back as :many
            ;; strings, regardless of how the user inserted (string vs
            ;; keyword). Compare via str-coercion so both shapes work.
            (and (= :enum (:kind spec)) (some? v))
            (when-not (contains? (:values spec) (str v))
              (throw (ex-info "invalid input value for enum"
                              {:error :invalid-text-representation
                               :type  (:enum-name spec)
                               :value v
                               :table table-name
                               :column col-name})))))))))

(defn- read-fk-constraints
  "All FK constraints where the given table is the CHILD side. Returns
   a vector of {:name :child-cols :parent-table :parent-cols} entries.
   Column lists come back as vectors (the stored form is JSON).
   Memoised per (schema, table)."
  [db table-name]
  (let [v (schema-cached db [::fk-child table-name]
                         #(let [rows (d/q '{:find [?n ?cc ?pt ?pc]
                                            :keys [name child-cols parent-table parent-cols]
                                            :in [$ ?tbl]
                                            :where [[?e :pg/fk-name ?n]
                                                    [?e :pg/fk-child-table ?tbl]
                                                    [?e :pg/fk-child-cols ?cc]
                                                    [?e :pg/fk-parent-table ?pt]
                                                    [?e :pg/fk-parent-cols ?pc]]}
                                          db table-name)]
                            (mapv (fn [{:keys [name child-cols parent-table parent-cols]}]
                                    {:name name
                                     :child-cols (vec (jb/parse-jsonb child-cols))
                                     :parent-table parent-table
                                     :parent-cols (vec (jb/parse-jsonb parent-cols))})
                                  rows)))]
    (if (= ::nil v) [] v)))

(defn- read-fks-referring-to
  "All FK constraints where the given table is the PARENT side — i.e.
   the FKs that must be checked when a row in `table-name` is deleted
   (or its PK is updated). Used by DELETE / UPDATE to enforce
   parent-side RESTRICT and CASCADE actions."
  [db table-name]
  (let [q-fn d/q]
    (map (fn [[n ct cc pc od]]
           {:name n :child-table ct
            :child-cols (vec (jb/parse-jsonb cc))
            :parent-cols (vec (jb/parse-jsonb pc))
            ;; PG default for missing ON DELETE is NO ACTION.
            :on-delete (or od :no-action)})
         (q-fn '{:find [?n ?ct ?cc ?pc ?od]
                 :in [$ ?pt]
                 :where [[?e :pg/fk-name ?n]
                         [?e :pg/fk-parent-table ?pt]
                         [?e :pg/fk-child-table ?ct]
                         [?e :pg/fk-child-cols ?cc]
                         [?e :pg/fk-parent-cols ?pc]
                         [(get-else $ ?e :pg/fk-on-delete :no-action) ?od]]}
               db table-name))))

(defn- enforce-fk-on-insert!
  "For each FK where this table is the child, verify every row in
   `entity-maps` references an existing parent-row. Raises 23503 on
   miss. Called inside apply-column-constraints' :db.fn/call so it
   sees the speculative txdb (if the same tx just inserted the parent,
   the child sees it)."
  [txdb table-name ns entity-maps]
  (let [fks (seq (read-fk-constraints txdb table-name))]
    (when fks
      (let [q-fn d/q]
        (doseq [em entity-maps
                {:keys [name child-cols parent-table parent-cols]} fks
                :let [child-vals (mapv #(get em (keyword ns %)) child-cols)]
                ;; FK is satisfied vacuously when any child-col is NULL
                ;; (PG's MATCH SIMPLE default). Only a fully-specified
                ;; tuple needs to match the parent.
                :when (every? some? child-vals)]
          (let [parent-attrs (mapv #(keyword parent-table %) parent-cols)
                patterns (mapv (fn [attr v] ['?p attr v])
                               parent-attrs child-vals)
                q {:find '[?p]
                   :where patterns}
                hit (ffirst (q-fn q txdb))]
            (when-not hit
              (throw (ex-info "foreign key violation"
                              {:error :foreign-key-violation
                               :table table-name
                               :constraint name
                               :detail (str "Key ("
                                            (str/join ", " child-cols)
                                            ")=("
                                            (str/join ", " (map pr-str child-vals))
                                            ") is not present in table \""
                                            parent-table "\"")})))))))))

(defn- find-fk-children
  "Find child eids that reference any of the parent eids via the given FK.
   Returns the eids as a set, or empty set if none."
  [db table-name fk eids]
  (let [q-fn d/q
        {:keys [child-table child-cols parent-cols]} fk
        parent-attrs (mapv #(keyword table-name %) parent-cols)
        child-attrs (mapv #(keyword child-table %) child-cols)]
    (into #{}
          (mapcat (fn [eid]
                    (let [parent-vals (mapv (fn [a]
                                              (ffirst (q-fn {:find '[?v]
                                                             :in '[$ ?e]
                                                             :where [['?e a '?v]]}
                                                            db eid)))
                                            parent-attrs)]
                      (when (every? some? parent-vals)
                        (let [patterns (mapv (fn [attr v] ['?c attr v])
                                             child-attrs parent-vals)
                              q {:find '[?c] :where patterns}]
                          (mapv first (q-fn q db)))))))
          eids)))

(defn- collect-fk-cascade-retractions!
  "Walk all FKs with this table as parent. For RESTRICT/NO ACTION, raise
   23503 if any child references the deleted eids. For CASCADE, recurse:
   collect the child eids and ALSO walk THEIR children. Returns a set of
   cascade-eids (across all child tables) to retract in the same tx.
   PG-equivalent (commands/trigger.c::RI_FKey_cascade_del). Walks
   iteratively against `db` (pre-transact); since retractEntity is
   atomic, the same tx removes parent + transitively-cascading children."
  [db table-name eids]
  (when (seq eids)
    (loop [pending [[table-name eids]]
           visited #{}                  ;; [table-name eid] pairs already cascaded
           cascade-eids #{}]
      (if (empty? pending)
        cascade-eids
        (let [[t es] (first pending)
              fks (read-fks-referring-to db t)
              ;; Bucket FKs by action.
              by-action (group-by :on-delete fks)
              ;; RESTRICT / NO ACTION: any child = abort.
              restrict-fks (concat (get by-action :no-action)
                                   (get by-action :restrict))]
          ;; Raise on first restrict violation.
          (doseq [{:keys [name parent-cols] :as fk} restrict-fks
                  :let [hits (find-fk-children db t fk es)]
                  :when (seq hits)
                  :let [parent-attrs (mapv #(keyword t %) parent-cols)
                        eid (first es)
                        parent-vals (mapv (fn [a]
                                            (ffirst (d/q
                                                     {:find '[?v]
                                                      :in '[$ ?e]
                                                      :where [['?e a '?v]]}
                                                     db eid)))
                                          parent-attrs)]]
            (throw (ex-info "foreign key still referenced"
                            {:error :foreign-key-violation
                             :table (:child-table fk)
                             :constraint name
                             :detail (str "Key ("
                                          (str/join ", " parent-cols)
                                          ")=("
                                          (str/join ", " (map pr-str parent-vals))
                                          ") is still referenced from table \""
                                          (:child-table fk) "\"")})))
          ;; CASCADE: collect new child eids, recurse on those.
          (let [cascades (get by-action :cascade)
                new-by-table
                (reduce (fn [acc fk]
                          (let [hits (find-fk-children db t fk es)
                                fresh (remove (fn [c]
                                                (contains? visited [(:child-table fk) c]))
                                              hits)]
                            (if (seq fresh)
                              (update acc (:child-table fk) (fnil into #{}) fresh)
                              acc)))
                        {} cascades)
                next-pending (concat (rest pending)
                                     (for [[t' es'] new-by-table] [t' es']))
                next-visited (into visited
                                   (mapcat (fn [[t' es']]
                                             (map vector (repeat t') es')))
                                   new-by-table)
                next-cascade (into cascade-eids
                                   (mapcat val) new-by-table)]
            ;; SET NULL / SET DEFAULT not yet supported — surface clearly.
            (when-let [unsupported (seq (concat (get by-action :set-null)
                                                (get by-action :set-default)))]
              (throw (ex-info "ON DELETE action not implemented"
                              {:error :feature-not-supported
                               :feature (str "ON DELETE "
                                             (name (:on-delete (first unsupported))))
                               :constraint (:name (first unsupported))})))
            (recur next-pending next-visited next-cascade)))))))

(defn- enforce-fk-restrict-on-delete!
  "Compatibility shim — older sites call this. Walks the FK graph and
   either raises 23503 (RESTRICT/NO ACTION) or returns nil (we don't
   add cascade retractions through this path; new callers should use
   collect-fk-cascade-retractions! and append to tx-data)."
  [db table-name eids]
  (collect-fk-cascade-retractions! db table-name eids)
  nil)

(defn- enforce-fk-restrict-on-update!
  "Mirror of enforce-fk-restrict-on-delete!, but for UPDATE: when an
   UPDATE mutates a column that is the parent side of some FK and the
   old value is still referenced by a child row, raise 23503.

   We match on the OLD value: PG's RESTRICT says 'the parent key value
   cannot be changed while children reference it', regardless of what
   the new value is (CASCADE would propagate the new value, but we
   reject CASCADE at DDL). Called with the pre-transact db."
  [db table-name tx-data]
  (let [fks (seq (read-fks-referring-to db table-name))]
    (when fks
      (let [q-fn d/q
            parent-col-attrs (into #{}
                                   (mapcat (fn [{:keys [parent-cols]}]
                                             (map #(keyword table-name %) parent-cols))
                                           fks))
            ;; Collect [eid attr old-val] for each op that rewrites a
            ;; parent-referenced column on a committed row.
            touched (for [op tx-data
                          :when (and (vector? op)
                                     (= :db/add (first op))
                                     (contains? parent-col-attrs (nth op 2)))
                          :let [[_ eid attr _new] op
                                old-val (ffirst (q-fn {:find '[?v]
                                                       :in '[$ ?e]
                                                       :where [['?e attr '?v]]}
                                                      db eid))]
                          :when (some? old-val)]
                      [eid attr old-val])]
        (doseq [[eid _attr _old] touched
                {:keys [name child-table child-cols parent-cols]} fks
                :let [parent-attrs (mapv #(keyword table-name %) parent-cols)
                      parent-vals (mapv (fn [a]
                                          (ffirst (q-fn {:find '[?v]
                                                         :in '[$ ?e]
                                                         :where [['?e a '?v]]}
                                                        db eid)))
                                        parent-attrs)]
                :when (every? some? parent-vals)
                :let [child-attrs (mapv #(keyword child-table %) child-cols)
                      patterns (mapv (fn [a v] ['?c a v]) child-attrs parent-vals)
                      child-hits (q-fn {:find '[?c] :where patterns} db)]
                :when (seq child-hits)]
          (throw (ex-info "foreign key still referenced"
                          {:error :foreign-key-violation
                           :table child-table
                           :constraint name
                           :detail (str "Key ("
                                        (str/join ", " parent-cols)
                                        ")=("
                                        (str/join ", " (map pr-str parent-vals))
                                        ") is still referenced from table \""
                                        child-table "\"")})))))))

(defn- read-column-constraints*
  [db table-name]
  (let [not-null-idents (into #{}
                              (map first)
                              (d/q '{:find [?ident]
                                     :in [$ ?tbl]
                                     :where [[?e :db/ident ?ident]
                                             [?e :pg/not-null true]
                                             [(namespace ?ident) ?ns]
                                             [(= ?ns ?tbl)]]}
                                   db table-name))
        default-rows (d/q '{:find [?ident ?dk]
                            :in [$ ?tbl]
                            :where [[?e :db/ident ?ident]
                                    [?e :pg/default-kind ?dk]
                                    [(namespace ?ident) ?ns]
                                    [(= ?ns ?tbl)]]}
                          db table-name)
        ident->default
        (into {}
              (for [[ident dk] default-rows]
                (let [dv (ffirst (d/q '{:find [?v]
                                        :in [$ ?i]
                                        :where [[?e :db/ident ?i]
                                                [?e :pg/default-value ?v]]}
                                      db ident))
                      da (ffirst (d/q '{:find [?a]
                                        :in [$ ?i]
                                        :where [[?e :db/ident ?i]
                                                [?e :pg/default-arg ?a]]}
                                      db ident))]
                  [ident [dk dv da]])))
        all-idents (into not-null-idents (keys ident->default))]
    (into {}
          (for [ident all-idents]
            [(name ident)
             {:attr ident
              :not-null? (contains? not-null-idents ident)
              :default (get ident->default ident)}]))))

(defn- read-column-constraints
  "Return {col-name {:attr ident :not-null? bool :default [kind value arg]}}
   for the columns of `table-name`, read from the schema entity's custom
   attrs. Memoised per (schema, table) — used to fire 2-3 d/q calls
   per INSERT against immutable schema metadata, ~0.6 ms/row of pure
   waste on the Pagila pg_dump replay."
  [db table-name]
  (let [v (schema-cached db [::col table-name]
                         #(read-column-constraints* db table-name))]
    (if (= ::nil v) {} v)))

(defn- apply-column-constraints
  "Wrap an INSERT tx-data vector in a :db.fn/call that validates
   every incoming entity against the table's registered constraints:

     1. Materialize DEFAULT for columns the user omitted.
     2. Raise 23502 if a :pg/not-null column is still nil.
     3. Evaluate each CHECK expression against the final entity map;
        raise 23514 on violation.
     4. For each FK where this table is the child, verify the
        referenced parent row exists; raise 23503 otherwise.

   The tx-fn runs inside Datahike's transactor against the
   speculative txdb, so CHECK / FK lookups see rows inserted earlier
   in the same transaction — matching PG's per-row deferrability
   default.

   Non-map items (retract ops, prior :db.fn/call wrappers) pass
   through unchanged. We run this AFTER auto-populate-identity so
   IDENTITY sequences fire before the null check sees the entity —
   otherwise an implicit SERIAL column would look null here."
  [tx-data table-name ns db]
  (let [cols (read-column-constraints db table-name)
        has-checks? (seq (read-check-constraints db table-name))
        has-fks? (seq (read-fk-constraints db table-name))
        has-domain-enum? (seq (read-domain-enum-checks db table-name))]
    (if (and (empty? cols) (not has-checks?) (not has-fks?) (not has-domain-enum?))
      tx-data
      [[:db.fn/call
        (fn [txdb]
          (let [q-fn d/q
                bump-seq! (fn [seq-name]
                            ;; Mirror the behavior in auto-populate-identity:
                            ;; find the sequence entity, compute next value,
                            ;; return [new-value [:db/add eid :__seq__/value v]].
                            (let [eid (ffirst (q-fn '{:find [?e]
                                                      :where [[?e :__seq__/name ?n]]
                                                      :in [$ ?n]}
                                                    txdb seq-name))
                                  curr (when eid
                                         (ffirst (q-fn '{:find [?v]
                                                         :where [[?e :__seq__/value ?v]]
                                                         :in [$ ?e]}
                                                       txdb eid)))
                                  incr (when eid
                                         (or (ffirst (q-fn '{:find [?i]
                                                             :where [[?e :__seq__/increment ?i]]
                                                             :in [$ ?e]}
                                                           txdb eid))
                                             1))
                                  nxt (when eid (+ (or curr 0) incr))]
                              (when eid
                                [nxt [:db/add eid :__seq__/value nxt]])))
                result
                (reduce
                 (fn [acc entry]
                   (if-not (map? entry)
                     (conj acc entry)
                     (let [{:keys [filled seq-ops]}
                           (reduce-kv
                            (fn [st col-name {:keys [attr not-null? default]}]
                              (let [ns-attr (keyword ns col-name)
                                    legacy (keyword (namespace attr) col-name)
                                    present-key (some #(when (contains? (:filled st) %) %)
                                                      [ns-attr attr legacy])]
                                (cond
                                  present-key
                                  (let [v (get-in st [:filled present-key])]
                                    (if (and not-null? (nil? v))
                                      (throw (ex-info "not-null violation"
                                                      {:error  :not-null-violation
                                                       :table  table-name
                                                       :column col-name}))
                                      st))

                                  default
                                  (let [[kind value arg] default
                                        v (eval-default kind value arg)]
                                    (cond
                                      (and (vector? v) (= ::nextval (first v)))
                                      (let [[nxt seq-tx] (bump-seq! (second v))]
                                        (if nxt
                                          (-> st
                                              (assoc-in [:filled ns-attr] nxt)
                                              (update :seq-ops conj seq-tx))
                                          (if not-null?
                                            (throw (ex-info "not-null violation"
                                                            {:error  :not-null-violation
                                                             :table  table-name
                                                             :column col-name}))
                                            st)))

                                      (nil? v)
                                      (if not-null?
                                        (throw (ex-info "not-null violation"
                                                        {:error  :not-null-violation
                                                         :table  table-name
                                                         :column col-name}))
                                        st)

                                      :else
                                      (assoc-in st [:filled ns-attr] v)))

                                  not-null?
                                  (throw (ex-info "not-null violation"
                                                  {:error  :not-null-violation
                                                   :table  table-name
                                                   :column col-name}))

                                  :else st)))
                            {:filled entry :seq-ops []}
                            cols)]
                       (into (conj acc filled) seq-ops))))
                 []
                 tx-data)
                ;; Second pass — CHECK + FK enforcement sees the final
                ;; entity maps (post-default, post-identity). Only map
                ;; entries count as rows; :db/add tuples from sequence
                ;; bumps etc. don't.
                filled-entities (filterv map? result)]
            (when has-checks?
              (doseq [em filled-entities]
                (enforce-check-constraints! txdb table-name ns em)))
            (when has-domain-enum?
              (enforce-domain-enum-checks! txdb table-name ns filled-entities))
            (when has-fks?
              (enforce-fk-on-insert! txdb table-name ns filled-entities))
            result))]])))

(defn- execute-insert [conn parsed & {:keys [tx-wrap] :or {tx-wrap identity}}]
  (try
    (let [table-name (:table parsed)
          db (d/db conn)
          tx-data (-> (:tx-data parsed)
                      (auto-populate-identity table-name db)
                      (apply-column-constraints table-name (:ns parsed) db)
                      tx-wrap)
          tx-report (d/transact conn tx-data)]
      (if-let [returning (:returning parsed)]
        ;; RETURNING: resolve row refs in VALUES order — either from
        ;; :row-refs atom (ON CONFLICT) or :db/id tempids on entity maps.
        (let [tempids (:tempids tx-report)
              db (:db-after tx-report)
              schema (dbi/-schema db)
              ns-prefix (str table-name "/")
              has-row? (fn [eid]
                         (some (fn [^datahike.datom.Datom d]
                                 (let [a (.-a d)]
                                   (and (keyword? a)
                                        (.startsWith (str (namespace a) "/") ns-prefix)
                                        (not= (name a) "db-row-exists"))))
                               (d/datoms db :eavt eid)))
              ordered-refs (if-let [refs (:row-refs parsed)]
                             @refs
                             (keep #(when (and (map? %) (string? (:db/id %)))
                                      (:db/id %))
                                   (:tx-data parsed)))
              ordered-eids (vec (keep (fn [ref]
                                        (cond
                                          (integer? ref) ref
                                          (string? ref) (get tempids ref)
                                          :else nil))
                                      ordered-refs))
              data-eids (if (seq ordered-eids)
                          (filterv has-row? ordered-eids)
                          (filterv has-row? (vals tempids)))]
          (build-returning-result returning db data-eids table-name schema))
        (empty-result (str "INSERT 0 " (:count parsed)))))
    (catch Exception e
      (classified-error "INSERT error: " e))))

(defn- build-delete-tx
  "Build entity IDs and tx-data for a DELETE against `db`.
   Returns {:eids [...] :tx-data [...]} using real entity IDs — callers
   that need speculative tempid remapping should remap afterward.

   When `parsed` carries an `:enriched-db` (CTEs were materialised at
   parse-sql time), the WHERE-clause query runs against it and the
   ctx sees its extended schema so virtual `:<cte>/<col>` attrs
   resolve. Returned eids are still real entity-ids in the live db
   because the speculative overlay never reassigns ids for existing
   entities."
  [db schema parsed]
  (let [{:keys [table alias where-expr enriched-db]} parsed
        query-db (or enriched-db db)
        query-schema (or (:schema enriched-db) schema)
        ;; Build alias map: {alias → table, table → table}
        default-key (or alias table)
        table-aliases (cond-> {table table}
                        alias (assoc alias table))
        ctx (#'sql/make-ctx query-schema table-aliases default-key
                            {:db query-db :parse-sql sql/parse-sql})
        _ (when where-expr
            (let [preds (#'sql/translate-predicate ctx where-expr)]
              (swap! (:where-clauses ctx) into preds)))
        evar (#'sql/entity-var! ctx default-key)
        _ (when (empty? @(:where-clauses ctx))
            (let [cols (pgs/column-info schema table)]
              (when-let [first-col (second cols)]
                (#'sql/col-var! ctx (:attr first-col)))))
        q {:find [evar] :where (vec @(:where-clauses ctx))}
        eids (mapv first (d/q q query-db))]
    {:eids eids
     :tx-data (mapv (fn [eid] [:db/retractEntity eid]) eids)}))

(defn- execute-delete [conn parsed schema & {:keys [tx-wrap] :or {tx-wrap identity}}]
  (try
    (let [{:keys [table]} parsed
          db (d/db conn)
          {:keys [eids tx-data]} (build-delete-tx db schema parsed)
          ;; For RETURNING, snapshot values BEFORE delete
          returning (:returning parsed)
          returning-result (when returning
                             (build-returning-result returning db eids table schema))
          ;; FK enforcement — RESTRICT raises, CASCADE returns extra eids
          ;; to retract atomically alongside the parent deletion.
          cascade-eids (collect-fk-cascade-retractions! db table eids)
          cascade-tx (when (seq cascade-eids)
                       (mapv (fn [e] [:db/retractEntity e]) cascade-eids))
          full-tx (cond-> (vec tx-data) (seq cascade-tx) (into cascade-tx))
          full-tx (tx-wrap full-tx)]
      (when (seq full-tx)
        (d/transact conn full-tx))
      (or returning-result
          (empty-result (str "DELETE " (count eids)))))
    (catch Exception e
      (classified-error "DELETE error: " e))))

;; Forward-declare so build-update-tx-for-bindings (below) can read the
;; prepared-statement param vector; the binding site is in executePrepared
;; further down, after the handler closure setup.
(declare ^:dynamic *cached-bound*)

(defn- build-update-tx-for-bindings
  "Build tx-data for a single UPDATE row, optionally with from-bindings for
   UPDATE ... FROM (VALUES ...) substitution. Returns {:eids [...] :tx-data [...]}.

   When `parsed` carries `:enriched-db` (CTEs materialised at parse-sql
   time), the row-matching query and translate-predicate ctx use that
   db/schema so virtual `:<cte>/<col>` attrs resolve. Resulting eids
   are still real entity-ids in the live db."
  [db schema parsed from-bindings]
  (let [{:keys [table alias ns assignments where-expr enriched-db]} parsed
        query-db (or enriched-db db)
        query-schema (or (:schema enriched-db) schema)
        default-key (or alias table)
        table-aliases (cond-> {table table}
                        alias (assoc alias table))
        ctx (#'sql/make-ctx query-schema table-aliases default-key
                            {:db query-db :parse-sql sql/parse-sql})
        _ (when where-expr
            (binding [params/*from-bindings* from-bindings]
              (let [preds (#'sql/translate-predicate ctx where-expr)]
                (swap! (:where-clauses ctx) into preds))))
        evar (#'sql/entity-var! ctx default-key)
        _ (when (empty? @(:where-clauses ctx))
            (let [cols (pgs/column-info schema table)]
              (when-let [first-col (second cols)]
                (#'sql/col-var! ctx (:attr first-col)))))
        where-clauses @(:where-clauses ctx)
        ;; Prepared-statement UPDATE: translate-predicate lifts every
        ;; JdbcParameter to a logic variable (e.g. `?p2`) and records a
        ;; ParamRef in :in-args. Without plumbing :in/:in-args through
        ;; to d/q, the row-matching query has an unbound var and
        ;; returns zero rows (manifesting as "UPDATE 0" for a row that
        ;; clearly exists). Substitute the ParamRefs against
        ;; *cached-bound* here so the d/q call has concrete literals.
        in-params @(:in-params ctx)
        in-args-raw @(:in-args ctx)
        in-args (if-let [bound *cached-bound*]
                  (sql/substitute-params in-args-raw
                                         (fn [idx] (nth bound idx)))
                  in-args-raw)
        q (cond-> {:find [evar] :where (vec where-clauses)}
            (seq in-params) (assoc :in (into ['$] in-params)))
        eids (mapv first
                   (if (seq in-args)
                     (apply d/q q query-db in-args)
                     (d/q q query-db)))
        ;; For prepared UPDATE, resolve ParamRef values BEFORE
        ;; coerce-insert-value — otherwise the coercion fires on a
        ;; placeholder record (no-op passthrough), then the bound
        ;; literal lands in tx-data untyped, and Datahike rejects e.g.
        ;; "2014-12-23 -08" against a :db.type/instant column.
        resolve-param (if-let [bound *cached-bound*]
                        (fn [v]
                          (if (sql/param-ref? v)
                            (nth bound (:idx v))
                            v))
                        identity)
        ;; Build {outer-alias-or-table → {col-string → value}} for each
        ;; eid so SET assignments containing scalar subqueries with
        ;; correlated outer-row references (Odoo's _parent_store_create
        ;; UPDATE: `concat((SELECT … WHERE id = node.parent_id), node.id,
        ;; '/')`) can resolve those references. The binding rides on
        ;; *from-bindings*, which expr.clj's Column branch already
        ;; consults for UPDATE…FROM(VALUES) bindings; we extend the
        ;; same map with the outer row keyed by alias-or-table.
        outer-key (or alias table)
        tx-data (vec (keep identity
                           (mapcat
                            (fn [eid]
                              (let [entity-map (into {} (map (fn [^datahike.datom.Datom d]
                                                               [(.-a d) (.-v d)])
                                                             (d/datoms db :eavt eid)))
                                    outer-binding (when outer-key
                                                    {outer-key
                                                     (into {}
                                                           (map (fn [[k v]] [(name k) v]))
                                                           entity-map)})
                                    eff-from-bindings (merge from-bindings outer-binding)]
                                (for [{:keys [column value-expr]} assignments
                                      :let [attr (keyword ns column)
                                            raw-val (binding [params/*from-bindings* eff-from-bindings
                                                              stmt/*eval-update-db* db]
                                                      (sql/eval-update-expr value-expr entity-map ns schema))
                                            resolved (resolve-param raw-val)
                                            val (when (some? resolved)
                                                  (#'sql/coerce-insert-value resolved attr schema))
                                            old-val (get entity-map attr)]]
                                  (if (nil? val)
                                ;; SET col = NULL → retract the attribute.
                                ;; Skip for tempid entities (no prior values to retract;
                                ;; Datahike rejects tempids in :db/retract).
                                    (when (and old-val (integer? eid))
                                      [:db/retract eid attr old-val])
                                    [:db/add eid attr val]))))
                            eids)))]
    {:eids eids :tx-data tx-data}))

(defn- build-update-tx
  "Build entity IDs and tx-data for an UPDATE against `db`.
   Returns {:eids [...] :tx-data [...]} using real entity IDs — callers
   that need speculative tempid remapping should pass an `eid->tempid` map
   and remap the tx-data afterward.

   For UPDATE ... FROM (VALUES ...) AS alias(cols), runs one update per
   VALUES row with the alias's columns bound to that row's literals."
  [db schema parsed]
  (if-let [{:keys [alias cols rows]} (:from-values parsed)]
    (reduce
     (fn [acc row]
       (let [binding-map {alias (zipmap cols row)}
             {:keys [eids tx-data]} (build-update-tx-for-bindings
                                     db schema parsed binding-map)]
         (-> acc
             (update :eids into eids)
             (update :tx-data into tx-data))))
     {:eids [] :tx-data []}
     rows)
    (build-update-tx-for-bindings db schema parsed nil)))

(defn- check-update-identity-collisions!
  "Pre-flight check: before running tx-data from build-update-tx, scan
   every `[:db/add eid a v]` whose attribute is `:db.unique/identity`.
   If any of those values already exist on a DIFFERENT entity, raise
   23505 — matching PG's `UPDATE t SET id=N WHERE ...` semantics when
   another row already has id=N.

   Without this, Datahike's writer silently mutates the identity value
   (identity is an upsert key, not a hard constraint), so the SQL
   client loses the 23505 signal their ORM branches on.

   Also catches intra-statement self-collisions (UPDATE t SET id = 5
   against two rows where both get id=5) via a per-tx `seen` set
   keyed by [attr val]."
  [db schema tx-data]
  (let [seen (volatile! {})]
    (doseq [op tx-data
            :when (and (vector? op) (= :db/add (first op)))
            :let  [[_ eid a v] op
                   schema-entry (get schema a)]
            :when (and (some? v)
                       (map? schema-entry)
                       (= :db.unique/identity (:db/unique schema-entry))
                       (not= :db.type/tuple   (:db/valueType schema-entry)))]
      (when-let [existing (ffirst (d/q '{:find [?e]
                                         :in [$ ?a ?v]
                                         :where [[?e ?a ?v]]}
                                       db a v))]
        (when-not (= existing eid)
          (throw (ex-info "unique violation"
                          {:error      :unique-violation
                           :table      (namespace a)
                           :column     (name a)
                           :constraint (str (namespace a) "_pkey")
                           :value      v
                           :datahike/collision [a v]}))))
      (when (contains? (get @seen a) v)
        (throw (ex-info "unique violation"
                        {:error      :unique-violation
                         :table      (namespace a)
                         :column     (name a)
                         :constraint (str (namespace a) "_pkey")
                         :value      v
                         :datahike/collision [a v]})))
      (vswap! seen update a (fnil conj #{}) v))))

(defn- check-not-null-on-update!
  "For every [:db/add eid attr nil] or [:db/retract eid attr …] op in
   an UPDATE's tx-data, raise 23502 if attr carries :pg/not-null.
   A retract of a non-null attr is equivalent to setting it to NULL,
   which PG rejects."
  [db tx-data]
  (let [q-fn d/q
        not-null-attrs (into #{}
                             (map first)
                             (q-fn '{:find [?ident]
                                     :where [[?e :db/ident ?ident]
                                             [?e :pg/not-null true]]}
                                   db))]
    (doseq [op tx-data]
      (when (vector? op)
        (let [[verb _eid attr val] op]
          (when (and (contains? not-null-attrs attr)
                     (or (= verb :db/retract)
                         (and (= verb :db/add) (nil? val))))
            (throw (ex-info "not-null violation"
                            {:error  :not-null-violation
                             :table  (namespace attr)
                             :column (name attr)}))))))))

(defn- check-updates-against-row-constraints!
  "For every eid touched by an UPDATE's tx-data, reconstruct the
   post-state entity map (current row + pending :db/add / :db/retract
   ops) and run it through enforce-check-constraints! and the child-
   side FK check. Mirrors PG's AFTER-trigger timing: the full row is
   validated once the UPDATE's assignments have been applied, not
   per-op.

   Called from execute-update against the pre-transact db; the
   speculative change isn't committed yet, so lookups (FK parent
   existence) see only committed data. That's fine — PG does the
   same for non-deferred constraints."
  [db table-name ns tx-data]
  (let [checks? (seq (read-check-constraints db table-name))
        fks?    (seq (read-fk-constraints db table-name))]
    (when (or checks? fks?)
      (let [;; Group ops by eid. Map form (for INSERT-in-UPDATE? rare)
            ;; pass through untouched.
            ops-by-eid (reduce (fn [acc op]
                                 (if (and (vector? op) (keyword? (first op)))
                                   (let [[verb eid attr val] op]
                                     (update acc eid (fnil conj []) [verb attr val]))
                                   acc))
                               {}
                               tx-data)]
        (doseq [[eid ops] ops-by-eid
                :when (integer? eid)
                :let [base (into {} (map (fn [^datahike.datom.Datom d]
                                           [(.-a d) (.-v d)]))
                                 (d/datoms db :eavt eid))
                      post (reduce (fn [m [verb attr val]]
                                     (case verb
                                       :db/add (assoc m attr val)
                                       :db/retract (dissoc m attr)
                                       m))
                                   base ops)]]
          (when checks?
            (enforce-check-constraints! db table-name ns post))
          (when fks?
            (enforce-fk-on-insert! db table-name ns [post])))))))

(defn- execute-update [conn parsed schema & {:keys [tx-wrap] :or {tx-wrap identity}}]
  (try
    (let [{:keys [table]} parsed
          db (d/db conn)
          {:keys [eids tx-data]} (build-update-tx db schema parsed)
          _ (check-update-identity-collisions! db schema tx-data)
          _ (check-not-null-on-update! db tx-data)
          _ (check-updates-against-row-constraints!
             db table (or (:ns parsed) table) tx-data)
          _ (enforce-fk-restrict-on-update! db table tx-data)
          tx-data (tx-wrap tx-data)
          tx-report (when (seq tx-data) (d/transact conn tx-data))
          returning (:returning parsed)]
      (if returning
        ;; RETURNING: read values from db-after
        (let [db-after (if tx-report (:db-after tx-report) db)]
          (build-returning-result returning db-after eids table (:schema db-after)))
        (empty-result (str "UPDATE " (count eids)))))
    (catch Exception e
      (classified-error "UPDATE error: " e))))

(defn- build-update-with-recursive-tx
  "Run the recursive CTE rule, then for each result row look up the target entity
   and produce :db/add tx-data for the SET mappings.
   Returns {:eids [...] :tx-data [...]}."
  [db parsed]
  (let [{:keys [table ns cte set-mappings join-info]} parsed
        {:keys [rule rule-name col-names rule-vars in-params in-args]} cte
        ;; Identify CTE-side and table-side of the join.
        ;; join-info has {:l-tbl :l-col :r-tbl :r-col} with column refs.
        cte-name (str rule-name)
        {:keys [l-tbl l-col r-tbl r-col]} join-info
        col-name-set (set col-names)
        ;; CTE side: matches the CTE alias/name OR has a col-name that's a CTE column
        cte-join-col (cond
                       (= cte-name l-tbl) l-col
                       (= cte-name r-tbl) r-col
                       (= table l-tbl) r-col
                       (= table r-tbl) l-col
                       (contains? col-name-set l-col) l-col
                       :else r-col)
        target-join-col (if (= cte-join-col l-col) r-col l-col)
        cte-join-idx (.indexOf ^java.util.List col-names cte-join-col)
        ;; Run the rule
        find-clause (apply vector :find rule-vars)
        rule-call (apply list rule-name rule-vars)
        in-clause (into '[$ %] in-params)
        q {:find rule-vars
           :in in-clause
           :where [rule-call]}
        rows (apply d/q q db rule in-args)
        ;; Build a lookup map: cte_join_value → {target_col → value}
        join-attr (keyword ns target-join-col)
        rows-by-join (into {}
                           (for [row rows]
                             [(nth row cte-join-idx)
                              (into {} (for [{:keys [target-col cte-col]} set-mappings
                                             :let [idx (.indexOf ^java.util.List col-names cte-col)]
                                             :when (>= idx 0)]
                                         [(keyword ns target-col) (nth row idx)]))]))
        ;; Find target entities by their join attribute
        eid+val (vec (d/q '[:find ?e ?v
                            :in $ ?attr [?v ...]
                            :where [?e ?attr ?v]]
                          db join-attr (vec (keys rows-by-join))))
        tx-data (vec (mapcat (fn [[eid join-val]]
                               (let [updates (get rows-by-join join-val)]
                                 (for [[attr v] updates
                                       :when (some? v)]
                                   [:db/add eid attr v])))
                             eid+val))]
    {:eids (mapv first eid+val) :tx-data tx-data}))

(defn- execute-update-with-recursive [conn parsed]
  (try
    (let [db (d/db conn)
          {:keys [eids tx-data]} (build-update-with-recursive-tx db parsed)]
      (when (seq tx-data) (d/transact conn tx-data))
      (empty-result (str "UPDATE " (count eids))))
    (catch Exception e
      (classified-error "UPDATE (WITH RECURSIVE) error: " e))))

;; ============================================================================
;; DDL execution
;; ============================================================================

(defn- ddl-tx-data
  "Collect all tx-data for a DDL CREATE, including inheritance metadata."
  [parsed]
  (let [base-tx (:tx-data parsed)
        inherit-tx (when-let [parent (:inherits parsed)]
                     (let [child (:table-name parsed)]
                       [{:db/ident :__inherit__/child :db/valueType :db.type/string
                         :db/cardinality :db.cardinality/one}
                        {:db/ident :__inherit__/parent :db/valueType :db.type/string
                         :db/cardinality :db.cardinality/one}
                        {:__inherit__/child child :__inherit__/parent parent}]))]
    (into (vec base-tx) inherit-tx)))

(defn- ensure-pg-schema!
  "Install pgwire-internal schema attributes that describe PG-side facts
   attached to Datahike schema entities.

   Column-level (attached to each user column's ident entity):
     :pg/type          — original SQL type name when Datahike's valueType
                         is a reduction (date/time/timestamp → instant;
                         jsonb/json → string).
     :pg/table-oid     — only on row-marker attrs; stable per-table OID
                         populating pg_class / pg_attribute / pg_index.
     :pg/not-null      — enforced at INSERT/UPDATE. PG error 23502.
     :pg/default-kind  — :literal | :fn | :nextval. Consumed by
                         INSERT-time default materialization.
     :pg/default-value — string form of the literal or function name.
     :pg/default-arg   — argument for :nextval (sequence name).

   Constraint entities (separate entities keyed by name):
     :pg/check-name      (unique/identity) + :pg/check-table +
     :pg/check-expr      — serialized CHECK expression. Evaluated
                           per-row at INSERT/UPDATE. PG error 23514.
     :pg/fk-name         (unique/identity) + :pg/fk-child-table +
     :pg/fk-child-cols   + :pg/fk-parent-table + :pg/fk-parent-cols +
     :pg/fk-on-delete / :pg/fk-on-update — child-side reference check
                           on INSERT/UPDATE; parent-side RESTRICT on
                           DELETE/UPDATE. PG error 23503.

   Column lists on FK entities are encoded as JSON-style strings
   (\"[\\\"a\\\",\\\"b\\\"]\") to keep ordering without introducing
   ordered-collection schema machinery. Persists with the user schema
   (file/kv backends keep constraints across restarts; :memory dies
   with the db — matches PG's catalog semantics)."
  [conn]
  (let [db (d/db conn)
        schema (dbi/-schema db)
        long1 {:db/valueType :db.type/long :db/cardinality :db.cardinality/one}
        str1  {:db/valueType :db.type/string :db/cardinality :db.cardinality/one}
        bool1 {:db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
        kw1   {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
        spec [[:pg/type str1]
              [:pg/table-oid long1]
              [:pg/not-null bool1]
              ;; PG-style atttypmod — encodes NUMERIC(p, s) precision +
              ;; scale, plus length for VARCHAR(n) etc. Stored as a long;
              ;; -1 means unconstrained (PG default).
              [:pg/typmod long1]
              ;; Native PG array column metadata (Option C storage):
              ;; element-keyword (:int4 / :text / …) drives from-pg-text
              ;; reconstruction; ndim records dimensionality so the
              ;; codec can validate shape and the OID inferer reports
              ;; the correct array OID (`pg/type "_T"` resolves it).
              [:pg/array-elem kw1]
              [:pg/array-ndim long1]
              [:pg/default-kind kw1]
              [:pg/default-value str1]
              [:pg/default-arg str1]
              ;; CHECK constraints — one entity per constraint. Name
              ;; is :db.unique/identity so repeated CREATE TABLE IF NOT
              ;; EXISTS on the same DDL doesn't duplicate.
              [:pg/check-name (assoc str1 :db/unique :db.unique/identity)]
              [:pg/check-table str1]
              [:pg/check-expr str1]
              ;; FOREIGN KEY constraints — one entity per FK.
              [:pg/fk-name (assoc str1 :db/unique :db.unique/identity)]
              [:pg/fk-child-table str1]
              [:pg/fk-child-cols str1]
              [:pg/fk-parent-table str1]
              [:pg/fk-parent-cols str1]
              [:pg/fk-on-delete kw1]
              [:pg/fk-on-update kw1]]
        missing (into []
                      (keep (fn [[ident tmpl]]
                              (when-not (get schema ident)
                                (assoc tmpl :db/ident ident))))
                      spec)]
    (when (seq missing)
      (d/transact conn missing))
    ;; User-facing hint attrs (:datahike.pg/*) installed via schema.clj's
    ;; own helper — keeps the hint schema definition colocated with its
    ;; consumers and lets bare-conn callers (no server) prime it by
    ;; calling pgs/ensure-hint-schema! directly.
    (pgs/ensure-hint-schema! conn)))

(defn- table-exists?
  "True when the given table name is already known to `db` — detected
   via the presence of its row-marker attribute, which every pgwire-
   created table carries. More reliable than scanning for individual
   column attrs, which may have been ALTER TABLE-added separately."
  [db table-name]
  (let [marker (pgs/row-marker-attr table-name)]
    (boolean (get (dbi/-schema db) marker))))

(defn- execute-ddl-in-tx
  "Execute DDL inside a transaction by applying schema changes to the
   speculative-db and adding to tx-buffer. This preserves uncommitted
   DML data that would be lost if we committed directly.
   Filters out schema attributes that already exist — ALTER TABLE ADD
   COLUMN and CREATE TABLE IF NOT EXISTS both rely on this."
  [tx-state tx-data command-tag]
  (try
    (let [spec-db (:speculative-db @tx-state)
          schema (:schema spec-db)
          ;; Filter out schema attributes that already exist
          ;; (CREATE TABLE on an existing table should be idempotent)
          new-tx-data (vec (remove (fn [datum]
                                     (and (map? datum)
                                          (:db/ident datum)
                                          (get schema (:db/ident datum))))
                                   tx-data))]
      (when (seq new-tx-data)
        (let [spec-report (dc/with spec-db new-tx-data)]
          (swap! tx-state (fn [st]
                            (-> st
                                (assoc :speculative-db (:db-after spec-report))
                                (update :tx-buffer into new-tx-data)))))))
    (empty-result command-tag)
    (catch Exception e
      (classified-error (str command-tag " error: ") e))))

(defn- execute-ddl-create [conn parsed tx-state]
  (let [tx-data (ddl-tx-data parsed)
        table-name (:table-name parsed)
        if-not-exists? (:if-not-exists? parsed)
        current-db (if (:in-tx? @tx-state)
                     (:speculative-db @tx-state)
                     (d/db conn))]
    (cond
      ;; CREATE TABLE on an existing table. PG raises 42P07
      ;; duplicate_table; IF NOT EXISTS downgrades it to a notice +
      ;; success. Before this check, collisions were silently
      ;; idempotent, which masked Hibernate/Flyway schema-drift bugs
      ;; (postgres.c: commands/tablecmds.c heap_create_with_catalog).
      (and table-name (table-exists? current-db table-name) (not if-not-exists?))
      (classified-error ""
                        (ex-info (str "relation \"" table-name "\" already exists")
                                 {:sqlstate "42P07"
                                  :table table-name
                                  :constraint table-name}))

      (:in-tx? @tx-state)
      (execute-ddl-in-tx tx-state tx-data "CREATE TABLE")

      :else
      (try
        (d/transact conn tx-data)
        (empty-result "CREATE TABLE")
        (catch Exception e
          (classified-error "CREATE TABLE error: " e))))))

;; ============================================================================
;; Query handler — the main dispatch
;; ============================================================================

(defn- parse-temporal-set
  "Parse `SET datahike.{as_of,system_at,since,history,branch,commit_id,
   valid_at,valid_from,valid_to} = '…'` and their RESET forms. Returns
   [key value] where key is :as-of | :since | :history | :branch |
   :commit-id | :valid-at | :valid-from | :valid-to and value is the
   string (or nil for reset/clear), or nil if the SQL isn't a
   recognized session-var op.

   `datahike.system_at` is a SQL:2011-compliant alias for
   `datahike.as_of` — both pin tx-time via `d/as-of`.

   Implemented on top of datahike.pg.sql.classify — the classifier gives
   us {:kind :set :var \"…\" :value \"…\"} or {:kind :reset :var \"…\"}
   without any regex of our own."
  [^String sql]
  (let [{:keys [kind var value]} (cls/classify sql)
        key (case var
              "datahike.as_of"      :as-of
              "datahike.system_at"  :as-of
              "datahike.since"      :since
              "datahike.history"    :history
              "datahike.branch"     :branch
              "datahike.commit_id"  :commit-id
              "datahike.valid_at"   :valid-at
              "datahike.valid_from" :valid-from
              "datahike.valid_to"   :valid-to
              nil)]
    (cond
      (nil? key) nil
      (= kind :reset) [key nil]
      (= kind :set)
      (let [v (or value "")]
        (cond
          ;; SET x = '' clears
          (= v "") [key nil]
          (= key :history)
          (cond (contains? #{"true" "on"} (str/lower-case v)) [key "true"]
                (contains? #{"false" "off"} (str/lower-case v)) [key nil]
                :else nil)
          :else [key v])))))

(defn- parse-statement-timeout-value
  "Decode the value portion of `SET statement_timeout`. Accepts integer
   ms (`5000`), suffixed time units (`'5s'`, `'250ms'`, `'2min'` —
   typically quoted), or the bareword `default` (returns 0 = disabled).
   Returns ms as a long, or nil if unparseable."
  [^String raw]
  (let [v (str/lower-case raw)]
    (cond
      (= v "default") 0
      (re-matches #"\d+" v) (Long/parseLong v)
      (re-matches #"(\d+)ms" v)  (Long/parseLong (second (re-matches #"(\d+)ms" v)))
      (re-matches #"(\d+)s" v)   (* 1000   (Long/parseLong (second (re-matches #"(\d+)s" v))))
      (re-matches #"(\d+)min" v) (* 60000  (Long/parseLong (second (re-matches #"(\d+)min" v))))
      :else nil)))

(defn- parse-statement-timeout
  "Recognise `SET statement_timeout = …` / `SET LOCAL statement_timeout
   TO …` / `RESET statement_timeout`. Returns the timeout in
   milliseconds (0 disables), or nil if the SQL is not a
   statement_timeout op.

   Implemented on top of cls/classify so the matcher cannot fire on a
   string literal that happens to contain the phrase
   `set statement_timeout`."
  [^String sql]
  (let [{:keys [kind var value]} (cls/classify sql)]
    (when (= "statement_timeout" var)
      (cond
        (= kind :reset) 0
        (= kind :set)   (when value (parse-statement-timeout-value value))))))

(defn- apply-temporal
  "Apply temporal + branch wrappers to a database based on session state.

   Precedence: commit-id wins over branch (pinning to a specific commit
   is a stronger assertion than \"head of some branch\"). Both are
   applied before the time-slice wrappers (as-of / since / history) so
   a client can, e.g., `SET datahike.branch = 'feature'` and then
   `SET datahike.as_of = '…'` to view that branch at a past point.

   `valid-at` is a separate (valid-time) axis routed through vt-aware
   secondary indices. It's composable with the tx-time axes — e.g.,
   `SET datahike.as_of = ... ; SET datahike.valid_at = ...` views the
   db at the tx-time of the first and filters secondary-index reads
   by the second.

   `branch` / `commit-id` are looked up via datahike.versioning's
   branch-as-db / commit-as-db. The input `db` param is the caller's
   starting point (usually `(d/db conn)`); when branch or commit-id is
   bound, we ignore that and read from the store instead.

   `per-stmt-override` (optional) is a map with the same shape as the
   relevant session-state keys, set by the SQL preprocessor when a
   statement carries an inline `FOR VALID_TIME …` or `FOR SYSTEM_TIME …`
   clause. Override values shadow session-state for the duration of
   this call only; session-state is unmodified. Supported override
   keys:
     `:valid-at <Date>`       — single-point pin (equivalent to
                                a session `SET datahike.valid_at`).
     `:valid-at :all`         — clear any session-scoped pin
                                (equivalent to `RESET datahike.valid_at`
                                for this statement).
     `:valid-between [a b]`   — interval pin (`d/valid-between`).
     `:as-of <Date>`          — tx-time pin (equivalent to a session
                                `SET datahike.as_of` / `system_at`).
     `:as-of :all`            — clear any session-scoped as-of
                                (equivalent to `RESET datahike.as_of`
                                for this statement)."
  ([db session-state]
   (apply-temporal db session-state nil))
  ([db session-state per-stmt-override]
   (let [base-state @session-state
         ;; Per-statement override shadows session-state. Special case:
         ;; `:{valid-at,as-of} :all` explicitly clears the marker for
         ;; this stmt.
         effective (cond-> base-state
                     per-stmt-override
                     (merge per-stmt-override)
                     (= :all (:valid-at per-stmt-override))
                     (dissoc :valid-at)
                     (= :all (:as-of per-stmt-override))
                     (dissoc :as-of))
         {:keys [as-of since history branch commit-id valid-at valid-between]}
         effective
         base (cond
                commit-id (versioning/commit-as-db (:store db) commit-id)
                branch    (versioning/branch-as-db (:store db) branch)
                :else     db)]
     (cond-> base
       history       (d/history)
       as-of         (d/as-of as-of)
       since         (d/since since)
       valid-at      (d/valid-at valid-at)
       valid-between (d/valid-between (first valid-between) (second valid-between))))))

(defn- parse-instant
  "Parse a timestamp string to a java.util.Date for temporal queries.
   Supports ISO format and tx-id (long)."
  [^String s]
  (try
    (if (re-matches #"\d+" s)
      ;; Transaction ID
      (Long/parseLong s)
      ;; ISO timestamp
      (java.util.Date/from (java.time.Instant/parse s)))
    (catch Exception _
      ;; Try simpler format: 2024-01-15
      (try
        (java.util.Date/from
         (.atStartOfDay (java.time.LocalDate/parse s)
                        java.time.ZoneOffset/UTC))
        (catch Exception _
          (throw (ex-info "cannot parse temporal value"
                          {:error :invalid-text-representation
                           :type "timestamp"
                           :value s})))))))

;; ============================================================================
;; Prepared-statement support — thread cached parse result + bound params
;; into the execute dispatch via dynamic bindings.
;; ============================================================================

(def ^:private ^:dynamic *cached-parsed*
  "When bound by executePrepared, the parsed sql/parse-sql result to
   reuse instead of re-parsing. Nil in the Simple Query path."
  nil)

(def ^:private ^:dynamic *cached-bound*
  "When bound by executePrepared alongside *cached-parsed*, a 1-indexed
   vector of decoded parameter values (element 0 unused). Applied to
   the parsed result via resolve-param-refs before dispatch."
  nil)

(def ^:private ^:dynamic *snapshot-db*
  "When bound (by FETCH from a cursor), replaces `(d/db conn)` at the
   top of `execute` so the SELECT runs against the cursor's frozen
   snapshot instead of whatever has committed since DECLARE. Nil on
   the normal Simple / Extended Query paths."
  nil)

(defn- resolve-param-refs
  "Given a parsed result from sql/parse-sql and a 1-indexed `bound`
   vector (element 0 unused), return a parsed result with all
   ParamRef placeholders replaced by bound values.

   Applied at Execute time for prepared statements:
   - `:in-args` vector may contain ParamRef sentinels (one per `$N`
     the SELECT translator encountered).
   - `:tx-data` for INSERT may contain ParamRef at any depth.
   - `:sub-results` (UNION / INTERSECT / EXCEPT branches) are parsed
     independently, so ParamRefs live inside each branch's :in-args.
     Recurse so the set-op executor sees real values, not sentinels."
  [parsed bound]
  (let [fetch (fn [idx] (nth bound idx))]
    (cond-> parsed
      (contains? parsed :in-args)
      (update :in-args sql/substitute-params fetch)
      (contains? parsed :tx-data)
      (update :tx-data sql/substitute-params fetch)
      (contains? parsed :sub-results)
      (update :sub-results
              (fn [subs]
                (mapv (fn [sub]
                        (cond-> sub
                          (contains? sub :in-args)
                          (update :in-args sql/substitute-params fetch)))
                      subs))))))

(defn- coerce-insert-tx-data
  "After ParamRef substitution, an INSERT's `:tx-data` entity maps may
   still hold the raw wire value for any parameter the client sent
   untyped (text format) — e.g. node-postgres binds `$1` = the string
   \"270\" for an integer column. translate-insert ran coerce-insert-value
   at parse time, but a ParamRef placeholder passes through unchanged, so
   the string was never narrowed to the column's `:db/valueType` and
   Datahike rejects it (\"invalid input syntax for column …\"). Re-run
   coerce-insert-value now that the concrete value is known.

   coerce-insert-value is idempotent on already-typed values (literals
   coerced at parse time stay put), so this is safe to apply to every
   attribute. A parameter that resolved to SQL NULL drops out of the
   entity map — matching translate-insert's own `keep` semantics, where a
   nil column value means \"don't assert this attribute\" (EAV null)."
  [parsed schema]
  (if (and (= :insert (:type parsed)) (seq (:tx-data parsed)))
    (update parsed :tx-data
            (fn [tx]
              (mapv (fn [entry]
                      (if (map? entry)
                        (reduce-kv
                         (fn [m attr v]
                           (cond
                             (= :db/id attr)   (assoc m attr v)
                             (not (keyword? attr)) (assoc m attr v)
                             (nil? v)          m
                             :else (if-let [c (#'sql/coerce-insert-value v attr schema)]
                                     (assoc m attr c)
                                     m)))
                         {} entry)
                        entry))
                    tx)))
    parsed))

(declare nextval!)

(defn- resolve-nextval-markers
  "Sibling pass to `resolve-param-refs`: walk `parsed`'s tx-data /
   in-args / sub-results and replace every `{:fn :nextval :seq-name S}`
   marker (emitted by translate-* for `nextval('s')` in VALUES / SET
   expressions) with the long produced by an actual nextval against
   the live conn.

   Runs after ParamRef substitution and before per-type dispatch so the
   markers never reach the transactor. Each call commits independently
   via the same CAS-retry path `SELECT nextval(...)` uses, matching
   PG's non-transactional `nextval` semantics: advances stick even if
   the surrounding tx rolls back, and concurrent advances yield
   distinct values."
  [parsed conn]
  (let [resolver #(nextval! conn %)
        resolve  #(sql/resolve-nextvals! % resolver)]
    (cond-> parsed
      (contains? parsed :in-args)     (update :in-args resolve)
      (contains? parsed :tx-data)     (update :tx-data resolve)
      (contains? parsed :sub-results)
      (update :sub-results
              (fn [subs]
                (mapv (fn [sub]
                        (cond-> sub
                          (contains? sub :in-args) (update :in-args resolve)
                          (contains? sub :tx-data) (update :tx-data resolve)))
                      subs))))))

(def ^:private compat-presets
  "Named bundles for :compat. Each maps to a :silently-accept set
   that's merged on top of the caller's explicit set.

   :strict (default) — reject every unsupported DDL with SQLSTATE 0A000.
   :permissive       — authorization/RLS DDL that real ORMs emit but
                       has no meaning in our data model: GRANT, REVOKE,
                       POLICY, RLS, CREATE/DROP EXTENSION. Makes
                       Hibernate, Odoo, Alembic, Flyway, etc. boot
                       cleanly.
   :pg-dump          — superset of :permissive that also accepts the
                       extra DDL `pg_dump` emits for non-data schema
                       objects we don't model: triggers, functions,
                       procedures, aggregates, materialized views,
                       rules, operators, casts, languages, partition
                       attach/detach, ALTER TYPE / ALTER DOMAIN
                       boilerplate. The data load + roundtrip still
                       work; advisory side-effects (audit triggers,
                       computed defaults driven by triggers) are lost.

   (COMMENT ON, LOCK TABLE, CREATE VIEW, CREATE INDEX and arbitrary
   SET vars are already silently accepted unconditionally — see
   sql/system-query?.)"
  {:strict     #{}
   :permissive #{:grant :revoke :policy :rls :create-extension}
   :pg-dump    #{:grant :revoke :policy :rls :create-extension
                 :trigger :function :procedure :aggregate
                 :materialized-view :rule :operator :cast :language
                 :attach-partition :alter-type :alter-domain
                 ;; non-ENUM CREATE TYPE forms (composite, range, base)
                 :type}})

(defn- resolve-silently-accept [{:keys [compat silently-accept]}]
  (let [preset (get compat-presets (or compat :strict))]
    (into (or preset #{}) (or silently-accept #{}))))

;; ============================================================================
;; System-type handlers
;; ----------------------------------------------------------------------------
;; Each handler takes a `ctx` map containing the closure-captured server
;; state (conn, session-id, the atoms) plus per-call context (sql, handler
;; self-reference) and a `parsed` map from sql/parse-sql (pre-classified,
;; with kind-specific fields merged in — :name, :args, :template, etc.).
;;
;; The dispatch in `.execute` builds the ctx once and delegates.
;; Moving the per-kind bodies out of the 1200-line reify lets each handler
;; be named, documented, and individually debuggable.
;; ============================================================================

(defn- void-result [col-name]
  (PgWireServer$QueryResult.
   (into-array String [col-name])
   (int-array [2278])  ; OID_VOID
   (into-array (Class/forName "[Ljava.lang.String;")
               [(into-array String [""])])
   "SELECT 1"))

(defn- single-row-result
  "Helper: one column, one row, one value of the given OID."
  [col-name ^long oid ^String value]
  (PgWireServer$QueryResult.
   (into-array String [col-name])
   (int-array [oid])
   (into-array (Class/forName "[Ljava.lang.String;")
               [(into-array String [value])])
   "SELECT 1"))

;; ============================================================================
;; System-query result metadata
;; ============================================================================
;;
;; Each row-producing system-query (the classified :current-database, :now,
;; :version, …) ships a tiny metadata fragment here. `parse` calls
;; `system-result-metadata` once and stores the result on the parsed map as
;; `:metadata {:names [...] :oids [...]}`; `describeResult` reads it to emit
;; RowDescription during Extended Query's Describe phase. Without this,
;; Describe returns nil → NoData → pgjdbc aborts with "Received resultset
;; tuples, but no field structure for them" when Execute later sends DataRow.
;;
;; Execution stays lazy: the handle-* fns at their existing sites are the
;; source of truth for the values. This fn only names the *shape* — keeping
;; it pure lets us compute safely at parse time for side-effecting queries
;; (nextval, advisory locks, pg_sleep) too.
;;
;; Types with no rows (SET, BEGIN, COMMIT, DECLARE CURSOR, …) are absent from
;; the dispatch — returning nil there is correct because describeResult →
;; NoData is the protocol-legal response for a row-less command.
;;
;; OID 2278 is PG's `void` OID — functions like pg_advisory_lock return void.

(def ^:private OID_VOID 2278)

(defn- show-metadata
  "SHOW has a per-setting shape: `SHOW tables` returns 3 columns, every
   other setting returns one. Mirrors `handle-show`'s cond."
  [parsed]
  (let [setting (or (:var parsed) "")]
    (if (= setting "tables")
      {:names ["Schema" "Name" "Owner"]
       :oids  [PgWireServer/OID_TEXT PgWireServer/OID_TEXT PgWireServer/OID_TEXT]}
      {:names [setting]
       :oids  [PgWireServer/OID_TEXT]})))

(defn- system-result-metadata
  "Return `{:names [...] :oids [...]}` for row-producing system queries,
   or nil for no-row system queries / types we don't describe. Covers the
   Extended Query Describe contract; must match the column shape each
   handle-* emits at Execute."
  [parsed]
  (case (:system-type parsed)
    :current-database       {:names ["current_database"]           :oids [PgWireServer/OID_TEXT]}
    :current-schema         {:names ["current_schema"]             :oids [PgWireServer/OID_TEXT]}
    :version                {:names ["version"]                    :oids [PgWireServer/OID_TEXT]}
    :now                    {:names ["now"]                        :oids [PgWireServer/OID_TIMESTAMP]}
    :pg-backend-pid         {:names ["pg_backend_pid"]             :oids [PgWireServer/OID_INT4]}
    :txid-current           {:names ["txid_current"]               :oids [PgWireServer/OID_INT8]}
    :pg-keywords            {:names ["string_agg"]                 :oids [PgWireServer/OID_TEXT]}
    :nextval                {:names ["nextval"]                    :oids [PgWireServer/OID_INT8]}
    :currval                {:names ["currval"]                    :oids [PgWireServer/OID_INT8]}
    :setval                 {:names ["setval"]                     :oids [PgWireServer/OID_INT8]}
    :set-config             {:names ["set_config"]                 :oids [PgWireServer/OID_TEXT]}
    :advisory-lock          {:names ["pg_advisory_lock"]           :oids [OID_VOID]}
    :advisory-xact-lock     {:names ["pg_advisory_xact_lock"]      :oids [OID_VOID]}
    :advisory-unlock-all    {:names ["pg_advisory_unlock_all"]     :oids [OID_VOID]}
    :pg-sleep               {:names ["pg_sleep"]                   :oids [OID_VOID]}
    :pg-notify              {:names ["pg_notify"]                  :oids [OID_VOID]}
    :try-advisory-lock      {:names ["pg_try_advisory_lock"]       :oids [PgWireServer/OID_BOOL]}
    :try-advisory-xact-lock {:names ["pg_try_advisory_xact_lock"]  :oids [PgWireServer/OID_BOOL]}
    :advisory-unlock        {:names ["pg_advisory_unlock"]         :oids [PgWireServer/OID_BOOL]}
    :get-fk-conname         {:names ["name"]                       :oids [PgWireServer/OID_TEXT]}
    ;; datahike.* branching / versioning functions. Multi-row results
    ;; (branches, parent_commits) still advertise a single-column row;
    ;; PG's protocol doesn't need per-row metadata, only per-column.
    :dh-branches            {:names ["branches"]                    :oids [PgWireServer/OID_TEXT]}
    :dh-current-branch      {:names ["current_branch"]              :oids [PgWireServer/OID_TEXT]}
    :dh-commit-id           {:names ["commit_id"]                   :oids [PgWireServer/OID_TEXT]}
    :dh-parent-commits      {:names ["parent_commits"]              :oids [PgWireServer/OID_TEXT]}
    :dh-create-branch       {:names ["create_branch"]               :oids [PgWireServer/OID_TEXT]}
    :dh-delete-branch       {:names ["delete_branch"]               :oids [PgWireServer/OID_TEXT]}
    :show                   (show-metadata parsed)
    :empty-catalog          (let [{:keys [names oids]
                                   :or {names ["id"] oids [PgWireServer/OID_INT8]}}
                                  (sql/extract-empty-catalog-shape (:sql parsed))]
                              {:names (vec names) :oids (vec oids)})
    :get-primary-keys       {:names ["TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME"
                                     "COLUMN_NAME" "KEY_SEQ" "PK_NAME"
                                     "IS_NOT_NULL"]
                             :oids  [PgWireServer/OID_TEXT PgWireServer/OID_TEXT
                                     PgWireServer/OID_TEXT PgWireServer/OID_TEXT
                                     PgWireServer/OID_INT2 PgWireServer/OID_TEXT
                                     PgWireServer/OID_BOOL]}
    :get-field-metadata     {:names ["oid" "attnum" "attname" "relname"
                                     "nspname" "notnull" "isautoincrement"]
                             :oids  [PgWireServer/OID_OID PgWireServer/OID_INT2
                                     PgWireServer/OID_TEXT PgWireServer/OID_TEXT
                                     PgWireServer/OID_TEXT PgWireServer/OID_BOOL
                                     PgWireServer/OID_BOOL]}
    nil))

(defn- describe-from-metadata
  "Turn `{:names :oids}` into a zero-row QueryResult for Describe."
  [{:keys [names oids]}]
  (PgWireServer$QueryResult.
   (into-array String names)
   (int-array oids)
   (into-array (Class/forName "[Ljava.lang.String;") (make-array String 0 0))
   "SELECT 0"))

;; --- Prepared-statement handlers --------------------------------------------

(defn- handle-prepare
  "PREPARE name [(types)] AS sql — classify extracts :name and :template."
  [{:keys [sql-prepared]} parsed]
  (let [pname (:name parsed)
        tmpl  (:template parsed)]
    (if (and pname tmpl)
      (do (swap! sql-prepared assoc pname {:sql tmpl})
          (empty-result "PREPARE"))
      (error-result "PREPARE: syntax error" "42601"))))

(defn- handle-execute-prepared
  "EXECUTE name[(args)] — substitute args into the stored template, then
   recurse through the handler's .execute so the substituted SQL flows
   through the full dispatch. Args are literal SQL expressions per PG
   SQL-level EXECUTE semantics (pgjdbc/psycopg2 use extended-query Bind,
   which goes through a type-aware path)."
  [{:keys [sql-prepared handler]} parsed]
  (let [pname (:name parsed)
        args-text (:args-text parsed)
        rec (when pname (get @sql-prepared pname))]
    (if-not rec
      (error-result (str "prepared statement \"" pname "\" does not exist") "26000")
      (let [args (parse-execute-args args-text)
            template (:sql rec)
            sql-out (substitute-prepared-params template args)]
        (.execute ^PgWireServer$QueryHandler handler sql-out)))))

(defn- handle-deallocate
  "DEALLOCATE [PREPARE] name | DEALLOCATE ALL"
  [{:keys [sql-prepared]} parsed]
  (cond
    (:all? parsed)
    (do (reset! sql-prepared {})
        (empty-result "DEALLOCATE ALL"))
    (:name parsed)
    (do (swap! sql-prepared dissoc (:name parsed))
        (empty-result "DEALLOCATE"))
    :else
    (error-result "DEALLOCATE: syntax error" "42601")))

;; --- Cursor handlers --------------------------------------------------------

(defn- handle-declare-cursor
  "DECLARE name [NO SCROLL|SCROLL] CURSOR [WITH HOLD] FOR select.
   Defer materialization: stash the SELECT and probe column metadata via
   LIMIT 0. Each FETCH rewrites the stored query's LIMIT/OFFSET so cursors
   over million-row tables never realize all rows at once.

   The snapshot captured here is PG-semantic: FETCH sees the db as of
   DECLARE, regardless of concurrent writers. In-tx cursors use the
   speculative-db — they don't observe subsequent writes by this tx
   (known limit)."
  [{:keys [conn session-state tx-state cursors handler]} parsed]
  (let [cname (:name parsed)
        cquery (:inner-sql parsed)
        snap (let [base (apply-temporal (d/db conn) session-state)]
               (if (:in-tx? @tx-state)
                 (or (:speculative-db @tx-state) base)
                 base))]
    (if (and cname cquery)
      (let [probe-sql (rewrite-cursor-page cquery 0 0)
            probe (binding [*snapshot-db* snap]
                    (.execute ^PgWireServer$QueryHandler handler probe-sql))]
        (if (.error ^PgWireServer$QueryResult probe)
          probe
          (do (swap! cursors assoc cname
                     {:sql     cquery
                      :columns (vec (.columnNames ^PgWireServer$QueryResult probe))
                      :oids    (vec (.columnOids ^PgWireServer$QueryResult probe))
                      :snap    snap
                      :pos     (atom 0)
                      :done?   (atom false)})
              (empty-result "DECLARE CURSOR"))))
      (error-result "DECLARE: syntax error" "42601"))))

(defn- handle-fetch-cursor
  "FETCH [count | ALL | NEXT] [FROM|IN] name. Forward-only — PRIOR/
   BACKWARD raise 0A000."
  [{:keys [cursors handler]} parsed]
  (let [direction (:direction parsed)
        cname (:name parsed)
        all? (= direction "all")
        n (cond all? nil
                (:count parsed) (:count parsed)
                :else 1)
        rec (when cname (get @cursors cname))]
    (cond
      (not rec)
      (error-result (str "cursor \"" cname "\" does not exist") "34000")
      (contains? #{"prior" "backward"} direction)
      (error-result "cursor can only scan forward" "0A000")
      :else
      (let [pos-atom (:pos rec)
            done-atom (:done? rec)
            start @pos-atom
            query-sql (rewrite-cursor-page (:sql rec) start n)]
        (if @done-atom
          (PgWireServer$QueryResult.
           (into-array String (:columns rec))
           (int-array (:oids rec))
           (into-array (Class/forName "[Ljava.lang.String;") (make-array String 0 0))
           "FETCH 0")
          (let [result (binding [*snapshot-db* (:snap rec)]
                         (.execute ^PgWireServer$QueryHandler handler query-sql))]
            (if (.error ^PgWireServer$QueryResult result)
              result
              (let [rows (.rows ^PgWireServer$QueryResult result)
                    nret (alength rows)]
                (reset! pos-atom (+ start nret))
                (when (or all? (and n (< nret n)))
                  (reset! done-atom true))
                (PgWireServer$QueryResult.
                 (into-array String (:columns rec))
                 (int-array (:oids rec))
                 rows
                 (str "FETCH " nret))))))))))

(defn- handle-close-cursor
  "CLOSE (name | ALL)"
  [{:keys [cursors]} parsed]
  (cond
    (:all? parsed)
    (do (reset! cursors {})
        (empty-result "CLOSE ALL"))
    (:name parsed)
    (do (swap! cursors dissoc (:name parsed))
        (empty-result "CLOSE CURSOR"))
    :else
    (error-result "CLOSE: syntax error" "42601")))

(defn- handle-move-cursor
  "MOVE repositions without returning rows. MOVE N trusts the caller;
   MOVE ALL runs a count-query against the cursor snapshot to find the
   true end."
  [{:keys [cursors handler]} parsed]
  (let [direction (:direction parsed)
        cname (:name parsed)
        rec (when cname (get @cursors cname))]
    (cond
      (not rec)
      (error-result (str "cursor \"" cname "\" does not exist") "34000")

      (contains? #{"prior" "backward"} direction)
      (error-result "cursor can only scan forward" "0A000")

      (= direction "all")
      (let [count-sql (str "SELECT count(*) FROM (" (:sql rec) ") _subq")
            result (binding [*snapshot-db* (:snap rec)]
                     (.execute ^PgWireServer$QueryHandler handler count-sql))]
        (if (.error ^PgWireServer$QueryResult result)
          result
          (let [rows (.rows ^PgWireServer$QueryResult result)
                total (if (pos? (alength rows))
                        (try (Long/parseLong (first (aget rows 0)))
                             (catch Throwable _ 0))
                        0)
                start @(:pos rec)
                end (long total)]
            (reset! (:pos rec) end)
            (reset! (:done? rec) true)
            (empty-result (str "MOVE " (- end start))))))

      :else
      (let [n (or (:count parsed) 1)]
        (swap! (:pos rec) + n)
        (empty-result (str "MOVE " n))))))

;; --- Transaction handlers ---------------------------------------------------

(defn- handle-begin
  [{:keys [conn tx-state session-state]} _parsed]
  (if (:in-tx? @tx-state)
    (tag-tx-status (empty-result "BEGIN") tx-state)
    (let [real-db (d/db conn)]
      (swap! tx-state assoc
             :in-tx? true :aborted? false :tx-buffer []
             :speculative-db (apply-temporal real-db session-state)
             ;; Snapshot :max-tx at BEGIN so COMMIT can detect concurrent
             ;; writes by other sessions (emits 40001 serialization_failure
             ;; — the code Odoo/ORMs retry on).
             :begin-max-tx (:max-tx real-db)
             :eid->tempid {} :savepoints [])
      (tag-tx-status (empty-result "BEGIN") tx-state))))

(defn- handle-savepoint
  "Savepoint names are unique within a tx but may be reused after RELEASE.
   Outside a tx PG raises 25P01."
  [{:keys [tx-state]} parsed]
  (let [name (:name parsed)]
    (if-not (:in-tx? @tx-state)
      (error-result "SAVEPOINT can only be used in transaction blocks" "25P01")
      (do (swap! tx-state update :savepoints (fnil conj [])
                 {:name name
                  :speculative-db (:speculative-db @tx-state)
                  :tx-buffer (:tx-buffer @tx-state)
                  :eid->tempid (:eid->tempid @tx-state)
                  :owned-locks (:owned-locks @tx-state)})
          (empty-result "SAVEPOINT")))))

(defn- handle-release-savepoint
  "RELEASE SAVEPOINT name — discards the named savepoint and any more
   recent. Row locks acquired since the savepoint become part of the
   outer scope (NOT released). PG raises 3B001 if name isn't active."
  [{:keys [tx-state]} parsed]
  (let [name (:name parsed)
        stack (:savepoints @tx-state)
        idx (when (and name (seq stack))
              (->> stack
                   (keep-indexed (fn [i sp] (when (= (:name sp) name) i)))
                   last))]
    (cond
      (not (:in-tx? @tx-state))
      (error-result "RELEASE SAVEPOINT can only be used in transaction blocks"
                    "25P01")
      (nil? idx)
      (do (swap! tx-state assoc :aborted? true)
          (error-result (str "savepoint \"" name "\" does not exist") "3B001"))
      :else
      (do (swap! tx-state update :savepoints subvec 0 idx)
          (empty-result "RELEASE SAVEPOINT")))))

(defn- handle-commit
  [{:keys [conn session-id tx-state]} _parsed]
  (if (:in-tx? @tx-state)
    (try
      (let [buf (:tx-buffer @tx-state)
            begin-max-tx (:begin-max-tx @tx-state)
            real-db (d/db conn)
            current-max-tx (when begin-max-tx (:max-tx real-db))
            advanced? (and begin-max-tx current-max-tx
                           (> current-max-tx begin-max-tx))]
        ;; Concurrent-write detection: fire 40001 only when our write set
        ;; overlaps a concurrent committer's. Wildcard case (retractEntity)
        ;; falls back to the pessimistic "any concurrent write conflicts".
        (when (and (seq buf) advanced?)
          (let [our-attrs   (tx-buffer-attrs buf)
                wildcard?   (contains? our-attrs ::wildcard)
                their-attrs (concurrent-write-attrs real-db begin-max-tx)
                conflict?   (or wildcard?
                                (= their-attrs ::any)
                                (some their-attrs our-attrs))]
            (when conflict?
              (throw (ex-info "could not serialize access due to concurrent update"
                              {:error  :serialization-failure
                               :detail (str "base=" begin-max-tx
                                            ", current=" current-max-tx
                                            ", overlap="
                                            (cond wildcard? "wildcard (retractEntity)"
                                                  (= their-attrs ::any) "query-failed"
                                                  :else (pr-str
                                                         (filter their-attrs our-attrs))))})))))
        (when (seq buf) (d/transact conn buf))
        (release-session-locks! session-id)
        (release-advisory-locks! session-id true)
        (swap! tx-state assoc
               :in-tx? false :aborted? false
               :owned-locks #{})
        (tag-tx-status (empty-result "COMMIT") tx-state))
      (catch Exception e
        (release-session-locks! session-id)
        (release-advisory-locks! session-id true)
        (swap! tx-state assoc
               :in-tx? false :aborted? false
               :owned-locks #{})
        (classified-error "COMMIT failed: " e)))
    (tag-tx-status (empty-result "COMMIT") tx-state)))

(defn- handle-rollback
  [{:keys [session-id tx-state]} _parsed]
  (release-session-locks! session-id)
  (release-advisory-locks! session-id true)
  (swap! tx-state assoc
         :in-tx? false :aborted? false
         :owned-locks #{})
  (tag-tx-status (empty-result "ROLLBACK") tx-state))

(defn- handle-rollback-to-savepoint
  "ROLLBACK TO SAVEPOINT name — rolls back changes since the named
   savepoint (which remains active). Locks acquired after the target
   are released. Raises 25P01 outside a tx, 3B001 if the savepoint
   name isn't active."
  [{:keys [tx-state]} parsed]
  (let [sp-stack (:savepoints @tx-state)
        name (:name parsed)
        target-idx (when (seq sp-stack)
                     (->> sp-stack
                          (keep-indexed (fn [i sp] (when (= (:name sp) name) i)))
                          last))]
    (cond
      (not (:in-tx? @tx-state))
      (error-result "ROLLBACK TO SAVEPOINT can only be used in transaction blocks"
                    "25P01")
      (nil? target-idx)
      (do (swap! tx-state assoc :aborted? true)
          (error-result (str "savepoint \"" name "\" does not exist") "3B001"))
      :else
      (let [target (nth sp-stack target-idx)
            {:keys [speculative-db tx-buffer eid->tempid
                    owned-locks]} target
            current-locks (:owned-locks @tx-state)
            to-release (clojure.set/difference current-locks
                                               (or owned-locks #{}))]
        (when (seq to-release)
          (swap! lock-registry
                 (fn [reg]
                   (reduce (fn [r k] (dissoc r k))
                           reg to-release))))
        (swap! tx-state assoc
               :aborted? false
               :speculative-db speculative-db
               :tx-buffer tx-buffer
               :eid->tempid eid->tempid
               :owned-locks (or owned-locks #{})
               ;; Keep savepoints up to AND INCLUDING the target (the
               ;; target stays active, more recent ones go).
               :savepoints (subvec sp-stack 0 (inc target-idx)))
        (empty-result "ROLLBACK")))))

(defn- handle-discard-all
  "DISCARD ALL: reset session state, release locks, drop savepoints,
   abort any in-progress tx, clear per-handler caches. Matches PG's
   behavior in src/backend/commands/discard.c — clients use this
   between connection-pool checkouts."
  [{:keys [session-id tx-state session-state sql-prepared cursors]} _parsed]
  (release-session-locks! session-id)
  (release-advisory-locks! session-id)
  (reset! tx-state {:in-tx? false :aborted? false
                    :session-id session-id
                    :owned-locks #{}})
  (reset! session-state {})
  (reset! sql-prepared {})
  (reset! cursors {})
  (empty-result "DISCARD ALL"))

;; --- Advisory-lock handlers -------------------------------------------------

(defn- handle-advisory-lock
  [{:keys [session-id]} parsed]
  (advisory-lock! (parse-advisory-key parsed) session-id false)
  (void-result "pg_advisory_lock"))

(defn- handle-try-advisory-lock
  [{:keys [session-id]} parsed]
  (let [got? (advisory-lock-try! (parse-advisory-key parsed) session-id false)]
    (single-row-result "pg_try_advisory_lock"
                       PgWireServer/OID_BOOL
                       (if got? "t" "f"))))

(defn- handle-advisory-xact-lock
  [{:keys [session-id tx-state]} parsed]
  (if-not (:in-tx? @tx-state)
    (error-result "pg_advisory_xact_lock requires a transaction" "25P01")
    (do (advisory-lock! (parse-advisory-key parsed) session-id true)
        (void-result "pg_advisory_xact_lock"))))

(defn- handle-try-advisory-xact-lock
  [{:keys [session-id tx-state]} parsed]
  (if-not (:in-tx? @tx-state)
    (error-result "pg_try_advisory_xact_lock requires a transaction" "25P01")
    (let [got? (advisory-lock-try! (parse-advisory-key parsed) session-id true)]
      (single-row-result "pg_try_advisory_xact_lock"
                         PgWireServer/OID_BOOL
                         (if got? "t" "f")))))

(defn- handle-advisory-unlock
  [{:keys [session-id]} parsed]
  (let [ok? (advisory-unlock! (parse-advisory-key parsed) session-id)]
    (single-row-result "pg_advisory_unlock"
                       PgWireServer/OID_BOOL
                       (if ok? "t" "f"))))

(defn- handle-advisory-unlock-all
  [{:keys [session-id]} _parsed]
  (release-advisory-locks! session-id)
  (void-result "pg_advisory_unlock_all"))

;; --- Session introspection --------------------------------------------------

(defn- handle-pg-backend-pid
  [{:keys [session-id]} _parsed]
  (single-row-result "pg_backend_pid"
                     PgWireServer/OID_INT4
                     (str (Math/abs
                           (bit-and 0x7fffffff
                                    (.hashCode ^String session-id))))))

(defn- handle-txid-current
  [{:keys [conn]} _parsed]
  (let [tx-id (try (:max-tx (d/db conn)) (catch Throwable _ 0))]
    (single-row-result "txid_current"
                       PgWireServer/OID_INT8
                       (str (or tx-id 0)))))

(defn- handle-pg-sleep
  "Honor the requested sleep, capped at 60s to prevent DoS via
   `pg_sleep(99999)`. classify emits :args as numbers (long or double)."
  [_ctx parsed]
  (let [secs (double (or (first (:args parsed)) 0))
        ms (long (min 60000 (Math/round (* 1000.0 secs))))]
    (when (pos? ms) (Thread/sleep ms))
    (void-result "pg_sleep")))

(defn- handle-pg-notify
  "No-op: pg-datahike has no LISTEN/NOTIFY delivery, so a NOTIFY with no
   subscribers is observably the same as a delivered NOTIFY ignored.
   Keeps Odoo's bus post-commit hook (addons/bus/models/bus.py) from
   tripping the registry build during --init=mail (and transitively
   --init=account)."
  [_ctx _parsed]
  (void-result "pg_notify"))

(defn- handle-now [_ctx _parsed]
  (single-row-result "now"
                     PgWireServer/OID_TIMESTAMP
                     (value->string (java.util.Date.))))

(defn- handle-pg-keywords [_ctx _parsed]
  (single-row-result "string_agg" PgWireServer/OID_TEXT ""))

;; --- Datahike versioning / branching ---------------------------------------

(defn- text-result
  "Multi-row single-column result with values rendered as plain text
   (OID_TEXT). tag is the CommandComplete label — e.g. \"SELECT 3\".
   Used by the datahike.* branching handlers whose natural output is
   a list of strings (branch names, parent commit UUIDs, …)."
  [^String col-name ^String tag values]
  (let [rows (into-array
              (Class/forName "[Ljava.lang.String;")
              (mapv (fn [v] (into-array String [(str v)])) values))]
    (PgWireServer$QueryResult.
     (into-array String [col-name])
     (int-array [PgWireServer/OID_TEXT])
     rows
     tag)))

(defn- handle-dh-branches
  "SELECT datahike.branches() → one row per registered branch name."
  [conn _ctx _parsed]
  (let [names (some->> (versioning/branches conn)
                       (map name)
                       sort
                       vec)]
    (text-result "branches" (str "SELECT " (count names)) names)))

(defn- handle-dh-current-branch
  "SELECT datahike.current_branch() → the session's pinned branch, or
   the conn's default branch when none is pinned."
  [conn session-state]
  (let [b (or (:branch @session-state)
              (get-in @conn [:config :branch])
              :db)]
    (single-row-result "current_branch" PgWireServer/OID_TEXT (name b))))

(defn- handle-dh-commit-id
  "SELECT datahike.commit_id() → UUID of the current session's db head
   (after any branch / commit-id / temporal SETs are applied)."
  [conn session-state]
  (let [db (apply-temporal (d/db conn) session-state)]
    (single-row-result "commit_id" PgWireServer/OID_TEXT
                       (str (versioning/commit-id db)))))

(defn- handle-dh-parent-commits
  "SELECT datahike.parent_commits() → UUIDs of the current db's
   immediate parent commits (zero rows at genesis; one row in the
   common case; two+ rows on a merge commit)."
  [conn session-state]
  (let [db (apply-temporal (d/db conn) session-state)
        parents (or (versioning/parent-commit-ids db) [])]
    (text-result "parent_commits"
                 (str "SELECT " (count parents))
                 (map str parents))))

(defn- handle-dh-create-branch
  "SELECT datahike.create_branch('new', 'from') → creates `:new` from
   the branch-keyword `:from` (or a commit UUID string). O(1) konserve
   write; does not go through the transaction writer."
  [conn parsed]
  (let [[new-name from] (:args parsed)]
    (when-not (and new-name from)
      (throw (ex-info "datahike.create_branch arity"
                      {:error :syntax-error
                       :detail "datahike.create_branch requires (new-name, from-branch-or-commit)"
                       :args (:args parsed)})))
    (let [from-ref (if (re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
                                   from)
                     (java.util.UUID/fromString from)
                     (keyword from))]
      (versioning/branch! conn from-ref (keyword new-name))
      (single-row-result "create_branch" PgWireServer/OID_TEXT new-name))))

(defn- handle-dh-delete-branch
  "SELECT datahike.delete_branch('name') → marks the branch unreachable;
   GC reclaims the underlying commits on the next sweep."
  [conn parsed]
  (let [[bname] (:args parsed)]
    (when-not bname
      (throw (ex-info "datahike.delete_branch arity"
                      {:error :syntax-error
                       :detail "datahike.delete_branch requires (branch-name)"
                       :args (:args parsed)})))
    (versioning/delete-branch! conn (keyword bname))
    (single-row-result "delete_branch" PgWireServer/OID_TEXT bname)))

;; --- Catalog probes ---------------------------------------------------------

(defn- handle-empty-catalog
  "Zero-row SELECT whose RowDescription matches the outer query's
   projection shape. pgjdbc's DatabaseMetaData.getTables / getColumns /
   getIndexInfo issue 12+-column SELECTs against pg_constraint /
   pg_description and fail on column-index-out-of-range if the shape
   is wrong. SELECT * (or anything we can't parse) falls back to the
   legacy 1-column id int8 shape."
  [{:keys [sql]} _parsed]
  (if-let [{:keys [names oids]} (sql/extract-empty-catalog-shape sql)]
    (PgWireServer$QueryResult.
     (into-array String names)
     (int-array oids)
     (into-array (Class/forName "[Ljava.lang.String;") (make-array String 0 0))
     "SELECT 0")
    (PgWireServer$QueryResult.
     (into-array String ["id"])
     (int-array [PgWireServer/OID_INT8])
     (into-array (Class/forName "[Ljava.lang.String;") (make-array String 0 0))
     "SELECT 0")))

(defn- handle-get-fk-conname
  "Odoo's post-add-foreign-key lookup. Returns a synthetic deterministic
   name since we don't track constraint entities the same way PG does."
  [{:keys [sql]} _parsed]
  (let [fk-name (str "fk_" (Math/abs (.hashCode ^String sql)))]
    (single-row-result "name" PgWireServer/OID_TEXT fk-name)))

(defn- handle-get-primary-keys
  "pgjdbc DatabaseMetaData.getPrimaryKeys / updatable-ResultSet PK probe.
   Resolves the PK columns from Datahike schema — :db.unique/identity
   attrs for single-col PK, :db/tupleAttrs for composite — without
   executing pgjdbc's wide catalog JOIN."
  [{:keys [conn sql]} _parsed]
  (let [schema-now (:schema (d/db conn))
        tname (second (re-find #"ct\.relname\s*=\s*'([^']+)'" sql))
        tuple-attr-kw (when tname (keyword tname "pg$pk_tuple"))
        tuple-props (when tuple-attr-kw (get schema-now tuple-attr-kw))
        cols
        (cond
          (and tuple-props (seq (:db/tupleAttrs tuple-props)))
          (map-indexed (fn [i a] [(name a) (inc i)])
                       (:db/tupleAttrs tuple-props))
          tname
          (let [single (some (fn [[a p]]
                               (when (and (keyword? a)
                                          (= tname (namespace a))
                                          (= :db.unique/identity (:db/unique p))
                                          (not= a tuple-attr-kw))
                                 a))
                             schema-now)]
            (when single [[(name single) 1]])))
        rows (into []
                   (for [[col-name key-seq] (or cols [])]
                     (into-array String
                                 [nil "public" tname col-name
                                  (str key-seq)
                                  (str tname "_pkey")
                                  "t"])))]
    (PgWireServer$QueryResult.
     (into-array String ["TABLE_CAT" "TABLE_SCHEM" "TABLE_NAME"
                         "COLUMN_NAME" "KEY_SEQ" "PK_NAME"
                         "IS_NOT_NULL"])
     (int-array [PgWireServer/OID_TEXT PgWireServer/OID_TEXT
                 PgWireServer/OID_TEXT PgWireServer/OID_TEXT
                 PgWireServer/OID_INT2 PgWireServer/OID_TEXT
                 PgWireServer/OID_BOOL])
     (into-array (Class/forName "[Ljava.lang.String;") rows)
     (str "SELECT " (count rows)))))

(defn- handle-get-field-metadata
  "pgjdbc PgResultSetMetaData.fetchFieldMetaData. Extracts (tableOid,
   attnum) pairs from pgjdbc's inline UNION ALL and resolves each via
   our materialized catalog (reverse pg_class.oid → relname, then
   pg_attribute for attname / attnotnull). pgjdbc only cares about the
   per-field row; nullable / autoincrement booleans answer with
   'not null = attnotnull' and 'autoinc = false'."
  [{:keys [conn sql]} _parsed]
  (let [first-pair (re-find #"SELECT\s+(\d+)\s+AS\s+oid\s*,\s*(\d+)\s+AS\s+attnum" sql)
        rest-pairs (re-seq #"UNION\s+ALL\s+SELECT\s+(\d+)\s*,\s*(\d+)\b" sql)
        pairs (into (if first-pair [[(Long/parseLong (nth first-pair 1))
                                     (Long/parseLong (nth first-pair 2))]]
                        [])
                    (map (fn [[_ o a]]
                           [(Long/parseLong o) (Long/parseLong a)]))
                    rest-pairs)
        db (d/db conn)
        tbl-by-oid (into {}
                         (map (fn [[oid ident]] [oid (namespace ident)]))
                         (d/q '{:find [?oid ?marker]
                                :where [[?e :pg/table-oid ?oid]
                                        [?e :db/ident ?marker]]}
                              db))
        schema (dbi/-schema db)
        virtual (pgs/derive-virtual-tables schema (pgs/schema-hints db))
        rows (into []
                   (for [[toid anum] pairs
                         :let [tname (get tbl-by-oid toid)
                               cols (get-in virtual [tname :columns])
                               col (nth cols (dec anum) nil)]
                         :when col]
                     (into-array String
                                 [(str toid)
                                  (str anum)
                                  (:name col)
                                  tname
                                  "public"
                                  (if (= :db.unique/identity (:unique col)) "t" "f")
                                  "f"])))]
    (PgWireServer$QueryResult.
     (into-array String ["oid" "attnum" "attname" "relname"
                         "nspname" "notnull" "isautoincrement"])
     (int-array [PgWireServer/OID_OID PgWireServer/OID_INT2
                 PgWireServer/OID_TEXT PgWireServer/OID_TEXT
                 PgWireServer/OID_TEXT PgWireServer/OID_BOOL
                 PgWireServer/OID_BOOL])
     (into-array (Class/forName "[Ljava.lang.String;") rows)
     (str "SELECT " (count rows)))))

;; --- Sequence handlers ------------------------------------------------------

(def ^:private nextval-max-retries
  "Bound on the optimistic-retry loop in handle-nextval. Real
   contention on a single sequence shouldn't push past a handful of
   retries even at thousands of qps; 100 is generous."
  100)

(defn nextval!
  "Atomically advance the named sequence on `conn` and return the new
   long. Shared core of `SELECT nextval(...)` and INSERT-VALUES nextval
   resolution.

   PG semantics: a `nextval` advance is never rolled back, even by
   ROLLBACK on the surrounding transaction. We match that by always
   committing to the live conn, regardless of any session's `:in-tx?`
   state.

   Atomicity: optimistic CAS-retry. Each iteration reads the current
   :__seq__/value, computes the new value, and submits

       [[:db/cas seq-eid :__seq__/value curr new]]

   to `d/transact`. Datahike's transactor serialises tx applications
   per conn, so two concurrent CAS submissions see each other: the
   first wins, the second's old-val no longer matches the
   transactor's current value and the CAS raises `:transact/cas`. We
   re-read and retry. The loser thread will see the winner's new
   value, compute the next slot, and commit cleanly.

   Why CAS instead of the simpler `:db.fn/call` pattern: Datahike's
   writer batches multiple in-flight transactions for a single
   commit, then writes the SAME `:db-after` (the batch's final db)
   into every tx-report in the batch. So reading `(:db-after report)`
   to recover the value this call assigned doesn't work — it returns
   the LAST tx in the batch. CAS sidesteps that: the value we
   intended is the literal `new` slot of the op-vec, available
   without reading `:db-after`.

   Throws ex-info `:undefined-sequence` if the named sequence does
   not exist, and `:serialization-failure` if the contention retry
   budget is exhausted."
  [conn ^String seq-name]
  (let [q-fn d/q
        db0 (d/db conn)
        ;; Schema-qualified name (`public.foo_seq`)? Sequences live in a
        ;; flat namespace in pg-datahike, so strip the schema prefix —
        ;; same convention CREATE SEQUENCE uses.
        bare-name (if (and seq-name (clojure.string/includes? seq-name "."))
                    (last (clojure.string/split seq-name #"\." 2))
                    seq-name)
        eid (ffirst (q-fn '{:find [?e]
                            :where [[?e :__seq__/name ?n]]
                            :in [$ ?n]}
                          db0 bare-name))
        _ (when-not eid
            (throw (ex-info "sequence does not exist"
                            {:error :undefined-sequence
                             :sequence seq-name})))
        incr (or (ffirst (q-fn '{:find [?i]
                                 :where [[?e :__seq__/increment ?i]]
                                 :in [$ ?e]}
                               db0 eid))
                 1)
        read-curr (fn []
                    (ffirst (q-fn '{:find [?v]
                                    :where [[?e :__seq__/value ?v]]
                                    :in [$ ?e]}
                                  (d/db conn) eid)))
        ;; CAS failure detection: Datahike's transactor raises an
        ;; ex-info with `{:error :transact/cas}`, but the writer's
        ;; throwable-promise + CompletableFuture wrapping strips
        ;; the structured ex-data by the time we see the throw on
        ;; the caller thread. The original message is preserved
        ;; (verbatim "_db.fn/cas failed_" substring), so we match
        ;; on that.
        cas-failure? (fn [^Throwable e]
                       (when-let [m (.getMessage e)]
                         (.contains m ":db.fn/cas failed")))]
    ;; Exponential backoff between retries: 1ms, 2ms, 4ms, …
    ;; capped at 100ms. Datahike's commit-loop waits up to 50ms
    ;; (commit-wait-time) between batches before flushing.
    (loop [attempt 0]
      (let [curr (or (read-curr) 0)
            next (+ curr incr)
            cas-ok?
            (try (d/transact conn [[:db/cas eid :__seq__/value curr next]])
                 true
                 (catch Throwable e
                   (if (cas-failure? e) false (throw e))))]
        (cond
          cas-ok? next
          (>= attempt nextval-max-retries)
          (throw (ex-info "nextval contention retry budget exhausted"
                          {:error :serialization-failure
                           :detail (str "nextval('" seq-name "') gave up after "
                                        nextval-max-retries " contention retries")
                           :sequence seq-name}))
          :else
          (do (Thread/sleep ^long (min 100 (bit-shift-left 1 (min 7 attempt))))
              (recur (inc attempt))))))))

(defn- handle-nextval
  "SELECT nextval('seq_name') — wire wrapper around `nextval!`."
  [{:keys [conn]} parsed]
  (try
    (single-row-result "nextval" PgWireServer/OID_INT8 (str (nextval! conn (:seq-name parsed))))
    (catch Exception e
      (classified-error "nextval error: " e))))

(defn- handle-currval
  "SELECT currval('seq_name') — read current value. PG raises 55000 if
   nextval hasn't fired in this session; we return 0 instead (simpler
   and good enough for the common idempotent-seed pattern)."
  [{:keys [conn tx-state]} parsed]
  (try
    (let [seq-name (some-> (:seq-name parsed)
                           (#(if (clojure.string/includes? % ".")
                               (last (clojure.string/split % #"\." 2)) %)))
          lookup-db (if (:in-tx? @tx-state)
                      (:speculative-db @tx-state)
                      (d/db conn))
          curr-val (ffirst (d/q '{:find [?v]
                                  :where [[?e :__seq__/name ?n]
                                          [?e :__seq__/value ?v]]
                                  :in [$ ?n]}
                                lookup-db seq-name))]
      (single-row-result "currval" PgWireServer/OID_INT8 (str (or curr-val 0))))
    (catch Exception e
      (classified-error "currval error: " e))))

(defn- handle-setval
  "SELECT setval('seq_name', N[, is_called]) — force the sequence to N.
   Returns the new value; PG's 3-arg form's is_called flag controls
   whether the NEXT nextval returns N or N+increment, which we ignore
   (we just persist N)."
  [{:keys [conn tx-state]} parsed]
  (try
    (let [seq-name (some-> (:seq-name parsed)
                           (#(if (clojure.string/includes? % ".")
                               (last (clojure.string/split % #"\." 2)) %)))
          new-val (:new-value parsed)
          lookup-db (if (:in-tx? @tx-state)
                      (:speculative-db @tx-state)
                      (d/db conn))
          seq-eid (ffirst (d/q '{:find [?e]
                                 :where [[?e :__seq__/name ?n]]
                                 :in [$ ?n]}
                               lookup-db seq-name))]
      (if seq-eid
        (let [setval-tx [[:db/add seq-eid :__seq__/value new-val]]]
          (if (:in-tx? @tx-state)
            (let [spec-report (dc/with (:speculative-db @tx-state) setval-tx)]
              (swap! tx-state (fn [st]
                                (-> st
                                    (assoc :speculative-db (:db-after spec-report))
                                    (update :tx-buffer into setval-tx)))))
            (d/transact conn setval-tx))
          (single-row-result "setval" PgWireServer/OID_INT8 (str new-val)))
        (error-result (str "Sequence not found: " seq-name))))
    (catch Exception e
      (classified-error "setval error: " e))))

;; ============================================================================
;; Per-type executors — each handles one (:type parsed) value.
;;
;; Every executor takes [ctx parsed] and returns a PgWireServer$QueryResult.
;; ctx is constructed once per query in make-query-handler's execute body
;; (see the comment block above that ctx literal for the key contract).
;; ============================================================================

;; Forward declarations — exec-system dispatches to exec-copy-from-stdin
;; for `:copy-from-stdin`, but exec-copy-from-stdin (which depends on
;; helpers like columns-from-schema, copy-flush-batch!) is defined further
;; below to keep related code contiguous.
(declare exec-copy-from-stdin)

(defn- exec-system
  "Dispatch on (:system-type parsed). System-types are recognised by
   cls/classify (token-driven) and don't go through JSqlParser — most
   delegate to the handle-X defns above; a handful are inline empty-
   result tags for synthetic success of no-op DDL (CREATE/DROP SCHEMA,
   COMMENT ON, LOCK TABLE, …)."
  [ctx parsed]
  (let [{:keys [conn session-state schema tx-state
                on-create-database on-delete-database registry-atom]} ctx]
    (case (:system-type parsed)
      :set      (empty-result "SET")
      :prepare          (handle-prepare ctx parsed)
      :execute-prepared (handle-execute-prepared ctx parsed)
      :deallocate       (handle-deallocate ctx parsed)
      :declare-cursor   (handle-declare-cursor ctx parsed)
      :fetch-cursor     (handle-fetch-cursor ctx parsed)
      :close-cursor     (handle-close-cursor ctx parsed)
      :move-cursor      (handle-move-cursor ctx parsed)
      :begin                 (handle-begin ctx parsed)
      :savepoint             (handle-savepoint ctx parsed)
      :release-savepoint     (handle-release-savepoint ctx parsed)
      :commit                (handle-commit ctx parsed)
      :rollback              (handle-rollback ctx parsed)
      :rollback-to-savepoint (handle-rollback-to-savepoint ctx parsed)
      :discard-all           (handle-discard-all ctx parsed)
      :discard-scoped
      ;; DISCARD PLANS/SEQUENCES/TEMP/LOCKS. No-op in our model;
      ;; report the actual variant in the command tag. classify
      ;; exposes the lowercased scope.
      (empty-result (str "DISCARD " (str/upper-case (:scope parsed))))
      :comment-on (empty-result "COMMENT")
      :lock-table (empty-result "LOCK TABLE")
      :maintenance-noop
      ;; VACUUM / REINDEX / CLUSTER — classify carries the verb
      ;; as :tag already ("VACUUM" / "REINDEX" / "CLUSTER").
      (empty-result (:tag parsed))
      :schema-noop
      ;; CREATE / DROP / ALTER SCHEMA — classify :tag already
      ;; encodes the full "CREATE SCHEMA" / "DROP SCHEMA" / etc.
      (empty-result (:tag parsed))

      :owner-noop
      ;; ALTER <object> ... OWNER TO <role>. pg_dump-emitted; we
      ;; don't have a role system. classify carries the matching
      ;; tag ("ALTER TABLE" / "ALTER SEQUENCE" / …).
      (empty-result (:tag parsed))

      :psql-meta
      ;; \restrict / \unrestrict / \connect / \c / \set markers
      ;; that pg_dump emits and that leaked through psql. classify
      ;; carries the original \-prefixed metacommand as :tag.
      (empty-result (:tag parsed))

      :set-config
      ;; SELECT pg_catalog.set_config('search_path', '', false) —
      ;; pg_dump session-prelude. We don't honor the GUC (no
      ;; equivalent in Datahike); just synthesize a 1-row result so
      ;; the SELECT completes cleanly. The 3-arg form returns the
      ;; new value as text; we return empty-string.
      (single-row-result "set_config" PgWireServer/OID_TEXT "")

      :copy-from-stdin
      ;; SQL `COPY t [(cols)] FROM STDIN [WITH (...)];`. Returns a
      ;; QueryResult flagged copyInMode — the wire layer emits
      ;; CopyInResponse and transitions to COPY-IN sub-protocol;
      ;; subsequent CopyData/CopyDone/CopyFail messages route to
      ;; the QueryHandler reify's copyChunk/copyComplete/copyAbort
      ;; methods, which mutate the :copy-state atom.
      (try
        (exec-copy-from-stdin ctx parsed)
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)
                state (or (:sqlstate data)
                          (case (:error data)
                            :undefined-table "42P01"
                            :feature-not-supported "0A000"
                            :syntax-error "42601"
                            "XX000"))]
            (-> (PgWireServer$QueryResult.
                 (str "COPY failed: " (.getMessage e)))
                (.withSqlstate state))))
        (catch Throwable e
          (-> (PgWireServer$QueryResult.
               (str "COPY failed: " (.getMessage e)))
              (.withSqlstate "XX000"))))

      :create-database
      ;; SQL `CREATE DATABASE foo [WITH (...)];`. Routed via the
      ;; operator-supplied :on-create-database hook. On success the
      ;; new conn is added to the server's mutable registry under
      ;; db-name; subsequent connections with database=foo route
      ;; to it. Without a hook configured we return SQLSTATE 0A000
      ;; — provisioning from SQL is a deployment policy decision,
      ;; not a default. See datahike.pg.sql.database/db-from-template.
      (let [{:keys [db-name options if-not-exists?]} parsed]
        (cond
          (and if-not-exists? registry-atom
               (contains? @registry-atom db-name))
          (empty-result "CREATE DATABASE")

          (nil? on-create-database)
          (-> (PgWireServer$QueryResult.
               (str "CREATE DATABASE not supported — configure "
                    ":on-create-database on the server"))
              (.withSqlstate "0A000"))

          (and registry-atom (contains? @registry-atom db-name))
          (-> (PgWireServer$QueryResult.
               (str "database \"" db-name "\" already exists"))
              (.withSqlstate "42P04"))

          :else
          (try
            (let [new-conn (on-create-database db-name options)]
              (when registry-atom
                (swap! registry-atom assoc db-name new-conn))
              (empty-result "CREATE DATABASE"))
            (catch clojure.lang.ExceptionInfo e
              (let [data (ex-data e)
                    state (or (:sqlstate data)
                              (case (:error data)
                                :syntax-error "42601"
                                :feature-not-supported "0A000"
                                "XX000"))]
                (-> (PgWireServer$QueryResult.
                     (str "CREATE DATABASE failed: " (.getMessage e)))
                    (.withSqlstate state))))
            (catch Throwable e
              (-> (PgWireServer$QueryResult.
                   (str "CREATE DATABASE failed: " (.getMessage e)))
                  (.withSqlstate "XX000"))))))

      :drop-database
      ;; SQL `DROP DATABASE [IF EXISTS] foo;`. Routed via
      ;; :on-delete-database. Removes the entry from the registry
      ;; on success and calls the hook so the operator can release
      ;; + delete the backing store.
      (let [{:keys [db-name if-exists?]} parsed
            existing (when registry-atom (get @registry-atom db-name))]
        (cond
          (and (nil? existing) if-exists?)
          (empty-result "DROP DATABASE")

          (nil? existing)
          (-> (PgWireServer$QueryResult.
               (str "database \"" db-name "\" does not exist"))
              (.withSqlstate "3D000"))

          (nil? on-delete-database)
          (-> (PgWireServer$QueryResult.
               (str "DROP DATABASE not supported — configure "
                    ":on-delete-database on the server"))
              (.withSqlstate "0A000"))

          :else
          (try
            (on-delete-database db-name existing nil)
            (when registry-atom
              (swap! registry-atom dissoc db-name))
            (empty-result "DROP DATABASE")
            (catch Throwable e
              (-> (PgWireServer$QueryResult.
                   (str "DROP DATABASE failed: " (.getMessage e)))
                  (.withSqlstate "XX000"))))))

      ;; Advisory locks (see defonce ^:private advisory-locks above).
      :advisory-lock           (handle-advisory-lock ctx parsed)
      :try-advisory-lock       (handle-try-advisory-lock ctx parsed)
      :advisory-xact-lock      (handle-advisory-xact-lock ctx parsed)
      :try-advisory-xact-lock  (handle-try-advisory-xact-lock ctx parsed)
      :advisory-unlock         (handle-advisory-unlock ctx parsed)
      :advisory-unlock-all     (handle-advisory-unlock-all ctx parsed)

      ;; Session introspection
      :pg-backend-pid          (handle-pg-backend-pid ctx parsed)
      :txid-current            (handle-txid-current ctx parsed)
      :pg-sleep                (handle-pg-sleep ctx parsed)
      :pg-notify               (handle-pg-notify ctx parsed)
      ;; Catalog probes (shape-matched in system-query?*)
      :empty-catalog      (handle-empty-catalog ctx parsed)
      :create-view        (empty-result "CREATE VIEW")
      :create-index       (empty-result "CREATE INDEX")
      :get-fk-conname     (handle-get-fk-conname ctx parsed)
      :get-primary-keys   (handle-get-primary-keys ctx parsed)
      :get-field-metadata (handle-get-field-metadata ctx parsed)

      ;; Simple info queries
      :show              (handle-show (:var parsed) schema session-state)
      :version           (handle-version)
      :pg-keywords       (handle-pg-keywords ctx parsed)
      :current-schema    (handle-current-schema)
      :current-database  (handle-current-database (:db-name @session-state))
      :now               (handle-now ctx parsed)

      ;; Sequence functions (classify supplies :seq-name / :new-value)
      :nextval           (handle-nextval ctx parsed)
      :currval           (handle-currval ctx parsed)
      :setval            (handle-setval ctx parsed)

      ;; datahike.* branching / versioning
      :dh-branches       (handle-dh-branches conn ctx parsed)
      :dh-current-branch (handle-dh-current-branch conn session-state)
      :dh-commit-id      (handle-dh-commit-id conn session-state)
      :dh-parent-commits (handle-dh-parent-commits conn session-state)
      :dh-create-branch  (handle-dh-create-branch conn parsed)
      :dh-delete-branch  (handle-dh-delete-branch conn parsed)
      (empty-result "OK"))))

(defn- exec-select
  "Execute a SELECT. Handles literal-row table-free SELECTs, FOR
   UPDATE row-locking variants (skip / nowait / block), aggregate-on-
   empty default rows, server-side null-safe ORDER BY, hidden ORDER-BY
   column stripping, window functions, HAVING, compound aggregate
   expressions, DISTINCT-on-aggregates, and schema-derived OID
   computation for the result-set metadata."
  [ctx parsed]
  (let [{:keys [db tx-state]} ctx
        {:keys [query find-aliases limit offset
                having has-aggregates? has-distinct?
                in-args hidden-count compound-exprs window-specs
                sql-order-by sql-limit sql-offset
                enriched-db literal-row literal-rows for-update]} parsed]
    (if (or literal-row literal-rows)
      ;; Table-free SELECT: return literal row(s) directly.
      ;; :literal-rows is used by table-function expansions
      ;; (unnest(array_fill(...))) that produce N rows from
      ;; compile-time-known arguments. Pass :select-item-oids
      ;; (via a synthetic schema-oids array keyed by
      ;; -1 sentinel) so SELECT TRUE reports BOOL even when
      ;; value inference would look at a String.
      (let [item-oids (:select-item-oids parsed)
            schema-oids (when item-oids
                          (int-array
                           (map #(int (or % -1)) item-oids)))]
        (format-query-result (or literal-rows [literal-row])
                             find-aliases
                             schema-oids))
      (let [;; Use enriched db when CTEs/derived tables created speculative data
            query-db (or enriched-db db)
            hidden-count (or hidden-count 0)
            q-input (cond-> query
                      limit  (assoc :limit limit)
                      offset (assoc :offset offset)
                      :always (assoc :cancel (current-cancel)))
            results (if (seq in-args)
                      (apply d/q q-input query-db in-args)
                      (d/q q-input query-db))
            ;; FOR UPDATE / FOR NO KEY UPDATE / SKIP LOCKED / NOWAIT.
            ;; Extract the `id` column from each result row, check the
            ;; server-wide lock registry, and either:
            ;;   :skip    — drop rows held by another session
            ;;   :nowait  — raise 55P03 if any row held by another
            ;;   :block   — acquire optimistically (no real blocking
            ;;              possible; behaves like :nowait for correctness)
            results
            (if for-update
              (let [{:keys [wait table]} for-update
                    id-idx (some (fn [[i a]]
                                   (when (and (string? a)
                                              (or (= a "id")
                                                  (str/ends-with? a ".id")))
                                     i))
                                 (map-indexed vector find-aliases))
                    session-id (:session-id @tx-state)]
                (if-not (and id-idx session-id)
                  results ;; can't lock without an id column; no-op
                  (let [results-vec (mapv (fn [r] (if (sequential? r) (vec r) [r]))
                                          results)]
                    (case wait
                      :skip
                      (let [kept (vec (keep (fn [row]
                                              (let [id (nth row id-idx nil)]
                                                (when (and (some? id)
                                                           (= :acquired (acquire-lock! session-id table id)))
                                                  (swap! tx-state update :owned-locks (fnil conj #{}) [table id])
                                                  row)))
                                            results-vec))]
                        kept)
                      :nowait
                      (let [conflict (some (fn [row]
                                             (let [id (nth row id-idx nil)]
                                               (when (and (some? id)
                                                          (= :conflict (acquire-lock! session-id table id)))
                                                 id)))
                                           results-vec)]
                        (if conflict
                          (throw (ex-info "lock not available"
                                          {:error :lock-not-available
                                           :table table}))
                          (do
                            (doseq [row results-vec
                                    :let [id (nth row id-idx nil)]
                                    :when (some? id)]
                              (swap! tx-state update :owned-locks (fnil conj #{}) [table id]))
                            results-vec)))
                      :block
                      ;; No way to actually wait; behave like :nowait
                      ;; (strict) — the session that would block simply
                      ;; sees the error. Most PG clients retry.
                      (let [conflict (some (fn [row]
                                             (let [id (nth row id-idx nil)]
                                               (when (and (some? id)
                                                          (= :conflict (acquire-lock! session-id table id)))
                                                 id)))
                                           results-vec)]
                        (if conflict
                          (throw (ex-info "lock not available"
                                          {:error :lock-not-available
                                           :table table}))
                          (do
                            (doseq [row results-vec
                                    :let [id (nth row id-idx nil)]
                                    :when (some? id)]
                              (swap! tx-state update :owned-locks (fnil conj #{}) [table id]))
                            results-vec)))))))
              results)
            ;; SQL requires: aggregate on empty result → one row with defaults
            ;; COUNT(*) → 0, SUM/AVG/MIN/MAX → NULL.
            ;; This applies ONLY when there is no GROUP BY — i.e.
            ;; every :find element is an aggregate form. With a
            ;; group-by column in :find (a plain `?var` element),
            ;; an empty result means zero matching groups, so PG
            ;; returns zero rows. The earlier always-on default
            ;; synthesis turned `WHERE …→ 0 rows GROUP BY status`
            ;; into a single bogus `[null, 0]` row.
            find-elems (:find query)
            all-aggregates? (and (seq find-elems)
                                 (every? (fn [elem]
                                           (and (seq? elem)
                                                (symbol? (first elem))))
                                         find-elems))
            results (if (and has-aggregates?
                             all-aggregates?
                             (empty? (seq results)))
                      (let [default-row (mapv (fn [elem]
                                                (let [agg-name (name (first elem))]
                                                  (if (or (= agg-name "count")
                                                          (= agg-name "count-distinct")
                                                          (= agg-name "filter-count")
                                                          (= agg-name "filter-count-distinct"))
                                                    0
                                                    nil)))
                                              find-elems)]
                        [default-row])
                      results)
            ;; Server-side null-safe sort (when ORDER BY has nullable columns)
            results (if sql-order-by
                      (let [null-safe-cmp
                            (fn [a b]
                              (let [av (if (sequential? a) a [a])
                                    bv (if (sequential? b) b [b])]
                                (loop [specs (partition 2 sql-order-by)]
                                  (if-let [[idx dir] (first specs)]
                                    (let [va (nth av idx nil)
                                          vb (nth bv idx nil)
                                          a-null? (or (nil? va) (= :__null__ va))
                                          b-null? (or (nil? vb) (= :__null__ vb))
                                          c (cond
                                              (and a-null? b-null?) 0
                                              ;; NULLs last for ASC, first for DESC (PG default)
                                              a-null? (if (= dir :asc) 1 -1)
                                              b-null? (if (= dir :asc) -1 1)
                                              :else (if (= dir :desc)
                                                      (compare vb va)
                                                      (compare va vb)))]
                                      (if (zero? c)
                                        (recur (rest specs))
                                        c))
                                    0))))]
                        (let [sorted (sort null-safe-cmp results)]
                          (cond->> sorted
                            sql-offset (drop sql-offset)
                            sql-limit  (take sql-limit))))
                      results)
            ;; Apply HAVING filter BEFORE trimming hidden columns:
            ;; HAVING can reference an aggregate that wasn't in the SELECT
            ;; projection — translate-select appends such aggregates as
            ;; hidden find-elements, and the HAVING :col-idx points at
            ;; them. Trimming first would strip the column the filter
            ;; needs and silently drop every row.
            results (apply-having results find-aliases having)
            ;; Strip hidden ORDER BY / HAVING-aggregate columns from
            ;; results and aliases.
            [results find-aliases]
            (if (pos? hidden-count)
              (let [visible (- (count (:find query)) hidden-count)]
                [(map (fn [row]
                        (if (sequential? row)
                          (vec (take visible row))
                          row))
                      results)
                 find-aliases])
              [results find-aliases])
            ;; Apply window functions: ROW_NUMBER, RANK, SUM OVER, etc.
            ;; Window specs reference column indices in the result tuples.
            ;; The window engine appends computed values to each row.
            [results find-aliases]
            (if (seq window-specs)
              (let [;; Ensure results are vectors of vectors
                    vec-results (mapv (fn [r] (if (sequential? r) (vec r) [r])) results)
                    windowed (window/execute-window-functions vec-results window-specs)
                    ;; Add window aliases
                    win-aliases (mapv (fn [spec]
                                        (or (:alias spec) (name (:op spec))))
                                      window-specs)
                    new-aliases (into (vec find-aliases) win-aliases)
                    ;; Hide internal window helper columns (__win_*)
                    visible-indices (into []
                                          (keep-indexed (fn [i a]
                                                          (when-not (and (string? a)
                                                                         (.startsWith ^String a "__win_"))
                                                            i)))
                                          new-aliases)
                    final-results (mapv (fn [row]
                                          (mapv #(nth row % nil) visible-indices))
                                        windowed)
                    final-aliases (mapv #(nth new-aliases %) visible-indices)]
                [final-results final-aliases])
              [results find-aliases])
            ;; Apply compound aggregate expressions: MAX(a) - MIN(a)
            ;; Each compound-expr has {:alias :op :l-idx :r-idx}.
            ;; We compute the derived value and replace the hidden agg columns.
            [results find-aliases]
            (if (seq compound-exprs)
              (let [ops {'+ + '- - '* * '/ /}
                    ;; Compute the compound values for each row
                    new-results
                    (mapv (fn [row]
                            (let [rv (if (sequential? row) (vec row) [row])]
                              (reduce (fn [r {:keys [op l-idx r-idx]}]
                                        (let [lv (nth r l-idx nil)
                                              rv-val (nth r r-idx nil)
                                              op-fn (get ops op)]
                                          (conj r (when (and lv rv-val op-fn
                                                             (number? lv) (number? rv-val))
                                                    (op-fn lv rv-val)))))
                                      rv compound-exprs)))
                          results)
                    ;; Build new aliases: keep originals + add compound aliases
                    compound-aliases (mapv :alias compound-exprs)
                    new-aliases (into (vec find-aliases) compound-aliases)
                    ;; Hide the internal aggregate columns (prefixed with __compound_)
                    visible-indices (into []
                                          (keep-indexed (fn [i a]
                                                          (when-not (and (string? a)
                                                                         (.startsWith ^String a "__compound_"))
                                                            i)))
                                          new-aliases)
                    final-results (mapv (fn [row]
                                          (mapv #(nth row %) visible-indices))
                                        new-results)
                    final-aliases (mapv #(nth new-aliases %) visible-indices)]
                [final-results final-aliases])
              [results find-aliases])
            ;; Apply DISTINCT deduplication for aggregate queries
            results (if (and has-distinct? has-aggregates?)
                      (distinct results)
                      results)]
        ;; Derive schema-based OIDs for proper type metadata.
        ;; Shared with describeResult; see compute-schema-oids.
        (let [parsed-with-shape (assoc parsed :find-aliases find-aliases :query query)
              schema-oids (compute-schema-oids parsed-with-shape db)
              ;; Blend parse-time OIDs (oid-infer)
              ;; over the -1 sentinel so empty
              ;; result sets and aggregate /
              ;; CAST / literal columns keep the
              ;; correct type when value inference
              ;; would otherwise fall back to TEXT.
              item-oids (:select-item-oids parsed)
              schema-oids (if (and item-oids (seq find-aliases))
                            (let [n (count find-aliases)
                                  out (int-array n)]
                              (dotimes [i n]
                                (let [so (aget ^ints schema-oids i)
                                      io (when (< i (count item-oids))
                                           (nth item-oids i))]
                                  (aset out i
                                        (int (if (and (= so -1) io) io so)))))
                              out)
                            schema-oids)
              sources (compute-column-sources parsed-with-shape db)
              result (format-query-result results find-aliases schema-oids)]
          (if sources
            (-> ^PgWireServer$QueryResult result
                (.withColumnSources (first sources) (second sources))
                (.withColumnTypmods (nth sources 2)))
            result))))))

(defn- exec-batchable-insert
  "Append-time half of deferred-CC batching. Builds the same tx-data
   as `execute-insert` (auto-populate-identity + apply-column-
   constraints), validates it via `dc/with` against the running
   speculative-db, and — on success — returns a `withBatchable`
   QueryResult so the wire layer holds the CommandComplete and parks
   the tx-data into its per-connection buffer.

   Constraint violations (NOT NULL, CHECK, FK, etc.) fire here via
   `dc/with` and surface synchronously: matches PG's IMMEDIATE
   constraint behaviour for auto-commit. Only system-level errors
   (konserve I/O) and cross-connection races against another
   connection's commit can land at flush time."
  [ctx parsed batch-state]
  (let [{:keys [conn]} ctx
        table-name (:table parsed)
        ;; Anchor on the spec-db when present so consecutive batched
        ;; INSERTs see each other's effects (sequence increments,
        ;; uniqueness, FKs to siblings). First call in a scope reads
        ;; live db.
        anchor-db (or (:spec-db @batch-state) (d/db conn))
        tx-wrap (or (:tx-wrap ctx) identity)
        tx-data (-> (:tx-data parsed)
                    (auto-populate-identity table-name anchor-db)
                    (apply-column-constraints table-name (:ns parsed) anchor-db)
                    tx-wrap)]
    (try
      (let [spec-report (dc/with anchor-db tx-data)]
        ;; Update the running spec-db so the next batchable INSERT
        ;; sees this row. The wire layer keeps the parallel tx-data
        ;; list and CommandComplete tags.
        (swap! batch-state assoc :spec-db (:db-after spec-report))
        (.withBatchable ^PgWireServer$QueryResult
         (empty-result (str "INSERT 0 " (:count parsed)))
                        tx-data))
      (catch Exception e
        ;; Synchronous validation failure — return a normal error
        ;; result. The batch state isn't updated, so subsequent
        ;; statements continue from the prior spec-db / live db.
        (classified-error "INSERT error: " e)))))

(defn- exec-insert
  [ctx parsed]
  (let [{:keys [conn tx-state batch-state]} ctx]
    (cond
      (:in-tx? @tx-state)
      (try
        (let [table-name (:table parsed)
              spec-db (:speculative-db @tx-state)
              tx-data (-> (:tx-data parsed)
                          (auto-populate-identity table-name spec-db)
                          (apply-column-constraints table-name
                                                    (:ns parsed)
                                                    spec-db))
              spec-report (dc/with spec-db tx-data)
              new-tempids (into {} (keep (fn [[tid eid]] (when (string? tid) [eid tid])))
                                (:tempids spec-report))
              db-after (:db-after spec-report)]
          (swap! tx-state (fn [ts]
                            (-> ts
                                (update :tx-buffer into tx-data)
                                (assoc :speculative-db db-after)
                                (update :eid->tempid merge new-tempids))))
          (if-let [returning (:returning parsed)]
            ;; RETURNING: read values from speculative db-after.
            ;; Order matters — Odoo matches RETURNING rows positionally
            ;; to VALUES. Extract tempids from the parsed tx-data
            ;; (preserves VALUES order) and resolve each to an eid.
            ;; Falls back to hash-order if tempids can't be recovered
            ;; (e.g. ON CONFLICT path where tx-data is :db.fn/call).
            (let [ns-prefix (str table-name "/")
                  tempids-map (:tempids spec-report)
                  has-row? (fn [eid]
                             (some (fn [^datahike.datom.Datom d]
                                     (let [a (.-a d)]
                                       (and (keyword? a)
                                            (.startsWith (str (namespace a) "/") ns-prefix)
                                            (not= (name a) "db-row-exists"))))
                                   (d/datoms db-after :eavt eid)))
                  ;; ON CONFLICT path records row-positional eids/tempids
                  ;; via :row-refs atom set in translate-insert.
                  ;; Non-ON-CONFLICT path uses :db/id tempids on entity maps.
                  ordered-refs (if-let [refs (:row-refs parsed)]
                                 @refs
                                 (keep #(when (and (map? %) (string? (:db/id %)))
                                          (:db/id %))
                                       (:tx-data parsed)))
                  ordered-eids (vec (keep (fn [ref]
                                            (cond
                                              ;; already an eid (DO UPDATE case)
                                              (integer? ref) ref
                                              ;; tempid string — resolve via tempids map
                                              (string? ref) (get tempids-map ref)
                                              :else nil))
                                          ordered-refs))
                  data-eids (if (seq ordered-eids)
                              (filterv has-row? ordered-eids)
                              (filterv has-row? (vals tempids-map)))]
              (build-returning-result returning db-after data-eids table-name (:schema db-after)))
            (empty-result (str "INSERT 0 " (:count parsed)))))
        (catch Exception e
          (swap! tx-state assoc :aborted? true)
          (classified-error "INSERT error: " e)))

      ;; Auto-commit + wire-layer-driven batching scope. Only INSERTs
      ;; whose tx-data is a vector (i.e. ordinary INSERT VALUES /
      ;; INSERT … SELECT) and that don't need RETURNING (which would
      ;; have to read db-after row-by-row to build the response) take
      ;; the batchable path. ON CONFLICT wraps tx-data in :db.fn/call
      ;; and writes through a row-refs atom — that's incompatible
      ;; with held-CC semantics, so it falls through.
      ;;
      ;; The Java wire layer handles deferral for both protocol modes:
      ;; for Simple Query 'Q' it holds CommandComplete strings; for
      ;; Extended Query (P/B/D/E) it holds the full per-statement
      ;; response byte chunks (ParseComplete + BindComplete +
      ;; NoData/RowDescription + CC) so the relative wire-message
      ;; order matches what default PG would emit.
      (and batch-state
           @batch-state
           (vector? (:tx-data parsed))
           (not (:returning parsed))
           (not *snapshot-db*))
      (exec-batchable-insert ctx parsed batch-state)

      :else
      (execute-insert conn parsed :tx-wrap (:tx-wrap ctx)))))

(defn- exec-update-with-recursive
  [ctx parsed]
  (let [{:keys [conn tx-state]} ctx]
    (if (:in-tx? @tx-state)
      (try
        (let [spec-db (:speculative-db @tx-state)
              eid->tempid (:eid->tempid @tx-state)
              {:keys [eids tx-data]} (build-update-with-recursive-tx spec-db parsed)
              spec-report (when (seq tx-data) (dc/with spec-db tx-data))
              commit-tx-data (mapv (fn [[op eid attr val]]
                                     [op (get eid->tempid eid eid) attr val])
                                   tx-data)]
          (swap! tx-state (fn [ts]
                            (cond-> ts
                              true (update :tx-buffer into commit-tx-data)
                              spec-report (assoc :speculative-db (:db-after spec-report)))))
          (empty-result (str "UPDATE " (count eids))))
        (catch Exception e
          (swap! tx-state assoc :aborted? true)
          (classified-error "UPDATE (WITH RECURSIVE) error: " e)))
      (execute-update-with-recursive conn parsed))))

(defn- exec-update
  [ctx parsed]
  (let [{:keys [conn schema tx-state]} ctx]
    (if (:in-tx? @tx-state)
      (try
        (let [spec-db (:speculative-db @tx-state)
              eid->tempid (:eid->tempid @tx-state)
              {:keys [eids tx-data]} (build-update-tx spec-db schema parsed)
              _ (check-update-identity-collisions! spec-db schema tx-data)
              _ (check-not-null-on-update! spec-db tx-data)
              _ (check-updates-against-row-constraints!
                 spec-db (:table parsed) (or (:ns parsed) (:table parsed)) tx-data)
              _ (enforce-fk-restrict-on-update! spec-db (:table parsed) tx-data)
              ;; Apply to speculative-db with ORIGINAL entity IDs
              spec-report (dc/with spec-db tx-data)
              ;; For the commit buffer, remap real eids to tempids.
              ;; Drop :db/retract ops that would remap onto a tempid —
              ;; the entity is new in this tx, so there's no prior
              ;; value to retract, and Datahike rejects tempids in
              ;; :db/retract.
              commit-tx-data (vec (keep (fn [[op eid attr val]]
                                          (let [mapped (get eid->tempid eid eid)]
                                            (when-not (and (= op :db/retract)
                                                           (not= mapped eid))
                                              [op mapped attr val])))
                                        tx-data))]
          (swap! tx-state (fn [ts]
                            (-> ts
                                (update :tx-buffer into commit-tx-data)
                                (assoc :speculative-db (:db-after spec-report)))))
          (empty-result (str "UPDATE " (count eids))))
        (catch Exception e
          (swap! tx-state assoc :aborted? true)
          (classified-error "UPDATE error: " e)))
      (execute-update conn parsed schema :tx-wrap (:tx-wrap ctx)))))

(defn- exec-delete
  [ctx parsed]
  (let [{:keys [conn schema tx-state]} ctx]
    (if (:in-tx? @tx-state)
      (try
        (let [spec-db (:speculative-db @tx-state)
              eid->tempid (:eid->tempid @tx-state)
              {:keys [eids]} (build-delete-tx spec-db schema parsed)
              _ (enforce-fk-restrict-on-delete! spec-db (:table parsed) eids)
              ;; Apply to speculative-db with ORIGINAL entity IDs
              spec-tx-data (mapv (fn [eid] [:db/retractEntity eid]) eids)
              spec-report (dc/with spec-db spec-tx-data)
              ;; For commit buffer, remap to tempids
              commit-tx-data (mapv (fn [eid] [:db/retractEntity (get eid->tempid eid eid)]) eids)]
          (swap! tx-state (fn [ts]
                            (-> ts
                                (update :tx-buffer into commit-tx-data)
                                (assoc :speculative-db (:db-after spec-report)))))
          (empty-result (str "DELETE " (count eids))))
        (catch Exception e
          (swap! tx-state assoc :aborted? true)
          (classified-error "DELETE error: " e)))
      (execute-delete conn parsed schema :tx-wrap (:tx-wrap ctx)))))

(defn- exec-ddl-create
  [ctx parsed]
  (let [{:keys [conn tx-state temp-tables]} ctx
        ^PgWireServer$QueryResult result (execute-ddl-create conn parsed tx-state)]
    ;; Record CREATE TEMP/TEMPORARY TABLE so the connection-close hook can
    ;; drop it (PG temp tables live for the session, not forever). Only
    ;; track on a non-error result so a failed/duplicate create doesn't
    ;; schedule a spurious drop.
    (when (and temp-tables (:temp? parsed) (nil? (.-error result)))
      (swap! temp-tables conj (:table-name parsed)))
    result))

(defn- exec-ddl-create-sequence
  [ctx parsed]
  (let [{:keys [conn tx-state]} ctx]
    (if (:in-tx? @tx-state)
      (execute-ddl-in-tx tx-state (:tx-data parsed) "CREATE SEQUENCE")
      (try
        (d/transact conn (:tx-data parsed))
        (empty-result "CREATE SEQUENCE")
        (catch Exception e
          (classified-error "CREATE SEQUENCE error: " e))))))

(defn- enum-tx-data
  "Build the registry tx-data for a CREATE TYPE … AS ENUM. Stored as a
   single entity under the `:datahike.pg.enum/*` namespace.

   - `:datahike.pg.enum/name` — unique by identity
   - `:datahike.pg.enum/values` — vector of strings (declaration order)
   - `:datahike.pg.enum/value-set` — same values as a `:db.type/string`
     :cardinality/many for fast membership tests"
  [type-name values]
  [;; idempotent schema attrs (ok to re-transact across CREATEs).
   {:db/ident :datahike.pg.enum/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :datahike.pg.enum/values
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}
   {:db/ident :datahike.pg.enum/values-ordered
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   ;; the entity itself. We store values both as a many-cardinality
   ;; set (for fast contains?) AND as a single ordered string
   ;; (newline-separated) so dump can recover declaration order.
   {:datahike.pg.enum/name type-name
    :datahike.pg.enum/values (set values)
    :datahike.pg.enum/values-ordered (clojure.string/join "\n" values)}])

(defn- exec-ddl-create-enum
  [ctx parsed]
  (let [{:keys [conn tx-state]} ctx
        tx-data (enum-tx-data (:type-name parsed) (:values parsed))]
    (if (:in-tx? @tx-state)
      (execute-ddl-in-tx tx-state tx-data "CREATE TYPE")
      (try
        (d/transact conn tx-data)
        (empty-result "CREATE TYPE")
        (catch Exception e
          (classified-error "CREATE TYPE error: " e))))))

(defn- domain-tx-data
  "Build the registry tx-data for a CREATE DOMAIN. Stored as a single
   entity under `:datahike.pg.domain/*`. Optional attrs (check-name,
   check-expr, default-raw) are dissoc'd when nil so we don't write
   `nil`s as datoms."
  [{:keys [domain-name base-type base-args
           check-name check-expr not-null default-raw]}]
  [{:db/ident :datahike.pg.domain/name
    :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :datahike.pg.domain/base-type
    :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.domain/base-args
    :db/valueType :db.type/string :db/cardinality :db.cardinality/many}
   {:db/ident :datahike.pg.domain/check-name
    :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.domain/check-expr
    :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.domain/not-null
    :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.domain/default-raw
    :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   (cond-> {:datahike.pg.domain/name domain-name
            :datahike.pg.domain/base-type base-type
            :datahike.pg.domain/not-null (boolean not-null)}
     (seq base-args)         (assoc :datahike.pg.domain/base-args (set base-args))
     check-name              (assoc :datahike.pg.domain/check-name check-name)
     check-expr              (assoc :datahike.pg.domain/check-expr check-expr)
     default-raw             (assoc :datahike.pg.domain/default-raw default-raw))])

(defn- exec-ddl-create-domain
  [ctx parsed]
  (let [{:keys [conn tx-state]} ctx
        tx-data (domain-tx-data (:domain parsed))]
    (if (:in-tx? @tx-state)
      (execute-ddl-in-tx tx-state tx-data "CREATE DOMAIN")
      (try
        (d/transact conn tx-data)
        (empty-result "CREATE DOMAIN")
        (catch Exception e
          (classified-error "CREATE DOMAIN error: " e))))))

(defn- exec-savepoint
  [ctx _parsed]
  (let [{:keys [tx-state]} ctx]
    (do (swap! tx-state update :savepoints (fnil conj [])
               {:speculative-db (:speculative-db @tx-state)
                :tx-buffer (:tx-buffer @tx-state)
                :eid->tempid (:eid->tempid @tx-state)})
        (empty-result "SAVEPOINT"))))

(defn- exec-release-savepoint
  [ctx _parsed]
  (let [{:keys [tx-state]} ctx]
    (do (swap! tx-state update :savepoints
               (fn [sp] (if (seq sp) (pop sp) [])))
        (empty-result "RELEASE SAVEPOINT"))))

(defn- exec-rollback-to-savepoint
  [ctx _parsed]
  (let [{:keys [tx-state]} ctx
        sp-stack (:savepoints @tx-state)]
    (when (seq sp-stack)
      (let [{:keys [speculative-db tx-buffer eid->tempid]} (peek sp-stack)]
        (swap! tx-state assoc
               :aborted? false  ;; ROLLBACK TO clears error state
               :speculative-db speculative-db
               :tx-buffer tx-buffer
               :eid->tempid eid->tempid
               :savepoints (pop sp-stack))))
    (empty-result "ROLLBACK")))

(defn- exec-ddl-create-index
  [_ctx _parsed]
  (empty-result "CREATE INDEX"))

(defn- exec-ddl-alter
  [ctx parsed]
  (let [{:keys [conn tx-state]} ctx]
    (try
      (let [{:keys [table operations]} parsed
            tx-data (vec (mapcat
                          (fn [{:keys [op columns]}]
                            (when (= op :add-column)
                              (for [{:keys [name type]} columns
                                    :let [base-type (str/replace type #"\s*\([^)]*\)" "")
                                          dh-type (or (get types/sql-name->dh-type type)
                                                      (get types/sql-name->dh-type base-type)
                                                      :db.type/string)]]
                                (cond-> {:db/ident (keyword table name)
                                         :db/valueType dh-type
                                         :db/cardinality :db.cardinality/one}
                                  (#{"jsonb" "json"} base-type)
                                  (assoc :pg/type base-type)))))
                          operations))]
        (if (seq tx-data)
          (if (:in-tx? @tx-state)
            (execute-ddl-in-tx tx-state tx-data "ALTER TABLE")
            (do (d/transact conn tx-data)
                (empty-result "ALTER TABLE")))
          (empty-result "ALTER TABLE")))
      (catch Exception e
        (classified-error "ALTER TABLE error: " e)))))

(defn- drop-table-tx!
  "Retract every data entity and schema attribute belonging to `table`'s
   namespace, committing against `conn`. Shared by the DROP TABLE handler
   and the connection-close temp-table cleanup. Returns the tx-report (or
   nil when the table had nothing to retract). Throws on transact failure
   — callers decide whether to surface or swallow."
  [conn table]
  (let [db (d/db conn)
        db-schema (dbi/-schema db)
        ;; All schema attributes in this table's namespace.
        table-attrs (into []
                          (keep (fn [[attr-kw _]]
                                  (when (and (keyword? attr-kw)
                                             (= (namespace attr-kw) table))
                                    attr-kw)))
                          db-schema)
        ;; Collect all entity IDs for this table. An earlier
        ;; version queried only the FIRST attr, which missed
        ;; rows where that attr was null (pgjdbc's boolfloat
        ;; inserts (i, a, NULL) — if first-attr was `b`, the
        ;; row was never retracted and accumulated across
        ;; DROP/CREATE cycles). Union across every attr to
        ;; catch all rows.
        data-eids (when (seq table-attrs)
                    (into #{}
                          (mapcat (fn [attr]
                                    (map first
                                         (d/q {:find '[?e]
                                               :where [['?e attr]]}
                                              db))))
                          table-attrs))
        ;; Retract all data entities
        data-tx-data (mapv (fn [eid] [:db/retractEntity eid]) (or data-eids []))
        ;; Retract the schema attribute definitions themselves
        schema-tx-data (mapv (fn [attr-kw]
                               (let [attr-eid (ffirst (d/q {:find ['?e]
                                                            :where [['?e :db/ident attr-kw]]}
                                                           db))]
                                 (when attr-eid [:db/retractEntity attr-eid])))
                             table-attrs)
        all-tx-data (into data-tx-data (filter some? schema-tx-data))]
    (when (seq all-tx-data)
      (d/transact conn all-tx-data))))

(defn- exec-ddl-drop
  [ctx parsed]
  (let [{:keys [conn temp-tables]} ctx]
    (try
      (let [table (:table parsed)]
        (drop-table-tx! conn table)
        ;; A DROP TABLE on a tracked temp table means close() must not
        ;; try to drop it again.
        (when temp-tables (swap! temp-tables disj table))
        (empty-result "DROP TABLE"))
      (catch Exception e
        (classified-error "DROP TABLE error: " e)))))

(defn- exec-ddl-drop-sequence
  [ctx parsed]
  (let [{:keys [conn]} ctx]
    (try
      (let [seq-name (:seq-name parsed)
            db (d/db conn)
            seq-eid (ffirst (d/q '{:find [?e]
                                   :where [[?e :__seq__/name ?n]]
                                   :in [$ ?n]}
                                 db seq-name))]
        (when seq-eid
          (d/transact conn [[:db/retractEntity seq-eid]]))
        (empty-result "DROP SEQUENCE"))
      (catch Exception e
        (classified-error "DROP SEQUENCE error: " e)))))

(defn- exec-set-operation
  [ctx parsed]
  (let [{:keys [db]} ctx
        {:keys [op sub-results enriched-db]} parsed
        ;; When the top-level UNION / INTERSECT
        ;; references catalog tables (or CTEs via
        ;; a parent scope), parse-sql attaches
        ;; :enriched-db here so each sub-query
        ;; runs against the enriched speculative
        ;; db, not the handler's raw connection db.
        query-db (or enriched-db db)
        ;; Execute each sub-query and strip any hidden ORDER-BY
        ;; columns before combining — sub-queries may add entity-id
        ;; (or similar) to :find for server-side sort, which must
        ;; not leak into UNION/INTERSECT/EXCEPT row comparison or
        ;; the returned result shape.
        exec-sub (fn [{:keys [query in-args find-aliases hidden-count]}]
                   (let [q-input (assoc query :cancel (current-cancel))
                         raw (if (seq in-args)
                               (apply d/q q-input query-db in-args)
                               (d/q q-input query-db))
                         hc (or hidden-count 0)
                         visible (- (count (:find query)) hc)
                         results (if (pos? hc)
                                   (map (fn [row]
                                          (if (sequential? row)
                                            (vec (take visible row))
                                            row))
                                        raw)
                                   raw)]
                     {:results results :find-aliases find-aliases}))
        executed (mapv exec-sub sub-results)
        find-aliases (:find-aliases (first executed))
        ;; Combine results based on operation type
        combined (case op
                   :union-all (mapcat :results executed)
                   :union     (distinct (mapcat :results executed))
                   :intersect (let [sets (map #(set (:results %)) executed)]
                                (apply clojure.set/intersection sets))
                   :except    (let [first-set (set (:results (first executed)))
                                    rest-sets (map #(set (:results %)) (rest executed))]
                                (apply clojure.set/difference first-set rest-sets))
                   (mapcat :results executed))]
    (format-query-result combined find-aliases)))

(defn- exec-full-join
  [ctx parsed]
  ;; FULL JOIN = LEFT JOIN results + right-only rows
  ;; Execute two LEFT JOINs (original + swapped), combine
  (let [{:keys [db]} ctx
        {:keys [left-query right-query find-aliases]} parsed
        ;; Strip hidden ORDER-BY columns from each sub-query's rows
        ;; so row-set comparison sees only projected columns.
        exec-select (fn [{:keys [query in-args hidden-count]}]
                      (let [q (assoc query :cancel (current-cancel))
                            raw (if (seq in-args)
                                  (apply d/q q db in-args)
                                  (d/q q db))
                            hc (or hidden-count 0)
                            visible (- (count (:find query)) hc)]
                        (if (pos? hc)
                          (map (fn [row]
                                 (if (sequential? row)
                                   (vec (take visible row))
                                   row))
                               raw)
                          raw)))
        left-results (vec (exec-select left-query))
        right-results (vec (exec-select right-query))
        ;; Right-only rows: appear in right-results but not left-results
        ;; (matched rows appear in both with same values)
        left-set (set left-results)
        right-only (remove left-set right-results)
        combined (concat left-results right-only)]
    (format-query-result combined (or find-aliases (:find-aliases left-query)))))

(defn- exec-error
  "Parse-time failure handler. Returns a plain `:message` string,
   optionally with an explicit :sqlstate (for known-unsupported DDL
   like GRANT/REVOKE/RLS — see CT6). Without one, run the message
   through the regex classifier so pgJDBC sees e.g. 42601 for syntax
   errors instead of XX000.

   Silent-accept escape hatch: if the parse result carries a
   :reject-kind and the handler was configured to swallow that kind
   (via :silently-accept / :compat :permissive), return a synthetic
   success tag instead of an error. Lets Hibernate/Odoo/Alembic-class
   clients boot cleanly against a Datahike-backed PG without each
   emitting its own \"IGNORE ERRORS\" try/catch."
  [ctx parsed]
  (let [{:keys [silently-accept tx-state]} ctx
        msg (:message parsed)
        kind (:reject-kind parsed)]
    (if (and kind (contains? silently-accept kind))
      (empty-result (or (:reject-tag parsed) "OK"))
      (let [code (or (:sqlstate parsed)
                     (errors/classify-message msg)
                     "XX000")]
        ;; Parse-time errors abort the surrounding tx the same way
        ;; execute-time errors do — otherwise the next stmt sees a
        ;; "live" tx instead of the canonical 25P02
        ;; in_failed_sql_transaction state.
        (when (:in-tx? @tx-state)
          (swap! tx-state assoc :aborted? true))
        (error-result msg code)))))

(defn- columns-from-schema
  "When `COPY t FROM stdin` is invoked WITHOUT an explicit column
   list, derive the column names from `t`'s schema. Order is the same
   as `pg_attribute.attnum` would expose — we look at every keyword
   attribute in the schema whose namespace matches `ns`, in
   alphabetical order (deterministic; pg_dump uses an explicit
   column list anyway, so this fallback is mostly used by hand-typed
   psql `COPY t FROM stdin` invocations)."
  [schema ns]
  (->> schema
       keys
       (filter keyword?)
       (filter #(= ns (namespace %)))
       (remove #(= "db-row-exists" (name %)))
       (mapv name)
       sort
       vec))

(defn- exec-copy-from-stdin
  "Initialise a COPY-IN session and return a QueryResult signalling
   `copyInMode`. The wire layer reads that and emits CopyInResponse,
   then routes subsequent CopyData/CopyDone/CopyFail messages to
   the QueryHandler reify's copyChunk/copyComplete/copyAbort
   methods (which read the session out of `:copy-state`)."
  [ctx parsed]
  (let [{:keys [schema copy-state]} ctx
        {:keys [ns table columns options]} parsed
        ns (or ns table)
        col-names (or columns (columns-from-schema schema ns))]
    (when (empty? col-names)
      (throw (ex-info (str "no columns found for COPY into \"" table "\"")
                      {:error :undefined-table :table table})))
    (let [format (:format options)
          decoder-ns (case format
                       :text 'datahike.pg.sql.copy.text-format
                       :csv  'datahike.pg.sql.copy.csv-format)
          _ (require decoder-ns)
          make-fn      (resolve (symbol (str decoder-ns) "make-decoder"))
          step-fn      (resolve (symbol (str decoder-ns) "decode-step"))
          finalize-fn  (resolve (symbol (str decoder-ns) "decode-finalize"))
          decoder      (make-fn (assoc options :columns col-names))]
      (reset! copy-state
              {:decoder         decoder
               :decode-step-fn  step-fn
               :decode-finalize-fn finalize-fn
               :columns         col-names
               :ns              ns
               :table           table
               :row-marker      (pgs/row-marker-attr table)
               :rows-committed  0
               :pending-rows    []
               :batch-size      1000
               :error           nil})
      ;; Return QueryResult signalling COPY-IN with the column count.
      (let [r (PgWireServer$QueryResult/empty "COPY 0")]
        (.withCopyInMode r (count col-names))
        r))))

(defn- copy-flush-batch!
  "Transact a batch of rows from the copy-state's pending buffer.
   Mutates the session: clears pending, adds row-count, and on
   error sets :error so future chunks no-op until copyComplete /
   copyAbort surfaces it."
  [ctx]
  (let [{:keys [conn copy-state]} ctx
        s @copy-state
        rows (:pending-rows s)]
    (when (and (seq rows) (nil? (:error s)))
      (try
        (let [tx-data' (-> rows
                           (auto-populate-identity (:table s) (d/db conn))
                           (apply-column-constraints (:table s) (:ns s) (d/db conn)))]
          (d/transact conn tx-data')
          (swap! copy-state #(-> %
                                 (assoc :pending-rows [])
                                 (update :rows-committed + (count rows)))))
        (catch Throwable e
          (swap! copy-state assoc :error (.getMessage e))
          (throw e))))))

(defn- copy-process-rows!
  "Fold a batch of decoded rows into the session: build entity maps
   via `copy/row->entity-map`, append to pending, and flush whenever
   we cross batch-size."
  [ctx rows]
  (let [{:keys [copy-state schema]} ctx
        {:keys [columns ns row-marker batch-size]} @copy-state]
    (doseq [row rows]
      (let [next-idx (-> @copy-state :rows-committed (+ (count (:pending-rows @copy-state))))
            entity (datahike.pg.sql.copy/row->entity-map
                    row columns ns row-marker schema next-idx)]
        (swap! copy-state update :pending-rows conj entity)))
    (when (>= (count (:pending-rows @copy-state)) batch-size)
      (copy-flush-batch! ctx))))

(defn make-query-handler
  "Create a PgWireServer.QueryHandler that dispatches SQL to Datahike.

   conn: a Datahike connection
   opts: optional map with
     :on-query         (fn [sql])  invoked on every SQL string
     :compat           :strict (default) | :permissive — named bundle
                       of features to silently accept, see compat-presets.
     :silently-accept  a set of reject-kinds to swallow on top of the
                       preset. Valid kinds: :grant :revoke :policy :rls
                       :create-extension. These return a synthetic
                       success tag (e.g. \"GRANT\") instead of SQLSTATE
                       0A000.
     :db-name          string — the database name this handler represents.
                       Returned by `current_database()`; defaults to
                       \"datahike\" when omitted.
     :registered-databases
                       seq of strings — all database names registered at
                       the server. Surfaced in the virtual `pg_database`
                       catalog so \\l and DatabaseMetaData enumerate the
                       server's tenancy. Typically supplied by
                       `start-server` with the keys of its registry;
                       omit for single-DB / bare-handler use.
     :dispatch-stats   optional atom; when supplied, the handler bumps
                       :fast-path-count or :full-parse-count on each
                       parse-sql invocation depending on whether
                       catalog/system-query? matched. Used by tests
                       and observability tooling to detect when an
                       upgrade silently demotes a probe to the slow
                       path.

   Supports temporal session variables:
     SET datahike.as_of = '2024-01-15T00:00:00Z'
     SET datahike.since = '2024-01-01T00:00:00Z'
     SET datahike.history = 'true'
     RESET datahike.as_of"
  ^PgWireServer$QueryHandler [conn & [{:keys [on-query db-name registered-databases initial-branch
                                              dispatch-stats
                                              on-create-database on-delete-database
                                              registry-atom
                                              tx-wrap]
                                       :as opts}]]
  (ensure-pg-schema! conn)
  (let [silently-accept (resolve-silently-accept opts)
        ;; Closure that bumps the caller-supplied stats atom by parse
        ;; result :type. Centralises the rule so the two parse-sql
        ;; call sites (parse + execute's non-cached branch) stay in
        ;; lock-step.
        bump-dispatch! (when dispatch-stats
                         (fn [parsed]
                           (let [bucket (if (= :system (:type parsed))
                                          :fast-path-count
                                          :full-parse-count)]
                             (swap! dispatch-stats update bucket (fnil inc 0)))))
        session-state (atom (cond-> {:db-name (or db-name "datahike")}
                              ;; When the factory saw `database=X:feature`
                              ;; in the StartupMessage, pin the branch so
                              ;; every query on this session reads/writes-
                              ;; read-path against it without needing an
                              ;; explicit SET.
                              initial-branch (assoc :branch initial-branch)))
        ;; session-id is unique per handler so the global lock-registry can
        ;; distinguish this connection's locks from others'.
        session-id (str (java.util.UUID/randomUUID))
        ;; Transaction state: {:in-tx? bool :aborted? bool :tx-buffer [] :speculative-db db :eid->tempid {}
        ;;                      :session-id str :owned-locks #{[table id]...}}
        tx-state (atom {:in-tx? false :aborted? false
                        :session-id session-id
                        :owned-locks #{}})
        ;; SQL-level prepared statements (PREPARE name AS ... ; EXECUTE
        ;; name(args)). Session-scoped: dropped on close / DISCARD ALL.
        ;; Keyed by lowercased name → {:sql "..." :types [...]}.
        sql-prepared (atom {})
        ;; Cursors (DECLARE name CURSOR FOR select; FETCH n FROM name;
        ;; CLOSE name). Eagerly materialized: we run the SELECT at
        ;; DECLARE and hand out slices on FETCH. Good enough for the
        ;; small-result-set ORM usage pattern — streaming cursors would
        ;; need a deeper refactor.
        cursors (atom {})
        ;; COPY-IN session state. Set when exec-copy-from-stdin returns
        ;; a copyInMode QueryResult; the wire layer then routes
        ;; CopyData / CopyDone / CopyFail messages here. Holds:
        ;;   {:decoder format-decoder
        ;;    :decode-step-fn   fn taking [decoder chunk] → [d' rows eod?]
        ;;    :decode-finalize-fn  fn taking [decoder] → [rows eod?]
        ;;    :columns ["id" "name" ...]
        ;;    :ns "users"  :table "users"
        ;;    :rows-committed long
        ;;    :pending-rows  vec of partial-batch rows
        ;;    :batch-size    long}
        copy-state (atom nil)
        ;; Names of CREATE TEMP/TEMPORARY tables created on this session.
        ;; Dropped in close() so they don't outlive the connection (PG
        ;; temp tables are session-scoped). See exec-ddl-create /
        ;; exec-ddl-drop. Not true per-session isolation — Datahike has a
        ;; single shared schema — but matches PG's session lifetime for
        ;; the sequential single-connection usage the conformance suites
        ;; exercise.
        temp-tables (atom #{})
        ;; Deferred-CC INSERT batching state (see exec-insert's batchable
        ;; branch). Set by beginBatchScope to a per-handler atom; nil when
        ;; the handler isn't being driven by a wire layer that supports
        ;; batching. The scope flag is also the signal that we're allowed
        ;; to return `(.withBatchable r)` results — direct .execute callers
        ;; never call beginBatchScope, so they always get synchronous
        ;; commits.
        ;;
        ;; Holds: {:spec-db <speculative-db>}  — the speculative-db built
        ;; up by sequential dc/with calls across batchable INSERTs. The
        ;; wire layer keeps the parallel tx-data list and CC tags; we
        ;; only need the running spec-db so the next dc/with sees prior
        ;; rows in the same scope.
        batch-state (atom nil)]
    (reify PgWireServer$QueryHandler
      (close [_]
        ;; pgwire client disconnected — equivalent to a PG backend
        ;; terminating. Release all row locks, advisory locks held by this
        ;; session, drop session-scoped temp tables, and clear transaction
        ;; state.
        (release-session-locks! session-id)
        (release-advisory-locks! session-id)
        ;; Drop CREATE TEMP tables — they live only for the session.
        ;; Best-effort: a table already dropped by hand, or one whose
        ;; create rolled back, simply has nothing to retract.
        (doseq [t @temp-tables]
          (try (drop-table-tx! conn t)
               (catch Exception _ nil)))
        (reset! temp-tables #{})
        (reset! copy-state nil)
        (reset! tx-state {:in-tx? false :aborted? false
                          :session-id session-id
                          :owned-locks #{}}))

      ;; --- COPY-IN sub-protocol callbacks --------------------------------
      ;; The wire layer in PgWireServer.java routes CopyData / CopyDone /
      ;; CopyFail messages here while the connection is in COPY-IN state
      ;; (entered when a previous execute() returned a copyInMode-flagged
      ;; QueryResult). The session lives on the :copy-state atom; ctx is
      ;; built fresh per call from this reify's closures (we don't have
      ;; the per-execute ctx available here because we're not inside an
      ;; execute() call — we're a separate Java callback).

      (copyChunk [_ chunk-bytes]
        (when-let [s @copy-state]
          (let [chunk (String. ^bytes chunk-bytes java.nio.charset.StandardCharsets/UTF_8)
                step-fn (:decode-step-fn s)
                [d' rows _eod?] (step-fn (:decoder s) chunk)
                ctx-fresh {:conn conn
                           :schema (:schema (d/db conn))
                           :copy-state copy-state}]
            (swap! copy-state assoc :decoder d')
            (when (seq rows)
              (copy-process-rows! ctx-fresh rows)))))

      (copyComplete [_]
        (let [s @copy-state]
          (if (nil? s)
            (PgWireServer$QueryResult/empty "COPY 0")
            (let [ctx-fresh {:conn conn
                             :schema (:schema (d/db conn))
                             :copy-state copy-state}
                  finalize-fn (:decode-finalize-fn s)
                  [final-rows _eod?] (finalize-fn (:decoder s))]
              (when (seq final-rows)
                (copy-process-rows! ctx-fresh final-rows))
              (try
                ;; Drain remaining pending rows
                (copy-flush-batch! ctx-fresh)
                (let [committed (:rows-committed @copy-state)]
                  (reset! copy-state nil)
                  (PgWireServer$QueryResult/empty (str "COPY " committed)))
                (catch Throwable e
                  (let [committed (:rows-committed @copy-state)]
                    (reset! copy-state nil)
                    (-> (PgWireServer$QueryResult.
                         (str "COPY failed after " committed " rows: "
                              (.getMessage e)))
                        (.withSqlstate
                         (or (some-> e ex-data :sqlstate)
                             "XX000"))))))))))

      (copyAbort [_ _reason]
        (reset! copy-state nil))

      ;; --- Deferred-CC INSERT batching ---------------------------------
      ;; The wire layer calls beginBatchScope once per connection; that
      ;; flips the per-handler batch-state atom from nil to
      ;; {:spec-db nil}. exec-insert keys on that scalar to decide
      ;; whether to take the batchable path. Direct .execute callers
      ;; (test fixtures, embedded use) never call beginBatchScope, so
      ;; batch-state stays nil and every INSERT commits synchronously
      ;; via execute-insert.
      (beginBatchScope [_]
        ;; Idempotent: only the first call activates scope; subsequent
        ;; calls leave a running spec-db (if any) intact.
        (compare-and-set! batch-state nil {:spec-db nil}))

      ;; flushBatch is called by the wire layer when it must drain
      ;; the per-connection buffer (Sync, end-of-Q, or before a non-
      ;; batchable statement). The argument is the in-arrival-order
      ;; list of tx-data payloads we returned via withBatchable.
      ;;
      ;; dc/with at append time already validated each row against
      ;; the running spec-db, so the only failure modes we can hit
      ;; here are system-level (konserve I/O) or cross-connection
      ;; races (another connection's commit landed between our
      ;; dc/with and this transact and invalidated the row's
      ;; uniqueness). Both are documented PG situations where an
      ;; Error at COMMIT is acceptable.
      (flushBatch [_ tx-data-list]
        ;; Clear the running spec-db so the next batchable INSERT
        ;; (after this flush) re-anchors on the now-committed live
        ;; db. Scope itself stays open until the connection closes.
        (when @batch-state
          (swap! batch-state assoc :spec-db nil))
        (when (and tx-data-list (pos? (.size tx-data-list)))
          (try
            (let [combined (vec (mapcat identity tx-data-list))]
              (when (seq combined)
                (d/transact conn combined)))
            nil
            (catch Exception e
              (classified-error "INSERT (batched) error: " e)))))

      ;; --- Extended Query protocol methods -------------------------------

      (parse [_ sql _param-oids]
        ;; Translate once, return the parsed map as opaque state. The
        ;; wire layer caches it under the Parse stmt name and feeds it
        ;; back via executePrepared. Note: `db` captured at parse time
        ;; drives CTE / catalog materialization; intervening DDL on the
        ;; same connection could make this stale, so named prepared
        ;; statements held across DDL may see stale schema.
        ;; `*registered-databases*` bound here so any catalog probe on
        ;; `pg_database` that lands during parse materialization sees
        ;; this server's actual registry instead of the legacy fallback.
        (binding [catalog/*registered-databases* registered-databases]
          (let [db (apply-temporal (d/db conn) session-state)
                parsed (sql/parse-sql sql (dbi/-schema db) db)
                ;; Attach the original SQL so downstream code that reads
                ;; `(:sql parsed)` (e.g. SAVEPOINT name regex) keeps
                ;; working even though parse-sql may not have set it for
                ;; non-system types.
                parsed (assoc parsed :sql sql)]
            (when bump-dispatch! (bump-dispatch! parsed))
            ;; Pre-compute result metadata for row-producing system
            ;; queries so describeResult emits a proper RowDescription
            ;; under Extended Query mode. Pure dispatch — no side
            ;; effects — so safe even for nextval / advisory-lock / etc.
            (if (and (= :system (:type parsed))
                     (:system-type parsed))
              (if-let [md (system-result-metadata parsed)]
                (assoc parsed :metadata md)
                parsed)
              parsed))))

      (describeParams [_ parsed]
        ;; Return a Java int[] of parameter OIDs so Describe('S', …)
        ;; emits a useful ParameterDescription. parse-sql infers OIDs
        ;; for the common INSERT VALUES / UPDATE SET / WHERE col OP ?
        ;; shapes; anything we can't infer falls back to 0 (unknown),
        ;; which drivers treat as 'any type' and bind as text.
        (let [n (or (:param-count parsed) 0)
              hints (:param-oids parsed)
              arr (int-array n)]
          (when (pos? n)
            (dotimes [i n]
              (aset arr i (int (or (get hints (inc i)) 0)))))
          arr))

      (describeResult [_ parsed]
        ;; Return the column metadata for a prepared SELECT without
        ;; executing. DML returns nil (NoData).
        ;;
        ;; The OID advertised here drives pgjdbc's client-side typing
        ;; decisions: setMaxFieldSize truncation rules, getObject class
        ;; (Long vs String), etc. So we resolve from the schema via
        ;; compute-schema-oids, matching the simple-query path — any -1
        ;; sentinels (no schema attr, e.g. aggregates) fall back to
        ;; OID_TEXT since we don't yet have values to infer from.
        ;;
        ;; :empty-catalog SELECTs (pg_catalog joins Datahike doesn't
        ;; implement — pg_index, pg_constraint, etc.) also land here:
        ;; the Execute handler will synthesize a zero-row result with a
        ;; column shape parsed from the SELECT list, so Describe has to
        ;; advertise that same shape. Returning NoData here and then
        ;; sending RowDescription + DataRows at Execute leaves pgjdbc's
        ;; state machine convinced the statement has no field structure
        ;; ("Received resultset tuples, but no field structure for them"
        ;; inside updatable-ResultSet metadata lookups for pgjdbc's PK
        ;; probe, which queries pg_index via that join).
        (cond
          (= :select (:type parsed))
          (let [aliases (:find-aliases parsed)
                db (d/db conn)
                resolved (compute-schema-oids parsed db)
                ;; Parse-time OIDs from oid-infer (one per find-alias,
                ;; nil for entries we can't statically type). Prefer
                ;; these over compute-schema-oids' -1 sentinel since
                ;; they cover literals, aggregates, CAST, function
                ;; calls, and arithmetic — shapes that have no schema
                ;; attribute. See datahike.pg.sql.oid-infer.
                item-oids (:select-item-oids parsed)
                oids (int-array
                      (for [i (range (count aliases))]
                        (let [schema-oid (aget ^ints resolved i)
                              item-oid (when item-oids
                                         (nth item-oids i nil))]
                          (cond
                            (not= schema-oid -1) schema-oid
                            (some? item-oid)     item-oid
                            :else                PgWireServer/OID_TEXT))))
                sources (compute-column-sources parsed db)
                qr (PgWireServer$QueryResult.
                    (into-array String aliases)
                    oids
                    (into-array (Class/forName "[Ljava.lang.String;")
                                (make-array String 0 0))
                    "SELECT 0")]
            (if sources
              (-> qr
                  (.withColumnSources (first sources) (second sources))
                  (.withColumnTypmods (nth sources 2)))
              qr))

          ;; UNION / UNION ALL / INTERSECT / EXCEPT — the executor
          ;; runs each branch separately and combines, but pgjdbc on
          ;; Extended Query needs RowDescription before the first
          ;; DataRow at Execute time. Without a Describe response
          ;; here, the client falls through to "Received resultset
          ;; tuples, but no field structure for them" the moment
          ;; Execute writes its DataRows. Use the first sub-result's
          ;; metadata: SQL set ops require all branches to agree on
          ;; arity + column types, so the first branch is canonical.
          (= :set-operation (:type parsed))
          (when-let [sub (first (:sub-results parsed))]
            (let [aliases (:find-aliases sub)
                  db (d/db conn)
                  resolved (compute-schema-oids sub db)
                  item-oids (:select-item-oids sub)
                  oids (int-array
                        (for [i (range (count aliases))]
                          (let [schema-oid (aget ^ints resolved i)
                                item-oid (when item-oids
                                           (nth item-oids i nil))]
                            (cond
                              (not= schema-oid -1) schema-oid
                              (some? item-oid)     item-oid
                              :else                PgWireServer/OID_TEXT))))]
              (PgWireServer$QueryResult.
               (into-array String aliases)
               oids
               (into-array (Class/forName "[Ljava.lang.String;")
                           (make-array String 0 0))
               "SELECT 0")))

          ;; Any system query whose parse attached :metadata (via
          ;; system-result-metadata) — covers :current-database, :now,
          ;; :version, :nextval, advisory locks, :empty-catalog,
          ;; :get-primary-keys, :get-field-metadata, :show, etc. No-row
          ;; system commands (SET, BEGIN, DECLARE CURSOR, …) have no
          ;; :metadata — they fall through to nil, i.e. NoData, which
          ;; is protocol-legal for a row-less command.
          (and (= :system (:type parsed)) (:metadata parsed))
          (describe-from-metadata (:metadata parsed))))

      (executePrepared [this parsed bound-params]
        ;; bound-params is a 1-indexed Object[] (element 0 unused). We
        ;; thread it through via dynamic bindings so the existing
        ;; `execute` dispatch body handles everything else unchanged.
        (binding [*cached-parsed* parsed
                  *cached-bound* (vec bound-params)]
          (.execute this (or (:sql parsed) ""))))

      (execute [this sql]
        ;; Use the query planner for SQL execution.
        ;; *registered-databases* bound here too so Simple Query (which
        ;; doesn't go through `parse` first) sees the registry when it
        ;; hits pg_database.
        (binding [catalog/*registered-databases* registered-databases
                  datahike.query/*force-legacy* false]
          (with-stmt-timeout (:statement-timeout @session-state)
        ;; If aborted, reject everything except ROLLBACK / ROLLBACK TO /
        ;; SAVEPOINT … — the latter two match PG behavior where a client
        ;; can ROLLBACK TO a still-valid savepoint to recover without
        ;; aborting the whole tx. SQLSTATE 25P02 is the canonical code for
        ;; "current transaction is aborted, commands ignored until end of
        ;; transaction block" (in_failed_sql_transaction). Tag tx status
        ;; 'E' so the wire layer carries the right ReadyForQuery marker.
        ;; Check aborted-tx state. Allowed while aborted: ROLLBACK,
        ;; ROLLBACK TO, and RELEASE — these let a client escape the
        ;; aborted state (either ending the tx or popping back to a
        ;; valid savepoint). classify does the routing.
            (if (and (:aborted? @tx-state)
                     (not (contains? #{:rollback :rollback-to-savepoint
                                       :release-savepoint}
                                     (:kind (cls/classify sql)))))
              (tag-tx-status
               (error-result
                "current transaction is aborted, commands ignored until end of transaction block"
                "25P02")
               tx-state)
              (try
                (when on-query (on-query sql))
                (let [;; Check for temporal SET commands first
                      temporal-set (parse-temporal-set sql)
                      timeout-ms  (when-not temporal-set (parse-statement-timeout sql))]
                  (cond
                    temporal-set
                    (let [[k v] temporal-set]
                      (if v
                        (swap! session-state assoc k
                               (case k
                                 :history   true
                                 :branch    (keyword v)
                                 :commit-id (try (java.util.UUID/fromString v)
                                                 (catch Exception _
                                                   (throw (ex-info "invalid commit UUID"
                                                                   {:error :invalid-text-representation
                                                                    :type "uuid"
                                                                    :value v}))))
                                 (parse-instant v)))
                        (swap! session-state dissoc k))
                      (empty-result "SET"))
                    timeout-ms
                    (do (if (zero? timeout-ms)
                          (swap! session-state dissoc :statement-timeout)
                          (swap! session-state assoc :statement-timeout timeout-ms))
                        (empty-result "SET"))
                    :else
                    (let [;; SQL:2011 `FOR VALID_TIME …` per-statement
                          ;; preprocessor: strip the clause and apply its
                          ;; override to apply-temporal for this query only.
                          ;; Session-state is untouched. See
                          ;; `datahike.pg.sql.temporal/preprocess`.
                          {stripped-sql :sql per-stmt-override :override}
                          (sql-temporal/preprocess sql)
                          sql stripped-sql
                          ;; FETCH from a cursor binds *snapshot-db* so the
                    ;; SELECT sees the state captured at DECLARE, not
                    ;; whatever committed since. Non-cursor paths get
                    ;; the normal live db.
                          real-db (or *snapshot-db*
                                      (apply-temporal (d/db conn) session-state per-stmt-override))
                          db (if (and (not *snapshot-db*) (:in-tx? @tx-state))
                               (or (:speculative-db @tx-state) real-db)
                               real-db)
                          ;; Use the IDB protocol method, not keyword
                          ;; access. AsOfDB / SinceDB / HistoricalDB are
                          ;; defrecords with only [origin-db time-point]
                          ;; fields; `(:schema wrapper)` returns nil
                          ;; because defrecord ILookup never reaches the
                          ;; protocol. `(dbi/-schema wrapper)` correctly
                          ;; delegates to origin-db's schema (datahike's
                          ;; `db.cljc:553`). Without this, SELECT under
                          ;; `SET datahike.as_of = …` collapses to errors
                          ;; like \"Query for unknown vars\" because
                          ;; column-info / derive-virtual-tables receive
                          ;; an empty schema map. Note: the schema
                          ;; returned is the *current* schema cached on
                          ;; the origin-db, not a true historical schema —
                          ;; columns added after the as-of timestamp are
                          ;; visible to the translator but contain no
                          ;; data at that time. A real historical schema
                          ;; would require a `schema-as-of` upstream in
                          ;; datahike (see TODO).
                          schema (dbi/-schema db)
                    ;; Prepared-statement path: reuse the Parse-time result
                    ;; and resolve ParamRef placeholders against the bound
                    ;; values decoded by the wire layer. Simple Query and
                    ;; non-parameterized prepared statements re-parse — bump
                    ;; the dispatch counter only on the re-parse branch
                    ;; (Extended-Query Parse already counted at this.parse).
                          parsed (if-let [cached *cached-parsed*]
                                   (if-let [bound *cached-bound*]
                                     ;; Re-coerce INSERT values after ParamRef
                                     ;; substitution so untyped text params
                                     ;; (e.g. node-postgres "270" → int column)
                                     ;; narrow to the column's :db/valueType.
                                     (coerce-insert-tx-data
                                      (resolve-param-refs cached bound) schema)
                                     cached)
                                   (let [p (sql/parse-sql sql schema db)]
                                     (when bump-dispatch! (bump-dispatch! p))
                                     p))
                          ;; Sibling pass to ParamRef substitution: any
                          ;; `nextval('s')` markers left in tx-data/in-args
                          ;; resolve here against the live conn (PG's
                          ;; non-transactional nextval semantics).
                          parsed (resolve-nextval-markers parsed conn)]
                      ;; ctx is the dispatch context shared across every
                      ;; per-type executor. Keys:
                      ;;   :conn           — Datahike conn for THIS db
                      ;;   :session-id     — UUID, unique per pgwire connection
                      ;;   :session-state  — atom of session GUCs / temporal vars
                      ;;   :tx-state       — atom of {:in-tx? :aborted? :tx-buffer …}
                      ;;   :sql-prepared   — atom of session-scope PREPAREd stmts
                      ;;   :cursors        — atom of session-scope DECLAREd cursors
                      ;;   :silently-accept — set of reject-kinds to swallow as success
                      ;;   :handler        — the QueryHandler reify (for re-entrancy)
                      ;;   :sql            — the original SQL string
                      ;;   :db             — speculative or live Datahike db value
                      ;;   :schema         — (dbi/-schema db); cached to avoid repeated reach-in
                      ;;   :on-create-database / :on-delete-database
                      ;;                   — operator-supplied provisioning hooks for SQL
                      ;;                     CREATE/DROP DATABASE; nil → 0A000
                      ;;   :registry-atom  — server-level {name → conn} atom; CREATE/DROP
                      ;;                     DATABASE swap! through it
                      ;;   :copy-state     — atom holding the COPY-IN session
                      ;;                     (decoder, decoder fns, target table, batch
                      ;;                     accumulator, rows-committed counter); set
                      ;;                     by exec-copy-from-stdin, mutated by
                      ;;                     copyChunk/copyComplete/copyAbort callbacks
                      (let [ctx {:conn conn
                                 :session-id session-id
                                 :session-state session-state
                                 :tx-state tx-state
                                 :sql-prepared sql-prepared
                                 :cursors cursors
                                 :copy-state copy-state
                                 :temp-tables temp-tables
                                 :batch-state batch-state
                                 :silently-accept silently-accept
                                 :handler this
                                 :sql sql
                                 :db db
                                 :schema schema
                                 :on-create-database on-create-database
                                 :on-delete-database on-delete-database
                                 :registry-atom registry-atom
                                 ;; Optional `(fn [tx-data] -> tx-data)`
                                 ;; called BEFORE every INSERT/UPDATE/
                                 ;; DELETE's d/transact. Lets framework
                                 ;; consumers (e.g. datahike-accounting)
                                 ;; inject [:db.fn/call validate tx-data]
                                 ;; so their transactor-side validators
                                 ;; fire for SQL writes too. Default:
                                 ;; identity (no wrap).
                                 ;;
                                 ;; SCOPE: fires on auto-commit paths
                                 ;; (typical psql Simple Query) — both
                                 ;; the regular and batchable INSERT
                                 ;; flows. NOT yet wrapping in-tx
                                 ;; (BEGIN/COMMIT) writes, which buffer
                                 ;; via dc/with against a speculative
                                 ;; db and flush at commit; that path
                                 ;; would need wrap firing both
                                 ;; speculatively per-statement and at
                                 ;; commit. Track-issue follow-up.
                                 :tx-wrap (or tx-wrap identity)}]
                        (case (:type parsed)
                          :system                (exec-system ctx parsed)
                          :select                (exec-select ctx parsed)
                          :insert                (exec-insert ctx parsed)
                          :update-with-recursive (exec-update-with-recursive ctx parsed)
                          :update                (exec-update ctx parsed)
                          :delete                (exec-delete ctx parsed)
                          ;; Every DDL exec-* invalidates the per-schema cache.
                          ;; PG metadata (`:pg/not-null` etc.) lives on schema-
                          ;; attribute entities but not in `(dbi/-schema db)`, so
                          ;; identity-keyed caches can't detect a constraint
                          ;; add via ALTER TABLE without an explicit bust.
                          :ddl-create            (do (invalidate-schema-cache!)
                                                     (exec-ddl-create ctx parsed))
                          :ddl-create-sequence   (do (invalidate-schema-cache!)
                                                     (exec-ddl-create-sequence ctx parsed))
                          :ddl-create-enum       (do (invalidate-schema-cache!)
                                                     (exec-ddl-create-enum ctx parsed))
                          :ddl-create-domain     (do (invalidate-schema-cache!)
                                                     (exec-ddl-create-domain ctx parsed))
                          :savepoint             (exec-savepoint ctx parsed)
                          :release-savepoint     (exec-release-savepoint ctx parsed)
                          :rollback-to-savepoint (exec-rollback-to-savepoint ctx parsed)
                          :ddl-create-index      (do (invalidate-schema-cache!)
                                                     (exec-ddl-create-index ctx parsed))
                          :ddl-alter             (do (invalidate-schema-cache!)
                                                     (exec-ddl-alter ctx parsed))
                          :ddl-drop              (do (invalidate-schema-cache!)
                                                     (exec-ddl-drop ctx parsed))
                          :ddl-drop-sequence     (do (invalidate-schema-cache!)
                                                     (exec-ddl-drop-sequence ctx parsed))
                          :set-operation         (exec-set-operation ctx parsed)
                          :full-join             (exec-full-join ctx parsed)
                          :error                 (exec-error ctx parsed)
                          ;; fallback
                          (error-result (str "Unknown parse result type: " (:type parsed))))))))
                (catch Exception e
                  (when (:in-tx? @tx-state) (swap! tx-state assoc :aborted? true))
                  (classified-error "" e))))))))))

;; ============================================================================
;; Server lifecycle
;; ============================================================================

(defn- reject-unknown-db-handler
  "QueryHandler that rejects every query with SQLSTATE 3D000. Returned
   when the StartupMessage's `database` param doesn't match any entry
   in the registry — matches PG's behaviour for a non-existent DB."
  [db-name]
  (reify PgWireServer$QueryHandler
    (close [_])
    (parse [_ _ _]
      ;; Direct throw to Java wire layer — use pg-error so .getMessage()
      ;; returns the PG-shaped string. Java's catch still emits XX000
      ;; because we don't subclass PgProtocolException, but the message
      ;; text now carries the right user-facing form.
      (throw (errors/pg-error :undefined-database {:database db-name})))
    (describeParams [_ _] (int-array 0))
    (executePrepared [_ _ _]
      (-> (PgWireServer$QueryResult. (str "database \"" db-name "\" does not exist"))
          (.withSqlstate "3D000")))
    (execute [_ _]
      (-> (PgWireServer$QueryResult. (str "database \"" db-name "\" does not exist"))
          (.withSqlstate "3D000")))))

(defn- normalize-registry
  "Accept either a single Datahike conn or a {name → conn} map and
   return a map. When given a single conn, name it `default-name` so
   `start-server` callers that don't care about multi-tenancy still get
   working `current_database()` + `pg_database` without extra ceremony."
  [conn-or-registry default-name]
  (if (map? conn-or-registry)
    conn-or-registry
    {default-name conn-or-registry}))

(defn- parse-db-name
  "Split a StartupMessage `database` value into [base-name branch].
   Accepts `base[:branch]` — `:branch` is optional and, when present,
   seeds the handler's session-state so every query against the
   connection reads/writes-read-path against that branch.

   Returns [base-name branch-keyword-or-nil]."
  [^String database]
  (if-let [idx (and database (.indexOf database ":"))]
    (if (neg? idx)
      [database nil]
      [(.substring database 0 idx)
       (keyword (.substring database (inc idx)))])
    [database nil]))

(defn make-query-handler-factory
  "Build a QueryHandlerFactory that routes on the StartupMessage's
   `database` parameter.

   registry: {name → conn} map of database name to Datahike conn.
   opts:     forwarded to make-query-handler (e.g. :on-query, :compat).

   Clients that pass `database=X` land on `(registry \"X\")`. An optional
   `:branch` suffix — `database=X:feature` — pins the connection's
   session-state to the `:feature` branch from the first query. Both
   the `current_database()` reply and the `pg_database` catalog still
   report the base name; the branch is a session-state detail.

   Unknown base-names get a handler that errors every query with 3D000
   invalid_catalog_name — matching PG's behaviour on a non-existent db.

   Intended for callers who want to wrap their own PgWireServer (e.g.
   custom host/port binding); `start-server` uses it internally."
  ^PgWireServer$QueryHandlerFactory [registry-or-atom & [opts]]
  ;; Accept either a plain map (legacy) or an atom around one (new).
  ;; Wrapping a plain map in a fresh atom gives a single, internal
  ;; mutable surface for the SQL `CREATE/DROP DATABASE` handlers and
  ;; `add-database!` / `remove-database!` to share — without
  ;; changing the call shapes that were already public.
  (let [registry-atom (if (instance? clojure.lang.Atom registry-or-atom)
                        registry-or-atom
                        (atom registry-or-atom))
        opts (assoc opts :registry-atom registry-atom)]
    (reify PgWireServer$QueryHandlerFactory
      (create [_]
        ;; No startup params available (shouldn't happen with the real
        ;; Java server, but keep it safe). Use the first registered name.
        (let [registry @registry-atom
              names (vec (keys registry))]
          (make-query-handler (get registry (first names))
                              (assoc opts :db-name (first names)
                                     :registered-databases names))))
      (create [_ startup-params]
        (let [registry @registry-atom
              names (vec (keys registry))
              raw (or (.get ^java.util.Map startup-params "database")
                      (first names))
              [requested branch] (parse-db-name raw)]
          (if-let [conn (get registry requested)]
            (make-query-handler conn
                                (cond-> (assoc opts :db-name requested
                                               :registered-databases names)
                                  branch (assoc :initial-branch branch)))
            (reject-unknown-db-handler requested)))))))

(defn start-server
  "Start a PostgreSQL wire protocol server for one or more Datahike
   connections.

   `conn-or-registry` is either:
     - a single Datahike conn (convenience — treated as
       `{\"datahike\" conn}`), or
     - a map `{name → conn}` of database name to conn. Clients route
       via the StartupMessage `database` parameter (e.g.
       `jdbc:postgresql://…/prod` lands on the `\"prod\"` conn).

   Options:
     :port      — Port to listen on (default 5432)
     :host      — Host to bind to (default \"127.0.0.1\")
     :on-query  — Callback (fn [sql-string]) for logging
     :default   — Database name used when `conn-or-registry` is a bare
                  conn (default \"datahike\"). Ignored when a map is
                  supplied.
     :on-create-database
                — Hook (fn [db-name parsed-options]) -> conn that runs
                  when a SQL client issues `CREATE DATABASE name [WITH …]`.
                  Without this hook configured, `CREATE DATABASE`
                  returns SQLSTATE 0A000 (provisioning from SQL is a
                  deployment policy decision, not a default). See
                  `datahike.pg.sql.database/db-from-template` for the
                  common template-driven helper.
     :on-delete-database
                — Hook (fn [db-name conn parsed-options]) that runs on
                  `DROP DATABASE name`. Should release the conn and
                  delete the backing store. Symmetric with
                  `:on-create-database`; without it, DROP DATABASE
                  returns 0A000.
     :database-template
                — Convenience shorthand: a partial datahike config
                  template that pg-datahike uses to build both
                  `:on-create-database` and `:on-delete-database` via
                  `db-from-template` / `db-delete-from-template`. The
                  template can interpolate `{{name}}` in string values
                  (handy for file backends with per-database paths).
                  Mutually composable with the explicit hooks; if both
                  are given, the explicit hook wins.

   Returns a map with :server (PgWireServer), :registry-atom (an atom
   holding the live {name → conn} map — mutated by SQL CREATE/DROP
   DATABASE and `add-database!` / `remove-database!`), :port, :host.

   Examples:
     ;; single DB, no SQL provisioning
     (def srv (pg/start-server conn {:port 5433}))

     ;; multi-DB, static
     (def srv (pg/start-server {\"prod\" prod-conn
                                \"staging\" staging-conn}
                               {:port 5432}))

     ;; SQL CREATE/DROP DATABASE provisioned in-memory
     (def srv (pg/start-server {}
                {:port 5432
                 :database-template {:store {:backend :memory}
                                     :schema-flexibility :write}}))

     (pg/stop-server srv)"
  [conn-or-registry & [{:keys [port host on-query default
                               on-create-database on-delete-database
                               database-template]
                        :or {port 5432 host "127.0.0.1" default "datahike"}
                        :as opts}]]
  (let [registry (normalize-registry conn-or-registry default)
        registry-atom (atom registry)
        ;; Build hooks from template if not explicitly supplied. The
        ;; explicit hook wins over the template-built one — operators
        ;; who want different create/delete behavior just pass the fns.
        on-create (or on-create-database
                      (when database-template
                        (require 'datahike.pg.sql.database)
                        ((resolve 'datahike.pg.sql.database/db-from-template)
                         database-template)))
        on-delete (or on-delete-database
                      (when database-template
                        (require 'datahike.pg.sql.database)
                        ((resolve 'datahike.pg.sql.database/db-delete-from-template)
                         database-template)))
        factory-opts (-> (select-keys opts [:on-query :compat :silently-accept
                                            :dispatch-stats :tx-wrap])
                         (cond-> on-create (assoc :on-create-database on-create)
                                 on-delete (assoc :on-delete-database on-delete)))
        factory  (make-query-handler-factory registry-atom factory-opts)
        server   (PgWireServer. (int port) ^String host factory)]
    (.start server)
    (println (str "Datahike PgWire server listening on " host ":" port
                  " — databases: " (vec (keys registry))))
    {:server server :registry-atom registry-atom :port port :host host}))

(defn add-database!
  "Add a Datahike conn to a running pg-datahike server's registry under
   `name`. Subsequent client connections with `database=name` will
   route to it. Returns the new registry contents.

   Symmetric with the SQL `CREATE DATABASE` path: `add-database!` is
   the Clojure-side knob, `:on-create-database` is the SQL-side
   knob, and they share the same atom so either source is visible
   to both."
  [server-result name conn]
  (swap! (:registry-atom server-result) assoc name conn))

(defn remove-database!
  "Remove a database from a running pg-datahike server's registry.
   Does NOT release the conn or delete the backing store — that's
   the operator's call. Returns the new registry contents."
  [server-result name]
  (swap! (:registry-atom server-result) dissoc name))

(defn databases
  "Return the current set of database names registered with the server."
  [server-result]
  (set (keys @(:registry-atom server-result))))

(defn stop-server
  "Stop a running PgWire server."
  [{:keys [^PgWireServer server]}]
  (when server
    (.stop server)
    (println "Datahike PgWire server stopped.")))
