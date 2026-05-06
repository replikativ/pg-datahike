(ns datahike.pg.sql.params
  "Prepared-statement parameter substitution and PG OID inference.

   Three concerns live together here because they all deal with the
   `?` / `$N` placeholder lifecycle:

   1. ParamRef record + `param-ref?`. Emitted by translators at Parse
      time inside tx-data / query structures, replaced by real values
      at Execute time via `substitute-params`.

   2. `*bound-params*` dynamic var: when bound to a 1-indexed vector
      of resolved values, translator branches (e.g. the JdbcParameter
      expression) resolve placeholders in-line instead of emitting
      ParamRef. This lets the same translator body serve both
      prepared-Parse (returns structured AST with ParamRefs) and
      re-translation-during-Execute (returns fully-bound form).

   3. OID inference (`infer-param-oid-for-column`,
      `insert-param-oids`, `update-param-oids`, `where-param-oids`) —
      walks a JSqlParser AST, maps each placeholder index to the PG
      type OID of the column it's bound against. Consumed by
      describeParams to populate the ParameterDescription message so
      pgjdbc / psycopg2 / etc. size their binary binds correctly.

   `*parse-db*` is bound by parse-sql (higher-level) to the current
   db snapshot so OID inference can consult :pg/type metadata that
   (:schema db) doesn't surface."
  (:require [clojure.string :as str]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression
            CastExpression JdbcParameter Parenthesis NotExpression]
           [net.sf.jsqlparser.expression.operators.relational
            Between InExpression ExpressionList]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression]
           [net.sf.jsqlparser.statement.insert Insert]
           [net.sf.jsqlparser.statement.update Update UpdateSet]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Identifier unquoting (local copy — also re-exported from datahike.pg.sql)

(defn unquote-ident
  "Strip SQL double-quote delimiters from an identifier.
   PostgreSQL uses double quotes for case-sensitive or reserved-word
   identifiers: '\"MyTable\"' → 'MyTable', 'my_table' → 'my_table'"
  [^String s]
  (if (and s (str/starts-with? s "\"") (str/ends-with? s "\""))
    (subs s 1 (dec (count s)))
    s))

;; ---------------------------------------------------------------------------
;; Placeholder record + dynamic vars

(defrecord ParamRef [idx])

(defn param-ref?
  "True when `x` is a parameter placeholder emitted by translate-* during
   prepared-statement parsing. Appears in tx-data (INSERT/UPDATE VALUES)
   and nested inside parsed structures that the handler walks at Bind/
   Execute time to substitute real values."
  [x]
  (instance? ParamRef x))

(def ^:dynamic *bound-params*
  "Dynamically bound at Execute time to a 1-indexed vector (or nil if
   no params). When set, translate-expr's JdbcParameter branch resolves
   placeholders to concrete values in-line; otherwise (Parse time) it
   emits `?pN` in-param vars and records the index in ctx.

   This split lets the same translator body serve both prepared-Parse
   and re-translation-during-Execute (UPDATE/DELETE keep where-expr as
   a JSqlParser AST and re-translate on each Execute)."
  nil)

(def ^:dynamic *parse-db*
  "Bound by parse-sql to the live db snapshot so downstream helpers
   (e.g. pg-type-of-attr) can consult Datahike for attribute metadata
   that :schema doesn't surface (:pg/type and friends). Not meant to
   flow beyond the parse phase — clear it before dispatching to the
   execute path."
  nil)

(def ^:dynamic *parse-sql*
  "Bound by parse-sql to itself so top-level translate-* entries in
   datahike.pg.sql.stmt can seed `:parse-sql` into make-ctx without a
   cyclic require on sql.clj. Downstream expression translators call
   it to re-parse inner SQL strings for IN / EXISTS subqueries."
  nil)

(def ^:dynamic *from-bindings*
  "When bound (by build-update-tx handling UPDATE ... FROM (VALUES ...)),
   a map {alias-name → {col-name → literal}} used by the Column branches
   of translate-expr and eval-update-expr to substitute row-level values
   for references like `__tmp.col` to the VALUES alias."
  nil)

;; ---------------------------------------------------------------------------
;; ParamRef substitution

(defn substitute-params
  "Walk `x` replacing every ParamRef with the corresponding bound value
   from `bound` (1-indexed: `(->ParamRef 1)` → `(bound 1)` ... so `bound`
   is either a vector (we nth by idx-1) or a function idx→value).
   Leaves everything else untouched.

   The wire layer calls this at Execute time to resolve placeholders
   inside INSERT tx-data.

   Maps with nil values after substitution have those keys dissoc'd.
   An INSERT like `INSERT INTO t (a, b) VALUES (?, ?)` with
   `setString(1, null)` ends up as `{:t/a nil :t/b \"x\"}` here —
   `d/transact` rejects `[:db/add eid :t/a nil]` as `:transact/syntax`,
   but the correct PG behaviour for a nullable column is to simply
   not assert the attribute. The translate-time row-builder already
   drops nil literals (NullValue), but those land as ParamRef sentinels
   at parse time and only resolve to nil here."
  [x bound]
  (let [fetch (if (fn? bound) bound #(nth bound (dec (long %))))]
    (letfn [(walk [v]
              (cond
                (param-ref? v)  (fetch (:idx v))
                (map? v)        (reduce-kv (fn [m k x]
                                             (let [v' (walk x)]
                                               (if (nil? v')
                                                 m
                                                 (assoc m k v'))))
                                           {} v)
                (vector? v)     (mapv walk v)
                (seq? v)        (map walk v)
                :else           v))]
      (walk x))))

;; ---------------------------------------------------------------------------
;; nextval() marker + resolution
;;
;; Translators emit `{:fn :nextval :seq-name "s"}` for `nextval('s')`
;; expressions in INSERT VALUES / UPDATE SET. These markers can't be
;; resolved at Parse or Bind time — they need a live conn to advance
;; the sequence entity. So they survive substitute-params and are
;; resolved in a sibling pass right before transact, with the same
;; CAS-retry path SELECT nextval(...) uses (PG semantics: nextval is
;; non-transactional — advances stick even if the surrounding tx
;; rolls back, and concurrent advances yield distinct values).

(defn nextval-marker?
  "True if v is the placeholder map produced for a `nextval('seq')`
   expression in a VALUES / SET tuple."
  [v]
  (and (map? v) (= :nextval (:fn v)) (string? (:seq-name v))))

(defn resolve-nextvals!
  "Walk `x` replacing every `{:fn :nextval :seq-name S}` marker with the
   long produced by an actual `nextval('S')` against the live conn.
   Each call commits independently via CAS-retry — same path
   `handle-nextval` uses for `SELECT nextval(...)`. PG semantics:
   non-transactional advances; concurrent callers get distinct values.

   `nextval-fn` is `(fn [seq-name] long-or-throw)`. Decoupling the
   resolver from the conn lets server.clj wire `handle-nextval` in
   without `params.clj` taking a server.clj dependency.

   Sibling shape to `substitute-params`: leaves functions, records,
   and other opaque values alone, recurses into map values / vectors /
   seqs."
  [x nextval-fn]
  (letfn [(walk [v]
            (cond
              (nextval-marker? v) (nextval-fn (:seq-name v))
              (map? v)            (reduce-kv (fn [m k x] (assoc m k (walk x)))
                                             {} v)
              (vector? v)         (mapv walk v)
              (seq? v)            (map walk v)
              :else               v))]
    (walk x)))

;; ---------------------------------------------------------------------------
;; AST parameter-index walker

(defn has-param-marker?
  "Fast scan: does SQL contain a `?` or `$N` placeholder OUTSIDE a
   quoted string, dollar-quoted body, or comment? When false, the
   parser doesn't need to walk the AST for parameter indices — a
   real win for pg_dump-style INSERTs (literal-only) where the
   reflection-based AST walk dominated parse time.

   pgjdbc rewrites `?` to numbered `$N` before sending Parse, so the
   on-wire SQL never has `?` from a JDBC client — must detect both
   forms."
  [^String sql]
  (let [n (long (.length sql))]
    (loop [i (long 0), in-sq false, in-dq false, in-dollar false, dollar-tag nil]
      (if (>= i n)
        false
        (let [c (.charAt sql i)]
          (cond
            ;; Found `?` in non-quoted, non-comment context.
            (and (not in-sq) (not in-dq) (not in-dollar) (= c \?))
            true

            ;; Found `$N` (digit follows `$`) in non-quoted, non-
            ;; comment context. Plain `$$...$$` (no digit, no tag)
            ;; is the dollar-quote case, handled below.
            (and (not in-sq) (not in-dq) (not in-dollar)
                 (= c \$) (< (inc i) n)
                 (Character/isDigit (.charAt sql (unchecked-inc i))))
            true

            ;; Dollar-quoted string body.
            (and (not in-sq) (not in-dq) in-dollar)
            (let [taglen (long (.length ^String dollar-tag))]
              (if (and (= c \$) (<= (+ i taglen) n)
                       (= dollar-tag (subs sql i (+ i taglen))))
                (recur (+ i taglen) in-sq in-dq false nil)
                (recur (unchecked-inc i) in-sq in-dq in-dollar dollar-tag)))

            ;; Potential dollar-quote start.
            (and (not in-sq) (not in-dq) (= c \$))
            (let [tag-end (long (loop [j (unchecked-inc i)]
                                  (if (>= j n) j
                                      (let [c2 (.charAt sql j)]
                                        (if (or (Character/isLetterOrDigit c2) (= c2 \_))
                                          (recur (unchecked-inc j)) j)))))]
              (if (and (< tag-end n) (= \$ (.charAt sql tag-end)))
                (recur (unchecked-inc tag-end) in-sq in-dq true (subs sql i (unchecked-inc tag-end)))
                (recur (unchecked-inc i) in-sq in-dq in-dollar dollar-tag)))

            ;; '...'
            (and (not in-dq) (= c \'))
            (recur (unchecked-inc i) (not in-sq) in-dq in-dollar dollar-tag)

            ;; "..."
            (and (not in-sq) (= c \"))
            (recur (unchecked-inc i) in-sq (not in-dq) in-dollar dollar-tag)

            ;; -- line comment
            (and (not in-sq) (not in-dq) (not in-dollar)
                 (= c \-) (< (inc i) n) (= \- (.charAt sql (unchecked-inc i))))
            (let [eol (long (.indexOf sql (int \newline) (int i)))]
              (recur (if (neg? eol) n (unchecked-inc eol)) in-sq in-dq in-dollar dollar-tag))

            ;; /* block comment */
            (and (not in-sq) (not in-dq) (not in-dollar)
                 (= c \/) (< (inc i) n) (= \* (.charAt sql (unchecked-inc i))))
            (let [end (long (loop [j (+ i 2)]
                              (cond
                                (>= (inc j) n) n
                                (and (= \* (.charAt sql j)) (= \/ (.charAt sql (unchecked-inc j))))
                                (+ j 2)
                                :else (recur (unchecked-inc j)))))]
              (recur end in-sq in-dq in-dollar dollar-tag))

            :else
            (recur (unchecked-inc i) in-sq in-dq in-dollar dollar-tag)))))))

(defn ast-param-indices
  "Recursively walk a JSqlParser AST, returning a sorted set of
   1-based parameter indices (`?` / `$N` placeholders).

   Uses reflection to traverse any node's zero-arg getters. This is
   called once per Parse to determine how many parameters a prepared
   statement expects — the cost is bounded by AST size, which is tiny
   compared to query execution."
  [node]
  (let [seen (java.util.IdentityHashMap.)
        walk (fn walk [n]
               (cond
                 (nil? n) #{}
                 (instance? JdbcParameter n) #{(.getIndex ^JdbcParameter n)}
                 (.containsKey seen n) #{}
                 (or (string? n) (number? n) (boolean? n) (keyword? n)) #{}
                 :else
                 (do (.put seen n true)
                     (cond
                       (instance? java.lang.Iterable n)
                       (reduce (fn [s v] (into s (walk v))) #{} n)
                       (.isArray (class n))
                       (if (.isPrimitive (.getComponentType (class n)))
                         #{}
                         (reduce (fn [s v] (into s (walk v))) #{} n))
                       ;; JSqlParser AST node: scan no-arg getters
                       (.startsWith (.getName (class n)) "net.sf.jsqlparser.")
                       (let [ms (.getMethods (class n))]
                         (reduce
                          (fn [acc ^java.lang.reflect.Method m]
                            (let [mn (.getName m)]
                              (if (and (zero? (count (.getParameterTypes m)))
                                       (or (.startsWith mn "get") (.startsWith mn "is"))
                                       (not (= "getClass" mn))
                                       (not (= "getDataType" mn)))
                                (try (into acc (walk (.invoke m n (object-array 0))))
                                     (catch Throwable _ acc))
                                acc)))
                          #{} ms))
                       :else #{}))))]
    (into (sorted-set) (walk node))))

;; ---------------------------------------------------------------------------
;; PG OID inference

(defn- pg-type-of-attr
  "Look up the :pg/type string attached to a schema ident entity.
   Datahike's (:schema db) only surfaces schema-governing attrs
   (:db/valueType, :db/cardinality, :db/unique) — custom attrs like
   :pg/type live on the ident entity itself and need a Datalog query
   to see. Reads from an explicitly-passed db, or falls back to
   *parse-db* when nil."
  [db attr]
  (when-let [d (or db *parse-db*)]
    (ffirst ((requiring-resolve 'datahike.api/q)
             '{:find [?pt]
               :in [$ ?ident]
               :where [[?e :db/ident ?ident]
                       [?e :pg/type ?pt]]}
             d attr))))

(defn infer-param-oid-for-column
  "Given a schema and a (table-namespace, column-name), return the PG
   OID that matches the attribute's :db/valueType, or nil if we don't
   know. Used by describeParams to populate ParameterDescription so
   drivers can size buffers correctly for typed INSERT/UPDATE/WHERE
   placeholders.

   :pg/type on the attr — recorded by translate-create-table for types
   that don't have a 1:1 Datahike mapping (date/time/timestamp all
   collapse to :db.type/instant; jsonb vs json both use :db.type/string)
   — takes precedence so we round-trip the original SQL type. Without
   this, pgjdbc Describes a DATE column's param as `timestamp` (1114)
   and rejects subsequent binds as \"Can't change resolved type for
   param: 1 from 1082 to 1114\".

   `db` is optional — pass nil and the :pg/type refinement is skipped.
   Accepts schema either as the map {ident → props} returned by
   `pgs/schema-of`, or as a seq of `{:db/ident ... :db/valueType ...}`
   entries (the `:db/add` transaction form)."
  ([schema table-ns col-name] (infer-param-oid-for-column schema table-ns col-name nil))
  ([schema table-ns col-name db]
   (when (and table-ns col-name schema)
     (let [attr (keyword table-ns col-name)
           vt   (or (when (map? schema)
                      (:db/valueType (get schema attr)))
                    (when (coll? schema)
                      (some (fn [a] (when (= (:db/ident a) attr) (:db/valueType a)))
                            schema)))
           ;; :pg/type is either attached by translate-create-table in
           ;; the current tx (still a :db/add-form vector) or already
           ;; committed on the ident entity (needs a Datalog lookup).
           pg-type (or (when (coll? schema)
                         (some (fn [a] (when (= (:db/ident a) attr) (:pg/type a)))
                               schema))
                       (pg-type-of-attr db attr))]
       (or (get types/pg-name->oid pg-type)
           (get types/dh-type->oid vt))))))

(defn insert-param-oids
  "Walk an INSERT AST: for each `VALUES (..., ?, ...)` row, positional
   column i → attribute type → PG OID. Returns a map {param-index → oid}.

   Only covers the flat single-row / multi-row VALUES case — which is
   what JDBC setObject/setString produces for the common ORM path."
  [^Insert insert schema]
  (try
    (let [table-ns (when-let [^Table t (.getTable insert)]
                     (unquote-ident (.getName t)))
          cols (some-> (.getColumns insert)
                       (->> (mapv #(unquote-ident (.getColumnName ^Column %)))))
          select (.getSelect insert)
          col-oid (fn [col] (infer-param-oid-for-column schema table-ns col))]
      (when (and table-ns (seq cols)
                 (instance? net.sf.jsqlparser.statement.select.Values select))
        (let [rows (.getExpressions ^net.sf.jsqlparser.statement.select.Values select)
              result (java.util.HashMap.)]
          (doseq [row (seq (if (instance? net.sf.jsqlparser.expression.operators.relational.ExpressionList rows)
                             [rows] rows))]
            (let [exprs (seq (if (instance? net.sf.jsqlparser.expression.operators.relational.ExpressionList row)
                               row [row]))]
              (doseq [[i e] (map vector (range) exprs)]
                (when (and (instance? JdbcParameter e) (< i (count cols)))
                  (when-let [oid (col-oid (nth cols i))]
                    (.put result (.getIndex ^JdbcParameter e) oid))))))
          (when (pos? (.size result)) (into {} result)))))
    (catch Throwable _ nil)))

(defn update-param-oids
  "Walk an UPDATE AST: for each SET col = ?, map param index to the
   column attribute's PG OID."
  [^Update update schema]
  (try
    (let [^Table t (.getTable update)
          table-ns (when t (unquote-ident (.getName t)))
          col-oid (fn [col] (infer-param-oid-for-column schema table-ns col))
          sets (.getUpdateSets update)
          result (java.util.HashMap.)]
      (doseq [^UpdateSet us (or sets [])]
        (let [cols (.getColumns us)
              vals (.getValues us)]
          (when (and cols vals)
            (doseq [[^Column c v] (map vector cols vals)]
              (when (instance? JdbcParameter v)
                (when-let [oid (col-oid (unquote-ident (.getColumnName c)))]
                  (.put result (.getIndex ^JdbcParameter v) oid)))))))
      (when (pos? (.size result)) (into {} result)))
    (catch Throwable _ nil)))

(defn collect-table-aliases
  "Given a FROM item and a sequence of JOINs, build a map
   `{alias-name → real-table-name}`. Tables without an alias still get
   an entry mapping the table name to itself (so lookup is uniform)."
  [from-item joins]
  (let [add-pair (fn [m ^Table t alias-obj]
                   (when t
                     (let [real (unquote-ident (.getName t))
                           alias-name (when alias-obj
                                        (unquote-ident
                                         (.getName ^net.sf.jsqlparser.expression.Alias
                                          alias-obj)))]
                       (cond-> m
                         real (assoc real real)
                         alias-name (assoc alias-name real)))))]
    (cond->
     (if (instance? Table from-item)
       (add-pair {} ^Table from-item (.getAlias ^Table from-item))
       {})
      (seq joins)
      (as-> m
            (reduce (fn [acc ^net.sf.jsqlparser.statement.select.Join j]
                      (let [item (.getRightItem j)]
                        (if (instance? Table item)
                          (add-pair acc ^Table item (.getAlias ^Table item))
                          acc)))
                    m joins)))))

(defn where-param-oids
  "Walk an expression tree (a WHERE clause) and for each comparison
   `col = ?` / `? = col` / `col IN (?,?)` / `col BETWEEN ? AND ?`,
   map the `?` param index to the column's PG OID. Best-effort.

   `default-table-ns` is used when a column has no explicit table
   qualifier. `aliases` is an optional `{alias-name → real-table-name}`
   map (derived by the caller from FROM/JOIN clauses) so `JOIN t a ON
   a.col = ?` resolves `a` to `t`'s real schema."
  ([expr schema default-table-ns]
   (where-param-oids expr schema default-table-ns {}))
  ([expr schema default-table-ns aliases]
   (try
     (let [seen (java.util.IdentityHashMap.)
           result (java.util.HashMap.)
           col-ns-name (fn [^Column c]
                         (let [cn (unquote-ident (.getColumnName c))
                               tab (when-let [t (.getTable c)] (.getName ^Table t))
                               tab-unq (when tab (unquote-ident tab))
                              ;; Alias → real table if known, else the
                              ;; identifier itself (handles both cases:
                              ;; `person.col` where `person` is the
                              ;; table, and `p.col` where `p` is an alias).
                               tns (or (get aliases tab-unq)
                                       tab-unq
                                       default-table-ns)]
                           [tns cn]))
           record-param! (fn [^JdbcParameter p ^Column c]
                           (let [[tns cn] (col-ns-name c)]
                             (when-let [oid (infer-param-oid-for-column schema tns cn)]
                               (.put result (.getIndex p) oid))))
           ;; Strip CAST/Parenthesis wrappers so we can see the
           ;; Column / JdbcParameter inside. Returns the inner node.
           unwrap (fn unwrap [n]
                    (cond
                      (instance? CastExpression n)
                      (recur (.getLeftExpression ^CastExpression n))
                      (instance? Parenthesis n)
                      (recur (.getExpression ^Parenthesis n))
                      :else n))
           ;; If `n` (or any Parenthesis-wrapped descendant) is a
           ;; `CAST(? AS T)`, return the cast target's OID so callers
           ;; can record an explicit param→OID mapping. PG semantics:
           ;; the cast target overrides the comparand column's type
           ;; for ParameterDescription. Resolves both canonical names
           ;; (`int4`, `text`) and SQL aliases (`int`, `integer`,
           ;; `bigint`, …) via sql-name→elem-kw.
           cast-target-oid
           (fn cast-target-oid [n]
             (cond
               (instance? Parenthesis n)
               (recur (.getExpression ^Parenthesis n))
               (instance? CastExpression n)
               (let [ce ^CastExpression n
                     dt (.getColDataType ce)
                     type-str (when dt
                                (str/lower-case (str (.getDataType dt))))]
                 (or (get types/pg-name->oid type-str)
                     (when-let [kw (get types/sql-name->elem-kw type-str)]
                       (get types/elem-kw->oid kw))))))
           ;; Bind a param against (a) an explicit cast target on its
           ;; own side, or (b) the comparand column on the other side.
           bind-param! (fn [^JdbcParameter p side-with-cast comparand-col]
                         (if-let [oid (cast-target-oid side-with-cast)]
                           (.put result (.getIndex p) oid)
                           (when (instance? Column comparand-col)
                             (record-param! p ^Column comparand-col))))
           walk (fn walk [n]
                  (cond
                    (nil? n) nil
                    (.containsKey seen n) nil
                    (or (string? n) (number? n) (boolean? n) (keyword? n)) nil
                    :else
                    (do (.put seen n true)
                        (cond
                         ;; col OP ?  or  ? OP col — also accept either
                         ;; side wrapped in CAST(... AS T) or parens.
                          (instance? net.sf.jsqlparser.expression.operators.relational.ComparisonOperator n)
                          (let [l (.getLeftExpression
                                   ^net.sf.jsqlparser.expression.operators.relational.ComparisonOperator n)
                                r (.getRightExpression
                                   ^net.sf.jsqlparser.expression.operators.relational.ComparisonOperator n)
                                lb (unwrap l) rb (unwrap r)]
                            (cond
                              (and (instance? Column lb) (instance? JdbcParameter rb))
                              (bind-param! rb r lb)
                              (and (instance? Column rb) (instance? JdbcParameter lb))
                              (bind-param! lb l rb))
                            (walk l) (walk r))

                         ;; col IN (?, ?, ...) — RHS items may also be
                         ;; CAST-wrapped (`IN (CAST(? AS INT), ?)`).
                          (instance? InExpression n)
                          (let [l (.getLeftExpression ^InExpression n)
                                rhs (.getRightExpression ^InExpression n)
                                lb (unwrap l)]
                            (when (instance? Column lb)
                              (when (instance? ExpressionList rhs)
                                (doseq [e rhs]
                                  (let [eb (unwrap e)]
                                    (when (instance? JdbcParameter eb)
                                      (bind-param! eb e lb))))))
                            (walk l) (walk rhs))

                         ;; col BETWEEN ? AND ?
                          (instance? Between n)
                          (let [l (.getLeftExpression ^Between n)
                                s (.getBetweenExpressionStart ^Between n)
                                e (.getBetweenExpressionEnd ^Between n)
                                lb (unwrap l) sb (unwrap s) eb (unwrap e)]
                            (when (instance? Column lb)
                              (when (instance? JdbcParameter sb) (bind-param! sb s lb))
                              (when (instance? JdbcParameter eb) (bind-param! eb e lb)))
                            (walk l) (walk s) (walk e))

                          (instance? AndExpression n)
                          (do (walk (.getLeftExpression ^AndExpression n))
                              (walk (.getRightExpression ^AndExpression n)))
                          (instance? OrExpression n)
                          (do (walk (.getLeftExpression ^OrExpression n))
                              (walk (.getRightExpression ^OrExpression n)))
                          (instance? Parenthesis n)
                          (walk (.getExpression ^Parenthesis n))
                          (instance? NotExpression n)
                          (walk (.getExpression ^NotExpression n))
                          (instance? CastExpression n)
                          (walk (.getLeftExpression ^CastExpression n))))))]
       (walk expr)
       (when (pos? (.size result)) (into {} result)))
     (catch Throwable _ nil))))
