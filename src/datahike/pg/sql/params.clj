(ns datahike.pg.sql.params
  "Prepared-statement parameter substitution and PG OID inference.

   Three concerns live together here because they all deal with the
   `?` / `$N` placeholder lifecycle:

   1. ParamRef record + `param-ref?`. Emitted by translators at Parse
      time inside tx-data / query structures, replaced by real values
      at Execute time via `substitute-params`.

   2. `*bound-params*` dynamic var: when bound to a 0-indexed vector
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
            [datahike.api :as d]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.cast :as sql-cast]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression
            AnalyticExpression CastExpression Function JdbcParameter Parenthesis NotExpression
            LongValue StringValue DoubleValue DateValue TimestampValue
            SignedExpression BinaryExpression]
           [net.sf.jsqlparser.expression.operators.relational
            Between InExpression ExpressionList]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression]
           [net.sf.jsqlparser.statement.insert Insert]
           [net.sf.jsqlparser.statement.select Select]
           [net.sf.jsqlparser.statement.update Update UpdateSet]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Identifier unquoting (local copy — also re-exported from datahike.pg.sql)

(def ^:const max-identifier-bytes
  "PostgreSQL's NAMEDATALEN - 1 (pg_config_manual.h). Identifiers longer
   than this are truncated; PG also emits a NOTICE, which we don't."
  63)

(defn fold-identifier
  "Apply PostgreSQL's case rule to an already-unquoted identifier:
   fold ASCII A-Z to lower case, and truncate to 63 bytes.

   ASCII-only, and deliberately not `.toLowerCase` of any flavour.
   PostgreSQL's `downcase_truncate_identifier` (scansup.c) only maps
   A-Z under standard encodings — verified against a UTF8 server, where
   `CREATE TABLE ÄTest` yields the relation `Ätest`: the `T` folded, the
   `Ä` did not. A Unicode-aware fold would produce `ätest` and miss it.

   Being ASCII-only also sidesteps the locale trap that would otherwise
   need guarding against: `clojure.string/lower-case` uses the default
   locale, under which Turkish folds \"ID\" to \"ıd\" and every
   identifier on the server silently mis-resolves.

   Truncation is by BYTES, without splitting a character — PG uses
   pg_mbcliplen for the same reason."
  [^String s]
  (when s
    (let [n (.length s)
          sb (StringBuilder. n)]
      (dotimes [i n]
        (let [c (.charAt s i)]
          (.append sb (if (and (>= (int c) (int \A)) (<= (int c) (int \Z)))
                        (char (+ (int c) 32))
                        c))))
      (let [folded (.toString sb)]
        (if (<= (alength (.getBytes folded java.nio.charset.StandardCharsets/UTF_8))
                max-identifier-bytes)
          folded
          (loop [end (min (.length folded) max-identifier-bytes)]
            (let [cand (subs folded 0 end)]
              (if (<= (alength (.getBytes cand java.nio.charset.StandardCharsets/UTF_8))
                      max-identifier-bytes)
                cand
                (recur (dec end))))))))))

(defn unquote-ident
  "Normalise a SQL identifier to the form used as a Datahike name.

   PostgreSQL folds UNQUOTED identifiers to lower case at parse time and
   leaves quoted ones alone, so `CREATE TABLE MixedCase` names the table
   `mixedcase` while `\"MixedCase\"` names it `MixedCase`. We used to
   store whatever was typed, which meant `SELECT ... FROM mixedcase`
   raised 42P01 for a table created as `MixedCase`, and — worse —
   `pg_class.relname` reported `MixedCase`, so a client that folds the
   name (as PostgreSQL does) and reflects it found nothing.

   The fold has to happen HERE rather than in the callers: the
   quoted/unquoted distinction lives only in the raw text JSqlParser
   hands us, and is gone the moment the quotes are stripped.

   A quoted identifier also un-escapes doubled quotes: `\"a\"\"b\"` is
   the single name `a\"b`."
  [^String s]
  (if (and s (>= (count s) 2) (str/starts-with? s "\"") (str/ends-with? s "\""))
    (str/replace (subs s 1 (dec (count s))) "\"\"" "\"")
    (fold-identifier s)))

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

(defn seek-param-ref
  "Return a ParamRef whose bound value is coerced once into the exact
   Datahike storage representation required by an indexed equality seek.

   Keep the coercion as data on the placeholder rather than as a closure so
   parsed plans remain readable and comparable.  A single SQL parameter can
   be compared with columns of different storage types; callers therefore
   create one transformed ParamRef per seek instead of mutating the raw
   parameter binding shared by every occurrence of `$N`."
  [idx value-type]
  (assoc (->ParamRef idx) ::coercion [:seek-key value-type]))

(defn transform-param-ref
  "Return `ref` with an Execute-time value transformation appended.

   INSERT translation encounters explicit casts before Bind has supplied a
   parameter value. Keeping the transformation on the ParamRef lets Parse
   remain value-free while preserving the cast's semantics when the concrete
   value arrives. A vector is used because casts can nest; transformations
   run from the innermost cast to the outermost one."
  [ref f]
  (update ref ::transforms (fnil conj []) f))

(defn resolve-param-ref
  "Resolve one ParamRef through the 1-based `fetch` function, applying any
   boundary coercion carried by the placeholder."
  [ref fetch]
  (let [value (reduce (fn [v f] (f v))
                      (fetch (:idx ref))
                      (::transforms ref))]
    (case (first (::coercion ref))
      :seek-key
      ;; SQL `col = NULL` is UNKNOWN and therefore cannot match.  A nil
      ;; Datahike pattern component means wildcard, so never pass nil into
      ;; the index seek: use the same no-match sentinel as an inexact numeric
      ;; comparison instead.
      (let [value-type (second (::coercion ref))]
        (if (or (nil? value) (= :__null__ value))
          (fns/seek-no-match-key value-type)
          (fns/seek-key value value-type)))

      value)))

(def ^:dynamic *bound-params*
  "Dynamically bound at Execute time to a 0-indexed vector (or nil if
   no params). When set, translate-expr's JdbcParameter branch resolves
   placeholders to concrete values in-line; otherwise (Parse time) it
   emits `?pN` in-param vars and records the index in ctx.

   This split lets the same translator body serve both prepared-Parse
   and re-translation-during-Execute (UPDATE/DELETE keep where-expr as
   a JSqlParser AST and re-translate on each Execute)."
  nil)

(def ^:dynamic *statement-time*
  "The wall-clock instant captured once at statement execution. SQL stable
   value functions (now/current_timestamp/current_date/current_time and
   their local variants) derive from this value so sibling calls agree.
   Nil during parsing and non-server use."
  nil)

(def ^:dynamic *scalar-subquery-cache*
  "Execution-local memo table for uncorrelated scalar subqueries.

   Parsed plans are shared by the global SQL cache, so mutable memo state must
   never be captured by a translated plan. The server binds a fresh atom for
   each statement execution; direct translator users may leave this nil, in
   which case scalar subqueries are evaluated without memoization."
  nil)

(def ^:dynamic *session-state*
  "The current pgwire connection's session-state atom while SQL is being
   translated. Session-valued expressions capture the atom (rather than a
   snapshot) so a prepared statement observes later SET/RESET changes. Nil
   for callers that use the SQL translator without a server session."
  nil)

(def ^:dynamic *parse-db*
  "Bound by parse-sql to the live db snapshot so downstream helpers
   (e.g. pg-type-of-attr) can consult Datahike for attribute metadata
   that :schema doesn't surface (:pg/type and friends). Not meant to
   flow beyond the parse phase — clear it before dispatching to the
   execute path."
  nil)

(def ^:dynamic *runtime-db*
  "The live statement snapshot while a translated query executes.

   Runtime subquery functions must read through this binding rather than a
   parse-time db captured in their closure; prepared statements otherwise
   keep returning answers from the snapshot on which they were prepared."
  nil)

(def ^:dynamic *cancel*
  "The current pgwire statement's cancellation token, or nil outside a
   cancellable execution. Translation-time work and nested queries use the
   same IDeref as the top-level Datahike query so CancelRequest and
   statement_timeout cannot be stranded at a subquery boundary."
  nil)

(defn check-cancel!
  "Raise Datahike's protocol-neutral cancellation marker when the current
   statement has been cancelled. Cheap and nil-safe for non-server callers."
  []
  (when-let [^clojure.lang.IDeref cancel *cancel*]
    (when (.deref cancel)
      (throw (ex-info "query canceled" {:datahike/canceled true})))))

(defn registered-enum-values
  "Return the declared label set for a CREATE TYPE ... AS ENUM registry
   entry, or nil when `type-name` is not a registered enum."
  ([type-name] (registered-enum-values *parse-db* type-name))
  ([db type-name]
   (when (and db type-name)
     (let [bare (-> (str type-name) (str/split #"\.") last unquote-ident)
           values (d/q '{:find [?value]
                         :in [$ ?name]
                         :where [[?enum :datahike.pg.enum/name ?name]
                                 [?enum :datahike.pg.enum/values ?value]]}
                       db bare)]
       (when (seq values)
         (into #{} (map (comp str first)) values))))))

(defn unsafe-enum-values
  "Return labels added to an existing enum in the current speculative
   transaction.  The pgwire transaction layer removes these marker facts at
   commit, so a committed database normally returns the empty set."
  [db type-name]
  (if (and db type-name
           ;; Databases created before transactional enum-label tracking do
           ;; not have this schema attr until their first ALTER TYPE. Avoid
           ;; querying an unknown attr; the ALTER transaction installs it
           ;; immediately before adding its first marker.
           (get (:schema db) :datahike.pg.enum/unsafe-values))
    (let [bare (-> (str type-name) (str/split #"\.") last unquote-ident)]
      (into #{}
            (map (comp str first))
            (d/q '{:find [?value]
                   :in [$ ?name]
                   :where [[?enum :datahike.pg.enum/name ?name]
                           [?enum :datahike.pg.enum/unsafe-values ?value]]}
                 db bare)))
    #{}))

(defn assert-enum-label-safe!
  "Raise PostgreSQL's unsafe-new-enum-value error when `label` was added to
   an already-committed enum in this transaction. Returns the string label."
  [db type-name label]
  (let [label (str label)]
    (when (contains? (unsafe-enum-values db type-name) label)
      (throw (ex-info (str "unsafe use of new value " (pr-str label)
                           " of enum type " type-name)
                      {:sqlstate "55P04"
                       :hint "New enum values must be committed before they can be used."})))
    label))

(def ^:private regtype-aliases
  {"boolean" "bool" "smallint" "int2" "integer" "int4" "int" "int4"
   "bigint" "int8" "real" "float4" "double precision" "float8"
   "decimal" "numeric" "character varying" "varchar" "character" "bpchar"})

(defn registered-type-oid
  "Resolve PostgreSQL's regtype input syntax for built-ins and the user type
   registries. Returns nil for an unknown name; callers decide whether their
   boundary should raise or retain the input."
  ([type-name] (registered-type-oid *parse-db* type-name))
  ([db type-name]
   (when type-name
     (let [bare (-> (str type-name) (str/split #"\.") last unquote-ident)
           canonical (get regtype-aliases bare bare)]
       (or (get types/pg-name->oid canonical)
           (some (fn [{:keys [name oid]}] (when (= name bare) oid))
                 (pgs/composite-types db))
           (some (fn [{:keys [name oid]}] (when (= name bare) oid))
                 (pgs/enum-types db)))))))

(defn registered-domain-spec
  "Return the persisted definition of a user domain, or nil. Optional fields
   are normalized away from Datahike's null sentinel."
  ([type-name] (registered-domain-spec *parse-db* type-name))
  ([db type-name]
   (when (and db type-name)
     (let [bare (-> (str type-name) (str/split #"\.") last unquote-ident)
           row (first
                (d/q '{:find [?base ?check-name ?check-expr ?not-null]
                       :in [$ ?name]
                       :where [[?e :datahike.pg.domain/name ?name]
                               [?e :datahike.pg.domain/base-type ?base]
                               [(get-else $ ?e :datahike.pg.domain/check-name :__null__) ?check-name]
                               [(get-else $ ?e :datahike.pg.domain/check-expr :__null__) ?check-expr]
                               [(get-else $ ?e :datahike.pg.domain/not-null false) ?not-null]]}
                     db bare))]
       (when row
         (let [[base check-name check-expr not-null] row
               present #(when (not= :__null__ %) %)]
           {:name bare :base-type base :check-name (present check-name)
            :check-expr (present check-expr) :not-null? (boolean not-null)}))))))

(defn cast-domain-value
  "Coerce and validate one value against a persisted domain definition.
   Reuses stmt/eval-check-predicate at runtime so casts and column writes do
   not grow separate CHECK semantics."
  [db spec value]
  (cond
    (or (nil? value) (= :__null__ value))
    (if (:not-null? spec)
      (throw (ex-info "domain not-null violation"
                      {:error :not-null-violation :sqlstate "23502"
                       :domain (:name spec)}))
      :__null__)

    :else
    (let [base (:base-type spec)
          enum-values (registered-enum-values db base)
          coerced (if enum-values
                    (let [label (str value)]
                      (if (contains? enum-values label)
                        (assert-enum-label-safe! db base label)
                        (throw (ex-info "invalid input value for enum"
                                        {:error :invalid-text-representation
                                         :enum? true :type base :value label}))))
                    (sql-cast/cast-scalar value base {:explicit? true}))]
      (when-let [check-expr (:check-expr spec)]
        (let [ast (try
                    (net.sf.jsqlparser.parser.CCJSqlParserUtil/parseCondExpression check-expr)
                    (catch Exception _
                      (net.sf.jsqlparser.parser.CCJSqlParserUtil/parseExpression check-expr)))
              eval-check (requiring-resolve 'datahike.pg.sql.stmt/eval-check-predicate)
              ok? (eval-check ast {(keyword "" "value") coerced} "" (:schema db))]
          (when (false? ok?)
            (throw (ex-info "domain check constraint violation"
                            {:error :check-violation
                             :constraint (or (:check-name spec)
                                             (str (:name spec) "_check"))
                             :domain (:name spec) :value coerced})))))
      coerced)))

(def ^:dynamic *parse-sql*
  "Bound by parse-sql to itself so top-level translate-* entries in
   datahike.pg.sql.stmt can seed `:parse-sql` into make-ctx without a
   cyclic require on sql.clj. Downstream expression translators call
   it to re-parse inner SQL strings for IN / EXISTS subqueries."
  nil)

(def ^:dynamic *from-bindings*
  "When bound (by build-update-tx handling UPDATE ... FROM),
   a map {alias-name → {col-name → literal}} used by the Column branches
   of translate-expr and eval-update-expr to substitute row-level values
   for references like `src.col` to the current FROM row."
  nil)

(def ^:dynamic *from-binding-oids*
  "Static PostgreSQL types for values supplied through `*from-bindings*`.

   Correlated subqueries are analyzed before any outer row exists. During
   that pass their outer references are represented by SQL NULL values, so
   the values themselves cannot carry a useful type. This parallel
   `{alias -> {column -> oid}}` map preserves each outer column's declared
   type without inventing a representative runtime value."
  nil)

(def ^:dynamic *from-source-aliases*
  "Aliases in *from-bindings* that belong to an UPDATE's FROM clause.
   The binding map can additionally contain the materialised target row
   for correlated expressions; unqualified SQL lookup must not mistake
   that implementation detail for another FROM item."
  nil)

(defn binding-column-owners
  "Return the aliases in `bindings` that expose `col-name`.

   Presence is tested with contains? so a SQL NULL remains a found value."
  [bindings col-name]
  (into [] (keep (fn [[alias row]]
                   (when (and (or (nil? *from-source-aliases*)
                                  (contains? *from-source-aliases* alias))
                              (contains? row col-name))
                     alias)))
        bindings))

(defn ambiguous-column!
  [col-name]
  (throw (ex-info (str "column reference \"" col-name "\" is ambiguous")
                  {:error :ambiguous-column :column col-name :sqlstate "42702"})))

(def ^:dynamic *lateral-outer-aliases*
  "When bound (by the correlated-LATERAL row producer), the set of OUTER
   alias names whose columns are supplied per row through
   `*from-bindings*`.

   Separate from `*from-bindings*` on purpose. Both carry per-row
   substitutions, but only a LATERAL inner needs the implicit-join
   branch suppressed: `c.tid = t.id` there is a filter against a
   constant, not a join against the relation `t`. Keying that
   suppression off `*from-bindings*` itself changed behaviour for every
   other user of it — including the correlated scalar subqueries
   asyncpg's introspection leans on — and cost 37 of its tests."
  nil)

;; ---------------------------------------------------------------------------
;; ParamRef substitution

(declare nextval-marker? call-marker?)

(defn substitute-params
  "Walk `x` replacing every ParamRef with the corresponding bound value
   from `bound` (1-indexed: `(->ParamRef 1)` → `(bound 1)` ... so `bound`
   is either a vector (we nth by idx-1) or a function idx→value).
   Leaves everything else untouched.

   The wire layer calls this at Execute time to resolve placeholders
   inside INSERT tx-data.

   Nil map values are preserved. At the INSERT boundary a present nil
   means explicit SQL NULL, whereas a missing key means the column was
   omitted and its DEFAULT must run. The constraint/default wrapper
   validates that distinction and removes nil entries immediately before
   returning Datahike tx-data.

   Identity preservation: deferred call-markers (`{:fn :nextval ...}`,
   `{:fn :now}`) pass through unchanged — same Clojure object in,
   same object out. Otherwise reduce-kv would mint new marker maps
   and resolve-nextvals! would call the underlying function multiple
   times when the same logical use appears in multiple parts of
   tx-data (e.g. a `:db.fn/call` arg AND an outer entity-map via
   `assoc`)."
  [x bound]
  (let [fetch (if (fn? bound) bound #(nth bound (dec (long %))))]
    (letfn [(walk [v]
              (cond
                (param-ref? v)   (resolve-param-ref v fetch)
                (call-marker? v) v
                (map? v)         (reduce-kv (fn [m k x]
                                              (assoc m k (walk x)))
                                            {} v)
                (vector? v)      (mapv walk v)
                (seq? v)         (map walk v)
                :else            v))]
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

(def ^:private call-fns
  "Function markers translate-* may emit for SQL constructs that must
   be re-evaluated per execute (i.e. NOT cacheable as a parse-time
   value). Resolved by `resolve-nextvals!` against a per-fn resolver."
  #{:nextval :now :eval :random-uuid :uuid-v7})

(defn call-marker?
  "True if v is a deferred function-call marker emitted by translate-*
   (`:nextval`, `:now`, and `:eval` — an arbitrary scalar expression in
   INSERT VALUES). These must survive the result-cache intact and be
   resolved per execute."
  [v]
  (and (map? v) (contains? call-fns (:fn v))))

(defn nextval-marker?
  "Back-compat alias: true only for the nextval flavour of call-marker."
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
  ([x nextval-fn] (resolve-nextvals! x nextval-fn nil))
  ([x nextval-fn eval-fn]
  ;; Identity-track: the same marker object can appear in multiple
  ;; parts of tx-data (e.g. inside a `:db.fn/call` arg AND in an
  ;; outer entity-map via `assoc`). Resolving it twice would advance
  ;; the sequence twice per logical use, or call now() twice and get
  ;; out-of-sync timestamps within one row. The IdentityHashMap
  ;; keeps marker identity → resolved-value through one walk, so
  ;; each unique marker resolves exactly once. (Callers must resolve
  ;; before any postwalk-based rebuild that would clone — and thus
  ;; un-share — the marker objects; see server.clj's dispatch order.)
  ;;
  ;; The function table here is intentionally minimal — extend by
  ;; adding to call-fns above and a clause here.
   (let [seen (java.util.IdentityHashMap.)
         resolve-marker
         (fn [v]
           (or (.get seen v)
               (let [resolved
                     (case (:fn v)
                       :nextval (nextval-fn (:seq-name v))
                       :now     (java.util.Date.)
                       :random-uuid (java.util.UUID/randomUUID)
                       :uuid-v7 (coerce/generate-uuid-v7)
                      ;; An arbitrary scalar expression in INSERT
                      ;; VALUES. Deferred rather than folded at parse
                      ;; time for the same reason `now()` is: the parse
                      ;; is cached, and a volatile function folded there
                      ;; would freeze on the first execution.
                       :eval    (if eval-fn
                                  (eval-fn (:sql v))
                                  (:sql v)))]
                 (.put seen v resolved)
                 resolved)))]
     (letfn [(walk [v]
               (cond
                 (call-marker? v) (resolve-marker v)
                 (map? v)         (reduce-kv (fn [m k x] (assoc m k (walk x)))
                                             {} v)
                 (vector? v)      (mapv walk v)
                 (seq? v)         (map walk v)
                 :else            v))]
       (walk x)))))

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

(defn ast-columns
  "Return every distinct top-level Column reachable from a JSqlParser AST.

   This deliberately uses the same guarded reflective traversal as
   `ast-param-indices`; statement-level name-resolution checks need to see
   columns inside predicates and RETURNING expressions without maintaining a
   second, inevitably incomplete list of AST node classes.  Nested SELECTs
   form their own name-resolution scope and are deliberately opaque here."
  [node]
  (let [seen (java.util.IdentityHashMap.)
        columns (transient [])]
    (letfn [(walk [n]
              (cond
                (nil? n) nil
                (.containsKey seen n) nil
                (or (string? n) (number? n) (boolean? n) (keyword? n)) nil
                :else
                (do
                  (.put seen n true)
                  (cond
                    (instance? Column n) (conj! columns n)
                    (instance? Select n) nil
                    (instance? java.lang.Iterable n) (doseq [v n] (walk v))
                    (.isArray (class n)) (when-not (.isPrimitive (.getComponentType (class n)))
                                           (doseq [v n] (walk v)))
                    (.startsWith (.getName (class n)) "net.sf.jsqlparser.")
                    (doseq [^java.lang.reflect.Method m (.getMethods (class n))
                            :let [mn (.getName m)]
                            :when (and (zero? (count (.getParameterTypes m)))
                                       (or (.startsWith mn "get") (.startsWith mn "is"))
                                       (not= "getClass" mn)
                                       (not= "getDataType" mn))]
                      (try (walk (.invoke m n (object-array 0)))
                           (catch Throwable _)))))))]
      (walk node)
      (persistent! columns))))

(defn ast-function-names
  "Return the lower-case names of functions reachable in an expression AST.

   Nested SELECTs are separate scopes and deliberately opaque. This is used
   before lowering to decide whether volatile scalar calls belong above an
   aggregate grouping step; relying on traversal order (`sum(x)+random()` vs
   `random()+sum(x)`) would otherwise change semantics."
  [node]
  (let [seen (java.util.IdentityHashMap.)
        names (transient #{})]
    (letfn [(walk [n]
              (cond
                (nil? n) nil
                (.containsKey seen n) nil
                (or (string? n) (number? n) (boolean? n) (keyword? n)) nil
                :else
                (do
                  (.put seen n true)
                  (cond
                    (instance? Function n)
                    (do (conj! names (str/lower-case (.getName ^Function n)))
                        (when-let [ps (.getParameters ^Function n)] (walk ps)))

                    (instance? AnalyticExpression n)
                    (do (conj! names (str/lower-case (.getName ^AnalyticExpression n)))
                        (doseq [^java.lang.reflect.Method m (.getMethods (class n))
                                :let [mn (.getName m)]
                                :when (and (zero? (count (.getParameterTypes m)))
                                           (.startsWith mn "get")
                                           (not= "getClass" mn)
                                           (not= "getDataType" mn)
                                           (not= "getName" mn))]
                          (try (walk (.invoke m n (object-array 0)))
                               (catch Throwable _))))

                    (instance? Select n) nil
                    (instance? java.lang.Iterable n) (doseq [v n] (walk v))
                    (.isArray (class n)) (when-not (.isPrimitive (.getComponentType (class n)))
                                           (doseq [v n] (walk v)))
                    (.startsWith (.getName (class n)) "net.sf.jsqlparser.")
                    (doseq [^java.lang.reflect.Method m (.getMethods (class n))
                            :let [mn (.getName m)]
                            :when (and (zero? (count (.getParameterTypes m)))
                                       (or (.startsWith mn "get") (.startsWith mn "is"))
                                       (not= "getClass" mn)
                                       (not= "getDataType" mn))]
                      (try (walk (.invoke m n (object-array 0)))
                           (catch Throwable _)))))))]
      (walk node)
      (persistent! names))))

;; ---------------------------------------------------------------------------
;; PG OID inference

(def ^:dynamic *declared-param-oids*
  "Map of 1-based `$N` index → PG OID for the parameters of the statement
   being translated, or nil when unknown.

   PostgreSQL treats the Parse message's declared parameter types as an
   INPUT to parse analysis (nodeFuncs.c exprType types a Param from
   paramTypes), not as something layered on afterwards -- so type
   inference over an expression containing `$N` needs them at translate
   time, not at Describe time.

   It matters for the SIMPLE protocol too, because the plan-cache rewrite
   turns every literal into a `$N` before the translator runs. Without
   this, `3 / 2` reported int8 and integer overflow could not be detected
   on all-literal arithmetic.

   A dynamic var rather than an argument: parse-sql is re-entered
   recursively for subqueries, CTEs and LATERAL bodies through the ctx's
   :parse-sql slot, and `$N` numbering is statement-global -- so
   inheritance is the correct semantics, not merely the convenient one."
  nil)

(defn pg-type-of-attr
  "Look up the :pg/type string attached to a schema ident entity.
   Datahike's (:schema db) only surfaces schema-governing attrs
   (:db/valueType, :db/cardinality, :db/unique) — custom attrs like
   :pg/type live on the ident entity itself and need a Datalog query
   to see. Reads from an explicitly-passed db, or falls back to
   *parse-db* when nil."
  [db attr]
  (when-let [d (or db *parse-db*)]
    (ffirst (d/q
             '{:find [?pt]
               :in [$ ?ident]
               :where [[?e :db/ident ?ident]
                       [?e :pg/type ?pt]]}
             d attr))))

(defn pg-typmod-of-attr
  "The `:pg/typmod` attached to a schema ident entity, by the same route
   `pg-type-of-attr` uses and for the same reason: it lives on the ident
   entity, not in Datahike's schema map.

   Reading it here rather than from a pre-enriched schema is what makes
   `numeric(p,s)` behave the same on UPDATE as on INSERT -- the enriched
   copy only ever reached the INSERT translator."
  [db attr]
  (when-let [d (or db *parse-db*)]
    (ffirst (d/q
             '{:find [?tm]
               :in [$ ?ident]
               :where [[?e :db/ident ?ident]
                       [?e :pg/typmod ?tm]]}
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
   what JDBC setObject/setString produces for the common ORM path.

   When the INSERT omits the explicit column list (`INSERT INTO t
   VALUES (?, ?, ?)`), falls back to the table's declared column order
   from `pgs/column-info` (which honours both schema entity-ID order and
   the `:datahike.pg/column-order` hint). This is what pgjdbc's
   `executeBatch` needs: setLong(1, …) wants param 1's OID at Describe
   time, and without inferred OIDs pgjdbc's resolved-type tracker
   raises `Can't change resolved type for param: N from <oid> to 0`."
  ([^Insert insert schema] (insert-param-oids insert schema nil))
  ([^Insert insert schema db]
   (try
     (let [table-ns (when-let [^Table t (.getTable insert)]
                      (unquote-ident (.getName t)))
           explicit-cols (some-> (.getColumns insert)
                                 (->> (mapv #(unquote-ident (.getColumnName ^Column %)))))
           cols (if (seq explicit-cols)
                  explicit-cols
                 ;; No column list: derive from schema. Drop the
                 ;; synthetic db_id prepended by column-info — INSERT
                 ;; VALUES is positional against user-declared columns.
                  (when table-ns
                    (let [info (pgs/column-info schema table-ns db)]
                      (when (seq info)
                        (vec (keep (fn [c]
                                     (when (not= :db/id (:attr c))
                                       (name (:attr c))))
                                   info))))))
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
     (catch Throwable _ nil))))

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
           ;; OID of a literal comparand, so `? OP <literal>` (e.g.
           ;; `$1 = 1`, with no column to borrow a type from) still
           ;; resolves the param's type. SignedExpression wraps a
           ;; negative numeric literal (`-1` → SignedExpression[LongValue]).
           literal-oid (fn literal-oid [n]
                         (cond
                           (instance? SignedExpression n)
                           (recur (.getExpression ^SignedExpression n))
                           (instance? LongValue n)
                           (let [v (.getValue ^LongValue n)]
                             (if (<= Integer/MIN_VALUE v Integer/MAX_VALUE)
                               types/oid-int4
                               types/oid-int8))
                           ;; PostgreSQL assigns an unadorned decimal literal
                           ;; NUMERIC, not FLOAT8.
                           (instance? DoubleValue n)    types/oid-numeric
                           (instance? StringValue n)    types/oid-text
                           (instance? DateValue n)      types/oid-date
                           (instance? TimestampValue n) types/oid-timestamp
                           :else nil))
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
                                (str/lower-case (str (.getDataType dt))))
                     elem-oid (or (get types/pg-name->oid type-str)
                                  (when-let [kw (get types/sql-name->elem-kw type-str)]
                                    (get types/elem-kw->oid kw))
                                  ;; User-defined composite type: report its
                                  ;; OID so the client (asyncpg) introspects it
                                  ;; and builds a composite codec instead of a
                                  ;; text one for `$1::my_composite`.
                                  (when-let [d *parse-db*]
                                    (some (fn [c]
                                            (when (= type-str (str/lower-case (:name c)))
                                              (:oid c)))
                                          (try (pgs/composite-types d) (catch Throwable _ nil)))))
                     ;; `T[]` — ColDataType carries array dimensions; map the
                     ;; element OID to its array OID (e.g. oid → oid[] 1028).
                     array? (when dt
                              (let [ad (.getArrayData dt)]
                                (and ad (pos? (.size ^java.util.List ad)))))]
                 (when elem-oid
                   (if array?
                     (get types/element-oid->array-oid elem-oid types/oid-text-array)
                     elem-oid)))))
           ;; Bind a param against (a) an explicit cast target on its
           ;; own side, or (b) the comparand column on the other side.
           bind-param! (fn [^JdbcParameter p side-with-cast comparand]
                         (if-let [oid (cast-target-oid side-with-cast)]
                           (.put result (.getIndex p) oid)
                           (cond
                             (instance? Column comparand)
                             (record-param! p ^Column comparand)
                             ;; `(a, b, …) OP $n` — the comparand is a row
                             ;; constructor (multi-element ExpressionList), so
                             ;; the param is an anonymous record (OID 2249).
                             ;; PG / asyncpg use this to detect & reject
                             ;; anonymous-composite param input.
                             (and (instance? ExpressionList comparand)
                                  (> (.size ^ExpressionList comparand) 1))
                             (.put result (.getIndex p) 2249)
                             ;; `? OP <literal>` — borrow the literal's type
                             ;; when there's no column comparand.
                             :else
                             (when-let [oid (or (cast-target-oid comparand)
                                                (literal-oid comparand))]
                               (.put result (.getIndex p) oid)))))
           ;; `col = ANY($n)` / `= ALL($n)` — JSqlParser parses the RHS as a
           ;; Function named any/all/some wrapping the parameter. Return that
           ;; JdbcParameter so the caller can type it as an ARRAY of col's
           ;; type. asyncpg's type-introspection (`oid = ANY($1::oid[])`)
           ;; depends on this.
           ;; The single arg expression inside ANY(...)/ALL(...)/SOME(...),
           ;; or nil. May itself be a CAST (`$1::oid[]`) — kept un-unwrapped
           ;; so the caller can honour the cast target before the column.
           any-all-arg (fn [n]
                         (when (instance? net.sf.jsqlparser.expression.Function n)
                           (let [f ^net.sf.jsqlparser.expression.Function n
                                 nm (str/lower-case (.getName f))]
                             (when (#{"any" "all" "some"} nm)
                               (let [exprs (some-> (.getParameters f) .getExpressions)]
                                 (when (= 1 (count exprs)) (first exprs)))))))
           ;; Bind `$n` in `col OP ANY($n)`: prefer an explicit cast on the
           ;; param (`$1::oid[]` → oid[]); otherwise the array OID of col's
           ;; type. Honouring the cast is what makes asyncpg's
           ;; `oid = ANY($1::oid[])` introspection work even though `oid` is
           ;; a catalog column with no schema-derived type.
           bind-any-arg! (fn [arg ^Column c]
                           (let [p (unwrap arg)]
                             (when (instance? JdbcParameter p)
                               (if-let [oid (cast-target-oid arg)]
                                 (.put result (.getIndex ^JdbcParameter p) oid)
                                 (let [[tns cn] (col-ns-name c)]
                                   (when-let [coid (infer-param-oid-for-column schema tns cn)]
                                     (.put result (.getIndex ^JdbcParameter p)
                                           (get types/element-oid->array-oid coid types/oid-text-array))))))))
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
                            ;; Fire whenever a side is a parameter — the
                            ;; comparand may be a Column (borrow its type)
                            ;; or a literal (`$1 = 1`; borrow the literal's
                            ;; type). bind-param! also honors a CAST on the
                            ;; param's own side first.
                            (cond
                              (instance? JdbcParameter lb) (bind-param! lb l rb)
                              (instance? JdbcParameter rb) (bind-param! rb r lb)
                              ;; col = ANY($n) — $n is an array (cast target,
                              ;; else array of col's type)
                              (and (instance? Column lb) (any-all-arg rb))
                              (bind-any-arg! (any-all-arg rb) lb)
                              (and (instance? Column rb) (any-all-arg lb))
                              (bind-any-arg! (any-all-arg lb) rb))
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
                          (do
                            ;; A standalone `CAST($n AS T)` / `$n::T` types
                            ;; the param directly from the cast target —
                            ;; e.g. `SELECT $1::int4`. (The comparison cases
                            ;; already honour a cast via bind-param!.)
                            (let [inner (unwrap (.getLeftExpression ^CastExpression n))]
                              (when (instance? JdbcParameter inner)
                                (when-let [oid (cast-target-oid n)]
                                  (.put result (.getIndex ^JdbcParameter inner) oid))))
                            (walk (.getLeftExpression ^CastExpression n)))
                         ;; COALESCE/GREATEST/LEAST/NULLIF use PostgreSQL's
                         ;; common-type selection. An unknown parameter can
                         ;; therefore borrow a concrete sibling's type, just
                         ;; as it can from the other side of an operator.
                         ;; Do not apply this to arbitrary functions: overload
                         ;; resolution may legitimately reject an unknown arg
                         ;; as ambiguous (for example abs($1)).
                          (instance? Function n)
                          (let [^Function f n
                                name (some-> (.getName f) str/lower-case)
                                args (vec (or (some-> (.getParameters f) .getExpressions)
                                              []))]
                            (when (#{"coalesce" "greatest" "least" "nullif"} name)
                              (when-let [comparand
                                         (some (fn [arg]
                                                 (let [base (unwrap arg)]
                                                   (when (and (not (instance? JdbcParameter base))
                                                              (or (cast-target-oid arg)
                                                                  (literal-oid base)))
                                                     arg)))
                                               args)]
                                (doseq [arg args]
                                  (let [base (unwrap arg)]
                                    (when (instance? JdbcParameter base)
                                      (bind-param! base arg comparand))))))
                            (doseq [arg args] (walk arg)))
                         ;; Arithmetic / concatenation / etc. — any other
                         ;; BinaryExpression (Addition, Subtraction, …). Recurse
                         ;; both sides so a nested `$n::T` or `col OP $n` deeper
                         ;; in the expression is still typed. (Comparisons,
                         ;; AND/OR are matched above; this is the fallback.)
                          (instance? BinaryExpression n)
                          (let [l (.getLeftExpression ^BinaryExpression n)
                                r (.getRightExpression ^BinaryExpression n)
                                lb (unwrap l)
                                rb (unwrap r)]
                            (when (instance? JdbcParameter lb)
                              (bind-param! lb l rb))
                            (when (instance? JdbcParameter rb)
                              (bind-param! rb r lb))
                            (walk l)
                            (walk r))))))]
       (walk expr)
       (when (pos? (.size result)) (into {} result)))
     (catch Throwable _ nil))))
