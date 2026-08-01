(ns datahike.pg.sql
  "SQL → Datahike Datalog translator.

   Parses SQL strings using JSqlParser and translates the AST into Datahike
   Datalog queries that can be executed by `datahike.api/q`.

   The core mapping: attribute namespace prefixes become virtual table names.
     :person/name  → table 'person', column 'name'
     :person/age   → table 'person', column 'age'

   Main entry points:
     (parse-sql sql schema)  → {:type :select :query {...} :args [...]}
                              | {:type :insert :tx-data [...]}
                              | {:type :system :result QueryResult}
                              | {:type :error :message str}"
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.query :as dq]
            [datahike.db.interface :as dbi]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.sql.copy :as copy]
            [datahike.pg.sql.database :as database]
            [datahike.pg.sql.types :as user-types]
            [datahike.pg.sql.rewrite :as rw]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.catalog :as catalog]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.ddl :as ddl]
            [datahike.pg.sql.expr :as expr]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.oid-infer :as oid]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.sql.template :as template]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.statement.select
            PlainSelect SelectItem Join
            ParenthesedSelect SetOperationList
            UnionOp IntersectOp ExceptOp]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression
            BooleanValue Function LongValue DoubleValue StringValue NullValue
            CaseExpression CastExpression ArrayConstructor JdbcParameter]
           [net.sf.jsqlparser.statement.create.table CreateTable ColumnDefinition]
           [net.sf.jsqlparser.statement.create.sequence CreateSequence]
           [net.sf.jsqlparser.statement.insert Insert]
           [net.sf.jsqlparser.statement.update Update UpdateSet]
           [net.sf.jsqlparser.statement.delete Delete]
           [net.sf.jsqlparser.statement.drop Drop]
           [net.sf.jsqlparser.statement.alter Alter AlterExpression]
           [net.sf.jsqlparser.statement SavepointStatement RollbackStatement Commit]
           [net.sf.jsqlparser.statement.create.index CreateIndex]))

(set! *warn-on-reflection* true)

;; ============================================================================
;; Re-exports for external callers (handler, tests)
;; ============================================================================
;; Record + placeholder fns moved to datahike.pg.sql.params. Re-export the
;; non-dynamic names so `sql/...` references keep resolving; callers that
;; need the dynamic vars (*bound-params*, *parse-db*) must use `params/...`
;; directly — `binding` doesn't follow Var aliases across namespaces.

(def ParamRef datahike.pg.sql.params.ParamRef)
(def ->ParamRef params/->ParamRef)
(def param-ref? params/param-ref?)
(def substitute-params params/substitute-params)
(def nextval-marker? params/nextval-marker?)
(def resolve-nextvals! params/resolve-nextvals!)
(def ^:private unquote-ident params/unquote-ident)

;; Aggregate + scalar fns moved to datahike.pg.sql.fns. Re-export at the old
;; `datahike.pg.sql/...` names so the qualified symbols emitted by the
;; translator (and cached in client prepared statements) keep resolving.
(def filter-sum            fns/filter-sum)
(def filter-sum-numeric    fns/filter-sum-numeric)
(def filter-avg            fns/filter-avg)
(def filter-avg-numeric    fns/filter-avg-numeric)
(def filter-min            fns/filter-min)
(def filter-max            fns/filter-max)
(def filter-count          fns/filter-count)
(def filter-count-distinct fns/filter-count-distinct)
(def filter-variance-samp  fns/filter-variance-samp)
(def filter-stddev-samp    fns/filter-stddev-samp)
(def filter-corr           fns/filter-corr)
(def filter-array-agg      fns/filter-array-agg)
(def filter-array-agg-ordered      fns/filter-array-agg-ordered)
(def filter-array-agg-ordered-desc fns/filter-array-agg-ordered-desc)
(def filter-percentile-cont fns/filter-percentile-cont)
(def filter-percentile-disc fns/filter-percentile-disc)
(def filter-mode            fns/filter-mode)
(def sql-+   fns/sql-+)
(def sql--   fns/sql--)
(def sql-*   fns/sql-*)
(def sql-div fns/sql-div)
(def sql-mod fns/sql-mod)
(def null-safe fns/null-safe)

;; Context primitives moved to datahike.pg.sql.ctx. Re-export at old names
;; so `#'sql/...` reach-ins from server.clj keep resolving.
(def make-ctx              ctx/make-ctx)
(def fresh-var!            ctx/fresh-var!)
(def entity-var!           ctx/entity-var!)
(def add-clause!           ctx/add-clause!)
(def col-var!              ctx/col-var!)
(def materialize-arg!      ctx/materialize-arg!)
(def null-guard-clauses    ctx/null-guard-clauses)
(def make-columns-optional! ctx/make-columns-optional!)
(def collect-vars          ctx/collect-vars)
(def resolve-column        ctx/resolve-column)
(def resolve-inherited-attr ctx/resolve-inherited-attr)

;; Catalog extension seam + system-query detection moved to
;; datahike.pg.sql.catalog. Re-export names that are reached in from
;; datahike.pg (public facade) or server.clj (system-query fast path).
(def register-catalog-table!     catalog/register-catalog-table!)
(def unregister-catalog-table!   catalog/unregister-catalog-table!)
(def extract-empty-catalog-shape catalog/extract-empty-catalog-shape)
(def system-query?               catalog/system-query?)

;; Expression + predicate translation moved to datahike.pg.sql.expr.
;; Re-export translate-predicate — server.clj reaches it via
;; `#'sql/translate-predicate` from its build-update-tx path.
(def translate-predicate expr/translate-predicate)

;; Statement-level translation (SELECT / INSERT / UPDATE / DELETE / CTE)
;; moved to datahike.pg.sql.stmt. Re-export:
;;   - eval-check-predicate / eval-update-expr — public reach-ins from
;;     server.clj for CHECK + UPDATE row-level evaluation.
;;   - coerce-insert-value — server.clj reaches via
;;     `#'sql/coerce-insert-value` at INSERT row build time.
;;   - translate-select / translate-insert / translate-update /
;;     translate-delete / select-item-alias — referenced by parse-sql
;;     dispatch below. Local private aliases avoid qualifying each site.
(def eval-check-predicate stmt/eval-check-predicate)
(def eval-update-expr     stmt/eval-update-expr)
(def coerce-insert-value stmt/coerce-insert-value)
(def ^:private translate-select    stmt/translate-select)
(def ^:private translate-insert    stmt/translate-insert)
(def ^:private translate-update    stmt/translate-update)
(def ^:private translate-delete    stmt/translate-delete)
(def ^:private select-item-alias   stmt/select-item-alias)

;; ============================================================================
;; Forward declarations
;; ============================================================================

;; parse-sql is called by ctx/make-ctx sites (passed through ctx's
;; :parse-sql slot so expression translators can recurse on subqueries)
;; before its own defn runs in load order. Resolves at run time.
(declare parse-sql)

(def ^:private catalog-cache-max-entries
  "Soft bound on the global catalog cache. A long-running server that
   sees many DDLs would otherwise accumulate stale enriched-db values
   indefinitely. LRU-evict oldest entries past this threshold."
  256)

(def ^:private global-catalog-cache
  "Server-wide cache: `{[schema-content-hash sorted-catalog-names] →
   enriched-db}`. Shared across every connection — Odoo's ~5000-row
   information_schema.columns now only builds once per user schema,
   not once per new connection.

   Bounded LRU: a synchronized LinkedHashMap with access-order so the
   least-recently-hit entry gets evicted when size > max. Content
   hash, not identity: two Datahike DBs with the same user schema
   (different in-memory IDs but same ident/type/card/unique shape)
   share the cache entry. Schema evolution via DDL changes the hash
   and naturally invalidates."
  (java.util.Collections/synchronizedMap
   (proxy [java.util.LinkedHashMap] [16 0.75 true]
     (removeEldestEntry [_]
       (> (.size ^java.util.LinkedHashMap this)
          catalog-cache-max-entries)))))

(def ^:dynamic *catalog-cache*
  "Tests can rebind this to an isolated `java.util.Map` to keep their
   data from polluting the global cache (or vice versa). Nil disables
   caching entirely. Defaults to the server-wide cache."
  global-catalog-cache)

(defn invalidate-catalog-cache!
  "Clear the server-wide enriched-db catalog cache. Called from every DDL
   exec branch (via invalidate-schema-cache!). The cache key is only the
   user-schema hash + catalog-table-names, which does NOT change when a
   CREATE TYPE / ENUM / DOMAIN adds a registry *entity* (new datoms under
   pre-existing idents) or when ALTER changes a column's typmod — so those
   would otherwise serve stale catalog rows (e.g. a 2nd composite invisible
   in pg_type). DDL is rare, so a full clear is the simplest correct fix."
  []
  (.clear ^java.util.Map global-catalog-cache))

(defn- cache-get [^java.util.Map cache k]
  (when cache (.get cache k)))

(defn- cache-put! [^java.util.Map cache k v]
  (when cache (.put cache k v))
  v)

(defn enrich-db-with-catalogs
  "Materialise the given catalog tables' schema + data on top of `db`,
   returning the enriched db (its `:schema` carries the catalog attrs).
   Returns `db` unchanged when `catalog-names` is empty.

   Cached in the server-wide LRU by [user-schema-hash sorted-names]; the
   cache is DDL-invalidated (invalidate-catalog-cache!). Callable at BOTH
   parse time (to translate against the catalog schema) and execute time
   (so a prepared catalog statement re-resolves fresh catalog rows against
   the current db instead of a stale parse-time snapshot — real PG re-plans
   on catalog change). `schema` is `db`'s user schema."
  [db schema catalog-names]
  (if (empty? catalog-names)
    db
    (let [existing (dbi/-schema db)
          ;; Skip catalogs already materialised on `db` (its row-marker attr
          ;; is present). Re-asserting a catalog's schema would fail — datahike
          ;; rejects "updating" an existing schema attr's custom metadata
          ;; (e.g. :pg/type on :pg_type/typtype) — and re-asserting its data
          ;; would duplicate rows. This makes nested enrichment idempotent:
          ;; a derived table / correlated subquery over pg_* runs against a db
          ;; whose outer scope already enriched the same catalogs.
          sorted-names (sort (remove #(contains? existing (pgs/row-marker-attr %))
                                     catalog-names))
          cache *catalog-cache*
          cache-key [(hash existing) sorted-names]]
      (cond
        (empty? sorted-names) db
        :else
        (or (cache-get cache cache-key)
            (let [combined-schema (vec (mapcat catalog/catalog-schema-for sorted-names))
                  combined-data (vec (mapcat #(catalog/catalog-data-for % schema db)
                                             sorted-names))
                  spec-db (d/db-with db combined-schema)
                  built (if (seq combined-data) (d/db-with spec-db combined-data) spec-db)]
              (cache-put! cache cache-key built)))))))

;; ============================================================================
;; parse-sql result cache
;; ============================================================================
;;
;; JSqlParser AST is the dominant per-row cost in parse-sql (~0.86 ms /
;; ~73% of total). Per-row pg-datahike translation adds another ~0.3 ms.
;; A workload that re-runs identical SQL — pgjdbc unnamed prepared
;; statements (default `prepareThreshold=5`), repeated `INSERT INTO log
;; VALUES (?)` calls, ORM-generated SELECT-by-id — pays this cost on
;; every Parse message.
;;
;; The cache key is `[sql schema-hash]`. The schema-hash captures
;; everything translation depends on: column types, identity unique-
;; ness, FK metadata. Two connections with the same user schema share
;; entries. DDL changes the schema → new hash → cache miss.
;;
;; We do NOT cache results that depend on transient state:
;;   - :type :system            (current_user, now(), session GUCs)
;;   - :type :error             (transient parse failures shouldn't pin)
;;   - :enriched-db tagged maps (catalog data depends on db rows, not
;;                               just schema; the enriched-db itself is
;;                               cached separately by *catalog-cache*)
;;   - bound-param substitution (callers Bind via resolve-param-refs
;;                               on the cached map; we cache the
;;                               un-substituted shape)

(def ^:private parse-cache-max-entries
  "Soft bound on the parse-sql result cache. 4k entries comfortably
   covers Odoo / Metabase shapes (≈ a few hundred unique queries per
   schema) plus headroom for ORM-generated INSERT/UPDATE shapes."
  4096)

(def ^:private global-parse-cache
  (java.util.Collections/synchronizedMap
   (proxy [java.util.LinkedHashMap] [16 0.75 true]
     (removeEldestEntry [_]
       (> (.size ^java.util.LinkedHashMap this)
          parse-cache-max-entries)))))

(def ^:dynamic *parse-cache*
  "Server-wide cache for parse-sql results. Tests can rebind to an
   isolated map; nil disables caching entirely."
  global-parse-cache)

(defn invalidate-parse-cache!
  "Clear the server-wide parse-sql result cache. Called from every DDL
   exec branch (via server/invalidate-schema-cache!). The cache key is
   `[sql (hash schema)]`, but translation also depends on the `:pg/*`
   metadata stored on ident *entities* (NOT NULL, CHECK, FK, defaults,
   typmod) which does not appear in `(dbi/-schema db)` — so an
   `ALTER TABLE … ADD CHECK / SET NOT NULL / ALTER COLUMN TYPE` leaves
   the hash unchanged and would keep serving parse results translated
   against the old constraints. The AST cache is untouched: JSqlParser
   output depends only on the SQL text."
  []
  (.clear ^java.util.Map global-parse-cache))

;; ----------------------------------------------------------------------------
;; JSqlParser AST cache
;;
;; JSqlParser is the dominant per-call cost in parse-sql (~73%).
;; Templated SQL strings are stable across rows of the same INSERT
;; shape, so caching the AST turns every-row-but-the-first into a
;; sub-µs lookup. Translation (~0.3 ms / call) still runs per call,
;; which lets us bind `*bound-params*` to per-row literals and reach
;; the inline-resolution branch in `expr/extract-value` — that's
;; what makes the lexical-template fast path correct (translate-insert
;; never produces ParamRefs inside `:db.fn/call` closure captures).
;;
;; Thread-safety: JSqlParser AST is read-only after parse for the
;; usages we have (all `.getXxx` / instance-of branches, no setters).
;; Concurrent translation against the same cached AST is safe.

(def ^:private ast-cache-max-entries 4096)

(def ^:private max-cacheable-sql-length
  "Statements longer than this are parsed but never cached. The parse
   and AST caches key on the (templated) SQL string and are bounded by
   ENTRY count, not bytes — a bulk `INSERT … VALUES (…)×5000` templates
   to a multi-MB string that would sit in the LRU as key + JSqlParser
   AST + parsed tx-data. 4096 such entries is a multi-GB heap. Bulk
   statements are exactly the ones that don't repeat verbatim, so
   skipping them costs ~nothing; the ORM/driver shapes the caches exist
   for are a few KB at most."
  65536)

(defn- cacheable-sql-size? [^String sql]
  (<= (.length sql) (int max-cacheable-sql-length)))

(def ^:private global-ast-cache
  (java.util.Collections/synchronizedMap
   (proxy [java.util.LinkedHashMap] [16 0.75 true]
     (removeEldestEntry [_]
       (> (.size ^java.util.LinkedHashMap this) ast-cache-max-entries)))))

(def ^:dynamic *ast-cache*
  "Server-wide JSqlParser AST cache, keyed on the preprocessed SQL
   string. Tests can rebind to an isolated map; nil disables caching."
  global-ast-cache)

(defn- ast-parse
  "JSqlParser parse with the result memoised in `*ast-cache*` keyed on
   the preprocessed SQL. Falls through to a direct parse when caching
   is disabled."
  [^String preprocessed]
  (if-let [cache (when (cacheable-sql-size? preprocessed) *ast-cache*)]
    (or (.get ^java.util.Map cache preprocessed)
        (let [ast (CCJSqlParserUtil/parse preprocessed)]
          (.put ^java.util.Map cache preprocessed ast)
          ast))
    (CCJSqlParserUtil/parse preprocessed)))

(defn- cacheable-parse?
  "True if this parse result can safely live in the cross-call cache.
   Excludes session-dependent system queries, transient errors,
   results enriched against db rows, and parsed maps that carry
   mutable per-call state.

   The `:row-refs` exclusion in particular guards against the ON
   CONFLICT path: translate-insert allocates an atom there and the
   `:db.fn/call` closure appends per-row tempids/eids into it.
   Caching that atom and returning it on a subsequent identical SQL
   would leak prior-call state into the new tx-data — d/transact
   would see the accumulated entries from the previous run."
  [parsed]
  (and parsed
       (not (#{:system :error} (:type parsed)))
       (not (:enriched-db parsed))
       (not (contains? parsed :row-refs))))

;; ============================================================================
;; Table alias tracking
;; ============================================================================

;; ============================================================================
;; Main entry point
;; ============================================================================

(defn- preprocess-sql
  "Preprocess SQL to handle constructs that JSqlParser can't parse.

   All rewrites are token-driven (`datahike.pg.sql.rewrite`) — a keyword
   the rule matches inside a string literal or comment is invisible to
   the matcher because those are `:string` / `:comment` tokens, not
   `:ident`s.

   Rules in `rw/default-rules`:
     - inline REFERENCES stripping + unsupported-action 0A000
     - CREATE [UNIQUE] INDEX ON … name injection
     - SELECT FROM … projection injection
     - reserved-keyword aliases (`AS select` → `AS \"select\"`)
     - COLLATE strip (qualified + bare)
     - OPERATOR(qual.op) → bare op
     - ALTER COLUMN … DROP DEFAULT removal
     - (PRIMARY KEY(col)) → (id serial) for INHERITS bodies
     - ALTER TABLE … TYPE … USING half stripping
     - reserved column name (INDEX/KEY varchar) quoting

   ::regnamespace / ::regclass casts are handled in
   expr/translate-cast-expr, no preprocessing needed."
  [^String sql]
  (rw/rewrite sql rw/default-rules))

(defn- stmt-with-items
  "Statement-agnostic WITH-list accessor. JSqlParser exposes
   `.getWithItemsList` separately on PlainSelect / SetOperationList /
   Insert / Update / Delete; everything else (DDL, COPY, …) has no
   WITH list. Returns the (possibly empty) Java List, or nil when the
   statement type doesn't carry one."
  [stmt]
  (cond
    (instance? PlainSelect stmt)       (.getWithItemsList ^PlainSelect stmt)
    (instance? SetOperationList stmt)  (.getWithItemsList ^SetOperationList stmt)
    (instance? Insert stmt)            (.getWithItemsList ^Insert stmt)
    (instance? Update stmt)            (.getWithItemsList ^Update stmt)
    (instance? Delete stmt)            (.getWithItemsList ^Delete stmt)
    :else                              nil))

(defn- plain-selects-in
  "All PlainSelects reachable from a SELECT body, descending
   ParenthesedSelect wrappers and SetOperationList (UNION/…) parts.
   Used to walk WITH-CTE bodies for nested bind-param OIDs — asyncpg's
   type introspection puts `$1::oid[]` inside a WITH RECURSIVE anchor,
   which the top-level WHERE walk never sees."
  [body]
  (cond
    (instance? PlainSelect body)       [body]
    (instance? ParenthesedSelect body) (plain-selects-in (.getSelect ^ParenthesedSelect body))
    (instance? SetOperationList body)  (vec (mapcat plain-selects-in (.getSelects ^SetOperationList body)))
    :else                              []))

(defn- cte-body-param-oids
  "Walk every WITH-CTE body of `stmt` for bind-param OIDs (WHERE / JOIN-ON /
   SELECT-list of each contained PlainSelect). Merged into the statement's
   param-oids so a param nested in a CTE (e.g. asyncpg's
   `WITH RECURSIVE … WHERE oid = ANY($1::oid[])`) is typed for
   ParameterDescription. Best-effort."
  [stmt schema]
  (try
    (apply merge
           (for [^net.sf.jsqlparser.statement.select.WithItem wi (or (stmt-with-items stmt) [])
                 :let [body (try (.getParenthesedStatement wi) (catch Throwable _ nil))]
                 ^PlainSelect ps (plain-selects-in
                                  (if (instance? ParenthesedSelect body)
                                    (.getSelect ^ParenthesedSelect body)
                                    body))
                 :let [from-item (.getFromItem ps)
                       tns (when (instance? Table from-item)
                             (unquote-ident (.getName ^Table from-item)))
                       aliases (params/collect-table-aliases from-item (.getJoins ps))]]
             (merge (params/where-param-oids (.getWhere ps) schema tns aliases)
                    (apply merge
                           (for [^SelectItem si (or (.getSelectItems ps) [])]
                             (params/where-param-oids (.getExpression si) schema tns aliases))))))
    (catch Throwable _ nil)))

(defn- materialize-withs!
  "Run the WITH-list fold for any statement type. Returns
   [enriched-db enriched-schema deferred-recursive-ctes] with the
   speculative db carrying `:<cte>/<col>` virtual attrs for each
   materialised CTE. `deferred-recursive-ctes` is a (possibly empty)
   vector of specs for parameterised recursive CTEs whose DATA must be
   materialised at Execute (after Bind) — see
   stmt/materialize-recursive-cte! and stmt/materialize-recursive-rows!.
   When `stmt` has no WITH list (or it's empty), returns [db schema []].

   - Non-recursive WITH items with a SELECT body go through
     `materialize-set-op!`.
   - Recursive WITH items in SELECT/INSERT/DELETE go through
     `materialize-recursive-cte!` (run the Datalog rule, persist rows).
     Recursive items in UPDATE are skipped here so translate-update's
     existing `:update-with-recursive` path keeps owning that case.
   - Data-modifying CTE bodies (`WITH x AS (INSERT … RETURNING …)`) are
     skipped — JSqlParser's `WithItem.getSelect()` casts to
     ParenthesedSelect and crashes on them, and the feature is not
     implemented end-to-end yet."
  [stmt db schema]
  (let [withs (stmt-with-items stmt)]
    (if (and db (seq withs))
      (reduce
       (fn [[curr-db curr-schema deferred] ^net.sf.jsqlparser.statement.select.WithItem wi]
         (let [cte-name   (str/trim (str (.getAlias wi)))
               recursive? (.isRecursive wi)
               body       (try (.getParenthesedStatement wi)
                               (catch Throwable _ nil))
               inner      (cond
                            (instance? PlainSelect body)      body
                            (instance? SetOperationList body) body
                            (instance? ParenthesedSelect body)
                            (.getSelect ^ParenthesedSelect body)
                            :else nil)]
           (cond
             ;; Recursive WITH on UPDATE — leave to translate-update.
             (and recursive? (instance? Update stmt))
             [curr-db curr-schema deferred]

             recursive?
             (let [rule! #(try (stmt/materialize-recursive-cte! wi cte-name curr-db curr-schema)
                               (catch Throwable _ nil))
                   iter! #(try (stmt/materialize-recursive-iterative! wi cte-name curr-db curr-schema)
                               (catch Throwable _ nil))
                   ;; Parameterised CTEs: prefer the iterative evaluator (it
                   ;; defers a table-full param anchor to Execute and handles
                   ;; complex bodies — asyncpg's typeinfo_tree); it bails (nil)
                   ;; on a table-free param anchor so the rule path's B2
                   ;; ground-rule-params owns that. Non-parameterised: keep the
                   ;; proven single-rule path first, iteration as the fallback
                   ;; for bodies the rule can't represent (LEFT JOIN, etc.).
                   paramy? (boolean (seq (try (params/ast-param-indices wi)
                                              (catch Throwable _ nil))))
                   m (if paramy? (or (iter!) (rule!)) (or (rule!) (iter!)))]
               (if m
                 ;; A parameterised recursive CTE enriches only the schema
                 ;; now and carries a `:deferred` spec for Execute-time data.
                 [(:db m) (:schema m)
                  (cond-> deferred (:deferred m) (conj (:deferred m)))]
                 [curr-db curr-schema deferred]))

             (some? inner)
             (if-let [m (stmt/materialize-set-op! inner cte-name curr-db curr-schema)]
               [(:db m) (:schema m) deferred]
               [curr-db curr-schema deferred])

             ;; Data-modifying CTE body or shape we don't recognise —
             ;; skip rather than crash. The outer translate-* will
             ;; surface a clearer error if the body's results are
             ;; actually needed.
             :else
             [curr-db curr-schema deferred])))
       [db schema []]
       withs)
      [db schema []])))

(defn- parse-sql*
  "Inner parse-sql implementation — does the actual work. Public
   parse-sql wraps this with the LRU result cache."
  [^String sql schema db]
  (binding [params/*parse-db* db
            params/*parse-sql* parse-sql
            ;; Use datahike's query PLANNER (not the legacy engine) for all
            ;; parse-time materialisation (derived tables / set-ops / CTEs /
            ;; correlated-subquery per-row eval). The legacy engine
            ;; cross-products multiple or-join relations (SQL LEFT JOINs),
            ;; which made asyncpg's {typeinfo} introspection derived table
            ;; (3 LEFT JOINs over pg_type/pg_range) take ~3s and the full
            ;; recursive introspection hang; the planner runs the same query
            ;; in <10ms. server.execute already binds this false for the
            ;; execute-time path — this extends it to parse-time so the two
            ;; phases use the same (fast) engine. The planner is datahike's
            ;; default now (disable via DATAHIKE_QUERY_PLANNER=false), so this
            ;; binding is a defensive explicit per-parse no-op against any
            ;; caller that disabled it.
            dq/*disable-planner* false
             ;; Per-query memoisation for `schema-hints` /
             ;; `derive-virtual-tables`. Both are called by every
             ;; `catalog-data-for*` invocation against the same db /
             ;; schema; without this binding we walk the user schema
             ;; once per catalog table referenced. IdentityHashMap
             ;; sidesteps datahike.db.DB's broken Object.equals (db.cljc:302
             ;; — auto-generated by defrecord but conflicts with the
             ;; record's custom seq, so any WeakHashMap.get on a DB key
             ;; throws "Datom cannot be cast to Map.Entry"). Lifetime
             ;; = this parse-sql call, no leak.
            pgs/*catalog-tx-cache* (pgs/make-catalog-tx-cache)]
    (try
    ;; Check system queries first. The classifier's output carries the
    ;; structural data we need (savepoint :name, SET :var/:value,
    ;; advisory-lock :args, pg_sleep duration) so we merge it into the
    ;; parsed map and avoid re-regex in the handler.
    ;; Classify once; pass the result into the system-query check so we
    ;; don't re-tokenize the same SQL twice per statement.
      (let [cls-info (cls/classify sql)
            sys-type (catalog/system-query?* sql cls-info)]
        (cond
          sys-type
          (let [base (merge
                      (when (contains? catalog/classify-system-kinds sys-type) cls-info)
                      {:type :system :system-type sys-type :sql sql})]
            (case sys-type
              :create-database
              (try
                (let [toks (database/tokenize sql)
                      parsed (database/parse-create-database toks)]
                  (merge base parsed))
                (catch Throwable e
                  {:type :error
                   :message (.getMessage e)
                   :sqlstate "42601"}))

              :drop-database
              (try
                (let [toks (database/tokenize sql)
                      parsed (database/parse-drop-database toks)]
                  (merge base parsed))
                (catch Throwable e
                  {:type :error
                   :message (.getMessage e)
                   :sqlstate "42601"}))

              :copy-from-stdin
              (try
                (let [toks (copy/tokenize sql)
                      parsed (copy/parse-copy-from-stdin toks)]
                  (merge base parsed))
                (catch clojure.lang.ExceptionInfo e
                  (let [data (ex-data e)
                        state (or (:sqlstate data)
                                  (case (:error data)
                                    :feature-not-supported "0A000"
                                    :syntax-error "42601"
                                    "XX000"))]
                    {:type :error
                     :message (.getMessage e)
                     :sqlstate state}))
                (catch Throwable e
                  {:type :error
                   :message (.getMessage e)
                   :sqlstate "XX000"}))

              :create-type-enum
              (try
                 ;; Skip the leading CREATE before passing to the parser —
                 ;; classify already consumed it conceptually.
                (let [toks (database/tokenize sql)
                      toks (drop-while (fn [t]
                                         (or (not= :ident (first t))
                                             (not= "create"
                                                   (clojure.string/lower-case
                                                    (second t)))))
                                       toks)
                      toks (rest toks) ;; past CREATE
                      parsed (user-types/parse-create-type-enum toks sql)]
                  (assoc base :type :ddl-create-enum
                         :type-name (:type-name parsed)
                         :values (:values parsed)
                         :original-sql sql))
                (catch Throwable e
                  {:type :error
                   :message (.getMessage e)
                   :sqlstate "42601"}))

              :create-type-composite
              (try
                (let [parsed (user-types/parse-create-type-composite sql)]
                  (assoc base :type :ddl-create-composite
                         :type-name (:type-name parsed)
                         :fields (:fields parsed)
                         :original-sql sql))
                (catch Throwable e
                  {:type :error
                   :message (.getMessage e)
                   :sqlstate "42601"}))

              :create-domain
              (try
                (let [toks (database/tokenize sql)
                      toks (drop-while (fn [t]
                                         (or (not= :ident (first t))
                                             (not= "create"
                                                   (clojure.string/lower-case
                                                    (second t)))))
                                       toks)
                      toks (rest toks)
                      parsed (user-types/parse-create-domain toks sql)]
                  (assoc base :type :ddl-create-domain :domain parsed))
                (catch Throwable e
                  {:type :error
                   :message (.getMessage e)
                   :sqlstate "42601"}))

              base))

        ;; Reject authorization/RLS/extension/COPY DDL with a clean PG
        ;; error code (0A000 feature_not_supported). The classifier tags
        ;; these with :reject-kind + :tag; handler optionally swallows
        ;; via :silently-accept / :compat :permissive.
          (:reject-kind cls-info)
          {:type :error
           :message (str (:tag cls-info) " is not supported by datahike pgwire")
           :sqlstate "0A000"
           :reject-kind (:reject-kind cls-info)
           :reject-tag (:tag cls-info)}

          :else
        ;; Fall through to JSqlParser.

      ;; Parse with JSqlParser (AST-cached; see ast-parse).
          (let [stmt (ast-parse (preprocess-sql sql))
            ;; Catalog materialisation: find every catalog table ref
            ;; anywhere in the AST (top-level, derived tables, UNION
            ;; branches, WHERE subqueries, CTE bodies) and inject a
            ;; speculative db with their schema + rows. Each branch
            ;; below uses `db` (now enriched) as the base so nested
            ;; scopes see the same catalog data without re-walking.
            ;; `orig-db` holds the un-enriched reference so the
            ;; `(not= cte-db orig-db)` check further down tags the
            ;; result with :enriched-db for the server's executor.
            ;;
            ;; Caching: the LRU is keyed by [schema-hash sorted-cat-names]
            ;; so two connections with the same user schema and the same
            ;; query-shape catalog set share the enriched-db. The
            ;; expensive `catalog-schema-for` + `catalog-data-for` walks
            ;; (each one re-derives virtual tables, traverses the schema,
            ;; and runs Datalog queries against the user db) MUST live
            ;; inside the cache-miss branch — earlier this block computed
            ;; combined-schema / combined-data eagerly even on cache
            ;; hits, so a 600-table Odoo startup paid for 5k catalog
            ;; rebuilds per probe regardless of memoisation.
                orig-db db
                ;; catalog-names-used: recorded on the parsed result so the
                ;; server can RE-RESOLVE the catalog enriched-db at execute
                ;; (prepared statements would otherwise reuse a stale
                ;; parse-time snapshot across a DDL).
                [db schema catalog-names-used]
                (if db
                  (let [used-catalogs (catalog/catalog-tables-in-stmt stmt)]
                    (if (empty? used-catalogs)
                      [db schema nil]
                      (let [enriched (enrich-db-with-catalogs db schema used-catalogs)]
                        [enriched (:schema enriched) (sort used-catalogs)])))
                  [db schema nil])
            ;; Top-level WITH-fold. Materialise every CTE body into a
            ;; speculative db (virtual `:<cte>/<col>` attrs) so all four
            ;; DML paths plus SELECT see the enriched db/schema before
            ;; their translator runs. Previously the fold lived inside
            ;; the PlainSelect branch only, so `WITH x AS (…)
            ;; UPDATE/DELETE/INSERT … FROM x` silently dropped the WITH
            ;; list and then surfaced an opaque "Cannot resolve any
            ;; more clauses" at execute time.
                pre-cte-db db
                ;; CTE names in scope (lowercased) — even those not
                ;; materialised (skipped data-modifying CTE bodies) — so the
                ;; undefined-table 42P01 check in translate-select exempts
                ;; `FROM <cte>`. See stmt/*cte-relations*.
                cte-relations (into #{}
                                    (keep (fn [^net.sf.jsqlparser.statement.select.WithItem wi]
                                            (some-> (.getAlias wi) str str/trim str/lower-case not-empty)))
                                    (stmt-with-items stmt))
                [db schema deferred-rec-ctes] (materialize-withs! stmt db schema)
                ;; Did the CTE fold materialise anything? If not, the only
                ;; enrichment is catalog data — which the server can safely
                ;; re-resolve at execute (stale-prepared-statement fix). With
                ;; a CTE, the baked enriched-db carries query-scoped
                ;; `:<cte>/*` attrs we can't rebuild from catalog names alone,
                ;; so we keep the parse-time snapshot.
                cte-materialized? (not (identical? pre-cte-db db))
            ;; Count prepared-statement placeholders once at the AST
            ;; level — reused for INSERT/UPDATE/DELETE which don't
            ;; accumulate placeholders in a ctx (unlike translate-select).
            ;; Short-circuit when the SQL contains no `?` outside
            ;; quoted strings/comments — the reflection-based AST walk
            ;; was a measurable hot-spot on literal-only INSERTs (the
            ;; pg_dump load path).
                param-indices (if (params/has-param-marker? sql)
                                (params/ast-param-indices stmt)
                                #{})
                param-count   (if (empty? param-indices) 0 (apply max param-indices))
            ;; Best-effort param OID inference so Describe('S') emits a
            ;; real ParameterDescription — lets JDBC drivers pick the
            ;; right binary encoding for setObject(). Safe to leave as
            ;; empty: unknown params default to 0 (text, any type).
                inferred-oids
                (when (pos? param-count)
                  (cond (instance? Insert stmt)
                        (let [values-oids (params/insert-param-oids stmt schema db)
                          ;; ON CONFLICT DO UPDATE SET col = ?: walk the
                          ;; conflict action's UpdateSet list the same
                          ;; way params/update-param-oids does.
                              on-conflict-oids
                              (try
                                (when-let [action (.getConflictAction ^Insert stmt)]
                                  (let [table-ns (when-let [^Table t (.getTable ^Insert stmt)]
                                                   (unquote-ident (.getName t)))
                                        col-oid (fn [col] (params/infer-param-oid-for-column
                                                           schema table-ns col))
                                        sets (.getUpdateSets action)
                                        r (java.util.HashMap.)]
                                    (doseq [^UpdateSet us (or sets [])]
                                      (let [cols (.getColumns us) vals (.getValues us)]
                                        (when (and cols vals)
                                          (doseq [[^Column c v] (map vector cols vals)]
                                            (when (instance? JdbcParameter v)
                                              (when-let [oid (col-oid (unquote-ident
                                                                       (.getColumnName c)))]
                                                (.put r (.getIndex ^JdbcParameter v) oid)))))))
                                    (when (pos? (.size r)) (into {} r))))
                                (catch Throwable _ nil))]
                          (merge values-oids on-conflict-oids))
                        (instance? Update stmt)
                        (let [from-set (params/update-param-oids stmt schema)
                              ^Table t (.getTable ^Update stmt)
                              tns (when t (unquote-ident (.getName t)))
                              joins (.getJoins ^Update stmt)
                              aliases (params/collect-table-aliases t joins)
                              join-oids (try
                                          (apply merge
                                                 (for [^Join j (or joins [])]
                                                   (params/where-param-oids (.getOnExpression j)
                                                                            schema tns aliases)))
                                          (catch Throwable _ nil))]
                          (merge (params/where-param-oids (.getWhere ^Update stmt) schema tns aliases)
                                 join-oids
                                 from-set))
                        (instance? Delete stmt)
                        (let [^Table t (.getTable ^Delete stmt)
                              tns (when t (unquote-ident (.getName t)))
                              aliases (params/collect-table-aliases t nil)]
                          (params/where-param-oids (.getWhere ^Delete stmt) schema tns aliases))
                        (instance? PlainSelect stmt)
                        (let [from-item (.getFromItem ^PlainSelect stmt)
                              tns (when (instance? Table from-item)
                                    (unquote-ident (.getName ^Table from-item)))
                              joins (.getJoins ^PlainSelect stmt)
                              aliases (params/collect-table-aliases from-item joins)
                              where-oids (params/where-param-oids (.getWhere ^PlainSelect stmt)
                                                                  schema tns aliases)
                              join-oids (try
                                          (apply merge
                                                 (for [^Join j (or joins [])]
                                                   (params/where-param-oids (.getOnExpression j)
                                                                            schema tns aliases)))
                                          (catch Throwable _ nil))
                              ;; SELECT-list expressions can carry params too
                              ;; (e.g. `SELECT 1 = $1`, `SELECT $1::int`,
                              ;; `SELECT col = $1`). where-param-oids walks an
                              ;; arbitrary expression for `… OP $n` / CAST and
                              ;; borrows the comparand/literal/cast type, so
                              ;; reuse it per select item. Without this the
                              ;; param stays undetermined → TEXT, and a client
                              ;; binding e.g. an int fails to encode.
                              select-oids (try
                                            (apply merge
                                                   (for [^SelectItem si (or (.getSelectItems ^PlainSelect stmt) [])]
                                                     (params/where-param-oids (.getExpression si)
                                                                              schema tns aliases)))
                                            (catch Throwable _ nil))
                              ;; params nested in WITH-CTE bodies (asyncpg's
                              ;; `WITH RECURSIVE … oid = ANY($1::oid[])`)
                              cte-oids (cte-body-param-oids stmt schema)]
                          (merge cte-oids select-oids where-oids join-oids))))
                attach-params #(cond-> (assoc % :param-count param-count)
                                 (seq inferred-oids) (assoc :param-oids inferred-oids))
                result
                ;; Expose CTE names to translate-select's undefined-table
                ;; (42P01) check so `FROM <cte>` is never mistaken for a
                ;; missing relation — including skipped data-modifying CTEs.
                (binding [stmt/*cte-relations* cte-relations]
                  (cond
          ;; SELECT (may have CTEs — WITH ... AS)
                    (instance? PlainSelect stmt)
                    (let [;; Rewrite RIGHT JOIN → LEFT JOIN by swapping FROM and JOIN items
                ;; This ensures the proven LEFT JOIN path handles it correctly.
                          _ (when-let [joins (.getJoins ^PlainSelect stmt)]
                              (doseq [^Join j joins]
                                (when (.isRight j)
                                  (let [from-item (.getFromItem ^PlainSelect stmt)
                                        join-item (.getRightItem j)]
                                    (.setFromItem ^PlainSelect stmt join-item)
                                    (.setRightItem j from-item)
                                    (.setLeft j true)
                                    (.setRight j false)))))
                ;; Detect FULL JOIN → handled as two LEFT JOIN queries in server
                ;; Also rewrite FULL→LEFT for the first query
                          has-full-join? (when-let [joins (.getJoins ^PlainSelect stmt)]
                                           (let [has-full? (some #(.isFull ^Join %) joins)]
                                             (when has-full?
                                               (doseq [^Join j joins]
                                                 (when (.isFull j)
                                                   (.setLeft j true)
                                                   (.setFull j false))))
                                             has-full?))
                ;; CTE materialisation already happened at parse-sql*'s
                ;; outer scope (see `materialize-withs!` below the
                ;; catalog enrichment block). `db`/`schema` here are the
                ;; enriched values; the local `cte-db`/`cte-schema`
                ;; names are kept for the downstream references that
                ;; still use them.
                          cte-db db
                          cte-schema schema

;; Table-free SELECT (e.g. SELECT 1+2, SELECT CASE WHEN 1>2 THEN 3 END)
                ;; Evaluate expressions directly without going through the query engine.
                ;; BUT only if there's no WHERE clause — a WHERE gate (e.g.
                ;; SELECT 'x' WHERE EXISTS (...)) needs the full translator.
                          trivial-table-free-item?
                          (fn [^SelectItem item]
                           ;; Plain literal / NULL / CASE / simple CAST of
                           ;; literal / ParenthesedSelect are handled by
                           ;; the direct-evaluation branch below. Anything
                           ;; more (Function, ArrayExpression, nested
                           ;; ArrayConstructor-in-CAST) must flow through
                           ;; translate-select.
                            (let [e (.getExpression item)
                                  simple-cast? (fn [^CastExpression c]
                                                 (let [inner (.getLeftExpression c)]
                                                   (or (instance? LongValue inner)
                                                       (instance? DoubleValue inner)
                                                       (instance? StringValue inner)
                                                       (instance? NullValue inner)
                                                       (instance? net.sf.jsqlparser.expression.BooleanValue inner))))
                                  _ nil]
                              (or (instance? LongValue e)
                                  (instance? DoubleValue e)
                                  (instance? StringValue e)
                                  (instance? NullValue e)
                                  (instance? CaseExpression e)
                                  (and (instance? CastExpression e) (simple-cast? e))
                                  (instance? net.sf.jsqlparser.statement.select.ParenthesedSelect e)
                                  (and (instance? net.sf.jsqlparser.schema.Column e)
                                       (nil? (.getTable ^net.sf.jsqlparser.schema.Column e))
                                       (contains? #{"true" "false"}
                                                  (some-> (.getColumnName ^net.sf.jsqlparser.schema.Column e)
                                                          clojure.string/lower-case)))
                                  (instance? net.sf.jsqlparser.expression.BooleanValue e))))
                          unnest-single-item?
                          (fn [^SelectItem item]
                           ;; SELECT unnest(...) is a set-returning function
                           ;; that the narrow table-free pattern below
                           ;; expands into literal-rows — must take the
                           ;; direct-eval branch even though Function isn't
                           ;; in the trivial set.
                            (let [e (.getExpression item)]
                              (and (instance? Function e)
                                   (= "unnest" (str/lower-case (.getName ^Function e))))))
                          gs-single-item?
                          (fn [^SelectItem item]
                           ;; SELECT generate_series(a,b[,c]) — a set-returning
                           ;; function in the target list (PG's legacy SRF-in-
                           ;; projection idiom, e.g. asyncpg's
                           ;; `SELECT generate_series(0, 20)`). Table-free,
                           ;; constant-arg forms expand into literal-rows via
                           ;; the same materialiser the FROM-clause SRF uses.
                            (let [e (.getExpression item)]
                              (and (instance? Function e)
                                   (= "generate_series" (str/lower-case (.getName ^Function e))))))
                          table-free? (and (nil? (.getFromItem ^PlainSelect stmt))
                                           (nil? (.getWhere ^PlainSelect stmt))
                                           (let [items (.getSelectItems ^PlainSelect stmt)]
                                             (or (every? trivial-table-free-item? items)
                                                 (and (= 1 (count items))
                                                      (or (unnest-single-item? (first items))
                                                          (gs-single-item? (first items)))))))
                ;; Narrow support for PG's set-returning-function idiom
                ;;   SELECT unnest(array_fill(expr, ARRAY[count]))  — N rows of expr
                ;;   SELECT unnest(ARRAY[e1,e2,e3])                 — N distinct rows
                ;; both produce `count` literal rows. Matches only fully-
                ;; constant forms (no FROM, no params); the general
                ;; table-function path is a follow-up.
                          [unnest-val unnest-count unnest-alias unnest-elts?]
                          (when (and table-free? (identical? db pre-cte-db))
                            (let [items (.getSelectItems ^PlainSelect stmt)]
                              (when (= 1 (count items))
                                (let [^SelectItem it (first items)
                                      alias-str (select-item-alias it)
                                      expr (.getExpression it)]
                                  (when (and (instance? Function expr)
                                             (= "unnest" (str/lower-case (.getName ^Function expr))))
                                    (let [params (some-> (.getParameters ^Function expr) .getExpressions)
                                          arg (when (= 1 (count params)) (first params))]
                                      (cond
                                       ;; unnest(array_fill(v, ARRAY[n])) — one distinct value repeated N times
                                        (and (instance? Function arg)
                                             (= "array_fill" (str/lower-case (.getName ^Function arg))))
                                        (let [args (some-> (.getParameters ^Function arg) .getExpressions)]
                                          (when (= 2 (count args))
                                            (let [val-expr (first args)
                                                  dim-expr (second args)]
                                              (when (instance? ArrayConstructor dim-expr)
                                                (let [dims (.getExpressions ^ArrayConstructor dim-expr)]
                                                  (when (and (= 1 (count dims))
                                                             (instance? LongValue (first dims)))
                                                    [val-expr
                                                     (.getValue ^LongValue (first dims))
                                                     (or alias-str "unnest")
                                                     false]))))))
                                       ;; unnest(ARRAY[e1,e2,...]) — emit each element as its own row
                                        (instance? ArrayConstructor arg)
                                        [(vec (.getExpressions ^ArrayConstructor arg))
                                         nil
                                         (or alias-str "unnest")
                                         true])))))))
                        ;; SELECT generate_series(...) in the target list:
                        ;; materialise the (constant-arg) SRF into literal rows
                        ;; via the shared FROM-clause table-function expander.
                        ;; nil when args aren't constant — falls through to the
                        ;; normal translator (which today errors, as before).
                          gs-materialized
                          (when (and table-free? (identical? db pre-cte-db))
                            (let [items (.getSelectItems ^PlainSelect stmt)]
                              (when (= 1 (count items))
                                (let [^SelectItem it (first items)
                                      e (.getExpression it)]
                                  (when (and (instance? Function e)
                                             (= "generate_series" (str/lower-case (.getName ^Function e))))
                                    (when-let [m (stmt/materialize-table-function
                                                  (net.sf.jsqlparser.statement.select.TableFunction. ^Function e))]
                                      (assoc m :alias (or (select-item-alias it)
                                                          (first (:aliases m))))))))))
                          literal-eval (fn [e]
                                         (cond
                                           (instance? LongValue e)    (.getValue ^LongValue e)
                                           (instance? DoubleValue e)  (.getValue ^DoubleValue e)
                                           (instance? StringValue e)  (.getNotExcapedValue ^StringValue e)
                                           (instance? NullValue e)    :__null__
                                           :else (str e)))
                          result (cond
                                  ;; unnest(ARRAY[e1,e2,e3]) — N rows, one per element.
                                  ;; PG flattens ALL dimensions, so for multi-dim
                                  ;; literals we route through stmt/extract-value
                                  ;; (which builds a typed PgArray, recursing on
                                  ;; nested ArrayConstructors) and read leaves via
                                  ;; pg-arr/flat-elements. literal-eval still
                                  ;; handles the scalar-only cases for the common
                                  ;; 1-D path.
                                   (and unnest-val unnest-elts?)
                                  ;; Multi-dim flatten: PG `unnest` walks
                                  ;; ALL leaves (`arrayfuncs.c:6255`).
                                  ;; Recurse into nested ArrayConstructor
                                  ;; children before evaluating each leaf
                                  ;; literal.
                                   (let [extract-leaves
                                         (fn extract-leaves [exprs]
                                           (mapcat (fn [e]
                                                     (if (instance? ArrayConstructor e)
                                                       (extract-leaves (.getExpressions ^ArrayConstructor e))
                                                       [(literal-eval e)]))
                                                   exprs))
                                         vs (vec (extract-leaves unnest-val))]
                                     {:type :select
                                      :query {:find [] :where []}
                                      :find-aliases [unnest-alias]
                                      :has-aggregates? false
                                      :has-distinct? false
                                      :in-args []
                                      :hidden-count 0
                                      :literal-rows (mapv vector vs)})

                                   unnest-val
                                   (let [fake-ctx (ctx/make-ctx cte-schema {} nil {:db cte-db :parse-sql parse-sql})
                               ;; Evaluate the fill expression once. CAST
                               ;; is the only complex form we need here;
                               ;; other literals fall back to raw text.
                                         v (cond
                                             (instance? CastExpression unnest-val)
                                             (let [c-expr ^CastExpression unnest-val
                                                   inner (.getLeftExpression c-expr)
                                                   type-str (str/lower-case (str (.getDataType (.getColDataType c-expr))))
                                                   raw (cond
                                                         (instance? LongValue inner) (.getValue ^LongValue inner)
                                                         (instance? DoubleValue inner) (.getValue ^DoubleValue inner)
                                                         (instance? StringValue inner) (.getNotExcapedValue ^StringValue inner)
                                                         :else (str inner))]
                                               (case (types/cast-category type-str)
                                                 :integer (long raw)
                                                 :float (double raw)
                                                 :text (str raw)
                                                 :boolean (Boolean/parseBoolean (str raw))
                                                 :date (expr/parse-timestamp-string (str raw))
                                                 :time (str raw)
                                                 :timestamp (expr/parse-timestamp-string (str raw))
                                                 raw))
                                             (instance? LongValue unnest-val) (.getValue ^LongValue unnest-val)
                                             (instance? DoubleValue unnest-val) (.getValue ^DoubleValue unnest-val)
                                             (instance? StringValue unnest-val) (.getNotExcapedValue ^StringValue unnest-val)
                                             :else (str unnest-val))
                                         n (long unnest-count)]
                                     {:type :select
                                      :query {:find [] :where []}
                                      :find-aliases [unnest-alias]
                                      :has-aggregates? false
                                      :has-distinct? false
                                      :in-args []
                                      :hidden-count 0
                                      :literal-rows (vec (repeat n [v]))})

                                   gs-materialized
                                   (let [{:keys [rows pg-types alias]} gs-materialized]
                                     {:type :select
                                      :query {:find [] :where []}
                                      :find-aliases [alias]
                                      :has-aggregates? false
                                      :has-distinct? false
                                      :in-args []
                                      :hidden-count 0
                                      :select-item-oids [(or (some-> ^String (first pg-types)
                                                                     types/pg-name->oid)
                                                             -1)]
                                      :literal-rows (mapv vec rows)})

                                   (and table-free? (identical? db pre-cte-db))
                         ;; Evaluate each SELECT expression as a Clojure expression
                                   (let [select-items (.getSelectItems ^PlainSelect stmt)
                                         fake-ctx (ctx/make-ctx cte-schema {} nil {:db cte-db :parse-sql parse-sql})
                                         vals-aliases
                                         (mapv (fn [^SelectItem item]
                                                 (let [expr (.getExpression item)
                                                       alias-str (select-item-alias item)
                                                       val (cond
                                                             (instance? LongValue expr) (.getValue ^LongValue expr)
                                                             (instance? DoubleValue expr) (.getValue ^DoubleValue expr)
                                                             (instance? StringValue expr) (.getNotExcapedValue ^StringValue expr)
                                                             (instance? NullValue expr) :__null__
                                                            ;; Bare TRUE/FALSE — JSqlParser 5.x emits a
                                                            ;; BooleanValue. Older versions emitted a
                                                            ;; Column with name "true"/"false". Return an
                                                            ;; actual Boolean so value inference reports
                                                            ;; BOOL (16), not TEXT, and value->string
                                                            ;; renders the PG text form 't'/'f'.
                                                             (instance? BooleanValue expr)
                                                             (.getValue ^BooleanValue expr)
                                                             (and (instance? net.sf.jsqlparser.schema.Column expr)
                                                                  (nil? (.getTable ^net.sf.jsqlparser.schema.Column expr))
                                                                  (#{"true" "false"}
                                                                   (some-> (.getColumnName ^net.sf.jsqlparser.schema.Column expr)
                                                                           str/lower-case)))
                                                             (Boolean/parseBoolean
                                                              (some-> (.getColumnName ^net.sf.jsqlparser.schema.Column expr)
                                                                      str/lower-case))
                                                             (instance? CaseExpression expr)
                                                             (let [case-fn (expr/translate-case-expr fake-ctx ^CaseExpression expr)
                                                                   in-args @(:in-args fake-ctx)
                                                                   fn-val (last in-args)]
                                                               (if fn-val (fn-val) :__null__))
                                                   ;; CAST / :: syntax
                                                             (instance? CastExpression expr)
                                                             (let [cdt (.getColDataType ^CastExpression expr)
                                                                   inner (.getLeftExpression ^CastExpression expr)
                                                                 ;; .getDataType is the BASE name ("int"); the `[]` is in
                                                                 ;; .getArrayData. (str cdt) carries the full "int[]".
                                                                   type-str (str/lower-case (str (.getDataType cdt)))
                                                                   full-str (str/lower-case (str cdt))
                                                                   ad (.getArrayData cdt)
                                                                   array? (and ad (pos? (.size ^java.util.List ad)))
                                                                   raw (cond
                                                                         (instance? LongValue inner) (.getValue ^LongValue inner)
                                                                         (instance? DoubleValue inner) (.getValue ^DoubleValue inner)
                                                                         (instance? StringValue inner) (.getNotExcapedValue ^StringValue inner)
                                                                         :else (str inner))]
                                                               (cond
                                                              ;; Array-target cast: parse the canonical array text
                                                              ;; (incl. `[lo:hi]=` bounds + multi-dim) into a PgArray
                                                              ;; instead of numeric-coercing the whole string. Without
                                                              ;; this `'{1,2}'::int[]` / `'[1:3][-1:0]={{..}}'::int[]`
                                                              ;; threw "For input string".
                                                                 array?
                                                                 (if (or (nil? raw) (= :__null__ raw))
                                                                   :__null__
                                                                   (pg-arr/from-pg-text (str raw) (types/cast-array-elem-kw full-str)))
                                                                 :else
                                                                 (case (types/cast-category type-str)
                                                                ;; CAST('1' AS BIGINT): inner was a
                                                                ;; StringValue, so raw is a String —
                                                                ;; (long "1") throws. Parse numerically.
                                                                   :integer (if (number? raw)
                                                                              (long raw)
                                                                              (Long/parseLong (str/trim (str raw))))
                                                                   :float   (if (number? raw)
                                                                              (double raw)
                                                                              (Double/parseDouble (str/trim (str raw))))
                                                                   :text (str raw)
                                                                   :boolean (if (instance? Boolean raw)
                                                                              raw
                                                                              (let [b (coerce/parse-bool-token (str raw))]
                                                                                (when (nil? b)
                                                                                  (throw (errors/pg-error :invalid-text-representation
                                                                                                          {:type "boolean" :value (str raw)})))
                                                                                b))
                                                                   :date (let [s (str/trim (str raw))]
                                                                           (or (try (java.time.LocalDate/parse
                                                                                     s (java.time.format.DateTimeFormatter/ofPattern "yyyy-M-d"))
                                                                                    (catch Exception _ nil))
                                                                               (let [d (expr/parse-timestamp-string s)]
                                                                                 (when (instance? java.util.Date d)
                                                                                   (-> ^java.util.Date d .toInstant
                                                                                       (.atZone java.time.ZoneOffset/UTC)
                                                                                       .toLocalDate)))))
                                                                   :time (let [s (str/trim (str raw))
                                                                               time-only (or (second (re-find #"^\d{4}-\d{1,2}-\d{1,2}[ T](.+)$" s)) s)]
                                                                           (try (java.time.LocalTime/parse time-only)
                                                                                (catch Exception _ s)))
                                                                   :timestamp
                                                       ;; Preserve sub-millisecond precision for CAST
                                                       ;; results (pgjdbc tests assert full '…130861'
                                                       ;; microseconds in their error strings).
                                                       ;; expr/parse-timestamp-string routes through
                                                       ;; java.util.Date which is millisecond-only.
                                                                   (let [s (-> (str raw) str/trim
                                                                               (str/replace #"(\d{4}-\d{2}-\d{2})\s+(\d)" "$1T$2"))]
                                                                     (or (try (java.time.LocalDateTime/parse s)
                                                                              (catch Exception _ nil))
                                                                         (expr/parse-timestamp-string (str raw))))
                                                                   :uuid (java.util.UUID/fromString (str raw))
                                                       ;; `N::bit(W)` — PG's bit-string type,
                                                       ;; emitted as a W-char '0'/'1' string
                                                       ;; of the low-W bits. We extract W from
                                                       ;; the type-str pattern (cast-category
                                                       ;; strips the `(…)` so re-parse here).
                                                                   :bit
                                                                   (let [w (or (some-> (re-find #"\((\d+)\)" type-str)
                                                                                       second
                                                                                       Integer/parseInt)
                                                                               1)
                                                                         n (long (if (number? raw)
                                                                                   raw
                                                                                   (Long/parseLong (str raw))))
                                                                         mask (if (< w 64) (dec (bit-shift-left 1 w)) -1)
                                                                         low  (bit-and n mask)
                                                                         s (Long/toBinaryString low)
                                                                         pad (- w (.length ^String s))]
                                                                     (if (pos? pad)
                                                                       (str (apply str (repeat pad \0)) s)
                                                                       s))
                                                                   raw)))
                                                   ;; Scalar subquery in projection —
                                                   ;; execute and take first value
                                                             (instance? ParenthesedSelect expr)
                                                             (if cte-db
                                                               (let [inner-parsed (parse-sql (str expr) cte-schema cte-db)
                                                                     inner-query (:query inner-parsed)
                                                                     inner-in-args (:in-args inner-parsed)
                                                                     q-fn d/q
                                                                     rows (if (seq inner-in-args)
                                                                            (apply q-fn inner-query cte-db inner-in-args)
                                                                            (q-fn inner-query cte-db))
                                                                     first-row (first rows)
                                                                     v (if (sequential? first-row) (first first-row) first-row)]
                                                                 (if (or (nil? v) (= :__null__ v)) :__null__ v))
                                                               :__null__)
                                                   ;; ARRAY[…] / ARRAY[[…],[…]] — format
                                                   ;; to PG's canonical text form {…}/{{…}}.
                                                   ;; We don't implement full array types yet,
                                                   ;; but pgjdbc tests that SELECT an array
                                                   ;; literal assert on the text form (e.g.
                                                   ;; testgetBadBoolean expects the error to
                                                   ;; contain "{{1,0},{0,1}}").
                                                             (instance? ArrayConstructor expr)
                                                             (let [fmt (fn fmt [e]
                                                                         (cond
                                                                           (instance? ArrayConstructor e)
                                                                           (str "{"
                                                                                (str/join ","
                                                                                          (map fmt (.getExpressions ^ArrayConstructor e)))
                                                                                "}")
                                                                           (instance? LongValue e) (str (.getValue ^LongValue e))
                                                                           (instance? DoubleValue e) (str (.getValue ^DoubleValue e))
                                                                           (instance? StringValue e)
                                                                           (str "\"" (.getNotExcapedValue ^StringValue e) "\"")
                                                                           (instance? NullValue e) "NULL"
                                                                           :else (str e)))]
                                                               (fmt expr))
                                                             :else (str expr))
                                                       alias (or alias-str (str expr))]
                                                   [val alias]))
                                               select-items)]
                                     {:type :select
                                      :query {:find [] :where []}
                                      :find-aliases (mapv second vals-aliases)
                                     ;; Parse-time OID inference for the
                                     ;; Extended Query Describe path — see
                                     ;; datahike.pg.sql.oid-infer.
                                      :select-item-oids
                                      (mapv (fn [^SelectItem item]
                                              (oid/expr-oid (.getExpression item)
                                                            {:db cte-db
                                                             :schema cte-schema}))
                                            select-items)
                                      :has-aggregates? false
                                      :has-distinct? false
                                      :in-args []
                                      :hidden-count 0
                                      :literal-row (mapv first vals-aliases)})

                                   :else
                                   (translate-select ^PlainSelect stmt cte-schema cte-db))
                ;; Pass enriched db to server when the top-level catalog
                ;; materialisation or CTE processing produced a different
                ;; db from the user's original — but only if
                ;; translate-select didn't already tag a tighter
                ;; enriched-db (e.g. one containing a derived-table's
                ;; `:<alias>/*` attrs). `identical?` (reference equality)
                ;; is the right check against `orig-db` — Datahike's
                ;; `=` returns true on structurally equal DB snapshots
                ;; and would mask the distinction.
                        ;; Did translate-select tag a derived-table enriched-db
                        ;; (FROM (…) AS sub / table-fn / derived JOIN)? Captured
                        ;; BEFORE the catalog-only fallback below sets one, so we
                        ;; can tell "derived-table materialised" from "catalog
                        ;; enrichment only".
                          derived-table-edb? (some? (:enriched-db result))
                          result (if (and (nil? (:enriched-db result))
                                          (not (identical? cte-db orig-db)))
                                   (assoc result :enriched-db cte-db)
                                   result)
                        ;; Record the catalog tables so the server can
                        ;; re-resolve the enriched-db at execute (stale-
                        ;; prepared-statement fix) — but only when the
                        ;; enrichment is catalog-only. A CTE (cte-materialized?)
                        ;; or derived table (derived-table-edb?) carries
                        ;; query-scoped `:<alias>/*` attrs in :enriched-db that
                        ;; catalog re-resolution would discard, so skip those.
                          result (if (and (seq catalog-names-used)
                                          (not cte-materialized?)
                                          (not derived-table-edb?))
                                   (assoc result :catalog-tables (vec catalog-names-used))
                                   result)
                        ;; Parameterised recursive CTEs: schema enriched at
                        ;; parse (above), but DATA must be materialised at
                        ;; Execute once `$n` is bound. Carry the specs so the
                        ;; server re-runs the rule with real params.
                          result (if (seq deferred-rec-ctes)
                                   (assoc result :deferred-recursive-ctes deferred-rec-ctes)
                                   result)]
                      (if has-full-join?
              ;; FULL JOIN: return two LEFT JOIN queries for server to combine
              ;; Query 1: already rewritten as LEFT JOIN above (all left + matched right)
              ;; Query 2: swap tables and do LEFT JOIN (all right + matched left)
                        (let [left-result (assoc result :type :select)
                    ;; Clone from original SQL and swap tables for RIGHT-side query
                              stmt2 (CCJSqlParserUtil/parse sql)
                              _ (doseq [^Join j (.getJoins ^PlainSelect stmt2)]
                                  (when (.isFull j)
                                    (let [from2 (.getFromItem ^PlainSelect stmt2)
                                          join2 (.getRightItem j)]
                                      (.setFromItem ^PlainSelect stmt2 join2)
                                      (.setRightItem j from2)
                                      (.setLeft j true)
                                      (.setFull j false))))
                              right-result (assoc (translate-select ^PlainSelect stmt2 cte-schema cte-db)
                                                  :type :select)]
                          {:type :full-join
                           :left-query left-result
                           :right-query right-result
                           :find-aliases (:find-aliases result)})
              ;; Regular SELECT (possibly with LEFT JOIN from RIGHT rewrite)
                        (assoc result :type :select)))

                    (instance? ParenthesedSelect stmt)
                    (let [inner (.getSelect ^ParenthesedSelect stmt)]
                      (if (instance? PlainSelect inner)
                        (let [result (translate-select ^PlainSelect inner schema db)]
                          (cond-> (assoc result :type :select)
                            (not (identical? db orig-db)) (assoc :enriched-db db)))
                        {:type :error :message (str "Unsupported nested select: " (type inner))}))

          ;; UNION / UNION ALL / INTERSECT / EXCEPT
                    (instance? SetOperationList stmt)
                    (let [^SetOperationList sol stmt
                          selects (.getSelects sol)
                          operations (.getOperations sol)
                ;; Parse each sub-select
                          sub-results (mapv (fn [s]
                                              (if (instance? PlainSelect s)
                                                (translate-select ^PlainSelect s schema db)
                                                {:error (str "Unsupported set operation member: " (type s))}))
                                            selects)
                ;; Determine operation type from first operation
                          op-type (when (seq operations)
                                    (let [op (first operations)]
                                      (cond
                                        (instance? UnionOp op)
                                        (if (.isAll ^UnionOp op) :union-all :union)
                                        (instance? IntersectOp op) :intersect
                                        (instance? ExceptOp op) :except
                                        :else :union-all)))]
                      (cond-> {:type :set-operation
                               :op op-type
                               :sub-results sub-results}
                       ;; Top-level catalog materialisation propagates
                       ;; to the server's set-operation executor via
                       ;; :enriched-db — each sub-query runs against it.
                        (not (identical? db orig-db))
                        (assoc :enriched-db db)))

          ;; INSERT
                    (instance? Insert stmt)
                    (cond-> (translate-insert ^Insert stmt schema db)
                      (not (identical? db orig-db)) (assoc :enriched-db db))

          ;; UPDATE
                    (instance? Update stmt)
                    (cond-> (translate-update ^Update stmt schema db)
                      (not (identical? db orig-db)) (assoc :enriched-db db))

          ;; DELETE
                    (instance? Delete stmt)
                    (cond-> (translate-delete ^Delete stmt schema)
                      (not (identical? db orig-db)) (assoc :enriched-db db))

          ;; CREATE TABLE
                    (instance? CreateTable stmt)
                    (ddl/translate-create-table ^CreateTable stmt db)

          ;; CREATE SEQUENCE
                    (instance? CreateSequence stmt)
                    (cond-> (ddl/translate-create-sequence ^CreateSequence stmt)
                      ;; IF NOT EXISTS is stripped pre-parse (JSqlParser's
                      ;; CreateSequence grammar has no such production —
                      ;; rewrite/create-sequence-if-not-exists-rule), so
                      ;; re-detect it from the ORIGINAL source and carry
                      ;; it for the executor's no-op-vs-42P07 decision.
                      (re-find #"(?i)create\s+(?:temp(?:orary)?\s+|unlogged\s+)?sequence\s+if\s+not\s+exists"
                               sql)
                      (assoc :if-not-exists? true))

          ;; DROP TABLE / DROP SEQUENCE
                    (instance? Drop stmt)
                    (let [^Drop d stmt
                          drop-type (when-let [t (.getType d)] (str/lower-case t))
                          obj-name (-> d .getName .getName)]
                      (if (= drop-type "sequence")
                        {:type :ddl-drop-sequence :seq-name obj-name}
                        {:type :ddl-drop :table obj-name}))

          ;; COMMIT (JSqlParser AST)
                    (instance? Commit stmt)
                    {:type :system :system-type :commit}

          ;; SAVEPOINT name
                    (instance? SavepointStatement stmt)
                    {:type :savepoint :name (.getSavepointName ^SavepointStatement stmt)}

          ;; ROLLBACK [TO SAVEPOINT name] / ROLLBACK WORK
                    (instance? RollbackStatement stmt)
                    (if (.isUsingSavepointKeyword ^RollbackStatement stmt)
                      {:type :rollback-to-savepoint :name (.getSavepointName ^RollbackStatement stmt)}
                      {:type :system :system-type :rollback})

          ;; CREATE INDEX — accepted as no-op
                    (instance? CreateIndex stmt)
                    {:type :ddl-create-index}

          ;; ALTER TABLE — extract operations for ADD COLUMN support
                    (instance? Alter stmt)
                    (let [^Alter alter-stmt stmt
                          table-name (unquote-ident (.getName (.getTable alter-stmt)))
                          expressions (.getAlterExpressions alter-stmt)
                          ops (mapv (fn [^AlterExpression exp]
                                      (let [op (str (.getOperation exp))]
                                        (cond
                                ;; ADD COLUMN
                                          (and (= op "ADD") (.hasColumn exp))
                                          (let [cdts (.getColDataTypeList exp)]
                                            {:op :add-column
                                             :columns (mapv (fn [^ColumnDefinition cdt]
                                                              {:name (unquote-ident (.getColumnName cdt))
                                                               :type (str/lower-case (str (.getDataType (.getColDataType cdt))))})
                                                            cdts)})
                                ;; ADD [CONSTRAINT name] PRIMARY KEY / UNIQUE —
                                ;; carry the columns so the executor can upgrade
                                ;; the attributes to :db/unique (single column)
                                ;; or :db/index (composite members) via
                                ;; datahike's index-backfill migration. Bare
                                ;; forms surface via getPkColumns/getUkColumns;
                                ;; named-constraint forms via getIndex.
                                          (= op "ADD")
                                          (let [idx (.getIndex exp)
                                                idx-type (some-> idx .getType str/upper-case)
                                                pk-cols (seq (.getPkColumns exp))
                                                uk-cols (seq (.getUkColumns exp))]
                                            (cond
                                              (or pk-cols (= idx-type "PRIMARY KEY"))
                                              {:op :add-primary-key
                                               :columns (mapv unquote-ident
                                                              (or pk-cols (.getColumnsNames idx)))}
                                              (or uk-cols (= idx-type "UNIQUE"))
                                              {:op :add-unique
                                               :columns (mapv unquote-ident
                                                              (or uk-cols (.getColumnsNames idx)))}
                                              ;; FK, CHECK, etc. — no-op
                                              :else {:op :add-constraint}))
                                ;; DROP — no-op
                                          (= op "DROP") {:op :drop}
                                ;; ALTER (SET NOT NULL, TYPE change, etc.) — no-op
                                          :else {:op :other :raw (str exp)})))
                                    expressions)]
                      {:type :ddl-alter :table table-name :operations ops})

                    :else
                    {:type :error :message (str "Unsupported SQL statement: " (type stmt))}))]
            (if (map? result) (attach-params result) result)))) ; close :else let, binding, cond, outer let
      (catch Exception e
         ;; Resolve the exception structurally: throw sites may carry
         ;; either :sqlstate (legacy / explicit override) or :error
         ;; (structured category). The errors namespace knows how to
         ;; map both to a (sqlstate, message, fields) tuple.
        (let [data (ex-data e)
              [classified-code classified-msg] (errors/classify-exception e)
               ;; Only prepend "SQL parse error:" when neither :sqlstate
               ;; nor a registered :error category was set — those came
               ;; from a real SQL-shape failure where the prefix is
               ;; useful diagnostic context. Structured throws already
               ;; produce PG-shaped messages.
              structured? (or (:sqlstate data)
                              (and (:error data)
                                   (contains? errors/error-categories
                                              (:error data))))
              msg (if structured?
                    classified-msg
                    (str "SQL parse error: " classified-msg))]
          {:type :error
           :message msg
           :sqlstate classified-code})))))

(defn- templated-parse
  "Lexical INSERT-VALUES fast path. Returns a parsed result on
   success, or nil if the templater rejected the SQL (caller falls
   through to parse-sql*).

   Two-tier fast path:

   1. Templater turns `INSERT INTO t (a, b) VALUES (1, 'x')` into
      `INSERT INTO t (a, b) VALUES (?, ?)` and captures the literals.
   2. We look up the templated SQL in the parse-sql result LRU.
      First row of an INSERT shape misses, falls through to
      parse-sql* (which produces a placeholder-shape parsed map with
      ParamRefs in row-attrs and outer entity-maps), and stores
      that in the result cache.
   3. Subsequent rows of the same shape hit the cache and run only
      `typed-substitute` — coerce-aware ParamRef substitution
      keyed on the column's `:db/valueType`. ~10 µs/row vs ~400 µs
      for translate-insert per call.

   Skip-conditions:
     - non-INSERT or INSERT … SELECT shapes (template-insert-sql
       returns nil),
     - ON CONFLICT clauses (template-insert-sql bails),
     - SQL with existing `?` / `$N` placeholders (would scramble
       param indices),
     - empty literal capture (no win),
     - any literal token the parser can't safely reduce."
  [^String sql schema db]
  (when (nil? params/*bound-params*)
    (when-not (params/has-param-marker? sql)
      (when-let [tem (template/template-insert-sql sql)]
        (when (seq (:literals tem))
          (let [bound (mapv template/parse-literal-token (:literals tem))]
            (when-not (some template/templater-fail? bound)
              (try
                (let [tem-sql (:templated tem)
                      cache (when (cacheable-sql-size? (:templated tem)) *parse-cache*)
                      cache-key (when cache [tem-sql (hash schema)])
                      placeholder-parsed
                      (or (when cache (cache-get cache cache-key))
                          (let [p (parse-sql* tem-sql schema db)]
                            (when (and cache (cacheable-parse? p))
                              (cache-put! cache cache-key p))
                            p))]
                  (when (and placeholder-parsed
                             (= :insert (:type placeholder-parsed))
                             (vector? (:tx-data placeholder-parsed)))
                    ;; Enrich the schema with pg metadata (NUMERIC(p,s)
                    ;; scale, array-elem) so the fast-path literal coercion
                    ;; rounds/pads numerics to their declared scale — same as
                    ;; translate-insert's slow path. Memoised per schema.
                    (when-let [substituted (template/typed-substitute
                                            placeholder-parsed
                                            (:literals tem)
                                            (stmt/enrich-schema-with-pg-array-meta schema db))]
                      ;; Reset placeholder counts on the substituted result
                      ;; — pgjdbc parsed the ORIGINAL SQL (no `?`) and pre-
                      ;; sized its SimpleParameterList for that. If we
                      ;; report the templated form's `:param-count 1` via
                      ;; describeParams, the wire-layer ParameterDescription
                      ;; carries one OID and pgjdbc's `setResolvedType(0,…)`
                      ;; ArrayIndex-OOBs on its 0-length internal array.
                      ;; The templated SQL's placeholders are already bound
                      ;; (typed-substitute resolved them); from the client's
                      ;; perspective there are no params to describe.
                      (assoc substituted
                             :param-count 0
                             :param-oids {}))))
                (catch Throwable _ nil)))))))))

(defn parse-sql
  "Parse a SQL statement and return a translation result.

   Returns one of:
     {:type :select :query <datalog-map> :find-aliases [...] ...}
     {:type :insert :tx-data [...] :count N}
     {:type :update :table str :ns str :assignments [...] :where-expr expr}
     {:type :delete :table str :ns str :where-expr expr}
     {:type :ddl-create :tx-data [...]}
     {:type :system :system-type keyword}
     {:type :error :message str}

   Optional db parameter enables subquery execution during translation.

   Three cache levels stack:
     - **Result cache** (`*parse-cache*`) — for SQL strings re-issued
       verbatim (pgjdbc unnamed prepared statements, ORM select-by-id).
     - **Lexical INSERT-VALUES templating** + AST cache — for `INSERT
       INTO t [(cols)] VALUES (lit, …)` shapes the literals are
       captured, the SQL is normalised to `(? , …)`, and the resulting
       AST is reused across all rows of the same shape. Translation
       still runs per row with `*bound-params*` bound so JdbcParameter
       nodes resolve to concrete values inline (no ParamRef closure
       captures).
     - **AST cache** (`*ast-cache*`) — covers everything else that
       hits JSqlParser, repeated or not."
  ([^String sql schema] (parse-sql sql schema nil))
  ([^String sql schema db]
   (let [;; A parse made under *from-bindings* (correlated subquery / LATERAL
         ;; per-row eval) resolves outer column refs to ROW-SPECIFIC constants,
         ;; so it must neither be served from nor written to the shared result
         ;; cache (whose key is only [sql schema]). Bypass caching entirely in
         ;; that case — otherwise the binding-free version (e.g. the parse done
         ;; for result-OID inference) poisons the entry and the correlated ref
         ;; collapses to an unbindable get-else ("Cannot resolve any clauses").
         cache (when (and (empty? params/*from-bindings*)
                          (cacheable-sql-size? sql))
                 *parse-cache*)
         schema-key (when cache (hash schema))
         cache-key (when cache [sql schema-key])
         cached (when cache (cache-get cache cache-key))]
     (cond
       cached cached

       :else
       (or (templated-parse sql schema db)
           (let [parsed (parse-sql* sql schema db)]
             (when (and cache (cacheable-parse? parsed))
               (cache-put! cache cache-key parsed))
             parsed))))))
