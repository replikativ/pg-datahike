(ns datahike.pg.sql.expr
  "Expression + predicate translation.

   Converts JSqlParser expression nodes (arithmetic, comparisons, CASE,
   CAST, function calls, IN / BETWEEN / EXISTS subqueries, JSON
   operators, …) into Datalog find-element expressions and
   where-clause predicates.

   Two families of entry points:

   - **Expression side** (`translate-expr`): evaluates to a value or
     binds a var. Produces `?v` symbols via materialization when the
     result feeds into a SELECT projection, ORDER BY key, aggregate
     argument, or outer SQL function call.

   - **Predicate side** (`translate-predicate`): produces a vec of
     Datalog where-clauses that filter rows. Recursively handles
     AND / OR / NOT, comparison ops with three-valued-logic null
     guards, IN-list / IN-subquery, BETWEEN, IS NULL, EXISTS and
     NOT EXISTS (including the correlated-subquery path with
     entity-var unification + not-join fallback), LIKE / ILIKE,
     JSON operators (@>, <@, ?, ?|, ?&), and regex match.

   `translate-expr` and `translate-predicate` are mutually recursive
   through `translate-case-expr` (WHEN predicates) and
   `translate-comparison` (operand expressions). Everything lives in
   one namespace so the recursion is a regular in-ns `declare` and
   doesn't cross any ns boundary.

   Back-edge to sql.clj: the EXISTS / IN-subquery branches need to
   recursively re-enter the parser for sub-SQL. Rather than import
   `datahike.pg.sql` (which would create a load cycle), we receive
   `parse-sql` through the ctx's `:parse-sql` slot — populated by
   `datahike.pg.sql/parse-sql` at top-level ctx construction and
   inherited by sub-ctxs (for CTE / correlated subquery / derived
   tables) when they copy from their outer ctx.

   Every translate-* fn takes a ctx first-argument (built by
   `datahike.pg.sql.ctx/make-ctx`) and returns either a value, a
   fresh var, or a vector of where-clauses (for the predicate side).
   Side effects flow through the ctx's atoms (`:where-clauses`,
   `:in-args`, `:param-placeholders`, `:entity-vars`, `:col->var`)."
  (:require [clojure.set :as set]
            [datahike.api :as d]
            [datahike.pg.sql.oid-infer :as oid-infer]
            [clojure.string :as str]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.errors :as errors]
            [datahike.pg.records :as pg-rec]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression
            Alias ArrayExpression Function LongValue DoubleValue StringValue NullValue
            BooleanValue Parenthesis NotExpression CaseExpression WhenClause
            SignedExpression CastExpression TimeKeyExpression JsonExpression
            TimezoneExpression ArrayConstructor JdbcParameter JdbcNamedParameter]
           [net.sf.jsqlparser.expression.operators.relational
            DoubleAnd EqualsTo ExistsExpression ExpressionList
            GreaterThan GreaterThanEquals InExpression IsBooleanExpression
            IsNullExpression JsonOperator LikeExpression MinorThan
            MinorThanEquals NotEqualsTo ParenthesedExpressionList
            RegExpMatchOperator Between]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression]
           [net.sf.jsqlparser.expression.operators.arithmetic
            Addition Subtraction Multiplication Division Modulo Concat]
           [net.sf.jsqlparser.statement.select
            PlainSelect SelectItem AllColumns ParenthesedSelect Join]))

(set! *warn-on-reflection* true)

;; Unqualified alias so the copied body's `unquote-ident` reads from
;; params — same pattern used by ctx / catalog / ddl.
(def ^:private unquote-ident params/unquote-ident)

;; ---------------------------------------------------------------------------
;; Forward declarations for the mutually-recursive translate-* family.

(declare translate-expr
         translate-predicate
         translate-case-expr
         translate-cast-expr
         translate-predicate-expr
         translate-comparison
         translate-function-call
         translate-binary-arith
         flatten-json-chain
         interpret-form
         parse-timestamp-string)

(defn string-value-text
  "Extract the text of a JSqlParser `StringValue`, applying SQL/PG
   semantics for the `N'...'` (national character) prefix.

   PG observed behaviour: `N'foo '` (with trailing space) stores as
   `'foo'` — a CHAR-style trailing-space trim. `'foo '` (no prefix)
   preserves the trailing space. The PG docs say `N'...'` is
   identical to `'...'`, but empirically (any 12+ release) PG
   coerces `N` literals through CHAR, which strips trailing blanks.
   The Chinook fixture relies on this — its source SQL has trailing
   spaces inside `N'...'` that PG strips, and our roundtrip needs
   to match.

   Pass-through for non-`N` prefixes (`E'...'`, `B'...'`, etc.) and
   for unprefixed strings."
  [^net.sf.jsqlparser.expression.StringValue sv]
  (let [v (.getNotExcapedValue sv)
        prefix (.getPrefix sv)]
    (if (and v prefix (.equalsIgnoreCase ^String prefix "N"))
      ;; CHAR-coerce: rstrip trailing ASCII spaces.
      (str/replace v #" +$" "")
      v)))

(defn- coerce-pg-array
  "Coerce a runtime value to a pg-arr record so the ANY/ALL/containment
   ops can index into it uniformly. Inputs come from four places:

     - an ArrayConstructor literal that already produced a pg-arr;
     - a native PG array column stored as canonical PG text
       (`\"{1,2,3}\"`), reconstructed via `from-pg-text`;
     - a catalog column (e.g. pg_constraint.conkey) stored the same
       way (no schema-side `:pg/array-elem`);
     - a Clojure collection from a function call.

   Two-arity form `(coerce-pg-array v elem-type)` is preferred when
   the call site knows the element keyword from schema metadata
   (translate-time capture from `:pg/array-elem`). The single-arity
   form auto-detects: tries `:int8` first (catches int catalogs and
   any int-typed user array), then `:float8`, then falls back to
   `:text` (which always succeeds because tokens parse as raw strings).

   Returns a `pg-arr/array` or nil if the value isn't array-shaped.
   Best-effort: a parse failure means the value wasn't an array, so
   the calling op can return false / true per ANY/ALL semantics."
  ([v] (coerce-pg-array v nil))
  ([v elem-type]
   (cond
     (pg-arr/array? v)
     v

     (and (string? v) (clojure.string/starts-with? (clojure.string/triml v) "{"))
     (let [trial (fn [t] (try (pg-arr/from-pg-text v t) (catch Throwable _ nil)))]
       (or (when elem-type (trial elem-type))
           (trial :int8)
           (trial :float8)
           (trial :text)))

     (sequential? v)
     (pg-arr/array (or elem-type :unknown) (vec v))

     :else nil)))

(defn translate-function-call
  "Translate a non-aggregate SQL function to a Datalog function binding.
   Adds the binding clause to where-clauses and returns the result variable."
  [ctx ^Function f]
  (let [raw-name (str/lower-case (.getName f))
        ;; Strip a leading `pg_catalog.` schema qualifier — pgjdbc &
        ;; friends explicitly qualify their catalog-function calls.
        fname (if (str/starts-with? raw-name "pg_catalog.")
                (subs raw-name (count "pg_catalog."))
                raw-name)
        params (.getParameters f)
        raw-args (when params
                   (mapv #(translate-expr ctx %) params))
        ;; Materialize complex sub-expressions into intermediate vars
        args (when raw-args
               (mapv #(ctx/materialize-arg! ctx %) raw-args))
        result-var (ctx/fresh-var! ctx)]
    (cond
      ;; ROW(a, b, …) — anonymous composite constructor. JSqlParser parses
      ;; it as a Function named "row". Build a PgRecord at runtime from the
      ;; field values, inferring each field's OID from its value (nested
      ;; ROW → another PgRecord → record OID 2249). A ::type cast wrapping
      ;; the ROW retypes it to the named composite; here it stays anonymous.
      (= fname "row")
      (let [fn-param (symbol (str "?row" (swap! (:var-counter ctx) inc)))
            row-fn   (fn [& vals] (pg-rec/make-record types/infer-oid-from-value (vec vals)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj row-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; PG privilege-check functions (has_*_privilege). We run without
      ;; a privilege model — one user, one schema, all granted — so
      ;; return `true` unconditionally. Metabase's describe-database
      ;; step queries these to filter out unreadable tables. Register
      ;; every variant so qualifier / arg-count mismatch doesn't fall
      ;; through to the unknown-function path.
      (contains? #{"has_table_privilege"
                   "has_any_column_privilege"
                   "has_column_privilege"
                   "has_sequence_privilege"
                   "has_database_privilege"
                   "has_schema_privilege"
                   "has_function_privilege"
                   "has_language_privilege"
                   "has_server_privilege"
                   "has_foreign_data_wrapper_privilege"
                   "pg_has_role"}
                 fname)
      (let [fn-param (symbol (str "?priv" (swap! (:var-counter ctx) inc)))
            priv-fn (fn [& _args] true)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj priv-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; current_database() / current_schema() used inline as a value
      ;; expression — the sole-select path is classified by
      ;; datahike.pg.sql.classify, but column-position use lands here.
      (= fname "current_database")
      (let [fn-param (symbol (str "?cur-db" (swap! (:var-counter ctx) inc)))
            ;; Resolve the bound db-name from session-state if available;
            ;; otherwise fall back to "datahike" (our default handler name).
            impl-fn (fn [] "datahike")]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
        result-var)

      (= fname "current_schema")
      (let [fn-param (symbol (str "?cur-sch" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [] "public")]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
        result-var)

      ;; NOW() → current timestamp as java.util.Date
      (= fname "now")
      (let [fn-param (symbol (str "?now-fn" (swap! (:var-counter ctx) inc)))
            now-fn (fn [] (java.util.Date.))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj now-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
        result-var)

      ;; current_user() / session_user() / user() / system_user() —
      ;; PG canonicalises all four to the role of the current
      ;; connection. We're single-tenant so they all return the
      ;; static handler name. The bare-identifier forms (no parens)
      ;; are handled in `translate-expr`.
      (contains? #{"current_user" "session_user" "user" "system_user"} fname)
      (let [fn-param (symbol (str "?cur-user" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [] "datahike")]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
        result-var)

      ;; current_setting(name [, missing_ok]) — return a GUC
      ;; parameter as text. We expose the static set of settings
      ;; that JDBC / psycopg2 / asyncpg / Metabase actually probe on
      ;; connect; everything else throws (PG: error 42704) unless
      ;; the caller passes missing_ok=true, in which case we return
      ;; NULL. Values are 0.1-stable; revisit if any client gates a
      ;; feature on the reported version.
      (= fname "current_setting")
      (let [fn-param (symbol (str "?cur-setting" (swap! (:var-counter ctx) inc)))
            settings {"server_version"             "15.0"
                      "server_version_num"         "150000"
                      "client_encoding"            "UTF8"
                      "server_encoding"            "UTF8"
                      "TimeZone"                   "UTC"
                      "DateStyle"                  "ISO, MDY"
                      "IntervalStyle"              "postgres"
                      "search_path"                "\"$user\", public"
                      "standard_conforming_strings" "on"
                      "lc_messages"                "C"
                      "lc_collate"                 "C"
                      "lc_ctype"                   "C"
                      "is_superuser"               "off"
                      "session_authorization"      "datahike"
                      "application_name"           "datahike"
                      "transaction_isolation"      "read committed"
                      "transaction_read_only"      "off"
                      "default_transaction_isolation" "read committed"}
            impl-fn (fn [name & [missing-ok]]
                      (or (get settings (str name))
                          (when missing-ok :__null__)
                          (throw (ex-info "unrecognized configuration parameter"
                                          {:error :undefined-object
                                           :kind "configuration parameter"
                                           :name name}))))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; format_type(oid, typmod) — return canonical type name.
      ;; typmod is currently ignored (we don't track VARCHAR(N) etc.;
      ;; that lands when :pg/atttypmod is plumbed through DDL).
      (= fname "format_type")
      (let [fn-param (symbol (str "?fmt-type" (swap! (:var-counter ctx) inc)))
            impl-fn (fn ([oid] (types/format-type oid -1))
                      ([oid typmod] (types/format-type oid typmod)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; obj_description(oid, catalog) / col_description(oid, attnum)
      ;; — PG returns the COMMENT ON text. We don't track comments;
      ;; returning NULL (instead of 0 rows) lets Metabase render an
      ;; empty description column rather than dropping the row.
      (contains? #{"obj_description" "col_description" "shobj_description"} fname)
      (let [fn-param (symbol (str "?desc" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [& _args] :__null__)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; pg_get_userbyid(oid) — single-tenant: every oid maps to the
      ;; static handler role.
      (= fname "pg_get_userbyid")
      (let [fn-param (symbol (str "?get-user" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [_oid] "datahike")]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (or (first args) :__null__)) result-var])
        result-var)

      ;; pg_get_indexdef(oid [, column, pretty]) — relational lowering.
      ;; The CREATE INDEX text is pre-baked into pg_index.indexdef at
      ;; catalog-data time; here we emit the lookup-by-oid pattern so
      ;; the surrounding SELECT composes correctly with WHERE / JOIN /
      ;; ORDER BY. Two-arg `column` form returns a single column name;
      ;; we don't model that yet — return the full def.
      (= fname "pg_get_indexdef")
      (let [arg-oid (or (first args) :__null__)
            idx-eid (ctx/fresh-var! ctx)]
        (swap! (:where-clauses ctx) conj
               [idx-eid :pg_index/indexrelid arg-oid])
        (swap! (:where-clauses ctx) conj
               [idx-eid :pg_index/indexdef result-var])
        result-var)

      ;; pg_get_constraintdef(oid [, pretty]) — same pattern as
      ;; pg_get_indexdef. condef is pre-rendered at catalog-data time.
      (= fname "pg_get_constraintdef")
      (let [arg-oid (or (first args) :__null__)
            con-eid (ctx/fresh-var! ctx)]
        (swap! (:where-clauses ctx) conj
               [con-eid :pg_constraint/oid arg-oid])
        (swap! (:where-clauses ctx) conj
               [con-eid :pg_constraint/condef result-var])
        result-var)

      ;; pg_typeof(value) — PG returns regtype (text-formatted as
      ;; the type name). We resolve it at translate time using the
      ;; expression's inferred OID so the result is a constant
      ;; string the planner can fold; falls back to "text" when the
      ;; argument is a derived expression we can't statically type.
      (= fname "pg_typeof")
      (let [arg-expr (first params)
            oid-env {:db            (:db ctx)
                     :schema        (:schema ctx)
                     :table-aliases (:table-aliases ctx)
                     :default-table (:default-table ctx)
                     :hints         (:hints ctx)}
            arg-oid (try
                      (oid-infer/expr-oid arg-expr oid-env)
                      (catch Throwable _ nil))
            type-name (or (get types/oid->pg-name arg-oid) "text")
            fn-param (symbol (str "?pg-typeof" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [_v] type-name)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (or (first args) :__null__)) result-var])
        result-var)

      ;; --- Array-returning functions -------------------------------------
      ;; current_schemas(bool) → name[]. We run with a single public
      ;; schema; include_implicit differs conceptually in PG (true ⇒
      ;; prepend pg_catalog) but both collapse to {public} here.
      (= fname "current_schemas")
      (let [fn-param (symbol (str "?cur-schemas" (swap! (:var-counter ctx) inc)))
            arg (or (first args) true)
            impl-fn (fn [_include-implicit]
                      (pg-arr/array :name ["public"]))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param arg) result-var])
        result-var)

      ;; string_to_array(s, sep [, null_str]) → text[]
      (= fname "string_to_array")
      (let [fn-param (symbol (str "?str-to-arr" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [s sep & [null-str]]
                      (if (or (nil? s) (= :__null__ s))
                        :__null__
                        (let [pat (if (or (nil? sep) (= "" sep))
                                    #""
                                    (java.util.regex.Pattern/compile
                                     (java.util.regex.Pattern/quote (str sep))))
                              pieces (if (= sep "")
                                       (mapv str (seq s))
                                       (vec (.split pat (str s) -1)))
                              coerced (mapv #(if (and null-str (= % null-str)) nil %)
                                            pieces)]
                          (pg-arr/array :text coerced))))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; regexp_split_to_array(s, pattern) → text[]
      (= fname "regexp_split_to_array")
      (let [fn-param (symbol (str "?re-split-arr" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [s re & _flags]
                      (if (or (nil? s) (= :__null__ s))
                        :__null__
                        (pg-arr/array :text
                                      (vec (.split (java.util.regex.Pattern/compile (str re))
                                                   (str s) -1)))))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; --- Array meta functions -----------------------------------------
      ;; array_length(arr, dim) — NULL for empty or out-of-range dim,
      ;; per-dim count otherwise. Note PG returns NULL for length-0
      ;; even when dim is in range, so we check positivity.
      ;; `coerce-pg-array` reconstructs the PgArray when arr is a
      ;; stored canonical-text string from a native array column.
      (= fname "array_length")
      (let [fn-param (symbol (str "?arr-len" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr dim]
                      (if-let [a (coerce-pg-array arr)]
                        (let [n (pg-arr/length-d a (long dim))]
                          (if (and n (pos? (long n))) n :__null__))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (or (second args) 1)) result-var])
        result-var)

      (= fname "array_upper")
      (let [fn-param (symbol (str "?arr-upper" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr dim]
                      (if-let [a (coerce-pg-array arr)]
                        (let [n (pg-arr/length-d a (long dim))]
                          (if (and n (pos? (long n)))
                            (pg-arr/ubound a (long dim))
                            :__null__))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (or (second args) 1)) result-var])
        result-var)

      (= fname "array_lower")
      (let [fn-param (symbol (str "?arr-lower" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr dim]
                      (if-let [a (coerce-pg-array arr)]
                        (if (and (pos? (pg-arr/length a))
                                 (<= (long dim) (pg-arr/ndim a)))
                          (pg-arr/lbound a (long dim))
                          :__null__)
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (or (second args) 1)) result-var])
        result-var)

      ;; cardinality(arr) — total leaf count across all dimensions
      ;; (PG: differs from array_length, which is per-dim and NULL for
      ;; empty). 0 for empty array; sum across all dims for multi-dim.
      (= fname "cardinality")
      (let [fn-param (symbol (str "?card" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr]
                      (if-let [a (coerce-pg-array arr)]
                        (count (pg-arr/flat-elements a))
                        0))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args)) result-var])
        result-var)

      ;; array_to_string(arr, sep [, null_replace])
      ;; array_to_string(arr, sep [, null_str]) — flattens ALL
      ;; dimensions to leaves and joins with sep. PG: per-leaf
      ;; iteration via ArrayGetNItems regardless of ndim
      ;; (varlena.c:3888). NULL elements are silently skipped when
      ;; null_str is absent; rendered as null_str when provided.
      (= fname "array_to_string")
      (let [fn-param (symbol (str "?arr-to-str" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr sep & [null-replace]]
                      (if-let [a (coerce-pg-array arr)]
                        (let [leaves (pg-arr/flat-elements a)
                              mapped (if null-replace
                                       (mapv #(if (nil? %) (str null-replace) (str %)) leaves)
                                       (keep (fn [e] (when (some? e) (str e))) leaves))]
                          (str/join (str sep) mapped))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; --- Array operator-style functions -------------------------------
      ;; array_append(arr, x) — PG rejects multi-dim
      ;; (`array_userfuncs.c:172-174`); only 1-D supported. NULL arr →
      ;; NULL; NULL x → appended as a NULL leaf. Empty arr starts
      ;; index at 1 (we keep lbound=1 for the result).
      (= fname "array_append")
      (let [fn-param (symbol (str "?arr-app" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr v]
                      (if-let [a (coerce-pg-array arr)]
                        (do
                          (when (pg-arr/multidim? a)
                            (throw (ex-info "array_append rejects multi-dim"
                                            {:error :array-element-error
                                             :detail "array_append: argument must be empty or one-dimensional array"
                                             :ndim (pg-arr/ndim a)})))
                          (pg-arr/array (:elem-type a)
                                        (conj (:elements a) v)
                                        nil
                                        (:lbounds a)))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (second args)) result-var])
        result-var)

      ;; array_prepend(x, arr) — PG rejects multi-dim
      ;; (`array_userfuncs.c:257-259`). 1-D only. PG inserts at
      ;; lbound-1 then restores lbound (line 268-274), so the result
      ;; lbound is unchanged from the input.
      (= fname "array_prepend")
      (let [fn-param (symbol (str "?arr-prep" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [v arr]
                      (if-let [a (coerce-pg-array arr)]
                        (do
                          (when (pg-arr/multidim? a)
                            (throw (ex-info "array_prepend rejects multi-dim"
                                            {:error :array-element-error
                                             :detail "array_prepend: argument must be empty or one-dimensional array"
                                             :ndim (pg-arr/ndim a)})))
                          (pg-arr/array (:elem-type a)
                                        (into [v] (:elements a))
                                        nil
                                        (:lbounds a)))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (second args)) result-var])
        result-var)

      ;; array_cat(a, b) and `||` — PG handles 3 cases via
      ;; cat-rejecting-mismatch in arrays.clj (line 316-548 of
      ;; array_userfuncs.c). NULL handling: a||NULL=a, NULL||b=b
      ;; (line 348-359). Both NULL → NULL.
      (= fname "array_cat")
      (let [fn-param (symbol (str "?arr-cat" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [a b]
                      (let [pa (coerce-pg-array a)
                            pb (coerce-pg-array b)]
                        (cond
                          (and pa pb) (pg-arr/concat-arrs pa pb)
                          pa pa
                          pb pb
                          :else :__null__)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (second args)) result-var])
        result-var)

      ;; array_position(arr, v [, start]) — PG rejects multi-dim
      ;; (`array_userfuncs.c:1345`). Returns position relative to
      ;; lbound. NULL arr → NULL; not-found → NULL; matches IS NOT
      ;; DISTINCT FROM (so NULL element matches NULL search target).
      (= fname "array_position")
      (let [fn-param (symbol (str "?arr-pos" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr v & [start]]
                      (if-let [a (coerce-pg-array arr)]
                        (do
                          (when (pg-arr/multidim? a)
                            (throw (ex-info "array_position rejects multi-dim"
                                            {:error :feature-not-supported
                                             :feature "array_position on multidimensional array"
                                             :detail "searching for elements in multidimensional arrays is not supported"
                                             :ndim (pg-arr/ndim a)})))
                          (let [lb (pg-arr/lbound a 1)
                                start-off (max 0 (- (long (or start lb)) (long lb)))
                                elts (subvec (:elements a) start-off)
                                idx (first (keep-indexed
                                            (fn [i e] (when (= e v) (+ i start-off (long lb))))
                                            elts))]
                            (or idx :__null__)))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param (first args) (second args) (drop 2 args)) result-var])
        result-var)

      ;; array_remove(arr, v) — PG rejects multi-dim
      ;; (`arrayfuncs.c:6423-6426`). 1-D only. Removes ALL elements
      ;; matching v (IS NOT DISTINCT FROM). Result lbound preserved.
      (= fname "array_remove")
      (let [fn-param (symbol (str "?arr-rem" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr v]
                      (if-let [a (coerce-pg-array arr)]
                        (do
                          (when (pg-arr/multidim? a)
                            (throw (ex-info "array_remove rejects multi-dim"
                                            {:error :feature-not-supported
                                             :feature "array_remove on multidimensional array"
                                             :detail "removing elements from multidimensional arrays is not supported"
                                             :ndim (pg-arr/ndim a)})))
                          (pg-arr/array (:elem-type a)
                                        (vec (remove #(= % v) (:elements a)))
                                        nil
                                        (:lbounds a)))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (second args)) result-var])
        result-var)

      ;; array_replace(arr, from, to) — replaces ALL leaf occurrences
      ;; regardless of dim (`arrayfuncs.c:6662`). Shape preserved.
      (= fname "array_replace")
      (let [fn-param (symbol (str "?arr-rep" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [arr from to]
                      (if-let [a (coerce-pg-array arr)]
                        (pg-arr/replace-leaves a #(if (= % from) to %))
                        :__null__))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param (first args) (second args) (nth args 2 nil)) result-var])
        result-var)

      ;; COALESCE(a, b, ...) → first non-null/non-sentinel arg
      (= fname "coalesce")
      (let [_ (ctx/make-columns-optional! ctx args)
            fn-param (symbol (str "?coalesce-fn" (swap! (:var-counter ctx) inc)))
            coalesce-fn (fn [& vals]
                          ;; Can't use `or` here — `false` is a valid value but
                          ;; falsy. Find first non-null explicitly.
                          (let [non-null (remove #(or (nil? %) (= :__null__ %)) vals)]
                            (if (seq non-null) (first non-null) :__null__)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj coalesce-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; NULLIF(a, b) → [(when (not= ?a ?b) ?a) ?result]
      (= fname "nullif")
      ;; NULLIF(a, b) = a if a != b, else :__null__
      (let [_ (ctx/make-columns-optional! ctx [(first args)])
            [a b] args
            fn-param (symbol (str "?nullif-fn" (swap! (:var-counter ctx) inc)))
            nullif-fn (fn [x y] (if (or (= :__null__ x)
                                        (= x y)
                                        (and (number? x) (number? y) (== x y)))
                                  :__null__ x))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj nullif-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param a b) result-var])
        result-var)

      ;; CONCAT(a, b, ...) → [(str ?a ?b ...) ?result]
      (= fname "concat")
      (do (swap! (:where-clauses ctx) conj
                 [(list* 'str args) result-var])
          result-var)

      ;; SUBSTR/SUBSTRING(s, start, len) → [(subs ?s (dec start) (+ (dec start) len)) ?result]
      (or (= fname "substr") (= fname "substring"))
      (let [[s start len] args]
        (if (and (number? start) len (number? len))
          ;; Constant offsets: precompute
          (let [from (dec (long start))
                to (+ from (long len))]
            (swap! (:where-clauses ctx) conj
                   [(list 'subs s from to) result-var])
            result-var)
          ;; Dynamic offsets
          (do (swap! (:where-clauses ctx) conj
                     [(list 'subs s
                            (if (number? start) (dec (long start)) (list 'dec start))
                            (if (and (number? start) (number? len))
                              (+ (dec (long start)) (long len))
                              (list '+ (if (number? start) (dec (long start)) (list 'dec start))
                                    (or len (list 'count s)))))
                      result-var])
              result-var)))

      ;; DATE_TRUNC(precision, ts) → floor to precision boundary.
      ;; Accepts ts as either:
      ;;   - java.util.Date  (the type Datahike returns for :db.type/instant
      ;;     attrs — Metabase's Question Builder breakouts go through this
      ;;     branch with `temporal-unit "month"`)
      ;;   - Long/epoch-seconds (some legacy callers / synthetic values)
      ;; and returns the same shape it received, so downstream
      ;; aggregations / projections see a value of the original type.
      ;; PG's DATE_TRUNC also returns a TIMESTAMP, so Date in / Date out
      ;; is the right wire-side default.
      (= fname "date_trunc")
      (let [[precision ts] args
            fn-param (symbol (str "?date-trunc" (swap! (:var-counter ctx) inc)))
            trunc-zdt (fn [unit ^java.time.ZonedDateTime zdt]
                        (case unit
                          "second"  (.truncatedTo zdt java.time.temporal.ChronoUnit/SECONDS)
                          "minute"  (.truncatedTo zdt java.time.temporal.ChronoUnit/MINUTES)
                          "hour"    (.truncatedTo zdt java.time.temporal.ChronoUnit/HOURS)
                          "day"     (.truncatedTo zdt java.time.temporal.ChronoUnit/DAYS)
                          "week"    (let [dow (.getValue (.getDayOfWeek zdt))
                                          start (.minusDays zdt (dec dow))]
                                      (.truncatedTo start java.time.temporal.ChronoUnit/DAYS))
                          "month"   (.truncatedTo (.withDayOfMonth zdt 1)
                                                  java.time.temporal.ChronoUnit/DAYS)
                          "quarter" (let [m (.getMonthValue zdt)
                                          qstart (- m (mod (dec m) 3))]
                                      (.truncatedTo (.withDayOfMonth (.withMonth zdt qstart) 1)
                                                    java.time.temporal.ChronoUnit/DAYS))
                          "year"    (.truncatedTo (.withDayOfYear zdt 1)
                                                  java.time.temporal.ChronoUnit/DAYS)
                          zdt))
            trunc-fn (fn [prec ts]
                       (when (and prec ts (not= :__null__ ts))
                         (let [unit (let [u (if (keyword? prec) (name prec) (str prec))]
                                      (str/replace u #"s$" ""))]
                           (cond
                             (instance? java.util.Date ts)
                             (let [zdt (.atZone (.toInstant ^java.util.Date ts)
                                                java.time.ZoneOffset/UTC)
                                   trunc (trunc-zdt unit zdt)]
                               (java.util.Date/from (.toInstant ^java.time.ZonedDateTime trunc)))

                             (number? ts)
                             (let [zdt (.atZone (java.time.Instant/ofEpochSecond (long ts))
                                                java.time.ZoneOffset/UTC)
                                   trunc (trunc-zdt unit zdt)]
                               (.getEpochSecond (.toInstant ^java.time.ZonedDateTime trunc)))

                             :else ts))))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj trunc-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param precision ts) result-var])
        result-var)

      ;; DATE_ADD(unit, amount, epoch-seconds) → add amount to timestamp
      (= fname "date_add")
      (let [[_unit amount ts] args]
        (swap! (:where-clauses ctx) conj [(list '+ ts amount) result-var])
        result-var)

      ;; DATE_DIFF(unit, end, start) → difference in days (epoch seconds / 86400)
      (= fname "date_diff")
      (let [[unit end-ts start-ts] args
            fn-param (symbol (str "?date-diff" (swap! (:var-counter ctx) inc)))
            diff-fn (fn [unit end start]
                      (when (and end start (number? end) (number? start))
                        (let [diff (- (long end) (long start))
                              ;; Normalize: accept both singular and plural
                              u (let [raw (if (keyword? unit) (name unit) (str unit))]
                                  (str/replace raw #"s$" ""))]
                          (case u
                            "year"   (let [e (java.time.Instant/ofEpochSecond (long end))
                                           s (java.time.Instant/ofEpochSecond (long start))]
                                       (- (.getYear (.atZone e java.time.ZoneOffset/UTC))
                                          (.getYear (.atZone s java.time.ZoneOffset/UTC))))
                            "month"  (let [e (.atZone (java.time.Instant/ofEpochSecond (long end)) java.time.ZoneOffset/UTC)
                                           s (.atZone (java.time.Instant/ofEpochSecond (long start)) java.time.ZoneOffset/UTC)]
                                       (+ (* 12 (- (.getYear e) (.getYear s)))
                                          (- (.getMonthValue e) (.getMonthValue s))))
                            "week"   (quot diff (* 7 86400))
                            "day"    (quot diff 86400)
                            "hour"   (quot diff 3600)
                            "minute" (quot diff 60)
                            "second" diff
                            diff))))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj diff-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param unit end-ts start-ts) result-var])
        result-var)

      ;; Known mapped functions. Emit via an in-param wrapping `null-safe`
      ;; so SQL NULL propagates (UPPER(NULL)=NULL etc.) instead of throwing
      ;; when a raw Clojure fn receives the `:__null__` keyword sentinel.
      (contains? fns/sql-fn->clj-fn fname)
      (let [clj-fn (get fns/sql-fn->clj-fn fname)
            wrapped (fns/null-safe clj-fn)
            fn-param (symbol (str "?fn-" fname "-"
                                  (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj wrapped)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; jsonb_build_object(k1, v1, k2, v2, ...) → in-param fn
      (= fname "jsonb_build_object")
      (let [fn-param (symbol (str "?jsonb-build-obj" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-build-object)
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      ;; jsonb_build_array(v1, v2, ...) → in-param fn
      (= fname "jsonb_build_array")
      (let [fn-param (symbol (str "?jsonb-build-arr" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-build-array)
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      ;; jsonb_strip_nulls(jsonb) → single-arg in-param fn
      (= fname "jsonb_strip_nulls")
      (let [fn-param (symbol (str "?jsonb-strip-nulls" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-strip-nulls)
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; jsonb_typeof(jsonb) → string type name
      (= fname "jsonb_typeof")
      (let [fn-param (symbol (str "?jsonb-typeof" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-typeof)
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; jsonb_array_length(jsonb) → integer
      (= fname "jsonb_array_length")
      (let [fn-param (symbol (str "?jsonb-arr-len" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-array-length)
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; to_jsonb(any) → parse/pass-through
      (= fname "to_jsonb")
      (let [fn-param (symbol (str "?to-jsonb" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/to-jsonb)
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; jsonb_pretty(jsonb) → formatted JSON string
      (= fname "jsonb_pretty")
      (let [fn-param (symbol (str "?jsonb-pretty" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-pretty)
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; jsonb_set(target, path, new_value) or (target, path, new_value, create_missing?)
      (= fname "jsonb_set")
      (let [fn-param (symbol (str "?jsonb-set" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-set)
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      ;; jsonb_extract_path(target, key1, key2, ...) → jsonb
      ;; Equivalent to #> operator
      (= fname "jsonb_extract_path")
      (let [target (first args)
            path (rest args)
            fn-param (symbol (str "?jsonb-path" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-get-path)
        (swap! (:where-clauses ctx) conj [(list fn-param target (vec path)) result-var])
        result-var)

      ;; jsonb_extract_path_text(target, key1, key2, ...) → text
      ;; Same as jsonb_extract_path but returns text
      (= fname "jsonb_extract_path_text")
      (let [target (first args)
            path (rest args)
            fn-param (symbol (str "?jsonb-pathtext" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-get-path-text)
        (swap! (:where-clauses ctx) conj [(list fn-param target (vec path)) result-var])
        result-var)

      ;; jsonb_insert(target, path, new_value [, insert_after])
      (= fname "jsonb_insert")
      (let [[target path-arg new-val & [_insert-after]] args
            fn-param (symbol (str "?jsonb-insert" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [t p v] (jb/serialize-jsonb (jb/jsonb-insert t p v))))
        (swap! (:where-clauses ctx) conj [(list fn-param target path-arg new-val) result-var])
        result-var)

      ;; jsonb_object_keys(jsonb) → returns set of keys; serialized as JSON array string
      (= fname "jsonb_object_keys")
      (let [target (first args)
            fn-param (symbol (str "?jsonb-keys" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [v] (jb/serialize-jsonb (vec (jb/jsonb-object-keys v)))))
        (swap! (:where-clauses ctx) conj [(list fn-param target) result-var])
        result-var)

      ;; Set-returning functions — when used in SELECT, serialize result
      ;; jsonb_object_agg(key, value) — builds jsonb object from key-value pairs
      ;; NOTE: True GROUP BY aggregation requires custom Datahike aggregate
      ;; support. This handles the scalar/subquery case where it receives
      ;; already-grouped data.
      (= fname "jsonb_object_agg")
      (let [[key-arg val-arg] args
            fn-param (symbol (str "?jsonb-oagg" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [k v] (jb/serialize-jsonb {(str k) v})))
        (swap! (:where-clauses ctx) conj [(list fn-param key-arg val-arg) result-var])
        result-var)

      ;; jsonb_agg(value) — collects values into jsonb array
      (= fname "jsonb_agg")
      (let [target (first args)
            fn-param (symbol (str "?jsonb-agg" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [v] (jb/serialize-jsonb [v])))
        (swap! (:where-clauses ctx) conj [(list fn-param target) result-var])
        result-var)

      ;; string_agg(value, delimiter) — concatenates strings with delimiter
      (= fname "string_agg")
      (let [[val-arg delim-arg] args
            fn-param (symbol (str "?stragg" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [v _d] (str v)))
        (swap! (:where-clauses ctx) conj [(list fn-param val-arg delim-arg) result-var])
        result-var)

      ;; array_agg is handled by the aggregate path (sql-aggregate->datalog
      ;; now includes it; stmt.clj routes it through translate-select's
      ;; aggregate branch which collects values across rows into a PgArray
      ;; via datahike.pg.sql.fns/filter-array-agg).

      ;; Set-returning jsonb functions — when used in SELECT, serialize result
      ;; (proper FROM-clause expansion requires LATERAL which is future work)
      (contains? #{"jsonb_each" "jsonb_each_text" "jsonb_array_elements" "jsonb_array_elements_text"} fname)
      (let [target (first args)
            srf-fn (case fname
                     "jsonb_each"              (fn [v] (jb/serialize-jsonb (vec (jb/jsonb-each v))))
                     "jsonb_each_text"         (fn [v] (jb/serialize-jsonb (vec (jb/jsonb-each-text v))))
                     "jsonb_array_elements"    (fn [v] (jb/serialize-jsonb (jb/jsonb-array-elements v)))
                     "jsonb_array_elements_text" (fn [v] (jb/serialize-jsonb (vec (jb/jsonb-array-elements-text v)))))
            fn-param (symbol (str "?jsonb-srf" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj srf-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param target) result-var])
        result-var)

      ;; PG's format() function — text formatting with %I (quoted
      ;; identifier), %L (quoted literal), %s (raw string), %%
      ;; (escape). Java's String.format chokes on %I/%L, so we
      ;; can't just delegate. Metabase's describe-fields query uses
      ;; FORMAT('%I.%I', schema, table) to build qualified names —
      ;; not optional.
      (= fname "format")
      (let [tmpl-arg (first args)
            value-args (vec (rest args))
            ;; Quote-an-identifier per PG: wrap in "..", escape internal "
            quote-id  (fn [^Object v]
                        (let [s (cond (nil? v) ""
                                      (string? v) v
                                      :else (str v))]
                          (str \" (str/replace s "\"" "\"\"") \")))
            ;; Quote-a-literal per PG: wrap in '..', escape '
            quote-lit (fn [^Object v]
                        (if (nil? v)
                          "NULL"
                          (str \' (str/replace (str v) "'" "''") \')))
            apply-fmt (fn [tmpl values]
                        (let [n (count tmpl)
                              sb (StringBuilder.)]
                          (loop [i 0
                                 vs values]
                            (if (>= i n)
                              (.toString sb)
                              (let [c (.charAt ^String tmpl i)]
                                (if (and (= c \%) (< (inc i) n))
                                  (let [spec (.charAt ^String tmpl (inc i))]
                                    (case spec
                                      \%  (do (.append sb \%)
                                              (recur (+ i 2) vs))
                                      \s  (do (.append sb (str (first vs)))
                                              (recur (+ i 2) (rest vs)))
                                      \I  (do (.append sb ^String (quote-id (first vs)))
                                              (recur (+ i 2) (rest vs)))
                                      \L  (do (.append sb ^String (quote-lit (first vs)))
                                              (recur (+ i 2) (rest vs)))
                                      ;; Unknown spec — keep literal so we don't
                                      ;; silently corrupt user-format strings.
                                      (do (.append sb \%)
                                          (.append sb spec)
                                          (recur (+ i 2) vs))))
                                  (do (.append sb c)
                                      (recur (inc i) vs))))))))
            fmt-fn (fn [tmpl & vs]
                     (apply-fmt (str tmpl) vs))
            fn-param (symbol (str "?pg-format" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj fmt-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param tmpl-arg value-args) result-var])
        result-var)

      ;; Unknown function — pass through as symbol
      :else
      (do (swap! (:where-clauses ctx) conj
                 [(apply list (symbol fname) args) result-var])
          result-var))))

(defn interpret-form
  "Interpret a Clojure-like form against a variable bindings map.
   Handles: arithmetic (+, -, *, /, rem), comparisons (>, <, >=, <=, =, not=),
   boolean (and, or, not), nil checks (nil?, some?), identity, and str.
   Constants and unbound symbols pass through unchanged."
  [form bindings]
  (cond
    (and (symbol? form) (contains? bindings form))
    (get bindings form)

    (or (number? form) (string? form) (keyword? form) (boolean? form) (nil? form))
    form

    (seq? form)
    (let [[op & args] form]
      (case op
        +   (let [[a b] (mapv #(interpret-form % bindings) args)]
              (when (and (some? a) (some? b) (not= :__null__ a) (not= :__null__ b))
                (+ a b)))
        -   (let [[a b] (mapv #(interpret-form % bindings) args)]
              (when (and (some? a) (some? b) (not= :__null__ a) (not= :__null__ b))
                (- a b)))
        *   (let [[a b] (mapv #(interpret-form % bindings) args)]
              (when (and (some? a) (some? b) (not= :__null__ a) (not= :__null__ b))
                (* a b)))
        /   (let [[a b] (mapv #(interpret-form % bindings) args)]
              (when (and (some? a) (some? b) (not= :__null__ a) (not= :__null__ b)
                         (not (zero? b)))
                (/ a b)))
        rem (let [[a b] (mapv #(interpret-form % bindings) args)]
              (when (and (some? a) (some? b) (not= :__null__ a) (not= :__null__ b)
                         (not (zero? b)))
                (rem a b)))
        (> >= < <=)
        (let [[a b] (mapv #(interpret-form % bindings) args)]
          (and (some? a) (some? b) (not= :__null__ a) (not= :__null__ b)
               (case op
                 >  (> a b)
                 >= (>= a b)
                 <  (< a b)
                 <= (<= a b))))
        =     (let [[a b] (mapv #(interpret-form % bindings) args)]
                (= a b))
        not=  (let [[a b] (mapv #(interpret-form % bindings) args)]
                (not= a b))
        and   (every? #(interpret-form % bindings) args)
        or    (some #(interpret-form % bindings) args)
        not   (not (interpret-form (first args) bindings))
        nil?  (let [v (interpret-form (first args) bindings)]
                (nil? v))
        some? (let [v (interpret-form (first args) bindings)]
                (some? v))
        identity (interpret-form (first args) bindings)
        str   (apply str (mapv #(interpret-form % bindings) args))
        ;; Fallback: a generated fn-param symbol bound by the Datalog
        ;; runtime to a user-registered fn (e.g. `?pg-format6` for
        ;; format(), `?case-fn7`, `?in-set8`) — call it on the
        ;; interpreted args. Falls back to clojure.core resolve for
        ;; literal symbols like `count`, `min`.
        (let [evaluated-args (mapv #(interpret-form % bindings) args)
              f-from-bindings (when (symbol? op) (get bindings op))
              f (or (when (fn? f-from-bindings) f-from-bindings)
                    (resolve op))]
          (when f (apply f evaluated-args)))))

    :else form))

(defn translate-case-expr
  "Translate a CASE WHEN expression by compiling a Clojure function
   and passing it as an :in parameter. Returns the result variable.

   Supports both searched CASE (CASE WHEN cond THEN ...) and simple
   CASE (CASE expr WHEN val THEN ...).

   The CASE function takes all referenced variables as arguments and
   evaluates the cond expression at query time."
  [ctx ^CaseExpression case-expr]
  (let [when-clauses (.getWhenClauses case-expr)
        else-expr (.getElseExpression case-expr)
        switch-expr (.getSwitchExpression case-expr)
        result-var (ctx/fresh-var! ctx)
        ;; For simple CASE (CASE x WHEN v THEN r), translate the switch expression
        switch-val (when switch-expr (translate-expr ctx switch-expr))
        ;; Translate conditions and then-values
        branches (mapv (fn [^WhenClause wc]
                         (let [when-val (.getWhenExpression wc)
                               then-val (.getThenExpression wc)
                               test (if switch-val
                                      (list '= switch-val (translate-expr ctx when-val))
                                      (translate-predicate-expr ctx when-val))
                               then (translate-expr ctx then-val)]
                           ;; Detect unsupported: aggregate refs in CASE branches
                           (when (or (map? test) (map? then))
                             (throw (ex-info "aggregate in CASE not supported"
                                             {:error :feature-not-supported
                                              :feature "aggregate function in CASE branch"
                                              :detail "CASE expressions referencing aggregate functions (e.g. CASE WHEN COUNT(*) > 1) are not supported in Datahike SQL. Use a subquery."
                                              :expr (str case-expr)})))
                           {:test test :then then}))
                       when-clauses)
        else-val (when else-expr (translate-expr ctx else-expr))
        ;; Collect all symbols (variables) from the branches
        all-forms (concat (mapcat (fn [{:keys [test then]}] [test then]) branches)
                          (when else-val [else-val]))
        param-vars (vec (ctx/collect-vars all-forms))
        ;; Build the function parameter symbol (for :in)
        fn-param (symbol (str "?case-fn" (swap! (:var-counter ctx) inc)))
        ;; Build the cond body as a Clojure fn.
        ;; Use :__null__ sentinel for missing ELSE (SQL NULL), not nil,
        ;; because Datahike function bindings ignore nil results.
        effective-else (if (nil? else-val) :__null__ else-val)
        case-fn (let [branch-data (mapv (fn [{:keys [test then]}] [test then]) branches)
                      pv param-vars
                      else-v effective-else]
                  (fn [& args]
                    (let [bindings (zipmap pv args)]
                      (or (some (fn [[test-form then-form]]
                                  (when (interpret-form test-form bindings)
                                    (interpret-form then-form bindings)))
                                branch-data)
                          (interpret-form else-v bindings)))))]
    ;; Make column args optional so entities with NULLs aren't excluded
    (ctx/make-columns-optional! ctx param-vars)
    ;; Register the :in parameter and its runtime value
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj case-fn)
    ;; Add the function-call binding: [(?case-fn ?a ?b ...) ?result]
    (swap! (:where-clauses ctx) conj
           [(apply list fn-param param-vars) result-var])
    result-var))

(defn translate-predicate-expr
  "Translate a SQL predicate expression into a Clojure boolean form
   suitable for use inside a cond binding. Unlike translate-predicate which
   returns Datalog where clauses, this returns a single form."
  [ctx expr]
  (cond
    (instance? AndExpression expr)
    (let [^AndExpression e expr]
      (list 'and
            (translate-predicate-expr ctx (.getLeftExpression e))
            (translate-predicate-expr ctx (.getRightExpression e))))

    (instance? OrExpression expr)
    (let [^OrExpression e expr]
      (list 'or
            (translate-predicate-expr ctx (.getLeftExpression e))
            (translate-predicate-expr ctx (.getRightExpression e))))

    (instance? GreaterThan expr)
    (let [^GreaterThan e expr]
      (list '> (translate-expr ctx (.getLeftExpression e))
            (translate-expr ctx (.getRightExpression e))))

    (instance? GreaterThanEquals expr)
    (let [^GreaterThanEquals e expr]
      (list '>= (translate-expr ctx (.getLeftExpression e))
            (translate-expr ctx (.getRightExpression e))))

    (instance? MinorThan expr)
    (let [^MinorThan e expr]
      (list '< (translate-expr ctx (.getLeftExpression e))
            (translate-expr ctx (.getRightExpression e))))

    (instance? MinorThanEquals expr)
    (let [^MinorThanEquals e expr]
      (list '<= (translate-expr ctx (.getLeftExpression e))
            (translate-expr ctx (.getRightExpression e))))

    (instance? EqualsTo expr)
    (let [^EqualsTo e expr
          right (.getRightExpression e)
          any-arr? (and (instance? Function right)
                        (#{"any" "all"}
                         (str/lower-case (.getName ^Function right))))]
      (if any-arr?
        ;; col = ANY(arr) / col = ALL(arr). The array may be an
        ;; ArrayConstructor literal (handled efficiently by the
        ;; translate-predicate WHERE path via or-join) or a runtime
        ;; expression. Here we go through the runtime dispatch since
        ;; predicate-expr is consumed in projection / CASE contexts
        ;; where we need a single boolean-valued form.
        (let [^Function fn-expr right
              kind (str/lower-case (.getName fn-expr))
              arr-expr (some-> (.getParameters fn-expr) (.get 0))
              col-val (translate-expr ctx (.getLeftExpression e))
              arr-val (translate-expr ctx arr-expr)
              col-val (if (seq? col-val) (ctx/materialize-arg! ctx col-val) col-val)
              arr-val (if (seq? arr-val) (ctx/materialize-arg! ctx arr-val) arr-val)
              fn-param (symbol (str "?pg-" kind (swap! (:var-counter ctx) inc)))
              op-fn (case kind
                      "any" (fn [c a]
                              (if-let [arr (coerce-pg-array a)]
                                (boolean (pg-arr/member? arr c)) false))
                      "all" (fn [c a]
                              (if-let [arr (coerce-pg-array a)]
                                (pg-arr/all-match? arr #(= % c)) true)))
              result-var (ctx/fresh-var! ctx)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj op-fn)
          (swap! (:where-clauses ctx) conj
                 [(list fn-param col-val arr-val) result-var])
          result-var)
        (list '= (translate-expr ctx (.getLeftExpression e))
              (translate-expr ctx right))))

    (instance? NotEqualsTo expr)
    (let [^NotEqualsTo e expr]
      (list 'not= (translate-expr ctx (.getLeftExpression e))
            (translate-expr ctx (.getRightExpression e))))

    (instance? IsNullExpression expr)
    (let [^IsNullExpression e expr
          v (translate-expr ctx (.getLeftExpression e))]
      (if (.isNot e)
        (list 'some? v)
        (list 'nil? v)))

    (instance? NotExpression expr)
    (let [^NotExpression e expr
          inner (translate-predicate-expr ctx (.getExpression e))]
      ;; Datahike parses `(not <seq>)` inside a function-binding clause
      ;; as negation-as-failure, so `[(not (= ?a 1)) ?v]` doesn't bind
      ;; ?v — it just filters. Materialise nested seq forms first so
      ;; we emit `[(not ?inner) ?v]` which resolves via clojure.core/not.
      (list 'not (if (seq? inner)
                   (ctx/materialize-arg! ctx inner)
                   inner)))

    ;; col [NOT] IN (literal-list-or-subquery) used inside CASE WHEN /
    ;; nested AND-OR. Lower to a single contains?/or-join form rather
    ;; than the WHERE-clause vector form translate-predicate emits.
    (instance? InExpression expr)
    (let [^InExpression e expr
          not-in? (.isNot e)
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          right (.getRightExpression e)
          vals (cond
                 (instance? ParenthesedExpressionList right)
                 (mapv #(translate-expr ctx %) ^ParenthesedExpressionList right)
                 (instance? ExpressionList right)
                 (mapv #(translate-expr ctx %) ^ExpressionList right)
                 ;; IN (SELECT …) — evaluate the subquery once at
                 ;; translate-time and lift its result column to a
                 ;; value list. Same conservative pattern as the
                 ;; scalar-subquery branch below (translate-expr's
                 ;; ParenthesedSelect handler): non-correlated only;
                 ;; correlated subqueries that throw inside the inner
                 ;; translator fall back to an empty list, which makes
                 ;; the outer `IN` evaluate to false (or, if wrapped
                 ;; in `IS NOT TRUE`, to true). Surfaced by Odoo's
                 ;; view-loading probes that wrap an IN-subquery in
                 ;; IS NOT TRUE.
                 (instance? ParenthesedSelect right)
                 (if-let [parse-fn (:parse-sql ctx)]
                   (try
                     (let [inner-sql (str (.getSelect ^ParenthesedSelect right))
                           parsed   (parse-fn inner-sql (:schema ctx) (:db ctx))
                           q        (:query parsed)
                           in-args  (:in-args parsed)
                           query-db (or (:enriched-db parsed) (:db ctx))
                           q-fn     d/q
                           rows     (if (seq in-args)
                                      (apply q-fn q query-db in-args)
                                      (q-fn q query-db))]
                       (mapv (fn [r] (if (sequential? r) (first r) r)) rows))
                     (catch Throwable _ []))
                   [])
                 :else
                 (throw (ex-info "IN form unsupported in predicate-expr context"
                                 {:error :feature-not-supported
                                  :feature (str "IN expression form: " (.getName ^Class (type right)))
                                  :expr (str right)})))
          non-null-vals (filterv some? vals)
          has-param? (some symbol? non-null-vals)
          set-form (if has-param?
                     ;; Parameter-laden lists — runtime set construction
                     ;; via in-param fn so each Bind sees the current
                     ;; values. (Same trick we use for IN in WHERE.)
                     (let [fn-param (symbol (str "?in-set" (swap! (:var-counter ctx) inc)))
                           build (fn [& xs] (set xs))]
                       (swap! (:in-params ctx) conj fn-param)
                       (swap! (:in-args ctx) conj build)
                       (let [out-var (ctx/fresh-var! ctx)]
                         (swap! (:where-clauses ctx) conj
                                [(apply list fn-param non-null-vals) out-var])
                         out-var))
                     (set non-null-vals))
          base (list 'contains? set-form col)]
      (if not-in? (list 'not base) base))

    ;; col [NOT] LIKE 'pat' inside CASE WHEN. Reuse the LIKE→regex
    ;; compile from translate-predicate (precomputed Pattern literal).
    (instance? LikeExpression expr)
    (let [^LikeExpression e expr
          not-like? (.isNot e)
          case-insensitive? (.isCaseInsensitive e)
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          pattern (translate-expr ctx (.getRightExpression e))
          pat-str (str pattern)
          ^Character esc (or (when-let [c (.getEscape e)]
                               (when-not (str/blank? (str c)) (Character/valueOf (char (first (str c))))))
                             (Character/valueOf \\))
          re-sb (StringBuilder. "^")
          _ (loop [i 0]
              (when (< i (count pat-str))
                (let [c (.charAt ^String pat-str i)]
                  (if (= c (.charValue esc))
                    (if (< (inc i) (count pat-str))
                      (let [next-c (.charAt ^String pat-str (inc i))]
                        (.append re-sb (java.util.regex.Pattern/quote (str next-c)))
                        (recur (+ i 2)))
                      (recur (inc i)))
                    (case c
                      \% (do (.append re-sb ".*") (recur (inc i)))
                      \_ (do (.append re-sb ".") (recur (inc i)))
                      (do (.append re-sb (java.util.regex.Pattern/quote (str c)))
                          (recur (inc i))))))))
          _ (.append re-sb "$")
          re-str (str re-sb)
          re-str (if case-insensitive? (str "(?i)" re-str) re-str)
          re-obj (re-pattern re-str)
          base (list 'boolean (list 're-find re-obj col))]
      (if not-like? (list 'not base) base))

    ;; col [NOT] BETWEEN lo AND hi inside CASE WHEN.
    (instance? Between expr)
    (let [^Between e expr
          not-between? (.isNot e)
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          lo  (translate-expr ctx (.getBetweenExpressionStart e))
          hi  (translate-expr ctx (.getBetweenExpressionEnd e))
          base (list 'and (list '<= lo col) (list '<= col hi))]
      (if not-between? (list 'not base) base))

    ;; col IS [NOT] {TRUE|FALSE|UNKNOWN} inside CASE WHEN.
    (instance? IsBooleanExpression expr)
    (let [^IsBooleanExpression e expr
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          ;; JSqlParser exposes both isTrue/isNot — combine.
          true? (.isTrue e)
          not? (.isNot e)
          target true?  ;; whether comparing to TRUE
          base (list '= col target)]
      (if not? (list 'not base) base))

    ;; col ~ 'pat' / col !~ 'pat' inside CASE WHEN. Same pre-compile
    ;; trick as the WHERE form.
    (instance? RegExpMatchOperator expr)
    (let [^RegExpMatchOperator e expr
          op-type (str (.getOperatorType e))
          negate? (or (= op-type "NOT_MATCH_CASESENSITIVE")
                      (= op-type "NOT_MATCH_CASEINSENSITIVE"))
          ci? (or (= op-type "MATCH_CASEINSENSITIVE")
                  (= op-type "NOT_MATCH_CASEINSENSITIVE"))
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          pattern (translate-expr ctx (.getRightExpression e))
          re-str (let [s (str pattern)] (if ci? (str "(?i)" s) s))
          re-obj (re-pattern re-str)
          base (list 'boolean (list 're-find re-obj col))]
      (if negate? (list 'not base) base))

    ;; A bare value/column expression used as a boolean: must be a
    ;; non-predicate type (literal, Column, Function, etc.). Routing
    ;; through translate-expr on a recognised predicate type would
    ;; ping-pong, so guard explicitly.
    :else
    (cond
      ;; Predicate types that translate-predicate-expr should handle —
      ;; if we reach here it's a gap, fail loudly rather than recurse.
      (or (instance? ExistsExpression expr)
          (instance? JsonOperator expr)
          (instance? DoubleAnd expr))
      (throw (ex-info "predicate not supported in inline boolean context"
                      {:error :feature-not-supported
                       :feature (str "predicate of type " (.getName ^Class (type expr))
                                     " in inline boolean context")
                       :expr (str expr)}))
      :else
      (translate-expr ctx expr))))

(defn parse-timestamp-string
  "Parse a timestamp string in various formats to java.util.Date.
   Handles: ISO-8601, PostgreSQL/JDBC timestamps with various timezone formats,
   date-only strings, and date+offset (e.g. '2000-09-07 -07').
   Returns the original string if all parsing attempts fail."
  [^String s]
  (let [trimmed (str/trim s)
        ;; Strip trailing timezone offset from date-only strings:
        ;; "2000-09-07 -07" → "2000-09-07", "2000-09-07 +00" → "2000-09-07"
        date-only (second (re-find #"^(\d{4}-\d{2}-\d{2})\s+[+-]\d{2}$" trimmed))
        ;; Normalize timestamp formats to ISO-8601
        normalized (-> trimmed
                       (str/replace #"(\d{4}-\d{2}-\d{2})\s+(\d)" "$1T$2")
                       (str/replace #"\+(\d{2})$" "+$1:00")
                       (str/replace #"(?<=\d)-(\d{2})$" "-$1:00"))]
    (or
     ;; Date-only with timezone offset stripped
     (when date-only
       (try
         (java.util.Date/from
          (.toInstant (.atStartOfDay (java.time.LocalDate/parse date-only)
                                     java.time.ZoneOffset/UTC)))
         (catch Exception _ nil)))
     ;; Full timestamp (normalized)
     (try (java.util.Date/from (java.time.Instant/parse normalized))
          (catch Exception _ nil))
     ;; Raw string as-is
     (try (java.util.Date/from (java.time.Instant/parse s))
          (catch Exception _ nil))
     ;; LocalDateTime (no timezone) — treat as UTC
     (try (java.util.Date/from
           (.toInstant (java.time.LocalDateTime/parse normalized)
                       java.time.ZoneOffset/UTC))
          (catch Exception _ nil))
     ;; Date-only (no timezone)
     (try (java.util.Date/from
           (.toInstant (.atStartOfDay (java.time.LocalDate/parse trimmed)
                                      java.time.ZoneOffset/UTC)))
          (catch Exception _ nil))
     ;; PG-lenient date: accepts single-digit month/day (e.g. '2010-11-3').
     ;; PG's date input is forgiving about leading zeros; LocalDate.parse
     ;; above requires strict yyyy-MM-dd, so fall through to a pattern
     ;; formatter that accepts 1- or 2-digit month/day fields.
     (try (java.util.Date/from
           (.toInstant (.atStartOfDay
                        (java.time.LocalDate/parse
                         trimmed
                         (java.time.format.DateTimeFormatter/ofPattern "yyyy-M-d"))
                        java.time.ZoneOffset/UTC)))
          (catch Exception _ nil))
     ;; PG 'MDY' default style accepts 'M/d/y' (US-slash), e.g. '8/10/7777'.
     ;; Only consider it if the trimmed input looks like a slash date;
     ;; don't try on every string.
     (when (re-matches #"\d{1,2}/\d{1,2}/\d{1,4}" trimmed)
       (try (java.util.Date/from
             (.toInstant (.atStartOfDay
                          (java.time.LocalDate/parse
                           trimmed
                           (java.time.format.DateTimeFormatter/ofPattern "M/d/y"))
                          java.time.ZoneOffset/UTC)))
            (catch Exception _ nil)))
     ;; PG also accepts 'Y/M/d' when the year leads (4 digits): the
     ;; canonical Chinook fixture uses '2002/8/14'-style dates. Real
     ;; PG parses these via DateStyle=ISO,MDY so we mirror that.
     (when (re-matches #"\d{4}/\d{1,2}/\d{1,2}" trimmed)
       (try (java.util.Date/from
             (.toInstant (.atStartOfDay
                          (java.time.LocalDate/parse
                           trimmed
                           (java.time.format.DateTimeFormatter/ofPattern "yyyy/M/d"))
                          java.time.ZoneOffset/UTC)))
            (catch Exception _ nil)))
     ;; All parsing failed — return raw string
     s)))

(defn translate-cast-expr
  "Translate a CAST expression to a Datalog function binding.
   For constant values, performs the cast at translation time.
   For variable references, adds a runtime cast binding."
  [ctx ^CastExpression cast-expr]
  (let [inner (.getLeftExpression cast-expr)
        col-data-type (.getColDataType cast-expr)
        ;; Use (str col-data-type) not getDataType so `int[]` / `text[]`
        ;; survive — getDataType returns just the base `"int"` and
        ;; exposes the `[]` via getArrayData.
        type-str (when col-data-type
                   (str/lower-case (str col-data-type)))
        inner-raw (translate-expr ctx inner)
        ;; Type classification from centralized registry
        cast-cat (types/cast-category type-str)
        is-int? (= :integer cast-cat)
        is-float? (= :float cast-cat)
        is-numeric? (= :numeric cast-cat)
        is-text? (= :text cast-cat)
        is-bool? (= :boolean cast-cat)
        is-date? (= :date cast-cat)
        is-time? (= :time cast-cat)
        is-ts? (= :timestamp cast-cat)
        ;; For display/serialization :date and :time still use the
        ;; timestamp runtime path (via parse-timestamp-string) but we
        ;; remember the original cast category so downstream code
        ;; (value->string) can emit the right PG text format.
        any-ts? (or is-ts? is-date? is-time?)
        is-uuid? (= :uuid cast-cat)
        is-array? (= :array cast-cat)]
    (cond
      ;; CAST(<expr> AS T[]) — accept an existing PgArray unchanged
      ;; (element-type retype not supported; we only use the target to
      ;; type an empty / untyped literal). Runtime or compile-time.
      is-array?
      (let [target-elem (types/cast-array-elem-kw type-str)
            fn-param (symbol (str "?cast-arr" (swap! (:var-counter ctx) inc)))
            cast-fn (fn [v]
                      (cond
                        (nil? v)              :__null__
                        (= :__null__ v)       :__null__
                        (pg-arr/array? v)     (pg-arr/array (or target-elem (:elem-type v))
                                                            (:elements v))
                        ;; A bound array param arrives as canonical PG text
                        ;; ("{16384}") or a Clojure collection — reconstruct it
                        ;; (e.g. asyncpg sends `$1::oid[]` as binary oid[] which
                        ;; the wire layer decodes to "{…}"). Without this the
                        ;; cast returned NULL and `col = any($1)` matched nothing.
                        :else                 (if-let [a (coerce-pg-array v target-elem)]
                                                (pg-arr/array (or target-elem (:elem-type a))
                                                              (:elements a))
                                                :__null__)))
            result-var (ctx/fresh-var! ctx)
            inner-val (if (seq? inner-raw) (ctx/materialize-arg! ctx inner-raw) inner-raw)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj cast-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var])
        result-var)
      :else
      (if (and (not (symbol? inner-raw)) (not (seq? inner-raw)))
      ;; Constant value — cast at translation time
        (cond
          is-int?  (coerce/coerce-numeric inner-raw :long)
          is-float? (coerce/coerce-numeric inner-raw :double)
          ;; ::numeric keeps arbitrary precision — parse via the string
          ;; form so a literal's scale survives (0.001000 → scale 6),
          ;; never via double (which would drop trailing zeros).
          is-numeric? (try (java.math.BigDecimal. (str/trim (str inner-raw)))
                           (catch Exception _ inner-raw))
          is-text? (str inner-raw)
          is-bool? (if (instance? Boolean inner-raw)
                     inner-raw
                     (let [b (coerce/parse-bool-token (str inner-raw))]
                       (when (nil? b)
                         (throw (errors/pg-error :invalid-text-representation
                                                 {:type "boolean" :value (str inner-raw)})))
                       b))
        ;; ::date — extract the LocalDate so serialization can omit the
        ;; time part ("2017-03-13" instead of "2017-03-13 00:00:00").
          is-date? (try
                     (java.time.LocalDate/parse
                      (str/trim (str inner-raw))
                      (java.time.format.DateTimeFormatter/ofPattern "yyyy-M-d"))
                     (catch Exception _
                       (let [d (parse-timestamp-string (str inner-raw))]
                         (when (instance? java.util.Date d)
                           (-> ^java.util.Date d .toInstant
                               (.atZone java.time.ZoneOffset/UTC)
                               .toLocalDate)))))
        ;; ::time — extract the LocalTime so serialization emits only
        ;; "HH:MM:SS[.fff]" and drops any date component the input had.
          is-time? (let [s (str/trim (str inner-raw))
                       ;; Accept "HH:MM:SS[.f]", "YYYY-MM-DD HH:MM:SS[.f]",
                       ;; and ISO-8601 with T separator.
                         time-only (or (second (re-find #"^\d{4}-\d{1,2}-\d{1,2}[ T](.+)$" s)) s)]
                     (try (java.time.LocalTime/parse time-only)
                          (catch Exception _ s)))
          is-ts?   (parse-timestamp-string (str inner-raw))
          is-uuid? (java.util.UUID/fromString (str inner-raw))
        ;; ::regnamespace — resolve schema name to namespace OID
        ;; We support a single namespace 'public' with OID 2200
          (= type-str "regnamespace") 2200
        ;; ::regclass — resolve the literal name to the table OID. Prefer
        ;; the stable :pg/table-oid from CREATE TABLE; for legacy tables
        ;; without one, match the hashCode fallback that pg_class and
        ;; pg_attribute synthesize so WHERE attrelid = 'x'::regclass
        ;; actually joins. 0 for unknown names.
          (= type-str "regclass")
          (let [n (str inner-raw)]
            (or (when params/*parse-db* (pgs/table-oid params/*parse-db* n))
                (when (seq n) (Math/abs (.hashCode ^String n)))
                0))
          :else    inner-raw)
      ;; Variable/expression — add runtime cast binding
        (let [inner-val (ctx/materialize-arg! ctx inner-raw)
              result-var (ctx/fresh-var! ctx)]
          (cond
            is-date?
            (let [fn-param (symbol (str "?cast-date" (swap! (:var-counter ctx) inc)))
                  date-fn (fn [v]
                            (when v
                              ;; Temporal instances (now()/current_timestamp
                              ;; bindings, stored instants) must not round-trip
                              ;; through `str` — `(str Date)` is RFC-822ish and
                              ;; unparseable, which dropped the row (issue #13).
                              (cond
                                (instance? java.util.Date v)
                                (-> ^java.util.Date v .toInstant
                                    (.atZone java.time.ZoneOffset/UTC) .toLocalDate)
                                (instance? java.time.Instant v)
                                (-> ^java.time.Instant v
                                    (.atZone java.time.ZoneOffset/UTC) .toLocalDate)
                                (instance? java.time.LocalDate v) v
                                (instance? java.time.LocalDateTime v)
                                (.toLocalDate ^java.time.LocalDateTime v)
                                :else
                                (let [s (str/trim (str v))]
                                  (or (try (java.time.LocalDate/parse
                                            s
                                            (java.time.format.DateTimeFormatter/ofPattern "yyyy-M-d"))
                                           (catch Exception _ nil))
                                      (when-let [d (parse-timestamp-string s)]
                                        (when (instance? java.util.Date d)
                                          (-> ^java.util.Date d .toInstant
                                              (.atZone java.time.ZoneOffset/UTC)
                                              .toLocalDate))))))))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj date-fn)
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var]))

            is-time?
            (let [fn-param (symbol (str "?cast-time" (swap! (:var-counter ctx) inc)))
                  time-fn (fn [v]
                            (when v
                              (cond
                                (instance? java.util.Date v)
                                (-> ^java.util.Date v .toInstant
                                    (.atZone java.time.ZoneOffset/UTC) .toLocalTime)
                                (instance? java.time.Instant v)
                                (-> ^java.time.Instant v
                                    (.atZone java.time.ZoneOffset/UTC) .toLocalTime)
                                (instance? java.time.LocalTime v) v
                                (instance? java.time.LocalDateTime v)
                                (.toLocalTime ^java.time.LocalDateTime v)
                                :else
                                (let [s (str/trim (str v))
                                      time-only (or (second (re-find #"^\d{4}-\d{1,2}-\d{1,2}[ T](.+)$" s)) s)]
                                  (try (java.time.LocalTime/parse time-only)
                                       (catch Exception _ s))))))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj time-fn)
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var]))

            is-ts?
          ;; Timestamp cast: use an in-param function for runtime parsing
            (let [ts-fn-param (symbol (str "?cast-ts" (swap! (:var-counter ctx) inc)))
                  ts-fn (fn [v]
                          (when v
                            (cond
                              (instance? java.util.Date v) v
                              (instance? java.time.LocalDateTime v) v
                              (instance? java.time.Instant v)
                              (java.util.Date/from ^java.time.Instant v)
                              (instance? java.time.LocalDate v)
                              (.atStartOfDay ^java.time.LocalDate v)
                              :else (parse-timestamp-string (str v)))))]
              (swap! (:in-params ctx) conj ts-fn-param)
              (swap! (:in-args ctx) conj ts-fn)
              (swap! (:where-clauses ctx) conj [(list ts-fn-param inner-val) result-var]))

            is-uuid?
            (let [uuid-fn-param (symbol (str "?cast-uuid" (swap! (:var-counter ctx) inc)))
                  uuid-fn (fn [v] (when v (java.util.UUID/fromString (str v))))]
              (swap! (:in-params ctx) conj uuid-fn-param)
              (swap! (:in-args ctx) conj uuid-fn)
              (swap! (:where-clauses ctx) conj [(list uuid-fn-param inner-val) result-var]))

          ;; ::regnamespace — always resolve to OID 2200 (single namespace)
            (= type-str "regnamespace")
            (do (swap! (:where-clauses ctx) conj [(list 'identity 2200) result-var])
                result-var)

          ;; ::regclass — resolve table name to relation OID. Same
          ;; precedence as the literal branch: :pg/table-oid, then
          ;; hashCode fallback, then 0.
            (= type-str "regclass")
            (let [fn-param (symbol (str "?regclass" (swap! (:var-counter ctx) inc)))
                  db params/*parse-db*
                  lookup (fn [v]
                           (when (some? v)
                             (let [n (str v)]
                               (or (when db (pgs/table-oid db n))
                                   (when (seq n) (Math/abs (.hashCode ^String n)))
                                   0))))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj lookup)
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var])
              result-var)

            ;; Numeric / boolean casts on a runtime value. The value may
            ;; arrive as a String — e.g. an extended-protocol parameter
            ;; (`$1::INTEGER`) is sent in text format, so inner-val is the
            ;; String "0". A bare `(long "0")` raises a ClassCastException
            ;; ("String cannot be cast to Number"), so route through the
            ;; string-tolerant coerce helpers via an in-param fn, the same
            ;; way the date/ts/uuid branches above do.
            ;; ::numeric — preserve arbitrary precision / scale. The value
            ;; may already be a BigDecimal (asyncpg sends numeric params in
            ;; binary, decoded to BigDecimal) — keep it; a text param
            ;; (node) parses via the string form so its scale survives.
            ;; Never route through double, which drops precision.
            is-numeric?
            (let [fn-param (symbol (str "?cast-num" (swap! (:var-counter ctx) inc)))
                  cast-fn (fn [v]
                            (when (and (some? v) (not= :__null__ v))
                              (cond
                                (instance? java.math.BigDecimal v) v
                                (instance? java.math.BigInteger v) (java.math.BigDecimal. ^java.math.BigInteger v)
                                (integer? v) (java.math.BigDecimal/valueOf (long v))
                                :else (java.math.BigDecimal. (str/trim (str v))))))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj cast-fn)
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var]))

            (or is-int? is-float? is-bool?)
            (let [fn-param (symbol (str "?cast-num" (swap! (:var-counter ctx) inc)))
                  cast-fn (cond
                            is-int?   (fn [v] (when (and (some? v) (not= :__null__ v))
                                                (coerce/coerce-numeric v :long)))
                            is-float? (fn [v] (when (and (some? v) (not= :__null__ v))
                                                (coerce/coerce-numeric v :double)))
                            :else     (fn [v] (when (and (some? v) (not= :__null__ v))
                                                (if (boolean? v) v
                                                    (coerce/parse-bool-token (str v))))))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj cast-fn)
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var]))

            :else
            ;; Text / unknown scalar cast → stringify. EXCEPT a PgRecord (a
            ;; ROW(...) being cast to a named composite, e.g.
            ;; `ROW(..)::test_composite`): keep the record so value->string
            ;; emits canonical record_out text and the column's composite OID
            ;; (from cast-oid) drives the binary codec. `str`-ing it would
            ;; corrupt it to "…PgRecord@hash".
            (let [fn-param (symbol (str "?cast-s" (swap! (:var-counter ctx) inc)))
                  cast-fn  (fn [v]
                             (cond
                               (nil? v)           nil
                               (= :__null__ v)    :__null__
                               (pg-rec/record? v) v
                               ;; bytea: keep the byte[] so value->string emits
                               ;; PG hex `\x…` rather than the Java array toString.
                               (bytes? v)         v
                               :else              (str v)))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj cast-fn)
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var])))
          result-var)))))

(def ^:private arith-op->null-safe
  "Map Clojure arithmetic op-sym to the fully-qualified fns/null-safe variant
   defined in this ns. Datahike's query engine resolves these symbols at
   run time to the `null-safe` wrapped fns, so `1 + NULL = NULL` rather
   than throwing."
  '{+ datahike.pg.sql/sql-+
    - datahike.pg.sql/sql--
    * datahike.pg.sql/sql-*
    / datahike.pg.sql/sql-div
    rem datahike.pg.sql/sql-mod})

(defn translate-binary-arith
  "Translate a binary arithmetic expression. Materializes sub-expression
   operands. When operands are aggregate markers, returns a compound-agg
   descriptor instead of a Datalog form.

   Uses fns/null-safe arithmetic so `col + 1` evaluates to `:__null__` when
   `col` is NULL, matching SQL. The compound-agg path keeps the raw
   op-sym — aggregate compound evaluation is numeric-only and runs
   server-side after the query."
  [ctx ^net.sf.jsqlparser.expression.BinaryExpression expr op-sym]
  (let [l (translate-expr ctx (.getLeftExpression expr))
        r (translate-expr ctx (.getRightExpression expr))]
    (if (or (map? l) (map? r))
      ;; Compound aggregate expression: return descriptor for SELECT handler
      {:compound-agg true :op op-sym :left l :right r :expr expr}
      (let [emit-op (get arith-op->null-safe op-sym op-sym)]
        (list emit-op (if (seq? l) (ctx/materialize-arg! ctx l) l)
              (if (seq? r) (ctx/materialize-arg! ctx r) r))))))

(defn flatten-json-chain
  "Flatten a JsonExpression into {:base Expression :chain [[key op-str] ...]} pairs.
   JSqlParser encodes chained access recursively: data->'a'->>'b' is
   JsonExpression(base=data, idents=[JsonExpression(base='a', idents=['b'], ops=[->>])], ops=[->])
   We flatten to {:base data-Column, :chain [['a' '->'] ['b' '->>']]}.

   For a single-step access col->>'key':
   JsonExpression(base=col, idents=['key'], ops=[->>])
   → {:base col-Column, :chain [['key' '->>']]}"
  [^JsonExpression je]
  (let [base (.getExpression je)
        idents (vec (.getIdents je))
        ops (vec (.getOperators je))]
    (if (and (= 1 (count idents)) (instance? JsonExpression (first idents)))
      ;; Chained: outer op applies to (base -> inner-base), inner handles the rest.
      ;; data->'a'->>'b': outer op='->', ident=JsonExpression(base='a', idents=['b'], ops=[->>])
      ;; → step (-> base 'a') then flatten inner with base replaced by result
      (let [inner ^JsonExpression (first idents)
            inner-base (.getExpression inner)
            outer-key (cond
                        (instance? StringValue inner-base) (string-value-text ^StringValue inner-base)
                        (instance? LongValue inner-base)   (.getValue ^LongValue inner-base)
                        :else (str inner-base))
            outer-op (first ops)
            ;; Recurse: treat the inner JsonExpression as if its base were already resolved
            inner-chain (:chain (flatten-json-chain inner))]
        {:base base :chain (into [[outer-key outer-op]] inner-chain)})
      ;; Simple: single step — ident is a literal key or index
      (let [ident (first idents)
            key-val (cond
                      (instance? StringValue ident) (string-value-text ^StringValue ident)
                      (instance? LongValue ident)   (.getValue ^LongValue ident)
                      :else (str ident))]
        {:base base :chain [[key-val (first ops)]]}))))

(defn translate-expr
  "Translate a JSqlParser Expression to a value, variable, or predicate form.
   Returns a Datalog-compatible value or variable symbol."
  [ctx expr]
  (cond
    ;; current_schema (no parens) used as column reference — return 'public' constant
    (and (instance? Column expr)
         (= "current_schema" (.getColumnName ^Column expr))
         (nil? (.getTable ^Column expr)))
    "public"

    ;; current_user / session_user / user / system_user as bare
    ;; identifiers (PG keywords; JSqlParser surfaces them as Column).
    ;; All collapse to the static handler role.
    (and (instance? Column expr)
         (nil? (.getTable ^Column expr))
         (contains? #{"current_user" "session_user" "user" "system_user"}
                    (str/lower-case (.getColumnName ^Column expr))))
    "datahike"

    (instance? Column expr)
    (let [^Column col-expr expr
          tbl (.getTable col-expr)
          tbl-name (when tbl (unquote-ident (.getName ^Table tbl)))
          ;; JSqlParser parses `xs[2]` as a Column with a side-channel
          ;; `ArrayConstructor` carrying the indices: `(.getColumnName)`
          ;; returns the bare name `xs`; `(.getArrayConstructor)` is the
          ;; `[2]` part. For deeper chains like `xs[2][3]`, JSqlParser
          ;; emits `ArrayExpression(Column[xs[2]], 3)` — handled by the
          ;; ArrayExpression branch above. We only need to recognise
          ;; the single-bracket Column-with-ArrayConstructor case here
          ;; so users can write `WHERE xs[2] = 20` or `SELECT xs[2]`.
          ac (.getArrayConstructor col-expr)]
      (cond
        (and tbl-name params/*from-bindings* (contains? params/*from-bindings* tbl-name))
        ;; Bound by UPDATE ... FROM (VALUES ...) AS alias(cols)
        (get-in params/*from-bindings* [tbl-name (unquote-ident (.getColumnName col-expr))])

        (some? ac)
        ;; Walk the bracket-expressions left-to-right, applying
        ;; pg-arr/subscript once per `[expr]`. Each step routes the
        ;; input through coerce-pg-array so a stored canonical-text
        ;; array column is reconstructed transparently.
        (let [bare-col (let [c (Column.)]
                         (.setColumnName c (.getColumnName col-expr))
                         (when tbl (.setTable c ^Table tbl))
                         c)
              base-var (translate-expr ctx bare-col)
              base-var (if (seq? base-var) (ctx/materialize-arg! ctx base-var) base-var)
              idx-exprs (.getExpressions ^ArrayConstructor ac)]
          (reduce (fn [container idx-expr]
                    (let [idx-val   (translate-expr ctx idx-expr)
                          idx-val   (if (seq? idx-val) (ctx/materialize-arg! ctx idx-val) idx-val)
                          fn-param  (symbol (str "?col-sub" (swap! (:var-counter ctx) inc)))
                          sub-fn    (fn [arr i]
                                      (if-let [a (coerce-pg-array arr)]
                                        (let [v (pg-arr/subscript a i)]
                                          (if (nil? v) :__null__ v))
                                        :__null__))
                          result-var (ctx/fresh-var! ctx)]
                      (swap! (:in-params ctx) conj fn-param)
                      (swap! (:in-args ctx) conj sub-fn)
                      (swap! (:where-clauses ctx) conj
                             [(list fn-param container idx-val) result-var])
                      result-var))
                  base-var
                  idx-exprs))

        :else
        (let [resolved (ctx/resolve-column expr
                                           (:table-aliases ctx)
                                           (:default-table ctx)
                                           (:col-overrides ctx)
                                           (:derived-aliases ctx))]
          (ctx/col-var! ctx resolved))))

    (instance? AllColumns expr)
    :*

    (instance? LongValue expr)
    (.getValue ^LongValue expr)

    (instance? DoubleValue expr)
    (.getValue ^DoubleValue expr)

    (instance? StringValue expr)
    (string-value-text ^StringValue expr)

    (instance? NullValue expr)
    nil

    (instance? BooleanValue expr)
    (.getValue ^BooleanValue expr)

    ;; ARRAY[…] as a projection value. Element type inferred from the
    ;; first non-null literal element (LongValue→:int8, DoubleValue→
    ;; :float8, BooleanValue→:bool, StringValue→:text). Mixed/unknown
    ;; → :text (PG would raise; we're lenient). Elements may be vars,
    ;; so we bind via an in-param closure — analogous to the
    ;; jsonb_build_array branch below.
    (instance? ArrayConstructor expr)
    (let [exprs (.getExpressions ^ArrayConstructor expr)
          ;; Detect the leaf element type by walking through nested
          ;; ArrayConstructors. Without this, `ARRAY[ARRAY[1,2],
          ;; ARRAY[3,4]]` gets an outer `:elem-type :text` (no
          ;; LongValue at the top level) — wrong for OID inference
          ;; and for any operator that reads `:elem-type` directly.
          ;; Falls back to :text when no typed literal is found
          ;; (matches PG's empty-array-untyped behaviour).
          detect-elem (fn detect-elem [es]
                        (or (some (fn [e]
                                    (cond
                                      (instance? LongValue e)        :int8
                                      (instance? DoubleValue e)      :float8
                                      (instance? StringValue e)      :text
                                      (instance? BooleanValue e)     :bool
                                      (instance? ArrayConstructor e)
                                      (detect-elem (.getExpressions ^ArrayConstructor e))
                                      (instance? CastExpression e)
                                      (let [t (-> (str (.getColDataType ^CastExpression e))
                                                  clojure.string/lower-case
                                                  clojure.string/trim
                                                  ;; strip (p,s) typmod
                                                  (clojure.string/replace #"\s*\([^)]*\)" ""))]
                                        (get types/sql-name->elem-kw t))
                                      :else nil))
                                  es)
                            :text))
          elem-type (detect-elem exprs)
          args (mapv #(translate-expr ctx %) exprs)
          arg-vars (mapv #(if (seq? %) (ctx/materialize-arg! ctx %) %) args)
          fn-param (symbol (str "?pg-arr-ctor" (swap! (:var-counter ctx) inc)))
          build-fn (fn [& elements]
                     (pg-arr/array elem-type (vec elements)))
          result-var (ctx/fresh-var! ctx)]
      (if (empty? exprs)
        ;; Empty array — bind a constant PgArray value directly.
        (let [ident-param (symbol (str "?pg-arr-empty" (swap! (:var-counter ctx) inc)))
              empty-arr (pg-arr/array elem-type [])]
          (swap! (:in-params ctx) conj ident-param)
          (swap! (:in-args ctx) conj (fn [] empty-arr))
          (swap! (:where-clauses ctx) conj [(list ident-param) result-var])
          result-var)
        (do
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj build-fn)
          (swap! (:where-clauses ctx) conj
                 [(apply list fn-param arg-vars) result-var])
          result-var)))

    ;; arr[N] — PG array subscripting. JSqlParser surfaces both the
    ;; single-element form (getIndexExpression) and the slice form
    ;; (getStartIndexExpression + getStopIndexExpression). Out-of-range
    ;; returns NULL per PG semantics — pg-arr/subscript implements
    ;; that exactly. For slices we emit pg-arr/slice which returns a
    ;; new PgArray.
    (instance? ArrayExpression expr)
    (let [^ArrayExpression ae expr
          container (translate-expr ctx (.getObjExpression ae))
          container (if (seq? container) (ctx/materialize-arg! ctx container) container)
          slice? (nil? (.getIndexExpression ae))
          result-var (ctx/fresh-var! ctx)]
      (if (or slice?
              ;; JSqlParser 5 surfaces `arr[lo:hi]` as a single
              ;; IndexExpression of class JsonExpression with a
              ;; toString like "2:4" instead of separate start/stop
              ;; index expressions. Detect + split.
              (let [idx-expr (.getIndexExpression ae)]
                (and (some? idx-expr)
                     (instance? JsonExpression idx-expr)
                     (str/includes? (str idx-expr) ":"))))
        ;; Slice: arr[lo:hi] (either bound may be absent → use nil and
        ;; let pg-arr/slice fall back to defaults).
        (let [idx-expr (.getIndexExpression ae)
              json-slice? (and (some? idx-expr)
                               (instance? JsonExpression idx-expr)
                               (str/includes? (str idx-expr) ":"))
              [lo-exp hi-exp]
              (if json-slice?
                (let [[l h] (str/split (str idx-expr) #":" 2)]
                  [(when-not (str/blank? l) (Long/parseLong l))
                   (when-not (str/blank? h) (Long/parseLong h))])
                [(.getStartIndexExpression ae)
                 (.getStopIndexExpression ae)])
              lo (cond
                   json-slice?           lo-exp
                   (nil? lo-exp)         nil
                   :else                 (translate-expr ctx lo-exp))
              hi (cond
                   json-slice?           hi-exp
                   (nil? hi-exp)         nil
                   :else                 (translate-expr ctx hi-exp))
              lo (if (seq? lo) (ctx/materialize-arg! ctx lo) lo)
              hi (if (seq? hi) (ctx/materialize-arg! ctx hi) hi)
              fn-param (symbol (str "?pg-arr-slice" (swap! (:var-counter ctx) inc)))
              slice-fn (fn [arr lo hi]
                         (if-let [a (coerce-pg-array arr)]
                           (pg-arr/slice a lo hi)
                           :__null__))]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj slice-fn)
          (swap! (:where-clauses ctx) conj
                 [(list fn-param container lo hi) result-var])
          result-var)
        ;; Single element: arr[idx]. Wrap pg-arr/subscript so SQL-NULL
        ;; (out-of-range) rides as :__null__ — Datalog's var-binding
        ;; semantics drops rows whose find-var resolves to plain nil,
        ;; but preserves rows bound to our sentinel. value->string
        ;; converts back to SQL NULL on the wire.
        (let [idx (translate-expr ctx (.getIndexExpression ae))
              idx (if (seq? idx) (ctx/materialize-arg! ctx idx) idx)
              fn-param (symbol (str "?pg-arr-sub" (swap! (:var-counter ctx) inc)))
              sub-fn (fn [arr i]
                       (if-let [a (coerce-pg-array arr)]
                         (let [v (pg-arr/subscript a i)]
                           (if (nil? v) :__null__ v))
                         :__null__))]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj sub-fn)
          (swap! (:where-clauses ctx) conj
                 [(list fn-param container idx) result-var])
          result-var)))

    ;; Prepared-statement parameter placeholder: `?` (index auto-assigned
    ;; by JSqlParser) or `$N` (explicit 1-based index).
    ;;
    ;; Two modes:
    ;; - Parse time (*bound-params* is nil): emit a fresh `?pN` in-param
    ;;   var, record the index in :param-placeholders, and push a
    ;;   ParamRef sentinel into :in-args so Execute can swap in the
    ;;   bound value at the right position. Repeated use of the same
    ;;   index returns the same var so `WHERE a = $1 OR b = $1` unifies.
    ;; - Execute-time re-translation (*bound-params* bound): look up the
    ;;   value and return it directly as a literal. Used for UPDATE/
    ;;   DELETE where-expr that is kept as a JSqlParser AST at Parse.
    (instance? JdbcParameter expr)
    (let [idx (.getIndex ^JdbcParameter expr)]
      (if-let [bound params/*bound-params*]
        (nth bound (dec (long idx)))
        (let [holders (:param-placeholders ctx)]
          (if-let [existing (get @holders idx)]
            existing
            (let [v (symbol (str "?p" idx))]
              (swap! holders assoc idx v)
              (swap! (:in-params ctx) conj v)
              (swap! (:in-args ctx) conj (params/->ParamRef idx))
              v)))))

    (instance? Parenthesis expr)
    (translate-expr ctx (.getExpression ^Parenthesis expr))

    (instance? ParenthesedExpressionList expr)
    (let [^ParenthesedExpressionList pel expr]
      (if (= 1 (.size pel))
        (translate-expr ctx (.get pel 0))
        (mapv #(translate-expr ctx %) pel)))

    (instance? SignedExpression expr)
    (let [^SignedExpression se expr
          sign (.getSign se)
          inner (translate-expr ctx (.getExpression se))]
      (if (= sign \-)
        (if (number? inner) (- inner) (list '* -1 inner))
        inner))

    ;; Arithmetic — materialize sub-expression operands to ensure Datahike
    ;; evaluates flat function bindings (no nested lists like (+ ?a (* ?b ?c)))
    (instance? Addition expr) (translate-binary-arith ctx expr '+)
    (instance? Subtraction expr) (translate-binary-arith ctx expr '-)
    (instance? Multiplication expr) (translate-binary-arith ctx expr '*)
    (instance? Division expr) (translate-binary-arith ctx expr '/)
    (instance? Modulo expr) (translate-binary-arith ctx expr 'rem)

    ;; PG operators that overload on arrays: @> (contains), <@ (contained
    ;; by), && (overlap). JSqlParser uses JsonOperator for @> and <@,
    ;; DoubleAnd for &&. We dispatch at runtime to either the array
    ;; predicates in datahike.pg.arrays or the jsonb predicates in
    ;; datahike.pg.jsonb based on operand type.
    (or (instance? JsonOperator expr)
        (instance? DoubleAnd expr))
    (let [op-str (cond
                   (instance? JsonOperator expr)
                   (.getStringExpression ^JsonOperator expr)
                   (instance? DoubleAnd expr) "&&")
          ^net.sf.jsqlparser.expression.BinaryExpression be expr
          l (translate-expr ctx (.getLeftExpression be))
          r (translate-expr ctx (.getRightExpression be))
          l (if (seq? l) (ctx/materialize-arg! ctx l) l)
          r (if (seq? r) (ctx/materialize-arg! ctx r) r)
          fn-param (symbol (str "?pg-arr-op" (swap! (:var-counter ctx) inc)))
          op-fn (case op-str
                  "@>" (fn [a b]
                         (cond
                           (and (pg-arr/array? a) (pg-arr/array? b))
                           (pg-arr/contains-arr? a b)
                           :else (jb/jsonb-contains? a b)))
                  "<@" (fn [a b]
                         (cond
                           (and (pg-arr/array? a) (pg-arr/array? b))
                           (pg-arr/contains-arr? b a)
                           :else (jb/jsonb-contained? a b)))
                  "&&" (fn [a b]
                         (if (and (pg-arr/array? a) (pg-arr/array? b))
                           (pg-arr/overlap? a b)
                           false))
                  "?"  jb/jsonb-exists?
                  "?|" jb/jsonb-exists-any?
                  "?&" jb/jsonb-exists-all?
                  nil)
          result-var (ctx/fresh-var! ctx)]
      (when op-fn
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj op-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param l r) result-var]))
      result-var)

    ;; || — string concat on scalars, array concat on arrays. We dispatch
    ;; at runtime since the operand types aren't known at parse time.
    (instance? Concat expr)
    (let [^Concat e expr
          l (translate-expr ctx (.getLeftExpression e))
          r (translate-expr ctx (.getRightExpression e))
          l (if (seq? l) (ctx/materialize-arg! ctx l) l)
          r (if (seq? r) (ctx/materialize-arg! ctx r) r)
          fn-param (symbol (str "?pg-concat" (swap! (:var-counter ctx) inc)))
          concat-fn (fn [a b]
                      (cond
                        (and (pg-arr/array? a) (pg-arr/array? b))
                        (pg-arr/concat-arrs a b)
                        ;; Append/prepend scalar to array — PG allows
                        ;; `arr || scalar` and `scalar || arr`.
                        (pg-arr/array? a)
                        (pg-arr/array (:elem-type a) (conj (:elements a) b))
                        (pg-arr/array? b)
                        (pg-arr/array (:elem-type b) (into [a] (:elements b)))
                        :else (str a b)))
          result-var (ctx/fresh-var! ctx)]
      (swap! (:in-params ctx) conj fn-param)
      (swap! (:in-args ctx) conj concat-fn)
      (swap! (:where-clauses ctx) conj
             [(list fn-param l r) result-var])
      result-var)

    ;; CASE WHEN ... THEN ... ELSE ... END
    (instance? CaseExpression expr)
    (translate-case-expr ctx ^CaseExpression expr)

    ;; CAST(x AS type)
    (instance? CastExpression expr)
    (translate-cast-expr ctx ^CastExpression expr)

    ;; Functions — aggregate or scalar
    (instance? Function expr)
    (let [^Function f expr
          fname (str/lower-case (.getName f))
          params (when-let [pl (.getParameters f)]
                   (.getExpressions ^ExpressionList pl))]
      (cond
        ;; ARRAY(SELECT col FROM …) — PG's "construct array from
        ;; subquery". JSqlParser parses it as Function name="array"
        ;; with the inner SELECT as the sole argument. Pre-evaluate
        ;; the inner SELECT (catalog-enriched db is in scope) and
        ;; wrap all returned rows in a PgArray. Single-column inner
        ;; → flat 1-D array (matches PG); multi-column inner is rare
        ;; in PG idiom and would need a row-typed array, so we take
        ;; the first column only and document it.
        ;;
        ;; psql's `\d <table>` uses this:
        ;;   array_to_string(array(SELECT rolname FROM pg_roles
        ;;                          WHERE oid = ANY(pol.polroles)), ',')
        ;; Correlated against the outer pol row. With our virtual
        ;; pg_policy empty, the outer never iterates and the array()
        ;; never executes — but we still need a valid translation.
        (and (= fname "array")
             (= 1 (count params))
             (or (instance? PlainSelect (first params))
                 (instance? ParenthesedSelect (first params))))
        (let [arg (first params)
              inner (if (instance? ParenthesedSelect arg)
                      (.getSelect ^ParenthesedSelect arg)
                      arg)
              db (:db ctx)]
          (if (and db (instance? PlainSelect inner) (:parse-sql ctx))
            (try
              (let [parse-fn (:parse-sql ctx)
                    parsed   (parse-fn (str inner) (:schema ctx) db)
                    q        (:query parsed)
                    in-args  (:in-args parsed)
                    query-db (or (:enriched-db parsed) db)
                    q-fn     d/q
                    results  (if (seq in-args)
                               (apply q-fn q query-db in-args)
                               (q-fn q query-db))
                    flat     (vec (map (fn [row]
                                         (if (sequential? row) (first row) row))
                                       results))]
                (pg-arr/array :text flat))
              (catch Throwable _ (pg-arr/array :text [])))
            (pg-arr/array :text [])))

        (fns/aggregate-function? fname)
        ;; handled at select-item level — return marker
        {:aggregate true :fn fname :params (.getParameters f)}

        ;; Non-aggregate function → Datalog function binding
        :else (translate-function-call ctx f)))

    ;; CURRENT_TIMESTAMP, CURRENT_DATE, CURRENT_TIME
    (instance? TimeKeyExpression expr)
    (let [key-str (str/lower-case (str/trim (str expr)))
          result-var (ctx/fresh-var! ctx)
          now-fn (cond
                   (or (= key-str "current_timestamp")
                       (= key-str "now()"))
                   (fn [] (java.util.Date.))
                   ;; LocalDate (not a midnight java.util.Date) so the
                   ;; result renders as "yyyy-MM-dd" with OID 1082, like
                   ;; PG's date type.
                   (= key-str "current_date")
                   (fn [] (java.time.LocalDate/now java.time.ZoneOffset/UTC))
                   (= key-str "current_time")
                   (fn [] (java.util.Date.))
                   :else (fn [] (java.util.Date.)))
          fn-param (symbol (str "?now-fn" (swap! (:var-counter ctx) inc)))]
      (swap! (:in-params ctx) conj fn-param)
      (swap! (:in-args ctx) conj now-fn)
      (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
      result-var)

    ;; jsonb field/element access: col->'key', col->>'key', col->'a'->>'b'
    (instance? JsonExpression expr)
    (let [{:keys [base chain]} (flatten-json-chain ^JsonExpression expr)
          base-val (translate-expr ctx base)]
      (reduce
       (fn [current [key-val op-str]]
         (let [op-fn  (if (= op-str "->>") jb/jsonb-get-text jb/jsonb-get)
               param  (symbol (str "?json-op" (swap! (:var-counter ctx) inc)))
               result (ctx/fresh-var! ctx)]
           (swap! (:in-params ctx) conj param)
           (swap! (:in-args ctx) conj op-fn)
           (swap! (:where-clauses ctx) conj [(list param current key-val) result])
           result))
       base-val
       chain))

    ;; expr AT TIME ZONE 'zone' — e.g. now() AT TIME ZONE 'UTC'
    (instance? TimezoneExpression expr)
    (let [left (.getLeftExpression ^TimezoneExpression expr)]
      (if (and (instance? Function left)
               (= "now" (str/lower-case (.getName ^Function left))))
        (let [fn-param (symbol (str "?now-fn" (swap! (:var-counter ctx) inc)))
              now-fn (fn [] (java.util.Date.))
              result-var (ctx/fresh-var! ctx)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj now-fn)
          (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
          result-var)
        (translate-expr ctx left)))

    ;; Boolean-producing operators as SELECT-list projections.
    ;; PG treats `SELECT col > 5 FROM t` as projecting a BOOL column;
    ;; WHERE and HAVING are the dominant use of these, but they're
    ;; valid anywhere. Delegate to translate-predicate-expr which
    ;; produces a Clojure form; stmt.clj's :else branch materializes
    ;; it via ctx/materialize-arg! to bind the boolean to a fresh var.
    (or (instance? GreaterThan expr)
        (instance? GreaterThanEquals expr)
        (instance? MinorThan expr)
        (instance? MinorThanEquals expr)
        (instance? EqualsTo expr)
        (instance? NotEqualsTo expr)
        (instance? IsNullExpression expr)
        (instance? NotExpression expr)
        (instance? AndExpression expr)
        (instance? OrExpression expr)
        (instance? LikeExpression expr)
        (instance? Between expr)
        (instance? InExpression expr))
    (translate-predicate-expr ctx expr)

    ;; Scalar subquery in projection / expression position — `(SELECT
    ;; col FROM t WHERE …)`. PG semantics: returns one value per outer
    ;; row, NULL if the inner produces no rows.
    ;;
    ;; psql's `\d <table>` projects two such subqueries:
    ;;   (SELECT pg_get_expr(d.adbin, d.adrelid, true) FROM pg_attrdef d
    ;;    WHERE d.adrelid = a.attrelid AND d.adnum = a.attnum AND a.atthasdef)
    ;;   (SELECT c.collname FROM pg_collation c, pg_type t WHERE …)
    ;; Both reference catalog tables that are empty in our virtual
    ;; catalog (we don't track column defaults or collations as rows),
    ;; so the inner produces no rows for any outer row → NULL.
    ;;
    ;; Strategy: attempt to evaluate the inner SELECT once at
    ;; translate time against the (catalog-enriched) db. Three cases:
    ;;   1. Non-correlated, returns rows → use first row's first col.
    ;;   2. Non-correlated, returns 0 rows → :__null__.
    ;;   3. Correlated (references outer aliases the inner translator
    ;;      can't resolve) → translate-select throws → :__null__.
    ;; Case 3 is a planned conservative fallback: we don't yet have a
    ;; per-row scalar-subquery executor, but the catalog tables that
    ;; drive correlated psql probes (pg_attrdef, pg_collation) are
    ;; empty in our impl, so the correct PG result IS NULL.
    (or (instance? ParenthesedSelect expr) (instance? PlainSelect expr))
    (let [inner (if (instance? ParenthesedSelect expr)
                  (.getSelect ^ParenthesedSelect expr)
                  expr)
          db    (:db ctx)]
      (if (and db (instance? PlainSelect inner) (:parse-sql ctx))
        (try
          (let [parse-fn   (:parse-sql ctx)
                inner-sql  (str inner)
                parsed     (parse-fn inner-sql (:schema ctx) db)
                q          (:query parsed)
                in-args    (:in-args parsed)
                query-db   (or (:enriched-db parsed) db)
                q-fn       d/q
                results    (if (seq in-args)
                             (apply q-fn q query-db in-args)
                             (q-fn q query-db))
                first-row  (first results)
                v          (if (sequential? first-row) (first first-row) first-row)]
            (if (some? v) v :__null__))
          (catch Throwable _ :__null__))
        :__null__))

    :else
    (throw (ex-info "unsupported SQL expression"
                    {:error :feature-not-supported
                     :feature (str "expression of type " (.getName ^Class (type expr)))
                     :expr (str expr)}))))

;; ============================================================================
;; WHERE clause translation: SQL predicates → Datalog :where clauses
;; ============================================================================

(defn- literal-array-elements
  "If expr is an ArrayConstructor or '{…}' StringValue, return a vector of
   translated elements; else nil. Used to expand `col op ANY/ALL(<literal>)`
   into datalog branches without a runtime array allocation."
  [ctx arr-expr]
  (cond
    (instance? ArrayConstructor arr-expr)
    (mapv #(translate-expr ctx %)
          (.getExpressions ^ArrayConstructor arr-expr))
    (instance? StringValue arr-expr)
    (let [s (.getNotExcapedValue ^StringValue arr-expr)]
      (if (or (= s "{}") (str/blank? s))
        []
        (let [inner (subs s 1 (dec (count s)))]
          (mapv str/trim (str/split inner #",")))))
    :else nil))

(defn- translate-quantified-cmp
  "Translate `col <op> ANY/ALL(arr)` to where-clauses. Handles both
   literal-array and runtime-array cases. Returns a vector of clauses.
   `op` is the Clojure comparison symbol, `kind` is \"any\" or \"all\"."
  [ctx op left arr-expr kind]
  (let [elements (literal-array-elements ctx arr-expr)
        col (translate-expr ctx left)]
    (if elements
      (cond
        ;; <op> ANY(<literal>) — or-join over per-element comparisons.
        (= kind "any")
        (let [non-null (filterv some? elements)]
          (if (empty? non-null)
            [[(list 'not= col col)]]
            (let [shared-vars (vec (sort-by str (ctx/collect-vars col)))
                  branches (for [v non-null] [(list op col v)])
                  clause (if (seq shared-vars)
                           (concat ['or-join shared-vars] branches)
                           (concat ['or] branches))]
              [clause])))
        ;; <op> ALL(<literal>) — AND of per-element comparisons.
        :else
        (if (empty? elements)
          []
          (mapv (fn [v] [(list op col v)]) elements)))
      ;; Runtime array — dispatch via an in-param predicate function
      ;; that closes over the comparison op.
      (let [arr-val (translate-expr ctx arr-expr)
            col' (if (seq? col) (ctx/materialize-arg! ctx col) col)
            arr' (if (seq? arr-val) (ctx/materialize-arg! ctx arr-val) arr-val)
            fn-param (symbol (str "?pg-q" kind (swap! (:var-counter ctx) inc)))
            cmp-fn (requiring-resolve (symbol "clojure.core" (name op)))
            op-fn (case kind
                    "any" (fn [c a]
                            (if-let [arr (coerce-pg-array a)]
                              (boolean (pg-arr/any-match? arr #(cmp-fn c %)))
                              false))
                    "all" (fn [c a]
                            (if-let [arr (coerce-pg-array a)]
                              (pg-arr/all-match? arr #(cmp-fn c %))
                              true)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj op-fn)
        (swap! (:where-clauses ctx) conj
               [(list fn-param col' arr') result-var])
        [[(list 'identity result-var)]]))))

(defn- column-vtype
  "Return the Datahike `:db/valueType` of the schema attribute that
   `col` resolves to in the current ctx; nil when resolution fails or
   the attr isn't in the schema. Used for PG-style typinput dispatch
   on unknown-string operands of comparisons / IN / BETWEEN."
  [ctx ^Column col]
  (when-let [resolved (try (ctx/resolve-column col
                                               (:table-aliases ctx)
                                               (:default-table ctx)
                                               (:col-overrides ctx)
                                               (:derived-aliases ctx))
                           (catch Throwable _ nil))]
    (let [attr (cond (keyword? resolved) resolved
                     (and (vector? resolved) (= 3 (count resolved))) (nth resolved 2)
                     :else nil)]
      (when attr (get-in (:schema ctx) [attr :db/valueType])))))

(defn- coerce-unknown-literal
  "If `lit` is a `StringValue` and `col` resolves to a Datahike-typed
   schema attribute, return the typinput-coerced value (long, double,
   bigdec, bool, uuid, instant) per PG's unknown-literal resolution.
   Otherwise return nil so the caller falls through to translate-expr.

   Mirrors `parse_coerce.c:coerce_type` line 233 — when an `unknown`
   literal lands as the operand of an operator whose other side has a
   determined type, PG calls that type's typinput function to produce
   a typed Const. Without this, `WHERE c.oid = '16384'` compares long
   to string and matches nothing — real PG resolves the literal via
   `oidin('16384')`."
  [ctx ^Column col lit]
  (when (and (instance? StringValue lit)
             (instance? Column col))
    (when-let [vt (column-vtype ctx col)]
      (let [s (.getNotExcapedValue ^StringValue lit)
            v (coerce/coerce-unknown s vt parse-timestamp-string)]
        (when (not (identical? v s))   ; only signal coercion when it produced a typed value
          v)))))

(defn- coerce-comparison-operands
  "Apply PG-style unknown-literal coercion to a `[left right]` pair of
   AST nodes for a binary comparison. Returns `[left' right']` where
   each side is either the original AST node or a pre-resolved
   typed Clojure value (Long/Double/Boolean/UUID/Date/...). The
   caller's translate-expr branch handles both."
  [ctx left right]
  [(or (coerce-unknown-literal ctx right left) left)
   (or (coerce-unknown-literal ctx left right) right)])

(defn translate-comparison
  "Translate a binary comparison to Datalog predicate clauses.

   Per SQL three-valued logic, `col op V` when either operand is NULL
   yields UNKNOWN, which WHERE treats as FALSE (row filtered). Our
   nullable vars (from ctx/col-var!) may be bound to the `:__null__`
   sentinel when the underlying attribute is absent. We emit an explicit
   `(not= v :__null__)` guard for each nullable operand so rows with
   NULL values are filtered regardless of the operator's own behaviour
   on the sentinel — `=`/`contains?` would already filter them, but
   `not=`/`<`/`>`/`<=`/`>=` would either pass them through (wrong) or
   throw (for numeric comparisons).

   Returns a vector of clause forms; each appearing as its own entry
   in :where is AND-ed implicitly by Datalog.

   Special-cased: `col <op> ANY/ALL(arr)` on the RHS dispatches through
   `translate-quantified-cmp` so we don't try to bind an array against
   a scalar.

   Implicit text-to-int coercion: `<long-col> = '<digits>'` (and the
   reverse) coerces the digit literal to a long. Matches PG's implicit
   `oidin('16384')` resolution that powers psql's `\\d <table>` queries."
  [ctx op left right]
  (if (and (instance? Function right)
           (#{"any" "all"}
            (str/lower-case (.getName ^Function right))))
    (let [^Function fn-expr right
          kind (str/lower-case (.getName fn-expr))
          params (.getParameters fn-expr)
          arr-expr (when params (first params))]
      (translate-quantified-cmp ctx op left arr-expr kind))
    (let [[left right] (coerce-comparison-operands ctx left right)
          ;; Each side is either an AST node (translate-expr-bound) or
          ;; a pre-resolved typed Clojure value from coerce-unknown.
          l (if (instance? net.sf.jsqlparser.expression.Expression left)
              (translate-expr ctx left) left)
          r (if (instance? net.sf.jsqlparser.expression.Expression right)
              (translate-expr ctx right) right)
          l (if (seq? l) (ctx/materialize-arg! ctx l) l)
          r (if (seq? r) (ctx/materialize-arg! ctx r) r)
          guards (ctx/null-guard-clauses ctx [l r])]
      (conj guards [(list op l r)]))))

(defn translate-predicate
  "Translate a JSqlParser WHERE expression to Datalog :where clauses.
   Returns a vector of clause forms."
  [ctx expr]
  (cond
    (instance? AndExpression expr)
    (let [^AndExpression e expr]
      (into (translate-predicate ctx (.getLeftExpression e))
            (translate-predicate ctx (.getRightExpression e))))

    (instance? OrExpression expr)
    (let [^OrExpression e expr
          left-clauses (translate-predicate ctx (.getLeftExpression e))
          right-clauses (translate-predicate ctx (.getRightExpression e))
          ;; The canonical "always false" sentinel produced by EXISTS /
          ;; IN-subquery handlers when the inner evaluates to no rows.
          ;; A branch carrying this sentinel (alone or under an `and`)
          ;; is constant-false — it cannot match any outer row.
          ;; `(and false …)` short-circuits to false, so any AND containing
          ;; the sentinel is also constant-false.
          false-sentinel? (fn false-sentinel? [form]
                            (cond
                              (and (vector? form)
                                   (= 1 (count form))
                                   (= '(not= 1 1) (first form)))
                              true
                              (and (seq? form) (= 'and (first form)))
                              (some false-sentinel? (rest form))
                              :else false))
          ;; Build the per-branch form. translate-predicate returns a
          ;; *vector of clauses* implicitly ANDed; wrap it back into a
          ;; single form for OR composition. Empty vector = "no
          ;; constraint" = always-true; in OR that subsumes the other
          ;; branch (true OR x = true), so handled in the outer cond.
          mk-branch    (fn [cs]
                         (cond
                           (empty? cs)      ::always-true
                           (= 1 (count cs)) (first cs)
                           :else            (concat ['and] cs)))
          all-branches (mapv mk-branch [left-clauses right-clauses])]
      (cond
        ;; Any branch is always-true → OR(true, x) = true → no constraint.
        ;; Returning [] means translate-predicate's caller (the AND in
        ;; the surrounding WHERE) just skips this clause.
        (some #(= ::always-true %) all-branches)
        []

        ;; Drop constant-false branches. OR(false, x) = x; OR(false, false) = false.
        :else
        (let [live-branches (vec (remove false-sentinel? all-branches))]
          (cond
            ;; All branches false → OR is false. Emit one canonical
            ;; false-sentinel as a top-level clause; the surrounding AND
            ;; short-circuits to false, the query returns no rows.
            (empty? live-branches)
            [[(list 'not= 1 1)]]

            ;; Single live branch → unwrap to flat clauses so the outer
            ;; translate-predicate keeps its vec-of-clauses shape.
            (= 1 (count live-branches))
            (let [b (first live-branches)]
              (cond
                (and (seq? b) (= 'and (first b))) (vec (rest b))
                (vector? b)                       [b]
                :else                             [b]))

            ;; Two live branches → emit OR. shared-vars =
            ;; (branch-vars ∩ outer-bound-vars). Datomic / legacy-engine
            ;; semantics: shared-vars are the bridge between branches and
            ;; the outer query (limit-context projects each branch's
            ;; result to these). Branch-locals (e.g. the ?c1 introduced
            ;; inside a correlated EXISTS subquery) must stay out of
            ;; shared-vars or the post-projection `limit-rel`
            ;; mismatches across branches. Empty intersection → use
            ;; plain `or`.
            :else
            (let [branch-vars (apply set/union (map ctx/collect-vars live-branches))
                  outer-vars  (ctx/collect-vars @(:where-clauses ctx))
                  shared-vars (vec (sort-by str (set/intersection branch-vars outer-vars)))]
              [(if (seq shared-vars)
                 (concat ['or-join shared-vars] live-branches)
                 (concat ['or] live-branches))])))))

    (instance? EqualsTo expr)
    (let [^EqualsTo e expr
          left (.getLeftExpression e)
          right (.getRightExpression e)]
      ;; Special case: col = ANY(...) / col = ALL(...)
      ;;  - Literal ARRAY[…] or '{…}' → expand to or-join over literals
      ;;    (no allocation, best-planner hints)
      ;;  - Runtime expr (fn result, column) → bind the array at runtime
      ;;    and call pg-arr/member? (for ANY) or pg-arr/all-match? (ALL)
      (if (and (instance? Function right)
               (#{"any" "all"}
                (str/lower-case (.getName ^Function right))))
        (let [^Function fn-expr right
              kind (str/lower-case (.getName fn-expr))
              params (.getParameters fn-expr)
              arr-expr (when params (first params))
              array-elements (cond
                               (instance? ArrayConstructor arr-expr)
                               (mapv #(translate-expr ctx %) (.getExpressions ^ArrayConstructor arr-expr))
                               (instance? StringValue arr-expr)
                               (let [s (.getNotExcapedValue ^StringValue arr-expr)]
                                 (if (or (= s "{}") (str/blank? s))
                                   []
                                   (let [inner (subs s 1 (dec (count s)))]
                                     (mapv str/trim (str/split inner #",")))))
                               :else nil)]
          (cond
            ;; Literal ANY — or-join expansion (existing fast path).
            (and (= kind "any") array-elements)
            (let [col (translate-expr ctx left)
                  non-null-vals (filterv some? array-elements)
                  shared-vars (vec (sort-by str (ctx/collect-vars col)))]
              (if (empty? non-null-vals)
                [[(list 'not= col col)]]
                (let [in-clause (if (seq shared-vars)
                                  (concat ['or-join shared-vars]
                                          (for [v non-null-vals] [(list '= col v)]))
                                  (concat ['or]
                                          (for [v non-null-vals] [(list '= col v)])))]
                  [in-clause])))

            ;; Literal ALL — AND of per-element equalities.
            (and (= kind "all") array-elements)
            (if (empty? array-elements)
              []  ;; x = ALL(<empty>) is TRUE per PG
              (let [col (translate-expr ctx left)]
                (mapv (fn [v] [(list '= col v)]) array-elements)))

            ;; Runtime array — bind and dispatch through pg-arr.
            :else
            (let [col (translate-expr ctx left)
                  arr-val (translate-expr ctx arr-expr)
                  col (if (seq? col) (ctx/materialize-arg! ctx col) col)
                  arr-val (if (seq? arr-val) (ctx/materialize-arg! ctx arr-val) arr-val)
                  fn-param (symbol (str "?pg-" kind "-pred" (swap! (:var-counter ctx) inc)))
                  op-fn (case kind
                          "any" (fn [c a]
                                  (if-let [arr (coerce-pg-array a)]
                                    (boolean (pg-arr/member? arr c)) false))
                          "all" (fn [c a]
                                  (if-let [arr (coerce-pg-array a)]
                                    (pg-arr/all-match? arr #(= % c)) true)))
                  result-var (ctx/fresh-var! ctx)]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj op-fn)
              (swap! (:where-clauses ctx) conj
                     [(list fn-param col arr-val) result-var])
              [[(list 'identity result-var)]])))
        ;; Special case: column = value can be a ground filter. Skip
        ;; the fast-path when the Column carries an ArrayConstructor
        ;; (e.g. `xs[2]`) — that needs translate-expr's subscript
        ;; rewrite, which the fast-path bypasses by routing straight
        ;; through resolve-column / col-var!.
        (if (and (instance? Column left)
                 (nil? (.getArrayConstructor ^Column left))
                 (or (instance? LongValue right)
                     (instance? DoubleValue right)
                     (instance? StringValue right)))
          (let [resolved (ctx/resolve-column left
                                             (:table-aliases ctx)
                                             (:default-table ctx)
                                             (:col-overrides ctx)
                                             (:derived-aliases ctx))
                ;; PG-style unknown-literal coercion: `<typed-col> = '<lit>'`
                ;; routes the literal through the column's typinput when
                ;; it parses cleanly (oidin/int8in/numericin/boolin/…).
                ;; See coerce/coerce-unknown for the dispatch.
                coerced (coerce-unknown-literal ctx left right)
                val (or coerced (translate-expr ctx right))]
            (if (and (vector? resolved) (= :db-id (first resolved)))
              ;; db_id = N → bind entity var
              (let [evar (ctx/entity-var! ctx (second resolved))]
                [[(list '= evar val)]])
              ;; Regular column = value (including aliased columns)
              (let [v (ctx/col-var! ctx resolved)]
                [[(list '= v val)]])))
          (translate-comparison ctx '= (.getLeftExpression e) (.getRightExpression e)))))

    (instance? NotEqualsTo expr)
    (let [^NotEqualsTo e expr
          left (.getLeftExpression e)
          right (.getRightExpression e)]
      ;; Special case: col <> ALL(ARRAY[...]) → translate as NOT IN (same semantics)
      (if (and (instance? Function right)
               (= "all" (str/lower-case (.getName ^Function right))))
        (let [^Function fn-expr right
              params (.getParameters fn-expr)
              arr-expr (when params (first params))]
          (let [array-elements
                (cond
                  (instance? ArrayConstructor arr-expr)
                  (mapv #(translate-expr ctx %) (.getExpressions ^ArrayConstructor arr-expr))
                  (instance? StringValue arr-expr)
                  (let [s (.getNotExcapedValue ^StringValue arr-expr)]
                    (if (or (= s "{}") (str/blank? s)) []
                        (mapv str/trim (str/split (subs s 1 (dec (count s))) #","))))
                  :else nil)]
            (if array-elements
              (let [col (translate-expr ctx left)
                    elements array-elements
                    non-null-vals (filterv some? elements)
                    shared-vars (vec (sort-by str (ctx/collect-vars col)))
                    guards (ctx/null-guard-clauses ctx [col])]
                (cond
                ;; NOT IN with NULL in the list → always empty (SQL standard)
                  (some nil? elements)
                  [[(list 'not= col col)]]
                ;; NOT IN with empty list → all rows match
                  (empty? non-null-vals)
                  []
                  :else
                  (conj guards (list 'not [(list 'contains? (set non-null-vals) col)]))))
            ;; Fallback to normal comparison if not an array
              (translate-comparison ctx 'not= left right))))
        (translate-comparison ctx 'not= left right)))

    (instance? GreaterThan expr)
    (let [^GreaterThan e expr]
      (translate-comparison ctx '> (.getLeftExpression e) (.getRightExpression e)))

    (instance? GreaterThanEquals expr)
    (let [^GreaterThanEquals e expr]
      (translate-comparison ctx '>= (.getLeftExpression e) (.getRightExpression e)))

    (instance? MinorThan expr)
    (let [^MinorThan e expr]
      (translate-comparison ctx '< (.getLeftExpression e) (.getRightExpression e)))

    (instance? MinorThanEquals expr)
    (let [^MinorThanEquals e expr]
      (translate-comparison ctx '<= (.getLeftExpression e) (.getRightExpression e)))

    (instance? Between expr)
    (let [^Between e expr
          not-between? (.isNot e)
          left-ast (.getLeftExpression e)
          col (translate-expr ctx left-ast)
          ;; PG-style typinput on each bound when LHS is a typed Column
          ;; — `oid BETWEEN '16000' AND '17000'` and similar.
          coerce-bound (fn [bound-ast]
                         (or (when (instance? Column left-ast)
                               (coerce-unknown-literal ctx left-ast bound-ast))
                             (translate-expr ctx bound-ast)))
          lo (coerce-bound (.getBetweenExpressionStart e))
          hi (coerce-bound (.getBetweenExpressionEnd e))
          guards (ctx/null-guard-clauses ctx [col lo hi])]
      (if not-between?
        ;; NOT BETWEEN → val < lo OR val > hi. Guard against NULL col
        ;; (SQL: `col NOT BETWEEN a AND b` when col IS NULL → UNKNOWN → false).
        (into guards
              [(list 'or [(list '< col lo)] [(list '> col hi)])])
        ;; BETWEEN → val >= lo AND val <= hi. Same guard.
        (into guards
              [[(list '>= col lo)]
               [(list '<= col hi)]])))

    (instance? IsNullExpression expr)
    (let [^IsNullExpression e expr
          not-null? (.isNot e)
          inner (.getLeftExpression e)]
      (if (instance? Column inner)
        (let [^Column col inner
              resolved (ctx/resolve-column col
                                           (:table-aliases ctx)
                                           (:default-table ctx)
                                           (:col-overrides ctx)
                                           (:derived-aliases ctx))]
          (cond
            ;; db_id IS NULL doesn't make sense
            (and (vector? resolved) (= :db-id (first resolved)))
            []

            ;; For IS NULL / IS NOT NULL: use get-else to detect missing attributes.
            ;; This is consistent with our NULL synthesis approach.
            :else
            (let [;; Determine attr keyword and entity var
                  [alias-key kw]
                  (cond
                    (and (vector? resolved) (= :aliased (first resolved)))
                    [(nth resolved 1) (nth resolved 2)]
                    :else
                    [(namespace resolved) resolved])
                  evar (ctx/entity-var! ctx alias-key)
                  val-var (ctx/fresh-var! ctx)
                  ;; Ensure the entity var is bound by an anchor pattern.
                  ;; Prefer row-marker if available, else first column.
                  table-name (get (:table-aliases ctx) alias-key alias-key)
                  _ (when (empty? (filter (fn [c]
                                            (and (vector? c) (>= (count c) 2)
                                                 (= evar (first c))
                                                 (keyword? (second c))))
                                          @(:where-clauses ctx)))
                      (let [marker (pgs/row-marker-attr table-name)]
                        (if (get (:schema ctx) marker)
                          (ctx/add-clause! ctx [evar marker true])
                          (let [first-attr (or (when-let [db (:db ctx)]
                                                 (when-let [co (pgs/column-order-from-db db table-name)]
                                                   (keyword table-name (first co))))
                                               kw)]
                            (ctx/add-clause! ctx [evar first-attr (ctx/fresh-var! ctx)])))))]
              ;; Add get-else pattern: [(get-else $ ?e :attr :__null__) ?v]
              (ctx/add-clause! ctx [(list 'get-else '$ evar kw :__null__) val-var])
              (if not-null?
                ;; IS NOT NULL → value is not the sentinel
                [[(list 'not= val-var :__null__)]]
                ;; IS NULL → value is the sentinel
                [[(list '= val-var :__null__)]]))))
        ;; Non-column IS NULL — fallback
        (let [v (translate-expr ctx inner)]
          (if not-null?
            [[(list 'some? v)]]
            [[(list 'nil? v)]]))))

    ;; col IS TRUE / col IS FALSE / col IS NOT TRUE / col IS NOT FALSE
    (instance? IsBooleanExpression expr)
    (let [^IsBooleanExpression e expr
          is-true? (.isTrue e)
          is-not? (.isNot e)
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          target (if is-true? true false)]
      (if is-not?
        ;; IS NOT TRUE → (not= col true) or IS NOT FALSE → (not= col false)
        [[(list 'not= col target)]]
        ;; IS TRUE → (= col true) or IS FALSE → (= col false)
        [[(list '= col target)]]))

    ;; col ~ 'pattern' / col !~ 'pattern' / col ~* 'pattern' / col !~* 'pattern'
    (instance? RegExpMatchOperator expr)
    (let [^RegExpMatchOperator e expr
          op-type (str (.getOperatorType e))
          negate? (or (= op-type "NOT_MATCH_CASESENSITIVE")
                      (= op-type "NOT_MATCH_CASEINSENSITIVE"))
          case-insensitive? (or (= op-type "MATCH_CASEINSENSITIVE")
                                (= op-type "NOT_MATCH_CASEINSENSITIVE"))
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          right (.getRightExpression e)
          pattern (translate-expr ctx right)
          ;; Distinguish a parse-time-known regex literal from a
          ;; parameter placeholder. JdbcParameter / JdbcNamedParameter
          ;; turn into ParamRef records that cannot be `re-pattern`-ed
          ;; until Bind. For literals we compile once; for params we
          ;; defer compilation into the matcher closure.
          literal? (or (instance? StringValue right)
                       (string? pattern))
          ;; Two reasons we go through an in-param matcher rather than
          ;; emitting `[(re-find #"…" col)]` directly:
          ;;  (1) Datahike does not resolve nested fn calls inside a
          ;;      single predicate, so `[(not (re-find …))]` silently
          ;;      treats the inner call as opaque-truthy.
          ;;  (2) Datalog `(not […])` clauses don't compose with `or-join`
          ;;      when sibling branches reference different vars
          ;;      (Metabase's getSchemas idiom).
          matcher
          (if literal?
            (let [pat-str (str pattern)
                  re-str  (if case-insensitive? (str "(?i)" pat-str) pat-str)
                  re-obj  (re-pattern re-str)]
              (if negate?
                (fn [s] (and (some? s) (not (.find (.matcher ^java.util.regex.Pattern re-obj (str s))))))
                (fn [s] (and (some? s) (boolean (.find (.matcher ^java.util.regex.Pattern re-obj (str s))))))))
            ;; Parameter pattern: compile per-row (cheap; pgjdbc only
            ;; sends a regex param across a connection a handful of
            ;; times during sync). Cache the most recent (str pat) so
            ;; subsequent rows reuse the compiled Pattern.
            (let [cache (volatile! [nil nil])]
              (fn [pat-arg s]
                (and (some? s)
                     (some? pat-arg)
                     (not= :__null__ pat-arg)
                     (let [pat-str (str pat-arg)
                           re-str  (if case-insensitive? (str "(?i)" pat-str) pat-str)
                           [last-str ^java.util.regex.Pattern last-pat] @cache
                           p (if (= last-str re-str)
                               last-pat
                               (let [np (try (re-pattern re-str)
                                             (catch Throwable _ nil))]
                                 (vreset! cache [re-str np])
                                 np))
                           hit? (and p (.find (.matcher ^java.util.regex.Pattern p (str s))))]
                       (if negate? (not hit?) (boolean hit?)))))))
          fn-param (symbol (str "?re-match" (swap! (:var-counter ctx) inc)))
          _ (swap! (:in-params ctx) conj fn-param)
          _ (swap! (:in-args ctx) conj matcher)
          ;; Null-guard: SQL says NULL col → UNKNOWN → FALSE. Matcher
          ;; tolerates :__null__ via some? check, but the explicit guard
          ;; keeps the predicate consistent with the LIKE / `=` shapes.
          guards (ctx/null-guard-clauses ctx [col])
          pred-form (if literal?
                      (list fn-param col)
                      (list fn-param pattern col))]
      (conj guards [pred-form]))

    (instance? LikeExpression expr)
    (let [^LikeExpression e expr
          not-like? (.isNot e)
          case-insensitive? (.isCaseInsensitive e)
          col (translate-expr ctx (.getLeftExpression e))
          right-expr (.getRightExpression e)
          pattern (translate-expr ctx right-expr)
          ;; ESCAPE character (default: backslash per PostgreSQL).
          ^Character escape-char (let [esc (.getEscape e)]
                                   (if (and esc (not (str/blank? (str esc))))
                                     (Character/valueOf (char (first (str esc))))
                                     (Character/valueOf \\)))
          ;; Compile the SQL LIKE pattern → Java regex source. Same
          ;; rules whether the pattern is known at parse time or not:
          ;;   %  → .*       _  → .       <esc>X → literal X
          ;;   anything else → Pattern/quote
          like->regex (fn [^String pat-str]
                        (let [sb (StringBuilder. "^")]
                          (loop [i 0]
                            (when (< i (count pat-str))
                              (let [c (.charAt pat-str i)]
                                (if (= c (.charValue escape-char))
                                  (if (< (inc i) (count pat-str))
                                    (do (.append sb (java.util.regex.Pattern/quote
                                                     (str (.charAt pat-str (inc i)))))
                                        (recur (+ i 2)))
                                    (recur (inc i)))
                                  (case c
                                    \% (do (.append sb ".*") (recur (inc i)))
                                    \_ (do (.append sb ".") (recur (inc i)))
                                    (do (.append sb (java.util.regex.Pattern/quote (str c)))
                                        (recur (inc i))))))))
                          (.append sb "$")
                          (str sb)))
          ;; Same gate as RegExpMatchOperator: pre-compile only when the
          ;; right side is an actual literal. With Extended Query, pgjdbc
          ;; rewrites every LIKE-pattern literal to a JdbcParameter so
          ;; the SQL reaches us as `LIKE $1 ESCAPE '\'` — `pattern` is a
          ;; logic-var symbol then, and (str pattern) yields `?p1`,
          ;; which compiled as a regex never matches anything. Defer
          ;; compilation to per-row when the pattern isn't a literal.
          literal? (or (instance? StringValue right-expr)
                       (string? pattern))
          ;; Null-guard: re-find on :__null__ would throw; SQL says
          ;; NULL col → UNKNOWN → FALSE.
          guards (ctx/null-guard-clauses ctx [col])]
      (if literal?
        (let [regex-str (cond-> (like->regex (str pattern))
                          case-insensitive? (->> (str "(?i)")))
              re-obj (re-pattern regex-str)
              pred [(list 're-find re-obj col)]]
          (if not-like?
            (conj guards (list 'not pred))
            (conj guards pred)))
        ;; Parameter pattern → register an in-param matcher fn.
        ;; Cache the last (str pat) so re-Bind/re-Execute on the same
        ;; portal reuses the compiled Pattern.
        (let [cache (volatile! [nil nil])
              matcher (fn [pat-arg s]
                        (and (some? s)
                             (not= :__null__ s)
                             (some? pat-arg)
                             (not= :__null__ pat-arg)
                             (let [pat-str (str pat-arg)
                                   re-str (cond-> (like->regex pat-str)
                                            case-insensitive? (->> (str "(?i)")))
                                   [last-str ^java.util.regex.Pattern last-pat] @cache
                                   p (if (= last-str re-str)
                                       last-pat
                                       (let [np (try (re-pattern re-str)
                                                     (catch Throwable _ nil))]
                                         (vreset! cache [re-str np])
                                         np))]
                               (and p (boolean (.find (.matcher ^java.util.regex.Pattern p
                                                                (str s))))))))
              fn-param (symbol (str "?like" (swap! (:var-counter ctx) inc)))
              result-var (ctx/fresh-var! ctx)
              col' (if (seq? col) (ctx/materialize-arg! ctx col) col)
              pat' (if (seq? pattern) (ctx/materialize-arg! ctx pattern) pattern)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj matcher)
          (swap! (:where-clauses ctx) conj
                 [(list fn-param pat' col') result-var])
          (if not-like?
            (conj guards [(list 'not result-var)])
            (conj guards [(list 'identity result-var)])))))

    (instance? NotExpression expr)
    (let [^NotExpression e expr
          inner-expr (.getExpression e)
          ;; JSqlParser wraps NOT (expr) parenthesized groups as ParenthesedExpressionList.
          ;; Unwrap single-element lists to get the actual expression (e.g., OrExpression).
          inner-expr (if (and (instance? ParenthesedExpressionList inner-expr)
                              (= 1 (count ^ParenthesedExpressionList inner-expr)))
                       (first ^ParenthesedExpressionList inner-expr)
                       inner-expr)]
      ;; Special case: NOT EXISTS → handle as EXISTS with negated logic
      (if (instance? ExistsExpression inner-expr)
        (let [^ExistsExpression exists-expr inner-expr]
          ;; Temporarily set isNot and delegate to EXISTS handler
          (translate-predicate ctx (doto (ExistsExpression.) (.setNot true)
                                         (.setRightExpression (.getRightExpression exists-expr)))))
        (let [inner (translate-predicate ctx inner-expr)]
          [(concat ['not] inner)])))

    (instance? InExpression expr)
    (let [^InExpression e expr
          not-in? (.isNot e)
          left-ast (.getLeftExpression e)
          col (translate-expr ctx left-ast)
          right (.getRightExpression e)
          ;; PG-style typinput: when the LHS is a typed Column, route
          ;; each unknown StringValue in the IN-list through the
          ;; column's typinput. Mirrors `c.oid IN ('16384','16385')`
          ;; from pgjdbc's getColumns probe.
          translate-in-elem (fn [el]
                              (or (when (instance? Column left-ast)
                                    (coerce-unknown-literal ctx left-ast el))
                                  (translate-expr ctx el)))
          vals (cond
                 (instance? ParenthesedExpressionList right)
                 (mapv translate-in-elem ^ParenthesedExpressionList right)

                 (instance? ExpressionList right)
                 (mapv translate-in-elem ^ExpressionList right)

                 ;; Subquery: execute it and extract single-column values
                 (instance? ParenthesedSelect right)
                 (if-let [db (:db ctx)]
                   (let [inner-sql (str right)
                         inner-parsed ((:parse-sql ctx) inner-sql (:schema ctx) db)
                         inner-query (:query inner-parsed)
                         inner-in-args (:in-args inner-parsed)
                         ;; Use enriched-db when subquery has derived tables/CTEs
                         query-db (or (:enriched-db inner-parsed) db)
                         inner-results (if (seq inner-in-args)
                                         (apply d/q
                                                inner-query query-db inner-in-args)
                                         (d/q
                                          inner-query query-db))]
                     ;; Extract single-column values from results
                     (mapv (fn [row]
                             (if (sequential? row) (first row) row))
                           inner-results))
                   (throw (ex-info "subquery requires database context"
                                   {:error :internal-error
                                    :detail "subquery requires database context (use parse-sql with db parameter)"
                                    :expr (str right)})))

                 :else
                 (throw (ex-info "unsupported IN expression form"
                                 {:error :feature-not-supported
                                  :feature (str "IN with right-hand of type "
                                                (.getName ^Class (type right)))
                                  :expr (str right)})))
          non-null-vals (filterv some? vals)
          ;; Detect parameterised values — JdbcParameter substitution
          ;; emits `?pN` symbols. A `(contains? #{?p1} ?col)` clause
          ;; would compare ?col against the literal var, never matching.
          ;; For these, fall back to per-value equality which Datahike
          ;; resolves param vars correctly. Pure-literal lists keep
          ;; using the O(1) hash-set predicate.
          has-param? (some symbol? non-null-vals)
          guards (ctx/null-guard-clauses ctx [col])]
      (cond
        ;; NOT IN with NULL in the list → always empty (SQL standard)
        (and not-in? (some nil? vals))
        [[(list 'not= col col)]]

        ;; NOT IN with empty list → all rows match (everything is NOT IN {})
        (and not-in? (empty? non-null-vals))
        []

        ;; NOT IN with parameterised values — emit per-value not= guards
        ;; (one parameter per row of the list). Datahike conjoins them:
        ;; col matches NOT IN iff every (not= col p_i) holds.
        (and not-in? has-param?)
        (into guards
              (mapv (fn [v] [(list 'not= col v)]) non-null-vals))

        ;; NOT IN literal list — set-based predicate wrapped in `not`.
        ;; Using `(or-join ...)` with many branches explodes Datahike's
        ;; planner (OOM on large lists). A single `(contains? #{...} ?col)`
        ;; predicate is O(1) per row. NULL col must be filtered via
        ;; null-guard (SQL: NULL NOT IN (…) → UNKNOWN → FALSE).
        not-in?
        (let [val-set (set non-null-vals)]
          (conj guards (list 'not [(list 'contains? val-set col)])))

        ;; IN with empty or NULL-only list → nothing matches
        (empty? non-null-vals)
        [[(list 'not= col col)]]

        ;; IN with parameterised values — or-join across per-value
        ;; equality. Each branch binds the param var to col via `=`.
        ;; `[(= ?col ?p1)]` works because Datahike resolves both vars
        ;; from :in / :where bindings; `(contains? #{?p1} ?col)` does
        ;; NOT (the set member is the var symbol, not its value).
        has-param?
        (let [shared-vars (vec (distinct (concat (ctx/collect-vars col)
                                                 (filter symbol? non-null-vals))))
              branches (mapv (fn [v]
                               (list 'and [(list '= col v)]))
                             non-null-vals)]
          (conj guards (apply list 'or-join shared-vars branches)))

        ;; IN — set-based predicate (O(1) per row)
        ;; `(contains? #{...} :__null__)` returns false, so positive IN is already null-safe.
        :else
        (let [val-set (set non-null-vals)]
          [[(list 'contains? val-set col)]])))

    ;; EXISTS / NOT EXISTS subquery
    (instance? ExistsExpression expr)
    (let [^ExistsExpression e expr
          not-exists? (.isNot e)
          sub-select (.getRightExpression e)]
      (if-let [db (:db ctx)]
        ;; Try correlated EXISTS first: translate inner query to Datalog patterns
        ;; and use not-join/or-join for correlation with the outer query.
        ;; Falls back to uncorrelated execution if no db or parsing fails.
        (let [schema (:schema ctx)
              outer-aliases (:table-aliases ctx)]
          (try
            (let [;; Parse inner SELECT with a fresh context that knows about outer aliases
                  inner-ps (cond
                             (instance? ParenthesedSelect sub-select)
                             (.getSelect ^ParenthesedSelect sub-select)
                             (instance? PlainSelect sub-select) sub-select
                             :else nil)]
              (if (and inner-ps (instance? PlainSelect inner-ps))
                ;; Build inner translation context
                (let [^PlainSelect ips inner-ps
                      ;; Extract inner FROM table
                      inner-from (.getFromItem ips)
                      inner-joins (.getJoins ips)
                      inner-where (.getWhere ips)
                      ;; Build table-aliases for inner + outer tables
                      inner-info (when (instance? Table inner-from)
                                   (ctx/extract-table-info ^Table inner-from))
                      inner-name (:name inner-info)
                      inner-alias (or (:alias inner-info) inner-name)
                      ;; Combine outer + inner aliases so inner WHERE can reference both
                      combined-aliases (merge outer-aliases
                                              {inner-name inner-name}
                                              (when inner-alias {inner-alias inner-name}))
                      ;; Add JOIN tables
                      combined-aliases (reduce
                                        (fn [aliases ^Join j]
                                          (let [jt (.getRightItem j)]
                                            (if (instance? Table jt)
                                              (let [{jn :name ja :alias} (ctx/extract-table-info ^Table jt)]
                                                (cond-> (assoc aliases jn jn)
                                                  ja (assoc ja jn)))
                                              aliases)))
                                        combined-aliases
                                        (or inner-joins []))
                      ;; Create inner ctx with combined aliases
                      inner-ctx (ctx/make-ctx schema combined-aliases inner-alias {:db db :parse-sql (:parse-sql ctx)})
                      ;; Copy outer entity vars so correlation references resolve
                      _ (reset! (:entity-vars inner-ctx) @(:entity-vars ctx))
                      _ (reset! (:col->var inner-ctx) @(:col->var ctx))
                      ;; Translate inner FROM patterns
                      _ (when inner-from
                          (let [evar (ctx/entity-var! inner-ctx inner-alias)
                                marker (pgs/row-marker-attr inner-name)]
                            (when (get schema marker)
                              (ctx/add-clause! inner-ctx [evar marker true]))))
                      ;; Translate inner JOIN conditions
                      _ (doseq [^Join j (or inner-joins [])]
                          (let [jt (.getRightItem j)
                                on-expr (when (.getOnExpressions j)
                                          (first (.getOnExpressions j)))]
                            (when (and (instance? Table jt) on-expr)
                              (let [{jn :name ja :alias} (ctx/extract-table-info ^Table jt)
                                    j-alias (or ja jn)
                                    j-evar (ctx/entity-var! inner-ctx j-alias)
                                    j-marker (pgs/row-marker-attr jn)]
                                (when (get schema j-marker)
                                  (ctx/add-clause! inner-ctx [j-evar j-marker true]))
                                ;; Unwrap ParenthesedExpressionList for ON clause
                                ;; JSqlParser wraps ON (expr) as ParenthesedExpressionList
                                (let [actual-expr (if (instance? ParenthesedExpressionList on-expr)
                                                    (first ^ParenthesedExpressionList on-expr)
                                                    on-expr)
                                      preds (translate-predicate inner-ctx actual-expr)]
                                  (swap! (:where-clauses inner-ctx) into preds))))))
                      ;; Translate inner WHERE conditions
                      _ (when inner-where
                          (let [preds (translate-predicate inner-ctx inner-where)]
                            (swap! (:where-clauses inner-ctx) into preds)))
                      ;; Collect inner-only patterns (patterns that reference inner entity vars)
                      inner-evars (set (keep (fn [[k v]]
                                               (when-not (contains? @(:entity-vars ctx) k) v))
                                             @(:entity-vars inner-ctx)))
                      ;; All inner where clauses
                      inner-clauses (vec @(:where-clauses inner-ctx))
                      ;; Filter to only clauses that are NEW (not in outer ctx)
                      outer-clauses-set (set @(:where-clauses ctx))
                      new-clauses (vec (remove outer-clauses-set inner-clauses))
                      ;; === OPTIMIZATION: Value-based correlation for not-join ===
                      ;; Instead of correlating on entity vars (?m_eid), correlate on
                      ;; column values (?m_id). This allows Datahike to use indexed
                      ;; lookups inside not-join instead of full scans.
                      ;;
                      ;; Strategy: find equality predicates [(= ?inner ?outer)] where
                      ;; ?outer comes from an outer entity's attribute binding.
                      ;; Move that binding outside not-join, use the value var as
                      ;; the correlation variable, and rewrite the inner pattern to
                      ;; use the value directly.
                      outer-evars (set (vals @(:entity-vars ctx)))
                      outer-col-vars (set (vals @(:col->var ctx)))
                      ;; Find equality predicates that correlate inner with outer
                      ;; Pattern: [(= ?inner_var ?outer_var)] where ?outer_var is from outer
                      ;; Equality predicates come in two forms:
                      ;; - [(= ?a ?b)] — vector wrapping a list (from wrapped predicates)
                      ;; - (= ?a ?b) — bare list (from where-clauses before wrapping)
                      eq-preds (filter (fn [c]
                                         (or
                                          ;; Bare list form: (= ?a ?b)
                                          (and (seq? c) (= '= (first c)) (= 3 (count c)))
                                          ;; Wrapped form: [(= ?a ?b)]
                                          (and (vector? c) (= 1 (count c))
                                               (seq? (first c))
                                               (= '= (first (first c)))
                                               (= 3 (count (first c))))))
                                       new-clauses)
                      ;; For each equality predicate, check if one var is bound
                      ;; by a data pattern that uses an outer entity var.
                      ;; E.g. [(= ?d_module_id ?m_id)] where ?m_id comes from
                      ;; [?m_eid :mod/id ?m_id] (outer entity ?m_eid)
                      ;;
                      ;; Build a map: var → {:clause … :evar … :attr …}.
                      ;;
                      ;; Two clause shapes bind a var to an entity+attr:
                      ;; - Plain data pattern: `[?e :attr ?v]` (3 elems).
                      ;; - NULL-safe get-else:  `[(get-else $ ?e :attr
                      ;;   :__null__) ?v]` (2 elems: function-call +
                      ;;   result var). Emitted by ctx/col-var! for any
                      ;;   nullable attribute post-B2.
                      ;;
                      ;; Downstream correlation-rewriting needs the
                      ;; evar (to decide if an outer binding drives it)
                      ;; and the original clause (to relocate it into
                      ;; the outer context). Keep both in the source
                      ;; map so the get-else shape is handled uniformly.
                      inner-var-sources
                      (into {}
                            (keep (fn [c]
                                    (cond
                                      (and (vector? c) (= 3 (count c))
                                           (symbol? (first c))
                                           (keyword? (second c))
                                           (symbol? (nth c 2)))
                                      [(nth c 2)
                                       {:clause c :evar (first c) :attr (second c)}]

                                      (and (vector? c) (= 2 (count c))
                                           (seq? (first c))
                                           (= 'get-else (ffirst c))
                                           (symbol? (second c)))
                                      (let [[_ _ evar attr _] (first c)
                                            v (second c)]
                                        (when (and (symbol? evar) (keyword? attr))
                                          [v {:clause c :evar evar :attr attr}]))
                                      :else nil)))
                            new-clauses)
                      correlation-rewrites
                      (keep (fn [eq-clause]
                              (let [[_ v1 v2] (if (seq? eq-clause)
                                                eq-clause
                                                (first eq-clause))
                                    outer-bound? (fn [v]
                                                   (when-let [src (inner-var-sources v)]
                                                     (outer-evars (:evar src))))]
                                (cond
                                  (and (symbol? v2) (outer-bound? v2) (symbol? v1))
                                  {:eq-clause eq-clause :outer-var v2 :inner-var v1
                                   :outer-pattern (:clause (inner-var-sources v2))}
                                  (and (symbol? v1) (outer-bound? v1) (symbol? v2))
                                  {:eq-clause eq-clause :outer-var v1 :inner-var v2
                                   :outer-pattern (:clause (inner-var-sources v1))}
                                  :else nil)))
                            eq-preds)
                      ;; Move outer-bound patterns outside the not-join
                      patterns-to-move (set (keep :outer-pattern correlation-rewrites))
                      ;; Move these patterns to the outer ctx
                      _ (doseq [pat patterns-to-move]
                          (swap! (:where-clauses ctx) conj pat))
                      ;; Correlation vars: use the VALUE vars, not entity vars
                      corr-value-vars (set (keep :outer-var correlation-rewrites))
                      ;; Remove equality predicates that are now handled by correlation
                      eq-clauses-to-remove (set (map :eq-clause correlation-rewrites))
                      ;; Rewrite inner patterns: replace ?inner_var with
                      ;; ?outer_var when the inner pattern (plain data
                      ;; or get-else) binds inner-var via a non-outer
                      ;; entity var. The rename threads the
                      ;; correlation var through every inner clause so
                      ;; not-join's declared shared-vars actually
                      ;; constrain the inner search.
                      var-renames (into {}
                                        (keep (fn [{:keys [inner-var outer-var]}]
                                                (when-let [src (inner-var-sources inner-var)]
                                                  (when-not (outer-evars (:evar src))
                                                    [inner-var outer-var])))
                                              correlation-rewrites))
                      ;; Also unify inner-inner equalities: [(= ?a ?b)] where both
                      ;; come from data patterns → replace ?b with ?a everywhere
                      inner-eq-unifications
                      (keep (fn [eq-clause]
                              (let [[_ v1 v2] (if (seq? eq-clause) eq-clause (first eq-clause))
                                    s1 (inner-var-sources v1)
                                    s2 (inner-var-sources v2)]
                                (when (and (symbol? v1) (symbol? v2)
                                           (not (eq-clauses-to-remove eq-clause))
                                           s1 s2
                                           (not (outer-evars (:evar s1)))
                                           (not (outer-evars (:evar s2))))
                                  ;; Both are inner vars — unify v2 → v1
                                  {:eq-clause eq-clause :keep v1 :replace v2})))
                            eq-preds)
                      ;; Merge inner unifications into var-renames
                      var-renames (merge var-renames
                                         (into {} (map (fn [{:keys [keep replace]}]
                                                         [replace keep])
                                                       inner-eq-unifications)))
                      ;; Also remove these equality clauses
                      all-eq-to-remove (into eq-clauses-to-remove
                                             (map :eq-clause inner-eq-unifications))
                      ;; Build optimized inner clauses. Post-rewrite
                      ;; walks every clause replacing inner vars with
                      ;; their outer correlation partner — covers plain
                      ;; data patterns, get-else function bindings, and
                      ;; nested predicate lists like `(not= ?v
                      ;; :__null__)`. Without the walk, renames leak:
                      ;; a get-else still binds the old inner var and
                      ;; its null-guard keeps referencing the old name,
                      ;; producing two vars where one is expected.
                      rewrite-var  (fn [v] (get var-renames v v))
                      rewrite-form (fn rewrite-form [form]
                                     (cond
                                       (symbol? form) (rewrite-var form)
                                       (map-entry? form) form
                                       (map? form) (into {} (map (fn [[k v]] [k (rewrite-form v)]) form))
                                       (vector? form) (mapv rewrite-form form)
                                       (seq? form) (apply list (map rewrite-form form))
                                       :else form))
                      optimized-clauses (vec (keep (fn [c]
                                                     (cond
                                                       (patterns-to-move c) nil
                                                       (all-eq-to-remove c) nil
                                                       :else (rewrite-form c)))
                                                   new-clauses))
                      ;; Final correlation vars: prefer value vars, fall back to entity vars
                      all-inner-vars (set (mapcat (fn [clause]
                                                    (cond
                                                      (vector? clause) (filter symbol? clause)
                                                      (seq? clause) (filter symbol? (flatten clause))
                                                      :else []))
                                                  optimized-clauses))
                      entity-corr-vars (set/intersection outer-evars all-inner-vars)
                      corr-vars (vec (set/union corr-value-vars entity-corr-vars))
                      inner-in-params @(:in-params inner-ctx)
                      inner-in-args @(:in-args inner-ctx)
                      _ (swap! (:in-params ctx) into inner-in-params)
                      _ (swap! (:in-args ctx) into inner-in-args)]
                  (let [eval-uncorrelated
                        (fn []
                          ;; Inner has no reference to outer vars: just run
                          ;; the inner query standalone and collapse to a
                          ;; boolean WHERE contribution. [] means "no extra
                          ;; constraint"; [[(not= 1 1)]] is an always-false
                          ;; predicate that makes the outer row-set empty.
                          (let [inner-sql (str sub-select)
                                inner-parsed ((:parse-sql ctx) inner-sql schema db)
                                inner-query (:query inner-parsed)
                                inner-in-args (:in-args inner-parsed)
                                query-db (or (:enriched-db inner-parsed) db)
                                inner-results (if (seq inner-in-args)
                                                (apply d/q
                                                       inner-query query-db inner-in-args)
                                                (d/q
                                                 inner-query query-db))
                                has-results? (boolean (seq inner-results))]
                            (if (if not-exists? (not has-results?) has-results?)
                              []
                              [[(list 'not= 1 1)]])))]
                    (cond
                      ;; No clauses produced (e.g. SELECT 1 FROM empty) →
                      ;; treat as uncorrelated and evaluate inner standalone.
                      (empty? optimized-clauses)
                      (eval-uncorrelated)
                      ;; Clauses but no correlation vars — the inner scans
                      ;; don't share any var with the outer query. Merging
                      ;; the scans as WHERE would create an accidental
                      ;; Cartesian product (and trips a planner bug for
                      ;; free-var scans). NOT-JOIN with empty var list is
                      ;; also invalid in Datalog. Fall back to uncorrelated
                      ;; evaluation — the inner is existential and boolean.
                      (empty? corr-vars)
                      (eval-uncorrelated)
                      not-exists?
                      ;; NOT EXISTS → not-join with correlation vars
                      (let [wrapped (mapv (fn [c]
                                            (if (and (seq? c) (not (vector? c)))
                                              [c]
                                              c))
                                          optimized-clauses)]
                        [(apply list 'not-join corr-vars wrapped)])
                      :else
                      ;; EXISTS → use inner patterns as additional where clauses
                      optimized-clauses)))
                ;; Not a PlainSelect — fall back to uncorrelated execution
                (let [inner-sql (str sub-select)
                      inner-parsed ((:parse-sql ctx) inner-sql schema db)
                      inner-query (:query inner-parsed)
                      inner-in-args (:in-args inner-parsed)
                      query-db (or (:enriched-db inner-parsed) db)
                      inner-results (if (seq inner-in-args)
                                      (apply d/q
                                             inner-query query-db inner-in-args)
                                      (d/q
                                       inner-query query-db))
                      has-results? (boolean (seq inner-results))]
                  (if (if not-exists? (not has-results?) has-results?)
                    []
                    [[(list 'not= 1 1)]]))))
            (catch Exception _ex
              ;; Fallback to uncorrelated execution on any translation error
              (let [inner-sql (str sub-select)
                    inner-parsed ((:parse-sql ctx) inner-sql schema db)
                    inner-query (:query inner-parsed)
                    inner-in-args (:in-args inner-parsed)
                    query-db (or (:enriched-db inner-parsed) db)
                    inner-results (if (seq inner-in-args)
                                    (apply d/q
                                           inner-query query-db inner-in-args)
                                    (d/q
                                     inner-query query-db))
                    has-results? (boolean (seq inner-results))]
                (if (if not-exists? (not has-results?) has-results?)
                  []
                  [[(list 'not= 1 1)]])))))
        (throw (ex-info "EXISTS subquery requires database context"
                        {:error :internal-error
                         :detail "EXISTS subquery requires database context"
                         :expr (str expr)}))))

    ;; Parenthesized predicate
    (instance? Parenthesis expr)
    (translate-predicate ctx (.getExpression ^Parenthesis expr))

    ;; ParenthesedExpressionList — JSqlParser sometimes uses this instead of Parenthesis
    ;; for parenthesized expressions like (a = 1 OR b = 2). Unwrap single-element lists.
    (instance? ParenthesedExpressionList expr)
    (let [^ParenthesedExpressionList pel expr]
      (if (= 1 (count pel))
        (translate-predicate ctx (first pel))
        ;; Multi-element: shouldn't appear in WHERE, but handle gracefully
        (throw (ex-info "multi-element parens in WHERE"
                        {:error :feature-not-supported
                         :feature "multi-element ParenthesedExpressionList in WHERE"
                         :expr (str expr)}))))

    ;; jsonb field access in WHERE: name->>'key' = 'value'
    ;; JSqlParser folds the comparison into the JsonExpression's ident list
    (instance? JsonExpression expr)
    (let [^JsonExpression je expr
          idents (vec (.getIdents je))
          operators (vec (.getOperators je))
          last-ident (last idents)]
      (if (or (instance? EqualsTo last-ident)
              (instance? NotEqualsTo last-ident)
              (instance? GreaterThan last-ident)
              (instance? GreaterThanEquals last-ident)
              (instance? MinorThan last-ident)
              (instance? MinorThanEquals last-ident)
              (instance? LikeExpression last-ident)
              (instance? IsNullExpression last-ident))
        ;; The comparison is folded into the json chain.
        ;; Decompose: the left side of the comparison is the json key,
        ;; the right side is the comparison value.
        (let [comp-expr last-ident
              ;; Build a synthetic JsonExpression for the left side (without the comparison)
              ;; The json key is the left expression of the comparison
              json-key (cond
                         (instance? EqualsTo comp-expr) (.getLeftExpression ^EqualsTo comp-expr)
                         (instance? NotEqualsTo comp-expr) (.getLeftExpression ^NotEqualsTo comp-expr)
                         (instance? GreaterThan comp-expr) (.getLeftExpression ^GreaterThan comp-expr)
                         (instance? GreaterThanEquals comp-expr) (.getLeftExpression ^GreaterThanEquals comp-expr)
                         (instance? MinorThan comp-expr) (.getLeftExpression ^MinorThan comp-expr)
                         (instance? MinorThanEquals comp-expr) (.getLeftExpression ^MinorThanEquals comp-expr)
                         (instance? LikeExpression comp-expr) (.getLeftExpression ^LikeExpression comp-expr)
                         (instance? IsNullExpression comp-expr) (.getLeftExpression ^IsNullExpression comp-expr))
              ;; Extract the key string
              key-str (cond
                        (instance? StringValue json-key) (.getNotExcapedValue ^StringValue json-key)
                        (instance? LongValue json-key) (.getValue ^LongValue json-key)
                        :else (str json-key))
              ;; Build the json access: translate base through all operators
              base-val (translate-expr ctx (.getExpression je))
              ;; Apply all json operators (there might be a chain before the comparison)
              json-val (let [chain (map vector
                                        (if (> (count idents) 1)
                                          (conj (subvec idents 0 (dec (count idents))) json-key)
                                          [json-key])
                                        operators)]
                         (reduce
                          (fn [current [_ident op-str]]
                            (let [op-fn (if (= op-str "->>") jb/jsonb-get-text jb/jsonb-get)
                                  param (symbol (str "?json-w" (swap! (:var-counter ctx) inc)))
                                  result (ctx/fresh-var! ctx)]
                              (swap! (:in-params ctx) conj param)
                              (swap! (:in-args ctx) conj op-fn)
                              (swap! (:where-clauses ctx) conj [(list param current key-str) result])
                              result))
                          base-val
                          chain))
              ;; Now translate the comparison with the json-val as the left side
              comp-right (cond
                           (instance? EqualsTo comp-expr) (.getRightExpression ^EqualsTo comp-expr)
                           (instance? NotEqualsTo comp-expr) (.getRightExpression ^NotEqualsTo comp-expr)
                           (instance? GreaterThan comp-expr) (.getRightExpression ^GreaterThan comp-expr)
                           (instance? GreaterThanEquals comp-expr) (.getRightExpression ^GreaterThanEquals comp-expr)
                           (instance? MinorThan comp-expr) (.getRightExpression ^MinorThan comp-expr)
                           (instance? MinorThanEquals comp-expr) (.getRightExpression ^MinorThanEquals comp-expr)
                           (instance? LikeExpression comp-expr) (.getRightExpression ^LikeExpression comp-expr)
                           :else nil)
              right-val (when comp-right (translate-expr ctx comp-right))
              right-val (when right-val
                          (if (seq? right-val) (ctx/materialize-arg! ctx right-val) right-val))]
          (cond
            (instance? EqualsTo comp-expr)
            [[(list '= json-val right-val)]]
            (instance? NotEqualsTo comp-expr)
            [[(list 'not= json-val right-val)]]
            (instance? GreaterThan comp-expr)
            [[(list '> json-val right-val)]]
            (instance? GreaterThanEquals comp-expr)
            [[(list '>= json-val right-val)]]
            (instance? MinorThan comp-expr)
            [[(list '< json-val right-val)]]
            (instance? MinorThanEquals comp-expr)
            [[(list '<= json-val right-val)]]
            (instance? LikeExpression comp-expr)
            ;; Build LIKE regex using the extracted json-val as subject
            (let [^LikeExpression le comp-expr
                  not-like? (.isNot le)
                  case-insensitive? (.isCaseInsensitive le)
                  pattern (translate-expr ctx (.getRightExpression le))
                  ^Character escape-char (let [esc (.getEscape le)]
                                           (if (and esc (not (str/blank? (str esc))))
                                             (Character/valueOf (char (first (str esc))))
                                             (Character/valueOf \\)))
                  pat-str (str pattern)
                  regex-sb (StringBuilder. "^")
                  _ (loop [i 0]
                      (when (< i (count pat-str))
                        (let [c (.charAt ^String pat-str i)]
                          (if (= c (.charValue escape-char))
                            (if (< (inc i) (count pat-str))
                              (let [next-c (.charAt ^String pat-str (inc i))]
                                (.append regex-sb (java.util.regex.Pattern/quote (str next-c)))
                                (recur (+ i 2)))
                              (recur (inc i)))
                            (case c
                              \% (do (.append regex-sb ".*") (recur (inc i)))
                              \_ (do (.append regex-sb ".") (recur (inc i)))
                              (do (.append regex-sb (java.util.regex.Pattern/quote (str c)))
                                  (recur (inc i))))))))
                  _ (.append regex-sb "$")
                  regex-str (str regex-sb)
                  regex-str (if case-insensitive? (str "(?i)" regex-str) regex-str)
                  pred (list 're-find (re-pattern regex-str) json-val)]
              (if not-like?
                [[(list 'not [pred])]]
                [[pred]]))
            (instance? IsNullExpression comp-expr)
            (if (.isNot ^IsNullExpression comp-expr)
              [[(list 'some? json-val)]]
              [[(list 'nil? json-val)]])
            :else
            [[(list '= json-val right-val)]]))
        ;; Not a comparison — just translate as expression (shouldn't reach predicate)
        (let [v (translate-expr ctx expr)]
          [[(list 'identity v)]])))

    ;; jsonb operators: @>, <@, ?, ?|, ?&
    (instance? JsonOperator expr)
    (let [^JsonOperator jo expr
          op-str (.getStringExpression jo)
          left   (translate-expr ctx (.getLeftExpression jo))
          right  (translate-expr ctx (.getRightExpression jo))
          left   (if (seq? left)  (ctx/materialize-arg! ctx left)  left)
          right  (if (seq? right) (ctx/materialize-arg! ctx right) right)
          op-fn  (case op-str
                   "@>"  jb/jsonb-contains?
                   "<@"  jb/jsonb-contained?
                   "?"   jb/jsonb-exists?
                   "?|"  jb/jsonb-exists-any?
                   "?&"  jb/jsonb-exists-all?
                   nil)]
      (when op-fn
        (let [param      (symbol (str "?json-pred" (swap! (:var-counter ctx) inc)))
              result-var (ctx/fresh-var! ctx)]
          (swap! (:in-params ctx) conj param)
          (swap! (:in-args ctx) conj op-fn)
          (swap! (:where-clauses ctx) conj [(list param left right) result-var])
          [[(list 'identity result-var)]])))

    ;; Bare column as boolean predicate: WHERE col_name means WHERE col_name = TRUE
    (instance? Column expr)
    (let [resolved (ctx/resolve-column ^Column expr
                                       (:table-aliases ctx)
                                       (:default-table ctx)
                                       (:col-overrides ctx)
                                       (:derived-aliases ctx))
          col-var (ctx/col-var! ctx resolved)]
      [[(list '= col-var true)]])

    ;; Boolean literals — `WHERE true` adds no constraint, `WHERE false`
    ;; emits the canonical false-sentinel so the surrounding AND
    ;; short-circuits to no rows. psql's `\dC` (list casts) emits
    ;; `WHERE ((true AND fn1(...)) OR (true AND fn2(...)))` with
    ;; literal trues; this branch + the existing OrExpression false-
    ;; sentinel handling collapse it to the live-fn predicates.
    (instance? BooleanValue expr)
    (if (.getValue ^BooleanValue expr)
      []                              ; WHERE true → no constraint
      [[(list 'not= 1 1)]])           ; WHERE false → constant-false sentinel

    ;; Scalar (boolean) subquery in WHERE. psql's `\dT` emits
    ;;   WHERE (t.typrelid = 0 OR
    ;;          (SELECT c.relkind = 'c' FROM pg_class c WHERE c.oid = t.typrelid))
    ;; Try to evaluate the inner SELECT once at translate time; treat
    ;; the result as boolean. Correlated subqueries fail to translate
    ;; against the outer-bare ctx → fall back to constant-false (no
    ;; match) so the surrounding OR/AND short-circuits correctly. The
    ;; catalog tables driving these psql probes are empty in our
    ;; impl, so the correct PG result IS no match.
    (instance? ParenthesedSelect expr)
    (let [v (translate-expr ctx expr)]
      (cond
        (true? v)  []                       ; always-true → no constraint
        (false? v) [[(list 'not= 1 1)]]     ; always-false → drop branch
        (= :__null__ v) [[(list 'not= 1 1)]] ; NULL in bool position → false (PG 3VL)
        :else (let [v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
                [[(list '= v true)]])))

    ;; Boolean-valued expressions used directly as predicates:
    ;; `WHERE <bool-fn>(...)`, `WHERE CASE WHEN ... END`, etc. PG
    ;; accepts any boolean expression. Route through translate-expr
    ;; which binds the result to a fresh var, then assert `= true`.
    (or (instance? Function expr)
        (instance? CaseExpression expr)
        (instance? Parenthesis expr))
    (let [v (translate-expr ctx expr)
          v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
      [[(list '= v true)]])

    :else
    (throw (ex-info "unsupported WHERE expression"
                    {:error :feature-not-supported
                     :feature (str "WHERE expression of type " (.getName ^Class (type expr)))
                     :expr (str expr)}))))
