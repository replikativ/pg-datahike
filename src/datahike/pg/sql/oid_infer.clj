(ns datahike.pg.sql.oid-infer
  "Parse-time type inference for SELECT-item expressions.

   Mirrors PostgreSQL's `exprType` (src/backend/nodes/nodeFuncs.c): each
   node in the expression tree has a declared result OID, computed by
   walking the AST during parse/analyze. PG looks up operator and
   function signatures in `pg_operator` / `pg_proc`; we hardcode the
   equivalent rules for the set of SQL we handle.

   Consumed by `translate-select` to populate `:select-item-oids` on the
   parsed map, which `describeResult` reads to emit correct
   RowDescription OIDs via the Extended Query protocol — before Execute
   runs, so value-based inference isn't available.

   Return values:
     - A positive integer OID when the type is determined.
     - `nil` when we can't determine it (unknown function, unresolvable
       column, subquery, etc.) — callers fall back to TEXT (OID 25),
       matching the pre-existing behavior."
  (:require [clojure.string :as str]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.schema Column]
           [net.sf.jsqlparser.expression
            ArrayConstructor ArrayExpression BinaryExpression BooleanValue
            CaseExpression CastExpression DoubleValue Expression ExtractExpression
            Function JdbcNamedParameter JdbcParameter LongValue NotExpression
            NullValue Parenthesis SignedExpression StringValue TimeKeyExpression
            TimestampValue TimeValue DateValue WhenClause]
           [net.sf.jsqlparser.expression.operators.arithmetic
            Addition Concat Division Modulo Multiplication Subtraction]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression]
           [net.sf.jsqlparser.expression.operators.relational
            Between EqualsTo ExistsExpression GreaterThan GreaterThanEquals
            InExpression IsBooleanExpression IsNullExpression LikeExpression
            MinorThan MinorThanEquals NotEqualsTo RegExpMatchOperator]))

;; ---------------------------------------------------------------------------
;; Function return-type registry — keyed by lowercased SQL name.
;;
;; Values: either an OID integer (constant return type) or a keyword
;; signalling an arity-dependent rule resolved in `function-oid` below.
;;
;; Add an entry here to surface a function's type through Extended Query's
;; Describe. Without an entry we return nil → caller falls back to TEXT,
;; which matches the pre-existing "unknown = TEXT" behaviour.
;; ---------------------------------------------------------------------------

(def sql-fn->return-oid
  "Scalar function → return OID. Entries either a fixed OID or a keyword
   dispatched in `function-oid` for arg-dependent return types."
  {;; String functions → TEXT
   "upper"         types/oid-text
   "lower"         types/oid-text
   "concat"        types/oid-text
   "concat_ws"     types/oid-text
   "trim"          types/oid-text
   "ltrim"         types/oid-text
   "rtrim"         types/oid-text
   "substring"     types/oid-text
   "substr"        types/oid-text
   "replace"       types/oid-text
   "lpad"          types/oid-text
   "rpad"          types/oid-text
   "left"          types/oid-text
   "right"         types/oid-text
   "reverse"       types/oid-text
   "repeat"        types/oid-text
   "split_part"    types/oid-text
   "initcap"       types/oid-text
   "to_char"       types/oid-text
   "md5"           types/oid-text
   "quote_ident"   types/oid-text
   "quote_literal" types/oid-text
   "format"        types/oid-text
   ;; Length functions → INT4
   "length"        types/oid-int4
   "char_length"   types/oid-int4
   "character_length" types/oid-int4
   "octet_length"  types/oid-int4
   "bit_length"    types/oid-int4
   "position"      types/oid-int4
   "strpos"        types/oid-int4
   "ascii"         types/oid-int4
   ;; Math — return matches arg
   "abs"           :arg-type
   "ceil"          :arg-type
   "ceiling"       :arg-type
   "floor"         :arg-type
   "round"         :arg-type
   "sign"          :arg-type
   "mod"           :arg-type
   ;; Math — always float
   "sqrt"          types/oid-float8
   "exp"           types/oid-float8
   "ln"            types/oid-float8
   "log"           types/oid-float8
   "log10"         types/oid-float8
   "power"         types/oid-float8
   "pow"           types/oid-float8
   "sin"           types/oid-float8
   "cos"           types/oid-float8
   "tan"           types/oid-float8
   "asin"          types/oid-float8
   "acos"          types/oid-float8
   "atan"          types/oid-float8
   "atan2"         types/oid-float8
   "pi"            types/oid-float8
   "random"        types/oid-float8
   ;; Null handling — first arg type wins
   "coalesce"      :arg-type
   "nullif"        :arg-type
   "greatest"      :arg-type
   "least"         :arg-type
   ;; Date/time — constants + truncation
   "now"           types/oid-timestamptz
   "current_timestamp" types/oid-timestamptz
   "transaction_timestamp" types/oid-timestamptz
   "statement_timestamp"  types/oid-timestamptz
   "localtimestamp" types/oid-timestamp
   "current_date"  types/oid-date
   "current_time"  types/oid-time
   "localtime"     types/oid-time
   "date_trunc"    types/oid-timestamptz
   "date_part"     types/oid-float8
   "extract"       types/oid-float8
   "age"           types/oid-interval
   ;; Type checks / introspection
   "pg_typeof"     types/oid-text
   "version"       types/oid-text
   "format_type"   types/oid-text
   "current_setting" types/oid-text
   "current_database" types/oid-name
   "current_schema"   types/oid-name
   "current_user"  types/oid-name
   "session_user"  types/oid-name
   "user"          types/oid-name
   "system_user"   types/oid-name
   ;; pg_get_*def synthesizers — return SQL text
   "pg_get_indexdef"      types/oid-text
   "pg_get_constraintdef" types/oid-text
   "pg_get_userbyid"      types/oid-name
   "obj_description"      types/oid-text
   "col_description"      types/oid-text
   "shobj_description"    types/oid-text
   ;; Array-returning functions — result OID is T[] per PG
   "current_schemas"       types/oid-name-array
   "string_to_array"       types/oid-text-array
   "regexp_split_to_array" types/oid-text-array
   ;; Array meta — all return INT4 or NULL
   "array_length"  types/oid-int4
   "array_upper"   types/oid-int4
   "array_lower"   types/oid-int4
   "cardinality"   types/oid-int4
   "array_to_string" types/oid-text
   ;; Array → array returning (passthrough element type)
   "array_append"  :arg-type
   "array_prepend" :arg-type
   "array_cat"     :arg-type
   "array_position" types/oid-int4
   "array_remove"  :arg-type
   "array_replace" :arg-type
   "array_fill"    :arg-type})

(def sql-aggregate->return-oid
  "Aggregate function → return-OID rule. Three rule shapes:

   - integer (an OID): always returns this OID, regardless of input.
     Used for COUNT (always INT8) and the variance/correlation family
     (always FLOAT8).

   - `:arg-type`: returns the OID of the first argument. Used for
     MIN/MAX where the result-type tracks the input.

   - map `{<input-oid> <output-oid> … :default <fallback>}`:
     resolved by looking up the first argument's OID. Used for SUM/AVG
     to match PG's `pg_proc.dat` per-input-type return rules:

       AVG(int2|int4|int8|numeric) → numeric   (PG)
       AVG(float4|float8)          → float8
       SUM(int2|int4)              → int8
       SUM(int8|numeric)           → numeric  (overflow-safe)
       SUM(float4)                 → float4
       SUM(float8)                 → float8

     PG promotes integer SUMs to int8/numeric to prevent overflow,
     and AVG(int) to numeric to preserve fractional precision. Without
     these per-input rules, AVG(total_cents) renders as a truncated
     INT8 in Metabase even though the runtime returns a Double, and
     SUM(int8) silently overflows when the sum exceeds Long/MAX_VALUE."
  {"count"          types/oid-int8
   "count_distinct" types/oid-int8
   "sum"            {types/oid-int2    types/oid-int8
                     types/oid-int4    types/oid-int8
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float4
                     types/oid-float8  types/oid-float8
                     :default          :arg-type}
   "avg"            {types/oid-int2    types/oid-numeric
                     types/oid-int4    types/oid-numeric
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float8
                     types/oid-float8  types/oid-float8
                     :default          types/oid-float8}
   "min"            :arg-type
   "max"            :arg-type
   "stddev"         types/oid-float8
   "stddev_samp"    types/oid-float8
   "stddev_pop"     types/oid-float8
   "variance"       types/oid-float8
   "var_samp"       types/oid-float8
   "var_pop"        types/oid-float8
   "median"         :arg-type
   "corr"           types/oid-float8
   ;; Ordered-set aggregates (WITHIN GROUP). PG: percentile_cont
   ;; returns FLOAT8 for numeric/float input, INTERVAL for interval;
   ;; we only support numeric here so FLOAT8 covers it. percentile_disc
   ;; returns the input type. mode also returns the input type.
   "percentile_cont" types/oid-float8
   "percentile_disc" :arg-type
   "mode"            :arg-type})

;; ---------------------------------------------------------------------------
;; Inference
;; ---------------------------------------------------------------------------

(declare expr-oid)

(defn- unquote-ident
  "Strip surrounding single/double quotes from a SQL identifier."
  [s]
  (when s
    (let [s (str s)]
      (if (and (>= (count s) 2)
               (or (and (str/starts-with? s "\"") (str/ends-with? s "\""))
                   (and (str/starts-with? s "`")  (str/ends-with? s "`"))))
        (subs s 1 (dec (count s)))
        s))))

(defn- column-oid
  "Resolve a Column reference to an OID via the live schema. Returns nil
   if we can't find the attribute (derived tables, catalog views, etc.);
   caller falls back to TEXT.

   `table-aliases` maps SQL aliases → real table names; we synthesise
   the attribute keyword as `:<table>/<col>` and probe `db`'s schema."
  [^Column col {:keys [db schema table-aliases default-table hints]}]
  (when schema
    (let [col-name   (unquote-ident (.getColumnName col))
          col-table  (when-let [t (.getTable col)]
                       (or (unquote-ident (.getName t))
                           (unquote-ident (.getAlias t))))
          table-real (or (get table-aliases col-table col-table)
                         default-table)
          ;; Schema hints may rename the SQL column to a different
          ;; Datahike attribute local-name.
          hint-attr (when (and hints col-table)
                      (some (fn [[attr h]]
                              (when (and (= (:column h) col-name)
                                         (= (namespace attr) table-real))
                                attr))
                            hints))
          attr (or hint-attr
                   (when table-real
                     (keyword table-real col-name)))
          props (when attr (get schema attr))
          valuetype (:db/valueType props)]
      (when valuetype
        (pgs/oid-for-valuetype valuetype)))))

(defn- promoted-numeric
  "Numeric promotion for binary arithmetic, matching PG's simplified
   implicit-cast rules: any FLOAT makes the result FLOAT8; otherwise
   INT8. Returns nil if we can't type either side (let caller fall back)."
  [l-oid r-oid]
  (cond
    (or (= l-oid types/oid-float8) (= r-oid types/oid-float8)
        (= l-oid types/oid-float4) (= r-oid types/oid-float4)
        (= l-oid types/oid-numeric) (= r-oid types/oid-numeric))
    types/oid-float8
    (or (= l-oid types/oid-int8) (= r-oid types/oid-int8)
        (= l-oid types/oid-int4) (= r-oid types/oid-int4)
        (= l-oid types/oid-int2) (= r-oid types/oid-int2))
    types/oid-int8
    (and l-oid r-oid) types/oid-int8
    :else nil))

(defn- binary-arith-oid [^BinaryExpression e env]
  (promoted-numeric (expr-oid (.getLeftExpression e) env)
                    (expr-oid (.getRightExpression e) env)))

(defn- function-oid
  "Resolve a scalar or aggregate function reference. `:arg-type`
   sentinel in the registry means 'propagate the first argument's type'.
   Strips a leading `pg_catalog.` qualifier so qualified ORM-emitted
   function calls hit the same rules as unqualified ones."
  [^Function f env]
  (let [raw-name (str/lower-case (.getName f))
        fname (if (str/starts-with? raw-name "pg_catalog.")
                (subs raw-name (count "pg_catalog."))
                raw-name)
        ;; Extract arg exprs from JSqlParser's parameter list. May be null
        ;; for zero-arg fns like NOW() / PI() / RANDOM().
        args (try
               (when-let [pl (.getParameters f)]
                 (vec (.getExpressions pl)))
               (catch Exception _ nil))
        first-arg (first args)
        rule (or (get sql-aggregate->return-oid fname)
                 (get sql-fn->return-oid fname))]
    (cond
      (integer? rule) rule
      (= rule :arg-type) (cond
                           ;; COUNT(*) has no arg; handled above via :int8 but
                           ;; defensive: fall back if we see it.
                           (nil? first-arg) types/oid-int8
                           :else (expr-oid first-arg env))
      ;; Per-input-type map (SUM/AVG): look up by first-arg's OID,
      ;; fall back to :default. The :default may itself be :arg-type
      ;; (used by SUM for unknown inputs that aren't in the rule map).
      (map? rule) (let [arg-oid (when first-arg (expr-oid first-arg env))
                        result (or (get rule arg-oid)
                                   (get rule :default))]
                    (cond
                      (integer? result) result
                      (= result :arg-type) arg-oid
                      :else nil))
      :else nil)))

(defn- cast-oid
  "Map a SQL CAST target type-name to an OID. Uses `types/cast-category`
   so the set of recognised target types stays in one place."
  [^CastExpression c]
  (let [type-str (some-> (.getColDataType c) .getDataType str str/lower-case)]
    (case (types/cast-category type-str)
      :integer   types/oid-int8
      :float     types/oid-float8
      :text      types/oid-text
      :boolean   types/oid-bool
      :date      types/oid-date
      :time      types/oid-time
      :timestamp (cond
                   (re-find #"with time zone|timestamptz" type-str)
                   types/oid-timestamptz
                   :else types/oid-timestamp)
      :uuid      types/oid-uuid
      :bytes     types/oid-bytea
      :bit       types/oid-text
      nil)))

(defn- case-oid
  "CASE expression returns the type of its first non-nil branch. PG
   computes the least-common-supertype across branches; we use the first
   typed branch as a good-enough approximation (binary promotion is too
   aggressive — we'd promote INT + TEXT to TEXT, which may not be what
   the user intended)."
  [^CaseExpression e env]
  (let [branches (concat
                  (mapv #(.getThenExpression ^WhenClause %) (.getWhenClauses e))
                  (when-let [el (.getElseExpression e)] [el]))]
    (some #(expr-oid % env) branches)))

(defn- boolean-literal-column?
  "JSqlParser versions older than 5.x sometimes parse bare `TRUE`/`FALSE`
   as a `Column` with name `\"true\"` / `\"false\"` rather than a
   `BooleanValue`. Catch both spellings."
  [^Column col]
  (let [n (some-> (.getColumnName col) str str/lower-case)]
    (and (nil? (.getTable col))
         (#{"true" "false"} n))))

(defn expr-oid
  "Return the inferred PG OID for a JSqlParser expression, or nil if we
   can't determine it.

   `env` carries the live db and schema:
     {:db <datahike db>
      :schema <schema map>
      :table-aliases {alias → real-table-name}
      :default-table <string or nil>
      :hints <schema-hint map or nil>}"
  [^Expression expr env]
  (when expr
    (cond
      ;; --- Literals -----------------------------------------------------
      (instance? LongValue expr)      types/oid-int8
      (instance? DoubleValue expr)    types/oid-float8
      (instance? StringValue expr)    types/oid-text
      (instance? NullValue expr)      types/oid-text
      (instance? BooleanValue expr)   types/oid-bool
      (instance? DateValue expr)      types/oid-date
      (instance? TimeValue expr)      types/oid-time
      (instance? TimestampValue expr) types/oid-timestamp

      ;; TRUE/FALSE parsed as Column on older JSqlParser
      (and (instance? Column expr)
           (boolean-literal-column? expr))
      types/oid-bool

      ;; --- Column reference ---------------------------------------------
      (instance? Column expr)
      (column-oid expr env)

      ;; --- Parenthesis / sign wrappers ----------------------------------
      (instance? Parenthesis expr)
      (expr-oid (.getExpression ^Parenthesis expr) env)

      ;; JSqlParser represents `(e)` as a ParenthesedExpressionList of one
      ;; item when it has no explicit Parenthesis context — e.g. the object
      ;; expression of an ArrayExpression. Peek through single-item lists.
      (instance? net.sf.jsqlparser.expression.operators.relational.ParenthesedExpressionList expr)
      (let [^net.sf.jsqlparser.expression.operators.relational.ParenthesedExpressionList pel expr]
        (when (= 1 (.size pel))
          (expr-oid (.get pel 0) env)))

      (instance? SignedExpression expr)
      (expr-oid (.getExpression ^SignedExpression expr) env)

      ;; --- Boolean operators → BOOL -------------------------------------
      (instance? NotExpression expr)     types/oid-bool
      (instance? AndExpression expr)     types/oid-bool
      (instance? OrExpression expr)      types/oid-bool
      (instance? IsNullExpression expr)  types/oid-bool
      (instance? IsBooleanExpression expr) types/oid-bool
      (instance? InExpression expr)      types/oid-bool
      (instance? Between expr)           types/oid-bool
      (instance? LikeExpression expr)    types/oid-bool
      (instance? ExistsExpression expr)  types/oid-bool
      (instance? RegExpMatchOperator expr) types/oid-bool
      (instance? EqualsTo expr)          types/oid-bool
      (instance? NotEqualsTo expr)       types/oid-bool
      (instance? GreaterThan expr)       types/oid-bool
      (instance? GreaterThanEquals expr) types/oid-bool
      (instance? MinorThan expr)         types/oid-bool
      (instance? MinorThanEquals expr)   types/oid-bool

      ;; --- Arithmetic (numeric promotion) -------------------------------
      (instance? Addition expr)       (binary-arith-oid expr env)
      (instance? Subtraction expr)    (binary-arith-oid expr env)
      (instance? Multiplication expr) (binary-arith-oid expr env)
      (instance? Division expr)       types/oid-float8  ; PG: integer div is exact
      (instance? Modulo expr)         (binary-arith-oid expr env)

      ;; --- String concat (||) -------------------------------------------
      (instance? Concat expr) types/oid-text

      ;; --- Array constructor / subscript --------------------------------
      ;; ARRAY[…] → T[] where T is the LUB of element OIDs; fall back
      ;; to text[] for empty or mixed.
      (instance? ArrayConstructor expr)
      (let [elem-oids (->> (.getExpressions ^ArrayConstructor expr)
                           (keep #(expr-oid % env))
                           vec)
            first-oid (or (first elem-oids) types/oid-text)]
        (get types/element-oid->array-oid first-oid types/oid-text-array))

      ;; arr[N] / arr[lo:hi] → element OID (for single subscript)
      ;; or the container's array OID (for slice).
      (instance? ArrayExpression expr)
      (let [^ArrayExpression ae expr
            container-oid (expr-oid (.getObjExpression ae) env)
            slice? (nil? (.getIndexExpression ae))]
        (cond
          ;; Slice preserves the array type (returns a PgArray).
          slice? container-oid
          ;; Single subscript returns the element type.
          (nil? container-oid) nil
          :else (get types/array-oid->element-oid container-oid types/oid-text)))

      ;; --- CAST ---------------------------------------------------------
      (instance? CastExpression expr) (cast-oid expr)

      ;; --- CASE ---------------------------------------------------------
      (instance? CaseExpression expr) (case-oid expr env)

      ;; --- Function / aggregate ----------------------------------------
      (instance? Function expr) (function-oid expr env)

      ;; --- AnalyticExpression — window fns (OVER) and ordered-set
      ;; aggregates (WITHIN GROUP). Aggregate name lives on getName();
      ;; the value-bearing argument is either getExpression() (window
      ;; / FILTER aggregates) or the first ORDER BY element (WITHIN
      ;; GROUP ordered-set aggregates like PERCENTILE_DISC, MODE).
      (instance? net.sf.jsqlparser.expression.AnalyticExpression expr)
      (let [^net.sf.jsqlparser.expression.AnalyticExpression ae expr
            fname (str/lower-case (.getName ae))
            rule (or (get sql-aggregate->return-oid fname)
                     (get sql-fn->return-oid fname))
            arg-expr (or (.getExpression ae)
                         (when-let [obs (.getOrderByElements ae)]
                           (when (seq obs)
                             (.getExpression
                              ^net.sf.jsqlparser.statement.select.OrderByElement
                              (first obs)))))
            arg-oid (when arg-expr (expr-oid arg-expr env))]
        (cond
          (integer? rule)    rule
          (= rule :arg-type) arg-oid
          (map? rule)        (let [r (or (get rule arg-oid)
                                         (get rule :default))]
                               (cond
                                 (integer? r) r
                                 (= r :arg-type) arg-oid
                                 :else nil))
          :else              nil))

      ;; --- EXTRACT() --- always returns float8 in PG --------------------
      (instance? ExtractExpression expr) types/oid-float8

      ;; --- CURRENT_DATE / CURRENT_TIME / CURRENT_TIMESTAMP
      ;; (parsed as TimeKeyExpression by JSqlParser) ----------------------
      (instance? TimeKeyExpression expr)
      (let [k (some-> (.getStringValue ^TimeKeyExpression expr) str/lower-case)]
        (cond
          (str/includes? (or k "") "date") types/oid-date
          (str/includes? (or k "") "time") types/oid-timestamptz
          :else types/oid-timestamptz))

      ;; --- Placeholders — bind params use the param-oid hint -----------
      (instance? JdbcParameter expr)        nil
      (instance? JdbcNamedParameter expr)   nil

      :else nil)))
