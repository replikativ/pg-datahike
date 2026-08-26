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
            [datahike.pg.bits :as bits]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.schema Column]
           [net.sf.jsqlparser.expression
            ArrayConstructor ArrayExpression BinaryExpression BooleanValue
            CaseExpression CastExpression DoubleValue Expression ExtractExpression
            Function JdbcNamedParameter JdbcParameter LongValue NotExpression
            NullValue Parenthesis SignedExpression StringValue TimeKeyExpression
            TimestampValue TimeValue DateValue WhenClause]
           [net.sf.jsqlparser.expression.operators.arithmetic
            Addition BitwiseAnd BitwiseLeftShift BitwiseOr BitwiseRightShift
            BitwiseXor Concat Division Modulo Multiplication Subtraction]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression XorExpression]
           [net.sf.jsqlparser.expression.operators.relational
            Between EqualsTo ExistsExpression GreaterThan GreaterThanEquals
            InExpression IsBooleanExpression IsNullExpression LikeExpression
            MinorThan MinorThanEquals NotEqualsTo ParenthesedExpressionList
            RegExpMatchOperator]))

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
   ;; These have text and bit overloads; both preserve the first
   ;; argument's type. OVERLAY's keyword syntax stores its operands in
   ;; JSqlParser's named-parameter list, handled in `function-oid` below.
   "substring"     :arg-type
   "substr"        :arg-type
   "overlay"       :arg-type
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
   "to_number"     types/oid-numeric
   "md5"           types/oid-text
   "quote_ident"   types/oid-text
   "quote_literal" types/oid-text
   "format"        types/oid-text
   ;; The json family returns json (114), not jsonb (3802) — a client
   ;; that gets 3802 for row_to_json picks the jsonb codec and reads the
   ;; wrong punctuation back.
   "pg_is_in_recovery" types/oid-bool
   "to_json"       types/oid-json
   "row_to_json"   types/oid-json
   "json_agg"      types/oid-json
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
   "trunc"         :arg-type
   "sign"          :arg-type
   "mod"           :arg-type
   "gcd"           :arg-type
   ;; Degree trig is float8 -> float8; erf/erfc likewise. div, factorial
   ;; and trim_scale are numeric; scale / min_scale answer an int4.
   "sind" types/oid-float8 "cosd" types/oid-float8
   "tand" types/oid-float8 "cotd" types/oid-float8
   "asind" types/oid-float8 "acosd" types/oid-float8
   "atand" types/oid-float8 "atan2d" types/oid-float8
   "erf" types/oid-float8 "erfc" types/oid-float8
   "setseed" types/oid-void
   "random_normal" types/oid-float8
   "div" types/oid-numeric
   "factorial" types/oid-numeric
   "trim_scale" types/oid-numeric
   "numeric_inc" types/oid-numeric
   "scale" types/oid-int4
   "min_scale" types/oid-int4
   "lcm"           :arg-type
   "width_bucket"  types/oid-int4
   ;; Math — always float
   "sqrt"          :numeric-or-float8
   "cbrt"          types/oid-float8
   "degrees"       types/oid-float8
   "radians"       types/oid-float8
   "cot"           types/oid-float8
   "sinh"          types/oid-float8
   "cosh"          types/oid-float8
   "tanh"          types/oid-float8
   "asinh"         types/oid-float8
   "acosh"         types/oid-float8
   "atanh"         types/oid-float8
   "exp"           :numeric-or-float8
   "ln"            :numeric-or-float8
   "log"           :numeric-or-float8
   "log10"         :numeric-or-float8
   "power"         :numeric-or-float8
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
   ;; Null handling — PostgreSQL resolves a COMMON type across all the
   ;; arguments (select_common_type), it does not take the first one's.
   ;; `coalesce(numeric, float8)` is float8: numeric coerces to float8
   ;; implicitly and float8 does not coerce back.
   "coalesce"      :common-type
   "nullif"        :common-type
   "greatest"      :common-type
   "least"         :common-type
   ;; Date/time — constants + truncation
   "now"           types/oid-timestamptz
   "current_timestamp" types/oid-timestamptz
   "transaction_timestamp" types/oid-timestamptz
   "statement_timestamp"  types/oid-timestamptz
   "localtimestamp" types/oid-timestamp
   "current_date"  types/oid-date
   "current_time"  types/oid-time
   "localtime"     types/oid-time
   "gen_random_uuid" types/oid-uuid
   "uuidv4"        types/oid-uuid
   "uuidv7"        types/oid-uuid
   "uuid_extract_version" types/oid-int4
   "uuid_extract_timestamp" types/oid-timestamptz
   ;; date_trunc has three overloads and each RETURNS its second
   ;; argument's type (pg_proc.dat): timestamptz, timestamp, interval.
   ;; Reporting timestamptz for all of them made `date_trunc('day', ts)`
   ;; on a plain timestamp render with a `+00` offset, which is a
   ;; different instant for a client that reads it as local time.
   "date_trunc"    :arg2-type
   "date_part"     types/oid-float8
   "extract"       types/oid-float8
   "age"           types/oid-interval
   ;; Type checks / introspection
   ;; pg_typeof returns regtype, not text — this is the OID the
   ;; reporter of #19 saw and mistook for the bit type's own.
   "pg_typeof"     types/oid-regtype
   "enum_first"    :arg-type
   "enum_last"     :arg-type
   "enum_range"    types/oid-text-array
   "version"       types/oid-text
   "format_type"   types/oid-text
   "current_setting" types/oid-text
   "set_config"      types/oid-text
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
   "bool_and"       types/oid-bool
   "every"          types/oid-bool
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
   ;; The variance family follows AVG, not COUNT: PostgreSQL declares a
   ;; NUMERIC overload for int2/int4/int8/numeric and a float8 one only
   ;; for float4/float8 (pg_aggregate.dat). Reporting float8 for all of
   ;; them meant `stddev(int)` advertised — and computed — a double where
   ;; PG gives numeric.
   "stddev"         {types/oid-int2    types/oid-numeric
                     types/oid-int4    types/oid-numeric
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float8
                     types/oid-float8  types/oid-float8
                     :default          types/oid-float8}
   "stddev_samp"    {types/oid-int2    types/oid-numeric
                     types/oid-int4    types/oid-numeric
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float8
                     types/oid-float8  types/oid-float8
                     :default          types/oid-float8}
   "stddev_pop"     {types/oid-int2    types/oid-numeric
                     types/oid-int4    types/oid-numeric
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float8
                     types/oid-float8  types/oid-float8
                     :default          types/oid-float8}
   "variance"       {types/oid-int2    types/oid-numeric
                     types/oid-int4    types/oid-numeric
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float8
                     types/oid-float8  types/oid-float8
                     :default          types/oid-float8}
   "var_samp"       {types/oid-int2    types/oid-numeric
                     types/oid-int4    types/oid-numeric
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float8
                     types/oid-float8  types/oid-float8
                     :default          types/oid-float8}
   "var_pop"        {types/oid-int2    types/oid-numeric
                     types/oid-int4    types/oid-numeric
                     types/oid-int8    types/oid-numeric
                     types/oid-numeric types/oid-numeric
                     types/oid-float4  types/oid-float8
                     types/oid-float8  types/oid-float8
                     :default          types/oid-float8}
   "median"         :arg-type
   "corr"           types/oid-float8
   ;; Ordered-set aggregates (WITHIN GROUP). PG: percentile_cont
   ;; returns FLOAT8 for numeric/float input, INTERVAL for interval;
   ;; we only support numeric here so FLOAT8 covers it. percentile_disc
   ;; returns the input type. mode also returns the input type.
   "percentile_cont" types/oid-float8
   "percentile_disc" :arg-type
   "mode"            :arg-type
   ;; array_agg(x) → x's array OID; string_agg(x, sep) → text.
   "array_agg"       :arg-array
   "string_agg"      types/oid-text})

;; ---------------------------------------------------------------------------
;; Inference
;; ---------------------------------------------------------------------------

(declare expr-oid)

(defn- column-oid
  "Resolve a Column reference to an OID via the live schema. Returns nil
   if we can't find the attribute (derived tables, catalog views, etc.);
   caller falls back to TEXT.

   `table-aliases` maps SQL aliases → real table names; we synthesise
   the attribute keyword as `:<table>/<col>` and probe `db`'s schema.

   `:db.cardinality/many` columns project as PG arrays (`int8[]`,
   `text[]`, etc.) — see `col-var!`'s emit-many-ref-array branch. The
   OID returned here drives both describeResult and Metabase's column-
   type rendering, so we must promote to the array OID at this layer
   (not the underlying scalar) for the wire-level array literal to be
   parsed as an array on the client side."
  [^Column col {:keys [db schema table-aliases default-table hints]}]
  (when schema
    (let [col-name   (params/unquote-ident (.getColumnName col))
          col-table  (when-let [t (.getTable col)]
                       (or (params/unquote-ident (.getName t))
                           (params/unquote-ident (.getAlias t))))
          table-real (or (get table-aliases col-table col-table)
                         ;; An unqualified column uses default-table, which
                         ;; may itself be a SQL alias (including a CTE name
                         ;; redirected to its synthetic namespace). Resolve
                         ;; that through the same map as an explicit
                         ;; `alias.column`; otherwise execution finds the
                         ;; right attr while OID inference silently returns
                         ;; nil/text.
                         (get table-aliases default-table default-table)
                         default-table)
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
          valuetype (:db/valueType props)
          cardinality (:db/cardinality props)
          base-oid (when valuetype (pgs/oid-for-valuetype valuetype))
          ;; Native PG array column (Option C): the schema records
          ;; `:pg/type "_T"` on the ident entity. Look it up so the
          ;; column reports its declared array OID even though the
          ;; storage type is `:db.type/string`.
          pg-type-name (when (and db attr) (#'params/pg-type-of-attr db attr))
          pg-name-oid  (when pg-type-name (get types/pg-name->oid pg-type-name))]
      (cond
        ;; Native PG array column wins over the storage-type fallback.
        pg-name-oid pg-name-oid
        (nil? base-oid) nil
        ;; Cardinality-many → array OID. For ref attrs we project the
        ;; deref'd target PK (typically int8); for non-ref many attrs
        ;; (e.g. `:tag/aliases :db.type/string :many`) we project a
        ;; text array. The element type comes from the array OID
        ;; registry so this stays correct as new types are added.
        (= cardinality :db.cardinality/many)
        (get types/element-oid->array-oid base-oid types/oid-text-array)
        :else base-oid))))

(defn- promoted-numeric
  "Numeric promotion for binary arithmetic, matching PG's implicit-cast
   rules: any FLOAT makes the result FLOAT8, else any NUMERIC makes it
   NUMERIC, else INT8. Returns nil if we can't type either side (let
   caller fall back).

   numeric used to promote to float8, which was only invisible because
   decimal LITERALS were float8 too. `numeric + integer` is numeric in
   PostgreSQL; only float8 outranks it."
  [l-oid r-oid]
  (let [int-oid? #(contains? #{types/oid-int8 types/oid-int4 types/oid-int2} %)]
    (cond
      ;; float8 outranks everything, so one typed side settles it even
      ;; when the other is unknown.
      ;; float4 op float4 stays float4 (there is only float4pl et al);
      ;; anything mixed has no float4 operator and resolves to float8 --
      ;; which is why `real * 2` is double precision in PostgreSQL.
      (and (= l-oid types/oid-float4) (= r-oid types/oid-float4))
      types/oid-float4
      (or (= l-oid types/oid-float8) (= r-oid types/oid-float8)
          (= l-oid types/oid-float4) (= r-oid types/oid-float4))
      types/oid-float8
      ;; numeric outranks every integer width, and an unknown operand
      ;; here is a rewritten literal, which is never float8.
      (or (= l-oid types/oid-numeric) (= r-oid types/oid-numeric))
      types/oid-numeric
      ;; Integer arithmetic keeps the WIDER operand's width -- int2+int2
      ;; is int2pl and stays int2, int4+int8 is int48pl and becomes int8.
      ;; Collapsing all three to int8 made every integer expression
      ;; report bigint, so a binary client sized for int4 was handed an
      ;; 8-byte payload. One integer side also settles an UNKNOWN protocol
      ;; parameter or quoted literal: PostgreSQL resolves `$1 + 1` and
      ;; `'1' + int4_col` through the int4 operator. Decimal literals
      ;; rewritten by the plan cache carry a declared NUMERIC OID and
      ;; therefore take the numeric branch above rather than this one.
      (and (int-oid? l-oid) (int-oid? r-oid))
      (let [rank {types/oid-int2 0 types/oid-int4 1 types/oid-int8 2}]
        (if (>= (rank l-oid) (rank r-oid)) l-oid r-oid))
      (and (int-oid? l-oid) (nil? r-oid)) l-oid
      (and (nil? l-oid) (int-oid? r-oid)) r-oid
      (and l-oid r-oid) types/oid-int8
      :else nil)))

(defn- binary-arith-oid [^BinaryExpression e env]
  (let [left (.getLeftExpression e)
        right (.getRightExpression e)
        l0 (expr-oid left env)
        r0 (expr-oid right env)
        ;; StringValue is PostgreSQL's `unknown` pseudo-type until
        ;; operator resolution. When the opposite arithmetic operand is
        ;; typed, its typinput determines both the selected operator and
        ;; result OID (`real * '-10'` remains real, for example).
        l (if (instance? StringValue left) r0 l0)
        r (if (instance? StringValue right) l0 r0)
        date?    #(= % types/oid-date)
        time?    #(= % types/oid-time)
        timestamp? #(contains? #{types/oid-timestamp types/oid-timestamptz} %)
        money?   #(= % types/oid-money)
        money-factor? #(contains? #{types/oid-int2 types/oid-int4 types/oid-int8
                                    types/oid-float4 types/oid-float8} %)
        plus?    (instance? Addition e)
        minus?   (instance? Subtraction e)
        multiply? (instance? Multiplication e)
        divide?  (instance? Division e)]
    (cond
      ;; PostgreSQL's date operators do not follow numeric promotion:
      ;; `date - date` is an integer count of days and `date +/- integer`
      ;; stays a date. Promoting them numerically made the wire report
      ;; int8 for a value the renderer emits as `2020-01-02`, which a
      ;; binary-format client then failed to decode.
      (and minus? (date? l) (date? r))     types/oid-int4
      ;; These operators produce an interval, not a promoted number. The
      ;; result OID matters independently of the text rendering: binary
      ;; clients choose their decoder from RowDescription.
      (and minus? (time? l) (time? r)) types/oid-interval
      (and minus? (timestamp? l) (timestamp? r)) types/oid-interval
      ;; Keyed off the date operand only — see date-arith-op in expr.clj
      ;; for why the other one is not inspected.
      (and (or plus? minus?) (date? l))    types/oid-date
      (and plus? (date? r))                types/oid-date
      ;; money is a closed operator family, not ordinary numeric
      ;; promotion. Its Datahike carrier is BigDecimal, but +, -, *, and
      ;; scalar / retain money while money / money returns float8.
      (and divide? (money? l) (money? r)) types/oid-float8
      (and (or plus? minus?) (money? l) (money? r)) types/oid-money
      (and multiply?
           (or (and (money? l) (money-factor? r))
               (and (money-factor? l) (money? r)))) types/oid-money
      (and divide? (money? l) (money-factor? r)) types/oid-money
      :else (promoted-numeric l r))))

(defn resolve-aggregate-result-oid
  "Given an aggregate name and the OID of its first argument, return
   the result OID per `sql-aggregate->return-oid`. Returns nil for
   unknown aggregates or unresolvable rules (caller falls back).

   Public so the translator can derive the runtime fn variant from
   the same rule the OID inference uses — avoids drift between
   describeResult's reported type and the actual runtime."
  [agg-name input-oid]
  (let [rule (get sql-aggregate->return-oid (str/lower-case agg-name))]
    (cond
      (integer? rule)    rule
      (= rule :arg-type) input-oid
      ;; array_agg(x) → x's array OID (text→text[], int4→int4[], …).
      (= rule :arg-array) (when input-oid
                            (get types/element-oid->array-oid input-oid types/oid-text-array))
      (map? rule)        (let [r (or (get rule input-oid)
                                     (get rule :default))]
                           (cond
                             (integer? r) r
                             (= r :arg-type) input-oid
                             :else nil))
      :else              nil)))

(defn untyped-literal?
  "PostgreSQL's UNKNOWN: a quoted literal, NULL, or an unresolved
   parameter. Such an operand takes its type FROM the others, so it must
   not take part in resolving a common type or an operator -- `CASE WHEN
   … THEN NULL ELSE 2 END` is integer, and `flag = 'true'` is a boolean
   comparison.

   `expr-oid` reports text for a quoted literal because that is the
   right answer for a PROJECTION (`SELECT 'a'` is text); it is the wrong
   one for RESOLUTION, which is what this distinguishes."
  [e]
  (or (instance? StringValue e)
      (instance? NullValue e)
      (instance? JdbcNamedParameter e)
      ;; A `$N` is unknown only while its type is undeclared. The
      ;; plan-cache rewrite turns every bare number into one, so treating
      ;; the node itself as untyped would blind resolution to exactly the
      ;; literals it is meant to catch -- `flag = 10` reaches the
      ;; translator as `flag = $1` with int4 declared for $1.
      (and (instance? JdbcParameter e)
           (nil? (get params/*declared-param-oids* (.getIndex ^JdbcParameter e))))))

(defn resolution-oid
  "`expr-oid`, except that an untyped literal reports nil (UNKNOWN)."
  [e env]
  (when-not (untyped-literal? e) (expr-oid e env)))

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
               (when-let [pl (or (.getParameters f) (.getNamedParameters f))]
                 (vec (.getExpressions pl)))
               (catch Exception _ nil))
        first-arg (first args)
        rule (or (get sql-aggregate->return-oid fname)
                 (get sql-fn->return-oid fname)
                 (get-in fns/sql-function-specs [fname :return-oid]))]
    (cond
      ;; ROW(...) — anonymous composite constructor → record OID (2249).
      ;; A ::type cast wrapping it overrides via cast-oid.
      (= fname "row") 2249

      ;; Aggregate rule (registered in sql-aggregate->return-oid) —
      ;; delegate to the shared resolver so runtime variant selection
      ;; (in stmt.clj) doesn't have to recompute the same logic.
      (contains? sql-aggregate->return-oid fname)
      (let [input-oid (cond
                        (nil? first-arg) types/oid-int8 ; COUNT(*) defensive
                        :else (expr-oid first-arg env))]
        (resolve-aggregate-result-oid fname input-oid))
      (integer? rule) rule
      (= rule :arg-type) (when first-arg (expr-oid first-arg env))
      ;; The SECOND argument's type -- `date_trunc(unit, ts)` returns
      ;; whatever ts is.
      (= rule :arg2-type)
      (when-let [a2 (second args)]
        ;; A `date` has no date_trunc overload of its own and coerces
        ;; implicitly to BOTH timestamp and timestamptz, so function
        ;; resolution falls to the category's PREFERRED type -- verified
        ;; against the oracle: `date_trunc('month', date_col)` comes back
        ;; with a `+00`.
        (let [o (expr-oid a2 env)]
          (if (= o types/oid-date) types/oid-timestamptz o)))
      ;; COALESCE / NULLIF / GREATEST / LEAST resolve a COMMON type over
      ;; every argument, the same way CASE and UNION do.
      (= rule :common-type)
      (types/select-common-type (mapv #(resolution-oid % env) args)
                                (str/upper-case fname) false)
      ;; PostgreSQL declares BOTH a float8 and a numeric overload of
      ;; sqrt / exp / ln / log / log10 / power, and function resolution
      ;; prefers the candidate with an exact-type argument -- so ANY
      ;; numeric argument selects the numeric one, while all-integer
      ;; arguments fall to float8 (the preferred type in the NUMERIC
      ;; category). `2^10` is float8; `2.0^10` and `2^10.0` are numeric.
      (= rule :numeric-or-float8)
      ;; Two-argument `log` is the exception: PostgreSQL has only
      ;; log(numeric, numeric), so it is numeric whatever the arguments
      ;; look like. Reporting float8 there while the runtime answered a
      ;; numeric is precisely the Describe/Execute mismatch that
      ;; corrupts a binary client.
      (if (or (and (= fname "log") (= 2 (count args)))
              (some #(= types/oid-numeric (expr-oid % env)) (remove nil? args)))
        types/oid-numeric
        types/oid-float8)
      :else nil)))

(defn- composite-name->oid
  "Resolve a named composite type to its persisted OID. Enum values remain
   text on the wire until pgjdbc's custom-type introspection query is fully
   supported; their catalog OID is still available through pg_type/pg_enum."
  [type-str db]
  (when (and type-str db)
    (some (fn [{:keys [name oid]}] (when (= name type-str) oid))
          (pgs/composite-types db))))

(defn- cast-oid
  "Map a SQL CAST target type-name to an OID. Uses `types/cast-category`
   so the set of recognised target types stays in one place."
  [^CastExpression c env]
  (let [cdt      (.getColDataType c)
        ;; .getDataType returns the BASE name ("int") and exposes the `[]`
        ;; only via .getArrayData — so an array cast like `::int[]` must be
        ;; detected here and wrapped to the element's array OID, else it
        ;; reports the scalar (int4) and the binary array value mis-decodes.
        type-str (some-> cdt .getDataType str str/lower-case
                         types/base-type-name-of)
        ad       (when cdt (.getArrayData cdt))
        array?   (and ad (pos? (.size ^java.util.List ad)))
        scalar-oid
        (or
         (composite-name->oid type-str (:db env))
         (case (types/cast-category type-str)
      ;; Datahike stores every integer as a Clojure long, but an explicit
      ;; CAST asserts a specific PG width — report the matching OID so
      ;; clients parse the column correctly (e.g. node-postgres returns
      ;; int4 as a JS number but int8 as a string). The wire bytes are
      ;; identical in text mode; encodeBinary narrows the long to the
      ;; declared width for binary clients.
           :integer   (cond
                        (#{"smallint" "int2" "smallserial" "serial2"} type-str) types/oid-int2
                        (#{"bigint" "int8" "bigserial" "serial8"} type-str)      types/oid-int8
                        :else                                                    types/oid-int4)
           ;; `real` is a DISTINCT type, not a spelling of double
           ;; precision -- `1.1::real` is 1.100000023841858 as a float8
           ;; and `pg_typeof` says real.
           :float     (if (#{"real" "float4"} type-str) types/oid-float4 types/oid-float8)
           :numeric   types/oid-numeric
           :money     types/oid-money
           :text      (cond
                        (contains? #{"varchar" "character varying"} type-str) types/oid-varchar
                        (contains? #{"char" "character" "bpchar"} type-str) types/oid-bpchar
                        (= "name" type-str) types/oid-name
                        :else types/oid-text)
           :boolean   types/oid-bool
           :date      types/oid-date
           :time      types/oid-time
           :timestamp (cond
                        (re-find #"with time zone|timestamptz" type-str)
                        types/oid-timestamptz
                        :else types/oid-timestamp)
           :interval  types/oid-interval
           :uuid      types/oid-uuid
           :bytes     types/oid-bytea
           :bit       types/oid-bit
           :varbit    types/oid-varbit
           nil)
         ;; Fallback for types cast-category doesn't width-classify (jsonb,
         ;; json, inet, name, oid, …): the canonical pg_type-name → OID map is
         ;; comprehensive. Without this, `::jsonb` / `::jsonb[]` reported text
         ;; and the binary value mis-decoded.
         (get types/pg-name->oid type-str))]
    (cond
      (nil? scalar-oid) nil
      array? (get types/element-oid->array-oid scalar-oid types/oid-text-array)
      :else scalar-oid)))

(defn- case-oid
  "CASE expression returns the type of its first non-nil branch. PG
   computes the least-common-supertype across branches; we use the first
   typed branch as a good-enough approximation (binary promotion is too
   aggressive — we'd promote INT + TEXT to TEXT, which may not be what
   the user intended)."
  [^CaseExpression e env]
  (let [branches (concat
                  ;; parse_expr.c prepends CASE/ELSE before choosing the
                  ;; common type. Order matters when casts work both ways and
                  ;; neither type is preferred (varchar versus bpchar).
                  (when-let [el (.getElseExpression e)] [el])
                  (mapv #(.getThenExpression ^WhenClause %) (.getWhenClauses e)))]
    ;; The COMMON type of every branch, not the first branch that happens
    ;; to have one: `CASE WHEN … THEN 1.50::numeric ELSE 1.5::float8 END`
    ;; is float8 in PostgreSQL, and prints 1.5 rather than 1.50.
    (types/select-common-type (mapv #(resolution-oid % env) branches) "CASE" false)))

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
      ;; PostgreSQL types an integer literal as the NARROWEST of int4 /
      ;; int8 that holds it (scan.l -> make_const), not int8 always. It
      ;; is why `SELECT 1` reports integer, and why `int4col + 1` stays
      ;; int4 rather than widening to bigint.
      (instance? LongValue expr)
      (let [v (.getValue ^LongValue expr)]
        (if (and (>= v Integer/MIN_VALUE) (<= v Integer/MAX_VALUE))
          types/oid-int4
          types/oid-int8))
      ;; PostgreSQL types an unadorned decimal literal as numeric, not
      ;; float8 -- including one written with an exponent (`1.0e3`).
      (instance? DoubleValue expr)    types/oid-numeric
      ;; Bit-string literals MUST precede StringValue — JSqlParser also
      ;; uses StringValue for `B'1001000'` (prefix "B"). PG types both
      ;; `B'…'` and `X'…'` as bit (1560), not text (issue #28).
      (bits/bit-string-literal? expr)  types/oid-bit
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

      ;; PostgreSQL folds a unary sign INTO the constant before typing it
      ;; (gram.y doNegate), so `-2147483648` is int4 even though 2147483648
      ;; alone is int8. Typing the operand and ignoring the sign made
      ;; `abs(-2147483648)` an int8 abs, which succeeds, where PostgreSQL
      ;; raises 22003.
      (instance? SignedExpression expr)
      (let [^SignedExpression se expr
            inner (.getExpression se)]
        (if (and (= \- (.getSign se)) (instance? LongValue inner))
          (let [v (- (.getValue ^LongValue inner))]
            (if (and (>= v Integer/MIN_VALUE) (<= v Integer/MAX_VALUE))
              types/oid-int4
              types/oid-int8))
          (expr-oid inner env)))

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
      ;; Division consults its operands like every other arithmetic
      ;; operator. It used to be hardcoded float8 under the comment "PG:
      ;; integer div is exact" -- which is backwards: integer division in
      ;; PostgreSQL is exact precisely BECAUSE it stays integral, so
      ;; float8 is the one type it cannot be. `SELECT 3 / 2` sent the
      ;; correct text `1` under a float8 OID, and every typed client
      ;; turned it back into 1.0.
      (instance? Division expr)       (binary-arith-oid expr env)
      (instance? Modulo expr)         (binary-arith-oid expr env)

      ;; --- Bitwise operators ---------------------------------------------
      ;; `& | << >>` return the operand type: bit for bit operands (PG
      ;; declares them only on bit, so even varbit operands yield bit),
      ;; otherwise the promoted integer type.
      (or (instance? BitwiseAnd expr) (instance? BitwiseOr expr)
          (instance? BitwiseLeftShift expr) (instance? BitwiseRightShift expr)
          (instance? XorExpression expr))
      (let [^BinaryExpression e expr
            l (expr-oid (.getLeftExpression e) env)]
        (if (or (= l types/oid-bit) (= l types/oid-varbit))
          types/oid-bit
          (binary-arith-oid e env)))

      ;; `^` is exponentiation, not xor — float8 for integer operands,
      ;; matching PG's preference for float8 over numeric when neither
      ;; `^(float8,float8)` nor `^(numeric,numeric)` matches exactly.
      ;; `^` is numeric_power when an operand is numeric, like power().
      (instance? BitwiseXor expr)
      (let [^BinaryExpression e expr]
        (if (or (= types/oid-numeric (expr-oid (.getLeftExpression e) env))
                (= types/oid-numeric (expr-oid (.getRightExpression e) env)))
          types/oid-numeric
          types/oid-float8))

      ;; --- Concat (||) ---------------------------------------------------
      ;; bit || bit is `bitcat`, whose result is always bit varying (the
      ;; widths add, so no fixed-width type fits) — PG resolves the
      ;; varbit operator, not the text one. Everything else concatenates
      ;; as text.
      (instance? Concat expr)
      (let [^BinaryExpression e expr]
        (if (and (some-> (.getLeftExpression e) (expr-oid env) (= types/oid-bit))
                 (some-> (.getRightExpression e) (expr-oid env) (= types/oid-bit)))
          types/oid-varbit
          types/oid-text))

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
      (instance? CastExpression expr) (cast-oid expr env)

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
            arg-expr (or (.getExpression ae)
                         (when-let [obs (.getOrderByElements ae)]
                           (when (seq obs)
                             (.getExpression
                              ^net.sf.jsqlparser.statement.select.OrderByElement
                              (first obs)))))
            arg-oid (when arg-expr (expr-oid arg-expr env))]
        (or (resolve-aggregate-result-oid fname arg-oid)
            (let [rule (get sql-fn->return-oid fname)]
              (cond
                (integer? rule)    rule
                (= rule :arg-type) arg-oid
                :else              nil))))

      ;; --- EXTRACT() --- always returns float8 in PG --------------------
      (instance? ExtractExpression expr) types/oid-float8

      ;; --- CURRENT_DATE / CURRENT_TIME / CURRENT_TIMESTAMP
      ;; (parsed as TimeKeyExpression by JSqlParser) ----------------------
      (instance? TimeKeyExpression expr)
      (let [k (some-> (.getStringValue ^TimeKeyExpression expr) str/lower-case)]
        (cond
          (= k "current_date") types/oid-date
          (= k "current_time") types/oid-time
          (= k "current_timestamp") types/oid-timestamptz
          :else types/oid-timestamptz))

      ;; --- Placeholders -------------------------------------------------
      ;; A bare `$N` has no statically inferable type here: PG types it
      ;; from the Parse message's declared OID, which is per-statement
      ;; and therefore can't live in the (SQL-keyed) parse cache. We
      ;; return nil and let describeResult resolve it via
      ;; `param-placeholder-index` below. See issue #27.
      ;; A `$N` is typed from the declared parameter types when we have
      ;; them -- which is how PostgreSQL types a Param, and which is the
      ;; only way an expression over a rewritten literal can be typed at
      ;; all. nil when undeclared, so the caller still falls back.
      (instance? JdbcParameter expr)
      (get params/*declared-param-oids* (.getIndex ^JdbcParameter expr))
      (instance? JdbcNamedParameter expr)   nil

      :else nil)))

(defn param-placeholder-index
  "The 1-based `$N` index when `expr` is a bare parameter placeholder,
   else nil.

   Only a *bare* placeholder qualifies. `$1::int4` is a CastExpression
   whose type `cast-oid` already resolves, and `$1 + 1` gets its type
   from the arithmetic rule — in both cases PG takes the type from the
   expression, not from the parameter declaration, so reporting an index
   there would let a declared OID override a correctly inferred one.

   Parenthesised placeholders are unwrapped: PG's exprType sees through
   parens, so `SELECT ($1)` types the same as `SELECT $1`. JSqlParser
   spells a parenthesised expression two ways — `Parenthesis`, and
   `ParenthesedExpressionList` (a one-element list) for a select item —
   so both are unwrapped, repeatedly for `SELECT (($1))`."
  [expr]
  (loop [expr expr]
    (cond
      (instance? Parenthesis expr)
      (recur (.getExpression ^Parenthesis expr))

      (and (instance? ParenthesedExpressionList expr)
           (= 1 (.size ^ParenthesedExpressionList expr)))
      (recur (.get ^ParenthesedExpressionList expr 0))

      (instance? JdbcParameter expr)
      (.getIndex ^JdbcParameter expr))))
