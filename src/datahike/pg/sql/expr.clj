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
            [datahike.db.interface :as dbi]
            [datahike.pg.sql.oid-infer :as oid-infer]
            [clojure.string :as str]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.bits :as pg-bits]
            [datahike.pg.errors :as errors]
            [datahike.pg.records :as pg-rec]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.cast :as sql-cast]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.set-ops :as set-ops]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression
            Alias ArrayExpression Function LongValue DoubleValue StringValue NullValue
            BooleanValue Parenthesis NotExpression CaseExpression WhenClause
            SignedExpression CastExpression TimeKeyExpression JsonExpression
            ExtractExpression TrimFunction BinaryExpression
            TimezoneExpression ArrayConstructor JdbcParameter JdbcNamedParameter]
           [net.sf.jsqlparser.expression.operators.relational
            DoubleAnd EqualsTo ExistsExpression ExpressionList
            GreaterThan GreaterThanEquals InExpression IsBooleanExpression
            IsDistinctExpression IsUnknownExpression
            IsNullExpression JsonOperator LikeExpression MinorThan
            MinorThanEquals NotEqualsTo ParenthesedExpressionList
            RegExpMatchOperator Between]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression XorExpression]
           [net.sf.jsqlparser.expression.operators.arithmetic
            Addition Subtraction Multiplication Division Modulo Concat
            BitwiseAnd BitwiseOr BitwiseXor BitwiseLeftShift BitwiseRightShift]
           [net.sf.jsqlparser.statement.select
            PlainSelect SelectItem AllColumns ParenthesedSelect Join Values
            Select SetOperationList FromItem OrderByElement WithItem]))

(set! *warn-on-reflection* true)

;; Unqualified alias so the copied body's `unquote-ident` reads from
;; params — same pattern used by ctx / catalog / ddl.
(def ^:private unquote-ident params/unquote-ident)

;; ---------------------------------------------------------------------------
;; Forward declarations for the mutually-recursive translate-* family.

(declare jsonb-column? json-column? reject-json-operator!)
(declare coerce-unknown-literal coerce-comparison-operands)

(def ^:private ^:const max-inline-numeric-precision
  "BigDecimals wider than this travel as Datalog inputs instead of query-form
   constants. The cutoff is far above ordinary application numerics while
   bounding the cost of Clojure's scale-insensitive numeric hash."
  4096)
(declare source-oid string-value-text op-sym->sql)

(declare translate-expr
         column-value!
         translate-predicate
         translate-case-expr
         translate-cast-expr
         translate-predicate-expr
         translate-comparison
         check-comparison-types!
         translate-function-call
         translate-binary-arith
         strict-subquery-values
         strict-subquery-rows
         strict-subquery-row
         strict-scalar-subquery
         scalar-subquery-output-oid
         in-left-asts
         row-expression?
         row-comparison-expression?
         translate-row-comparison
         analyze-in-subquery!
         analyze-exists-subquery!
         uncorrelated-in-values-var!
         uncorrelated-in-rows-var!
         row-in-result-var!
         outer-alias-set
         correlated-in-var!
         correlated-row-in-var!
         operand-type-oid
         flatten-json-chain
         interpret-form
         parse-timestamp-string)

(defn- numeric-target-for-oid [oid]
  (case oid
    21 :long
    23 :long
    20 :long
    700 :float
    701 :double
    1700 :bigdec
    nil))

(defn- coerce-function-unknowns
  "Apply a function spec's argument-resolution rule to raw AST operands.
   PostgreSQL uses a known argument type to resolve unknown literals in
   homogeneous calls (for example div(numeric, unknown))."
  [ctx fname arg-exprs]
  (let [rule (get-in fns/sql-function-specs [fname :unknown-args])
        homogeneous-count (cond
                            (= :homogeneous rule) (count arg-exprs)
                            (map? rule) (:homogeneous-prefix rule))]
    (if homogeneous-count
      (let [homogeneous-args (take homogeneous-count arg-exprs)
            targets (keep #(numeric-target-for-oid
                            (try (source-oid ctx %)
                                 (catch Throwable _ nil)))
                          homogeneous-args)
            ;; Resolve from every known argument, not merely the first.
            ;; PostgreSQL promotes integer + numeric to numeric, and a
            ;; float argument outranks numeric for these overloads.
            rank {:long 0 :bigdec 1 :float 2 :double 3}
            target (when (seq targets) (apply max-key rank targets))]
        (if target
          (mapv (fn [idx arg]
                  (if (and (< idx homogeneous-count)
                           (instance? StringValue arg))
                    (coerce/coerce-numeric (string-value-text arg) target)
                    arg))
                (range) arg-exprs)
          arg-exprs))
      arg-exprs)))

(def ^:dynamic *conjunctive-where*
  "True while translating top-level AND-ed conjuncts of a WHERE (or an
   INNER JOIN's ON), where emitting a *data pattern* is sound: the
   pattern constrains the whole query exactly like the predicate it
   replaces. Enables the indexable fast paths (ctx/bind-col-value!,
   ctx/unify-inner-equijoin!). MUST be re-bound false inside OR / NOT
   branches — a data pattern added to the global :where set from inside
   a disjunct would constrain rows the disjunct shouldn't touch. Bound
   true by stmt.clj at the WHERE / inner-ON entry points; default false
   keeps every other context (CASE conditions, HAVING, projections,
   outer-join ON) on the predicate path."
  false)

(defn decode-e-string
  "Decode a PostgreSQL `E'...'` string body, per src/backend/parser/scan.l.

   Ordinary literals do NOT process escapes under
   `standard_conforming_strings = on` — only `E''` strings do, and this
   is the only place that difference is realised.

   The grammar is exact, not approximate:
     xeoctesc    \\[0-7]{1,3}
     xehexesc    \\x[0-9A-Fa-f]{1,2}
     xeunicode   \\uXXXX  |  \\UXXXXXXXX   (exactly 4 / exactly 8)
     single char b f n r t v  ->  the control character
     ANY other \\c          ->  c itself (PostgreSQL's documented
                                fallthrough: E'a\\qb' is `aqb`)
     ''                     ->  a single quote

   Decodes the RAW body rather than JSqlParser's `getNotExcapedValue`,
   because PostgreSQL accepts both `''` and `\\'` as a quote and the
   pre-collapsed form has already lost the distinction."
  ^String [^String body]
  (let [n (count body)
        sb (StringBuilder. n)
        hex? (fn [^Character ch] (and ch (Character/isLetterOrDigit ch)
                                      (>= (Character/digit (char ch) 16) 0)))
        oct? (fn [^Character ch] (and ch (<= (int \0) (int ch) (int \7))))]
    (loop [i 0]
      (if (>= i n)
        (.toString sb)
        (let [c (.charAt body i)]
          (cond
            ;; '' -> '
            (and (= c \') (< (inc i) n) (= (.charAt body (inc i)) \'))
            (do (.append sb \') (recur (+ i 2)))

            (not= c \\)
            (do (.append sb c) (recur (inc i)))

            (>= (inc i) n)
            (do (.append sb c) (recur (inc i)))

            :else
            (let [d (.charAt body (inc i))]
              (cond
                ;; \uXXXX / \UXXXXXXXX — the digit count is exact, and a
                ;; short one is an ERROR rather than a fallthrough.
                (or (= d \u) (= d \U))
                (let [want (if (= d \u) 4 8)
                      start (+ i 2)
                      end (+ start want)
                      digits (when (<= end n) (subs body start end))]
                  (if (and digits (every? #(hex? %) digits))
                    (let [cp (Long/parseLong digits 16)]
                      (when (zero? cp)
                        (throw (ex-info "invalid byte sequence for encoding \"UTF8\": 0x00"
                                        {:error :character-not-in-repertoire
                                         :sqlstate "22021"})))
                      (.appendCodePoint sb (int cp))
                      (recur end))
                    (throw (ex-info "invalid Unicode escape"
                                    {:error :invalid-escape-sequence
                                     :sqlstate "22025"
                                     :hint "Unicode escapes must be \\uXXXX or \\UXXXXXXXX."}))))

                ;; \xNN — one or two hex digits
                (= d \x)
                (let [d1 (when (< (+ i 2) n) (.charAt body (+ i 2)))
                      d2 (when (< (+ i 3) n) (.charAt body (+ i 3)))
                      ds (cond (and (hex? d1) (hex? d2)) (str d1 d2)
                               (hex? d1) (str d1)
                               :else nil)]
                  (if ds
                    (let [v (Integer/parseInt ds 16)]
                      (when (zero? v)
                        (throw (ex-info "invalid byte sequence for encoding \"UTF8\": 0x00"
                                        {:error :character-not-in-repertoire
                                         :sqlstate "22021"})))
                      (.append sb (char v))
                      (recur (+ i 2 (count ds))))
                    ;; `\x` with no hex digit falls through to plain `x`
                    (do (.append sb \x) (recur (+ i 2)))))

                ;; \0 .. \377 — one to three octal digits
                (oct? d)
                (let [ds (loop [j (inc i) acc (StringBuilder.)]
                           (if (and (< j n) (< (.length acc) 3) (oct? (.charAt body j)))
                             (recur (inc j) (.append acc (.charAt body j)))
                             (.toString acc)))
                      v (Integer/parseInt ds 8)]
                  (when (zero? v)
                    (throw (ex-info "invalid byte sequence for encoding \"UTF8\": 0x00"
                                    {:error :character-not-in-repertoire
                                     :sqlstate "22021"})))
                  (.append sb (char v))
                  (recur (+ i 1 (count ds))))

                :else
                (do (.append sb (case d
                                  \b \backspace
                                  \f \formfeed
                                  \n \newline
                                  \r \return
                                  \t \tab
                                  \v (char 11)
                                  d))
                    (recur (+ i 2)))))))))))

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
    (cond
      (and v prefix (.equalsIgnoreCase ^String prefix "N"))
      ;; CHAR-coerce: rstrip trailing ASCII spaces.
      (str/replace v #" +$" "")

      ;; E'...' is the ONLY string form that processes escapes. Decode
      ;; the raw body, not the `''`-collapsed one — see decode-e-string.
      (and prefix (.equalsIgnoreCase ^String prefix "E"))
      (decode-e-string (.getValue sv))

      :else v)))

(defn- oid-env
  "The environment `oid-infer/expr-oid` needs, pulled off a translation ctx."
  [ctx]
  {:db            (:db ctx)
   :schema        (:schema ctx)
   :table-aliases (:table-aliases ctx)
   :default-table (:default-table ctx)
   :from-binding-oids params/*from-binding-oids*
   :scalar-subquery-oid #(scalar-subquery-output-oid ctx %)
   :hints         (:hints ctx)})

(defn- source-oid
  "The OID of `expr`'s value, when it can be inferred. Used to tell a
   `date` from a `timestamp` when rendering to text: Datahike has only
   :db.type/instant, so both columns arrive as java.util.Date and the
   value itself carries no answer."
  [ctx expr]
  (try (oid-infer/expr-oid expr (oid-env ctx)) (catch Throwable _ nil)))

(defn- enum-name-of-expr
  "Recover the declared enum type from a cast or stored enum column. Enum
   values themselves are strings, so runtime inspection cannot distinguish
   two enum types (or an enum from text); the SQL expression must carry it."
  [ctx expr]
  (cond
    (instance? CastExpression expr)
    (let [type-name (some-> ^CastExpression expr .getColDataType str str/lower-case)]
      (when (params/registered-enum-values (:db ctx) type-name)
        (-> type-name (str/split #"\.") last params/unquote-ident)))

    (instance? Column expr)
    (when-let [resolved (try (ctx/resolve-column ^Column expr
                                                 (:table-aliases ctx)
                                                 (:default-table ctx)
                                                 (:col-overrides ctx)
                                                 (:derived-aliases ctx)
                                                 (:ci-index ctx))
                             (catch Throwable _ nil))]
      (when-let [attr (ctx/attr-of ctx resolved)]
        (ffirst
         (d/q '{:find [?name]
                :in [$ ?ident]
                :where [[?e :db/ident ?ident]
                        [?e :datahike.pg/enum-of ?name]]}
              (:db ctx) attr))))

    :else nil))

(defn enum-spec-for-exprs [ctx exprs]
  "Return the registry spec for the first enum-typed SQL expression. Public
   for statement lowering, which must use the same type recovery for ORDER BY
   and aggregates as predicate lowering does."
  (when-let [enum-name (some #(enum-name-of-expr ctx %) exprs)]
    (some #(when (= enum-name (:name %)) %)
          (pgs/enum-types (:db ctx)))))

(defn enum-rank-var!
  "Bind the declaration-order rank of enum `value` and return its logic var.
   SQL NULL remains the null sentinel so the normal server-side NULL ordering
   path continues to apply."
  [ctx spec value]
  (let [rank-by-label (zipmap (:values spec) (range))
        fn-param (symbol (str "?enum-rank-" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        rank-fn (fn [v]
                  (if (fns/sql-null? v)
                    :__null__
                    (get rank-by-label (str v) Long/MAX_VALUE)))]
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj rank-fn)
    (swap! (:where-clauses ctx) conj [(list fn-param value) result-var])
    (swap! (:nullable-vars ctx) conj result-var)
    result-var))

(def common-type-fns
  "The functions whose result type PostgreSQL resolves with
   `select_common_type` over their arguments rather than taking the
   first one's."
  #{"coalesce" "nullif" "greatest" "least"})

(defn coerce-to-common!
  "Coerce `v` to the common type of `arg-exprs`, per PostgreSQL's
   `select_common_type`, and return the coerced variable -- or `v`
   unchanged when every argument already has that type.

   CASE, COALESCE, NULLIF, GREATEST and LEAST all resolve one type for
   the whole construct and coerce each branch to it. Returning a branch
   value as-is is visibly wrong when the branches differ:
   `coalesce(n, f)` over a numeric and a float8 is float8 in PostgreSQL,
   so it prints 1.5 where the untouched numeric prints 1.50."
  [ctx v arg-exprs construct]
  (let [oids (mapv #(when-not (oid-infer/untyped-literal? %) (source-oid ctx %)) arg-exprs)
        common (types/select-common-type oids construct false)]
    (if-not (and common (some #(and % (not= % common)) oids))
      v
      (let [tname (get types/oid->pg-name common)
            cast1 (fn [x]
                    (if (or (nil? x) (= :__null__ x))
                      x
                      (try (sql-cast/cast-scalar x tname {:explicit? true})
                           (catch Throwable _ x))))]
        (if (or (symbol? v) (seq? v))
          (let [p (symbol (str "?common-cast" (swap! (:var-counter ctx) inc)))
                out (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) v)
                vv (if (seq? v) (ctx/materialize-arg! ctx v) v)]
            (swap! (:in-params ctx) conj p)
            (swap! (:in-args ctx) conj cast1)
            (ctx/add-clause! ctx [(list p vv) out])
            out)
          ;; already a value: fold now
          (cast1 v))))))

(defn- int-width-of
  "The declared integer width of a single expression, or nil."
  [ctx e]
  (get types/oid->integer-width
       (try (oid-infer/expr-oid e (oid-env ctx)) (catch Throwable _ nil))))

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

(declare translate-deferred-form)

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
        ;; The SQL keyword call forms -- `substring(s FROM 1 FOR 2)`,
        ;; `position('a' IN s)`, `trim(BOTH ' ' FROM s)` -- put their
        ;; operands in a NamedExpressionList and leave .getParameters
        ;; empty, so they arrived with no arguments at all.
        params (or (.getParameters f)
                   (some-> (.getNamedParameters f) .getExpressions))
        arg-exprs (when params (vec params))
        arg-exprs (coerce-function-unknowns ctx fname arg-exprs)
        lazy-args? (= fname "coalesce")
        raw-args (when arg-exprs
                   (mapv #(if (instance? net.sf.jsqlparser.expression.Expression %)
                            (if lazy-args?
                              (translate-deferred-form
                               ctx (fn [] (translate-expr ctx %)))
                              (translate-expr ctx %))
                            %)
                         arg-exprs))
        ;; `position(sub IN str)` names its operands the other way round
        ;; from the function it resolves to -- PostgreSQL's gram.y swaps
        ;; them before analysis, so do the same for the keyword form only.
        raw-args (if (and (= fname "position")
                          (nil? (.getParameters f))
                          (= 2 (count raw-args)))
                   [(second raw-args) (first raw-args)]
                   raw-args)
        ;; Materialize complex sub-expressions into intermediate vars
        args (binding [ctx/*defer-expression-materialization* lazy-args?]
               (when raw-args
                 (mapv #(ctx/materialize-arg! ctx %) raw-args)))
        ;; Strict-function nullability: `upper(s)` is NULL wherever s is,
        ;; and the result var is the operand a null-guard has to name.
        result-var (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) args)]
    (cond
      ;; SQL value functions with a precision modifier parse as ordinary
      ;; Function nodes (`current_timestamp(0)`, `localtime(3)`). They are
      ;; stable within one statement and are not catalog function lookups.
      (contains? #{"current_timestamp" "current_time"
                   "localtimestamp" "localtime"} fname)
      (let [fn-param (symbol (str "?sql-time" (swap! (:var-counter ctx) inc)))
            temporal-fn
            (fn [& [precision]]
              (let [^java.util.Date statement-time
                    (or params/*statement-time* (java.util.Date.))
                    instant (.toInstant statement-time)
                    p (long (min 6 (max 0 (or precision 6))))
                    factor (long (Math/pow 10 (- 9 p)))
                    nanos (.getNano instant)
                    truncated (.with instant java.time.temporal.ChronoField/NANO_OF_SECOND
                                     (* (quot nanos factor) factor))
                    zdt (.atZone truncated java.time.ZoneOffset/UTC)]
                (case fname
                  "current_timestamp" (java.util.Date/from truncated)
                  "current_time" (.toOffsetTime (.toOffsetDateTime zdt))
                  "localtimestamp" (.toLocalDateTime zdt)
                  "localtime" (.toLocalTime zdt))))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj temporal-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; PostgreSQL exposes date(timestamp) as the function-style spelling
      ;; of an explicit cast. It is used by the upstream expressions suite
      ;; and by application SQL generated independently of `::date`.
      (= fname "date")
      (let [fn-param (symbol (str "?date-cast" (swap! (:var-counter ctx) inc)))
            date-fn (fn [v]
                      (sql-cast/cast-scalar
                       v "date" {:parse-timestamp parse-timestamp-string}))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj date-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

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
            state params/*session-state*
            impl-fn (fn [] (or (some-> state deref :db-name) "datahike"))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
        result-var)

      (= fname "current_schema")
      (let [fn-param (symbol (str "?cur-sch" (swap! (:var-counter ctx) inc)))
            state params/*session-state*
            impl-fn (fn []
                      (let [path (or (some-> state deref :search-path)
                                     ["$user" "public"])]
                        (or (first (filter #{"public" "pg_catalog"} path))
                            :__null__)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
        result-var)

      (= fname "pg_get_viewdef")
      (let [fn-param (symbol (str "?viewdef" (swap! (:var-counter ctx) inc)))
            definitions (if-let [db (:db ctx)]
                          (into {}
                                (map (fn [[name definition]]
                                       [(long (Math/abs (.hashCode ^String name)))
                                        definition]))
                                (d/q '{:find [?name ?definition]
                                       :where [[?e :datahike.pg/view-name ?name]
                                               [?e :datahike.pg/view-definition ?definition]]}
                                     db))
                          {})
            impl-fn (fn [oid & _]
                      (let [oid (cond
                                  (number? oid) (long oid)
                                  (string? oid) (try (Long/parseLong oid)
                                                     (catch Exception _ nil))
                                  :else nil)]
                        (or (get definitions oid) :__null__)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; NOW() → current timestamp as java.util.Date
      (= fname "now")
      (let [fn-param (symbol (str "?now-fn" (swap! (:var-counter ctx) inc)))
            now-fn (fn [] (or params/*statement-time* (java.util.Date.)))]
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
                      "default_transaction_isolation" "read committed"
                      ;; asyncpg probes `jit` during type introspection —
                      ;; `SELECT current_setting('jit'), set_config('jit',
                      ;; 'off', false)` — and a 42704 there breaks its
                      ;; codec pipeline, not just the probe.
                      ;; DELIBERATE DIVERGENCE: PostgreSQL defaults this
                      ;; to `on`; we have no JIT at all, so `off` is the
                      ;; truthful answer. A client that reads this to
                      ;; decide whether to apply a JIT workaround gets
                      ;; the right answer from us, not a copied default.
                      "jit"                        "off"
                      ;; PostgreSQL's default. Above 0 it means "print
                      ;; shortest round-trip", which is what our float
                      ;; renderer does -- so reporting the real default
                      ;; is truthful, and SHOW returning empty was not.
                      "extra_float_digits"         "1"}
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

      ;; set_config(name, value, is_local) — assign a run-time parameter
      ;; and RETURN the new value as text.
      ;;
      ;; asyncpg turns `jit` off around its type-introspection query and
      ;; reads the result back, so this is on the path of every codec
      ;; lookup rather than a corner. It used to pass through as an
      ;; unresolved datalog symbol; once unknown functions started
      ;; raising 42883 that became a hard failure during PREPARE, which
      ;; asyncpg does not recover from.
      ;;
      ;; The value is accepted and echoed rather than stored: none of the
      ;; parameters a client sets this way changes how we execute, and
      ;; answering with the value the client just supplied is what makes
      ;; its read-back consistent.
      (= fname "set_config")
      (let [fn-param (symbol (str "?set-config" (swap! (:var-counter ctx) inc)))
            impl-fn (fn [_name value & _]
                      (if (or (nil? value) (= :__null__ value))
                        :__null__
                        (str value)))]
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
      ;; abs on a declared integer column is width-checked:
      ;; abs(INT_MIN) is out of range in PostgreSQL, while Java's abs
      ;; WRAPS and returns the negative input unchanged -- an absolute
      ;; value that is negative.
      (and (= fname "abs")
           (= 1 (count args))
           (int-width-of ctx (first params)))
      (let [w (int-width-of ctx (first params))
            fn-param (symbol (str "?abs-i" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [v] (fns/sql-int-abs w v)))
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; VOLATILE zero-argument functions. A datalog function binding with
      ;; no inputs is evaluated ONCE for the whole query, so `SELECT
      ;; random() FROM t` handed every row the same draw where PostgreSQL
      ;; draws per row (provolatile = 'v'). Feeding the entity var in as
      ;; an ignored argument is what makes the binding row-varying.
      ;;
      ;; With no FROM there is no entity var and nothing to vary over --
      ;; and a single row is exactly one draw, so folding is right there.
      (and (contains? #{"random" "random_normal"} fname)
           (:default-table ctx))
      (let [fn-param (symbol (str "?vol-" fname (swap! (:var-counter ctx) inc)))
            impl (get fns/sql-fn->clj-fn fname)
            evar (ctx/entity-var! ctx (:default-table ctx))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [_row & more] (apply impl (or more nil))))
        (if (:aggregate-projection? ctx)
          ;; A volatile scalar beside an aggregate belongs above the
          ;; grouping step: PostgreSQL evaluates it once per output group,
          ;; not once per input row.  Returning the form directly lets
          ;; split-aggregate-projection retain it in the deferred projection.
          ;; The wrapper ignores its first argument, so nil is the appropriate
          ;; synthetic row token when no input entity survives grouping.
          (apply list fn-param nil args)
          (do
            (swap! (:where-clauses ctx) conj
                   [(apply list fn-param evar args) result-var])
            result-var)))

      (= fname "pg_typeof")
      (let [arg-expr (first params)
            oid-env (oid-env ctx)
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
            ;; NULL in, NULL out -- cardinality of an unknown array is not 0.
            impl-fn (fn [arr]
                      (if-let [a (coerce-pg-array arr)]
                        (count (pg-arr/flat-elements a))
                        :__null__))]
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
            ;; array_append is NOT strict in its array argument: PostgreSQL
            ;; treats a NULL array as empty, so `array_append(NULL, 9)` is
            ;; `{9}` rather than NULL (array_userfuncs.c).
            impl-fn (fn [arr v]
                      (if-let [a (or (coerce-pg-array arr)
                                     (when (or (nil? arr) (= :__null__ arr))
                                       (pg-arr/array :unknown [])))]
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
            ;; Not strict in the array argument either -- see array_append.
            impl-fn (fn [v arr]
                      (if-let [a (or (coerce-pg-array arr)
                                     (when (or (nil? arr) (= :__null__ arr))
                                       (pg-arr/array :unknown [])))]
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
      (let [param-vars (vec (ctx/collect-vars args))
            _ (ctx/make-columns-optional! ctx param-vars)
            fn-param (symbol (str "?coalesce-fn" (swap! (:var-counter ctx) inc)))
            coalesce-fn (let [forms args pv param-vars]
                          (fn [& vals]
                            (let [bindings (zipmap pv vals)]
                              (reduce (fn [_ form]
                                        (let [value (interpret-form form bindings)]
                                          (if (or (nil? value) (= :__null__ value))
                                            :__null__
                                            (reduced value))))
                                      :__null__ forms))))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj coalesce-fn)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param param-vars) result-var])
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
      ;; Not `str`: a temporal argument renders as java.util.Date.toString
      ;; that way. Same reason as `||` — see types/temporal->pg-text.
      (let [oids (mapv #(source-oid ctx %) params)
            fn-param (symbol (str "?pg-concat-fn" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj
               (fn [& vs] (apply str (map types/->pg-text vs (concat oids (repeat nil))))))
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      ;; CONCAT_WS(separator, value, ...) skips NULL values but a NULL
      ;; separator makes the whole result NULL. It is variadic and non-strict
      ;; in every argument after the separator.
      (= fname "concat_ws")
      (let [oids (mapv #(source-oid ctx %) params)
            fn-param (symbol (str "?pg-concat-ws-fn" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj
               (fn [sep & vs]
                 (if (fns/sql-null? sep)
                   :__null__
                   (let [rendered (keep-indexed
                                   (fn [i v]
                                     (when-not (fns/sql-null? v)
                                       (types/->pg-text v (nth oids (inc i) nil))))
                                   vs)]
                     (str/join (str sep) rendered)))))
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      ;; SUBSTR/SUBSTRING(s, start [, len])
      ;; Through sql-substring rather than a raw `subs`: `subs` throws on
      ;; the `:__null__` sentinel ("class Keyword cannot be cast to
      ;; String") and on any offset outside the string, where PostgreSQL
      ;; clamps and returns as much as overlaps.
      (or (= fname "substr") (= fname "substring"))
      (let [[s start len] args]
        (swap! (:where-clauses ctx) conj
               [(if len
                  (list 'datahike.pg.sql/sql-substring s start len)
                  (list 'datahike.pg.sql/sql-substring s start))
                result-var])
        result-var)

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
                       ;; NULL out, not nil: a datalog binding that yields nil
                       ;; FILTERS THE ROW, so `date_trunc('month', d)` dropped
                       ;; every row whose d was NULL.
                       (if (or (fns/sql-null? prec) (fns/sql-null? ts))
                         :__null__
                         (let [unit (let [u (if (keyword? prec) (name prec) (str prec))]
                                      (str/replace u #"s$" ""))]
                           (cond
                             (instance? java.util.Date ts)
                             (let [zdt (.atZone (.toInstant ^java.util.Date ts)
                                                java.time.ZoneOffset/UTC)
                                   trunc (trunc-zdt unit zdt)]
                               (java.util.Date/from (.toInstant ^java.time.ZonedDateTime trunc)))

                             ;; A `date` column arrives as a LocalDate, which
                             ;; the fall-through returned UNTRUNCATED --
                             ;; `date_trunc('month', d)` answered d. PostgreSQL
                             ;; resolves a date argument to the timestamptz
                             ;; overload, so the result is an instant.
                             (instance? java.time.LocalDate ts)
                             (let [zdt (.atStartOfDay ^java.time.LocalDate ts
                                                      java.time.ZoneOffset/UTC)
                                   trunc (trunc-zdt unit zdt)]
                               (java.util.Date/from (.toInstant ^java.time.ZonedDateTime trunc)))

                             ;; A `timestamp` (no zone) stays zone-less.
                             (instance? java.time.LocalDateTime ts)
                             (let [zdt (.atZone ^java.time.LocalDateTime ts
                                                java.time.ZoneOffset/UTC)]
                               (.toLocalDateTime ^java.time.ZonedDateTime (trunc-zdt unit zdt)))

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
      (if (contains? #{types/oid-date types/oid-time types/oid-timestamp
                       types/oid-timestamptz}
                     (source-oid ctx (first params)))
        ;; PostgreSQL 18's date_add(timestamptz, interval [, zone]) is a
        ;; different signature from the legacy three-number compatibility
        ;; helper below. Until interval has a structural carrier, reject it
        ;; at the SQL boundary instead of adding strings or nils.
        (throw (errors/pg-error :feature-not-supported
                                {:feature "date_add with an interval"}))
        (let [[_unit amount ts] args]
          (swap! (:where-clauses ctx) conj [(list '+ ts amount) result-var])
          result-var))

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

      ;; Temporal to_char has its own picture language in PostgreSQL. The
      ;; mapped implementation is deliberately numeric-only.
      (and (= fname "to_char")
           (contains? #{types/oid-date types/oid-time types/oid-timestamp
                        types/oid-timestamptz types/oid-interval}
                      (source-oid ctx (first params))))
      (throw (errors/pg-error :feature-not-supported
                              {:feature "to_char with a temporal value"}))

      ;; User-defined enum input is database metadata, so the pure scalar
      ;; helper cannot validate it without this translation-time closure.
      (contains? #{"enum_first" "enum_last" "enum_range"} fname)
      (let [argc (count args)
            valid-arity? (if (= fname "enum_range") (contains? #{1 2} argc) (= 1 argc))
            _ (when-not valid-arity?
                (throw (errors/pg-error :undefined-function
                                        {:function fname :arity argc})))
            spec (enum-spec-for-exprs ctx arg-exprs)
            _ (when-not spec
                (throw (ex-info (str "function " fname " does not exist")
                                {:error :undefined-function :sqlstate "42883"})))
            values (:values spec)
            safe-value (fn [value]
                         (when (some? value)
                           (params/assert-enum-label-safe!
                            (:db ctx) (:name spec) value)))
            impl (case fname
                   "enum_first" (fn [_] (safe-value (first values)))
                   "enum_last" (fn [_] (safe-value (last values)))
                   "enum_range"
                   (fn
                     ([_]
                      (doseq [value values] (safe-value value))
                      (pg-arr/array :text values))
                     ([lo hi]
                      (let [lo (when-not (fns/sql-null? lo) (str lo))
                            hi (when-not (fns/sql-null? hi) (str hi))
                            start (if lo (.indexOf ^java.util.List values lo) 0)
                            end (if hi (.indexOf ^java.util.List values hi) (dec (count values)))
                            selected (if (and (<= 0 start) (<= start end))
                                       (subvec values start (inc end))
                                       [])]
                        (doseq [value selected] (safe-value value))
                        (pg-arr/array :text selected)))))
            fn-param (symbol (str "?fn-" fname "-" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj impl)
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      (= fname "pg_input_is_valid")
      (let [_ (fns/check-arity! fname (count args))
            db (:db ctx)
            valid? (fn [value type-name]
                     (if-let [values (params/registered-enum-values db type-name)]
                       (contains? values (str value))
                       (fns/pg-input-valid? value type-name)))
            fn-param (symbol (str "?fn-pg-input-is-valid-"
                                  (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fns/null-safe valid?))
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      ;; Known mapped functions. Emit via an in-param wrapping `null-safe`
      ;; so SQL NULL propagates (UPPER(NULL)=NULL etc.) instead of throwing
      ;; when a raw Clojure fn receives the `:__null__` keyword sentinel.
      (contains? fns/sql-fn->clj-fn fname)
      (let [;; Resolve the arity here, at translate time, the way PG
            ;; resolves against pg_proc during parse/analyze: a bad
            ;; argument count is 42883, not a runtime ArityException
            ;; surfacing as XX000.
            _ (fns/check-arity! fname (count args))
            clj-fn (get fns/sql-fn->clj-fn fname)
            ;; Strictness is the rule, not a law -- see fns/non-strict-fns.
            spec (get fns/sql-function-specs fname)
            wrapped (if (or (contains? fns/non-strict-fns fname)
                            (= false (:strict? spec)))
                      clj-fn
                      (fns/null-safe clj-fn))
            fn-param (symbol (str "?fn-" fname "-"
                                  (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj wrapped)
        (swap! (:where-clauses ctx) conj
               [(apply list fn-param args) result-var])
        result-var)

      ;; jsonb_build_object(k1, v1, k2, v2, ...) → in-param fn
      (= fname "json_build_object")
      (let [fn-param (symbol (str "?json-build-obj" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/json-build-object)
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      (= fname "json_build_array")
      (let [fn-param (symbol (str "?json-build-arr" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/json-build-array)
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

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
      (contains? #{"jsonb_strip_nulls" "json_strip_nulls"} fname)
      (let [fn-param (symbol (str "?jsonb-strip-nulls" (swap! (:var-counter ctx) inc)))
            ;; Same transformation, different RENDERING: a json result is
            ;; compact where a jsonb one is spaced. Returning text here
            ;; rather than a Clojure value is what lets the family decide,
            ;; since the output path cannot tell them apart.
            json? (= fname "json_strip_nulls")]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj
               (if json?
                 (comp jb/serialize-json jb/jsonb-strip-nulls)
                 jb/jsonb-strip-nulls))
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; jsonb_typeof(jsonb) → string type name
      (contains? #{"jsonb_typeof" "json_typeof"} fname)
      (let [fn-param (symbol (str "?jsonb-typeof" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-typeof)
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; jsonb_array_length(jsonb) → integer
      (contains? #{"jsonb_array_length" "json_array_length"} fname)
      (let [fn-param (symbol (str "?jsonb-arr-len" (swap! (:var-counter ctx) inc)))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj jb/jsonb-array-length)
        (swap! (:where-clauses ctx) conj [(list fn-param (first args)) result-var])
        result-var)

      ;; to_jsonb(any) / to_json(any) / row_to_json(record) →
      ;; parse/pass-through. `row_to_json` is `to_json` restricted to a
      ;; composite; with whole-row references producing a PgRecord it
      ;; needs no separate implementation, and it is the call PostgREST
      ;; puts on every read.
      (contains? #{"to_jsonb" "to_json" "row_to_json"} fname)
      (let [json-family? (contains? #{"to_json" "row_to_json"} fname)
            fn-param (symbol (str "?to-jsonb" (swap! (:var-counter ctx) inc)))
            ;; to_jsonb does NOT parse its argument: a text value becomes
            ;; a json STRING. Only an argument that already IS json/jsonb
            ;; passes through, and at runtime both are Clojure strings —
            ;; so the column type decides, as it does for `||` and `-`.
            already-json? (jsonb-column? ctx (first params))]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj
               (cond
                 ;; row_to_json is declared `row_to_json(record)`, so
                 ;; PostgreSQL rejects a non-composite argument at
                 ;; lookup: `row_to_json(1)` is
                 ;; `function row_to_json(integer) does not exist`.
                 ;; to_json takes `anyelement` and accepts it.
                 (= fname "row_to_json")
                 (fn [v]
                   (when-not (or (nil? v) (= :__null__ v) (pg-rec/record? v))
                     (throw (errors/pg-error
                             :undefined-function
                             {:function (str "row_to_json("
                                             (get types/oid->pg-name
                                                  (types/infer-oid-from-value v)
                                                  "unknown")
                                             ")")
                              :hint (str "No function matches the given name "
                                         "and argument types. You might need "
                                         "to add explicit type casts.")})))
                   (jb/to-json v already-json?))

                 json-family? #(jb/to-json % already-json?)
                 :else        #(jb/to-jsonb % already-json?)))
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
      (contains? #{"jsonb_extract_path" "json_extract_path"} fname)
      (let [target (first args)
            path (rest args)
            fn-param (symbol (str "?jsonb-path" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)
            json-family? (= fname "json_extract_path")]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj
               (fn [value steps]
                 (let [result (jb/jsonb-get-path value steps)]
                   (if (= :__null__ result)
                     :__null__
                     ((if json-family? jb/serialize-json jb/serialize-jsonb)
                      result)))))
        (swap! (:where-clauses ctx) conj [(list fn-param target (vec path)) result-var])
        result-var)

      ;; jsonb_extract_path_text(target, key1, key2, ...) → text
      ;; Same as jsonb_extract_path but returns text
      (contains? #{"jsonb_extract_path_text" "json_extract_path_text"} fname)
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
      (let [fn-param (symbol (str "?jsonb-insert" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        ;; The optional `insert_after` flag has to reach jsonb-insert —
        ;; dropping it silently turned every call into insert-before.
        (swap! (:in-args ctx) conj
               (fn ([t p v] (jb/serialize-jsonb (jb/jsonb-insert t p v)))
                 ([t p v a] (jb/serialize-jsonb (jb/jsonb-insert t p v a)))))
        (swap! (:where-clauses ctx) conj [(apply list fn-param args) result-var])
        result-var)

      ;; jsonb_object_keys(jsonb) → returns set of keys; serialized as JSON array string
      (contains? #{"jsonb_object_keys" "json_object_keys"} fname)
      (let [target (first args)
            fn-param (symbol (str "?jsonb-keys" (swap! (:var-counter ctx) inc)))
            result-var (ctx/fresh-var! ctx)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj (fn [v] (jb/serialize-jsonb (vec (jb/jsonb-object-keys v)))))
        (swap! (:where-clauses ctx) conj [(list fn-param target) result-var])
        result-var)

      ;; Set-returning functions — when used in SELECT, serialize result
      ;; jsonb_object_agg(key, value) — builds jsonb object from key-value pairs
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

      ;; Unknown function.
      ;;
      ;; This used to emit a datalog clause naming the symbol and let
      ;; execution fail, so a client got `Unknown function
      ;; 'json_build_object in [(json_build_object "a" 1) ?v1]` — our
      ;; internals, under XX000. PostgreSQL rejects an unresolvable
      ;; function at parse time with 42883.
      ;;
      ;; Names datalog CAN resolve are still passed through: that is how
      ;; a caller reaches a Clojure fn we did not enumerate, and turning
      ;; those into errors would remove working behaviour.
      :else
      (if (resolve (symbol fname))
        (do (swap! (:where-clauses ctx) conj
                   [(apply list (symbol fname) args) result-var])
            result-var)
        (throw (ex-info (str "function " fname " does not exist")
                        {:error :undefined-function
                         :sqlstate "42883"
                         :function fname}))))))

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
        ;; Kleene, not Clojure truthiness: the `:__null__` sentinel is
        ;; TRUTHY, so `every?`/`some?`/`not` all read UNKNOWN as TRUE.
        ;; Reduce pairwise through the same tables the datalog path uses.
        and   (reduce fns/sql-and3 true (mapv #(interpret-form % bindings) args))
        or    (reduce fns/sql-or3 false (mapv #(interpret-form % bindings) args))
        not   (fns/sql-not3 (interpret-form (first args) bindings))
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

(defn split-aggregate-projection
  "Split the Datalog clauses a select item emitted into the ones that
   belong to the QUERY and the projection FORM that has to run after it.

   An aggregate's value does not exist until the grouping step, so
   nothing computed FROM one can be a clause in the same query --
   `round(avg(x), 2)` emits `[(?pg-round ?agg ?scale) ?out]`, and `?agg`
   is a find element, not a binding. PostgreSQL draws the same line: the
   scan and the aggregate below, the projection above.

   Every SSA-style binding emitted while translating this SELECT item is
   above that line once its output feeds the final projection. Those bindings
   are inlined back into one nested form. Source-column bindings were emitted
   before this item's clause slice and therefore remain query inputs; this is
   what keeps an ordinary grouped column in the same expression
   (`sum(x) + id * 2`) available without making the derived scalar a new
   grouping key.

   Returns [query-clauses projection-form]."
  [v clauses agg-vars]
  (let [out-of (fn [c] (when (and (vector? c) (= 2 (count c)) (seq? (first c)))
                         (second c)))
        ;; `get-else` is a source-column read, not projection work. Inlining
        ;; it would hide the source variable from the 42803 grouping check
        ;; and leave post-processing without a db/eid execution context.
        source-binding? (fn [c]
                          (and (vector? c) (seq? (first c))
                               (= 'get-else (ffirst c))))
        by-out (into {} (keep (fn [c]
                                (when (and (not (source-binding? c))
                                           (out-of c))
                                  [(out-of c) (first c)])))
                     clauses)
        expanded (volatile! (set agg-vars))
        inline (fn inline [x]
                 (cond
                   (and (symbol? x) (contains? by-out x))
                   (do (vswap! expanded conj x)
                       (inline (get by-out x)))

                   (seq? x) (apply list (map inline x))
                   :else x))
        form (inline v)
        keep-cs (vec (remove (fn [c]
                               (contains? @expanded (out-of c)))
                             clauses))]
    [keep-cs form]))

(defn translate-deferred-form
  "Translate one lazy SQL operand into an interpreted form.

   Translation normally emits SSA-style Datalog bindings eagerly. CASE and
   COALESCE must instead evaluate only the selected operand, so capture the
   bindings emitted by this operand, inline the dependency chain feeding its
   result, and retain only source-column reads needed to supply runtime vars."
  [ctx translate]
  (let [before (vec @(:where-clauses ctx))
        start (count before)
        value (translate)
        after (vec @(:where-clauses ctx))
        emitted (subvec after start)
        [retained form] (split-aggregate-projection value emitted #{})]
    (reset! (:where-clauses ctx) (into before retained))
    form))

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
                               _ (when switch-expr
                                   (check-comparison-types! ctx '= switch-expr when-val))
                               test (translate-deferred-form
                                     ctx
                                     #(if switch-val
                                        (list 'datahike.pg.sql/sql-eq? switch-val
                                              (translate-expr ctx when-val))
                                        (translate-predicate-expr ctx when-val)))
                               then (translate-deferred-form
                                     ctx #(translate-expr ctx then-val))]
                           ;; Detect unsupported: aggregate refs in CASE branches
                           (when (or (map? test) (map? then))
                             (throw (ex-info "aggregate in CASE not supported"
                                             {:error :feature-not-supported
                                              :feature "aggregate function in CASE branch"
                                              :detail "CASE expressions referencing aggregate functions (e.g. CASE WHEN COUNT(*) > 1) are not supported in Datahike SQL. Use a subquery."
                                              :expr (str case-expr)})))
                           {:test test :then then}))
                       when-clauses)
        else-val (when else-expr
                   (translate-deferred-form
                    ctx #(translate-expr ctx else-expr)))
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
                      else-v effective-else
                      ;; A branch value that evaluates to nil is SQL NULL,
                      ;; and NULL has to leave here as the `:__null__`
                      ;; sentinel: a datalog function binding that yields
                      ;; nil FILTERS THE ROW, so `CASE WHEN id=1 THEN NULL
                      ;; ELSE 2 END` returned four rows where PostgreSQL
                      ;; returns five. `effective-else` covers a MISSING
                      ;; ELSE; this covers an explicit NULL in either arm.
                      nulled  (fn [v] (if (nil? v) :__null__ v))]
                  (fn [& args]
                    (let [bindings (zipmap pv args)]
                      ;; A branch is taken only when its test is TRUE.
                      ;; UNKNOWN is not: `CASE WHEN NULL THEN 'y' ELSE 'n'`
                      ;; is 'n', and the sentinel is truthy, so a bare
                      ;; `(when (interpret-form …))` took the branch.
                      ;;
                      ;; The hit is wrapped in a vector so that "a branch
                      ;; matched and produced FALSE or NULL" stays
                      ;; distinguishable from "no branch matched".
                      ;; `(or (some …) else)` could not tell those apart, so
                      ;; `CASE WHEN true THEN false ELSE true END` answered
                      ;; true. And the ELSE is evaluated only on a miss --
                      ;; CASE short-circuits, so `CASE WHEN 1=1 THEN 1 ELSE
                      ;; 2/0 END` must never run the division.
                      (if-let [hit (reduce (fn [_ [test-form then-form]]
                                             (when (true? (interpret-form test-form bindings))
                                               (reduced [(nulled (interpret-form then-form bindings))])))
                                           nil
                                           branch-data)]
                        (first hit)
                        (nulled (interpret-form else-v bindings))))))]
    ;; Make column args optional so entities with NULLs aren't excluded
    (ctx/make-columns-optional! ctx param-vars)
    ;; Register the :in parameter and its runtime value
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj case-fn)
    ;; Add the function-call binding: [(?case-fn ?a ?b ...) ?result]
    (swap! (:where-clauses ctx) conj
           [(apply list fn-param param-vars) result-var])
    ;; PostgreSQL gives ELSE the first (most significant) position when
    ;; selecting CASE's common type.
    (coerce-to-common! ctx result-var
                       (concat (when else-expr [else-expr])
                               (mapv (fn [^WhenClause wc] (.getThenExpression wc))
                                     when-clauses))
                       "CASE")))

(defn- materialize-nested!
  "Bottom-up, bind every compound ARGUMENT of a form to its own variable.

   Datahike resolves function/predicate arguments FLAT -- only top-level
   ?var symbols are substituted -- and rejects a nested seq argument
   outright (datahike.query.analyze/check-fn-args), because it would
   otherwise reach the function as a literal, never-evaluated list. So a
   projected boolean like

     (sql-and3 (sql-eq3? ?a 10) (sql-eq3? ?b 20))

   has to be flattened into its own clauses:

     [(sql-eq3? ?a 10) ?m1] [(sql-eq3? ?b 20) ?m2] [(sql-and3 ?m1 ?m2) ?r]

   `(quote x)` is the one seq shape datahike does accept as an argument,
   so it is passed through untouched."
  [ctx form]
  (if (and (seq? form) (not= 'quote (first form)))
    (let [[op & args] form]
      (cons op (mapv (fn [a]
                       (if (and (seq? a) (not= 'quote (first a)))
                         (ctx/materialize-arg! ctx (materialize-nested! ctx a))
                         a))
                     args)))
    form))

(defn like-pattern->regex
  "Compile a SQL LIKE pattern to a java.util.regex.Pattern.

   `%` is `.*`, `_` is `.`, everything else is quoted, and `escape`
   (default `\\`) escapes the next character. Anchored at both ends,
   because LIKE matches the WHOLE string.

   Public and shared: the UPDATE SET condition evaluator needs exactly this
   compilation, and adding a third copy of it beside the two already in
   this namespace is how the LIKE branches drift apart."
  ([pattern case-insensitive?] (like-pattern->regex pattern case-insensitive? \\))
  ([pattern case-insensitive? ^Character escape]
   (let [^String pat-str (str pattern)
         re-sb (StringBuilder. "^")]
     (loop [i 0]
       (when (< i (count pat-str))
         (let [c (.charAt pat-str i)]
           (if (= c (.charValue escape))
             (if (< (inc i) (count pat-str))
               (do (.append re-sb (java.util.regex.Pattern/quote
                                   (str (.charAt pat-str (inc i)))))
                   (recur (+ i 2)))
               (recur (inc i)))
             (case c
               \% (do (.append re-sb ".*") (recur (inc i)))
               \_ (do (.append re-sb ".") (recur (inc i)))
               (do (.append re-sb (java.util.regex.Pattern/quote (str c)))
                   (recur (inc i))))))))
     (.append re-sb "$")
     (re-pattern (cond->> (str re-sb) case-insensitive? (str "(?i)"))))))

(defn- any-all-op-fn
  "Runtime `x <op> ANY(arr)` / `x <op> ALL(arr)`.

   Kleene over the elements: ALL is the AND, ANY the OR, so a NULL element
   makes the answer UNKNOWN only when no other element has already settled
   it -- `2 <> ALL(ARRAY[2,NULL])` is FALSE because an element that EQUALS
   settles it, while `2 <> ALL(ARRAY[3,NULL])` is UNKNOWN.

   One definition for both operators. The `=` and `<>` branches each had
   their own copy forty lines apart, and only the `<>` one had been made
   three-valued -- so `3 = ANY(ARRAY[1,NULL])` answered FALSE where
   PostgreSQL says NULL."
  [kind cmp]
  (fn [c a]
    (let [arr (coerce-pg-array a)]
      (if (or (fns/sql-null? c) (nil? arr))
        :__null__
        (let [cmps (map (fn [el]
                          (if (or (nil? el) (= :__null__ el))
                            :__null__
                            (cmp c el)))
                        (pg-arr/flat-elements arr))]
          (if (= kind "all")
            (reduce fns/sql-and3 true cmps)
            (reduce fns/sql-or3 false cmps)))))))

(defn empty-aggregate-row
  "SQL requires an aggregate over an EMPTY relation to still produce ONE
   row -- COUNT is 0, everything else NULL -- but only when there is no
   GROUP BY, i.e. every `:find` element is an aggregate form. With a
   grouping column in `:find`, an empty result means zero matching groups
   and PostgreSQL returns zero rows.

   Datalog returns no rows at all either way, so the row has to be
   synthesised. Returns `[row]` when the rule applies, else nil.

   Shared deliberately: exec-select applies this to the top-level result,
   and every SCALAR SUBQUERY evaluator needs the identical rule --
   `SELECT (SELECT count(*) FROM t WHERE false)` is 0, not NULL -- but each
   of them ran `d/q` directly and answered NULL."
  [query]
  (let [find-elems (:find query)]
    (when (and (seq find-elems)
               (every? (fn [elem] (and (seq? elem) (symbol? (first elem)))) find-elems))
      [(mapv (fn [elem]
               (let [agg-name (name (first elem))]
                 (if (contains? #{"count" "count-distinct"
                                  "filter-count" "filter-count-distinct"}
                                agg-name)
                   0
                   nil)))
             find-elems)])))

(defn- nested-selects-in
  "Immediate SELECT nodes reachable from `node`; SELECT bodies stay opaque so
   every child can be analyzed under its own SQL name-resolution scope."
  [node]
  (let [seen (java.util.IdentityHashMap.)]
    (letfn [(walk [n]
              (cond
                (nil? n) []
                (.containsKey seen n) []
                (instance? Select n) [n]
                (or (string? n) (number? n) (boolean? n) (keyword? n)) []
                :else
                (do
                  (.put seen n true)
                  (cond
                    (instance? java.lang.Iterable n) (mapcat walk n)
                    (.isArray (class n))
                    (when-not (.isPrimitive (.getComponentType (class n)))
                      (mapcat walk n))
                    (.startsWith (.getName (class n)) "net.sf.jsqlparser.")
                    (mapcat (fn [^java.lang.reflect.Method m]
                              (let [mn (.getName m)]
                                (if (and (zero? (count (.getParameterTypes m)))
                                         (or (.startsWith mn "get") (.startsWith mn "is"))
                                         (not= mn "getClass")
                                         (not= mn "getASTNode")
                                         (not= mn "getParent")
                                         (not= mn "getDataType"))
                                  (try (walk (.invoke m n (object-array 0)))
                                       (catch Throwable _ []))
                                  [])))
                            (.getMethods (class n)))
                    :else []))))]
      (vec (walk node)))))

(defn contains-subquery?
  "True when an expression contains a SELECT node at any nesting depth."
  [node]
  (boolean (seq (nested-selects-in node))))

(defn- local-item-alias [^FromItem item]
  (or (try (some-> item .getAlias .getName unquote-ident str/lower-case)
           (catch Throwable _ nil))
      (when (instance? Table item)
        (some-> ^Table item .getName unquote-ident str/lower-case))))

(defn- plain-select-scope-nodes [^PlainSelect ps]
  (let [joins (or (.getJoins ps) [])
        group-exprs (try
                      (some-> ps .getGroupBy .getGroupByExpressionList seq)
                      (catch Throwable _ nil))]
    (concat
     (map #(.getExpression ^SelectItem %) (or (.getSelectItems ps) []))
     [(.getWhere ps) (.getHaving ps)
      (try (.getQualify ps) (catch Throwable _ nil))]
     group-exprs
     (map #(.getExpression ^OrderByElement %) (or (.getOrderByElements ps) []))
     (mapcat #(or (.getOnExpressions ^Join %) []) joins))))

(defn correlated-subquery-refs
  "Return qualified outer-column references in `inner`, respecting every
   SELECT's FROM/JOIN alias scope, including derived and set-op branches."
  [inner outer-aliases]
  (letfn [(refs [node visible-outers]
            (cond
              (instance? ParenthesedSelect node)
              (refs (.getSelect ^ParenthesedSelect node) visible-outers)

              (instance? SetOperationList node)
              (into #{} (mapcat #(refs % visible-outers))
                    (.getSelects ^SetOperationList node))

              (instance? PlainSelect node)
              (let [^PlainSelect ps node
                    joins (or (.getJoins ps) [])
                    with-items (or (try (.getWithItemsList ps)
                                        (catch Throwable _ nil))
                                   [])
                    local-items (cons (.getFromItem ps)
                                      (map #(.getRightItem ^Join %) joins))
                    locals (into #{} (keep local-item-alias) local-items)
                    visible (set/difference visible-outers locals)
                    scope-nodes (plain-select-scope-nodes ps)
                    own-refs (into #{}
                                   (keep (fn [^Column col]
                                           (let [alias (some-> col .getTable .getName
                                                               unquote-ident str/lower-case)]
                                             (when (contains? visible alias)
                                               [alias (-> col .getColumnName
                                                          unquote-ident str/lower-case)]))))
                                   (mapcat params/ast-columns scope-nodes))
                    children (concat (mapcat nested-selects-in scope-nodes)
                                     (mapcat nested-selects-in local-items))
                    ;; WITH definitions are attached to the SELECT but are
                    ;; outside the ordinary projection/FROM scope nodes.
                    ;; They can themselves correlate to an outer query (the
                    ;; asyncpg domain-type probe does so from a recursive CTE).
                    with-refs
                    (mapcat (fn [^WithItem wi]
                              (when-let [body (try (.getParenthesedStatement wi)
                                                   (catch Throwable _ nil))]
                                (refs body visible-outers)))
                            with-items)]
                (into own-refs (concat with-refs
                                       (mapcat #(refs % visible) children))))

              ;; Expression roots (notably CASE) are not SELECT scopes, but
              ;; can both refer directly to an outer column and contain scalar
              ;; SELECTs. `ast-columns` leaves SELECT bodies opaque, so direct
              ;; CASE references are collected here without leaking through an
              ;; inner alias scope; each immediate SELECT child is then handled
              ;; by `refs` under its own FROM shadowing rules.
              :else
              (let [own-refs (into #{}
                                   (keep (fn [^Column col]
                                           (let [alias (some-> col .getTable .getName
                                                               unquote-ident str/lower-case)]
                                             (when (contains? visible-outers alias)
                                               [alias (-> col .getColumnName
                                                          unquote-ident str/lower-case)]))))
                                   (params/ast-columns node))]
                (into own-refs (mapcat #(refs % visible-outers))
                      ;; Values is itself a Select in JSqlParser. Its columns
                      ;; are handled above; recursing into the node returned by
                      ;; nested-selects-in would revisit the identical object
                      ;; forever. Other expression roots only retain proper
                      ;; child SELECTs here.
                      (remove #(identical? node %) (nested-selects-in node))))))]
    (when (seq outer-aliases)
      (not-empty (refs inner (set outer-aliases))))))

(defn- nested-subquery-body?
  "True when a SELECT body contains another SELECT below its own scope."
  [inner]
  (cond
    (instance? ParenthesedSelect inner)
    (nested-subquery-body? (.getSelect ^ParenthesedSelect inner))

    (instance? PlainSelect inner)
    (let [^PlainSelect ps inner
          joins (or (.getJoins ps) [])
          nodes (concat (plain-select-scope-nodes ps)
                        [(.getFromItem ps)]
                        (map #(.getRightItem ^Join %) joins))]
      (boolean (seq (mapcat nested-selects-in nodes))))

    :else false))

(defn eval-correlated-scalar
  "Evaluate one SQL fragment for a correlated subquery against `query-db`
   with `*from-bindings*` already bound by the caller. `subquery?` true ->
   `sql` is a full SELECT (run as-is); false -> `sql` is a bare
   scalar/predicate expression, wrapped as `SELECT (<sql>)`.

   Returns the first cell, or `:__null__` -- NEVER nil. A Datalog function
   binding that yields nil FILTERS THE ROW, so a per-row evaluator that
   returned nil for an empty subquery would silently delete outer rows
   instead of giving them SQL NULL.

   One implementation for both callers: the deferred SELECT-list evaluator
   and the per-row WHERE-position binding below."
  [parse-fn sql subquery? inner-schema query-db]
  (let [run-sql (if subquery? sql (str "SELECT (" sql ")"))]
    (strict-scalar-subquery parse-fn run-sql inner-schema query-db {})))

(defn- translate-value-comparison-operands
  "Translate comparison operands in value position after applying the
   same unknown-literal typinput resolution as the WHERE lowering path."
  [ctx left right]
  (mapv (fn [operand]
          (if (instance? net.sf.jsqlparser.expression.Expression operand)
            (translate-expr ctx operand)
            operand))
        (coerce-comparison-operands ctx left right)))

(defn translate-predicate-expr
  "Translate a SQL predicate expression into a Clojure boolean form
   suitable for use inside a cond binding. Unlike translate-predicate which
   returns Datalog where clauses, this returns a single form."
  [ctx expr]
  (cond
    ;; Kleene AND/OR, not Clojure's. `true AND NULL` is NULL, and NULL is
    ;; carried as the `:__null__` sentinel -- which Clojure's `and` sees
    ;; as truthy, so `(and X :__null__)` answered the sentinel where a
    ;; FALSE operand should have made it false, and `(or false :__null__)`
    ;; answered the sentinel where it should be NULL.
    (instance? AndExpression expr)
    (let [^AndExpression e expr]
      (list 'datahike.pg.sql/sql-and3
            (translate-predicate-expr ctx (.getLeftExpression e))
            (translate-predicate-expr ctx (.getRightExpression e))))

    (instance? OrExpression expr)
    (let [^OrExpression e expr]
      (list 'datahike.pg.sql/sql-or3
            (translate-predicate-expr ctx (.getLeftExpression e))
            (translate-predicate-expr ctx (.getRightExpression e))))

    (row-comparison-expression? expr)
    (translate-row-comparison ctx expr)

    ;; The NaN-aware comparisons, as in translate-comparison: PostgreSQL
    ;; sorts NaN above everything, and IEEE-754 answers false for every
    ;; comparison involving one.
    (instance? GreaterThan expr)
    (let [^GreaterThan e expr
          l (.getLeftExpression e)
          r (.getRightExpression e)
          _ (check-comparison-types! ctx '> l r)]
      (apply list 'datahike.pg.sql/sql-gt3? (translate-value-comparison-operands ctx l r)))

    (instance? GreaterThanEquals expr)
    (let [^GreaterThanEquals e expr
          l (.getLeftExpression e)
          r (.getRightExpression e)
          _ (check-comparison-types! ctx '>= l r)]
      (apply list 'datahike.pg.sql/sql-ge3? (translate-value-comparison-operands ctx l r)))

    (instance? MinorThan expr)
    (let [^MinorThan e expr
          l (.getLeftExpression e)
          r (.getRightExpression e)
          _ (check-comparison-types! ctx '< l r)]
      (apply list 'datahike.pg.sql/sql-lt3? (translate-value-comparison-operands ctx l r)))

    (instance? MinorThanEquals expr)
    (let [^MinorThanEquals e expr
          l (.getLeftExpression e)
          r (.getRightExpression e)
          _ (check-comparison-types! ctx '<= l r)]
      (apply list 'datahike.pg.sql/sql-le3? (translate-value-comparison-operands ctx l r)))

    (instance? EqualsTo expr)
    (let [^EqualsTo e expr
          right (.getRightExpression e)
          _ (check-comparison-types! ctx '= (.getLeftExpression e) right)
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
              op-fn (any-all-op-fn kind fns/sql-eq?)
              result-var (ctx/fresh-var! ctx)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj op-fn)
          (swap! (:where-clauses ctx) conj
                 [(list fn-param col-val arr-val) result-var])
          result-var)
        ;; jsonb `=` in VALUE position (a projection, a CASE test) has
        ;; the same scale-insensitivity as in WHERE, and the same
        ;; reason not to be `=` on the canonical text.
        (let [l (.getLeftExpression e)]
          (apply list
                 (if (or (jsonb-column? ctx l) (jsonb-column? ctx right))
                   'datahike.pg.sql/jsonb-eq?
                   'datahike.pg.sql/sql-eq3?)
                 (translate-value-comparison-operands ctx l right)))))

    (instance? NotEqualsTo expr)
    ;; `not=` compared the `:__null__` sentinel structurally, so
    ;; `SELECT a <> 10` answered TRUE for a NULL a. It is UNKNOWN.
    (let [^NotEqualsTo e expr
          l (.getLeftExpression e)
          r (.getRightExpression e)
          _ (check-comparison-types! ctx 'not= l r)
          ;; `x <> ANY(arr)` / `x <> ALL(arr)` were not recognised at all --
          ;; only the `=` forms were -- so they reached the function table as
          ;; a call to a function named "all" and raised "function all does
          ;; not exist". `<> ALL` in particular is the array spelling of
          ;; NOT IN, so this is a common idiom.
          any-arr? (and (instance? Function r)
                        (#{"any" "all"} (str/lower-case (.getName ^Function r))))]
      (if any-arr?
        (let [^Function fn-expr r
              kind (str/lower-case (.getName fn-expr))
              arr-expr (some-> (.getParameters fn-expr) (.get 0))
              col-val (translate-expr ctx l)
              arr-val (translate-expr ctx arr-expr)
              col-val (if (seq? col-val) (ctx/materialize-arg! ctx col-val) col-val)
              arr-val (if (seq? arr-val) (ctx/materialize-arg! ctx arr-val) arr-val)
              fn-param (symbol (str "?pg-ne-" kind (swap! (:var-counter ctx) inc)))
              op-fn (any-all-op-fn kind fns/sql-ne?)
              result-var (ctx/fresh-var! ctx)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj op-fn)
          (swap! (:where-clauses ctx) conj
                 [(list fn-param col-val arr-val) result-var])
          result-var)
        (apply list
               (if (or (jsonb-column? ctx l) (jsonb-column? ctx r))
                 'datahike.pg.sql/jsonb-ne?
                 'datahike.pg.sql/sql-ne3?)
               (translate-value-comparison-operands ctx l r))))

    (instance? IsNullExpression expr)
    ;; SQL NULL is carried as the `:__null__` sentinel, not nil — a
    ;; datalog function binding that yields nil filters the row, so
    ;; every NULL-producing fn returns the sentinel. `nil?` therefore
    ;; answered false for a value that IS NULL: `SELECT p->'missing' IS
    ;; NULL` said false where PostgreSQL says true.
    (let [^IsNullExpression e expr
          v (translate-expr ctx (.getLeftExpression e))]
      (if (.isNot e)
        (list 'datahike.pg.sql/sql-not-null? v)
        (list 'datahike.pg.sql/sql-null? v)))

    (instance? NotExpression expr)
    (let [^NotExpression e expr
          inner (translate-predicate-expr ctx (.getExpression e))]
      ;; Kleene NOT: `NOT NULL` is NULL, not TRUE. Clojure's `not` sees
      ;; the `:__null__` sentinel as truthy and answered false.
      ;;
      ;; Datahike also parses `(not <seq>)` inside a function-binding
      ;; clause as negation-as-failure, so `[(not (= ?a 1)) ?v]` doesn't
      ;; bind ?v -- it just filters. `sql-not3` is an ordinary function
      ;; call and so has neither problem, but nested seq args still have
      ;; to be materialised.
      (list 'datahike.pg.sql/sql-not3
            (if (seq? inner) (ctx/materialize-arg! ctx inner) inner)))

    ;; col [NOT] IN (literal-list-or-subquery) used inside CASE WHEN /
    ;; nested AND-OR. Lower to a single contains?/or-join form rather
    ;; than the WHERE-clause vector form translate-predicate emits.
    (instance? InExpression expr)
    (let [^InExpression e expr
          not-in? (.isNot e)
          left-ast (.getLeftExpression e)
          left-asts (in-left-asts left-ast)
          row-in? (> (count left-asts) 1)
          left-oids (mapv #(operand-type-oid ctx %) left-asts)
          left-values (mapv #(translate-expr ctx %) left-asts)
          col (first left-values)
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          right (.getRightExpression e)
          inner (when (instance? ParenthesedSelect right)
                  (.getSelect ^ParenthesedSelect right))
          corr-refs (when inner
                      (correlated-subquery-refs inner (outer-alias-set ctx)))
          _ (when inner
              (analyze-in-subquery! ctx left-asts inner corr-refs))
          subquery-values-var (when (and inner (not row-in?) (not (seq corr-refs)))
                                (uncorrelated-in-values-var! ctx inner))
          subquery-rows-var (when (and inner row-in? (not (seq corr-refs)))
                              (uncorrelated-in-rows-var! ctx inner left-oids))
          _ (when (and row-in? (not inner))
              (throw (errors/pg-error
                      :feature-not-supported
                      {:message "row-valued IN with a literal row list is not implemented"})))
          vals (cond
                 (instance? ParenthesedExpressionList right)
                 (mapv (fn [v]
                         (check-comparison-types! ctx '= left-ast v)
                         (translate-expr ctx v))
                       ^ParenthesedExpressionList right)
                 (instance? ExpressionList right)
                 (mapv (fn [v]
                         (check-comparison-types! ctx '= left-ast v)
                         (translate-expr ctx v))
                       ^ExpressionList right)
                 ;; Uncorrelated subqueries are safe to execute once while
                 ;; translating. Correlated ones become a per-row binding;
                 ;; a failed translation must never masquerade as an empty RHS.
                 (instance? ParenthesedSelect right)
                 nil
                 :else
                 (throw (ex-info "IN form unsupported in predicate-expr context"
                                 {:error :feature-not-supported
                                  :feature (str "IN expression form: " (.getName ^Class (type right)))
                                  :expr (str right)})))
          non-null-vals (filterv some? vals)
          static-null? (not= (count vals) (count non-null-vals))
          has-param? (some symbol? non-null-vals)
          set-form (cond
                     subquery-values-var subquery-values-var
                     has-param?
                     ;; Parameter-laden lists — runtime set construction
                     ;; via in-param fn so each Bind sees the current
                     ;; values. (Same trick we use for IN in WHERE.)
                     (let [fn-param (symbol (str "?in-set" (swap! (:var-counter ctx) inc)))
                           build (fn [& xs]
                                   (cond-> (set xs)
                                     static-null? (conj :__null__)))]
                       (swap! (:in-params ctx) conj fn-param)
                       (swap! (:in-args ctx) conj build)
                       (let [out-var (ctx/fresh-var! ctx)]
                         (swap! (:where-clauses ctx) conj
                                [(apply list fn-param non-null-vals) out-var])
                         out-var))
                     ;; Keep a NULL marker in the set. `x IN (1, NULL)`
                     ;; is UNKNOWN rather than FALSE when x is not 1 --
                     ;; x might have equalled the unknown element -- so
                     ;; sql-in3? has to be able to see that one was there.
                     :else
                     (cond-> (set non-null-vals)
                       static-null? (conj :__null__)))
          base (when-not row-in?
                 (list 'datahike.pg.sql/sql-in3? set-form col))]
      (if (seq corr-refs)
        (if row-in?
          (correlated-row-in-var! ctx left-values left-oids inner corr-refs not-in?)
          (correlated-in-var! ctx col inner corr-refs not-in?))
        (if row-in?
          (row-in-result-var! ctx subquery-rows-var left-values not-in?)
          (if not-in?
            (list 'datahike.pg.sql/sql-not3 (ctx/materialize-arg! ctx base))
            base))))

    ;; col [NOT] LIKE 'pat' inside CASE WHEN. Reuse the LIKE→regex
    ;; compile from translate-predicate (precomputed Pattern literal).
    (instance? LikeExpression expr)
    (let [^LikeExpression e expr
          not-like? (.isNot e)
          case-insensitive? (.isCaseInsensitive e)
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          pattern (translate-expr ctx (.getRightExpression e))
          ^Character esc (or (when-let [c (.getEscape e)]
                               (when-not (str/blank? (str c)) (Character/valueOf (char (first (str c))))))
                             (Character/valueOf \\))
          re-obj (like-pattern->regex pattern case-insensitive? esc)
          base (list 'datahike.pg.sql/sql-like3? col re-obj)]
      (if not-like? (list 'datahike.pg.sql/sql-not3 base) base))

    ;; col [NOT] BETWEEN lo AND hi inside CASE WHEN.
    (instance? Between expr)
    (let [^Between e expr
          not-between? (.isNot e)
          left-ast (.getLeftExpression e)
          lo-ast (.getBetweenExpressionStart e)
          hi-ast (.getBetweenExpressionEnd e)
          _ (check-comparison-types! ctx '>= left-ast lo-ast)
          _ (check-comparison-types! ctx '<= left-ast hi-ast)
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)
          lo  (translate-expr ctx lo-ast)
          hi  (translate-expr ctx hi-ast)
          base (list 'datahike.pg.sql/sql-between3? col lo hi)]
      (if not-between? (list 'datahike.pg.sql/sql-not3 base) base))

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

    ;; col IS [NOT] UNKNOWN. For a boolean, UNKNOWN is exactly NULL --
    ;; and the test itself is 2-valued, never UNKNOWN.
    (instance? IsUnknownExpression expr)
    (let [^IsUnknownExpression e expr
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)]
      (if (.isNot e)
        (list 'datahike.pg.sql/sql-not-null? col)
        (list 'datahike.pg.sql/sql-null? col)))

    ;; a IS [NOT] DISTINCT FROM b -- the NULL-aware `<>`, also 2-valued.
    (instance? IsDistinctExpression expr)
    (let [^IsDistinctExpression e expr
          _ (check-comparison-types! ctx '= (.getLeftExpression e) (.getRightExpression e))
          l (translate-expr ctx (.getLeftExpression e))
          l (if (seq? l) (ctx/materialize-arg! ctx l) l)
          r (translate-expr ctx (.getRightExpression e))
          r (if (seq? r) (ctx/materialize-arg! ctx r) r)
          base (list 'datahike.pg.sql/sql-distinct? l r)]
      (if (.isNot e) (list 'not base) base))

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
          base (list 'datahike.pg.sql/sql-like3? col re-obj)]
      (if negate? (list 'datahike.pg.sql/sql-not3 base) base))

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
        ;; PostgreSQL accepts verbose timestamps with POSIX-style GMT zone
        ;; names. In that notation GMT+05 means five hours WEST of UTC (the
        ;; sign is intentionally opposite an ISO offset).
        [_ verbose-local posix-sign zone-hour zone-minute]
        (re-matches #"(?i)^[a-z]+,\s+(.+)\s+GMT([+-])(\d{2}):(\d{2})$" trimmed)
        ;; Strip trailing timezone offset from date-only strings:
        ;; "2000-09-07 -07" → "2000-09-07", "2000-09-07 +00" → "2000-09-07"
        date-only (second (re-find #"^(\d{4}-\d{2}-\d{2})\s+[+-]\d{2}$" trimmed))
        ;; Normalize timestamp formats to ISO-8601
        normalized (-> trimmed
                       (str/replace #"(\d{4}-\d{2}-\d{2})\s+(\d)" "$1T$2")
                       (str/replace #"\+(\d{2})$" "+$1:00")
                       (str/replace #"(?<=\d)-(\d{2})$" "-$1:00"))]
    (or
     (when verbose-local
       (try
         (let [local (java.time.LocalDateTime/parse
                      verbose-local
                      (java.time.format.DateTimeFormatter/ofPattern
                       "MMMM d, uuuu h:mm:ss.SS a"
                       java.util.Locale/ENGLISH))
               iso-sign (if (= posix-sign "+") "-" "+")
               offset (java.time.ZoneOffset/of
                       (str iso-sign zone-hour ":" zone-minute))]
           (java.util.Date/from (.toInstant local offset)))
         (catch Exception _ nil)))
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

(defn- null-preserving
  "Lift a runtime cast to SQL strictness: NULL in, NULL out -- carried as
   the `:__null__` sentinel, never as nil.

   Both halves matter, and each was wrong somewhere in this cond:

   - Returning nil is not \"NULL\": a datalog function binding that yields
     nil FILTERS THE ROW. `SELECT n::int FROM t` silently dropped every
     row whose n was NULL instead of projecting NULL for it.
   - Letting the sentinel reach the cast body is worse. The temporal
     branches only guarded with `(when v …)`, and `:__null__` is truthy,
     so they fell through to their string parser and `d::timestamp`
     emitted the literal text `:__null__` to the client."
  [f]
  (fn [v] (if (or (nil? v) (= :__null__ v)) :__null__ (f v))))

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
        _ (when-not (pgs/sql-type-exists? (:db ctx) type-str)
            (throw (errors/pg-error :undefined-object
                                    {:kind "type" :name type-str})))
        inner-raw (translate-expr ctx inner)
        ;; Type classification from centralized registry
        cast-cat (types/cast-category type-str)
        is-int? (= :integer cast-cat)
        is-float? (= :float cast-cat)
        is-numeric? (= :numeric cast-cat)
        is-money? (= :money cast-cat)
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
        is-bit? (or (= :bit cast-cat) (= :varbit cast-cat))
        is-array? (= :array cast-cat)
        enum-values (when (and (nil? cast-cat)
                               (not (contains? types/pg-name->oid type-str)))
                      (params/registered-enum-values (:db ctx) type-str))
        is-enum? (some? enum-values)
        domain-spec (params/registered-domain-spec (:db ctx) type-str)
        enum-cast (fn [v]
                    (if (or (nil? v) (= :__null__ v))
                      :__null__
                      (let [label (str v)]
                        (if (contains? enum-values label)
                          (params/assert-enum-label-safe! (:db ctx) type-str label)
                          (throw (ex-info "invalid input value for enum"
                                          {:error :invalid-text-representation
                                           :enum? true :type type-str
                                           :value label}))))))
        ;; `::json` / `::jsonb`. `cast-category` has no json branch, so
        ;; these fell through every arm and the value passed UNCHANGED —
        ;; the cast was a no-op that only set the wire OID, so
        ;; `'"abc'::jsonb` (unclosed quote) answered `"abc` where
        ;; PostgreSQL raises 22P02, and `'{"b":1,"a":2}'::jsonb` was not
        ;; canonicalised.
        json-cast (get {"json" :json "jsonb" :jsonb} type-str)
        ;; Text rendering and bit-to-number reinterpretation consult the
        ;; declared source type. Bit columns are stored as digit strings in
        ;; Datahike, so runtime class alone cannot distinguish bit B'10'
        ;; (integer 2) from text '10' (integer 10).
        src-oid (source-oid ctx inner)]
    (cond
      is-enum?
      (if (and (not (symbol? inner-raw)) (not (seq? inner-raw)))
        (enum-cast inner-raw)
        (let [fn-param (symbol (str "?cast-enum" (swap! (:var-counter ctx) inc)))
              result-var (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) inner-raw)
              inner-val (if (seq? inner-raw) (ctx/materialize-arg! ctx inner-raw) inner-raw)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj enum-cast)
          (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var])
          result-var))

      ;; Both types VALIDATE on input; only jsonb normalises after.
      domain-spec
      (if (and (not (symbol? inner-raw)) (not (seq? inner-raw)))
        (params/cast-domain-value (:db ctx) domain-spec inner-raw)
        (let [fn-param (symbol (str "?domain-cast-" (swap! (:var-counter ctx) inc)))
              result-var (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) inner-raw)
              inner-val (ctx/materialize-arg! ctx inner-raw)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj #(params/cast-domain-value (:db ctx) domain-spec %))
          (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var])
          result-var))

      json-cast
      (if (string? inner-raw)
        (do (jb/validate-json! inner-raw)
            (if (= :jsonb json-cast) (jb/serialize-jsonb inner-raw) inner-raw))
        (let [param (symbol (str "?json-cast" (swap! (:var-counter ctx) inc)))
              result (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) inner-raw)
              jsonb? (= :jsonb json-cast)]
          (swap! (:in-params ctx) conj param)
          (swap! (:in-args ctx) conj
                 (fn [v]
                   (cond
                     (or (nil? v) (= :__null__ v)) :__null__
                     (string? v) (do (jb/validate-json! v)
                                     (if jsonb? (jb/serialize-jsonb v) v))
                     :else (if jsonb? (jb/serialize-jsonb v) v))))
          (swap! (:where-clauses ctx) conj [(list param inner-raw) result])
          result))

      ;; CAST(<expr> AS bit(n) / bit varying(n)). Needed here as well as
      ;; in sql.clj's literal fast path: only a bare literal is folded
      ;; there, so `(-44)::bit(12)` (a SignedExpression) and any cast of
      ;; a column reach this site instead, and without a branch the
      ;; value passed through UNCHANGED — `(-44)::bit(12)` answered -44.
      ;; Scalar casts delegate to the one shared implementation
      ;; (datahike.pg.sql.cast). This site used to carry its own
      ;; category dispatch with no bit branch at all, so a cast that
      ;; reached HERE rather than sql.clj's literal fast path passed the
      ;; value through untouched — `(-44)::bit(12)` answered -44.
      ;;
      ;; Compile-time fold when the operand is already a value; otherwise
      ;; bind a runtime fn, since the same semantics must apply to a
      ;; column as to a literal.
      ;; Also claim a scalar cast whose OPERAND is already a bit value:
      ;; the is-int?/is-text? branches below predate PgBit and would
      ;; stringify the record ('101'::bit(3)::text) or hand it to
      ;; coerce-numeric ('101'::bit(3)::int).
      (or is-bit?
          (and (pg-bits/pg-bit? inner-raw)
               (or is-int? is-float? is-numeric? is-text?)))
      (let [cast1 #(sql-cast/cast-scalar % type-str
                                         {:explicit? true
                                          :parse-timestamp parse-timestamp-string})]
        (if (and (not (symbol? inner-raw)) (not (seq? inner-raw)))
          (cast1 inner-raw)
          (let [fn-param (symbol (str "?cast-bit" (swap! (:var-counter ctx) inc)))
                result-var (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) inner-raw)
                inner-val (if (seq? inner-raw) (ctx/materialize-arg! ctx inner-raw) inner-raw)]
            (swap! (:in-params ctx) conj fn-param)
            (swap! (:in-args ctx) conj cast1)
            (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var])
            result-var)))

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
            result-var (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) inner-raw)
            inner-val (if (seq? inner-raw) (ctx/materialize-arg! ctx inner-raw) inner-raw)]
        (swap! (:in-params ctx) conj fn-param)
        (swap! (:in-args ctx) conj cast-fn)
        (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var])
        result-var)
      :else
      (if (and (not (symbol? inner-raw)) (not (seq? inner-raw)))
      ;; Constant value — cast at translation time
        (cond
          ;; Casting NULL yields NULL for every target type. Without this
          ;; the folds below stringify nil — `(str nil)` is "" — so
          ;; `NULL::text` became the EMPTY STRING and every NULL-aware
          ;; construct downstream took the wrong branch:
          ;;   NULL::text IS NULL          -> false
          ;;   coalesce(NULL::text, 'x')   -> ''
          ;;   length(NULL::text)          -> 0
          ;; and `NULL::bool` raised `invalid input syntax for type
          ;; boolean: ""`, which is the empty string being parsed back.
          ;; The runtime (non-constant) branches below all guard with
          ;; `(when v ...)` already; only this compile-time fold did not,
          ;; and `cast-scalar` makes the same check for the same reason.
          (or (nil? inner-raw) (= :__null__ inner-raw)) inner-raw
          ;; Delegate rather than re-implement. The runtime branch below
          ;; already routes these three through cast-scalar; this fold
          ;; kept its own copy, so a NESTED cast -- which reaches here
          ;; rather than sql.clj's bare-literal fast path -- missed
          ;; everything cast-scalar knows. `2.5::numeric::int4` truncated
          ;; to 2 (PostgreSQL rounds to 3) and `99999999999::int8::int4`
          ;; passed through unchecked.
          ;;
          ;; ::numeric still keeps arbitrary precision: cast-scalar parses
          ;; via the string form so a literal's scale survives (0.001000 →
          ;; scale 6), never via double.
          (or is-int? is-float? is-numeric? is-money?)
          (sql-cast/cast-scalar inner-raw type-str
                                {:explicit? true
                                 :parse-timestamp parse-timestamp-string})
          is-text? (types/->pg-text inner-raw src-oid)
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
          is-time? (sql-cast/cast-scalar
                    inner-raw type-str
                    {:explicit? true
                     :parse-timestamp parse-timestamp-string})
          is-ts?   (parse-timestamp-string (str inner-raw))
          is-uuid? (coerce/parse-uuid inner-raw)
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
          (= type-str "regtype")
          (or (params/registered-type-oid params/*parse-db* inner-raw) inner-raw)
          :else    inner-raw)
      ;; Variable/expression — add runtime cast binding
        (let [inner-val (ctx/materialize-arg! ctx inner-raw)
              result-var (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) inner-raw)]
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
              (swap! (:in-args ctx) conj (null-preserving date-fn))
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var]))

            is-time?
            (let [fn-param (symbol (str "?cast-time" (swap! (:var-counter ctx) inc)))
                  time-fn (fn [v]
                            (sql-cast/cast-scalar
                             v type-str
                             {:explicit? true
                              :parse-timestamp parse-timestamp-string}))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj (null-preserving time-fn))
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
              (swap! (:in-args ctx) conj (null-preserving ts-fn))
              (swap! (:where-clauses ctx) conj [(list ts-fn-param inner-val) result-var]))

            is-uuid?
            (let [uuid-fn-param (symbol (str "?cast-uuid" (swap! (:var-counter ctx) inc)))
                  uuid-fn coerce/parse-uuid]
              (swap! (:in-params ctx) conj uuid-fn-param)
              (swap! (:in-args ctx) conj (null-preserving uuid-fn))
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

            (= type-str "regtype")
            (let [fn-param (symbol (str "?regtype" (swap! (:var-counter ctx) inc)))
                  db params/*parse-db*
                  lookup (fn [v]
                           (when (some? v)
                             (or (params/registered-type-oid db v) v)))]
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
            (or is-numeric? is-money?)
            (let [fn-param (symbol (str "?cast-num" (swap! (:var-counter ctx) inc)))
                  ;; Through cast-scalar, like the int/float branch below.
                  ;; Its own copy of the coercion could not see the
                  ;; `numeric(p,s)` modifier, so a typmod cast on a COLUMN
                  ;; was a no-op while the same cast on a literal (folded
                  ;; above) applied it.
                  cast-fn (fn [v]
                            (sql-cast/cast-scalar
                             v type-str
                             {:explicit? true
                              :parse-timestamp parse-timestamp-string}))]
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj (null-preserving cast-fn))
              (swap! (:where-clauses ctx) conj [(list fn-param inner-val) result-var]))

            (or is-int? is-float? is-bool?)
            (let [fn-param (symbol (str "?cast-num" (swap! (:var-counter ctx) inc)))
                  ;; Through the shared cast impl so a value whose type is
                  ;; only known at RUNTIME still gets full semantics — a
                  ;; bit string cast to int must reinterpret its bits
                  ;; (5, not the digits 101), and coerce-numeric alone
                  ;; cannot see a PgBit.
                  cast-fn (null-preserving
                           (fn [v] (sql-cast/cast-scalar
                                    (if (and (string? v)
                                             (contains? #{types/oid-bit types/oid-varbit} src-oid))
                                      (pg-bits/parse-bit-literal
                                       v (= src-oid types/oid-varbit))
                                      v)
                                    type-str
                                    {:explicit? true
                                     :parse-timestamp parse-timestamp-string})))]
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
                               ;; Same reason as the numeric branch above: a
                               ;; PgBit reaching `str` stringifies as a
                               ;; defrecord instead of its digit run.
                               :else              (sql-cast/cast-scalar
                                                   v type-str
                                                   {:explicit? true
                                                    :parse-timestamp parse-timestamp-string
                                                    :src-oid src-oid})))]
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

(defn- int-arith-width
  "The declared integer width this arithmetic node must be checked at,
   or nil when no integer width governs it.

   int2 and int4 do not exist at runtime -- every integer is a Java long
   -- so overflow can only be detected against the width the TRANSLATOR
   can see. The rule mirrors PostgreSQL's operator resolution: the
   widest integer operand wins, and any operand known to be numeric or
   floating point takes the expression out of integer arithmetic
   entirely.

   An UNTYPED operand does not veto. By the time we get here the
   plan-cache rewrite has turned literals into `$N` placeholders, so
   `i4col + 1` presents one typed column and one unknown -- and refusing
   to type that would give up on the single most common shape there is.
   PostgreSQL would coerce the literal to the operator's type, which is
   what taking the known side amounts to. It is safe because the runtime
   op re-checks: a value that turns out NOT to be integral falls through
   to the generic operator, so `i4col + 1.5` still promotes to numeric."
  [ctx ^net.sf.jsqlparser.expression.BinaryExpression expr]
  (let [env (oid-env ctx)
        oid (fn [e] (try (oid-infer/expr-oid e env) (catch Throwable _ nil)))
        oids [(oid (.getLeftExpression expr)) (oid (.getRightExpression expr))]
        known (keep #(when % (get types/oid->integer-width %)) oids)
        non-int? (some (fn [o]
                         (and o (nil? (get types/oid->integer-width o))))
                       oids)
        ;; An unknown operand counts as int4 -- PostgreSQL's type for a
        ;; bare integer literal, which is what an unknown almost always
        ;; is here. This is not a detail: `smallint + 1` is int24pl and
        ;; yields INT4 in PostgreSQL, so taking the column's int2 width
        ;; would reject 32767 + 1, which is perfectly legal.
        ;;
        ;; Only when SOMETHING is typed, though. Two unknowns give us no
        ;; anchor at all, and guessing int4 there would report "integer
        ;; out of range" for `9223372036854775807 + 1`, whose operands
        ;; are int8.
        widths (if (seq known)
                 (concat known (repeat (count (filter nil? oids)) :int4))
                 nil)]
    (when (and (seq widths) (not non-int?))
      (let [rank {:int2 2 :int4 4 :int8 8}]
        (reduce (fn [a b] (if (> (rank b) (rank a)) b a)) widths)))))

(defn- float4-arith-op
  "The float4 runtime op for `op-sym`, or nil.

   PostgreSQL declares `float4 op float4 -> float4` and nothing mixed:
   there is no float4-with-int4 operator, so `real * 2` resolves to
   float8. Both operands must therefore be float4 for this to apply --
   which is also why an untyped operand disqualifies it, unlike the
   integer case where an unknown is PostgreSQL's int4 literal."
  [ctx ^net.sf.jsqlparser.expression.BinaryExpression expr op-sym]
  (let [env (oid-env ctx)
        oid (fn [e] (try (oid-infer/expr-oid e env) (catch Throwable _ nil)))
        left (.getLeftExpression expr)
        right (.getRightExpression expr)
        l0 (oid left)
        r0 (oid right)
        ;; A quoted literal is `unknown`, not text, during PostgreSQL
        ;; operator lookup. Against float4 it therefore selects the
        ;; float4/float4 operator; an actually typed integer literal still
        ;; widens the expression to float8 as documented above.
        l (if (instance? StringValue left) r0 l0)
        r (if (instance? StringValue right) l0 r0)]
    (when (and (= l types/oid-float4) (= r types/oid-float4))
      (get '{+ datahike.pg.sql/sql-f4+
             - datahike.pg.sql/sql-f4-
             * datahike.pg.sql/sql-f4*
             / datahike.pg.sql/sql-f4div}
           op-sym))))

(defn- int-arith-op
  "The width-checked runtime op for `op-sym`, or nil to use the generic
   one."
  [ctx expr op-sym]
  (when-let [w (int-arith-width ctx expr)]
    (when-let [sym (get '{+ datahike.pg.sql/sql-int+
                          - datahike.pg.sql/sql-int-
                          * datahike.pg.sql/sql-int*
                          / datahike.pg.sql/sql-int-div}
                        op-sym)]
      [sym w])))

(defn- date-arith-op
  "The temporal-arithmetic fn to emit for `expr`, or nil to use the numeric one.

   `date + integer`, `date - integer` and `date - date` are separate
   operators in PostgreSQL (date_pli / date_mii / date_mi). The choice
   has to be made HERE, from the declared column type, because Datahike
   stores `date` and `timestamp` columns alike as java.util.Date — a
   runtime type test cannot tell them apart, and PostgreSQL answers the
   two differently (`timestamp - timestamp` is an interval, `date - date`
   is a count of days). Typing it statically also keeps a timestamp
   column raising rather than quietly acquiring date semantics.

   Only the DATE operand is inspected. The other one deliberately is not:
   by the time we get here the planner has rewritten literals to `$N`
   placeholders for the plan cache, so `d + 1` presents an untyped
   parameter and no amount of static inference will recover the `1`.
   That costs nothing, because PostgreSQL gives `date` no other
   right-hand operator to be confused with, and sql-date- dispatches
   days-vs-difference on the runtime value anyway."
  [ctx ^net.sf.jsqlparser.expression.BinaryExpression expr op-sym]
  (let [env (oid-env ctx)
        oid (fn [e] (try (oid-infer/expr-oid e env) (catch Throwable _ nil)))
        l (.getLeftExpression expr)
        r (.getRightExpression expr)
        loid (oid l)
        roid (oid r)
        date? #(= % types/oid-date)
        integer? #(contains? #{types/oid-int2 types/oid-int4 types/oid-int8} %)
        timestamp? #(contains? #{types/oid-timestamp types/oid-timestamptz} %)
        temporal? #(contains? #{types/oid-date types/oid-time types/oid-timestamp
                                types/oid-timestamptz types/oid-interval} %)]
    (cond
      (and (= op-sym '-) (date? loid) (date? roid))
      'datahike.pg.sql/sql-date-
        ;; An unknown right operand is normally a plan-cache parameter for
        ;; an integer literal. Preserve that established date +/- path.
      (and (contains? #{'+ '-} op-sym) (date? loid)
           (or (nil? roid) (integer? roid)))
      (if (= op-sym '+) 'datahike.pg.sql/sql-date+ 'datahike.pg.sql/sql-date-)
        ;; `integer + date` commutes. `integer - date` is not an operator
        ;; in PostgreSQL, so only `+` picks the right-hand date up.
      (and (= op-sym '+) (date? roid) (or (nil? loid) (integer? loid)))
      'datahike.pg.sql/sql-date+
      (and (= op-sym '-) (timestamp? loid) (timestamp? roid))
      'datahike.pg.sql/sql-timestamp-
      (and (= op-sym '-) (= types/oid-time loid) (= types/oid-time roid))
      'datahike.pg.sql/sql-time-
        ;; Any other typed temporal combination must not reach numeric +/-,
        ;; whose Number casts leak a JVM implementation error.
      (or (temporal? loid) (temporal? roid))
      'datahike.pg.sql/sql-unsupported-temporal-arithmetic
      :else nil)))

(defn- agg-marker?
  "An aggregate placeholder produced by translate-expr, as distinct from
   any other map-like value."
  [x]
  (and (map? x) (or (:aggregate x) (:compound-agg x))))

(defn- coerce-arithmetic-unknown
  "Resolve a quoted unknown operand from the other arithmetic operand's
   numeric OID. The column-specific path keeps index/type metadata behavior;
   the OID fallback covers casts and function expressions such as
   `'Infinity'::numeric / '0'`."
  [ctx typed-expr unknown-expr]
  (or (when (instance? Column typed-expr)
        (coerce-unknown-literal ctx typed-expr unknown-expr))
      (when (and (instance? StringValue unknown-expr)
                 (not (pg-bits/bit-string-literal? unknown-expr)))
        (when-let [target (numeric-target-for-oid (source-oid ctx typed-expr))]
          (coerce/coerce-numeric (string-value-text unknown-expr) target)))))

(defn- money-arith-op
  "Select PostgreSQL's fixed-width money operator from operand OIDs.
   BigDecimal alone cannot make this decision because numeric uses the
   same Datahike carrier."
  [ctx ^net.sf.jsqlparser.expression.BinaryExpression expr op-sym]
  (let [left (.getLeftExpression expr)
        right (.getRightExpression expr)
        l0 (source-oid ctx left)
        r0 (source-oid ctx right)
        l (if (instance? StringValue left) r0 l0)
        r (if (instance? StringValue right) l0 r0)
        money? #(= types/oid-money %)
        factor? #(contains? #{types/oid-int2 types/oid-int4 types/oid-int8
                              types/oid-float4 types/oid-float8} %)]
    (cond
      (and (= op-sym '+) (money? l) (money? r)) 'datahike.pg.sql/sql-money+
      (and (= op-sym '-) (money? l) (money? r)) 'datahike.pg.sql/sql-money-
      (and (= op-sym '*)
           (or (and (money? l) (factor? r))
               (and (factor? l) (money? r)))) 'datahike.pg.sql/sql-money*
      (and (= op-sym '/) (money? l) (money? r)) 'datahike.pg.sql/sql-money-div-money
      (and (= op-sym '/) (money? l) (factor? r)) 'datahike.pg.sql/sql-money-div
      :else nil)))

(defn- check-scalar-subquery-arithmetic-types!
  "Do not let a scalar subquery's resolved output type fall back to JVM
   arithmetic. Unlike a quoted literal, `(SELECT '1')` has already crossed
   a query boundary and is TEXT, so PostgreSQL cannot coerce it to integer."
  [ctx op left right]
  (when (or (instance? ParenthesedSelect left)
            (instance? PlainSelect left)
            (instance? ParenthesedSelect right)
            (instance? PlainSelect right))
    (let [left-oid (source-oid ctx left)
          right-oid (source-oid ctx right)
          arithmetic-category? #(contains? #{:N :D :T} (get types/oid->category %))]
      (when (and left-oid right-oid
                 (not (and (arithmetic-category? left-oid)
                           (arithmetic-category? right-oid))))
        (throw (errors/pg-error
                :undefined-function
                {:detail (str "operator does not exist: "
                              (get types/oid->pg-name left-oid "?") " " op " "
                              (get types/oid->pg-name right-oid "?"))
                 :hint (str "No operator matches the given name and argument "
                            "types. You might need to add explicit type casts.")}))))))

(defn translate-binary-arith
  "Translate a binary arithmetic expression. Materializes sub-expression
   operands. When operands are aggregate markers, returns a compound-agg
   descriptor instead of a Datalog form.

   Uses fns/null-safe arithmetic so `col + 1` evaluates to `:__null__` when
   `col` is NULL, matching SQL. The compound-agg path keeps the raw
   op-sym — aggregate compound evaluation is numeric-only and runs
   server-side after the query."
  [ctx ^net.sf.jsqlparser.expression.BinaryExpression expr op-sym]
  (let [left-expr (.getLeftExpression expr)
        right-expr (.getRightExpression expr)
        _ (check-scalar-subquery-arithmetic-types! ctx op-sym left-expr right-expr)
        ;; PG puts prefix `~` at the generic-operator precedence level,
        ;; BELOW `+ - * / %` and `^`, so `~1 + 1` means `~(1 + 1)` = -3
        ;; and `~2 * 3` means `~(2 * 3)` = -7. JSqlParser binds the `~`
        ;; to its own operand instead, giving `(~1) + 1` = -1.
        ;;
        ;; Re-associating here rather than rebuilding the AST keeps it to
        ;; a choice of which operand to feed the arithmetic and what to
        ;; wrap the result in. Same-level operators (`& | << >>`) are
        ;; left-associative in both grammars — `~1 & 3` is `(~1) & 3` in
        ;; PG too — so they deliberately do NOT come through here.
        not-prefix? (and (instance? SignedExpression left-expr)
                         (= \~ (.getSign ^SignedExpression left-expr)))
        effective-left (if not-prefix?
                         (.getExpression ^SignedExpression left-expr)
                         left-expr)
        ;; A quoted literal starts as PostgreSQL's pseudo-type `unknown`.
        ;; Once the other arithmetic operand establishes a column type,
        ;; PostgreSQL runs that type's input function before invoking the
        ;; operator. Comparisons already did this through
        ;; coerce-unknown-literal, but arithmetic sent the raw String to
        ;; Clojure's +/*/... and leaked a String->Number ClassCastException.
        ;; Reuse the same typinput dispatch here for both operand orders.
        coerced-left (coerce-arithmetic-unknown ctx right-expr effective-left)
        coerced-right (coerce-arithmetic-unknown ctx effective-left right-expr)
        ;; A successful typinput result is already a Datalog-compatible
        ;; runtime value, not a JSqlParser node; do not feed it back through
        ;; translate-expr (which quite correctly rejects raw Float/Double
        ;; objects as AST expressions).
        l (if (some? coerced-left)
            coerced-left
            (translate-expr ctx effective-left))
        r (if (some? coerced-right)
            coerced-right
            (translate-expr ctx right-expr))]
    ;; `map?`, not agg-marker? -- a defrecord IS a map, so any
    ;; record-valued operand (a numeric NaN/Infinity carrier, a PgBit, a
    ;; PgArray) was mistaken for an aggregate marker and turned the whole
    ;; expression into a compound-aggregate descriptor. `'NaN'::numeric +
    ;; 1` came back as the descriptor's printed form.
    (if (or (agg-marker? l) (agg-marker? r))
      ;; Compound aggregate expression: return descriptor for SELECT handler
      {:compound-agg true :op op-sym :left l :right r :expr expr}
      (let [lv (if (seq? l) (ctx/materialize-arg! ctx l) l)
            rv (if (seq? r) (ctx/materialize-arg! ctx r) r)
            int-op (int-arith-op ctx expr op-sym)
            emit-op (or (money-arith-op ctx expr op-sym)
                        (date-arith-op ctx expr op-sym)
                        (first int-op)
                        (float4-arith-op ctx expr op-sym)
                        (get arith-op->null-safe op-sym op-sym))
            ;; The width travels as a leading constant argument rather
            ;; than as nine separate fns.
            arith (if (and int-op (= emit-op (first int-op)))
                    (list emit-op (second int-op) lv rv)
                    (list emit-op lv rv))]
        (if not-prefix?
          (list 'datahike.pg.sql/sql-bit-not (ctx/materialize-arg! ctx arith))
          arith)))))

(defn translate-binary-fn
  "Translate a binary expression into a call to `fn-sym`, constant-folding
   through `f` when both operands are already values.

   Used by the bitwise operators, whose operands may be either integers
   or bit strings — the dispatch happens inside the runtime fn rather
   than here, because the operand types generally aren't known until
   execution."
  [ctx ^net.sf.jsqlparser.expression.BinaryExpression expr fn-sym f]
  (let [left-expr (.getLeftExpression expr)
        right-expr (.getRightExpression expr)
        coerced-left (coerce-arithmetic-unknown ctx right-expr left-expr)
        coerced-right (coerce-arithmetic-unknown ctx left-expr right-expr)
        l (if (some? coerced-left)
            coerced-left
            (translate-expr ctx left-expr))
        r (if (some? coerced-right)
            coerced-right
            (translate-expr ctx right-expr))
        ;; Fold only when BOTH operands are already values. A column
        ;; reference translates to a Datalog VAR (a symbol), which is
        ;; neither a seq nor a map — folding on "not a seq" applied the
        ;; runtime fn to the symbol itself, and the resulting clause
        ;; matched nothing, so `SELECT flags & 6 FROM t` came back empty.
        value? (fn [x] (or (number? x) (pg-bits/pg-bit? x)))]
    (if (and (value? l) (value? r))
      (f l r)
      (list fn-sym
            (if (seq? l) (ctx/materialize-arg! ctx l) l)
            (if (seq? r) (ctx/materialize-arg! ctx r) r)))))

(defn- generic-bitwise-node?
  [expr]
  (or (instance? BitwiseAnd expr)
      (instance? BitwiseOr expr)
      (instance? BitwiseLeftShift expr)
      (instance? BitwiseRightShift expr)))

(defn- rebuild-generic-bitwise
  "Copy a JSqlParser generic bitwise node with new children. Avoid mutating
   the cached AST: the same parsed tree may be translated concurrently."
  [node left right]
  (cond
    (instance? BitwiseAnd node)        (BitwiseAnd. left right)
    (instance? BitwiseOr node)         (BitwiseOr. left right)
    (instance? BitwiseLeftShift node)  (BitwiseLeftShift. left right)
    (instance? BitwiseRightShift node) (BitwiseRightShift. left right)))

(defn- normalize-xor-tree
  "Give rewritten PostgreSQL `#` the generic-operator precedence.

   JSqlParser parses `a XOR b | c` as `a XOR (b | c)`. PostgreSQL parses
   `a # b | c` as `(a # b) | c`, because both operators occupy one
   left-associative level. Rotate every generic bitwise node on XOR's right
   edge, producing fresh nodes so AST-cache entries remain immutable."
  [left right]
  (if (generic-bitwise-node? right)
    (rebuild-generic-bitwise
     right
     (normalize-xor-tree left
                         (.getLeftExpression
                          ^net.sf.jsqlparser.expression.BinaryExpression right))
     (.getRightExpression
      ^net.sf.jsqlparser.expression.BinaryExpression right))
    (XorExpression. left right)))

(defn- translate-hash-xor
  [ctx ^XorExpression expr]
  (let [normalized (normalize-xor-tree (.getLeftExpression expr)
                                       (.getRightExpression expr))]
    (if (instance? XorExpression normalized)
      (translate-binary-fn ctx normalized
                           'datahike.pg.sql/sql-bit-xor fns/sql-bit-xor)
      (translate-expr ctx normalized))))

(defn flatten-json-chain
  "Flatten a JsonExpression into {:base Expression :chain [[key-expr op-str] ...]} pairs.
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
            outer-op (first ops)
            ;; Recurse: treat the inner JsonExpression as if its base were already resolved
            inner-chain (:chain (flatten-json-chain inner))]
        {:base base :chain (into [[inner-base outer-op]] inner-chain)})
      ;; Keep the AST node. The RHS of #>/#>> is an arbitrary text[]
      ;; expression (ARRAY constructors, casts, parameters, function calls),
      ;; not merely a string literal. Stringifying it here destroyed the
      ;; expression before either SELECT or UPDATE could evaluate it.
      {:base base :chain [[(first idents) (first ops)]]})))

(defn- whole-row-ref-alias
  "The table alias a bare identifier denotes as a PostgreSQL WHOLE-ROW
   reference, or nil when it is an ordinary column.

   PostgreSQL resolves a bare name as a COLUMN first and only then as a
   relation, so a table that happens to have a column of its own name
   keeps the column meaning. `ctx/relation-in-scope?` makes the same
   test for the 42703 exemption; this is the value side of it."
  [ctx expr]
  (when (instance? Column expr)
    (let [^Column col expr]
      (when (nil? (.getTable col))
        (let [nm (unquote-ident (.getColumnName col))
              aliases (:table-aliases ctx)]
          (when (or (contains? aliases nm) (= nm (:default-table ctx)))
            ;; The FULL resolution, same as the value path: the short
            ;; arity cannot search the other relations in scope, so a
            ;; column belonging to a JOINED relation looked unresolvable
            ;; and a name that also named a relation -- `FROM t,
            ;; generate_series(1,2) g`, where the SRF's single column is
            ;; called `g` too -- was read as a whole-ROW reference and
            ;; came back as a record.
            (let [attr (ctx/attr-of ctx (ctx/resolve-column
                                         col aliases (:default-table ctx)
                                         (:col-overrides ctx)
                                         (:derived-aliases ctx)
                                         (:ci-index ctx)))]
              (when-not (and attr (get (:schema ctx) attr))
                nm))))))))

(defn- translate-whole-row-ref
  "Bind a var to a PgRecord holding every column of `alias-name`'s table,
   in CREATE TABLE order.

   `db_id` is dropped: it is our surrogate for the entity, and
   PostgreSQL's whole-row value contains only the declared columns —
   including it would put an extra leading field in `(1,a)` and an extra
   key in `row_to_json`.

   Field `:name` is what makes `to_json`/`row_to_json` render
   `{\"id\":1,\"nm\":\"a\"}` — jsonb/normalize-tree reads it, falling back
   to positional `f1`, `f2` without it."
  [ctx alias-name]
  (let [table-name (get (:table-aliases ctx) alias-name alias-name)
        cols (remove #(= :db/id (:attr %))
                     (pgs/column-info (:schema ctx) table-name (:db ctx)))
        ;; `[:aliased …]` means specifically "the alias differs from the
        ;; table name"; using it unconditionally mis-resolves the plain
        ;; `FROM t` case.
        attr-ref (fn [c] (if (= alias-name table-name)
                           (:attr c)
                           [:aliased alias-name (:attr c)]))
        vars (mapv #(column-value! ctx (attr-ref %)) cols)
        meta-cols (mapv #(select-keys % [:name :oid]) cols)
        rec-fn (fn [& vals]
                 (pg-rec/->PgRecord
                  2249
                  (mapv (fn [c v]
                          {:oid (:oid c)
                           :name (:name c)
                           :value (when-not (= :__null__ v) v)})
                        meta-cols vals)))
        fn-param (symbol (str "?wholerow" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)]
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj rec-fn)
    (ctx/add-clause! ctx [(cons fn-param vars) result-var])
    result-var))

(defn- outer-alias-set
  "The FROM aliases (and bare table names) a subquery could correlate
   against, lowercased."
  [ctx]
  (into #{} (comp (map (fn [a] (some-> a name str/lower-case)))
                  (remove nil?))
        (concat (keys (or (:table-aliases ctx) {}))
                [(:default-table ctx)])))

(defn- throw-subquery-error! [p]
  (when (= :error (:type p))
    (throw (ex-info (:message p)
                    {:error :subquery-error
                     :sqlstate (or (:sqlstate p) "XX000")}))))

(defn- leaf-output-oids
  "Visible output OIDs for a translated SELECT leaf. Resolution OIDs retain
   PostgreSQL UNKNOWN where relevant; declared output OIDs fill the remaining
   analyzer gaps. Both vectors are already expanded for SELECT *."
  [p]
  (throw-subquery-error! p)
  (let [declared (vec (:select-item-oids p))
        resolution (vec (:select-item-resolution-oids p))]
    (mapv #(or (nth resolution % nil) (nth declared % nil))
          (range (count declared)))))

(defn- leaf-resolution-oids
  "Visible output OIDs while retaining PostgreSQL UNKNOWN as nil."
  [p]
  (throw-subquery-error! p)
  (let [declared (vec (:select-item-oids p))
        resolution (vec (:select-item-resolution-oids p))]
    (mapv #(nth resolution % nil) (range (count declared)))))

(defn- parsed-output-oids
  "Return a parsed SELECT/set-operation's visible output OIDs, validating
   equal branch widths while resolving set operations left-associatively."
  [p]
  (throw-subquery-error! p)
  (if (= :set-operation (:type p))
    (let [branches (mapv leaf-resolution-oids (:sub-results p))
          width (count (first branches))]
      (when-not (and (seq branches) (every? #(= width (count %)) branches))
        (throw (errors/pg-error
                :syntax-error
                {:message "each UNION query must have the same number of columns"})))
      (reduce (fn [left right]
                (mapv #(types/select-common-type [%1 %2] "UNION" true)
                      left right))
              (first branches)
              (rest branches)))
    (leaf-output-oids p)))

(defn- reject-unapplied-subquery-stages! [p]
  ;; This evaluator sits below the wire SELECT executor. Until these stages
  ;; share one execution pipeline, an explicit 0A000 is safer than consuming
  ;; plausible-looking raw Datalog rows with SQL semantics omitted.
  (when (or (seq (:project-set p))
            (seq (:window-specs p))
            (seq (:compound-exprs p))
            (seq (:correlated-subqueries p))
            (:distinct-on-n p)
            (:having p)
            (:fetch-with-ties? p)
            (:for-update p)
            (seq (:deferred-recursive-ctes p)))
    (throw (errors/pg-error
            :feature-not-supported
            {:message "this subquery requires SELECT post-processing that is not implemented"}))))

(defn- raw-in-subquery-having?
  "True when a SELECT AST contains a HAVING clause in any set-op branch.

   JSqlParser 5 omits GROUP BY and HAVING from `PlainSelect.toString` when
   the SELECT has no FROM item.  IN-subquery execution reparses that string,
   so inspecting only the reparsed plan would silently lose the clause (and
   can expose a Datahike unknown-var error for aggregate-only SELECTs).  Keep
   the unsupported-stage boundary on the original AST as well."
  [select]
  (cond
    (instance? ParenthesedSelect select)
    (raw-in-subquery-having? (.getSelect ^ParenthesedSelect select))

    (instance? SetOperationList select)
    (boolean (some raw-in-subquery-having?
                   (.getSelects ^SetOperationList select)))

    (instance? PlainSelect select)
    (some? (.getHaving ^PlainSelect select))

    :else false))

(defn- apply-subquery-order-limit
  "Apply the server-side SELECT tail used when nullable ORDER BY keys keep
   sorting out of Datalog. Rows still include any hidden sort projections."
  [rows {:keys [sql-order-by sql-offset sql-limit has-aggregates? limit offset]}]
  (let [tail-offset (or sql-offset (when has-aggregates? offset))
        tail-limit (or sql-limit (when has-aggregates? limit))
        ordered (if sql-order-by
                  (sort (fn [a b]
                          (loop [parts (partition 3 sql-order-by)]
                            (if-let [[idx dir nulls] (first parts)]
                              (let [c (fns/order-key-cmp (nth a idx nil)
                                                         (nth b idx nil)
                                                         dir nulls)]
                                (if (zero? c) (recur (rest parts)) c))
                              0)))
                        rows)
                  rows)
        offset-rows (cond->> ordered tail-offset (drop tail-offset))]
    (vec (cond->> offset-rows tail-limit (take tail-limit)))))

(defn- run-subquery-leaf
  [p db]
  (throw-subquery-error! p)
  (reject-unapplied-subquery-stages! p)
  (let [q (cond-> (:query p)
            (and (:limit p) (not (:has-aggregates? p))) (assoc :limit (:limit p))
            (and (:offset p) (not (:has-aggregates? p))) (assoc :offset (:offset p)))
        in-args (:in-args p)
        query-db (or (:enriched-db p) db)
        raw (cond
              (:literal-rows p) (:literal-rows p)
              (:literal-row p) [(:literal-row p)]
              (nil? q)
              (throw (ex-info "subquery produced no executable query"
                              {:error :internal-error :sqlstate "XX000"}))
              (seq in-args) (apply d/q q query-db in-args)
              :else (d/q q query-db))
        raw (or (when (and q (empty? (seq raw))) (empty-aggregate-row q)) raw)
        raw (apply-subquery-order-limit raw p)
        hidden (long (or (:hidden-count p) 0))]
    (if (pos? hidden)
      (mapv (fn [row]
              (let [v (if (sequential? row) (vec row) [row])]
                (subvec v 0 (- (count v) hidden))))
            raw)
      (mapv #(if (sequential? %) (vec %) [%]) raw))))

(defn- validate-parsed-in-plan! [p]
  (if (= :set-operation (:type p))
    (let [ops (:set-ops p)]
      (when (or (not (apply = ops))
                (some #{:intersect-all :except-all :unknown} ops))
        (throw (errors/pg-error
                :feature-not-supported
                {:message "mixed or ALL INTERSECT/EXCEPT operations in an IN subquery are not implemented"})))
      (doseq [branch (:sub-results p)]
        (throw-subquery-error! branch)
        (reject-unapplied-subquery-stages! branch)))
    (reject-unapplied-subquery-stages! p))
  p)

(defn- run-parsed-subquery
  [p db]
  (validate-parsed-in-plan! p)
  (if (= :set-operation (:type p))
    (let [output-oids (parsed-output-oids p)
          rows (mapv (fn [branch]
                       (mapv #(set-ops/coerce-row % output-oids)
                             (run-subquery-leaf branch (or (:enriched-db p) db))))
                     (:sub-results p))]
      (let [combined (case (:op p)
                       :union-all (vec (mapcat identity rows))
                       :union (vec (distinct (mapcat identity rows)))
                       :intersect (vec (apply set/intersection (map set rows)))
                       :except (let [[head & tail] rows]
                                 (vec (reduce set/difference (set head) (map set tail))))
                       (vec (mapcat identity rows)))
            result (apply-subquery-order-limit combined p)]
        result))
    (run-subquery-leaf p db)))

(def ^:private row-op->sym
  {:= '= :<> 'not= :< '< :<= '<= :> '> :>= '>=})

(defn- check-row-field-type! [op left-oid right-oid]
  (let [op-sym (get row-op->sym op '=)]
    (when-not (types/comparison-compatible? op-sym left-oid right-oid)
      (throw (errors/pg-error
              :undefined-function
              {:detail (str "operator does not exist: "
                            (get types/oid->pg-name left-oid "?") " "
                            (get op-sym->sql op-sym (str op-sym)) " "
                            (get types/oid->pg-name right-oid "?"))
               :hint "No operator matches the given name and argument types. You might need to add explicit type casts."})))))

(defn- check-row-runtime-field-type! [op left-oid right-oid]
  ;; comparison-compatible? deliberately treats uncategorized types leniently
  ;; for common-type selection. Operator lookup cannot: PostgreSQL provides
  ;; neither int4[] = int8[] nor enum = text. UNKNOWN remains coercible, and
  ;; equal custom OIDs still name the same operator family.
  (when (and left-oid right-oid
             (not= left-oid right-oid)
             (or (nil? (get types/oid->category left-oid))
                 (nil? (get types/oid->category right-oid))))
    (let [op-sym (get row-op->sym op '=)]
      (throw (errors/pg-error
              :undefined-function
              {:detail (str "operator does not exist: "
                            (get types/oid->pg-name left-oid "?") " "
                            (get op-sym->sql op-sym (str op-sym)) " "
                            (get types/oid->pg-name right-oid "?"))
               :hint "No operator matches the given name and argument types. You might need to add explicit type casts."}))))
  (check-row-field-type! op left-oid right-oid)
  (let [ordering? (contains? #{:< :<= :> :>=} op)
        oids (remove nil? [left-oid right-oid])
        unsupported (some (fn [oid]
                            (when (or (= oid types/oid-jsonb)
                                      ;; These use storage carriers whose JVM
                                      ;; equality is not PostgreSQL equality
                                      ;; (notably 1 day = 24 hours). Typed
                                      ;; dispatch must precede support here.
                                      (contains? #{types/oid-time
                                                   types/oid-interval
                                                   types/oid-timetz}
                                                 oid)
                                      (and ordering?
                                           (or (contains? types/array-oid->element-oid oid)
                                               (nil? (get types/oid->pg-name oid)))))
                              oid))
                          oids)]
    (when unsupported
      (throw (errors/pg-error
              :feature-not-supported
              {:message (str "row comparison for "
                             (or (get types/oid->pg-name unsupported)
                                 "a user-defined type")
                             " is not implemented")})))))

(defn- in-left-asts [left]
  (cond
    (instance? ParenthesedExpressionList left)
    (vec ^ParenthesedExpressionList left)

    (and (instance? Function left)
         (= "row" (some-> ^Function left .getName str/lower-case)))
    (vec (or (some-> ^Function left .getParameters .getExpressions) []))

    :else [left]))

(defn- row-expression?
  "True for an explicit SQL row constructor, not a scalar parenthesis."
  [expr]
  (or (and (instance? ParenthesedExpressionList expr)
           (> (.size ^ParenthesedExpressionList expr) 1))
      (and (instance? Function expr)
           (= "row" (some-> ^Function expr .getName str/lower-case)))))

(defn- row-comparison-op [expr]
  (cond
    (instance? EqualsTo expr) :=
    (instance? NotEqualsTo expr) :<>
    (instance? MinorThan expr) :<
    (instance? MinorThanEquals expr) :<=
    (instance? GreaterThan expr) :>
    (instance? GreaterThanEquals expr) :>=))

(defn- row-comparison-expression?
  "Recognise PostgreSQL row comparisons and row-vs-Sublink comparisons.

   A multi-column scalar subquery is only legal on the right of an explicit
   row expression. A subquery on the left retains ordinary scalar-subquery
   width rules."
  [expr]
  (when (row-comparison-op expr)
    (let [^BinaryExpression expr expr
          left (.getLeftExpression expr)
          right (.getRightExpression expr)]
      (and (row-expression? left)
           (or (row-expression? right)
               (instance? ParenthesedSelect right))))))

(defn- check-row-output-types! [op left-oids right-oids comparison-oids]
  (when (not= (count left-oids) (count right-oids))
    (throw (errors/pg-error
            :syntax-error
            {:message (if (< (count right-oids) (count left-oids))
                        "subquery has too few columns"
                        "subquery has too many columns")})))
  (doseq [[left-oid right-oid comparison-oid]
          (map vector left-oids right-oids comparison-oids)]
    (when (and left-oid comparison-oid)
      (check-row-field-type! op left-oid right-oid))))

(defn- check-in-output-types! [left-oids right-oids right-resolution-oids]
  (check-row-output-types! := left-oids right-oids right-resolution-oids))

(defn- strict-subquery-rows
  "Execute an IN subquery and return all visible rows.

   Expected left OIDs establish row width and per-position operator
   compatibility before any result is consumed. Translation errors remain
   SQL answers rather than being collapsed into an empty RHS."
  ([parse-fn inner schema db left-oids relation-namespaces]
   (strict-subquery-rows parse-fn inner schema db left-oids relation-namespaces
                         {:op := :unknown-from-left? true}))
  ([parse-fn inner schema db left-oids relation-namespaces
    {:keys [op unknown-from-left? expected-width]
     :or {op := unknown-from-left? true}}]
   (let [runtime-db params/*runtime-db*
        ;; Query-local relations live only in the enriched parse-time DB.
        ;; Ordinary prepared subqueries must instead read the current
        ;; execution snapshot.
         runtime-schema (when runtime-db
                          (try (dbi/-schema runtime-db)
                               (catch Throwable _ nil)))
         fallback-schema (when db
                           (try (dbi/-schema db)
                                (catch Throwable _ nil)))
         enriched? (and db runtime-schema
                        (some #(not (contains? runtime-schema %))
                              (keys fallback-schema)))
         db (cond
              (seq relation-namespaces) db
              enriched? db
              runtime-db runtime-db
              :else db)
         p (binding [ctx/*relation-namespaces* relation-namespaces]
             (parse-fn (str inner) schema db))
         output-oids (parsed-output-oids p)
         resolution-oids (if (= :set-operation (:type p))
                           output-oids
                           (leaf-resolution-oids p))
         comparison-oids (if unknown-from-left? resolution-oids output-oids)
         _ (when (and expected-width (not= expected-width (count output-oids)))
             (throw (errors/pg-error
                     :syntax-error
                     {:message "subquery must return only one column"})))
         _ (when left-oids
             (check-row-output-types! op left-oids output-oids comparison-oids))
         target-oids (mapv (fn [left-oid output-oid resolution-oid]
                             (if (and unknown-from-left? left-oid (nil? resolution-oid))
                               left-oid
                               output-oid))
                           left-oids output-oids resolution-oids)
         rows (run-parsed-subquery p db)]
     (if (seq target-oids)
       (mapv #(set-ops/coerce-row % target-oids) rows)
       rows))))

(defn- strict-subquery-values
  "Execute a scalar IN subquery and return every value in its output column.

   Translation errors are SQL answers and must reach the client; multiple
   rows are expected, but multiple visible columns are not."
  ([parse-fn inner schema db]
   (strict-subquery-values parse-fn inner schema db nil {}))
  ([parse-fn inner schema db left-oid]
   (strict-subquery-values parse-fn inner schema db left-oid {}))
  ([parse-fn inner schema db left-oid relation-namespaces]
   (mapv first
         (strict-subquery-rows parse-fn inner schema db
                               [left-oid] relation-namespaces))))

(defn- strict-subquery-row
  "Execute a row-valued scalar subquery with PostgreSQL cardinality rules."
  [parse-fn inner schema db left-oids relation-namespaces op]
  (let [rows (strict-subquery-rows parse-fn inner schema db left-oids
                                   relation-namespaces
                                   {:op op :unknown-from-left? false})]
    (case (count rows)
      0 (vec (repeat (count left-oids) :__null__))
      1 (first rows)
      (throw (ex-info "more than one row returned by a subquery used as an expression"
                      {:error :cardinality-violation :sqlstate "21000"})))))

(defn strict-scalar-subquery
  "Execute a scalar subquery with PostgreSQL width and cardinality rules.

   Translation and execution errors remain SQL errors. Empty results become
   the SQL NULL sentinel because nil would filter a Datalog function binding."
  ([parse-fn inner schema db]
   (strict-scalar-subquery parse-fn inner schema db {}))
  ([parse-fn inner schema db relation-namespaces]
   (let [rows (strict-subquery-rows parse-fn inner schema db nil
                                    relation-namespaces
                                    {:expected-width 1
                                     :unknown-from-left? false})]
     (case (count rows)
       0 :__null__
       1 (let [v (ffirst rows)] (if (some? v) v :__null__))
       (throw (ex-info "more than one row returned by a subquery used as an expression"
                       {:error :cardinality-violation :sqlstate "21000"}))))))

(defn- correlation-oids [ctx corr-refs]
  (reduce (fn [m [alias col]]
            (assoc-in m [alias col]
                      (source-oid ctx (Column. (Table. ^String alias) ^String col))))
          {} corr-refs))

(defn- analyze-in-subquery!
  "Analyze an IN subquery once, before scanning the outer relation.

   Outer references are SQL NULL placeholders accompanied by their declared
   OIDs. This catches missing inner columns, star-expanded arity and operator
   type errors even when the outer relation contains no rows."
  [ctx left-asts inner corr-refs]
  (when (raw-in-subquery-having? inner)
    (throw (errors/pg-error
            :feature-not-supported
            {:message "this IN subquery requires SELECT post-processing that is not implemented"})))
  (let [parse-fn (:parse-sql ctx)
        corr-oids (correlation-oids ctx corr-refs)
        null-bindings (reduce (fn [m [alias col]]
                                (assoc-in m [alias col] :__null__))
                              {} corr-refs)
        parsed (binding [params/*from-bindings* null-bindings
                         params/*from-binding-oids* corr-oids
                         params/*lateral-outer-aliases* (set (map first corr-refs))]
                 (parse-fn (str inner) (:schema ctx) (:db ctx)))
        _ (validate-parsed-in-plan! parsed)
        output-oids (parsed-output-oids parsed)
        resolution-oids (if (= :set-operation (:type parsed))
                          output-oids
                          (leaf-resolution-oids parsed))
        left-oids (mapv #(operand-type-oid ctx %) left-asts)]
    (check-in-output-types! left-oids output-oids resolution-oids)
    parsed))

(defn- uncorrelated-in-values-var!
  "Bind an uncorrelated IN subquery's values at query execution time."
  [ctx inner]
  (let [parse-fn (:parse-sql ctx)
        schema (:schema ctx)
        fallback-db (:db ctx)
        relation-namespaces ctx/*relation-namespaces*
        fn-param (symbol (str "?in-subquery" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        evaluate (fn []
                   (strict-subquery-values parse-fn inner schema fallback-db nil
                                           relation-namespaces))]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (ctx/add-clause! ctx [(list fn-param) result-var])
    result-var))

(defn- uncorrelated-in-rows-var!
  "Bind all rows of an uncorrelated row-valued IN subquery at execution."
  [ctx inner left-oids]
  (let [parse-fn (:parse-sql ctx)
        schema (:schema ctx)
        fallback-db (:db ctx)
        relation-namespaces ctx/*relation-namespaces*
        fn-param (symbol (str "?row-in-subquery" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        evaluate (fn []
                   (strict-subquery-rows parse-fn inner schema fallback-db left-oids
                                         relation-namespaces))]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (ctx/add-clause! ctx [(list fn-param) result-var])
    result-var))

(defn- row-in-result-var!
  "Compare materialized left fields with runtime RHS rows."
  [ctx rows-var left-values not-in?]
  (let [left-values (mapv #(if (seq? %) (ctx/materialize-arg! ctx %) %) left-values)
        fn-param (symbol (str "?row-in" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        evaluate (fn [rows & lhs]
                   (let [result (fns/sql-row-in3? rows lhs)]
                     (if not-in? (fns/sql-not3 result) result)))]
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (ctx/add-clause! ctx [(apply list fn-param rows-var left-values) result-var])
    (swap! (:nullable-vars ctx) conj result-var)
    result-var))

(defn- correlated-in-var!
  "Bind scalar IN/NOT IN as one three-valued result per outer row."
  [ctx col inner corr-refs not-in?]
  (let [parse-fn (:parse-sql ctx)
        schema (:schema ctx)
        db (:db ctx)
        relation-namespaces ctx/*relation-namespaces*
        corr-refs (vec corr-refs)
        corr-oids (correlation-oids ctx corr-refs)
        arg-vars (mapv (fn [[a c]]
                         (translate-expr ctx (Column. (Table. ^String a) ^String c)))
                       corr-refs)
        col (if (seq? col) (ctx/materialize-arg! ctx col) col)
        fn-param (symbol (str "?corr-in" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        eval-in (fn [lhs & outer-vals]
                  (let [fb (reduce (fn [m [[a c] v]] (assoc-in m [a c] v))
                                   {} (map vector corr-refs outer-vals))
                        values (binding [params/*from-bindings* fb
                                         params/*from-binding-oids* corr-oids
                                         params/*lateral-outer-aliases*
                                         (set (map first corr-refs))]
                                 (strict-subquery-values parse-fn inner schema db nil
                                                         relation-namespaces))
                        result (fns/sql-in3? values lhs)]
                    (if not-in? (fns/sql-not3 result) result)))]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj eval-in)
    (ctx/add-clause! ctx [(apply list fn-param col arg-vars) result-var])
    (swap! (:nullable-vars ctx) conj result-var)
    result-var))

(defn- correlated-row-in-var!
  "Bind row-valued IN/NOT IN once per outer row."
  [ctx left-values left-oids inner corr-refs not-in?]
  (let [parse-fn (:parse-sql ctx)
        schema (:schema ctx)
        db (:db ctx)
        relation-namespaces ctx/*relation-namespaces*
        corr-refs (vec corr-refs)
        corr-oids (correlation-oids ctx corr-refs)
        arg-vars (mapv (fn [[a c]]
                         (translate-expr ctx (Column. (Table. ^String a) ^String c)))
                       corr-refs)
        left-values (mapv #(if (seq? %) (ctx/materialize-arg! ctx %) %) left-values)
        left-width (count left-values)
        fn-param (symbol (str "?corr-row-in" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        eval-in (fn [& args]
                  (let [lhs (vec (take left-width args))
                        outer-vals (drop left-width args)
                        fb (reduce (fn [m [[a c] v]] (assoc-in m [a c] v))
                                   {} (map vector corr-refs outer-vals))
                        rows (binding [params/*from-bindings* fb
                                       params/*from-binding-oids* corr-oids
                                       params/*lateral-outer-aliases*
                                       (set (map first corr-refs))]
                               (strict-subquery-rows parse-fn inner schema db left-oids
                                                     relation-namespaces))
                        result (fns/sql-row-in3? rows lhs)]
                    (if not-in? (fns/sql-not3 result) result)))]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj eval-in)
    (ctx/add-clause! ctx [(apply list fn-param (concat left-values arg-vars)) result-var])
    (swap! (:nullable-vars ctx) conj result-var)
    result-var))

(defn- resolved-outer-column-ref [ctx ^Column col]
  (let [col-name (-> col .getColumnName unquote-ident str/lower-case)
        resolved (ctx/resolve-column col
                                     (:table-aliases ctx)
                                     (:default-table ctx)
                                     (:col-overrides ctx)
                                     (:derived-aliases ctx)
                                     (:ci-index ctx))
        attr (ctx/attr-of ctx resolved)
        aliases (distinct (concat (:relation-aliases (meta (:table-aliases ctx)))
                                  (keys (:table-aliases ctx))
                                  [(:default-table ctx)]))
        owners (keep (fn [alias]
                       (when alias
                         (let [qualified (Column. (Table. ^String (name alias))
                                                  ^String col-name)]
                           (when (= resolved
                                    (ctx/resolve-column qualified
                                                        (:table-aliases ctx)
                                                        (:default-table ctx)
                                                        (:col-overrides ctx)
                                                        (:derived-aliases ctx)
                                                        (:ci-index ctx)))
                             alias))))
                     aliases)
        owner (or (when (some #{(:default-table ctx)} owners)
                    (:default-table ctx))
                  (first owners))]
    (ctx/validate-column! ctx attr)
    (when-not owner
      (throw (errors/pg-error :undefined-column {:column col-name})))
    [(name owner) col-name]))

(defn- row-subquery-correlation-refs [ctx inner]
  (let [qualified (correlated-subquery-refs inner (outer-alias-set ctx))
        ;; An inner SELECT without FROM resolves its unqualified columns in
        ;; the outer scope. PostgreSQL's own ROWCOMPARE regression uses
        ;; `(SELECT f1, f2)` in precisely this shape.
        unqualified (when (and (instance? PlainSelect inner)
                               (nil? (.getFromItem ^PlainSelect inner)))
                      (into #{}
                            (keep (fn [^Column col]
                                    (when (str/blank?
                                           (some-> col .getTable .getName))
                                      (resolved-outer-column-ref ctx col))))
                            (mapcat params/ast-columns
                                    (plain-select-scope-nodes inner))))]
    (not-empty (into (set qualified) unqualified))))

(defn- analyze-row-comparison-subquery!
  [ctx op left-oids inner corr-refs]
  (let [corr-oids (correlation-oids ctx corr-refs)
        null-bindings (reduce (fn [m [alias col]]
                                (assoc-in m [alias col] :__null__))
                              {} corr-refs)
        parsed (binding [params/*from-bindings* null-bindings
                         params/*from-binding-oids* corr-oids
                         params/*from-source-aliases* (set (map first corr-refs))
                         params/*lateral-outer-aliases* (set (map first corr-refs))]
                 ((:parse-sql ctx) (str inner) (:schema ctx) (:db ctx)))
        _ (validate-parsed-in-plan! parsed)
        output-oids (parsed-output-oids parsed)]
    ;; Unlike IN, a subquery's UNKNOWN output is resolved to text before a
    ;; RowCompareExpr selects its per-field operators.
    (check-row-output-types! op left-oids output-oids output-oids)
    (doseq [[left-oid right-oid] (map vector left-oids output-oids)]
      (check-row-runtime-field-type! op left-oid right-oid))
    output-oids))

(defn- translate-row-fields [ctx asts counterpart-oids]
  (mapv (fn [ast counterpart-oid]
          (let [v (translate-expr ctx ast)]
            (if (and counterpart-oid (oid-infer/untyped-literal? ast))
              (set-ops/coerce-value v counterpart-oid)
              v)))
        asts counterpart-oids))

(defn- row-comparison-result-var!
  [ctx op left-values right-values]
  (let [width (count left-values)
        values (mapv #(if (seq? %) (ctx/materialize-arg! ctx %) %)
                     (concat left-values right-values))
        fn-param (symbol (str "?row-cmp" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        evaluate (fn [& args]
                   (fns/sql-row-compare3 op
                                         (take width args)
                                         (drop width args)))]
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (ctx/add-clause! ctx [(apply list fn-param values) result-var])
    (swap! (:nullable-vars ctx) conj result-var)
    result-var))

(defn- uncorrelated-row-subquery-var!
  [ctx op inner left-oids]
  (let [parse-fn (:parse-sql ctx)
        schema (:schema ctx)
        fallback-db (:db ctx)
        relation-namespaces ctx/*relation-namespaces*
        fn-param (symbol (str "?row-sublink" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        evaluate (fn []
                   (strict-subquery-row parse-fn inner schema fallback-db left-oids
                                        relation-namespaces op))]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (ctx/add-clause! ctx [(list fn-param) result-var])
    result-var))

(defn- row-subquery-result-var!
  [ctx op rhs-var left-values]
  (let [left-values (mapv #(if (seq? %) (ctx/materialize-arg! ctx %) %) left-values)
        fn-param (symbol (str "?row-sublink-cmp" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        evaluate (fn [rhs & lhs] (fns/sql-row-compare3 op lhs rhs))]
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (ctx/add-clause! ctx [(apply list fn-param rhs-var left-values) result-var])
    (swap! (:nullable-vars ctx) conj result-var)
    result-var))

(defn- correlated-row-subquery-comparison-var!
  [ctx op inner left-values left-oids corr-refs]
  (let [parse-fn (:parse-sql ctx)
        schema (:schema ctx)
        db (:db ctx)
        relation-namespaces ctx/*relation-namespaces*
        corr-refs (vec corr-refs)
        corr-oids (correlation-oids ctx corr-refs)
        corr-values (mapv (fn [[alias col]]
                            (translate-expr ctx (Column. (Table. ^String alias)
                                                         ^String col)))
                          corr-refs)
        left-values (mapv #(if (seq? %) (ctx/materialize-arg! ctx %) %) left-values)
        left-width (count left-values)
        fn-param (symbol (str "?corr-row-sublink" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        evaluate (fn [& args]
                   (let [lhs (vec (take left-width args))
                         outer-values (drop left-width args)
                         bindings (reduce (fn [m [[alias col] v]]
                                            (assoc-in m [alias col] v))
                                          {} (map vector corr-refs outer-values))
                         rhs (binding [params/*from-bindings* bindings
                                       params/*from-binding-oids* corr-oids
                                       params/*from-source-aliases*
                                       (set (map first corr-refs))
                                       params/*lateral-outer-aliases*
                                       (set (map first corr-refs))]
                               (strict-subquery-row parse-fn inner schema db left-oids
                                                    relation-namespaces op))]
                     (fns/sql-row-compare3 op lhs rhs)))]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (ctx/add-clause! ctx
                     [(apply list fn-param (concat left-values corr-values)) result-var])
    (swap! (:nullable-vars ctx) conj result-var)
    result-var))

(defn- translate-row-comparison [ctx expr]
  (let [^BinaryExpression expr expr
        op (row-comparison-op expr)
        left-asts (in-left-asts (.getLeftExpression expr))
        right (.getRightExpression expr)
        inner (when (instance? ParenthesedSelect right)
                (.getSelect ^ParenthesedSelect right))]
    (if inner
      (let [left-oids (mapv #(operand-type-oid ctx %) left-asts)
            corr-refs (row-subquery-correlation-refs ctx inner)
            right-oids (analyze-row-comparison-subquery!
                        ctx op left-oids inner corr-refs)
            left-values (translate-row-fields ctx left-asts right-oids)]
        (if (seq corr-refs)
          (correlated-row-subquery-comparison-var!
           ctx op inner left-values left-oids corr-refs)
          (let [rhs-var (uncorrelated-row-subquery-var! ctx op inner left-oids)]
            (row-subquery-result-var! ctx op rhs-var left-values))))
      (let [right-asts (in-left-asts right)]
        (when-not (= (count left-asts) (count right-asts))
          (throw (errors/pg-error
                  :syntax-error
                  {:message "unequal number of entries in row expressions"})))
        (let [left-oids (mapv #(operand-type-oid ctx %) left-asts)
              right-oids (mapv #(operand-type-oid ctx %) right-asts)]
          (doseq [[left-oid right-oid] (map vector left-oids right-oids)]
            (check-row-runtime-field-type! op left-oid right-oid))
          (row-comparison-result-var!
           ctx op
           (translate-row-fields ctx left-asts right-oids)
           (translate-row-fields ctx right-asts left-oids)))))))

(defn- analyze-exists-subquery!
  "Validate a correlated EXISTS body before the outer relation is scanned."
  [ctx inner corr-refs]
  (let [corr-oids (correlation-oids ctx corr-refs)
        null-bindings (reduce (fn [m [alias col]]
                                (assoc-in m [alias col] :__null__))
                              {} corr-refs)
        parsed (binding [params/*from-bindings* null-bindings
                         params/*from-binding-oids* corr-oids
                         params/*lateral-outer-aliases* (set (map first corr-refs))]
                 ((:parse-sql ctx) (str inner) (:schema ctx) (:db ctx)))]
    (throw-subquery-error! parsed)
    (validate-parsed-in-plan! parsed)))

(defn- analyze-scalar-subquery!
  "Validate scalar width and executable stages before scanning an outer row."
  [ctx inner corr-refs]
  (let [corr-oids (correlation-oids ctx corr-refs)
        unqualified-outer-scope? (and (instance? PlainSelect inner)
                                      (nil? (.getFromItem ^PlainSelect inner)))
        null-bindings (reduce (fn [m [alias col]]
                                (assoc-in m [alias col] :__null__))
                              {} corr-refs)
        parsed (binding [params/*from-bindings* null-bindings
                         params/*from-binding-oids* corr-oids
                         params/*from-source-aliases*
                         (when unqualified-outer-scope?
                           (set (map first corr-refs)))
                         params/*lateral-outer-aliases* (set (map first corr-refs))]
                 ((:parse-sql ctx) (str inner) (:schema ctx) (:db ctx)))]
    (throw-subquery-error! parsed)
    (validate-parsed-in-plan! parsed)
    (when (not= 1 (count (parsed-output-oids parsed)))
      (throw (errors/pg-error
              :syntax-error
              {:message "subquery must return only one column"})))
    parsed))

(defn scalar-subquery-output-oid
  "Analyze a scalar subquery and return its sole PostgreSQL output OID.

   This is the callback seam used by oid-infer: the generic AST walker does
   not know SQL scopes, while this namespace can validate correlated names,
   scalar width and set-operation common types against the active context."
  [ctx expr]
  (let [inner (loop [node expr]
                (if (instance? ParenthesedSelect node)
                  (recur (.getSelect ^ParenthesedSelect node))
                  node))
        values-rows (when (instance? Values inner)
                      (let [raw (vec (.getExpressions ^Values inner))]
                        (if (and (seq raw)
                                 (instance? ParenthesedExpressionList (first raw)))
                          (mapv #(vec ^ParenthesedExpressionList %) raw)
                          [raw])))]
    (if values-rows
      (let [row (first values-rows)]
        (when (not= 1 (count row))
          (throw (errors/pg-error
                  :syntax-error
                  {:message "subquery must return only one column"})))
        (oid-infer/expr-oid (first row) (oid-env ctx)))
      (let [corr-refs (when (and (:db ctx) (:parse-sql ctx))
                        (seq (row-subquery-correlation-refs ctx inner)))
            parsed (analyze-scalar-subquery! ctx inner corr-refs)]
        (first (parsed-output-oids parsed))))))

(defn- uncorrelated-scalar-var!
  "Bind an uncorrelated scalar subquery at query execution time."
  [ctx inner]
  (let [parse-fn (:parse-sql ctx)
        schema (:schema ctx)
        fallback-db (:db ctx)
        relation-namespaces ctx/*relation-namespaces*
        fn-param (symbol (str "?scalar-subquery" (swap! (:var-counter ctx) inc)))
        result-var (ctx/fresh-var! ctx)
        cache-key (Object.)
        evaluate (fn []
                   (if-let [cache params/*scalar-subquery-cache*]
                     (if (contains? @cache cache-key)
                       (get @cache cache-key)
                       (let [value (strict-scalar-subquery
                                    parse-fn inner schema fallback-db relation-namespaces)]
                         (swap! cache assoc cache-key value)
                         value))
                     (strict-scalar-subquery
                      parse-fn inner schema fallback-db relation-namespaces)))
        form (list fn-param)]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj evaluate)
    (if ctx/*defer-expression-materialization*
      form
      (do
        (ctx/add-clause! ctx [form result-var])
        (swap! (:nullable-vars ctx) conj result-var)
        result-var))))

(defn- correlated-scalar-var!
  "Bind a variable to a CORRELATED scalar subquery's value, evaluated once
   per outer row, and return the variable.

   The subquery becomes an ordinary Datalog function binding --
   `[(?corr-fn ?outer-col …) ?v]` -- so it is evaluated INSIDE the query,
   with the outer row's values already bound. That is what makes it work
   in WHERE: the filter runs before grouping and before LIMIT, exactly
   where SQL puts it, with no post-pass to reorder.

   Before this, a correlated scalar subquery in an expression was
   evaluated ONCE at translate time. When the inner happened to translate
   anyway -- which it does whenever the correlation predicate's outer
   column resolves to a same-named inner one -- the result was a single
   CONSTANT folded into the predicate: `WHERE 0 > (SELECT min(i) FROM ft
   t2 WHERE t2.id <> ft.id)` became `0 > -3`, the GLOBAL minimum, and
   every row passed."
  [ctx inner corr-refs]
  (let [parse-fn (:parse-sql ctx)
        schema   (:schema ctx)
        db       (:db ctx)
        inner-sql (str inner)
        corr-refs (vec corr-refs)
        corr-oids (correlation-oids ctx corr-refs)
        unqualified-outer-scope? (and (instance? PlainSelect inner)
                                      (nil? (.getFromItem ^PlainSelect inner)))
        ;; Through translate-expr, so the outer column resolves exactly as
        ;; it would anywhere else in the statement -- aliases, ref columns
        ;; and the nullable-var bookkeeping included.
        arg-vars (mapv (fn [[a c]]
                         (translate-expr
                          ctx
                          (net.sf.jsqlparser.schema.Column.
                           (net.sf.jsqlparser.schema.Table. ^String a)
                           ^String c)))
                       corr-refs)
        ;; Datahike may memoize a function binding by its argument tuple.
        ;; PostgreSQL rescans a correlated volatile subplan for every
        ;; physical outer row, even when two rows carry equal correlation
        ;; values. Thread the outer entity id as an ignored identity token.
        row-token (when-let [alias (:default-table ctx)]
                    (ctx/entity-var! ctx alias))
        call-args (cond-> arg-vars row-token (conj row-token))
        out-var  (ctx/fresh-var! ctx)
        fn-param (symbol (str "?corr-scalar" (swap! (:var-counter ctx) inc)))
        f (fn [& all-vals]
            (let [outer-vals (take (count corr-refs) all-vals)
                  fb (reduce (fn [m [[a c] v]] (assoc-in m [a c] v))
                             {} (map vector corr-refs outer-vals))]
              (binding [params/*from-bindings* fb
                        params/*from-binding-oids* corr-oids
                        params/*from-source-aliases*
                        (when unqualified-outer-scope?
                          (set (map first corr-refs)))
                        ;; Without it the inner translator reads `t2.id =
                        ;; ft.id` as an implicit JOIN against the relation
                        ;; `ft` and adds ft to the inner FROM -- the
                        ;; correlation predicate dissolves and every outer
                        ;; row gets the same uncorrelated answer.
                        params/*lateral-outer-aliases* (set (map first corr-refs))]
                (eval-correlated-scalar parse-fn inner-sql true schema db))))]
    (reset! (:runtime-subqueries? ctx) true)
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj f)
    (let [form (apply list fn-param call-args)]
      (if ctx/*defer-expression-materialization*
        form
        (do
          (ctx/add-clause! ctx [form out-var])
          ;; It can be NULL, so comparisons against it need the three-valued
          ;; guard every other nullable value gets.
          (swap! (:nullable-vars ctx) conj out-var)
          out-var)))))

(defn column-value!
  "Return a column's SQL value variable. Most Datahike scalar values are
   already their SQL representation. NUMERIC specials use reserved
   BigDecimals at rest, while bit/varbit use canonical digit strings; both
   are decoded at this boundary. Join planning and literal equality can still
   bind the raw datom directly through col-var!."
  [ctx resolved]
  (let [raw (ctx/col-var! ctx resolved)
        attr (ctx/attr-of ctx resolved)
        attr-schema (get (:schema ctx) attr)
        pg-type (or (:pg/type attr-schema)
                    (params/pg-type-of-attr (:db ctx) attr))
        decode-fn (cond
                    (= :db.type/bigdec (:db/valueType attr-schema))
                    types/numeric-storage->value

                    (contains? #{"bit" "varbit"} pg-type)
                    (fn [v]
                      (if (string? v)
                        (pg-bits/parse-bit-literal v (= "varbit" pg-type))
                        v))

                    :else nil)]
    (if decode-fn
      (let [decode-param (symbol (str "?column-decode" (swap! (:var-counter ctx) inc)))
            decoded (ctx/propagate-nullability! ctx (ctx/fresh-var! ctx) raw)]
        (swap! (:in-params ctx) conj decode-param)
        (swap! (:in-args ctx) conj decode-fn)
        (ctx/add-clause! ctx [(list decode-param raw) decoded])
        decoded)
      raw)))

(defn- column-storage-value
  "Encode a typed SQL value for direct binding against a stored datom."
  [ctx resolved v]
  (let [attr (ctx/attr-of ctx resolved)]
    (if (= :db.type/bigdec (get-in (:schema ctx) [attr :db/valueType]))
      (types/numeric-value->storage v)
      v)))

(defn- session-value-expr!
  "Lower a bare SQL session value to a zero-argument Datalog input fn.
   Capture the session atom now so prepared statements observe later changes."
  [ctx prefix value-fn]
  (let [result-var (ctx/fresh-var! ctx)
        fn-param (symbol (str "?" prefix (swap! (:var-counter ctx) inc)))
        state params/*session-state*
        impl-fn (fn [] (or (value-fn state) :__null__))]
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj impl-fn)
    (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
    result-var))

(defn- session-current-schema [state]
  (let [path (or (some-> state deref :search-path) ["$user" "public"])]
    (first (filter #{"public" "pg_catalog"} path))))

(defn- bare-session-value-column? [expr]
  (and (instance? Column expr)
       (nil? (.getTable ^Column expr))
       (contains? #{"current_schema" "current_catalog"
                    "current_user" "session_user" "user" "system_user"
                    "localtime" "localtimestamp"}
                  (str/lower-case (.getColumnName ^Column expr)))))

(defn translate-expr
  "Translate a JSqlParser Expression to a value, variable, or predicate form.
   Returns a Datalog-compatible value or variable symbol."
  [ctx expr]
  (cond
    ;; current_schema (no parens) used as column reference — return 'public' constant
    (and (instance? Column expr)
         (= "current_schema" (.getColumnName ^Column expr))
         (nil? (.getTable ^Column expr)))
    (session-value-expr! ctx "cur-sch" session-current-schema)

    ;; CURRENT_CATALOG is the bare SQL-value spelling paired with
    ;; current_database(). Generic expressions need both operands; the
    ;; sole-projection classifier still supplies the real connection name.
    (and (instance? Column expr)
         (= "current_catalog" (str/lower-case (.getColumnName ^Column expr)))
         (nil? (.getTable ^Column expr)))
    (session-value-expr! ctx "cur-cat"
                         (fn [state]
                           (or (some-> state deref :db-name) "datahike")))

    ;; current_user / session_user / user / system_user as bare
    ;; identifiers (PG keywords; JSqlParser surfaces them as Column).
    ;; All collapse to the static handler role.
    (and (instance? Column expr)
         (nil? (.getTable ^Column expr))
         (contains? #{"current_user" "session_user" "user" "system_user"}
                    (str/lower-case (.getColumnName ^Column expr))))
    "datahike"

    ;; JSqlParser surfaces bare LOCALTIME / LOCALTIMESTAMP as unqualified
    ;; Columns even though SQL defines them as value functions. This must
    ;; precede the general Column branch below.
    (and (instance? Column expr)
         (nil? (.getTable ^Column expr))
         (contains? #{"localtime" "localtimestamp"}
                    (str/lower-case (.getColumnName ^Column expr))))
    (let [key-str (str/lower-case (.getColumnName ^Column expr))
          result-var (ctx/fresh-var! ctx)
          fn-param (symbol (str "?sql-local-time" (swap! (:var-counter ctx) inc)))
          value-fn (fn []
                     (let [^java.util.Date st (or params/*statement-time* (java.util.Date.))
                           ^java.time.Instant instant (.toInstant st)
                           ^java.time.ZonedDateTime zdt
                           (.atZone instant java.time.ZoneOffset/UTC)]
                       (if (= key-str "localtime")
                         (.toLocalTime zdt)
                         (.toLocalDateTime zdt))))]
      (swap! (:in-params ctx) conj fn-param)
      (swap! (:in-args ctx) conj value-fn)
      (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
      result-var)

    ;; A bare identifier naming a table in scope is a PostgreSQL
    ;; WHOLE-ROW REFERENCE: `SELECT t FROM t` yields the composite
    ;; `(1,a)`, not NULL. `relation-in-scope?` already stopped this from
    ;; raising 42703, but nothing then produced a value, so it bound
    ;; nothing and read as NULL — and `to_json(t)` / `json_agg(t)`
    ;; inherited that silently.
    (whole-row-ref-alias ctx expr)
    (translate-whole-row-ref ctx (whole-row-ref-alias ctx expr))

    (instance? Column expr)
    (let [^Column col-expr expr
          tbl (.getTable col-expr)
          tbl-name (when tbl (unquote-ident (.getName ^Table tbl)))
          col-name (unquote-ident (.getColumnName col-expr))
          binding-owners (when (and (nil? tbl-name) (seq params/*from-source-aliases*))
                           (params/binding-column-owners params/*from-bindings* col-name))
          ;; UPDATE's target relation is represented in ctx, while each
          ;; FROM row is a constant binding. Account for both scopes before
          ;; choosing an unqualified FROM column.
          target-column? (when (seq binding-owners)
                           (try
                             (let [resolved (ctx/resolve-column
                                             col-expr (:table-aliases ctx)
                                             (:default-table ctx)
                                             (:col-overrides ctx)
                                             (:derived-aliases ctx) (:ci-index ctx))
                                   attr (ctx/attr-of ctx resolved)]
                               (boolean (and attr (contains? (:schema ctx) attr))))
                             (catch Exception _ false)))
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
        ;; Bound by the current UPDATE ... FROM row.
        (get-in params/*from-bindings* [tbl-name col-name])

        (and (nil? tbl-name) (> (count binding-owners) 1))
        (params/ambiguous-column! col-name)

        (and (nil? tbl-name) (= 1 (count binding-owners)) target-column?)
        (params/ambiguous-column! col-name)

        (and (nil? tbl-name) (= 1 (count binding-owners)))
        (get-in params/*from-bindings* [(first binding-owners) col-name])

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
                                           (:derived-aliases ctx) (:ci-index ctx))]
          (column-value! ctx resolved))))

    (instance? AllColumns expr)
    :*

    (instance? LongValue expr)
    (.getValue ^LongValue expr)

    ;; An unadorned decimal literal is `numeric` in PostgreSQL, not
    ;; float8 -- `SELECT 0.1 + 0.2` is 0.3, not 0.30000000000000004, and
    ;; `SELECT 1.10` keeps its trailing zero. `.getValue` has already
    ;; gone through a double and lost both the exactness and the scale,
    ;; so the literal is rebuilt from the ORIGINAL TOKEN, which
    ;; JSqlParser preserves in the node's string form.
    (instance? DoubleValue expr)
    (let [v (types/decimal-literal expr (.getValue ^DoubleValue expr))]
      ;; Clojure's numeric `hasheq` calls BigDecimal.stripTrailingZeros.
      ;; For a valid PostgreSQL value such as 4e131071, that repeatedly
      ;; divides a 131072-digit BigInteger while Datahike hashes the query
      ;; form, turning a constant SELECT into minutes of CPU on JDK 21.
      ;; Keep unusually wide literals out of the query form and pass them
      ;; through Datalog's ordinary scalar-input boundary instead. This does
      ;; not change the BigDecimal value or its display scale, and leaves
      ;; normal literals embedded for the usual small-query fast path.
      (if (and (instance? java.math.BigDecimal v)
               (> (.precision ^java.math.BigDecimal v)
                  max-inline-numeric-precision))
        (let [p (symbol (str "?wide-numeric" (swap! (:var-counter ctx) inc)))]
          (swap! (:in-params ctx) conj p)
          (swap! (:in-args ctx) conj v)
          p)
        v))

    ;; Bit-string literals — MUST precede the plain StringValue branch,
    ;; which JSqlParser also uses for `B'1001000'` (prefix "B").
    ;;
    ;; `B'…'` and `X'…'` are the SQL standard's bit-string literals and
    ;; PG types both as `bit` (1560), not text: `SELECT B'1001000'`
    ;; describes as bit and `pg_typeof` answers `bit` (issue #28).
    ;; Without this they reached the client as a bare String, i.e. text
    ;; (25), losing the width that makes bit comparison and ordering
    ;; correct. Everything downstream — width coercion, ordering,
    ;; `length()`, the 1560/1562 OID — already understands PgBit; only
    ;; the literal was missing.
    (pg-bits/bit-string-literal? expr)
    (pg-bits/bit-string-literal-value expr)

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
          source-type (source-oid ctx (.getExpression se))
          inner (translate-expr ctx (.getExpression se))
          inner (if (seq? inner) (ctx/materialize-arg! ctx inner) inner)]
      (case sign
        ;; Width-checked when the operand has a declared integer type:
        ;; negating INT_MIN is out of range in PostgreSQL, while Java's
        ;; `-` wraps it back to itself. `(* -1 x)` could not express that
        ;; because the multiplication carries no width.
        \- (if (= source-type types/oid-interval)
             ;; Intervals are still text-backed. Until they have a
             ;; structural value, never send that carrier through numeric
             ;; multiplication and leak String->Number to a client.
             (list 'datahike.pg.sql/sql-unsupported-temporal-arithmetic inner)
             (let [w (when-not (number? inner)
                       (int-width-of ctx (.getExpression se)))]
               (cond
                 (number? inner) (- inner)
                 w (list 'datahike.pg.sql/sql-int-neg w inner)
                 ;; Route through numeric-special-aware multiplication so
                 ;; -Infinity swaps sign and -NaN remains NaN. Bare Clojure
                 ;; multiplication casts the carrier record to Number.
                 :else (list 'datahike.pg.sql/sql-* -1 inner))))
        ;; `~` — bitwise NOT, over integers and bit strings alike.
        ;; Previously fell through to the identity branch below, so
        ;; `SELECT ~1` answered 1 instead of -2: a silent wrong answer,
        ;; not an unsupported-feature error.
        \~ (if (or (number? inner) (pg-bits/pg-bit? inner))
             (fns/sql-bit-not inner)
             (list 'datahike.pg.sql/sql-bit-not inner))
        inner))

    ;; Arithmetic — materialize sub-expression operands to ensure Datahike
    ;; evaluates flat function bindings (no nested lists like (+ ?a (* ?b ?c)))
    ;;
    ;; A leading `~` is re-associated inside translate-binary-arith —
    ;; PG's prefix `~` binds looser than these operators. See there.
    (instance? Addition expr) (translate-binary-arith ctx expr '+)
    ;; `jsonb - key` / `jsonb - idx` deletes; only `-` on numbers is
    ;; arithmetic. Routing everything to translate-binary-arith made
    ;; `SELECT p - 'b'` throw a raw ClassCastException (String cannot be
    ;; cast to Number). Same reasoning as `||`: the stored value is a
    ;; string, so the column type decides, not the runtime value.
    (and (instance? Subtraction expr)
         (jsonb-column? ctx (.getLeftExpression ^net.sf.jsqlparser.expression.BinaryExpression expr)))
    (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
          l (translate-expr ctx (.getLeftExpression e))
          r (translate-expr ctx (.getRightExpression e))
          l (if (seq? l) (ctx/materialize-arg! ctx l) l)
          r (if (seq? r) (ctx/materialize-arg! ctx r) r)
          fn-param (symbol (str "?jb-del" (swap! (:var-counter ctx) inc)))
          del-fn (fn [a k]
                   (cond
                     (integer? k) (jb/jsonb-delete-idx a k)
                     (record? k)  (jb/jsonb-delete-keys a (or (:elements k) []))
                     (sequential? k) (jb/jsonb-delete-keys a k)
                     :else (jb/jsonb-delete-key a k)))
          result-var (ctx/fresh-var! ctx)]
      (swap! (:in-params ctx) conj fn-param)
      (swap! (:in-args ctx) conj del-fn)
      (swap! (:where-clauses ctx) conj [(list fn-param l r) result-var])
      result-var)

    (instance? Subtraction expr) (translate-binary-arith ctx expr '-)
    (instance? Multiplication expr) (translate-binary-arith ctx expr '*)
    (instance? Division expr) (translate-binary-arith ctx expr '/)
    (instance? Modulo expr) (translate-binary-arith ctx expr 'rem)

    ;; --- Bitwise operators ----------------------------------------------
    ;; `&` `|` `<<` `>>` over integers and bit strings. JSqlParser's
    ;; precedence for these already matches PG's — one shared level,
    ;; left-associative, BELOW `+`/`-` — which is what makes
    ;; `4 | 3 & 1` = 1 and `1 << 2 + 3` = 32 come out right.
    ;;
    ;; `^` is EXPONENTIATION in PG, not xor (PG spells xor `#`), despite
    ;; JSqlParser naming the node BitwiseXor. Its precedence there is
    ;; above `*` and left-associative, which JSqlParser also matches, so
    ;; `2 ^ 3 ^ 3` = 512 and `2 ^ 3 * 2` = 16.
    ;;
    ;; Bare `#` is token-rewritten to JSqlParser's XorExpression. Its AST is
    ;; normalized below because the parser gives XOR lower precedence than
    ;; these operators while PostgreSQL puts them all at one level.
    (instance? BitwiseAnd expr)
    (translate-binary-fn ctx expr 'datahike.pg.sql/sql-bit-and fns/sql-bit-and)
    (instance? BitwiseOr expr)
    (translate-binary-fn ctx expr 'datahike.pg.sql/sql-bit-or fns/sql-bit-or)
    (instance? BitwiseLeftShift expr)
    (translate-binary-fn ctx expr 'datahike.pg.sql/sql-bit-shift-left fns/sql-bit-shift-left)
    (instance? BitwiseRightShift expr)
    (translate-binary-fn ctx expr 'datahike.pg.sql/sql-bit-shift-right fns/sql-bit-shift-right)
    (instance? BitwiseXor expr)
    (translate-binary-fn ctx expr 'datahike.pg.sql/sql-power-op fns/sql-power-op)
    (instance? XorExpression expr)
    (translate-hash-xor ctx expr)

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
          _ (reject-json-operator! ctx op-str
                                   (.getLeftExpression be) (.getRightExpression be))
          l (translate-expr ctx (.getLeftExpression be))
          r (translate-expr ctx (.getRightExpression be))
          l (if (seq? l) (ctx/materialize-arg! ctx l) l)
          r (if (seq? r) (ctx/materialize-arg! ctx r) r)
          fn-param (symbol (str "?pg-arr-op" (swap! (:var-counter ctx) inc)))
          ;; An array COLUMN arrives as canonical PG text ("{1,2,3}"), not as
          ;; a PgArray, so every `(and (array? a) (array? b))` test below
          ;; failed against a stored column and `arr @> ARRAY[1]` silently
          ;; matched nothing. Coerce the text side using the other's element
          ;; type before deciding which family of operator this is.
          as-arrays (fn [a b]
                      (cond
                        (and (pg-arr/array? a) (string? b))
                        [a (coerce-pg-array b (:elem-type a))]
                        (and (pg-arr/array? b) (string? a))
                        [(coerce-pg-array a (:elem-type b)) b]
                        :else [a b]))
          op-fn (case op-str
                  "@>" (fn [a b]
                         (let [[a b] (as-arrays a b)]
                           (cond
                             (and (pg-arr/array? a) (pg-arr/array? b))
                             (pg-arr/contains-arr? a b)
                             :else (jb/jsonb-contains? a b))))
                  "<@" (fn [a b]
                         (let [[a b] (as-arrays a b)]
                           (cond
                             (and (pg-arr/array? a) (pg-arr/array? b))
                             (pg-arr/contains-arr? b a)
                             :else (jb/jsonb-contained? a b))))
                  "&&" (fn [a b]
                         (let [[a b] (as-arrays a b)]
                           (if (and (pg-arr/array? a) (pg-arr/array? b))
                             (pg-arr/overlap? a b)
                             false)))
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
          ;; `||` on jsonb is jsonb_concat, not string concatenation.
          ;; Decided here rather than in the runtime cond because the
          ;; stored value is a string either way — see jsonb-column?.
          jsonb-concat? (or (jsonb-column? ctx (.getLeftExpression e))
                            (jsonb-column? ctx (.getRightExpression e)))
          ;; A temporal operand has to render the PostgreSQL way, and the
          ;; date/timestamp distinction is only visible here — see
          ;; types/temporal->pg-text.
          l-oid (source-oid ctx (.getLeftExpression e))
          r-oid (source-oid ctx (.getRightExpression e))
          null? (fn [x] (or (nil? x) (= :__null__ x)))
          concat-fn (fn [a b]
                      (cond
                        ;; `||` is STRICT: NULL on either side makes the
                        ;; whole expression NULL. This is exactly where it
                        ;; differs from concat(), which ignores its NULL
                        ;; arguments -- and the reason PostgreSQL ships
                        ;; both. Falling through to `str` treated a NULL as
                        ;; the empty string, so `'a' || NULL` answered 'a'.
                        ;; Strictness holds for the array and jsonb
                        ;; overloads too, hence the guard ahead of them.
                        (or (null? a) (null? b)) :__null__
                        jsonb-concat? (jb/jsonb-concat a b)
                        (and (pg-arr/array? a) (pg-arr/array? b))
                        (pg-arr/concat-arrs a b)
                        ;; bit || bit is bitcat, not string concat — the
                        ;; generic `(str a b)` below would stringify the
                        ;; PgBit records themselves.
                        (and (pg-bits/pg-bit? a) (pg-bits/pg-bit? b))
                        (pg-bits/concat-bits a b)
                        (and (bytes? a) (bytes? b))
                        (byte-array (concat a b))
                        ;; Append/prepend scalar to array — PG allows
                        ;; `arr || scalar` and `scalar || arr`.
                        (pg-arr/array? a)
                        (pg-arr/array (:elem-type a) (conj (:elements a) b))
                        (pg-arr/array? b)
                        (pg-arr/array (:elem-type b) (into [a] (:elements b)))
                        :else (str (types/->pg-text a l-oid)
                                   (types/->pg-text b r-oid))))
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

    ;; `agg(x) FILTER (WHERE p)` nested in a larger expression. JSqlParser
    ;; surfaces the FILTER form as an AnalyticExpression, so it did not
    ;; reach the aggregate branch below and the whole expression was
    ;; rejected -- "expression of type AnalyticExpression is not
    ;; supported" -- where PostgreSQL just adds 1 to a filtered sum.
    ;; A windowed aggregate (`OVER …`) is NOT hoisted: the window pass
    ;; computes it after the query from the whole result set.
    (and (instance? net.sf.jsqlparser.expression.AnalyticExpression expr)
         (:hoisted-aggs ctx)
         (let [^net.sf.jsqlparser.expression.AnalyticExpression ae expr]
           (and (= "FILTER_ONLY" (str (.getType ae)))
                (fns/aggregate-function? (str/lower-case (.getName ae))))))
    (let [v (ctx/fresh-var! ctx)]
      (swap! (:hoisted-aggs ctx) conj {:var v :fn-node expr})
      v)

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
        ;; An aggregate NESTED inside a larger expression -- `round(avg(x),
        ;; 2)`, `coalesce(sum(x), 0)`, `upper(max(s))`. PostgreSQL hoists
        ;; these: the aggregate is computed by the grouping step and the
        ;; expression around it is a projection over the result. We do the
        ;; same -- allocate a variable, register the aggregate under it,
        ;; and hand the variable back so the enclosing translation
        ;; produces an ordinary form over it.
        ;;
        ;; Without a sink (HAVING, and any caller that has no grouping
        ;; step to hoist INTO) the old marker is returned instead.
        (if-let [sink (:hoisted-aggs ctx)]
          (let [v (ctx/fresh-var! ctx)]
            (swap! sink conj {:var v :fn-node f})
            v)
          {:aggregate true :fn fname :params (.getParameters f)})

        ;; Non-aggregate function → Datalog function binding
        :else
        (let [v (translate-function-call ctx f)]
          (if (contains? common-type-fns fname)
            (coerce-to-common! ctx v
                               (or (some-> (.getParameters f) .getExpressions)
                                   (some-> (.getNamedParameters f) .getExpressions))
                               (str/upper-case fname))
            v))))

    ;; CURRENT_TIMESTAMP, CURRENT_DATE, CURRENT_TIME
    (instance? TimeKeyExpression expr)
    (let [key-str (str/lower-case (str/trim (str expr)))
          result-var (ctx/fresh-var! ctx)
          now-fn (cond
                   (or (= key-str "current_timestamp")
                       (= key-str "now()"))
                   (fn [] (or params/*statement-time* (java.util.Date.)))
                   ;; LocalDate (not a midnight java.util.Date) so the
                   ;; result renders as "yyyy-MM-dd" with OID 1082, like
                   ;; PG's date type.
                   (= key-str "current_date")
                   (fn [] (let [^java.util.Date st (or params/*statement-time* (java.util.Date.))]
                            (-> st .toInstant (.atZone java.time.ZoneOffset/UTC) .toLocalDate)))
                   (= key-str "current_time")
                   (fn [] (let [^java.util.Date st (or params/*statement-time* (java.util.Date.))]
                            (-> st .toInstant (.atZone java.time.ZoneOffset/UTC)
                                .toOffsetDateTime .toOffsetTime)))
                   :else (fn [] (or params/*statement-time* (java.util.Date.))))
          fn-param (symbol (str "?now-fn" (swap! (:var-counter ctx) inc)))]
      (swap! (:in-params ctx) conj fn-param)
      (swap! (:in-args ctx) conj now-fn)
      (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
      result-var)

    ;; jsonb field/element access: col->'key', col->>'key', col->'a'->>'b'
    (instance? JsonExpression expr)
    (let [{:keys [base chain]} (flatten-json-chain ^JsonExpression expr)
          base-val (translate-expr ctx base)
          ;; `->` and `#>` yield a JSON VALUE; `->>` and `#>>` yield text.
          ;; A json string must therefore render QUOTED (`"x"`, not `x`),
          ;; but the output path sees a Clojure string either way and
          ;; cannot tell it from a SQL text value. Serialising the FINAL
          ;; result of a value-returning chain settles it at the one
          ;; place that knows — intermediate steps stay values, so
          ;; `d->'a'->>'b'` is unaffected.
          value-op? (contains? #{"->" "#>"} (second (last chain)))]
      (cond-> (reduce
               (fn [current [key-expr op-str]]
                 (let [translated-key (translate-expr ctx key-expr)
                       key-val (if (seq? translated-key)
                                 (ctx/materialize-arg! ctx translated-key)
                                 translated-key)
                       op-fn  (jb/op op-str)
                       param  (symbol (str "?json-op" (swap! (:var-counter ctx) inc)))
                       result (ctx/fresh-var! ctx)]
                   (swap! (:in-params ctx) conj param)
                   (swap! (:in-args ctx) conj op-fn)
                   (swap! (:where-clauses ctx) conj [(list param current key-val) result])
                   result))
               base-val
               chain)
        value-op?
        (as-> v (let [param  (symbol (str "?json-render" (swap! (:var-counter ctx) inc)))
                      result (ctx/fresh-var! ctx)]
                  (swap! (:in-params ctx) conj param)
                  (swap! (:in-args ctx) conj
                         (fn [x] (if (or (nil? x) (= :__null__ x))
                                   :__null__
                                   (jb/serialize-jsonb x))))
                  (swap! (:where-clauses ctx) conj [(list param v) result])
                  result))))

    ;; expr AT TIME ZONE 'zone' — e.g. now() AT TIME ZONE 'UTC'
    (instance? TimezoneExpression expr)
    (let [left (.getLeftExpression ^TimezoneExpression expr)]
      (if (and (instance? Function left)
               (= "now" (str/lower-case (.getName ^Function left))))
        (let [fn-param (symbol (str "?now-fn" (swap! (:var-counter ctx) inc)))
              now-fn (fn [] (or params/*statement-time* (java.util.Date.)))
              result-var (ctx/fresh-var! ctx)]
          (swap! (:in-params ctx) conj fn-param)
          (swap! (:in-args ctx) conj now-fn)
          (swap! (:where-clauses ctx) conj [(list fn-param) result-var])
          result-var)
        (translate-expr ctx left)))

    ;; EXTRACT(field FROM value) is its own AST node, not a Function, so it
    ;; never reached the function table at all.
    (instance? ExtractExpression expr)
    (let [^ExtractExpression e expr
          v (translate-expr ctx (.getExpression e))
          v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
      (list 'datahike.pg.sql/sql-extract (str (.getName e)) v))

    ;; TRIM([LEADING|TRAILING|BOTH] [chars] FROM s) is also its own node.
    ;; With a FROM, `.getExpression` is the CHARACTER SET and
    ;; `.getFromExpression` is the string -- the other way round from the
    ;; bare `trim(s)` form.
    (instance? TrimFunction expr)
    (let [^TrimFunction e expr
          spec (str/lower-case (str (.getTrimSpecification e)))
          from-e (.getFromExpression e)
          str-e (if from-e from-e (.getExpression e))
          chars-e (when from-e (.getExpression e))
          v (translate-expr ctx str-e)
          v (if (seq? v) (ctx/materialize-arg! ctx v) v)
          c (when chars-e
              (let [cv (translate-expr ctx chars-e)]
                (if (seq? cv) (ctx/materialize-arg! ctx cv) cv)))
          f (case spec
              "leading"  'datahike.pg.sql/sql-ltrim
              "trailing" 'datahike.pg.sql/sql-rtrim
              'datahike.pg.sql/sql-btrim)]
      (if c (list f v c) (list f v)))

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
        (instance? IsBooleanExpression expr)
        (instance? IsUnknownExpression expr)
        (instance? IsDistinctExpression expr)
        (instance? InExpression expr))
    ;; Flatten: this result becomes a datalog clause, and datahike rejects
    ;; nested forms as function arguments. (CASE / FILTER reach
    ;; translate-predicate-expr directly and interpret the form tree
    ;; instead, so they neither need nor want this.)
    (materialize-nested! ctx (translate-predicate-expr ctx expr))

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
    ;; Strategy: a CORRELATED inner becomes a per-row function binding;
    ;; an uncorrelated one becomes a zero-argument runtime binding so a
    ;; prepared statement reads its execution snapshot. Both share strict
    ;; scalar width, cardinality and error propagation.
    (or (instance? ParenthesedSelect expr) (instance? PlainSelect expr))
    (let [inner (loop [node expr]
                  (if (instance? ParenthesedSelect node)
                    (recur (.getSelect ^ParenthesedSelect node))
                    node))]
      ;; `SELECT (VALUES (1))` is PostgreSQL's scalar-subquery spelling for
      ;; a one-row, one-column VALUES relation. JSqlParser puts Values behind
      ;; ParenthesedSelect, but the ordinary scalar-subquery executor below
      ;; only understands PlainSelect and used to pass a nil query to d/q.
      ;; A scalar VALUES expression needs no nested query: lower its sole
      ;; expression in the current context and enforce scalar cardinality.
      (if (instance? Values inner)
        (let [raw (vec (.getExpressions ^Values inner))
              rows (if (and (seq raw)
                            (instance? ParenthesedExpressionList (first raw)))
                     (mapv #(vec ^ParenthesedExpressionList %) raw)
                     [raw])]
          (when (> (count rows) 1)
            (throw (ex-info "more than one row returned by a subquery used as an expression"
                            {:error :cardinality-violation :sqlstate "21000"})))
          (let [row (first rows)]
            (when (not= 1 (count row))
              (throw (ex-info "subquery must return only one column"
                              {:error :syntax-error :sqlstate "42601"})))
            (translate-expr ctx (first row))))
        (let [db    (:db ctx)
              corr-refs (when (and db (:parse-sql ctx))
                          (seq (row-subquery-correlation-refs ctx inner)))]
          (if (and db (:parse-sql ctx))
            (do
              (analyze-scalar-subquery! ctx inner corr-refs)
              (if corr-refs
                (correlated-scalar-var! ctx inner corr-refs)
                (uncorrelated-scalar-var! ctx inner)))
            :__null__))))

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
                                               (:derived-aliases ctx) (:ci-index ctx))
                           (catch Throwable _ nil))]
    (let [attr (cond (keyword? resolved) resolved
                     (and (vector? resolved) (= 3 (count resolved))) (nth resolved 2)
                     :else nil)]
      (when attr (get-in (:schema ctx) [attr :db/valueType])))))

(defn- column-pg-type
  "Return a column's declared PostgreSQL type, retaining distinctions
   which Datahike's carrier cannot express (notably money vs numeric)."
  [ctx ^Column col]
  (when-let [resolved (try (ctx/resolve-column col
                                               (:table-aliases ctx)
                                               (:default-table ctx)
                                               (:col-overrides ctx)
                                               (:derived-aliases ctx) (:ci-index ctx))
                           (catch Throwable _ nil))]
    (when-let [attr (ctx/attr-of ctx resolved)]
      (or (get-in (:schema ctx) [attr :pg/type])
          (params/pg-type-of-attr (:db ctx) attr)))))

(defn jsonb-column?
  "Whether `expr` is a column reference of `jsonb` type.

   A stored jsonb value IS a string, so runtime dispatch cannot tell it
   from `text` — `p || '{\"z\":9}'` looks like two strings and fell to
   string concatenation. The type has to come from the schema, and
   `:pg/type` is an ident-entity fact, so the lookup falls back to the
   query exactly as `coerce-insert-value` does."
  [ctx expr]
  (boolean
   (or
    ;; An explicit `::jsonb` cast is a jsonb operand even though it is
    ;; not a column — `'[1,2,3]'::jsonb - 1`.
    (and (instance? CastExpression expr)
         (contains? #{"json" "jsonb"}
                    (some-> ^CastExpression expr .getColDataType .getDataType
                            str/lower-case)))
    ;; UPDATE ... FROM rows are runtime constant bindings rather than
    ;; relations in ctx.  Type probing must therefore be tolerant of an
    ;; otherwise-unresolvable qualified binding (`v.j`), just as
    ;; column-pg-type is.  A failed probe means "type unknown", not a
    ;; missing-FROM error; translate-expr resolves the bound value below.
    (and (instance? Column expr)
         (= "jsonb" (column-pg-type ctx ^Column expr))))))

(defn- json-column?
  "Whether `expr` is a column of the text-faithful `json` type.

   PostgreSQL gives `json` SIX operators — `->`, `->>`, `#>`, `#>>` and
   their int variants — and nothing else. It has no `=`, no `<`, no
   `@>`, no `?`, and no btree or hash operator class at all, so
   `'1'::json = '1'::json` is 42883 there. We accepted all of them
   silently, comparing the stored text."
  [ctx expr]
  (boolean
   (and (instance? Column expr)
        (= "json" (column-pg-type ctx ^Column expr)))))

(defn- reject-json-operator!
  "PostgreSQL has no such operator on `json`; raise as it does."
  [ctx op-str l r]
  (when (or (json-column? ctx l) (json-column? ctx r))
    (throw (ex-info (str "operator does not exist: json " op-str " json")
                    {:error :undefined-function
                     :sqlstate "42883"
                     :hint (str "No operator matches the given name and argument "
                                "types. You might need to add explicit type casts.")}))))

(defn- jsonb-canonical-operand
  "Canonicalize a literal being compared against a `jsonb` column.

   PostgreSQL's jsonb `=` compares VALUES; ours compares the stored
   CANONICAL TEXT. That works only if both sides are in the same
   canonical form, and until now only the stored side was — so
   `WHERE p = '{ \"b\":1, \"a\":2 }'` answered zero rows against a
   stored `{\"a\": 1, \"b\": 2}`, turning an equality into a test of
   how the literal happened to be spelled.

   `:pg/type` is an ident-entity fact rather than a `:db/*` key, so it
   is absent from the schema map unless enriched; fall back to the
   query, as `coerce-insert-value` does, or a column would silently
   read as not-jsonb here too.

   Residual gap: canonical TEXT equality is still finer than
   PostgreSQL's, which ignores numeric scale (`1.00` = `1`). Closing
   that needs structural comparison; see doc/jsonb-plan.md."
  [ctx resolved v]
  (if-not (string? v)
    v
    (let [attr (ctx/attr-of ctx resolved)]
      (if (and attr
               (= "jsonb" (or (get-in (:schema ctx) [attr :pg/type])
                              (params/pg-type-of-attr (:db ctx) attr))))
        (jb/serialize-jsonb v)
        v))))

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
  (cond
    ;; A bit-string literal compared against a bit column. Datahike has
    ;; no bit type, so a `bit`/`varbit` column stores PG's text form —
    ;; the digit run — and the literal has to come down to that form to
    ;; match. (Comparing PgBit against the stored String matches
    ;; nothing, which is how `WHERE b = B'1001000'` silently returned
    ;; zero rows once literals started producing PgBit.)
    ;;
    ;; Only for a string-typed column: against anything else the value
    ;; keeps its bit type and the normal operator rules apply.
    (and (instance? Column col)
         (pg-bits/bit-string-literal? lit)
         (= :db.type/string (column-vtype ctx col)))
    (pg-bits/to-pg-text (pg-bits/bit-string-literal-value lit))

    (and (instance? StringValue lit)
         (instance? Column col))
    (let [s (.getNotExcapedValue ^StringValue lit)]
      (if (= "money" (column-pg-type ctx col))
        (sql-cast/cast-scalar s "money" {})
        (when-let [vt (column-vtype ctx col)]
          (let [v (coerce/coerce-unknown s vt parse-timestamp-string)]
            (when (not (identical? v s)) ; signal only when coercion produced a typed value
              v)))))))

(defn- coerce-comparison-operands
  "Apply PG-style unknown-literal coercion to a `[left right]` pair of
   AST nodes for a binary comparison. Returns `[left' right']` where
   each side is either the original AST node or a pre-resolved
   typed Clojure value (Long/Double/Boolean/UUID/Date/...). The
   caller's translate-expr branch handles both."
  [ctx left right]
  (let [coerce-for-expr
        (fn [typed lit]
          (when (instance? StringValue lit)
            (when-let [vtype (some-> (source-oid ctx typed)
                                     types/dh-type-for-oid)]
              ;; Text expressions need no typinput coercion, and using the
              ;; parser node's raw body here would undo E'' escape decoding.
              (when-not (= :db.type/string vtype)
                (coerce/coerce-unknown
                 (.getNotExcapedValue ^StringValue lit)
                 vtype parse-timestamp-string)))))]
    [(or (coerce-unknown-literal ctx right left)
         (coerce-for-expr right left)
         left)
     (or (coerce-unknown-literal ctx left right)
         (coerce-for-expr left right)
         right)]))

(def ^:private op-sym->sql
  "The SQL spelling of a comparison operator, for PostgreSQL's
   `operator does not exist` message."
  {'= "=" 'not= "<>" '< "<" '> ">" '<= "<=" '>= ">="})

(def ^:private estimated-relation-prefixes
  "Namespaces of relations whose column types WE inferred rather than the
   user declaring them."
  ["__cte" "__sub__" "__lsub__" "__srf"])

(defn- estimated-column-type?
  "True when `e` is a column of a CTE, a derived table or a LATERAL
   relation.

   PostgreSQL knows those columns' types exactly -- it propagates them
   from the subquery's target list. We ESTIMATE them: the materialiser
   samples the rows, and a column that is NULL in every sampled row
   falls back to text. Checking an operator against an estimate raises
   on our own guess, which is what happened to asyncpg's `typeinfo`
   introspection: `ti.oid = tt.range_subtype` compared a catalog oid
   against a recursive-CTE column whose values are all NULL here, so we
   called it text and rejected a query PostgreSQL accepts."
  [ctx e]
  (boolean
   (when (instance? Column e)
     (when-let [attr (try (ctx/attr-of ctx (ctx/resolve-column
                                            ^Column e
                                            (:table-aliases ctx)
                                            (:default-table ctx)
                                            (:col-overrides ctx)
                                            (:derived-aliases ctx)
                                            (:ci-index ctx)))
                          (catch Throwable _ nil))]
       (when-let [ns- (namespace attr)]
         (some #(str/starts-with? ns- %) estimated-relation-prefixes))))))

(defn- operand-type-oid
  "An operand's type for OPERATOR resolution, or nil when PostgreSQL
   would call it UNKNOWN.

   A quoted literal and NULL are untyped constants there -- they take
   the other operand's type, so they can never make a comparison
   ill-typed. `expr-oid` reports text for both because that is the right
   answer for a PROJECTION (`SELECT 'a'` is text); it is the wrong one
   here, and using it would make `flag = 'true'` an error."
  [ctx e]
  (when-not (or (oid-infer/untyped-literal? e) (estimated-column-type? ctx e))
    (source-oid ctx e)))

(defn- check-comparison-types!
  "Raise 42883 when PostgreSQL has no operator for these operand types.

   PostgreSQL resolves an operator by finding a candidate both arguments
   coerce to implicitly; with none, `true = 1` is an ERROR, not FALSE.
   We compared anything against anything and answered, which is a wrong
   answer wearing the right shape -- a driver probing types with
   `WHERE oid = 'x'` got a row count instead of the error it tests for.

   Only fires when BOTH operand types are known: an operand we could not
   type stays lenient, which is the safe direction for a check whose
   whole risk is a false positive."
  [ctx op left right]
  (let [a (operand-type-oid ctx left)
        b (operand-type-oid ctx right)]
    (when-not (types/comparison-compatible? op a b)
      (throw (errors/pg-error
              :undefined-function
              {:detail (str "operator does not exist: "
                            (get types/oid->pg-name a "?") " "
                            (get op-sym->sql op (str op)) " "
                            (get types/oid->pg-name b "?"))
               :hint (str "No operator matches the given name and argument "
                          "types. You might need to add explicit type casts.")})))))

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
  (check-comparison-types! ctx op left right)
  (if (and (instance? Function right)
           (#{"any" "all"}
            (str/lower-case (.getName ^Function right))))
    (let [^Function fn-expr right
          kind (str/lower-case (.getName fn-expr))
          params (.getParameters fn-expr)
          arr-expr (when params (first params))]
      (translate-quantified-cmp ctx op left arr-expr kind))
    (or
     ;; Implicit-join equality (`FROM a, b WHERE a.x = b.y`) between two
     ;; plain columns in a top-level conjunct: unify on a shared logic
     ;; var (hash join) instead of get-else + predicate (cross product).
     ;; Same machinery as the explicit INNER JOIN ON path.
     ;; NOT for jsonb: unifying on a shared logic var makes the join
     ;; key TEXT equality, which misses `1.00` = `1`. Fall through to
     ;; the predicate below, which uses jsonb-eq?.
     (when (and (= op '=) *conjunctive-where*
                (instance? Column left) (instance? Column right)
                (nil? (.getArrayConstructor ^Column left))
                (nil? (.getArrayConstructor ^Column right))
                (not (jsonb-column? ctx left))
                (not (jsonb-column? ctx right))
                ;; UPDATE FROM values are constants for this invocation,
                ;; not a second Datalog relation. This gate is deliberately
                ;; UPDATE-specific; using *from-bindings* alone also catches
                ;; correlated/LATERAL machinery and changes its join plans.
                (not (and (seq params/*from-source-aliases*)
                          (some (fn [^Column c]
                                  (let [t (some-> (.getTable c) .getName unquote-ident)
                                        cn (unquote-ident (.getColumnName c))]
                                    (if t
                                      (and (contains? params/*from-source-aliases* t)
                                           (contains? (get params/*from-bindings* t) cn))
                                      (seq (params/binding-column-owners
                                            params/*from-bindings* cn)))))
                                [left right])))
                ;; NOT when either side names a table bound in
                ;; *from-bindings*. Those columns are CONSTANTS supplied
                ;; per outer row (UPDATE ... FROM (VALUES …), and a
                ;; correlated LATERAL inner), not a relation to join
                ;; against. This branch runs before translate-expr, so
                ;; it would unify `c.tid = t.id` into a join against the
                ;; real table `t` and never substitute the binding —
                ;; which made a LATERAL inner answer no rows.
                ;; ...and neither side names a LATERAL outer alias.
                ;; Inside a correlated LATERAL inner, `c.tid = t.id` is a
                ;; filter against a per-row CONSTANT, not a join against
                ;; the relation `t` — unifying them here would run before
                ;; translate-expr and the binding would never be
                ;; substituted. Gated on the lateral-specific var, not on
                ;; *from-bindings* itself: keying it off the latter
                ;; changed behaviour for every other user of it and cost
                ;; 37 asyncpg tests.
                ;;
                ;; And gated on the COLUMN, not just the alias. The set of
                ;; outer references is collected LEXICALLY by regex
                ;; (correlated-subquery-refs) while the values arrive in a
                ;; runtime map, and the two can disagree. Suppressing on the
                ;; alias alone meant that when the regex missed a reference,
                ;; the join was suppressed AND the outer table still entered
                ;; the inner FROM -- a CROSS PRODUCT. asyncpg's recursive
                ;; {typeinfo} introspection query hung outright on that.
                ;; Requiring the binding to actually supply the column makes
                ;; a miss fall back to the ordinary join: slower, but right.
                (not (some (fn [^Column c]
                             (when-let [t (some-> (.getTable c) .getName unquote-ident)]
                               (and (contains? (or params/*lateral-outer-aliases* #{}) t)
                                    (contains? (get params/*from-bindings* t)
                                               (unquote-ident (.getColumnName c))))))
                           [left right])))
       (let [resolve-col #(try (ctx/resolve-column ^Column %
                                                   (:table-aliases ctx)
                                                   (:default-table ctx)
                                                   (:col-overrides ctx)
                                                   (:derived-aliases ctx) (:ci-index ctx))
                               (catch Throwable _ nil))
             l-res (resolve-col left)
             r-res (resolve-col right)
             ;; Unifying on a shared logic var makes the join key VALUE
             ;; equality, which is type-sensitive -- so two numeric
             ;; columns of different storage types would never unify and
             ;; `WHERE n = f` (numeric vs float8) answered no rows. Raw
             ;; equality also disagrees with PostgreSQL on floating NaN.
             ;; Fall through to sql-eq?, which handles both semantics.
             numeric-vtypes #{:db.type/long :db.type/double
                              :db.type/float :db.type/bigdec}
             vtype-of (fn [res]
                        (when-let [a (ctx/attr-of ctx res)]
                          (get-in (:schema ctx) [a :db/valueType])))
             lv (vtype-of l-res)
             rv (vtype-of r-res)
             cross-numeric? (and lv rv (not= lv rv)
                                 (numeric-vtypes lv) (numeric-vtypes rv))
             floating? (some #{:db.type/float :db.type/double} [lv rv])]
         (when (and l-res r-res (not cross-numeric?) (not floating?)
                    (ctx/unify-inner-equijoin! ctx l-res r-res))
           [])))
     ;; jsonb `=` / `<>` compare VALUES and are numeric-scale
     ;; INSENSITIVE, so they cannot lower to `=` on the canonical text:
     ;; `'1.00'::jsonb = '1'::jsonb` is TRUE in PostgreSQL and the texts
     ;; differ. There was already a jsonb branch, but only on the path
     ;; taken when the right operand is a BARE literal — so `j = '1'`
     ;; was right while `j = '1'::jsonb`, `'1'::jsonb = j` and
     ;; column-to-column comparison all fell through to here and
     ;; answered false. Decide it BEFORE coerce-comparison-operands,
     ;; which replaces the AST nodes this test needs.
     (let [enum-spec (when (contains? #{'< '> '<= '>=} op)
                       (enum-spec-for-exprs ctx [left right]))
           jsonb-cmp? (and (contains? #{'= 'not=} op)
                           (or (jsonb-column? ctx left)
                               (jsonb-column? ctx right)))
           [left right] (coerce-comparison-operands ctx left right)
           ;; Each side is either an AST node (translate-expr-bound) or
           ;; a pre-resolved typed Clojure value from coerce-unknown.
           l (if (instance? net.sf.jsqlparser.expression.Expression left)
               (translate-expr ctx left) left)
           r (if (instance? net.sf.jsqlparser.expression.Expression right)
               (translate-expr ctx right) right)
           l (if (seq? l) (ctx/materialize-arg! ctx l) l)
           r (if (seq? r) (ctx/materialize-arg! ctx r) r)
           guards (ctx/null-guard-clauses ctx [l r])
           enum-cmp-param
           (when enum-spec
             (let [rank (zipmap (:values enum-spec) (range))
                   cmp (case op < < > > <= <= >= >=)
                   p (symbol (str "?enum-cmp-" (swap! (:var-counter ctx) inc)))]
               (swap! (:in-params ctx) conj p)
               (swap! (:in-args ctx) conj
                      (fn [a b]
                        (cmp (get rank (str a) Long/MAX_VALUE)
                             (get rank (str b) Long/MAX_VALUE))))
               p))]
       (conj guards
             (cond
               enum-cmp-param
               [(list enum-cmp-param l r)]
               (and jsonb-cmp? (= op '=))
               [(list 'datahike.pg.sql/jsonb-eq? l r)]
               jsonb-cmp?
               [(list 'datahike.pg.sql/jsonb-ne? l r)]
               :else
               ;; `=` / `<>` compare numbers by VALUE across types; see
               ;; fns/sql-eq?. The ordering operators are already
               ;; cross-type in Clojure and pass through unchanged.
               [(list (case op
                        = 'datahike.pg.sql/sql-eq?
                        not= 'datahike.pg.sql/sql-ne?
                        ;; NaN sorts above everything in PostgreSQL, so
                        ;; the ordering operators need it too -- IEEE-754
                        ;; makes all four false for any NaN operand.
                        < 'datahike.pg.sql/sql-lt?
                        > 'datahike.pg.sql/sql-gt?
                        <= 'datahike.pg.sql/sql-le?
                        >= 'datahike.pg.sql/sql-ge?
                        op)
                      l r)]))))))

(declare translate-predicate translate-predicate-false)

(defn- false-sentinel?
  "The canonical \"always false\" clause produced by EXISTS / IN-subquery
   handlers when the inner query evaluates to no rows, or by an OR whose
   every branch is constant-false. `(and false …)` short-circuits, so any
   `and` containing the sentinel is itself constant-false."
  [form]
  (cond
    (and (vector? form) (= 1 (count form)) (= '(not= 1 1) (first form)))
    true
    (and (seq? form) (= 'and (first form)))
    (some false-sentinel? (rest form))
    :else false))

(defn- combine-disjuncts
  "Combine per-branch clause vectors into a single OR clause form.

   Shared by the `OrExpression` branch of `translate-predicate` and the
   `AndExpression` branch of `translate-predicate-false` — De Morgan makes
   `NOT (a AND b)` a disjunction, so both need the same construction."
  [ctx branch-clauses]
  (let [;; translate-predicate returns a *vector of clauses* implicitly
        ;; ANDed; wrap it back into a single form for OR composition.
        ;; Empty vector = "no constraint" = always-true.
        mk-branch    (fn [cs]
                       (cond
                         (empty? cs)      ::always-true
                         (= 1 (count cs)) (first cs)
                         :else            (concat ['and] cs)))
        all-branches (mapv mk-branch branch-clauses)]
    (cond
      ;; Any branch always-true → OR(true, x) = true → no constraint.
      ;; Returning [] means the caller (the surrounding AND) skips it.
      (some #(= ::always-true %) all-branches)
      []

      ;; Drop constant-false branches. OR(false, x) = x.
      :else
      (let [live-branches (vec (remove false-sentinel? all-branches))]
        (cond
          ;; All branches false → OR is false. Emit one canonical
          ;; false-sentinel; the surrounding AND short-circuits and the
          ;; query returns no rows.
          (empty? live-branches)
          [[(list 'not= 1 1)]]

          ;; Single live branch → unwrap to flat clauses so the caller
          ;; keeps its vec-of-clauses shape.
          (= 1 (count live-branches))
          (let [b (first live-branches)]
            (cond
              (and (seq? b) (= 'and (first b))) (vec (rest b))
              (vector? b)                       [b]
              :else                             [b]))

          ;; Several live branches → emit OR. shared-vars =
          ;; (branch-vars ∩ outer-bound-vars). Datomic / legacy-engine
          ;; semantics: shared-vars are the bridge between branches and
          ;; the outer query (limit-context projects each branch's result
          ;; to these). Branch-locals (e.g. the ?c1 introduced inside a
          ;; correlated EXISTS subquery) must stay out of shared-vars or
          ;; the post-projection `limit-rel` mismatches across branches.
          ;; Empty intersection → use plain `or`.
          :else
          (let [branch-vars (apply set/union (map ctx/collect-vars live-branches))
                outer-vars  (ctx/collect-vars @(:where-clauses ctx))
                shared-vars (vec (sort-by str (set/intersection branch-vars outer-vars)))]
            [(if (seq shared-vars)
               (concat ['or-join shared-vars] live-branches)
               (concat ['or] live-branches))]))))))

(defn- ground-true?
  "Evaluate a variable-free clause form and say whether it is TRUE.

   Truthiness, except that the `:__null__` sentinel is NOT true -- it is
   UNKNOWN, which a qual rejects. `re-find` and friends answer with a
   match rather than `true`, so a bare `true?` would misread them."
  [form]
  (let [v (interpret-form form {})]
    (and (some? v) (not= :__null__ v) (not (false? v)))))

(defn- negated-clauses
  "Emit `(not <clauses>)` -- or, when the clauses mention no variables,
   decide the negation NOW and answer with a constant.

   A datalog `not` is a NOT-JOIN and needs variables to join on, so a
   ground body raises \"Join variables should not be empty\". That is
   reachable from any predicate whose operands are all literals:
   `NOT (1 = 1)`, `'aa' NOT LIKE 'a%'`."
  [clauses]
  (if (empty? (ctx/collect-vars clauses))
    (if (every? (comp ground-true? first) clauses)
      [[(list 'not= 1 1)]]   ; body is TRUE  -> its negation matches nothing
      [])                    ; body is FALSE -> its negation matches every row
    [(concat ['not] clauses)]))

(defn- two-valued-predicate?
  "True when the expression can only be TRUE or FALSE, never UNKNOWN.

   `NOT` over such an expression is a plain set complement, so no null
   guards are owed — and emitting them would be actively wrong:
   `NOT (a IS NOT NULL)` must keep exactly the rows a guard would remove."
  [e]
  (cond
    (nil? e) true
    (instance? Parenthesis e) (two-valued-predicate? (.getExpression ^Parenthesis e))
    (and (instance? ParenthesedExpressionList e)
         (= 1 (count ^ParenthesedExpressionList e)))
    (two-valued-predicate? (first ^ParenthesedExpressionList e))
    (instance? IsNullExpression e) true
    (instance? IsBooleanExpression e) true
    (instance? IsUnknownExpression e) true
    (instance? IsDistinctExpression e) true
    (instance? ExistsExpression e) true
    (instance? NotExpression e) (two-valued-predicate? (.getExpression ^NotExpression e))
    (instance? AndExpression e)
    (let [^AndExpression a e]
      (and (two-valued-predicate? (.getLeftExpression a))
           (two-valued-predicate? (.getRightExpression a))))
    (instance? OrExpression e)
    (let [^OrExpression o e]
      (and (two-valued-predicate? (.getLeftExpression o))
           (two-valued-predicate? (.getRightExpression o))))
    :else false))

(def ^:private op->may-kw
  "Binary comparison predicates that `sql-may?` can express."
  {'datahike.pg.sql/sql-eq? :eq
   'datahike.pg.sql/sql-ne? :ne
   'datahike.pg.sql/sql-lt? :lt
   'datahike.pg.sql/sql-gt? :gt
   'datahike.pg.sql/sql-le? :le
   'datahike.pg.sql/sql-ge? :ge})

(defn- may-clause
  "M(φ) -- \"φ is TRUE or UNKNOWN\" -- as a single clause, or nil when the
   translated φ has no such form.

   Recognises the shape `null-guards + one binary comparison`. The guards
   are DROPPED on purpose: they assert the operands are non-NULL, which is
   exactly the case M is meant to also admit."
  [clauses]
  (let [guard? (fn [c] (and (vector? c) (= 1 (count c)) (seq? (first c))
                            (= 'not= (ffirst c))
                            (contains? #{:__null__ nil} (nth (first c) 2 ::none))))
        body   (remove guard? clauses)]
    (when (= 1 (count body))
      (let [c (first body)]
        (when (and (vector? c) (= 1 (count c)) (seq? (first c)))
          (let [[op l r] (first c)]
            (when (and (= 3 (count (first c))) (op->may-kw op))
              [(list 'datahike.pg.sql/sql-may? (op->may-kw op) l r)])))))))

(defn- conjunct-spine
  "Flatten an AND spine (through parentheses) into its conjuncts."
  [e]
  (cond
    (instance? Parenthesis e) (conjunct-spine (.getExpression ^Parenthesis e))
    (and (instance? ParenthesedExpressionList e)
         (= 1 (count ^ParenthesedExpressionList e)))
    (conjunct-spine (first ^ParenthesedExpressionList e))
    (instance? AndExpression e)
    (let [^AndExpression a e]
      (into (conjunct-spine (.getLeftExpression a))
            (conjunct-spine (.getRightExpression a))))
    :else [e]))

(defn translate-predicate-false
  "F(φ): the clauses selecting the rows where φ evaluates to **FALSE**.

   SQL's `NOT φ` is TRUE exactly where φ is FALSE — not merely where φ
   \"is not TRUE\". A datalog `(not <goal>)` is set complement, so it keeps
   the FALSE rows *and* the UNKNOWN rows; that UNKNOWN set is precisely
   where we used to diverge from PostgreSQL. So NOT descends into φ and
   builds its false-set directly:

     F(a AND b) = F(a) OR F(b)
     F(a OR b)  = F(a) AND F(b)
     F(NOT a)   = T(a)
     F(atom)    = every operand IS NOT NULL, AND (not atom)

   The leaf rule is where UNKNOWN gets dropped: an atom with a NULL
   operand is UNKNOWN, hence not FALSE, hence not in F. Note this is not
   NNF/De Morgan rewriting — the tree is walked once and a single `not`
   is emitted per leaf, which measured ~3x cheaper than pushing negation
   down to the leaves and re-planning the resulting positive form."
  [ctx expr]
  (cond
    (instance? Parenthesis expr)
    (translate-predicate-false ctx (.getExpression ^Parenthesis expr))

    (and (instance? ParenthesedExpressionList expr)
         (= 1 (count ^ParenthesedExpressionList expr)))
    (translate-predicate-false ctx (first ^ParenthesedExpressionList expr))

    (instance? AndExpression expr)
    ;; A conjunction is FALSE exactly when it is not (TRUE or UNKNOWN), so
    ;; when every conjunct has an M-form the whole thing collapses to ONE
    ;; negation -- same plan shape, and ~2x cheaper than the De Morgan
    ;; disjunction of per-conjunct negations below. Attempt it against a
    ;; ctx snapshot: translating is side-effecting, so a partial attempt
    ;; has to be rolled back before the general path re-translates.
    (let [snap (ctx/snapshot ctx)
          mays (reduce (fn [acc c]
                         (if-let [m (may-clause (translate-predicate ctx c))]
                           (conj acc m)
                           (reduced nil)))
                       [] (conjunct-spine expr))]
      (if mays
        ;; Ground conjunction (`NOT (0 = -1 AND 1 <= 2.5)`) -- no variables to
        ;; NOT-JOIN on, same problem as the ground atom below.
        (negated-clauses mays)
        (do (ctx/restore! ctx snap)
            (let [^AndExpression e expr]
              (combine-disjuncts ctx [(translate-predicate-false ctx (.getLeftExpression e))
                                      (translate-predicate-false ctx (.getRightExpression e))])))))

    (instance? OrExpression expr)
    (let [^OrExpression e expr]
      (into (translate-predicate-false ctx (.getLeftExpression e))
            (translate-predicate-false ctx (.getRightExpression e))))

    (instance? NotExpression expr)
    (translate-predicate ctx (.getExpression ^NotExpression expr))

    :else
    (let [inner (translate-predicate ctx expr)]
      (cond
        ;; φ is unconstrained (always TRUE) → never FALSE.
        (empty? inner) [[(list 'not= 1 1)]]
        ;; φ is constant-false → F(φ) is every row.
        (every? false-sentinel? inner) []

        ;; No variables anywhere in φ -- `NOT (1 = 1)`, or the constant
        ;; disjunct of `NOT (i = 10 OR 2.5 <> 0)`. See `negated-clauses`.
        (empty? (ctx/collect-vars inner))
        (negated-clauses inner)

        :else
        (let [;; DERIVE the guards from the operand vars rather than
              ;; hoisting whichever ones happen to be present: the
              ;; equality path emits none, because a NULL equals nothing
              ;; and the guard is redundant right up until you negate it.
              guards (when-not (two-valued-predicate? expr)
                       (ctx/null-guard-clauses
                        ctx (into #{} (filter symbol?) (flatten (seq inner)))))]
          (conj (vec guards) (concat ['not] inner)))))))

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
    ;; Inside a disjunct, data-pattern emission is unsound (it would
    ;; constrain rows the other branch should keep) — force the
    ;; predicate paths.
    (let [^OrExpression e expr]
      (combine-disjuncts
       ctx (binding [*conjunctive-where* false]
             [(translate-predicate ctx (.getLeftExpression e))
              (translate-predicate ctx (.getRightExpression e))])))

    (row-comparison-expression? expr)
    (let [result (translate-row-comparison ctx expr)]
      [[(list 'true? result)]])

    (instance? EqualsTo expr)
    (let [^EqualsTo e expr
          left (.getLeftExpression e)
          right (.getRightExpression e)
          ;; Here as well as in translate-comparison: the index fast
          ;; paths below (bind-col-param! / bind-col-value!) never reach
          ;; it, and they are exactly the shape `col = <literal>` takes.
          _ (check-comparison-types! ctx '= left right)]
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
                                          (for [v non-null-vals] [(list 'datahike.pg.sql/sql-eq? col v)]))
                                  (concat ['or]
                                          (for [v non-null-vals] [(list 'datahike.pg.sql/sql-eq? col v)])))]
                  [in-clause])))

            ;; Literal ALL — AND of per-element equalities.
            (and (= kind "all") array-elements)
            (if (empty? array-elements)
              []  ;; x = ALL(<empty>) is TRUE per PG
              (let [col (translate-expr ctx left)]
                (mapv (fn [v] [(list 'datahike.pg.sql/sql-eq? col v)]) array-elements)))

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
        (if (and *conjunctive-where*
                 (instance? Column left)
                 (nil? (.getArrayConstructor ^Column left))
                 (instance? JdbcParameter right))
          ;; col = $N in a top-level conjunct: index-seekable data
          ;; pattern with the :in-bound param var (Parse time), or the
          ;; value-bound pattern when *bound-params* already inlined the
          ;; literal (Execute-time re-translation).
          (let [resolved (ctx/resolve-column left
                                             (:table-aliases ctx)
                                             (:default-table ctx)
                                             (:col-overrides ctx)
                                             (:derived-aliases ctx) (:ci-index ctx))
                pv (->> (translate-expr ctx right)
                        (jsonb-canonical-operand ctx resolved)
                        (column-storage-value ctx resolved))]
            (cond
              (and (symbol? pv) (ctx/bind-col-param! ctx resolved pv)) []
              (and (not (symbol? pv)) (ctx/bind-col-value! ctx resolved pv)) []
              :else
              ;; Fallback: classic get-else + equality (repeated
              ;; translate-expr on the same JdbcParameter returns the
              ;; cached ?pN without duplicating :in-args).
              (let [v (ctx/col-var! ctx resolved)
                    guards (ctx/null-guard-clauses ctx [v])]
                (conj guards [(list 'datahike.pg.sql/sql-eq? v pv)]))))
          (if (and (instance? Column left)
                   (nil? (.getArrayConstructor ^Column left))
                   (or (instance? LongValue right)
                       (instance? DoubleValue right)
                       (instance? StringValue right)))
            (let [resolved (ctx/resolve-column left
                                               (:table-aliases ctx)
                                               (:default-table ctx)
                                               (:col-overrides ctx)
                                               (:derived-aliases ctx) (:ci-index ctx))
                ;; PG-style unknown-literal coercion: `<typed-col> = '<lit>'`
                ;; routes the literal through the column's typinput when
                ;; it parses cleanly (oidin/int8in/numericin/boolin/…).
                ;; See coerce/coerce-unknown for the dispatch.
                  coerced (coerce-unknown-literal ctx left right)
                  val (->> (or coerced (translate-expr ctx right))
                           (jsonb-canonical-operand ctx resolved)
                           (column-storage-value ctx resolved))]
              (cond
                (and (vector? resolved) (= :db-id (first resolved)))
              ;; db_id = N → bind entity var
                (let [evar (ctx/entity-var! ctx (second resolved))]
                  [[(list '= evar val)]])
              ;; Top-level conjunct on a plain column with a
              ;; matching-typed constant → value-bound data pattern
              ;; [?e :attr v]: an indexable clause instead of a
              ;; get-else scan + equality predicate over every row.
                ;; jsonb `=` compares VALUES, and PostgreSQL's is
                ;; numeric-scale insensitive, so it cannot be a datom
                ;; pattern or a plain `=` on the canonical text — those
                ;; answer false for `1.00` vs `1`. Emit the structural
                ;; predicate instead; it fast-paths on text equality, and
                ;; nothing is lost by skipping the value-bound pattern
                ;; because jsonb attributes are never `:db/index`ed.
                (jsonb-column? ctx left)
                (let [v (ctx/col-var! ctx resolved)]
                  [[(list 'datahike.pg.sql/jsonb-eq? v val)]])

                (and *conjunctive-where* (ctx/bind-col-value! ctx resolved val))
                []
              ;; Regular column = value (including aliased columns)
                :else
                (let [v (ctx/col-var! ctx resolved)]
                  [[(list 'datahike.pg.sql/sql-eq? v val)]])))
            (translate-comparison ctx '= (.getLeftExpression e) (.getRightExpression e))))))

    (instance? NotEqualsTo expr)
    (let [^NotEqualsTo e expr
          left (.getLeftExpression e)
          right (.getRightExpression e)
          _ (check-comparison-types! ctx 'not= left right)]
      ;; Special case: col <> ALL(ARRAY[...]) → translate as NOT IN (same
      ;; semantics), which keeps the O(1) set predicate for a literal list.
      ;; A RUNTIME array (a column, a function result) and the `<> ANY` form
      ;; have no literal list to build, and both fell through to a plain
      ;; comparison against the Function node -- `2 <> ALL(arr)` compared the
      ;; number to the call itself and matched the wrong rows.
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
            ;; Runtime array expression: no literal list to build, so use the
            ;; value-position translation and collapse it at the qual.
              (let [v (translate-predicate-expr ctx expr)
                    v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
                [[(list 'true? v)]]))))
        (if (and (instance? Function right)
                 (= "any" (str/lower-case (.getName ^Function right))))
          (let [v (translate-predicate-expr ctx expr)
                v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
            [[(list 'true? v)]])
          (translate-comparison ctx 'not= left right))))

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
          lo-ast (.getBetweenExpressionStart e)
          hi-ast (.getBetweenExpressionEnd e)
          _ (check-comparison-types! ctx '>= left-ast lo-ast)
          _ (check-comparison-types! ctx '<= left-ast hi-ast)
          col (translate-expr ctx left-ast)
          ;; PG-style typinput on each bound when LHS is a typed Column
          ;; — `oid BETWEEN '16000' AND '17000'` and similar.
          coerce-bound (fn [bound-ast]
                         (or (when (instance? Column left-ast)
                               (coerce-unknown-literal ctx left-ast bound-ast))
                             (translate-expr ctx bound-ast)))
          lo (coerce-bound lo-ast)
          hi (coerce-bound hi-ast)]
      ;; One predicate per form, not a disjunction and not a pair of
      ;; comparisons. `(or [(< col lo)] [(> col hi)])` binds different var
      ;; sets in its two branches, which datalog rejects the moment lo and
      ;; hi are different variables (`id NOT BETWEEN 0 AND i`). And the
      ;; comparisons go through sql-le?, which orders NaN as PostgreSQL
      ;; does; clojure.core's `<=` does not.
      ;; NO null-guards: the predicates decide NULL themselves, under the
      ;; Kleene AND. Guarding all three operands non-NULL first would drop
      ;; the rows where `1 NOT BETWEEN 3 AND NULL` is TRUE.
      [[(list (if not-between?
                'datahike.pg.sql/sql-not-between?
                'datahike.pg.sql/sql-between?)
              col lo hi)]])

    (instance? IsNullExpression expr)
    (let [^IsNullExpression e expr
          not-null? (.isNot e)
          inner (.getLeftExpression e)]
      (if (and (instance? Column inner)
               (not (bare-session-value-column? inner)))
        (let [^Column col inner
              resolved (ctx/resolve-column col
                                           (:table-aliases ctx)
                                           (:default-table ctx)
                                           (:col-overrides ctx)
                                           (:derived-aliases ctx) (:ci-index ctx))]
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
                    [(nth resolved 1) (ctx/attr-of ctx resolved)]
                    :else
                    ;; INHERITS: this branch did no resolution at all, so
                    ;; `WHERE inherited_col IS NULL` was inverted — the
                    ;; child-namespace attr is always absent, making the
                    ;; test true for every row.
                    [(namespace resolved) (ctx/attr-of ctx resolved)])
                  _ (ctx/validate-column! ctx kw)
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
        ;; Non-column IS NULL — e.g. `(p->'k') IS NULL`.
        ;;
        ;; SQL NULL arrives here as the `:__null__` sentinel, not as
        ;; nil: a datalog function binding that yields nil filters the
        ;; row, so every fn that can produce NULL returns the sentinel
        ;; instead. Testing `nil?` therefore answered FALSE for a value
        ;; that IS SQL NULL — `p->'missing' IS NULL` said false where
        ;; PostgreSQL says true. Accept both.
        (let [v (translate-expr ctx inner)
              v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
          (if not-null?
            [[(list 'datahike.pg.sql/sql-not-null? v)]]
            [[(list 'datahike.pg.sql/sql-null? v)]]))))

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

    ;; col IS [NOT] UNKNOWN — for a boolean, UNKNOWN is exactly NULL.
    (instance? IsUnknownExpression expr)
    (let [^IsUnknownExpression e expr
          col (translate-expr ctx (.getLeftExpression e))
          col (if (seq? col) (ctx/materialize-arg! ctx col) col)]
      (if (.isNot e)
        [[(list 'datahike.pg.sql/sql-not-null? col)]]
        [[(list 'datahike.pg.sql/sql-null? col)]]))

    ;; a IS [NOT] DISTINCT FROM b — the NULL-aware `<>`.
    (instance? IsDistinctExpression expr)
    (let [^IsDistinctExpression e expr
          _ (check-comparison-types! ctx '= (.getLeftExpression e) (.getRightExpression e))
          l (translate-expr ctx (.getLeftExpression e))
          l (if (seq? l) (ctx/materialize-arg! ctx l) l)
          r (translate-expr ctx (.getRightExpression e))
          r (if (seq? r) (ctx/materialize-arg! ctx r) r)]
      [[(list (if (.isNot e)
                'datahike.pg.sql/sql-not-distinct?
                'datahike.pg.sql/sql-distinct?)
              l r)]])

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
            (into guards (negated-clauses [pred]))
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
        ;; `NOT φ` selects exactly the rows where φ is FALSE. Under
        ;; negation a data pattern is unsound (it would constrain the
        ;; outer query, not the negated branch) — predicate paths only.
        (binding [*conjunctive-where* false]
          (translate-predicate-false ctx inner-expr))))

    (instance? InExpression expr)
    (let [^InExpression e expr
          not-in? (.isNot e)
          left-ast (.getLeftExpression e)
          left-asts (in-left-asts left-ast)
          row-in? (> (count left-asts) 1)
          left-oids (mapv #(operand-type-oid ctx %) left-asts)
          left-values (mapv #(translate-expr ctx %) left-asts)
          col (first left-values)
          right (.getRightExpression e)
          inner (when (instance? ParenthesedSelect right)
                  (.getSelect ^ParenthesedSelect right))
          corr-refs (when inner
                      (correlated-subquery-refs inner (outer-alias-set ctx)))
          _ (when inner
              (analyze-in-subquery! ctx left-asts inner corr-refs))
          subquery-values-var (when (and inner (not row-in?) (not (seq corr-refs)))
                                (uncorrelated-in-values-var! ctx inner))
          subquery-rows-var (when (and inner row-in? (not (seq corr-refs)))
                              (uncorrelated-in-rows-var! ctx inner left-oids))
          _ (when (and row-in? (not inner))
              (throw (errors/pg-error
                      :feature-not-supported
                      {:message "row-valued IN with a literal row list is not implemented"})))
          ;; PG-style typinput: when the LHS is a typed Column, route
          ;; each unknown StringValue in the IN-list through the
          ;; column's typinput. Mirrors `c.oid IN ('16384','16385')`
          ;; from pgjdbc's getColumns probe.
          translate-in-elem (fn [el]
                              (check-comparison-types! ctx '= left-ast el)
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
                 nil

                 :else
                 (throw (ex-info "unsupported IN expression form"
                                 {:error :feature-not-supported
                                  :feature (str "IN with right-hand of type "
                                                (.getName ^Class (type right)))
                                  :expr (str right)})))
          ;; A NULL in the list is nil from a LITERAL list but the
          ;; `:__null__` sentinel from a SUBQUERY, and only nil was being
          ;; filtered. So the sentinel stayed in the membership set and
          ;; `WHERE i IN (SELECT k …)` matched the rows where i IS NULL
          ;; against the subquery's NULL; and the `NOT IN` NULL rule below
          ;; never fired for a subquery, so `i NOT IN (SELECT k …)`
          ;; returned rows where PostgreSQL returns none.
          null-val?     (fn [v] (or (nil? v) (= :__null__ v)))
          non-null-vals (filterv (complement null-val?) vals)
          has-null-val? (boolean (some null-val? vals))
          ;; Detect parameterised values — JdbcParameter substitution
          ;; emits `?pN` symbols. A `(contains? #{?p1} ?col)` clause
          ;; would compare ?col against the literal var, never matching.
          ;; For these, fall back to per-value equality which Datahike
          ;; resolves param vars correctly. Pure-literal lists keep
          ;; using the O(1) hash-set predicate.
          has-param? (some symbol? non-null-vals)
          guards (ctx/null-guard-clauses ctx [col])]
      (if inner
        (let [result-var (cond
                           (and row-in? (seq corr-refs))
                           (correlated-row-in-var! ctx left-values left-oids
                                                   inner corr-refs not-in?)

                           row-in?
                           (row-in-result-var! ctx subquery-rows-var left-values not-in?)

                           (seq corr-refs)
                           (correlated-in-var! ctx col inner corr-refs not-in?)

                           :else
                           (let [base (list 'datahike.pg.sql/sql-in3?
                                            subquery-values-var col)
                                 form (if not-in?
                                        (list 'datahike.pg.sql/sql-not3
                                              (ctx/materialize-arg! ctx base))
                                        base)]
                             (ctx/materialize-arg! ctx form)))]
          ;; WHERE admits only TRUE. SQL UNKNOWN is a truthy sentinel in
          ;; Clojure, so an identity predicate would incorrectly keep it.
          [[(list 'true? result-var)]])
        (cond
        ;; NOT IN with NULL in the list → always empty (SQL standard):
        ;; `x NOT IN (…, NULL)` is UNKNOWN for every x that matches
        ;; nothing else, and FALSE for one that does.
          (and not-in? has-null-val?)
          [[(list 'not= col col)]]

        ;; NOT IN with empty list → all rows match (everything is NOT IN {})
          (and not-in? (empty? non-null-vals))
          []

        ;; NOT IN with parameterised values — emit per-value not= guards
        ;; (one parameter per row of the list). Datahike conjoins them:
        ;; col matches NOT IN iff every (not= col p_i) holds.
          (and not-in? has-param?)
          (into guards
                (mapv (fn [v] [(list 'datahike.pg.sql/sql-ne? col v)]) non-null-vals))

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
                                 (list 'and [(list 'datahike.pg.sql/sql-eq? col v)]))
                               non-null-vals)]
            (conj guards (apply list 'or-join shared-vars branches)))

        ;; IN — set-based predicate (O(1) per row)
        ;; `(contains? #{...} :__null__)` returns false, so positive IN is already null-safe.
          :else
          (let [val-set (set non-null-vals)]
            [[(list 'datahike.pg.sql/sql-in? val-set col)]]))))

    ;; EXISTS / NOT EXISTS subquery
    (instance? ExistsExpression expr)
    (let [^ExistsExpression e expr
          not-exists? (.isNot e)
          sub-select (.getRightExpression e)
          inner (cond
                  (instance? ParenthesedSelect sub-select)
                  (.getSelect ^ParenthesedSelect sub-select)
                  (instance? PlainSelect sub-select) sub-select
                  :else nil)
          corr-refs (when inner
                      (correlated-subquery-refs inner (outer-alias-set ctx)))
          _ (when (seq corr-refs)
              (analyze-exists-subquery! ctx inner corr-refs))
          _ (when (and (seq corr-refs) (nested-subquery-body? inner))
              (throw (errors/pg-error
                      :feature-not-supported
                      {:message "correlated EXISTS containing nested subqueries is not implemented"})))]
      (if-let [db (:db ctx)]
        ;; Try correlated EXISTS first: translate inner query to Datalog patterns
        ;; and use not-join/or-join for correlation with the outer query.
        ;; Uncorrelated forms can fall back to standalone execution; a failed
        ;; correlated lowering is reported explicitly rather than misread.
        (let [schema (:schema ctx)
              outer-aliases (:table-aliases ctx)
              snapshot (ctx/snapshot ctx)]
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
                      inner-ctx (assoc (ctx/make-ctx schema combined-aliases inner-alias
                                                     {:db db :parse-sql (:parse-sql ctx)})
                                       ;; The inner translation is part of the same
                                       ;; prepared statement.  Sharing these slots
                                       ;; makes repeated `$1` references reuse ?p1
                                       ;; and keeps generated function parameters
                                       ;; globally fresh across both scopes.
                                       :var-counter (:var-counter ctx)
                                       :in-params (:in-params ctx)
                                       :in-args (:in-args ctx)
                                       :param-placeholders (:param-placeholders ctx))
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
                                          (and (seq? c)
                                               (contains? #{'= 'datahike.pg.sql/sql-eq?}
                                                          (first c))
                                               (= 3 (count c)))
                                          ;; Wrapped form: [(= ?a ?b)]
                                          (and (vector? c) (= 1 (count c))
                                               (seq? (first c))
                                               (contains? #{'= 'datahike.pg.sql/sql-eq?}
                                                          (first (first c)))
                                               (= 3 (count (first c))))))
                                       new-clauses)
                      ;; For each equality predicate, check if one var is bound
                      ;; by a data pattern that uses an outer entity var.
                      ;; E.g. [(= ?d_module_id ?m_id)] where ?m_id comes from
                      ;; [?m_eid :mod/id ?m_id] (outer entity ?m_eid)
                      ;;
                      ;; Build a map: var → {:clauses […] :evar … :attr …}.
                      ;;
                      ;; Two clause shapes bind a var to an entity+attr:
                      ;; - Plain data pattern: `[?e :attr ?v]` (3 elems).
                      ;; - NULL-safe get-else:  `[(get-else $ ?e :attr
                      ;;   :__null__) ?v]` (2 elems: function-call +
                      ;;   result var). Emitted by ctx/col-var! for any
                      ;;   nullable attribute post-B2.
                      ;;
                      ;; Downstream correlation-rewriting needs the evar (to
                      ;; decide if an outer binding drives it) and all source
                      ;; clauses (to relocate them into the outer context).
                      ;; Keep both so get-else and decoded values are handled
                      ;; uniformly.
                      direct-var-sources
                      (into {}
                            (keep (fn [c]
                                    (cond
                                      (and (vector? c) (= 3 (count c))
                                           (symbol? (first c))
                                           (keyword? (second c))
                                           (symbol? (nth c 2)))
                                      [(nth c 2)
                                       {:clauses [c] :evar (first c) :attr (second c)}]

                                      (and (vector? c) (= 2 (count c))
                                           (seq? (first c))
                                           (= 'get-else (ffirst c))
                                           (symbol? (second c)))
                                      (let [[_ _ evar attr _] (first c)
                                            v (second c)]
                                        (when (and (symbol? evar) (keyword? attr))
                                          [v {:clauses [c] :evar evar :attr attr}]))
                                      :else nil)))
                            new-clauses)
                      ;; NUMERIC storage decoding introduces
                      ;; `[(?column-decode ?raw) ?sql-value]`. Carry the
                      ;; raw datom's source through that binding so the
                      ;; correlation planner can compare declared storage
                      ;; types and relocate the complete outer binding.
                      inner-var-sources
                      (reduce (fn [sources c]
                                (if (and (vector? c) (= 2 (count c))
                                         (seq? (first c))
                                         (symbol? (ffirst c))
                                         (str/starts-with? (name (ffirst c)) "?column-decode")
                                         (symbol? (second c)))
                                  (let [raw (second (first c))]
                                    (if-let [source (sources raw)]
                                      (assoc sources (second c)
                                             (update source :clauses conj c))
                                      sources))
                                  sources))
                              direct-var-sources
                              new-clauses)
                      numeric-vtypes #{:db.type/long :db.type/double
                                       :db.type/float :db.type/bigdec}
                      storage-unifiable?
                      (fn [eq-clause left-source right-source]
                        (let [op (first (if (seq? eq-clause)
                                          eq-clause
                                          (first eq-clause)))
                              lv (get-in schema [(:attr left-source) :db/valueType])
                              rv (get-in schema [(:attr right-source) :db/valueType])
                              floating-vtypes #{:db.type/float :db.type/double}]
                          ;; Raw `=` is emitted only where Datalog value
                          ;; unification is already the intended comparison.
                          ;; sql-eq? additionally performs PostgreSQL numeric
                          ;; promotion and NaN equality. It must remain an
                          ;; explicit predicate for cross-storage numerics and
                          ;; floating columns (where NaN = NaN is true in SQL).
                          (or (= '= op)
                              (and (not (floating-vtypes lv))
                                   (not (floating-vtypes rv))
                                   (not (and lv rv (not= lv rv)
                                             (numeric-vtypes lv)
                                             (numeric-vtypes rv)))))))
                      correlations
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
                                   :outer-source (inner-var-sources v2)
                                   :inner-source (inner-var-sources v1)
                                   :outer-patterns (:clauses (inner-var-sources v2))}
                                  (and (symbol? v1) (outer-bound? v1) (symbol? v2))
                                  {:eq-clause eq-clause :outer-var v1 :inner-var v2
                                   :outer-source (inner-var-sources v1)
                                   :inner-source (inner-var-sources v2)
                                   :outer-patterns (:clauses (inner-var-sources v1))}
                                  :else nil)))
                            eq-preds)
                      correlation-rewrites
                      (filter (fn [{:keys [eq-clause outer-source inner-source]}]
                                (and inner-source
                                     (storage-unifiable? eq-clause outer-source inner-source)))
                              correlations)
                      ;; Move outer-bound patterns outside the not-join
                      patterns-to-move (set (mapcat :outer-patterns correlations))
                      ;; Move these patterns to the outer ctx
                      _ (doseq [pat patterns-to-move]
                          (swap! (:where-clauses ctx) conj pat))
                      ;; Correlation vars: use the VALUE vars, not entity vars
                      corr-value-vars (set (keep :outer-var correlations))
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
                                           (not (outer-evars (:evar s2)))
                                           (storage-unifiable? eq-clause s1 s2))
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
                      all-inner-vars (set (mapcat ctx/collect-vars optimized-clauses))
                      entity-corr-vars (set/intersection outer-evars all-inner-vars)
                      ;; A projected/filter-bound outer column is already in
                      ;; col->var, so the fresh inner ctx reuses its var but
                      ;; does not repeat its source pattern. Detect that shared
                      ;; value directly in the optimized clauses as well.
                      existing-value-corr-vars (set/intersection outer-col-vars all-inner-vars)
                      corr-vars (vec (set/union corr-value-vars
                                                existing-value-corr-vars
                                                entity-corr-vars))]
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
                ;; Translation is side-effecting. Roll back every partial
                ;; clause/input before reporting the unsupported shape;
                ;; otherwise leaked inner vars corrupt later translation.
              (ctx/restore! ctx snapshot)
              (if (seq corr-refs)
                (throw (errors/pg-error
                        :feature-not-supported
                        {:message "this correlated EXISTS shape is not implemented"}))
                (throw _ex)))))
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

    ;; A bare `d->'a'` standing alone as a WHERE predicate.
    ;;
    ;; This branch used to also carry ~110 lines that decomposed a
    ;; comparison folded into the JsonExpression's ident list, on the
    ;; premise recorded in its comment that "JSqlParser folds the
    ;; comparison into the JsonExpression's ident list". jsqlparser 5.2
    ;; does no such thing — `d->>'k' = 'x'` parses as an EqualsTo whose
    ;; LEFT is the JsonExpression, so it reaches translate-expr through
    ;; the comparison branch and never arrives here. Verified for `=`,
    ;; `<>`, `>`, `>=`, `<`, `<=`, LIKE and IS NULL, and for chained
    ;; access: the last ident of a JsonExpression is always a value or a
    ;; nested JsonExpression, never a comparison. The guard could not
    ;; fire, and the code behind it had a latent bug (it reused the LAST
    ;; key for every step of a chain) that no test ever saw.
    (instance? JsonExpression expr)
    (let [v (translate-expr ctx expr)]
      [[(list 'identity v)]])

    ;; Containment / overlap / existence: @>, <@, &&, ?, ?|, ?&
    ;;
    ;; Delegated to the value-position translation rather than re-deciding
    ;; here. That copy knew only the JSONB implementations, so `arr @>
    ;; ARRAY[1]` matched nothing against an array column, and `&&`
    ;; (DoubleAnd, a different AST class) was not handled at all -- it
    ;; reached the catch-all and raised "WHERE expression of type ... is not
    ;; supported".
    (or (instance? JsonOperator expr) (instance? DoubleAnd expr))
    (let [v (translate-expr ctx expr)
          v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
      ;; `true?`, not `identity`: these are three-valued (NULL operand ->
      ;; NULL), and the `:__null__` sentinel is truthy in a datalog
      ;; predicate, so `identity` would let the NULL rows through.
      [[(list 'true? v)]])

    ;; Bare column as boolean predicate: WHERE col_name means WHERE col_name = TRUE
    (instance? Column expr)
    (let [^Column col expr
          table-name (some-> (.getTable col) .getName unquote-ident)]
      (if (and table-name params/*from-bindings*
               (contains? params/*from-bindings* table-name))
        ;; A correlated outer boolean arrives as a concrete per-row binding,
        ;; not as an inner-relation attribute. Resolving it through ctx made
        ;; the inner query scan a nonexistent `:<outer>/col` datom, so psql's
        ;; `... AND a.atthasdef` scalar subquery always returned NULL.
        (if (true? (get-in params/*from-bindings*
                           [table-name (unquote-ident (.getColumnName col))]))
          []
          [[(list 'not= 1 1)]])
        (let [resolved (ctx/resolve-column col
                                           (:table-aliases ctx)
                                           (:default-table ctx)
                                           (:col-overrides ctx)
                                           (:derived-aliases ctx) (:ci-index ctx))
              col-var (ctx/col-var! ctx resolved)]
          [[(list '= col-var true)]])))

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
