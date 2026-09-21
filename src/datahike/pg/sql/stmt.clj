(ns datahike.pg.sql.stmt
  "Statement-level translation: SELECT / INSERT / UPDATE / DELETE / CTE.

   The top half of this namespace (translate-select + its join /
   HAVING / materialization machinery) maps a PlainSelect AST to a
   Datalog query-map plus row-formatting metadata the handler uses
   at execute time.

   The middle half handles INSERT / UPDATE / DELETE: `extract-value`
   lifts JSqlParser literal/expression nodes into Clojure values,
   `coerce-insert-value` adapts them to the target column's
   :db/valueType, and `translate-insert` / `translate-update` /
   `translate-delete` produce tx-data + (for UPDATE/DELETE) an
   eids-walk query. INSERT RETURNING and UPDATE RETURNING land in
   `extract-returning`; UPDATE expressions evaluated per row at handler
   time go through the SELECT translator. CHECK constraints are evaluated
   by datahike.pg.sql.row-eval.

   The bottom half implements CTEs: `translate-cte-branch`
   materializes one WITH-clause body against an enriched db so the
   outer SELECT / DML can reference it as a virtual table;
   `translate-recursive-cte` handles the WITH RECURSIVE form by
   iterating until the CTE converges.

   The three blocks live in one namespace because they are mutually
   recursive:

     translate-select → translate-recursive-cte        (WITH in SELECT)
     translate-recursive-cte → translate-select         (CTE body)
     translate-insert → translate-select                (INSERT ... SELECT)
     translate-insert → translate-recursive-cte         (INSERT ... WITH)
     translate-select → extract-value                   (scalar subqueries)

   Dependencies on already-extracted namespaces are all one-way:

     stmt → expr (translate-expr, translate-predicate, …)
     stmt → ctx  (make-ctx, col-var!, resolve-column, …)
     stmt → fns  (aggregate lookups)
     stmt → params (ParamRef, *from-bindings*, *parse-db*)
     stmt → jsonb, schema, types   (type coercion + jsonb ops)"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [datahike.api :as d]
            [datahike.array :as dh-array]
            [datahike.datom]
            [datahike.query :as dq]
            [datahike.pg.cache :as pg-cache]
            [datahike.pg.catalog.basis :as catalog-basis]
            [datahike.pg.errors :as errors]
            [datahike.pg.input :as input]
            [datahike.pg.constraints.row :as row-constraints]
            [datahike.pg.constraints.unique :as unique-constraints]
            [datahike.pg.window :as window]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.records :as pg-rec]
            [datahike.pg.schema :as pgs]
            [datahike.pg.keywords :as pg-kw]
            [datahike.pg.sql.cast :as sql-cast]
            [datahike.pg.sql.ddl :as ddl]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.expr :as expr]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.oid-infer :as oid]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types]
            [datahike.pg.tsearch :as tsearch]
            [datahike.pg.vector :as pg-vector]
            [datahike.pg.bits :as pg-bits]
            [datahike.pg.arrays :as pg-arr])
  (:import [datahike.datom Datom]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression
            Alias Function LongValue DoubleValue StringValue NullValue
            BooleanValue Parenthesis SignedExpression CastExpression
            JsonExpression TimezoneExpression TimeKeyExpression ArrayConstructor JdbcParameter
            CaseExpression WhenClause RowConstructor
            NotExpression ExtractExpression TrimFunction ArrayExpression]
           [net.sf.jsqlparser.expression.operators.arithmetic
            Modulo BitwiseAnd BitwiseOr BitwiseXor]
           [net.sf.jsqlparser.expression.operators.relational
            GreaterThan GreaterThanEquals MinorThan MinorThanEquals
            EqualsTo NotEqualsTo IsNullExpression Between LikeExpression
            InExpression JsonOperator
            ParenthesedExpressionList]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression]
           [net.sf.jsqlparser.expression.operators.arithmetic
            Addition Subtraction Multiplication Division Concat]
           [net.sf.jsqlparser.statement.select
            PlainSelect SelectItem AllColumns AllTableColumns OrderByElement
            GroupByElement Limit Offset Join
            ParenthesedSelect ParenthesedFromItem SetOperationList
            Values FromItem]
           [net.sf.jsqlparser.statement.insert Insert]
           [net.sf.jsqlparser.statement.update Update UpdateSet]
           [net.sf.jsqlparser.statement.delete Delete]))

(set! *warn-on-reflection* true)

(defn- quote-sql-ident [value]
  (str "\"" (str/replace (str value) "\"" "\"\"") "\""))

(defn- expanded-select-item [qualifier column-name]
  (let [quoted-column (quote-sql-ident column-name)
        ^Table table (doto (Table.)
                       (.setName (quote-sql-ident qualifier)))
        ^Column column (doto (Column.)
                         (.setTable table)
                         (.setColumnName quoted-column))]
    (SelectItem. column (Alias. quoted-column true))))

(def ^:dynamic *freeze-view-stars?*
  "When true, every translated SELECT scope expands its stars in the owned
   AST. CREATE VIEW binds this while translating a cloned statement; ordinary
   SELECTs therefore neither copy nor serialize their AST."
  false)

(defn- freeze-select-stars!
  "Expand stars in one SELECT scope in place.

   PostgreSQL expands stars when a view is created. Keeping the original star
   would let later ALTER TABLE operations change its shape. Nested SELECTs are
   visited by the normal translator recursion, so mutating each scope also
   freezes derived tables and joins before the root is serialized."
  [^PlainSelect select select-items star-relations table-aliases schema db]
  (when (some (fn [^SelectItem item]
                (instance? AllColumns (.getExpression item)))
              select-items)
    (let [merged-of (or (:merged-columns-by-alias (meta table-aliases)) {})
          expand-relation
          (fn [alias real]
            ;; A merged column is emitted ONCE, with the left side's
            ;; expansion: `SELECT * FROM a JOIN b USING (id)` gives id,
            ;; a's remaining columns, then b's.
            (let [merged (set (get merged-of alias))]
              (for [column (pgs/column-info schema real db)
                    :when (and (not= "db_id" (:name column))
                               (not (contains? merged (:name column))))]
                (expanded-select-item alias (:name column)))))
          expanded
          (mapcat
           (fn [^SelectItem original]
             (let [expression (.getExpression original)]
               (cond
                 (instance? AllTableColumns expression)
                 (let [^AllTableColumns all-table expression
                       raw (some-> (.getTable all-table) .getName
                                   params/unquote-ident)
                       real (or (get table-aliases raw) raw)]
                   (expand-relation raw real))

                 (instance? AllColumns expression)
                 (mapcat (fn [[alias real]] (expand-relation alias real))
                         star-relations)

                 :else [original])))
           select-items)]
      (.setSelectItems select
                       (java.util.ArrayList. ^java.util.Collection
                        (vec expanded))))))

(defn- bare-non-integer-constant?
  "A constant in ORDER BY / GROUP BY that is not an integer. PostgreSQL
   reads a bare INTEGER constant there as a select-list ordinal and
   rejects every other bare constant with 42601 rather than sorting or
   grouping every row by the same value — see the `!IsA(&aconst->val,
   Integer)` branch of findTargetlistEntrySQL92. A compound expression
   like `1+1` is not a bare constant and stays an ordinary expression."
  [e]
  (or (instance? StringValue e)
      (instance? DoubleValue e)
      (instance? NullValue e)
      (instance? net.sf.jsqlparser.expression.DateValue e)
      (instance? net.sf.jsqlparser.expression.TimestampValue e)
      (and (instance? net.sf.jsqlparser.expression.BooleanValue e))))

(defn- exact-schema-for-grouping?
  "The 42803 check needs to know a column reference really is a column,
   which is only knowable where the schema is exhaustive — the same
   `:schema-flexibility :write` gate `ctx/validate-column!` uses, and
   for the same reason. Fails permissive, including on a FilteredDB
   (temporal queries) whose config lookup throws."
  [db]
  (= :write (try (:schema-flexibility (:config db)) (catch Throwable _ nil))))

;; Unqualified aliases so the copied body reads naturally — same
;; pattern used by ctx / ddl / catalog / expr.
(def ^:private unquote-ident params/unquote-ident)
(def ^:private ->ParamRef params/->ParamRef)

;; ---------------------------------------------------------------------------
;; Forward declarations — mutually recursive statement translators.

(declare translate-select
         translate-select*
         translate-insert
         translate-update
         translate-delete
         translate-join
         translate-cte-branch
         translate-recursive-cte
         extract-value
         strict-scalar-value
         coerce-insert-value
         extract-returning
         materialize-table-function
         materialize-derived-select!
         materialize-set-op!
         apply-compound-projections
         match-aggregate-index
         select-item-alias
         eval-values-literal
         apply-sql-cast
         parse-bytea-hex
         join-type)

(defn join-type
  "Determine the type of a JOIN: :inner, :left, :right, :full, or :cross.
   LEFT JOIN and LEFT OUTER JOIN are both :left (JSqlParser may or may not set isOuter)."
  [^Join join]
  (cond
    (.isFull join)  :full
    (.isLeft join)  :left
    (.isRight join) :right
    (.isCross join) :cross
    :else           :inner))

(defn validate-lateral-join-shapes!
  "Reject a RIGHT/FULL JOIN whose table function has a correlated column
   argument. PostgreSQL permits a LATERAL reference to the left side only for
   INNER and LEFT joins. This must run on the pristine parser AST: later
   translation passes may replace the table function while deriving schemas."
  [statement]
  (when-let [^PlainSelect select
             (cond
               (instance? PlainSelect statement) statement
               (instance? net.sf.jsqlparser.statement.select.Select statement)
               (.getPlainSelect ^net.sf.jsqlparser.statement.select.Select statement)
               :else nil)]
    (doseq [^Join j (.getJoins select)
            :when (or (.isRight j) (.isFull j))
            :let [rt (.getRightItem j)]
            :when (instance? net.sf.jsqlparser.statement.select.TableFunction rt)
            :let [tf ^net.sf.jsqlparser.statement.select.TableFunction rt
                  params (vec (or (.getParameters (.getFunction tf)) []))
                  ^Column outer-col (some #(when (instance? Column %) %) params)]
            :when outer-col]
      (let [outer-table (some-> outer-col .getTable .getName unquote-ident)]
        (throw
         (ex-info
          (str "invalid reference to FROM-clause entry for table \""
               (or outer-table "?") "\"")
          {:error :invalid-column-reference
           :sqlstate "42P10"
           :table outer-table
           :detail (str "The combining JOIN type must be INNER or LEFT "
                        "for a LATERAL reference.")}))))))

(defn- relation-column-names
  "The column names `relation` exposes, lower-cased, or nil when they
   cannot be read."
  [schema db relation]
  (when (and relation schema)
    (not-empty (into #{} (comp (map :name) (remove #(= "db_id" %)))
                     (pgs/column-info schema relation db)))))

(defn join-merged-columns
  "The columns `join` MERGES with the relations on its left: the names
   `USING (…)` lists, or every name both sides share for a NATURAL join
   (transformJoinUsingAlias / transformJoinOnClause, parse_clause.c).
   `left` is {alias relation}.

   PostgreSQL turns each into an equality between the two sides AND
   merges the pair into one output column. Neither happened here, so a
   USING or NATURAL join was a cross product -- `a JOIN b USING (id)`
   answered every pair of rows."
  [^Join join left right-relation schema db]
  (let [right-cols (relation-column-names schema db right-relation)
        named (mapv #(params/unquote-ident (str %)) (or (.getUsingColumns join) []))
        natural? (and (.isNatural join) (empty? named))
        wanted (if natural?
                 (when right-cols
                   (vec (for [[_ rel] left
                              c (or (relation-column-names schema db rel) #{})
                              :when (contains? right-cols c)]
                          c)))
                 named)]
    (vec (distinct
          (keep (fn [c]
                  (when-let [owner (some (fn [[alias rel]]
                                           (when (contains? (or (relation-column-names schema db rel) #{}) c)
                                             alias))
                                         left)]
                    {:column c :left-alias owner}))
                wanted)))))

(defn- clause-vars
  "The logic variables a Datalog clause mentions, at any depth."
  [clause]
  (into #{}
        (filter (fn [x] (and (symbol? x) (str/starts-with? (name x) "?"))))
        (tree-seq coll? seq clause)))

(defn translate-join
  "Add join clauses for a SQL JOIN to the context.
   For INNER joins with ref-based ON (a.ref_col = b.db_id), unifies the ref
   column variable with the target entity variable.
   For LEFT joins, records the ref-attr and right-table alias for later
   or-join wrapping in translate-select.
   Returns {:name str :alias str :join-type keyword :ref-attr kw :left-entity-var sym}."
  [ctx ^Join join _default-table]
  (let [right-table (.getRightItem join)
        ;; A LEFT JOIN LATERAL is INNER as far as the or-join pass is
        ;; concerned: its NULL-extended row is produced by the row
        ;; producer itself (see the `:outer?` spec), and letting the
        ;; or-join construction wrap the function-binding clause instead
        ;; raised the datalog-internal "Cannot parse rule-vars".
        jtype (if (or (instance? net.sf.jsqlparser.statement.select.LateralSubSelect right-table)
                      (instance? net.sf.jsqlparser.statement.select.TableFunction right-table))
                :inner
                (join-type join))
        {:keys [name alias]}
        (cond
          (instance? Table right-table)
          (ctx/extract-table-info ^Table right-table)
          ;; Derived table / table function — the caller already
          ;; materialized it into the db; we just pull its alias here so
          ;; that column refs on the right side resolve correctly.
          (instance? ParenthesedSelect right-table)
          (let [a (when-let [al (.getAlias ^ParenthesedSelect right-table)]
                    (unquote-ident (str/trim (.getName ^Alias al))))]
            {:name a :alias a})

          ;; A set-returning function in join / comma position. Its alias
          ;; was not pulled here, so no entity var was created for it and
          ;; the row-marker anchor pass never saw it: `SELECT count(*)
          ;; FROM t, generate_series(1,3)` answered t's row count instead
          ;; of the cross product. (A CORRELATED SRF declares no row
          ;; marker -- its rows come from a function binding -- so the
          ;; anchor pass skips it and this only registers the alias.)
          (instance? net.sf.jsqlparser.statement.select.TableFunction right-table)
          (let [a (when-let [al (.getAlias ^net.sf.jsqlparser.statement.select.TableFunction
                                 right-table)]
                    (unquote-ident (str/trim (.getName ^Alias al))))]
            {:name a :alias a}))
        right-alias (or alias name)
        ;; Multi-condition ON (A AND B AND ...) lands as a single
        ;; AndExpression, not split per `getOnExpressions`. Flatten so
        ;; each conjunct is recognised as its own EqualsTo and the
        ;; LEFT-JOIN branch records ref-info for every join key.
        ;; Without this, a multi-cond ON falls through to the generic
        ;; predicate path and emits INNER-style equality predicates,
        ;; which break LEFT JOIN semantics on empty right tables.
        ;; Recursively descend through both AndExpression and
        ;; size-1 ParenthesedExpressionList, since `ON (a AND b)` parses
        ;; as a paren-list wrapping an AndExpression and we want to
        ;; route each conjunct through the per-EqualsTo branch below.
        flatten-and (fn flatten-and [e]
                      (cond
                        (instance? net.sf.jsqlparser.expression.operators.conditional.AndExpression e)
                        (let [^net.sf.jsqlparser.expression.operators.conditional.AndExpression ae e]
                          (concat (flatten-and (.getLeftExpression ae))
                                  (flatten-and (.getRightExpression ae))))
                        (and (instance? ParenthesedExpressionList e)
                             (= 1 (.size ^ParenthesedExpressionList e)))
                        (flatten-and (.get ^ParenthesedExpressionList e 0))
                        :else [e]))
        ;; `USING (c)` and NATURAL are equalities PostgreSQL synthesises
        ;; before planning; without them the join is a cross product.
        merged (join-merged-columns join (:table-aliases ctx) name
                                    (:schema ctx) (:db ctx))
        using-on (mapv (fn [{:keys [column left-alias]}]
                         (EqualsTo. (Column. (Table. ^String left-alias) ^String column)
                                    (Column. (Table. ^String right-alias) ^String column)))
                       merged)
        on-exprs (concat (some-> (.getOnExpressions join) seq
                                 (->> (mapcat flatten-and)))
                         using-on)
        ;; Track ref-attr info for outer join post-processing
        ref-info (atom nil)]
    ;; For LEFT/RIGHT/FULL joins, pre-register the right-alias entity var in
    ;; :left-join-evars BEFORE translating any ON-clause expressions. This
    ;; guarantees that ctx/col-var! on right-side columns emits plain data
    ;; patterns (rather than get-else), so the or-join post-processing in
    ;; translate-select can detect and relocate them into the matched
    ;; branch. A get-else outside the or-join would try to resolve against
    ;; the `:__null__` sentinel entity-id and throw.
    ;;
    ;; Ref-based joins ALSO later conj the ref-var (the unified entity var
    ;; bound to the right alias after the swap) so ctx/col-var! keeps emitting
    ;; plain patterns after the entity-var rebind. Both registrations are
    ;; safe: left-join-evars is a set and serves only as a lookup predicate
    ;; inside col-var!.
    (when (and right-alias (#{:left :right :full} jtype))
      (swap! (:left-join-evars ctx) conj (ctx/entity-var! ctx right-alias)))
    (when on-exprs
      (doseq [raw-expr on-exprs]
        ;; Unwrap ParenthesedExpressionList — JSqlParser wraps `ON (x = y)` as
        ;; `ParenthesedExpressionList(EqualsTo(...))` rather than a bare EqualsTo.
        (let [expr (if (and (instance? ParenthesedExpressionList raw-expr)
                            (= 1 (.size ^ParenthesedExpressionList raw-expr)))
                     (.get ^ParenthesedExpressionList raw-expr 0)
                     raw-expr)]
          (cond
          ;; M2M JOIN: ON <col> = ANY(<m2m-array-col>) or symmetric.
          ;; Recognises the SQL idiom `JOIN tag t ON t.db_id = ANY(a.tags)`
          ;; where `a.tags` is a :db.cardinality/many :db.type/ref column.
          ;; Datahike-side, M2M refs are stored as N datoms — one per
          ;; element. So the natural data pattern is
          ;;   [?a-eid :account/tags ?t-eid]
          ;; which iterates per (source, target) pair, exactly the SQL
          ;; semantics. Emit that pattern; no entity-var swap needed.
            (let [is-any-fn? (fn [e]
                               (and (instance? net.sf.jsqlparser.expression.Function e)
                                    (= "any" (clojure.string/lower-case
                                              (.getName ^net.sf.jsqlparser.expression.Function e)))))
                  any-arg (fn [^net.sf.jsqlparser.expression.Function f]
                            (let [params (.getParameters f)
                                  exprs (when params (.getExpressions params))]
                              (when (and exprs (= 1 (.size exprs)))
                                (.get exprs 0))))
                  any-form (when (instance? EqualsTo expr)
                             (let [^EqualsTo eq expr
                                   l (.getLeftExpression eq)
                                   r (.getRightExpression eq)]
                               (cond
                                 (and (instance? Column l) (is-any-fn? r)
                                      (instance? Column (any-arg r)))
                                 {:scalar l :array (any-arg r)}
                                 (and (instance? Column r) (is-any-fn? l)
                                      (instance? Column (any-arg l)))
                                 {:scalar r :array (any-arg l)}
                                 :else nil)))]
              (when any-form
                (let [{:keys [scalar array]} any-form
                      scalar-resolved (ctx/resolve-column ^Column scalar
                                                          (:table-aliases ctx)
                                                          (:default-table ctx)
                                                          (:col-overrides ctx)
                                                          (:derived-aliases ctx) (:ci-index ctx))
                      array-resolved (ctx/resolve-column ^Column array
                                                         (:table-aliases ctx)
                                                         (:default-table ctx)
                                                         (:col-overrides ctx)
                                                         (:derived-aliases ctx) (:ci-index ctx))
                      bare-attr (fn [r] (if (vector? r) (nth r 2) r))
                      schema (:schema ctx)
                      m2m-attr (when-not (and (vector? array-resolved)
                                              (= :db-id (first array-resolved)))
                                 (bare-attr array-resolved))
                      ;; Confirm m2m-attr is :db.cardinality/many ref
                      m2m? (and (keyword? m2m-attr)
                                (= :db.type/ref
                                   (get-in schema [m2m-attr :db/valueType]))
                                (= :db.cardinality/many
                                   (get-in schema [m2m-attr :db/cardinality])))
                      ;; The scalar side must be db_id (entity-id of the
                      ;; target table). Otherwise this isn't an M2M JOIN.
                      scalar-db-id? (and (vector? scalar-resolved)
                                         (= :db-id (first scalar-resolved)))]
                  (when (and m2m? scalar-db-id?)
                    (let [;; Source alias — the table that OWNS the M2M attr.
                          src-alias (if (vector? array-resolved)
                                      (second array-resolved)
                                      (namespace array-resolved))
                          ;; Target alias — the table whose db_id appears.
                          tgt-alias (second scalar-resolved)
                          src-evar (ctx/entity-var! ctx src-alias)
                          tgt-evar (ctx/entity-var! ctx tgt-alias)]
                      (ctx/add-clause! ctx [src-evar m2m-attr tgt-evar])
                      true)))))
            ;; M2M-JOIN branch consumed the ON clause; nothing else to do.
            ;; Fall through to other ON conjuncts via the outer doseq.
            nil

          ;; Ref-unification for EqualsTo with db_id
            (and (instance? EqualsTo expr)
                 (let [^EqualsTo eq expr
                       left (.getLeftExpression eq)
                       right (.getRightExpression eq)]
                   (and (instance? Column left) (instance? Column right))))
            (let [^EqualsTo eq expr
                  left (.getLeftExpression eq)
                  right (.getRightExpression eq)
                  left-resolved (ctx/resolve-column ^Column left (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx) (:ci-index ctx))
                  right-resolved (ctx/resolve-column ^Column right (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx) (:ci-index ctx))
                  schema (:schema ctx)
                  hints  (:hints ctx)
                  ;; Pull target-unique fact for an attr: true when the
                  ;; attr is :db.unique/identity or :db.unique/value.
                  unique-attr? (fn [a]
                                 (and (keyword? a)
                                      (contains? #{:db.unique/identity :db.unique/value}
                                                 (get-in schema [a :db/unique]))))
                  ;; Pull ref-target fact: is `a` a :db.type/ref?
                  ref-attr? (fn [a]
                              (and (keyword? a)
                                   (= :db.type/ref (get-in schema [a :db/valueType]))))
                  ;; :datahike.pg/references hint override: a ref attr
                  ;; carrying this hint joins against the named target
                  ;; attr specifically. If the opposite side resolves to
                  ;; that attr, the join is an FK-via-ref.
                  references-of (fn [a] (get-in hints [a :references]))
                  ;; Extract bare attr from a :aliased vector or return as-is.
                  bare-attr (fn [r] (if (vector? r) (nth r 2) r))
                  ;; Extract alias from a :aliased vector or return nil
                  ;; (unaliased resolves use default-table).
                  alias-of  (fn [r] (if (vector? r)
                                      (second r)
                                      (namespace r)))
                  l-bare (when-not (and (vector? left-resolved)
                                        (= :db-id (first left-resolved)))
                           (bare-attr left-resolved))
                  r-bare (when-not (and (vector? right-resolved)
                                        (= :db-id (first right-resolved)))
                           (bare-attr right-resolved))
                  ;; FK-via-ref detection. Picks ref side + unique side;
                  ;; honors an explicit :datahike.pg/references hint if
                  ;; set, else falls back to "RHS is a unique attr". Keep
                  ;; the original resolved form (possibly [:aliased …])
                  ;; so col-var! allocates the logic var against the
                  ;; user-written alias, not the attr's namespace.
                  fk-via-ref
                  (cond
                    (and (ref-attr? l-bare)
                         (or (= (references-of l-bare) r-bare)
                             (unique-attr? r-bare)))
                    {:ref-resolved left-resolved
                     :target-alias (alias-of right-resolved)
                     :ref-attr     l-bare}

                    (and (ref-attr? r-bare)
                         (or (= (references-of r-bare) l-bare)
                             (unique-attr? l-bare)))
                    {:ref-resolved right-resolved
                     :target-alias (alias-of left-resolved)
                     :ref-attr     r-bare}

                    :else nil)
                  derived-aliases (:derived-aliases ctx)
                  ;; Skip ref/db_id unification when the db-id side is
                  ;; a derived alias — `derived.db_id` is a projected
                  ;; value column in the speculative db, NOT a real
                  ;; entity-id we can unify with the ref attr's value.
                  ;; Falls through to value-equality JOIN, which
                  ;; correctly looks up `[?d :derived/db_id ?val]` and
                  ;; matches against the outer ref's value.
                  derived? (fn [resolved]
                             (and (vector? resolved)
                                  (= :db-id (first resolved))
                                  (contains? derived-aliases (second resolved))))
                  [ref-side db-id-side]
                  (cond
                    (and (vector? left-resolved) (= :db-id (first left-resolved))
                         (not (derived? left-resolved)))
                    [right-resolved left-resolved]
                    (and (vector? right-resolved) (= :db-id (first right-resolved))
                         (not (derived? right-resolved)))
                    [left-resolved right-resolved]
                    :else nil)]
              (cond
                fk-via-ref
                ;; ON p.fk = c.pk where p.fk is :db.type/ref and c.pk is
                ;; :db.unique/identity (or explicitly named by
                ;; :datahike.pg/references). Same mechanic as db_id
                ;; unification: the ref attr's value IS the target
                ;; entity-id, so we get the raw ref-eid var (not the
                ;; deref'd target-PK that ref-targeted col-var! returns)
                ;; and rebind the target alias's entity-var to it. The
                ;; target's unique column then resolves via the bound
                ;; entity.
                (let [{:keys [ref-resolved ref-attr target-alias]} fk-via-ref
                      ref-var (ctx/ref-eid-var! ctx ref-resolved)
                      ;; Capture the LEFT alias's ORIGINAL entity-var
                      ;; before any potential swap; needed by the
                      ;; LEFT JOIN post-processor to drive iteration
                      ;; from the LEFT (so empty-right rows surface).
                      original-left-evar (ctx/entity-var! ctx target-alias)]
                  ;; INNER joins: unify entity vars. OUTER joins keep
                  ;; the LEFT alias's own entity-var (skip the swap).
                  (when-not (#{:left :right :full} jtype)
                    (swap! (:entity-vars ctx) assoc target-alias ref-var))
                  (when (#{:left :right :full} jtype)
                    (reset! ref-info {:ref-var ref-var
                                      :ref-attr ref-attr
                                      :right-alias target-alias
                                      ;; Which side OWNS the ref attribute.
                                      ;; This lowering was written for the
                                      ;; owner on the JOINED side (`FROM
                                      ;; transaction LEFT JOIN posting ON
                                      ;; posting.transaction = t.db_id`) and
                                      ;; assumed it either way, so the other
                                      ;; direction emitted a pattern whose
                                      ;; entity and value were one variable
                                      ;; and the join answered nothing.
                                      :owner-on-left? (= target-alias right-alias)
                                      :owner-evar (ctx/entity-var! ctx (alias-of ref-resolved))
                                      :left-table-evar original-left-evar
                                      :left-evar (ctx/entity-var! ctx (:default-table ctx))})
                    (swap! (:left-join-evars ctx) conj ref-var)))

                ref-side
                (do
                ;; Always create the ref pattern [?left-eid :ref-attr ?ref-var]
                  ;; Use ref-eid-var! to get the raw ref entity-id var —
                  ;; col-var! would return the SQL-projection deref'd PK
                  ;; for ref-targeted attrs, which is the wrong thing to
                  ;; rebind a target alias's entity-var to.
                  (let [ref-var (ctx/ref-eid-var! ctx ref-side)
                        db-id-alias (second db-id-side)
                        ;; Capture the LEFT alias's ORIGINAL entity-var
                        ;; before any potential swap. The post-processor
                        ;; needs it to drive LEFT iteration from the
                        ;; LEFT table (the proper LEFT JOIN semantics)
                        ;; rather than from the right table via get-else.
                        original-left-evar (ctx/entity-var! ctx db-id-alias)]
                    ;; INNER joins: unify entity vars for the
                    ;; optimization "the right-table entity IS the ref
                    ;; value". OUTER joins (left/right/full): keep the
                    ;; LEFT alias's own entity-var so the LEFT iteration
                    ;; can drive the or-join. Without this, an empty-
                    ;; right-side row gets dropped because the get-else
                    ;; has no entity to iterate over.
                    (when-not (#{:left :right :full} jtype)
                      (swap! (:entity-vars ctx) assoc db-id-alias ref-var))
                  ;; For outer joins: also record ref-info for or-join wrapping
                    (when (#{:left :right :full} jtype)
                      (reset! ref-info {:ref-var ref-var
                                        :ref-attr (if (vector? ref-side) (nth ref-side 2) ref-side)
                                        :right-alias db-id-alias
                                        ;; See :owner-on-left? above: the
                                        ;; db_id side is the JOINED table
                                        ;; when the ref's owner is the one
                                        ;; we are joining FROM.
                                        :owner-on-left? (= db-id-alias right-alias)
                                        :owner-evar (ctx/entity-var! ctx (alias-of ref-side))
                                        ;; Original LEFT entity-var
                                        ;; (the one OUR alias points to,
                                        ;; e.g. ?t_eid for "t"). Used by
                                        ;; the post-processor to bind the
                                        ;; LEFT iteration in matched.
                                        :left-table-evar original-left-evar
                                        ;; Default-table evar for
                                        ;; backward compat with code that
                                        ;; reads :left-evar
                                        :left-evar (ctx/entity-var! ctx (:default-table ctx))})
                    ;; Record right entity var so ctx/make-columns-optional! skips it
                      (swap! (:left-join-evars ctx) conj ref-var))))

                :else
                ;; No db_id / ref involved — value equality join (ON t1.a = t2.x)
                (if (#{:left :right :full} jtype)
                ;; For outer joins: record join key info for or-join wrapping.
                ;; Don't add equality predicate — the or-join handles matching.
                ;; The right-alias entity var is already registered in
                ;; :left-join-evars (see translate-join top), so col-var!
                ;; emits plain patterns for right-side columns referenced
                ;; here.
                  (let [l-var (expr/translate-expr ctx left)
                        l-resolved (ctx/resolve-column ^Column left (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx) (:ci-index ctx))
                        r-resolved (ctx/resolve-column ^Column right (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx) (:ci-index ctx))
                      ;; Right-side attr is the one from THIS join's right
                      ;; table (== right-alias). The previous heuristic
                      ;; compared l-ns to (:default-table ctx) (the FROM
                      ;; table) — that works for a single LEFT JOIN, but
                      ;; for chained joins like
                      ;;   FROM main
                      ;;     LEFT JOIN a AS aa ON main.x = aa.y
                      ;;     LEFT JOIN b AS bb ON aa.z = bb.w
                      ;; the second join's ON sides reference `aa` and
                      ;; `bb`, neither equals the FROM table, so the
                      ;; heuristic falls through to the swapped branch
                      ;; unconditionally and ends up using the LEFT
                      ;; operand's attr as right-key-attr. That produces
                      ;; matched-key patterns of the form
                      ;;   [right-evar <left-table-attr> left-key-var]
                      ;; which is malformed (entity-var from the right
                      ;; table, attribute namespace from the left table)
                      ;; and surfaces downstream as an or-join branch
                      ;; whose limit-rel projection drops different
                      ;; subsets of the join-vars per branch — the
                      ;; `Can't sum relations with different attrs`
                      ;; failure on Odoo's ir_model_access access-group
                      ;; query.
                        [left-key-var right-key-attr]
                        (let [l-ns (if (vector? l-resolved) (second l-resolved) (namespace l-resolved))
                              l-attr (if (vector? l-resolved) (nth l-resolved 2) l-resolved)
                              r-attr (if (vector? r-resolved) (nth r-resolved 2) r-resolved)]
                          (if (= l-ns right-alias)
                          ;; LHS is from THIS join's right table → swap
                            [(expr/translate-expr ctx right) l-attr]
                          ;; LHS is from another table (the "left" side
                          ;; of the join from this join's perspective)
                            [l-var r-attr]))]
                    ;; ACCUMULATE. `reset!` here kept only the LAST
                    ;; equality of a multi-condition ON, so
                    ;; `ON (b.x = a.x AND b.y = a.y)` joined on `y`
                    ;; alone and answered rows that match neither pair.
                    (swap! ref-info
                           (fn [info]
                             (-> (or info {})
                                 (assoc :value-join? true
                                        :right-alias right-alias
                                        :left-evar (ctx/entity-var! ctx (:default-table ctx)))
                                 (update :value-keys (fnil conj [])
                                         {:left-key-var left-key-var
                                          :right-key-attr right-key-attr})))))
                ;; For inner joins: unify on a shared logic var when both
                ;; sides are plain columns — indexable data patterns the
                ;; engine hash-joins in O(n), see ctx/unify-inner-equijoin!.
                ;; The predicate fallback below cross-products the two
                ;; relations before filtering (O(n²) time and heap).
                ;;
                ;; Fallback: equality predicate + SQL null guards.
                ;; Datalog (= :__null__ :__null__) is TRUE, but SQL INNER
                ;; JOIN on NULL=NULL must NOT match (3-valued logic: NULL =
                ;; NULL is UNKNOWN, filtered out as non-TRUE). Emit explicit
                ;; not-null guards for each side so rows whose join-column
                ;; is NULL are excluded. nil and the :__null__ sentinel are
                ;; both treated as SQL NULL.
                  ;; A jsonb join key must NOT unify on a shared logic
                  ;; var: that makes the join TEXT equality, so
                  ;; `1.00` and `1` fail to match where PostgreSQL
                  ;; joins them. Fall through to the predicate.
                  (or (and (not (or (expr/jsonb-column? ctx left)
                                    (expr/jsonb-column? ctx right)))
                           (ctx/unify-inner-equijoin! ctx left-resolved right-resolved))
                      (let [l-var (expr/translate-expr ctx left)
                            r-var (expr/translate-expr ctx right)
                            eq-fn (if (or (expr/jsonb-column? ctx left)
                                          (expr/jsonb-column? ctx right))
                                    'datahike.pg.sql/jsonb-eq?
                                    '=)]
                        (ctx/add-clause! ctx [(list eq-fn l-var r-var)])
                        (when (symbol? l-var)
                          (ctx/add-clause! ctx [(list 'not= l-var :__null__)])
                          (ctx/add-clause! ctx [(list 'not= l-var nil)]))
                        (when (symbol? r-var)
                          (ctx/add-clause! ctx [(list 'not= r-var :__null__)])
                          (ctx/add-clause! ctx [(list 'not= r-var nil)])))))))

          ;; Fall back to regular predicate translation. For OUTER joins
          ;; (LEFT/RIGHT/FULL), capture the predicate clauses for the
          ;; or-join post-processor — applying them globally would
          ;; convert the LEFT JOIN into an INNER JOIN by filtering out
          ;; rows whose right side has no match (NULL filter rejects).
          ;; The translate-predicate call still side-effects col-var
          ;; data patterns into where-clauses for any right-side
          ;; columns the predicate references; only the predicate
          ;; itself is deferred.
            :else
            (if (#{:left :right :full} jtype)
              (let [preds (expr/translate-predicate ctx expr)]
                (swap! ref-info update :matched-only-preds (fnil into [])
                       (vec preds))
                ;; A conjunct that reads a COLUMN, as opposed to `ON true`
                ;; / `ON (1=1)` which fold to a constant. PostgreSQL
                ;; refuses a FULL JOIN whose ON clause has one of these
                ;; and no equality between the relations (see the
                ;; refusal in sql.clj), so the distinction has to survive
                ;; translation.
                ;;
                ;; A literal `false` conjunct makes the WHOLE clause
                ;; constant -- PostgreSQL's constant folding turns
                ;; `x AND false` into `false` before the join-condition
                ;; check, and answers it as a cross join that matches
                ;; nothing. (A conjunct that merely evaluates to false,
                ;; `1=2`, is not recognised here: we refuse where
                ;; PostgreSQL answers, which is the safe direction.)
                (if (= "false" (str/lower-case (str/trim (str expr))))
                  (swap! ref-info assoc :constant-false-on? true)
                  (when (some (comp seq clause-vars) preds)
                    (swap! ref-info assoc :variable-pred? true))))
              ;; INNER-join ON conjunct = top-level conjunct: allow the
              ;; indexable data-pattern fast paths.
              (let [preds (binding [expr/*conjunctive-where* true]
                            (expr/translate-predicate ctx expr))]
                (swap! (:where-clauses ctx) into preds)))))))
    (let [info @ref-info]
      ;; A predicate-only outer join previously fell into the ref-join
      ;; post-processor with nil join variables and emitted a malformed
      ;; `(or-join [nil ...])`. Refuse that still-unsupported shape before
      ;; Datahike's rule parser sees it. Equijoins and the separately
      ;; materialized LATERAL ON TRUE path remain supported.
      ;;
      ;; An ON clause with no equality BETWEEN the relations is a NESTED
      ;; LOOP: every right row is considered for every left row, and the
      ;; conditions filter. PostgreSQL joins that way whenever it has to
      ;; -- `ON (b.y > a.y)`, `ON (b.v IS NOT NULL)`, `ON true` -- and
      ;; most of the ON-clause space has no equality in it, so refusing
      ;; it refused a large part of the language. The lowering is the
      ;; equi-join's with the row-existence marker in place of the key
      ;; pattern: it enumerates the right relation rather than seeking
      ;; into it.
      (let [info (if (and (#{:left :right :full} jtype)
                          (not (:value-join? info))
                          (nil? (:ref-attr info)))
                   (assoc info :nested-loop? true :right-alias right-alias
                          :left-evar (ctx/entity-var! ctx (:default-table ctx)))
                   info)]
        {:name name :alias right-alias :join-type jtype :ref-info info}))))

(defn select-item-alias
  "The explicit `AS` label of a select item, or nil.

   An UNQUOTED alias is down-cased, the way PG's lexer folds every
   unquoted identifier (scan.l's `downcase_truncate_identifier`), so
   `SELECT 1 AS Foo` names the column `foo`. A quoted one keeps its
   case: `AS \"Foo\"` stays `Foo`."
  [^SelectItem item]
  (when-let [alias (.getAlias item)]
    (let [raw (.getName ^Alias alias)]
      (if (and raw (str/starts-with? raw "\""))
        (unquote-ident raw)
        (some-> raw str/lower-case)))))

(def ^:private sql-type->internal-name
  "SQL type spelling → the name PostgreSQL actually stores in pg_type.

   A cast names its output column after the type, but after the
   grammar's `SystemTypeName` rewrite — so `1::int` is `int4`, not
   `int`, and `x::character varying` is `varchar`. Types not listed here
   (text, date, json, user-defined) already are their own internal name.
   See gram.y's Numeric/Character/ConstDatetime productions."
  {"int" "int4", "integer" "int4"
   "bigint" "int8"
   "smallint" "int2"
   "real" "float4"
   "double precision" "float8", "double" "float8"
   "boolean" "bool"
   "decimal" "numeric", "dec" "numeric"
   "char" "bpchar", "character" "bpchar"
   "character varying" "varchar"
   "bit varying" "varbit"
   "timestamp with time zone" "timestamptz"
   "timestamp without time zone" "timestamp"
   "time with time zone" "timetz"
   "time without time zone" "time"})

(defn- figure-colname*
  "PostgreSQL's `FigureColnameInternal` (parse_target.c), returning
   `[name strength]` where strength is 0 (no idea), 1 (second-best) or
   2 (good). nil name means no idea.

   Strength only matters at two recursion points — a cast and a CASE —
   where a *good* name from the operand is kept but a second-best one is
   overwritten. That is what makes `a::text` be `a` while `1::int8::text`
   is `text` and `CASE … ELSE 2::int8 END` is `case`."
  [expr]
  (cond
    (nil? expr) [nil 0]

    ;; A column reference is named by its last component: `t.a` is `a`.
    (instance? Column expr)
    [(unquote-ident (.getColumnName ^Column expr)) 2]

    ;; Function calls — including COALESCE / GREATEST / LEAST / window
    ;; functions, which JSqlParser also surfaces as calls and which PG
    ;; special-cases to the same names. Schema qualification is dropped.
    (instance? Function expr)
    [(expr/resolution-name (.getName ^Function expr)) 2]

    (instance? net.sf.jsqlparser.expression.AnalyticExpression expr)
    [(expr/resolution-name (.getName ^net.sf.jsqlparser.expression.AnalyticExpression expr)) 2]

    ;; A cast takes the operand's name when that name is a good one, and
    ;; otherwise the target type's — so the type name is only a fallback.
    (instance? CastExpression expr)
    (let [[n s] (figure-colname* (.getLeftExpression ^CastExpression expr))]
      (if (> s 1)
        [n s]
        (let [dt (.getColDataType ^CastExpression expr)
              raw (str/lower-case (str (.getDataType dt)))
              ;; Array brackets aren't part of the type's name: PG keeps
              ;; them in a separate arrayBounds field, so `::text[]` is
              ;; named `text`.
              base (str/replace raw #"\[.*$" "")
              base (str/trim (str/replace base #"\(.*$" ""))]
          [(get sql-type->internal-name base base) 1])))

    ;; CASE inherits a good name from its ELSE arm, else it is "case".
    (instance? CaseExpression expr)
    (let [[n s] (figure-colname* (.getElseExpression ^CaseExpression expr))]
      (if (> s 1) [n s] ["case" 1]))

    ;; Parens are not a node in PG's tree at all — see straight through.
    (instance? Parenthesis expr)
    (figure-colname* (.getExpression ^Parenthesis expr))
    (and (instance? ParenthesedExpressionList expr)
         (= 1 (count ^ParenthesedExpressionList expr)))
    (figure-colname* (first ^ParenthesedExpressionList expr))

    ;; A scalar subquery is named after its own single output column —
    ;; `SELECT (SELECT id FROM t)` is `id`, and a subquery whose column
    ;; is itself unnamed propagates `?column?`.
    (instance? ParenthesedSelect expr)
    (let [inner (.getSelect ^ParenthesedSelect expr)]
      (cond
        (instance? PlainSelect inner)
        (let [^SelectItem it (first (.getSelectItems ^PlainSelect inner))]
          (if it
            [(or (select-item-alias it)
                 (first (figure-colname* (.getExpression it)))
                 "?column?")
             2]
            [nil 0]))

        ;; PostgreSQL names VALUES relation columns column1, column2, …;
        ;; the scalar form necessarily exposes only its first column.
        (instance? Values inner)
        ["column1" 2]

        :else [nil 0]))

    (instance? net.sf.jsqlparser.expression.operators.relational.ExistsExpression expr)
    ["exists" 2]
    (instance? ArrayConstructor expr) ["array" 2]
    (instance? net.sf.jsqlparser.expression.RowConstructor expr) ["row" 2]

    ;; CURRENT_DATE / CURRENT_TIMESTAMP / USER / … — JSqlParser surfaces
    ;; these bare keywords as a TimeKeyExpression or a Column, and PG
    ;; names the column after the keyword.
    (instance? TimeKeyExpression expr)
    [(str/lower-case (str/replace (.getStringValue ^TimeKeyExpression expr) #"\(.*$" "")) 2]

    ;; Everything else — literals (including `B'101'`), placeholders,
    ;; operators, boolean expressions, IS NULL, BETWEEN, IN — has no
    ;; case in PG's switch and falls through to the default.
    :else [nil 0]))

(defn figure-colname
  "The name PostgreSQL gives an un-aliased SELECT output column.

   Mirrors `FigureColname` (parse_target.c:1711): consult the rules,
   and fall back to the literal `?column?`. We used to answer with
   whatever was closest to hand — the datalog variable (`p1`, `v1`), or
   the expression's own SQL text (`B'1001000'`, `B'101'::varbit`) —
   none of which any PostgreSQL client would see."
  [expr]
  (or (first (figure-colname* expr)) "?column?"))

(declare match-aggregate-index*)

(defn match-aggregate-index
  "Try to find the index of an aggregate function in the find-elements.
   For COUNT(*) → look for (count ?x), for SUM(col) → (sum ?x) or
   (datahike.pg.sql/filter-sum ?x). Returns the 0-based index or nil.

   Matches both the raw Datalog aggregate symbol and our ns-qualified
   null-filtering variant (filter-sum/avg/min/max/count[-distinct]) so
   HAVING clauses resolve regardless of which variant the SELECT
   projection emitted."
  ([^Function f find-elems find-aliases] (match-aggregate-index f find-elems find-aliases nil))
  ([^Function f find-elems find-aliases resolved]
   ;; `resolved` is this Function's translated :find element, when the
   ;; caller could compute it. Prefer an EXACT match on it: the fallbacks
   ;; below match only the aggregate OPERATOR, so with `min(1)` in the
   ;; SELECT list and `min(i)` in HAVING they resolved to the same column
   ;; and the HAVING filtered on the wrong aggregate entirely.
   (or (when resolved
         (some (fn [[i elem]] (when (= elem resolved) i))
               (map-indexed vector find-elems)))
       (match-aggregate-index* f find-elems find-aliases))))

(defn- match-aggregate-index*
  [^Function f find-elems find-aliases]
  (let [fname (expr/resolution-name (.getName f))
        agg-sym (get fns/sql-aggregate->datalog fname)
        ;; Base name without the ns qualifier: for matching against
        ;; (count ?x) emitted by COUNT(*) special case.
        base-name (case fname
                    "count"          'count
                    "sum"            'sum
                    "avg"            'avg
                    "min"            'min
                    "max"            'max
                    "count_distinct" 'count-distinct
                    agg-sym)]
    (or (some (fn [[i a]] (when (= a fname) i))
              (map-indexed vector find-aliases))
        (some (fn [[i elem]]
                (when (and (seq? elem)
                           (let [op (first elem)]
                             (or (= op agg-sym)
                                 (= op base-name))))
                  i))
              (map-indexed vector find-elems)))))

;; ============================================================================
;; Derived-table materialization — shared by FROM (...) AS t and
;; JOIN (...) AS t subqueries, as well as table functions in those positions.
;; ============================================================================

(declare translate-select)
(declare extract-value)

(defn- stratum-range-predicates
  "Translate a complete conjunction of simple comparisons on `attr`.

   Returns Stratum `:where` clauses, or nil when any predicate is not safely
   representable. Declining the whole conjunction is essential: choosing top-N
   before an unpushed filter can silently under-fill a PostgreSQL LIMIT."
  [ctx attr attr-schema predicate schema db]
  (letfn [(column-attr [candidate]
            (when (instance? Column candidate)
              (let [resolved (ctx/resolve-column
                              candidate (:table-aliases ctx) (:default-table ctx)
                              (:col-overrides ctx) (:derived-aliases ctx)
                              (:ci-index ctx))]
                (ctx/attr-of ctx resolved))))
          (literal-value [candidate]
            (when (or (instance? LongValue candidate)
                      (instance? DoubleValue candidate)
                      (instance? StringValue candidate)
                      (instance? JdbcParameter candidate)
                      (and (instance? SignedExpression candidate)
                           (or (instance? LongValue
                                          (.getExpression ^SignedExpression candidate))
                               (instance? DoubleValue
                                          (.getExpression ^SignedExpression candidate)))))
              (let [value (if (instance? JdbcParameter candidate)
                            (->ParamRef (.getIndex ^JdbcParameter candidate))
                            (extract-value candidate schema db))
                    value-type (:db/valueType attr-schema)]
                ;; Parameter OID inference has already tied a JdbcParameter to
                ;; this column's PostgreSQL type. Preserve its ParamRef in the
                ;; cached candidate descriptor; executePrepared and the simple
                ;; numeric-template path substitute the decoded value before
                ;; Stratum sees it. Baking the first literal into this plan
                ;; would make the parse cache return wrong ranges later.
                (when (or (params/param-ref? value)
                          (case value-type
                            :db.type/long (integer? value)
                            (:db.type/double :db.type/float) (number? value)
                            :db.type/string (string? value)
                            false))
                  value))))
          (comparison-op [candidate]
            (cond
              (instance? EqualsTo candidate) :=
              (instance? GreaterThan candidate) :>
              (instance? GreaterThanEquals candidate) :>=
              (instance? MinorThan candidate) :<
              (instance? MinorThanEquals candidate) :<=
              :else nil))
          (reverse-op [op]
            ({:> :<, :>= :<=, :< :>, :<= :>=, := :=} op))
          (walk [candidate]
            (cond
              (instance? Parenthesis candidate)
              (walk (.getExpression ^Parenthesis candidate))

              (instance? AndExpression candidate)
              (let [^AndExpression and-expr candidate
                    left (walk (.getLeftExpression and-expr))
                    right (walk (.getRightExpression and-expr))]
                (when (and (some? left) (some? right))
                  (into left right)))

              (comparison-op candidate)
              (let [^net.sf.jsqlparser.expression.BinaryExpression comparison candidate
                    left (.getLeftExpression comparison)
                    right (.getRightExpression comparison)
                    op (comparison-op comparison)
                    left-attr (column-attr left)
                    right-attr (column-attr right)
                    left-value (literal-value left)
                    right-value (literal-value right)
                    col-key (keyword (name attr))]
                (cond
                  (and (= attr left-attr) (some? right-value))
                  [[op col-key right-value]]

                  (and (= attr right-attr) (some? left-value))
                  [[(reverse-op op) col-key left-value]]

                  :else nil))

              :else nil))]
    (if predicate (walk predicate) [])))
(declare apply-sql-cast)
(declare eval-corr-scalar)

(defn- srf-const-eval
  "Evaluate a table-function argument expression to a constant value at
   translate time. Handles the literal forms a constant-arg SRF uses
   (`generate_series(2,4)`, `unnest(ARRAY[…])`). Returns `::corr` for a
   non-constant (correlated) argument — e.g. a Column reference in
   `LATERAL generate_series(1, t.n)`. The LATERAL path (future) will pass
   `materialize-table-function` a different eval-fn that resolves such
   references per outer row from `*from-bindings*`; this is the seam that
   lets the same materialiser serve both callers without a rewrite."
  [expr]
  (cond
    (instance? LongValue expr)   (.getValue ^LongValue expr)
    (instance? DoubleValue expr) (types/decimal-literal expr (.getValue ^DoubleValue expr))
    (instance? StringValue expr) (expr/string-value-text ^StringValue expr)
    (instance? JdbcParameter expr)
    (if-let [bound params/*bound-params*]
      (nth bound (dec (long (.getIndex ^JdbcParameter expr))) ::corr)
      ::corr)
    (instance? SignedExpression expr)
    (let [v (srf-const-eval (.getExpression ^SignedExpression expr))]
      (if (number? v) (- v) ::corr))
    (instance? CastExpression expr)
    (let [v (srf-const-eval (.getLeftExpression ^CastExpression expr))]
      (if (= ::corr v) ::corr (apply-sql-cast v ^CastExpression expr)))
    ;; No schema here: an element that is not a literal cannot be
    ;; evaluated, which is what ::corr says.
    (instance? ArrayConstructor expr)
    (try (extract-value ^ArrayConstructor expr) (catch Exception _ ::corr))
    :else ::corr))

(defn- numeric-series-error! [message]
  (throw (errors/pg-error :invalid-parameter-value {:message message})))

(defn- numeric-series
  "Materialize PostgreSQL's finite NUMERIC generate_series variant.

   BigDecimal addition preserves the greater operand scale, which also
   preserves PostgreSQL's visible scale for cases such as 0.0, 1.0, ... ."
  [start stop step]
  (doseq [[value label] [[start "start value"] [stop "stop value"] [step "step size"]]]
    (when (types/numeric-special? value)
      (numeric-series-error!
       (str label " cannot be " (if (= :nan (:kind value)) "NaN" "infinity")))))
  (let [^java.math.BigDecimal start start
        ^java.math.BigDecimal stop stop
        ^java.math.BigDecimal step step
        direction (.signum step)]
    (when (zero? direction)
      (numeric-series-error! "step size cannot equal zero"))
    (loop [value start
           result (transient [])]
      (if (if (pos? direction)
            (pos? (.compareTo value stop))
            (neg? (.compareTo value stop)))
        (persistent! result)
        (recur (.add value step) (conj! result [value]))))))

(defn- coldef-pg-type
  "The `:pg/type` for a column-definition-list entry like `a int`.

   Delegates to `ddl/pg-type-hint`, which makes the same decision for
   CREATE TABLE and ALTER TABLE. This used to be a narrow local copy
   because that function lived on an unmerged branch; keeping two would
   have let the column-definition list and DDL drift apart on exactly
   the types Datahike collapses (every integer onto :db.type/long,
   every temporal onto :db.type/instant).

   The `(n)` suffix is stripped here because `pg-type-hint`'s DDL caller
   strips it before calling and so must this one — `numeric(10,2)` has
   to reach it as `numeric`."
  [^String t]
  (some-> t (str/replace #"\s*\([^)]*\)" "") (ddl/pg-type-hint false)))

(def ^:private known-srf-names
  "Function names `materialize-table-function` knows how to turn into a
   relation. Used only to tell an UNKNOWN function in FROM apart from a
   known one whose arguments we could not evaluate (a correlated LATERAL
   argument): the first is 42883, the second is not."
  #{"unnest" "generate_series" "pg_get_keywords"
    "pg_input_error_info"
    "jsonb_array_elements" "json_array_elements"
    "jsonb_array_elements_text" "json_array_elements_text"
    "jsonb_each" "json_each" "jsonb_each_text" "json_each_text"
    "jsonb_object_keys" "json_object_keys"
    "regexp_split_to_table" "string_to_table"
    "now" "current_timestamp" "transaction_timestamp"
    "statement_timestamp" "clock_timestamp"})

(defn- srf-base-name
  "The lower-cased name a FROM-clause function call resolves under, by
   the rule every other call uses (`expr/resolution-name`):
   `pg_catalog.generate_series` is `generate_series`, and any other
   qualifier stays part of the name and resolves to nothing.

   Dropping the LAST dot-separated segment instead made up a function:
   `nosuchschema.unnest(…)` returned rows where PostgreSQL raises."
  [^String n]
  (when n
    (expr/resolution-name (str/lower-case n))))

(defn- table-function-has-column-reference?
  "Whether a table function argument contains a column reference.

   Look through the complete argument expression. Checking only for an
   argument that *is* a Column misses `generate_series(1, t.n + 1)` and
   `unnest(ARRAY[t.n])`; the latter was then materialised once as the string
   `r` and silently returned that value for every outer row."
  [^net.sf.jsqlparser.statement.select.TableFunction tf]
  (boolean
   (some seq
         (map params/ast-columns
              (or (.getParameters (.getFunction tf)) [])))))

(defn- reject-unmaterialized-table-function!
  "Raise a PostgreSQL error when a FROM table function reaches the end of
   the materialisation paths.

   An unknown name is ordinary function resolution failure (42883). A name
   we implement but cannot shape or evaluate in this context is an explicit
   capability boundary (0A000). Neither case may fall through as an absent
   relation: that used to return only the outer rows in comma joins, or leak
   an unbound Datalog entity var when WITH ORDINALITY exposed columns."
  [^net.sf.jsqlparser.statement.select.TableFunction tf]
  (let [fname (srf-base-name (.getName (.getFunction tf)))]
    (if (contains? known-srf-names fname)
      (throw (errors/pg-error
              :feature-not-supported
              {:message (str "table function " fname
                             " cannot be evaluated in this context")}))
      (throw (errors/pg-error
              :undefined-function
              {:function (str fname "()")
               :hint (str "No function matches the given name and "
                          "argument types. You might need to add "
                          "explicit type casts.")})))))

(defn- target-list-srf?
  "Whether expr is a set-returning function supported by ProjectSet.

   Keep this deliberately narrower than `known-srf-names`: several entries in
   that set are scalar compatibility shims when used outside FROM.  These two
   functions have unambiguous PostgreSQL set semantics in a SELECT list."
  [expr]
  (and (instance? Function expr)
       (contains? #{"generate_series" "unnest"}
                  (srf-base-name (.getName ^Function expr)))))

(defn contains-target-list-srf?
  "Whether an expression contains a ProjectSet-capable SRF in this query
   level. `ast-function-names` deliberately does not descend into subqueries,
   matching PostgreSQL's rule that an SRF inside a nested SELECT belongs to
   that SELECT's placement context."
  [expr]
  (boolean
   (some #(contains? #{"generate_series" "unnest"} (srf-base-name %))
         (params/ast-function-names expr))))

(defn- reject-prohibited-target-srf!
  "Reject expression contexts in which PostgreSQL cannot conditionally or
   repeatedly execute an SRF. Scalar expressions around SRFs remain legal;
   these are the explicit parse_func/parse_agg placement boundaries."
  [expr]
  (when (contains-target-list-srf? expr)
    (cond
      (instance? CaseExpression expr)
      (throw (errors/pg-error
              :feature-not-supported
              {:message "set-returning functions are not allowed in CASE"}))

      (and (instance? Function expr)
           (= "coalesce" (srf-base-name (.getName ^Function expr))))
      (throw (errors/pg-error
              :feature-not-supported
              {:message "set-returning functions are not allowed in COALESCE"}))

      (and (instance? Function expr)
           (fns/aggregate-function?
            (srf-base-name (.getName ^Function expr))))
      (throw (errors/pg-error
              :feature-not-supported
              {:message "aggregate function calls cannot contain set-returning function calls"}))

      (instance? net.sf.jsqlparser.expression.AnalyticExpression expr)
      (throw (errors/pg-error
              :feature-not-supported
              {:message "window function calls cannot contain set-returning function calls"})))))

(defn validate-srf-row-count!
  "Reject SRFs in LIMIT/FETCH/OFFSET before any table-free SELECT shortcut can
   bypass the ordinary relational translator."
  [^PlainSelect select]
  (let [limit (some-> (.getLimit select) .getRowCount)
        fetch (some-> (.getFetch select) .getExpression)
        offset (some-> (.getOffset select) .getOffset)]
    (when (or (contains-target-list-srf? limit)
              (contains-target-list-srf? fetch))
      (throw (errors/pg-error
              :feature-not-supported
              {:message "set-returning functions are not allowed in LIMIT"})))
    (when (contains-target-list-srf? offset)
      (throw (errors/pg-error
              :feature-not-supported
              {:message "set-returning functions are not allowed in OFFSET"})))))

(defn materialize-table-function
  "Produce rows for a `TableFunction` FROM item. Supports the common
   constant-arg set-returning functions:
     - `unnest(ARRAY[…])`            (+ WITH ORDINALITY)
     - `generate_series(start,stop[,step])` over integers (+ WITH ORDINALITY)
     - `now()` / `current_timestamp` / `{statement,transaction,clock}_timestamp`
       (one row, current time)

   Returns {:aliases [col-names] :rows [[v1 v2 …] …] :vtypes [kw kw …]}
   or nil if the function isn't one we expand.

   `eval-fn` resolves an argument expression to a value; it defaults to
   `srf-const-eval` (literals only). The LATERAL nested-loop will pass an
   eval-fn that resolves correlated arguments per outer row — see
   srf-const-eval's note. That is why arguments flow through eval-fn here
   rather than being pattern-matched as literals inline."
  ([tf] (materialize-table-function tf srf-const-eval nil))
  ([tf eval-fn] (materialize-table-function tf eval-fn nil))
  ([^net.sf.jsqlparser.statement.select.TableFunction tf eval-fn coldefs]
   (let [^net.sf.jsqlparser.expression.Function f (.getFunction tf)
         ;; Strip a schema qualifier: PostgreSQL resolves `pg_catalog.foo()`
         ;; and `foo()` to the same function through search_path, and pgjdbc
         ;; writes the qualified form. Matching the raw name meant EVERY
         ;; schema-qualified SRF in FROM missed this cond, returned nil, and
         ;; surfaced as the internal `Query for unknown vars: [?_eid]` --
         ;; `pg_catalog.generate_series(1,3)` included.
         fname (srf-base-name (.getName f))
         params (vec (or (.getParameters f) []))
         _ (when (and (not (target-list-srf? f))
                      (contains-target-list-srf? f))
             (throw (errors/pg-error
                     :feature-not-supported
                     {:message "set-returning functions must appear at top level of FROM"})))
         with-ord? (some-> (.getWithClause tf) str
                           (->> (= "ORDINALITY")))
         vtype-of (fn [v]
                    (cond
                      (instance? Long v)    :db.type/long
                      (instance? Double v)  :db.type/double
                      (instance? java.math.BigDecimal v) :db.type/bigdec
                      (types/numeric-special? v) :db.type/bigdec
                      (instance? Boolean v) :db.type/boolean
                      (inst? v)             :db.type/instant
                      :else                 :db.type/string))
         ;; pg-types: optional per-column :pg/type override (e.g. "int4")
         ;; so the virtual-table column advertises the PG width even though
         ;; Datahike stores a long. nil entry = use the :db/valueType OID.
         with-ordinality (fn [aliases rows vtypes pg-types]
                           (if with-ord?
                             {:aliases (conj aliases "ordinality")
                              :rows    (vec (map-indexed (fn [i r] (conj (vec r) (long (inc i)))) rows))
                              :vtypes  (conj vtypes :db.type/long)
                              ;; PG numbers ordinality as bigint (int8) — no override
                              :pg-types (conj (vec pg-types) nil)}
                             {:aliases aliases :rows rows :vtypes vtypes
                              :pg-types (vec pg-types)}))]
     (cond
       (= fname "pg_input_error_info")
       (let [[value type-name] (mapv eval-fn params)]
         (when (= 2 (count params))
           (with-ordinality ["message" "detail" "hint" "sql_error_code"]
             [(if-let [values (params/registered-enum-values type-name)]
                (if (contains? values (str value))
                  [nil nil nil nil]
                  [(str "invalid input value for enum " type-name ": "
                        (pr-str (str value))) nil nil "22P02"])
                (fns/pg-input-error-info value type-name))]
             [:db.type/string :db.type/string :db.type/string :db.type/string]
             ["text" "text" "text" "text"])))

       (= fname "unnest")
       ;; PG `unnest` flattens ALL dimensions into one row per leaf
       ;; (`arrayfuncs.c`, ArrayGetNItems over ndim) — `ARRAY[[1,2],[3,4]]`
       ;; yields 4 rows, not 2 sub-arrays.
       (let [pa (when (seq params) (eval-fn (first params)))
             vals (cond
                    (pg-arr/array? pa) (vec (pg-arr/flat-elements pa))
                    (sequential? pa)   (vec pa)
                    :else              nil)]
         (when vals
           (with-ordinality ["unnest"] (mapv vector vals) [(vtype-of (first vals))] [nil])))

       (= fname "generate_series")
       (let [args (mapv eval-fn params)]
         (when (>= (count args) 2)
           (let [[start stop supplied-step] args
                 numeric? (some #(or (instance? java.math.BigDecimal %)
                                     (types/numeric-special? %))
                                (take 3 args))]
             (cond
               numeric?
               (let [as-numeric #(if (integer? %) (bigdec %) %)
                     start (as-numeric start)
                     stop (as-numeric stop)
                     step (as-numeric (or supplied-step java.math.BigDecimal/ONE))]
                 (when (every? #(or (instance? java.math.BigDecimal %)
                                    (types/numeric-special? %))
                               [start stop step])
                   (with-ordinality ["generate_series"]
                     (numeric-series start stop step)
                     [:db.type/bigdec] ["numeric"])))

               (every? integer? (take 3 args))
               (let [step (long (or supplied-step 1))]
                 (when-not (zero? step)
                   (let [vals (vec (range start
                                          (if (pos? step) (inc stop) (dec stop))
                                          step))]
                     ;; PG types integer generate_series as int4 — advertise
                     ;; that so clients parse the values as numbers, not int8
                     ;; strings.
                     (with-ordinality ["generate_series"]
                       (mapv (fn [v] [(long v)]) vals)
                       [:db.type/long] ["int4"]))))))))

       ;; json_to_recordset / jsonb_to_recordset expand an ARRAY of
       ;; objects into typed rows; the *_record forms take one object.
       ;; Their shape comes entirely from the `AS r(a int, b text)`
       ;; column-definition list -- PostgreSQL raises 42601 without one
       ;; -- which is why the alias' colDataType had to be threaded in
       ;; here rather than only its name.
       (contains? #{"json_to_recordset" "jsonb_to_recordset"
                    "json_to_record" "jsonb_to_record"} fname)
       (when (seq coldefs)
         (let [v (eval-fn (first params))
               parsed (jb/parse-jsonb v)
               maps (if (str/ends-with? fname "recordset")
                      (if (sequential? parsed) parsed [])
                      [parsed])
               names (mapv first coldefs)
               types (mapv second coldefs)
               ;; The cast produces the value the DECLARED type implies —
               ;; `date` gives a LocalDate — but the row is transacted
               ;; into an attribute whose storage type is
               ;; :db.type/instant, which only accepts a java.util.Date.
               ;; Without this normalisation `AS r(d date)` failed the
               ;; transaction with `invalid input syntax for column "d"`.
               ->storage (fn [v]
                           (condp instance? v
                             java.time.LocalDate
                             (java.util.Date/from
                              (.toInstant (.atStartOfDay ^java.time.LocalDate v
                                                         java.time.ZoneOffset/UTC)))
                             java.time.LocalDateTime
                             (java.util.Date/from
                              (.toInstant ^java.time.LocalDateTime v java.time.ZoneOffset/UTC))
                             v))
               cell (fn [m t nm]
                      (let [raw (when (map? m) (get m nm))]
                        (when (some? raw)
                          (->storage
                           (try (sql-cast/cast-scalar raw t {:explicit? true})
                                (catch Throwable _ raw))))))]
           (with-ordinality names
             (mapv (fn [m] (mapv (fn [t nm] (cell m t nm)) types names)) maps)
             (mapv (fn [t] (or (get types/sql-name->dh-type (str/lower-case t))
                               :db.type/string))
                   types)
             (mapv coldef-pg-type types))))

       ;; The json/jsonb expansion family. Every implementation already
       ;; existed in datahike.pg.jsonb and was wired for the SELECT-list
       ;; path, where it SERIALISES the whole collection into one cell;
       ;; in FROM position it has to become rows. Without an entry here
       ;; the FROM item materialised to nothing and the query answered
       ;; ZERO ROWS silently -- or, with count(*), the internal
       ;; `Query for unknown vars: [?_eid]`.
       ;;
       ;; The `json_` and `jsonb_` spellings differ only in the
       ;; punctuation of a returned document: json_each gives
       ;; `{"x":1}` where jsonb_each gives `{"x": 1}`.
       (contains? #{"jsonb_array_elements" "json_array_elements"} fname)
       (let [json? (str/starts-with? fname "json_")
             ser (if json? jb/serialize-json jb/serialize-jsonb)]
         (with-ordinality ["value"]
           (mapv (fn [x] [(ser x)]) (jb/jsonb-array-elements (eval-fn (first params))))
           [:db.type/string] [(if json? "json" "jsonb")]))

       (contains? #{"jsonb_array_elements_text" "json_array_elements_text"} fname)
       (with-ordinality ["value"]
         (mapv vector (jb/jsonb-array-elements-text (eval-fn (first params))))
         [:db.type/string] ["text"])

       (contains? #{"jsonb_each" "json_each"} fname)
       (let [json? (str/starts-with? fname "json_")
             ser (if json? jb/serialize-json jb/serialize-jsonb)]
         (with-ordinality ["key" "value"]
           (mapv (fn [[k v]] [k (ser v)]) (jb/jsonb-each (eval-fn (first params))))
           [:db.type/string :db.type/string] ["text" (if json? "json" "jsonb")]))

       (contains? #{"jsonb_each_text" "json_each_text"} fname)
       (with-ordinality ["key" "value"]
         (mapv vec (jb/jsonb-each-text (eval-fn (first params))))
         [:db.type/string :db.type/string] ["text" "text"])

       (contains? #{"jsonb_object_keys" "json_object_keys"} fname)
       ;; PG names the column after the function, not "value".
       (with-ordinality [fname]
         (mapv vector (jb/jsonb-object-keys (eval-fn (first params))))
         [:db.type/string] ["text"])

       ;; regexp_split_to_table(string, pattern) / string_to_table(string,
       ;; delimiter) — the difference is regex vs literal separator.
       (contains? #{"regexp_split_to_table" "string_to_table"} fname)
       (let [[sv dv] (mapv eval-fn (take 2 params))]
         (when (and (string? sv) (string? dv))
           (with-ordinality [fname]
             (mapv vector
                   (if (= fname "regexp_split_to_table")
                     (str/split sv (re-pattern dv))
                     (str/split sv (re-pattern (java.util.regex.Pattern/quote dv)))))
             [:db.type/string] ["text"])))

       ;; pg_get_keywords() — a real catalog SRF. pgjdbc calls it on
       ;; every connection through getSQLKeywords(); without it the
       ;; aggregate over a missing relation answered one NULL row, and
       ;; pgjdbc's castNonNull turns that into an AssertionError, which
       ;; Hibernate's catch(SQLException) fallback does not catch.
       (= fname "pg_get_keywords")
       (with-ordinality ["word" "catcode" "barelabel" "catdesc" "baredesc"]
         pg-kw/keyword-rows
         [:db.type/string :db.type/string :db.type/boolean
          :db.type/string :db.type/string]
         ["text" "char" "bool" "text" "text"])

       (contains? #{"now" "current_timestamp" "transaction_timestamp"
                    "statement_timestamp" "clock_timestamp"} fname)
       ;; Scalar function used as a one-row table. The timestamp is
       ;; captured at translate time (good enough — the value is "recent";
       ;; sub-statement clock precision isn't meaningful here).
       (with-ordinality [fname] [[(java.util.Date.)]] [:db.type/instant] [nil])

       :else nil))))

(defn- project-set-values
  "Evaluate one top-level target-list SRF for a base result row."
  [^Function f args]
  (let [params (vec (or (.getParameters f) []))
        values (mapv #(if (= :__null__ %) nil %) args)
        by-expr (java.util.IdentityHashMap.)]
    (doseq [[e v] (map vector params values)]
      (.put by-expr e v))
    (if-let [materialized
             (materialize-table-function
              (net.sf.jsqlparser.statement.select.TableFunction. f)
              (fn [e]
                (if (.containsKey by-expr e)
                  (.get by-expr e)
                  ::corr)))]
      (mapv first (:rows materialized))
      [])))

(defn apply-project-set
  "Expand base SELECT rows using PostgreSQL ProjectSet semantics.

   Every SRF is evaluated once per base row. Multiple SRFs advance in
   parallel to the longest result and shorter results are padded with SQL
   NULL. A base row disappears only when every SRF is empty."
  [rows specs]
  (let [apply-level
        (fn [rows level-specs]
          (vec
           (mapcat
            (fn [row]
              (let [row (if (sequential? row) (vec row) [row])
                    values (mapv (fn [{:keys [function arg-indices]}]
                                   (project-set-values
                                    function (mapv #(nth row % nil) arg-indices)))
                                 level-specs)
                    n (reduce max 0 (map count values))]
                (for [i (range n)]
                  (reduce (fn [out [{:keys [out-pos]} vs]]
                            (assoc out out-pos (if (< i (count vs))
                                                 (nth vs i)
                                                 :__null__)))
                          row
                          (map vector level-specs values)))))
            rows)))]
    (reduce apply-level rows
            (map second (sort-by first (group-by :level specs))))))

(defn- table-fn->virtual-table
  "Materialise a constant-arg `TableFunction` FROM item into a virtual
   table in a speculative db, so the outer query can scan/join it like a
   real relation. Returns {:db :schema :name :alias :aliases} or nil.

   The FROM alias becomes the table name; for a single-column SRF the
   alias also names the column, matching PG (`generate_series(2,4) AS foo`
   projects a column named `foo`)."
  [^net.sf.jsqlparser.statement.select.TableFunction tf db]
  (let [talias (when-let [a (.getAlias tf)]
                 (unquote-ident (str/trim (.getName ^Alias a))))
        ;; `AS s(r)` renames the columns positionally. Without this the
        ;; column kept the ALIAS name, so `SELECT r FROM
        ;; generate_series(1,3) AS s(r)` resolved nothing — it read as
        ;; NULL before the unknown-column check and as 42703 after.
        ;; pgjdbc's TypeInfoCache introspection uses exactly this shape,
        ;; which is why ResultSet.getObject on a jsonb column failed.
        alias-cols (when-let [a (.getAlias tf)]
                     (seq (mapv (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                  (unquote-ident (.-name c)))
                                (or (.getAliasColumns ^Alias a) []))))
        ;; `AS r(a int, b text)` carries a TYPE per column as well as a
        ;; name. Only the names were read, so the record-shaping SRFs --
        ;; whose entire shape comes from this list -- had nothing to
        ;; build from.
        alias-coldefs (when-let [a (.getAlias tf)]
                        (seq (keep (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                     (when-let [t (.-colDataType c)]
                                       [(unquote-ident (.-name c)) (str t)]))
                                   (or (.getAliasColumns ^Alias a) []))))
        fname  (or (srf-base-name (.getName (.getFunction tf))) "tf")
        ;; Storage namespace, not the user's alias — see
        ;; materialize-derived-select! for why they must differ.
        sub-name (str "__srf__" (or talias fname))]
    (when-let [{:keys [aliases rows vtypes pg-types]}
               (materialize-table-function
                tf
                ;; Literals first; then fall back to evaluating a CONSTANT
                ;; scalar expression. `srf-const-eval` alone only knew
                ;; literals, so `generate_series(1, array_upper(
                ;; current_schemas(false), 1))` -- the shape in pgjdbc's
                ;; TypeInfoCache probe -- failed to materialise and its
                ;; alias resolved to nothing, which is why
                ;; ResultSet.getObject on any non-trivial type failed.
                ;;
                ;; A genuinely correlated argument (a Column, under
                ;; LATERAL) does not evaluate standalone: the inner parse
                ;; raises, eval-corr-scalar answers nil, and we return
                ;; ::corr exactly as before. That keeps this a widening of
                ;; the constant case rather than a change to the
                ;; correlated one.
                (fn [e]
                  (let [v (srf-const-eval e)]
                    (if (not= ::corr v)
                      v
                      (let [pf params/*parse-sql*]
                        (if-let [r (and pf db
                                        (eval-corr-scalar pf (str e) false
                                                          (:schema db) db))]
                          r
                          ::corr)))))
                alias-coldefs)]
      (let [_ (when (and alias-cols (> (count alias-cols) (count aliases)))
                (throw (ex-info (str "table \"" (or talias "") "\" has " (count aliases)
                                     " columns available but " (count alias-cols)
                                     " columns specified")
                                {:error :invalid-column-reference :sqlstate "42P10"})))
            aliases (cond
                      ;; Positional rename; PostgreSQL lets the list be
                      ;; SHORTER than the column list, leaving the rest.
                      alias-cols (vec (map-indexed (fn [i a] (or (nth alias-cols i nil) a))
                                                   aliases))
                      (and talias (= 1 (count aliases))) [talias]
                      :else aliases)
            ;; The row-existence marker. `count(*)` and `SELECT *` have no
            ;; column to enumerate otherwise, so a scan returned ZERO rows
            ;; even though the rows are there:
            ;;   SELECT count(*) FROM generate_series(1,10) g  ->  0
            ;; while `count(g)` correctly answered 10.
            ;; sequence->virtual-table below already transacts one and its
            ;; comment names this exact hazard.
            marker (pgs/row-marker-attr sub-name)
            schema-tx (conj (mapv (fn [a vt pt]
                                    (cond-> {:db/ident       (keyword sub-name a)
                                             :db/valueType   vt
                                             :db/cardinality :db.cardinality/one}
                                      pt (assoc :pg/type pt)))
                                  aliases vtypes (concat (or pg-types []) (repeat nil)))
                            {:db/ident       marker
                             :db/valueType   :db.type/boolean
                             :db/cardinality :db.cardinality/one})
            spec-db (d/db-with db schema-tx)
            data-tx (mapv (fn [row]
                            (into {marker true}
                                  (keep-indexed
                                   (fn [i a]
                                     (let [v (nth row i nil)]
                                       (when (some? v) [(keyword sub-name a) v])))
                                   aliases)))
                          rows)
            spec-db2 (if (seq data-tx) (d/db-with spec-db data-tx) spec-db)]
        {:db spec-db2 :schema (:schema spec-db2)
         :name sub-name :alias (or talias sub-name) :aliases aliases}))))

(def ^:private correlated-srf-shapes
  "Column shape for the SRFs we can evaluate PER OUTER ROW.

   A correlated SRF cannot be materialised once, so its columns have to
   be declared without looking at the data — that is the only thing this
   map is for. `:cols` are the default column names PostgreSQL uses,
   `:vtypes` their Datahike storage types, `:pg-types` the declared type
   to advertise (nil = derive from the storage type).

   Deliberately smaller than `known-srf-names`: an SRF whose output type
   depends on its INPUT data (unnest of an arbitrary array, the json
   expansions) cannot state a static shape, so it stays on the
   materialise-once path and a correlated use of it still raises."
  {"generate_series" {:cols ["generate_series"] :vtypes [:db.type/long]
                      :pg-types ["int4"]}
   "regexp_split_to_table" {:cols ["regexp_split_to_table"] :vtypes [:db.type/string]
                            :pg-types ["text"]}
   "string_to_table" {:cols ["string_to_table"] :vtypes [:db.type/string]
                      :pg-types ["text"]}})

(defn- srf-rows-fn
  "Runtime row producer for a correlated SRF: a fn of the evaluated
   arguments returning a VECTOR OF TUPLES.

   Must never return nil. `bind-by-fn` drops the outer tuple when the fn
   answers nil (query.cljc: `:when (not (nil? val))`), which is
   indistinguishable from an empty result for an inner LATERAL but would
   be wrong for an outer one — and a nil here would silently swallow
   rows rather than raise."
  [fname]
  (case fname
    "generate_series"
    (fn [& args]
      (let [[start stop step] (map #(when (number? %) (long %)) args)
            step (or step 1)]
        (if (or (nil? start) (nil? stop) (zero? step))
          []
          (mapv vector (range start (if (pos? step) (inc stop) (dec stop)) step)))))

    ("regexp_split_to_table" "string_to_table")
    (fn [sv dv]
      (if-not (and (string? sv) (string? dv))
        []
        (mapv vector
              (if (= fname "regexp_split_to_table")
                (str/split sv (re-pattern dv))
                (str/split sv (re-pattern (java.util.regex.Pattern/quote dv)))))))
    nil))

(defn- correlated-table-fn->spec
  "A `TableFunction` FROM item whose arguments reference outer columns —
   `FROM t, LATERAL generate_series(1, t.n)`.

   It cannot be materialised once, because its rows depend on the outer
   row. But Datahike's function binding already IS a parameterized nested
   loop: `bind-by-fn` applies the fn once per production tuple and
   expands the result through the binding form, so

     [(f ?n) [[?v ?ord]]]

   yields one row per element PER OUTER ROW inside the ordinary flat
   `:where`. No second query, no speculative data, no nested-loop step
   outside the single Datalog query.

   What still has to exist is the RELATION: `SELECT *`, `count(*)` and
   OID inference all read the schema. So register the columns (with no
   data) in a speculative db and hand back the vars the emitter will
   bind, which `ctx/col-var!` is pre-seeded with so a column reference
   resolves to the bound var instead of emitting an attribute lookup.

   Returns nil when this is not a correlated SRF we can shape."
  [^net.sf.jsqlparser.statement.select.TableFunction tf db var-counter]
  (let [^net.sf.jsqlparser.expression.Function f (.getFunction tf)
        fname (srf-base-name (.getName f))
        shape (get correlated-srf-shapes fname)
        params (vec (or (.getParameters f) []))
        ;; Correlation is a SYNTACTIC test on purpose. Evaluating instead does not
        ;; discriminate: `srf-const-eval` answers ::corr for a constant
        ;; EXPRESSION too (`array_upper(current_schemas(false), 1)` --
        ;; the pgjdbc TypeInfoCache shape), and the widened evaluation
        ;; the materialise-once path uses goes the other way, happily
        ;; resolving `t.n` against some arbitrary row and reporting it
        ;; constant. The current binding path supports a bare outer column;
        ;; nested expressions are detected separately and rejected rather
        ;; than materialised as constants.
        corr? (some #(instance? Column %) params)]
    (when (and shape corr? (seq params))
      (let [talias (when-let [a (.getAlias tf)]
                     (unquote-ident (.getName ^Alias a)))
            alias-cols (when-let [a (.getAlias tf)]
                         (seq (mapv (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                      (unquote-ident (.-name c)))
                                    (or (.getAliasColumns ^Alias a) []))))
            base-cols (:cols shape)
            ;; `AS g(x)` renames positionally; a single-column SRF also
            ;; takes the bare alias as its column name, as PG does for
            ;; `generate_series(1,3) AS foo`.
            cols (cond
                   alias-cols (vec (map-indexed (fn [i c] (or (nth alias-cols i nil) c))
                                                base-cols))
                   (and talias (= 1 (count base-cols))) [talias]
                   :else base-cols)
            sub-name (str "__lsrf__" (or talias fname))
            marker (pgs/row-marker-attr sub-name)
            schema-tx (conj (mapv (fn [c vt pt]
                                    (cond-> {:db/ident (keyword sub-name c)
                                             :db/valueType vt
                                             :db/cardinality :db.cardinality/one}
                                      pt (assoc :pg/type pt)))
                                  cols (:vtypes shape)
                                  (concat (or (:pg-types shape) []) (repeat nil)))
                            {:db/ident marker
                             :db/valueType :db.type/boolean
                             :db/cardinality :db.cardinality/one})
            spec-db (d/db-with db schema-tx)
            vars (mapv (fn [c] (symbol (str "?" sub-name "_" c))) cols)
            ;; Ordinality is NOT a column here: it exists so Datalog's SET
            ;; semantics cannot collapse duplicate rows. `unnest(ARRAY[1,1,1])`
            ;; returns ONE row without it, verified against the engine.
            ord-var (symbol (str "?" sub-name "_ord" (swap! var-counter inc)))]
        {:db spec-db :schema (:schema spec-db)
         :name sub-name :alias (or talias sub-name)
         :cols cols :vars vars :ord-var ord-var
         :marker marker :fname fname :params params}))))

(defn- select-item-col-name
  "Output column name for one inner SELECT item, as PostgreSQL names it:
   the explicit alias, else a plain column's own name, else the function
   name, else a positional `?column?`-style fallback."
  [^net.sf.jsqlparser.statement.select.SelectItem si i]
  (or (when-let [a (.getAlias si)] (unquote-ident (str/trim (.getName ^Alias a))))
      (let [e (.getExpression si)]
        (cond
          (instance? Column e) (str/lower-case (unquote-ident (.getColumnName ^Column e)))
          (instance? net.sf.jsqlparser.expression.Function e)
          (str/lower-case (srf-base-name (.getName ^net.sf.jsqlparser.expression.Function e)))
          :else nil))
      (str "column" (inc i))))

(declare correlated-subquery-refs)
(declare eval-values-literal)

(defn- lateral-rows-fn
  "Row producer for a correlated LATERAL subquery: given the outer
   values (in `corr-refs` order), run the inner and return its rows as a
   vector of tuples.

   NEVER returns nil — `bind-by-fn` drops the outer tuple on nil, which
   would silently swallow rows rather than raise. An empty result IS the
   right answer for an inner LATERAL: PostgreSQL eliminates the outer
   row, and an empty collection binding does exactly that.

   The inner is parsed per invocation. `sql/parse-sql` refuses to cache
   a parse made under `*from-bindings*` (the bindings are substituted
   into the AST), so hoisting the parse out of the loop would need the
   bindings threaded as parameters instead — worth doing, and noted in
   the backlog, but correctness first."
  [inner-sql corr-refs inner-schema query-db n-cols]
  ;; Capture the parse fn NOW. `*parse-sql*` is bound during
  ;; TRANSLATION; this closure runs during query EXECUTION, by which
  ;; time the binding is gone. Reading it there yielded nil, so every
  ;; invocation returned no rows — and because an empty LATERAL
  ;; eliminates its outer row, the whole query answered empty rather
  ;; than failing.
  (let [parse-fn params/*parse-sql*]
    (fn [& outer-vals]
      (let [fb (reduce (fn [m [[a c] v]] (assoc-in m [a c] v))
                       {} (map vector corr-refs outer-vals))]
        (binding [params/*from-bindings* fb
                  ;; The outer row is an ENCLOSING level. A bare column
                  ;; resolves to it only when the inner level has no such
                  ;; column -- so `VALUES (outer_col)` works, and `(SELECT id
                  ;; FROM ft t2 WHERE t2.id = ft.id)` keeps its `id` on t2.
                  ;; Treating the outer row as a SAME-level source made that
                  ;; bare `id` ambiguous between t2 and ft; the error was
                  ;; swallowed, the producer returned no rows, and an INNER
                  ;; LATERAL over the same table answered empty.
                  params/*outer-scope-aliases* (set (map first corr-refs))
                  params/*lateral-outer-aliases* (set (map first corr-refs))]
          (or (try
                (let [p (when parse-fn (parse-fn inner-sql inner-schema query-db))
                      plans (if (and (= :set-operation (:type p))
                                     (= :union-all (:op p)))
                              (:sub-results p)
                              [p])
                      first-q (some :query plans)
                      run-plan (fn [plan]
                                 (when-let [q (:query plan)]
                                   (let [ia (:in-args plan)
                                         qdb (or (:enriched-db plan)
                                                 (:enriched-db p)
                                                 query-db)]
                                     (if (seq ia)
                                       (apply d/q q qdb ia)
                                       (d/q q qdb)))))
                      res (vec (mapcat #(or (run-plan %) []) plans))]
                  (when first-q
                    (let [res res
                          ;; An aggregate over an empty relation is still
                          ;; ONE row -- `LATERAL (SELECT count(*) … WHERE
                          ;; ch.pid = t.id)` is 0 for an outer row with no
                          ;; children, not "no row". The same rule every
                          ;; other subquery evaluator applies.
                          res (or (when (empty? (seq res))
                                    (expr/empty-aggregate-row first-q))
                                  res)]
                      ;; TAKE the visible columns. The inner's `:find`
                      ;; carries trailing bookkeeping vars — the entity
                      ;; var for ordering/bag semantics, and any hidden
                      ;; grouping keys — which are stripped at the wire
                      ;; layer, not here. Passing them through made the
                      ;; produced tuple wider than the binding form, so
                      ;; the relation binding matched nothing.
                      ;; NO ordinality here: the emitter appends it for
                      ;; every producer, so adding it a second time made
                      ;; each tuple one element wider than the binding
                      ;; form it feeds.
                      (mapv (fn [r] (vec (take n-cols (if (sequential? r) r [r]))))
                            res))))
                (catch Throwable _ nil))
              []))))))

(defn- trivially-true-on?
  "True when a JOIN's ON condition is the constant TRUE -- `ON true`, or
   absent. The only condition an OUTER LATERAL can be given without
   changing what its NULL-extended row means."
  [^Join j]
  (let [es (seq (.getOnExpressions j))]
    (or (nil? es)
        (and (= 1 (count es))
             (let [t (str/lower-case (str/trim (str (first es))))]
               (or (= t "true") (= t "1 = 1")))))))

(defn- values-expression-rows
  "Return a VALUES node as rows of expression ASTs. JSqlParser flattens a
   single row but wraps each row of a multi-row VALUES in its own expression
   list."
  [^Values values]
  (let [raw (vec (.getExpressions values))]
    (if (and (seq raw) (instance? ParenthesedExpressionList (first raw)))
      (mapv (fn [^ParenthesedExpressionList row] (vec row)) raw)
      [raw])))

(defn- column-vtype
  "Best-effort storage type for a column expression in a virtual relation."
  [^Column c schema]
  (let [table (some-> (.getTable c) .getName unquote-ident str/lower-case)
        col (str/lower-case (unquote-ident (.getColumnName c)))
        matches (keep (fn [[k attr]]
                        (when (and (keyword? k)
                                   (= col (str/lower-case (name k)))
                                   (or (nil? table)
                                       (= table (str/lower-case (namespace k)))
                                       (str/ends-with? (str/lower-case (namespace k))
                                                       (str "__" table))))
                          (:db/valueType attr)))
                      schema)]
    (when (= 1 (count (distinct matches))) (first matches))))

(defn- expression-vtype
  "Infer enough of an expression's Datahike carrier type to declare a
   correlated VALUES relation. This deliberately follows numeric promotion;
   other complex expressions retain the existing text fallback."
  [e schema]
  (cond
    (instance? LongValue e) :db.type/long
    (instance? DoubleValue e) :db.type/bigdec
    (instance? Column e) (column-vtype e schema)
    (instance? SignedExpression e)
    (expression-vtype (.getExpression ^SignedExpression e) schema)
    (instance? Parenthesis e)
    (expression-vtype (.getExpression ^Parenthesis e) schema)
    (instance? CastExpression e)
    (let [^CastExpression ce e
          target (some-> (.getColDataType ce) .getDataType str str/lower-case)]
      (case (types/cast-category target)
        :integer :db.type/long
        :float :db.type/double
        :numeric :db.type/bigdec
        :boolean :db.type/boolean
        :text :db.type/string
        (expression-vtype (.getLeftExpression ce) schema)))
    (instance? net.sf.jsqlparser.expression.BinaryExpression e)
    (let [^net.sf.jsqlparser.expression.BinaryExpression be e
          types (set (keep #(expression-vtype % schema)
                           [(.getLeftExpression be) (.getRightExpression be)]))]
      (cond
        (types :db.type/bigdec) :db.type/bigdec
        (types :db.type/double) :db.type/double
        (types :db.type/long) :db.type/long
        :else nil))
    :else nil))

(defn- lateral-values-corr-refs
  "Correlation references made by a lateral VALUES body.

   Qualified references use the normal detector. PostgreSQL also permits bare
   outer columns here. Resolve those only when their schema attribute has one
   logical owner in scope. Correlation arguments use the storage namespace;
   this matters for derived relations, whose user alias has no attributes."
  [^Values values outer-aliases schema]
  (let [raw-qualified (or (correlated-subquery-refs values outer-aliases) #{})
        storage-owner (fn [alias col]
                        (let [a (str/lower-case alias)
                              c (str/lower-case col)]
                          (or (some (fn [k]
                                      (let [ns (when (keyword? k)
                                                 (str/lower-case (namespace k)))]
                                        (when (and ns (= c (str/lower-case (name k)))
                                                   (= ns a))
                                          ns)))
                                    (keys schema))
                              (some (fn [k]
                                      (let [ns (when (keyword? k)
                                                 (str/lower-case (namespace k)))]
                                        (when (and ns (= c (str/lower-case (name k)))
                                                   (str/ends-with? ns (str "__" a))
                                                   (contains? outer-aliases ns))
                                          ns)))
                                    (keys schema))
                              alias)))
        qualified (set (map (fn [[a c]] [(storage-owner a c) c]) raw-qualified))
        alias-for-ns (fn [ns]
                       (or (some #(when (= ns (str/lower-case %)) %) outer-aliases)
                           (first
                            (sort-by count
                                     (filter (fn [a]
                                               (str/ends-with?
                                                ns (str "__" (str/lower-case a))))
                                             outer-aliases)))))
        candidates (reduce (fn [m k]
                             (if (and (keyword? k)
                                      (not= "db-row-exists" (name k)))
                               (let [ns (str/lower-case (namespace k))]
                                 (if-let [a (alias-for-ns ns)]
                                   (update m (str/lower-case (name k))
                                           (fnil conj #{}) a)
                                   m))
                               m))
                           {} (keys schema))
        sql (str values)
        bare (for [[col aliases] candidates
                   :when (= 1 (count aliases))
                   :let [alias (first aliases)]
                   :when (re-find
                          (re-pattern
                           (str "(?i)(?<![\\w.])"
                                (java.util.regex.Pattern/quote col)
                                "(?![\\w]|\\s*\\.)"))
                          sql)]
               [alias col])]
    (not-empty (into (set qualified) bare))))

(defn- values-as-select-sql
  "Turn a VALUES body into SELECT branches so the existing correlated
   subquery executor can translate each expression under outer bindings."
  [^Values values]
  (str/join " UNION ALL "
            (map (fn [row] (str "SELECT " (str/join ", " (map str row))))
                 (values-expression-rows values))))

(defn- lateral-subselect->spec
  "`JOIN LATERAL (SELECT …) s ON true` whose inner references an outer
   column.

   Same shape as a correlated SRF: the inner is run once per outer row
   through a function binding, so the whole thing stays inside one
   Datalog query. The difference is that the \"function\" is a SQL
   subquery, executed with `*from-bindings*` holding the outer values —
   the mechanism `run-correlated-spec` already uses for correlated
   scalar subqueries in the SELECT list.

   The relation's COLUMN SHAPE has to be known at translate time, and
   the inner cannot be executed to find it (that is what being
   correlated means). Names come from the inner's select items, as
   PostgreSQL derives them. Types are best-effort: a plain column
   reference resolves against the schema, anything else defaults to
   text. Nothing is ever transacted into these attributes — they exist
   so `SELECT *`, `count(*)` and OID inference have a relation to read —
   so a wrong guess costs a reported OID, not a wrong value.

   Returns nil for an UNcorrelated derived table, which belongs on the
   existing materialise-once path."
  [^net.sf.jsqlparser.statement.select.LateralSubSelect ls db schema
   outer-aliases var-counter outer-level]
  (let [inner (.getSelect ls)
        values? (instance? Values inner)
        corr-refs (seq (if values?
                         (lateral-values-corr-refs inner outer-aliases schema)
                         ;; `outer-level` is what lets an UNQUALIFIED name
                         ;; inside the LATERAL be the outer item's column,
                         ;; as `FROM t, LATERAL (SELECT b)` means.
                         (correlated-subquery-refs inner outer-aliases outer-level)))]
    (when (and corr-refs (or (instance? PlainSelect inner) values?))
      (let [talias (when-let [a (.getAlias ls)]
                     (unquote-ident (str/trim (.getName ^Alias a))))
            alias-cols (when-let [a (.getAlias ls)]
                         (seq (mapv (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                      (unquote-ident (.-name c)))
                                    (or (.getAliasColumns ^Alias a) []))))
            items (when-not values? (vec (.getSelectItems ^PlainSelect inner)))
            value-rows (when values? (values-expression-rows inner))
            cols (vec (or alias-cols
                          (map-indexed (fn [i si] (select-item-col-name si i)) items)
                          (map-indexed (fn [i _] (str "column" (inc i)))
                                       (first value-rows))))
            ;; Best-effort storage type: a bare column reference keeps
            ;; the type it has in the schema.
            ;;
            ;; An UNQUALIFIED column belongs to the inner's own FROM item
            ;; -- `(SELECT v FROM c WHERE …)` projects c.v. Requiring the
            ;; qualifier meant every unqualified projection fell back to
            ;; :db.type/string, so the relation declared TEXT for an
            ;; integer column: Describe reported the wrong type, and
            ;; `WHERE s.v > 10` looked like text > integer.
            inner-from-name (when (instance? PlainSelect inner)
                              (when-let [fi (.getFromItem ^PlainSelect inner)]
                                (when (instance? net.sf.jsqlparser.schema.Table fi)
                                  (some-> (.getName ^net.sf.jsqlparser.schema.Table fi)
                                          unquote-ident str/lower-case))))
            vtype-of (fn [^net.sf.jsqlparser.statement.select.SelectItem si]
                       (let [e (.getExpression si)]
                         (or (when (instance? Column e)
                               (let [^Column c e
                                     t (or (some-> (.getTable c) .getName unquote-ident
                                                   str/lower-case)
                                           inner-from-name)
                                     n (str/lower-case (unquote-ident (.getColumnName c)))]
                                 (when t (get-in schema [(keyword t n) :db/valueType]))))
                             :db.type/string)))
            vtypes (if values?
                     (mapv (fn [i]
                             (or (some #(expression-vtype (nth % i nil) schema)
                                       value-rows)
                                 :db.type/string))
                           (range (count cols)))
                     (mapv vtype-of items))
            sub-name (str "__lsub__" (or talias "anon"))
            ;; NO row marker. Every other virtual table declares one so
            ;; `count(*)` and `SELECT *` have something to enumerate, but
            ;; those all carry DATA. This relation's rows are produced by
            ;; a function binding, so a declared marker only lets the
            ;; alias-anchor pass emit `[?s_eid :__lsub__s/db-row-exists
            ;; true]` — which matches nothing and makes the whole query
            ;; unsatisfiable.
            schema-tx (mapv (fn [c vt]
                              {:db/ident (keyword sub-name c)
                               :db/valueType vt
                               :db/cardinality :db.cardinality/one})
                            cols vtypes)
            spec-db (d/db-with db schema-tx)]
        {:db spec-db :schema (:schema spec-db)
         :name sub-name :alias (or talias sub-name)
         :cols cols
         :vars (mapv (fn [c] (symbol (str "?" sub-name "_" c))) cols)
         :ord-var (symbol (str "?" sub-name "_ord" (swap! var-counter inc)))
         :corr-refs (vec corr-refs)
         :inner-sql (if values? (values-as-select-sql inner) (str inner))
         :n-cols (count cols)}))))

(defn- sequence->virtual-table
  "Materialise `FROM <sequence-name>` into a one-row virtual table.

   PostgreSQL exposes every sequence as a relation with three columns —
   last_value, log_cnt, is_called — and `SELECT * FROM myseq` is the
   classic way to read a sequence's position. We store the last value
   HANDED OUT, so a never-advanced sequence holds `start - increment`;
   PG's equivalent encoding is last_value=start with is_called=false,
   which is what this reconstructs.

   log_cnt is PG's count of WAL-preallocated values — an internal
   recovery detail with no analogue here, reported as 0.

   Returns {:db :schema :name :alias} or nil when `tbl` is not a
   sequence, in which case the caller treats it as an ordinary table."
  [^Table tbl db]
  (let [tname (unquote-ident (.getName tbl))
        talias (when-let [a (.getAlias tbl)]
                 (unquote-ident (str/trim (.getName ^Alias a))))
        row (first (d/q '{:find [?v ?s ?i]
                          :in [$ ?n]
                          :where [[?e :__seq__/name ?n]
                                  [?e :__seq__/value ?v]
                                  [?e :__seq__/start ?s]
                                  [?e :__seq__/increment ?i]]}
                        db tname))]
    (when row
      (let [[value start increment] row
            called? (not= value (- start increment))
            ;; The row-marker attr is what anchors entity enumeration for
            ;; a table whose columns are all read through get-else (the
            ;; same idiom the pg_* catalog tables use). Without it the
            ;; projection has nothing to iterate and the scan returns
            ;; zero rows even though the row is there.
            schema-tx [{:db/ident (keyword tname "last_value")
                        :db/valueType :db.type/long
                        :db/cardinality :db.cardinality/one}
                       {:db/ident (keyword tname "log_cnt")
                        :db/valueType :db.type/long
                        :db/cardinality :db.cardinality/one}
                       {:db/ident (keyword tname "is_called")
                        :db/valueType :db.type/boolean
                        :db/cardinality :db.cardinality/one}
                       {:db/ident (pgs/row-marker-attr tname)
                        :db/valueType :db.type/boolean
                        :db/cardinality :db.cardinality/one}]
            spec-db (d/db-with db schema-tx)
            spec-db2 (d/db-with spec-db
                                [{(keyword tname "last_value") (if called? value start)
                                  (keyword tname "log_cnt") 0
                                  (keyword tname "is_called") called?
                                  (pgs/row-marker-attr tname) true}])]
        {:db spec-db2 :schema (:schema spec-db2)
         :name tname :alias (or talias tname)}))))

(defn materialize-derived-select!
  "Given a ParenthesedSelect in FROM/JOIN position, return an enriched db
   that has a virtual table populated with the subquery's results.

   Handles two inner shapes:
     1. `(SELECT … FROM <table-or-derived> …)` — runs the inner select
        against `db` and materializes its rows (existing behaviour).
     2. `(SELECT * FROM table_function(...) [WITH ORDINALITY])` — expands
        the table function directly without running a query.

   Returns {:db spec-db :name sub-name :alias sub-alias :aliases cols}
   or nil if the shape isn't recognised."
  [^ParenthesedSelect ps db schema]
  (let [sub-alias (when-let [a (.getAlias ps)]
                    (unquote-ident (str/trim (.getName ^Alias a))))
        ;; Storage namespace, deliberately NOT the user's alias: a
        ;; derived table aliased to an existing table's name used to be
        ;; materialised into that table's namespace, and the two merged
        ;; instead of the derived table shadowing it — `SELECT x FROM
        ;; (SELECT 1 AS x) AS t` returned the base table's rows too.
        ;; Deterministic (not gensym/nanoTime) because the parse caches
        ;; key on the enriched schema; a fresh name per parse would make
        ;; every derived-table query a cache miss.
        sub-name (str "__sub__" (or sub-alias "anon"))
        alias-cols (when-let [a (.getAlias ps)]
                     (seq (mapv (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                  (unquote-ident (.-name c)))
                                (or (.getAliasColumns ^Alias a) []))))
        with-fn d/db-with
        inner (.getSelect ps)
        inner-ps (when (instance? PlainSelect inner) inner)
        inner-from (when inner-ps (.getFromItem ^PlainSelect inner-ps))]
    (cond
      ;; (SELECT * FROM unnest(ARRAY[...]) [WITH ORDINALITY]) AS t
      (and inner-ps
           (instance? net.sf.jsqlparser.statement.select.TableFunction inner-from))
      (when-let [{:keys [aliases rows vtypes]}
                 (materialize-table-function
                  ^net.sf.jsqlparser.statement.select.TableFunction inner-from)]
        (let [schema-tx (mapv (fn [alias vtype]
                                {:db/ident (keyword sub-name alias)
                                 :db/valueType vtype
                                 :db/cardinality :db.cardinality/one})
                              aliases vtypes)
              spec-db (with-fn db schema-tx)
              data-tx (mapv (fn [row]
                              (into {}
                                    (keep-indexed
                                     (fn [i a]
                                       (let [v (nth row i nil)]
                                         (when (some? v)
                                           [(keyword sub-name a) v])))
                                     aliases)))
                            rows)
              spec-db2 (if (seq data-tx) (with-fn spec-db data-tx) spec-db)]
          {:db      spec-db2
           :schema  (:schema spec-db2)
           :name    sub-name
           :alias   (or sub-alias sub-name)
           :aliases aliases}))

      ;; (SELECT … FROM real-table …) AS t — run the inner select.
      ;; Inner may also be a SetOperationList (UNION/INTERSECT/EXCEPT) —
      ;; handle that by translating each branch, executing, and combining.
      (or inner-ps (instance? net.sf.jsqlparser.statement.select.SetOperationList inner))
      (materialize-set-op! inner sub-name db schema sub-alias)

      ;; FROM (VALUES (...), (...)) AS v(a,b). materialize-set-op! already
      ;; knows how to evaluate and type literal VALUES rows; this shape was
      ;; simply never dispatched to it, so the relation silently had no rows.
      (instance? Values inner)
      (materialize-set-op! inner sub-name db schema sub-alias alias-cols)

      :else nil)))

;; ── Correlated-subquery resolution (shared by exec-select + derived-table
;;    materialisation). A SELECT item that defers a correlated subquery
;;    (Slice A / Layer 1) threads hidden `__corr_N` columns into :find and
;;    omits the subquery's own output position; these helpers run the subquery
;;    per outer row, splice its value at the out-pos, and drop the __corr_
;;    columns. `parse-fn` parses the inner SQL per row (sql/parse-sql at
;;    Execute, *parse-sql* at parse-time materialisation).

(defn correlated-splice
  "Assemble `n-output` columns from `visible` (non-correlation columns in
   order) and `out-pos->val` (subquery output-position → value)."
  [visible out-pos->val n-output]
  (loop [p 0, v (seq visible), out []]
    (if (= p n-output)
      out
      (if (contains? out-pos->val p)
        (recur (inc p) v (conj out (get out-pos->val p)))
        (recur (inc p) (next v) (conj out (first v)))))))

(defn- eval-corr-scalar
  "Evaluate one SQL fragment for a deferred correlated item -- see
   `expr/eval-correlated-scalar`, which this and the WHERE-position
   binding both call. Kept as a local name so the CASE-branch evaluator
   below reads the same as it did.

   It returned nil on error where the shared one returns the NULL
   sentinel; the difference is invisible here, because every caller
   treats nil as SQL NULL, and the shared one has to be strict about it
   (a Datalog binding that yields nil FILTERS THE ROW)."
  [parse-fn sql subquery? inner-schema query-db]
  ;; The row bindings are an OUTER level: visible to an unqualified
  ;; reference only when the fragment's own FROM has no such column. That
  ;; covers both the no-FROM WHEN/THEN fragments and a full subquery whose
  ;; inner columns must shadow same-named outer ones -- see
  ;; params/*outer-scope-aliases*.
  (binding [params/*outer-scope-aliases* (set (keys params/*from-bindings*))]
    (expr/eval-correlated-scalar parse-fn sql subquery? inner-schema query-db)))

(defn- eval-corr-then
  "Evaluate a CASE branch THEN/ELSE spec with *from-bindings* bound."
  [parse-fn then-spec inner-schema query-db]
  (cond
    (nil? then-spec)          :__null__
    (:subquery-sql then-spec) (eval-corr-scalar parse-fn (:subquery-sql then-spec) true inner-schema query-db)
    :else                     (eval-corr-scalar parse-fn (:expr-sql then-spec) false inner-schema query-db)))

(defn run-correlated-spec
  "Value of a deferred correlated SELECT item for one outer row. `fb` is the
   per-row *from-bindings*. :scalar runs the subquery; :case walks branches."
  [parse-fn spec fb inner-schema query-db]
  ;; See the server-side twin: without *lateral-outer-aliases* the inner
  ;; translator turns the correlation predicate into an implicit JOIN.
  (let [aliases (set (keys fb))]
    (binding [params/*from-bindings* fb
              ;; Outer values are row constants, not inner Datalog
              ;; relations. Keep the LATERAL scope for the entire CASE
              ;; because a selected branch may contain its own WITH RECURSIVE
              ;; query. They are an OUTER level, so an inner column shadows
              ;; a same-named outer one, as PostgreSQL requires.
              params/*outer-scope-aliases* aliases
              params/*lateral-outer-aliases* aliases]
      (case (:kind spec)
        :case
        (let [hit (some (fn [{:keys [when-sql then]}]
                          (when (true? (eval-corr-scalar parse-fn when-sql false inner-schema query-db))
                            [(eval-corr-then parse-fn then inner-schema query-db)]))
                        (:branches spec))]
          (if hit (first hit) (eval-corr-then parse-fn (:else spec) inner-schema query-db)))
        (eval-corr-scalar parse-fn (:inner-sql spec) true inner-schema query-db)))))

(defn resolve-correlated-rows
  "Resolve a parsed SELECT's deferred correlated subqueries against raw result
   `rows`: per outer row, run each subquery with the correlation columns bound
   into *from-bindings*, splice the value at its out-pos, and drop the hidden
   __corr_ columns. Returns [resolved-rows resolved-aliases resolved-oids],
   where resolved-oids carries each subquery's declared OID at its spliced
   position (nil for visible columns) so a caller materialising the result can
   type array columns from the subquery's OID rather than the runtime value
   class (e.g. array_agg(atttypid) → oid[] not int8[]). No-op (returns
   [rows find-aliases nil]) when there are no correlated subqueries."
  [parse-fn parsed rows query-db inner-schema]
  (if-let [cs (:correlated-subqueries parsed)]
    (let [{:keys [subqueries corr-col->idx n-output]} cs
          find-aliases  (:find-aliases parsed)
          corr-idx-set  (set (vals corr-col->idx))
          visible-idxs  (vec (remove corr-idx-set (range (count find-aliases))))
          out-pos->subq (into {} (map (juxt :out-pos identity)) subqueries)
          run-1 (fn [subq rv]
                  (let [fb (reduce (fn [m [a c]]
                                     (assoc-in m [a c] (nth rv (get corr-col->idx [a c]) nil)))
                                   {} (:corr-refs subq))]
                    (run-correlated-spec parse-fn subq fb inner-schema query-db)))
          new-rows (mapv (fn [row]
                           (let [rv  (if (sequential? row) (vec row) [row])
                                 vis (mapv #(nth rv % nil) visible-idxs)
                                 sv  (into {} (map (fn [[op sq]] [op (run-1 sq rv)])) out-pos->subq)]
                             (correlated-splice vis sv n-output)))
                         rows)
          vis-aliases (mapv #(nth find-aliases %) visible-idxs)
          new-aliases (correlated-splice vis-aliases
                                         (into {} (map (fn [[op sq]] [op (:alias sq)])) out-pos->subq)
                                         n-output)
          ;; Carry the VISIBLE columns' inferred OIDs (not nil) so a caller
          ;; materialising the result can preserve their read-back OID (e.g.
          ;; {typeinfo}.typtype is char(18); without this it'd be typed text and
          ;; asyncpg's `kind == b'c'` check would fail). Spliced positions get
          ;; the correlated subquery's declared OID.
          item-oids   (:select-item-oids parsed)
          new-oids    (correlated-splice (mapv #(nth item-oids % nil) visible-idxs)
                                         (into {} (map (fn [[op sq]] [op (:oid sq)])) out-pos->subq)
                                         n-output)]
      [new-rows new-aliases new-oids])
    [rows (:find-aliases parsed) nil]))

(defn materialize-set-op!
  "Run a SELECT (PlainSelect, SetOperationList, or VALUES) and persist its rows
   under `target-name/<col>` in a speculative db. Returns the same
   `{:db :schema :name :alias :aliases}` map shape as
   `materialize-derived-select!` so callers can swap them.

   Used by both the derived-table path (FROM (...) AS t) and the CTE
   path (WITH t AS (...)), since SQL set operations over heterogeneous
   tables can't be expressed natively in Datalog — we have to flatten
   them into a single virtual table.

   `target-name` is the NAMESPACE the rows are stored under; `alias` is
   the name the user wrote. They differ for CTEs, where the namespace is
   synthetic so a CTE cannot collide with a real table of the same name
   — see `datahike.pg.sql/cte-namespace`."
  ([inner target-name db schema]
   (materialize-set-op! inner target-name db schema nil nil))
  ([inner target-name db schema alias]
   (materialize-set-op! inner target-name db schema alias nil))
  ([inner target-name db schema alias explicit-aliases]
   (let [with-fn d/db-with
         is-union? (instance? net.sf.jsqlparser.statement.select.SetOperationList inner)
         values? (instance? Values inner)
         branch-parsed
         (cond
           values?
           (let [^Values values inner
                 raw-exprs (.getExpressions values)
                 row-exprs (if (and (seq raw-exprs)
                                    (instance? ParenthesedExpressionList (first raw-exprs)))
                             (mapv #(vec (iterator-seq (.iterator ^ParenthesedExpressionList %)))
                                   raw-exprs)
                             [(vec raw-exprs)])
                 rows (mapv #(mapv eval-values-literal %) row-exprs)
                 width (count (first rows))
                 aliases (if (= width (count explicit-aliases))
                           (vec explicit-aliases)
                           (mapv #(str "column" (inc %)) (range width)))]
             (when (some #{:unhandled} (mapcat identity rows))
               (throw (ex-info "VALUES CTE contains an unsupported expression"
                               {:error :unsupported-feature :sqlstate "0A000"})))
             {:op nil
              :branches [{:literal-rows rows
                          :find-aliases aliases
                          :select-item-oids (mapv (fn [e]
                                                    (try (oid/expr-oid e {:schema schema :db db})
                                                         (catch Throwable _ nil)))
                                                  (first row-exprs))}]})

           is-union?
           (let [^net.sf.jsqlparser.statement.select.SetOperationList sol inner
                 branches (.getSelects sol)
                 ops (.getOperations sol)
                 op-kind (when (seq ops)
                           (let [op (first ops)]
                             (cond
                               (instance? net.sf.jsqlparser.statement.select.UnionOp op)
                               (if (.isAll ^net.sf.jsqlparser.statement.select.UnionOp op)
                                 :union-all :union)
                               (instance? net.sf.jsqlparser.statement.select.IntersectOp op)
                               :intersect
                               (instance? net.sf.jsqlparser.statement.select.ExceptOp op)
                               :except
                               :else :union)))
                 parsed (mapv (fn [s]
                                (when (instance? PlainSelect s)
                                  (translate-select ^PlainSelect s schema db)))
                              branches)]
             {:op op-kind :branches parsed})

           :else
           {:op nil :branches [(translate-select ^PlainSelect inner schema db)]})
         _ (when (and (some? (:op branch-parsed))
                      (not= :union-all (:op branch-parsed))
                      (some #{types/oid-vector}
                            (mapcat :select-item-oids (:branches branch-parsed))))
             (throw (errors/pg-error
                     :feature-not-supported
                     {:message (str "set operations that deduplicate vector "
                                    "values are not supported")})))
         sub-parsed (first (:branches branch-parsed))
        ;; Per-column expected OID from the inner translate-select's
        ;; oid-infer pass. Used as a default when the materialised
        ;; rows are empty or numerically-mixed (samples alone can't
        ;; pick a type then). Aligns the speculative-db's
        ;; :db/valueType with what describeResult will tell clients.
         sub-oids (:select-item-oids sub-parsed)
         q-fn d/q
         run-branch (fn [{:keys [query in-args sql-limit sql-offset hidden-count
                                 project-set project-order-by project-limit project-offset]
                          :as p}]
                      ;; A body that reads a `$n` is run HERE, at parse
                      ;; time, which is before Bind: the placeholders were
                      ;; still sentinels and the relation came out empty,
                      ;; for every binding. Mark the statement so the
                      ;; server re-parses it at Execute, where
                      ;; `params/*bound-params*` supplies the values.
                      (let [_ (when (some params/param-ref? in-args)
                                (params/materialisation-params!))]
                        (if-let [literal-rows (:literal-rows p)]
                          literal-rows
                          (let [q (cond-> query
                                    (:limit p)  (assoc :limit (:limit p))
                                    (:offset p) (assoc :offset (:offset p)))
                           ;; If translate-select materialized derived
                           ;; tables (FROM (…) AS sub) or catalog refs
                           ;; under it, the resulting query references
                           ;; speculative attrs (`:sub/*`) only present
                           ;; in :enriched-db. Run against that, falling
                           ;; back to the outer db when the branch was
                           ;; a plain table reference.
                                exec-db (or (:enriched-db p) db)
                                raw (if (seq in-args)
                                      (apply q-fn q exec-db in-args)
                                      (q-fn q exec-db))
                                _ (when (seq project-order-by)
                                    (throw (errors/pg-error
                                            :feature-not-supported
                                            {:feature "derived SELECT ordered by a set-returning function"})))
                                raw (if (seq project-set)
                                      (apply-project-set raw project-set)
                                      raw)
                                raw (cond->> raw
                                      sql-offset (drop sql-offset)
                                      sql-limit  (take sql-limit)
                                      project-offset (drop project-offset)
                                      project-limit (take project-limit))
                                hc (or hidden-count 0)
                                visible (- (count (:find query)) hc)
                                raw (if (pos? hc)
                                      (mapv #(if (sequential? %) (vec (take visible %)) %) raw)
                                      raw)
                              ;; A derived SELECT has the same logical
                              ;; projection as a top-level SELECT. In
                              ;; particular, expressions over aggregates are
                              ;; represented physically by hidden aggregate
                              ;; slots and reconstructed afterward. Persisting
                              ;; the physical rows leaked `__compound_*` as a
                              ;; virtual column and omitted the declared alias.
                                [rows _]
                                (apply-compound-projections raw (:find-aliases p)
                                                            query in-args
                                                            (:compound-exprs p))]
                            rows))))
         branch-rows (mapv run-branch (:branches branch-parsed))
         ;; Apply the same logical projection to the first branch's aliases.
         ;; Set-operation branches must agree in width; SQL takes the exposed
         ;; column names from this branch.
         sub-aliases (if (seq (:compound-exprs sub-parsed))
                       (second (apply-compound-projections
                                [] (:find-aliases sub-parsed)
                                (:query sub-parsed) (:in-args sub-parsed)
                                (:compound-exprs sub-parsed)))
                       (:find-aliases sub-parsed))
         sub-results (case (:op branch-parsed)
                       :union-all (mapcat identity branch-rows)
                       :union     (distinct (mapcat identity branch-rows))
                       :intersect (let [sets (map set branch-rows)]
                                    (apply clojure.set/intersection sets))
                       :except    (let [[a & bs] branch-rows]
                                    (reduce (fn [acc r] (apply disj acc r))
                                            (set a) bs))
                      ;; nil → not a UNION, single branch
                       (first branch-rows))
        ;; Window functions inside a derived table or CTE. The window pass
        ;; only ever ran at the top level, so `SELECT * FROM (SELECT …,
        ;; row_number() OVER (…) rn FROM t) s` -- the standard way to use a
        ;; window at all -- did not merely lose the window column: the
        ;; hidden `__win_*` helper columns reached the materialiser as real
        ;; attributes and the query failed outright with `column
        ;; "__win_ord_1" of relation "__sub__…" does not exist`.
        ;;
        ;; Before LIMIT, which SQL applies after the window functions.
        ;; (Single-branch only, like the correlated resolution above.)
         win-specs (when (nil? (:op branch-parsed)) (:window-specs sub-parsed))
         win-resolved
         (when (seq win-specs)
           (let [rows (window/execute-window-functions
                       (mapv (fn [r] (if (sequential? r) (vec r) [r])) sub-results)
                       win-specs)
                 aliases (into (vec sub-aliases) (map :alias) win-specs)
                 ;; The window executor APPENDS its values, and reading
                 ;; them back in that order put the window column last:
                 ;; `SELECT * FROM (SELECT a, row_number() OVER (…) rn, b
                 ;; FROM t) x` answered a, b, rn. Each spec knows its
                 ;; target-list position, and the top-level path has
                 ;; always restored them by it; so does this one now.
                 keep-idx (window/window-projection-indices sub-aliases win-specs)]
             [(mapv (fn [r] (mapv #(nth r % nil) keep-idx)) rows)
              (mapv #(nth aliases %) keep-idx)
              (when sub-oids
                (let [padded (into (vec sub-oids) (repeat (count win-specs) nil))]
                  (mapv #(nth padded % nil) keep-idx)))]))
         sub-results (if win-resolved (first win-resolved) sub-results)
         sub-aliases (if win-resolved (second win-resolved) sub-aliases)
         sub-oids    (if win-resolved (nth win-resolved 2) sub-oids)
         sub-results (cond->> sub-results
                       (:sql-offset sub-parsed) (drop (:sql-offset sub-parsed))
                       (:sql-limit sub-parsed)  (take (:sql-limit sub-parsed)))
        ;; Resolve deferred correlated subqueries (Slice A / Layer 1) so a
        ;; derived table whose SELECT contains a correlated CASE subquery
        ;; (asyncpg's {typeinfo} attrtypoids/attrnames) materialises the
        ;; subquery values and drops the hidden __corr_ columns — otherwise
        ;; they'd be persisted as bogus `:<alias>/__corr_N` attrs. No-op when
        ;; the branch has no correlated subqueries. (Single-branch only; the
        ;; UNION branches keep their raw shape — correlated subqueries inside
        ;; a set-op branch are not a shape we materialise.)
         corr-resolved (when (and (nil? (:op branch-parsed))
                                  (:correlated-subqueries sub-parsed))
                         (resolve-correlated-rows params/*parse-sql* sub-parsed
                                                  sub-results db schema))
         sub-results (if corr-resolved (first corr-resolved) sub-results)
         sub-aliases (if corr-resolved (second corr-resolved) sub-aliases)
        ;; Correlated-subquery columns carry their declared OID (e.g. oid[] for
        ;; array_agg(atttypid)); use it to type array columns instead of the
        ;; runtime value class. Visible columns stay nil (value-sampled).
         sub-oids    (if corr-resolved (nth corr-resolved 2) sub-oids)
         duplicate-aliases (->> sub-aliases frequencies
                                (keep (fn [[column n]] (when (> n 1) column)))
                                seq)
         _ (when duplicate-aliases
             ;; Using one Datahike ident for two projected columns used to
             ;; make db-with attempt an incompatible schema update and leak
             ;; its internal error. Preserve neither that leak nor a silently
             ;; collapsed row shape while duplicate derived columns await a
             ;; distinct physical-column representation.
             (throw (errors/pg-error
                     :feature-not-supported
                     {:message "duplicate columns in a derived table are not supported"})))
        ;; Walk every row rather than just the first — UNION across
        ;; tables of different shapes (or first-row-all-NULL cases) can
        ;; otherwise mis-type a column as :string when later rows have
        ;; longs.
        ;;
        ;; Predicate-based: Datahike's :db.type/long requires *exactly*
        ;; Long (the schema spec is `(= (class %) java.lang.Long)`),
        ;; so an Integer-returning aggregate like COUNT — or any Java
        ;; int promoted by JDBC unwrapping — would otherwise fall to
        ;; the :else string branch and reject the row at transact
        ;; time. The data-tx step coerces each value to the inferred
        ;; type's expected class (see col-coerce).
         fits-long? (fn [^Number n]
                      (and (<= Long/MIN_VALUE (.longValue n))
                           (<= (.longValue n) Long/MAX_VALUE)))
         sample-rows (fn [col-idx]
                       (keep (fn [row]
                               (let [vs (if (sequential? row) (vec row) [row])
                                     v  (nth vs col-idx nil)]
                                 (when (and (some? v) (not= :__null__ v)) v)))
                             sub-results))
        ;; PG-style numeric LUB for mixed integer/float/numeric
        ;; samples. Mirrors a small slice of select_common_type_from_oids
        ;; in PG's parser/parse_coerce.c — wider/more-precise wins.
         numeric-lub (fn [vs]
                       (let [has-bigdec? (some #(instance? java.math.BigDecimal %) vs)
                             has-float?  (some #(or (instance? Double %)
                                                    (instance? Float %)) vs)
                             has-bigint? (some #(and (instance? java.math.BigInteger %)
                                                     (not (fits-long? %)))
                                               vs)]
                         (cond
                           has-bigdec? :db.type/bigdec
                           has-bigint? :db.type/bigdec  ; promote to numeric to keep precision
                           has-float?  :db.type/double
                           :else       :db.type/long)))
        ;; PG-style type categorisation per `select_common_type_from_oids`
        ;; (src/backend/parser/parse_coerce.c). Mixed values within a
        ;; category promote per category rules; cross-category falls to
        ;; :db.type/string with a warning (PG would error 42804 — we
        ;; soft-fail for compatibility with the existing EAV-as-NULL
        ;; design where ad-hoc UNIONs across types are tolerated).
         value-category (fn [v]
                          (cond
                            (boolean? v)                              :boolean
                            (instance? java.util.Date v)              :datetime
                            (instance? java.util.UUID v)              :uuid
                            (pg-vector/vector-value? v)                :vector
                            (or (number? v) (types/numeric-special? v)) :numeric
                            (or (string? v) (keyword? v) (symbol? v)) :string
                            :else                                     :unknown))
         col-vtype (fn [col-idx]
                     (let [samples (sample-rows col-idx)
                          ;; OID-hint default — used when samples are
                          ;; empty, or to disambiguate between equally
                          ;; plausible types (a single Long sample for
                          ;; a column declared NUMERIC by oid-infer
                          ;; should pick :db.type/bigdec, not :long).
                           hint-vtype (some-> (nth sub-oids col-idx nil)
                                              types/dh-type-for-oid)]
                       (cond
                        ;; No samples — trust the OID hint, else string.
                         (empty? samples)
                         (or hint-vtype :db.type/string)

                         :else
                         (let [cats (into #{} (map value-category) samples)]
                           (cond
                            ;; Single-category — straightforward mapping.
                             (= cats #{:boolean})  :db.type/boolean
                             (= cats #{:datetime}) :db.type/instant
                             (= cats #{:uuid})     :db.type/uuid
                             (= cats #{:vector})   :db.type/float-array
                             (= cats #{:string})   :db.type/string

                             (= cats #{:numeric})
                             (let [lub (numeric-lub samples)]
                               (cond
                                 (and (= lub :db.type/long)
                                      (= hint-vtype :db.type/bigdec)) :db.type/bigdec
                                 (and (= lub :db.type/long)
                                      (= hint-vtype :db.type/double)) :db.type/double
                                 :else lub))

                            ;; Cross-category. PG would raise
                            ;; ERRCODE_DATATYPE_MISMATCH (42804). We
                            ;; coerce to :db.type/string and stringify
                            ;; values at the boundary — matches the
                            ;; existing EAV-as-NULL leniency. Exception:
                            ;; if the OID hint is set, trust it (callers
                            ;; that ran oid-infer have a more
                            ;; authoritative answer than sampled rows).
                             :else
                             (or hint-vtype :db.type/string))))))
        ;; Coercion to the runtime class Datahike's schema spec
        ;; demands. Without this, Integer values (e.g. COUNT result)
        ;; pass type inference but are rejected by `db-with` because
        ;; the spec is `(= (class %) java.lang.Long)`.
        ;; Coerce a sample value to the runtime class Datahike's
        ;; schema spec demands for the inferred vtype. Numeric LUB
        ;; can promote samples (e.g. Long → BigDecimal when another
        ;; row's value was BigDecimal); the coercer makes that
        ;; promotion concrete at the data-tx step.
         col-coerce (fn [vtype]
                      (case vtype
                        :db.type/long    (fn [v]
                                           (cond
                                             (instance? Long v) v
                                             (instance? java.math.BigInteger v) (.longValueExact ^java.math.BigInteger v)
                                             :else (long v)))
                        :db.type/double  (fn [v] (if (instance? Double v) v (double v)))
                        ;; VALUES/UNION common-type resolution can promote
                        ;; unknown text rows to NUMERIC based on a typed first
                        ;; row. Use numeric's typinput rather than BigDecimal's
                        ;; constructor so NaN/+/-Infinity follow the same
                        ;; storage encoding as ordinary table writes.
                        :db.type/bigdec  (fn [v]
                                           (-> (coerce/coerce-numeric v :bigdec)
                                               types/numeric-value->storage))
                        :db.type/string  str
                        identity))
        ;; Always emit a row-existence marker so `t.*` expansion in
        ;; the OUTER select has an entity anchor even when every
        ;; non-marker column is NULL on a given row (e.g. Metabase's
        ;; `NULL as role` projection in build_privilege_map).
         row-marker (pgs/row-marker-attr target-name)
        ;; Per-column array element kw. A column whose samples are PgArrays
        ;; (or whose OID hint is a T[] OID) is materialised the way a real
        ;; array column is: :db.type/string holding canonical PG text
        ;; ("{1,2,3}") + a :pg/array-elem datom so its read-back OID is T[].
        ;; Without this the PgArray was Java-`str`'d to "…PgArray@hash" and
        ;; the column typed as text (asyncpg's introspection then mis-decoded
        ;; attrtypoids/attrnames as a string). Element kw drives both the
        ;; value coercion (to-pg-text) and the array OID.
        ;; Prefer the inner select-item's array OID hint (authoritative — it
        ;; reflects the column's DECLARED element type, e.g. array_agg(atttypid)
        ;; → oid[]) over the value-sampled element type (which can only see the
        ;; runtime class, e.g. Long → int8, losing the oid distinction asyncpg
        ;; relies on). Fall back to the sample when oid-infer can't decide.
         col-array-elem (mapv (fn [i]
                                (or (some-> (nth sub-oids i nil)
                                            types/array-oid->element-oid
                                            types/oid->elem-kw)
                                    (some-> (first (filter pg-arr/array? (sample-rows i))) :elem-type)))
                              (range (count sub-aliases)))
        ;; :pg/type to preserve the read-back OID for types whose datahike
        ;; valueType would otherwise report the wrong OID: arrays ("_T"), and
        ;; the OID-preserving scalars char(18)/oid(26) (dh-type-for-oid → string/
        ;; long → would report text/int8). asyncpg's typeinfo decodes typtype as
        ;; "char" → bytes b'c'; if we send it as text it sees the str 'c' and
        ;; `kind == b'c'` fails, so it never builds the composite codec.
        ;; Per-column record sample. A composite value -- `ROW(a,b)`, a
        ;; whole-row reference, `_pg_expandarray`'s (x,n) -- is
        ;; materialised the way an array column is: canonical PG text
        ;; plus the layout the text cannot carry (field names and OIDs),
        ;; so it RENDERS right and a field can still be read out of it.
        ;; Without this the PgRecord was Java-`str`'d to
        ;; "…PgRecord@hash" -- the outer half of pgjdbc's primary-key
        ;; query, `(result.keys).x`, reads a record out of a derived
        ;; table.
         col-record (mapv (fn [i] (first (filter pg-rec/record? (sample-rows i))))
                          (range (count sub-aliases)))
         col-pg-type (mapv (fn [i]
                             (cond
                               (nth col-array-elem i) (str "_" (name (nth col-array-elem i)))
                               (nth col-record i)     "record"
                               :else (get types/oid-preserving-pg-name (nth sub-oids i nil))))
                           (range (count sub-aliases)))
        ;; Per-column inferred type + coercion fn, computed once.
         col-types (mapv (fn [i] (if (or (nth col-array-elem i) (nth col-record i))
                                   :db.type/string
                                   (col-vtype i)))
                         (range (count sub-aliases)))
         col-coercions (mapv (fn [i]
                               (cond
                                 (nth col-array-elem i)
                                 (fn [v] (cond
                                           (pg-arr/array? v) (pg-arr/to-pg-text v)
                                           (string? v)       v
                                           :else             (str v)))

                                 (nth col-record i)
                                 (fn [v] (cond
                                           (pg-rec/record? v) (pg-rec/to-pg-text v)
                                           (string? v)        v
                                           :else              (str v)))

                                 :else (col-coerce (nth col-types i))))
                             (range (count sub-aliases)))
         schema-tx (conj
                    (vec (for [[i a] (map-indexed vector sub-aliases)]
                           (cond-> {:db/ident (keyword target-name a)
                                    :db/valueType (nth col-types i)
                                    :db/cardinality :db.cardinality/one}
                            ;; :pg/type drives oid-infer's read-back OID (array
                            ;; "_T" or OID-preserving scalar char/oid).
                             (nth col-pg-type i)   (assoc :pg/type (nth col-pg-type i))
                            ;; :pg/array-elem drives canonical-text array decode.
                             (nth col-array-elem i) (assoc :pg/array-elem (nth col-array-elem i))
                            ;; :pg/record-fields carries what record text
                            ;; drops: `name:oid` per field.
                             (nth col-record i)
                             (assoc :pg/record-fields
                                    (pg-rec/layout->text (pg-rec/layout (nth col-record i)))))))
                    {:db/ident       row-marker
                     :db/valueType   :db.type/boolean
                     :db/cardinality :db.cardinality/one})
         spec-db (with-fn db schema-tx)
         data-tx (vec (for [row sub-results]
                        (let [vals (if (sequential? row) (vec row) [row])
                              cols (into {} (keep-indexed
                                             (fn [i a]
                                               (let [v (nth vals i nil)]
                                                 (when (and (some? v) (not= :__null__ v))
                                                   [(keyword target-name a)
                                                    ((nth col-coercions i) v)])))
                                             sub-aliases))]
                         ;; Always include the row marker so the
                         ;; entity exists even if every projected
                         ;; column was NULL.
                          (assoc cols row-marker true))))
         spec-db2 (if (seq data-tx) (with-fn spec-db data-tx) spec-db)]
     {:db      spec-db2
      ;; `:pg/record-fields` is transacted onto the attribute entity, and
      ;; `(:schema db)` does not surface custom attrs (the same blind spot
      ;; `:pg/type` and `:pg/not-null` go through `schema-hints` for). The
      ;; translator reads the layout straight off this map to rebuild a
      ;; record out of its text, so fold it in here rather than making
      ;; every reader run a Datalog query for it.
      :schema  (reduce (fn [sch i]
                         (if-let [r (nth col-record i)]
                           (assoc-in sch [(keyword target-name (nth sub-aliases i))
                                          :pg/record-fields]
                                     (pg-rec/layout->text (pg-rec/layout r)))
                           sch))
                       (:schema spec-db2)
                       (range (count sub-aliases)))
      :name    target-name
      :alias   (or alias target-name)
      :aliases sub-aliases})))

(defn- materialize-parenthesed-values!
  "Materialize JSqlParser's FROM (VALUES ...) AS v(a,b) shape.

   This parses as a ParenthesedFromItem whose alias lives on the wrapper
   and whose child is a bare Values node."
  [^ParenthesedFromItem pfi db schema]
  (let [inner (.getFromItem pfi)]
    (when (instance? Values inner)
      (let [alias-obj (.getAlias pfi)
            alias (when alias-obj (unquote-ident (.getName ^Alias alias-obj)))
            aliases (when alias-obj
                      (seq (mapv (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                   (unquote-ident (.-name c)))
                                 (or (.getAliasColumns ^Alias alias-obj) []))))
            target-name (str "__values__" (or alias "anon"))]
        (materialize-set-op! inner target-name db schema alias aliases)))))

(defn- unwrap-derived-parentheses
  "Normalize redundant FROM parentheses around a derived SELECT.

   JSqlParser represents `FROM ((SELECT ...)) alias` as a
   ParenthesedFromItem around a ParenthesedSelect, while the ordinary derived
   path consumes ParenthesedSelect directly. Parenthesized join groups are
   intentionally left alone; only wrappers with no internal joins and an
   eventual derived SELECT are transparent. The outer alias belongs to the
   resulting derived relation and is transferred to it."
  [item]
  (let [original item]
    (loop [current item outer-alias nil]
      (if (and (instance? ParenthesedFromItem current)
               (empty? (.getJoins ^ParenthesedFromItem current)))
        (let [^ParenthesedFromItem pfi current
              alias (or outer-alias (.getAlias pfi))]
          (recur (.getFromItem pfi) alias))
        (if (instance? ParenthesedSelect current)
          (do (when outer-alias
                (.setAlias ^ParenthesedSelect current outer-alias))
              current)
          original)))))

(def ^:dynamic *anonymous-derived-counter*
  "Query-tree-local counter used to give unaliased derived relations distinct,
   deterministic storage namespaces. Nested translate-select calls inherit the
   same atom so an inner anonymous relation cannot collide with one owned by an
   outer SELECT."
  nil)

(def ^:dynamic *cte-namespaces*
  "`{cte-name -> synthetic-namespace}` for the WITH items in scope.

   A CTE is materialised into a speculative db as ordinary attributes,
   and the namespace used to be the CTE's own name — so a CTE whose name
   matched a real table wrote into that table's namespace. The two then
   MERGED rather than the CTE shadowing the table: scans saw the union of
   both relations' rows, `SELECT *` listed the union of their columns,
   and a CTE row whose primary key matched a base row UPSERTED onto it.
   `WITH t AS (...) DELETE FROM t WHERE id IN (SELECT id FROM t)` deleted
   the real table's rows.

   Giving each CTE a synthetic namespace and keeping its user-visible
   name as an ALIAS routes the whole thing through the same path as
   `FROM emp e`, which already resolves alias-to-relation correctly. It
   also leaves the base table's namespace untouched, so PostgreSQL's
   escape hatch — a schema-qualified `public.t` still reaching the real
   table — keeps working.

   Bound by parse-sql around translation, and re-bound by the
   execute-time UPDATE/DELETE re-translation, which resolves the WHERE
   clause afresh and would otherwise lose the mapping."
  {})

;; The translator binds ctx/*relation-namespaces* from this so that
;; extract-table-info — the one place every FROM item passes through —
;; performs the redirect.

(def ^:dynamic *cte-relations*
  "Lowercased names of CTEs (WITH items) in scope for the statement being
   translated. Bound by parse-sql* so the undefined-table 42P01 check
   exempts CTE references that aren't materialised into the schema — most
   notably data-modifying CTE bodies (`WITH x AS (INSERT … RETURNING …)`),
   which are skipped during WITH-fold."
  #{})

(defn- relation-known?
  "True when `tname` names something a query can scan: a user table / CTE
   / derived table whose columns live in `schema` (any attribute in that
   namespace — every pgwire table carries at least its row-marker), a CTE
   in scope (`*cte-relations*`), or a catalog relation already materialised
   into that schema. Used to raise a clean 42P01 for a genuinely-absent relation
   instead of the cryptic 'Query for unknown vars' failure (SELECT *) or a
   silently-empty result (SELECT col). Column-level EAV permissiveness is
   intentionally NOT touched — an existing table's missing column still
   reads as NULL."
  [schema tname]
  (or (nil? tname)
      (let [t (str/lower-case tname)]
        (or (contains? *cte-relations* t)
            ;; Case-insensitive: the reference has been folded, but a
            ;; database created before folding — or a Datalog-native one —
            ;; stores `:MixedCase/...`. This also fixes a latent
            ;; inconsistency: `t` was lowercased above and then compared
            ;; against the RAW `tname`.
            (some (fn [[k _]] (and (keyword? k)
                                   (= (str/lower-case (namespace k)) t)))
                  schema)))))

(defn- stored-relation-known?
  "True when tname has a physical schema namespace.

   SELECT sources may resolve a materialised CTE through *cte-relations*, but
   INSERT/UPDATE/DELETE targets may not: PostgreSQL CTEs are read-only names,
   and a same-named stored table remains the DML target when one exists."
  [schema tname]
  (let [t (some-> tname str/lower-case)]
    (boolean
     (and t
          (some (fn [[k _]]
                  (and (keyword? k)
                       (= (str/lower-case (namespace k)) t)))
                schema)))))

(defn correlated-subquery-refs
  "Given a scalar-subquery `inner` (a JSqlParser Select) and the set of
   `outer-aliases` (lowercased outer FROM aliases/table names), return the
   set of [outer-alias col] correlation references the inner makes, or nil
   when uncorrelated. Delegating to the scope-aware AST walker is essential:
   an inner `FROM other AS t` shadows an outer `t` and must never be replaced
   by the outer row binding."
  ([inner outer-aliases] (expr/correlated-subquery-refs inner outer-aliases))
  ([inner outer-aliases outer-level]
   (expr/correlated-subquery-refs inner outer-aliases outer-level)))

(defn unwrap-parens
  "Peel redundant Parenthesis / single-element ParenthesedExpressionList
   wrappers so `(CASE … END)` and `((expr))` reach their inner node. A
   ParenthesedSelect (a scalar subquery) is NOT unwrapped — it's a leaf here."
  [^net.sf.jsqlparser.expression.Expression e]
  (cond
    (instance? net.sf.jsqlparser.expression.Parenthesis e)
    (recur (.getExpression ^net.sf.jsqlparser.expression.Parenthesis e))
    (and (instance? net.sf.jsqlparser.expression.operators.relational.ParenthesedExpressionList e)
         (= 1 (.size ^net.sf.jsqlparser.expression.operators.relational.ParenthesedExpressionList e)))
    (recur (.get ^net.sf.jsqlparser.expression.operators.relational.ParenthesedExpressionList e 0))
    :else e))

(defn- subquery-expr?
  "True if a JSqlParser expression is a scalar subquery node."
  [e]
  (or (instance? ParenthesedSelect e) (instance? PlainSelect e)))

(defn- subquery-inner
  "The PlainSelect/SetOp inside a (Parenthesed)Select expression."
  [e]
  (if (instance? ParenthesedSelect e) (.getSelect ^ParenthesedSelect e) e))

(defn correlated-select-item-spec
  "Detect a correlated scalar subquery in a SELECT-list item expression `e`,
   returning a deferral spec (without :out-pos/:alias/:oid, which the caller
   adds) or nil. Two shapes:

   - `:scalar` — the item IS a scalar subquery `(SELECT … <outer ref> …)`.
   - `:case`   — the item is a CASE whose THEN/ELSE contains a correlated
     subquery (asyncpg's `CASE WHEN typtype='c' THEN (SELECT array_agg(…)
     WHERE c.reltype = t.oid) END`). The single-rule CASE compiler would
     pre-evaluate the subquery once at parse (→ NULL); instead we defer and
     let exec-select evaluate the whole CASE per outer row.

   `:corr-refs` is the set of [outer-alias col] references threaded into
   :find as hidden columns so exec-select can bind *from-bindings* per row."
  [^net.sf.jsqlparser.expression.Expression e0 outer-aliases single-outer-alias]
  (let [e (unwrap-parens e0)]
    (cond
      (subquery-expr? e)
      (let [inner (subquery-inner e)]
        (when (instance? PlainSelect inner)
          (when-let [refs (correlated-subquery-refs inner outer-aliases)]
            {:kind :scalar :inner-sql (str inner) :corr-refs (vec refs)})))

      (instance? net.sf.jsqlparser.expression.CaseExpression e)
      (let [ce ^net.sf.jsqlparser.expression.CaseExpression e
            when-clauses (.getWhenClauses ce)
            else-expr (.getElseExpression ce)
            then->spec (fn [^net.sf.jsqlparser.expression.Expression t]
                         (when t
                           (if (subquery-expr? t)
                             {:subquery-sql (str (subquery-inner t))}
                             {:expr-sql (str t)})))
          ;; THEN/ELSE branches that are subqueries
            subqs (concat
                   (keep (fn [^net.sf.jsqlparser.expression.WhenClause wc]
                           (let [t (.getThenExpression wc)] (when (subquery-expr? t) t)))
                         when-clauses)
                   (when (subquery-expr? else-expr) [else-expr]))
          ;; Only defer when some THEN/ELSE subquery is itself correlated; an
          ;; uncorrelated subquery (or a plain CASE) needs no per-row eval.
            correlated? (some (fn [s]
                                (let [inner (subquery-inner s)]
                                  (and (instance? PlainSelect inner)
                                       (seq (correlated-subquery-refs inner outer-aliases)))))
                              subqs)
            qualified-refs (or (correlated-subquery-refs ce outer-aliases) #{})
            ;; params/ast-columns treats SELECT nodes as scope boundaries, so
            ;; these are only the CASE's own WHEN/plain-branch columns. When
            ;; the outer SELECT has exactly one source, bare CASE columns
            ;; belong to that row and must ride as hidden correlation inputs
            ;; too. Multi-source ownership needs schema-aware resolution and
            ;; is deliberately not guessed here.
            unqualified-cols (when single-outer-alias
                               (into #{}
                                     (keep (fn [^Column col]
                                             (when (str/blank?
                                                    (some-> col .getTable .getName))
                                               (unquote-ident (.getColumnName col)))))
                                     (params/ast-columns ce)))
            unqualified-refs (when single-outer-alias
                               (set (map #(vector single-outer-alias %)
                                         unqualified-cols)))
            corr-refs (into qualified-refs (or unqualified-refs #{}))]
        (when correlated?
          {:kind :case
           :branches (mapv (fn [^net.sf.jsqlparser.expression.WhenClause wc]
                             {:when-sql (str (.getWhenExpression wc))
                              :then (then->spec (.getThenExpression wc))})
                           when-clauses)
           :else (then->spec else-expr)
         ;; All outer refs across the whole CASE (WHEN conditions + subqueries).
           :corr-refs (vec corr-refs)}))

      :else nil)))

(defn- correlated-item-oid
  "Best-effort result OID for a deferred correlated item, for the extended-
   protocol RowDescription. Uses the inner subquery's first-projection OID
   (count→int8, array_agg→array, …) via the full parse-sql so catalog
   columns type correctly. nil → caller defaults to text."
  [spec schema db parse-fn]
  (try
    (when parse-fn
      (let [sql (case (:kind spec)
                  :scalar (:inner-sql spec)
                  :case   (some #(get-in % [:then :subquery-sql]) (:branches spec)))
            fb (reduce (fn [m [alias col]]
                         (assoc-in m [alias col] :__null__))
                       {} (:corr-refs spec))
            aliases (set (keys fb))]
        ;; OID analysis runs before an outer row exists, but nested WITH
        ;; queries must still resolve their correlated relation. SQL-NULL
        ;; placeholders are sufficient to determine the scalar output type.
        (when sql
          (binding [params/*from-bindings* fb
                    params/*outer-scope-aliases* aliases
                    params/*lateral-outer-aliases* aliases]
            (first (:select-item-oids (parse-fn sql schema db)))))))
    (catch Throwable _ nil)))

(def ^:private two-arg-aggs
  "Aggregates whose implementation takes [v1 v2] pairs."
  #{"string_agg" "corr" "jsonb_object_agg" "json_object_agg"})

(def ^:private null-preserving-aggs
  "Aggregates that KEEP a NULL input value, so an excluded row has to be
   marked as something other than NULL -- see `fns/filtered-out`."
  #{"array_agg" "jsonb_agg" "json_agg"})

(def ^:private throwing-projection-ops
  "Runtime arithmetic functions that can raise for some row values. A
   Datalog planner may reorder function clauses ahead of SQL WHERE
   predicates, so SELECT projections containing these are deferred until
   after filtering when the query shape permits it."
  '#{datahike.pg.sql/sql-div
     datahike.pg.sql/sql-int-div
     datahike.pg.sql/sql-f4div
     datahike.pg.sql/sql-mod
     datahike.pg.sql/sql-money-div
     datahike.pg.sql/sql-money-div-money})

(defn- deferred-projection-op? [x]
  (and (symbol? x)
       (or (contains? throwing-projection-ops x)
           (str/starts-with? (name x) "?nextval-marker-"))))

(def ^:dynamic *unmatched-rows-only*
  "True while translating the SWAPPED half of a FULL JOIN, which
   contributes exactly the rows the other half cannot have: the ones
   with no match. The outer-join lowering then emits its unmatched
   branch alone, instead of an or-join whose matched branch repeats
   every row the first half already answered.

   A FULL JOIN used to be two LEFT JOINs whose results were combined by
   removing, from the second, every row that appeared in the first --
   which loses a right-only row that happens to equal a left row's
   projection, and loses duplicates outright."
  false)

(defn- clause-bound-vars
  "The logic variables a clause BINDS, as opposed to reads: a data
   pattern binds its entity and value, a function clause binds its
   output. `[(?coalesce-fn2 ?y_y2) ?v1]` binds `?v1` and only reads
   `?y_y2` -- a distinction an outer join depends on, since a variable
   merely read by a projection is not a variable the join can key on."
  [clause]
  (cond
    (and (vector? clause) (= 3 (count clause)) (keyword? (second clause)))
    (into #{} (filter symbol?) [(first clause) (nth clause 2)])

    (and (vector? clause) (= 2 (count clause)) (seq? (first clause)))
    (if (symbol? (second clause)) #{(second clause)} #{})

    :else #{}))

(defn- throwing-projection?
  [form clauses]
  (boolean
   (some deferred-projection-op?
         (tree-seq coll? seq (cons form (map first clauses))))))

(defn- inline-projection-bindings
  "Inline THROWING SSA-style function bindings emitted for one SELECT item.

   Safe storage lookup/decoder bindings remain in Datalog. A throwing
   nested binding and every binding that depends on it move into the
   returned post-filter form. Returns [query-clauses form]."
  [form clauses]
  (let [binding? #(and (vector? %) (= 2 (count %))
                       (seq? (first %)) (symbol? (second %)))
        deferred (volatile!
                  (into #{}
                        (keep (fn [clause]
                                (when (and (binding? clause)
                                           (some deferred-projection-op?
                                                 (tree-seq coll? seq
                                                           (first clause))))
                                  (second clause))))
                        clauses))
        above? (fn [clause]
                 (boolean
                  (some @deferred
                        (filter symbol?
                                (tree-seq coll? seq (first clause))))))
        keep-clauses
        (vec (remove (fn [clause]
                       (when (and (binding? clause)
                                  (or (contains? @deferred (second clause))
                                      (above? clause)))
                         (vswap! deferred conj (second clause))
                         true))
                     clauses))
        by-out (into {} (keep (fn [clause]
                                (when (and (binding? clause)
                                           (contains? @deferred (second clause)))
                                  [(second clause) (first clause)])))
                     clauses)
        inline (fn inline [x]
                 (cond
                   (and (symbol? x) (contains? by-out x))
                   (inline (get by-out x))

                   (seq? x) (apply list (map inline x))
                   :else x))]
    [keep-clauses (inline form)]))

(defn- inline-all-projection-bindings
  "Inline every scalar binding emitted for one INSERT SELECT item.

   Data-pattern clauses remain in the source query to fetch leaf values. The
   scalar expression itself is carried to candidate preparation so target-list
   items run left-to-right for one row before the next row is requested."
  [form clauses]
  (let [binding? #(and (vector? %) (= 2 (count %))
                       (seq? (first %)) (symbol? (second %))
                       ;; get-else is the nullable column fetch itself, not a
                       ;; SELECT-list computation. It must stay in Datalog so
                       ;; the deferred form has a concrete leaf value.
                       (not= 'get-else (ffirst %)))
        by-out (into {} (keep (fn [clause]
                                (when (binding? clause)
                                  [(second clause) (first clause)])))
                     clauses)
        inline (fn inline [x]
                 (cond
                   (and (symbol? x) (contains? by-out x))
                   (inline (get by-out x))

                   (seq? x) (apply list (map inline x))
                   :else x))]
    [(vec (remove binding? clauses)) (inline form)]))

(defn compound-projection-indices
  "Indices that turn the physical compound-projection shape into its SQL
   SELECT-list shape. Hidden aggregate inputs are removed and deferred
   expressions are restored to the positions recorded while lowering."
  [aliases compound-exprs]
  (let [visible-indices (into []
                              (keep-indexed
                               (fn [i a]
                                 (when-not (and (string? a)
                                                (.startsWith ^String a "__compound_"))
                                   i)))
                              aliases)
        positions (mapv :out-pos compound-exprs)
        n-visible (count visible-indices)
        reorder? (and (every? some? positions)
                      (= (count positions) (count (distinct positions)))
                      (every? #(< -1 % n-visible) positions))]
    (if reorder?
      (let [base-count (- n-visible (count compound-exprs))
            compound-at (into {}
                              (map-indexed (fn [i pos] [pos (+ base-count i)]))
                              positions)
            visible-order
            (first
             (reduce (fn [[order next-base] pos]
                       (if-let [compound-idx (get compound-at pos)]
                         [(conj order compound-idx) next-base]
                         [(conj order next-base) (inc next-base)]))
                     [[] 0]
                     (range n-visible)))]
        (mapv #(nth visible-indices %) visible-order))
      visible-indices)))

(defn apply-compound-projections
  "Evaluate deferred SELECT projection forms and remove their hidden inputs.

   Shared by the normal SELECT executor and INSERT ... SELECT, which must
   consume the same visible row shape. Returns [rows aliases]."
  [results aliases query in-args compound-exprs]
  (if (seq compound-exprs)
    (let [row-bindings
          (fn [row]
            (let [rv (if (sequential? row) (vec row) [row])]
              (into (into {} (keep-indexed (fn [i e]
                                             (when (symbol? e)
                                               [e (nth rv i nil)])))
                          (:find query))
                    (zipmap (rest (:in query)) in-args))))
          new-results
          (mapv (fn [row]
                  (let [rv (if (sequential? row) (vec row) [row])
                        binds (row-bindings row)]
                    (reduce (fn [r {:keys [form slots]}]
                              (let [b (reduce (fn [m [sym idx]]
                                                (assoc m sym (nth r idx nil)))
                                              binds slots)
                                    val (expr/interpret-form form b)]
                                (conj r (if (= :__null__ val) nil val))))
                            rv compound-exprs)))
                results)
          new-aliases (into (vec aliases) (map :alias compound-exprs))
          visible-indices (compound-projection-indices new-aliases compound-exprs)]
      [(mapv (fn [row] (mapv #(nth row %) visible-indices)) new-results)
       (mapv #(nth new-aliases %) visible-indices)])
    [results aliases]))

(defn- defer-compound-projections
  "Build inert per-row projection markers and remove their hidden inputs.

   INSERT SELECT uses this sibling of `apply-compound-projections`: candidate
   preparation resolves the markers in SELECT-list order, so a later cast or
   scalar error cannot run before an earlier volatile item on the same row or
   cause any later source row to be evaluated."
  [results aliases query in-args compound-exprs]
  (if (seq compound-exprs)
    (let [row-bindings
          (fn [row]
            (let [rv (if (sequential? row) (vec row) [row])]
              (into (into {} (keep-indexed (fn [i e]
                                             (when (symbol? e)
                                               [e (nth rv i nil)])))
                          (:find query))
                    (zipmap (rest (:in query)) in-args))))
          new-results
          (mapv (fn [row]
                  (let [rv (if (sequential? row) (vec row) [row])
                        binds (row-bindings row)]
                    (reduce (fn [r {:keys [form slots]}]
                              (let [b (reduce (fn [m [sym idx]]
                                                (assoc m sym (nth r idx nil)))
                                              binds slots)]
                                (conj r {:fn :projection
                                         :projection-fn
                                         (fn [] (expr/interpret-form form b))
                                         :args []})))
                            rv compound-exprs)))
                results)
          new-aliases (into (vec aliases) (map :alias compound-exprs))
          visible-indices (compound-projection-indices new-aliases compound-exprs)]
      [(mapv (fn [row] (mapv #(nth row %) visible-indices)) new-results)
       (mapv #(nth new-aliases %) visible-indices)])
    [results aliases]))

(defn- filter-arg-var!
  "Bind a fresh variable to `inner-expr`'s value on the rows where
   `filter-expr` is TRUE, and to `excluded` on every other row -- the NULL
   sentinel for the aggregates that skip nulls, `fns/filtered-out` for the
   ones that preserve them.

   That single shape is all an aggregate FILTER needs: every `filter-*`
   aggregate already skips the sentinel, so a filtered aggregate is just the
   ordinary aggregate over this column -- in a GROUP BY and over a window
   frame alike, with no second code path for either.

   FILTER (WHERE p) admits a row only when p is TRUE. UNKNOWN does not
   qualify, and the `:__null__` sentinel is truthy, so a bare `if` would
   admit the NULL rows.

   For COUNT(*) the value is 1 -- there is no argument. For COUNT(x) it is
   x, so a row that passes the filter with a NULL x is still not counted,
   which is what `count(x)` means; the previous COUNT-FILTER shape emitted
   1 for every passing row and counted those in."
  [ctx filter-expr inner-expr default-table count-star? arg2-expr excluded]
  ;; JSqlParser nodes retain parent links. The generic reflective walker
  ;; therefore reaches the enclosing aggregate when started at its FILTER
  ;; predicate (`n > 10` appeared to contain `sum`). Reparse only the
  ;; predicate text to get a detached tree for this structural check.
  (when (some fns/aggregate-function?
              (params/ast-function-names
               (CCJSqlParserUtil/parseCondExpression (str filter-expr))))
    (throw (errors/pg-error
            :grouping-error
            {:message "aggregate functions are not allowed in FILTER"})))
  (let [cond-form (expr/translate-predicate-expr ctx filter-expr)
        inner-val (cond
                    count-star? 1
                    inner-expr (expr/translate-expr ctx inner-expr)
                    :else (ctx/entity-var! ctx default-table))
        ;; A two-argument aggregate (string_agg's delimiter, corr's second
        ;; series) reaches its implementation as a [v1 v2] PAIR -- the same
        ;; shape the unfiltered path builds. Without it `string_agg(s, ',')
        ;; FILTER (WHERE …)` handed the aggregate a bare value and died on
        ;; "Don't know how to create ISeq from: Keyword".
        arg2-val (when arg2-expr (expr/translate-expr ctx arg2-expr))
        case-var (ctx/fresh-var! ctx)
        cond-vars (vec (ctx/collect-vars cond-form))
        param-vars (vec (distinct (concat cond-vars
                                          (when (symbol? inner-val) [inner-val])
                                          (when (symbol? arg2-val) [arg2-val]))))
        compiled-fn (let [pv param-vars, cf cond-form, iv inner-val
                          i2 arg2-val, pair? (some? arg2-expr), out excluded]
                      (fn [& args]
                        (let [bindings (zipmap pv args)]
                          (if (true? (expr/interpret-form cf bindings))
                            (if pair?
                              [(expr/interpret-form iv bindings)
                               (expr/interpret-form i2 bindings)]
                              (expr/interpret-form iv bindings))
                            ;; An excluded row of a PAIR aggregate has to stay
                            ;; a pair: `filter-string-agg` reads `(first p)` of
                            ;; every element, and a bare marker there died with
                            ;; "Don't know how to create ISeq from: Keyword".
                            (if pair? [out out] out)))))
        fn-param (symbol (str "?filter-fn" (swap! (:var-counter ctx) inc)))]
    (swap! (:in-params ctx) conj fn-param)
    (swap! (:in-args ctx) conj compiled-fn)
    (ctx/add-clause! ctx [(apply list fn-param param-vars) case-var])
    ;; A table-free correlated scalar subquery has one implicit input row.
    ;; Its FILTER predicate can reference an outer binding, but there is no
    ;; local entity var to preserve. Adding one manufactured an unbound
    ;; `?_eid` and made Datahike reject the otherwise valid query.
    (when default-table
      (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table)))
    case-var))

(defn translate-select
  "Translate a SELECT while sharing anonymous relation identities across its
   complete nested query tree."
  [^PlainSelect select schema & [db]]
  (binding [*anonymous-derived-counter*
            (or *anonymous-derived-counter* (atom -1))]
    (translate-select* select schema db)))

(defn translate-select*
  "Translate a PlainSelect into a Datalog query map + metadata.
  Returns {:query map :find-aliases [...] :has-aggregates? bool}"
  [^PlainSelect select schema & [db]]
  (let [_ (validate-srf-row-count! select)
        ;; A parenthesized join group is a relation tree, not a single
        ;; right-hand relation. The current outer-join lowering expects a
        ;; concrete right alias and otherwise constructs a malformed or-join
        ;; rule whose empty variable vector Datahike rejects as XX000.
        _ (doseq [^Join join (.getJoins select)
                  :let [right (.getRightItem join)]
                  :when (and (instance? ParenthesedFromItem right)
                             (seq (.getJoins ^ParenthesedFromItem right)))]
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "parenthesized join groups are not implemented"})))
        name-anonymous-derived
        (fn [item]
          (when (and (instance? ParenthesedSelect item)
                     (nil? (.getAlias ^ParenthesedSelect item)))
            ;; PostgreSQL 17 permits a derived relation without an explicit
            ;; alias. Give each occurrence a stable query-local identity so
            ;; two anonymous subqueries do not both materialize into
            ;; `__sub__anon` and merge their rows/columns.
            (.setAlias ^ParenthesedSelect item
                       (Alias. (str "__anon_" (swap! *anonymous-derived-counter* inc)))))
          item)
        expand-view
        (fn [item]
          (if (and db (instance? Table item))
            (let [^Table table item
                  view-name (unquote-ident (.getName table))
                  [definition columns-str]
                  (first
                   (d/q '{:find [?definition ?columns]
                          :in [$ ?name-attr ?definition-attr ?columns-attr ?view-name]
                          :where [[?e ?name-attr ?view-name]
                                  [?e ?definition-attr ?definition]
                                  [?e ?columns-attr ?columns]]}
                        db :datahike.pg/view-name
                        :datahike.pg/view-definition
                        :datahike.pg/view-columns view-name))]
              (if definition
                (let [columns (try (edn/read-string columns-str)
                                   (catch Exception _ nil))
                      quote-ident (fn [s]
                                    (str "\"" (str/replace (str s) "\"" "\"\"") "\""))
                      projection (if (seq columns)
                                   (str/join
                                    ", "
                                    (map (fn [{:keys [name]}]
                                           (let [q (quote-ident name)]
                                             (str "__view_source." q " AS " q)))
                                         columns))
                                   "*")
                      ^net.sf.jsqlparser.statement.select.Select wrapper
                      (CCJSqlParserUtil/parse
                       (str "SELECT * FROM (SELECT " projection
                            " FROM (" definition ") AS __view_source) AS __view"))
                      ^PlainSelect wrapper-select (.getPlainSelect wrapper)
                      ^ParenthesedSelect derived (.getFromItem wrapper-select)]
                  (.setAlias derived (or (.getAlias table) (Alias. view-name)))
                  derived)
                item))
            item))
        ;; FROM clause — may be a Table, view, or derived table (subquery)
        from-item (-> (.getFromItem select)
                      unwrap-derived-parentheses
                      expand-view
                      name-anonymous-derived)
        ;; `FROM <sequence>` reads the sequence's position — nil for
        ;; every ordinary table, so this only costs a lookup when the
        ;; FROM item is a bare relation name (issue #26).
        seq-vt (when (and db (instance? Table from-item))
                 (sequence->virtual-table ^Table from-item db))
        values-vt (when (and db (instance? ParenthesedFromItem from-item))
                    (materialize-parenthesed-values!
                     ^ParenthesedFromItem from-item db schema))
        ;; A correlated SRF in FROM — `FROM t, LATERAL generate_series(1, t.n)`.
        ;; Detected here so the relation exists for SELECT * / count(*)
        ;; / OID inference; the actual per-outer-row binding is emitted
        ;; after `ctx` exists (search lsrf-spec below).
        lsrf-var-counter (atom 0)
        lsrf-spec (when (and db (instance? net.sf.jsqlparser.statement.select.TableFunction
                                           from-item))
                    (correlated-table-fn->spec
                     ^net.sf.jsqlparser.statement.select.TableFunction from-item
                     db lsrf-var-counter))
        ;; Handle derived tables: FROM (SELECT ...) AS sub, including
        ;; table-function forms like (SELECT * FROM unnest(ARRAY[…])
        ;; WITH ORDINALITY) AS sub.
        [db schema name alias]
        (cond
          values-vt
          [(:db values-vt) (:schema values-vt) (:name values-vt) (:alias values-vt)]

          (and db (instance? ParenthesedSelect from-item))
          (if-let [{sub-db :db sub-schema :schema
                    sub-name :name sub-alias :alias}
                   (materialize-derived-select!
                    ^ParenthesedSelect from-item db schema)]
            [sub-db sub-schema sub-name sub-alias]
            [db schema nil nil])

          ;; Bare set-returning function in FROM: `FROM generate_series(2,4)`,
          ;; `FROM now()`. Materialise the (constant-arg) function into a
          ;; virtual table the rest of the query scans normally. Correlated
          ;; (LATERAL) table functions are future work — see
          ;; doc/design-alignment.md.
          ;; Correlated SRF: relation registered, rows bound per outer row.
          lsrf-spec
          [(:db lsrf-spec) (:schema lsrf-spec) (:name lsrf-spec) (:alias lsrf-spec)]

          (and db (instance? net.sf.jsqlparser.statement.select.TableFunction from-item))
          (if-let [{vdb :db vschema :schema vname :name valias :alias}
                   (when-not (table-function-has-column-reference? from-item)
                     (table-fn->virtual-table
                      ^net.sf.jsqlparser.statement.select.TableFunction from-item db))]
            [vdb vschema vname valias]
            (reject-unmaterialized-table-function! from-item))

          ;; A sequence is a relation in PG: `SELECT * FROM myseq` reads
          ;; its position. Materialise the three-column form so the rest
          ;; of the query scans it like any other table (issue #26).
          seq-vt
          [(:db seq-vt) (:schema seq-vt) (:name seq-vt) (:alias seq-vt)]

          ;; Regular table
          :else
          (let [{tname :name talias :alias} (when (instance? Table from-item)
                                              (ctx/extract-table-info ^Table from-item))
                ;; The reference has been case-folded; storage may not be.
                ;; A database created before folding holds `:MixedCase/*`,
                ;; and a Datalog-native one holds whatever its attributes
                ;; were named. Resolve the folded name back to the stored
                ;; one — identity when they already agree, i.e. the common
                ;; path.
                stored (when tname
                         (let [c (pgs/canonical-table (pgs/ci-index schema) tname)]
                           (if (pgs/ambiguous? c) tname c)))]
            [db schema (or stored tname) (or talias tname)]))
        ;; default-table is the alias key used for entity-var lookup.
        default-table (or alias name)

        ;; A genuinely-absent user relation in FROM raises 42P01 (PG's
        ;; undefined_table) instead of failing later with a cryptic
        ;; "Query for unknown vars" (SELECT *) or returning a silent empty
        ;; result (SELECT col). Catalog tables (pg_*/information_schema),
        ;; CTEs, derived tables and table functions are exempt — see
        ;; relation-known?. Column-level EAV permissiveness is unchanged:
        ;; an *existing* table's unknown column still reads as NULL.
        _ (when (and (instance? Table from-item)
                     (not (relation-known? schema name)))
            (throw (ex-info (str "relation \"" name "\" does not exist")
                            {:error :undefined-table
                             :sqlstate "42P01"
                             :table name})))

        ;; Build table aliases: {alias → real-table-name}
        ;; For self-joins, the alias is the key; for regular usage, table name is the key too.
        table-aliases (cond-> {}
                        (and name alias) (assoc alias name)
                        name             (assoc name name))

        ;; Process JOINs. For each join whose right-item is a
        ;; ParenthesedSelect (derived table / table function like
        ;; `JOIN (SELECT * FROM unnest(…) WITH ORDINALITY) AS t`)
        ;; materialize it into the db and register its alias. The JOIN
        ;; is then handled as an ordinary table join in translate-join.
        joins (.getJoins select)
        ;; Aliases a LATERAL inner can correlate WITH. Only the FROM
        ;; item is in scope here; PostgreSQL also allows correlating to
        ;; an EARLIER join item, which would need this to grow as the
        ;; reduce walks the joins.
        outer-alias-set (into #{} (comp (keep identity) (map str/lower-case))
                              [name alias])
        ;; The same relation as a namespace level, so an unqualified
        ;; name inside a LATERAL can be attributed to it.
        outer-level (delay
                      (when-let [vis (or alias name)]
                        (let [cols (when (and name schema)
                                     (not-empty (into #{} (map :name)
                                                      (pgs/column-info schema name db))))]
                          {:items [{:alias vis :relation name
                                    :columns (or cols #{})
                                    ;; A relation whose columns we cannot
                                    ;; read owns nothing we can claim.
                                    :columns-unknown? (nil? cols)}]})))
        _ (validate-lateral-join-shapes! select)
        ;; Two relations visible under one name is 42712, raised when the
        ;; namespace is built -- before any column lookup, which is why
        ;; PostgreSQL reports the relation and not an ambiguous column
        ;; (checkNameSpaceConflicts, parse_relation.c).
        _ (let [visible (keep (fn [item]
                                (when item
                                  ;; unquote-ident folds an unquoted name and
                                  ;; preserves a quoted one, which is what
                                  ;; makes `t2 AS "T"` a different relation
                                  ;; from `t` -- do not fold again.
                                  (if-let [a (.getAlias ^FromItem item)]
                                    (unquote-ident (.getName ^Alias a))
                                    (when (instance? Table item)
                                      (unquote-ident (.getName ^Table item))))))
                              (cons from-item (map #(.getRightItem ^Join %) (or joins []))))]
            (when-let [dup (first (for [[n c] (frequencies visible) :when (> c 1)] n))]
              (throw (ex-info (str "table name \"" dup "\" specified more than once")
                              {:error :duplicate-alias :sqlstate "42712" :table dup}))))
        ;; The relations this statement's WITH list introduces, including
        ;; a recursive one referring to itself.
        with-names (into #{}
                         (keep (fn [^net.sf.jsqlparser.statement.select.WithItem wi]
                                 (some-> (.getAlias wi) str str/trim
                                         params/unquote-ident str/lower-case)))
                         (or (.getWithItemsList select) []))
        [db schema join-aliases derived-joins lsrf-specs]
        (reduce
         (fn [[db schema aliases derived lsrfs] ^Join j]
           (let [raw-rt (.getRightItem j)
                 rt (-> raw-rt
                        unwrap-derived-parentheses
                        expand-view
                        name-anonymous-derived)
                 _ (when-not (identical? raw-rt rt) (.setRightItem j rt))
                 ;; A joined relation is checked like the FROM item
                 ;; above: absent, it used to surface as "missing
                 ;; FROM-clause entry" for the first column that named
                 ;; it, rather than PostgreSQL's 42P01 for the relation.
                 ;; A name this statement's own WITH list declares counts
                 ;; as known: a recursive CTE's body joins the CTE
                 ;; itself, and its rows exist only while the rule runs.
                 _ (when (instance? Table rt)
                     (let [rn (unquote-ident (.getName ^Table rt))]
                       (when-not (or (relation-known? schema rn)
                                     (contains? with-names (str/lower-case rn))
                                     ;; A catalog relation exists in
                                     ;; PostgreSQL whether or not this
                                     ;; layer materialises rows for it.
                                     (str/starts-with? (str/lower-case rn) "pg_")
                                     (str/starts-with? (str/lower-case rn) "information_schema"))
                         (throw (ex-info (str "relation \"" rn "\" does not exist")
                                         {:error :undefined-table
                                          :sqlstate "42P01"
                                          :table rn})))))]
             (cond
               ;; A correlated SRF is ALWAYS in a join/comma position —
               ;; it has to have an outer row to correlate WITH — so this
               ;; branch, not the FROM-item one, is what actually fires
               ;; for `FROM t, LATERAL generate_series(1, t.n)`.
               (and db (instance? net.sf.jsqlparser.statement.select.TableFunction rt)
                    (correlated-table-fn->spec
                     ^net.sf.jsqlparser.statement.select.TableFunction rt db lsrf-var-counter))
               (let [spec (correlated-table-fn->spec
                           ^net.sf.jsqlparser.statement.select.TableFunction rt
                           db lsrf-var-counter)]
                 [(:db spec) (:schema spec)
                  (assoc aliases (:alias spec) (:name spec))
                  derived
                  (conj lsrfs (assoc spec :join j))])

               ;; An UNCORRELATED set-returning function in join or comma
               ;; position -- `FROM t, generate_series(1, 3) g`. Its rows
               ;; do not depend on the outer row, so it materialises once
               ;; into a virtual table exactly as it does in FROM-item
               ;; position; only the FROM-item position had a branch for
               ;; it, so the join form left the alias unregistered and
               ;; every reference to it raised `column "g" does not
               ;; exist`.
               (and db (instance? net.sf.jsqlparser.statement.select.TableFunction rt)
                    (not (table-function-has-column-reference? rt))
                    (table-fn->virtual-table
                     ^net.sf.jsqlparser.statement.select.TableFunction rt db))
               (let [{vdb :db vschema :schema vname :name valias :alias}
                     (table-fn->virtual-table
                      ^net.sf.jsqlparser.statement.select.TableFunction rt db)]
                 [vdb vschema
                  (cond-> (assoc aliases vname vname)
                    (and valias (not= valias vname)) (assoc valias vname))
                  ;; Its rows live in the speculative db's own entity-id
                  ;; space, so it joins BY VALUE like a derived table.
                  (conj derived {:join j :alias (or valias vname)})
                  lsrfs])

               ;; A TableFunction must be handled by one of the two branches
               ;; above. Falling through used to ignore an unknown function
               ;; in a comma/join position entirely, returning the OUTER rows
               ;; as if the function were not in the query. WITH ORDINALITY
               ;; was worse: its projected columns leaked an unbound ?f_eid
               ;; into the Datalog query. Preserve neither failure mode.
               (instance? net.sf.jsqlparser.statement.select.TableFunction rt)
               (reject-unmaterialized-table-function! rt)

               (instance? Table rt)
               (let [{jn :name ja :alias} (ctx/extract-table-info ^Table rt)]
                 [db schema
                  (cond-> aliases
                    (and jn ja) (assoc ja jn)
                    jn          (assoc jn jn))
                  derived lsrfs])

               ;; LateralSubSelect EXTENDS ParenthesedSelect, so this must
               ;; come first or a LATERAL subquery is mistaken for an
               ;; ordinary derived table and materialised once with the
               ;; outer column unbound.
               (and db (instance? net.sf.jsqlparser.statement.select.LateralSubSelect rt)
                    (lateral-subselect->spec rt db schema outer-alias-set lsrf-var-counter @outer-level))
               (let [spec (lateral-subselect->spec rt db schema outer-alias-set
                                                   lsrf-var-counter @outer-level)]
                 ;; An OUTER lateral has to preserve the outer row with
                 ;; NULLs when the inner is empty, and an empty collection
                 ;; binding DROPS it -- that is the inner-join semantics
                 ;; this relies on. The producer supplies the missing row
                 ;; instead: one tuple of NULLs, which is precisely what
                 ;; LEFT JOIN LATERAL … ON TRUE means. (The or-join
                 ;; construction the other OUTER joins use cannot be
                 ;; applied here -- it reached the fn-binding clause and
                 ;; raised the datalog-internal `Cannot parse rule-vars`.)
                 ;;
                 ;; ON TRUE only. With a real condition, a row the
                 ;; condition rejects still has to survive as NULLs, and a
                 ;; producer that has already emitted its rows cannot
                 ;; distinguish that from a match. Refuse rather than
                 ;; answer wrongly.
                 (let [outer? (boolean (or (.isLeft j) (.isRight j)
                                           (.isFull j) (.isOuter j)))]
                   (when (and outer? (not (trivially-true-on? j)))
                     (throw (errors/pg-error
                             :feature-not-supported
                             {:feature "OUTER JOIN LATERAL (subquery) with a join condition"})))
                   (when (or (.isRight j) (.isFull j))
                     (throw (errors/pg-error
                             :feature-not-supported
                             {:feature "RIGHT/FULL JOIN LATERAL (subquery)"})))
                   [(:db spec) (:schema spec)
                    (assoc aliases (:alias spec) (:name spec))
                    derived
                    (conj lsrfs (cond-> (assoc spec :join j)
                                  outer? (assoc :outer? true)))]))

               (and db (instance? ParenthesedSelect rt))
               (if-let [{spec-db :db spec-schema :schema
                         sub-name :name sub-alias :alias}
                        (materialize-derived-select! ^ParenthesedSelect rt db schema)]
                 [spec-db spec-schema
                  ;; Register the USER'S alias too, not just the storage
                  ;; namespace. Only `sub-name` was registered, so
                  ;; `FROM t JOIN (SELECT …) s ON …` left `s` naming
                  ;; nothing and every reference to it raised
                  ;; "missing FROM-clause entry for table s". The
                  ;; from-item path always registered both, which is why
                  ;; `FROM (SELECT …) s JOIN t` worked and the same
                  ;; subquery on the right did not.
                  (cond-> (assoc aliases sub-name sub-name)
                    (and sub-alias (not= sub-alias sub-name))
                    (assoc sub-alias sub-name))
                  (conj derived {:join j :alias (or sub-alias sub-name)})
                  lsrfs]
                 [db schema aliases derived lsrfs])

               (and db (instance? ParenthesedFromItem rt))
               (if-let [{spec-db :db spec-schema :schema
                         sub-name :name sub-alias :alias}
                        (materialize-parenthesed-values!
                         ^ParenthesedFromItem rt db schema)]
                 [spec-db spec-schema
                  (cond-> (assoc aliases sub-name sub-name)
                    (and sub-alias (not= sub-alias sub-name))
                    (assoc sub-alias sub-name))
                  (conj derived {:join j :alias (or sub-alias sub-name)})
                  lsrfs]
                 [db schema aliases derived lsrfs])

               :else
               [db schema aliases derived lsrfs])))
         [db schema {} [] []]
         joins)
        table-aliases (merge table-aliases join-aliases)

        ;; PostgreSQL permits a relation alias to rename its output columns:
        ;; `FROM v AS v1(x1)`. This matters especially for self-joining a CTE,
        ;; where each occurrence exposes a different name for the same stored
        ;; `:v/x` attribute. Keep the override keyed by the relation ALIAS,
        ;; not its storage namespace, so v1(x1), v2(x2) remain independent.
        table-column-overrides
        (reduce
         (fn [out ^Table table]
           (let [{tname :name talias :alias} (ctx/extract-table-info table)
                 alias-obj (.getAlias table)
                 exposed (when alias-obj
                           (some->> (.getAliasColumns ^Alias alias-obj)
                                    (mapv (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                            (unquote-ident (.-name c))))))
                 alias-key (or talias tname)
                 real-name (get table-aliases alias-key tname)
                 ;; db_id is pg-datahike's synthetic entity projection,
                 ;; not a declared relation column and therefore does not
                 ;; consume a name in PostgreSQL's alias column list.
                 attrs (into [] (comp (remove #(= :db/id (:attr %)))
                                      (map :attr))
                             (pgs/column-info schema real-name db))]
             (if (seq exposed)
               (assoc out alias-key (into {} (map vector exposed attrs)))
               out)))
         {}
         (cond-> (into [] (keep (fn [^Join j]
                                  (let [rt (.getRightItem j)]
                                    (when (instance? Table rt) rt)))
                                (or joins [])))
           (instance? Table from-item) (conj from-item)))

        ;; Aliases of derived tables in JOIN positions. translate-join
        ;; consults this to skip the ref/db_id unification path, which
        ;; assumes the right-side alias names a real entity in the live
        ;; db. Derived rows live in their own entity-id space in the
        ;; speculative db; we have to JOIN them by value, not by
        ;; entity-id unification.
        derived-alias-set (into #{} (map :alias) derived-joins)

        ;; The relations in FROM ORDER, as `SELECT *` must expand them.
        ;; `table-aliases` is a MAP — it has no order and it also holds a
        ;; `{name -> name}` entry per item — so star expansion used only
        ;; `default-table` and silently dropped every joined relation:
        ;; `SELECT * FROM t JOIN c` returned t's columns alone.
        star-relations
        (into (if default-table
                [[default-table (get table-aliases default-table default-table)]]
                [])
              (keep (fn [^Join j]
                      (let [rt (.getRightItem j)]
                        (cond
                          (instance? Table rt)
                          (let [{jn :name ja :alias} (ctx/extract-table-info ^Table rt)]
                            (when jn [(or ja jn) jn]))
                          ;; A derived table or SRF in join position. Match
                          ;; the spec to THIS join: choosing the first
                          ;; derived item silently omitted later relations,
                          ;; and correlated SRFs were not represented in
                          ;; derived-joins at all.
                          :else
                          (when-let [{a :alias n :name}
                                     (some (fn [spec]
                                             (when (identical? j (:join spec)) spec))
                                           (concat derived-joins lsrf-specs))]
                            [a (or (get join-aliases a) n)])))))
              (or joins []))

        ;; Preserve FROM occurrences for unqualified column resolution.
        ;; The alias map necessarily collapses a self-join's two values to
        ;; the same storage namespace; occurrence metadata lets ctx still
        ;; raise PostgreSQL's 42702 for `FROM t x, t y ... b`.
        ;; What each USING / NATURAL join merges: one output column, not
        ;; two. Recorded per right-hand alias so `SELECT *` emits it once
        ;; and an unqualified reference is not ambiguous.
        ;; Walked in FROM order: a join merges against the relations to
        ;; its LEFT only, which is also what decides the owner of each
        ;; merged column.
        merged-joins
        (:merged
         (reduce (fn [{:keys [left] :as acc} ^Join j]
                   (let [rt (.getRightItem j)]
                     (if-not (instance? Table rt)
                       acc
                       (let [{jn :name ja :alias} (ctx/extract-table-info ^Table rt)
                             ralias (or ja jn)
                             cols (join-merged-columns j left jn schema db)]
                         (cond-> (update acc :left assoc ralias jn)
                           (seq cols)
                           (update :merged conj
                                   {:right ralias
                                    :columns (mapv :column cols)
                                    :owners (mapv (juxt :left-alias :column) cols)
                                    :outer? (not= :inner (join-type j))}))))))
                 {:left (cond-> {}
                          default-table (assoc default-table
                                               (get table-aliases default-table default-table)))
                  :merged []}
                 (or joins [])))
        table-aliases (with-meta table-aliases
                        {:relation-aliases (mapv first star-relations)
                         ;; Both sides drop it from their own expansion;
                         ;; it is emitted once, first, from its owner.
                         :merged-columns-by-alias
                         (reduce (fn [m {:keys [right columns owners]}]
                                   (reduce (fn [m [a c]] (update m a (fnil conj #{}) c))
                                           (update m right (fnil into #{}) columns)
                                           owners))
                                 {} merged-joins)
                         :merged-order (vec (mapcat :owners merged-joins))
                         ;; An INNER join's merged column is the left
                         ;; side's: the equality makes the two the same
                         ;; value. An OUTER join's is COALESCE of both,
                         ;; which this does not build, so it stays
                         ;; ambiguous rather than answering a NULL.
                         :merged-columns (into #{}
                                               (comp (remove :outer?) (mapcat :columns))
                                               merged-joins)})

        ;; Create context
        hints (pgs/schema-hints db)
        ctx (ctx/make-ctx schema table-aliases default-table
                          {:db db
                           :parse-sql params/*parse-sql*
                           :hints hints
                           :derived-aliases derived-alias-set
                           :computed-aliases (into #{} (map :alias)
                                                   (cond-> (vec lsrf-specs)
                                                     lsrf-spec (conj lsrf-spec)))
                           :column-overrides table-column-overrides
                           :ref-targets (pgs/validate-ref-targets!
                                         db schema
                                         (pgs/derive-ref-targets schema hints))})

        ;; Correlated SRF: bind its rows PER OUTER ROW.
        ;;
        ;; Pre-seeding `:col->var` is what makes this work without a new
        ;; resolution concept — `ctx/col-var!` returns a cached var
        ;; without emitting the `get-else` attribute lookup it would
        ;; otherwise produce, so every column reference, `SELECT *`
        ;; expansion and ORDER BY resolves to the fn-bound var.
        ;;
        ;; The ordinality var goes into `:with`: Datalog is set-based, so
        ;; without it a function returning [7 7 7] collapses to ONE row.
        _ (doseq [lsrf-spec (cond-> (vec lsrf-specs) lsrf-spec (conj lsrf-spec))]
            (let [{:keys [cols vars ord-var fname params name marker
                          inner-sql corr-refs]} lsrf-spec
                  subquery? (some? inner-sql)
                  rows-fn (if subquery?
                            (lateral-rows-fn inner-sql corr-refs schema db
                                             (:n-cols lsrf-spec))
                            (srf-rows-fn fname))
                  arg-vals (if subquery?
                             ;; The outer columns the inner correlates
                             ;; with, in the order lateral-rows-fn
                             ;; rebuilds *from-bindings* from. Binding
                             ;; them as function ARGUMENTS is what makes
                             ;; the engine run the inner once per outer
                             ;; row.
                             (mapv (fn [[a c]]
                                     ;; Resolve the outer reference the way
                                     ;; any other column reference resolves
                                     ;; -- through the alias map, the
                                     ;; case-folding index, renames and
                                     ;; INHERITS. `(keyword a c)` treated the
                                     ;; ALIAS as a namespace, so `FROM t x,
                                     ;; LATERAL (SELECT x.c)` raised 42P01
                                     ;; for a relation that is in scope.
                                     (let [col (Column. (Table. ^String a) ^String c)]
                                       (ctx/col-var!
                                        ctx (ctx/resolve-column col (:table-aliases ctx)
                                                                (:default-table ctx)
                                                                (:col-overrides ctx)
                                                                (:derived-aliases ctx)
                                                                (:ci-index ctx)))))
                                   corr-refs)
                             (mapv (fn [e]
                                     (let [v (srf-const-eval e)]
                                       (if (not= ::corr v)
                                         v
                                       ;; A correlated argument resolves
                                       ;; to the OUTER column's var, which
                                       ;; is exactly what makes the fn run
                                       ;; once per outer row.
                                         (expr/translate-expr ctx e))))
                                   params))
                  fn-param (symbol (str "?lsrf-fn" (swap! (:var-counter ctx) inc)))
                  ;; [[?c1 ?c2 … ?ord]] — a RELATION binding, so one
                  ;; produced tuple becomes one row.
                  binding-form [(conj (vec vars) ord-var)]]
              (doseq [[c v] (map vector cols vars)]
                (swap! (:col->var ctx) assoc [(:alias lsrf-spec) (keyword name c)] v)
                (swap! (:col->var ctx) assoc (keyword name c) v))
              (swap! (:in-params ctx) conj fn-param)
              (swap! (:in-args ctx) conj
                     (let [outer? (:outer? lsrf-spec)
                           null-row [(vec (repeat (count vars) :__null__))]]
                       (fn [& as]
                         (let [rs (apply rows-fn as)
                               ;; LEFT JOIN LATERAL: no inner row means one
                               ;; row of NULLs, not the outer row's removal.
                               rs (if (and outer? (empty? rs)) null-row rs)]
                           ;; NEVER nil: bind-by-fn drops the outer tuple on
                           ;; nil, which would swallow rows silently.
                           (vec (map-indexed (fn [i r] (conj (vec r) (long (inc i))))
                                             (or rs [])))))))
              (ctx/add-clause! ctx [(apply list fn-param arg-vals) binding-form])
              ;; `:with` preserves bag multiplicity, which is the whole
              ;; point here — but that is exactly what DISTINCT must not
              ;; have. `has-distinct?` works by WITHHOLDING the entity
              ;; var from :with-vars, so adding the ordinality
              ;; unconditionally re-introduced the duplicates DISTINCT
              ;; was asked to remove.
              (when-not (some? (.getDistinct select))
                (swap! (:with-vars ctx) conj ord-var))
              ;; The row marker never gets a datom (there is no data), so
              ;; anything that scans for it must not be emitted.
              (when marker
                (swap! (:col->var ctx) assoc (keyword name (clojure.core/name marker))
                       (first vars)))))

        ;; Process JOIN ON conditions and track join types
        join-infos (when joins
                     (mapv (fn [^Join j]
                             (translate-join ctx j default-table))
                           joins))

        ;; WHERE clause
        ;; Pre-populate entity vars from FROM clause so WHERE (especially
        ;; correlated EXISTS) can reference outer table variables.
        ;; Without this, entity vars are created lazily during SELECT
        ;; item processing, which happens AFTER WHERE.
        _ (ctx/entity-var! ctx default-table)
        _ (doseq [{:keys [alias]} join-infos]
            (when alias (ctx/entity-var! ctx alias)))

        ;; Extract SELECT items NOW (before WHERE) so we can pre-scan
        ;; aggregation aliases — needed for the WHERE-references-an-
        ;; aggregation-alias detector. Real PG raises 42703 for any
        ;; unresolved column in WHERE, but pgwire-datahike treats
        ;; missing attrs as NULL by design (EAV semantics, see
        ;; datahike.test.pg-server-test/test-semantic-errors). The
        ;; targeted exception: when the unresolved column NAME matches
        ;; a SELECT-list aggregation alias, the user almost certainly
        ;; meant HAVING — emit a helpful 42703 with a hint instead of
        ;; silently filtering all rows.
        select-items (.getSelectItems select)
        agg-aliases-warning-set
        (into #{}
              (keep (fn [^SelectItem item]
                      (when-let [a (.getAlias item)]
                        (let [expr (.getExpression item)]
                          (when (or (and (instance? Function expr)
                                         (fns/aggregate-function?
                                          (expr/resolution-name (.getName ^Function expr))))
                                    (instance? net.sf.jsqlparser.expression.AnalyticExpression expr))
                            (str/lower-case
                             (unquote-ident (.getName ^Alias a))))))))
              select-items)
        ctx (assoc ctx :agg-aliases-warning-set agg-aliases-warning-set)

        where-expr (.getWhere select)
        ;; *conjunctive-where* enables the indexable data-pattern fast
        ;; paths (value-bound `[?e :attr v]`, shared-var equi-join
        ;; unification) — sound only for top-level AND-ed conjuncts;
        ;; expr.clj re-binds it false inside OR / NOT branches.
        _ (when where-expr
            (binding [expr/*conjunctive-where* true]
              (let [preds (expr/translate-predicate ctx where-expr)]
                (swap! (:where-clauses ctx) into preds))))

        has-distinct? (some? (.getDistinct select))
        ;; `DISTINCT ON (exprs)` keeps the FIRST row of each group of rows
        ;; sharing those expressions -- it is not plain DISTINCT, which
        ;; dedupes the whole row. Treating it as the latter returned every
        ;; row whose projection happened to differ, silently too many.
        ;; PostgreSQL requires the ON expressions to be the leading ORDER BY
        ;; ones, so "first" is well defined once the sort has run.
        distinct-on-items (some-> (.getDistinct select) .getOnSelectItems)

        ;; GROUP BY
        group-by-element (.getGroupBy select)
        group-by (when group-by-element
                   (seq (.getGroupByExpressions ^GroupByElement group-by-element)))
        ;; PostgreSQL resolves a bare name in GROUP BY as: a local FROM
        ;; column first, then an OUTPUT-COLUMN ALIAS, then an outer
        ;; column (parse_clause.c's findTargetlistEntrySQL92). So
        ;; `SELECT a AS x FROM t GROUP BY x` is legal and groups by the
        ;; aliased expression. Translating such an item as a column
        ;; would report it as undefined.
        select-alias-names (into #{} (keep select-item-alias) (.getSelectItems select))
        ;; The translated grouping keys. Datalog derives grouping from
        ;; the NON-AGGREGATE :find elements, so these have to reach
        ;; :find or the GROUP BY has no effect at all — see
        ;; `group-by-hidden` below, which is where they get added.
        group-by-alias-only?
        (fn [g] (and (instance? Column g)
                     (nil? (.getTable ^Column g))
                     (contains? select-alias-names
                                (unquote-ident (.getColumnName ^Column g)))
                     ;; a real column of the table wins
                     (nil? (get schema
                                (keyword (or default-table "")
                                         (unquote-ident (.getColumnName ^Column g)))))))
        ;; GROUP BY items naming an output-column alias. Their grouping
        ;; key is the select item itself, which is already in :find, so
        ;; they contribute no var — but the 42803 check still has to
        ;; count them as grouped, which it does by resolving the alias
        ;; through find-aliases once the select list is translated.
        group-by-alias-names
        (when (seq group-by)
          (into #{}
                (comp (filter group-by-alias-only?)
                      (map #(unquote-ident (.getColumnName ^Column %))))
                group-by))
        ;; A bare integer constant in GROUP BY is a 1-based ordinal into
        ;; the select list, same rule as ORDER BY. Like an alias, it
        ;; names an item that is already projected, so it contributes no
        ;; new :find element — only a grouping key, resolved once the
        ;; select list is translated.
        _ (when (seq group-by)
            (when (some bare-non-integer-constant? group-by)
              (throw (ex-info "non-integer constant in GROUP BY"
                              {:error :syntax-error
                               :sqlstate "42601"}))))
        group-by-ordinals
        (when (seq group-by)
          (into #{} (comp (filter #(instance? LongValue %))
                          (map #(.getValue ^LongValue %)))
                group-by))
        group-by-vars
        (when (seq group-by)
          (vec
           (keep (fn [g]
                   (when-not (or (group-by-alias-only? g)
                                 (instance? LongValue g))
                     ;; A bare column translates to its logic var, which
                     ;; col-var! caches — so a column that is ALSO
                     ;; projected yields the same symbol here and in
                     ;; :find, and is not double-counted. A compound
                     ;; expression (`GROUP BY sal / 10`) translates to a
                     ;; form, which :find cannot hold, so bind it to a
                     ;; var first.
                     (let [t (expr/translate-expr ctx g)]
                       (cond
                         (symbol? t) t
                         (seq? t)    (ctx/materialize-arg! ctx t)
                         :else       nil))))
                 group-by)))

        ;; HAVING clause
        having-expr (.getHaving select)

        ;; Process select items
        find-elements (atom [])
        find-aliases (atom [])
        has-aggregates? (atom false)
        compound-exprs (atom [])  ;; [{:alias str :op sym :l-idx int :r-idx int}]
        window-specs (atom [])    ;; [{:op kw :partition-by [idx] :order-by [[idx dir]] :frame {...}}]
        project-set-specs (atom [])
        lower-project-srf!
        (fn lower-project-srf! [^Function f alias-str visible?]
          (let [children (atom [])
                arg-vars
                (mapv (fn [arg]
                        (if (target-list-srf? arg)
                          (let [{:keys [out-var] :as child}
                                (lower-project-srf! ^Function arg nil false)]
                            (swap! children conj child)
                            out-var)
                          (let [v (expr/translate-expr ctx arg)]
                            (cond
                              (symbol? v) v
                              (seq? v) (ctx/materialize-arg! ctx v)
                              :else (ctx/materialize-arg!
                                     ctx (list 'identity
                                               (if (nil? v) :__null__ v)))))))
                      (vec (or (.getParameters f) [])))
                level (if (seq @children)
                        (inc (reduce max (map :level @children)))
                        0)
                out-var (ctx/fresh-var! ctx)
                out-pos (when visible? (count @find-elements))
                spec {:function f :arg-vars arg-vars :level level
                      :out-var out-var :out-pos out-pos}]
            (ctx/add-clause! ctx [(list 'identity :__null__) out-var])
            (doseq [child @children]
              (swap! project-set-specs conj child))
            (swap! project-set-specs conj spec)
            (when visible?
              (swap! find-elements conj out-var)
              (swap! find-aliases conj
                     (or alias-str (srf-base-name (.getName f)))))
            spec))

        ;; Lightweight oid-env — built before aggregate dispatch so the
        ;; SUM/AVG branches can pick the numeric-precision runtime
        ;; variant (filter-sum-numeric / filter-avg-numeric) when the
        ;; input column's OID is INT8 / NUMERIC. Mirrors the fuller
        ;; oid-env constructed below for select-item OID inference;
        ;; both pull from the same fields.
        agg-oid-env {:db db :schema schema
                     :table-aliases table-aliases
                     :default-table default-table
                     :scalar-subquery-oid #(expr/scalar-subquery-output-oid ctx %)
                     :hints (pgs/schema-hints db)}
        reject-vector-distinct-aggregate!
        (fn [distinct? inner-expr]
          (when (and distinct? inner-expr
                     (= types/oid-vector
                        (try (oid/expr-oid inner-expr agg-oid-env)
                             (catch Throwable _ nil))))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "DISTINCT aggregates over vector values are not supported"}))))
        reject-ordered-set-boundary!
        (fn [fname agg-sym inner-expr within-group?]
          (when (and within-group?
                     (contains? #{"rank" "dense_rank"
                                  "percent_rank" "cume_dist"}
                                fname))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message (str "hypothetical-set aggregate " fname
                                   " is not supported")})))
          (when (and within-group? (nil? agg-sym))
            (throw (errors/pg-error
                    :undefined-function
                    {:function (str fname "(...) WITHIN GROUP")})))
          (when (and within-group?
                     (contains? #{"percentile_cont" "percentile_disc"} fname)
                     (instance? ArrayConstructor inner-expr))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message (str fname
                                   " with an array of fractions is not supported")}))))
        ;; Per-input-type runtime variant for precision-sensitive
        ;; aggregates. Single source of truth: oid-infer's
        ;; `sql-aggregate->return-oid` says e.g. AVG(int8) → numeric;
        ;; if the inferred result OID is NUMERIC and we have a
        ;; BigDecimal runtime variant, use it. Avoids the previous
        ;; duplication where stmt.clj redundantly enumerated which
        ;; (agg, input-oid) pairs need numeric runtimes.
        pick-precision-variant
        (fn [agg-name input-oid]
          (let [result-oid (oid/resolve-aggregate-result-oid agg-name input-oid)]
            (cond
              (= result-oid types/oid-numeric)
              (case agg-name
                "sum" 'datahike.pg.sql/filter-sum-numeric
                "avg" 'datahike.pg.sql/filter-avg-numeric
                ;; The variance family is NUMERIC over int2/int4/int8/
                ;; numeric, and its numeric runtime is not merely more
                ;; precise -- the float one OVERFLOWED on int8 input.
                ("stddev" "stddev_samp") 'datahike.pg.sql/filter-stddev-samp-numeric
                "stddev_pop"             'datahike.pg.sql/filter-stddev-pop-numeric
                ("variance" "var_samp")  'datahike.pg.sql/filter-variance-samp-numeric
                "var_pop"                'datahike.pg.sql/filter-variance-pop-numeric
                nil)
              ;; sum(float4) accumulates at float4 precision (float4pl),
              ;; so it needs its own runtime too.
              (= result-oid types/oid-float4)
              (case agg-name
                "sum" 'datahike.pg.sql/filter-sum-float4
                nil)
              :else nil)))

        ;; `agg(x) FILTER (WHERE p)` and the bare `agg(x)` that JSqlParser
        ;; also surfaces as an AnalyticExpression. Same two rules as
        ;; emit-agg!: the aggregate is the one `sql-aggregate->datalog`
        ;; names, at the precision `pick-precision-variant` picks.
        emit-analytic-agg!
        (fn [^net.sf.jsqlparser.expression.AnalyticExpression ae alias0]
          (let [fname (expr/resolution-name (.getName ae))
                agg-sym (get fns/sql-aggregate->datalog fname)
                inner-expr (.getExpression ae)
                filter-expr (.getFilterExpression ae)
                within-group? (= "WITHIN_GROUP" (str (.getType ae)))
                idx (count @find-elements)]
            ;; Only an aggregate takes FILTER. Without this the name fell
            ;; through to the default aggregate below, so an unknown one
            ;; ANSWERED -- `nosuchfn(a) FILTER (WHERE true)` as a COUNT.
            (when (and filter-expr (not within-group?))
              (expr/validate-decorated-call!
               ctx fname (second (expr/call-node-name+args ae)) "FILTER"
               (some? agg-sym)))
            (reject-ordered-set-boundary! fname agg-sym inner-expr within-group?)
            (reject-vector-distinct-aggregate! (.isDistinct ae) inner-expr)
            (reset! has-aggregates? true)
            (if (and filter-expr agg-sym)
              (let [is-count? (= fname "count")
                    count-star? (and is-count?
                                     (or (nil? inner-expr)
                                         (instance? AllColumns inner-expr)))
                    case-var (filter-arg-var! ctx filter-expr inner-expr
                                              default-table count-star?
                                              (when (contains? two-arg-aggs fname)
                                                (.getOffset ae))
                                              (if (contains? null-preserving-aggs fname)
                                                fns/filtered-out
                                                :__null__))
                    ;; Per-input-type variant — same numeric-promotion
                    ;; rule as the non-FILTER aggregate path.
                    filter-precision-variant
                    (when (and inner-expr (not count-star?))
                      (pick-precision-variant
                       fname
                       (oid/expr-oid inner-expr agg-oid-env)))
                    ;; The same aggregate the unfiltered form uses.
                    ;; This was a four-name `case` whose default was
                    ;; `filter-sum`, so `array_agg(x) FILTER (…)`,
                    ;; `string_agg`, `stddev` and every other aggregate
                    ;; silently computed a SUM instead.
                    filter-agg (cond
                                 is-count? 'datahike.pg.sql/filter-count
                                 filter-precision-variant filter-precision-variant
                                 :else (or agg-sym 'datahike.pg.sql/filter-sum))]
                (swap! find-elements conj (list filter-agg case-var))
                (swap! find-aliases conj (or alias0 fname)))
              ;; No filter — treat as regular aggregate. A name that is
              ;; not one defaulted to COUNT here, which answers a number
              ;; for a function that does not exist.
              (let [v (if inner-expr (expr/translate-expr ctx inner-expr)
                          (ctx/entity-var! ctx default-table))]
                (when-not agg-sym
                  (expr/undefined-function!
                   ctx fname (second (expr/call-node-name+args ae))))
                (swap! find-elements conj (list agg-sym v))
                (swap! find-aliases conj (or alias0 fname))))
            idx))

        ;; ONE aggregate emitter. The select-item branch below and the
        ;; HOISTED aggregates (an aggregate nested inside a larger
        ;; expression -- `round(avg(x), 2)`) both go through it, so a
        ;; nested aggregate gets the same COUNT(*) / DISTINCT / FILTER /
        ;; two-argument / in-aggregate-ORDER-BY handling and the same
        ;; precision variant as a top-level one. Appends the aggregate's
        ;; form to `find-elements` under `alias0` and returns its index.
        emit-agg-fn!
        (fn [^Function f-node alias0]
          (let [idx (count @find-elements)]
            (let [^Function f f-node
                  fname (expr/resolution-name (.getName f))
                  agg-sym (get fns/sql-aggregate->datalog fname)
                  params (.getParameters f)
                  is-distinct? (.isDistinct f)]
              (when (contains? #{"percentile_cont" "percentile_disc" "mode"} fname)
                (throw (errors/pg-error
                        :undefined-function
                        {:function (str fname "(...) without WITHIN GROUP")})))
              (reject-vector-distinct-aggregate!
               is-distinct? (when (seq params) (first params)))
              (reset! has-aggregates? true)
              (let [is-count-star? (or (nil? params)
                                       (= 0 (count params))
                                       (and (= 1 (count params))
                                            (instance? AllColumns (first params))))
                    is-count-col? (and (= fname "count")
                                       (not is-count-star?)
                                       params (pos? (count params)))]
                (if (and (= fname "count") is-count-star?)
                        ;; COUNT(*) — count entities using row-marker if available
                  (let [evar (ctx/entity-var! ctx default-table)
                        table-name (get (:table-aliases ctx) default-table default-table)
                        marker-attr (pgs/row-marker-attr table-name)]
                    (when (empty? @(:where-clauses ctx))
                      (if (get schema marker-attr)
                        (ctx/add-clause! ctx [evar marker-attr true])
                        (let [cols (pgs/column-info schema table-name db)]
                          (when-let [first-col (second cols)]
                            (ctx/col-var! ctx (:attr first-col))))))
                    (if is-distinct?
                      (swap! find-elements conj (list 'count-distinct evar))
                      (swap! find-elements conj (list 'count evar)))
                    (swap! find-aliases conj (or alias0 "count")))
                        ;; Multi-argument aggregates (CORR)
                        ;; Two-argument aggregates: CORR(y,x) and the
                        ;; object-aggs, which all fold over [a b] pairs.
                        ;; string_agg folds over [value delimiter] pairs, the
                        ;; same two-argument shape CORR and the object
                        ;; aggregates already use — it was a per-row fn that
                        ;; stringified one value and dropped the delimiter, so
                        ;; it returned one row per input row.
                  (if (and (contains? #{'datahike.pg.sql/filter-corr
                                        'datahike.pg.sql/filter-jsonb-object-agg
                                        'datahike.pg.sql/filter-json-object-agg
                                        'datahike.pg.sql/filter-string-agg}
                                      agg-sym)
                           params (= 2 (count params)))
                    (let [;; CORR has only a float8 overload. PostgreSQL's
                          ;; function resolver therefore feeds unknown string
                          ;; literals through float8in before aggregation
                          ;; (`corr(g, 'NaN')`). Leaving them as strings leaks
                          ;; parser type uncertainty into the runtime and used
                          ;; to end in a String->Number ClassCastException.
                          translate-arg
                          (fn [arg]
                            (if (and (= agg-sym 'datahike.pg.sql/filter-corr)
                                     (instance? StringValue arg))
                              (coerce/coerce-numeric (.getValue ^StringValue arg) :double)
                              (expr/translate-expr ctx arg)))
                          v1 (translate-arg (first params))
                          v2 (translate-arg (second params))
                          v1 (if (seq? v1) (ctx/materialize-arg! ctx v1) v1)
                          v2 (if (seq? v2) (ctx/materialize-arg! ctx v2) v2)
                          pair-var (ctx/fresh-var! ctx)
                                ;; string_agg(expr, delim ORDER BY …) — element
                                ;; order is observable in the joined string, so
                                ;; it needs the same treatment array_agg gets.
                                ;; The triple carries the delimiter along with
                                ;; the sort key and the value.
                          order-els (when (= agg-sym 'datahike.pg.sql/filter-string-agg)
                                      (seq (.getOrderByElements f)))]
                      ;; Preserve duplicate input rows when a relation drives
                      ;; the aggregate. A table-free aggregate has exactly one
                      ;; synthetic input row and no entity variable to bind;
                      ;; minting `?_eid` here made the final Datalog query fail
                      ;; with "Query for unknown vars" instead of aggregating
                      ;; its constant arguments.
                      (when default-table
                        (swap! (:with-vars ctx) conj
                               (ctx/entity-var! ctx default-table)))
                      (if order-els
                        (let [key-vars (mapv (fn [^net.sf.jsqlparser.statement.select.OrderByElement o]
                                               (let [kv (expr/translate-expr ctx (.getExpression o))]
                                                 (if (seq? kv) (ctx/materialize-arg! ctx kv) kv)))
                                             order-els)
                              sort-key (if (= 1 (count key-vars))
                                         (first key-vars)
                                         (ctx/materialize-arg! ctx (apply list 'vector key-vars)))
                              all-desc? (every? (fn [^net.sf.jsqlparser.statement.select.OrderByElement o]
                                                  (not (.isAsc o)))
                                                order-els)]
                          (ctx/add-clause! ctx [(list 'vector sort-key v1 v2) pair-var])
                          (swap! find-elements conj
                                 (list (if all-desc?
                                         'datahike.pg.sql/filter-string-agg-ordered-desc
                                         'datahike.pg.sql/filter-string-agg-ordered)
                                       pair-var)))
                        (do
                          (ctx/add-clause! ctx [(list 'vector v1 v2) pair-var])
                          (swap! find-elements conj (list agg-sym pair-var))))
                      (swap! find-aliases conj (or alias0 fname)))
                          ;; Single-argument: COUNT(col), SUM(col), AVG(col), etc.
                    (let [inner-expr (first params)
                          v (expr/translate-expr ctx inner-expr)
                                ;; Materialize expression args (e.g. SUM(a * b) → SUM(?v))
                          v (if (seq? v) (ctx/materialize-arg! ctx v) v)
                                ;; A CONSTANT argument -- `count(1)`, `avg(2.5)`,
                                ;; `min(-1)` -- still has to reach the aggregate
                                ;; as a VARIABLE. Datahike's find-spec parser has
                                ;; no IFindVars implementation for a Constant, so
                                ;; `(count 1)` raised a raw protocol error. Bind
                                ;; it per row: the entity var is already in :with,
                                ;; so `[(identity 1) ?c]` gives one ?c per row and
                                ;; `count` then counts rows, as PostgreSQL does.
                          v (if (symbol? v)
                              v
                              (ctx/materialize-arg! ctx (list 'identity v)))
                          enum-spec (when (and inner-expr
                                               (contains? #{"min" "max"} fname))
                                      (expr/enum-spec-for-exprs ctx [inner-expr]))
                          [agg-sym v]
                          (if enum-spec
                            (let [rank-var (expr/enum-rank-var! ctx enum-spec v)
                                  pair-var (ctx/fresh-var! ctx)]
                              (ctx/add-clause! ctx [(list 'vector rank-var v) pair-var])
                              [(if (= fname "min")
                                 'datahike.pg.sql/filter-enum-min
                                 'datahike.pg.sql/filter-enum-max)
                               pair-var])
                            [agg-sym v])
                                ;; Per-input-type variant for SUM/AVG. Compute
                                ;; OID against the original AST expression
                                ;; (post-ref-deref `v` is a logic var with no
                                ;; OID rule). Falls back silently to default
                                ;; agg-sym when input type doesn't match the
                                ;; precision-sensitive set.
                          precision-variant (when inner-expr
                                              (pick-precision-variant
                                               fname
                                               (oid/expr-oid inner-expr agg-oid-env)))
                          agg-sym (cond
                                    (and is-count-col? is-distinct?)
                                    'datahike.pg.sql/filter-count-distinct
                                    is-count-col?
                                    'datahike.pg.sql/filter-count
                                    precision-variant precision-variant
                                    :else agg-sym)
                                ;; Distinct aggregates (e.g. SUM(DISTINCT x)) deduplicate
                                ;; their input collection rather than doing a set scan.
                          is-dh-distinct? (= agg-sym 'count-distinct)]
                            ;; Prevent set deduplication for non-distinct aggregates:
                            ;; adding the entity var to :with preserves duplicate rows.
                            ;; Only when there IS a table -- a table-free
                            ;; `SELECT count(1)` has no entity to vary over, and
                            ;; minting one produced an unbound `?_eid` ("Query for
                            ;; unknown vars").
                      (when (and default-table (not is-dh-distinct?))
                        (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table)))
                            ;; array_agg(expr ORDER BY …): collect [sort-key value]
                            ;; pairs and sort in the agg fn so element order honors
                            ;; the in-aggregate ORDER BY (composite field order in
                            ;; asyncpg's introspection depends on this). Direction
                            ;; is taken uniformly from the keys (all-DESC → desc).
                      (let [order-els (when (= fname "array_agg")
                                        (seq (.getOrderByElements f)))]
                        (if order-els
                          (let [key-vars (mapv (fn [^net.sf.jsqlparser.statement.select.OrderByElement o]
                                                 (let [kv (expr/translate-expr ctx (.getExpression o))]
                                                   (if (seq? kv) (ctx/materialize-arg! ctx kv) kv)))
                                               order-els)
                                sort-key (if (= 1 (count key-vars))
                                           (first key-vars)
                                           (ctx/materialize-arg! ctx (apply list 'vector key-vars)))
                                pair-var (ctx/fresh-var! ctx)
                                all-desc? (every? (fn [^net.sf.jsqlparser.statement.select.OrderByElement o]
                                                    (not (.isAsc o)))
                                                  order-els)
                                ord-sym (if all-desc?
                                          'datahike.pg.sql/filter-array-agg-ordered-desc
                                          'datahike.pg.sql/filter-array-agg-ordered)]
                            (ctx/add-clause! ctx [(list 'vector sort-key v) pair-var])
                            (swap! find-elements conj (list ord-sym pair-var)))
                          (swap! find-elements conj (list agg-sym v))))
                      (swap! find-aliases conj (or alias0 fname)))))))
            idx))

        ;; One entry point for both node shapes: JSqlParser surfaces a bare
        ;; `sum(x)` as a Function and `sum(x) FILTER (…)` as an
        ;; AnalyticExpression, and a HOISTED aggregate can be either.
        emit-agg!
        (fn [f-node alias0]
          (expr/validate-aggregate-node! ctx f-node)
          (if (instance? net.sf.jsqlparser.expression.AnalyticExpression f-node)
            (emit-analytic-agg! f-node alias0)
            (emit-agg-fn! f-node alias0)))

        ;; --- Correlated scalar subqueries in the SELECT list (slice A of the
        ;; per-row / LATERAL executor — doc/design-alignment.md). A
        ;; scalar subquery that references an OUTER FROM alias is DEFERRED:
        ;; the item loop skips it (so it isn't evaluated-once → NULL), the
        ;; outer-correlation columns it reads are threaded into :find as
        ;; hidden cols, and exec-select runs the inner per outer row. When
        ;; none are present, `correlated-subqs` is empty and the SELECT path
        ;; below is unchanged.
        outer-aliases (into #{}
                            (comp cat (remove nil?) (map str/lower-case))
                            [(keys table-aliases) (vals table-aliases) [default-table]])
        single-outer-alias
        (when (and from-item (empty? joins))
          (some-> (or (try (some-> ^Alias (.getAlias ^FromItem from-item)
                                   .getName)
                           (catch Throwable _ nil))
                      (when (instance? Table from-item)
                        (.getName ^Table from-item)))
                  unquote-ident str/lower-case))
        correlated-subqs
        (when db
          (into []
                (keep-indexed
                 (fn [i ^SelectItem item]
                   (when-let [spec (correlated-select-item-spec
                                    (.getExpression item) outer-aliases single-outer-alias)]
                     (assoc spec
                            :out-pos i
                            :alias (or (select-item-alias item) "?column?")
                            :oid (correlated-item-oid spec schema db (:parse-sql ctx)))))
                 select-items)))
        corr-out-positions (into #{} (map :out-pos) correlated-subqs)
        ;; distinct [alias col] correlation columns, in stable order
        corr-cols-needed (vec (distinct (mapcat :corr-refs correlated-subqs)))
        ;; the item loop processes everything EXCEPT the deferred subqueries
        loop-items (into [] (keep-indexed (fn [i it] (when-not (corr-out-positions i) it))
                                          select-items))

        _ (doseq [^SelectItem item loop-items]
            (let [raw-expr (.getExpression item)
                  ;; A CAST over an aggregate used to be PEELED here and
                  ;; dispatched as the inner aggregate, on the grounds that
                  ;; the cast only re-types the result. It does not:
                  ;; `avg(n)::int` ROUNDS, and dropping the conversion
                  ;; answered 15.0000000000000000 where PostgreSQL answers
                  ;; 15. It is an expression over an aggregate like any
                  ;; other, and the hoisting path below handles it.
                  expr raw-expr
                  alias-str (select-item-alias item)
                  _ (reject-prohibited-target-srf! expr)]
              (cond
                ;; SELECT t.* — table-qualified wildcard. JSqlParser
                ;; surfaces this as AllTableColumns (which extends
                ;; AllColumns, so the AllTableColumns check MUST come
                ;; first). Resolve the alias → real table name, then
                ;; expand to that table's columns. Critical for CTE
                ;; projections (`select t.* from cte t`) and derived
                ;; tables — Metabase's build_privilege_map and pgjdbc's
                ;; getSchemas idiom both rely on it.
                (instance? net.sf.jsqlparser.statement.select.AllTableColumns expr)
                (let [^net.sf.jsqlparser.statement.select.AllTableColumns atc expr
                      ^net.sf.jsqlparser.schema.Table tbl (.getTable atc)
                      raw-name (when tbl
                                 (str/lower-case
                                  (or (when-let [a (.getAlias tbl)]
                                        (.getName ^Alias a))
                                      (.getName tbl))))
                      ;; Resolve alias → real table via table-aliases.
                      real (or (get table-aliases raw-name) raw-name)
                      cols (pgs/column-info schema real db)]
                  (doseq [col cols
                          :when (not= "db_id" (:name col))]
                    ;; Route through the [:aliased <alias> :real/col]
                    ;; form so the resulting var uses the FROM alias's
                    ;; entity-var (`?<alias>_eid`) rather than the
                    ;; canonical table's. Without this, `select t.*
                    ;; from t1 t` would emit anchor patterns under
                    ;; both `?t_eid` and `?t1_eid` and the planner
                    ;; sees them as independent — driving cartesian
                    ;; products at best, zero rows when the duplicate
                    ;; clause confuses constraint propagation.
                    (let [v (expr/column-value! ctx [:aliased raw-name (:attr col)])]
                      (swap! find-elements conj v)
                      (swap! find-aliases conj (:name col)))))

                ;; SELECT * — expand to all user columns (exclude db_id)
                (instance? AllColumns expr)
                ;; `default-table` is the query's ALIAS for the relation,
                ;; which is not the relation's name whenever the two
                ;; differ — `FROM emp e`, and every CTE / derived table /
                ;; table function, whose rows live in a synthetic
                ;; namespace. Resolving it is what the `t.*` branch above
                ;; already does; without it here, `SELECT * FROM emp e`
                ;; returned a row with ZERO columns.
                ;; EVERY relation in FROM order, not just the default
                ;; table. `SELECT * FROM t JOIN c` used to return t's
                ;; columns alone — a silently narrower row, which is
                ;; worse than an error because the client cannot tell.
                (doseq [[ali col-name]
                        ;; A merged column (USING / NATURAL) is emitted
                        ;; ONCE and FIRST, from the side that owns it,
                        ;; then every relation's remaining columns in
                        ;; FROM order -- PostgreSQL's expansion order.
                        (concat (:merged-order (meta table-aliases))
                                (for [[ali real] star-relations
                                      col (pgs/column-info schema real db)
                                      :when (and (not= "db_id" (:name col))
                                                 (not (contains?
                                                       (set (get (:merged-columns-by-alias
                                                                  (meta table-aliases)) ali))
                                                       (:name col))))]
                                  [ali (:name col)]))
                        :let [real (get table-aliases ali ali)
                              col (or (first (filter #(= col-name (:name %))
                                                     (pgs/column-info schema real db)))
                                      {:name col-name :attr (keyword real col-name)})]]
                    ;; Route through the [:aliased …] form so the column
                    ;; binds against the alias's entity var, matching the
                    ;; `t.*` expansion.
                  (let [v (expr/column-value! ctx (if (= real ali)
                                                    (:attr col)
                                                    [:aliased ali (:attr col)]))]
                    (swap! find-elements conj v)
                    (swap! find-aliases conj (or alias-str (:name col)))))

                ;; AnalyticExpression: FILTER aggregate or window function.
                ;; FILTER: has filterExpression, no partition/orderBy/window.
                ;; WINDOW: has partition/orderBy/window, or is a ranking function.
                (instance? net.sf.jsqlparser.expression.AnalyticExpression expr)
                (let [^net.sf.jsqlparser.expression.AnalyticExpression ae expr
                      fname (expr/resolution-name (.getName ae))
                      agg-sym (get fns/sql-aggregate->datalog fname)
                      inner-expr (.getExpression ae)
                      filter-expr (.getFilterExpression ae)
                      partition-list (.getPartitionExpressionList ae)
                      order-by-list (.getOrderByElements ae)
                      window-elem (.getWindowElement ae)
                      ;; AnalyticType.WITHIN_GROUP is the ordered-set
                      ;; aggregate flavor (PERCENTILE_CONT / _DISC, MODE).
                      ;; It carries an ORDER BY clause but is NOT a
                      ;; window function — we must NOT route it through
                      ;; the partition+frame post-processing path.
                      analytic-type (str (.getType ae))
                      within-group? (= "WITHIN_GROUP" analytic-type)
                      ordered-set-fn? (contains? #{"percentile_cont"
                                                   "percentile_disc"
                                                   "mode"} fname)
                      ranking-fns #{"row_number" "rank" "dense_rank" "ntile"
                                    "percent_rank" "cume_dist" "lag" "lead"}
                      ;; JSqlParser's AnalyticType says which of the three
                      ;; shapes this is: OVER (a window function),
                      ;; FILTER_ONLY (`agg(x) FILTER (WHERE …)`), or
                      ;; WITHIN_GROUP (an ordered-set aggregate). Inferring it
                      ;; from the PRESENCE of a partition / order / frame
                      ;; instead missed the empty window: `sum(i) OVER ()` has
                      ;; none of them, so it fell through to the plain
                      ;; aggregate path and raised "column must appear in the
                      ;; GROUP BY clause".
                      is-window? (and (not within-group?)
                                      (or (= "OVER" analytic-type)
                                          (seq partition-list) (seq order-by-list)
                                          window-elem (contains? ranking-fns fname)))]
                  (reject-ordered-set-boundary! fname agg-sym inner-expr within-group?)
                  (cond
                    ;; Ordered-set aggregate via WITHIN GROUP — translate
                    ;; with the same pair-aggregate pattern filter-corr
                    ;; uses: the percentile fraction (constant per query)
                    ;; rides alongside each per-row x value as a
                    ;; `[p x]` vector, and the aggregate fn unpacks p
                    ;; from the first pair. MODE has no parameter and
                    ;; receives raw x values.
                    (and within-group? ordered-set-fn?)
                    (do
                      (reset! has-aggregates? true)
                      (when (empty? order-by-list)
                        (throw (ex-info "WITHIN GROUP missing"
                                        {:error :syntax-error
                                         :detail (str fname " requires WITHIN GROUP (ORDER BY ...)")
                                         :fname fname})))
                      (let [first-ob ^net.sf.jsqlparser.statement.select.OrderByElement
                            (first order-by-list)
                            x-expr (.getExpression first-ob)
                            x-var (expr/translate-expr ctx x-expr)
                            x-var (if (seq? x-var) (ctx/materialize-arg! ctx x-var) x-var)
                            agg-fn-sym (get fns/sql-aggregate->datalog fname)]
                        (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table))
                        (cond
                          ;; MODE — no parameter, aggregate raw values
                          (= fname "mode")
                          (swap! find-elements conj (list agg-fn-sym x-var))
                          ;; PERCENTILE_CONT/_DISC — pair `p` with each x
                          :else
                          (let [p-val (expr/translate-expr ctx inner-expr)
                                p-val (if (seq? p-val) (ctx/materialize-arg! ctx p-val) p-val)
                                pair-var (ctx/fresh-var! ctx)]
                            (ctx/add-clause! ctx [(list 'vector p-val x-var) pair-var])
                            (swap! find-elements conj (list agg-fn-sym pair-var))))
                        (swap! find-aliases conj (or alias-str fname))))

                    is-window?
                    ;; Window function: collect spec for server-side post-processing.
                    ;; All base columns must be in :find so the post-processor can
                    ;; partition, sort, and compute values from the result tuples.
                    (let [_ (expr/validate-decorated-call!
                             ctx fname (second (expr/call-node-name+args ae)) "OVER"
                             (or (some? agg-sym) (expr/window-function-name? fname)))
                          ;; `OVER w` names a window defined once in the
                          ;; statement's WINDOW clause. The name was never
                          ;; resolved, so such a window had no PARTITION BY, no
                          ;; ORDER BY and no frame at all -- every row of the
                          ;; table in a single frame, silently.
                          ^net.sf.jsqlparser.expression.WindowDefinition
                          named-win (when-let [wn (.getWindowName ae)]
                                      (or (some (fn [^net.sf.jsqlparser.expression.WindowDefinition wd]
                                                  (when (= wn (.getWindowName wd)) wd))
                                                (.getWindowDefinitions select))
                                          (throw (errors/pg-error
                                                  :undefined-object
                                                  {:message (str "window \"" wn "\" does not exist")}))))
                          partition-list (or (when named-win (.getPartitionExpressionList named-win))
                                             partition-list)
                          order-by-list (or (when named-win (.getOrderByElements named-win))
                                            order-by-list)
                          window-elem (or (when named-win (.getWindowElement named-win))
                                          window-elem)
                          ;; Translate PARTITION BY columns to find-element indices
                          part-idxs (when (seq partition-list)
                                      (mapv (fn [pexpr]
                                              (let [v (expr/translate-expr ctx pexpr)
                                                    v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
                                                ;; Ensure column is in find
                                                (when-not (some #{v} @find-elements)
                                                  (swap! find-elements conj v)
                                                  (swap! find-aliases conj (str "__win_part_" (count @find-elements))))
                                                (.indexOf ^java.util.List @find-elements v)))
                                            partition-list))
                          ;; Translate ORDER BY columns
                          ob-specs (when (seq order-by-list)
                                     (mapv (fn [^net.sf.jsqlparser.statement.select.OrderByElement obe]
                                             (let [v (expr/translate-expr ctx (.getExpression obe))
                                                   v (if (seq? v) (ctx/materialize-arg! ctx v) v)
                                                   asc? (.isAsc obe)
                                                   ;; Explicit NULLS FIRST / NULLS LAST was
                                                   ;; parsed and dropped here (the statement's
                                                   ;; own ORDER BY next door keeps it), so
                                                   ;; `rank() OVER (ORDER BY v NULLS FIRST)`
                                                   ;; ranked the NULLs last.
                                                   nulls (condp = (str (.getNullOrdering obe))
                                                           "NULLS_FIRST" :first
                                                           "NULLS_LAST"  :last
                                                           nil)]
                                               (when-not (some #{v} @find-elements)
                                                 (swap! find-elements conj v)
                                                 (swap! find-aliases conj (str "__win_ord_" (count @find-elements))))
                                               [(.indexOf ^java.util.List @find-elements v)
                                                (if asc? :asc :desc)
                                                nulls]))
                                           order-by-list))
                          ;; The function's own arguments. `.getExpression`
                          ;; is the first, `.getOffset` the second and
                          ;; `.getDefaultValue` the third -- which is how
                          ;; `lag(v, 2, -1)`, `nth_value(v, 2)` and
                          ;; `string_agg(v, ',')` carry theirs. The offset
                          ;; and default were parsed and then DROPPED, so
                          ;; `lead(v, 2, -1)` ran as `lead(v)`.
                          ;;
                          ;; `count(*)` has AllColumns as its "argument",
                          ;; which is not a value expression -- translating it
                          ;; put a non-var into :find and Datahike rejected
                          ;; the whole query ("Cannot parse :find"). COUNT(*)
                          ;; counts rows in the frame, so there is no column
                          ;; to reference. NTILE's argument is its bucket
                          ;; COUNT, not a column, and it hit exactly that
                          ;; failure: `ntile(2) OVER (ORDER BY i)` put the
                          ;; literal 2 into :find and could not run at all.
                          count-star? (and (or (nil? inner-expr)
                                               (instance? AllColumns inner-expr))
                                           (nil? filter-expr))
                          ntile? (= fname "ntile")
                          arg-expr (when-not (or count-star? ntile? filter-expr) inner-expr)
                          ;; A window argument has to travel in :find so the
                          ;; post-processor can read it off the result tuple.
                          find-idx! (fn [e tag]
                                      (let [v (expr/translate-expr ctx e)
                                            v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
                                        (when-not (some #{v} @find-elements)
                                          (swap! find-elements conj v)
                                          (swap! find-aliases conj (str tag (count @find-elements))))
                                        (.indexOf ^java.util.List @find-elements v)))
                          ;; `agg(x) FILTER (WHERE p) OVER (…)`. The filter was
                          ;; parsed and DROPPED, so the window aggregate ran over
                          ;; every row of the frame. It is applied the way the
                          ;; non-window FILTER path applies it: the argument
                          ;; column becomes `x when p else NULL`, and every
                          ;; aggregate already skips the NULL sentinel -- so the
                          ;; filter costs the window engine nothing and works for
                          ;; whichever aggregate is on top.
                          filter-var (when filter-expr
                                       (filter-arg-var! ctx filter-expr inner-expr
                                                        default-table count-star?
                                                        (when (contains? two-arg-aggs fname)
                                                          (.getOffset ae))
                                                        (if (contains? null-preserving-aggs fname)
                                                          fns/filtered-out
                                                          :__null__)))
                          col-idx (cond
                                    filter-var (do (when-not (some #{filter-var} @find-elements)
                                                     (swap! find-elements conj filter-var)
                                                     (swap! find-aliases conj (str "__win_col_" (count @find-elements))))
                                                   (.indexOf ^java.util.List @find-elements filter-var))
                                    arg-expr (find-idx! arg-expr "__win_col_"))
                          off-expr (.getOffset ae)
                          def-expr (.getDefaultValue ae)
                          ;; string_agg's delimiter (and any other
                          ;; two-argument aggregate's second operand) is a
                          ;; per-row value like the first, so it rides in
                          ;; :find too and reaches the aggregate as the
                          ;; [value delimiter] pair it already expects.
                          two-arg-agg? (and off-expr (contains? #{"string_agg" "corr"} fname))
                          ;; A CONSTANT second operand -- which the delimiter
                          ;; almost always is -- must not go into :find:
                          ;; Datahike rejects a non-variable there ("Cannot
                          ;; parse :find"), which is what `string_agg(x, ',')
                          ;; OVER ()` died of.
                          arg2-const (when two-arg-agg?
                                       (let [v (expr/translate-expr ctx off-expr)]
                                         (when-not (or (symbol? v) (seq? v)) v)))
                          arg2-idx (when (and two-arg-agg? (nil? arg2-const))
                                     (find-idx! off-expr "__win_arg2_"))
                          const-of (fn [e] (when e
                                             (let [v (expr/translate-expr ctx e)]
                                               (when (number? v) (long v)))))
                          offset-n (when (and off-expr (not two-arg-agg?)) (const-of off-expr))
                          default-val (when def-expr (expr/translate-expr ctx def-expr))
                          ;; Parse frame specification. JSqlParser exposes the
                          ;; bounds structurally -- WindowRange.getStart/getEnd
                          ;; for `BETWEEN a AND b`, WindowElement.getOffset for
                          ;; a lone start bound -- and each WindowOffset says
                          ;; PRECEDING / FOLLOWING / CURRENT with a nil
                          ;; expression for UNBOUNDED. Reading them off the
                          ;; toString() instead is what made `ROWS UNBOUNDED
                          ;; PRECEDING` come out as the whole partition and
                          ;; `RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED
                          ;; FOLLOWING` come out as a running total.
                          parse-bound
                          (fn [^net.sf.jsqlparser.expression.WindowOffset o default-bound]
                            (if (nil? o)
                              default-bound
                              (let [t (str (.getType o))
                                    e (.getExpression o)
                                    n (when e (let [v (expr/translate-expr ctx e)]
                                                (when (number? v) (long v))))]
                                (case t
                                  "CURRENT" :current-row
                                  "PRECEDING" (if n [n :preceding] :unbounded-preceding)
                                  "FOLLOWING" (if n [n :following] :unbounded-following)
                                  default-bound))))
                          frame (if window-elem
                                  (let [range? (= "RANGE" (str (.getType window-elem)))
                                        r (.getRange window-elem)]
                                    (if r
                                      {:type (if range? :range :rows)
                                       :start (parse-bound (.getStart r) :unbounded-preceding)
                                       :end   (parse-bound (.getEnd r) :current-row)}
                                      ;; Only a start bound was given; SQL
                                      ;; defines the end as CURRENT ROW.
                                      {:type (if range? :range :rows)
                                       :start (parse-bound (.getOffset window-elem) :unbounded-preceding)
                                       :end :current-row}))
                                  ;; The SQL default frame: the whole partition
                                  ;; without an ORDER BY, and RANGE (peers, not
                                  ;; rows) up to the current row with one.
                                  (if (seq order-by-list)
                                    {:type :range :start :unbounded-preceding :end :current-row}
                                    {:type :rows :start :unbounded-preceding :end :unbounded-following}))
                          ;; Build window spec
                          _ (expr/validate-aggregate-node! ctx ae)
                          op-kw (keyword fname)
                          ;; The aggregate is the SAME function the plain
                          ;; (non-window) path uses, chosen by the same
                          ;; precision rule -- so `sum(numeric) OVER ()` keeps
                          ;; its scale and `avg(int) OVER ()` is NUMERIC, and
                          ;; every aggregate the window engine's private `case`
                          ;; never named (array_agg, string_agg, stddev, …)
                          ;; works by construction rather than answering NULL.
                          arg-oid (when-let [e (or arg-expr (when filter-expr
                                                              (when-not (instance? AllColumns inner-expr)
                                                                inner-expr)))]
                                    (try (oid/expr-oid e agg-oid-env)
                                         (catch Throwable _ nil)))
                          ;; Interval values currently use their textual wire
                          ;; representation internally. Feeding those strings
                          ;; to the numeric SUM/AVG implementations leaked a
                          ;; JVM ClassCastException. Until intervals have a
                          ;; structural value type, refuse these two aggregate
                          ;; overloads explicitly instead of exposing an
                          ;; internal failure as SQL behavior.
                          interval-arg?
                          (or (= types/oid-interval arg-oid)
                              (and (instance? CastExpression arg-expr)
                                   (= "interval"
                                      (some-> ^CastExpression arg-expr
                                              .getColDataType str str/lower-case))))
                          _ (when (and interval-arg?
                                       (contains? #{"sum" "avg"} fname))
                              (throw (errors/pg-error
                                      :feature-not-supported
                                      {:feature (str fname "(interval) as a window function")})))
                          agg-sym (when agg-sym
                                    (or (pick-precision-variant fname arg-oid) agg-sym))
                          result-oid (try (oid/expr-oid expr agg-oid-env)
                                          (catch Throwable _ nil))
                          item-index (.indexOf ^java.util.List select-items item)
                          visible-base (count (remove (fn [a]
                                                        (and (string? a)
                                                             (or (.startsWith ^String a "__win_")
                                                                 (.startsWith ^String a "__compound_"))))
                                                      @find-aliases))
                          prior-correlated (count (filter #(< (:out-pos %) item-index)
                                                          correlated-subqs))
                          output-pos (+ visible-base
                                        (count @window-specs)
                                        (count @compound-exprs)
                                        prior-correlated)
                          win-spec (cond-> {:op op-kw
                                            :out-pos output-pos
                                            :partition-by (or part-idxs [])
                                            :order-by (or ob-specs [])
                                            :frame frame}
                                     result-oid (assoc :oid result-oid)
                                     count-star? (assoc :count-star? true)
                                     col-idx (assoc :col-idx col-idx)
                                     arg2-idx (assoc :arg2-idx arg2-idx)
                                     (some? arg2-const) (assoc :arg2-const arg2-const)
                                     offset-n (assoc :offset-n offset-n)
                                     (some? default-val) (assoc :default-val default-val)
                                     agg-sym (assoc :agg-sym agg-sym)
                                     ntile? (assoc :ntile-n (const-of inner-expr)))]
                      ;; Don't add alias to find-aliases — the server adds it
                      ;; after computing the window values. find-aliases must match
                      ;; the Datalog :find elements count.
                      (swap! window-specs conj (assoc win-spec :alias (or alias-str fname))))

                    ;; Not a window — handle as FILTER aggregate or plain aggregate
                    :else
                    (emit-analytic-agg! ae alias-str)))

                ;; Aggregate: COUNT(*), SUM(col), etc.
                (and (instance? Function expr)
                     (fns/aggregate-function? (expr/resolution-name (.getName ^Function expr))))
                (emit-agg! ^Function expr alias-str)

                ;; PostgreSQL evaluates a top-level set-returning function in
                ;; the SELECT list through a ProjectSet node. Keep one
                ;; placeholder in the visible projection and carry the SRF
                ;; arguments as trailing hidden columns; exec-select expands
                ;; each base row after its base ORDER BY and before LIMIT.
                (target-list-srf? expr)
                (lower-project-srf! ^Function expr alias-str true)

                ;; Regular column or expression
                :else
                ;; An aggregate may be nested ANYWHERE in this expression --
                ;; `round(avg(x), 2)`, `coalesce(sum(x), 0)`, `max(a) - min(a)`.
                ;; translate-expr hoists each one into a variable and
                ;; registers it here; what comes back is an ordinary form
                ;; over those variables, evaluated per GROUP after the query.
                ;;
                ;; This used to handle binary ARITHMETIC over aggregates only,
                ;; with its own tree shape and its own four-operator
                ;; evaluator. Anything else -- a function call, a cast, a
                ;; CASE -- either raised (`round(avg(x),2)`: "No matching ctor
                ;; found for class java.math.BigDecimal") or returned the
                ;; internal aggregate MARKER MAP to the client as data
                ;; (`coalesce(sum(n),0)` answered `{":fn": "sum", …}`).
                (let [sink (atom [])
                      before (count @(:where-clauses ctx))
                      out-pos (+ (count (remove #(and (string? %)
                                                      (.startsWith ^String % "__compound_"))
                                                @find-aliases))
                                 (count @compound-exprs))
                      aggregate-projection?
                      (boolean (some fns/aggregate-function?
                                     (params/ast-function-names expr)))
                      v (binding [expr/*defer-stateful-projection?* true]
                          (expr/translate-expr (assoc ctx
                                                      :hoisted-aggs sink
                                                      :aggregate-projection?
                                                      aggregate-projection?) expr))
                      hoisted @sink]
                  (if (seq hoisted)
                    (let [all (vec @(:where-clauses ctx))
                          [keep-cs form] (expr/split-aggregate-projection
                                          v (subvec all before) (map :var hoisted))
                          ;; The clauses ABOVE the aggregate line never
                          ;; belonged to the query -- they reference a find
                          ;; element rather than a binding, so datahike left
                          ;; the projection variable unbound and the column
                          ;; came back as the literal symbol `?v2`.
                          _ (reset! (:where-clauses ctx)
                                    (into (subvec all 0 before) keep-cs))
                          slots (mapv (fn [{:keys [var fn-node]}]
                                        [var (emit-agg! fn-node
                                                        (str "__compound_" (count @find-elements)))])
                                      hoisted)]
                      (swap! compound-exprs conj
                             ;; figure-colname, not the expression's text:
                             ;; PostgreSQL names a computed column
                             ;; `?column?`, and the raw text also leaked the
                             ;; `$1` the plan-cache rewrite left behind
                             ;; (`sum(v)/$1`).
                             {:alias (or alias-str (figure-colname expr))
                              :out-pos out-pos
                              :form  form
                              :slots slots}))
                    ;; Regular non-aggregate expression
                    (let [all-clauses (vec @(:where-clauses ctx))
                          emitted (subvec all-clauses before)
                          effectful? (some #(and (symbol? %)
                                                 (str/starts-with?
                                                  (clojure.core/name %)
                                                  "?nextval-marker-"))
                                           (tree-seq coll? seq
                                                     (cons v (map first emitted))))
                          scalar-binding? (some #(and (vector? %)
                                                      (= 2 (count %))
                                                      (seq? (first %))
                                                      (symbol? (second %))
                                                      (not= 'get-else (ffirst %)))
                                                emitted)
                          ordered-expression?
                          (some (fn [^OrderByElement obe]
                                  (let [order-expr (.getExpression obe)]
                                    (or (= (str expr) (str order-expr))
                                        (and alias-str
                                             (instance? Column order-expr)
                                             (nil? (.getTable ^Column order-expr))
                                             (= alias-str
                                                (.getColumnName ^Column order-expr)))
                                        (and (instance? LongValue order-expr)
                                             (= (inc (.indexOf ^java.util.List
                                                      select-items item))
                                                (.getValue ^LongValue order-expr))))))
                                (.getOrderByElements select))
                          defer? (and (nil? group-by-element)
                                      (or (and expr/*defer-all-projection-computations?*
                                               scalar-binding?
                                               (not ordered-expression?))
                                          (and (not has-distinct?)
                                               (or (and effectful?
                                                        (empty? (.getOrderByElements select)))
                                                   (and where-expr
                                                        (empty? (.getOrderByElements select))
                                                        (throwing-projection? v emitted))))))]
                      (if defer?
                        (let [[keep-cs form]
                              ((if expr/*defer-all-projection-computations?*
                                 inline-all-projection-bindings
                                 inline-projection-bindings)
                               v emitted)
                              _ (reset! (:where-clauses ctx)
                                        (into (subvec all-clauses 0 before) keep-cs))
                              in-vars (set @(:in-params ctx))
                              value-vars (->> (tree-seq coll? seq form)
                                              (filter symbol?)
                                              (filter #(and (.startsWith ^String (clojure.core/name %) "?")
                                                            (not (contains? in-vars %))))
                                              distinct)]
                          ;; Carry each leaf value through the filtered query.
                          ;; Existing projected vars are reused; new ones are
                          ;; hidden and stripped after post-processing.
                          (doseq [value-var value-vars
                                  :when (not (some #{value-var} @find-elements))]
                            (swap! find-elements conj value-var)
                            (swap! find-aliases conj
                                   (str "__compound_guard_" (count @find-aliases))))
                          (swap! compound-exprs conj
                                 {:alias (or alias-str (figure-colname expr))
                                  :out-pos out-pos
                                  :form form
                                  :slots []}))
                        (let [v (cond
                                  (seq? v)          (ctx/materialize-arg! ctx v)
                                  (not (symbol? v)) (let [var (ctx/fresh-var! ctx)
                                                         ;; Datahike drops rows when a fn-binding
                                                         ;; produces nil; use the :__null__ sentinel
                                                         ;; for SQL NULL projections so the row
                                                         ;; survives. The wire layer maps the
                                                         ;; sentinel back to NULL on output.
                                                          bind-v (if (nil? v) :__null__ v)]
                                                      (ctx/add-clause! ctx [(list 'identity bind-v) var])
                                                      var)
                                  :else             v)]
                          (swap! find-elements conj v)
                          ;; PG's naming rules — NOT the datalog variable
                          ;; (`p1`, `v1`) or the expression's SQL text, which
                          ;; is what the old fallback chain produced.
                          (swap! find-aliases conj (or alias-str
                                                       (figure-colname expr)))))))))))

        ;; ProjectSet arguments must survive the base Datalog query so the
        ;; executor can evaluate each SRF per base row. Append them after all
        ;; visible SELECT items, preserving the SQL projection positions.
        project-base-find-count (count @find-elements)
        _ (doseq [v (mapcat :arg-vars @project-set-specs)
                  :when (neg? (.indexOf ^java.util.List @find-elements v))]
            (swap! find-elements conj v))
        project-set-specs
        (mapv (fn [spec]
                (assoc spec
                       :out-pos (or (:out-pos spec)
                                    (.indexOf ^java.util.List @find-elements
                                              (:out-var spec)))
                       :arg-indices (mapv #(.indexOf ^java.util.List
                                            @find-elements %)
                                          (:arg-vars spec))))
              @project-set-specs)
        project-set-hidden (- (count @find-elements) project-base-find-count)

        ;; Thread each distinct correlation column (e.g. t.oid) into :find
        ;; as a trailing hidden column so every outer row carries the value
        ;; exec-select binds into *from-bindings* before running the inner.
        ;; Returns {[alias col] → index-in-find}.
        corr-col->idx
        (when (seq corr-cols-needed)
          (into {}
                (map-indexed
                 (fn [n [alias col]]
                   (let [colexpr (doto (net.sf.jsqlparser.schema.Column.)
                                   (.setColumnName col)
                                   (.setTable (net.sf.jsqlparser.schema.Table. ^String alias)))
                         v (expr/translate-expr ctx colexpr)
                         idx (count @find-elements)]
                     (swap! find-elements conj v)
                     (swap! find-aliases conj (str "__corr_" n))
                     [[alias col] idx]))
                 corr-cols-needed)))

        ;; Parse-time OID inference for each select-item expression.
        ;; Walks the JSqlParser AST to produce a result OID per element
        ;; of :find-aliases, mirroring PG's exprType (see
        ;; datahike.pg.sql.oid-infer). Consumed by describeResult so
        ;; Extended Query's RowDescription has correct types before
        ;; Execute runs. Window-function / compound-aggregate columns
        ;; don't line up with a simple select-item index, so we bail
        ;; to nil for any index we can't resolve — caller falls back
        ;; to the value-based inference used by the Simple Query path.
        oid-env {:db db :schema schema
                 :table-aliases table-aliases
                 :default-table default-table
                 :from-binding-oids params/*from-binding-oids*
                 :scalar-subquery-oid #(expr/scalar-subquery-output-oid ctx %)
                 :hints (pgs/schema-hints db)}
        ;; OID inference per select-item. The expr-oid walker handles
        ;; AnalyticExpression (windows, WITHIN GROUP) and arithmetic
        ;; combinations of aggregates, so we don't gate on window/
        ;; compound — both inferences are sound. Padding to
        ;; find-aliases length absorbs extra entries those features
        ;; add to :find that don't map 1:1 to a select-item.
        ;; Accumulates TWO index-aligned vectors in one pass — the OID
        ;; per output column, and the 1-based `$N` index when the column
        ;; is a bare parameter placeholder. They must be built together:
        ;; a `*` select item contributes N entries to both, so computing
        ;; them separately would drift out of alignment.
        ;;
        ;; The param index (not its type) is what gets cached, because
        ;; the parse cache is keyed by SQL text while the parameter's
        ;; declared type is per-Parse-message. describeResult resolves
        ;; index → OID against :declared-param-oids at Describe time.
        [select-item-oids* select-item-resolution-oids* select-item-param-idx*]
        (let [[oids resolution-oids idxs]
              (reduce
               (fn [[v rv pv] ^SelectItem item]
                 (let [expr (.getExpression item)]
                   (cond
                           ;; AllTableColumns must come BEFORE AllColumns
                           ;; (it's a subclass).
                     (instance? net.sf.jsqlparser.statement.select.AllTableColumns expr)
                     (let [^net.sf.jsqlparser.statement.select.AllTableColumns atc expr
                           ^net.sf.jsqlparser.schema.Table tbl (.getTable atc)
                           raw-name (when tbl
                                      (str/lower-case
                                       (or (when-let [a (.getAlias tbl)]
                                             (.getName ^Alias a))
                                           (.getName tbl))))
                           real (or (get table-aliases raw-name) raw-name)
                           cols (pgs/column-info schema real db)
                           picked (remove #(= "db_id" (:name %)) cols)]
                       [(into v (map :oid) picked)
                        (into rv (map :oid) picked)
                        (into pv (repeat (count picked) nil))])
                     (instance? AllColumns expr)
                     (let [cols (pgs/column-info
                                 schema (get table-aliases default-table default-table) db)
                           picked (remove #(= "db_id" (:name %)) cols)]
                       [(into v (map :oid) picked)
                        (into rv (map :oid) picked)
                        (into pv (repeat (count picked) nil))])
                     :else
                     [(conj v (oid/expr-oid expr oid-env))
                      (conj rv (oid/resolution-oid expr oid-env))
                      (conj pv (oid/param-placeholder-index expr))])))
               [[] [] []]
                   ;; loop-items excludes deferred correlated subqueries, so
                   ;; these OIDs line up with the non-subquery part of
                   ;; find-aliases (the __corr_ tail pads to nil below).
               loop-items)
                ;; find-aliases may be longer than acc when SELECT
                ;; contains JOIN-driven entity vars added to :find
                ;; for :with semantics. Pad with nil so the vector
                ;; lines up index-for-index with find-aliases.
              n (count @find-aliases)
              pad (fn [acc] (vec (take n (concat acc (repeat nil)))))]
          [(pad oids) (pad resolution-oids) (pad idxs)])
        select-item-oids select-item-oids*
        select-item-resolution-oids select-item-resolution-oids*
        select-item-param-idx select-item-param-idx*
        _ (when (and has-distinct? (empty? distinct-on-items)
                     (some #{types/oid-vector} select-item-oids))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "DISTINCT over vector values is not supported"})))
        distinct-on-oids
        (when (seq distinct-on-items)
          (mapv (fn [^SelectItem item]
                  (try (oid/expr-oid (.getExpression item) oid-env)
                       (catch Throwable _ nil)))
                distinct-on-items))
        _ (when (some #{types/oid-vector} distinct-on-oids)
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "DISTINCT ON over vector keys is not supported"})))
        group-by-oids
        (when (seq group-by)
          (mapv (fn [g]
                  (cond
                    (instance? LongValue g)
                    (nth select-item-oids (dec (.getValue ^LongValue g)) nil)

                    (group-by-alias-only? g)
                    (let [alias (unquote-ident (.getColumnName ^Column g))
                          idx (.indexOf ^java.util.List @find-aliases alias)]
                      (nth select-item-oids idx nil))

                    :else
                    (try (oid/expr-oid g oid-env)
                         (catch Throwable _ nil))))
                group-by))
        _ (when (some #{types/oid-vector} group-by-oids)
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "GROUP BY over vector values is not supported"})))

        ;; For JOINs: add entity vars to :with to prevent dedup of rows
        ;; from different entity combinations that produce identical values.
        ;; For LEFT JOINs, only add the left table's entity var (not the
        ;; right-side which may be synthetic in the unmatched branch).
        _ (when (seq join-infos)
            (let [right-aliases (set (map :alias join-infos))
                  left-join? (some #(= :left (:join-type %)) join-infos)]
              (doseq [[alias-key evar] @(:entity-vars ctx)]
                (when (and (or (not left-join?)
                               (not (contains? right-aliases alias-key)))
                           ;; A computed relation has no entities; its
                           ;; entity var is never bound, so putting it in
                           ;; :with makes the query unsatisfiable.
                           (not (contains? (:computed-aliases ctx) alias-key)))
                  (swap! (:with-vars ctx) conj evar)))))

        ;; Anchor for the default-table entity var. If every clause
        ;; referencing evar is a get-else (e.g. ctx/col-var! output), the
        ;; var is never actually bound — get-else needs its entity input
        ;; already grounded. Add a plain data pattern so iteration has
        ;; a starting point. Applies equally to aggregate queries
        ;; (GROUP BY col_with_null would drop the NULL-group row
        ;; otherwise, since no data pattern drives entity enumeration).
        _ (when default-table
            (let [table-name (get table-aliases default-table default-table)
                  evar (ctx/entity-var! ctx default-table)
                  marker (pgs/row-marker-attr table-name)
                  has-plain-anchor?
                  (some (fn [c]
                          (and (vector? c)
                               (= 3 (count c))
                               (= evar (first c))
                               (keyword? (second c))))
                        @(:where-clauses ctx))]
              (when-not has-plain-anchor?
                (if (get schema marker)
                  (ctx/add-clause! ctx [evar marker true])
                  (when-let [cols (pgs/column-info schema table-name db)]
                    (when-let [first-col (second cols)]
                      (ctx/add-clause! ctx [evar (:attr first-col) (ctx/fresh-var! ctx)])))))))

        ;; SQL bag semantics for non-aggregate / non-DISTINCT queries.
        ;; Without evar in :with, two entities with identical SELECT-column
        ;; values collapse into one tuple at the set-level find. Adding
        ;; evar preserves per-entity rows (SQL's default ALL behavior).
        ;; This is independent of the anchor above — anchor gives us a
        ;; binding for evar; :with preserves row multiplicity.
        ;;
        ;; Skipped when GROUP BY is present without an aggregate: the user
        ;; asked for distinct groups, so preserving entity-level multiplicity
        ;; would defeat the dedup the :find tuple is supposed to produce.
        _ (when (and (not @has-aggregates?) (not has-distinct?)
                     (not (seq group-by))
                     default-table)
            (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table)))

        ;; Translate a Function into the same `(agg-sym ?v)` shape
        ;; the SELECT-item aggregate branch emits. Returns nil for
        ;; shapes we don't synthesize (COUNT(*), CORR, ordered-set
        ;; aggregates with WITHIN GROUP, …) — those are rare in a
        ;; HAVING-only position and can be added if needed.
        translate-agg
        (fn [^Function f]
          (let [fname (expr/resolution-name (.getName f))
                params (.getParameters f)
                is-count-star? (or (nil? params)
                                   (zero? (count params))
                                   (and (= 1 (count params))
                                        (instance? AllColumns (first params))))]
            (cond
              (and (= fname "count") is-count-star? default-table)
              (let [evar (ctx/entity-var! ctx default-table)
                    table-name (get (:table-aliases ctx) default-table default-table)
                    marker-attr (pgs/row-marker-attr table-name)]
                (when (empty? @(:where-clauses ctx))
                  (if (get schema marker-attr)
                    (ctx/add-clause! ctx [evar marker-attr true])
                    (when-let [cols (pgs/column-info schema table-name db)]
                      (when-let [first-col (second cols)]
                        (ctx/col-var! ctx (:attr first-col))))))
                (list 'count evar))
              (and params (= 1 (count params)) (not is-count-star?))
              (let [agg-sym (get fns/sql-aggregate->datalog fname)
                    v (expr/translate-expr ctx (first params))
                    v (if (seq? v) (ctx/materialize-arg! ctx v) v)
                    ;; A CONSTANT argument still has to reach the aggregate as
                    ;; a VARIABLE -- Datahike's find-spec parser has no
                    ;; IFindVars for a Constant. Same binding the projection
                    ;; path does; this path (HAVING / ORDER BY) missed it, so
                    ;; `HAVING sum(1) IS NOT NULL` raised a raw protocol error.
                    v (if (symbol? v) v (ctx/materialize-arg! ctx (list 'identity v)))
                    precision-variant (pick-precision-variant
                                       fname
                                       (oid/expr-oid (first params) agg-oid-env))]
                (when agg-sym
                  (when-not (= fname "count")
                    (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table)))
                  (list (or precision-variant agg-sym) v))))))

        ;; ORDER BY — resolve aliases to find-elements before creating patterns
        order-by (.getOrderByElements select)
        order-by-spec (when (seq order-by)
                        (let [fe-snap @find-elements
                              fa-snap @find-aliases]
                          (mapv (fn [^OrderByElement obe]
                                  (let [expr (.getExpression obe)
                                        asc? (.isAsc obe)
                                        ;; Explicit NULLS FIRST / NULLS LAST. PostgreSQL's
                                        ;; DEFAULT is NULLS LAST for ASC and NULLS FIRST for
                                        ;; DESC (NULL sorts as the largest value), which the
                                        ;; comparator already did -- but an explicit clause
                                        ;; was DISCARDED, so `ORDER BY x ASC NULLS FIRST`
                                        ;; silently returned the default order instead.
                                        nulls (condp = (str (.getNullOrdering obe))
                                                "NULLS_FIRST" :first
                                                "NULLS_LAST"  :last
                                                nil)
                                        order-source-expr
                                        (cond
                                          (instance? LongValue expr)
                                          (let [pos (.getValue ^LongValue expr)]
                                            (when (<= 1 pos (count select-items))
                                              (.getExpression ^SelectItem
                                               (nth select-items (dec pos)))))

                                          (instance? Column expr)
                                          (let [col-name (.getColumnName ^Column expr)
                                                tbl (.getTable ^Column expr)
                                                alias-idx (when (nil? tbl)
                                                            (some (fn [[i a]]
                                                                    (when (= a col-name) i))
                                                                  (map-indexed vector fa-snap)))]
                                            (if (and alias-idx (< alias-idx (count select-items)))
                                              (let [selected (.getExpression ^SelectItem
                                                              (nth select-items alias-idx))]
                                                ;; `SELECT * ... ORDER BY col`: star expansion
                                                ;; creates one find alias per physical column,
                                                ;; but `select-items` still contains only the
                                                ;; single `*` AST node. Keep the ORDER BY column
                                                ;; as the type source in that case.
                                                (if (instance? AllColumns selected) expr selected))
                                              expr))

                                          :else expr)
                                        matching-select-idx
                                        (some (fn [[i ^SelectItem selected]]
                                                (when (= (str expr)
                                                         (str (.getExpression selected)))
                                                  i))
                                              (map-indexed vector select-items))
                                        ;; Check if ORDER BY references a SELECT alias
                                        v (cond
                                            ;; A bare integer constant is a 1-based
                                            ;; ORDINAL into the select list, not a value
                                            ;; to sort by (PostgreSQL's
                                            ;; findTargetlistEntrySQL92 accepts only an
                                            ;; integer A_Const here, so `ORDER BY 1+1`
                                            ;; stays an expression). We translated it as
                                            ;; a constant, which sorts by nothing — so
                                            ;; `ORDER BY 2 DESC` silently returned rows
                                            ;; in whatever order the scan produced.
                                            ;; Only an INTEGER constant is an ordinal.
                                            ;; PostgreSQL rejects any other bare
                                            ;; constant outright rather than sorting
                                            ;; every row by the same value
                                            ;; (findTargetlistEntrySQL92's
                                            ;; !IsA(Integer) branch).
                                            (bare-non-integer-constant? expr)
                                            (throw (ex-info "non-integer constant in ORDER BY"
                                                            {:error :syntax-error
                                                             :sqlstate "42601"}))

                                            (instance? LongValue expr)
                                            (let [pos (.getValue ^LongValue expr)]
                                              (when (or (< pos 1) (> pos (count fa-snap)))
                                                (throw (ex-info (str "ORDER BY position " pos
                                                                     " is not in select list")
                                                                {:error :invalid-column-reference
                                                                 :sqlstate "42P10"})))
                                              (nth fe-snap (dec pos)))

                                            (instance? Column expr)
                                            (let [col-name (.getColumnName ^Column expr)
                                                  tbl (.getTable ^Column expr)
                                                  ;; Only alias-resolve unqualified column refs
                                                  alias-idx (when (nil? tbl)
                                                              (some (fn [[i a]]
                                                                      (when (= a col-name) i))
                                                                    (map-indexed vector fa-snap)))]
                                              (if alias-idx
                                                (nth fe-snap alias-idx)
                                                (expr/translate-expr ctx expr)))

                                            (some? matching-select-idx)
                                            (nth fe-snap matching-select-idx)

                                            (target-list-srf? expr)
                                            (or (some (fn [{:keys [function out-var]}]
                                                        (when (= (str function) (str expr))
                                                          out-var))
                                                      project-set-specs)
                                                (throw (errors/pg-error
                                                        :feature-not-supported
                                                        {:feature "ORDER BY an unprojected set-returning function"})))

                                            :else (expr/translate-expr ctx expr))
                                        ;; Materialize expression results for ORDER BY.
                                        ;; NOT aggregate forms — those are find-elements looked
                                        ;; up by index, not function bindings. Other qualified
                                        ;; datahike.* calls (sql-+, sql-*, null-safe scalar ops)
                                        ;; SHOULD be materialized so the resulting bind var can
                                        ;; be referenced by :order-by.
                                        ;;
                                        ;; Includes the precision-variant aggregate fns
                                        ;; (filter-sum-numeric / filter-avg-numeric) emitted
                                        ;; for INT8/NUMERIC inputs by pick-precision-variant —
                                        ;; without these, ORDER BY <agg-alias> against a
                                        ;; numeric column re-materialised the agg-form as a
                                        ;; where-clause function call and failed at execute
                                        ;; time with "Unknown function filter-sum-numeric".
                                        agg-syms (into (set (vals fns/sql-aggregate->datalog))
                                                       '#{datahike.pg.sql/filter-sum-numeric
                                                          datahike.pg.sql/filter-avg-numeric})
                                        ;; A form that is ALREADY a find element is an
                                        ;; aggregate the projection emitted, not a scalar
                                        ;; expression to bind — materialising it turns
                                        ;; `(count ?e)` into the where-clause
                                        ;; `[(count ?e) ?v]`, where Datalog calls
                                        ;; clojure.core/count on an entity id and fails
                                        ;; with "count not supported on this type: Long".
                                        ;; COUNT(*) emits the bare `count` symbol, which
                                        ;; the agg-syms allowlist never covered, so
                                        ;; `ORDER BY <count alias>` hit exactly that.
                                        ;; Checking membership in :find is self-
                                        ;; maintaining; the allowlist had already drifted
                                        ;; twice.
                                        in-find? (contains? (set fe-snap) v)
                                        v (if (and (seq? v)
                                                   (not in-find?)
                                                   (not (contains? agg-syms (first v))))
                                            (ctx/materialize-arg! ctx v)
                                            v)
                                        enum-spec (expr/enum-spec-for-exprs
                                                   ctx [order-source-expr])
                                        v (if (and enum-spec
                                                   (not (seq? v))
                                                   (not (map? v)))
                                            (expr/enum-rank-var! ctx enum-spec v)
                                            v)
                                        ;; An aggregate written out in ORDER BY rather
                                        ;; than referenced by alias. PostgreSQL allows it
                                        ;; whether or not it is projected — `SELECT dept
                                        ;; … GROUP BY dept ORDER BY count(*)` is ordinary
                                        ;; SQL. If the projection already emitted this
                                        ;; aggregate, order by THAT element; otherwise
                                        ;; synthesize it and let the hidden-element pass
                                        ;; below append it to :find and strip it again.
                                        v (if (and (map? v) (:aggregate v)
                                                   (instance? Function expr))
                                            (let [f ^Function expr]
                                              (if-let [idx (match-aggregate-index
                                                            f fe-snap fa-snap)]
                                                (nth fe-snap idx)
                                                (when-let [elem (translate-agg f)]
                                                  ;; :find now carries an aggregate, so
                                                  ;; the passes that key on that must see
                                                  ;; it — otherwise the default-order
                                                  ;; branch adds the entity var to :find
                                                  ;; and breaks the grouping.
                                                  (reset! has-aggregates? true)
                                                  elem)))
                                            v)
                                        ;; Still a marker: an aggregate shape
                                        ;; translate-agg does not synthesize (CORR,
                                        ;; ordered-set aggregates with WITHIN GROUP, …).
                                        _ (when (or (nil? v) (and (map? v) (:aggregate v)))
                                            (throw (ex-info "ORDER BY on aggregate not in SELECT list"
                                                            {:error :feature-not-supported
                                                             :feature "ORDER BY on aggregate not in SELECT list"
                                                             :detail (str "ORDER BY on aggregate not in SELECT list is not supported: " (str expr))})))]
                                    [v (if asc? :asc :desc) nulls]))
                                order-by)))

;; LIMIT / OFFSET / FETCH FIRST
        limit-expr (.getLimit select)
        limit-val (when limit-expr
                    (let [rc (.getRowCount ^Limit limit-expr)]
                      (when (contains-target-list-srf? rc)
                        (throw (errors/pg-error
                                :feature-not-supported
                                {:message "set-returning functions are not allowed in LIMIT"})))
                      (let [value (extract-value rc schema db)]
                        (when-not (params/param-ref? value)
                          (when (and (some? value) (not (integer? value)))
                            (throw (errors/pg-error
                                    :feature-not-supported
                                    {:detail (str "LIMIT expression is not supported: " rc)})))
                          (when (and (integer? value) (neg? value))
                            (throw (errors/pg-error
                                    :invalid-row-count-in-limit-clause
                                    {:message "LIMIT must not be negative"})))
                          value))))
        ;; Fetch.getRowCount returns primitive `long` and unboxes a nullable
        ;; field.  It crashes for the SQL-standard default count and for
        ;; expression counts, so always inspect getExpression instead.
        fetch-expr (.getFetch select)
        fetch-param (when fetch-expr
                      (.getFetchParam
                       ^net.sf.jsqlparser.statement.select.Fetch fetch-expr))
        fetch-with-ties? (and fetch-param
                              (str/includes? (str/upper-case (str fetch-param))
                                             "WITH TIES"))
        fetch-count-expr (when fetch-expr
                           (.getExpression
                            ^net.sf.jsqlparser.statement.select.Fetch fetch-expr))
        _ (when (contains-target-list-srf? fetch-count-expr)
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "set-returning functions are not allowed in LIMIT"})))
        fetch-count (when fetch-expr
                      (cond
                        ;; An omitted count defaults to one.
                        (nil? fetch-count-expr) 1
                        ;; PostgreSQL rejects a bare NULL for WITH TIES, but
                        ;; deliberately permits a computed NULL such as
                        ;; (NULL + 1), which behaves as no limit.
                        (instance? NullValue fetch-count-expr)
                        (when fetch-with-ties?
                          (throw
                           (errors/pg-error
                            :invalid-row-count-in-limit-clause
                            {:message "row count cannot be null in FETCH FIRST ... WITH TIES clause"})))
                        :else (extract-value fetch-count-expr schema db)))
        _ (when (and (some? fetch-count)
                     (not (integer? fetch-count)))
            (throw
             (errors/pg-error
              :feature-not-supported
              {:detail (str "FETCH FIRST row count expression is not supported: "
                            fetch-count-expr)})))
        _ (when (and (integer? fetch-count) (neg? fetch-count))
            (throw
             (errors/pg-error
              :invalid-row-count-in-limit-clause
              {:message "FETCH FIRST row count must not be negative"})))
        limit-val (if fetch-expr fetch-count limit-val)
        _ (when (and fetch-with-ties? (empty? order-by))
            (throw
             (errors/pg-error
              :syntax-error
              {:message "WITH TIES cannot be specified without ORDER BY clause"})))
        offset-expr (.getOffset select)
        offset-val (when offset-expr
                     (let [ofs (.getOffset ^Offset offset-expr)]
                       (when (contains-target-list-srf? ofs)
                         (throw (errors/pg-error
                                 :feature-not-supported
                                 {:message "set-returning functions are not allowed in OFFSET"})))
                       (let [value (extract-value ofs schema db)]
                         (when-not (params/param-ref? value)
                           (when (and (some? value) (not (integer? value)))
                             (throw (errors/pg-error
                                     :feature-not-supported
                                     {:detail (str "OFFSET expression is not supported: " ofs)})))
                           (when (and (integer? value) (neg? value))
                             (throw (errors/pg-error
                                     :invalid-row-count-in-result-offset-clause
                                     {:message "OFFSET must not be negative"})))
                           value))))

        ;; FOR UPDATE / FOR NO KEY UPDATE / FOR SHARE / FOR KEY SHARE
        ;; + optional NOWAIT / SKIP LOCKED / (default: block)
        for-mode-obj (.getForMode select)
        for-mode (when for-mode-obj
                   (let [s (.name ^net.sf.jsqlparser.statement.select.ForMode for-mode-obj)]
                     (case s
                       "UPDATE"         :update
                       "NO_KEY_UPDATE"  :no-key-update
                       "SHARE"          :share
                       "KEY_SHARE"      :key-share
                       nil)))
        for-update (when for-mode
                     {:mode for-mode
                      :wait (cond (.isSkipLocked select) :skip
                                  (.isNoWait select)     :nowait
                                  :else                  :block)
                      :table (or alias name)})
        _ (when (and fetch-with-ties?
                     (= :skip (:wait for-update)))
            (throw
             (errors/pg-error
              :syntax-error
              {:message "SKIP LOCKED and WITH TIES options cannot be used together"})))

        ;; LEFT JOIN post-processing: wrap right-table patterns in or-join
        ;; (RIGHT JOINs are rewritten to LEFT JOINs at AST level before reaching here)
        _ (when (some #(= :left (:join-type %)) join-infos)
            (doseq [{:keys [join-type ref-info alias]} join-infos
                    :when (and (= :left join-type) ref-info)]
              (cond
                ;; NESTED LOOP: an ON clause with no equality between the
                ;; relations. Every right row is considered for every
                ;; left row and the conditions filter -- PostgreSQL's
                ;; `ON (b.y > a.y)`, `ON (b.v IS NOT NULL)`, `ON true`.
                ;; The shape is the equi-join's with the ROW MARKER in
                ;; place of the key pattern: it enumerates the right
                ;; relation instead of seeking into it.
                (:nested-loop? ref-info)
                (let [{:keys [right-alias matched-only-preds left-evar]} ref-info
                      right-evar (ctx/entity-var! ctx alias)
                      all-clauses @(:where-clauses ctx)
                      right-table (get (:table-aliases ctx) right-alias right-alias)
                      right-marker (pgs/row-marker-attr right-table)
                      right-side? (fn [c]
                                    (or (and (vector? c) (= 3 (count c))
                                             (= right-evar (first c))
                                             (keyword? (second c)))
                                        (and (vector? c) (= 2 (count c))
                                             (seq? (first c))
                                             (= 'get-else (first (first c)))
                                             (= right-evar (nth (vec (first c)) 2 nil)))))
                      right-clauses (vec (filter right-side? all-clauses))
                      left-clauses (vec (remove right-side? all-clauses))
                      ;; Read every right column as get-else: a row
                      ;; missing one is still a row of the relation.
                      right-reads (mapv (fn [c]
                                          (if (and (vector? c) (= 3 (count c))
                                                   (not= right-marker (second c)))
                                            [(list 'get-else '$ right-evar (second c) :__null__)
                                             (nth c 2)]
                                            c))
                                        (remove #(and (vector? %) (= 3 (count %))
                                                      (= right-marker (second %)))
                                                right-clauses))
                      right-vars (vec (distinct (keep second right-reads)))
                      ;; The LEFT row's anchor, mentioned so the branch
                      ;; is evaluated per left row -- which is what a
                      ;; nested loop is -- and so the negation below has
                      ;; a head variable that appears inside it.
                      anchor-bind (when left-evar
                                    [(list 'identity left-evar) (gensym "?nl-anchor")])
                      matched-parts (into (cond-> [[right-evar right-marker true]]
                                            anchor-bind (conj anchor-bind))
                                          (concat right-reads matched-only-preds))
                      pred-vars (into #{} (mapcat clause-vars) matched-parts)
                      bound-outside (into #{} (mapcat clause-bound-vars) left-clauses)
                      outer-left-vars (vec (distinct
                                            (filter #(and (bound-outside %)
                                                          (not= % right-evar)
                                                          (not (some #{%} right-vars)))
                                                    pred-vars)))
                      shared-vars (vec (distinct (concat outer-left-vars
                                                         (keep identity [left-evar])
                                                         [right-evar] right-vars)))
                      matched (apply list 'and matched-parts)
                      ;; `ON true` and `ON (b.v IS NOT NULL)` read no
                      ;; LEFT column, so there is no left variable to
                      ;; negate under -- and a bare `not` cannot be
                      ;; resolved (nothing in it is bound) while an empty
                      ;; `not-join` head is not a clause. Negate under
                      ;; the LEFT row's own anchor: the question "has the
                      ;; right relation no row satisfying this?" then has
                      ;; a binding to hang on, and answers the same for
                      ;; every left row, which is what a condition
                      ;; independent of the left row means.
                      neg-head (vec (distinct (concat outer-left-vars
                                                      (keep identity [left-evar]))))
                      no-match (list* 'not-join neg-head matched-parts)
                      unmatched (apply list 'and
                                       (into [no-match]
                                             (mapv (fn [v] [(list 'ground :__null__) v])
                                                   (cons right-evar right-vars))))
                      oj-clause (list* 'or-join shared-vars matched unmatched nil)]
                  (swap! (:with-vars ctx) conj right-evar)
                  (reset! (:where-clauses ctx)
                          (if *unmatched-rows-only*
                            (into left-clauses (rest unmatched))
                            (conj left-clauses oj-clause))))

                (:value-join? ref-info)
                ;; VALUE-EQUALITY LEFT JOIN: ON t1.a = t2.x
                ;; (RIGHT JOINs are rewritten to LEFT at AST level)
                (let [{:keys [value-keys right-alias left-evar
                              matched-only-preds]} ref-info
                      ;; EVERY equality of the ON clause, not just one.
                      left-key-vars (mapv :left-key-var value-keys)
                      right-key-attrs (mapv :right-key-attr value-keys)
                      right-evar (ctx/entity-var! ctx right-alias)
                      ;; Convert each left join key to get-else so NULL-key entities are
                      ;; included. They'll go to the unmatched branch via not-join.
                      _ (doseq [left-key-var left-key-vars]
                          (let [clauses @(:where-clauses ctx)
                                key-pattern (first (filter (fn [c]
                                                             (and (vector? c) (= 3 (count c))
                                                                  (= left-key-var (nth c 2))
                                                                  (keyword? (second c))))
                                                           clauses))]
                            (when key-pattern
                              (let [[evar attr _] key-pattern]
                                (reset! (:where-clauses ctx)
                                        (mapv (fn [c]
                                                (if (= c key-pattern)
                                                  [(list 'get-else '$ evar attr :__null__) left-key-var]
                                                  c))
                                              @(:where-clauses ctx)))))))
                      all-clauses @(:where-clauses ctx)
                      ;; Right-side clauses: data patterns on the right entity var
                      right-clause? (fn [clause]
                                      (and (vector? clause) (= 3 (count clause))
                                           (= right-evar (first clause))
                                           (keyword? (second clause))))
                      ;; ... and the get-else form a nullable column read
                      ;; takes: `[(get-else $ ?fu_eid :fu/v :__null__) ?v]`.
                      ;; Only data patterns were recognised, so a column
                      ;; read by a WHERE over the nullable side stayed
                      ;; OUTSIDE the or-join, reading the right entity var
                      ;; that the unmatched branch grounds to `:__null__` --
                      ;; `Bad format for entity-id in pattern`, as XX000,
                      ;; for `LEFT JOIN fu ON (ft.id = fu.id) WHERE fu.v IS
                      ;; NOT NULL`. The READ belongs inside the join; the
                      ;; predicate stays outside, where it filters the
                      ;; null-extended rows as PostgreSQL does.
                      right-get-else? (fn [clause]
                                        (and (vector? clause) (= 2 (count clause))
                                             (seq? (first clause))
                                             (= 'get-else (first (first clause)))
                                             (= right-evar (nth (vec (first clause)) 2 nil))))
                      right-side? (fn [c] (or (right-clause? c) (right-get-else? c)))
                      right-clauses (vec (filter right-clause? all-clauses))
                      right-reads (vec (filter right-get-else? all-clauses))
                      left-clauses (vec (remove right-side? all-clauses))
                      ;; Also remove any right-side marker/get-else from left clauses
                      right-table (get (:table-aliases ctx) right-alias right-alias)
                      right-marker (pgs/row-marker-attr right-table)
                      left-clauses (vec (remove
                                         (fn [c]
                                           (and (vector? c) (>= (count c) 3)
                                                (= right-evar (first c))
                                                (= right-marker (second c))))
                                         left-clauses))
                      ;; Separate right-side clauses into key and non-key
                      key-attr? (set right-key-attrs)
                      right-key-clauses (vec (filter #(key-attr? (second %)) right-clauses))
                      right-non-key (vec (remove #(or (key-attr? (second %))
                                                      (= right-marker (second %)))
                                                 right-clauses))
                      ;; Right-side value variables (from non-key patterns
                      ;; and from the column reads that moved inside)
                      right-val-vars (vec (distinct
                                           (concat (keep (fn [[_ _ v]] v) right-non-key)
                                                   (keep second right-reads))))
                      ;; The right-side key vars a SELECT asked for, by attr
                      key-var-by-attr (into {} (map (fn [[_ a v]] [a v])) right-key-clauses)
                      left-key? (set left-key-vars)
                      all-right-vars (vec (distinct
                                           (concat (remove left-key? (vals key-var-by-attr))
                                                   right-val-vars)))
                      ;; Branch 1: matched — a right entity matching EVERY
                      ;; equality of the ON clause. One data pattern per
                      ;; equality, all on the same right entity var.
                      ;; Use get-else for non-key right columns (they may be NULL)
                      matched-keys (mapv (fn [{:keys [left-key-var right-key-attr]}]
                                           [right-evar right-key-attr left-key-var])
                                         value-keys)
                      matched-non-key (into (mapv (fn [[_e a v]]
                                                    [(list 'get-else '$ right-evar a :__null__) v])
                                                  right-non-key)
                                            ;; already in get-else form
                                            right-reads)
                      ;; If a right key var was in SELECT and differs from its
                      ;; left key, bind it
                      matched-key-binds (vec (keep (fn [{:keys [left-key-var right-key-attr]}]
                                                     (let [rv (key-var-by-attr right-key-attr)]
                                                       (when (and rv (not= rv left-key-var))
                                                         [(list 'identity left-key-var) rv])))
                                                   value-keys))
                      ;; Right-side filter predicates from the ON clause
                      ;; (e.g. `… AND d.objsubid = 0`) — applied inside
                      ;; the matched branch only. Without this they'd
                      ;; act as global filters and convert the LEFT JOIN
                      ;; into an INNER JOIN.
                      matched-parts (into (vec matched-keys)
                                          (concat matched-non-key
                                                  matched-key-binds
                                                  matched-only-preds))
                      matched (apply list 'and matched-parts)
                      ;; A condition may read a LEFT column that is not a
                      ;; join key (`ON b.x = a.x AND b.y > a.y`). Such a
                      ;; var has to cross into the or-join, or it is a
                      ;; fresh unbound var inside the branch and the whole
                      ;; join answers nothing.
                      ;; Bound OUTSIDE means bound by a left clause, not
                      ;; merely mentioned by one: a projection over a right
                      ;; column (`COALESCE(y.y2, -1)`) reads a right var in
                      ;; the outer query, and taking that as an outer
                      ;; binding put the right var in the not-join's head,
                      ;; where it made the unmatched branch fire alongside
                      ;; the matched one -- two rows per matched left row.
                      bound-outside (into #{} (mapcat clause-bound-vars) left-clauses)
                      matched-bound (into #{right-evar}
                                          (mapcat clause-bound-vars)
                                          matched-parts)
                      right-var? (set all-right-vars)
                      matched-vars (into #{} (mapcat clause-vars) matched-parts)
                      outer-left-vars (vec (distinct
                                            (concat left-key-vars
                                                    (filter #(and (bound-outside %)
                                                                  (not (left-key? %))
                                                                  (not (matched-bound %))
                                                                  (not (right-var? %)))
                                                            matched-vars))))
                      ;; Shared vars for or-join: every left var a condition
                      ;; reads + all right-side vars + right entity var
                      shared-vars (vec (distinct (concat outer-left-vars
                                                         [right-evar]
                                                         all-right-vars)))
                      ;; Branch 2: unmatched — no right entity satisfies the
                      ;; WHOLE condition. Negating the key pattern alone
                      ;; dropped a left row whose key matched while another
                      ;; condition did not, instead of null-extending it.
                      null-bindings (into [[(list 'ground :__null__) right-evar]]
                                          (mapv (fn [v] [(list 'ground :__null__) v])
                                                all-right-vars))
                      unmatched (apply list 'and
                                       (into [(list* 'not-join outer-left-vars matched-parts)]
                                             null-bindings))
                      oj-clause (list* 'or-join shared-vars matched unmatched nil)]
                  ;; Add right entity var to :with for dedup prevention
                  (swap! (:with-vars ctx) conj right-evar)
                  (reset! (:where-clauses ctx)
                          (if *unmatched-rows-only*
                            (into left-clauses (rest unmatched))
                            (conj left-clauses oj-clause))))

                :else
                ;; REF-BASED LEFT JOIN: ON p.dept = d.db_id
                ;;
                ;; LEFT iteration semantics: surface every LEFT row,
                ;; even those without a matching RIGHT row, with the
                ;; right side bound to :__null__. This requires:
                ;;   - the LEFT entity-var (?d_eid) drives the outer
                ;;     iteration (anchored elsewhere in the query —
                ;;     usually the default-table anchor pass);
                ;;   - the original ref data pattern is REMOVED from
                ;;     the outer where (so it doesn't filter LEFT
                ;;     rows) and goes into the matched branch;
                ;;   - the unmatched branch uses `not-join` to assert
                ;;     "no right row points at this LEFT" and grounds
                ;;     the right-side vars to :__null__.
                ;;
                ;; The translate-join code conditionally skips the
                ;; entity-var swap for LEFT joins (see line ~309), so
                ;; the LEFT alias's entity-var (e.g. ?t_eid) is still
                ;; intact here. We pull it from ref-info's
                ;; :left-table-evar field.
                (if (:owner-on-left? ref-info)
                  ;; THE OTHER DIRECTION: the ref's owner is the table we
                  ;; are joining FROM, and its value IS the joined
                  ;; entity -- `FROM person p LEFT JOIN company c ON
                  ;; p.company = c.db_id`. One pattern says the whole
                  ;; join, `[?p_eid :person/company ?c_eid]`; matched
                  ;; reads the right side through ?c_eid, unmatched says
                  ;; this left row has no such company and nulls it.
                  ;;
                  ;; The branch below assumed the owner was always the
                  ;; JOINED side, so it took the joined alias for the
                  ;; owner and emitted `[?c_eid :person/company ?c_eid]`
                  ;; -- entity and value one variable -- and the join
                  ;; answered NOTHING, for every row.
                  (let [{:keys [ref-var ref-attr owner-evar matched-only-preds]} ref-info
                        right-evar (ctx/entity-var! ctx alias)
                        all-clauses @(:where-clauses ctx)
                        ref-binding? (fn [c]
                                       (and (vector? c) (= 3 (count c))
                                            (= ref-attr (second c))
                                            (= ref-var (nth c 2))))
                        right-side? (fn [c]
                                      (or (and (vector? c) (= 3 (count c))
                                               (= right-evar (first c))
                                               (keyword? (second c)))
                                          (and (vector? c) (= 2 (count c))
                                               (seq? (first c))
                                               (= 'get-else (first (first c)))
                                               (= right-evar (nth (vec (first c)) 2 nil)))))
                        right-clauses (vec (filter right-side? all-clauses))
                        left-clauses (vec (remove (fn [c] (or (ref-binding? c) (right-side? c)))
                                                  all-clauses))
                        right-marker (pgs/row-marker-attr
                                      (get (:table-aliases ctx) alias alias))
                        right-clauses (vec (remove #(and (vector? %) (= 3 (count %))
                                                         (= right-marker (second %)))
                                                   right-clauses))
                        ;; A right-side column may be missing on the
                        ;; matched entity; read it as get-else so the row
                        ;; still matches, with NULL for that column.
                        right-reads (mapv (fn [c]
                                            (if (and (vector? c) (= 3 (count c)))
                                              [(list 'get-else '$ right-evar (second c) :__null__)
                                               (nth c 2)]
                                              c))
                                          right-clauses)
                        right-vars (vec (distinct (keep second right-reads)))
                        ;; `p.company` itself is a LEFT column: its value
                        ;; is the person's, matched or not, so it stays
                        ;; bound outside the join. Binding it inside and
                        ;; grounding it to NULL in the unmatched branch
                        ;; made a left row whose ref exists but fails
                        ;; another condition fit NEITHER branch -- it
                        ;; disappeared instead of being null-extended.
                        matched-parts (into [[owner-evar ref-attr right-evar]]
                                            (concat right-reads matched-only-preds))
                        pred-vars (into #{} (mapcat clause-vars) matched-only-preds)
                        bound-outside (into #{} (mapcat clause-bound-vars) left-clauses)
                        outer-left-vars (vec (distinct
                                              (cons owner-evar
                                                    (filter #(and (bound-outside %)
                                                                  (not= % owner-evar)
                                                                  (not= % right-evar)
                                                                  (not= % ref-var)
                                                                  (not (some #{%} right-vars)))
                                                            pred-vars))))
                        shared-vars (vec (distinct (concat outer-left-vars
                                                           [right-evar]
                                                           right-vars)))
                        matched (apply list 'and matched-parts)
                        unmatched (apply list 'and
                                         (into [(list* 'not-join outer-left-vars matched-parts)]
                                               (mapv (fn [v] [(list 'ground :__null__) v])
                                                     (cons right-evar right-vars))))
                        oj-clause (list* 'or-join shared-vars matched unmatched nil)]
                    (swap! (:with-vars ctx) conj right-evar)
                    (reset! (:where-clauses ctx)
                            (if *unmatched-rows-only*
                              (into left-clauses (rest unmatched))
                              (conj left-clauses oj-clause))))

                  (let [{:keys [ref-var ref-attr left-table-evar matched-only-preds]} ref-info
                        owner-evar (ctx/entity-var! ctx alias)
                        all-clauses @(:where-clauses ctx)
                      ;; The original ref pattern from the ON clause:
                      ;; `[?p_eid :posting/transaction ?ref-var]`. We
                      ;; strip it from the outer where (so it doesn't
                      ;; force iteration over postings) and re-emit
                      ;; into the matched branch.
                        ref-binding? (fn [clause]
                                       (and (vector? clause) (= 3 (count clause))
                                            (= ref-attr (second clause))
                                            (= ref-var (nth clause 2))))
                      ;; Right-side data patterns on ref-var (used to
                      ;; project right-side columns via the ref's
                      ;; deref'd identity).
                        right-clause? (fn [clause]
                                        (and (vector? clause) (= 3 (count clause))
                                             (= ref-var (first clause))
                                             (keyword? (second clause))))
                        right-clauses (vec (filter right-clause? all-clauses))
                      ;; Outer (LEFT-driving) clauses: drop the ref-binding
                      ;; (moves into matched) and any right-clauses (those
                      ;; only make sense when matched).
                        left-clauses (vec (remove (fn [c]
                                                    (or (ref-binding? c)
                                                        (right-clause? c)))
                                                  all-clauses))
                      ;; Vars introduced by right-side patterns; needed in
                      ;; shared-vars and as :__null__ bindings in unmatched.
                        right-vars (vec (distinct
                                         (keep (fn [clause]
                                                 (when (and (vector? clause) (= 3 (count clause)))
                                                   (nth clause 2)))
                                               right-clauses)))
                      ;; If the LEFT alias was swapped in translate-join
                      ;; (legacy / non-LEFT path that leaked here), we
                      ;; might not have left-table-evar. Fall back to
                      ;; the post-swap value, which works for
                      ;; LEFT-without-empty-rows but loses null-side
                      ;; semantics. The translate-join change above
                      ;; keeps left-table-evar populated for LEFT.
                        left-evar (or left-table-evar
                                      (when (and ref-var (not= ref-var owner-evar))
                                        ref-var))
                      ;; ?owner-evar (the right-side entity-var) needs
                      ;; binding in both branches: matched via the ref
                      ;; data pattern; unmatched via ground :__null__.
                        include-owner? (and owner-evar (not (some #(= owner-evar %) right-vars)))
                      ;; A second ON condition (`… AND c.name = 'Acme'`)
                      ;; was read by nobody here: the branch took the ref
                      ;; pattern and stopped, so the whole join answered
                      ;; NOTHING. It belongs in the matched branch, like
                      ;; the value-join path, and any LEFT var it reads
                      ;; has to cross into the or-join with it.
                        pred-vars (into #{} (mapcat clause-vars) matched-only-preds)
                        outer-left-vars (vec (distinct
                                              (concat
                                               (when left-evar [left-evar])
                                               (filter (fn [v]
                                                         (and (not= v ref-var)
                                                              (not= v owner-evar)
                                                              (not (some #{v} right-vars))
                                                              (some #(contains? (clause-bound-vars %) v)
                                                                    left-clauses)))
                                                       pred-vars))))
                        shared-vars (vec (distinct
                                          (concat
                                           outer-left-vars
                                           [ref-var]
                                           (when include-owner? [owner-evar])
                                           right-vars)))
                      ;; Matched branch:
                      ;;   - the ref data pattern `[?p_eid ref-attr ?t_eid]`
                      ;;     binds owner-evar (?p_eid) and unifies its
                      ;;     value with the LEFT entity-var (?t_eid).
                      ;;   - we ALSO bind ref-var (the original ref's
                      ;;     value var) to left-evar via identity, so
                      ;;     downstream clauses that reference ref-var
                      ;;     (right-clauses, shared-vars exposure) keep
                      ;;     working. `[(= a b)]` is a predicate (a&b
                      ;;     must be bound); `[(identity left-evar)
                      ;;     ref-var]` is a function-binding (binds
                      ;;     ref-var to left-evar's value).
                        matched-ref-bind (cond
                                           (and include-owner? left-evar)
                                           [[owner-evar ref-attr left-evar]
                                            [(list 'identity left-evar) ref-var]]
                                           include-owner?
                                           [[owner-evar ref-attr ref-var]]
                                           left-evar
                                           [[(list 'identity left-evar) ref-var]]
                                           :else [])
                        matched-parts (vec (concat matched-ref-bind right-clauses
                                                   matched-only-preds))
                        matched (apply list 'and matched-parts)
                      ;; Unmatched branch: assert no right row points at
                      ;; this LEFT, and ground all right-side + owner vars
                      ;; to :__null__. Without the LEFT entity-var we
                      ;; can't express "no right matches THIS row", so
                      ;; degrade to ref-var = :__null__ (legacy behavior).
                        null-bindings (mapv (fn [v] [(list 'ground :__null__) v])
                                            (cond-> (vec right-vars)
                                              include-owner? (conj owner-evar)
                                              true           (conj ref-var)))
                        not-match-guard
                        (if left-evar
                        ;; "no posting points at this transaction" -- and,
                        ;; when the ON clause says more, none that also
                        ;; satisfies the rest of it. Negating the ref
                        ;; pattern alone would drop a LEFT row whose ref
                        ;; matched but whose predicate did not, instead of
                        ;; null-extending it. Only the right-side patterns
                        ;; a predicate READS come along: the others are
                        ;; display columns, and a row missing one of those
                        ;; is still a match.
                          (if (seq matched-only-preds)
                            [(list* 'not-join outer-left-vars
                                    (concat matched-ref-bind
                                            (filter (fn [c]
                                                      (some #(contains? pred-vars %)
                                                            (clause-vars c)))
                                                    right-clauses)
                                            matched-only-preds))]
                            (let [inner-eid (gensym "?lj-inner-")]
                              [(list 'not-join [left-evar]
                                     [inner-eid ref-attr left-evar])]))
                        ;; legacy: ref-var = :__null__
                          [[(list '= ref-var :__null__)]])
                        unmatched (apply list 'and
                                         (concat not-match-guard null-bindings))
                        oj-clause (list* 'or-join shared-vars matched unmatched nil)]
                    (reset! (:where-clauses ctx)
                            (if *unmatched-rows-only*
                              (into left-clauses (rest unmatched))
                              (conj left-clauses oj-clause))))))))

        nullable-order-vars (atom #{})

        ;; NULL synthesis: convert display-only column patterns to get-else.
        ;; This makes entities with missing attributes show NULL instead of
        ;; being excluded from results. The planner handles get-else efficiently
        ;; via LOptionalScan (fused into the merge loop, no function call overhead).
        ;;
        ;; Rules:
        ;; - ANCHOR: at least one plain pattern per entity (establishes entity set)
        ;; - REQUIRED: columns in predicates/comparisons or inside aggregates → plain
        ;; - OPTIONAL: display-only SELECT columns → convert to get-else
        _ (let [all-clauses @(:where-clauses ctx)
                find-elems-snapshot @find-elements
                ;; Vars inside aggregate forms in :find are "aggregated" — keep plain
                agg-vars (set (mapcat (fn [elem]
                                        (when (and (seq? elem) (symbol? (first elem)))
                                          ;; (sum ?v) → ?v is aggregated
                                          (filter #(and (symbol? %)
                                                        (str/starts-with? (str %) "?"))
                                                  (rest elem))))
                                      find-elems-snapshot))
                ;; Vars referenced in predicate/filter clauses.
                ;; Predicates: bare lists like (= ?a ?b), (> ?x 5), (re-find ...)
                ;; Function bindings: [(fn ?input) ?output] — these produce display
                ;; values, not filters. Only include vars from actual predicates,
                ;; not from function bindings that just compute display columns.
                pred-vars (set (mapcat (fn [clause]
                                         (cond
                                           ;; Bare predicate list: (= ?a ?b), (> ?x 5)
                                           (seq? clause)
                                           (filter #(and (symbol? %)
                                                         (str/starts-with? (str %) "?"))
                                                   (flatten clause))
                                           ;; Wrapped predicate in vector: [(= ?a ?b)]
                                           ;; vs function binding: [(fn ?in) ?out]
                                           ;; Distinguish by arity: predicates have 1 elem, bindings 2
                                           (and (vector? clause) (= 1 (count clause)) (seq? (first clause)))
                                           (filter #(and (symbol? %)
                                                         (str/starts-with? (str %) "?"))
                                                   (flatten clause))
                                           :else nil))
                                       all-clauses))
                ;; Required vars: in aggregates, in predicates, or entity vars
                entity-var-set (set (vals @(:entity-vars ctx)))
                ;; ORDER BY vars are required (must be plain patterns for Datahike sorting)
                order-vars (set (keep (fn [[v _dir]] (when (symbol? v) v))
                                      (or order-by-spec [])))
                required-vars (-> agg-vars
                                  (into pred-vars)
                                  (into entity-var-set)
                                  (into order-vars))
                ;; Add row-marker anchors for entities that have the marker in schema.
                ;; This ensures all SQL-inserted entities (including all-NULL rows) are found.
                ;; Entity vars on the optional side of LEFT JOINs should NOT get
                ;; marker anchors outside the or-join (they're handled inside it)
                optional-aliases (set (keep (fn [{:keys [join-type alias]}]
                                              (when (= :left join-type) alias))
                                            join-infos))
                entity-has-anchor (atom #{})
                _ (doseq [[alias-key evar] @(:entity-vars ctx)]
                    (when-not (or (contains? optional-aliases alias-key)
                                  (contains? (:computed-aliases ctx) alias-key))
                      (let [tname (get (:table-aliases ctx) alias-key alias-key)
                            marker (pgs/row-marker-attr tname)]
                        (when (get schema marker)
                          (let [already? (some (fn [c]
                                                 (and (vector? c) (>= (count c) 3)
                                                      (= evar (first c))
                                                      (= marker (second c))))
                                               @(:where-clauses ctx))]
                            (when-not already?
                              (swap! (:where-clauses ctx) conj [evar marker true]))
                            (swap! entity-has-anchor conj evar))))))
                ;; Re-read after possible marker additions
                all-clauses @(:where-clauses ctx)
                ;; Build function-binding dependency: input-var → output-var
                ;; For [(fn ?input "key") ?output], record ?input → ?output
                ;; This lets us trace jsonb chains: [?e :t/col ?v] → [(op ?v "k") ?r] → ?r in :find
                fn-output-of (into {}
                                   (keep (fn [clause]
                                           (when (and (vector? clause) (= 2 (count clause))
                                                      (seq? (first clause)))
                                             (let [fn-call (first clause)
                                                   out-var (second clause)
                                                   in-vars (filter #(and (symbol? %)
                                                                         (str/starts-with? (str %) "?"))
                                                                   (rest fn-call))]
                                               (when (and (symbol? out-var) (seq in-vars))
                                                 [(first in-vars) out-var]))))
                                         all-clauses))
                ;; Trace transitively: a var "reaches :find" if it's directly in :find
                ;; or it feeds into a function whose output reaches :find
                find-set (set find-elems-snapshot)
                reaches-find? (fn reaches-find? [v seen]
                                (or (contains? find-set v)
                                    (when-let [next (fn-output-of v)]
                                      (when-not (contains? seen next)
                                        (reaches-find? next (conj seen v))))))
                ;; Find data patterns that are candidates for get-else conversion
                optional-patterns (atom [])
                required-join-patterns @(:required-join-patterns ctx)
                _ (doseq [clause all-clauses]
                    (when (and (vector? clause)
                               (= 3 (count clause))
                               (symbol? (first clause))
                               (keyword? (second clause))
                               (symbol? (nth clause 2)))
                      (let [evar (first clause)
                            vvar (nth clause 2)]
                        (if (or (contains? required-vars vvar)
                                ;; Equi-join unification patterns ARE the
                                ;; join — get-else-ing them would make the
                                ;; :__null__ sentinel joinable.
                                (contains? required-join-patterns clause)
                                (not (reaches-find? vvar #{})))
                          ;; Required or doesn't reach :find → stays as anchor
                          (swap! entity-has-anchor conj evar)
                          ;; Candidate for get-else
                          (swap! optional-patterns conj clause)))))
                ;; Ensure every entity has at least one anchor before converting
                ;; If an entity only has optional patterns, the first one stays plain
                _ (doseq [[evar patterns] (clojure.core/group-by first @optional-patterns)]
                    (when-not (contains? @entity-has-anchor evar)
                      ;; Promote first optional to anchor
                      (swap! entity-has-anchor conj evar)
                      (swap! optional-patterns
                             (fn [ps] (vec (remove #(= % (first patterns)) ps))))))
                ;; Rewrite: replace optional data patterns with get-else
                optional-set (set @optional-patterns)
                ;; Track which variables became optional (for server-side NULL sorting)
                _ (when (seq optional-set)
                    (reset! nullable-order-vars (set (map #(nth % 2) optional-set))))]
            (when (seq optional-set)
              (let [new-clauses (vec (mapcat
                                      (fn [clause]
                                        (if (contains? optional-set clause)
                                          (let [[evar attr vvar] clause]
                                            [[(list 'get-else '$ evar attr :__null__) vvar]])
                                          [clause]))
                                      all-clauses))]
                (reset! (:where-clauses ctx) new-clauses))))

        ;; ORDER BY on optional columns: any ORDER BY variable still bound
        ;; by a required data pattern needs to become optional (get-else) so
        ;; rows with missing attributes still appear. Also register the var
        ;; in nullable-order-vars so server-side NULL-aware sorting kicks in.
        _ (when (seq order-by-spec)
            (let [ob-vars (keep (fn [pair]
                                  (let [v (first pair)]
                                    (when (and (symbol? v)
                                               (not (clojure.string/starts-with?
                                                     (clojure.core/name v) "?_")))
                                      v)))
                                order-by-spec)
                  current-clauses @(:where-clauses ctx)
                  required-join-patterns @(:required-join-patterns ctx)
                  to-convert (keep (fn [v]
                                     (first
                                      (filter (fn [c]
                                                (and (vector? c) (= 3 (count c))
                                                     (symbol? (first c))
                                                     (keyword? (second c))
                                                     (= v (nth c 2))
                                                     (not (contains? required-join-patterns c))
                                                     (not= "db-row-exists"
                                                           (clojure.core/name (second c)))
                                                     (not= "id"
                                                           (clojure.core/name (second c)))))
                                              current-clauses)))
                                   ob-vars)]
              (when (seq to-convert)
                (reset! (:where-clauses ctx)
                        (mapv (fn [c]
                                (let [match (some #(when (= c %) %) to-convert)]
                                  (if match
                                    [(list 'get-else '$ (first c) (second c) :__null__)
                                     (nth c 2)]
                                    c)))
                              current-clauses))
                (swap! nullable-order-vars into (map #(nth % 2) to-convert)))))

        ;; Clause ordering hint for the legacy query engine.
        ;; The legacy engine's resolve-clauses does iterative resolution but
        ;; struggles when function bindings (get-else) appear before their
        ;; entity var is bound by a data pattern. The new query planner
        ;; handles this automatically via dependency-aware scheduling.
        ;; This reordering is a no-op for the planner (it reorders anyway)
        ;; but prevents "Cannot resolve" errors in legacy mode.
        _ (let [clauses @(:where-clauses ctx)
                data-patterns (filterv (fn [c]
                                         (and (vector? c) (keyword? (second c))))
                                       clauses)
                other-clauses (filterv (fn [c]
                                         (not (and (vector? c) (keyword? (second c)))))
                                       clauses)]
            (when (and (seq data-patterns) (seq other-clauses))
              (reset! (:where-clauses ctx) (into data-patterns other-clauses))))

        ;; HAVING is a predicate OVER aggregates, so it is translated
        ;; exactly the way an expression over aggregates in the select
        ;; list is: hoist each aggregate into a hidden `:find` element and
        ;; keep a FORM over the variables bound to them, evaluated per
        ;; group after the query.
        ;;
        ;; It used to be a SHAPE MATCHER -- `{:op :col-idx :value}`, built
        ;; by an AST walker that knew AND/OR, the six comparisons and IS
        ;; NULL, and required an aggregate on the left with a literal on
        ;; the right. Everything else was silently DROPPED rather than
        ;; refused, so `HAVING sum(n) + 1 > 11` returned EVERY group. The
        ;; form carries whatever the predicate translator produces --
        ;; arithmetic on either side, NOT, BETWEEN, IN, a CASE -- with the
        ;; same three-valued logic WHERE uses.
        ;;
        ;; find-aliases tracks the VISIBLE projection, so the alias
        ;; `emit-agg!` appends is dropped again and the column rides on
        ;; `:hidden-count`, which is what strips it at the wire layer.
        [having-spec having-hidden]
        (if having-expr
          (let [sink (atom [])
                before (count @(:where-clauses ctx))
                form (expr/translate-predicate-expr
                      (assoc ctx
                             :hoisted-aggs sink
                             :aggregate-projection?
                             (boolean (some fns/aggregate-function?
                                            (params/ast-function-names having-expr))))
                      having-expr)
                all (vec @(:where-clauses ctx))
                [keep-cs pform] (expr/split-aggregate-projection
                                 form (subvec all before) (map :var @sink))
                _ (reset! (:where-clauses ctx) (into (subvec all 0 before) keep-cs))
                n-hidden (atom 0)
                slots (mapv
                       (fn [{:keys [var fn-node]}]
                         (let [idx (emit-agg! fn-node nil)
                               elem (nth @find-elements idx)
                               ;; The same aggregate may already be
                               ;; projected -- `SELECT sum(n) … HAVING
                               ;; sum(n) > 1` -- in which case read that
                               ;; column instead of computing it twice.
                               prior (first (keep-indexed
                                             (fn [i e] (when (and (< i idx) (= e elem)) i))
                                             @find-elements))]
                           (swap! find-aliases pop)
                           (if prior
                             (do (swap! find-elements pop) [var prior])
                             (do (swap! n-hidden inc) [var idx]))))
                       @sink)]
            [{:form pform :slots slots} @n-hidden])
          [nil 0])

        ;; HAVING creates one implicit group even when neither it nor the
        ;; projection contains an aggregate. Without this, the ordinary
        ;; no-ORDER-BY path appended the entity id to :find and
        ;; `SELECT 1 FROM t HAVING true` returned one row per entity.
        ;;
        ;; If both the projection and HAVING are constant, the implicit
        ;; group also exists for an empty input and no source value can
        ;; affect it. PostgreSQL consequently does not evaluate a dead
        ;; `WHERE 1/a` in this shape. Mark it so the where-clause snapshot
        ;; below can retain only bindings needed by the constant projection.
        source-var->col
        (persistent!
         (reduce (fn [m [k v]]
                   (if (and (vector? k) (keyword? (second k)))
                     (assoc! m v k)
                     m))
                 (transient {}) @(:col->var ctx)))
        visible-find (take (count @find-aliases) @find-elements)
        having-form-vars (if having-spec
                           (ctx/collect-vars (:form having-spec))
                           #{})
        source-vars-in-result
        (into #{}
              (filter #(contains? source-var->col %))
              (concat (filter symbol? visible-find)
                      having-form-vars
                      (keep (fn [[v _dir _nulls]]
                              (when (symbol? v) v))
                            order-by-spec)))
        degenerate-having?
        (and having-expr
             (not @has-aggregates?)
             (empty? source-vars-in-result))
        _ (when having-expr (reset! has-aggregates? true))

        ;; PostgreSQL rejects every source column that is neither aggregated
        ;; nor grouped (42803), including hidden ORDER BY columns and columns
        ;; referenced only by HAVING. This deliberately runs AFTER HAVING
        ;; aggregate hoisting so there is one validation boundary for the
        ;; entire query expression tree.
        _ (when (and (or (seq group-by) @has-aggregates?)
                     (exact-schema-for-grouping? db))
            (let [fe @find-elements
                  fa @find-aliases
                  ordinal-elems
                  (mapv (fn [pos]
                          (when (or (< pos 1) (> pos (count fa)))
                            (throw (ex-info (str "GROUP BY position " pos
                                                 " is not in select list")
                                            {:error :invalid-column-reference
                                             :sqlstate "42P10"})))
                          (let [el (nth fe (dec pos))]
                            (when (seq? el)
                              (throw (ex-info "aggregate functions are not allowed in GROUP BY"
                                              {:error :grouping-error
                                               :sqlstate "42803"})))
                            el))
                        group-by-ordinals)
                  gvars (into (into (set group-by-vars) ordinal-elems)
                              (keep (fn [[a el]]
                                      (when (contains? group-by-alias-names a) el))
                                    (map vector fa fe)))
                  pk-grouped (into #{}
                                   (keep (fn [v]
                                           (when-let [[_ attr] (source-var->col v)]
                                             (when (= :db.unique/identity
                                                      (:db/unique (get schema attr)))
                                               (namespace attr)))))
                                   group-by-vars)]
              (doseq [v source-vars-in-result]
                (when-not (contains? gvars v)
                  (when-let [[alias-key attr] (source-var->col v)]
                    (when-not (contains? pk-grouped (namespace attr))
                      (throw (ex-info (str "column \"" (or alias-key (namespace attr))
                                           "." (clojure.core/name attr)
                                           "\" must appear in the GROUP BY clause "
                                           "or be used in an aggregate function")
                                      {:error :grouping-error
                                       :sqlstate "42803"
                                       :column (clojure.core/name attr)}))))))))

        ;; GROUP BY keys that are not projected must still reach :find,
        ;; because Datalog derives grouping from the non-aggregate :find
        ;; elements — there is no separate grouping clause. Without this
        ;; the GROUP BY was inert whenever its columns were not in the
        ;; SELECT list, so `SELECT count(*) FROM g GROUP BY dept`
        ;; collapsed all five rows into ONE group and answered 5 where
        ;; PostgreSQL answers 2 and 3. `SELECT dept, count(*) … GROUP BY
        ;; dept` only ever worked because projecting the key happened to
        ;; put it in :find.
        ;;
        ;; They ride on :hidden-count like the HAVING-only aggregates
        ;; above: appended at the end of :find, stripped by the wire
        ;; layer, and deliberately not added to find-aliases, which
        ;; tracks the VISIBLE projection.
        group-by-hidden
        (if (seq group-by-vars)
          (let [existing (set @find-elements)]
            (->> group-by-vars
                 (remove #(contains? existing %))
                 distinct
                 (reduce (fn [n v] (swap! find-elements conj v) (inc n)) 0)))
          0)

        ;; Snapshot where-clauses AFTER the HAVING-aggregate translation
        ;; has had a chance to add column bindings via col-var!. If we
        ;; snapshot before, the aggregate's input var (e.g. ?sales_amount)
        ;; references no `:where` clause and Datahike rejects it as
        ;; "Query for unknown vars".
        where-clauses
        (if degenerate-having?
          (let [projection-vars (set (filter symbol? visible-find))]
            (filterv (fn [clause]
                       (some projection-vars (ctx/collect-vars clause)))
                     @(:where-clauses ctx)))
          @(:where-clauses ctx))
        find-elems @find-elements

        find-elems-vec (vec find-elems)
        ;; ORDER BY: check if any order-by variable is nullable (get-else).
        ;; If so, sorting must happen in the server with a null-aware comparator.
        ;; Otherwise, use Datahike's optimized :order-by.
        ;; Vars produced by ctx/col-var! are tracked in ctx's :nullable-vars (always
        ;; get-else-bound); earlier passes also add to nullable-order-vars.
        nullable-vars (into @nullable-order-vars @(:nullable-vars ctx))
        ;; An explicit NULLS ordering that differs from PostgreSQL's default
        ;; for that direction can only be honoured by the server-side
        ;; comparator -- Datahike's :order-by has no way to express it.
        explicit-nulls? (and order-by-spec
                             (some (fn [[_v dir nulls]]
                                     (and nulls
                                          (not= nulls (if (= dir :asc) :last :first))))
                                   order-by-spec))
        effectful-order-vars
        (let [bindings (filterv #(and (vector? %)
                                      (= 2 (count %))
                                      (seq? (first %))
                                      (symbol? (second %)))
                                @(:where-clauses ctx))
              direct (into #{}
                           (keep (fn [clause]
                                   (when (some deferred-projection-op?
                                               (tree-seq coll? seq (first clause)))
                                     (second clause))))
                           bindings)]
          (loop [tainted direct]
            (let [next-tainted
                  (into tainted
                        (keep (fn [clause]
                                (when (some tainted
                                            (filter symbol?
                                                    (tree-seq coll? seq
                                                              (first clause))))
                                  (second clause))))
                        bindings)]
              (if (= tainted next-tainted)
                tainted
                (recur next-tainted)))))
        effectful-order? (some #(contains? effectful-order-vars (first %))
                               order-by-spec)
        has-nullable-order? (and order-by-spec
                                 (or fetch-with-ties?
                                     effectful-order?
                                     explicit-nulls?
                                     ;; DISTINCT ON keeps the FIRST row per
                                     ;; ON-key, so it needs the rows in a
                                     ;; known order HERE, in the same pass
                                     ;; that dedupes them.
                                     (seq distinct-on-items)
                                     (some (fn [[v _dir]]
                                             (and (symbol? v)
                                                  (contains? nullable-vars v)))
                                           order-by-spec)))
        project-out-vars (into #{} (map :out-var) project-set-specs)
        [find-elems-vec hidden-count order-by-flat sql-order-by project-order-by]
        (if order-by-spec
          (let [;; seq? covers an aggregate form contributed by ORDER BY that
                ;; the projection did not emit — it rides on :hidden-count
                ;; exactly like a plain sort key.
                missing (filterv (fn [[v _dir]]
                                   (and (or (symbol? v) (seq? v))
                                        (neg? (.indexOf ^java.util.List find-elems-vec v))))
                                 order-by-spec)
                extended-find (into find-elems-vec (map first missing))
                ;; Datahike's :order-by takes [idx dir] pairs; the server-side
                ;; comparator takes [idx dir nulls] triples so it can honour an
                ;; explicit NULLS FIRST / NULLS LAST.
                ob (vec (mapcat
                         (fn [[v dir]]
                           (let [idx (.indexOf ^java.util.List extended-find v)]
                             (when (>= idx 0) [idx dir])))
                         order-by-spec))
                ob3 (vec (mapcat
                          (fn [[v dir nulls]]
                            (let [idx (.indexOf ^java.util.List extended-find v)]
                              (when (>= idx 0) [idx dir nulls])))
                          order-by-spec))
                hidden (+ project-set-hidden (count missing)
                          having-hidden group-by-hidden)
                project-order? (some project-out-vars (map first order-by-spec))]
            (cond
              project-order?
              ;; An SRF output does not exist until ProjectSet has expanded
              ;; the base rows, so the complete SQL sort belongs above that
              ;; stage. Sorting the placeholder in Datalog is a no-op and can
              ;; produce the wrong order for mixed base/SRF keys.
              [extended-find hidden nil nil ob3]

              has-nullable-order?
              ;; Nullable ORDER BY → server-side sort (don't emit :order-by to Datahike)
              [extended-find hidden nil ob3 nil]

              :else
              ;; Non-nullable → Datahike handles it
              [extended-find hidden ob nil nil]))
          ;; No explicit SQL ORDER BY: default to a deterministic order on
          ;; the primary FROM table's entity var. Entity ids are issued
          ;; monotonically by d/transact, so this matches insertion order —
          ;; the behavior every heap-scanning PG client (pgjdbc, Odoo,
          ;; Hibernate) implicitly relies on when no ORDER BY is given.
          ;; Skipped for:
          ;;   - aggregates / DISTINCT (their shape is projection-defined,
          ;;     not row-defined),
          ;;   - GROUP BY without an aggregate (the user asked for distinct
          ;;     groups; adding the eid var to :find would prevent the
          ;;     dedup since Datahike's set semantics keys on the full
          ;;     :find tuple), and
          ;;   - queries with no single default table (subqueries, joins
          ;;     handled separately).
          (if-let [evar (and (not @has-aggregates?)
                             (not has-distinct?)
                             (not (seq group-by))
                             ;; A unique equality in a simple single-table
                             ;; query yields at most one row. Sorting that row
                             ;; by its hidden entity id changes no observable
                             ;; result and used to dominate prepared point-read
                             ;; latency.
                             (not (and (empty? joins)
                                       @(:unique-value-bound? ctx)))
                             default-table
                             (ctx/entity-var! ctx default-table))]
            (let [already (.indexOf ^java.util.List find-elems-vec evar)
                  extended-find (if (neg? already)
                                  (conj find-elems-vec evar)
                                  find-elems-vec)
                  idx (if (neg? already) (dec (count extended-find)) already)
                  hidden (+ project-set-hidden (if (neg? already) 1 0)
                            having-hidden group-by-hidden)]
              [extended-find hidden [idx :asc] nil nil])
            [find-elems-vec (+ project-set-hidden having-hidden group-by-hidden)
             nil nil nil]))

        in-params @(:in-params ctx)
        in-args @(:in-args ctx)
        ;; ANN is an access path, never the semantic implementation.  Expose
        ;; a candidate request only for pgvector's indexable shape: one
        ;; ascending distance key plus a positive LIMIT.  exec-select may use
        ;; it to restrict entity ids, after which the ordinary Datalog
        ;; distance expression above performs exact recheck/order/limit.
        secondary-candidate
        (when (and (pos-int? limit-val)
                   (= 1 (count order-by-spec))
                   (= :asc (second (first order-by-spec)))
                   (not= :first (nth (first order-by-spec) 2 nil))
                   (not fetch-with-ties?)
                   default-table
                   (not @has-aggregates?)
                   (not has-distinct?)
                   (empty? joins)
                   (empty? group-by)
                   (nil? having-expr)
                   (empty? @window-specs)
                   (empty? project-set-specs)
                   (empty? correlated-subqs))
          (let [order-var (ffirst order-by-spec)]
            (some (fn [candidate]
                    (when (= order-var (:result-var candidate))
                      (assoc candidate
                             :limit limit-val
                             :prefer-entity-filter? (some? where-expr)
                             :candidate-limit (+ limit-val (or offset-val 0)))))
                  @(:vector-distance-candidates ctx))))
        scalar-order-candidate
        ;; A PostgreSQL B-tree can answer a one-column ORDER BY / LIMIT from
        ;; its physical order. Keep this shape separate from ANN: membership,
        ;; value, and order are all exact, while ordinary WHERE predicates are
        ;; still evaluated by the primary query after candidate restriction.
        ;; More complex ORDER BY aliases/expressions remain on the ordinary
        ;; Datahike path until expression indexes have a schema contract.
        (when (and (pos-int? limit-val)
                   (= 1 (count order-by))
                   (= 1 (count order-by-spec))
                   (not fetch-with-ties?)
                   default-table
                   (not @has-aggregates?)
                   (not has-distinct?)
                   (empty? joins)
                   (empty? group-by)
                   (nil? having-expr)
                   (empty? @window-specs)
                   (empty? project-set-specs)
                   (empty? correlated-subqs))
          (let [^OrderByElement obe (first order-by)
                order-expr (.getExpression obe)]
            (when (instance? Column order-expr)
              (let [resolved (ctx/resolve-column
                              order-expr table-aliases default-table
                              (:col-overrides ctx) (:derived-aliases ctx)
                              (:ci-index ctx))
                    attr (ctx/attr-of ctx resolved)
                    attr-schema (get schema attr)
                    alias (cond
                            (and (vector? resolved)
                                 (= :aliased (first resolved)))
                            (second resolved)

                            (keyword? resolved) (namespace resolved)
                            :else nil)
                    range-predicates
                    (when attr
                      (stratum-range-predicates
                       ctx attr attr-schema where-expr schema db))]
                (when (and attr alias (some? range-predicates))
                  {:entity-var (ctx/entity-var! ctx alias)
                   :attribute attr
                   :direction (second (first order-by-spec))
                   :nulls (nth (first order-by-spec) 2 nil)
                   :where range-predicates
                   :limit limit-val
                   :candidate-limit (+ limit-val (or offset-val 0))})))))
        text-search-candidates
        ;; A conjunctive @@ predicate may use Scriptum as a complete posting-
        ;; list source. The ordinary translated predicate remains in :where,
        ;; so candidate selection cannot change PostgreSQL-visible matching.
        (when (and default-table
                   (not @has-aggregates?)
                   (not has-distinct?))
          (vec @(:text-search-candidates ctx)))
        ;; A constant implicit HAVING group deliberately detached from its
        ;; source relation above must not keep that relation's entity id in
        ;; :with: it is now unbound and would suppress the singleton row.
        with-vars (if degenerate-having? [] @(:with-vars ctx))
        ;; A top-level equality on a unique attribute proves that this
        ;; simple single-table query produces at most one source row. SQL
        ;; bag preservation is therefore vacuous, and keeping the entity id
        ;; in :with would unnecessarily disqualify Datahike's direct
        ;; point-lookup executor. Do not remove any other :with vars (for
        ;; example ordinality), and do not apply the proof across joins: a
        ;; unique row on one side can still join to many rows on the other.
        with-vars (if (and (empty? joins)
                           @(:unique-value-bound? ctx)
                           default-table)
                    (remove #{(ctx/entity-var! ctx default-table)} with-vars)
                    with-vars)
        ;; Remove :with vars that appear in :find (Datahike disallows overlap)
        find-syms (set (mapcat (fn [elem]
                                 (if (seq? elem) (filter symbol? (flatten elem)) [elem]))
                               find-elems-vec))
        with-vars (remove find-syms with-vars)
        ;; === OPTIMIZATION: Unify equality-joined variables ===
        ;; Replaces [(= ?a ?b)] with same-var usage in data patterns.
        ;; This allows Datahike to use indexed lookups instead of full scans.
        ;; E.g. [?d :dep/mid ?d_mid] [?m :mod/id ?m_id] [(= ?d_mid ?m_id)]
        ;;    → [?d :dep/mid ?join] [?m :mod/id ?join]
        ;;
        ;; Step 1: Compute the renames map
        eq-renames
        (let [var-pats (into {}
                             (keep (fn [c]
                                     (when (and (vector? c) (= 3 (count c)) (symbol? (nth c 2)))
                                       [(nth c 2) c]))
                                   where-clauses))
              eq-unifications (keep (fn [c]
                                      (let [[sym v1 v2] (if (and (seq? c) (symbol? (first c)))
                                                          c
                                                          (when (and (vector? c) (= 1 (count c)) (seq? (first c)))
                                                            (first c)))]
                                        (when (and (= sym '=)
                                                   (symbol? v1) (symbol? v2)
                                                   (var-pats v1) (var-pats v2))
                                          {:eq-clause c :keep v1 :replace v2})))
                                    where-clauses)]
          {:renames (into {} (map (fn [{:keys [keep replace]}] [replace keep]) eq-unifications))
           :eq-set  (set (map :eq-clause eq-unifications))})

        ;; Step 2: Apply renames to where-clauses
        where-clauses
        (let [{:keys [renames eq-set]} eq-renames]
          (if (empty? renames)
            where-clauses
            (let [rename-form (fn rename-form [c]
                                (cond
                                  (eq-set c) nil
                                  (symbol? c) (get renames c c)
                                  (vector? c) (mapv rename-form c)
                                  (seq? c) (apply list (map rename-form c))
                                  :else c))]
              (vec (keep rename-form where-clauses)))))

        ;; Step 3: Apply same renames to find-elems-vec
        find-elems-vec
        (let [{:keys [renames]} eq-renames]
          (if (empty? renames)
            find-elems-vec
            (mapv (fn [elem]
                    (cond
                      (symbol? elem) (get renames elem elem)
                      (seq? elem) (apply list (map #(if (symbol? %) (get renames % %) %) elem))
                      :else elem))
                  find-elems-vec)))

        ;; These plan shapes require ProjectSet to move across another
        ;; executor stage. Running it in the generic pre-window/pre-DISTINCT
        ;; position produces plausible but wrong rows (for example a window
        ;; count sees the expanded rows). Keep the boundary explicit until
        ;; those stages carry projection-position metadata and can be ordered
        ;; exactly like PostgreSQL's plan.
        _project-window-boundary
        (when (and (seq project-set-specs) (seq @window-specs))
          (throw (errors/pg-error
                  :feature-not-supported
                  {:feature "combining window functions with target-list set-returning functions"})))
        _project-distinct-on-boundary
        (when (and (seq project-set-specs) (seq distinct-on-items))
          (throw (errors/pg-error
                  :feature-not-supported
                  {:feature "DISTINCT ON with target-list set-returning functions"})))

        query-map (cond-> {:find  find-elems-vec
                           :where (vec where-clauses)}
                    ;; Add :in clause if we have extra params (CASE fns, etc.)
                    (seq in-params)
                    (assoc :in (into ['$] in-params))
                    ;; Add :with for deduplication prevention
                    (seq with-vars)
                    (assoc :with (vec with-vars))
                    ;; order-by
                    (seq order-by-flat)
                    (assoc :order-by order-by-flat))
        _freeze-view-stars
        (when *freeze-view-stars?*
          (freeze-select-stars! select select-items star-relations
                                table-aliases schema db))]

    (cond-> {:query           query-map
             ;; When server-side sort is needed, limit/offset are applied there too
             :limit           (when (and (empty? project-set-specs)
                                         (not sql-order-by))
                                limit-val)
             :offset          (when (and (empty? project-set-specs)
                                         (not sql-order-by))
                                offset-val)
             :find-aliases    @find-aliases
             ;; OID per find-alias for Extended Query Describe. nil slots
             ;; fall back to value-based inference at Execute time (via
             ;; compute-schema-oids) or TEXT when neither path resolves.
             :select-item-oids select-item-oids
             ;; Unlike an ordinary projection, UNION/INTERSECT/EXCEPT keep
             ;; bare strings, NULL and undeclared parameters as UNKNOWN while
             ;; finding a per-column common type. This parallel vector
             ;; preserves that analyzer-only distinction.
             :select-item-resolution-oids select-item-resolution-oids
             ;; Index-aligned with :select-item-oids — the 1-based `$N`
             ;; for output columns that are a bare placeholder, so
             ;; describeResult can type them from the Parse message's
             ;; declared OID (issue #27).
             :select-item-param-idx select-item-param-idx
             :has-aggregates? @has-aggregates?
             :has-distinct?   has-distinct?
             ;; PostgreSQL requires the DISTINCT ON expressions to be the
             ;; LEADING ORDER BY expressions, so they are exactly the first
             ;; N sort keys -- no second resolution path needed.
             :distinct-on-n   (when (seq distinct-on-items) (count distinct-on-items))
             :in-args         in-args
             ;; Deferred correlated items re-parse their inner SQL per
             ;; outer row too, so an outer `$N` inside them must still be
             ;; resolvable then -- the simple-query numeric templater keys
             ;; off this flag, and without it `(SELECT v FROM lc WHERE
             ;; lt.id = 1)` ran with an unbound `$1` and answered NULL.
             :runtime-subqueries? (or @(:runtime-subqueries? ctx)
                                      (boolean (seq correlated-subqs)))
             :hidden-count    hidden-count
             ;; Pass enriched db when derived tables or derived-table-joins
             ;; created speculative data (FROM (…) AS sub or JOIN (…) AS sub).
             :enriched-db     (when (or (instance? ParenthesedSelect from-item)
                                        values-vt
                                        ;; bare SRF in FROM materialised into
                                        ;; a virtual table (table-fn->virtual-table)
                                        (instance? net.sf.jsqlparser.statement.select.TableFunction from-item)
                                        ;; `FROM <sequence>` — its row lives only
                                        ;; in the speculative db built above, so
                                        ;; Execute has to run against that db and
                                        ;; not the caller's (issue #26).
                                        seq-vt
                                        (seq derived-joins))
                                db)
             ;; Server-side sort for nullable ORDER BY columns
             :sql-order-by    sql-order-by
             :sql-limit       (when (and (empty? project-set-specs) sql-order-by)
                                limit-val)
             :sql-offset      (when (and (empty? project-set-specs) sql-order-by)
                                offset-val)
             :fetch-with-ties? fetch-with-ties?
             :project-set     (when (seq project-set-specs) project-set-specs)
             :project-order-by project-order-by
             :project-limit   (when (seq project-set-specs) limit-val)
             :project-offset  (when (seq project-set-specs) offset-val)
             ;; Compound aggregate expressions (MAX(a)-MIN(a)) for server-side computation
             :compound-exprs  (when (seq @compound-exprs) @compound-exprs)
             ;; Window function specs for server-side post-processing
             :window-specs    (when (seq @window-specs) @window-specs)
             ;; Correlated scalar subqueries (slice A): each is run per outer
             ;; row by exec-select, which binds the correlation columns
             ;; (whose Datalog result indices are in :corr-col->idx) into
             ;; *from-bindings*, then splices the value at :out-pos.
             :correlated-subqueries (when (seq correlated-subqs)
                                      {:subqueries correlated-subqs
                                       :corr-col->idx corr-col->idx
                                       :n-output (count select-items)})
             ;; FOR UPDATE row-locking (SKIP LOCKED / NOWAIT / blocking)
             :for-update      for-update
             ;; Prepared-statement param placeholders {index → ?var}.
             ;; The wire layer resolves these at Bind time by appending
             ;; decoded values to :in-args in index order; an empty map
             ;; means no parameters were used.
             :param-placeholders @(:param-placeholders ctx)}
      secondary-candidate
      (assoc :secondary-candidate secondary-candidate)
      scalar-order-candidate
      (assoc :secondary-order-candidate scalar-order-candidate)
      (seq text-search-candidates)
      (assoc :secondary-text-candidates text-search-candidates)
      ;; Include join metadata for outer join handling
      ;; A nested-loop outer join whose ON clause reads a column. Legal
      ;; for LEFT and RIGHT; a FULL JOIN with one is what PostgreSQL
      ;; refuses as "only supported with merge-joinable or hash-joinable
      ;; join conditions", and only the FULL assembly in sql.clj knows
      ;; it is assembling one.
      (some #(let [info (:ref-info %)]
               (and (:nested-loop? info) (:variable-pred? info)
                    (not (:constant-false-on? info))))
            join-infos)
      (assoc :variable-nested-loop-join? true)

      (some #(#{:left :right :full} (:join-type %)) join-infos)
      (assoc :join-infos join-infos
             :left-table (get table-aliases default-table default-table)
             :right-tables (mapv #(get table-aliases (:alias %) (:name %)) join-infos))
      ;; Include HAVING as post-filter metadata
      having-expr
      (assoc :having having-spec))))

;; ============================================================================
;; DML translation: INSERT, UPDATE, DELETE
;; ============================================================================

(def parse-bytea-hex
  "Re-exported from datahike.pg.sql.coerce, where it moved so the shared
   cast implementation (a leaf namespace) could reach it."
  coerce/parse-bytea-hex)

(defn apply-sql-cast
  "Apply a SQL CAST to a value. Returns the value cast to the target type.
   Handles nil safely (returns nil). Idempotent: if `inner` is already the
   target type, returns it unchanged (avoids lossy round-trips through str,
   e.g. Date → `(str d)` → unparseable string)."
  [inner ^CastExpression ce]
  (if (nil? inner)
    nil
    (let [col-data-type (.getColDataType ce)
          type-str (when col-data-type
                     (types/normalize-sql-type-name (str col-data-type)))
          cast-cat (types/cast-category type-str)
          ;; CAST(<x> AS T[]): JSqlParser exposes the array dim via
          ;; getArrayData (a list, size = ndim) rather than embedding
          ;; `[]` in the type string. We retype an empty / untyped
          ;; PgArray to match the target element-keyword and pass
          ;; non-array inputs through `pg-arr/array` for consistency.
          array-data (try (.getArrayData col-data-type) (catch Throwable _ nil))
          array-target? (and (some? type-str) (seq array-data))]
      (when (and array-target?
                 (= :vector (types/cast-category
                             (str/replace type-str #"\[\]$" ""))))
        (throw (errors/pg-error
                :feature-not-supported
                {:message "vector arrays are not supported"})))
      (if array-target?
        (let [elem-kw (or (get types/sql-name->elem-kw type-str) :text)]
          (cond
            (pg-arr/array? inner) (pg-arr/array elem-kw (:elements inner)
                                                (:dims inner) (:lbounds inner))
            (sequential? inner)   (pg-arr/array elem-kw (vec inner))
            (string? inner)       (pg-arr/from-pg-text inner elem-kw)
            :else                 (pg-arr/array elem-kw [inner])))
        ;; Scalar targets go through the one shared implementation
        ;; (datahike.pg.sql.cast) rather than a fourth private copy of
        ;; the category dispatch — see that namespace's docstring.
        (sql-cast/cast-scalar
         inner type-str
         {:explicit? true
          :parse-timestamp expr/parse-timestamp-string
          :resolve-regclass
          (fn [n]
            (or (when (and params/*parse-db* (some? inner))
                  (pgs/table-oid params/*parse-db* n))
                (when (seq n) (Math/abs (.hashCode ^String n)))
                0))
          :resolve-regtype #(or (params/registered-type-oid params/*parse-db* %) %)})))))

(defn- widen-integral
  "Widen a narrow integral result to Long. `length()` answers an
   Integer (its PG type is int4), and Datahike's :db.type/long rejects
   anything but a Long — so an evaluated `length('hello')` failed the
   transaction with \"invalid input syntax\" while `abs(-7)`, which
   already answers a Long, went through."
  [v]
  (if (and (integer? v) (not (instance? Long v)) (not (instance? clojure.lang.BigInt v))
           (not (instance? java.math.BigInteger v)))
    (long v)
    v))

(defn sql-value
  "A result cell as an SQL value: the NULL sentinel as nil."
  [v]
  (when-not (or (nil? v) (= :__null__ v)) (widen-integral v)))

(defn run-const-select-row
  "The first `n` values of a parsed one-row `SELECT <expr>, ...`, NULL as
   nil; nil when the query yields no row."
  [p db n]
  (when (= :error (:type p))
    (throw (ex-info (str (:message p))
                    {:sqlstate (:sqlstate p) :error-fields (:error-fields p)})))
  (let [row (if-let [q (:query p)]
              (let [ia (:in-args p)
                    qdb (or (:enriched-db p) db)]
                (first (if (seq ia) (apply d/q q qdb ia) (d/q q qdb))))
              (:literal-row p))
        row (if (sequential? row) row [row])]
    (mapv sql-value (take n (concat row (repeat nil))))))

(defn run-const-select
  "The single value of a parsed one-row `SELECT <expr>`, NULL as nil."
  [p db]
  (first (run-const-select-row p db 1)))

(defn const-select-fn
  "A function of the in-args vector computing the row run-const-select-row
   would (all find columns, NULL as nil; nil for no row),
   for a plan whose query is only a chain of function clauses -- the shape
   the translator emits for a FROM-less one-row SELECT. nil for any other
   shape (data patterns, or/not, rules), which only d/q evaluates.

   For expressions evaluated once per written row (CHECK constraints): d/q
   costs far more than the functions it calls, most of it re-planning a
   query the statement's own queries have pushed out of its cache. The
   clauses run in the order the translator emitted them, which is the
   order their inputs are bound; a function returning nil binds nothing
   and so yields no row, as in d/q."
  [p]
  (let [{:keys [find where in] :as q} (:query p)]
    (when (and (map? q) (= '$ (first in)) (seq find) (every? symbol? find)
               (empty? (:with q)) (nil? (:enriched-db p))
               (every? (fn [c]
                         (and (vector? c) (seq? (first c)) (symbol? (ffirst c))
                              (<= 1 (count c) 2)
                              (or (= 1 (count c)) (symbol? (second c)))))
                       where))
      (let [in-syms (vec (rest in))
            var-sym? #(and (symbol? %) (str/starts-with? (name %) "?"))
            resolve-fn (fn [f]
                         (when-not (var-sym? f)
                           (some-> (when (namespace f) (requiring-resolve f)) deref)))
            clauses (mapv (fn [[[f & args] out]]
                            {:f f :fixed (resolve-fn f) :args (vec args) :out out})
                          where)]
        (when (and (every? #(or (:fixed %) (var-sym? (:f %))) clauses)
                   ;; Every variable a clause reads is bound before it, and
                   ;; the find variable at the end: the order d/q would
                   ;; have to find is the order written.
                   (let [bound-at-end
                         (reduce (fn [bound {:keys [f args out]}]
                                   (if (every? #(or (not (var-sym? %)) (bound %))
                                               (cons f args))
                                     (cond-> bound out (conj out))
                                     (reduced nil)))
                                 (set in-syms) clauses)]
                     (and bound-at-end (every? bound-at-end find))))
          (fn [in-args]
            (let [env (zipmap in-syms in-args)
                  arg (fn [env a] (if (var-sym? a) (get env a) a))
                  env (reduce (fn [env {:keys [f fixed args out]}]
                                (let [v (apply (or fixed (get env f)) (map #(arg env %) args))]
                                  (cond
                                    (nil? out) (if v env (reduced nil))
                                    (nil? v) (reduced nil)
                                    :else (assoc env out v))))
                              env clauses)
                  row (when env (mapv #(sql-value (get env %)) find))]
              row)))))))

(defn- const-value
  "Value of an INSERT VALUES expression that is not a plain literal,
   computed exactly as PostgreSQL would: as the one-row `SELECT <expr>`.

   Every evaluation error propagates with its SQLSTATE (a bare column is
   42703, as in PostgreSQL: VALUES has no FROM), and NULL comes back as
   nil. This replaced a fallback to the expression's SQL TEXT, which stored
   `'a' || NULL`, `CASE ... END` and bare identifiers verbatim, and the
   internal `:__null__` sentinel for NULL results -- silently.

   Parameters: before Bind the values do not exist, so an expression over
   `$N` becomes a placeholder evaluated at Bind with every parameter bound
   (params/expression-param-ref)."
  [e schema db]
  (let [pf params/*parse-sql*
        sql (str "SELECT " e)]
    (when-not (and pf schema db)
      (throw (errors/pg-error :feature-not-supported
                              {:message (str "cannot evaluate expression here: " e)})))
    (if params/*bound-params*
      (run-const-select (pf sql schema db) db)
      (let [p (pf sql schema db)]
        (if-let [idxs (seq (keys (:param-placeholders p)))]
          (params/expression-param-ref
           (apply max idxs)
           (fn [bound]
             (binding [params/*bound-params* bound]
               (run-const-select (pf sql schema db) db))))
          (run-const-select p db))))))

(defn- extract-numeric-binary
  "Evaluate a binary numeric expression in INSERT ... VALUES using the
   same PostgreSQL arithmetic helpers as SELECT/UPDATE."
  [left right schema db operation]
  (let [l (extract-value left schema db)
        r (extract-value right schema db)]
    (cond
      (or (nil? l) (nil? r)) nil
      (and (or (number? l) (types/numeric-special? l))
           (or (number? r) (types/numeric-special? r)))
      (operation l r)
      :else ::unhandled)))

(defn extract-value
  "Extract a Clojure value from a JSqlParser expression for INSERT VALUES.
   Optional schema+db params enable scalar subquery evaluation.

   When the expression is a JdbcParameter (prepared-statement placeholder):
     - if `params/*bound-params*` is bound (lexical-template fast path
       or execute-time re-translation), resolve the parameter inline
       to the bound value — no ParamRef leaves this function;
     - otherwise emit a ParamRef sentinel that the wire layer resolves
       at Bind time against the decoded client value."
  ([e] (extract-value e nil nil))
  ([e schema db]
   (cond
     (instance? JdbcParameter e)
     (let [idx (.getIndex ^JdbcParameter e)]
       (if-let [bound params/*bound-params*]
         (nth bound (dec (long idx)))
         (->ParamRef idx)))

     (instance? LongValue e)
    ;; JSqlParser stores every integer literal as LongValue, including
    ;; PG-valid ones that overflow Long (e.g. `VALUES (9223372036854775808)`
    ;; for a NUMERIC or FLOAT column). `.getValue` calls parseLong
    ;; which throws; `.getStringValue` returns the original digit string.
    ;; Fall through to BigInteger so coerce-insert-value can down-convert
    ;; to double / float / bigdec as needed.
     (try (.getValue ^LongValue e)
          (catch NumberFormatException _
            (java.math.BigInteger. ^String (.getStringValue ^LongValue e))))
     (instance? DoubleValue e) (types/decimal-literal e (.getValue ^DoubleValue e))
     (instance? StringValue e) (expr/string-value-text ^StringValue e)
     (instance? BooleanValue e) (.getValue ^BooleanValue e)
     (instance? NullValue e) nil
     ;; `DEFAULT` keyword in a VALUES position. JSqlParser 5.x parses
     ;; this as a bare Column named "DEFAULT" with no table qualifier
     ;; (verified by probing parse output — see commit message). Real
     ;; PG semantics: substitute the column's DEFAULT clause, or NULL
     ;; if none. For pg-datahike, returning nil is sufficient — the
     ;; row-attrs builder drops nil values via `(when (some? coerced)
     ;; ...)`, so the column ends up absent from the transacted entity
     ;; and datahike's missing-attr semantics take over (read as NULL).
     ;; Columns with stored DEFAULTs like nextval()/now() are handled
     ;; by the auto-populate-identity path further downstream — but
     ;; Odoo writes those values explicitly in its INSERTs, so this
     ;; fallback covers the load-bearing case (Odoo's
     ;; ir_act_window_view multi-row INSERTs that use DEFAULT for
     ;; nullable FK columns like view_id).
     (and (instance? Column e)
          (nil? (.getTable ^Column e))
          (= "default" (str/lower-case (unquote-ident (.getColumnName ^Column e)))))
     ::insert-default
     (instance? SignedExpression e)
     (let [^SignedExpression se e
           inner (extract-value (.getExpression se) schema db)]
       (if (and (= (.getSign se) \-) (number? inner))
         (- inner)
         inner))
     (instance? CastExpression e)
     (let [^CastExpression ce e
           inner (extract-value (.getLeftExpression ce) schema db)]
       ;; Parse must stay value-free. Applying the cast to a ParamRef here
       ;; tries to coerce the placeholder record itself (for example to
       ;; bigint) and rejects the Parse before pgjdbc can send Bind. Retain
       ;; the exact cast operation on the placeholder and apply it when the
       ;; bound value is substituted. This also composes nested casts in
       ;; inside-out order.
       (if (params/param-ref? inner)
         (params/transform-param-ref
          inner
          (fn [value]
            (binding [params/*parse-db* db]
              (apply-sql-cast value ce))))
         (apply-sql-cast inner ce)))
     (instance? Addition e)
     (let [^Addition expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-+)]
       (if (= ::unhandled value)
         (const-value e schema db)
         value))
     (instance? Subtraction e)
     (let [^Subtraction expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql--)]
       (if (= ::unhandled value)
         (const-value e schema db)
         value))
     (instance? Multiplication e)
     (let [^Multiplication expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-*)]
       (if (= ::unhandled value)
         (const-value e schema db)
         value))
     (instance? Division e)
     (let [^Division expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-div)]
       (if (= ::unhandled value)
         (const-value e schema db)
         value))
     (instance? Modulo e)
     (let [^Modulo expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-mod)]
       (if (= ::unhandled value)
         (const-value e schema db)
         value))
    ;; Parenthesized single expression — unwrap
     (instance? ParenthesedExpressionList e)
     (let [^ParenthesedExpressionList pel e]
       (if (= (count pel) 1)
         (extract-value (first pel) schema db)
         (const-value e schema db)))
    ;; Scalar subquery: (SELECT id FROM table WHERE ...)
     (instance? ParenthesedSelect e)
     (strict-scalar-value (.getSelect ^ParenthesedSelect e)
                          schema db params/*parse-sql*)
     (instance? net.sf.jsqlparser.expression.Function e)
     (let [^net.sf.jsqlparser.expression.Function f e
           fname (expr/resolution-name (.getName f))]
       (cond
         ;; nextval('seq_name') in INSERT VALUES → marker resolved
         ;; per-execute by resolve-nextval-markers.
         (= fname "nextval")
         (let [params (.getParameters f)
               arg (first (.getExpressions params))]
           {:fn :nextval :seq-name (extract-value arg schema db)})

         ;; now() and friends emit a marker too, so the parsed map
         ;; doesn't bake in a parse-time Date — that would freeze
         ;; the timestamp on every cache hit and make all rows of
         ;; one INSERT shape carry the same wallclock. Resolved at
         ;; execute time (resolve-nextval-markers).
         (#{"now" "current_timestamp" "transaction_timestamp"
            "statement_timestamp" "clock_timestamp"
            "localtimestamp" "localtime" "current_date" "current_time"} fname)
         {:fn :now}

         ;; UUID generators are volatile too. A concrete UUID folded into
         ;; the parse cache would be reused by every execution of the same
         ;; INSERT shape and immediately violate uniqueness.
         (#{"gen_random_uuid" "uuidv4" "uuidv7"} fname)
         {:fn (if (= "uuidv7" fname) :uuid-v7 :random-uuid)}

         ;; Any other function. This was `(str e)`, which stored the SQL
         ;; TEXT: `INSERT INTO t VALUES (1, repeat('x',5))` put the
         ;; 14-character string `repeat('x', 5)` in the column.
         ;;
         ;; Evaluate it HERE, at parse time, rather than deferring a
         ;; marker to execute time. The volatile functions — nextval,
         ;; now and friends — are already handled above precisely
         ;; because folding them into a cached parse would freeze them;
         ;; everything reaching this branch is deterministic, so folding
         ;; is safe. It also keeps the value inside `coerce-insert-value`,
         ;; which a deferred marker escapes: a resolved marker arrived
         ;; uncoerced and `length('hello')` into an int column failed
         ;; with "invalid input syntax".
         ;;
         ;; An expression that cannot be evaluated raises, with its
         ;; SQLSTATE; it used to fall back to storing its text.
         :else (const-value e schema db)))
    ;; Bare CURRENT_TIMESTAMP / CURRENT_DATE / CURRENT_TIME (no parens)
    ;; parse as TimeKeyExpression, not Function — same marker as the
    ;; function forms above; without this branch the keyword fell
    ;; through to `(str e)` and the literal string reached the
    ;; transactor (issue #14).
     (instance? TimeKeyExpression e)
     {:fn :now}
    ;; now() AT TIME ZONE 'UTC' → current timestamp marker, like the
    ;; bare-function case above. Any other AT TIME ZONE is an ordinary
    ;; expression; it used to become now() as well.
     (and (instance? TimezoneExpression e)
          (let [left (.getLeftExpression ^TimezoneExpression e)]
            (and (instance? net.sf.jsqlparser.expression.Function left)
                 (= "now" (expr/resolution-name (.getName ^net.sf.jsqlparser.expression.Function left))))))
     {:fn :now}

    ;; ArrayConstructor literal: ARRAY[1,2,3] / ARRAY[ARRAY[1,2],…].
    ;; Build a typed PgArray; coerce-insert-value will serialize it
    ;; for storage on an array column. Recurses on nested
    ;; ArrayConstructors so multi-dim literals build the right shape.
    ;; Element-type detection mirrors expr.clj's recursive walker so
    ;; INSERT and SELECT translation produce the same elem-type for
    ;; equivalent literals.
    ;;
    ;; Nested ArrayConstructors materialize as PgArray instances at
    ;; the inner level — but PgArray is a defrecord (map-like), which
    ;; the outer ctor's `compute-dims` treats as a scalar. Unwrap any
    ;; PgArray children's `:elements` so the outer build sees a
    ;; uniform nested-vector structure and computes dims correctly.
     (instance? ArrayConstructor e)
     (let [exprs (.getExpressions ^ArrayConstructor e)
           detect (fn detect [es]
                    (or (some (fn [x]
                                (cond
                                  (instance? LongValue x)        :int8
                                  (instance? DoubleValue x)      :float8
                                  (instance? StringValue x)      :text
                                  (instance? BooleanValue x)     :bool
                                  (instance? ArrayConstructor x)
                                  (detect (.getExpressions ^ArrayConstructor x))
                                  :else nil))
                              es)
                        :text))
           elem-type (detect exprs)
           unwrap   (fn [v] (if (pg-arr/array? v) (:elements v) v))
           elements (mapv #(unwrap (extract-value % schema db)) exprs)]
       (pg-arr/array elem-type elements))

     :else (const-value e schema db))))

(defn- apply-numeric-typmod
  "PG NUMERIC(p,s) on input: round/pad to scale `s` (1 → 1.00, 1.239 →
   1.24) AND reject a value whose integer part no longer fits precision
   `p`. Unconstrained NUMERIC (both nil) leaves the value untouched.
   Only acts on BigDecimals.

   The precision half was missing entirely: `p` was decoded and then
   discarded, so 22003 numeric field overflow was never raised on any
   write path."
  [v p scale]
  (if (or (instance? java.math.BigDecimal v) (types/numeric-special? v))
    (cond
      (and p scale) (sql-cast/apply-numeric-typmod v p scale)
      scale         (.setScale ^java.math.BigDecimal v (int scale)
                               java.math.RoundingMode/HALF_UP)
      :else         v)
    v))

(defn coerce-insert-value
  "Coerce a value to match the schema type for an attribute.

   `:db.type/ref` columns: SQL FK semantics says `INSERT/UPDATE … SET
   col = N` writes the target's PK value (matching what the
   read-side ref-deref returns). Datahike's transact requires either
   an entity-id or a lookup-ref. We convert the user-supplied PK
   value to a lookup-ref `[target-pk-attr val]` using the same
   convention `derive-ref-targets` uses on the read side, keeping
   read and write FK semantics symmetric. Falls through to the raw
   value when no target is resolvable (hint-only refs without a
   threaded db, or genuinely-unmapped refs)."
  [val attr schema & [db]]
  ;; The query engine's NULL sentinel is SQL NULL here too: INSERT ...
  ;; SELECT passed `:__null__` through as a value, and Datahike rejected it.
  (when (and (some? val) (not= :__null__ val))
    ;; A deferred call marker is NOT a value yet — `{:fn :nextval …}`,
    ;; `{:fn :now}`, `{:fn :eval …}` are resolved per execute, after
    ;; this. Coercing one here would treat the marker MAP as data: for a
    ;; jsonb or text column it serialised to `{":fn": ":eval", …}` and
    ;; that reached the transactor.
    (if (params/call-marker? val)
      val
      (let [vtype     (get-in schema [attr :db/valueType])
            ;; Both of these describe the DECLARED SQL type, and both live
            ;; on the ident entity rather than in Datahike's schema map, so
            ;; both need the db fallback. The enriched `:pg/numeric-scale`
            ;; only ever reached the INSERT translator, which is why
            ;; `numeric(p,s)` rounded on INSERT but not on UPDATE.
            typmod    (or (get-in schema [attr :pg/typmod])
                          (params/pg-typmod-of-attr db attr))
            num-scale (or (second (when typmod (types/decode-numeric-typmod typmod)))
                          (get-in schema [attr :pg/numeric-scale]))
            num-prec  (or (first (when typmod (types/decode-numeric-typmod typmod)))
                          (get-in schema [attr :pg/numeric-precision]))
          ;; ONLY jsonb normalizes. PG `json` is the text-faithful type — it
          ;; keeps key order, whitespace and duplicate keys — so it must NOT
          ;; be canonicalized.
          ;;
          ;; `:pg/type` is an ident-entity fact, not a `:db/*` key, so it is
          ;; absent from Datahike's schema map unless the caller enriched it
          ;; first. Exactly one of the three callers did, so canonicalization
          ;; silently did NOT happen for UPDATE or for a parameterised INSERT
          ;; — i.e. for every client that is not psql. Absent metadata has to
          ;; mean "ask", not "not jsonb".
            pg-type   (or (get-in schema [attr :pg/type])
                          (params/pg-type-of-attr db attr))
            ;; An array column's element type, from the same ident
            ;; entity: DDL names the array type `_<elem>`. Read from the
            ;; schema map alone, it was absent unless the caller had
            ;; enriched the schema, and a PgArray fell through to the
            ;; string branch -- `ARRAY[7,8]` from INSERT ... SELECT or an
            ;; UPDATE was stored as the text `[7, 8]`.
            elem-kw   (or (get-in schema [attr :pg/array-elem])
                          (when (and pg-type (str/starts-with? pg-type "_"))
                            (keyword (subs pg-type 1))))
            jsonb?    (= "jsonb" pg-type)
            ;; The declared integer width, when the column has one.
            int-type  (when (contains? #{"int2" "int4" "int8" "oid"} pg-type) pg-type)
          ;; PostgreSQL validates BOTH types on input — `json_in` does a
          ;; full RFC-8259 parse and only then stores the original bytes.
          ;; We validated neither, so malformed text reached storage:
          ;; `'"abc'::jsonb` became the string `"abc`.
            json-ish? (contains? #{"json" "jsonb"} pg-type)
            _         (when (and json-ish? (string? val))
                        (jb/validate-json! val))]
        (cond
        ;; ParamRef is a defrecord placeholder for a `?` parameter
        ;; resolved at Bind time. Don't coerce it here — the branches
        ;; below would incorrectly treat it as a Clojure map (records
        ;; satisfy `map?`) and either jsonb-serialize it into a
        ;; "{\"idx\":N}" string for :db.type/string columns or stringify
        ;; via `(str val)`. Pass it through; substitute-params replaces
        ;; it with the decoded wire value, which already has the right
        ;; type from Bind. (Must precede the jsonb branch below, or a
        ;; jsonb `?` param would be serialized as its placeholder record.)
          (params/param-ref? val) val

        ;; jsonb columns are :db.type/string, so a `'{…}'::jsonb` STRING literal would
        ;; otherwise be stored verbatim (non-canonical → equality/DISTINCT wrong,
        ;; behaving like PG `json`). Canonicalize every jsonb write here — string
        ;; literal or Clojure map/vector alike — keyed on the :pg/type tag.
          jsonb? (jb/serialize-jsonb val)

          (= "tsvector" pg-type) (tsearch/canonical-tsvector val)

          (= "tsquery" pg-type) (tsearch/canonical-tsquery val)

        ;; time / timetz columns keep their text form, so the write is where
        ;; PostgreSQL's input function has to run: it validates ('25:00' is
        ;; 22008, 'garbage' 22007) and canonicalises ('1:2:3 PM' is stored
        ;; as 13:02:03). The raw input string used to be stored verbatim.
          (and (#{"time" "timetz"} pg-type) (some? val) (not= :__null__ val))
          (types/->pg-text (sql-cast/cast-scalar val pg-type {:explicit? true})
                           (if (= "time" pg-type) types/oid-time types/oid-timetz))

        ;; pgvector's vector type is Datahike's native float array. Parse
        ;; every write through the same input function, even if it is already
        ;; a float[], so vector(n) is enforced on INSERT and UPDATE alike.
          (= "vector" pg-type) (pg-vector/coerce val typmod)

        ;; money shares Datahike's BigDecimal carrier with numeric, but its
        ;; SQL input function accepts currency/grouping syntax and enforces
        ;; an int64 count-of-cents range. Dispatch on the declared type
        ;; before the generic :db.type/bigdec assignment branch.
          (= "money" pg-type) (sql-cast/parse-money val)

        ;; BIT and BIT VARYING share string storage with text/json, but
        ;; expression evaluation carries them as PgBit so width and type
        ;; survive operators. Persist only PostgreSQL's canonical digit run;
        ;; otherwise INSERT ... SELECT stringifies the record as
        ;; `{:bits ...}`, and the next read fails on the opening `{`.
        ;; Assignment coercion is deliberately non-explicit: fixed bit(n)
        ;; requires exactly n bits, while varbit(n) accepts shorter input and
        ;; rejects longer input instead of silently truncating it.
          (contains? #{"bit" "varbit"} pg-type)
          (let [target-type (str pg-type (when typmod (str "(" typmod ")")))]
            (pg-bits/to-pg-text
             (sql-cast/cast-to-bit val target-type false)))

        ;; Native PG array column (`:pg/array-elem` recorded by DDL)
        ;; — Option C storage: serialize a PgArray (or coerce a
        ;; sequential value into one) to canonical PG text. Strings
        ;; that already look like array text pass through unchanged.
          (some? elem-kw)
          (cond
            (pg-arr/array? val)
            (pg-arr/to-pg-text val)
            (and (string? val)
                 (clojure.string/starts-with? (clojure.string/triml val) "{"))
            val
            (sequential? val)
            (pg-arr/to-pg-text (pg-arr/array elem-kw (vec val)))
            :else
          ;; Last-ditch: string-coerce. Lets clients send a single
          ;; element to an array column and have it stored as a
          ;; 1-element array (PG would reject this; we tolerate to
          ;; mirror the permissive behaviour of `:db.type/string`
          ;; coercion above).
            (pg-arr/to-pg-text (pg-arr/array elem-kw [val])))
        ;; :db.type/ref column with a numeric/string PK value →
        ;; lookup-ref. Already-vector values (explicit `[:k v]`) pass
        ;; through unchanged. Convention-based target resolution
        ;; (hints aren't visible here without db access — see
        ;; coerce-insert-value-with-hints for the hint-aware path).
        ;;
        ;; Wrap into `[pk-attr val]` ONLY when val's runtime type
        ;; matches the target PK's `:db/valueType`. If it doesn't
        ;; (e.g. user wrote `account=143` against an account whose
        ;; PK is `:db.type/string`), pass val through as a direct
        ;; entity-id reference. The read-side surfaces ref columns
        ;; as raw entity-ids when no PK target is resolvable, so this
        ;; symmetric write path keeps round-trips honest.
          (and (= vtype :db.type/ref)
               (not (vector? val))
               (some? val))
          (let [target-entry (get (pgs/derive-ref-targets schema {}) attr)
                target-pk-attr (if (vector? target-entry) (first target-entry) target-entry)
                target-vtype (when target-pk-attr
                               (get-in schema [target-pk-attr :db/valueType]))
                val-matches-pk?
                (case target-vtype
                  :db.type/string  (string? val)
                  :db.type/long    (integer? val)
                  :db.type/uuid    (or (instance? java.util.UUID val) (string? val))
                  :db.type/keyword (or (keyword? val) (string? val))
                ;; No target or unknown PK type: treat as entity-id.
                  false)]
            (if (and target-pk-attr val-matches-pk?)
              [target-pk-attr val]
              val))
        ;; INSERT/UPSERT construction can pass an already-coerced row back
        ;; through this function. Keep numeric-special storage encoding
        ;; idempotent; applying NUMERIC(p,s) to the out-of-domain sentinel
        ;; would correctly diagnose it as an enormous finite value instead.
          (and (= vtype :db.type/bigdec)
               (types/numeric-special-storage? val))
          val
        ;; BigInteger / BigInt lands here when a SQL literal overflows
        ;; Long; routed through `coerce/coerce-numeric` so :db.type/long
        ;; raises 22003 instead of silently wrapping via .longValue,
        ;; while float/double/bigdec land at finite/±Infinity/exact as
        ;; PG does.
          (or (instance? java.math.BigInteger val)
              (instance? clojure.lang.BigInt val))
          (case vtype
            :db.type/float  (coerce/coerce-numeric val :float)
            :db.type/double (coerce/coerce-numeric val :double)
            :db.type/bigdec (apply-numeric-typmod
                             (coerce/coerce-numeric val :bigdec) num-prec num-scale)
            :db.type/long   (coerce/coerce-numeric val :long)
            val)
        ;; BigDecimal cannot carry PostgreSQL NUMERIC's NaN/+/-Infinity.
        ;; Store their reserved out-of-domain BigDecimal representatives so
        ;; the attribute remains a normal, ordered :db.type/bigdec index.
          (and (= vtype :db.type/bigdec) (types/numeric-special? val))
          (types/numeric-value->storage val)
        ;; Numeric coercion across `:db.type/{long,double,float,bigdec}`
        ;; — handles both the string→number and number→number paths.
        ;; `coerce-numeric` raises 22003/22P02 with the right SQLSTATE.
          ;; `decimal?` is in each of these because a decimal LITERAL is
          ;; numeric, not float8 -- so `INSERT INTO t(f) VALUES (1.5)`
          ;; into a float8 column now hands a BigDecimal to a branch that
          ;; only knew Double and Long, and the raw value reached the
          ;; transactor as `1.5M`.
          ;; Through the cast implementation, so a write gets the same
          ;; width discipline a CAST does: PostgreSQL ROUNDS on the way to
          ;; an integer (and rounds float and numeric sources differently)
          ;; and raises 22003 when the value does not fit the declared
          ;; width. We truncated and never range-checked, so the catalog
          ;; advertised `smallint` over stored values of 100000.
          ;; `integer?` too, not just the fractional/string cases: a Long
          ;; that is simply too large for a declared int2/int4 column has
          ;; nothing to coerce but everything to reject.
          (and (= vtype :db.type/long)
               (or (string? val) (number? val)))
          (sql-cast/cast-to-integer val (or int-type "int8"))
          (and (= vtype :db.type/double)
               (or (string? val) (integer? val) (decimal? val)))
          (sql-cast/cast-to-float val "float8")
          (and (= vtype :db.type/float)
               (or (string? val) (integer? val) (decimal? val)))
          (sql-cast/cast-to-float val "real")
        ;; PG boolin: 't'/'yes'/'on'/'1' etc. — Boolean/parseBoolean
        ;; would silently turn '1' into false (issue #12).
          (and (= vtype :db.type/boolean) (string? val))
          (input/parse-bool val)
        ;; :db.type/keyword: SQL has no keyword literal, so clients
        ;; send the bare name as a string. Coerce 'draft' → :draft and
        ;; 'foo/bar' → :foo/bar (Clojure's `keyword` accepts both
        ;; forms). Empty / blank strings stay as-is so datahike's
        ;; rejection still surfaces (an empty keyword `:` is invalid).
          ;; Keep bpchar compact internally. PostgreSQL's padding is a wire
          ;; representation concern; storing the blanks would make Datahike
          ;; equality/index lookups disagree with SQL's blank-insensitive
          ;; bpchar comparison and make length(c) include the padding.
          ;;
          ;; varchar(n) / char(n) on the WRITE path. An assignment
          ;; REFUSES an over-long value where an explicit cast truncates
          ;; -- so the column cannot hold text its own declared type
          ;; forbids. Nothing enforced this, and nothing recorded the
          ;; length either until the DDL started keeping it.
          (and (= vtype :db.type/string) (string? val)
               (contains? #{"varchar" "bpchar"} pg-type) typmod
               (> (count ^String val) (- (long typmod) 4)))
          (let [n (- (long typmod) 4)
                excess (subs ^String val (int n))]
            ;; SQL permits excess trailing spaces for both character(n)
            ;; and varchar(n); they carry no information and are truncated.
            ;; Any non-space excess is still assignment error 22001.
            (if (every? #(= \space %) excess)
              (subs ^String val 0 (int n))
              (throw (errors/pg-error
                      :string-data-right-truncation
                      {:message (str "value too long for type "
                                     (if (= "bpchar" pg-type) "character(" "character varying(")
                                     n ")")}))))
          (and (= vtype :db.type/keyword) (string? val))
          (if (clojure.string/blank? val) val (keyword val))
        ;; Already-keyword passes through. Symbols coerce to keywords.
          (and (= vtype :db.type/keyword) (keyword? val)) val
          (and (= vtype :db.type/keyword) (symbol? val)) (keyword val)
        ;; :db.type/symbol — analogous to keyword, no SQL literal.
          (and (= vtype :db.type/symbol) (string? val))
          (if (clojure.string/blank? val) val (symbol val))
          (and (= vtype :db.type/symbol) (symbol? val)) val
          (and (= vtype :db.type/symbol) (keyword? val))
          (symbol (namespace val) (name val))
        ;; :db.type/uuid — accept already-UUID values (param-bound or
        ;; from CAST) directly; text is read by uuid_in.
          (and (= vtype :db.type/uuid) (instance? java.util.UUID val)) val
          (and (= vtype :db.type/uuid) (string? val))
          (input/parse-uuid val)
        ;; jsonb: serialize Clojure maps/vectors to JSON strings for :db.type/string columns
          (and (= vtype :db.type/string) (or (map? val) (sequential? val)))
          (jb/serialize-jsonb val)
          (and (= vtype :db.type/string) (not (string? val)))
          (types/->pg-text val nil)
        ;; bytea: decode PG `\xHEX` hex literal to byte array; fall back to
        ;; raw UTF-8 bytes for non-hex strings so the value stays representable.
          (and (= vtype :db.type/bytes) (string? val))
          (or (parse-bytea-hex val) (.getBytes ^String val "UTF-8"))
          (and (= vtype :db.type/bytes) (bytes? val)) val
        ;; Numeric/decimal: bigdec via coerce-numeric — raises 22P02 on
        ;; bad-syntax strings instead of silently keeping the original.
          (and (= vtype :db.type/bigdec) (or (string? val) (number? val)))
          (types/numeric-value->storage
           (apply-numeric-typmod (coerce/coerce-numeric val :bigdec)
                                 num-prec num-scale))
          (and (= vtype :db.type/instant) (string? val))
          (expr/parse-timestamp-string val)
          (and (= vtype :db.type/instant) (instance? java.util.Date val)) val
        ;; java.time.* — produced by SQL casts (`::date`, `::timestamp`)
        ;; and by parameterized queries when the wire layer decodes
        ;; PG's date/timestamp/timestamptz types. Datahike's
        ;; :db.type/instant requires java.util.Date specifically.
          (and (= vtype :db.type/instant) (instance? java.time.Instant val))
          (java.util.Date/from ^java.time.Instant val)
          (and (= vtype :db.type/instant) (instance? java.time.LocalDate val))
          (java.util.Date/from
           (.toInstant (.atStartOfDay ^java.time.LocalDate val
                                      java.time.ZoneOffset/UTC)))
          (and (= vtype :db.type/instant) (instance? java.time.LocalDateTime val))
          (java.util.Date/from
           (.toInstant ^java.time.LocalDateTime val
                       java.time.ZoneOffset/UTC))
          (and (= vtype :db.type/instant) (instance? java.time.OffsetDateTime val))
          (java.util.Date/from (.toInstant ^java.time.OffsetDateTime val))
          (and (= vtype :db.type/instant) (instance? java.time.ZonedDateTime val))
          (java.util.Date/from (.toInstant ^java.time.ZonedDateTime val))
          :else val)))))

(def ^:private dml-scalar-cache
  "One InitPlan per uncorrelated scalar subquery of a DML statement,
   keyed by the statement's time: PostgreSQL runs such a subquery once,
   however many rows the statement writes."
  (ThreadLocal.))

(defn- reject-grouping!
  "Reject aggregate and window calls in `expr`, an expression computing
   one row's value -- RETURNING, an UPDATE's SET list, ON CONFLICT's
   (parse_agg.c's EXPR_KIND_RETURNING / EXPR_KIND_UPDATE_SOURCE).
   Nested SELECTs are their own level and may group."
  [expr context]
  (when (seq (params/ast-window-names expr))
    (throw (ex-info (str "window functions are not allowed in " context)
                    {:sqlstate "42P20"})))
  (when (some fns/aggregate-function? (params/ast-function-names expr))
    (throw (errors/pg-error
            :grouping-error
            {:message (str "aggregate functions are not allowed in " context)}))))

(defn- dml-scalar-correlated?
  [inner]
  (let [aliases (set (keys params/*from-bindings*))
        qualified (expr/correlated-subquery-refs inner aliases)
        directly-qualified
        (some (fn [^Column col]
                (when-let [qualifier (some-> col .getTable .getName unquote-ident)]
                  (contains? aliases qualifier)))
              (params/ast-columns inner))
        ;; ANY bare name the outer row also exposes, FROM clause or not: an
        ;; unqualified reference reaches the outer level whenever the inner
        ;; level lacks the column (params/*outer-scope-aliases*). Counting
        ;; one the inner level would have answered only forgoes the
        ;; once-per-statement cache; missing a real one would reuse one
        ;; row's answer for every row.
        unqualified (some (fn [^Column col]
                            (and (str/blank? (some-> col .getTable .getName))
                                 (seq (params/binding-column-owners
                                       params/*from-bindings*
                                       (unquote-ident (.getColumnName col))
                                       nil))))
                          (when (instance? PlainSelect inner)
                            (mapcat params/ast-columns
                                    (remove nil? (expr/plain-select-scope-nodes inner)))))]
    (boolean (or (seq qualified) directly-qualified unqualified))))

(defn- strict-scalar-value
  [inner schema db parse-fn]
  (when (and db parse-fn)
    (let [;; Everything in *from-bindings* -- the UPDATE target row and
          ;; its FROM rows -- belongs to the ENCLOSING statement, so it is
          ;; an outer level for this subquery: an inner column shadows it,
          ;; and it answers a bare name only when the inner has none. This
          ;; replaces a "does the subquery have a FROM clause" test that
          ;; approximated the same rule.
          evaluate #(binding [params/*lateral-outer-aliases*
                              (set (keys params/*from-bindings*))
                              params/*from-source-aliases* nil
                              params/*outer-scope-aliases*
                              (set (keys params/*from-bindings*))]
                      (expr/strict-scalar-subquery parse-fn inner schema db))
          v (if (dml-scalar-correlated? inner)
              (evaluate)
              (if-let [token params/*statement-time*]
                (let [state (.get dml-scalar-cache)
                      cache (if (and state (identical? token (:token state)))
                              (:values state)
                              {})
                      ;; One InitPlan per scalar-subquery OCCURRENCE. Two
                      ;; syntactically identical `(SELECT random())` nodes in
                      ;; the same UPDATE are independent in PostgreSQL, while
                      ;; this AST object is stable across all target rows.
                      key inner]
                  (if (contains? cache key)
                    (get cache key)
                    (let [value (evaluate)
                          values (assoc cache key value)]
                      (.set dml-scalar-cache {:token token :values values})
                      value)))
                (evaluate)))]
      (when-not (= :__null__ v) v))))

(defn extract-returning
  "Preserve a RETURNING target list as typed descriptors.

   RETURNING is a SELECT-like projection, not a list of column names: it
   admits `*`, `table.*`, arbitrary expressions and aliases.  Keeping the
   expression AST here lets the write executor evaluate the projection over
   each affected row and lets Describe infer the same result shape before
   execution."
  [returning-clause]
  (when returning-clause
    (mapv (fn [^SelectItem item]
            (let [item-expr (.getExpression item)]
              (when (contains-target-list-srf? item-expr)
                (throw (errors/pg-error
                        :feature-not-supported
                        {:message "set-returning functions are not allowed in RETURNING"})))
              (reject-grouping! item-expr "RETURNING")
              ;; A sequence advance commits outside the statement, and
              ;; RETURNING is projected over a SPECULATIVE db so that an
              ;; error in it still aborts the write. Advancing from there
              ;; invalidates that db (40001). It needs the reservation
              ;; INSERT's sequence defaults make before the write.
              (when (contains? (params/ast-function-names item-expr) "nextval")
                (throw (errors/pg-error
                        :feature-not-supported
                        {:feature "nextval() in RETURNING"})))
              (cond
                (instance? AllColumns item-expr)
                {:kind :star}

                (instance? AllTableColumns item-expr)
                {:kind :star
                 :table (some-> ^AllTableColumns item-expr .getTable .getName unquote-ident)}

                :else
                {:kind :expr
                 :expr item-expr
                 :name (or (select-item-alias item) (figure-colname item-expr))})))
          returning-clause)))

(defn- reject-hidden-target-name!
  "A DML target alias completely hides the target's original relation
   name -- unless a FROM item brings that name back, which is legal
   because the alias is what the target is called then (`UPDATE t q SET
   … FROM t WHERE t.id = q.id`)."
  ([statement raw-table alias-name] (reject-hidden-target-name! statement raw-table alias-name nil))
  ([statement raw-table alias-name from-names]
   (when (and alias-name (not= alias-name raw-table)
              (not (contains? (set from-names) raw-table)))
     (when (some (fn [^Column col]
                   (when-let [table (.getTable col)]
                     (= raw-table (unquote-ident (.getName ^Table table)))))
                 (params/ast-columns statement))
       (throw (ex-info (str "invalid reference to FROM-clause entry for table \""
                            raw-table "\"")
                       {:error :undefined-table
                        :sqlstate "42P01"
                        :table raw-table
                        :hint (str "Perhaps you meant to reference the table alias \""
                                   alias-name "\".")}))))))

;; Array/type metadata lives on ident entities, independently of schema-map
;; identity. Exact catalog inputs distinguish native metadata transactions as
;; well as SQL DDL, without retaining a database snapshot in the key.
(def ^:private enriched-schema-cache
  (pg-cache/bounded-cache 64))

(defn invalidate-enriched-schema-cache!
  "Clear the array-meta/typmod enriched-schema cache. Called from the
   server's DDL exec path."
  []
  (.clear ^java.util.Map enriched-schema-cache))

(defn- compute-array-meta-enriched [schema db]
  (let [pg-meta (try
                  (into {}
                        (map (fn [[ident elem ndim]]
                               [ident (cond-> {}
                                        elem (assoc :pg/array-elem elem)
                                        ndim (assoc :pg/array-ndim ndim))]))
                        (d/q
                         '{:find  [?ident ?elem ?ndim]
                           :where [[?e :db/ident ?ident]
                                   [?e :pg/array-elem ?elem]
                                   [(get-else $ ?e :pg/array-ndim 1) ?ndim]]}
                         db))
                  (catch Throwable _ {}))
        ;; NUMERIC(p,s): surface the declared scale so INSERT coercion can
        ;; round/pad values to it (PG numeric(p,s) input semantics). Only
        ;; constrained columns carry :pg/typmod; unconstrained `numeric`
        ;; has none, so its scale is left intact.
        ;; Precision as well as scale. `p` was decoded here and thrown
        ;; away, which is why 22003 numeric field overflow was never
        ;; raised on the INSERT path -- the value was rounded to scale
        ;; and then stored however big it was.
        scale-meta (try
                     (into {}
                           (keep (fn [[ident typmod]]
                                   (let [[p s] (types/decode-numeric-typmod typmod)]
                                     (when s [ident (cond-> {:pg/numeric-scale s}
                                                      p (assoc :pg/numeric-precision p))]))))
                           (d/q
                            '{:find  [?ident ?typmod]
                              :where [[?e :db/ident ?ident]
                                      [?e :pg/typmod ?typmod]]}
                            db))
                     (catch Throwable _ {}))
        ;; :pg/type — the original SQL type when the datahike valueType isn't 1:1
        ;; (jsonb/json both reduce to :db.type/string; date/time/timestamp to
        ;; :db.type/instant). Surfaced so INSERT coercion can tell a jsonb column
        ;; from a plain text column and canonicalize it (both are :db.type/string).
        type-meta (try
                    (into {}
                          (map (fn [[ident pgtype]] [ident {:pg/type pgtype}]))
                          (d/q
                           '{:find  [?ident ?pgtype]
                             :where [[?e :db/ident ?ident]
                                     [?e :pg/type ?pgtype]]}
                           db))
                    (catch Throwable _ {}))
        ;; :pg/typmod itself, not only the decoded numeric scale -- the
        ;; varchar(n) length check reads it directly, and INSERT has no
        ;; db to fall back on.
        typmod-meta (try
                      (into {}
                            (map (fn [[ident tm]] [ident {:pg/typmod tm}]))
                            (d/q
                             '{:find  [?ident ?tm]
                               :where [[?e :db/ident ?ident]
                                       [?e :pg/typmod ?tm]]}
                             db))
                      (catch Throwable _ {}))]
    (reduce-kv (fn [s ident more] (update s ident merge more))
               schema (merge-with merge pg-meta scale-meta type-meta typmod-meta))))

(defn enrich-schema-with-pg-array-meta
  "Datahike's `:schema` map only carries `:db/*` keys; pgwire-side
   metadata like `:pg/array-elem` lives as ident-entity facts. For
   array column INSERTs we need that metadata available via
   `(get-in schema [attr :pg/array-elem])`, so this helper queries
   db for every ident's array-elem/ndim and merges the results into
   the schema map. Memoised by the supplied schema and exact catalog inputs."
  [schema db]
  (if (nil? db)
    schema
    (let [^java.util.Map outer enriched-schema-cache
          k [schema (catalog-basis/capture db)]]
      (or (.get outer k)
          (locking outer
            (or (.get outer k)
                (let [enriched (compute-array-meta-enriched schema db)]
                  (.put outer k enriched)
                  enriched)))))))

(defn- constraint-name->conflict-cols
  "Resolve `ON CONFLICT ON CONSTRAINT <name>` to the attributes that
   constraint covers, or nil when we can't.

   We synthesize constraint names for pg_constraint the way PG's
   defaults read — `<table>_pkey` for the primary key and
   `<table>_<column>_key` for a UNIQUE column — so the reverse mapping
   is just as mechanical. Only unique-ish constraints can be an ON
   CONFLICT arbiter in PG anyway.

   Returning nil here is the caller's cue to raise, NOT to fall back to
   an empty conflict column list — an empty list means \"nothing to
   compare\", i.e. the row never conflicts, so an unrecognised
   constraint name would silently turn an upsert into a plain insert
   that then overwrites the existing row."
  [constraint-name table-name ns schema db]
  (let [cname (str/lower-case (unquote-ident constraint-name))
        descriptor (some #(when (and (= table-name (:table %))
                                     (= cname (str/lower-case (:name %))))
                            %)
                         (unique-constraints/index-descriptors db))
        cols (pgs/column-info schema table-name db)
        unique-cols (filter :unique cols)]
    (cond
      descriptor
      (:attrs descriptor)

      (= cname (str/lower-case (str table-name "_pkey")))
      (when-let [pk (first (filter #(= :db.unique/identity
                                       (get-in schema [(:attr %) :db/unique]))
                                   unique-cols))]
        [(:attr pk)])

      :else
      (when-let [c (first (filter #(= cname
                                      (str/lower-case
                                       (str table-name "_" (:name %) "_key")))
                                  unique-cols))]
        [(:attr c)]))))

(declare canonical-relation)

(defn- resolve-conflict-target
  "Turn an ON CONFLICT target into the vector of attributes to arbitrate
   on, raising 0A000 for the forms we don't implement.

   Returns nil for a targetless `ON CONFLICT`, which PG defines as
   \"any unique constraint\" and the callers approximate by comparing
   every inserted column.

   The unimplemented forms used to be parsed and then silently dropped,
   which is the worst possible handling: `ON CONFLICT ON CONSTRAINT
   t_pkey DO NOTHING` degraded to an empty arbiter list — never
   conflicts — so the insert proceeded and Datahike's
   :db.unique/identity upsert overwrote the row the statement was
   explicitly asking to leave alone. Raising loses the statement;
   silence lost the data."
  [conflict-target table-name ns schema db]
  (when conflict-target
    (let [^net.sf.jsqlparser.statement.insert.InsertConflictTarget ct conflict-target
          idx-cols (.getIndexColumnNames ct)
          cname (.getConstraintName ct)]
      (when (.getWhereExpression ct)
        ;; `ON CONFLICT (col) WHERE pred` names a PARTIAL index as the
        ;; arbiter. Honouring it needs index metadata we don't keep, and
        ;; ignoring it changes which rows are treated as conflicting.
        (throw (ex-info "ON CONFLICT with an index predicate is not supported"
                        {:error :feature-not-supported :sqlstate "0A000"})))
      (cond
        (seq idx-cols)
        (let [attrs (vec
                     (distinct
                      (map (fn [column]
                             (let [logical-name (unquote-ident column)
                                   [_ [storage-name]]
                                   (canonical-relation
                                    schema (pgs/schema-hints db)
                                    table-name [logical-name])]
                               (ctx/resolve-inherited-attr
                                (keyword table-name storage-name) schema db)))
                           idx-cols)))
              unique-colsets
              (into #{}
                    (keep (fn [[attr m]]
                            (when (and (keyword? attr)
                                       (= table-name (namespace attr))
                                       (some? (:db/unique m)))
                              (if (= :db.type/tuple (:db/valueType m))
                                (set (:db/tupleAttrs m))
                                #{attr}))))
                    schema)
              unique-colsets (into unique-colsets
                                   (map set)
                                   (unique-constraints/unique-arbiters
                                    db table-name))]
          (when-not (contains? unique-colsets (set attrs))
            (throw (ex-info
                    "there is no unique or exclusion constraint matching the ON CONFLICT specification"
                    {:error :invalid-column-reference :sqlstate "42P10"})))
          attrs)

        cname
        (or (constraint-name->conflict-cols cname table-name ns schema db)
            (throw (ex-info (str "constraint \"" (unquote-ident cname)
                                 "\" for table \"" table-name "\" does not exist")
                            {:error :undefined-object :sqlstate "42704"})))

        :else nil))))

(defn- conflict-update-assignments
  "Validate and flatten ON CONFLICT DO UPDATE's SET list.

   Simple tuple assignment is represented by parallel column/value lists and
   can be lowered pairwise. A row-subquery has multiple targets but one value
   expression; reject that explicitly instead of applying only its first pair."
  [^net.sf.jsqlparser.statement.insert.InsertConflictAction conflict-action
   table-name ns schema db]
  (let [assignments
        (mapv
         identity
         (mapcat
          (fn [^UpdateSet us]
            (let [cols (vec (.getColumns us))
                  values (vec (.getValues us))]
              (when-not (= (count cols) (count values))
                (throw (ex-info "multi-column ON CONFLICT update from a row expression is not supported"
                                {:error :feature-not-supported :sqlstate "0A000"})))
              (map (fn [^Column col value-expr]
                     ;; ON CONFLICT's SET list computes one row's values,
                     ;; like an UPDATE's, and PostgreSQL names it that way.
                     (reject-grouping! value-expr "UPDATE")
                     (when-let [^Table qualifier (.getTable col)]
                       (throw (ex-info
                               (str "column \"" (unquote-ident (.getName qualifier))
                                    "\" of relation \"" table-name "\" does not exist")
                               {:error :undefined-column
                                :sqlstate "42703"
                                :hint "SET target columns cannot be qualified with the relation name."})))
                     (let [col-name (unquote-ident (.getColumnName col))
                           storage-name (second (canonical-relation
                                                 schema (pgs/schema-hints db)
                                                 table-name [col-name]))
                           attr (ctx/resolve-inherited-attr
                                 (keyword table-name (first storage-name)) schema db)]
                       (doseq [^Column ref (params/ast-columns value-expr)
                               :let [qualifier (some-> ref .getTable .getName unquote-ident)
                                     ref-name (unquote-ident (.getColumnName ref))]
                               :when (and (= "excluded" (some-> qualifier str/lower-case))
                                          (let [ci (pgs/ci-index schema (pgs/schema-hints db))]
                                            (nil? (pgs/canonical-attr ci table-name ref-name))))]
                         (throw (ex-info (str "column excluded." ref-name " does not exist")
                                         {:error :undefined-column
                                          :sqlstate "42703"
                                          :column ref-name})))
                       {:attr attr :col-name col-name :value-expr value-expr}))
                   cols values)))
          (.getUpdateSets conflict-action)))
        duplicate (some (fn [[attr n]] (when (> n 1) attr))
                        (frequencies (map :attr assignments)))]
    (when duplicate
      (throw (ex-info (str "multiple assignments to same column \""
                           (name duplicate) "\"")
                      {:error :syntax-error
                       :sqlstate "42601"
                       :column (name duplicate)})))
    assignments))

(defn- canonical-relation
  "Resolve a folded table name back to the name it is STORED under, and
   each column name likewise.

   References arrive case-folded (PostgreSQL folds unquoted identifiers),
   but storage may not be: a database created before folding landed holds
   `:MixedCase/ColA`, and a Datalog-native one holds whatever its
   attributes were named. Identity when the two already agree, which is
   every database created by a current pg-datahike.

   The WRITE paths need this as much as the read paths do, and more
   urgently: an INSERT that folded without resolving would assert
   `:mixedcase/cola` alongside an existing `:MixedCase/ColA` and split
   the table in two, with half the rows invisible to any single query and
   no error at any point."
  ([schema tname col-names]
   (canonical-relation schema nil tname col-names))
  ([schema hints tname col-names]
   (let [ci (pgs/ci-index schema hints)
         t (let [c (pgs/canonical-table ci tname)]
             (if (pgs/ambiguous? c) tname c))]
     [t (mapv (fn [c]
                (let [a (pgs/canonical-attr ci t c)]
                  (cond
                    (and a (not (pgs/ambiguous? a))) (name a)
                    (pgs/registered-relation? ci t)
                    (throw (ex-info (str "column \"" c "\" does not exist")
                                    {:error :undefined-column
                                     :sqlstate "42703"
                                     :column c}))
                    :else c)))
              col-names)])))

(defn- conflict-set-params
  "Build the explicit db.fn argument used to carry parameters occurring in
   ON CONFLICT's SET/WHERE AST through Execute-time substitution."
  [update-assignments update-where]
  (let [idxs (into (sorted-set)
                   (concat (mapcat #(params/ast-param-indices (:value-expr %))
                                   update-assignments)
                           (when update-where
                             (params/ast-param-indices update-where))))]
    (when (seq idxs)
      (reduce (fn [v i] (assoc v (dec (long i)) (->ParamRef i)))
              (vec (repeat (long (apply max idxs)) nil))
              idxs))))

(defn- conflict-plan
  [^net.sf.jsqlparser.statement.insert.InsertConflictAction conflict-action
   conflict-target table-name ns schema db target-alias]
  (let [do-nothing? (= (.getConflictActionType conflict-action)
                       net.sf.jsqlparser.statement.insert.ConflictActionType/DO_NOTHING)
        conflict-cols (resolve-conflict-target
                       conflict-target table-name ns schema db)
        update-where (.getWhereExpression conflict-action)
        update-assignments (when-not do-nothing?
                             (conflict-update-assignments
                              conflict-action table-name ns schema db))]
    {:table-name table-name
     :ns ns
     :schema schema
     :target-alias target-alias
     :parse-fn params/*parse-sql*
     :do-nothing? do-nothing?
     :conflict-cols conflict-cols
     :update-where update-where
     :update-assignments update-assignments
     :set-params (conflict-set-params update-assignments update-where)}))

(defn- conflict-unique-specs [txdb table-name]
  (let [schema (:schema txdb)
        native
        (into []
              (keep (fn [[attr m]]
                      (when (and (keyword? attr)
                                 (= table-name (namespace attr))
                                 (some? (:db/unique m)))
                        (let [cols (if (= :db.type/tuple (:db/valueType m))
                                     (vec (:db/tupleAttrs m))
                                     [attr])]
                          {:cols cols
                           :name (if (= :db.unique/identity (:db/unique m))
                                   (str table-name "_pkey")
                                   (str table-name "_"
                                        (str/join "_" (map name cols)) "_key"))}))))
              schema)
        durable (mapv (fn [{:keys [attrs name]}]
                        {:cols attrs :name name})
                      (filter #(= table-name (:table %))
                              (unique-constraints/index-descriptors txdb)))]
    (->> (concat native durable)
         (reduce (fn [out spec] (assoc out (:cols spec) spec)) {})
         vals
         vec)))

(defn- targetless-conflict-arbiters [txdb table-name]
  (mapv :cols (conflict-unique-specs txdb table-name)))

(defn- arbiter-entry [attrs cols]
  (let [values (mapv #(get attrs %) cols)]
    (when (and (seq cols) (every? some? values))
      {:cols cols
       :values values
       :key (mapv (fn [attr value]
                    [attr (unique-constraints/canonical-key-value value)])
                  cols values)})))

(defn- conflict-row-map [txdb eid]
  (into {} (map (fn [^Datom d] [(.-a d) (.-v d)]))
        (d/datoms txdb :eavt eid)))

(defn- cardinality-violation! []
  (throw (ex-info
          "ON CONFLICT DO UPDATE command cannot affect row a second time"
          {:error :cardinality-violation
           :sqlstate "21000"
           :hint (str "Ensure that no rows proposed for insertion within "
                      "the same command have duplicate constrained values.")})))

(defn- conflict-unique-violation!
  [table-name {constraint-name :name :keys [cols values]}]
  (throw (ex-info "unique violation"
                  {:error :unique-violation
                   :sqlstate "23505"
                   :table table-name
                   :constraint constraint-name
                   :columns (mapv clojure.core/name cols)
                   :value values})))

(defn- evaluate-conflict-update
  [txdb attrs old-map set-params
   {:keys [table-name ns schema target-alias parse-fn
           update-where update-assignments]}]
  (let [logical-columns (remove #(= :db/id (:attr %))
                                (pgs/column-info schema table-name txdb))
        value-for (fn [row {:keys [name attr]}]
                    (let [resolved (ctx/resolve-inherited-attr attr schema txdb)
                          storage (or (when (contains? row resolved) resolved)
                                      (some #(when (and (keyword? %)
                                                        (= name (clojure.core/name %))) %)
                                            (keys row)))]
                      (when storage (get row storage))))
        target-logical (into {}
                             (map (fn [{:keys [name] :as column}]
                                    [(keyword table-name name)
                                     (value-for old-map column)]))
                             logical-columns)
        excluded-map (merge
                      (into {}
                            (map (fn [{:keys [name] :as column}]
                                   [(keyword "excluded" name)
                                    (value-for attrs column)]))
                            logical-columns)
                      ;; Inherited columns are stored under the parent's
                      ;; namespace.  Keep those concrete candidate values
                      ;; authoritative when a child-column lookup above has
                      ;; no direct value.
                      (into {} (map (fn [[attr value]]
                                      [(keyword "excluded" (name attr)) value]))
                            attrs))
        combined (merge old-map target-logical excluded-map)
        ;; The conflicting row and `excluded` are two relations in the
        ;; translator's scope (ExecOnConflictUpdate binds the existing
        ;; tuple and the proposed one), so the whole SET list is ONE
        ;; projection over them -- not an expression at a time through a
        ;; second evaluator.
        row-scope (fn [asts]
                    (binding [params/*bound-params* (or set-params params/*bound-params*)]
                      ((requiring-resolve 'datahike.pg.sql.row-eval/row-values)
                       asts combined ns schema txdb
                       {:alias target-alias :excluded? true})))]
    (when (or (nil? update-where)
              (true? (let [v (first (:values (row-scope [update-where])))]
                       (when (some? v) (boolean v)))))
      (reduce
       (fn [{:keys [row-after] :as result}
            [{:keys [attr]} new-val]]
         ;; PostgreSQL evaluates every SET RHS against the pre-update target
         ;; row, not against assignments earlier in the same SET list.
         (let [attr (or (when (contains? old-map attr) attr)
                        (some #(when (and (keyword? %)
                                          (= (name attr) (name %))) %)
                              (keys old-map))
                        attr)
               old-val (get old-map attr)
               coerced (when (some? new-val)
                         (coerce-insert-value new-val attr schema))]
           (cond-> (if (nil? new-val)
                     (assoc result :row-after (dissoc row-after attr))
                     (assoc-in result [:row-after attr] coerced))
             (and (nil? new-val) (some? old-val))
             (update :ops conj [:db/retract (:db/id result) attr old-val])

             (some? new-val)
             (update :ops conj [:db/add (:db/id result) attr coerced]))))
       {:db/id (:db/id old-map) :ops [] :row-after old-map}
       (map vector update-assignments
            (:values (row-scope (mapv :value-expr update-assignments))))))))

(defn- materialize-conflict-defaults
  [txdb attrs constraint-plan schema]
  (row-constraints/prepare-candidate
   attrs constraint-plan
   (fn [value attr]
     (coerce-insert-value value attr schema txdb))))

(defn- validate-conflict-row-constraints!
  [txdb table-name attrs constraint-plan effective-rows include-fk?]
  ;; row-eval sits above this namespace (it translates through
  ;; datahike.pg.sql), hence the runtime resolve.
  (let [eval-check ((requiring-resolve 'datahike.pg.sql.row-eval/check-fn) txdb)]
    (if include-fk?
      (row-constraints/validate-mutation!
       txdb table-name attrs constraint-plan effective-rows eval-check nil)
      (row-constraints/validate-pre-arbiter!
       txdb table-name attrs constraint-plan eval-check nil))))

(defn- reduce-on-conflict
  [txdb row-attrs set-params
   {:keys [table-name conflict-cols do-nothing?] :as plan}
   & [initial]]
  (let [cached-context (::conflict-context initial)
        unique-specs (or (:unique-specs cached-context)
                         (conflict-unique-specs txdb table-name))
        arbiters (if conflict-cols
                   [conflict-cols]
                   (mapv :cols unique-specs))
        spec-by-cols (into {} (map (juxt :cols identity)) unique-specs)
        constraint-plan (or (:constraint-plan cached-context)
                            (row-constraints/constraint-plan txdb table-name))
        entries-for (fn [attrs specs]
                      (into []
                            (keep (fn [{:keys [cols name]}]
                                    (some-> (arbiter-entry attrs cols)
                                            (assoc :name name))))
                            specs))
        arbiter-specs (mapv (fn [cols]
                              (or (get spec-by-cols cols) {:cols cols}))
                            arbiters)
        matching-current
        (fn [key->ref effective-rows {:keys [cols values key]}]
          (or (get key->ref key)
              (when-let [eid (unique-constraints/conflicting-eid
                              txdb table-name cols values)]
                (if-let [effective (get effective-rows eid)]
                  (when (= key (:key (arbiter-entry effective cols))) eid)
                  eid))))]
    (reduce
     (fn [{:keys [key->ref affected-existing effective-rows]
           :as result}
          raw-attrs]
       (let [{attrs :attrs}
             (materialize-conflict-defaults
              txdb raw-attrs constraint-plan (:schema txdb))
             _ (validate-conflict-row-constraints!
                txdb table-name attrs constraint-plan effective-rows false)
             arbiter-entries (entries-for attrs arbiter-specs)
             all-entries (entries-for attrs unique-specs)
             existing (some #(matching-current key->ref effective-rows %)
                            arbiter-entries)]
         (cond
           existing
           (cond
             do-nothing? result

             (or (string? existing) (contains? affected-existing existing))
             (cardinality-violation!)

             :else
             (let [old-map (assoc (or (get effective-rows existing)
                                      (conflict-row-map txdb existing))
                                  :db/id existing)]
               (if-let [{:keys [ops row-after]}
                        (evaluate-conflict-update txdb attrs old-map set-params plan)]
                 (let [post-entries (entries-for row-after unique-specs)
                       _ (validate-conflict-row-constraints!
                          txdb table-name row-after constraint-plan
                          (assoc effective-rows existing row-after) false)
                       _ (doseq [entry post-entries
                                 :let [other (matching-current
                                              key->ref effective-rows entry)]
                                 :when (and other (not= existing other))]
                           (conflict-unique-violation! table-name entry))
                       without-old (into {}
                                         (remove (fn [[_ ref]] (= existing ref)))
                                         key->ref)
                       with-post (reduce #(assoc %1 (:key %2) existing)
                                         without-old post-entries)]
                   (-> result
                       (update :tx-data into ops)
                       (update :row-refs conj existing)
                       (update :affected inc)
                       (update :mutated-rows conj row-after)
                       (assoc :key->ref with-post)
                       (update :affected-existing conj existing)
                       (assoc-in [:effective-rows existing] row-after)))
                 result)))

           :else
           (let [_ (doseq [entry all-entries
                           :let [other (matching-current
                                        key->ref effective-rows entry)]
                           :when other]
                     (conflict-unique-violation! table-name entry))
                 tempid (str (gensym "upsert-"))
                 clean-attrs (if (:preserve-nulls? plan)
                               attrs
                               (into {} (remove (comp nil? val)) attrs))
                 inserted-entries all-entries]
             (-> result
                 (update :tx-data conj (assoc clean-attrs :db/id tempid))
                 (update :row-refs conj tempid)
                 (update :affected inc)
                 (update :mutated-rows conj attrs)
                 (assoc-in [:effective-rows tempid] attrs)
                 (update :key->ref
                         #(reduce (fn [m entry] (assoc m (:key entry) tempid))
                                  % inserted-entries)))))))
     (assoc (or initial
                {:tx-data []
                 :row-refs []
                 :affected 0
                 :mutated-rows []
                 :key->ref {}
                 :affected-existing #{}
                 :effective-rows {}})
            ::conflict-context {:unique-specs unique-specs
                                :constraint-plan constraint-plan})
     row-attrs)))

(defn- on-conflict-result
  [row-attrs row-count returning plan]
  (let [row-refs (atom [])
        affected (atom 0)
        set-params (:set-params plan)
        candidate-step (fn [txdb state candidate runtime-set-params]
                         (reduce-on-conflict
                          txdb [candidate] runtime-set-params
                          (assoc plan :preserve-nulls? true) state))
        tx-fn (fn [txdb row-attrs set-params]
                (reset! row-refs [])
                (reset! affected 0)
                (let [result (reduce-on-conflict txdb row-attrs set-params plan)]
                  (doseq [row (:mutated-rows result)]
                    (validate-conflict-row-constraints!
                     txdb (:table-name plan) row
                     (row-constraints/constraint-plan txdb (:table-name plan))
                     (:effective-rows result) true))
                  (reset! row-refs (:row-refs result))
                  (reset! affected (:affected result))
                  (:tx-data result)))]
    (cond-> {:type :insert
             :row-refs row-refs
             :affected-count affected
             :insert-mode :on-conflict
             :insert-candidates row-attrs
             :insert-candidate-step candidate-step
             :tx-data [[:db.fn/call tx-fn row-attrs set-params]]
             :count row-count
             :table (:table-name plan)
             :ns (:ns plan)}
      returning (assoc :returning returning))))

(defn- insert-select-order-cmp [order-spec]
  (fn [a b]
    (let [av (if (sequential? a) a [a])
          bv (if (sequential? b) b [b])]
      (loop [specs (partition 3 order-spec)]
        (if-let [[idx dir nulls] (first specs)]
          (let [va (nth av idx nil)
                vb (nth bv idx nil)
                a-null? (or (nil? va) (= :__null__ va))
                b-null? (or (nil? vb) (= :__null__ vb))
                nulls-first? (if nulls (= nulls :first) (= dir :desc))
                c (cond
                    (and a-null? b-null?) 0
                    a-null? (if nulls-first? -1 1)
                    b-null? (if nulls-first? 1 -1)
                    (= dir :desc) (fns/order-cmp vb va)
                    :else (fns/order-cmp va vb))]
            (if (zero? c) (recur (rest specs)) c))
          0)))))

(defn- resolve-order-keys
  [results order-spec resolve-value]
  (if (and resolve-value (seq order-spec))
    (let [idxs (mapv first (partition 3 order-spec))]
      (map (fn [row]
             (let [rv (if (sequential? row) (vec row) [row])]
               (reduce (fn [r idx]
                         (if (< idx (count r))
                           (update r idx resolve-value)
                           r))
                       rv idxs)))
           results))
    results))

(defn- shape-insert-select-results [results parsed resolve-value]
  (let [sql-cmp (when (seq (:sql-order-by parsed))
                  (insert-select-order-cmp (:sql-order-by parsed)))
        ;; An ORDER BY key is evaluated for every input row before LIMIT. Only
        ;; resolve those key cells here; non-key volatile projections remain
        ;; deferred until their candidate survives shaping.
        results (resolve-order-keys results (:sql-order-by parsed) resolve-value)
        results (if sql-cmp (sort sql-cmp results) results)
        results (if sql-cmp
                  (cond->> results
                    (:sql-offset parsed) (drop (:sql-offset parsed))
                    (:sql-limit parsed) (take (:sql-limit parsed)))
                  results)
        results (if-let [specs (seq (:project-set parsed))]
                  (apply-project-set results specs)
                  results)
        results (resolve-order-keys results (:project-order-by parsed) resolve-value)
        project-cmp (when (seq (:project-order-by parsed))
                      (insert-select-order-cmp (:project-order-by parsed)))
        results (if project-cmp (sort project-cmp results) results)]
    (if (seq (:project-set parsed))
      (cond->> results
        (:project-offset parsed) (drop (:project-offset parsed))
        (:project-limit parsed) (take (:project-limit parsed)))
      results)))

(defn insert-sequence-defaults
  "Return storage-attr -> sequence-name for INSERT defaults which must reserve
   a value before the surrounding statement transaction runs. Identity
   sequences are distinguished by their durable generation metadata, so a
   user-created sequence which merely follows the table_column_seq naming
   convention is not treated as an identity."
  [db table-name]
  (if-not (and db (get (:schema db) :__seq__/name))
    {}
    (let [schema (:schema db)
          tables [table-name]
          nextval-defaults
          (into {}
                (keep (fn [{:keys [attr default]}]
                        (let [[kind _ sequence-name] default]
                          (when (= :nextval kind)
                            [attr sequence-name]))))
                (row-constraints/column-specs db table-name))
          identity-sequences
          (if (get schema :__seq__/identity-generation)
            (d/q '{:find [?name]
                   :where [[?sequence :__seq__/name ?name]
                           [?sequence :__seq__/identity-generation ?generation]]}
                 db)
            [])
          identity-defaults
          (into {}
                (mapcat
                 (fn [table]
                   (let [prefix (str table "_")]
                     (keep (fn [[sequence-name]]
                             (let [suffix-start (- (count sequence-name) 4)]
                               (when (and (str/starts-with? sequence-name prefix)
                                          (str/ends-with? sequence-name "_seq")
                                          (< (count prefix) suffix-start))
                                 (let [attr (keyword
                                             table
                                             (subs sequence-name
                                                   (count prefix) suffix-start))]
                                   (when (get schema attr)
                                     [attr sequence-name])))))
                           identity-sequences)))
                 tables))]
      (merge identity-defaults nextval-defaults))))

(defn- populate-insert-sequence-defaults [attrs defaults]
  (reduce-kv (fn [row attr sequence-name]
               ;; A present nil is explicit SQL NULL and suppresses a default.
               (if (contains? row attr)
                 row
                 (assoc row attr {:fn :nextval :seq-name sequence-name
                                  :generated-default? true})))
             attrs defaults))

(defn- validate-insert-row-widths!
  "Require every VALUES/SELECT row to match the INSERT target list.

   Defaults fill columns omitted from the target list, never expressions
   omitted from a row. Zipping columns and values silently accepted both
   `INSERT (a,b,c) VALUES (1,2)` and its over-wide inverse."
  [col-names rows]
  (let [target-count (count col-names)]
    (doseq [row rows
            :let [value-count (count row)]
            :when (not= target-count value-count)]
      (throw (ex-info (if (< value-count target-count)
                        "INSERT has more target columns than expressions"
                        "INSERT has more expressions than target columns")
                      {:error :syntax-error
                       :sqlstate "42601"
                       :target-count target-count
                       :value-count value-count}))))
  rows)

(defn materialize-insert-select
  "Execute a translated INSERT ... SELECT source against `db` and build the
   ordinary ordered INSERT candidate plan.

   Translation deliberately stores this description instead of source rows:
   PostgreSQL runs the source at Execute, and prepared statements must observe
   the current statement snapshot. Stateful projection calls remain inert
   markers here and are resolved candidate-by-candidate by the server."
  [{:keys [source col-names source-attrs table-name ns ancestor-tables
           sequence-defaults conflict-plan returning]}
   db resolve-value]
  (let [source-db db
        query (:query source)
        in-args (:in-args source)
        raw-results (cond
                      (:literal-rows source) (:literal-rows source)
                      (:literal-row source) [(:literal-row source)]
                      (seq in-args) (apply d/q query source-db in-args)
                      :else (d/q query source-db))
        results (shape-insert-select-results raw-results source resolve-value)
        hidden-count (long (or (:hidden-count source) 0))
        results (if (pos? hidden-count)
                  (map (fn [row]
                         (let [v (if (sequential? row) (vec row) [row])]
                           (subvec v 0 (- (count v) hidden-count))))
                       results)
                  results)
        [results _] (defer-compound-projections results
                                                (:find-aliases source)
                                                query in-args
                                                (:compound-exprs source))
        ;; Plain DISTINCT is above projection: non-injective expressions such
        ;; as CAST('01' AS int) must deduplicate their concrete output, not the
        ;; raw leaf values carried through Datalog. This stage necessarily
        ;; evaluates every source row before ModifyTable sees a candidate.
        results (if (and (:has-distinct? source)
                         (nil? (:distinct-on-n source)))
                  (distinct (map resolve-value results))
                  results)
        rows (validate-insert-row-widths!
              col-names
              (mapv (fn [row]
                      (if (sequential? row) (vec row) [row]))
                    results))
        row-attrs
        (mapv (fn [row]
                (into {}
                      (keep (fn [[attr val]]
                              (let [value (when-not (= :__null__ val) val)]
                                (when (or (some? value)
                                          (nil? val)
                                          (= :__null__ val))
                                  [attr value]))))
                      (map vector source-attrs row)))
              rows)
        marker-attrs (into [(pgs/row-marker-attr table-name)]
                           (map pgs/row-marker-attr ancestor-tables))
        row-attrs (mapv (fn [row]
                          (reduce (fn [r marker]
                                    (if (get (:schema db) marker)
                                      (assoc r marker true)
                                      r))
                                  row marker-attrs))
                        row-attrs)
        row-attrs (mapv #(populate-insert-sequence-defaults
                          % sequence-defaults)
                        row-attrs)
        result (if conflict-plan
                 (on-conflict-result row-attrs (count row-attrs)
                                     returning conflict-plan)
                 (let [entities (mapv (fn [attrs]
                                        (assoc attrs :db/id
                                               (str (gensym "insert-select-"))))
                                      row-attrs)]
                   (cond-> {:type :insert
                            :insert-mode :plain
                            :insert-candidates entities
                            :tx-data entities
                            :count (count entities)
                            :table table-name :ns ns}
                     returning (assoc :returning returning))))]
    (assoc result :insert-source-order source-attrs)))

(defn- expand-insert-values-srfs
  "Expand top-level SRFs in one INSERT ... VALUES row.

   PostgreSQL treats this context like an INSERT target list (unlike a
   standalone VALUES relation): same-level SRFs advance in lockstep, shorter
   results are NULL-padded, and an all-empty level produces no input row."
  [row-exprs eval-fn]
  (let [cells (mapv (fn [e]
                      (if (target-list-srf? e)
                        (let [tf (net.sf.jsqlparser.statement.select.TableFunction.
                                  ^Function e)
                              materialized (materialize-table-function tf eval-fn nil)]
                          {:set-values (mapv first (:rows materialized))})
                        {:scalar-value
                         (try
                           (eval-fn e)
                           (catch Exception ex
                             ;; Constant/expression failures belong to row
                             ;; execution, not translation. Keeping the error
                             ;; as an ordered marker lets an earlier nextval in
                             ;; the same target list retain its PostgreSQL side
                             ;; effect before this value raises.
                             {:fn :raise
                              :message (ex-message ex)
                              :data (or (ex-data ex)
                                        {:error :data-exception})}))}))
                    row-exprs)
        set-cells (filter :set-values cells)]
    (if (empty? set-cells)
      [(mapv :scalar-value cells)]
      (let [width (reduce max 0 (map (comp count :set-values) set-cells))]
        (mapv (fn [i]
                (mapv (fn [{:keys [set-values scalar-value]}]
                        (if set-values (nth set-values i nil) scalar-value))
                      cells))
              (range width))))))

(defn- literal-catalog-expression?
  "Closed expression vocabulary for target-local INSERT admission. Anything
   that can consult other catalog objects retains whole-catalog validation."
  ([expression] (literal-catalog-expression? expression 32))
  ([expression remaining]
   (and (pos? remaining)
        (cond
          (#{LongValue DoubleValue StringValue NullValue BooleanValue} (class expression)) true
          (= JdbcParameter (class expression))
          (every? #{16 20 21 23 25 700 701 1042 1043}
                  (vals params/*declared-param-oids*))
          (= Parenthesis (class expression))
          (literal-catalog-expression? (.getExpression ^Parenthesis expression) (dec remaining))
          (= SignedExpression (class expression))
          (literal-catalog-expression? (.getExpression ^SignedExpression expression) (dec remaining))
          (= ParenthesedExpressionList (class expression))
          (and (= 1 (count expression))
               (literal-catalog-expression? (first expression) (dec remaining)))
          :else false))))

(defn- target-delete-expression?
  "Closed target-only predicate vocabulary for DELETE admission. Qualified
   columns may name only the target or its alias; functions and subqueries
   deliberately fall back to whole-catalog validation."
  ([expression target alias]
   (target-delete-expression? expression target alias 64))
  ([expression target alias remaining]
   (and (pos? remaining)
        (or
         (nil? expression)
         (when (instance? Column expression)
           (let [qualifier (some-> (.getTable ^Column expression) .getName unquote-ident)]
             (or (str/blank? qualifier) (= target qualifier) (= alias qualifier))))
         (contains? #{LongValue DoubleValue StringValue NullValue BooleanValue}
                    (class expression))
         (and (instance? JdbcParameter expression)
              ;; Parameter coercion may consult a user-defined type.  The
              ;; target-only certificate does not observe unrelated type
              ;; catalog rows, so admit only the same closed builtin scalar
              ;; set as literal INSERT.  An empty map is fine only when this
              ;; expression is not a parameter (handled by the branch above).
              (seq params/*declared-param-oids*)
              (every? #{16 20 21 23 25 700 701 1042 1043}
                      (vals params/*declared-param-oids*)))
         (when (instance? Parenthesis expression)
           (target-delete-expression? (.getExpression ^Parenthesis expression)
                                      target alias (dec remaining)))
         ;; Even a literal CAST can name a domain or enum outside the target
         ;; relation.  Keep all casts on whole-catalog validation until the
         ;; certificate explicitly observes their type dependencies.
         (when (instance? NotExpression expression)
           (target-delete-expression? (.getExpression ^NotExpression expression)
                                      target alias (dec remaining)))
         (when (instance? IsNullExpression expression)
           (target-delete-expression? (.getLeftExpression ^IsNullExpression expression)
                                      target alias (dec remaining)))
         (when (contains? #{AndExpression OrExpression EqualsTo NotEqualsTo
                            GreaterThan GreaterThanEquals MinorThan MinorThanEquals}
                          (class expression))
           (let [binary ^net.sf.jsqlparser.expression.BinaryExpression expression]
             (and (target-delete-expression? (.getLeftExpression binary)
                                             target alias (dec remaining))
                  (target-delete-expression? (.getRightExpression binary)
                                             target alias (dec remaining)))))))))

(defn translate-insert
  "Translate an INSERT statement to Datahike transaction data.
   Supports single-row and multi-row VALUES, with or without column list.
  Handles ON CONFLICT (UPSERT) via :db.fn/call for atomic execution."
  [^Insert insert schema db]
  (let [schema (enrich-schema-with-pg-array-meta schema db)
        table (.getTable insert)
        raw-table (unquote-ident (.getName ^Table table))
        _ (when-not (stored-relation-known? schema raw-table)
            (throw (ex-info (str "relation \"" raw-table "\" does not exist")
                            {:error :undefined-table
                             :sqlstate "42P01"
                             :table raw-table})))
        columns (.getColumns insert)
        ;; Session temp relations are presented under their logical SQL name,
        ;; while inheritance/order metadata is stored under the unique
        ;; physical namespace. The visible schema carries that lookup only as
        ;; metadata so it cannot be mistaken for a column.
        temp-table-map params/*temp-table-map*
        physical->logical (set/map-invert temp-table-map)
        db-table (get temp-table-map raw-table raw-table)
        ancestor-db-tables (ctx/inheritance-ancestors db db-table)
        ancestor-tables (mapv #(get physical->logical % %) ancestor-db-tables)
        raw-cols (if (seq columns)
                   (mapv #(unquote-ident (.getColumnName ^Column %)) columns)
                   (let [db-order (pgs/column-order-from-db db db-table)
                         ;; A session-visible temp table can map to a unique
                         ;; physical namespace in `schema` while `db` has no
                         ;; logical-name ident to order. Fall back to the
                         ;; supplied schema in that case. Preserve an empty
                         ;; order for an INHERITS child: its parent columns are
                         ;; intentionally prepended below.
                         own-order (if (or (seq db-order) (seq ancestor-tables))
                                     db-order
                                     (when-let [cols (pgs/column-info schema raw-table)]
                                       (mapv :name (rest cols))))
                         parent-order (mapcat #(pgs/column-order-from-db db %)
                                              (reverse ancestor-db-tables))]
                     ;; PostgreSQL orders inherited columns before the
                     ;; child's own columns for INSERT without a target list.
                     ;; An empty child column-order is still a real value, so
                     ;; plain `or` previously hid the parent completely.
                     ;; Current catalogs persist inherited columns in the
                     ;; child's own complete attnum sequence. Prepending the
                     ;; parent's *current* order after ALTER PARENT ADD COLUMN
                     ;; reshuffles child values (a,c,b became a,b,c). Only the
                     ;; legacy fallback, where the child has no durable order,
                     ;; needs ancestor synthesis.
                     (if (seq db-order)
                       (vec db-order)
                       (vec (distinct (concat parent-order own-order))))))
        [table-name col-names] (canonical-relation
                                schema (pgs/schema-hints db)
                                raw-table raw-cols)
        ns table-name
        sequence-defaults (into {}
                                (map (fn [[attr sequence-name]]
                                       [(keyword (get physical->logical
                                                      (namespace attr)
                                                      (namespace attr))
                                                 (name attr))
                                        sequence-name]))
                                (insert-sequence-defaults db db-table))
        resolve-target-attr
        (fn [col-name]
          (if (contains? temp-table-map raw-table)
            (let [db-attr (keyword db-table col-name)
                  resolved (if db
                             (ctx/resolve-inherited-attr db-attr (:schema db) db)
                             db-attr)
                  logical-ns (get physical->logical (namespace resolved)
                                  (namespace resolved))]
              (keyword logical-ns (name resolved)))
            (let [raw-attr (keyword ns col-name)]
              (if db
                (ctx/resolve-inherited-attr raw-attr schema db)
                raw-attr))))
        ;; PostgreSQL rejects a target column named twice. We built a
        ;; map from the column list, so the last value silently won and
        ;; `INSERT INTO t (id, id) VALUES (91, 92)` reported INSERT 0 1
        ;; having stored 92 — a row the client never asked for. Checked
        ;; only for an EXPLICIT column list; the implicit one is derived
        ;; from the schema and cannot repeat.
        _ (when (seq columns)
            (when-let [dup (first (for [[c n] (frequencies col-names)
                                        :when (> n 1)]
                                    c))]
              (throw (ex-info (str "column \"" dup "\" specified more than once")
                              {:error :duplicate-column
                               :sqlstate "42701"
                               :column dup}))))
        select (.getSelect insert)
        ;; ON CONFLICT handling
        ^net.sf.jsqlparser.statement.insert.InsertConflictAction
        conflict-action (.getConflictAction insert)
        ^net.sf.jsqlparser.statement.insert.InsertConflictTarget
        conflict-target (.getConflictTarget insert)
        target-alias (some-> ^Table table .getAlias .getName unquote-ident)
        _ (when conflict-action
            (reject-hidden-target-name! conflict-action raw-table target-alias))
        _ (when (some (fn [^Column col]
                        (= "excluded"
                           (some-> col .getTable .getName unquote-ident str/lower-case)))
                      (params/ast-columns (.getReturningClause insert)))
            (throw (ex-info
                    "invalid reference to FROM-clause entry for table \"excluded\""
                    {:error :undefined-table
                     :sqlstate "42P01"
                     :table "excluded"
                     :detail (str "There is an entry for table \"excluded\", but it cannot "
                                  "be referenced from this part of the query.")})))
        _ (when (and conflict-action
                     (= (.getConflictActionType conflict-action)
                        net.sf.jsqlparser.statement.insert.ConflictActionType/DO_UPDATE)
                     (nil? conflict-target))
            (throw (ex-info
                    "ON CONFLICT DO UPDATE requires inference specification or constraint name"
                    {:error :syntax-error
                     :sqlstate "42601"
                     :hint "For example, ON CONFLICT (column_name)."})))]
    (cond
      ;; Handle DEFAULT VALUES — insert with no explicit column values
      (.isOnlyDefaultValues ^Insert insert)
      (let [marker (pgs/row-marker-attr table-name)
            attrs (reduce
                   (fn [row ancestor]
                     (let [ancestor-marker (pgs/row-marker-attr ancestor)]
                       (if (get schema ancestor-marker)
                         (assoc row ancestor-marker true)
                         row)))
                   {marker true}
                   ancestor-tables)
            attrs (populate-insert-sequence-defaults attrs sequence-defaults)
            returning (extract-returning (.getReturningClause insert))]
        (if conflict-action
          (on-conflict-result
           [attrs] 1 returning
           (conflict-plan conflict-action conflict-target table-name ns
                          schema db target-alias))
          (let [entity (assoc attrs :db/id (str (gensym "default-")))]
            (cond-> {:type :insert
                     :insert-mode :plain
                     :insert-candidates [entity]
                     :tx-data [entity]
                     :count 1
                     :table table-name :ns ns}
              returning (assoc :returning returning)))))

      ;; INSERT INTO ... SELECT ... — run the SELECT against current db,
      ;; then treat each result row as if it were a VALUES tuple.
      ;; Handles patterns like:
      ;;   INSERT INTO t (k,v) SELECT 'x', 'y' WHERE EXISTS (...)
      ;;   INSERT INTO t (a,b) SELECT a, b FROM other_table WHERE ...
      (and (or (instance? PlainSelect select)
               (instance? ParenthesedSelect select))
           (seq col-names)
           db)
      (let [inner-select (if (instance? ParenthesedSelect select)
                           (.getSelect ^ParenthesedSelect select)
                           select)
            inner-parsed (binding [expr/*defer-all-projection-computations?* true]
                           (params/*parse-sql* (str inner-select) schema db))
            ;; parse-sql CATCHES: a source SELECT that failed to
            ;; translate comes back as {:type :error}, not as a throw.
            ;; Ignoring that carried a nil :query into d/q, whose
            ;; "Query should be a vector or a map" then replaced the
            ;; real diagnosis — so `INSERT INTO t (id) SELECT nope FROM
            ;; t` reported XX000 instead of the inner 42703.
            _ (when (= :error (:type inner-parsed))
                (throw (ex-info (str (:message inner-parsed))
                                {:sqlstate (or (:sqlstate inner-parsed) "XX000")})))
            source-plan
            {:source inner-parsed
             :col-names col-names
             :source-attrs (mapv resolve-target-attr col-names)
             :table-name table-name
             :ns ns
             :ancestor-tables ancestor-tables
             :sequence-defaults sequence-defaults
             :conflict-plan (when conflict-action
                              (conflict-plan conflict-action conflict-target
                                             table-name ns schema db target-alias))
             :returning (extract-returning (.getReturningClause insert))}]
        ;; Source rows belong to execution, not parsing. Besides prepared
        ;; statements observing the current snapshot, this keeps volatile
        ;; source projections out of Parse and lets the server interleave one
        ;; projected candidate with target defaults and validation.
        {:type :insert
         :insert-mode :deferred-select
         :insert-candidates []
         :tx-data []
         :count 0
         :table table-name
         :ns ns
         :insert-source source-plan
         ;; Describe runs before deferred source materialization, but must
         ;; advertise the same RETURNING row shape Execute will produce.
         :returning (:returning source-plan)})

      ;; Normal INSERT with VALUES
      (and (instance? Values select) (seq col-names))
      (let [^Values values select
            expr-list (.getExpressions values)
            ;; Bind extract-value with schema+db for scalar subquery support
            ev (fn [e] (extract-value e schema db))
            ;; Multi-row INSERT: VALUES (1,'a'), (2,'b') →
            ;; JSqlParser 5.x: each row is a ParenthesedExpressionList
            ;; Disambiguation when all PELs have exactly 1 element:
            ;;   - If PEL count == column count → single row with parenthesized scalars
            ;;     e.g. INSERT INTO t(a,b,c) VALUES (('1'::int), ('x'), ('2'::int))
            ;;   - If PEL count != column count → multi-row single-column table
            ;;     e.g. INSERT INTO t(name) VALUES ('alice'), ('bob')
            all-pel? (every? #(instance? ParenthesedExpressionList %) expr-list)
            num-cols (count col-names)
            row-exprs (cond
                        ;; All PELs have >1 element → genuine multi-row VALUES
                        (and all-pel?
                             (every? #(> (count %) 1) expr-list))
                        (mapv (fn [^ParenthesedExpressionList pel] (vec pel))
                              expr-list)

                        ;; All PELs have exactly 1 element AND count matches columns
                        ;; → single row with parenthesized expressions
                        (and all-pel?
                             (every? #(= (count %) 1) expr-list)
                             (= (count expr-list) num-cols))
                        [(mapv (fn [^ParenthesedExpressionList pel] (first pel))
                               expr-list)]

                        ;; All PELs have exactly 1 element but count != columns
                        ;; → multi-row for single-column (or N-column) table
                        (and all-pel?
                             (every? #(= (count %) 1) expr-list))
                        (mapv (fn [^ParenthesedExpressionList pel] (vec pel))
                              expr-list)

                        ;; Direct list of values (single row without parens)
                        :else
                        [(vec expr-list)])
            rows (vec (mapcat #(expand-insert-values-srfs % ev) row-exprs))
            _ (if (seq columns)
                (validate-insert-row-widths! col-names rows)
                ;; Without an explicit target list PostgreSQL permits a
                ;; short VALUES row and fills trailing columns from defaults;
                ;; it still rejects rows wider than the table.
                (doseq [row rows
                        :when (> (count row) num-cols)]
                  (throw (ex-info "INSERT has more expressions than target columns"
                                  {:error :syntax-error
                                   :sqlstate "42601"
                                   :target-count num-cols
                                   :value-count (count row)}))))
            ;; Build row attribute maps
            ;; For INHERITS: resolve inherited columns to parent namespace
            row-attrs (mapv (fn [row]
                              (into {}
                                    (keep (fn [[col-name val]]
                                            (let [attr (resolve-target-attr col-name)
                                                  coerced (coerce-insert-value val attr schema)]
                                          ;; DEFAULT means omitted; explicit
                                          ;; NULL remains a present nil so a
                                          ;; declared default is not applied.
                                          ;; Explicit NULL must remain distinct
                                          ;; from an omitted/DEFAULT column until
                                          ;; the ON CONFLICT reducer has applied
                                          ;; defaults and NOT NULL checks.
                                              (cond
                                                (= ::insert-default val) nil
                                                (some? coerced) [attr coerced]
                                                :else [attr nil])))
                                          (map vector col-names row))))
                            rows)
        ;; Add row-existence marker for this table
            marker (pgs/row-marker-attr table-name)
            has-marker? (boolean (get schema marker))
            row-attrs (if has-marker?
                        (mapv #(assoc % marker true) row-attrs)
                        row-attrs)
        ;; For INHERITS: also add parent's row-marker so parent queries find this entity
            row-attrs (reduce
                       (fn [rows ancestor]
                         (let [marker (pgs/row-marker-attr ancestor)]
                           (if (get schema marker)
                             (mapv #(assoc % marker true) rows)
                             rows)))
                       row-attrs ancestor-tables)
            row-attrs (mapv #(populate-insert-sequence-defaults
                              % sequence-defaults)
                            row-attrs)
            result
            (if conflict-action
              (on-conflict-result
               row-attrs (count rows) nil
               (conflict-plan conflict-action conflict-target table-name ns
                              schema db target-alias))
          ;; No ON CONFLICT — normal INSERT.
          ;;
          ;; PG semantics: a duplicate value on any unique constraint
          ;; raises 23505 and aborts the statement. Datahike's
          ;; `:db.unique/value` attrs raise on duplicate natively
          ;; (errors.clj maps `:db.error/unique` → 23505), but
          ;; `:db.unique/identity` attrs upsert — which is correct for
          ;; Datalog callers but wrong for SQL INSERT.
          ;;
          ;; So for identity attrs — both scalar and `:db.type/tuple`
          ;; (multi-col PK) — we add a `:db.fn/call` that runs before
          ;; the row-maps and throws on any value that already exists
          ;; on another entity. Also catches intra-batch self-collisions.
          ;;
          ;; The fn resolves constraints from the txdb schema at run
          ;; time so it picks up ALTER TABLE-added constraints too.
              {:type :insert
               :tx-data
               (into
                ;; Pass row-attrs as an explicit `:db.fn/call` arg AND
                ;; keep entity-maps in outer tx-data. The arg form is
                ;; reachable by substitute-params (which can't peek
                ;; into a Clojure closure), enabling the templater's
                ;; result-cache fast path. The outer entity-maps stay
                ;; visible to apply-column-constraints / auto-populate-
                ;; identity, which expect to walk maps in the outer
                ;; tx-data shape and would no-op if we hid them.
                ;;
                ;; Identity preservation: substitute-params and
                ;; resolve-nextvals! both keep nextval-marker objects
                ;; intact across walks, so the same marker appearing
                ;; in BOTH the args and the outer entity-maps gets
                ;; resolved exactly once (see datahike.pg.sql.params).
                [[:db.fn/call
                  ;; :datahike.pg/fresh-insert (attached via with-meta at
                  ;; the end of this fn form) — this tx-fn either throws
                  ;; (23505) or emits the payload rows as FRESH entities
                  ;; (gensym tempids, never upserts). The commit conflict
                  ;; ring uses the tag to attribute such ops as writing no
                  ;; existing rows instead of marking the whole commit
                  ;; opaque (which disabled row-level conflict detection
                  ;; for every INSERT-bearing transaction).
                  (with-meta
                    (fn unique-check [txdb row-attrs]
                      (let [schema (:schema txdb)
                            q-fn d/q
                      ;; Partition identity attrs by shape.
                      ;;   scalar-ids → {:attr constraint-name}
                      ;;   tuple-ids  → [{:attr :cols [component-attrs] :name c}]
                            scalar-ids
                            (into {}
                                  (keep (fn [[attr m]]
                                          (when (and (map? m)
                                                     (= :db.unique/identity (:db/unique m))
                                                     (not= :db.type/tuple (:db/valueType m))
                                                     (keyword? attr))
                                            [attr (str table-name "_pkey")])))
                                  schema)
                            tuple-ids
                            (into []
                                  (keep (fn [[attr m]]
                                          (when (and (map? m)
                                                     (= :db.unique/identity (:db/unique m))
                                                     (= :db.type/tuple (:db/valueType m))
                                                     (seq (:db/tupleAttrs m))
                                                     (keyword? attr))
                                            {:attr attr
                                             :cols (:db/tupleAttrs m)
                                             :name (str table-name "_pkey")})))
                                  schema)
                            seen (volatile! {})
                            raise! (fn [attr val constraint]
                                     (throw (ex-info "unique violation"
                                                     {:error      :unique-violation
                                                      :table      table-name
                                                      :column     (name attr)
                                                      :constraint constraint
                                                      :value      val
                                                      :datahike/collision [attr val]})))]
                        (doseq [attrs row-attrs]
                    ;; 1) Scalar identity checks
                          (doseq [[a v] attrs
                                  :when (and (contains? scalar-ids a) (some? v))]
                            (let [cname (get scalar-ids a)]
                              (when (ffirst (q-fn '{:find [?e]
                                                    :in [$ ?a ?v]
                                                    :where [[?e ?a ?v]]}
                                                  txdb a v))
                                (raise! a v cname))
                              (when (contains? (get @seen a) v)
                                (raise! a v cname))
                              (vswap! seen update a (fnil conj #{}) v)))
                    ;; 2) Tuple identity checks (multi-col PK).
                    ;; Mirror Datahike's auto-population: the tuple
                    ;; value is the vector of component-attr values in
                    ;; :db/tupleAttrs order. Skip rows where any
                    ;; component is absent — those can't be enforced
                    ;; until the writer sees the full entity.
                          (doseq [tid tuple-ids
                                  :let [attr (:attr tid)
                                        cols (:cols tid)
                                        cname (:name tid)
                                        tuple-val (mapv #(get attrs %) cols)]
                                  :when (every? some? tuple-val)]
                            (when (ffirst (q-fn '{:find [?e]
                                                  :in [$ ?a ?v]
                                                  :where [[?e ?a ?v]]}
                                                txdb attr tuple-val))
                              (raise! attr tuple-val cname))
                            (when (contains? (get @seen attr) tuple-val)
                              (raise! attr tuple-val cname))
                            (vswap! seen update attr (fnil conj #{}) tuple-val)))
                        []))
                    {:datahike.pg/fresh-insert true})
                  row-attrs]]
                (vec (mapcat
                      (fn [attrs]
                        (when (seq attrs)
                          [(assoc attrs :db/id (str (gensym "new-")))]))
                      row-attrs)))
               :count (count rows)
               :table table-name :ns ns})
        ;; Add RETURNING clause if present
            returning (extract-returning (.getReturningClause insert))]
        (cond-> (assoc result :alias target-alias)
          (not conflict-action)
          (assoc :insert-mode :plain
                 :insert-candidates (filterv map? (:tx-data result)))
          (and (not conflict-action) (not returning)
               (empty? ancestor-tables) (empty? sequence-defaults)
               (empty? (.getWithItemsList insert))
               (every? literal-catalog-expression? (mapcat identity row-exprs)))
          (assoc :catalog-dependency-shape :literal-insert-v1
                 :catalog-target-name raw-table)
          returning (assoc :returning returning))))))

(defn translate-delete
  "Translate a DELETE statement to Datahike retraction query + tx-data."
  [^Delete delete schema]
  (let [table (.getTable delete)
        _ (when-not table
            (throw (ex-info "syntax error at end of input"
                            {:error :syntax-error :sqlstate "42601"})))
        raw-table (unquote-ident (.getName ^Table table))
        _ (when-not (stored-relation-known? schema raw-table)
            (throw (ex-info (str "relation \"" raw-table "\" does not exist")
                            {:error :undefined-table
                             :sqlstate "42P01"
                             :table raw-table})))
        table-name (first (canonical-relation schema raw-table []))
        alias-obj (.getAlias ^Table table)
        alias-name (when alias-obj (unquote-ident (.getName ^Alias alias-obj)))
        _ (reject-hidden-target-name! delete raw-table alias-name)
        ns table-name
        where-expr (.getWhere delete)]
    (let [returning (.getReturningClause delete)]
      (cond-> {:type :delete
               :table table-name
               :alias alias-name
               :ns ns
               :where-expr where-expr}
        (and (not returning)
             (target-delete-expression? where-expr raw-table alias-name))
        (assoc :catalog-dependency-shape :target-delete-v1
               :catalog-target-name raw-table)
        returning
        (assoc :returning (extract-returning returning))))))

(declare translate-recursive-cte)

(defn eval-values-literal
  "Evaluate a literal JSqlParser expression from a VALUES row.
   Handles simple literals, casts, and parenthesis; returns the raw value.
   Anything else returns :unhandled — the caller decides whether to fall back.

   A JdbcParameter returns a ParamRef that the wire layer resolves at
   Bind time, allowing prepared INSERTs / UPDATE FROM VALUES with ?/$N."
  [expr]
  (cond
    (instance? JdbcParameter expr) (->ParamRef (.getIndex ^JdbcParameter expr))
    (instance? LongValue expr)
    ;; Fall back to BigInteger when the literal overflows Long
    ;; (symmetric with extract-value above).
    (try (.getValue ^LongValue expr)
         (catch NumberFormatException _
           (java.math.BigInteger. ^String (.getStringValue ^LongValue expr))))
    (instance? DoubleValue expr)  (types/decimal-literal expr (.getValue ^DoubleValue expr))
    (pg-bits/bit-string-literal? expr)
    (pg-bits/to-pg-text (pg-bits/bit-string-literal-value expr))
    (instance? StringValue expr)  (.getNotExcapedValue ^StringValue expr)
    (instance? BooleanValue expr) (.getValue ^BooleanValue expr)
    (instance? NullValue expr)    nil
    (instance? Parenthesis expr)  (eval-values-literal (.getExpression ^Parenthesis expr))
    (instance? CastExpression expr)
    (let [^CastExpression ce expr
          inner (eval-values-literal (.getLeftExpression ce))]
      (if (= :unhandled inner)
        :unhandled
        (apply-sql-cast inner ce)))
    (instance? SignedExpression expr)
    (let [^SignedExpression se expr
          inner (eval-values-literal (.getExpression se))]
      (if (= :unhandled inner)
        :unhandled
        (if (= (.getSign se) \-) (- inner) inner)))
    :else :unhandled))

(defn- row-subquery-columns
  "`SET (a, b) = (SELECT x, y …)`: one scalar subquery per target
   column, each selecting that column of the row.

   PostgreSQL evaluates the subquery ONCE per row (a MULTIEXPR sublink);
   this evaluates it once per column, which differs only for a volatile
   subquery. What it preserves is what the values are: no row gives every
   column NULL, and more than one row is 21000, because each scalar
   subquery answers that way by itself.

   nil when `exprs` is not a single row subquery of matching width, which
   leaves the caller's own arity check to reject it."
  [cols exprs]
  (when (= 1 (count exprs))
    (let [sel (when (instance? ParenthesedSelect (first exprs))
                (.getSelect ^ParenthesedSelect (first exprs)))
          ps (when (instance? PlainSelect sel) sel)
          items (some-> ps .getSelectItems vec)]
      (when (= (count cols) (count items))
        (mapv (fn [item]
                (doto (ParenthesedSelect.)
                  (.setSelect (doto (PlainSelect.)
                                (.setSelectItems [item])
                                (.setFromItem (.getFromItem ^PlainSelect ps))
                                (.setJoins (.getJoins ^PlainSelect ps))
                                (.setWhere (.getWhere ^PlainSelect ps))
                                (.setGroupByElement (.getGroupBy ^PlainSelect ps))
                                (.setHaving (.getHaving ^PlainSelect ps))
                                (.setOrderByElements (.getOrderByElements ^PlainSelect ps))
                                (.setLimit (.getLimit ^PlainSelect ps))
                                (.setOffset (.getOffset ^PlainSelect ps))))))
              items)))))

(defn translate-update
  "Translate an UPDATE statement to Datahike retract+assert pairs.
   A WITH clause, recursive or not, rides along in :with-sql: the SET
   list is translated at Execute as a query over the target, and the CTE
   is materialised there like any other relation."
  [^Update update schema db]
  (let [table (.getTable update)
        raw-table (unquote-ident (.getName ^Table table))
        _ (when-not (stored-relation-known? schema raw-table)
            (throw (ex-info (str "relation \"" raw-table "\" does not exist")
                            {:error :undefined-table
                             :sqlstate "42P01"
                             :table raw-table})))
        table-name (first (canonical-relation schema raw-table []))
        canonical-target-column
        (fn [col-name]
          (let [ci (pgs/ci-index schema (pgs/schema-hints db))
                attr (pgs/canonical-attr ci table-name col-name)]
            (cond
              (and attr (not (pgs/ambiguous? attr))) (name attr)
              (or (pgs/registered-relation? ci table-name)
                  (nil? (get schema (keyword table-name col-name))))
              (throw (ex-info (str "column \"" col-name "\" of relation \""
                                   table-name "\" does not exist")
                              {:error :undefined-column
                               :sqlstate "42703"
                               :column col-name}))
              :else col-name)))
        alias-obj (.getAlias ^Table table)
        alias-name (when alias-obj (unquote-ident (.getName ^Alias alias-obj)))
        ;; The name each FROM relation is visible under: its alias, else
        ;; its own name (only a Table has one to clash with).
        from-items (cons (.getFromItem update)
                         (map #(.getRightItem ^Join %) (or (.getJoins update) [])))
        from-names (keep (fn [item]
                           (when item
                             (if-let [a (.getAlias ^FromItem item)]
                               (unquote-ident (.getName ^Alias a))
                               (when (instance? Table item)
                                 (unquote-ident (.getName ^Table item))))))
                         from-items)
        ;; PostgreSQL's target is in the range table with the rest, so a
        ;; FROM relation visible under the same name is 42712
        ;; (setTargetTable / checkNameSpaceConflicts).
        _ (when-let [dup (some (set from-names) [(or alias-name raw-table)])]
            (throw (ex-info (str "table name \"" dup "\" specified more than once")
                            {:error :duplicate-alias :sqlstate "42712" :table dup})))
        _ (reject-hidden-target-name! update raw-table alias-name from-names)
        ns table-name
        where-expr (.getWhere update)
        update-sets (.getUpdateSets update)
        _ (doseq [^UpdateSet us update-sets
                  value-expr (.getValues us)]
            (when (contains-target-list-srf? value-expr)
              (throw (errors/pg-error
                      :feature-not-supported
                      {:message "set-returning functions are not allowed in UPDATE"})))
            (reject-grouping! value-expr "UPDATE"))
        ;; PostgreSQL does not permit qualification on the left-hand side
        ;; of SET, even when it names the target alias.  JSqlParser preserves
        ;; that qualifier separately on Column; dropping it silently accepted
        ;; `UPDATE t x SET x.c = ...` as though the user wrote `c = ...`.
        _ (when-let [qualified
                     (first (for [^UpdateSet us update-sets
                                  ^Column c (.getColumns us)
                                  :let [^Table target-qualifier (.getTable c)]
                                  :when (some-> target-qualifier .getName not-empty)]
                              c))]
            (let [^Table target-qualifier (.getTable ^Column qualified)
                  qualifier (some-> target-qualifier .getName unquote-ident)]
              (throw (ex-info (str "column \"" qualifier "\" of relation \""
                                   table-name "\" does not exist")
                              {:error :undefined-column
                               :sqlstate "42703"
                               :column qualifier
                               :hint "SET target columns cannot be qualified with the relation name."}))))
        ;; Same hazard as the INSERT column list, different SQLSTATE:
        ;; `UPDATE t SET sal = 1, sal = 2` built one assignment map and
        ;; the last write won, reporting UPDATE 1 for a statement
        ;; PostgreSQL refuses.
        _ (when-let [dup (first (for [[c n] (frequencies
                                             (mapcat (fn [^UpdateSet us]
                                                       (map #(unquote-ident
                                                              (.getColumnName ^Column %))
                                                            (.getColumns us)))
                                                     update-sets))
                                      :when (> n 1)]
                                  c))]
            (throw (ex-info (str "multiple assignments to same column \"" dup "\"")
                            {:error :syntax-error
                             :sqlstate "42601"
                             :column dup})))
        withs (.getWithItemsList update)
        ;; The WITH clause rides along into the SET query, which is
        ;; parsed as ordinary SQL: the CTE is then materialised by the
        ;; SELECT translator, for the FROM relation and for a subquery
        ;; in SET alike.
        with-sql (when (seq withs)
                   ;; A WithItem renders its own RECURSIVE keyword.
                   (str "WITH " (str/join ", " (map str withs)) " "))
        ;; The FROM relation as written, joins included: the SET list is
        ;; translated at Execute as a query over the target joined to it
        ;; (server/update-set-plan). The shapes below are what the
        ;; per-source-row fallback can still do by itself.
        from-sql (when-let [fi (.getFromItem update)]
                   ;; A comma-separated relation is a "simple" join, and
                   ;; its toString leaves the comma out.
                   (str fi (apply str (map (fn [^Join j]
                                             (str (if (.isSimple j) ", " " ") j))
                                           (or (.getJoins update) [])))))]
    (cond-> {:type :update
             :table table-name
             :alias alias-name
             :ns ns
               ;; The FROM relation as written. The SET list is
               ;; translated again at Execute as a query over the target
               ;; joined to it (server/update-set-plan), which is how
               ;; PostgreSQL plans UPDATE ... FROM.
             :with-sql with-sql
             :from-sql from-sql
               ;; The parameter types this statement was translated with:
               ;; the SET list is translated again at Execute
               ;; (server/update-set-plan) and must see the same ones.
             :declared-param-oids params/*declared-param-oids*
             :where-expr where-expr
             :assignments
             (vec
              (mapcat
               (fn [^UpdateSet us]
                 (let [cols (vec (.getColumns us))
                       exprs (or (row-subquery-columns (vec (.getColumns us))
                                                       (vec (.getValues us)))
                                 (vec (.getValues us)))]
                   (when-not (= (count cols) (count exprs))
                       ;; A multi-column assignment from anything else --
                       ;; ROW(...), a set-operation subquery -- still needs
                       ;; one evaluation yielding a record. Applying only
                       ;; the first pair is silent partial corruption.
                     (throw (ex-info
                             "multi-column UPDATE from a row expression is not supported"
                             {:error :feature-not-supported :sqlstate "0A000"})))
                   (map (fn [^Column col value-expr]
                          {:column (canonical-target-column
                                    (unquote-ident (.getColumnName col)))
                           :value-expr value-expr})
                        cols exprs)))
               update-sets))}
      (.getReturningClause update)
      (assoc :returning (extract-returning (.getReturningClause update))))))

;; ============================================================================
;; WITH RECURSIVE: translate to Datalog rules
;; ============================================================================

(defn translate-cte-branch
  "Translate one branch of a recursive CTE (anchor or recursive PlainSelect)
   into Datalog rule body clauses.

   - cte-name: the CTE's name (e.g. \"__parent_store_compute\")
   - col-names: CTE column names in order (e.g. [\"id\" \"parent_path\"])
   - rule-vars: corresponding rule output vars (e.g. [?id ?parent_path])
   - rule-name: the rule's name as a symbol (for self-references)
   - schema, db: from the outer context
   - virtual-cte-schema: if non-nil, the CTE is referenceable as a virtual
     table in this branch (for the recursive branch only).

   Returns a vector of clauses for use as a rule body, with the SELECT items
   bound to the rule output vars."
  [^PlainSelect ps cte-name col-names rule-vars rule-name schema db virtual-cte-schema]
  (let [;; If this branch references the CTE (recursive branch), we need to
        ;; provide a virtual schema for the CTE so translate-select can resolve
        ;; references like __pp.id, __pp.parent_path
        eff-schema (if virtual-cte-schema (merge schema virtual-cte-schema) schema)
        ;; Translate the SELECT body using the existing translator
        result (translate-select ps eff-schema db)
        query (:query result)
        find-vars (:find query)
        where-clauses (:where query)
        ;; :in clause is [$ ?param1 ?param2 ...] — drop $ to get just the params
        in-params (vec (rest (or (:in query) ['$])))
        in-args (:in-args result)
        ;; The find-vars correspond positionally to col-names (the SELECT items
        ;; produce values for the CTE columns). To bind them to rule output vars,
        ;; we substitute each find-var with the rule output var throughout the
        ;; clauses (using identity bindings causes Datahike's recursive rule
        ;; evaluator to hang).
        var-rename (zipmap find-vars rule-vars)
        rename-form (fn rename-form [c]
                      (cond
                        (symbol? c) (get var-rename c c)
                        (vector? c) (mapv rename-form c)
                        (seq? c) (apply list (map rename-form c))
                        :else c))
        bind-clauses []  ;; no separate bind clauses — we rename in-place
        ;; If recursive: detect patterns referencing the virtual CTE and
        ;; replace them with rule calls. Patterns can be:
        ;;   [?cte_eid :__pp/col ?val]                                  (plain)
        ;;   [(get-else $ ?cte_eid :__pp/col :__null__) ?val]           (NULL-aware)
        rewritten-clauses
        (if (nil? virtual-cte-schema)
          where-clauses
          (let [cte-ns cte-name
                ;; Helper: extract [evar attr val-var] from a CTE-referencing clause
                extract-cte-binding
                (fn [c]
                  (cond
                    ;; Plain data pattern [?e :ns/col ?v]
                    (and (vector? c) (= 3 (count c))
                         (keyword? (second c))
                         (= cte-ns (namespace (second c))))
                    [(first c) (name (second c)) (nth c 2)]
                    ;; get-else [(get-else $ ?e :ns/col :__null__) ?v]
                    (and (vector? c) (= 2 (count c))
                         (seq? (first c))
                         (= 'get-else (ffirst c)))
                    (let [[_ _ evar attr _default] (first c)
                          val-var (second c)]
                      (when (and (keyword? attr) (= cte-ns (namespace attr)))
                        [evar (name attr) val-var]))
                    :else nil))
                ;; Collect (evar, col, val-var) tuples from all CTE references
                cte-bindings (keep extract-cte-binding where-clauses)
                cte-evars (set (map first cte-bindings))
                ;; Group by evar: {evar {col val-var}}
                evar->col-bindings
                (reduce (fn [acc [evar col val-var]]
                          (assoc-in acc [evar col] val-var))
                        {} cte-bindings)
                ;; Build a rule call for each CTE entity var
                evar->rule-call
                (into {}
                      (for [[evar col-bindings] evar->col-bindings]
                        [evar (apply list rule-name
                                     (for [col col-names]
                                       (or (get col-bindings col)
                                           (symbol (str "?_cte_unused_" col)))))]))]
            ;; Replace CTE patterns; collect rule calls separately so they
            ;; can be placed AFTER data patterns (Datalog rule evaluation
            ;; requires inputs to be bound before recursive calls).
            (let [rule-calls (atom [])
                  rule-call-set (atom #{})
                  add-rule-call! (fn [rc]
                                   (when (and rc (not (@rule-call-set rc)))
                                     (swap! rule-call-set conj rc)
                                     (swap! rule-calls conj rc)))
                  non-cte-clauses
                  (vec
                   (keep (fn [c]
                           (cond
                              ;; Plain CTE data pattern → register rule call, drop pattern
                             (and (vector? c) (= 3 (count c))
                                  (keyword? (second c))
                                  (= cte-ns (namespace (second c))))
                             (do (add-rule-call! (get evar->rule-call (first c))) nil)
                              ;; CTE get-else clause → register rule call, drop pattern
                             (and (vector? c) (= 2 (count c))
                                  (seq? (first c))
                                  (= 'get-else (ffirst c))
                                  (let [[_ _ _ attr _] (first c)]
                                    (and (keyword? attr) (= cte-ns (namespace attr)))))
                             (do (add-rule-call! (get evar->rule-call (nth (vec (first c)) 2))) nil)
                              ;; Drop row-marker for CTE entity vars
                             (and (vector? c) (= 3 (count c))
                                  (contains? cte-evars (first c)))
                             nil
                             :else c))
                         where-clauses))
                  ;; Split non-cte clauses: data patterns first, then function bindings.
                  ;; This ensures the recursive call has its inputs bound before being called.
                  data-patterns (filterv (fn [c]
                                           (and (vector? c) (= 3 (count c))
                                                (keyword? (second c))))
                                         non-cte-clauses)
                  other-clauses (filterv (fn [c]
                                           (not (and (vector? c) (= 3 (count c))
                                                     (keyword? (second c)))))
                                         non-cte-clauses)]
              ;; Order: data patterns → rule calls → other clauses (preds, fn bindings)
              (vec (concat data-patterns @rule-calls other-clauses)))))]
    ;; Rename the SELECT find-vars to the rule head's output vars and
    ;; drop row-marker anchors. The rest of the body — including
    ;; `[(get-else $ ?e :ns/col :__null__) ?v]` clauses and
    ;; `[(= ?v :__null__)]` NULL checks — passes through unchanged.
    ;; Datahike's planner (post PR #826) recognises get-else in rule
    ;; bodies the same way it does at top level (LOptionalScan), so the
    ;; `?e` entity var is bound via the synthetic attribute scan.
    ;;
    ;; Row-marker patterns `[?e :ns/db-row-exists true]` come from
    ;; translate-select's entity-anchor injection. CTE-namespace markers
    ;; have already been swapped to rule calls upstream in
    ;; `rewritten-clauses`; real-table markers are dropped here because
    ;; the get-else clauses bind the entity var via LOptionalScan,
    ;; making the marker an extra unused scan.
    (let [renamed-clauses (mapv rename-form rewritten-clauses)
          marker-free-clauses (filterv (fn [c]
                                         (not (and (vector? c) (= 3 (count c))
                                                   (keyword? (second c))
                                                   (= "db-row-exists" (name (second c))))))
                                       renamed-clauses)
          final-clauses (into (vec marker-free-clauses) bind-clauses)]
      {:clauses final-clauses
       :in-params in-params
       :in-args in-args})))

(defn- select-references-relation?
  "Whether a SELECT-shaped node contains a FROM/JOIN reference to relation.

   JSqlParser's WithItem.isRecursive reports the clause keyword, not whether
   this particular CTE is self-referential. PostgreSQL permits ordinary CTEs
   inside WITH RECURSIVE, so use the relation reference to choose the
   fixed-point path. The lexical walk intentionally includes nested subqueries:
   those are still self-references and PostgreSQL's recursion validator must
   reject them before lowering."
  [select relation]
  (let [quoted (java.util.regex.Pattern/quote
                (str/lower-case (unquote-ident relation)))
        sql (str/lower-case (str select))]
    (boolean
     (or
      (re-find (re-pattern (str "(?is)\\b(?:from|(?:(?:inner|left|right|full|cross)\\s+)?join)"
                                "\\s+(?:only\\s+)?(?:\\\"?"
                                quoted "\\\"?)(?=\\s|[,);]|$)"))
               sql)
      ;; JSqlParser renders an implicit CROSS JOIN as `FROM left, right`.
      ;; Require a relation-position follower after the optional alias so a
      ;; projection such as `SELECT 1, cte_name FROM stored` is not mistaken
      ;; for a recursive scan merely because it also contains a comma.
      (re-find (re-pattern (str "(?is),\\s+(?:only\\s+)?(?:\\\"?"
                                quoted "\\\"?)"
                                "(?:\\s+(?:as\\s+)?[a-z_][a-z0-9_$]*)?"
                                "\\s*(?=,|\\bwhere\\b|\\b(?:inner|left|right|full|cross)?\\s*join\\b|"
                                "\\bgroup\\b|\\border\\b|\\blimit\\b|\\bunion\\b|\\)|$)"))
               sql)))))

(defn recursive-cte-self-reference?
  "True when a WITH RECURSIVE item actually references its own relation."
  [^net.sf.jsqlparser.statement.select.WithItem wi]
  (let [body (try (.getParenthesedStatement wi) (catch Throwable _ nil))]
    (and body
         (select-references-relation? body (str/trim (str (.getAlias wi)))))))

(defn- recursive-cte-branches
  "Split a WITH RECURSIVE item into [anchor recursive]. PostgreSQL requires
   recursive references to have the form `<anchor> UNION [ALL] <recursive>`;
   rejecting other set operations before lowering prevents invalid recursion
   from becoming unbounded work. Returns nil for a non-recursive PlainSelect,
   which callers may handle as an anchor-only item."
  [^net.sf.jsqlparser.statement.select.WithItem wi]
  (let [select (let [s (.getSelect wi)]
                 (if (instance? ParenthesedSelect s)
                   (.getSelect ^ParenthesedSelect s) s))]
    (cond
      (instance? PlainSelect select)
      (throw (errors/pg-error
              :syntax-error
              {:message (str "recursive query \"" (str/trim (str (.getAlias wi)))
                             "\" does not have the form non-recursive-term "
                             "UNION [ALL] recursive-term")}))

      (instance? SetOperationList select)
      (let [^SetOperationList sol select
            selects (.getSelects sol)
            ops (.getOperations sol)]
        (when-not (and (= 2 (count selects))
                       (= 1 (count ops))
                       (instance? net.sf.jsqlparser.statement.select.UnionOp
                                  (first ops)))
          (throw (errors/pg-error
                  :syntax-error
                  {:message (str "recursive query \"" (str/trim (str (.getAlias wi)))
                                 "\" does not have the form non-recursive-term "
                                 "UNION [ALL] recursive-term")})))
        (when-not (every? #(instance? PlainSelect %) selects)
          (throw (errors/pg-error
                  :feature-not-supported
                  {:feature "nested recursive UNION branches"})))
        (let [cte-name (str/lower-case
                        (unquote-ident (str/trim (str (.getAlias wi)))))
              _ (when (select-references-relation? (first selects) cte-name)
                  (throw
                   (ex-info
                    (str "recursive reference to query \"" cte-name
                         "\" must not appear within its non-recursive term")
                    {:error :invalid-recursion
                     :sqlstate "42P19"
                     :query cte-name})))
              ^PlainSelect recursive (second selects)
              table-name (fn [item]
                           (when (instance? Table item)
                             (str/lower-case
                              (unquote-ident (.getName ^Table item)))))
              recursive-functions
              (into #{}
                    (mapcat (fn [^SelectItem item]
                              (params/ast-function-names (.getExpression item))))
                    (.getSelectItems recursive))]
          (when (some fns/aggregate-function? recursive-functions)
            (throw
             (ex-info
              "aggregate functions are not allowed in a recursive query's recursive term"
              {:error :grouping-error :sqlstate "42803" :query cte-name})))
          (when (or (seq (.getOrderByElements recursive))
                    (seq (.getOrderByElements sol)))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "ORDER BY in a recursive query is not implemented"})))
          ;; PostgreSQL reports OFFSET ahead of LIMIT when both occur.
          (when (or (some? (.getOffset recursive))
                    (some? (.getOffset sol)))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "OFFSET in a recursive query is not implemented"})))
          (when (or (some? (.getLimit recursive))
                    (some? (.getLimit sol)))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "LIMIT in a recursive query is not implemented"})))
          (when (re-find #"(?i)\bFOR\s+(?:NO\s+KEY\s+)?(?:UPDATE|SHARE)\b"
                         (str recursive))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "FOR UPDATE/SHARE in a recursive query is not implemented"})))
          ;; PostgreSQL parse_agg.c/checkWellFormedRecursionWalker rejects a
          ;; recursive self-reference on the nullable side of every outer
          ;; join. Letting it reach fixed-point evaluation can keep producing
          ;; NULL-extended rows after the client has gone away.
          (reduce
           (fn [left-names ^Join join]
             (let [right-name (table-name (.getRightItem join))
                   left-ref? (contains? left-names cte-name)
                   right-ref? (= right-name cte-name)
                   invalid? (or (and (.isLeft join) right-ref?)
                                (and (.isRight join) left-ref?)
                                (and (.isFull join) (or left-ref? right-ref?)))]
               (when invalid?
                 (throw
                  (ex-info
                   (str "recursive reference to query \"" cte-name
                        "\" must not appear within an outer join")
                   {:error :invalid-recursion
                    :sqlstate "42P19"
                    :query cte-name})))
               (cond-> left-names right-name (conj right-name))))
           (cond-> #{} (table-name (.getFromItem recursive))
                   (conj (table-name (.getFromItem recursive))))
           (.getJoins recursive))
          [(first selects) recursive]))

      :else nil)))

(defn validate-recursive-cte-shape!
  "Raise PostgreSQL's structural error for a recursive CTE whose recursive
   reference is not combined with its anchor by UNION [ALL]. Called before
   fallback evaluators, whose capability probes intentionally catch errors."
  [^net.sf.jsqlparser.statement.select.WithItem wi]
  (recursive-cte-branches wi)
  nil)

(defn translate-recursive-cte
  "Translate a WITH RECURSIVE CTE definition into a Datalog rule.
   Returns: {:rule [...] :rule-name sym :col-names [...] :rule-vars [...]
             :in-params [...] :in-args [...]}"
  [^net.sf.jsqlparser.statement.select.WithItem wi schema db]
  (let [cte-name (str/trim (str (.getAlias wi)))
        rule-name (symbol cte-name)
        col-list (.getWithItemList wi)
        col-names (mapv (fn [item]
                          (let [expr (.getExpression ^SelectItem item)]
                            (unquote-ident
                             (cond
                               (instance? Column expr) (.getColumnName ^Column expr)
                               :else (str expr)))))
                        col-list)
        rule-vars (mapv #(symbol (str "?_rule_" cte-name "_" %)) col-names)
        ;; Build a virtual schema for the CTE (used in recursive branch only)
        ;; All columns default to :db.type/string — actual types come from
        ;; expressions in the SELECT items, not column constraints.
        virtual-cte-schema (into {(keyword cte-name "db-row-exists")
                                  {:db/valueType :db.type/boolean
                                   :db/cardinality :db.cardinality/one}}
                                 (for [c col-names]
                                   [(keyword cte-name c)
                                    {:db/valueType :db.type/string
                                     :db/cardinality :db.cardinality/one}]))
        select (let [s (.getSelect wi)]
                 (if (instance? ParenthesedSelect s)
                   (.getSelect ^ParenthesedSelect s) s))
        [anchor recursive] (or (recursive-cte-branches wi)
                               (when (instance? PlainSelect select)
                                 [select nil]))
        anchor-result (when anchor
                        (translate-cte-branch anchor cte-name col-names rule-vars
                                              rule-name schema db nil))
        recursive-result (when recursive
                           (translate-cte-branch recursive cte-name col-names rule-vars
                                                 rule-name schema db virtual-cte-schema))
        rule-head (apply list rule-name rule-vars)
        rule (cond-> []
               anchor-result    (conj (into [rule-head] (:clauses anchor-result)))
               recursive-result (conj (into [rule-head] (:clauses recursive-result))))
        all-in-params (into (or (:in-params anchor-result) [])
                            (when recursive-result (:in-params recursive-result)))
        all-in-args (into (or (:in-args anchor-result) [])
                          (when recursive-result (:in-args recursive-result)))]
    {:rule rule
     :rule-name rule-name
     :col-names col-names
     :rule-vars rule-vars
     :in-params all-in-params
     :in-args all-in-args
     ;; The anchor PlainSelect (UNION's non-recursive branch) — used by
     ;; materialize-recursive-cte! to infer column value-types from the
     ;; anchor's SELECT expressions when data can't be materialised at
     ;; parse time (B2: a parameterised anchor whose `$n` is unbound until
     ;; Bind). SQL gives a recursive CTE its column types from the anchor.
     :anchor anchor}))

(defn- infer-recursive-vtype
  "Minimal value-type detector for rows produced by a recursive CTE
   rule. Rule output values come from the SELECT items inside the CTE
   branches (Long arithmetic, literal strings, etc.) — far narrower
   than the cross-table UNION shapes materialize-set-op! has to handle,
   so a small per-value classifier suffices."
  [v]
  (cond
    (nil? v)                                    :db.type/string
    (instance? Long v)                          :db.type/long
    (instance? Integer v)                       :db.type/long
    (instance? Double v)                        :db.type/double
    (instance? Float v)                         :db.type/double
    (instance? java.math.BigDecimal v)          :db.type/bigdec
    (instance? java.math.BigInteger v)          :db.type/bigdec
    (instance? Boolean v)                       :db.type/boolean
    (instance? java.util.UUID v)                :db.type/uuid
    (instance? java.util.Date v)                :db.type/instant
    :else                                       :db.type/string))

(defn- recursive-coercion
  "Per-value coercion fn for a recursive-CTE column's datahike value type.
   Idempotent on already-typed values."
  [vtype]
  (case vtype
    :db.type/long   (fn [v] (if (instance? Long v) v (long v)))
    :db.type/double (fn [v] (if (instance? Double v) v (double v)))
    :db.type/bigdec (fn [v]
                      (cond
                        (instance? java.math.BigDecimal v) v
                        (instance? java.math.BigInteger v) (java.math.BigDecimal. ^java.math.BigInteger v)
                        (integer? v) (java.math.BigDecimal/valueOf (long v))
                        (float? v)   (java.math.BigDecimal/valueOf (double v))
                        :else (java.math.BigDecimal. (str v))))
    :db.type/string str
    identity))

(defn- recursive-schema-tx
  "Datahike schema tx for a materialised recursive CTE: one
   `:<target-name>/<col>` attr per column plus the row-existence marker.
   `col-array-elems` (optional, nil-padded) carries each column's array
   element kw for array-valued columns — stored as a :pg/array-elem datom so
   the column's read-back OID is T[] (mirrors real array columns; the value
   is canonical PG text in a :db.type/string column)."
  ([target-name col-names col-types row-marker]
   (recursive-schema-tx target-name col-names col-types row-marker nil nil))
  ([target-name col-names col-types row-marker col-array-elems]
   (recursive-schema-tx target-name col-names col-types row-marker col-array-elems nil))
  ([target-name col-names col-types row-marker col-array-elems col-pg-types]
   (conj
    (vec (for [[i c] (map-indexed vector col-names)]
           (cond-> {:db/ident       (keyword target-name c)
                    :db/valueType   (nth col-types i)
                    :db/cardinality :db.cardinality/one}
             ;; :pg/type round-trips the column's OID (array "_T" or the
             ;; OID-preserving scalars char/oid); :pg/array-elem drives the
             ;; canonical-text array decode.
             (and col-pg-types (nth col-pg-types i nil))
             (assoc :pg/type (nth col-pg-types i))
             (and col-array-elems (nth col-array-elems i nil))
             (assoc :pg/array-elem (nth col-array-elems i)))))
    {:db/ident       row-marker
     :db/valueType   :db.type/boolean
     :db/cardinality :db.cardinality/one})))

(defn- recursive-data-tx
  "Entity maps for the rows a recursive CTE produced, coercing each value
   to its column's datahike type and tagging every row with `row-marker`."
  [rows col-names col-types target-name row-marker]
  (let [coercions (mapv recursive-coercion col-types)]
    (vec (for [row rows]
           (let [vs (vec row)
                 cols (into {} (keep-indexed
                                (fn [i c]
                                  (let [v (nth vs i nil)]
                                    (when (some? v)
                                      [(keyword target-name c)
                                       ((nth coercions i) v)])))
                                col-names))]
             (assoc cols row-marker true))))))

(defn run-recursive-rule
  "Evaluate a recursive-CTE Datalog rule to a fixed point and return the
   raw result rows. `in-args` must already be free of ParamRef sentinels
   (substituted at Bind for the parameterised path).

   Forces the query planner on regardless of caller context — Datahike's
   base (relational) engine can't evaluate the recursive bodies
   translate-recursive-cte emits (head var bound through a function op then
   filtered by a predicate, datahike PR #825)."
  [db rule rule-name rule-vars in-params in-args]
  (let [rule-call (apply list rule-name rule-vars)
        q {:find  rule-vars
           :in    (into '[$ %] in-params)
           :where [rule-call]}]
    (binding [dq/*disable-planner* false]
      (apply d/q q db rule in-args))))

(defn- anchor-col-vtypes
  "Best-effort per-column datahike value-types for a recursive CTE,
   inferred from the anchor (non-recursive) branch's SELECT expressions
   via oid/expr-oid — used when rows can't be materialised at parse time
   (parameterised anchor). Columns we can't infer default to string; under
   :read schema-flexibility this only affects RowDescription OID accuracy,
   never data insertion (db-with does not enforce valueType)."
  [^PlainSelect anchor col-names schema db]
  (let [n (count col-names)
        oids (try
               (let [from-item     (.getFromItem anchor)
                     joins         (.getJoins anchor)
                     default-table (when (instance? Table from-item)
                                     (unquote-ident (.getName ^Table from-item)))
                     table-aliases (params/collect-table-aliases from-item joins)
                     oid-env {:db db :schema schema
                              :table-aliases table-aliases
                              :default-table default-table
                              :hints (pgs/schema-hints db)}]
                 (mapv (fn [^SelectItem si]
                         (try (oid/expr-oid (.getExpression si) oid-env)
                              (catch Throwable _ nil)))
                       (.getSelectItems anchor)))
               (catch Throwable _ nil))
        oids (vec (take n (concat (or oids []) (repeat nil))))]
    (mapv (fn [oid]
            (condp = oid
              types/oid-bool        :db.type/boolean
              types/oid-int8        :db.type/long
              types/oid-int4        :db.type/long
              types/oid-int2        :db.type/long
              26                    :db.type/long      ; oid
              types/oid-float8      :db.type/double
              700                   :db.type/double    ; float4
              types/oid-numeric     :db.type/bigdec
              types/oid-uuid        :db.type/uuid
              types/oid-date        :db.type/instant
              types/oid-timestamp   :db.type/instant
              types/oid-timestamptz :db.type/instant
              :db.type/string))
          oids)))

(defn- ground-rule-params
  "Inline now-bound prepared-statement params into a recursive-CTE rule.

   A Datalog rule body cannot see the outer query's `:in` vars, so a
   parameterised anchor like `SELECT $1::int` — compiled to
   `[(?cast-fn ?p1) ?out]` with `?p1`/`?cast-fn` supplied via `:in` —
   fails at rule eval (\"Unknown function ?cast-fn\"). At Execute the
   params are concrete, so we fold them directly into the rule:

   - plain value params (`?p1` → 1) are substituted as literal constants;
   - a clause whose FUNCTION position is a fn-valued param (the compiled
     CAST/coercion closure) with all-ground args is pre-evaluated and
     rewritten to `[(ground <result>) ?out]`.

   Returns the grounded rule. Params that remain referenced (e.g. a fn
   param applied to a rule var we can't pre-evaluate) are left in place;
   the caller passes only those through `:in`."
  [rule in-params in-args]
  (let [pmap    (zipmap in-params in-args)
        fn-vars (set (keep (fn [[k v]] (when (fn? v) k)) pmap))
        subst   (fn subst [form]
                  (cond
                    (and (symbol? form) (contains? pmap form) (not (fn-vars form)))
                    (get pmap form)
                    (vector? form) (mapv subst form)
                    (seq? form)    (apply list (map subst form))
                    :else form))
        eval-clause (fn [clause]
                      (if (and (vector? clause) (= 2 (count clause)) (seq? (first clause)))
                        (let [call  (first clause)
                              f-sym (first call)
                              cargs (rest call)]
                          (if (and (symbol? f-sym) (fn-vars f-sym)
                                   (every? (complement symbol?) cargs))
                            [(list 'ground (apply (get pmap f-sym) cargs)) (second clause)]
                            clause))
                        clause))]
    (mapv (fn [branch]
            (into [(first branch)]
                  (map (comp eval-clause subst) (rest branch))))
          rule)))

(defn materialize-recursive-rows!
  "Execute-time counterpart for a DEFERRED recursive CTE (see
   materialize-recursive-cte!): ground the now-bound params into the rule
   (in-args already substituted by resolve-param-refs), run it to a fixed
   point, coerce the rows to the parse-time `col-types`, and db-with the
   data into `db` (whose schema already carries the CTE attrs from parse).
   Returns the data-enriched db; on rule-eval failure returns `db` unchanged
   so the outer query degrades to an empty CTE rather than crashing."
  [{:keys [rule rule-name rule-vars col-names col-types in-params in-args
           target-name row-marker]} db]
  (try
    (let [grounded   (ground-rule-params rule in-params in-args)
          ;; Keep only params still referenced after grounding.
          referenced (set (filter symbol? (tree-seq coll? seq grounded)))
          pmap       (zipmap in-params in-args)
          rem-params (filterv referenced in-params)
          rem-args   (mapv pmap rem-params)
          rows (run-recursive-rule db grounded rule-name rule-vars rem-params rem-args)
          data-tx (recursive-data-tx rows col-names col-types target-name row-marker)]
      (if (seq data-tx) (d/db-with db data-tx) db))
    (catch Throwable _ db)))

(defn materialize-recursive-cte!
  "Run a WITH RECURSIVE CTE rule to a fixed point and materialize the
   resulting rows into a speculative db under `:<target-name>/<col>`
   virtual attrs. Mirrors the result shape of `materialize-set-op!`
   so callers (parse-sql) can swap implementations based on
   `(.isRecursive wi)`.

   When the CTE is parameterised (a `$n` appears in its body, so `in-args`
   carries ParamRef sentinels that aren't bound until Bind), DATA can't be
   produced at parse time. We then enrich only the SCHEMA (column attrs,
   with value-types inferred from the anchor branch) and return a
   `:deferred` spec; the server re-runs the rule at Execute via
   materialize-recursive-rows! once the params are bound.

   Reuses `translate-recursive-cte` for the rule construction."
  [^net.sf.jsqlparser.statement.select.WithItem wi target-name db schema]
  (let [{:keys [rule rule-name col-names rule-vars in-params in-args anchor]}
        (translate-recursive-cte wi schema db)
        row-marker (pgs/row-marker-attr target-name)
        ;; ParamRef sentinels in in-args ⇒ the anchor/recursive body
        ;; references a `$n` not bound until Bind. Defer data to Execute.
        deferred? (boolean (some params/param-ref? in-args))]
    (if deferred?
      (let [col-types (anchor-col-vtypes anchor col-names schema db)
            schema-tx (recursive-schema-tx target-name col-names col-types row-marker)
            spec-db   (d/db-with db schema-tx)]
        {:db      spec-db
         :schema  (:schema spec-db)
         :name    target-name
         :alias   target-name
         :aliases col-names
         :deferred {:rule rule :rule-name rule-name :rule-vars rule-vars
                    :col-names col-names :col-types col-types
                    :in-params in-params :in-args in-args
                    :target-name target-name :row-marker row-marker}})
      ;; No params — materialise data now (column types from the rows).
      (let [rows (run-recursive-rule db rule rule-name rule-vars in-params in-args)
            col-types (mapv (fn [i]
                              (let [samples (keep #(nth (vec %) i nil) rows)
                                    vtypes  (into #{} (map infer-recursive-vtype) samples)]
                                (cond
                                  (empty? vtypes)         :db.type/string
                                  (= 1 (count vtypes))    (first vtypes)
                                  ;; Mixed numerics → bigdec; anything else → string.
                                  (every? #{:db.type/long :db.type/double :db.type/bigdec} vtypes)
                                  :db.type/bigdec
                                  :else :db.type/string)))
                            (range (count col-names)))
            schema-tx (recursive-schema-tx target-name col-names col-types row-marker)
            spec-db (d/db-with db schema-tx)
            data-tx (recursive-data-tx rows col-names col-types target-name row-marker)
            spec-db2 (if (seq data-tx) (d/db-with spec-db data-tx) spec-db)]
        {:db      spec-db2
         :schema  (:schema spec-db2)
         :name    target-name
         :alias   target-name
         :aliases col-names}))))

(defn- visible-query-rows
  "Run a translate-select result's :query against `exec-db`, dropping any
   hidden trailing columns (entity/order-by vars), and return row vectors."
  [{:keys [query in-args hidden-count]} exec-db]
  (let [vis (- (count (:find query)) (or hidden-count 0))
        raw (if (seq in-args)
              (apply d/q query exec-db in-args)
              (d/q query exec-db))]
    (mapv (fn [r] (let [v (if (sequential? r) (vec r) [r])] (vec (take vis v))))
          raw)))

(defn materialize-recursive-iterative!
  "FALLBACK recursive-CTE evaluator (B1): semi-naive iteration instead of a
   single Datalog rule. Used when materialize-recursive-cte!'s rule encoding
   can't represent the body (LEFT JOIN → not-join, correlated subqueries,
   nested recursion — e.g. asyncpg's typeinfo introspection).

   Runs the anchor as an ordinary SELECT, materialises its rows under
   `:<target>/<col>`, then repeatedly runs the recursive branch — translated
   once against the seeded virtual table — folding NOVEL rows back in until a
   fixed point. Each iteration is a plain query the engine already handles.

   Parameterised CTEs are DEFERRED to Execute (B2-style): when the anchor has
   a `$n` and a real FROM clause (asyncpg's `FROM {typeinfo} ti WHERE
   ti.oid = any($1)`), only the schema is enriched at parse and a `:deferred`
   {:kind :iterative …} spec is returned; the server runs the anchor with the
   bound params and iterates at Execute (materialize-recursive-iterative-rows!).

   Returns the standard {:db :schema :name :alias :aliases [:deferred]} map, or
   nil when it can't apply: a non-`UNION` shape, a parameterised TABLE-FREE
   anchor (`SELECT $1::int` — the param constant-folds in translation, so the
   rule path / B2 must own it), or any translation/eval failure."
  [^net.sf.jsqlparser.statement.select.WithItem wi target-name db schema]
  (try
    (when-let [[anchor recursive] (recursive-cte-branches wi)]
      (let [col-list  (.getWithItemList wi)
            col-names (mapv (fn [item]
                              (let [expr (.getExpression ^SelectItem item)]
                                (unquote-ident
                                 (if (instance? Column expr)
                                   (.getColumnName ^Column expr)
                                   (str expr)))))
                            col-list)
            row-marker (pgs/row-marker-attr target-name)
            ;; AST-level param detection (robust to translation constant-folding
            ;; a table-free `$n` cast). A parameterised CTE can't run its anchor
            ;; at parse, so we defer — unless the anchor has no FROM (table-free
            ;; `SELECT $1::int`), where the param folds away and the rule/B2
            ;; path resolves it correctly; bail to that.
            anchor-param? (boolean (seq (params/ast-param-indices anchor)))
            param? (or anchor-param?
                       (boolean (seq (params/ast-param-indices recursive))))
            anchor-from (.getFromItem ^PlainSelect anchor)
            mk-data-tx (fn [coercions rows]
                         (vec (for [row rows]
                                (assoc (into {} (keep-indexed
                                                 (fn [i c]
                                                   (let [v (nth row i nil)]
                                                     (when (and (some? v) (not= :__null__ v))
                                                       [(keyword target-name c) ((nth coercions i) v)])))
                                                 col-names))
                                       row-marker true))))]
        (cond
          ;; Table-free parameterised anchor → rule/B2 owns it.
          (and param? anchor-param? (nil? anchor-from))
          nil

          ;; Parameterised (table-full) → defer data to Execute. Enrich only the
          ;; schema now; column types come from the anchor's inferred OIDs.
          param?
          (let [anchor-parsed (translate-select anchor schema db)
                anchor-oids (:select-item-oids anchor-parsed)
                col-types (mapv (fn [i]
                                  (or (some-> (nth anchor-oids i nil) types/dh-type-for-oid)
                                      :db.type/string))
                                (range (count col-names)))
                ;; Array columns (e.g. typeinfo_tree.attrtypoids from the
                ;; {typeinfo} array_agg) carry their element kw so the CTE
                ;; column's OID is T[] — the values arrive as canonical PG text.
                col-array-elems (mapv (fn [i]
                                        (some-> (nth anchor-oids i nil)
                                                types/array-oid->element-oid
                                                types/oid->elem-kw))
                                      (range (count col-names)))
                ;; :pg/type per column to round-trip the OID: array "_T", or the
                ;; OID-preserving scalars char/oid (e.g. typeinfo_tree.kind =
                ;; typtype is char — asyncpg needs it decoded as bytes b'c' to
                ;; recognise the composite, not the str 'c').
                col-pg-types (mapv (fn [i]
                                     (if-let [ae (nth col-array-elems i)]
                                       (str "_" (name ae))
                                       (get types/oid-preserving-pg-name (nth anchor-oids i nil))))
                                   (range (count col-names)))
                schema-tx (recursive-schema-tx target-name col-names col-types row-marker col-array-elems col-pg-types)
                spec0 (d/db-with db schema-tx)
                rec-parsed (binding [*cte-relations* #{(str/lower-case target-name)}]
                             (translate-select recursive (:schema spec0) spec0))]
            {:db spec0 :schema (:schema spec0) :name target-name
             :alias target-name :aliases col-names
             :deferred {:kind :iterative
                        :target-name target-name :row-marker row-marker
                        :col-names col-names :col-types col-types
                        :anchor (select-keys anchor-parsed [:query :in-args :hidden-count :enriched-db])
                        :recursive (select-keys rec-parsed [:query :in-args :hidden-count :enriched-db])}})

          ;; No params — materialise now.
          :else
          (let [anchor-parsed (translate-select anchor schema db)
                anchor-edb  (or (:enriched-db anchor-parsed) db)
                anchor-rows (visible-query-rows anchor-parsed anchor-edb)
                anchor-oids (:select-item-oids anchor-parsed)
                col-types (mapv (fn [i]
                                  (let [samples (keep #(nth % i nil) anchor-rows)
                                        vtypes  (into #{} (map infer-recursive-vtype) samples)]
                                    (cond
                                      (= 1 (count vtypes)) (first vtypes)
                                      (and (seq vtypes)
                                           (every? #{:db.type/long :db.type/double :db.type/bigdec} vtypes))
                                      :db.type/bigdec
                                      (seq vtypes) :db.type/string
                                      :else (or (some-> (nth anchor-oids i nil) types/dh-type-for-oid)
                                                :db.type/string))))
                                (range (count col-names)))
                coercions (mapv recursive-coercion col-types)
                schema-tx (recursive-schema-tx target-name col-names col-types row-marker)
                spec0 (d/db-with (d/db-with db schema-tx) (mk-data-tx coercions anchor-rows))
                rec-parsed (binding [*cte-relations* #{(str/lower-case target-name)}]
                             (translate-select recursive (:schema spec0) spec0))]
            (loop [cur spec0, seen (set anchor-rows), i 0]
              (if (> i 100000)
                {:db cur :schema (:schema cur) :name target-name
                 :alias target-name :aliases col-names}
                (let [rows  (visible-query-rows rec-parsed cur)
                      novel (vec (remove seen rows))]
                  (if (empty? novel)
                    {:db cur :schema (:schema cur) :name target-name
                     :alias target-name :aliases col-names}
                    (recur (d/db-with cur (mk-data-tx coercions novel))
                           (into seen novel) (inc i))))))))))
    (catch Throwable _ nil)))

(defn materialize-recursive-iterative-rows!
  "Execute-time materialisation for a DEFERRED iterative recursive CTE (see
   materialize-recursive-iterative!). `spec` is the :deferred map with its
   anchor/recursive :in-args already param-substituted by resolve-param-refs.
   Runs the anchor against its parse-time enriched-db, folds the rows into the
   recursive branch's enriched-db (which carries the CTE schema + any derived
   tables it referenced), and iterates to a fixed point. The recursive step is
   TOLERANT — if an iteration throws (e.g. an array-membership join the engine
   can't resolve), we stop and keep what we have (asyncpg needs only the anchor
   rows for a composite of core-typed fields). Returns the data-enriched db
   (the caller's query-db, whose schema already has the CTE attrs); on anchor
   failure returns `db` unchanged."
  [{:keys [anchor recursive col-names col-types target-name row-marker]} db]
  (try
    (let [coercions (mapv recursive-coercion col-types)
          mk-data-tx (fn [rows]
                       (vec (for [row rows]
                              (assoc (into {} (keep-indexed
                                               (fn [i c]
                                                 (let [v (nth row i nil)]
                                                   (when (and (some? v) (not= :__null__ v))
                                                     [(keyword target-name c) ((nth coercions i) v)])))
                                               col-names))
                                     row-marker true))))
          anchor-edb  (or (:enriched-db anchor) db)
          anchor-rows (visible-query-rows anchor anchor-edb)
          ;; The recursive branch was translated against a schema-only spec
          ;; whose :enriched-db carries the CTE attrs + any derived tables it
          ;; referenced; seed it with the (param-bound) anchor rows.
          rec-base    (or (:enriched-db recursive) db)
          base        (d/db-with rec-base (mk-data-tx anchor-rows))]
      ;; Bounded recursion: a type-dependency chain is shallow, and a result
      ;; column like typeinfo_tree.depth makes a type re-derived via a longer
      ;; path a DISTINCT row, so a deep/correlated body can keep producing
      ;; "novel" rows over the whole catalog. The cap keeps Execute responsive;
      ;; for asyncpg the ANCHOR rows are what build the codec (dependent
      ;; core-type rows are resolved from its builtin codecs), so a partial
      ;; recursion is still correct for the cases that matter.
      (loop [cur base, seen (set anchor-rows), i 0]
        (if (>= i 64)
          cur
          (let [rows  (try (visible-query-rows recursive cur) (catch Throwable _ :stop))
                novel (when (not= rows :stop) (vec (remove seen rows)))]
            (if (or (= rows :stop) (empty? novel))
              cur
              (recur (d/db-with cur (mk-data-tx novel)) (into seen novel) (inc i)))))))
    (catch Throwable _ db)))
