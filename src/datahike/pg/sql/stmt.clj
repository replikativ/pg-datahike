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
   `extract-returning`; CHECK / UPDATE expressions evaluated
   per-row at handler time go through `eval-check-predicate` /
   `eval-update-expr`.

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
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.datom]
            [datahike.query :as dq]
            [datahike.pg.cache :as pg-cache]
            [datahike.pg.errors :as errors]
            [datahike.pg.window :as window]
            [datahike.pg.jsonb :as jb]
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
            Values]
           [net.sf.jsqlparser.statement.insert Insert]
           [net.sf.jsqlparser.statement.update Update UpdateSet]
           [net.sf.jsqlparser.statement.delete Delete]))

(set! *warn-on-reflection* true)

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
         coerce-insert-value
         eval-check-predicate
         eval-update-expr
         extract-returning
         materialize-table-function
         materialize-derived-select!
         materialize-set-op!
         apply-compound-projections
         match-aggregate-index
         select-item-alias
         eval-values-literal
         extract-from-values
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
        on-exprs (some-> (.getOnExpressions join) seq
                         (->> (mapcat flatten-and)))
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
                    (reset! ref-info {:value-join? true
                                      :left-key-var left-key-var
                                      :right-key-attr right-key-attr
                                      :right-alias right-alias
                                      :left-evar (ctx/entity-var! ctx (:default-table ctx))}))
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
                       (vec preds)))
              ;; INNER-join ON conjunct = top-level conjunct: allow the
              ;; indexable data-pattern fast paths.
              (let [preds (binding [expr/*conjunctive-where* true]
                            (expr/translate-predicate ctx expr))]
                (swap! (:where-clauses ctx) into preds)))))))
    {:name name :alias right-alias :join-type jtype :ref-info @ref-info}))

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
    [(let [n (str/lower-case (.getName ^Function expr))]
       (if-let [i (str/last-index-of n ".")] (subs n (inc i)) n))
     2]

    (instance? net.sf.jsqlparser.expression.AnalyticExpression expr)
    [(str/lower-case (.getName ^net.sf.jsqlparser.expression.AnalyticExpression expr)) 2]

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
  (let [fname (str/lower-case (.getName f))
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
(declare apply-sql-cast)
(declare eval-corr-scalar)
(declare ^:dynamic *eval-update-db*)

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
      (nth bound (.getIndex ^JdbcParameter expr) ::corr)
      ::corr)
    (instance? SignedExpression expr)
    (let [v (srf-const-eval (.getExpression ^SignedExpression expr))]
      (if (number? v) (- v) ::corr))
    (instance? CastExpression expr)
    (let [v (srf-const-eval (.getLeftExpression ^CastExpression expr))]
      (if (= ::corr v) ::corr (apply-sql-cast v ^CastExpression expr)))
    (instance? ArrayConstructor expr) (extract-value ^ArrayConstructor expr)
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
  "The bare, lower-cased name of a function call, with any schema
   qualifier removed: `pg_catalog.generate_series` -> `generate_series`.

   PostgreSQL resolves the qualified and unqualified forms to the same
   function through search_path, and we serve one schema, so the last
   dot-separated segment is the name."
  [^String n]
  (when n
    (let [n (str/lower-case n)
          i (.lastIndexOf n ".")]
      (if (neg? i) n (subs n (inc i))))))

(defn- target-list-srf?
  "Whether expr is a set-returning function supported by ProjectSet.

   Keep this deliberately narrower than `known-srf-names`: several entries in
   that set are scalar compatibility shims when used outside FROM.  These two
   functions have unambiguous PostgreSQL set semantics in a SELECT list."
  [expr]
  (and (instance? Function expr)
       (contains? #{"generate_series" "unnest"}
                  (srf-base-name (.getName ^Function expr)))))

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
        ;; Correlated iff an argument IS a column reference. This is a
        ;; SYNTACTIC test on purpose. Evaluating instead does not
        ;; discriminate: `srf-const-eval` answers ::corr for a constant
        ;; EXPRESSION too (`array_upper(current_schemas(false), 1)` --
        ;; the pgjdbc TypeInfoCache shape), and the widened evaluation
        ;; the materialise-once path uses goes the other way, happily
        ;; resolving `t.n` against some arbitrary row and reporting it
        ;; constant. A Column argument is exactly what "correlated"
        ;; means, and a function-call argument can never be one.
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
                  ;; Bare columns in a LATERAL VALUES/SELECT body use the
                  ;; same unique-owner lookup as UPDATE ... FROM. Mark these
                  ;; bindings as visible sources so `VALUES (outer_col)` can
                  ;; resolve without a qualifier, as PostgreSQL permits.
                  params/*from-source-aliases* (set (map first corr-refs))
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
   outer-aliases var-counter]
  (let [inner (.getSelect ls)
        values? (instance? Values inner)
        corr-refs (seq (if values?
                         (lateral-values-corr-refs inner outer-aliases schema)
                         (correlated-subquery-refs inner outer-aliases)))]
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
  (expr/eval-correlated-scalar parse-fn sql subquery? inner-schema query-db))

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
  (binding [params/*from-bindings* fb
            *eval-update-db* query-db]
    (case (:kind spec)
      :case
      (let [hit (some (fn [{:keys [when-sql then]}]
                        (when (true? (eval-corr-scalar parse-fn when-sql false inner-schema query-db))
                          [(eval-corr-then parse-fn then inner-schema query-db)]))
                      (:branches spec))]
        (if hit (first hit) (eval-corr-then parse-fn (:else spec) inner-schema query-db)))
      ;; Scalar only -- see the server-side twin for why the :case path is
      ;; deliberately left alone.
      (binding [params/*lateral-outer-aliases* (set (keys fb))]
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
                          rows)))
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
                 keep-idx (vec (keep-indexed
                                (fn [i a] (when-not (and (string? a)
                                                         (.startsWith ^String a "__win_"))
                                            i))
                                aliases))]
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
         col-pg-type (mapv (fn [i]
                             (if-let [ae (nth col-array-elem i)]
                               (str "_" (name ae))
                               (get types/oid-preserving-pg-name (nth sub-oids i nil))))
                           (range (count sub-aliases)))
        ;; Per-column inferred type + coercion fn, computed once.
         col-types (mapv (fn [i] (if (nth col-array-elem i) :db.type/string (col-vtype i)))
                         (range (count sub-aliases)))
         col-coercions (mapv (fn [i]
                               (if (nth col-array-elem i)
                                 (fn [v] (cond
                                           (pg-arr/array? v) (pg-arr/to-pg-text v)
                                           (string? v)       v
                                           :else             (str v)))
                                 (col-coerce (nth col-types i))))
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
                             (nth col-array-elem i) (assoc :pg/array-elem (nth col-array-elem i)))))
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
      :schema  (:schema spec-db2)
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
   when uncorrelated.

   Detection is lexical — it finds `alias.col` occurrences of an OUTER alias
   in the inner SQL (negative-lookbehind so `xt.` doesn't match alias `t`).
   Robust enough for catalog / introspection shapes; AST-precise detection
   (which would also respect inner shadowing) is a later refinement. This is
   the first slice of the correlated-subquery / LATERAL executor — see
   doc/design-alignment.md."
  [inner outer-aliases]
  (when (seq outer-aliases)
    (let [sql (str inner)
          refs (for [a outer-aliases
                     [_ col] (re-seq
                              (re-pattern
                               (str "(?i)(?<![\\w.])"
                                    (java.util.regex.Pattern/quote a)
                                    "\\.([A-Za-z_][A-Za-z0-9_]*)"))
                              sql)]
                 [a (str/lower-case col)])]
      (not-empty (set refs)))))

(defn- unwrap-parens
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
  [^net.sf.jsqlparser.expression.Expression e0 outer-aliases]
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
                              subqs)]
        (when correlated?
          {:kind :case
           :branches (mapv (fn [^net.sf.jsqlparser.expression.WhenClause wc]
                             {:when-sql (str (.getWhenExpression wc))
                              :then (then->spec (.getThenExpression wc))})
                           when-clauses)
           :else (then->spec else-expr)
         ;; All outer refs across the whole CASE (WHEN conditions + subqueries).
           :corr-refs (vec (or (correlated-subquery-refs ce outer-aliases) []))}))

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
                  :case   (some #(get-in % [:then :subquery-sql]) (:branches spec)))]
        (when sql (first (:select-item-oids (parse-fn sql schema db))))))
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

(defn- throwing-projection?
  [form clauses]
  (boolean
   (some throwing-projection-ops
         (filter symbol?
                 (tree-seq coll? seq (cons form (map first clauses)))))))

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
                                           (some throwing-projection-ops
                                                 (filter symbol?
                                                         (tree-seq coll? seq
                                                                   (first clause)))))
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
    (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table))
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
  (let [name-anonymous-derived
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
                  definition (ffirst
                              (d/q '{:find [?definition]
                                     :in [$ ?name-attr ?definition-attr ?view-name]
                                     :where [[?e ?name-attr ?view-name]
                                             [?e ?definition-attr ?definition]]}
                                   db :datahike.pg/view-name
                                   :datahike.pg/view-definition view-name))]
              (if definition
                (let [^net.sf.jsqlparser.statement.select.Select wrapper
                      (CCJSqlParserUtil/parse
                       (str "SELECT * FROM (" definition ") AS __view"))
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
                   (table-fn->virtual-table
                    ^net.sf.jsqlparser.statement.select.TableFunction from-item db)]
            [vdb vschema vname valias]
            ;; An unrecognised function in FROM used to fall through to
            ;; "no relation", so `SELECT * FROM nosuchfunc(1)` answered
            ;; ZERO ROWS and `count(*)` raised the internal
            ;; `Query for unknown vars: [?_eid]`. PostgreSQL raises
            ;; 42883. A name we DO know but could not materialise is a
            ;; different case -- a correlated LATERAL argument -- and
            ;; keeps the old behaviour until LATERAL lands.
            (let [fname (srf-base-name
                         (.getName (.getFunction
                                    ^net.sf.jsqlparser.statement.select.TableFunction from-item)))]
              (if (contains? known-srf-names fname)
                [db schema nil nil]
                (throw (errors/pg-error
                        :undefined-function
                        {:function (str fname "()")
                         :hint (str "No function matches the given name and "
                                    "argument types. You might need to add "
                                    "explicit type casts.")})))))

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
        _ (validate-lateral-join-shapes! select)
        [db schema join-aliases derived-joins lsrf-specs]
        (reduce
         (fn [[db schema aliases derived lsrfs] ^Join j]
           (let [raw-rt (.getRightItem j)
                 rt (-> raw-rt
                        unwrap-derived-parentheses
                        expand-view
                        name-anonymous-derived)
                 _ (when-not (identical? raw-rt rt) (.setRightItem j rt))]
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
                    (lateral-subselect->spec rt db schema outer-alias-set lsrf-var-counter))
               (let [spec (lateral-subselect->spec rt db schema outer-alias-set
                                                   lsrf-var-counter)]
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
        table-aliases (with-meta table-aliases
                        {:relation-aliases (mapv first star-relations)})

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
                                     (ctx/col-var! ctx (keyword a c)))
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
                                          (str/lower-case (.getName ^Function expr))))
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
                     :hints (pgs/schema-hints db)}
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
          (let [fname (str/lower-case (.getName ae))
                agg-sym (get fns/sql-aggregate->datalog fname)
                inner-expr (.getExpression ae)
                filter-expr (.getFilterExpression ae)
                idx (count @find-elements)]
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
              ;; No filter — treat as regular aggregate
              (let [v (if inner-expr (expr/translate-expr ctx inner-expr)
                          (ctx/entity-var! ctx default-table))]
                (swap! find-elements conj (list (or agg-sym 'count) v))
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
                  fname (str/lower-case (.getName f))
                  agg-sym (get fns/sql-aggregate->datalog fname)
                  params (.getParameters f)
                  is-distinct? (.isDistinct f)]
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
        correlated-subqs
        (when db
          (into []
                (keep-indexed
                 (fn [i ^SelectItem item]
                   (when-let [spec (correlated-select-item-spec (.getExpression item) outer-aliases)]
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
                  alias-str (select-item-alias item)]
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
                (doseq [[ali real] star-relations]
                  (doseq [col (pgs/column-info schema real db)
                          :when (not= "db_id" (:name col))]
                    ;; Route through the [:aliased …] form so the column
                    ;; binds against the alias's entity var, matching the
                    ;; `t.*` expansion.
                    (let [v (expr/column-value! ctx (if (= real ali)
                                                      (:attr col)
                                                      [:aliased ali (:attr col)]))]
                      (swap! find-elements conj v)
                      (swap! find-aliases conj (or alias-str (:name col))))))

                ;; AnalyticExpression: FILTER aggregate or window function.
                ;; FILTER: has filterExpression, no partition/orderBy/window.
                ;; WINDOW: has partition/orderBy/window, or is a ranking function.
                (instance? net.sf.jsqlparser.expression.AnalyticExpression expr)
                (let [^net.sf.jsqlparser.expression.AnalyticExpression ae expr
                      fname (str/lower-case (.getName ae))
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
                    (let [;; `OVER w` names a window defined once in the
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
                          win-spec (cond-> {:op op-kw
                                            :partition-by (or part-idxs [])
                                            :order-by (or ob-specs [])
                                            :frame frame}
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
                     (fns/aggregate-function? (str/lower-case (.getName ^Function expr))))
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
                      v (expr/translate-expr (assoc ctx
                                                    :hoisted-aggs sink
                                                    :aggregate-projection?
                                                    aggregate-projection?) expr)
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
                          defer? (and where-expr
                                      (nil? group-by-element)
                                      (not has-distinct?)
                                      (empty? (.getOrderByElements select))
                                      (throwing-projection? v emitted))]
                      (if defer?
                        (let [[keep-cs form] (inline-projection-bindings v emitted)
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
        [select-item-oids* select-item-param-idx*]
        (let [[oids idxs]
              (reduce
               (fn [[v pv] ^SelectItem item]
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
                        (into pv (repeat (count picked) nil))])
                     (instance? AllColumns expr)
                     (let [cols (pgs/column-info
                                 schema (get table-aliases default-table default-table) db)
                           picked (remove #(= "db_id" (:name %)) cols)]
                       [(into v (map :oid) picked)
                        (into pv (repeat (count picked) nil))])
                     :else
                     [(conj v (oid/expr-oid expr oid-env))
                      (conj pv (oid/param-placeholder-index expr))])))
               [[] []]
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
          [(pad oids) (pad idxs)])
        select-item-oids select-item-oids*
        select-item-param-idx select-item-param-idx*

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
          (let [fname (str/lower-case (.getName f))
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
                      (when (instance? LongValue rc)
                        (.getValue ^LongValue rc))))
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
                       (when (instance? LongValue ofs)
                         (.getValue ^LongValue ofs))))

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
              (if (:value-join? ref-info)
                ;; VALUE-EQUALITY LEFT JOIN: ON t1.a = t2.x
                ;; (RIGHT JOINs are rewritten to LEFT at AST level)
                (let [{:keys [left-key-var right-key-attr right-alias left-evar
                              matched-only-preds]} ref-info
                      right-evar (ctx/entity-var! ctx right-alias)
                      ;; Convert left join key to get-else so NULL-key entities are included.
                      ;; They'll go to the unmatched branch via not-join.
                      _ (let [clauses @(:where-clauses ctx)
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
                                            @(:where-clauses ctx))))))
                      all-clauses @(:where-clauses ctx)
                      ;; Right-side clauses: data patterns on the right entity var
                      right-clause? (fn [clause]
                                      (and (vector? clause) (= 3 (count clause))
                                           (= right-evar (first clause))
                                           (keyword? (second clause))))
                      right-clauses (vec (filter right-clause? all-clauses))
                      left-clauses (vec (remove right-clause? all-clauses))
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
                      right-key-clause (first (filter #(= right-key-attr (second %)) right-clauses))
                      right-non-key (vec (remove #(or (= right-key-attr (second %))
                                                      (= right-marker (second %)))
                                                 right-clauses))
                      ;; Right-side value variables (from non-key patterns)
                      right-val-vars (vec (distinct
                                           (keep (fn [[_ _ v]] v) right-non-key)))
                      ;; Also include the right-side key var if it was requested in SELECT
                      right-key-var (when right-key-clause (nth right-key-clause 2))
                      all-right-vars (vec (distinct
                                           (concat (when (and right-key-var
                                                              (not= right-key-var left-key-var))
                                                     [right-key-var])
                                                   right-val-vars)))
                      ;; Shared vars for or-join: left-key + all right-side vars + right entity var
                      shared-vars (vec (distinct (concat [left-key-var right-evar] all-right-vars)))
                      ;; Branch 1: matched — right entity with matching key
                      ;; Use get-else for non-key right columns (they may be NULL)
                      matched-key [right-evar right-key-attr left-key-var]
                      matched-non-key (mapv (fn [[_e a v]]
                                              [(list 'get-else '$ right-evar a :__null__) v])
                                            right-non-key)
                      ;; If right key var was in SELECT and differs from left key, bind it
                      matched-key-bind (when (and right-key-var (not= right-key-var left-key-var))
                                         [[(list 'identity left-key-var) right-key-var]])
                      ;; Right-side filter predicates from the ON clause
                      ;; (e.g. `… AND d.objsubid = 0`) — applied inside
                      ;; the matched branch only. Without this they'd
                      ;; act as global filters and convert the LEFT JOIN
                      ;; into an INNER JOIN.
                      matched-parts (into [matched-key]
                                          (concat matched-non-key
                                                  matched-key-bind
                                                  matched-only-preds))
                      matched (apply list 'and matched-parts)
                      ;; Branch 2: unmatched — no right entity with this key
                      null-bindings (into [[(list 'ground :__null__) right-evar]]
                                          (mapv (fn [v] [(list 'ground :__null__) v])
                                                all-right-vars))
                      unmatched (apply list 'and
                                       (into [(list 'not-join [left-key-var]
                                                    [right-evar right-key-attr left-key-var])]
                                             null-bindings))
                      oj-clause (list* 'or-join shared-vars matched unmatched nil)]
                  ;; Add right entity var to :with for dedup prevention
                  (swap! (:with-vars ctx) conj right-evar)
                  (reset! (:where-clauses ctx) (conj left-clauses oj-clause)))

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
                (let [{:keys [ref-var ref-attr left-table-evar]} ref-info
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
                      shared-vars (vec (distinct
                                        (concat
                                         (when left-evar [left-evar])
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
                      matched (apply list 'and
                                     (concat matched-ref-bind right-clauses))
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
                        ;; "no posting points at this transaction"
                        (let [inner-eid (gensym "?lj-inner-")]
                          [(list 'not-join [left-evar]
                                 [inner-eid ref-attr left-evar])])
                        ;; legacy: ref-var = :__null__
                        [[(list '= ref-var :__null__)]])
                      unmatched (apply list 'and
                                       (concat not-match-guard null-bindings))
                      oj-clause (list* 'or-join shared-vars matched unmatched nil)]
                  (reset! (:where-clauses ctx)
                          (conj left-clauses oj-clause))))))

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
        has-nullable-order? (and order-by-spec
                                 (or fetch-with-ties?
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
        ;; A constant implicit HAVING group deliberately detached from its
        ;; source relation above must not keep that relation's entity id in
        ;; :with: it is now unbound and would suppress the singleton row.
        with-vars (if degenerate-having? [] @(:with-vars ctx))
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
                    (assoc :order-by order-by-flat))]

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
      ;; Include join metadata for outer join handling
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
                     (str/lower-case (str (.getDataType col-data-type))))
          cast-cat (types/cast-category type-str)
          ;; CAST(<x> AS T[]): JSqlParser exposes the array dim via
          ;; getArrayData (a list, size = ndim) rather than embedding
          ;; `[]` in the type string. We retype an empty / untyped
          ;; PgArray to match the target element-keyword and pass
          ;; non-array inputs through `pg-arr/array` for consistency.
          array-data (try (.getArrayData col-data-type) (catch Throwable _ nil))
          array-target? (and (some? type-str) (seq array-data))]
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

(defn- eval-const-expr
  "Evaluate a constant scalar expression by running it as a one-row
   SELECT. Returns nil when it cannot be evaluated — an unimplemented
   function, or no parse hook in scope — so callers can fall back."
  [e schema db]
  (try
    (when-let [pf params/*parse-sql*]
      (when (and schema db)
        (let [p (pf (str "SELECT " e) schema db)]
          (if-let [q (:query p)]
            (let [ia (:in-args p)
                  qdb (or (:enriched-db p) db)
                  r (first (if (seq ia) (apply d/q q qdb ia) (d/q q qdb)))]
              (widen-integral (if (sequential? r) (first r) r)))
            (let [lr (:literal-row p)]
              (widen-integral (if (sequential? lr) (first lr) lr)))))))
    (catch Throwable _ nil)))

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
     (apply-sql-cast (extract-value (.getLeftExpression ^CastExpression e) schema db)
                     ^CastExpression e)
     (instance? Addition e)
     (let [^Addition expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-+)]
       (if (= ::unhandled value)
         (or (eval-const-expr e schema db) (str e))
         value))
     (instance? Subtraction e)
     (let [^Subtraction expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql--)]
       (if (= ::unhandled value)
         (or (eval-const-expr e schema db) (str e))
         value))
     (instance? Multiplication e)
     (let [^Multiplication expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-*)]
       (if (= ::unhandled value)
         (or (eval-const-expr e schema db) (str e))
         value))
     (instance? Division e)
     (let [^Division expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-div)]
       (if (= ::unhandled value)
         (or (eval-const-expr e schema db) (str e))
         value))
     (instance? Modulo e)
     (let [^Modulo expression e
           value (extract-numeric-binary (.getLeftExpression expression)
                                         (.getRightExpression expression)
                                         schema db fns/sql-mod)]
       (if (= ::unhandled value)
         (or (eval-const-expr e schema db) (str e))
         value))
    ;; Parenthesized single expression — unwrap
     (instance? ParenthesedExpressionList e)
     (let [^ParenthesedExpressionList pel e]
       (if (= (count pel) 1)
         (extract-value (first pel) schema db)
         (str e)))
    ;; Scalar subquery: (SELECT id FROM table WHERE ...)
     (instance? ParenthesedSelect e)
     (if db
       (let [inner (.getSelect ^ParenthesedSelect e)]
         (if (instance? PlainSelect inner)
           (let [parsed (if params/*parse-sql*
                          (params/*parse-sql* (str inner) schema db)
                          (translate-select ^PlainSelect inner schema db))
                 _ (when (= :error (:type parsed))
                     (throw (ex-info (:message parsed)
                                     {:sqlstate (or (:sqlstate parsed) "XX000")})))
                 q (:query parsed)
                 in-args (:in-args parsed)
                 query-db (or (:enriched-db parsed) db)
                 results (cond
                           (:literal-rows parsed) (:literal-rows parsed)
                           (:literal-row parsed) [(:literal-row parsed)]
                           (seq in-args) (apply d/q q query-db in-args)
                           :else (d/q q query-db))
                 first-row (first results)]
             (if (sequential? first-row) (first first-row) first-row))
           nil))
       nil)
     (instance? net.sf.jsqlparser.expression.Function e)
     (let [^net.sf.jsqlparser.expression.Function f e
           fname (str/lower-case (.getName f))]
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
         ;; Falls back to the old text behaviour when the expression
         ;; cannot be evaluated (an unimplemented function), rather than
         ;; storing nil — which would turn a wrong value into a failed
         ;; INSERT.
         :else (or (eval-const-expr e schema db) (str e))))
    ;; Bare CURRENT_TIMESTAMP / CURRENT_DATE / CURRENT_TIME (no parens)
    ;; parse as TimeKeyExpression, not Function — same marker as the
    ;; function forms above; without this branch the keyword fell
    ;; through to `(str e)` and the literal string reached the
    ;; transactor (issue #14).
     (instance? TimeKeyExpression e)
     {:fn :now}
     (instance? TimezoneExpression e)
    ;; now() AT TIME ZONE 'UTC' → current timestamp marker, like the
    ;; bare-function case above.
     (let [left (.getLeftExpression ^TimezoneExpression e)]
       (if (and (instance? net.sf.jsqlparser.expression.Function left)
                (= "now" (str/lower-case (.getName ^net.sf.jsqlparser.expression.Function left))))
         {:fn :now}
         {:fn :now}))  ;; any timezone expression defaults to current time

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

     :else (str e))))

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
  (when (some? val)
    ;; A deferred call marker is NOT a value yet — `{:fn :nextval …}`,
    ;; `{:fn :now}`, `{:fn :eval …}` are resolved per execute, after
    ;; this. Coercing one here would treat the marker MAP as data: for a
    ;; jsonb or text column it serialised to `{":fn": ":eval", …}` and
    ;; that reached the transactor.
    (if (params/call-marker? val)
      val
      (let [vtype     (get-in schema [attr :db/valueType])
            elem-kw   (get-in schema [attr :pg/array-elem])
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
            jsonb?    (= "jsonb" pg-type)
            ;; The declared integer width, when the column has one.
            int-type  (when (contains? #{"int2" "int4" "int8"} pg-type) pg-type)
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
          (coerce/coerce-numeric val :double)
          (and (= vtype :db.type/float)
               (or (string? val) (integer? val) (decimal? val)))
          (sql-cast/cast-to-float val "real")
        ;; PG boolin: 't'/'yes'/'on'/'1' etc. — Boolean/parseBoolean
        ;; would silently turn '1' into false (issue #12).
          (and (= vtype :db.type/boolean) (string? val))
          (let [b (coerce/parse-bool-token val)]
            (when (nil? b)
              (throw (errors/pg-error :invalid-text-representation
                                      {:type "boolean" :value val})))
            b)
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
        ;; from CAST) directly. String parse handled below by
        ;; falling through to coerce-unknown.
          (and (= vtype :db.type/uuid) (instance? java.util.UUID val)) val
          (and (= vtype :db.type/uuid) (string? val))
          (coerce/parse-uuid val)
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
                                      (java.time.ZoneOffset/UTC))))
          (and (= vtype :db.type/instant) (instance? java.time.LocalDateTime val))
          (java.util.Date/from
           (.toInstant ^java.time.LocalDateTime val
                       (java.time.ZoneOffset/UTC)))
          (and (= vtype :db.type/instant) (instance? java.time.OffsetDateTime val))
          (java.util.Date/from (.toInstant ^java.time.OffsetDateTime val))
          (and (= vtype :db.type/instant) (instance? java.time.ZonedDateTime val))
          (java.util.Date/from (.toInstant ^java.time.ZonedDateTime val))
          :else val)))))

(declare eval-update-expr eval-update-cond)

(def ^:dynamic *eval-update-db*
  "Bound by build-update-tx-for-bindings to the live db when evaluating
   per-row UPDATE SET expressions. Read by the Function (`concat`,
   etc.) and ParenthesedSelect (scalar subquery) branches of
   `eval-update-expr` — which need a db handle to dispatch to
   translate-select for inner subqueries. nil when an UPDATE has no
   subquery / function-call assignments, which is the common case."
  nil)

(defn eval-check-predicate
  "Evaluate a CHECK-style JSqlParser Expression against an entity map
   and return a tri-state: true (satisfied), false (violation), or
   nil (unknown — PG treats as satisfied). This is distinct from
   eval-update-expr which returns the arithmetic value of the
   expression; predicates need comparison / logical ops that only
   make sense at enforcement time.

   Column refs resolve via eval-update-expr so any entity-map-aware
   coercion (namespace-qualified lookups) stays consistent between
   SET-value evaluation and CHECK evaluation."
  [expr entity-map ns-str schema]
  (letfn [(operand [e] (eval-update-expr e entity-map ns-str schema))
          (bool-cmp [^net.sf.jsqlparser.expression.BinaryExpression e op]
            (let [l (operand (.getLeftExpression e))
                  r (operand (.getRightExpression e))]
              (when (and (some? l) (some? r)) (op l r))))
          (num-pair [l r]
            (cond
              (and (number? l) (number? r)) [l r]
              (and (some? l) (some? r))
              (try [(Double/parseDouble (str l)) (Double/parseDouble (str r))]
                   (catch Exception _ nil))
              :else nil))]
    (cond
      (instance? net.sf.jsqlparser.expression.operators.relational.EqualsTo expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (operand (.getLeftExpression e))
            r (operand (.getRightExpression e))]
        (when (and (some? l) (some? r))
          (cond
            (and (number? l) (number? r)) (== l r)
            :else (= l r))))
      (instance? net.sf.jsqlparser.expression.operators.relational.NotEqualsTo expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (operand (.getLeftExpression e))
            r (operand (.getRightExpression e))]
        (when (and (some? l) (some? r))
          (cond
            (and (number? l) (number? r)) (not (== l r))
            :else (not= l r))))
      (instance? GreaterThan expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (operand (.getLeftExpression e))
            r (operand (.getRightExpression e))]
        (when-let [[a b] (num-pair l r)] (> a b)))
      (instance? GreaterThanEquals expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (operand (.getLeftExpression e))
            r (operand (.getRightExpression e))]
        (when-let [[a b] (num-pair l r)] (>= a b)))
      (instance? MinorThan expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (operand (.getLeftExpression e))
            r (operand (.getRightExpression e))]
        (when-let [[a b] (num-pair l r)] (< a b)))
      (instance? MinorThanEquals expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (operand (.getLeftExpression e))
            r (operand (.getRightExpression e))]
        (when-let [[a b] (num-pair l r)] (<= a b)))
      (instance? net.sf.jsqlparser.expression.operators.conditional.AndExpression expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (eval-check-predicate (.getLeftExpression e) entity-map ns-str schema)
            r (eval-check-predicate (.getRightExpression e) entity-map ns-str schema)]
        ;; PG 3VL: AND of (true,unknown) = unknown; (false,x) = false.
        (cond (or (false? l) (false? r)) false
              (and (true? l) (true? r)) true
              :else nil))
      (instance? net.sf.jsqlparser.expression.operators.conditional.OrExpression expr)
      (let [^net.sf.jsqlparser.expression.BinaryExpression e expr
            l (eval-check-predicate (.getLeftExpression e) entity-map ns-str schema)
            r (eval-check-predicate (.getRightExpression e) entity-map ns-str schema)]
        (cond (or (true? l) (true? r)) true
              (and (false? l) (false? r)) false
              :else nil))
      (instance? net.sf.jsqlparser.expression.NotExpression expr)
      (let [^net.sf.jsqlparser.expression.NotExpression e expr
            v (eval-check-predicate (.getExpression e) entity-map ns-str schema)]
        (when (some? v) (not v)))
      (instance? net.sf.jsqlparser.expression.operators.relational.IsNullExpression expr)
      (let [^net.sf.jsqlparser.expression.operators.relational.IsNullExpression e expr
            v (operand (.getLeftExpression e))]
        (if (.isNot e) (some? v) (nil? v)))
      (instance? net.sf.jsqlparser.expression.Parenthesis expr)
      (eval-check-predicate
       (.getExpression ^net.sf.jsqlparser.expression.Parenthesis expr)
       entity-map ns-str schema)
      (instance? net.sf.jsqlparser.expression.operators.relational.InExpression expr)
      (let [^net.sf.jsqlparser.expression.operators.relational.InExpression e expr
            l (operand (.getLeftExpression e))
            rlist (.getRightExpression e)
            items (when (instance? net.sf.jsqlparser.expression.operators.relational.ExpressionList rlist)
                    (mapv operand
                          (.getExpressions
                           ^net.sf.jsqlparser.expression.operators.relational.ExpressionList rlist)))
            hit? (boolean (and items (some #(= l %) items)))]
        (if (.isNot e) (not hit?) hit?))

      ;; `x BETWEEN lo AND hi` — symmetric, inclusive bounds, PG 3VL.
      ;; Without this clause the :else fallback stringified the
      ;; expression and returned a truthy "expression-text" — domains
      ;; like `CHECK (VALUE BETWEEN 1 AND 100)` then accepted any
      ;; value silently.
      (instance? net.sf.jsqlparser.expression.operators.relational.Between expr)
      (let [^net.sf.jsqlparser.expression.operators.relational.Between e expr
            v  (operand (.getLeftExpression e))
            lo (operand (.getBetweenExpressionStart e))
            hi (operand (.getBetweenExpressionEnd e))]
        (if (or (nil? v) (nil? lo) (nil? hi))
          nil
          (let [in? (or (when-let [[a b c] (and (number? v) (number? lo) (number? hi)
                                                [v lo hi])]
                          (and (>= a b) (<= a c)))
                        (try
                          (let [[a b c] [(Double/parseDouble (str v))
                                         (Double/parseDouble (str lo))
                                         (Double/parseDouble (str hi))]]
                            (and (>= a b) (<= a c)))
                          (catch Exception _ nil)))]
            (cond
              (nil? in?) nil
              (.isNot e) (not in?)
              :else      in?))))

      ;; Leaf truthy-check — `CHECK (active)` where `active` is a
      ;; boolean column lands here. We only enter this branch for
      ;; shapes whose `operand` result is genuinely a stored value
      ;; (Column ref, scalar literal). Other AST shapes (Function,
      ;; LikeExpression, RegExp, IS DISTINCT FROM, …) return `nil`
      ;; below to honestly admit "unknown" — better to leave a
      ;; row uncommitted-as-validated than to silently mark every
      ;; row as passing because the operand stringified to a non-
      ;; empty SQL fragment.
      (or (instance? Column expr)
          (instance? LongValue expr)
          (instance? DoubleValue expr)
          (instance? StringValue expr)
          (instance? BooleanValue expr)
          (instance? NullValue expr))
      (let [v (operand expr)]
        (cond (nil? v) nil
              (false? v) false
              :else true))

      ;; Unrecognised shape — return nil (PG 3VL unknown). This
      ;; matches the conservative "we couldn't evaluate, treat as
      ;; satisfied" stance for CHECK constraints, but distinct from
      ;; the explicit `true` we emit when we DID evaluate to a
      ;; satisfied predicate. A future LIKE / regex / function-call
      ;; clause should land above this `:else`.
      :else nil)))

(defn- num-operand
  "PG-style unknown-operand resolution for arithmetic: a text-format
   wire parameter decodes as a String; in numeric context PG casts it
   to the numeric type. Numeric-looking strings parse (long or double
   by shape); everything else passes through unchanged so non-numeric
   operands still fail the arithmetic's number? guards."
  [x]
  (if (and (string? x)
           (re-matches #"\s*[+-]?\d+(\.\d+)?([eE][+-]?\d+)?\s*" x))
    (try (coerce/coerce-numeric x (if (re-find #"[.eE]" x) :double :long))
         (catch Exception _ x))
    x))

(defn- sql-numeric? [x]
  (or (number? x) (types/numeric-special? x)))

(defn- temporal-value?
  "A stored date/timestamp. Dates come back as java.util.Date from the
   :db.type/instant attribute; the java.time types appear via casts."
  [v]
  (or (instance? java.util.Date v)
      (instance? java.time.LocalDate v)
      (instance? java.time.LocalDateTime v)
      (instance? java.time.Instant v)))

(defn- shift-days
  "`date + n` / `date - n`: shift by n DAYS, preserving the value's type."
  [v ^long n]
  (cond
    (instance? java.time.LocalDate v)     (.plusDays ^java.time.LocalDate v n)
    (instance? java.time.LocalDateTime v) (.plusDays ^java.time.LocalDateTime v n)
    (instance? java.time.Instant v)       (.plus ^java.time.Instant v n java.time.temporal.ChronoUnit/DAYS)
    (instance? java.util.Date v)
    (java.util.Date/from (.plus (.toInstant ^java.util.Date v) n java.time.temporal.ChronoUnit/DAYS))
    :else nil))

(defn eval-update-cond
  "Evaluate a boolean expression in UPDATE SET position -- the tests of a
   CASE. Three-valued: returns true / false / :__null__, so the caller can
   apply PostgreSQL's rule that a branch is taken only on TRUE.

   A small evaluator rather than a reuse of the WHERE translator: this runs
   per ENTITY against an already-materialised entity-map, not as a datalog
   clause over the whole relation."
  [expr entity-map ns-str schema]
  (let [ev (fn [e] (eval-update-expr e entity-map ns-str schema))
        ;; SELECT-list predicates and UPDATE expressions are evaluated here,
        ;; outside expr/translate-comparison's Datalog path. Preserve the
        ;; same PostgreSQL rule in both lowering paths: an unknown string
        ;; literal takes the known column operand's declared type. Runtime
        ;; class is insufficient for money because numeric shares its
        ;; BigDecimal carrier.
        comparison-values
        (fn [left right]
          (let [coerce-one
                (fn [typed unknown v]
                  (if (and (instance? Column typed)
                           (instance? StringValue unknown)
                           (let [attr (keyword ns-str
                                               (unquote-ident
                                                (.getColumnName ^Column typed)))]
                             (= "money"
                                (or (get-in schema [attr :pg/type])
                                    (params/pg-type-of-attr nil attr)))))
                    (sql-cast/parse-money v)
                    v))]
            [(coerce-one right left (ev left))
             (coerce-one left right (ev right))]))]
    (cond
      (instance? AndExpression expr)
      (fns/sql-and3 (eval-update-cond (.getLeftExpression ^AndExpression expr) entity-map ns-str schema)
                    (eval-update-cond (.getRightExpression ^AndExpression expr) entity-map ns-str schema))
      (instance? OrExpression expr)
      (fns/sql-or3 (eval-update-cond (.getLeftExpression ^OrExpression expr) entity-map ns-str schema)
                   (eval-update-cond (.getRightExpression ^OrExpression expr) entity-map ns-str schema))
      (instance? net.sf.jsqlparser.expression.NotExpression expr)
      (fns/sql-not3 (eval-update-cond (.getExpression ^net.sf.jsqlparser.expression.NotExpression expr)
                                      entity-map ns-str schema))
      (instance? Parenthesis expr)
      (eval-update-cond (.getExpression ^Parenthesis expr) entity-map ns-str schema)
      (instance? IsNullExpression expr)
      (let [v (ev (.getLeftExpression ^IsNullExpression expr))]
        (if (.isNot ^IsNullExpression expr) (some? v) (nil? v)))
      (instance? EqualsTo expr)
      (apply fns/sql-eq3? (comparison-values (.getLeftExpression ^EqualsTo expr)
                                             (.getRightExpression ^EqualsTo expr)))
      (instance? NotEqualsTo expr)
      (apply fns/sql-ne3? (comparison-values (.getLeftExpression ^NotEqualsTo expr)
                                             (.getRightExpression ^NotEqualsTo expr)))
      (instance? GreaterThan expr)
      (apply fns/sql-gt3? (comparison-values (.getLeftExpression ^GreaterThan expr)
                                             (.getRightExpression ^GreaterThan expr)))
      (instance? GreaterThanEquals expr)
      (apply fns/sql-ge3? (comparison-values (.getLeftExpression ^GreaterThanEquals expr)
                                             (.getRightExpression ^GreaterThanEquals expr)))
      (instance? MinorThan expr)
      (apply fns/sql-lt3? (comparison-values (.getLeftExpression ^MinorThan expr)
                                             (.getRightExpression ^MinorThan expr)))
      (instance? MinorThanEquals expr)
      (apply fns/sql-le3? (comparison-values (.getLeftExpression ^MinorThanEquals expr)
                                             (.getRightExpression ^MinorThanEquals expr)))
      (instance? BooleanValue expr) (.getValue ^BooleanValue expr)

      ;; The predicate surface eval-update-expr does not itself decide.
      ;; These all have three-valued implementations already.
      (instance? Between expr)
      (let [^Between b expr
            v (ev (.getLeftExpression b))]
        (if (.isNot b)
          (fns/sql-not-between? v (ev (.getBetweenExpressionStart b)) (ev (.getBetweenExpressionEnd b)))
          (fns/sql-between? v (ev (.getBetweenExpressionStart b)) (ev (.getBetweenExpressionEnd b)))))

      (instance? LikeExpression expr)
      (let [^LikeExpression l expr
            v (ev (.getLeftExpression l))
            pat (ev (.getRightExpression l))
            r (fns/sql-like3? v (expr/like-pattern->regex (str pat) (.isCaseInsensitive l)))]
        (if (.isNot l) (fns/sql-not3 r) r))

      (instance? InExpression expr)
      (let [^InExpression i expr
            v (ev (.getLeftExpression i))
            right (.getRightExpression i)
            vals (when (instance? ParenthesedExpressionList right)
                   (mapv ev ^ParenthesedExpressionList right))
            r (if vals (fns/sql-in3? (set vals) v) :__null__)]
        (if (.isNot i) (fns/sql-not3 r) r))

      (instance? JsonOperator expr)
      (let [^JsonOperator jo expr
            l (ev (.getLeftExpression jo))
            r (ev (.getRightExpression jo))]
        (case (.getStringExpression jo)
          "@>" (jb/jsonb-contains? l r)
          "<@" (jb/jsonb-contained? l r)
          "?"  (jb/jsonb-exists? l r)
          "?|" (jb/jsonb-exists-any? l r)
          "?&" (jb/jsonb-exists-all? l r)
          :__null__))

      ;; A bare column or a value expression used as a boolean.
      (or (instance? Column expr) (instance? JdbcParameter expr))
      (let [v (ev expr)] (cond (nil? v) :__null__ (= :__null__ v) :__null__ :else (boolean v)))

      ;; Anything else is REFUSED. The old `:else` evaluated the node with
      ;; eval-update-expr and took its truthiness -- and eval-update-expr
      ;; STRINGIFIED whatever it did not know, so a non-empty string made
      ;; EVERY unrecognised predicate unconditionally TRUE. `UPDATE t SET x
      ;; = CASE WHEN s LIKE 'zzz%' THEN 'a' ELSE 'b' END` took the THEN
      ;; branch on every row.
      :else
      (throw (ex-info "UPDATE SET condition not supported"
                      {:error :feature-not-supported
                       :feature (str "UPDATE SET condition of type "
                                     (.getName ^Class (type expr)))
                       :expr (str expr)})))))

(defn eval-update-expr
  "Evaluate an UPDATE SET expression for a specific entity.
   For simple literals, returns the literal value.
   For expressions (col + 1, col * 2), evaluates against the entity's current values.

   JdbcParameter placeholders return a ParamRef — the tx-build step runs
   substitute-params once bound values are available from Bind."
  [value-expr entity-map ns-str schema]
  (cond
    (instance? JdbcParameter value-expr)
    ;; Execute-time re-evaluation (*bound-params* bound): resolve to the
    ;; concrete value so arithmetic like `SET bal = bal + $1` computes —
    ;; a ParamRef operand made `+` throw ClassCastException (hit by
    ;; pgbench -M prepared). Parse time keeps the ParamRef sentinel for
    ;; the tx-build substitution pass.
    (let [idx (.getIndex ^JdbcParameter value-expr)]
      (if-let [bound params/*bound-params*]
        (nth bound (dec (long idx)))
        (->ParamRef idx)))

    (instance? LongValue value-expr)
    (.getValue ^LongValue value-expr)

    (instance? DoubleValue value-expr)
    (types/decimal-literal value-expr (.getValue ^DoubleValue value-expr))

    (instance? StringValue value-expr)
    (expr/string-value-text ^StringValue value-expr)

    (instance? BooleanValue value-expr)
    (.getValue ^BooleanValue value-expr)

    (instance? NullValue value-expr)
    nil

    ;; Column reference — look up current value (or EXCLUDED.col for upsert,
    ;; or the VALUES alias for UPDATE ... FROM (VALUES ...))
    (instance? Column value-expr)
    (let [^Column col-expr value-expr
          col-name (unquote-ident (.getColumnName col-expr))
          tbl (.getTable col-expr)
          tbl-name (when tbl (unquote-ident (.getName ^Table tbl)))
          binding-owners (when (and (nil? tbl-name) (seq params/*from-source-aliases*))
                           (params/binding-column-owners params/*from-bindings* col-name))
          target-column? (contains? schema (keyword ns-str col-name))]
      (cond
        ;; Bound by the current UPDATE ... FROM row (or the materialised
        ;; target row used by correlated UPDATE expressions).
        (and tbl-name params/*from-bindings* (contains? params/*from-bindings* tbl-name))
        (get-in params/*from-bindings* [tbl-name col-name])

        (and (nil? tbl-name) (> (count binding-owners) 1))
        (params/ambiguous-column! col-name)

        (and (nil? tbl-name) (= 1 (count binding-owners)) target-column?)
        (params/ambiguous-column! col-name)

        (and (nil? tbl-name) (= 1 (count binding-owners)))
        (get-in params/*from-bindings* [(first binding-owners) col-name])

        (and tbl-name (= "EXCLUDED" (.toUpperCase ^String tbl-name)))
        (get entity-map (keyword "excluded" col-name))

        :else
        (let [attr (keyword ns-str col-name)
              v (get entity-map attr)
              v (if (= :db.type/bigdec (get-in schema [attr :db/valueType]))
                  (types/numeric-storage->value v)
                  v)
              ;; `a[1]` parses as a COLUMN with an array constructor
              ;; attached, not as an ArrayExpression -- so the plain column
              ;; lookup returned the WHOLE array and the subscript was
              ;; silently dropped.
              subs (some-> (.getArrayConstructor col-expr) .getExpressions)]
          (if-let [idx (when (= 1 (count subs))
                         (let [i (eval-update-expr (first subs) entity-map ns-str schema)]
                           (when (integer? i) (long i))))]
            (let [arr (cond (pg-arr/array? v) v
                            (string? v) (try (pg-arr/from-pg-text v :unknown)
                                             (catch Throwable _ nil)))]
              (when arr (nth (pg-arr/flat-elements arr) (dec idx) nil)))
            v))))

    ;; Arithmetic context: a wire parameter of unknown type decodes as a
    ;; String; PG resolves `int + $1` by casting the unknown operand to
    ;; the numeric type. Mirror that for numeric-looking strings only —
    ;; anything else keeps its type and fails the arithmetic like PG's
    ;; 22P02 would.
    (instance? Addition value-expr)
    (let [^Addition e value-expr
          l0 (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r0 (eval-update-expr (.getRightExpression e) entity-map ns-str schema)
          l (num-operand l0)
          r (num-operand r0)]
      (cond
        ;; `date + n` shifts by n DAYS. A date column is stored as a
        ;; java.util.Date, which num-operand turns into nothing, so
        ;; `UPDATE t SET d = d + 1` silently WIPED the column instead of
        ;; advancing it.
        (and (temporal-value? l0) (number? r)) (shift-days l0 (long r))
        (and (number? l) (temporal-value? r0)) (shift-days r0 (long l))
        (and (sql-numeric? l) (sql-numeric? r)) (fns/sql-+ l r)))

    ;; Negative literal operand: `SET x = x + -123` parses the RHS as a
    ;; SignedExpression; without this branch it fell to `(str value-expr)`
    ;; and the arithmetic threw String→Number (hit by pgbench's tpcb
    ;; script, whose :delta is uniform over [-5000, 5000]).
    (instance? SignedExpression value-expr)
    (let [^SignedExpression se value-expr
          inner (eval-update-expr (.getExpression se) entity-map ns-str schema)]
      (when (number? inner)
        (if (= \- (.getSign se)) (- inner) inner)))

    ;; Subtraction: numeric subtract or jsonb key deletion (col - 'key' or col - idx)
    (instance? Subtraction value-expr)
    (let [^Subtraction e value-expr
          l (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r (eval-update-expr (.getRightExpression e) entity-map ns-str schema)]
      (cond
        (and (nil? l)) nil
        ;; `date - n` shifts back n DAYS, the mirror of the Addition branch.
        ;; Checked BEFORE the jsonb-deletion case, which would otherwise
        ;; try to parse a Date as JSON.
        (and (temporal-value? l) (number? (num-operand r)))
        (shift-days l (- (long (num-operand r))))
        ;; jsonb key/index deletion: left is jsonb (string containing JSON, map, or vector)
        (and (some? l) (some? r)
             (or (map? (jb/parse-jsonb l)) (sequential? (jb/parse-jsonb l))))
        (let [result (if (integer? r)
                       (jb/jsonb-delete-idx l (long r))
                       (jb/jsonb-delete-key l (str r)))]
          (jb/serialize-jsonb result))
        ;; Numeric subtraction (num-operand: unknown-type wire params
        ;; arrive as strings — cast in numeric context like PG)
        (and (sql-numeric? (num-operand l)) (sql-numeric? (num-operand r)))
        (fns/sql-- (num-operand l) (num-operand r))
        :else nil))

    (instance? Multiplication value-expr)
    (let [^Multiplication e value-expr
          l (num-operand (eval-update-expr (.getLeftExpression e) entity-map ns-str schema))
          r (num-operand (eval-update-expr (.getRightExpression e) entity-map ns-str schema))]
      (when (and (sql-numeric? l) (sql-numeric? r)) (fns/sql-* l r)))

    (instance? Division value-expr)
    (let [^Division e value-expr
          l (num-operand (eval-update-expr (.getLeftExpression e) entity-map ns-str schema))
          r (num-operand (eval-update-expr (.getRightExpression e) entity-map ns-str schema))]
      ;; fns/sql-div, not `/`, and NO zero guard. Skipping a zero divisor
      ;; produced nil, which this function's caller reads as `SET col =
      ;; NULL` -- so `UPDATE t SET c = x/0` reported success and RETRACTED
      ;; the column, where PostgreSQL raises 22012 and leaves the row
      ;; alone. sql-div also brings integer division, so `SET i = 7/2`
      ;; stores 3 rather than the Ratio 7/2.
      (when (and (sql-numeric? l) (sql-numeric? r)) (fns/sql-div l r)))

    ;; String/jsonb concatenation: col || '...'::jsonb merges jsonb; string concat otherwise
    (instance? Concat value-expr)
    (let [^Concat e value-expr
          l (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r (eval-update-expr (.getRightExpression e) entity-map ns-str schema)]
      (when (and (some? l) (some? r))
        (let [lp (jb/parse-jsonb l)
              rp (jb/parse-jsonb r)]
          (if (or (map? lp) (sequential? lp) (map? rp) (sequential? rp))
            ;; jsonb concat/merge
            (jb/serialize-jsonb (jb/jsonb-concat l r))
            ;; plain string concat
            (str l r)))))

    ;; jsonb field access: col->'key' or col->>'key' in UPDATE SET expressions
    (instance? JsonExpression value-expr)
    (let [{:keys [base chain]} (expr/flatten-json-chain ^JsonExpression value-expr)
          base-val (eval-update-expr base entity-map ns-str schema)]
      (reduce
       (fn [current [key-expr op-str]]
         (let [key-val (eval-update-expr key-expr entity-map ns-str schema)
               r ((jb/op op-str) current key-val)]
           ;; `->` is serialised here and NOT in the SELECT emitter. That
           ;; divergence predates the shared registry; it is preserved
           ;; deliberately so this refactor stays behaviour-preserving,
           ;; and is resolved when the operator semantics are fixed.
           ;;
           ;; A missing key is SQL NULL, which must leave as nil: the
           ;; sentinel reached the column and `SET s = j->>'k'` wrote the
           ;; literal text ":__null__" for every row without that key.
           (cond
             (= :__null__ r) nil
             (= op-str "->>") r
             :else (jb/serialize-jsonb r))))
       base-val
       chain))

    (instance? Parenthesis value-expr)
    (eval-update-expr (.getExpression ^Parenthesis value-expr)
                      entity-map ns-str schema)

    ;; AT TIME ZONE — evaluate inner expression. Java Date is UTC-based so
    ;; timezone is effectively a no-op for our purposes (PG's default is UTC).
    (instance? net.sf.jsqlparser.expression.TimezoneExpression value-expr)
    (eval-update-expr (.getLeftExpression ^net.sf.jsqlparser.expression.TimezoneExpression value-expr)
                      entity-map ns-str schema)

    ;; Bare CURRENT_TIMESTAMP / CURRENT_DATE / CURRENT_TIME (no parens)
    ;; — TimeKeyExpression, same value as the function forms below
    ;; (issue #14's UPDATE-path twin).
    (instance? TimeKeyExpression value-expr)
    (java.util.Date.)

    ;; Function call.
    (instance? net.sf.jsqlparser.expression.Function value-expr)
    (let [^net.sf.jsqlparser.expression.Function f value-expr
          fname (str/lower-case (.getName f))
          ;; The SQL keyword call forms -- `substring(s FROM 1 FOR 2)`,
          ;; `position('a' IN s)` -- put their operands in a
          ;; NamedExpressionList and leave .getParameters empty, so they
          ;; arrived with NO arguments and fell through to the stringifying
          ;; fallback below.
          args (or (some-> (.getParameters f) .getExpressions)
                   (some-> (.getNamedParameters f) .getExpressions))
          args (if (and (= fname "position") (nil? (.getParameters f)) (= 2 (count args)))
                 ;; gram.y swaps these before analysis -- see the same
                 ;; adjustment in translate-function-call.
                 [(second args) (first args)]
                 args)]
      (case fname
        ("now" "current_timestamp" "localtimestamp") (java.util.Date.)
        ("current_date") (java.util.Date.)
        ;; concat(...) — PG's NULL-safe string concatenation. Required
        ;; by Odoo's _parent_store_create UPDATE that builds parent_path
        ;; from a correlated scalar subquery + node.id + literal '/'.
        "concat"
        (apply str (map #(let [v (eval-update-expr % entity-map ns-str schema)]
                           (if (some? v) (str v) ""))
                        args))
        ;; coalesce / nullif are not in the shared table -- the SELECT path
        ;; special-cases them in the translator, which this evaluator cannot
        ;; reuse -- so they need spelling out here.
        ("substring" "substr")
        (let [[a b c] (mapv #(eval-update-expr % entity-map ns-str schema) args)
              r (if (some? c) (fns/sql-substring a b c) (fns/sql-substring a b))]
          (if (= :__null__ r) nil r))
        "coalesce"
        (first (remove nil? (map #(eval-update-expr % entity-map ns-str schema) args)))
        "nullif"
        (let [[a b] (mapv #(eval-update-expr % entity-map ns-str schema) args)]
          (when-not (true? (fns/sql-eq3? a b)) a))
        ;; Anything else in the shared function table. Without this the
        ;; fallback STRINGIFIED the call, so `UPDATE t SET i = coalesce(i,0)`
        ;; handed the numeric coercion the text "coalesce(i, 0)" and raised
        ;; "invalid input syntax for numeric".
        (if-let [impl (get fns/sql-fn->clj-fn fname)]
          (let [vs (mapv #(eval-update-expr % entity-map ns-str schema) args)
                spec (get fns/sql-function-specs fname)
                wrapped (if (or (contains? fns/non-strict-fns fname)
                                (= false (:strict? spec)))
                          impl
                          (fns/null-safe impl))
                r (apply wrapped vs)]
            (if (= :__null__ r) nil r))
          ;; Refuse rather than stringify: this fallback wrote the SQL
          ;; source text of the call into the column.
          (throw (ex-info "UPDATE SET function not supported"
                          {:error :feature-not-supported
                           :feature (str "UPDATE SET function " fname)
                           :expr (str value-expr)})))))

    ;; Scalar subquery: (SELECT col FROM tbl WHERE ...). Used inside
    ;; concat()/etc. arguments for correlated reads. Requires a live db
    ;; (bound by the server-side caller as *eval-update-db*); without
    ;; one we have no db to query, so fall through with nil.
    ;;
    ;; Outer-row correlation rides on `*from-bindings*`: the caller
    ;; binds {outer-alias → {col-string → entity-value}} so the inner
    ;; SELECT's WHERE references like `node.parent_id` resolve via
    ;; expr.clj's existing from-bindings Column branch.
    (instance? ParenthesedSelect value-expr)
    (when-let [db *eval-update-db*]
      (let [inner (.getSelect ^ParenthesedSelect value-expr)]
        (when (instance? PlainSelect inner)
          (let [parsed (translate-select ^PlainSelect inner schema db)
                q (:query parsed)
                in-args (:in-args parsed)
                results (if (seq in-args) (apply d/q q db in-args) (d/q q db))
                first-row (first results)]
            (if (sequential? first-row) (first first-row) first-row)))))

    ;; Cast expression: evaluate inner and cast
    (instance? CastExpression value-expr)
    (apply-sql-cast (eval-update-expr (.getLeftExpression ^CastExpression value-expr)
                                      entity-map ns-str schema)
                    ^CastExpression value-expr)

    ;; Parenthesized single expression — unwrap
    (instance? ParenthesedExpressionList value-expr)
    (let [^ParenthesedExpressionList pel value-expr]
      (if (= (count pel) 1)
        (eval-update-expr (first pel) entity-map ns-str schema)
        (str value-expr)))

    ;; CASE WHEN … THEN … ELSE … END. Without this the fallback stringified
    ;; the whole expression, so `UPDATE t SET i = CASE …` wrote the SQL text
    ;; into the column (or, for a numeric column, raised on the coercion).
    ;; A branch is taken only when its test is TRUE -- UNKNOWN is not.
    (instance? CaseExpression value-expr)
    (let [^CaseExpression ce value-expr
          switch (.getSwitchExpression ce)
          switch-v (when switch (eval-update-expr switch entity-map ns-str schema))
          ev (fn [e] (eval-update-expr e entity-map ns-str schema))
          taken (reduce (fn [_ ^WhenClause wc]
                          (let [when-e (.getWhenExpression wc)
                                hit? (if switch
                                       (true? (fns/sql-eq3? switch-v (ev when-e)))
                                       (true? (eval-update-cond when-e entity-map ns-str schema)))]
                            (when hit? (reduced [(ev (.getThenExpression wc))]))))
                        nil (.getWhenClauses ce))]
      (if taken
        (first taken)
        (when-let [else (.getElseExpression ce)] (ev else))))

    ;; Arithmetic and bitwise operators that already have an implementation
    ;; in the shared function table. Without these the fallback stringified
    ;; the expression, and `UPDATE t SET n = 7 % 3` was a SILENT NO-OP --
    ;; the text failed the numeric coercion and the column kept its old
    ;; value, with no error.
    (instance? Modulo value-expr)
    (let [^Modulo e value-expr]
      (fns/sql-mod (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
                   (eval-update-expr (.getRightExpression e) entity-map ns-str schema)))

    (or (instance? BitwiseAnd value-expr) (instance? BitwiseOr value-expr)
        (instance? BitwiseXor value-expr))
    (let [^net.sf.jsqlparser.expression.BinaryExpression e value-expr
          l (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r (eval-update-expr (.getRightExpression e) entity-map ns-str schema)]
      (when (and (some? l) (some? r))
        (cond
          (instance? BitwiseAnd value-expr) (bit-and (long l) (long r))
          (instance? BitwiseOr value-expr)  (bit-or (long l) (long r))
          :else                             (bit-xor (long l) (long r)))))

    ;; EXTRACT(field FROM v) / TRIM(… FROM s) are their own AST nodes, not
    ;; Functions, so they never reached the function table.
    (instance? ExtractExpression value-expr)
    (let [^ExtractExpression e value-expr
          v (fns/sql-extract (str (.getName e))
                             (eval-update-expr (.getExpression e) entity-map ns-str schema))]
      (if (= :__null__ v) nil v))

    (instance? TrimFunction value-expr)
    (let [^TrimFunction e value-expr
          spec (str/lower-case (str (.getTrimSpecification e)))
          from-e (.getFromExpression e)
          str-e (if from-e from-e (.getExpression e))
          chars-e (when from-e (.getExpression e))
          v (eval-update-expr str-e entity-map ns-str schema)
          c (when chars-e (eval-update-expr chars-e entity-map ns-str schema))
          f (case spec "leading" fns/sql-ltrim "trailing" fns/sql-rtrim fns/sql-btrim)
          r (if c (f v c) (f v))]
      (if (= :__null__ r) nil r))

    ;; ARRAY[…] constructor, and `arr[i]` subscripting.
    (instance? ArrayConstructor value-expr)
    ;; Canonical PG text, not the PgArray record: an array COLUMN is stored
    ;; as "{1,2}" text, so handing the record straight to the coercion wrote
    ;; the Clojure vector's toString ("[7, 8]") into the column.
    (pg-arr/to-pg-text
     (pg-arr/array :unknown
                   (mapv #(eval-update-expr % entity-map ns-str schema)
                         (.getExpressions ^ArrayConstructor value-expr))))

    (instance? ArrayExpression value-expr)
    (let [^ArrayExpression e value-expr
          base (eval-update-expr (.getObjExpression e) entity-map ns-str schema)
          idx  (eval-update-expr (.getIndexExpression e) entity-map ns-str schema)]
      ;; An array COLUMN arrives as canonical PG text; an ARRAY[…] literal
      ;; as a PgArray record. Accept either.
      (let [arr (cond (pg-arr/array? base) base
                      (string? base) (try (pg-arr/from-pg-text base :unknown)
                                          (catch Throwable _ nil)))]
        (when (and arr (integer? idx))
          (nth (pg-arr/flat-elements arr) (dec (long idx)) nil))))

    ;; A PREDICATE in value position -- `SET b = (n > 5)`,
    ;; `SET s = (n IN (10,20))::text`. eval-update-cond already knows the
    ;; whole predicate surface three-valued; the fallback stringified them,
    ;; so the column received the SQL source text "n IN (10, 20)".
    (or (instance? EqualsTo value-expr) (instance? NotEqualsTo value-expr)
        (instance? GreaterThan value-expr) (instance? GreaterThanEquals value-expr)
        (instance? MinorThan value-expr) (instance? MinorThanEquals value-expr)
        (instance? Between value-expr) (instance? LikeExpression value-expr)
        (instance? InExpression value-expr) (instance? IsNullExpression value-expr)
        (instance? NotExpression value-expr) (instance? JsonOperator value-expr)
        (instance? AndExpression value-expr) (instance? OrExpression value-expr))
    (let [v (eval-update-cond value-expr entity-map ns-str schema)]
      (if (= :__null__ v) nil v))

    ;; Anything else is REFUSED, not stringified. `:else (str value-expr)`
    ;; wrote the SQL source text of the expression into the column --
    ;; `UPDATE t SET a = ARRAY[7,8]` stored the string "ARRAY[7, 8]" in an
    ;; int[] column, no error raised. Silent data corruption is worse than
    ;; an honest refusal, which is the stance doc/design-alignment.md
    ;; already takes for OUTER LATERAL.
    :else
    (throw (ex-info "UPDATE SET expression not supported"
                    {:error :feature-not-supported
                     :feature (str "UPDATE SET expression of type "
                                   (.getName ^Class (type value-expr)))
                     :expr (str value-expr)}))))

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
  "A DML target alias completely hides the target's original relation name."
  [statement raw-table alias-name]
  (when (and alias-name (not= alias-name raw-table))
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
                                  alias-name "\".")})))))

;; Per-schema cache for enriched-schema. NOT a pure function of the
;; schema map: the :pg/array-elem / :pg/typmod / :pg/type facts live on
;; ident *entities* in the db, so two databases with structurally equal
;; schema maps can enrich differently — the cache must key on schema
;; IDENTITY (same object ⇒ same database generation), not equality.
;; CREATE TABLE / ALTER ADD COLUMN mint a new schema map (new identity
;; → natural miss), but a typmod-only ALTER COLUMN TYPE does not, so
;; the server's DDL path also clears this cache explicitly
;; (invalidate-enriched-schema-cache!, wired into
;; server/invalidate-schema-cache!).
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
   the schema map. Memoised per schema-identity — was ~0.7 ms/row of
   pure recomputation on the Pagila pg_dump replay before caching."
  [schema db]
  (if (nil? db)
    schema
    (let [^java.util.Map outer enriched-schema-cache
          k (pg-cache/identity-key schema)]
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
        cols (pgs/column-info schema table-name db)
        unique-cols (filter :unique cols)]
    (cond
      (= cname (str/lower-case (str table-name "_pkey")))
      (when-let [pk (first unique-cols)]
        [(keyword ns (:name pk))])

      :else
      (when-let [c (first (filter #(= cname
                                      (str/lower-case
                                       (str table-name "_" (:name %) "_key")))
                                  unique-cols))]
        [(keyword ns (:name c))]))))

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
        (let [attrs (vec (distinct (map #(keyword ns (unquote-ident %)) idx-cols)))
              _ (doseq [attr attrs]
                  (when-not (get schema attr)
                    (throw (ex-info (str "column \"" (name attr) "\" does not exist")
                                    {:error :undefined-column
                                     :sqlstate "42703"
                                     :column (name attr)}))))
              unique-colsets
              (into #{}
                    (keep (fn [[attr m]]
                            (when (and (keyword? attr)
                                       (= table-name (namespace attr))
                                       (some? (:db/unique m)))
                              (if (= :db.type/tuple (:db/valueType m))
                                (set (:db/tupleAttrs m))
                                #{attr}))))
                    schema)]
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
   table-name ns schema]
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
               (when-let [^Table qualifier (.getTable col)]
                 (throw (ex-info
                         (str "column \"" (unquote-ident (.getName qualifier))
                              "\" of relation \"" table-name "\" does not exist")
                         {:error :undefined-column
                          :sqlstate "42703"
                          :hint "SET target columns cannot be qualified with the relation name."})))
               (let [col-name (unquote-ident (.getColumnName col))
                     attr (keyword ns col-name)]
                 (when-not (get schema attr)
                   (throw (ex-info (str "column \"" col-name "\" does not exist")
                                   {:error :undefined-column
                                    :sqlstate "42703"
                                    :column col-name})))
                 (doseq [^Column ref (params/ast-columns value-expr)
                         :let [qualifier (some-> ref .getTable .getName unquote-ident)
                               ref-name (unquote-ident (.getColumnName ref))]
                         :when (and (= "excluded" (some-> qualifier str/lower-case))
                                    (not (get schema (keyword ns ref-name))))]
                   (throw (ex-info (str "column excluded." ref-name " does not exist")
                                   {:error :undefined-column
                                    :sqlstate "42703"
                                    :column ref-name})))
                 {:attr attr :col-name col-name :value-expr value-expr}))
             cols values)))
    (.getUpdateSets conflict-action))))

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
  [schema tname col-names]
  (let [ci (pgs/ci-index schema)
        t (let [c (pgs/canonical-table ci tname)] (if (pgs/ambiguous? c) tname c))]
    [t (mapv (fn [c]
               (let [a (pgs/canonical-attr ci t c)]
                 (if (and a (not (pgs/ambiguous? a))) (name a) c)))
             col-names)]))

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
        parent-table (when db
                       (ffirst (d/q
                                '{:find [?p]
                                  :where [[?e :__inherit__/child ?c]
                                          [?e :__inherit__/parent ?p]]
                                  :in [$ ?c]}
                                db raw-table)))
        raw-cols (if (seq columns)
                   (mapv #(unquote-ident (.getColumnName ^Column %)) columns)
                   (let [own-order (or (pgs/column-order-from-db db raw-table)
                                       (when-let [cols (pgs/column-info schema raw-table)]
                                         (mapv :name (rest cols))))
                         parent-order (when parent-table
                                        (pgs/column-order-from-db db parent-table))]
                     ;; PostgreSQL orders inherited columns before the
                     ;; child's own columns for INSERT without a target list.
                     ;; An empty child column-order is still a real value, so
                     ;; plain `or` previously hid the parent completely.
                     (vec (distinct (concat parent-order own-order)))))
        [table-name col-names] (canonical-relation schema raw-table raw-cols)
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
        ns table-name
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
            entity {marker true :db/id (str (gensym "default-"))}
            returning (extract-returning (.getReturningClause insert))]
        (cond-> {:type :insert
                 :tx-data [entity]
                 :count 1
                 :table table-name :ns ns}
          returning (assoc :returning returning)))

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
            inner-parsed (params/*parse-sql* (str inner-select) schema db)
            ;; parse-sql CATCHES: a source SELECT that failed to
            ;; translate comes back as {:type :error}, not as a throw.
            ;; Ignoring that carried a nil :query into d/q, whose
            ;; "Query should be a vector or a map" then replaced the
            ;; real diagnosis — so `INSERT INTO t (id) SELECT nope FROM
            ;; t` reported XX000 instead of the inner 42703.
            _ (when (= :error (:type inner-parsed))
                (throw (ex-info (str (:message inner-parsed))
                                {:sqlstate (or (:sqlstate inner-parsed) "XX000")})))
            inner-query (:query inner-parsed)
            inner-in-args (:in-args inner-parsed)
            q-fn d/q
            ;; Table-free SELECT (`SELECT 1, 2, 3` — no FROM clause) is
            ;; produced by translate-select with `:literal-row` /
            ;; `:literal-rows` set and `:query {:find [] :where []}`.
            ;; The execute-select path handles this via a literal-row
            ;; short-circuit; we mirror it here so INSERT … SELECT
            ;; routes the same data, otherwise running d/q on the
            ;; empty query returns `[[]]` and silently drops the row.
            ;; A set-returning function in the source FROM is
            ;; materialised into a SPECULATIVE db, handed back as
            ;; `:enriched-db`. Running the inner query against the plain
            ;; `db` scans a database where that virtual table does not
            ;; exist, so `INSERT INTO t SELECT … FROM generate_series(…)`
            ;; answered `INSERT 0 0` — the standard bulk-load idiom,
            ;; silently a no-op, while the same SELECT run on its own
            ;; returned its rows. Same `(or (:enriched-db …) …)` the
            ;; correlated-scalar path already uses.
            inner-db (or (:enriched-db inner-parsed) db)
            inner-results (cond
                            (:literal-rows inner-parsed) (:literal-rows inner-parsed)
                            (:literal-row  inner-parsed) [(:literal-row inner-parsed)]
                            (seq inner-in-args)
                            (apply q-fn inner-query inner-db inner-in-args)
                            :else (q-fn inner-query inner-db))
            _ (when (seq (:project-order-by inner-parsed))
                (throw (errors/pg-error
                        :feature-not-supported
                        {:feature "INSERT ... SELECT ordered by a set-returning function"})))
            inner-results (if-let [specs (seq (:project-set inner-parsed))]
                            (let [expanded (apply-project-set inner-results specs)]
                              (cond->> expanded
                                (:project-offset inner-parsed)
                                (drop (:project-offset inner-parsed))
                                (:project-limit inner-parsed)
                                (take (:project-limit inner-parsed))))
                            inner-results)
            ;; The normal SELECT executor removes trailing helper find
            ;; elements (usually the entity id used for stable default
            ;; ordering) before projection post-processing. INSERT-SELECT
            ;; runs the query directly and must mirror that row shape.
            hidden-count (long (or (:hidden-count inner-parsed) 0))
            inner-results (if (pos? hidden-count)
                            (mapv (fn [row]
                                    (let [v (if (sequential? row) (vec row) [row])]
                                      (subvec v 0 (- (count v) hidden-count))))
                                  inner-results)
                            inner-results)
            [inner-results _]
            (apply-compound-projections inner-results
                                        (:find-aliases inner-parsed)
                                        inner-query inner-in-args
                                        (:compound-exprs inner-parsed))
            rows (validate-insert-row-widths!
                  col-names
                  (mapv (fn [row]
                          (if (sequential? row) (vec row) [row]))
                        inner-results))
            ;; Build row-attrs the same way the VALUES branch does below.
            row-attrs
            (mapv (fn [row]
                    (into {}
                          (keep (fn [[col-name val]]
                                  (let [raw-attr (keyword ns col-name)
                                        attr (if db
                                               (ctx/resolve-inherited-attr raw-attr schema db)
                                               raw-attr)
                                        ;; A row coming FROM A SELECT carries SQL
                                        ;; NULL as the `:__null__` sentinel, not
                                        ;; nil, and the sentinel reached the
                                        ;; coercion as a value: `INSERT INTO t
                                        ;; SELECT …` raised "invalid input syntax
                                        ;; for column" the moment any selected
                                        ;; value was NULL. A NULL column is simply
                                        ;; an absent datom.
                                        coerced (when-not (= :__null__ val)
                                                  (coerce-insert-value val attr schema db))]
                                    ;; INSERT SELECT has no DEFAULT token: a
                                    ;; SQL NULL is explicit and must suppress a
                                    ;; column default.  Preserve a nil entry for
                                    ;; the constraint/default wrapper, which
                                    ;; validates it and removes it before the
                                    ;; Datahike transaction is returned.
                                    (when (or (some? coerced)
                                              (nil? val)
                                              (= :__null__ val))
                                      [attr coerced])))
                                (map vector col-names row))))
                  rows)
            marker (pgs/row-marker-attr table-name)
            has-marker? (boolean (get schema marker))
            row-attrs (if has-marker?
                        (mapv #(assoc % marker true) row-attrs)
                        row-attrs)
            parent-table (when db
                           (ffirst (q-fn
                                    '{:find [?p]
                                      :where [[?e :__inherit__/child ?c]
                                              [?e :__inherit__/parent ?p]]
                                      :in [$ ?c]}
                                    db table-name)))
            row-attrs (if parent-table
                        (let [parent-marker (pgs/row-marker-attr parent-table)]
                          (if (get schema parent-marker)
                            (mapv #(assoc % parent-marker true) row-attrs)
                            row-attrs))
                        row-attrs)
            conflict-action (.getConflictAction insert)
            returning (extract-returning (.getReturningClause insert))]
        (cond
          ;; Empty result set — nothing to insert.
          (empty? row-attrs)
          (cond-> {:type :insert :tx-data [] :count 0
                   :table table-name :ns ns}
            returning (assoc :returning returning))
          ;; Non-empty with ON CONFLICT — delegate via :db.fn/call so the
          ;; conflict lookup and the write are one atomic transaction.
          ;;
          ;; This arm used to ignore the conflict target entirely and
          ;; arbitrate on ALL inserted columns, which silently destroyed
          ;; data: `INSERT INTO t (id,title) SELECT 1,'discard' ON
          ;; CONFLICT (id) DO NOTHING` found no row matching BOTH id=1
          ;; and title='discard', inserted, and Datahike's
          ;; :db.unique/identity upsert then overwrote the existing
          ;; title. DO UPDATE was likewise unimplemented and behaved as
          ;; DO NOTHING — with the same overwrite as a consolation
          ;; prize. Both now share the VALUES arm's semantics.
          conflict-action
          (let [row-refs (atom [])
                affected (atom 0)
                do-nothing? (= (.getConflictActionType conflict-action)
                               net.sf.jsqlparser.statement.insert.ConflictActionType/DO_NOTHING)
                conflict-cols (resolve-conflict-target
                               conflict-target table-name ns schema db)
                update-where (.getWhereExpression conflict-action)
                update-assignments
                (when-not do-nothing?
                  (conflict-update-assignments conflict-action table-name ns schema))]
            (cond-> {:type :insert
                     :row-refs row-refs
                     :affected-count affected
                     :tx-data
                     ;; row-attrs travels as an explicit arg, not a closed-over
                     ;; value — see the VALUES ON CONFLICT branch below for why
                     ;; (substitute-params / resolve-nextvals! walk data only).
                     [[:db.fn/call
                       (fn [txdb row-attrs]
                         (reset! row-refs [])
                         (reset! affected 0)
                         (let [q d/q]
                           (vec
                            (mapcat
                             (fn [attrs]
                               ;; A targetless ON CONFLICT means "any unique
                               ;; constraint"; comparing every inserted
                               ;; column is our approximation of that.
                               (let [effective-cols (or conflict-cols (vec (keys attrs)))
                                     conflict-pairs (mapv (fn [col] [col (get attrs col)]) effective-cols)
                                     all-vals-present? (and (seq conflict-pairs)
                                                            (every? (fn [[_ v]] (some? v)) conflict-pairs))
                                     existing (when all-vals-present?
                                                (ffirst
                                                 (q {:find '[?e]
                                                     :where (mapv (fn [[col val]] ['?e col val]) conflict-pairs)}
                                                    txdb)))]
                                 (if existing
                                   (let [old-map (into {} (map (fn [^Datom d] [(.-a d) (.-v d)]))
                                                       (d/datoms txdb :eavt existing))
                                         excluded-map (into {} (map (fn [[k v]]
                                                                      [(keyword "excluded" (name k)) v]))
                                                            attrs)
                                         combined (merge old-map excluded-map)]
                                     (swap! row-refs conj existing)
                                     (if (or do-nothing?
                                             (and update-where
                                                  (not (true? (eval-check-predicate
                                                               update-where combined ns schema)))))
                                       []
                                       (do
                                         (swap! affected inc)
                                         (vec (keep
                                               (fn [{:keys [attr value-expr]}]
                                                 (let [new-val
                                                       (if (and (instance? Column value-expr)
                                                                (when-let [t (.getTable ^Column value-expr)]
                                                                  (= "EXCLUDED" (.toUpperCase (.getName ^Table t)))))
                                                         (get attrs attr)
                                                         (eval-update-expr value-expr combined ns schema))]
                                                   (when (some? new-val)
                                                     [:db/add existing attr
                                                      (or (coerce-insert-value new-val attr schema) new-val)])))
                                               update-assignments)))))
                                   (let [tempid (str (gensym "insert-select-"))]
                                     (swap! row-refs conj tempid)
                                     (swap! affected inc)
                                     [(assoc attrs :db/id tempid)]))))
                             row-attrs))))
                       row-attrs]]
                     :count (count row-attrs)
                     :table table-name :ns ns}
              returning (assoc :returning returning)))
          :else
          (cond-> {:type :insert
                   :tx-data (vec (mapcat
                                  (fn [attrs]
                                    (when (seq attrs)
                                      [(assoc attrs :db/id (str (gensym "insert-select-")))]))
                                  row-attrs))
                   :count (count row-attrs)
                   :table table-name :ns ns}
            returning (assoc :returning returning))))

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
            rows (cond
                   ;; All PELs have >1 element → genuine multi-row VALUES
                   (and all-pel?
                        (every? #(> (count %) 1) expr-list))
                   (mapv (fn [^ParenthesedExpressionList pel]
                           (mapv ev pel))
                         expr-list)

                   ;; All PELs have exactly 1 element AND count matches columns
                   ;; → single row with parenthesized expressions
                   (and all-pel?
                        (every? #(= (count %) 1) expr-list)
                        (= (count expr-list) num-cols))
                   [(mapv (fn [^ParenthesedExpressionList pel]
                            (ev (first pel)))
                          expr-list)]

                   ;; All PELs have exactly 1 element but count != columns
                   ;; → multi-row for single-column (or N-column) table
                   (and all-pel?
                        (every? #(= (count %) 1) expr-list))
                   (mapv (fn [^ParenthesedExpressionList pel]
                           (mapv ev pel))
                         expr-list)

                   ;; Direct list of values (single row without parens)
                   :else
                   [(mapv ev expr-list)])
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
                                            (let [raw-attr (keyword ns col-name)
                                                  attr (if db
                                                         (ctx/resolve-inherited-attr raw-attr schema db)
                                                         raw-attr)
                                                  coerced (coerce-insert-value val attr schema)]
                                          ;; DEFAULT means omitted; explicit
                                          ;; NULL remains a present nil so a
                                          ;; declared default is not applied.
                                          ;; ON CONFLICT's tx-fn still owns its
                                          ;; row maps, so retain its historical
                                          ;; omission there until that path has
                                          ;; the same constraint wrapper.
                                              (cond
                                                (= ::insert-default val) nil
                                                (some? coerced) [attr coerced]
                                                (nil? (.getConflictAction insert)) [attr nil]
                                                :else nil)))
                                          (map vector col-names row))))
                            rows)
        ;; Add row-existence marker for this table
            marker (pgs/row-marker-attr table-name)
            has-marker? (boolean (get schema marker))
            row-attrs (if has-marker?
                        (mapv #(assoc % marker true) row-attrs)
                        row-attrs)
        ;; For INHERITS: also add parent's row-marker so parent queries find this entity
            row-attrs (if parent-table
                        (let [parent-marker (pgs/row-marker-attr parent-table)]
                          (if (get schema parent-marker)
                            (mapv #(assoc % parent-marker true) row-attrs)
                            row-attrs))
                        row-attrs)
            result
            (if conflict-action
          ;; ON CONFLICT — build :db.fn/call for atomic upsert
              (let [action-type (.getConflictActionType conflict-action)
                    do-nothing? (= action-type net.sf.jsqlparser.statement.insert.ConflictActionType/DO_NOTHING)
                    conflict-cols (resolve-conflict-target
                                   conflict-target table-name ns schema db)
                    ;; `DO UPDATE … WHERE cond` — decided per conflicting
                    ;; row at transaction time. Ignoring it (the previous
                    ;; behaviour) updated rows the statement excluded.
                    update-where (.getWhereExpression conflict-action)
                ;; Parse DO UPDATE SET assignments
                    update-assignments
                    (when-not do-nothing?
                      (conflict-update-assignments conflict-action table-name ns schema))
                ;; DO UPDATE SET may itself carry placeholders
                ;; (`SET title = $2`, `SET n = n + $3`). Those live in the
                ;; JSqlParser value-expr, which eval-update-expr only
                ;; resolves when `*bound-params*` is bound — so hoist the
                ;; indices into a 0-based ParamRef vector that travels as a
                ;; `:db.fn/call` ARG (reachable by substitute-params) and
                ;; rebind it around the evaluation inside the tx-fn.
                    set-params
                    (let [idxs (into (sorted-set)
                                     (concat
                                      (mapcat #(params/ast-param-indices (:value-expr %))
                                              update-assignments)
                                      ;; `DO UPDATE … WHERE t.n > $4` puts
                                      ;; placeholders in the condition too.
                                      (when update-where
                                        (params/ast-param-indices update-where))))]
                      (when (seq idxs)
                        ;; 0-based layout — eval-update-expr's JdbcParameter
                        ;; branch reads `(nth bound (dec idx))`. Unused slots
                        ;; stay nil; substitute-params walks vectors
                        ;; element-wise so they survive the pass.
                        (reduce (fn [v i] (assoc v (dec (long i)) (->ParamRef i)))
                                (vec (repeat (long (apply max idxs)) nil))
                                idxs)))]
            ;; Shared atom: fn writes [eid-or-tempid] in row order so the
            ;; RETURNING dispatch can resolve ids in VALUES order (not hash order).
            ;; Existing rows (DO UPDATE) store the eid; new rows store the tempid.
                (let [row-refs (atom [])
                      ;; PG counts only the rows an ON CONFLICT statement
                      ;; actually inserted or updated: a DO NOTHING that
                      ;; hit a conflict reports `INSERT 0 0`, and a
                      ;; three-row VALUES where one conflicts reports
                      ;; `INSERT 0 2`. The parse-time row count can't know
                      ;; that, so the tx-fn tallies it.
                      affected (atom 0)]
                  {:type :insert
                   :row-refs row-refs
                   :affected-count affected
                   :tx-data
                   ;; `row-attrs` / `set-params` are passed as explicit
                   ;; `:db.fn/call` ARGS rather than captured by the
                   ;; closure: substitute-params (Execute-time ParamRef
                   ;; resolution), resolve-nextvals! and the INSERT value
                   ;; re-coercion all walk tx-data as data and cannot see
                   ;; inside a Clojure fn. Closing over them left `$N`
                   ;; placeholders in the conflict lookup and in the
                   ;; asserted values — every parameterized
                   ;; `INSERT … ON CONFLICT` (the shape every ORM emits)
                   ;; died with "ParamRef cannot be cast to Number".
                   [[:db.fn/call
                     (fn [txdb row-attrs set-params]
                 ;; A prepared statement's parsed map — and with it this
                 ;; atom — is reused across Executes, and an in-transaction
                 ;; write replays its buffer at COMMIT, so the refs from a
                 ;; previous run must not leak into this one's RETURNING.
                       (reset! row-refs [])
                       (reset! affected 0)
                 ;; Pre-fetch sequence state for identity column auto-population.
                 ;; Sequences are named <table>_<col>_seq.
                       (let [q-fn d/q
                             seq-prefix (str table-name "_")
                             seq-results (q-fn '{:find [?name] :where [[?e :__seq__/name ?name]]
                                                 :in [$ ?prefix]}
                                               txdb seq-prefix)
                             identity-cols
                             (vec (keep (fn [[sname]]
                                          ;; PG's auto-generated SERIAL/IDENTITY sequences
                                          ;; are named `<table>_<col>_seq` — require a
                                          ;; non-empty `<col>` between prefix and suffix
                                          ;; or we'll false-match a sequence the user
                                          ;; happens to have named `<table>_seq`
                                          ;; (no col), e.g. an `ord_seq` next to an
                                          ;; `ord` table.
                                          (let [pref-len (count seq-prefix)
                                                tail-end (- (count sname) 4)]
                                            (when (and (str/starts-with? sname seq-prefix)
                                                       (str/ends-with? sname "_seq")
                                                       (< pref-len tail-end))
                                              (let [col-name (subs sname pref-len tail-end)
                                                    attr (keyword table-name col-name)]
                                                (when (get (:schema txdb) attr)
                                                  {:col col-name :attr attr :seq-name sname})))))
                                        seq-results))
                             seq-state (atom
                                        (into {}
                                              (for [{:keys [col seq-name]} identity-cols
                                                    :let [seq-eid (ffirst (q-fn '{:find [?e]
                                                                                  :where [[?e :__seq__/name ?n]]
                                                                                  :in [$ ?n]}
                                                                                txdb seq-name))
                                                          curr-val (or (when seq-eid
                                                                         (ffirst (q-fn '{:find [?v]
                                                                                         :where [[?e :__seq__/value ?v]]
                                                                                         :in [$ ?e]}
                                                                                       txdb seq-eid)))
                                                                       0)
                                                          increment 1]
                                                    :when seq-eid]
                                                [col {:eid seq-eid :val curr-val :inc increment}])))
                             ;; PostgreSQL arbitrates all proposed rows as one
                             ;; command.  The txdb is the pre-command snapshot,
                             ;; so it cannot reveal a duplicate emitted earlier
                             ;; by this same tx-fn; retain those arbiter keys
                             ;; explicitly. DO NOTHING keeps the first row,
                             ;; while DO UPDATE must raise cardinality_violation
                             ;; instead of updating/upserting the same row twice.
                             seen-arbiters (atom {})
                             seen-existing-rows (atom #{})
                             ;; Targetless ON CONFLICT means every unique
                             ;; constraint, not equality across every inserted
                             ;; column.  Build its scalar/tuple arbiter column
                             ;; sets from the Datahike schema.
                             targetless-arbiters
                             (when-not conflict-cols
                               (into []
                                     (keep (fn [[attr m]]
                                             (when (and (keyword? attr)
                                                        (= table-name (namespace attr))
                                                        (some? (:db/unique m)))
                                               (if (= :db.type/tuple (:db/valueType m))
                                                 (vec (:db/tupleAttrs m))
                                                 [attr]))))
                                     (:schema txdb)))]
                         (vec (mapcat
                               (fn [attrs]
                         ;; Build multi-column conflict query:
                         ;; {:find [?e] :where [[?e :col1 val1] [?e :col2 val2] ...]}
                         ;; If no conflict target was given (ON CONFLICT DO NOTHING),
                         ;; PG checks all unique constraints — fall back to all
                         ;; inserted attribute keys (the natural-key case for m2m
                         ;; relation tables).
                                 (let [arbiter-cols (if conflict-cols
                                                      [conflict-cols]
                                                      targetless-arbiters)
                                       candidate-pairs
                                       (mapv (fn [cols]
                                               (mapv (fn [col] [col (get attrs col)]) cols))
                                             arbiter-cols)
                                       conflict-pair-sets
                                       (filterv (fn [pairs]
                                                  (and (seq pairs)
                                                       (every? (fn [[_ v]] (some? v)) pairs)))
                                                candidate-pairs)
                                       arbiter-keys conflict-pair-sets
                                       existing (some (fn [pairs]
                                                        (ffirst
                                                         (q-fn
                                                          {:find '[?e]
                                                           :where (mapv
                                                                   (fn [[col val]] ['?e col val])
                                                                   pairs)}
                                                          txdb)))
                                                      conflict-pair-sets)
                                       prior (or (some #(get @seen-arbiters %) arbiter-keys)
                                                 (when (contains? @seen-existing-rows existing)
                                                   existing))]
                                   (cond
                                     prior
                                     (if do-nothing?
                                       []
                                       (throw (ex-info
                                               "ON CONFLICT DO UPDATE command cannot affect row a second time"
                                               {:error :cardinality-violation
                                                :sqlstate "21000"
                                                :hint (str "Ensure that no rows proposed for insertion within "
                                                           "the same command have duplicate constrained values.")})))

                                     existing
                                     (if (or do-nothing?
                                       ;; DO UPDATE … WHERE cond — the
                                       ;; conflicting row is left alone
                                       ;; when the condition isn't TRUE.
                                       ;; Evaluated against the row's
                                       ;; current values plus EXCLUDED,
                                       ;; the same map the SET
                                       ;; expressions see. UNKNOWN counts
                                       ;; as not-true, per WHERE.
                                             (and update-where
                                                  (let [old-map (into {} (map (fn [^Datom d]
                                                                                [(.-a d) (.-v d)]))
                                                                      (d/datoms txdb :eavt existing))
                                                        excluded-map (into {} (map (fn [[k v]]
                                                                                     [(keyword "excluded" (name k)) v]))
                                                                           attrs)]
                                                    (binding [params/*bound-params*
                                                              (or set-params params/*bound-params*)]
                                                      (not (true? (eval-check-predicate
                                                                   update-where
                                                                   (merge old-map excluded-map)
                                                                   ns schema)))))))
                                       [] ;; DO NOTHING / condition not met
                                 ;; DO UPDATE SET
                                       (do
                                         (doseq [arbiter-key arbiter-keys]
                                           (swap! seen-arbiters assoc arbiter-key existing))
                                         (swap! seen-existing-rows conj existing)
                                         (swap! row-refs conj existing)
                                         (swap! affected inc)
                                         (vec (keep
                                               (fn [{:keys [attr col-name value-expr]}]
                                                 (let [old-val (ffirst
                                                                (q-fn
                                                                 {:find '[?v]
                                                                  :in '[$ ?e ?a]
                                                                  :where [['?e '?a '?v]]}
                                                                 txdb existing attr))
                                                         ;; Evaluate the update expression
                                                       new-val
                                                       (cond
                                                ;; Simple: EXCLUDED.col → use new value
                                                         (and (instance? net.sf.jsqlparser.schema.Column value-expr)
                                                              (when-let [t (.getTable ^net.sf.jsqlparser.schema.Column value-expr)]
                                                                (= "EXCLUDED" (.toUpperCase (.getName ^Table t)))))
                                                         (get attrs attr)

                                                ;; Expression: e.g. v + EXCLUDED.v
                                                ;; Evaluate using server's eval-update-expr
                                                         :else
                                                         (let [;; Build entity map with current values + EXCLUDED values
                                                               old-datoms (d/datoms txdb :eavt existing)
                                                               old-map (into {} (map (fn [^Datom d]
                                                                                       [(.-a d) (.-v d)])
                                                                                     old-datoms))
                                                      ;; EXCLUDED.col → use attrs from the INSERT values
                                                               excluded-map (into {} (map (fn [[k v]]
                                                                                            [(keyword "excluded" (name k)) v])
                                                                                          attrs))
                                                               combined (merge old-map excluded-map)]
                                                  ;; set-params (0-based, ParamRefs already
                                                  ;; substituted by the wire layer) lets
                                                  ;; eval-update-expr resolve `$N` operands
                                                  ;; inline — same idiom the UPDATE path uses.
                                                           (binding [params/*bound-params*
                                                                     (or set-params params/*bound-params*)]
                                                             (eval-update-expr
                                                              value-expr combined ns schema))))]
                                                   (if (nil? new-val)
                                                     (when (some? old-val)
                                                       [:db/retract existing attr old-val])
                                                     [:db/add existing attr
                                                      (or (coerce-insert-value new-val attr schema) new-val)])))
                                               update-assignments))))
                             ;; No conflict — normal insert with identity population
                                     :else
                                     (let [tempid (str (gensym "upsert-"))
                                   ;; Auto-populate identity columns
                                           populated (reduce
                                                      (fn [m {:keys [col attr]}]
                                                        (if (contains? m attr)
                                                          m
                                                          (let [{:keys [val inc]} (get @seq-state col)
                                                                new-val (+ val inc)]
                                                            (swap! seq-state assoc-in [col :val] new-val)
                                                            (assoc m attr new-val))))
                                                      attrs identity-cols)
                                   ;; Generate sequence update datoms
                                           seq-updates (keep
                                                        (fn [{:keys [col attr]}]
                                                          (when-not (contains? attrs attr)
                                                            (let [{:keys [eid val]} (get @seq-state col)]
                                                              (when eid [:db/add eid :__seq__/value val]))))
                                                        identity-cols)]
                                       ;; Record new tempid at row position
                                       (doseq [arbiter-key arbiter-keys]
                                         (swap! seen-arbiters assoc arbiter-key tempid))
                                       (swap! row-refs conj tempid)
                                       (swap! affected inc)
                                       (let [clean-populated
                                             (into {} (remove (comp nil? val)) populated)]
                                         (into [(assoc clean-populated :db/id tempid)]
                                               seq-updates))))))
                               row-attrs))))
                     row-attrs
                     set-params]]
                   :count (count rows)
                   :table table-name :ns ns}))
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
        (cond-> result
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
    (cond-> {:type :delete
             :table table-name
             :alias alias-name
             :ns ns
             :where-expr where-expr}
      (.getReturningClause delete)
      (assoc :returning (extract-returning (.getReturningClause delete))))))

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

(defn extract-from-values
  "If an UPDATE's FROM clause is `(VALUES (...), (...)) AS alias(col1, col2, ...)`,
   extract it. Returns {:alias str :cols [str] :rows [[literal ...] ...]} or nil."
  [^Update update]
  (when-let [from-item (.getFromItem update)]
    (when (instance? ParenthesedFromItem from-item)
      (let [^ParenthesedFromItem pfi from-item
            inner (.getFromItem pfi)]
        (when (instance? Values inner)
          (let [^Values vs inner
                alias-obj (.getAlias pfi)
                alias-name (when alias-obj (unquote-ident (.getName ^Alias alias-obj)))
                alias-cols (when alias-obj
                             (some->> (.getAliasColumns ^Alias alias-obj)
                                      (mapv (fn [^net.sf.jsqlparser.expression.Alias$AliasColumn c]
                                              (unquote-ident (.-name c))))))
                raw-exprs (.getExpressions vs)
                ;; JSqlParser quirk: multi-row VALUES produces a list of
                ;; ParenthesedExpressionList; single-row VALUES flattens to
                ;; the column expressions directly (one row).
                rows (if (and (seq raw-exprs)
                              (instance? ParenthesedExpressionList (first raw-exprs)))
                       (mapv (fn [^ParenthesedExpressionList row]
                               (mapv eval-values-literal
                                     (iterator-seq (.iterator row))))
                             raw-exprs)
                       [(mapv eval-values-literal raw-exprs)])]
            (when (and alias-name (seq alias-cols)
                       (every? (fn [r] (not-any? #(= :unhandled %) r)) rows))
              {:alias alias-name :cols alias-cols :rows rows})))))))

(defn- extract-update-from-table
  "Extract the single ordinary table form supported by UPDATE ... FROM.
   More complex FROM trees remain explicit feature gaps rather than being
   silently ignored."
  [^Update update schema]
  (when-let [from-item (.getFromItem update)]
    (when (seq (.getJoins update))
      (throw (ex-info "UPDATE FROM with multiple relations is not supported"
                      {:error :feature-not-supported :sqlstate "0A000"})))
    (if (instance? Table from-item)
      (let [{raw-name :name alias :alias} (ctx/extract-table-info from-item)
            _ (when-not (relation-known? schema raw-name)
                (throw (ex-info (str "relation \"" raw-name "\" does not exist")
                                {:error :undefined-table
                                 :sqlstate "42P01"
                                 :table raw-name})))
            table-name (first (canonical-relation schema raw-name []))]
        {:table table-name :alias alias})
      (throw (ex-info "UPDATE FROM source is not supported"
                      {:error :feature-not-supported :sqlstate "0A000"})))))

(defn translate-update
  "Translate an UPDATE statement to Datahike retract+assert pairs.
   Handles UPDATE with WITH RECURSIVE CTE — for these, the result is
   {:type :update-with-recursive ...} containing the rule, columns,
   and target table info for the server to execute."
  [^Update update schema db]
  (let [table (.getTable update)
        raw-table (unquote-ident (.getName ^Table table))
        _ (when-not (stored-relation-known? schema raw-table)
            (throw (ex-info (str "relation \"" raw-table "\" does not exist")
                            {:error :undefined-table
                             :sqlstate "42P01"
                             :table raw-table})))
        table-name (first (canonical-relation schema raw-table []))
        alias-obj (.getAlias ^Table table)
        alias-name (when alias-obj (unquote-ident (.getName ^Alias alias-obj)))
        _ (reject-hidden-target-name! update raw-table alias-name)
        ns table-name
        where-expr (.getWhere update)
        update-sets (.getUpdateSets update)
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
        from-values (extract-from-values update)
        recursive? (and withs (seq withs)
                        (some #(.isRecursive ^net.sf.jsqlparser.statement.select.WithItem %) withs))
        ;; The recursive CTE executor owns its FROM relation, which is a
        ;; virtual relation absent from the base schema at this point.
        from-table (when (and (not recursive?) (not from-values))
                     (extract-update-from-table update schema))]
    (if recursive?
      ;; WITH RECURSIVE UPDATE: translate the CTE(s) to Datalog rule(s) and
      ;; let the server execute the iterative update.
      (let [recursive-cte (first (filter #(.isRecursive ^net.sf.jsqlparser.statement.select.WithItem %)
                                         withs))
            cte-info (translate-recursive-cte recursive-cte schema db)]
        {:type :update-with-recursive
         :table table-name
         :ns ns
         :cte cte-info
         ;; Parse the SET clause: which target columns get which CTE columns
         :set-mappings
         (mapv (fn [^UpdateSet us]
                 (let [target-col (unquote-ident (.getColumnName ^Column (first (.getColumns us))))
                       value-expr (first (.getValues us))
                       ;; Expected form: cte_alias.cte_col → cte column name
                       cte-col (when (instance? Column value-expr)
                                 (unquote-ident (.getColumnName ^Column value-expr)))]
                   {:target-col target-col
                    :cte-col cte-col}))
               update-sets)
         ;; Parse the WHERE join condition to find the join column
         ;; Expected form: row.id = cte_alias.cte_col
         :join-info
         (when where-expr
           (let [parse-eq (fn [^net.sf.jsqlparser.expression.operators.relational.EqualsTo eq]
                            (let [l (.getLeftExpression eq)
                                  r (.getRightExpression eq)
                                  l-col (when (instance? Column l) (unquote-ident (.getColumnName ^Column l)))
                                  r-col (when (instance? Column r) (unquote-ident (.getColumnName ^Column r)))
                                  l-tbl (when (instance? Column l)
                                          (when-let [t (.getTable ^Column l)]
                                            (unquote-ident (.getName ^Table t))))
                                  r-tbl (when (instance? Column r)
                                          (when-let [t (.getTable ^Column r)]
                                            (unquote-ident (.getName ^Table t))))]
                              ;; Identify which side is the target table
                              {:l-tbl l-tbl :l-col l-col :r-tbl r-tbl :r-col r-col}))]
             (when (instance? net.sf.jsqlparser.expression.operators.relational.EqualsTo where-expr)
               (parse-eq where-expr))))})
      ;; Regular UPDATE
      (cond-> {:type :update
               :table table-name
               :alias alias-name
               :ns ns
               :where-expr where-expr
               :assignments
               (vec
                (mapcat
                 (fn [^UpdateSet us]
                   (let [cols (vec (.getColumns us))
                         exprs (vec (.getValues us))]
                     (when-not (= (count cols) (count exprs))
                       ;; A multi-column assignment sourced by ROW(...) or a
                       ;; sub-SELECT needs one evaluation yielding a record.
                       ;; Refuse it until that lowering exists; applying only
                       ;; the first pair is silent partial data corruption.
                       (throw (ex-info
                               "multi-column UPDATE from a row expression is not supported"
                               {:error :feature-not-supported :sqlstate "0A000"})))
                     (map (fn [^Column col value-expr]
                            {:column (unquote-ident (.getColumnName col))
                             :value-expr value-expr})
                          cols exprs)))
                 update-sets))}
        from-values (assoc :from-values from-values)
        from-table (assoc :from-table from-table)
        (.getReturningClause update)
        (assoc :returning (extract-returning (.getReturningClause update)))))))

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

   Reuses `translate-recursive-cte` for the rule construction; rule
   eval here is the SELECT counterpart of `build-update-with-recursive-tx`
   in the server."
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
