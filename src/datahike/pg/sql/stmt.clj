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
            [datahike.pg.jsonb :as jb]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.expr :as expr]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.oid-infer :as oid]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types]
            [datahike.pg.arrays :as pg-arr])
  (:import [datahike.datom Datom]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression
            Alias Function LongValue DoubleValue StringValue NullValue
            BooleanValue Parenthesis SignedExpression CastExpression
            JsonExpression TimezoneExpression ArrayConstructor JdbcParameter]
           [net.sf.jsqlparser.expression.operators.relational
            GreaterThan GreaterThanEquals MinorThan MinorThanEquals
            EqualsTo NotEqualsTo IsNullExpression
            ParenthesedExpressionList]
           [net.sf.jsqlparser.expression.operators.conditional
            AndExpression OrExpression]
           [net.sf.jsqlparser.expression.operators.arithmetic
            Addition Subtraction Multiplication Division Concat]
           [net.sf.jsqlparser.statement.select
            PlainSelect SelectItem AllColumns OrderByElement
            GroupByElement Limit Offset Join
            ParenthesedSelect ParenthesedFromItem SetOperationList
            Values]
           [net.sf.jsqlparser.statement.insert Insert]
           [net.sf.jsqlparser.statement.update Update UpdateSet]
           [net.sf.jsqlparser.statement.delete Delete]))

(set! *warn-on-reflection* true)

;; Unqualified aliases so the copied body reads naturally — same
;; pattern used by ctx / ddl / catalog / expr.
(def ^:private unquote-ident params/unquote-ident)
(def ^:private ->ParamRef params/->ParamRef)

;; ---------------------------------------------------------------------------
;; Forward declarations — mutually recursive statement translators.

(declare translate-select
         translate-insert
         translate-update
         translate-delete
         translate-join
         translate-having-expr
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

(defn translate-join
  "Add join clauses for a SQL JOIN to the context.
   For INNER joins with ref-based ON (a.ref_col = b.db_id), unifies the ref
   column variable with the target entity variable.
   For LEFT joins, records the ref-attr and right-table alias for later
   or-join wrapping in translate-select.
   Returns {:name str :alias str :join-type keyword :ref-attr kw :left-entity-var sym}."
  [ctx ^Join join _default-table]
  (let [jtype (join-type join)
        right-table (.getRightItem join)
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
                                                          (:derived-aliases ctx))
                      array-resolved (ctx/resolve-column ^Column array
                                                         (:table-aliases ctx)
                                                         (:default-table ctx)
                                                         (:col-overrides ctx)
                                                         (:derived-aliases ctx))
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
                  left-resolved (ctx/resolve-column ^Column left (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx))
                  right-resolved (ctx/resolve-column ^Column right (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx))
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
                        l-resolved (ctx/resolve-column ^Column left (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx))
                        r-resolved (ctx/resolve-column ^Column right (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx))
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
                ;; For inner joins: equality predicate + SQL null guards.
                ;; Datalog (= :__null__ :__null__) is TRUE, but SQL INNER
                ;; JOIN on NULL=NULL must NOT match (3-valued logic: NULL =
                ;; NULL is UNKNOWN, filtered out as non-TRUE). Emit explicit
                ;; not-null guards for each side so rows whose join-column
                ;; is NULL are excluded. nil and the :__null__ sentinel are
                ;; both treated as SQL NULL.
                  (let [l-var (expr/translate-expr ctx left)
                        r-var (expr/translate-expr ctx right)]
                    (ctx/add-clause! ctx [(list '= l-var r-var)])
                    (when (symbol? l-var)
                      (ctx/add-clause! ctx [(list 'not= l-var :__null__)])
                      (ctx/add-clause! ctx [(list 'not= l-var nil)]))
                    (when (symbol? r-var)
                      (ctx/add-clause! ctx [(list 'not= r-var :__null__)])
                      (ctx/add-clause! ctx [(list 'not= r-var nil)]))))))

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
              (let [preds (expr/translate-predicate ctx expr)]
                (swap! (:where-clauses ctx) into preds)))))))
    {:name name :alias right-alias :join-type jtype :ref-info @ref-info}))

(defn select-item-alias [^SelectItem item]
  (when-let [alias (.getAlias item)]
    (unquote-ident (.getName ^Alias alias))))

(defn match-aggregate-index
  "Try to find the index of an aggregate function in the find-elements.
   For COUNT(*) → look for (count ?x), for SUM(col) → (sum ?x) or
   (datahike.pg.sql/filter-sum ?x). Returns the 0-based index or nil.

   Matches both the raw Datalog aggregate symbol and our ns-qualified
   null-filtering variant (filter-sum/avg/min/max/count[-distinct]) so
   HAVING clauses resolve regardless of which variant the SELECT
   projection emitted."
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

(defn translate-having-expr
  "Translate a HAVING expression into a Clojure predicate fn form.
   The result is a map {:op symbol :col-idx int :value val} for simple cases,
   or a nested structure for AND/OR.
   The server applies this as a post-filter on result tuples."
  [expr find-elems find-aliases]
  (cond
    (instance? AndExpression expr)
    (let [^AndExpression e expr]
      {:op :and
       :clauses [(translate-having-expr (.getLeftExpression e) find-elems find-aliases)
                 (translate-having-expr (.getRightExpression e) find-elems find-aliases)]})

    (instance? OrExpression expr)
    (let [^OrExpression e expr]
      {:op :or
       :clauses [(translate-having-expr (.getLeftExpression e) find-elems find-aliases)
                 (translate-having-expr (.getRightExpression e) find-elems find-aliases)]})

    ;; Comparison: aggregate op value
    (or (instance? GreaterThan expr)
        (instance? GreaterThanEquals expr)
        (instance? MinorThan expr)
        (instance? MinorThanEquals expr)
        (instance? EqualsTo expr)
        (instance? NotEqualsTo expr))
    (let [left (cond
                 (instance? GreaterThan expr) (.getLeftExpression ^GreaterThan expr)
                 (instance? GreaterThanEquals expr) (.getLeftExpression ^GreaterThanEquals expr)
                 (instance? MinorThan expr) (.getLeftExpression ^MinorThan expr)
                 (instance? MinorThanEquals expr) (.getLeftExpression ^MinorThanEquals expr)
                 (instance? EqualsTo expr) (.getLeftExpression ^EqualsTo expr)
                 (instance? NotEqualsTo expr) (.getLeftExpression ^NotEqualsTo expr))
          right (cond
                  (instance? GreaterThan expr) (.getRightExpression ^GreaterThan expr)
                  (instance? GreaterThanEquals expr) (.getRightExpression ^GreaterThanEquals expr)
                  (instance? MinorThan expr) (.getRightExpression ^MinorThan expr)
                  (instance? MinorThanEquals expr) (.getRightExpression ^MinorThanEquals expr)
                  (instance? EqualsTo expr) (.getRightExpression ^EqualsTo expr)
                  (instance? NotEqualsTo expr) (.getRightExpression ^NotEqualsTo expr))
          op (cond
               (instance? GreaterThan expr) '>
               (instance? GreaterThanEquals expr) '>=
               (instance? MinorThan expr) '<
               (instance? MinorThanEquals expr) '<=
               (instance? EqualsTo expr) '=
               (instance? NotEqualsTo expr) 'not=)
          ;; Left: aggregate function or alias reference
          col-idx (cond
                    (instance? Function left)
                    (match-aggregate-index ^Function left find-elems find-aliases)
                    ;; Column reference — resolve as alias
                    (instance? Column left)
                    (let [col-name (.getColumnName ^Column left)]
                      (some (fn [[i a]] (when (= a col-name) i))
                            (map-indexed vector find-aliases)))
                    :else nil)
          value (cond
                  (instance? LongValue right) (.getValue ^LongValue right)
                  (instance? DoubleValue right) (.getValue ^DoubleValue right)
                  (instance? StringValue right) (expr/string-value-text ^StringValue right)
                  :else (str right))]
      {:op op :col-idx col-idx :value value})

    ;; IS NULL / IS NOT NULL
    (instance? IsNullExpression expr)
    (let [^IsNullExpression e expr
          not-null? (.isNot e)
          inner (.getLeftExpression e)
          col-idx (when (instance? Column inner)
                    (let [col-name (.getColumnName ^Column inner)]
                      (some (fn [[i a]] (when (= a col-name) i))
                            (map-indexed vector find-aliases))))]
      {:op (if not-null? :is-not-null :is-null) :col-idx col-idx})

    :else
    {:op :unsupported :expr (str expr)}))

;; ============================================================================
;; Derived-table materialization — shared by FROM (...) AS t and
;; JOIN (...) AS t subqueries, as well as table functions in those positions.
;; ============================================================================

(declare translate-select)
(declare extract-value)
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
    (instance? DoubleValue expr) (.getValue ^DoubleValue expr)
    (instance? StringValue expr) (expr/string-value-text ^StringValue expr)
    (instance? SignedExpression expr)
    (let [v (srf-const-eval (.getExpression ^SignedExpression expr))]
      (if (number? v) (- v) ::corr))
    (instance? ArrayConstructor expr) (extract-value ^ArrayConstructor expr)
    :else ::corr))

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
  ([tf] (materialize-table-function tf srf-const-eval))
  ([^net.sf.jsqlparser.statement.select.TableFunction tf eval-fn]
   (let [^net.sf.jsqlparser.expression.Function f (.getFunction tf)
         fname (str/lower-case (or (.getName f) ""))
         params (vec (or (.getParameters f) []))
         with-ord? (some-> (.getWithClause tf) str
                           (->> (= "ORDINALITY")))
         vtype-of (fn [v]
                    (cond
                      (instance? Long v)    :db.type/long
                      (instance? Double v)  :db.type/double
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
       ;; Integer series (inclusive of stop, like PG). Numeric/timestamp
       ;; variants are future work; non-integer args fall through to nil.
       (let [args (mapv eval-fn params)]
         (when (and (>= (count args) 2)
                    (every? integer? (take 3 args)))
           (let [[start stop step] args
                 step (long (or step 1))]
             (when-not (zero? step)
               (let [vals (vec (range start
                                      (if (pos? step) (inc stop) (dec stop))
                                      step))]
                 ;; PG types integer generate_series as int4 — advertise
                 ;; that so clients parse the values as numbers, not int8
                 ;; strings.
                 (with-ordinality ["generate_series"]
                   (mapv (fn [v] [(long v)]) vals)
                   [:db.type/long] ["int4"]))))))

       (contains? #{"now" "current_timestamp" "transaction_timestamp"
                    "statement_timestamp" "clock_timestamp"} fname)
       ;; Scalar function used as a one-row table. The timestamp is
       ;; captured at translate time (good enough — the value is "recent";
       ;; sub-statement clock precision isn't meaningful here).
       (with-ordinality [fname] [[(java.util.Date.)]] [:db.type/instant] [nil])

       :else nil))))

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
        fname  (str/lower-case (or (.getName (.getFunction tf)) "tf"))
        sub-name (or talias (str fname "_" (System/nanoTime)))]
    (when-let [{:keys [aliases rows vtypes pg-types]} (materialize-table-function tf)]
      (let [aliases (if (and talias (= 1 (count aliases))) [talias] aliases)
            schema-tx (mapv (fn [a vt pt]
                              (cond-> {:db/ident       (keyword sub-name a)
                                       :db/valueType   vt
                                       :db/cardinality :db.cardinality/one}
                                pt (assoc :pg/type pt)))
                            aliases vtypes (concat (or pg-types []) (repeat nil)))
            spec-db (d/db-with db schema-tx)
            data-tx (mapv (fn [row]
                            (into {}
                                  (keep-indexed
                                   (fn [i a]
                                     (let [v (nth row i nil)]
                                       (when (some? v) [(keyword sub-name a) v])))
                                   aliases)))
                          rows)
            spec-db2 (if (seq data-tx) (d/db-with spec-db data-tx) spec-db)]
        {:db spec-db2 :schema (:schema spec-db2)
         :name sub-name :alias sub-name :aliases aliases}))))

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
        sub-name (or sub-alias (str "derived_" (System/nanoTime)))
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
           :alias   sub-name
           :aliases aliases}))

      ;; (SELECT … FROM real-table …) AS t — run the inner select.
      ;; Inner may also be a SetOperationList (UNION/INTERSECT/EXCEPT) —
      ;; handle that by translating each branch, executing, and combining.
      (or inner-ps (instance? net.sf.jsqlparser.statement.select.SetOperationList inner))
      (materialize-set-op! inner sub-name db schema)

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
  "Evaluate one SQL fragment for a deferred correlated item against `query-db`
   with *from-bindings* already bound. `subquery?` true → run `sql` as-is;
   false → wrap as `SELECT (<sql>)`. First cell, or nil on error/empty."
  [parse-fn sql subquery? inner-schema query-db]
  (try
    (let [run-sql (if subquery? sql (str "SELECT (" sql ")"))
          p   (parse-fn run-sql inner-schema query-db)
          q   (:query p) ia (:in-args p) qdb (or (:enriched-db p) query-db)]
      (if (nil? q)
        (let [lr (:literal-row p)] (if (sequential? lr) (first lr) lr))
        (let [res (if (seq ia) (apply d/q q qdb ia) (d/q q qdb)) fr (first res)]
          (if (sequential? fr) (first fr) fr))))
    (catch Throwable _ nil)))

(defn- eval-corr-then
  "Evaluate a CASE branch THEN/ELSE spec with *from-bindings* bound."
  [parse-fn then-spec inner-schema query-db]
  (cond
    (nil? then-spec)          :__null__
    (:subquery-sql then-spec) (eval-corr-scalar parse-fn (:subquery-sql then-spec) true inner-schema query-db)
    :else                     (eval-corr-scalar parse-fn (:expr-sql then-spec) false inner-schema query-db)))

(defn- run-correlated-spec
  "Value of a deferred correlated SELECT item for one outer row. `fb` is the
   per-row *from-bindings*. :scalar runs the subquery; :case walks branches."
  [parse-fn spec fb inner-schema query-db]
  (binding [params/*from-bindings* fb
            *eval-update-db* query-db]
    (case (:kind spec)
      :case
      (let [hit (some (fn [{:keys [when-sql then]}]
                        (when (true? (eval-corr-scalar parse-fn when-sql false inner-schema query-db))
                          [(eval-corr-then parse-fn then inner-schema query-db)]))
                      (:branches spec))]
        (if hit (first hit) (eval-corr-then parse-fn (:else spec) inner-schema query-db)))
      (eval-corr-scalar parse-fn (:inner-sql spec) true inner-schema query-db))))

(defn resolve-correlated-rows
  "Resolve a parsed SELECT's deferred correlated subqueries against raw result
   `rows`: per outer row, run each subquery with the correlation columns bound
   into *from-bindings*, splice the value at its out-pos, and drop the hidden
   __corr_ columns. Returns [resolved-rows resolved-aliases]. No-op (returns
   [rows find-aliases]) when there are no correlated subqueries."
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
                                         n-output)]
      [new-rows new-aliases])
    [rows (:find-aliases parsed)]))

(defn materialize-set-op!
  "Run a SELECT (PlainSelect or SetOperationList) and persist its rows
   under `target-name/<col>` in a speculative db. Returns the same
   `{:db :schema :name :alias :aliases}` map shape as
   `materialize-derived-select!` so callers can swap them.

   Used by both the derived-table path (FROM (...) AS t) and the CTE
   path (WITH t AS (...)), since SQL set operations over heterogeneous
   tables can't be expressed natively in Datalog — we have to flatten
   them into a single virtual table."
  [inner target-name db schema]
  (let [with-fn d/db-with
        is-union? (instance? net.sf.jsqlparser.statement.select.SetOperationList inner)
        branch-parsed
        (if is-union?
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
          {:op nil :branches [(translate-select ^PlainSelect inner schema db)]})
        sub-parsed (first (:branches branch-parsed))
        sub-aliases (:find-aliases sub-parsed)
        ;; Per-column expected OID from the inner translate-select's
        ;; oid-infer pass. Used as a default when the materialised
        ;; rows are empty or numerically-mixed (samples alone can't
        ;; pick a type then). Aligns the speculative-db's
        ;; :db/valueType with what describeResult will tell clients.
        sub-oids (:select-item-oids sub-parsed)
        q-fn d/q
        run-branch (fn [{:keys [query in-args sql-limit sql-offset hidden-count] :as p}]
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
                           raw (cond->> raw
                                 sql-offset (drop sql-offset)
                                 sql-limit  (take sql-limit))
                           hc (or hidden-count 0)
                           visible (- (count (:find query)) hc)]
                       (if (pos? hc)
                         (mapv #(if (sequential? %) (vec (take visible %)) %) raw)
                         raw)))
        branch-rows (mapv run-branch (:branches branch-parsed))
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
        sub-oids    (if corr-resolved nil sub-oids)
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
                           (number? v)                               :numeric
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
                       :db.type/bigdec  (fn [v]
                                          (cond
                                            (instance? java.math.BigDecimal v) v
                                            (instance? java.math.BigInteger v) (java.math.BigDecimal. ^java.math.BigInteger v)
                                            (integer? v) (java.math.BigDecimal/valueOf (long v))
                                            (float? v)   (java.math.BigDecimal/valueOf (double v))
                                            :else        (java.math.BigDecimal. (str v))))
                       :db.type/string  str
                       identity))
        ;; Always emit a row-existence marker so `t.*` expansion in
        ;; the OUTER select has an entity anchor even when every
        ;; non-marker column is NULL on a given row (e.g. Metabase's
        ;; `NULL as role` projection in build_privilege_map).
        row-marker (pgs/row-marker-attr target-name)
        ;; Per-column inferred type + coercion fn, computed once.
        col-types (mapv (fn [i] (col-vtype i)) (range (count sub-aliases)))
        col-coercions (mapv col-coerce col-types)
        schema-tx (conj
                   (vec (for [[i a] (map-indexed vector sub-aliases)]
                          {:db/ident (keyword target-name a)
                           :db/valueType (nth col-types i)
                           :db/cardinality :db.cardinality/one}))
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
     :alias   target-name
     :aliases sub-aliases}))

(defn- agg-cast-inner
  "If `expr` is a CAST (or parenthesised CAST) wrapping an aggregate —
   `count(*)::int4`, `sum(x)::numeric`, an aggregate AnalyticExpression —
   return that inner aggregate so the select-item dispatch registers it as
   an aggregate instead of routing the whole CAST through the scalar-
   expression path (which feeds the aggregate's spec map into the cast
   coercer → \"cannot coerce PersistentArrayMap to bigint\").

   The wrapping cast's result OID is computed independently by
   `select-item-oids` (which walks the original CAST via oid-infer), and an
   integer-width cast over count/sum/min/max doesn't change the value, so
   dropping the runtime cast here is correct for those. A value-changing
   cast over an aggregate (e.g. `avg(x)::int` truncation) loses the
   truncation — acceptable for now, and strictly better than the previous
   hard error. Returns nil when `expr` is not a cast-over-aggregate."
  [expr]
  (letfn [(peel [e]
            (cond
              (instance? CastExpression e) (peel (.getLeftExpression ^CastExpression e))
              (instance? Parenthesis e)    (peel (.getExpression ^Parenthesis e))
              :else e))]
    (when (instance? CastExpression expr)
      (let [i (peel expr)]
        (when (or (and (instance? Function i)
                       (fns/aggregate-function? (str/lower-case (.getName ^Function i))))
                  (instance? net.sf.jsqlparser.expression.AnalyticExpression i))
          i)))))

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
   in scope (`*cte-relations*`), or a catalog relation the catalog layer
   synthesises (`pg_*`, `information_schema`, or any schema-qualified
   name). Used to raise a clean 42P01 for a genuinely-absent relation
   instead of the cryptic 'Query for unknown vars' failure (SELECT *) or a
   silently-empty result (SELECT col). Column-level EAV permissiveness is
   intentionally NOT touched — an existing table's missing column still
   reads as NULL."
  [schema tname]
  (or (nil? tname)
      (let [t (str/lower-case tname)]
        (or (str/starts-with? t "pg_")
            (str/starts-with? t "information_schema")
            (str/includes? t ".")            ; schema-qualified catalog ref
            (contains? *cte-relations* t)
            (some (fn [[k _]] (and (keyword? k) (= (namespace k) tname)))
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
   doc/correlated-lateral-plan.md."
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

(defn translate-select
  "Translate a PlainSelect into a Datalog query map + metadata.
   Returns {:query map :find-aliases [...] :has-aggregates? bool}"
  [^PlainSelect select schema & [db]]
  (let [;; FROM clause — may be a Table or a derived table (subquery)
        from-item (.getFromItem select)
        ;; Handle derived tables: FROM (SELECT ...) AS sub, including
        ;; table-function forms like (SELECT * FROM unnest(ARRAY[…])
        ;; WITH ORDINALITY) AS sub.
        [db schema name alias]
        (cond
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
          (and db (instance? net.sf.jsqlparser.statement.select.TableFunction from-item))
          (if-let [{vdb :db vschema :schema vname :name valias :alias}
                   (table-fn->virtual-table
                    ^net.sf.jsqlparser.statement.select.TableFunction from-item db)]
            [vdb vschema vname valias]
            [db schema nil nil])

          ;; Regular table
          :else
          (let [{tname :name talias :alias} (when (instance? Table from-item)
                                              (ctx/extract-table-info ^Table from-item))]
            [db schema tname talias]))
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
        [db schema join-aliases derived-joins]
        (reduce
         (fn [[db schema aliases derived] ^Join j]
           (let [rt (.getRightItem j)]
             (cond
               (instance? Table rt)
               (let [{jn :name ja :alias} (ctx/extract-table-info ^Table rt)]
                 [db schema
                  (cond-> aliases
                    (and jn ja) (assoc ja jn)
                    jn          (assoc jn jn))
                  derived])

               (and db (instance? ParenthesedSelect rt))
               (if-let [{spec-db :db spec-schema :schema
                         sub-name :name} (materialize-derived-select!
                                          ^ParenthesedSelect rt db schema)]
                 [spec-db spec-schema
                  (assoc aliases sub-name sub-name)
                  (conj derived {:join j :alias sub-name})]
                 [db schema aliases derived])

               :else
               [db schema aliases derived])))
         [db schema {} []]
         joins)
        table-aliases (merge table-aliases join-aliases)

        ;; Aliases of derived tables in JOIN positions. translate-join
        ;; consults this to skip the ref/db_id unification path, which
        ;; assumes the right-side alias names a real entity in the live
        ;; db. Derived rows live in their own entity-id space in the
        ;; speculative db; we have to JOIN them by value, not by
        ;; entity-id unification.
        derived-alias-set (into #{} (map :alias) derived-joins)

        ;; Create context
        hints (pgs/schema-hints db)
        ctx (ctx/make-ctx schema table-aliases default-table
                          {:db db
                           :parse-sql params/*parse-sql*
                           :hints hints
                           :derived-aliases derived-alias-set
                           :ref-targets (pgs/validate-ref-targets!
                                         db schema
                                         (pgs/derive-ref-targets schema hints))})

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
        _ (when where-expr
            (let [preds (expr/translate-predicate ctx where-expr)]
              (swap! (:where-clauses ctx) into preds)))

        has-distinct? (some? (.getDistinct select))

        ;; GROUP BY
        group-by-element (.getGroupBy select)
        group-by (when group-by-element
                   (seq (.getGroupByExpressions ^GroupByElement group-by-element)))
        _ (when (seq group-by)
            (doseq [g group-by] (expr/translate-expr ctx g)))

        ;; HAVING clause
        having-expr (.getHaving select)

        ;; Process select items
        find-elements (atom [])
        find-aliases (atom [])
        has-aggregates? (atom false)
        compound-exprs (atom [])  ;; [{:alias str :op sym :l-idx int :r-idx int}]
        window-specs (atom [])    ;; [{:op kw :partition-by [idx] :order-by [[idx dir]] :frame {...}}]

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
            (when (= result-oid types/oid-numeric)
              (case agg-name
                "sum" 'datahike.pg.sql/filter-sum-numeric
                "avg" 'datahike.pg.sql/filter-avg-numeric
                nil))))

        ;; --- Correlated scalar subqueries in the SELECT list (slice A of the
        ;; per-row / LATERAL executor — doc/correlated-lateral-plan.md). A
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
                  ;; A CAST over an aggregate (count(*)::int4) dispatches as
                  ;; the inner aggregate; the cast only re-types the result
                  ;; (handled by select-item-oids). See agg-cast-inner.
                  expr (or (agg-cast-inner raw-expr) raw-expr)
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
                    (let [v (ctx/col-var! ctx [:aliased raw-name (:attr col)])]
                      (swap! find-elements conj v)
                      (swap! find-aliases conj (:name col)))))

                ;; SELECT * — expand to all user columns (exclude db_id)
                (instance? AllColumns expr)
                (let [cols (pgs/column-info schema default-table db)]
                  (doseq [col cols
                          :when (not= "db_id" (:name col))]
                    (let [v (ctx/col-var! ctx (:attr col))]
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
                      is-window? (and (not within-group?)
                                      (or (seq partition-list) (seq order-by-list)
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
                    (let [;; Translate PARTITION BY columns to find-element indices
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
                                                   asc? (.isAsc obe)]
                                               (when-not (some #{v} @find-elements)
                                                 (swap! find-elements conj v)
                                                 (swap! find-aliases conj (str "__win_ord_" (count @find-elements))))
                                               [(.indexOf ^java.util.List @find-elements v)
                                                (if asc? :asc :desc)]))
                                           order-by-list))
                          ;; Translate aggregate column (for SUM/AVG/etc.)
                          col-idx (when inner-expr
                                    (let [v (expr/translate-expr ctx inner-expr)
                                          v (if (seq? v) (ctx/materialize-arg! ctx v) v)]
                                      (when-not (some #{v} @find-elements)
                                        (swap! find-elements conj v)
                                        (swap! find-aliases conj (str "__win_col_" (count @find-elements))))
                                      (.indexOf ^java.util.List @find-elements v)))
                          ;; Parse frame specification
                          frame (if window-elem
                                  (let [wt (str/upper-case (str (.getType window-elem)))
                                        range? (str/starts-with? wt "RANGE")
                                        ;; JSqlParser 5.1 does not expose structured frame-bound types
                                        ;; (no WindowRange.getStart() / WindowOffset.getType() etc.), so
                                        ;; we fall back to parsing the toString() representation with
                                        ;; str/upper-case + str/starts-with? / regex.
                                        parse-bound (fn [s default-bound]
                                                      (let [upper (str/upper-case (str s))]
                                                        (cond
                                                          (str/starts-with? upper "UNBOUNDED PRECEDING") :unbounded-preceding
                                                          (str/starts-with? upper "UNBOUNDED FOLLOWING") :unbounded-following
                                                          (str/starts-with? upper "CURRENT ROW") :current-row
                                                          ;; Numeric bound: "5 PRECEDING" or "3 FOLLOWING"
                                                          (re-find #"(\d+)\s+PRECEDING" upper)
                                                          [(Long/parseLong (second (re-find #"(\d+)\s+PRECEDING" upper))) :preceding]
                                                          (re-find #"(\d+)\s+FOLLOWING" upper)
                                                          [(Long/parseLong (second (re-find #"(\d+)\s+FOLLOWING" upper))) :following]
                                                          :else default-bound)))
                                        start-bound (parse-bound (.getRange window-elem) :unbounded-preceding)
                                        off (.getOffset window-elem)
                                        end-bound (if (nil? off)
                                                    (if (seq order-by-list) :current-row :unbounded-following)
                                                    (parse-bound off :unbounded-following))]
                                    {:type (if range? :range :rows)
                                     :start start-bound :end end-bound})
                                  ;; Default frame per SQL standard
                                  (if (seq order-by-list)
                                    {:type :rows :start :unbounded-preceding :end :current-row}
                                    {:type :rows :start :unbounded-preceding :end :unbounded-following}))
                          ;; Build window spec
                          op-kw (keyword fname)
                          win-spec (cond-> {:op op-kw
                                            :partition-by (or part-idxs [])
                                            :order-by (or ob-specs [])
                                            :frame frame}
                                     col-idx (assoc :col-idx col-idx)
                                     (= fname "ntile")
                                     (assoc :ntile-n (when inner-expr
                                                       (let [v (expr/translate-expr ctx inner-expr)]
                                                         (when (number? v) (long v))))))]
                      ;; Don't add alias to find-aliases — the server adds it
                      ;; after computing the window values. find-aliases must match
                      ;; the Datalog :find elements count.
                      (swap! window-specs conj (assoc win-spec :alias (or alias-str fname))))

                    ;; Not a window — handle as FILTER aggregate or plain aggregate
                    :else
                    (do
                      (reset! has-aggregates? true)
                      (if (and filter-expr agg-sym)
                        (let [cond-form (expr/translate-predicate-expr ctx filter-expr)
                              inner-val (if inner-expr (expr/translate-expr ctx inner-expr)
                                            (ctx/entity-var! ctx default-table))
                              case-var (ctx/fresh-var! ctx)
                          ;; Collect all referenced variables
                              cond-vars (vec (ctx/collect-vars cond-form))
                              all-param-vars (vec (distinct (concat cond-vars
                                                                    (when (symbol? inner-val) [inner-val]))))
                              is-count? (= fname "count")
                          ;; COUNT FILTER: 1 if matched, 0 if not → SUM
                          ;; All others: value if matched, :__null__ if not → filter-* aggregate
                              compiled-fn (let [pv all-param-vars
                                                cf cond-form
                                                iv inner-val
                                                cnt? is-count?]
                                            (fn [& args]
                                              (let [bindings (zipmap pv args)]
                                                (if (expr/interpret-form cf bindings)
                                                  (if cnt? 1 (expr/interpret-form iv bindings))
                                                  (if cnt? 0 :__null__)))))
                              fn-param (symbol (str "?filter-fn" (swap! (:var-counter ctx) inc)))
                          ;; Per-input-type variant — same numeric-promotion
                          ;; rule as the non-FILTER aggregate path.
                              filter-precision-variant
                              (when inner-expr
                                (pick-precision-variant
                                 fname
                                 (oid/expr-oid inner-expr agg-oid-env)))
                          ;; Choose aggregate: COUNT→sum, others→filter-aware variant
                              filter-agg (cond
                                           is-count? 'sum
                                           filter-precision-variant filter-precision-variant
                                           :else
                                           (case fname
                                             "sum" 'datahike.pg.sql/filter-sum
                                             "avg" 'datahike.pg.sql/filter-avg
                                             "min" 'datahike.pg.sql/filter-min
                                             "max" 'datahike.pg.sql/filter-max
                                             'datahike.pg.sql/filter-sum))]
                          (swap! (:in-params ctx) conj fn-param)
                          (swap! (:in-args ctx) conj compiled-fn)
                          (ctx/add-clause! ctx [(apply list fn-param all-param-vars) case-var])
                          (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table))
                          (swap! find-elements conj (list filter-agg case-var))
                          (swap! find-aliases conj (or alias-str fname)))
                    ;; No filter — treat as regular aggregate
                        (let [v (if inner-expr (expr/translate-expr ctx inner-expr)
                                    (ctx/entity-var! ctx default-table))]
                          (swap! find-elements conj (list (or agg-sym 'count) v))
                          (swap! find-aliases conj (or alias-str fname)))))))

                ;; Aggregate: COUNT(*), SUM(col), etc.
                (and (instance? Function expr)
                     (fns/aggregate-function? (str/lower-case (.getName ^Function expr))))
                (let [^Function f expr
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
                        (swap! find-aliases conj (or alias-str "count")))
                      ;; Multi-argument aggregates (CORR)
                      (if (and (= agg-sym 'datahike.pg.sql/filter-corr) params (= 2 (count params)))
                        (let [v1 (expr/translate-expr ctx (first params))
                              v2 (expr/translate-expr ctx (second params))
                              pair-var (ctx/fresh-var! ctx)]
                          (ctx/add-clause! ctx [(list 'vector v1 v2) pair-var])
                          (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table))
                          (swap! find-elements conj (list 'datahike.pg.sql/filter-corr pair-var))
                          (swap! find-aliases conj (or alias-str "corr")))
                        ;; Single-argument: COUNT(col), SUM(col), AVG(col), etc.
                        (let [inner-expr (first params)
                              v (expr/translate-expr ctx inner-expr)
                              ;; Materialize expression args (e.g. SUM(a * b) → SUM(?v))
                              v (if (seq? v) (ctx/materialize-arg! ctx v) v)
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
                          (when-not is-dh-distinct?
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
                          (swap! find-aliases conj (or alias-str fname)))))))

                ;; Regular column or expression
                :else
                (let [v (expr/translate-expr ctx expr)]
                  (if (:compound-agg v)
                    ;; Compound aggregate expression: MAX(a) - MIN(a)
                    ;; Add each aggregate as find-elements. Record the
                    ;; arithmetic expression for server-side post-processing.
                    (let [add-agg!
                          (fn [agg-marker]
                            (when (:aggregate agg-marker)
                              (let [{:keys [fn params]} agg-marker
                                    agg-sym (get fns/sql-aggregate->datalog fn)
                                    inner (when params (first params))
                                    inner-var (if inner
                                                (let [iv (expr/translate-expr ctx inner)]
                                                  (if (seq? iv) (ctx/materialize-arg! ctx iv) iv))
                                                (ctx/entity-var! ctx default-table))]
                                (reset! has-aggregates? true)
                                (when (not= agg-sym 'count-distinct)
                                  (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table)))
                                (let [idx (count @find-elements)]
                                  (swap! find-elements conj (list agg-sym inner-var))
                                  (swap! find-aliases conj (str "__compound_" idx))
                                  idx))))
                          {:keys [op left right]} v
                          l-idx (add-agg! left)
                          r-idx (add-agg! right)]
                      (swap! compound-exprs conj {:alias (or alias-str (str (:expr v)))
                                                  :op op :l-idx l-idx :r-idx r-idx}))
                    ;; Regular non-aggregate expression
                    (let [v (cond
                              (seq? v)            (ctx/materialize-arg! ctx v)
                              (not (symbol? v))   (let [var (ctx/fresh-var! ctx)
                                                        ;; Datahike drops rows when a fn-binding
                                                        ;; produces nil; use the :__null__ sentinel
                                                        ;; for SQL NULL projections so the row
                                                        ;; survives. The wire layer maps the
                                                        ;; sentinel back to NULL on output.
                                                        bind-v (if (nil? v) :__null__ v)]
                                                    (ctx/add-clause! ctx [(list 'identity bind-v) var])
                                                    var)
                              :else               v)
                          col-alias (when (and (nil? alias-str) (instance? Column expr))
                                      (unquote-ident (.getColumnName ^Column expr)))]
                      (swap! find-elements conj v)
                      (swap! find-aliases conj (or alias-str
                                                   col-alias
                                                   (when (symbol? v)
                                                     (subs (str v) 1))
                                                   (str v)))))))))

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
        select-item-oids
        (let [acc (reduce
                   (fn [v ^SelectItem item]
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
                               cols (pgs/column-info schema real db)]
                           (into v (keep (fn [col]
                                           (when (not= "db_id" (:name col))
                                             (:oid col)))
                                         cols)))
                         (instance? AllColumns expr)
                         (let [cols (pgs/column-info schema default-table db)]
                           (into v (keep (fn [col]
                                           (when (not= "db_id" (:name col))
                                             (:oid col)))
                                         cols)))
                         :else
                         (conj v (oid/expr-oid expr oid-env)))))
                   []
                   ;; loop-items excludes deferred correlated subqueries, so
                   ;; these OIDs line up with the non-subquery part of
                   ;; find-aliases (the __corr_ tail pads to nil below).
                   loop-items)
                ;; find-aliases may be longer than acc when SELECT
                ;; contains JOIN-driven entity vars added to :find
                ;; for :with semantics. Pad with nil so the vector
                ;; lines up index-for-index with find-aliases.
              n (count @find-aliases)]
          (vec (take n (concat acc (repeat nil)))))

        ;; For JOINs: add entity vars to :with to prevent dedup of rows
        ;; from different entity combinations that produce identical values.
        ;; For LEFT JOINs, only add the left table's entity var (not the
        ;; right-side which may be synthetic in the unmatched branch).
        _ (when (seq join-infos)
            (let [right-aliases (set (map :alias join-infos))
                  left-join? (some #(= :left (:join-type %)) join-infos)]
              (doseq [[alias-key evar] @(:entity-vars ctx)]
                (when (or (not left-join?)
                          (not (contains? right-aliases alias-key)))
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

        ;; ORDER BY — resolve aliases to find-elements before creating patterns
        order-by (.getOrderByElements select)
        order-by-spec (when (seq order-by)
                        (let [fe-snap @find-elements
                              fa-snap @find-aliases]
                          (mapv (fn [^OrderByElement obe]
                                  (let [expr (.getExpression obe)
                                        asc? (.isAsc obe)
                                        ;; Check if ORDER BY references a SELECT alias
                                        v (if (instance? Column expr)
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
                                            (expr/translate-expr ctx expr))
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
                                        v (if (and (seq? v)
                                                   (not (contains? agg-syms (first v))))
                                            (ctx/materialize-arg! ctx v)
                                            v)
                                        ;; Detect unsupported: aggregate in ORDER BY not in SELECT
                                        _ (when (and (map? v) (:aggregate v))
                                            (throw (ex-info "ORDER BY on aggregate not in SELECT list"
                                                            {:error :feature-not-supported
                                                             :feature "ORDER BY on aggregate not in SELECT list"
                                                             :detail (str "ORDER BY on aggregate not in SELECT list is not supported: " (str expr))})))]
                                    [v (if asc? :asc :desc)]))
                                order-by)))

;; LIMIT / OFFSET / FETCH FIRST
        limit-expr (.getLimit select)
        limit-val (when limit-expr
                    (let [rc (.getRowCount ^Limit limit-expr)]
                      (when (instance? LongValue rc)
                        (.getValue ^LongValue rc))))
        ;; FETCH FIRST N ROWS ONLY (SQL:2008 syntax, used by Hibernate)
        fetch-expr (.getFetch select)
        limit-val (or limit-val
                      (when fetch-expr
                        (let [^net.sf.jsqlparser.statement.select.Fetch f fetch-expr]
                          (.getRowCount f))))
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
                    (when-not (contains? optional-aliases alias-key)
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
                _ (doseq [clause all-clauses]
                    (when (and (vector? clause)
                               (= 3 (count clause))
                               (symbol? (first clause))
                               (keyword? (second clause))
                               (symbol? (nth clause 2)))
                      (let [evar (first clause)
                            vvar (nth clause 2)]
                        (if (or (contains? required-vars vvar)
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
                  to-convert (keep (fn [v]
                                     (first
                                      (filter (fn [c]
                                                (and (vector? c) (= 3 (count c))
                                                     (symbol? (first c))
                                                     (keyword? (second c))
                                                     (= v (nth c 2))
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

        ;; HAVING-only aggregates: if the HAVING expression references an
        ;; aggregate that isn't already in the SELECT projection, the
        ;; aggregate needs to be computed all the same — otherwise
        ;; `match-aggregate-index` can't resolve it and the server's
        ;; `apply-having` drops the predicate silently.
        ;;
        ;; Walk HAVING's JSqlParser tree, collect every Function whose
        ;; name is an aggregate, and append each one to find-elements as
        ;; a hidden column. Reuses the same translation shape as the
        ;; SELECT-item aggregate branch (`(agg-sym ?inner-var)`), so
        ;; downstream `match-aggregate-index` resolution and the
        ;; server's HAVING post-filter work without further changes.
        ;; The wire layer strips trailing :hidden-count columns from
        ;; results, so HAVING-only aggregates never reach the client.
        having-agg-fns (when having-expr
                         (let [found (atom [])
                               walk (fn walk [^net.sf.jsqlparser.expression.Expression e]
                                      (when e
                                        (cond
                                          (instance? Function e)
                                          (let [^Function f e
                                                fname (str/lower-case (.getName f))]
                                            (when (fns/aggregate-function? fname)
                                              (swap! found conj f)))
                                          (instance? AndExpression e)
                                          (do (walk (.getLeftExpression ^AndExpression e))
                                              (walk (.getRightExpression ^AndExpression e)))
                                          (instance? OrExpression e)
                                          (do (walk (.getLeftExpression ^OrExpression e))
                                              (walk (.getRightExpression ^OrExpression e)))
                                          (instance? GreaterThan e)         (do (walk (.getLeftExpression ^GreaterThan e))         (walk (.getRightExpression ^GreaterThan e)))
                                          (instance? GreaterThanEquals e)   (do (walk (.getLeftExpression ^GreaterThanEquals e))   (walk (.getRightExpression ^GreaterThanEquals e)))
                                          (instance? MinorThan e)           (do (walk (.getLeftExpression ^MinorThan e))           (walk (.getRightExpression ^MinorThan e)))
                                          (instance? MinorThanEquals e)     (do (walk (.getLeftExpression ^MinorThanEquals e))     (walk (.getRightExpression ^MinorThanEquals e)))
                                          (instance? EqualsTo e)            (do (walk (.getLeftExpression ^EqualsTo e))            (walk (.getRightExpression ^EqualsTo e)))
                                          (instance? NotEqualsTo e)         (do (walk (.getLeftExpression ^NotEqualsTo e))         (walk (.getRightExpression ^NotEqualsTo e)))
                                          (instance? IsNullExpression e)    (walk (.getLeftExpression ^IsNullExpression e)))))]
                           (walk having-expr)
                           @found))
        ;; Append each HAVING-only aggregate as a hidden find element.
        ;; `find-aliases` gets a sentinel "__having_agg_<i>" so its
        ;; length keeps matching find-elements; the hidden-count below
        ;; trims them from the visible projection.
        having-hidden
        (let [existing-agg-shapes
              (into #{}
                    (filter (fn [el] (and (seq? el) (symbol? (first el)))))
                    @find-elements)
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
                          precision-variant (pick-precision-variant
                                             fname
                                             (oid/expr-oid (first params) agg-oid-env))]
                      (when agg-sym
                        (when-not (= fname "count")
                          (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table)))
                        (list (or precision-variant agg-sym) v))))))]
          (->> having-agg-fns
               (keep (fn [f]
                       (when-let [elem (translate-agg f)]
                         (when-not (contains? existing-agg-shapes elem)
                           (reset! has-aggregates? true)
                           (swap! find-elements conj elem)
                           ;; Intentionally NOT extending find-aliases:
                           ;; aliases track the visible projection. The
                           ;; resulting (count find-elements) >
                           ;; (count find-aliases) — the gap rides on
                           ;; :hidden-count so the wire layer strips
                           ;; these columns before emitting rows, and
                           ;; describeResult / RowDescription stay
                           ;; aligned with find-aliases.
                           elem))))
               count))
        ;; Snapshot where-clauses AFTER the HAVING-aggregate translation
        ;; has had a chance to add column bindings via col-var!. If we
        ;; snapshot before, the aggregate's input var (e.g. ?sales_amount)
        ;; references no `:where` clause and Datahike rejects it as
        ;; "Query for unknown vars".
        where-clauses @(:where-clauses ctx)
        find-elems @find-elements

        find-elems-vec (vec find-elems)
        ;; ORDER BY: check if any order-by variable is nullable (get-else).
        ;; If so, sorting must happen in the server with a null-aware comparator.
        ;; Otherwise, use Datahike's optimized :order-by.
        ;; Vars produced by ctx/col-var! are tracked in ctx's :nullable-vars (always
        ;; get-else-bound); earlier passes also add to nullable-order-vars.
        nullable-vars (into @nullable-order-vars @(:nullable-vars ctx))
        has-nullable-order? (and order-by-spec
                                 (some (fn [[v _dir]]
                                         (and (symbol? v)
                                              (contains? nullable-vars v)))
                                       order-by-spec))
        [find-elems-vec hidden-count order-by-flat sql-order-by]
        (if order-by-spec
          (let [missing (filterv (fn [[v _dir]]
                                   (and (symbol? v)
                                        (neg? (.indexOf ^java.util.List find-elems-vec v))))
                                 order-by-spec)
                extended-find (into find-elems-vec (map first missing))
                ob (vec (mapcat
                         (fn [[v dir]]
                           (let [idx (.indexOf ^java.util.List extended-find v)]
                             (when (>= idx 0) [idx dir])))
                         order-by-spec))
                hidden (+ (count missing) having-hidden)]
            (if has-nullable-order?
              ;; Nullable ORDER BY → server-side sort (don't emit :order-by to Datahike)
              [extended-find hidden nil ob]
              ;; Non-nullable → Datahike handles it
              [extended-find hidden ob nil]))
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
                  hidden (+ (if (neg? already) 1 0) having-hidden)]
              [extended-find hidden [idx :asc] nil])
            [find-elems-vec having-hidden nil nil]))

        in-params @(:in-params ctx)
        in-args @(:in-args ctx)
        with-vars @(:with-vars ctx)
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
             :limit           (when-not sql-order-by limit-val)
             :offset          (when-not sql-order-by offset-val)
             :find-aliases    @find-aliases
             ;; OID per find-alias for Extended Query Describe. nil slots
             ;; fall back to value-based inference at Execute time (via
             ;; compute-schema-oids) or TEXT when neither path resolves.
             :select-item-oids select-item-oids
             :has-aggregates? @has-aggregates?
             :has-distinct?   has-distinct?
             :in-args         in-args
             :hidden-count    hidden-count
             ;; Pass enriched db when derived tables or derived-table-joins
             ;; created speculative data (FROM (…) AS sub or JOIN (…) AS sub).
             :enriched-db     (when (or (instance? ParenthesedSelect from-item)
                                        ;; bare SRF in FROM materialised into
                                        ;; a virtual table (table-fn->virtual-table)
                                        (instance? net.sf.jsqlparser.statement.select.TableFunction from-item)
                                        (seq derived-joins))
                                db)
             ;; Server-side sort for nullable ORDER BY columns
             :sql-order-by    sql-order-by
             :sql-limit       (when sql-order-by limit-val)
             :sql-offset      (when sql-order-by offset-val)
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
      (assoc :having (translate-having-expr having-expr find-elems-vec @find-aliases)))))

;; ============================================================================
;; DML translation: INSERT, UPDATE, DELETE
;; ============================================================================

(defn parse-bytea-hex
  "Decode a PostgreSQL bytea hex-format literal (`\\xDEADBEEF`) to a byte array.
   Accepts both `\\x...` and `\\\\x...` prefixes (JDBC/psycopg2 escape variants).
   Returns nil for values that don't look like hex bytea literals."
  [s]
  (when (string? s)
    (let [trimmed (str/trim s)
          without-prefix (cond
                           (str/starts-with? trimmed "\\x") (subs trimmed 2)
                           (str/starts-with? trimmed "\\\\x") (subs trimmed 3)
                           :else nil)]
      (when (and without-prefix
                 (re-matches #"[0-9a-fA-F]*" without-prefix)
                 (even? (count without-prefix)))
        (let [n (/ (count without-prefix) 2)
              bs (byte-array n)]
          (dotimes [i n]
            (aset-byte bs i
                       (unchecked-byte
                        (Integer/parseInt
                         (subs without-prefix (* 2 i) (+ 2 (* 2 i)))
                         16))))
          bs)))))

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
      (cond
        array-target?
        (let [elem-kw (or (get types/sql-name->elem-kw type-str) :text)]
          (cond
            (pg-arr/array? inner) (pg-arr/array elem-kw (:elements inner)
                                                (:dims inner) (:lbounds inner))
            (sequential? inner)   (pg-arr/array elem-kw (vec inner))
            (string? inner)       (pg-arr/from-pg-text inner elem-kw)
            :else                 (pg-arr/array elem-kw [inner])))

        (= cast-cat :integer)   (coerce/coerce-numeric inner :long)
        (= cast-cat :float)     (coerce/coerce-numeric inner :double)
        (= cast-cat :text)      (if (string? inner) inner (str inner))
        (= cast-cat :boolean)   (if (boolean? inner) inner (Boolean/parseBoolean (str inner)))
        (= cast-cat :timestamp) (if (instance? java.util.Date inner)
                                  inner
                                  (expr/parse-timestamp-string (str inner)))
        (= cast-cat :uuid)      (if (instance? java.util.UUID inner)
                                  inner
                                  (java.util.UUID/fromString (str inner)))
        (= cast-cat :bytes)     (cond
                                  (bytes? inner) inner
                                  (string? inner) (or (parse-bytea-hex inner)
                                                      (.getBytes ^String inner "UTF-8"))
                                  :else inner)
        ;; ::regnamespace — resolve to namespace OID
        (= type-str "regnamespace") 2200
        ;; ::regclass — match the same precedence used elsewhere:
        ;; :pg/table-oid first, then the hashCode fallback.
        (= type-str "regclass")
        (let [n (str inner)]
          (or (when (and params/*parse-db* (some? inner))
                (pgs/table-oid params/*parse-db* n))
              (when (seq n) (Math/abs (.hashCode ^String n)))
              0))
        ;; No category match — return as-is
        :else inner))))

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
     (instance? DoubleValue e) (.getValue ^DoubleValue e)
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
     nil
     (instance? SignedExpression e)
     (let [^SignedExpression se e
           inner (extract-value (.getExpression se) schema db)]
       (if (and (= (.getSign se) \-) (number? inner))
         (- inner)
         inner))
     (instance? CastExpression e)
     (apply-sql-cast (extract-value (.getLeftExpression ^CastExpression e) schema db)
                     ^CastExpression e)
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
           (let [parsed (translate-select ^PlainSelect inner schema db)
                 q (:query parsed)
                 in-args (:in-args parsed)
                 results (if (seq in-args)
                           (apply d/q q db in-args)
                           (d/q q db))
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

         :else (str e)))
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

(defn- apply-numeric-scale
  "PG NUMERIC(p,s) rounds/pads a value to scale `s` on input (e.g. 1 →
   1.00, 1.239 → 1.24). `scale` nil (unconstrained NUMERIC) leaves the
   value's own scale untouched. Only acts on BigDecimals."
  [v scale]
  (if (and scale (instance? java.math.BigDecimal v))
    (.setScale ^java.math.BigDecimal v (int scale) java.math.RoundingMode/HALF_UP)
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
  [val attr schema]
  (when (some? val)
    (let [vtype     (get-in schema [attr :db/valueType])
          elem-kw   (get-in schema [attr :pg/array-elem])
          num-scale (get-in schema [attr :pg/numeric-scale])]
      (cond
        ;; ParamRef is a defrecord placeholder for a `?` parameter
        ;; resolved at Bind time. Don't coerce it here — the branches
        ;; below would incorrectly treat it as a Clojure map (records
        ;; satisfy `map?`) and either jsonb-serialize it into a
        ;; "{\"idx\":N}" string for :db.type/string columns or stringify
        ;; via `(str val)`. Pass it through; substitute-params replaces
        ;; it with the decoded wire value, which already has the right
        ;; type from Bind.
        (params/param-ref? val) val

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
          :db.type/bigdec (apply-numeric-scale (coerce/coerce-numeric val :bigdec) num-scale)
          :db.type/long   (coerce/coerce-numeric val :long)
          val)
        ;; Numeric coercion across `:db.type/{long,double,float,bigdec}`
        ;; — handles both the string→number and number→number paths.
        ;; `coerce-numeric` raises 22003/22P02 with the right SQLSTATE.
        (and (= vtype :db.type/long)
             (or (string? val) (instance? Double val)))
        (coerce/coerce-numeric val :long)
        (and (= vtype :db.type/double) (or (string? val) (integer? val)))
        (coerce/coerce-numeric val :double)
        (and (= vtype :db.type/float) (or (string? val) (integer? val)))
        (coerce/coerce-numeric val :float)
        (and (= vtype :db.type/boolean) (string? val))
        (Boolean/parseBoolean val)
        ;; :db.type/keyword: SQL has no keyword literal, so clients
        ;; send the bare name as a string. Coerce 'draft' → :draft and
        ;; 'foo/bar' → :foo/bar (Clojure's `keyword` accepts both
        ;; forms). Empty / blank strings stay as-is so datahike's
        ;; rejection still surfaces (an empty keyword `:` is invalid).
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
        (try (java.util.UUID/fromString val) (catch Exception _ val))
        ;; jsonb: serialize Clojure maps/vectors to JSON strings for :db.type/string columns
        (and (= vtype :db.type/string) (or (map? val) (sequential? val)))
        (jb/serialize-jsonb val)
        (and (= vtype :db.type/string) (not (string? val))) (str val)
        ;; bytea: decode PG `\xHEX` hex literal to byte array; fall back to
        ;; raw UTF-8 bytes for non-hex strings so the value stays representable.
        (and (= vtype :db.type/bytes) (string? val))
        (or (parse-bytea-hex val) (.getBytes ^String val "UTF-8"))
        (and (= vtype :db.type/bytes) (bytes? val)) val
        ;; Numeric/decimal: bigdec via coerce-numeric — raises 22P02 on
        ;; bad-syntax strings instead of silently keeping the original.
        (and (= vtype :db.type/bigdec) (or (string? val) (number? val)))
        (apply-numeric-scale (coerce/coerce-numeric val :bigdec) num-scale)
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
        :else val))))

(declare eval-update-expr)

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

(defn eval-update-expr
  "Evaluate an UPDATE SET expression for a specific entity.
   For simple literals, returns the literal value.
   For expressions (col + 1, col * 2), evaluates against the entity's current values.

   JdbcParameter placeholders return a ParamRef — the tx-build step runs
   substitute-params once bound values are available from Bind."
  [value-expr entity-map ns-str schema]
  (cond
    (instance? JdbcParameter value-expr)
    (->ParamRef (.getIndex ^JdbcParameter value-expr))

    (instance? LongValue value-expr)
    (.getValue ^LongValue value-expr)

    (instance? DoubleValue value-expr)
    (.getValue ^DoubleValue value-expr)

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
          col-name (.getColumnName col-expr)
          tbl (.getTable col-expr)
          tbl-name (when tbl (unquote-ident (.getName ^Table tbl)))]
      (cond
        ;; Bound by UPDATE ... FROM (VALUES ...) AS alias(cols)
        (and tbl-name params/*from-bindings* (contains? params/*from-bindings* tbl-name))
        (get-in params/*from-bindings* [tbl-name (unquote-ident col-name)])

        (and tbl-name (= "EXCLUDED" (.toUpperCase ^String tbl-name)))
        (get entity-map (keyword "excluded" col-name))

        :else
        (get entity-map (keyword ns-str col-name))))

    ;; Arithmetic: recurse on both sides
    (instance? Addition value-expr)
    (let [^Addition e value-expr
          l (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r (eval-update-expr (.getRightExpression e) entity-map ns-str schema)]
      (when (and l r) (+ l r)))

    ;; Subtraction: numeric subtract or jsonb key deletion (col - 'key' or col - idx)
    (instance? Subtraction value-expr)
    (let [^Subtraction e value-expr
          l (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r (eval-update-expr (.getRightExpression e) entity-map ns-str schema)]
      (cond
        (and (nil? l)) nil
        ;; jsonb key/index deletion: left is jsonb (string containing JSON, map, or vector)
        (and (some? l) (some? r)
             (or (map? (jb/parse-jsonb l)) (sequential? (jb/parse-jsonb l))))
        (let [result (if (integer? r)
                       (jb/jsonb-delete-idx l (long r))
                       (jb/jsonb-delete-key l (str r)))]
          (jb/serialize-jsonb result))
        ;; Numeric subtraction
        (and (number? l) (number? r)) (- l r)
        :else nil))

    (instance? Multiplication value-expr)
    (let [^Multiplication e value-expr
          l (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r (eval-update-expr (.getRightExpression e) entity-map ns-str schema)]
      (when (and l r) (* l r)))

    (instance? Division value-expr)
    (let [^Division e value-expr
          l (eval-update-expr (.getLeftExpression e) entity-map ns-str schema)
          r (eval-update-expr (.getRightExpression e) entity-map ns-str schema)]
      (when (and l r (not (zero? r))) (/ l r)))

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
       (fn [current [key-val op-str]]
         (if (= op-str "->>")
           (jb/jsonb-get-text current key-val)
           (jb/serialize-jsonb (jb/jsonb-get current key-val))))
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

    ;; Function call.
    (instance? net.sf.jsqlparser.expression.Function value-expr)
    (let [^net.sf.jsqlparser.expression.Function f value-expr
          fname (str/lower-case (.getName f))
          args (some-> (.getParameters f) .getExpressions)]
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
        (str value-expr)))

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

    ;; Fallback: try as string
    :else (str value-expr)))

(defn extract-returning
  "Extract RETURNING clause column names from a ReturningClause.
   Returns nil if no RETURNING, :* for RETURNING *, or [col-name ...] for specific columns."
  [returning-clause]
  (when returning-clause
    (let [items (vec returning-clause)]
      (if (and (= 1 (count items))
               (instance? AllColumns (.getExpression ^SelectItem (first items))))
        :*
        (mapv #(unquote-ident (.getColumnName ^Column (.getExpression ^SelectItem %))) items)))))

;; Per-schema cache for enriched-schema. The enrichment is a pure
;; function of the schema (the db is just a query source). Schema is
;; immutable until DDL transacts. CREATE TABLE adds new attrs which
;; mints a new schema-map (new identity → new cache key), so no
;; explicit invalidator is needed — the only writer of
;; `:pg/array-elem` is `translate-create-table`, in the same tx that
;; mints the new schema. ALTER TABLE adding new columns does the
;; same.
(def ^:private enriched-schema-cache
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

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
        scale-meta (try
                     (into {}
                           (keep (fn [[ident typmod]]
                                   (let [[_p s] (types/decode-numeric-typmod typmod)]
                                     (when s [ident {:pg/numeric-scale s}]))))
                           (d/q
                            '{:find  [?ident ?typmod]
                              :where [[?e :db/ident ?ident]
                                      [?e :pg/typmod ?typmod]]}
                            db))
                     (catch Throwable _ {}))]
    (reduce-kv (fn [s ident more] (update s ident merge more))
               schema (merge-with merge pg-meta scale-meta))))

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
    (let [^java.util.Map outer enriched-schema-cache]
      (or (.get outer schema)
          (locking outer
            (or (.get outer schema)
                (let [enriched (compute-array-meta-enriched schema db)]
                  (.put outer schema enriched)
                  enriched)))))))

(defn translate-insert
  "Translate an INSERT statement to Datahike transaction data.
   Supports single-row and multi-row VALUES, with or without column list.
   Handles ON CONFLICT (UPSERT) via :db.fn/call for atomic execution."
  [^Insert insert schema db]
  (let [schema (enrich-schema-with-pg-array-meta schema db)
        table (.getTable insert)
        table-name (unquote-ident (.getName ^Table table))
        ns table-name
        columns (.getColumns insert)
        col-names (if (seq columns)
                    (mapv #(unquote-ident (.getColumnName ^Column %)) columns)
                    (or (pgs/column-order-from-db db table-name)
                        (when-let [cols (pgs/column-info schema table-name)]
                          (mapv :name (rest cols)))))
        select (.getSelect insert)
        ;; ON CONFLICT handling
        ^net.sf.jsqlparser.statement.insert.InsertConflictAction
        conflict-action (.getConflictAction insert)
        ^net.sf.jsqlparser.statement.insert.InsertConflictTarget
        conflict-target (.getConflictTarget insert)]
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
            inner-results (cond
                            (:literal-rows inner-parsed) (:literal-rows inner-parsed)
                            (:literal-row  inner-parsed) [(:literal-row inner-parsed)]
                            (seq inner-in-args)
                            (apply q-fn inner-query db inner-in-args)
                            :else (q-fn inner-query db))
            rows (mapv (fn [row]
                         (if (sequential? row) (vec row) [row]))
                       inner-results)
            ;; Build row-attrs the same way the VALUES branch does below.
            row-attrs
            (mapv (fn [row]
                    (into {}
                          (keep (fn [[col-name val]]
                                  (let [raw-attr (keyword ns col-name)
                                        attr (if db
                                               (ctx/resolve-inherited-attr raw-attr schema db)
                                               raw-attr)
                                        coerced (coerce-insert-value val attr schema)]
                                    (when (some? coerced)
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
          ;; Non-empty with ON CONFLICT DO NOTHING — delegate via :db.fn/call
          ;; that checks conflict on all inserted columns (PG semantics for
          ;; ON CONFLICT without explicit target). Reuses auto-populate-identity
          ;; for identity columns downstream.
          conflict-action
          (let [row-refs (atom [])
                do-nothing? (= (.getConflictActionType conflict-action)
                               net.sf.jsqlparser.statement.insert.ConflictActionType/DO_NOTHING)]
            (cond-> {:type :insert
                     :row-refs row-refs
                     :tx-data
                     [[:db.fn/call
                       (fn [txdb]
                         (let [q d/q]
                           (vec
                            (mapcat
                             (fn [attrs]
                               (let [effective-cols (vec (keys attrs))
                                     conflict-pairs (mapv (fn [col] [col (get attrs col)]) effective-cols)
                                     all-vals-present? (and (seq conflict-pairs)
                                                            (every? (fn [[_ v]] (some? v)) conflict-pairs))
                                     existing (when all-vals-present?
                                                (ffirst
                                                 (q {:find '[?e]
                                                     :where (mapv (fn [[col val]] ['?e col val]) conflict-pairs)}
                                                    txdb)))]
                                 (if existing
                                   (do (swap! row-refs conj existing) [])
                                   (let [tempid (str (gensym "insert-select-"))]
                                     (swap! row-refs conj tempid)
                                     [(assoc attrs :db/id tempid)]))))
                             row-attrs))))]]
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
                                          ;; some? — `false` is a legitimate value
                                          ;; for boolean columns, so don't use when-let.
                                              (when (some? coerced)
                                                [attr coerced])))
                                          (map vector col-names row))))
                            rows)
        ;; Add row-existence marker for this table
            marker (pgs/row-marker-attr table-name)
            has-marker? (boolean (get schema marker))
            row-attrs (if has-marker?
                        (mapv #(assoc % marker true) row-attrs)
                        row-attrs)
        ;; For INHERITS: also add parent's row-marker so parent queries find this entity
            parent-table (when db
                           (ffirst (d/q
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
            result
            (if conflict-action
          ;; ON CONFLICT — build :db.fn/call for atomic upsert
              (let [action-type (.getConflictActionType conflict-action)
                    do-nothing? (= action-type net.sf.jsqlparser.statement.insert.ConflictActionType/DO_NOTHING)
                    conflict-cols (when conflict-target
                                    (mapv #(keyword ns (unquote-ident %)) (.getIndexColumnNames conflict-target)))
                ;; Parse DO UPDATE SET assignments
                    update-assignments
                    (when-not do-nothing?
                      (mapv (fn [^net.sf.jsqlparser.statement.update.UpdateSet us]
                              (let [col-name (unquote-ident (.getColumnName ^Column (first (.getColumns us))))
                                    attr (keyword ns col-name)
                                    value-expr (first (.getValues us))]
                                {:attr attr :col-name col-name :value-expr value-expr}))
                            (.getUpdateSets conflict-action)))]
            ;; Shared atom: fn writes [eid-or-tempid] in row order so the
            ;; RETURNING dispatch can resolve ids in VALUES order (not hash order).
            ;; Existing rows (DO UPDATE) store the eid; new rows store the tempid.
                (let [row-refs (atom [])]
                  {:type :insert
                   :row-refs row-refs
                   :tx-data
                   [[:db.fn/call
                     (fn [txdb]
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
                                                [col {:eid seq-eid :val curr-val :inc increment}])))]
                         (vec (mapcat
                               (fn [attrs]
                         ;; Build multi-column conflict query:
                         ;; {:find [?e] :where [[?e :col1 val1] [?e :col2 val2] ...]}
                         ;; If no conflict target was given (ON CONFLICT DO NOTHING),
                         ;; PG checks all unique constraints — fall back to all
                         ;; inserted attribute keys (the natural-key case for m2m
                         ;; relation tables).
                                 (let [effective-cols (or conflict-cols (vec (keys attrs)))
                                       conflict-pairs (mapv (fn [col] [col (get attrs col)]) effective-cols)
                                       all-vals-present? (and (seq conflict-pairs)
                                                              (every? (fn [[_ v]] (some? v)) conflict-pairs))
                                       existing (when all-vals-present?
                                                  (ffirst
                                                   (q-fn
                                                    {:find '[?e]
                                                     :where (mapv (fn [[col val]] ['?e col val])
                                                                  conflict-pairs)}
                                                    txdb)))]
                                   (if existing
                                     (do
                               ;; Record existing eid at row position (DO UPDATE)
                                       (swap! row-refs conj existing)
                                       (if do-nothing?
                                         [] ;; DO NOTHING
                                 ;; DO UPDATE SET
                                         (vec (keep
                                               (fn [{:keys [attr col-name value-expr]}]
                                                 (let [;; Evaluate the update expression
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
                                                           (eval-update-expr
                                                            value-expr combined ns schema)))]
                                                   (when (some? new-val)
                                                     [:db/add existing attr
                                                      (or (coerce-insert-value new-val attr schema) new-val)])))
                                               update-assignments))))
                             ;; No conflict — normal insert with identity population
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
                                       (swap! row-refs conj tempid)
                                       (into [(assoc populated :db/id tempid)] seq-updates)))))
                               row-attrs))))]]
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
  [^Delete delete _schema]
  (let [table (.getTable delete)
        table-name (unquote-ident (.getName ^Table table))
        alias-obj (.getAlias ^Table table)
        alias-name (when alias-obj (unquote-ident (.getName ^Alias alias-obj)))
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
    (instance? DoubleValue expr)  (.getValue ^DoubleValue expr)
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

(defn translate-update
  "Translate an UPDATE statement to Datahike retract+assert pairs.
   Handles UPDATE with WITH RECURSIVE CTE — for these, the result is
   {:type :update-with-recursive ...} containing the rule, columns,
   and target table info for the server to execute."
  [^Update update schema db]
  (let [table (.getTable update)
        table-name (unquote-ident (.getName ^Table table))
        alias-obj (.getAlias ^Table table)
        alias-name (when alias-obj (unquote-ident (.getName ^Alias alias-obj)))
        ns table-name
        where-expr (.getWhere update)
        update-sets (.getUpdateSets update)
        withs (.getWithItemsList update)
        from-values (extract-from-values update)]
    (if (and withs (seq withs)
             (some #(.isRecursive ^net.sf.jsqlparser.statement.select.WithItem %) withs))
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
               :assignments (mapv (fn [^UpdateSet us]
                                    (let [cols (.getColumns us)
                                          exprs (.getValues us)]
                                      {:column (unquote-ident (.getColumnName ^Column (first cols)))
                                       :value-expr (first exprs)}))
                                  update-sets)}
        from-values (assoc :from-values from-values)
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
        select (.getSelect wi)
        select (if (instance? ParenthesedSelect select)
                 (.getSelect ^ParenthesedSelect select)
                 select)
        [anchor recursive]
        (cond
          (instance? SetOperationList select)
          (let [^SetOperationList sol select
                selects (.getSelects sol)]
            (when (= 2 (count selects))
              [(first selects) (second selects)]))
          (instance? PlainSelect select)
          [select nil])
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
   `:<target-name>/<col>` attr per column plus the row-existence marker."
  [target-name col-names col-types row-marker]
  (conj
   (vec (for [[i c] (map-indexed vector col-names)]
          {:db/ident       (keyword target-name c)
           :db/valueType   (nth col-types i)
           :db/cardinality :db.cardinality/one}))
   {:db/ident       row-marker
    :db/valueType   :db.type/boolean
    :db/cardinality :db.cardinality/one}))

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
   legacy engine can't evaluate the recursive bodies translate-recursive-cte
   emits (head var bound through a function op then filtered by a predicate,
   datahike PR #825)."
  [db rule rule-name rule-vars in-params in-args]
  (let [rule-call (apply list rule-name rule-vars)
        q {:find  rule-vars
           :in    (into '[$ %] in-params)
           :where [rule-call]}]
    (binding [dq/*force-legacy* false]
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

(defn- recursive-cte-branches
  "Split a WITH RECURSIVE item into [anchor recursive] PlainSelects, or nil
   if it isn't the expected `<anchor> UNION [ALL] <recursive>` shape."
  [^net.sf.jsqlparser.statement.select.WithItem wi]
  (let [select (let [s (.getSelect wi)]
                 (if (instance? ParenthesedSelect s)
                   (.getSelect ^ParenthesedSelect s) s))]
    (when (instance? SetOperationList select)
      (let [selects (.getSelects ^SetOperationList select)]
        (when (and (= 2 (count selects))
                   (instance? PlainSelect (first selects))
                   (instance? PlainSelect (second selects)))
          [(first selects) (second selects)])))))

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

   Returns the standard {:db :schema :name :alias :aliases} map, or nil when
   it can't apply: a non-`UNION` shape, a parameterised branch (the committed
   rule path / B2 owns params for now), or any translation/eval failure."
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
            anchor-parsed (translate-select anchor schema db)
            ;; Params are owned by the rule path (B2); bail so the caller's
            ;; rule materialisation keeps deferring them.
            anchor-params? (some params/param-ref? (:in-args anchor-parsed))]
        (when-not anchor-params?
          (let [anchor-edb  (or (:enriched-db anchor-parsed) db)
                anchor-rows (visible-query-rows anchor-parsed anchor-edb)
                anchor-oids (:select-item-oids anchor-parsed)
                ;; Column value-types: prefer sampled rows, fall back to the
                ;; anchor's inferred OIDs (SQL takes a recursive CTE's column
                ;; types from the anchor branch).
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
                mk-data-tx (fn [rows]
                             (vec (for [row rows]
                                    (assoc (into {} (keep-indexed
                                                     (fn [i c]
                                                       (let [v (nth row i nil)]
                                                         (when (and (some? v) (not= :__null__ v))
                                                           [(keyword target-name c) ((nth coercions i) v)])))
                                                     col-names))
                                           row-marker true))))
                schema-tx (recursive-schema-tx target-name col-names col-types row-marker)
                spec0 (d/db-with (d/db-with db schema-tx) (mk-data-tx anchor-rows))
                rec-parsed (binding [*cte-relations* #{(str/lower-case target-name)}]
                             (translate-select recursive (:schema spec0) spec0))]
            ;; A parameterised recursive branch also bows out to the rule path.
            (when-not (some params/param-ref? (:in-args rec-parsed))
              (loop [cur spec0, seen (set anchor-rows), i 0]
                (if (> i 100000)
                  {:db cur :schema (:schema cur) :name target-name
                   :alias target-name :aliases col-names}
                  (let [rows  (visible-query-rows rec-parsed cur)
                        novel (vec (remove seen rows))]
                    (if (empty? novel)
                      {:db cur :schema (:schema cur) :name target-name
                       :alias target-name :aliases col-names}
                      (recur (d/db-with cur (mk-data-tx novel))
                             (into seen novel) (inc i)))))))))))
    (catch Throwable _ nil)))

