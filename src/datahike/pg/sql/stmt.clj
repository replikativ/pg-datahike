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
            [datahike.datom]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.expr :as expr]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.oid-infer :as oid]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
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
        flatten-and (fn flatten-and [e]
                      (if (instance? net.sf.jsqlparser.expression.operators.conditional.AndExpression e)
                        (let [^net.sf.jsqlparser.expression.operators.conditional.AndExpression ae e]
                          (concat (flatten-and (.getLeftExpression ae))
                                  (flatten-and (.getRightExpression ae))))
                        [e]))
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
                ;; entity-id, so we col-var! the ref to get ?ref-var and
                ;; rebind the target alias's entity-var to it. The
                ;; target's unique column then resolves via the bound
                ;; entity.
                (let [{:keys [ref-resolved ref-attr target-alias]} fk-via-ref
                      ref-var (ctx/col-var! ctx ref-resolved)]
                  (swap! (:entity-vars ctx) assoc target-alias ref-var)
                  (when (#{:left :right :full} jtype)
                    (reset! ref-info {:ref-var ref-var
                                      :ref-attr ref-attr
                                      :right-alias target-alias
                                      :left-evar (ctx/entity-var! ctx (:default-table ctx))})
                    (swap! (:left-join-evars ctx) conj ref-var)))

                ref-side
                (do
                ;; Always create the ref pattern [?left-eid :ref-attr ?ref-var]
                  (let [ref-var (ctx/col-var! ctx ref-side)
                        db-id-alias (second db-id-side)]
                  ;; Always unify entity vars (the right-table entity IS the ref value)
                    (swap! (:entity-vars ctx) assoc db-id-alias ref-var)
                  ;; For outer joins: also record ref-info for or-join wrapping
                    (when (#{:left :right :full} jtype)
                      (reset! ref-info {:ref-var ref-var
                                        :ref-attr (if (vector? ref-side) (nth ref-side 2) ref-side)
                                        :right-alias db-id-alias
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
                      ;; Determine which side is left vs right table
                        left-alias (:default-table ctx)
                        l-resolved (ctx/resolve-column ^Column left (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx))
                        r-resolved (ctx/resolve-column ^Column right (:table-aliases ctx) (:default-table ctx) (:col-overrides ctx) (:derived-aliases ctx))
                      ;; Right-side attr is the one from the right table
                        [left-key-var right-key-attr]
                        (let [l-ns (if (vector? l-resolved) (second l-resolved) (namespace l-resolved))
                              r-ns (if (vector? r-resolved) (second r-resolved) (namespace r-resolved))
                              r-attr (if (vector? r-resolved) (nth r-resolved 2) r-resolved)]
                          (if (= l-ns left-alias)
                            [l-var r-attr]
                          ;; Swapped: right col is actually from left table
                            [(expr/translate-expr ctx right) (if (vector? l-resolved) (nth l-resolved 2) l-resolved)]))]
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
                  (instance? StringValue right) (.getNotExcapedValue ^StringValue right)
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

(defn materialize-table-function
  "Produce rows for a `TableFunction` FROM item. Currently supports
   `unnest(ARRAY[…])` with optional `WITH ORDINALITY`.

   Returns {:aliases [col-names] :rows [[v1 v2 …] …] :vtypes [kw kw …]}
   or nil if the function isn't one we know how to expand.

   For `unnest(ARRAY[v1, v2, v3]) WITH ORDINALITY`:
     :aliases = [\"unnest\" \"ordinality\"]
     :rows    = [[v1 1] [v2 2] [v3 3]]
     :vtypes  = inferred per value"
  [^net.sf.jsqlparser.statement.select.TableFunction tf]
  (let [^net.sf.jsqlparser.expression.Function f (.getFunction tf)
        fname (str/lower-case (or (.getName f) ""))
        with-ord? (some-> (.getWithClause tf) str
                          (->> (= "ORDINALITY")))
        vtype-of (fn [v]
                   (cond
                     (instance? Long v)    :db.type/long
                     (instance? Double v)  :db.type/double
                     (instance? Boolean v) :db.type/boolean
                     :else                 :db.type/string))]
    (when (= fname "unnest")
      (let [params (.getParameters f)
            first-p (when (and params (pos? (count params)))
                      (.get params 0))]
        (when (instance? ArrayConstructor first-p)
          (let [vals (mapv #(extract-value %)
                           (.getExpressions ^ArrayConstructor first-p))]
            (if with-ord?
              {:aliases ["unnest" "ordinality"]
               :rows    (vec (map-indexed (fn [i v] [v (long (inc i))]) vals))
               :vtypes  [(vtype-of (first vals)) :db.type/long]}
              {:aliases ["unnest"]
               :rows    (mapv vector vals)
               :vtypes  [(vtype-of (first vals))]})))))))

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
        with-fn (requiring-resolve 'datahike.api/db-with)
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
  (let [with-fn (requiring-resolve 'datahike.api/db-with)
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
        q-fn (requiring-resolve 'datahike.api/q)
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
        ;; Walk every row rather than just the first — UNION across
        ;; tables of different shapes (or first-row-all-NULL cases) can
        ;; otherwise mis-type a column as :string when later rows have
        ;; longs.
        col-vtype (fn [col-idx]
                    (let [samples (keep (fn [row]
                                          (let [vs (if (sequential? row) (vec row) [row])
                                                v  (nth vs col-idx nil)]
                                            (when (and (some? v) (not= :__null__ v)) v)))
                                        sub-results)]
                      ;; Match the speculative-db's :db/valueType to the
                      ;; runtime types Datahike actually accepts. Hitting
                      ;; the :else string branch when samples include a
                      ;; Date / UUID / etc. makes the subsequent transact
                      ;; reject the row with a schema-mismatch error
                      ;; (datahike.db.transaction "Bad entity value …
                      ;; Must be conform to: string?"). Test trigger:
                      ;; LEFT JOIN to a derived table that projects
                      ;; `customer.created_at` (DateTime) — Metabase
                      ;; emits this from the Question Builder whenever a
                      ;; user joins through a temporal column.
                      (cond
                        (every? #(instance? Long %)             samples) :db.type/long
                        (every? #(instance? Double %)           samples) :db.type/double
                        (every? #(instance? Boolean %)          samples) :db.type/boolean
                        (every? #(instance? java.util.Date %)   samples) :db.type/instant
                        (every? #(instance? java.util.UUID %)   samples) :db.type/uuid
                        (every? #(instance? java.math.BigDecimal %) samples) :db.type/bigdec
                        (every? #(instance? java.math.BigInteger %) samples) :db.type/bigint
                        :else :db.type/string)))
        ;; Always emit a row-existence marker so `t.*` expansion in
        ;; the OUTER select has an entity anchor even when every
        ;; non-marker column is NULL on a given row (e.g. Metabase's
        ;; `NULL as role` projection in build_privilege_map).
        row-marker (pgs/row-marker-attr target-name)
        schema-tx (conj
                   (vec (for [[i a] (map-indexed vector sub-aliases)]
                          {:db/ident (keyword target-name a)
                           :db/valueType (col-vtype i)
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
                                                  [(keyword target-name a) v])))
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
        (if (and db (instance? ParenthesedSelect from-item))
          (if-let [{sub-db :db sub-schema :schema
                    sub-name :name sub-alias :alias}
                   (materialize-derived-select!
                    ^ParenthesedSelect from-item db schema)]
            [sub-db sub-schema sub-name sub-alias]
            [db schema nil nil])
          ;; Regular table
          (let [{tname :name talias :alias} (when (instance? Table from-item)
                                              (ctx/extract-table-info ^Table from-item))]
            [db schema tname talias]))
        ;; default-table is the alias key used for entity-var lookup.
        default-table (or alias name)

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
        ctx (ctx/make-ctx schema table-aliases default-table
                          {:db db
                           :parse-sql params/*parse-sql*
                           :hints (pgs/schema-hints db)
                           :derived-aliases derived-alias-set})

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

        where-expr (.getWhere select)
        _ (when where-expr
            (let [preds (expr/translate-predicate ctx where-expr)]
              (swap! (:where-clauses ctx) into preds)))

        ;; SELECT items
        select-items (.getSelectItems select)
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

        _ (doseq [^SelectItem item select-items]
            (let [expr (.getExpression item)
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
                      ranking-fns #{"row_number" "rank" "dense_rank" "ntile"
                                    "percent_rank" "cume_dist" "lag" "lead"}
                      is-window? (or (seq partition-list) (seq order-by-list)
                                     window-elem (contains? ranking-fns fname))]
                  (if is-window?
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
                          ;; Choose aggregate: COUNT→sum, others→filter-aware variant
                              filter-agg (if is-count?
                                           'sum
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
                              agg-sym (cond
                                        (and is-count-col? is-distinct?)
                                        'datahike.pg.sql/filter-count-distinct
                                        is-count-col?
                                        'datahike.pg.sql/filter-count
                                        :else agg-sym)
                              ;; Distinct aggregates (e.g. SUM(DISTINCT x)) deduplicate
                              ;; their input collection rather than doing a set scan.
                              is-dh-distinct? (= agg-sym 'count-distinct)]
                          ;; Prevent set deduplication for non-distinct aggregates:
                          ;; adding the entity var to :with preserves duplicate rows.
                          (when-not is-dh-distinct?
                            (swap! (:with-vars ctx) conj (ctx/entity-var! ctx default-table)))
                          (swap! find-elements conj (list agg-sym v))
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
        select-item-oids
        (when (and (empty? @window-specs) (empty? @compound-exprs))
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
                     select-items)
                ;; find-aliases may be longer than acc when SELECT
                ;; contains JOIN-driven entity vars added to :find
                ;; for :with semantics. Pad with nil so the vector
                ;; lines up index-for-index with find-aliases.
                n (count @find-aliases)]
            (vec (take n (concat acc (repeat nil))))))

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
        _ (when (and (not @has-aggregates?) (not has-distinct?) default-table)
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
                                        agg-syms (set (vals fns/sql-aggregate->datalog))
                                        v (if (and (seq? v)
                                                   (not (contains? agg-syms (first v))))
                                            (ctx/materialize-arg! ctx v)
                                            v)
                                        ;; Detect unsupported: aggregate in ORDER BY not in SELECT
                                        _ (when (and (map? v) (:aggregate v))
                                            (throw (ex-info (str "ORDER BY on aggregate not in SELECT list is not supported: " (str expr))
                                                            {:expr (str expr)
                                                             :sqlstate "0A000"})))]
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
                (let [{:keys [ref-var ref-attr left-evar]} ref-info
                      all-clauses @(:where-clauses ctx)
                      right-clause? (fn [clause]
                                      (and (vector? clause) (= 3 (count clause))
                                           (= ref-var (first clause))
                                           (keyword? (second clause))))
                      right-clauses (vec (filter right-clause? all-clauses))
                      other-clauses (vec (remove right-clause? all-clauses))
                      ref-binding? (fn [clause]
                                     (and (vector? clause) (= 3 (count clause))
                                          (= ref-attr (second clause))
                                          (= ref-var (nth clause 2))))
                      ;; Convert ref pattern to get-else so entities with NULL ref are included
                      left-clauses (mapv (fn [c]
                                           (if (ref-binding? c)
                                             [(list 'get-else '$ left-evar ref-attr :__null__) ref-var]
                                             c))
                                         other-clauses)
                      right-vars (vec (distinct
                                       (keep (fn [clause]
                                               (when (and (vector? clause) (= 3 (count clause)))
                                                 (nth clause 2)))
                                             right-clauses)))
                      shared-vars (vec (distinct (concat [ref-var] right-vars)))
                      ;; Matched: ref is not :__null__ → look up right entity using ref-var as eid
                      matched-non-key (mapv (fn [[_re a v]]
                                              [(list 'get-else '$ ref-var a :__null__) v])
                                            right-clauses)
                      matched (apply list 'and
                                     (into [[(list 'not= ref-var :__null__)]]
                                           matched-non-key))
                      ;; Unmatched: ref is :__null__ → all right-side vars are NULL
                      null-bindings (mapv (fn [v] [(list 'ground :__null__) v]) right-vars)
                      unmatched (apply list 'and
                                       (into [[(list '= ref-var :__null__)]]
                                             null-bindings))
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

        ;; Build the Datalog query map
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
                         order-by-spec))]
            (if has-nullable-order?
              ;; Nullable ORDER BY → server-side sort (don't emit :order-by to Datahike)
              [extended-find (count missing) nil ob]
              ;; Non-nullable → Datahike handles it
              [extended-find (count missing) ob nil]))
          ;; No explicit SQL ORDER BY: default to a deterministic order on
          ;; the primary FROM table's entity var. Entity ids are issued
          ;; monotonically by d/transact, so this matches insertion order —
          ;; the behavior every heap-scanning PG client (pgjdbc, Odoo,
          ;; Hibernate) implicitly relies on when no ORDER BY is given.
          ;; Skipped for aggregates/DISTINCT (their shape is
          ;; projection-defined, not row-defined) and for queries with no
          ;; single default table (subqueries, joins handled separately).
          (if-let [evar (and (not @has-aggregates?)
                             (not has-distinct?)
                             default-table
                             (ctx/entity-var! ctx default-table))]
            (let [already (.indexOf ^java.util.List find-elems-vec evar)
                  extended-find (if (neg? already)
                                  (conj find-elems-vec evar)
                                  find-elems-vec)
                  idx (if (neg? already) (dec (count extended-find)) already)
                  hidden (if (neg? already) 1 0)]
              [extended-find hidden [idx :asc] nil])
            [find-elems-vec 0 nil nil]))

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
          cast-cat (types/cast-category type-str)]
      (cond
        (= cast-cat :integer)   (if (integer? inner) inner (Long/parseLong (str inner)))
        (= cast-cat :float)     (if (float? inner) inner (Double/parseDouble (str inner)))
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

   When the expression is a JdbcParameter (prepared-statement placeholder),
   returns a ParamRef that the wire layer resolves at Bind time against
   the decoded client value."
  ([e] (extract-value e nil nil))
  ([e schema db]
   (cond
     (instance? JdbcParameter e)
     (->ParamRef (.getIndex ^JdbcParameter e))

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
     (instance? StringValue e) (.getNotExcapedValue ^StringValue e)
     (instance? BooleanValue e) (.getValue ^BooleanValue e)
     (instance? NullValue e) nil
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
                           (apply (requiring-resolve 'datahike.api/q) q db in-args)
                           ((requiring-resolve 'datahike.api/q) q db))
                 first-row (first results)]
             (if (sequential? first-row) (first first-row) first-row))
           nil))
       nil)
     (instance? net.sf.jsqlparser.expression.Function e)
     (let [^net.sf.jsqlparser.expression.Function f e
           fname (str/lower-case (.getName f))]
      ;; Support nextval('seq_name') in INSERT VALUES
       (if (= fname "nextval")
         (let [params (.getParameters f)
               arg (first (.getExpressions params))]
           {:fn :nextval :seq-name (extract-value arg schema db)})
         (if (= fname "now")
           (java.util.Date.)
           (str e))))
     (instance? TimezoneExpression e)
    ;; now() AT TIME ZONE 'UTC' → current timestamp
     (let [left (.getLeftExpression ^TimezoneExpression e)]
       (if (and (instance? net.sf.jsqlparser.expression.Function left)
                (= "now" (str/lower-case (.getName ^net.sf.jsqlparser.expression.Function left))))
         (java.util.Date.)
         (java.util.Date.)))  ;; any timezone expression defaults to current time
     :else (str e))))

(defn coerce-insert-value
  "Coerce a value to match the schema type for an attribute."
  [val attr schema]
  (when (some? val)
    (let [vtype (get-in schema [attr :db/valueType])]
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
        ;; BigInteger or clojure.lang.BigInt lands here when a SQL
        ;; literal overflows Long — `(- (BigInteger. "...N"))` returns
        ;; BigInt (not BigInteger), so checking both types is required.
        ;; Down-convert to the column type: float/double lose precision
        ;; but become finite (or ±Infinity) matching PG; bigdec is
        ;; exact; long truncates via Number.longValue (matching PG's
        ;; implicit overflow behaviour for bigint, though a strict
        ;; 22003 raise would be more correct).
        (or (instance? java.math.BigInteger val)
            (instance? clojure.lang.BigInt val))
        (case vtype
          :db.type/float  (float  (.doubleValue ^Number val))
          :db.type/double (double (.doubleValue ^Number val))
          :db.type/bigdec (bigdec val)
          :db.type/long   (.longValue ^Number val)
          val)
        (and (= vtype :db.type/double) (integer? val)) (double val)
        (and (= vtype :db.type/float) (integer? val)) (float val)
        (and (= vtype :db.type/long) (instance? Double val)) (long val)
        (and (= vtype :db.type/long) (string? val))
        (try (Long/parseLong val) (catch NumberFormatException _ val))
        (and (= vtype :db.type/double) (string? val))
        (try (Double/parseDouble val) (catch NumberFormatException _ val))
        ;; PG allows 'N'::real and 'N'::float4 — both land here as a
        ;; string value against a :db.type/float column. Parse via
        ;; Double/parseDouble then narrow to float; on overflow the
        ;; narrowing produces ±Infinity which matches PG's behavior.
        (and (= vtype :db.type/float) (string? val))
        (try (float (Double/parseDouble val)) (catch NumberFormatException _ val))
        (and (= vtype :db.type/boolean) (string? val))
        (Boolean/parseBoolean val)
        ;; jsonb: serialize Clojure maps/vectors to JSON strings for :db.type/string columns
        (and (= vtype :db.type/string) (or (map? val) (sequential? val)))
        (jb/serialize-jsonb val)
        (and (= vtype :db.type/string) (not (string? val))) (str val)
        ;; bytea: decode PG `\xHEX` hex literal to byte array; fall back to
        ;; raw UTF-8 bytes for non-hex strings so the value stays representable.
        (and (= vtype :db.type/bytes) (string? val))
        (or (parse-bytea-hex val) (.getBytes ^String val "UTF-8"))
        (and (= vtype :db.type/bytes) (bytes? val)) val
        ;; Timestamp coercion: parse various timestamp formats to java.util.Date
        ;; BigDecimal coercion: numeric/decimal columns
        (and (= vtype :db.type/bigdec) (string? val))
        (try (BigDecimal. ^String val) (catch NumberFormatException _ val))
        (and (= vtype :db.type/bigdec) (number? val))
        (bigdec val)
        (and (= vtype :db.type/instant) (string? val))
        (expr/parse-timestamp-string val)
        (and (= vtype :db.type/instant) (instance? java.util.Date val)) val
        :else val))))

(declare eval-update-expr)

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
      :else
      ;; Fallback — the operand may itself evaluate to a truthy value
      ;; (e.g. boolean column `CHECK (active)`). Anything not-nil and
      ;; not false counts as satisfied.
      (let [v (operand expr)]
        (cond (nil? v) nil
              (false? v) false
              :else true)))))

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
    (.getNotExcapedValue ^StringValue value-expr)

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

    ;; Function call — currently only now()/current_timestamp → current Date.
    (instance? net.sf.jsqlparser.expression.Function value-expr)
    (let [fname (str/lower-case (.getName ^net.sf.jsqlparser.expression.Function value-expr))]
      (case fname
        ("now" "current_timestamp" "localtimestamp") (java.util.Date.)
        ("current_date") (java.util.Date.)
        (str value-expr)))

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

(defn translate-insert
  "Translate an INSERT statement to Datahike transaction data.
   Supports single-row and multi-row VALUES, with or without column list.
   Handles ON CONFLICT (UPSERT) via :db.fn/call for atomic execution."
  [^Insert insert schema db]
  (let [table (.getTable insert)
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
            q-fn (requiring-resolve 'datahike.api/q)
            inner-results (if (seq inner-in-args)
                            (apply q-fn inner-query db inner-in-args)
                            (q-fn inner-query db))
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
                         (let [q (requiring-resolve 'datahike.api/q)]
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
                           (ffirst ((requiring-resolve 'datahike.api/q)
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
                       (let [q-fn (requiring-resolve 'datahike.api/q)
                             seq-prefix (str table-name "_")
                             seq-results (q-fn '{:find [?name] :where [[?e :__seq__/name ?name]]
                                                 :in [$ ?prefix]}
                                               txdb seq-prefix)
                             identity-cols
                             (vec (keep (fn [[sname]]
                                          (when (and (str/starts-with? sname seq-prefix)
                                                     (str/ends-with? sname "_seq"))
                                            (let [col-name (subs sname (count seq-prefix)
                                                                 (- (count sname) 4))
                                                  attr (keyword table-name col-name)]
                                              (when (get (:schema txdb) attr)
                                                {:col col-name :attr attr :seq-name sname}))))
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
                                                               old-datoms ((requiring-resolve 'datahike.api/datoms) txdb :eavt existing)
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
                [[:db.fn/call
                  (fn [txdb]
                    (let [schema (:schema txdb)
                          q-fn (requiring-resolve 'datahike.api/q)
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
                                   (throw (ex-info
                                           (format "duplicate key value violates unique constraint \"%s\""
                                                   constraint)
                                           {:sqlstate "23505"
                                            :table table-name
                                            :column (name attr)
                                            :constraint constraint
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
                      []))]]
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
    ;; Datahike's recursive rule evaluator can't handle get-else clauses
    ;; (causes infinite loops). Convert them back to plain data patterns or
    ;; missing? checks. NULL synthesis isn't needed inside rule bodies
    ;; because rule outputs are bound directly by renaming find vars.
    ;; Also drop row-marker anchors (db-row-exists) which cause infinite loops
    ;; in recursive rule evaluation — the data patterns suffice as anchors.
    (let [;; Apply var renaming so find-vars become rule output vars
          renamed-clauses (mapv rename-form rewritten-clauses)
          ;; Drop row-marker patterns: [?e :ns/db-row-exists true]
          marker-free-clauses (filterv (fn [c]
                                         (not (and (vector? c) (= 3 (count c))
                                                   (keyword? (second c))
                                                   (= "db-row-exists" (name (second c))))))
                                       renamed-clauses)
          final-clauses (into (vec marker-free-clauses) bind-clauses)
          ;; First pass: identify (get-else ?e :attr :__null__) ?v patterns
          ;; and check if ?v is used in [(= ?v :__null__)] check.
          getelse-vars (into {}
                             (keep (fn [c]
                                     (when (and (vector? c) (= 2 (count c))
                                                (seq? (first c))
                                                (= 'get-else (ffirst c)))
                                       (let [[_ _ evar attr _default] (first c)
                                             val-var (second c)]
                                         [val-var {:evar evar :attr attr}])))
                                   final-clauses))
          null-check-vars (into #{}
                                (keep (fn [c]
                                        (when (and (vector? c) (= 1 (count c))
                                                   (seq? (first c))
                                                   (= '= (ffirst c))
                                                   (= :__null__ (last (first c))))
                                      ;; [(= ?v :__null__)] → ?v
                                          (second (first c))))
                                      final-clauses))
          rule-bound-vars (set rule-vars)
          transformed-clauses
          (vec (keep
                (fn [c]
                  (cond
                     ;; get-else clause
                    (and (vector? c) (= 2 (count c))
                         (seq? (first c))
                         (= 'get-else (ffirst c)))
                    (let [[_ _ evar attr _default] (first c)
                          val-var (second c)]
                      (cond
                         ;; If used in NULL check → use missing? + drop val-var
                        (contains? null-check-vars val-var)
                        [(list 'missing? '$ evar attr)]
                         ;; Otherwise → plain data pattern
                        :else
                        [evar attr val-var]))
                     ;; NULL check that we converted to missing? — drop it
                    (and (vector? c) (= 1 (count c))
                         (seq? (first c))
                         (= '= (ffirst c))
                         (= :__null__ (last (first c)))
                         (contains? getelse-vars (second (first c))))
                    nil
                    :else c))
                final-clauses))]
      {:clauses transformed-clauses
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
     :in-args all-in-args}))

