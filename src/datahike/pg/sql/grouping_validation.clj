(ns datahike.pg.sql.grouping-validation
  "Reject grouping operations whose semantics the executor does not implement.
   Traversal follows typed forward children and JSqlParser's expression visitor;
   it neither follows parser parent links nor changes cached ASTs."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors])
  (:import [java.util IdentityHashMap]
           [java.util.function Consumer]
           [datahike.pg GroupingExpressionVisitor]
           [net.sf.jsqlparser.expression Expression ExpressionVisitorAdapter Function
            AnalyticExpression WindowDefinition WindowElement WindowOffset
            JsonFunction JsonAggregateFunction JsonKeyValuePair JsonFunctionExpression
            Function$HavingClause KeepExpression RowConstructor]
           [net.sf.jsqlparser.expression.operators.relational ExpressionList]
           [net.sf.jsqlparser.statement ExplainStatement]
           [net.sf.jsqlparser.statement.select Select PlainSelect ParenthesedSelect
            SetOperationList Values WithItem SelectVisitorAdapter SelectItem
            ParenthesedFromItem TableFunction Join OrderByElement GroupByElement Limit]
           [net.sf.jsqlparser.statement.insert Insert ParenthesedInsert]
           [net.sf.jsqlparser.statement.update Update ParenthesedUpdate UpdateSet]
           [net.sf.jsqlparser.statement.delete Delete ParenthesedDelete]
           [net.sf.jsqlparser.statement.create.table CreateTable]
           [net.sf.jsqlparser.statement.create.view CreateView]))

(set! *warn-on-reflection* true)

(defn- unsupported! [feature]
  (throw (errors/pg-error :feature-not-supported
                          {:message (str feature " is not supported")})))

(defn- bare-function-name [^Function f]
  ;; Quotes and qualification distinguish ordinary user functions from SQL
  ;; grouping syntax. Do not unquote or strip schema components here.
  (when (= 1 (count (.getMultipartName f)))
    (str/lower-case (.getName f))))

(defn- validate-group-item! [item]
  (cond
    (instance? Function item)
    (when-let [feature (get {"rollup" "ROLLUP" "cube" "CUBE"}
                            (bare-function-name item))]
      (unsupported! feature))

    ;; An explicit ROW(...) is one record-valued expression, even ROW().
    ;; Parenthesized grouping lists have different syntax and expansion rules.
    (instance? RowConstructor item) nil

    (instance? ExpressionList item)
    (if (empty? item)
      (unsupported! "GROUP BY ()")
      (doseq [child item] (validate-group-item! child)))))

(defn- validate-group! [^GroupByElement group]
  (when group
    (when (seq (.getGroupingSets group)) (unsupported! "GROUPING SETS"))
    (when (.isMysqlWithRollup group) (unsupported! "WITH ROLLUP"))
    (when (empty? (.getGroupByExpressions group)) (unsupported! "GROUP BY ()"))
    (doseq [item (.getGroupByExpressions group)] (validate-group-item! item))))

(defn validate!
  "Validate all represented SELECT scopes before any source materialization.
   Non-query statements have no grouping work to validate."
  [statement]
  (let [seen (IdentityHashMap.)
        expression-seen (IdentityHashMap.)
        expression-visitor (volatile! nil)]
    (letfn [(expression! [^Expression expression]
              (when expression
                ;; Select implements Expression too. Its own visited set must
                ;; see it before any expression-level identity suppression.
                (if (instance? Select expression)
                  (node! expression)
                  (when-not (.containsKey expression-seen expression)
                    (.put expression-seen expression true)
                    (cond
                      ;; The stock5.2 visitor omits named args and several
                      ;; aggregate/window/JSON fields. Enumerate these forward
                      ;; children rather than relying on its partial traversal.
                      (instance? Function expression)
                      (let [^Function f expression]
                        (when (= "grouping" (bare-function-name f)) (unsupported! "GROUPING"))
                        (expression! (.getParameters f))
                        (expression! (.getNamedParameters f))
                        (expression! (.getKeep f))
                        (expression! (.getHavingClause f))
                        (orders! (.getOrderByElements f))
                        (limit! (.getLimit f))
                        (when (instance? Expression (.getAttribute f))
                          (expression! (.getAttribute f))))

                      (instance? AnalyticExpression expression)
                      (let [^AnalyticExpression f expression]
                        (when (= "grouping" (some-> (.getName f) str/lower-case)) (unsupported! "GROUPING"))
                        (doseq [child [(.getExpression f) (.getOffset f) (.getDefaultValue f)
                                       (.getKeep f) (.getHavingClause f) (.getFilterExpression f)
                                       (.getPartitionExpressionList f)]]
                          (expression! child))
                        (orders! (.getOrderByElements f))
                        (orders! (.getFuncOrderBy f))
                        (frame! (.getWindowElement f))
                        (when-let [window (.getWindowDefinition f)] (window! window))
                        (limit! (.getLimit f)))

                      (instance? Function$HavingClause expression)
                      (expression! (.getExpression ^Function$HavingClause expression))

                      (instance? KeepExpression expression)
                      (orders! (.getOrderByElements ^KeepExpression expression))

                      (instance? JsonFunction expression)
                      (let [^JsonFunction f expression]
                        (doseq [^JsonKeyValuePair pair (.getKeyValuePairs f)
                                child [(.getKey pair) (.getValue pair)]
                                :when (instance? Expression child)]
                          (expression! child))
                        (doseq [^JsonFunctionExpression child (.getExpressions f)]
                          (expression! (.getExpression child))))

                      (instance? JsonAggregateFunction expression)
                      (let [^JsonAggregateFunction f expression]
                        (doseq [child [(.getExpression f) (.getKey f) (.getValue f)
                                       (.getFilterExpression f) (.getPartitionExpressionList f)]
                                :when (instance? Expression child)]
                          (expression! child))
                        (orders! (.getExpressionOrderByElements f))
                        (orders! (.getOrderByElements f))
                        (frame! (.getWindowElement f)))

                      :else
                      (.accept expression ^ExpressionVisitorAdapter @expression-visitor nil))))))
            (items! [items]
              (doseq [^SelectItem item items] (expression! (.getExpression item))))
            (orders! [items]
              (doseq [^OrderByElement item items] (expression! (.getExpression item))))
            (withs! [items]
              (doseq [^WithItem item items] (node! (.getParenthesedStatement item))))
            (joins! [joins]
              (doseq [^Join join joins]
                (node! (.getRightItem join))
                (doseq [on (.getOnExpressions join)] (expression! on))))
            (updates! [updates]
              (doseq [^UpdateSet update updates] (expression! (.getValues update))))
            (limit! [^Limit limit]
              (when limit
                (expression! (.getRowCount limit))
                (expression! (.getOffset limit))
                (expression! (.getByExpressions limit))))
            (frame! [^WindowElement element]
              (when element
                (doseq [^WindowOffset offset (if-let [r (.getRange element)]
                                               [(.getStart r) (.getEnd r)]
                                               [(.getOffset element)])]
                  (when offset (expression! (.getExpression offset))))))
            (window! [^WindowDefinition window]
              (expression! (.getPartitionExpressionList window))
              (orders! (.getOrderByElements window))
              (frame! (.getWindowElement window)))
            (node! [node]
              (when (and node (not (.containsKey seen node)))
                (.put seen node true)
                (when (instance? Select node)
                  (let [^Select select node]
                    (withs! (.getWithItemsList select))
                    (orders! (.getOrderByElements select))
                    (limit! (.getLimit select))
                    (when-let [offset (.getOffset select)] (expression! (.getOffset offset)))
                    (when-let [fetch (.getFetch select)] (expression! (.getExpression fetch)))))
                (cond
                  (instance? PlainSelect node)
                  (let [^PlainSelect select node]
                    (validate-group! (.getGroupBy select))
                    (items! (.getSelectItems select))
                    (node! (.getFromItem select))
                    (joins! (.getJoins select))
                    (doseq [e [(.getWhere select) (.getHaving select) (.getQualify select)]]
                      (expression! e))
                    (when-let [distinct (.getDistinct select)] (items! (.getOnSelectItems distinct)))
                    (when-let [group (.getGroupBy select)]
                      (doseq [e (.getGroupByExpressions group)] (expression! e)))
                    (doseq [window (.getWindowDefinitions select)] (window! window)))

                  (instance? ParenthesedSelect node) (node! (.getSelect ^ParenthesedSelect node))
                  (instance? SetOperationList node) (doseq [s (.getSelects ^SetOperationList node)] (node! s))
                  (instance? Values node) (expression! (.getExpressions ^Values node))
                  (instance? WithItem node) (node! (.getParenthesedStatement ^WithItem node))
                  (instance? ParenthesedFromItem node)
                  (do (node! (.getFromItem ^ParenthesedFromItem node))
                      (joins! (.getJoins ^ParenthesedFromItem node)))
                  (instance? TableFunction node) (expression! (.getFunction ^TableFunction node))
                  (instance? ParenthesedInsert node) (node! (.getInsert ^ParenthesedInsert node))
                  (instance? ParenthesedUpdate node) (node! (.getUpdate ^ParenthesedUpdate node))
                  (instance? ParenthesedDelete node) (node! (.getDelete ^ParenthesedDelete node))
                  (instance? Insert node)
                  (let [^Insert insert node]
                    (withs! (.getWithItemsList insert))
                    (node! (.getSelect insert))
                    (updates! (.getSetUpdateSets insert))
                    (updates! (.getDuplicateUpdateSets insert))
                    (when-let [target (.getConflictTarget insert)]
                      (expression! (.getIndexExpression target))
                      (expression! (.getWhereExpression target)))
                    (when-let [action (.getConflictAction insert)]
                      (updates! (.getUpdateSets action))
                      (expression! (.getWhereExpression action)))
                    (items! (.getReturningClause insert)))
                  (instance? Update node)
                  (let [^Update update node]
                    (withs! (.getWithItemsList update))
                    (updates! (.getUpdateSets update))
                    (expression! (.getWhere update))
                    (node! (.getFromItem update))
                    (node! (.getSelect update))
                    (joins! (.getJoins update))
                    (joins! (.getStartJoins update))
                    (orders! (.getOrderByElements update))
                    (items! (.getReturningClause update)))
                  (instance? Delete node)
                  (let [^Delete delete node]
                    (withs! (.getWithItemsList delete))
                    (expression! (.getWhere delete))
                    (joins! (.getJoins delete))
                    (orders! (.getOrderByElements delete))
                    (items! (.getReturningClause delete)))
                  (instance? CreateTable node) (node! (.getSelect ^CreateTable node))
                  (instance? CreateView node) (node! (.getSelect ^CreateView node))
                  (instance? ExplainStatement node) (node! (.getStatement ^ExplainStatement node)))))]
      (vreset! expression-visitor
               (GroupingExpressionVisitor.
                (reify Consumer
                  (accept [_ expression] (expression! expression)))))
      (.setSelectVisitor ^ExpressionVisitorAdapter @expression-visitor
                         (proxy [SelectVisitorAdapter] []
                           (visit [select context] (node! select) nil)))
      (node! statement)))
  nil)
