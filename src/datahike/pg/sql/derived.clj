(ns datahike.pg.sql.derived
  "Prove and rewrite restrictions through inert derived-table projections.

   Only typed current databases, ordinary base relations, identity-preserving
   scalar column projections and built-in integral equality are admitted. An unproved shape keeps the normal materializer.
   The returned expression owns its nodes; the shared parser AST is read-only."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
  (:import [datahike.db DB]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression Alias BooleanValue JdbcParameter LongValue SignedExpression]
           [net.sf.jsqlparser.expression.operators.conditional AndExpression]
           [net.sf.jsqlparser.expression.operators.relational EqualsTo ParenthesedExpressionList]
           [net.sf.jsqlparser.statement.select PlainSelect ParenthesedSelect LateralSubSelect Join Select SelectItem]))

(set! *warn-on-reflection* true)

(defn- present? [value]
  (and (some? value) (not (false? value))
       (not (and (instance? java.util.Collection value) (.isEmpty ^java.util.Collection value)))
       (not (and (string? value) (empty? value)))))

(defn- select-boundary? [^Select select]
  ;; Both a PlainSelect and its ParenthesedSelect wrapper can carry barriers.
  ;; Direct calls also work in native images; a JVM test audits new getters.
  (and (empty? (.getWithItemsList select))
       (not (.isOracleSiblings select))
       (not (.isNoWait select))
       (nil? (.getForClause select))
       (empty? (.getOrderByElements select))
       (nil? (.getLimit select))
       (nil? (.getLimitBy select))
       (nil? (.getOffset select))
       (nil? (.getFetch select))
       (nil? (.getIsolation select))
       (nil? (.getForMode select))
       (nil? (.getForUpdateTable select))
       (nil? (.getWait select))
       (not (.isSkipLocked select))
       (nil? (.getPivot select))
       (nil? (.getUnPivot select))))

(defn- plain-shape? [^PlainSelect select]
  (and (select-boundary? select)
       (empty? (.getIntoTables select))
       (empty? (.getLateralViews select))
       (not (.isUsingFinal select))
       (not (.isUsingOnly select))
       (not (.isUseWithNoLog select))
       (nil? (.getIntoTempTable select))
       (nil? (.getSampleClause select))
       (nil? (.getOptimizeFor select))
       (nil? (.getTop select))
       (nil? (.getSkip select))
       (not (.getMySqlHintStraightJoin select))
       (nil? (.getFirst select))
       (nil? (.getDistinct select))
       (nil? (.getBigQuerySelectQualifier select))
       (nil? (.getHaving select))
       (nil? (.getQualify select))
       (nil? (.getGroupBy select))
       (nil? (.getOracleHierarchical select))
       (nil? (.getPreferringClause select))
       (nil? (.getOracleHint select))
       (nil? (.getForXmlPath select))
       (nil? (.getKsqlWindow select))
       (not (.isEmitChanges select))
       (empty? (.getWindowDefinitions select))
       (not (.getMySqlSqlCalcFoundRows select))
       (nil? (.getMySqlSqlCacheFlag select))
       (nil? (.getAlias select))))

(defn- unwrap [expression]
  (if (instance? ParenthesedExpressionList expression)
    (when (= 1 (.size ^ParenthesedExpressionList expression))
      (recur (.get ^ParenthesedExpressionList expression 0)))
    expression))

(defn- quote-ident ^String [^String value]
  (str "\"" (str/replace value "\"" "\"\"") "\""))

(defn- column-copy [^Column column alias]
  (doto (Column.)
    (.setTable (Table. (quote-ident alias)))
    (.setColumnName (.getColumnName column))))

(def ^:private integral-ranges
  {types/oid-int2 [-32768N 32767N]
   types/oid-int4 [-2147483648N 2147483647N]
   types/oid-int8 [-9223372036854775808N 9223372036854775807N]})

(def ^:private identity-storage
  ;; These scalar representations cross the current materializer unchanged.
  ;; SQL int2/int4 use Long storage just like int8; an OID need not equal its
  ;; storage type's default OID. Other normalization paths need their own proof.
  (merge (zipmap (keys integral-ranges) (repeat :db.type/long))
         {types/oid-text :db.type/string
          types/oid-varchar :db.type/string
          types/oid-bpchar :db.type/string
          types/oid-bool :db.type/boolean}))

(defn- inert-column? [column]
  (and (= :db.cardinality/one (:cardinality column))
       (not (:ref? column))
       (nil? (:references column))
       (contains? identity-storage (:oid column))
       (= (:valuetype column) (get identity-storage (:oid column)))))

(defn- integral-column? [column]
  (and (inert-column? column)
       (= :db.type/long (:valuetype column))
       (contains? integral-ranges (:oid column))))

(defn- source [^Table table schema db cte-names]
  (let [name (params/unquote-ident (.getName table))
        namespace (some-> (.getSchemaName table) params/unquote-ident)
        alias (if-let [alias (.getAlias table)]
                (params/unquote-ident (.getName ^Alias alias)) name)
        canonical (pgs/canonical-table (pgs/ci-index schema) name)]
    (when (and name alias (not (contains? cte-names name))
               (or (nil? namespace) (contains? #{"public" "pg_temp"} namespace))
               (not (str/starts-with? name "__"))
               (not (str/starts-with? name "pg_"))
               (not= name "information_schema")
               canonical (not (pgs/ambiguous? canonical))
               (let [{:keys [marker native]} (pgs/relation-membership-attrs schema canonical)]
                 (or marker (seq native)))
               (empty? (some-> (.getAlias table) .getAliasColumns))
               (nil? (.getPivot table)) (nil? (.getUnPivot table))
               (nil? (.getSampleClause table)) (nil? (.getIndexHint table))
               (nil? (.getSqlServerHints table))
               (not (present? (.getDatabaseName table)))
               (not (present? (.getCatalogName table)))
               ;; getName strips a DB-link suffix; inspect the raw reference.
               (not (str/includes? (.getFullyQualifiedName table) "@"))
               (not (str/includes? alias "@"))
               (not (d/q '[:find ?e . :in $ ?name :where [?e :datahike.pg/view-name ?name]]
                         db name)))
      {:alias alias :columns (pgs/column-info schema canonical db)})))

(defn- sources [^PlainSelect inner schema db cte-names]
  (let [joins (vec (or (.getJoins inner) []))
        items (cons (.getFromItem inner) (map #(.getRightItem ^Join %) joins))]
    (when (and (every? #(instance? Table %) items)
               (every? (fn [^Join join]
                         (and (or (.isSimple join) (.isCross join) (.isInner join))
                              (not (or (.isLeft join) (.isRight join) (.isFull join)
                                       (.isOuter join) (.isNatural join) (.isApply join)
                                       (.isSemi join) (.isStraight join) (.isGlobal join)
                                       (.isWindowJoin join)))
                              (nil? (.getJoinWindow join)) (nil? (.getJoinHint join))
                              (empty? (.getUsingColumns join)))) joins))
      (let [sources (mapv #(source % schema db cte-names) items)]
        (when (and (every? some? sources)
                   (= (count sources) (count (set (map :alias sources))))) sources)))))

(defn- local-column? [^Column column]
  (let [table (.getTable column)]
    (and (nil? (.getArrayConstructor column))
         (= "." (.getTableDelimiter column))
         (or (nil? table)
             (and (nil? (.getSchemaName table))
                  (not (str/includes? (.getFullyQualifiedName table) "@"))
                  (not (present? (.getDatabaseName table)))
                  (not (present? (.getCatalogName table))))))))

(defn- resolve-base [sources ^Column column]
  (let [qualifier (some-> (.getTable column) .getName params/unquote-ident)
        name (params/unquote-ident (.getColumnName column))
        matches (for [source sources
                      :when (or (nil? qualifier) (= qualifier (:alias source)))
                      info (:columns source)
                      :when (and (= name (:name info)) (not= :db/id (:attr info)))]
                  (assoc info :expression (column-copy column (:alias source))))]
    (when (and (local-column? column) (= 1 (count matches))) (first matches))))

(defn- literal [expression]
  (let [expression (unwrap expression)]
    (cond
      (instance? LongValue expression)
      {:expression (LongValue. (.getStringValue ^LongValue expression))
       :integer (bigint (.getBigIntegerValue ^LongValue expression))}
      (instance? SignedExpression expression)
      (let [sign (.getSign ^SignedExpression expression)
            inner (.getExpression ^SignedExpression expression)]
        (when (and (contains? #{\+ \-} sign) (instance? LongValue inner))
          (let [integer (bigint (.getBigIntegerValue ^LongValue inner))]
            {:expression (SignedExpression. sign (LongValue. (.getStringValue ^LongValue inner)))
             :integer (if (= sign \-) (- integer) integer)})))
      (instance? JdbcParameter expression)
      (let [^JdbcParameter parameter expression]
        {:expression (doto (JdbcParameter.)
                       (.setIndex (.getIndex parameter))
                       (.setUseFixedIndex (.isUseFixedIndex parameter))
                       (.setParameterCharacter (.getParameterCharacter parameter)))
         :parameter (.getIndex parameter)})
      :else nil)))

(defn- compatible? [oid value]
  (when-let [[low high] (get integral-ranges oid)]
    (if (contains? value :integer)
      (<= low (:integer value) high)
      (when-let [index (:parameter value)]
        (let [declared (get params/*declared-param-oids* index)]
          (or (nil? declared) (zero? declared)
              (when-let [[source-low source-high] (get integral-ranges declared)]
                (<= low source-low source-high high))))))))

(defn- rewrite-equality [^EqualsTo equality resolve-column]
  (let [left (unwrap (.getLeftExpression equality))
        right (unwrap (.getRightExpression equality))
        left-column (when (instance? Column left) (resolve-column left))
        right-column (when (instance? Column right) (resolve-column right))
        left-value (when-not left-column (literal left))
        right-value (when-not right-column (literal right))]
    (when (and (zero? (.getOldOracleJoinSyntax equality))
               (zero? (.getOraclePriorPosition equality))
               (or (and left-column right-column
                        (integral-column? left-column)
                        (integral-column? right-column)
                        (= (:oid left-column) (:oid right-column)))
                   (and (integral-column? left-column) (compatible? (:oid left-column) right-value))
                   (and (integral-column? right-column) (compatible? (:oid right-column) left-value))))
      (doto (EqualsTo.)
        (.setLeftExpression (:expression (or left-column left-value)))
        (.setRightExpression (:expression (or right-column right-value)))))))

(defn- rewrite-predicate [expression resolve-column]
  (let [expression (unwrap expression)]
    (cond
      (instance? EqualsTo expression) (rewrite-equality expression resolve-column)
      (instance? AndExpression expression)
      (when-not (.isUseOperator ^AndExpression expression)
        (let [left (rewrite-predicate (.getLeftExpression ^AndExpression expression) resolve-column)
              right (rewrite-predicate (.getRightExpression ^AndExpression expression) resolve-column)]
          (when (and left right) (AndExpression. left right))))
      (instance? BooleanValue expression) (BooleanValue. (.getValue ^BooleanValue expression))
      :else nil)))

(defn additional-where
  "Return an owned restriction for an eligible explicit derived FROM item.

   The parent WHERE remains in place. Existing child WHERE/ON and every projected
   item must be inert; only proven integral comparisons are duplicated. No rows,
   bound values, shared AST nodes or new parameter indices enter this rewrite."
  [^PlainSelect outer item schema db {:keys [cte-names] :or {cte-names #{}}}]
  (when (and (instance? DB db)
             ;; :read admits values outside the declared storage type. Temporal
             ;; and filtered views likewise need a separate identity/effect proof.
             (= :write (:schema-flexibility (dbi/-config db)))
             (instance? ParenthesedSelect item)
             (not (instance? LateralSubSelect item))
             (empty? (.getJoins outer)) (empty? (.getWithItemsList outer))
             (empty? params/*from-bindings*)
             (.getWhere outer))
    (let [^ParenthesedSelect derived item
          inner (.getSelect derived)
          alias (some-> (.getAlias derived) .getName params/unquote-ident)]
      (when (and alias (select-boundary? derived)
                 ;; A derived relation's alias list renames columns only.
                 (every? #(nil? (.-colDataType ^net.sf.jsqlparser.expression.Alias$AliasColumn %))
                         (or (some-> (.getAlias derived) .getAliasColumns) []))
                 (nil? (.getSampleClause derived))
                 (instance? PlainSelect inner) (plain-shape? inner))
        (let [^PlainSelect inner inner
              sources (sources inner schema db cte-names)]
          (when sources
            (let [resolve-column #(resolve-base sources %)
                  predicates (cons (.getWhere inner)
                                   (mapcat #(or (.getOnExpressions ^Join %) [])
                                           (or (.getJoins inner) [])))
                  projection (mapv (fn [^SelectItem item]
                                     (let [expression (unwrap (.getExpression item))]
                                       (when (instance? Column expression)
                                         (when-let [column (resolve-column expression)]
                                           (assoc column :name
                                                  (if-let [alias (.getAlias item)]
                                                    (params/unquote-ident (.getName ^Alias alias))
                                                    (params/unquote-ident (.getColumnName ^Column expression))))))))
                                   (.getSelectItems inner))
                  aliases (mapv #(params/unquote-ident (.-name ^net.sf.jsqlparser.expression.Alias$AliasColumn %))
                                (or (some-> (.getAlias derived) .getAliasColumns) []))
                  names (into aliases (drop (count aliases) (map :name projection)))]
              (when (and (seq projection) (every? inert-column? projection)
                         (<= (count aliases) (count projection))
                         (= (count names) (count (set names)))
                         (every? #(or (nil? %) (rewrite-predicate % resolve-column)) predicates))
                (let [by-name (zipmap names projection)
                      resolve-output
                      (fn [^Column column]
                        (let [qualifier (some-> (.getTable column) .getName params/unquote-ident)]
                          (when (and (local-column? column)
                                     (or (nil? qualifier) (= alias qualifier)))
                            (get by-name (params/unquote-ident (.getColumnName column))))))]
                  (rewrite-predicate (.getWhere outer) resolve-output))))))))))
