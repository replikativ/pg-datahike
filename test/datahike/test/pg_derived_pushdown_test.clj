(ns datahike.test.pg-derived-pushdown-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.query :as query]
            [datahike.pg.server :as pg]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.derived :as derived]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.types :as types])
  (:import [datahike.pg PgWireServer$QueryResult]
           [java.sql Connection DriverManager]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.statement.select Select PlainSelect ParenthesedSelect SampleClause Limit Offset OrderByElement Pivot UnPivot]
           [net.sf.jsqlparser.expression ArrayConstructor Parenthesis]
           [net.sf.jsqlparser.schema Column]
           [net.sf.jsqlparser.expression.operators.conditional AndExpression]
           [org.postgresql PGStatement]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)
(def ^:dynamic *port* nil)

(defn- result [text] (.execute *handler* text))
(defn- rows [text]
  (let [^PgWireServer$QueryResult r (result text)]
    (when (.-error r) (throw (ex-info (.-error r) {:sqlstate (.-sqlstate r)})))
    (mapv vec (.-rows r))))

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          handler (pg/make-query-handler conn)
          {:keys [server]} (pg/start-server {"pushdown" conn} {:port 0})]
      (try
        (binding [*conn* conn *handler* handler *port* (.getPort server)]
          (result "CREATE TABLE push_l(id int,payload int)")
          (result "CREATE TABLE push_r(id int)")
          (result "INSERT INTO push_l VALUES(1,10),(2,20),(2,NULL),(NULL,40)")
          (result "INSERT INTO push_r VALUES(10),(20),(30),(40),(50)")
          (result "CREATE VIEW push_view AS SELECT id FROM push_l")
          (d/transact conn [{:db/ident :push_native/id :db/valueType :db.type/long
                             :db/cardinality :db.cardinality/one}
                            {:db/ident :push_native/payload :db/valueType :db.type/long
                             :db/cardinality :db.cardinality/one}
                            {:push_native/id 2 :push_native/payload 20}
                            {:push_native/id 2 :push_native/payload 30}])
          (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :once fixture)

(defn- ast [text] (.getPlainSelect ^Select (CCJSqlParserUtil/parse text)))
(defn- candidate [text]
  (let [outer (ast text) db (d/db *conn*)]
    (derived/additional-where outer (.getFromItem outer) (:schema db) db {})))

(def product "SELECT a.id AS x,a.payload AS y FROM push_l a,push_r b")

(deftest selection-crosses-only-inert-projection
  (doseq [legacy? [false true]]
    (binding [query/*disable-planner* legacy?]
      (is (= {["2" "20"] 5 ["2" nil] 5}
             (frequencies (rows (str "SELECT x,y FROM (" product ") t WHERE x=2")))))
      (is (= [["10"]]
             (rows (str "SELECT count(*) FROM (" product ") t WHERE x=2"))))
      (is (= {["2" nil "5"] 1 ["2" "20" "5"] 1}
             (frequencies
              (rows (str "SELECT x,y,count(*) FROM (" product ") t WHERE x=2 "
                         "GROUP BY x,y")))))
      (is (= [["1" "10"]]
             (rows "SELECT x,y FROM (SELECT id x,payload y FROM push_l WHERE id=1) t WHERE x=1")))
      (is (= [["2" "20"]]
             (rows "SELECT x,y FROM (SELECT a.id x,b.id y FROM push_l a JOIN push_r b ON a.payload=b.id) t WHERE x=2")))
      (is (= [["10"]]
             (rows "SELECT count(*) FROM (SELECT a.id x,b.id y FROM push_native a,push_r b) t WHERE x=2"))))))

(deftest child-filter-is-present-before-the-materializer-runs
  (let [original d/q
        child-queries (atom [])
        restricted? (fn [q]
                      (some #(and (vector? %) (= :push_l/id (second %))
                                  (or (= 2 (nth % 2 nil))
                                      (contains? (set (:in q)) (nth % 2 nil))))
                            (tree-seq coll? seq q)))]
    (with-redefs [d/q (fn [q & args]
                        (when (and (map? q)
                                   (some #{:push_l/payload} (tree-seq coll? seq q))
                                   (not-any? #(and (keyword? %) (= "__sub__t" (namespace %)))
                                             (tree-seq coll? seq q)))
                          (swap! child-queries conj q)
                          (when-not (restricted? q)
                            (throw (ex-info "unfiltered child materialization" {}))))
                        (apply original q args))]
      (is (= [["10"]]
             (rows (str "SELECT count(*) FROM (" product ") t WHERE x=2")))))
    (is (seq @child-queries))
    (is (every? restricted? @child-queries))))

(deftest alias-binding-and-owned-expression-nodes
  (is (= "\"a\".id = 2" (str (candidate (str "SELECT z FROM (" product ") t(z) WHERE z=2")))))
  (is (= "\"a\".id = 2" (str (candidate (str "SELECT t.x FROM (" product ") t WHERE t.x=2")))))
  (is (= "\"a\".id = 2"
         (str (candidate (str "SELECT x,y FROM (" product ") t WHERE x=2 GROUP BY GROUPING SETS(x,y)")))))
  (is (nil? (candidate (str "SELECT x FROM (" product ") t(z) WHERE x=2"))))
  (is (= "\"a\".id = 2"
         (str (candidate "SELECT \"X\" FROM (SELECT a.id AS \"X\" FROM push_l a) t WHERE \"X\"=2"))))
  (is (nil? (candidate "SELECT x FROM (SELECT a.id AS \"X\" FROM push_l a) t WHERE x=2")))
  (let [outer (ast (str "SELECT x FROM (" product ") t WHERE x=$2 AND y=$1"))
        original (str outer)
        db (d/db *conn*)]
    (dotimes [_ 2]
      (let [rewritten (derived/additional-where outer (.getFromItem outer) (:schema db) db {})]
        (is (some? rewritten))
        (is (= #{1 2} (params/ast-param-indices rewritten)))))
    (let [rewrites (mapv deref
                         (repeatedly 4 #(future (derived/additional-where
                                                 outer (.getFromItem outer) (:schema db) db {}))))]
      (is (every? some? rewrites))
      (is (apply = (map str rewrites))))
    (is (= original (str outer)))))

(deftest barriers-retain-the-original-query-boundary
  (doseq [inner ["SELECT id x FROM push_l LIMIT 1"
                 "SELECT id x FROM push_l OFFSET 0"
                 "SELECT id x FROM push_l FETCH FIRST 1 ROW ONLY"
                 "SELECT DISTINCT id x FROM push_l"
                 "SELECT DISTINCT ON(id) id x FROM push_l ORDER BY id"
                 "SELECT id x FROM push_l ORDER BY id"
                 "SELECT count(*) x FROM push_l"
                 "SELECT id x FROM push_l GROUP BY id"
                 "SELECT id x FROM push_l GROUP BY GROUPING SETS(id)"
                 "SELECT id x FROM push_l HAVING true"
                 "SELECT id x,row_number() OVER() r FROM push_l"
                 "SELECT id x,random() r FROM push_l"
                 "SELECT id x,generate_series(1,2) r FROM push_l"
                 "SELECT id::bigint x FROM push_l"
                 "SELECT id x FROM push_l WHERE random()>0"
                 "SELECT a.id x FROM push_l a LEFT JOIN push_r b ON a.id=b.id"
                 "SELECT id x FROM push_l UNION ALL SELECT id FROM push_r"
                 "SELECT id x FROM (SELECT id FROM push_l) nested"
                 "SELECT * FROM push_l"
                 "SELECT id x FROM push_l FOR UPDATE"]]
    (is (nil? (candidate (str "SELECT x FROM (" inner ") t WHERE x=2"))) inner))
  (doseq [predicate ["x=2147483648" "x::bigint=2" "x=2 OR x=1" "x=2 AND random()>0" "x=(SELECT 2)"]]
    (is (nil? (candidate (str "SELECT x FROM (SELECT id x FROM push_l) t WHERE " predicate))) predicate))
  (is (nil? (candidate "SELECT t.x FROM (SELECT id x FROM push_l) t JOIN push_r r ON true WHERE t.x=2")))
  (is (nil? (candidate "SELECT x FROM (SELECT 1 x,2 x) t WHERE x=2")))
  (is (nil? (candidate "SELECT x FROM (SELECT id x FROM push_view) t WHERE x=2")))
  (binding [params/*from-bindings* {"outer.x" 1}]
    (is (nil? (candidate (str "SELECT x FROM (" product ") t WHERE x=2")))))
  (binding [params/*declared-param-oids* {1 types/oid-int8}]
    (is (nil? (candidate (str "SELECT x FROM (" product ") t WHERE x=$1")))))
  (binding [params/*declared-param-oids* {1 types/oid-int4}]
    (is (some? (candidate "SELECT x FROM (SELECT id x FROM push_native) t WHERE x=$1"))))
  (is (= [] (rows "SELECT x FROM (SELECT id x FROM push_l ORDER BY id LIMIT 1) t WHERE x=2")))
  (is (= [["2"] ["2"]]
         (rows "SELECT x FROM (SELECT id x FROM push_l OFFSET 1) t WHERE x=2 ORDER BY x"))))

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/pushdown?user=x&sslmode=disable&binaryTransfer=false")))

(defn- prepared-rows [^Connection connection text values]
  (with-open [statement (.prepareStatement connection text)]
    (doseq [[i value] (map-indexed vector values)]
      (.setObject statement (int (inc i)) value))
    (with-open [rs (.executeQuery statement)]
      (loop [result []]
        (if (.next rs)
          (recur (conj result [(.getString rs 1)])) result)))))

(deftest parent-parameters-materialize-at-execute-and-reuse-the-shared-ast
  (let [text (str "SELECT count(*) FROM (" product ") t WHERE x=?")]
    (with-open [connection (jdbc)
                statement (.prepareStatement connection text)]
      (.setPrepareThreshold ^PGStatement statement 1)
      (doseq [[value expected] [[(int 2) "10"] [(int 1) "5"] [(int 99) "0"]
                                [(int 2) "10"] [nil "0"] [(int 2) "10"]]]
        (.setObject statement 1 value)
        (with-open [rs (.executeQuery statement)]
          (is (.next rs))
          (is (= expected (.getString rs 1)))
          (is (not (.next rs)))))
      (is (= [["5"]]
             (prepared-rows connection
                            (str "SELECT count(*) FROM (" product ") t WHERE x=? AND y=?")
                            [(int 2) (int 20)])))))
  (let [parsed (sql/parse-sql (str "SELECT x FROM (" product ") t WHERE x=$1")
                              (:schema (d/db *conn*)) (d/db *conn*))]
    (is (:materialisation-params? parsed))
    (is (:execution-materialization? parsed))))

(deftest repeated-simple-literals-and-data-changes-refresh-the-child
  (let [text #(str "SELECT count(*) FROM (" product ") t WHERE x=" %)]
    (doseq [[value expected] [[2 "10"] [1 "5"] [99 "0"] [2 "10"]]]
      (is (= [[expected]] (rows (text value)))))
    (try
      (result "INSERT INTO push_l VALUES(2,99)")
      (is (= [["15"]] (rows (text 2))))
      (is (= [["5"]] (rows (text 1))))
      (finally (result "DELETE FROM push_l WHERE payload=99")))
    (is (= [["10"]] (rows (text 2))))))

(deftest selective-product-materializes-twenty-rather-than-four-hundred-rows
  (result "CREATE TABLE push_big_l(id int)")
  (result "CREATE TABLE push_big_r(id int)")
  (let [values (clojure.string/join "," (map #(str "(" % ")") (range 1 21)))]
    (result (str "INSERT INTO push_big_l VALUES" values))
    (result (str "INSERT INTO push_big_r VALUES" values)))
  (let [original d/q outputs (atom [])]
    (with-redefs [d/q (fn [q & args]
                        (let [child? (and (map? q)
                                          (some #{:push_big_l/id} (tree-seq coll? seq q))
                                          (some #{:push_big_r/id} (tree-seq coll? seq q)))
                              result (apply original q args)]
                          (when child?
                            (swap! outputs conj (count result))
                            (when (> (count result) 20)
                              (throw (ex-info "unrestricted product materialized" {}))))
                          result))]
      (is (= [["20"]]
             (rows (str "SELECT count(*) FROM (SELECT a.id x,b.id y "
                        "FROM push_big_l a,push_big_r b) t WHERE x=3")))))
    (is (seq @outputs))
    (is (some #{20} @outputs))
    (is (every? #(<= % 20) @outputs))))

(deftest parser-feature-inventory-requires-a-new-safety-audit-on-upgrade
  ;; Production checks are explicit, including in native images. This JVM-only
  ;; inventory detects parser decorations that those checks have not reviewed.
  (let [getters (into #{}
                      (for [^java.lang.reflect.Method method (.getMethods PlainSelect)
                            :when (and (zero? (alength (.getParameterTypes method)))
                                       (contains? #{PlainSelect Select} (.getDeclaringClass method))
                                       (or (.startsWith (.getName method) "get")
                                           (.startsWith (.getName method) "is")))]
                        (.getName method)))]
    (is (= #{"isUseBrackets" "getFromItem" "getIntoTables" "getSelectItems" "getWhere"
             "getLateralViews" "getJoins" "isUsingFinal" "isUsingOnly" "isUseWithNoLog"
             "getIntoTempTable" "getSampleClause" "getOptimizeFor" "getTop" "getSkip"
             "getMySqlHintStraightJoin" "getFirst" "getDistinct" "getBigQuerySelectQualifier"
             "getHaving" "getQualify" "getGroupBy" "getOracleHierarchical" "getPreferringClause"
             "getOracleHint" "getForXmlPath" "getKsqlWindow" "isEmitChanges" "getWindowDefinitions"
             "getMySqlSqlCalcFoundRows" "getMySqlSqlCacheFlag" "getWithItemsList"
             "isOracleSiblings" "isNoWait" "getForClause" "getOrderByElements" "getLimit"
             "getLimitBy" "getOffset" "getFetch" "getIsolation" "getForMode" "getForUpdateTable"
             "getWait" "isSkipLocked" "getAlias" "getPivot" "getUnPivot" "getSelectBody"
             "getValues" "getPlainSelect" "getSetOperationList"}
           getters))))

(deftest owned-ast-decorations-must-not-be-erased-by-rewriting
  (let [db (d/db *conn*)
        text (str "SELECT x FROM (" product ") t WHERE x=2")
        rewrite (fn [outer]
                  (derived/additional-where outer (.getFromItem outer) (:schema db) db {}))]
    (let [outer (ast text)
          alias (.getAlias ^ParenthesedSelect (.getFromItem outer))]
      (.setAliasColumns alias
                        [(net.sf.jsqlparser.expression.Alias$AliasColumn.
                          "x" (net.sf.jsqlparser.statement.create.table.ColDataType.))])
      (is (nil? (rewrite outer)) "derived alias columns may rename but not declare types"))
    (doseq [[feature decorate]
            [[:wrapper-limit #(.setLimit ^ParenthesedSelect % (Limit.))]
             [:wrapper-offset #(.setOffset ^ParenthesedSelect % (Offset.))]
             [:wrapper-order #(.setOrderByElements ^ParenthesedSelect % [(OrderByElement.)])]
             [:wrapper-sample #(.setSampleClause ^ParenthesedSelect % (SampleClause.))]
             [:wrapper-pivot #(.setPivot ^ParenthesedSelect % (Pivot.))]
             [:wrapper-unpivot #(.setUnPivot ^ParenthesedSelect % (UnPivot.))]]]
      (let [outer (ast text)]
        (decorate (.getFromItem outer))
        (is (nil? (rewrite outer)) (name feature))))
    (doseq [[feature decorate]
            [[:column-array #(.setArrayConstructor ^Column % (ArrayConstructor. (into-array net.sf.jsqlparser.expression.Expression [])))]
             [:column-delimiter #(.setTableDelimiter ^Column % ":")]]]
      (let [outer (ast text)
            inner (.getSelect ^ParenthesedSelect (.getFromItem outer))
            column (.getExpression (.getSelectItem ^PlainSelect inner 0))]
        (decorate column)
        (is (nil? (rewrite outer)) (name feature))))
    (let [outer (ast text)
          inner (.getSelect ^ParenthesedSelect (.getFromItem outer))]
      (.setName ^net.sf.jsqlparser.schema.Table (.getFromItem ^PlainSelect inner) "push_l@remote")
      (is (nil? (rewrite outer)) "getName must not hide a database-link suffix"))
    (let [outer (ast text)
          original (.getWhere outer)
          list (doto (Parenthesis.) (.add original) (.add original))]
      (.setWhere outer list)
      (is (nil? (rewrite outer)) "a multi-expression list is not transparent parentheses"))
    (let [outer (ast (str "SELECT x FROM (" product ") t WHERE x=2 AND y=20"))]
      (.setUseOperator ^AndExpression (.getWhere outer) true)
      (is (nil? (rewrite outer)) "the alternate && operator is not boolean AND"))
    (is (some? (candidate "SELECT x FROM (SELECT (a.id) x FROM push_l a) t WHERE (x=2)")))))

(deftest volatile-ineligible-child-is-evaluated-once-after-template-fallback
  (let [calls (atom 0)
        simple "SELECT v,n FROM (SELECT random() v,1 n FROM push_l WHERE id=1) t WHERE n=1"]
    (is (nil? (candidate simple)))
    (with-redefs [fns/sql-random (fn [] (swap! calls inc) 0.125)]
      (dotimes [_ 2]
        (is (= [["0.125" "1"]] (rows simple))))
      (is (= 2 @calls) "the unbound numeric template never evaluates the volatile target"))))

(deftest additional-parser-decoration-inventories-require-a-safety-audit
  (doseq [[class expected]
          [[ParenthesedSelect #{"getAlias" "getPivot" "getUnPivot" "getSampleClause" "getSelect"
                                "getValues" "getPlainSelect" "getSetOperationList"}]
           [Column #{"getArrayConstructor" "getTable" "getTableName" "getUnquotedTableName"
                     "getSchemaName" "getUnquotedSchemaName" "getCatalogName" "getUnquotedCatalogName"
                     "getColumnName" "getUnquotedColumnName" "getTableDelimiter" "getFullyQualifiedName"
                     "getUnquotedName" "getCommentText"}]
           [AndExpression #{"isUseOperator" "getStringExpression"}]
           [Parenthesis #{"getExpression"}]]]
    (is (= expected
           (into #{}
                 (for [^java.lang.reflect.Method method (.getMethods ^Class class)
                       :when (and (= class (.getDeclaringClass method))
                                  (zero? (alength (.getParameterTypes method)))
                                  (or (.startsWith (.getName method) "get")
                                      (.startsWith (.getName method) "is")))]
                   (.getName method))))
        (.getName ^Class class))))

(defn- outcome [text]
  (try {:rows (rows text)}
       (catch clojure.lang.ExceptionInfo error {:sqlstate (:sqlstate (ex-data error))})))

(deftest storage-representation-is-proved-separately-from-the-advertised-oid
  (d/transact *conn*
              [{:db/ident :push_string_hint/value :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one :pg/type "int8"}
               {:push_string_hint/value "01"} {:push_string_hint/value "1"}
               {:db/ident :push_coerce/id :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one}
               {:db/ident :push_coerce/payload :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one :pg/type "int8"}
               {:push_coerce/id 1 :push_coerce/payload "1"}
               {:push_coerce/id 2 :push_coerce/payload "not-a-number"}
               {:db/ident :push_many/value :db/valueType :db.type/long
                :db/cardinality :db.cardinality/many}
               {:push_many/value #{1 2}}
               {:db/ident :push_target/key :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one}
               {:db/ident :push_ref/value :db/valueType :db.type/ref
                :db/cardinality :db.cardinality/one}
               {:db/id "target" :push_target/key 1}
               {:push_ref/value "target"}
               {:db/ident :push_hinted_link/value :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one}
               {:push_hinted_link/value 1}])
  (pgs/set-hint! *conn* :push_hinted_link/value {:references :push_target/key})
  (doseq [table ["push_string_hint" "push_many" "push_ref" "push_hinted_link"]]
    (is (nil? (candidate (str "SELECT x FROM (SELECT value x FROM " table ") t WHERE x=1")))
        table))
  (let [text "SELECT x,payload FROM(SELECT id x,payload FROM push_coerce)t WHERE x=1"]
    (is (nil? (candidate text))
        "an unreferenced coercing-hint projection also prevents the rewrite")
    (is (= (with-redefs [derived/additional-where (fn [& _] nil)] (outcome text))
           (outcome text))))
  (let [db (d/db *conn*)
        info (first (filter #(= :push_string_hint/value (:attr %))
                            (pgs/column-info (:schema db) "push_string_hint" db)))
        text "SELECT x FROM (SELECT value x FROM push_string_hint) t WHERE x=1"]
    (is (= types/oid-int8 (:oid info)))
    (is (= :db.type/string (:valuetype info)))
    (is (= (with-redefs [derived/additional-where (fn [& _] nil)] (outcome text))
           (outcome text))
        "an advertised integral OID does not bypass the materialization boundary")))

(deftest typed-scalar-text-and-small-integer-projections-remain-eligible
  (result "CREATE TABLE push_scalars(id smallint,name text,enabled boolean)")
  (result "INSERT INTO push_scalars VALUES(1,'one',true),(2,'two',false)")
  (let [text "SELECT x,name,enabled FROM (SELECT id x,name,enabled FROM push_scalars) t WHERE x=2"
        db (d/db *conn*)
        info (first (filter #(= :push_scalars/id (:attr %))
                            (pgs/column-info (:schema db) "push_scalars" db)))]
    (is (= types/oid-int2 (:oid info)))
    (is (= :db.type/long (:valuetype info)))
    (is (= :db.cardinality/one (:cardinality info)))
    (is (some? (candidate text)))
    (is (= [["2" "two" "f"]] (rows text))))
  (is (some? (candidate (str "SELECT x,y FROM (" product ") t WHERE x=2")))
      "SQL int4 stored as Long keeps its predicate proof"))

(deftest permissive-and-wrapped-databases-keep-the-existing-materialization-boundary
  (doseq [[flexibility values] [[:read [1 "01" "1" "2"]] [:write [1 2]]]]
    (let [cfg {:store {:backend :memory :id (random-uuid)}
               :schema-flexibility flexibility :keep-history? true}]
      (d/create-database cfg)
      (let [conn (d/connect cfg) handler (pg/make-query-handler conn)]
        (try
          (d/transact conn [{:db/ident :push_loose/value :db/valueType :db.type/long
                             :db/cardinality :db.cardinality/one :pg/type "int8"}])
          (d/transact conn (mapv #(hash-map :push_loose/value %) values))
          (let [text "SELECT x FROM (SELECT value x FROM push_loose) t WHERE x=1"
                db (d/db conn)
                outer (ast text)]
            (binding [*conn* conn *handler* handler]
              (when (= :read flexibility)
                (is (nil? (candidate text)))
                (is (= (with-redefs [derived/additional-where (fn [& _] nil)] (outcome text))
                       (outcome text))
                    "mixed read-flex values retain the unoptimized boundary's result or error")))
            (doseq [source [(d/filter db (fn [_ _] true)) (d/history db) (d/as-of db (:max-tx db))]]
              (is (nil? (derived/additional-where outer (.getFromItem outer)
                                                  (:schema db) source {}))
                  "a view needs its own storage/effect proof")))
          (finally (d/release conn) (d/delete-database cfg)))))))
