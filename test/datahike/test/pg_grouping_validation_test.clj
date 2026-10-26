(ns datahike.test.pg-grouping-validation-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.grouping-validation :as grouping])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [java.sql Connection DriverManager SQLException]
           [org.postgresql PGStatement]))

(defn- validation-state [text]
  (try (grouping/validate! (CCJSqlParserUtil/parse text)) nil
       (catch clojure.lang.ExceptionInfo e (first (errors/classify-exception e)))))

(deftest grouping-constructs-are-rejected-at-their-ast-role
  (doseq [text ["SELECT 1 GROUP BY GROUPING SETS((),())"
                "SELECT a FROM t GROUP BY GROUPING SETS(a)"
                "SELECT a FROM t GROUP BY a,ROLLUP(a)"
                "SELECT a,b FROM t GROUP BY CUBE((a,b))"
                "SELECT a FROM t GROUP BY ()"
                "SELECT a FROM t GROUP BY a,()"
                "SELECT a FROM t GROUP BY a WITH ROLLUP"
                "SELECT GROUPING(a) FROM t GROUP BY a"
                "SELECT a FROM t GROUP BY a HAVING GROUPING(a)=0"
                "SELECT a FROM t GROUP BY a ORDER BY GROUPING(a)"]]
    (is (= "0A000" (validation-state text)) text)))

(deftest validation-preserves-ordinary-grouping-and-user-function-names
  (doseq [text ["SELECT a,count(*) FROM t GROUP BY a"
                "SELECT a AS b FROM t GROUP BY b"
                "SELECT a FROM t GROUP BY (a)"
                "SELECT a FROM t GROUP BY ROW()"
                "SELECT a FROM t GROUP BY ROW(schema_name.rollup(a))"
                "SELECT a FROM t GROUP BY ROW(rollup(a))"
                "SELECT CAST(row_number() OVER(ORDER BY a) AS int) FROM t"
                "SELECT CAST(sum(a ORDER BY a) OVER() AS int) FROM t"
                "SELECT CAST(\"grouping\"(a) AS int) FROM t"
                "SELECT a FROM t GROUP BY 1"
                "SELECT schema_name.rollup(a) FROM t GROUP BY schema_name.rollup(a)"
                "SELECT \"rollup\"(a) FROM t GROUP BY \"rollup\"(a)"
                "SELECT schema_name.grouping(a) FROM t"
                "SELECT \"grouping\"(a) FROM t"
                "SELECT 'GROUPING SETS(a)' AS cube /* ROLLUP(a) */"]]
    (is (nil? (validation-state text)) text)))

(deftest forward-traversal-reaches-nested-select-scopes
  (doseq [text ["WITH x AS(SELECT 1 GROUP BY GROUPING SETS(())) SELECT * FROM x"
                "SELECT * FROM(SELECT a FROM t GROUP BY ROLLUP(a)) x"
                "SELECT (SELECT count(*) FROM t GROUP BY CUBE(a))"
                "SELECT coalesce((SELECT count(*) FROM t GROUP BY GROUPING SETS(())),1)"
                "SELECT CASE WHEN true THEN (SELECT 1 GROUP BY ()) END"
                "SELECT 1 UNION ALL SELECT 2 GROUP BY ()"
                "SELECT 1 WHERE EXISTS(SELECT 1 GROUP BY GROUPING SETS(()))"
                "SELECT * FROM a JOIN b ON a.id=(SELECT 1 GROUP BY ())"
                "INSERT INTO x SELECT 1 GROUP BY GROUPING SETS(())"
                "UPDATE x SET a=(SELECT 1 GROUP BY GROUPING SETS(()))"
                "INSERT INTO x VALUES(1) ON CONFLICT(a) DO UPDATE SET a=(SELECT 1 GROUP BY ())"
                "SELECT 1 ORDER BY (SELECT 1 GROUP BY ())"
                "SELECT * FROM t LIMIT (SELECT 1 GROUP BY ())"
                "DELETE FROM x WHERE a=(SELECT 1 GROUP BY ())"
                "CREATE TABLE x AS SELECT 1 GROUP BY GROUPING SETS(())"
                "CREATE VIEW x AS SELECT 1 GROUP BY GROUPING SETS(())"]]
    (is (= "0A000" (validation-state text)) text)))

(deftest expression-families-expose-their-grouping-and-nested-query-children
  (doseq [text ["SELECT row_number() OVER(ORDER BY GROUPING(a)) FROM t GROUP BY a"
                "SELECT row_number() OVER(PARTITION BY GROUPING(a)) FROM t GROUP BY a"
                "SELECT sum(a) FILTER(WHERE GROUPING(a)=0) FROM t GROUP BY a"
                "SELECT sum(a) FILTER(WHERE EXISTS(SELECT 1 GROUP BY())) FROM t"
                "SELECT substring('abc' FROM(SELECT 1 GROUP BY()))"
                "SELECT json_object('k' VALUE (SELECT 1 GROUP BY()))"
                "SELECT json_objectagg('k' VALUE (SELECT 1 GROUP BY())) FROM t"
                "SELECT json_arrayagg(a ORDER BY GROUPING(a)) FROM t GROUP BY a"
                "SELECT json_arrayagg(a) OVER(PARTITION BY GROUPING(a) ORDER BY a ROWS 1 PRECEDING) FROM t GROUP BY a"
                "SELECT row_number() OVER(ORDER BY a ROWS (SELECT 1 GROUP BY()) PRECEDING) FROM t"
                "SELECT sum(a ORDER BY (SELECT 1 GROUP BY())) OVER() FROM t"]]
    (is (= "0A000" (validation-state text)) text)))

(deftest wrapper-dispatch-cannot-bypass-special-expression-children
  (doseq [text ["SELECT -GROUPING(a) FROM t GROUP BY a"
                "SELECT CAST(GROUPING(a) AS int) FROM t GROUP BY a"
                "SELECT CAST(row_number() OVER(ORDER BY GROUPING(a)) AS int) FROM t GROUP BY a"
                "SELECT CAST(row_number() OVER(PARTITION BY GROUPING(a)) AS int) FROM t GROUP BY a"
                "SELECT -length(substring('abc' FROM(SELECT 1 GROUP BY())))"
                "SELECT CAST(json_object('k' VALUE (SELECT 1 GROUP BY())) AS text)"
                "SELECT CAST(json_arrayagg(a ORDER BY GROUPING(a)) AS text) FROM t GROUP BY a"
                "SELECT CAST(sum(a ORDER BY (SELECT 1 GROUP BY())) OVER() AS int) FROM t"]]
    (is (= "0A000" (validation-state text)) text)))

(deftest empty-grouping-syntax-is-not-an-absent-clause
  (is (= "0A000" (validation-state "SELECT 1 FROM t GROUP BY ()")))
  (is (= "0A000" (validation-state "SELECT 1 FROM t GROUP BY GROUPING SETS(())")))
  ;; JSqlParser rejects GROUPING SETS() itself. It does not produce a default
  ;; empty collection which could be mistaken for absent grouping syntax.
  (try
    (sql/parse-sql "SELECT 1 GROUP BY GROUPING SETS()" {})
    (is false "malformed empty grouping-list syntax must be rejected")
    (catch clojure.lang.ExceptionInfo e
      (is (= "42601" (first (errors/classify-exception e)))))))

(deftest cached-ast-is-unchanged-by-rejection
  (let [ast (CCJSqlParserUtil/parse
             "WITH x AS(SELECT a FROM t GROUP BY ROLLUP(a)) SELECT * FROM x")
        original (str ast)]
    (dotimes [_ 2]
      (is (= "0A000" (try (grouping/validate! ast) nil
                          (catch clojure.lang.ExceptionInfo e (first (errors/classify-exception e))))))
      (is (= original (str ast))))))

(defn- ^Connection connect [port]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" port
        "/grouping_validation?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c text]
  (with-open [s (.createStatement c)] (.execute s text)))

(defn- state [^Connection c text]
  (try (exec! c text) nil (catch SQLException e (.getSQLState e))))

(defn- rows [^Connection c text]
  (with-open [s (.createStatement c), r (.executeQuery s text)]
    (let [n (.getColumnCount (.getMetaData r))]
      (loop [out []]
        (if (.next r)
          (recur (conj out (mapv #(.getString r (int %)) (range 1 (inc n)))))
          out)))))

(deftest execution-and-explain-reject-before-represented-effects
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"grouping_validation" conn} {:port 0})]
      (try
        (with-open [c (connect (.getPort server))]
          (exec! c "CREATE SEQUENCE grouping_effect")
          (exec! c "CREATE TABLE grouping_source(a int)")
          (exec! c "CREATE TABLE grouping_empty(a int)")
          (exec! c "INSERT INTO grouping_source VALUES(1),(2)")
          (doseq [text ["SELECT 1 GROUP BY GROUPING SETS((),())"
                        "SELECT 1 FROM grouping_source GROUP BY ()"
                        "SELECT 1 FROM grouping_empty GROUP BY ()"
                        "SELECT 1 FROM grouping_empty GROUP BY GROUPING SETS(())"
                        "SELECT a FROM grouping_source GROUP BY a,()"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT -GROUPING(a) FROM x GROUP BY a"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT CAST(row_number() OVER(ORDER BY GROUPING(a)) AS int) FROM x GROUP BY a"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT CAST(substring('abc' FROM(SELECT 1 GROUP BY())) AS text) FROM x"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT CAST(json_object('k' VALUE (SELECT 1 GROUP BY())) AS text) FROM x"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT row_number() OVER(ORDER BY GROUPING(a)) FROM x GROUP BY a"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT row_number() OVER(PARTITION BY GROUPING(a)) FROM x GROUP BY a"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT sum(a) FILTER(WHERE EXISTS(SELECT 1 GROUP BY())) FROM x"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT substring('abc' FROM(SELECT 1 GROUP BY())) FROM x"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT json_object('k' VALUE (SELECT 1 GROUP BY())) FROM x"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT json_arrayagg(a ORDER BY (SELECT 1 GROUP BY())) FROM x"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT json_arrayagg(a) OVER(PARTITION BY GROUPING(a)) FROM x GROUP BY a"
                        "SELECT x FROM(SELECT nextval('grouping_effect') x) t GROUP BY GROUPING SETS(x)"
                        "WITH x AS(SELECT nextval('grouping_effect') a) SELECT a FROM x GROUP BY ROLLUP(a)"
                        "WITH x AS(SELECT nextval('grouping_effect') a),y AS(SELECT a FROM grouping_source GROUP BY CUBE(a)) SELECT * FROM x,y"
                        "SELECT nextval('grouping_effect'),(SELECT a FROM grouping_source GROUP BY GROUPING SETS(a))"
                        "SELECT * FROM generate_series(1,nextval('grouping_effect')::int) g GROUP BY CUBE(g)"
                        "INSERT INTO grouping_source SELECT nextval('grouping_effect')::int GROUP BY ()"
                        "CREATE TABLE grouping_bad AS SELECT nextval('grouping_effect') a GROUP BY GROUPING SETS(())"]]
            (testing text
              (is (= "0A000" (state c text)))
              ;; The EXPLAIN analyzer supports SELECT/WITH producers; DML
              ;; EXPLAIN parsing is a separate existing limitation.
              (when (or (.startsWith text "SELECT") (.startsWith text "WITH"))
                (is (= "0A000" (state c (str "EXPLAIN " text)))))
              (is (= [["1" "f"]] (rows c "SELECT last_value,is_called FROM grouping_effect")))))
          (testing "ordinary grouping still executes"
            (is (= [["1" "1"] ["2" "1"]]
                   (rows c "SELECT a,count(*) FROM grouping_source GROUP BY a ORDER BY a"))))
          (testing "extended-protocol Parse rejects the unsupported shape"
            (with-open [p (.prepareStatement c
                                             "SELECT a FROM grouping_source WHERE a=? GROUP BY GROUPING SETS(a)")]
              (.setPrepareThreshold ^PGStatement p 1)
              (.setInt p 1 1)
              (is (= "0A000" (try (.executeQuery p) nil
                                  (catch SQLException e (.getSQLState e)))))))
          (testing "failed statement retains normal transaction recovery"
            (exec! c "BEGIN")
            (exec! c "SAVEPOINT grouping_save")
            (is (= "0A000" (state c "SELECT a FROM grouping_source GROUP BY ROLLUP(a)")))
            (is (= "25P02" (state c "SELECT 1")))
            (exec! c "ROLLBACK TO SAVEPOINT grouping_save")
            (is (= [["1"]] (rows c "SELECT 1")))
            (exec! c "ROLLBACK")))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))
