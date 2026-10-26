(ns datahike.test.pg-explain-analysis-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.stmt :as stmt])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"explain_analysis" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/explain_analysis?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [s (.createStatement c)] (.execute s sql)))

(defn- rows [^Connection c sql]
  (with-open [s (.createStatement c), r (.executeQuery s sql)]
    (let [n (.getColumnCount (.getMetaData r))]
      (loop [out []]
        (if (.next r)
          (recur (conj out (mapv #(.getString r (int %)) (range 1 (inc n)))))
          out)))))

(defn- explain-plan [^Connection c sql]
  (let [result (rows c (str "EXPLAIN " sql))]
    (is (= 1 (count result)))
    (let [plan (ffirst result)]
      (is (re-find #"^Datahike Query " plan))
      plan)))

(defn- sqlstate [^Connection c sql]
  (try
    (exec! c sql)
    nil
    (catch SQLException e (.getSQLState e))))

(defn- sequence-state [^Connection c]
  (first (rows c "SELECT last_value,is_called FROM explain_analysis_seq")))

(deftest explain-derived-cartesian-does-not-materialize-rows
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_rows (id int)")
    (exec! c "INSERT INTO explain_rows VALUES (1),(2)")
    (explain-plan
     c
     (str "SELECT x,y FROM ("
          "SELECT t1.id x,t2.id y FROM explain_rows t1,explain_rows t2"
          ") t WHERE x=1 GROUP BY GROUPING SETS(x,y) ORDER BY 1,2"))))

(deftest explain-values-and-srfs-do-not-evaluate-arguments
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE explain_analysis_seq")
    (testing "VALUES metadata does not evaluate its cells"
      (explain-plan c "SELECT * FROM (VALUES(nextval('explain_analysis_seq'))) v(n)")
      (is (= ["1" "f"] (sequence-state c))))
    (testing "FROM-function metadata does not invoke its arguments"
      (explain-plan
       c
       (str "SELECT * FROM generate_series("
            "nextval('explain_analysis_seq')::int,"
            "(nextval('explain_analysis_seq')+2)::int) g"))
      (is (= ["1" "f"] (sequence-state c))))
    (testing "static alias errors are still reported before evaluation"
      (is (= "42P10"
             (sqlstate
              c
              (str "EXPLAIN SELECT * FROM generate_series("
                   "nextval('explain_analysis_seq')::int,3) AS g(a,b)"))))
      (is (= ["1" "f"] (sequence-state c))))
    (testing "ordinary execution still owns the effect"
      (is (= [["1"]]
             (rows c "SELECT nextval('explain_analysis_seq')")))
      (is (= ["1" "t"] (sequence-state c))))))

(deftest explain-recursive-cte-uses-anchor-metadata-only
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE explain_analysis_seq")
    (with-redefs [stmt/run-recursive-rule
                  (fn [& _]
                    (throw (ex-info "recursive runner was invoked" {})))]
      (explain-plan
       c
       (str "WITH RECURSIVE r(n) AS ("
            "SELECT nextval('explain_analysis_seq') "
            "UNION ALL SELECT n+1 FROM r WHERE n<3) SELECT n FROM r")))
    (is (= ["1" "f"] (sequence-state c)))
    (testing "anchor typmod must still match the recursive UNION output"
      (is (= "42804"
             (sqlstate
              c
              (str "EXPLAIN WITH RECURSIVE r(v) AS ("
                   "SELECT ('abc'::varchar(3)) UNION ALL "
                   "SELECT ((v||'x')::varchar(5)) FROM r WHERE length(v)<5) "
                   "SELECT v FROM r"))))
      (is (= ["1" "f"] (sequence-state c))))))

(deftest explain-expression-producers-are-inert-or-refused
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE explain_analysis_seq")
    (exec! c "CREATE TABLE explain_rows (id int)")
    (exec! c "INSERT INTO explain_rows VALUES (1),(2)")
    (testing "a runtime ARRAY subquery is analyzed but not run"
      (explain-plan c "SELECT ARRAY(SELECT id FROM explain_rows ORDER BY id)")
      (is (= ["1" "f"] (sequence-state c))))
    (testing "an eager WHERE EXISTS fallback fails before row production"
      (is (= "0A000"
             (sqlstate
              c
              (str "EXPLAIN SELECT id FROM explain_rows WHERE EXISTS ("
                   "SELECT 1 FROM explain_rows f "
                   "WHERE f.id>nextval('explain_analysis_seq'))"))))
      (is (= ["1" "f"] (sequence-state c))))
    (testing "static errors inside EXISTS precede the execution boundary"
      (is (= "42P01"
             (sqlstate
              c
              "EXPLAIN SELECT id FROM explain_rows WHERE EXISTS (SELECT 1 FROM absent_relation)")))
      (is (= ["1" "f"] (sequence-state c))))
    (testing "EXPLAIN ANALYZE remains an inert capability boundary"
      (is (= "0A000"
             (sqlstate c "EXPLAIN ANALYZE SELECT nextval('explain_analysis_seq')")))
      (is (= ["1" "f"] (sequence-state c))))))

(deftest named-describe-of-explain-is-inert
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE explain_analysis_seq")
    (with-open [ps (.prepareStatement
                    c
                    (str "EXPLAIN SELECT * FROM (VALUES("
                         "nextval('explain_analysis_seq'))) v(n)"))]
      (let [md (.getMetaData ps)]
        (is (= 1 (.getColumnCount md)))
        (is (= "QUERY PLAN" (.getColumnLabel md 1)))))
    (is (= ["1" "f"] (sequence-state c)))))
