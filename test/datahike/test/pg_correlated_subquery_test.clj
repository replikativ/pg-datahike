(ns datahike.test.pg-correlated-subquery-test
  "Correlated scalar and IN subqueries, and the aggregate-over-an-empty-
   relation rule they depend on.

   `SELECT p.id, (SELECT count(*) FROM ch WHERE ch.pid = p.id) FROM p` is
   one of the most common shapes in application SQL, and it was WRONG for
   every row: the deferral machinery detected the correlation and ran the
   inner per outer row, but the inner TRANSLATOR turned `ch.pid = p.id`
   into an implicit JOIN against the relation `p` -- adding p to the
   inner FROM -- so the correlation predicate dissolved and every row got
   the same uncorrelated answer.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn cs-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"cs" conn} {:port 0})]
      (try (binding [*port* (.getPort server) *conn* conn] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each cs-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/cs?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- sqlstate [^Connection c sql]
  (try
    (exec! c sql)
    nil
    (catch java.sql.SQLException e (.getSQLState e))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE p (id int, nm text)")
  (exec! c "CREATE TABLE ch (id int, pid int)")
  (exec! c "INSERT INTO p VALUES (1,'a'),(2,'b'),(3,'c')")
  ;; p=1 has two children, p=2 has one, p=3 has none -- so the correlated
  ;; count differs per row AND one row exercises the empty-aggregate rule.
  (exec! c "INSERT INTO ch VALUES (1,1),(2,1),(3,2)"))

(deftest correlated-scalar-subquery-in-the-select-list
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["2" "1" "0"]
           (col c 2 "SELECT id, (SELECT count(*) FROM ch WHERE ch.pid = p.id) AS c FROM p ORDER BY id"))
        "the count must differ per outer row, and be 0 where there are none")
    (is (= ["2" "1" "0"]
           (col c 2 (str "SELECT id, (SELECT count(*) FROM ch c2 WHERE c2.pid = p.id) AS c "
                         "FROM p ORDER BY id")))
        "with the inner table aliased")
    (is (= ["2" "3" nil]
           (col c 2 "SELECT id, (SELECT max(id) FROM ch WHERE ch.pid = p.id) AS c FROM p ORDER BY id"))
        "a non-COUNT aggregate over no rows is NULL")
    (testing "an explicit outer alias"
      (is (= ["2" "1" "0"]
             (col c 2 (str "SELECT id, (SELECT count(*) FROM ch WHERE ch.pid = o.id) AS c "
                           "FROM p o ORDER BY id")))))
    (testing "self-correlation -- the same table on both sides"
      (is (= ["a" "b" "c"]
             (col c 2 "SELECT id, (SELECT nm FROM p p2 WHERE p2.id = p.id) AS c FROM p ORDER BY id"))))
    (testing "the outer correlation value may itself be NULL"
      (exec! c "INSERT INTO p VALUES (4, NULL)")
      (is (= ["2" "1" "0" "0"]
             (col c 2 (str "SELECT id, (SELECT count(*) FROM ch WHERE ch.pid = p.id) AS c "
                           "FROM p ORDER BY id")))
          "no child matches a NULL parent id, so the count is 0"))))

(deftest aggregate-over-an-empty-relation-in-a-scalar-subquery
  (with-open [c (jdbc)]
    (seed! c)
    ;; SQL requires an aggregate with no GROUP BY to produce ONE row even
    ;; when nothing matches -- COUNT is 0, everything else NULL. Datalog
    ;; returns no rows at all, so the row has to be synthesised. exec-select
    ;; already did that for the top-level result; each of the THREE scalar
    ;; subquery evaluators ran d/q directly and answered NULL.
    (is (= "0" (one c "SELECT (SELECT count(*) FROM ch WHERE pid = 999)")))
    (is (nil? (one c "SELECT (SELECT min(id) FROM ch WHERE pid = 999)")))
    (is (= "3" (one c "SELECT (SELECT count(*) FROM ch)")))
    (testing "the top-level forms, which already worked"
      (is (= "0" (one c "SELECT count(*) FROM ch WHERE pid = 999")))
      (is (nil? (one c "SELECT min(id) FROM ch WHERE pid = 999"))))
    (testing "but a GROUP BY means zero matching GROUPS, hence zero rows"
      (is (= [] (col c 1 "SELECT pid FROM ch WHERE pid = 999 GROUP BY 1"))))))

(deftest correlated-in-is-three-valued-and-analyzed-before-execution
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE outer_values (k int, v boolean)")
    (exec! c "CREATE TABLE inner_values (owner int, v int)")
    (exec! c "INSERT INTO outer_values VALUES (1,true),(2,false),(3,true),(NULL,false)")
    (exec! c "INSERT INTO inner_values VALUES (1,1),(1,NULL),(2,1),(2,NULL)")
    (is (= ["t" nil "f" "f"]
           (col c 1 (str "SELECT k IN (SELECT v FROM inner_values i WHERE i.owner = o.k) "
                         "FROM outer_values o ORDER BY k NULLS LAST"))))
    (is (= ["f" nil "t" "t"]
           (col c 1 (str "SELECT k NOT IN (SELECT v FROM inner_values i WHERE i.owner = o.k) "
                         "FROM outer_values o ORDER BY k NULLS LAST"))))
    (is (= ["f" nil "t" "t"]
           (col c 1 (str "SELECT NOT (k IN (SELECT v FROM inner_values i WHERE i.owner = o.k)) "
                         "FROM outer_values o ORDER BY k NULLS LAST"))))
    (testing "inner names resolve in the inner scope, not against an outer homonym"
      (is (= ["1"]
             (col c 1 (str "SELECT k FROM outer_values o "
                           "WHERE k IN (SELECT v FROM inner_values i WHERE i.owner = o.k) "
                           "ORDER BY k")))))
    (testing "a direct inner table alias shadows the same outer alias"
      (is (= ["t" nil nil nil]
             (col c 1 (str "SELECT k IN (SELECT v FROM inner_values i WHERE i.owner = 1) "
                           "FROM outer_values i ORDER BY k NULLS LAST")))))
    (testing "derived and set-operation branch aliases shadow outer aliases"
      (is (= ["t" "f" "f" nil]
             (col c 1 (str "SELECT k IN (SELECT o.x FROM (SELECT 1 AS x) o) "
                           "FROM outer_values o ORDER BY k NULLS LAST"))))
      (is (= ["t" "t" "f" nil]
             (col c 1 (str "SELECT k IN (SELECT o.x FROM (SELECT 1 AS x) o "
                           "UNION SELECT o.x FROM (SELECT 2 AS x) o) "
                           "FROM outer_values o ORDER BY k NULLS LAST")))))
    (testing "an empty outer relation cannot hide analyzer errors"
      (exec! c "CREATE TABLE empty_outer (k int)")
      (exec! c "CREATE TABLE text_inner (owner int, v text)")
      (exec! c "CREATE TABLE two_columns (a int, b int)")
      (is (= "42703"
             (sqlstate c (str "SELECT k FROM empty_outer o WHERE k IN "
                              "(SELECT missing FROM inner_values i WHERE i.owner = o.k)"))))
      (is (= "42601"
             (sqlstate c (str "SELECT k FROM empty_outer o WHERE k IN "
                              "(SELECT * FROM two_columns i WHERE i.a = o.k)"))))
      (is (= "42883"
             (sqlstate c (str "SELECT k FROM empty_outer o WHERE k IN "
                              "(SELECT v FROM text_inner i WHERE i.owner = o.k)")))))))

(deftest correlated-exists-is-safe-in-boolean-composition
  (with-open [c (jdbc)]
    (seed! c)
    (testing "EXISTS inside OR does not leak its inner scan into the outer query"
      (is (= "2"
             (one c (str "SELECT count(*) FROM p o WHERE "
                         "(EXISTS (SELECT 1 FROM ch i WHERE i.pid = o.id) OR o.id < 0)")))))
    (testing "NOT EXISTS remains two-valued"
      (is (= ["3"]
             (col c 1 (str "SELECT id FROM p o WHERE NOT EXISTS "
                           "(SELECT 1 FROM ch i WHERE i.pid = o.id) ORDER BY id")))))
    (testing "nested correlated shapes fail explicitly instead of running an N+1 plan"
      (is (= "0A000"
             (sqlstate c (str "SELECT id FROM p o WHERE EXISTS "
                              "(SELECT 1 FROM ch i WHERE o.id IN "
                              "(SELECT pid FROM ch j WHERE j.id = i.id)) ORDER BY id")))))
    (testing "an empty outer relation cannot hide inner analyzer errors"
      (exec! c "CREATE TABLE no_parents (id int)")
      (is (= "42703"
             (sqlstate c (str "SELECT id FROM no_parents o WHERE EXISTS "
                              "(SELECT 1 FROM ch i WHERE i.missing = o.id)")))))
    (testing "correlation preserves PostgreSQL cross-storage numeric equality"
      (exec! c "CREATE TABLE numeric_outer (id numeric)")
      (exec! c "CREATE TABLE float_inner (id double precision)")
      (exec! c "INSERT INTO numeric_outer VALUES (1.5)")
      (exec! c "INSERT INTO float_inner VALUES (1.5)")
      (is (= "1"
             (one c (str "SELECT count(*) FROM numeric_outer o WHERE EXISTS "
                         "(SELECT 1 FROM float_inner i WHERE i.id = o.id)"))))
      (is (= [["1.5"]]
             (rows c (str "SELECT o.id FROM numeric_outer o WHERE EXISTS "
                          "(SELECT 1 FROM float_inner i WHERE i.id = o.id)")))
          "a projected outer column is already bound before EXISTS lowering"))
    (testing "floating NaN correlation retains PostgreSQL equality semantics"
      (exec! c "CREATE TABLE nan_outer (id double precision)")
      (exec! c "CREATE TABLE nan_inner (id double precision)")
      (exec! c "INSERT INTO nan_outer VALUES ('NaN')")
      (exec! c "INSERT INTO nan_inner VALUES ('NaN')")
      (is (= "1"
             (one c (str "SELECT count(*) FROM nan_outer o WHERE EXISTS "
                         "(SELECT 1 FROM nan_inner i WHERE i.id = o.id)")))))))

(deftest prepared-correlated-exists-reuses-a-parameter-across-scopes
  (with-open [c (jdbc)]
    (seed! c)
    (let [handler (pg/make-query-handler *conn*)
          prepared (.parse handler
                           (str "SELECT id FROM p o WHERE o.id > $1 AND EXISTS "
                                "(SELECT 1 FROM ch i WHERE i.pid = o.id AND i.pid > $1) "
                                "ORDER BY id")
                           (int-array [23]))
          result (.executePrepared handler prepared (object-array [nil (long 0)]))]
      (is (nil? (.error result)) (.error result))
      (is (= [["1"] ["2"]]
             (vec (map vec (.rows result))))))))

(deftest prepared-correlated-exists-reads-the-execution-snapshot
  (with-open [c (jdbc)]
    (seed! c)
    (with-open [statement (.prepareStatement
                           c (str "SELECT id FROM p o WHERE EXISTS "
                                  "(SELECT 1 FROM ch i WHERE i.pid = o.id) ORDER BY id"))]
      (letfn [(answers []
                (with-open [rs (.executeQuery statement)]
                  (loop [out []]
                    (if (.next rs)
                      (recur (conj out (.getString rs 1)))
                      out))))]
        (is (= ["1" "2"] (answers)))
        (exec! c "INSERT INTO ch VALUES (4,3)")
        (is (= ["1" "2" "3"] (answers)))))))

(deftest in-subquery-result-semantics
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE owners (k int)")
    (exec! c "CREATE TABLE owned (owner int)")
    (exec! c "INSERT INTO owners VALUES (0),(1)")
    (exec! c "INSERT INTO owned VALUES (1)")
    (testing "an aggregate over an empty input still contributes one row"
      (is (= "t" (one c "SELECT 0 IN (SELECT count(*) FROM owned WHERE false)")))
      (is (= ["t" "t"]
             (col c 1 (str "SELECT k IN (SELECT count(*) FROM owned i WHERE i.owner = o.k) "
                           "FROM owners o ORDER BY k")))))
    (testing "empty and NULL membership follow PostgreSQL ANY semantics"
      (is (= "f" (one c "SELECT 2 IN (SELECT owner FROM owned WHERE false)")))
      (is (= "t" (one c "SELECT 2 NOT IN (SELECT owner FROM owned WHERE false)")))
      (is (= "f" (one c "SELECT NULL::int IN (SELECT owner FROM owned WHERE false)")))
      (is (= "t" (one c "SELECT NULL::int NOT IN (SELECT owner FROM owned WHERE false)")))
      (is (nil? (one c "SELECT NULL::int IN (SELECT owner FROM owned)")))
      (is (nil? (one c "SELECT NULL::int NOT IN (SELECT owner FROM owned)"))))
    (testing "uncorrelated output types are analyzed before rows are consumed"
      (exec! c "CREATE TABLE text_values (v text)")
      (is (= "42883" (sqlstate c "SELECT 1 IN (SELECT v FROM text_values)"))))
    (testing "set-operation subqueries expose the first branch's scalar width"
      (is (= "t" (one c "SELECT '1'::text IN (SELECT '1'::name UNION ALL SELECT '1'::name)")))
      (is (= "t" (one c "SELECT 1::numeric IN (SELECT 1::int INTERSECT SELECT 1::numeric)")))
      (is (= "f" (one c "SELECT 1::numeric IN (SELECT 1::int EXCEPT SELECT 1::numeric)")))
      (is (= "t" (one c "SELECT 1 IN ((SELECT 1) UNION (SELECT 2))")))
      (is (= "t" (one c (str "SELECT timestamp '2020-01-01' IN "
                             "(SELECT date '2020-01-01' UNION "
                             "SELECT timestamp '2020-01-02')")))))
    (testing "unsupported post-query stages fail explicitly, never with a plausible wrong value"
      (is (= "0A000"
             (sqlstate c "SELECT 2 IN (SELECT generate_series(1,3))")))
      (is (= "0A000"
             (sqlstate c "SELECT 1 IN (SELECT count(*) HAVING false)")))
      (is (= "0A000"
             (sqlstate c (str "SELECT 1 IN (SELECT 1 UNION ALL SELECT 1 "
                              "INTERSECT SELECT 1)"))))
      (is (= "0A000"
             (sqlstate c "SELECT 1 IN (SELECT 1 INTERSECT ALL SELECT 1)"))))))

(deftest prepared-in-subqueries-read-the-execution-snapshot
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE dynamic_inner (owner int, v int)")
    (exec! c "CREATE TABLE dynamic_outer (k int)")
    (exec! c "INSERT INTO dynamic_inner VALUES (1,1)")
    (exec! c "INSERT INTO dynamic_outer VALUES (2)")
    (with-open [uncorrelated (.prepareStatement
                              c "SELECT 2 IN (SELECT v FROM dynamic_inner)")
                correlated (.prepareStatement
                            c (str "SELECT k IN (SELECT v FROM dynamic_inner i WHERE i.owner = o.k) "
                                   "FROM dynamic_outer o"))
                uncorrelated-param (.prepareStatement
                                    c (str "SELECT 2 IN (SELECT v FROM dynamic_inner "
                                           "WHERE owner = ?)"))
                correlated-param (.prepareStatement
                                  c (str "SELECT k IN (SELECT v FROM dynamic_inner i "
                                         "WHERE i.owner = o.k AND i.v >= ?) "
                                         "FROM dynamic_outer o"))
                value-in (.prepareStatement c "SELECT 2 IN (1,NULL,?)")
                value-not-in (.prepareStatement c "SELECT 2 NOT IN (1,NULL,?)")
                setop-cte (.prepareStatement
                           c (str "WITH current_values AS "
                                  "(SELECT v FROM dynamic_inner WHERE owner = 2) "
                                  "SELECT 2 IN (SELECT v FROM current_values) "
                                  "UNION ALL SELECT false"))
                cte (.prepareStatement
                     c (str "WITH current_values AS "
                            "(SELECT v FROM dynamic_inner WHERE owner = 2) "
                            "SELECT 2 IN (SELECT v FROM current_values)"))]
      (letfn [(answer [statement]
                (with-open [rs (.executeQuery statement)]
                  (.next rs)
                  (.getString rs 1)))
              (answers [statement]
                (with-open [rs (.executeQuery statement)]
                  (loop [out []]
                    (if (.next rs)
                      (recur (conj out (.getString rs 1)))
                      out))))]
        (.setInt uncorrelated-param 1 2)
        (.setInt correlated-param 1 0)
        (.setInt value-in 1 3)
        (.setInt value-not-in 1 3)
        (is (= "f" (answer uncorrelated)))
        (is (= "f" (answer correlated)))
        (is (= "f" (answer uncorrelated-param)))
        (is (= "f" (answer correlated-param)))
        (is (nil? (answer value-in))
            "a static NULL survives runtime construction of a parameterized IN list")
        (is (nil? (answer value-not-in))
            "NOT IN preserves UNKNOWN for a parameterized list containing NULL")
        (is (= "f" (answer cte)))
        (is (= ["f" "f"] (answers setop-cte)))
        (exec! c "INSERT INTO dynamic_inner VALUES (2,2)")
        (is (= "t" (answer uncorrelated))
            "the prepared plan must not retain its Parse-time db")
        (is (= "t" (answer correlated))
            "the correlated closure must scan the current statement snapshot")
        (is (= "t" (answer uncorrelated-param))
            "bound parameters are visible while reparsing an uncorrelated RHS")
        (is (= "t" (answer correlated-param))
            "bound parameters are visible while reparsing a correlated RHS")
        (is (= "t" (answer cte))
            "prepared CTE enrichment must be rebuilt on the current snapshot")
        (is (= #{"t" "f"} (set (answers setop-cte)))
            "set-operation branches propagate runtime-subquery freshness")))))

(deftest postgres-17-correlated-in-regression-slice
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE subselect_tbl (f1 integer, f2 integer, f3 float)")
    (exec! c (str "INSERT INTO subselect_tbl VALUES "
                  "(1,2,3),(2,3,4),(3,4,5),(1,1,1),"
                  "(2,2,2),(3,3,3),(6,7,8),(8,9,NULL)"))
    (is (= [["1" "1"] ["1" "2"] ["2" "2"]
            ["2" "3"] ["3" "3"] ["3" "4"]]
           (rows c (str "SELECT f1, f2 FROM subselect_tbl upper_t "
                        "WHERE f1 IN (SELECT f2 FROM subselect_tbl WHERE f1 = upper_t.f1) "
                        "ORDER BY f1, f2"))))
    (is (= [["1" "1"] ["2" "2"] ["2" "4"]
            ["3" "3"] ["3" "5"]]
           (rows c (str "SELECT f1, f3 FROM subselect_tbl upper_t WHERE f1 IN "
                        "(SELECT f2 FROM subselect_tbl WHERE CAST(upper_t.f2 AS float) = f3) "
                        "ORDER BY f1, f3"))))
    (is (= [["1" "3"] ["2" "4"] ["3" "5"] ["6" "8"]]
           (rows c (str "SELECT f1, f3 FROM subselect_tbl upper_t WHERE f3 IN "
                        "(SELECT upper_t.f1 + f2 FROM subselect_tbl "
                        "WHERE f2 = CAST(f3 AS integer)) ORDER BY f1"))))))
