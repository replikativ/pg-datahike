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
            [datahike.pg.server :as pg]
            [datahike.pg.sql.fns :as fns])
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

(deftest row-valued-in-subqueries-follow-postgresql-three-valued-semantics
  (with-open [c (jdbc)]
    (testing "row equality folds fields before membership folds RHS rows"
      (is (= "t" (one c "SELECT (1,2) IN (SELECT 1,2)")))
      (is (= "t" (one c "SELECT ROW(1,2) IN (SELECT 1,2)")))
      (is (= "t" (one c "SELECT (1,2) IN (SELECT '1','2')"))
          "plain UNKNOWN outputs are coerced from the corresponding left fields")
      (is (= "t" (one c (str "SELECT (1,2) IN "
                             "(SELECT '1','2' UNION ALL SELECT 1,2)")))
          "set operations resolve each output field across their branches")
      (is (nil? (one c "SELECT (1,2) IN (SELECT 1,NULL)")))
      (is (= "f" (one c "SELECT (1,2) IN (SELECT 2,NULL)"))
          "a false field dominates a NULL field within one row")
      (is (nil? (one c (str "SELECT (1,2) IN "
                            "(SELECT 2,NULL::int UNION ALL SELECT 1,NULL::int)"))))
      (is (= "t" (one c (str "SELECT (1,2) IN "
                             "(SELECT 1,NULL::int UNION ALL SELECT 1,2)")))
          "a true RHS row dominates an unknown RHS row")
      (is (= "f" (one c "SELECT (NULL::int,2) IN (SELECT 1,3)")))
      (is (nil? (one c "SELECT (NULL::int,2) IN (SELECT 1,2)"))))
    (testing "empty RHS follows ANY semantics even for NULL left fields"
      (is (= "f" (one c "SELECT (NULL::int,2) IN (SELECT 1,2 WHERE false)")))
      (is (= "t" (one c "SELECT (NULL::int,2) NOT IN (SELECT 1,2 WHERE false)"))))
    (testing "width and type errors are raised during analysis"
      (is (= "42601" (sqlstate c "SELECT (1,2) IN (SELECT 1)")))
      (is (= "42601" (sqlstate c "SELECT (1,2) IN (SELECT 1,2,3)")))
      (is (= "42883"
             (sqlstate c (str "SELECT (1,2) IN "
                              "(SELECT '1','2' UNION ALL SELECT '1','2')")))
          "an all-UNKNOWN set-operation output resolves to text, as in PostgreSQL")
      (exec! c "CREATE TABLE empty_row_outer (a int, b int)")
      (exec! c "CREATE TABLE empty_row_inner (a int, b text)")
      (is (= "42883"
             (sqlstate c (str "SELECT a FROM empty_row_outer o WHERE (a,b) IN "
                              "(SELECT a,b FROM empty_row_inner)")))
          "empty relations cannot hide an operator mismatch"))))

(deftest postgres-17-row-in-regression-slice
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE row_subselect (f1 integer, f2 integer, f3 float)")
    (exec! c (str "INSERT INTO row_subselect VALUES "
                  "(1,2,3),(2,3,4),(3,4,5),(1,1,1),"
                  "(2,2,2),(3,3,3),(6,7,8),(8,9,NULL)"))
    (is (= [["1" "2"] ["6" "7"] ["8" "9"]]
           (rows c (str "SELECT f1,f2 FROM row_subselect WHERE (f1,f2) NOT IN "
                        "(SELECT f2,CAST(f3 AS int4) FROM row_subselect "
                        "WHERE f3 IS NOT NULL) ORDER BY f1,f2"))))
    (is (= ["1" "2" "2" "3" "3"]
           (col c 1 (str "SELECT f1 FROM row_subselect upper_row WHERE (f1,f2) IN "
                         "(SELECT f2,CAST(f3 AS int4) FROM row_subselect "
                         "WHERE f3 IS NOT NULL) ORDER BY f1"))))))

(deftest correlated-row-in-reads-the-execution-snapshot
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE row_owners (owner int, a int, b int)")
    (exec! c "CREATE TABLE row_owned (owner int, a int, b int)")
    (exec! c "INSERT INTO row_owners VALUES (1,10,20),(2,30,40)")
    (exec! c "INSERT INTO row_owned VALUES (1,10,20)")
    (with-open [statement (.prepareStatement
                           c (str "SELECT owner FROM row_owners o WHERE (a,b) IN "
                                  "(SELECT a,b FROM row_owned i WHERE i.owner=o.owner) "
                                  "ORDER BY owner"))
                parameterized (.prepareStatement
                               c (str "SELECT owner FROM row_owners o WHERE (?::int,b) IN "
                                      "(SELECT a,b FROM row_owned i WHERE i.owner=o.owner) "
                                      "ORDER BY owner"))
                uncorrelated (.prepareStatement
                              c "SELECT (30,40) IN (SELECT a,b FROM row_owned)")]
      (letfn [(answers []
                (with-open [rs (.executeQuery statement)]
                  (loop [out []]
                    (if (.next rs)
                      (recur (conj out (.getString rs 1)))
                      out))))
              (param-answers [n]
                (.setInt parameterized 1 n)
                (with-open [rs (.executeQuery parameterized)]
                  (loop [out []]
                    (if (.next rs)
                      (recur (conj out (.getString rs 1)))
                      out))))]
        (is (= ["1"] (answers)))
        (is (= ["1"] (param-answers 10)))
        (is (= [] (param-answers 30)))
        (is (= "f" (one c "SELECT (30,40) IN (SELECT a,b FROM row_owned)")))
        (with-open [rs (.executeQuery uncorrelated)]
          (.next rs)
          (is (= "f" (.getString rs 1))))
        (exec! c "INSERT INTO row_owned VALUES (2,30,40)")
        (is (= ["1" "2"] (answers)))
        (is (= ["2"] (param-answers 30)))
        (with-open [rs (.executeQuery uncorrelated)]
          (.next rs)
          (is (= "t" (.getString rs 1))))))))

(deftest row-comparisons-follow-postgresql-semantics
  (with-open [c (jdbc)]
    (testing "equality examines every field under three-valued logic"
      (is (= "t" (one c "SELECT ROW(1,2) = ROW(1,2)")))
      (is (= "t" (one c "SELECT (1,2) = (1,2)")))
      (is (= "f" (one c "SELECT ROW(1,2,3) = ROW(1,NULL,4)")))
      (is (= "t" (one c "SELECT ROW(1,2,3) <> ROW(1,NULL,4)")))
      (is (nil? (one c "SELECT ROW(1,2) = ROW(1,NULL)"))))
    (testing "ordering stops at the first unequal pair"
      (is (= "t" (one c "SELECT ROW(1,2) < ROW(1,3)")))
      (is (= "f" (one c "SELECT ROW(1,2) < ROW(1,1)")))
      (is (nil? (one c "SELECT ROW(1,2) < ROW(1,NULL)")))
      (is (= "t" (one c "SELECT ROW(1,2,3) < ROW(1,3,NULL)")))
      (is (= "t" (one c "SELECT ROW(1,2) <= ROW(1,2)")))
      (is (= "t" (one c "SELECT ROW(1,2) >= ROW(1,2)")))
      (is (= "t" (one c "SELECT ROW(2,0) > ROW(1,NULL)")))
      (is (= "t"
             (one c (str "SELECT ROW(date '2020-01-01',1) < "
                         "ROW(timestamp '2020-01-02',1)")))
          "cross-type temporal fields compare through their common timeline"))
    (testing "operator selection and width checking are per field"
      (is (= "t" (one c "SELECT ROW(1,2) = ROW('1','2')")))
      (is (= "t" (one c "SELECT ROW(1,2) = ROW(1::bigint,2::numeric)")))
      (is (= "42601" (sqlstate c "SELECT ROW(1,2) = ROW(1)")))
      (is (= "42883" (sqlstate c "SELECT ROW(1,2) < ROW(1::text,2)")))
      (is (= "t"
             (one c (str "SELECT ROW('80000000-0000-0000-0000-000000000000'::uuid) > "
                         "ROW('00000000-0000-0000-0000-000000000000'::uuid)")))
          "UUID fields use PostgreSQL's unsigned byte ordering")
      (is (= "t"
             (one c "SELECT ROW(ARRAY[1]::int4[]) = ROW(ARRAY[1]::int4[])")))
      (is (= "42883"
             (sqlstate c (str "SELECT ROW(ARRAY[1]::int4[]) = "
                              "ROW(ARRAY[1]::int8[])"))))
      (is (= "42883"
             (sqlstate c (str "SELECT ROW(ARRAY[1]::int4[]) = "
                              "ROW('{1}'::text)"))))
      (is (= "0A000"
             (sqlstate c "SELECT ROW('{}'::jsonb) = ROW('{}'::jsonb)"))
          "specialized field families fail explicitly until row dispatch supports them")
      (is (= "0A000"
             (sqlstate c (str "SELECT ROW('1 day'::interval) = "
                              "ROW('24 hours'::interval)"))))
      (is (= "0A000"
             (sqlstate c (str "SELECT ROW('12:00+00'::timetz) = "
                              "ROW('12:00+00'::timetz)"))))
      (is (= "0A000"
             (sqlstate c (str "SELECT ROW('12:00'::time) = "
                              "ROW('12:00'::time)"))))
      (is (= "0A000"
             (sqlstate c "SELECT ROW(ARRAY[1]) < ROW(ARRAY[2])"))))
    (testing "predicate position retains lexicographic and NULL behavior"
      (exec! c "CREATE TABLE row_ordering (a int, b int)")
      (exec! c "INSERT INTO row_ordering VALUES (1,2),(2,3),(2,NULL),(3,1)")
      (is (= ["1" "2"]
             (col c 1 "SELECT a FROM row_ordering WHERE (a,b) < (2,4) ORDER BY a,b"))))))

(deftest row-comparison-scalar-subqueries-enforce-shape-and-cardinality
  (with-open [c (jdbc)]
    (is (= "t" (one c "SELECT ROW(1,2) = (SELECT 1,2)")))
    (is (= "t" (one c "SELECT ROW('1',2) = (SELECT 1,2)")))
    (is (= "t" (one c "SELECT ROW(1,2) < (SELECT 1,3)")))
    (is (nil? (one c "SELECT ROW(1,2) = (SELECT 1,2 WHERE false)")))
    (is (nil? (one c "SELECT ROW(1,2) <> (SELECT 1,NULL::int)")))
    (is (= "21000"
           (sqlstate c (str "SELECT ROW(1,2) = "
                            "(SELECT 1,2 UNION ALL SELECT 1,2)"))))
    (is (= "42601" (sqlstate c "SELECT ROW(1,2) = (SELECT 1)")))
    (is (= "42883" (sqlstate c "SELECT ROW(1,2) = (SELECT '1','2')"))
        "a subquery UNKNOWN output resolves to text, unlike an IN sublink")
    (testing "unqualified outward references preserve SQL name resolution"
      (exec! c "CREATE TABLE outer_left (a int)")
      (exec! c "CREATE TABLE outer_right (b int, c int)")
      (exec! c "INSERT INTO outer_left VALUES (9)")
      (exec! c "INSERT INTO outer_right VALUES (1,2)")
      (is (= "t"
             (one c (str "SELECT ROW(1,2) = (SELECT b,c) "
                         "FROM outer_left l, outer_right r")))
          "a column owned by a non-default joined relation binds to its owner")
      (is (= "42703"
             (sqlstate c (str "SELECT ROW(1,2) = (SELECT missing,c) "
                              "FROM outer_left l, outer_right r"))))
      (exec! c "CREATE TABLE outer_ambiguous (b int, c int)")
      (is (= "42702"
             (sqlstate c (str "SELECT ROW(1,2) = (SELECT b,c) "
                              "FROM outer_right r, outer_ambiguous x")))))))

(deftest postgres-17-row-comparison-subquery-slice
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE rowcompare_subselect (f1 int, f2 int)")
    (exec! c (str "INSERT INTO rowcompare_subselect VALUES "
                  "(1,2),(2,3),(3,4),(1,1),(2,2),(3,3),(6,7),(8,9)"))
    (is (= ["t" "f" "f" "f" "f" "f" "f" "f"]
           (col c 1 (str "SELECT ROW(1,2) = (SELECT f1,f2) "
                         "FROM rowcompare_subselect ORDER BY db_id"))))
    (is (= (vec (repeat 8 "f"))
           (col c 1 (str "SELECT ROW(1,2) = (SELECT 3,4) "
                         "FROM rowcompare_subselect ORDER BY db_id"))))
    (is (= "21000"
           (sqlstate c (str "SELECT ROW(1,2) = "
                            "(SELECT f1,f2 FROM rowcompare_subselect)"))))))

(deftest prepared-row-comparison-subquery-reads-the-execution-snapshot
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE rowcompare_singleton (a int, b int)")
    (exec! c "INSERT INTO rowcompare_singleton VALUES (1,2)")
    (with-open [statement (.prepareStatement
                           c "SELECT ROW(3,4) = (SELECT a,b FROM rowcompare_singleton)")
                parameterized (.prepareStatement
                               c (str "SELECT ROW(?::int,?::int) = "
                                      "(SELECT a,b FROM rowcompare_singleton)"))]
      (letfn [(answer []
                (with-open [rs (.executeQuery statement)]
                  (.next rs)
                  (.getString rs 1)))
              (param-answer [a b]
                (.setInt parameterized 1 a)
                (.setInt parameterized 2 b)
                (with-open [rs (.executeQuery parameterized)]
                  (.next rs)
                  (.getString rs 1)))]
        (is (= "f" (answer)))
        (is (= "t" (param-answer 1 2)))
        (exec! c "UPDATE rowcompare_singleton SET a=3,b=4")
        (is (= "t" (answer)))
        (is (= "t" (param-answer 3 4)))
        (is (= "f" (param-answer 1 2)))))))

(deftest simple-query-number-templating-does-not-capture-inner-parameters
  (let [handler (pg/make-query-handler *conn*)
        result (.execute handler "SELECT ROW(1,2) = (SELECT 1,2)")]
    (is (nil? (.error result)) (.error result))
    (is (= [["t"]] (mapv vec (.rows result))))))

(deftest scalar-subqueries-enforce-postgresql-width-cardinality-and-errors
  (with-open [c (jdbc)]
    (is (= "2" (one c "SELECT (SELECT 2)")))
    (is (= "2" (one c "SELECT ((SELECT 2) UNION SELECT 2)")))
    (is (= "1" (one c "SELECT (SELECT 1 UNION ALL SELECT 2 LIMIT 1)")))
    (is (= "2" (one c "SELECT (SELECT 1 UNION ALL SELECT 2 LIMIT 1 OFFSET 1)")))
    (is (= "1" (one c "SELECT (SELECT 2 UNION ALL SELECT 1 ORDER BY 1 LIMIT 1)")))
    (is (= "1" (one c "SELECT (SELECT ARRAY[1,2,3])[1]")))
    (is (= "2" (one c "SELECT ((SELECT ARRAY[1,2,3]))[2]")))
    (is (= "3" (one c "SELECT (((SELECT ARRAY[1,2,3])))[3]")))
    (is (= "2201W"
           (sqlstate c "SELECT (SELECT 1 UNION ALL SELECT 2 LIMIT -1)")))
    (is (= "2201X"
           (sqlstate c "SELECT (SELECT 1 UNION ALL SELECT 2 OFFSET -1)")))
    (is (nil? (one c "SELECT (SELECT 1 WHERE false)")))
    (is (= "21000"
           (sqlstate c "SELECT (SELECT x FROM (VALUES (1),(2)) v(x))")))
    (is (= "42601" (sqlstate c "SELECT (SELECT 1,2)")))
    (is (= "22012" (sqlstate c "SELECT (SELECT 1/0)")))))

(deftest correlated-scalar-set-operations-use-each-outer-row
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE scalar_outer (k int)")
    (exec! c "INSERT INTO scalar_outer VALUES (1),(2)")
    (is (= [["1" "1"] ["2" "2"]]
           (rows c (str "SELECT k, (SELECT o.k UNION SELECT o.k) "
                        "FROM scalar_outer o ORDER BY k"))))
    (is (= [["1" "1"] ["2" "2"]]
           (rows c "SELECT k, (SELECT k) FROM scalar_outer ORDER BY k"))
        "a unique unqualified column resolves outward when the inner has no FROM")
    (exec! c "INSERT INTO scalar_outer VALUES (1)")
    (let [calls (atom 0)]
      (with-redefs [fns/sql-random (fn [] (double (swap! calls inc)))]
        (is (= ["1" "2" "3"]
               (col c 1 (str "SELECT (SELECT random() WHERE o.k=o.k) "
                             "FROM scalar_outer o ORDER BY db_id")))
            "equal correlation values still execute once per physical row")))))

(deftest scalar-subquery-types-participate-in-expression-analysis
  (with-open [c (jdbc)]
    (is (= "integer" (one c "SELECT pg_typeof((SELECT 1))")))
    (is (= "text" (one c "SELECT pg_typeof((SELECT NULL))")))
    (is (= "real"
           (one c "SELECT pg_typeof((SELECT 1::real) * (SELECT 2::real))")))
    (is (= "date"
           (one c "SELECT pg_typeof((SELECT DATE '2020-01-01') + 1)")))
    (is (= "2020-01-02" (one c "SELECT (SELECT DATE '2020-01-01') + 1")))
    (is (= "42883" (sqlstate c "SELECT (SELECT true) = 1")))
    (is (= "42883" (sqlstate c "SELECT (SELECT NULL) = 1")))
    (is (= "42883" (sqlstate c "SELECT (SELECT '1') + 1")))
    (is (= "22003" (sqlstate c "SELECT (SELECT 2147483647) + 1")))
    (exec! c "CREATE TABLE scalar_typed_outer (i int, b boolean)")
    (exec! c "INSERT INTO scalar_typed_outer VALUES (1,true)")
    (is (= "integer"
           (one c "SELECT pg_typeof((SELECT o.i)) FROM scalar_typed_outer o")))
    (is (= "42883"
           (sqlstate c "SELECT (SELECT o.b) = o.i FROM scalar_typed_outer o")))))

(deftest case-and-coalesce-evaluate-only-the-selected-scalar-subquery
  (with-open [c (jdbc)]
    (testing "runtime cardinality and arithmetic faults in dead arms are skipped"
      (is (= "1"
             (one c (str "SELECT CASE WHEN true THEN 1 ELSE "
                         "(SELECT x FROM (VALUES (1),(2)) v(x)) END"))))
      (is (= "1"
             (one c (str "SELECT COALESCE(1, "
                         "(SELECT x FROM (VALUES (1),(2)) v(x)))"))))
      (is (= "1" (one c "SELECT CASE WHEN true THEN 1 ELSE (SELECT 1/0) END")))
      (is (= "1" (one c "SELECT COALESCE(1, (SELECT 1/0))")))
      (is (= "1" (one c "SELECT CASE WHEN true THEN 1 ELSE abs((SELECT 1/0)) END")))
      (is (= "1" (one c "SELECT COALESCE(1, abs((SELECT 1/0)))")))
      (is (= "1"
             (one c (str "SELECT CASE WHEN true THEN 1 ELSE "
                         "CASE WHEN true THEN (SELECT 1/0) ELSE 2 END END"))))
      (is (= "1"
             (one c "SELECT CASE WHEN true THEN 1 ELSE ((SELECT 1/0) + 1) END"))))
    (testing "the same faults are retained when the value is selected"
      (is (= "21000"
             (sqlstate c (str "SELECT CASE WHEN false THEN 1 ELSE "
                              "(SELECT x FROM (VALUES (1),(2)) v(x)) END"))))
      (is (= "21000"
             (sqlstate c (str "SELECT COALESCE(NULL::int, "
                              "(SELECT x FROM (VALUES (1),(2)) v(x)))"))))
      (is (= "22012" (sqlstate c "SELECT CASE WHEN false THEN 1 ELSE (SELECT 1/0) END")))
      (is (= "22012" (sqlstate c "SELECT COALESCE(NULL::int, (SELECT 1/0))"))))
    (testing "analysis still visits dead subqueries"
      (is (= "42601"
             (sqlstate c "SELECT CASE WHEN true THEN 1 ELSE (SELECT 1,2) END")))
      (exec! c "CREATE TABLE scalar_analysis_source (x int)")
      (is (= "42703"
             (sqlstate c (str "SELECT COALESCE(1, "
                              "(SELECT missing FROM scalar_analysis_source))")))))))

(deftest prepared-scalar-subquery-reads-the-execution-snapshot
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE scalar_snapshot (v int)")
    (exec! c "INSERT INTO scalar_snapshot VALUES (1)")
    (with-open [statement (.prepareStatement c "SELECT (SELECT v FROM scalar_snapshot)")]
      (letfn [(answer []
                (with-open [rs (.executeQuery statement)]
                  (.next rs)
                  (.getString rs 1)))]
        (is (= "1" (answer)))
        (exec! c "UPDATE scalar_snapshot SET v=2")
        (is (= "2" (answer)))
        (exec! c "INSERT INTO scalar_snapshot VALUES (3)")
        (is (= "21000"
               (try (answer) nil
                    (catch java.sql.SQLException e (.getSQLState e)))))))))

(deftest prepared-set-operation-scalar-applies-limit-and-offset-at-execute
  (with-open [c (jdbc)
              statement (.prepareStatement
                         c "SELECT (SELECT 1 UNION ALL SELECT 2 LIMIT ? OFFSET ?)")]
    (letfn [(answer [limit offset]
              (.setInt statement 1 limit)
              (.setInt statement 2 offset)
              (try
                (with-open [rs (.executeQuery statement)]
                  (.next rs)
                  (.getString rs 1))
                (catch java.sql.SQLException e (.getSQLState e))))]
      (is (= "1" (answer 1 0)))
      (is (= "2" (answer 1 1)))
      (is (= "21000" (answer 2 0)))
      (is (= "2201W" (answer -1 0)))
      (is (= "2201X" (answer 1 -1))))))

(deftest scalar-subqueries-in-writes-use-the-statement-and-transaction-snapshot
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE scalar_source (v int)")
    (exec! c "INSERT INTO scalar_source VALUES (0),(123456)")
    (exec! c "CREATE TABLE scalar_target (key int PRIMARY KEY, val text)")
    (exec! c (str "INSERT INTO scalar_target VALUES "
                  "(1,(SELECT v FROM scalar_source WHERE v=0)::text)"))
    (exec! c (str "UPDATE scalar_target SET val="
                  "(SELECT v FROM scalar_source WHERE v=123456)::text WHERE key=1"))
    (is (= "123456" (one c "SELECT val FROM scalar_target WHERE key=1")))
    (exec! c (str "INSERT INTO scalar_target VALUES (1,'discard') "
                  "ON CONFLICT (key) DO UPDATE SET val='seen with subselect ' || "
                  "(SELECT v FROM scalar_source WHERE v != 0 LIMIT 1)::text"))
    (is (= "seen with subselect 123456"
           (one c "SELECT val FROM scalar_target WHERE key=1")))
    (exec! c "INSERT INTO scalar_source VALUES (1),(2)")
    (exec! c (str "INSERT INTO scalar_target AS t VALUES (1,'discard') "
                  "ON CONFLICT (key) DO UPDATE SET val="
                  "(SELECT v FROM scalar_source WHERE v=t.key)::text"))
    (is (= "1" (one c "SELECT val FROM scalar_target WHERE key=1"))
        "a scalar subquery sees the aliased conflict target row")
    (exec! c (str "INSERT INTO scalar_target VALUES (1,'discard') "
                  "ON CONFLICT (key) DO UPDATE SET val=(SELECT excluded.key)::text"))
    (is (= "1" (one c "SELECT val FROM scalar_target WHERE key=1"))
        "a scalar subquery sees the qualified EXCLUDED row")
    (exec! c (str "INSERT INTO scalar_target SELECT 1,'discard' "
                  "ON CONFLICT (key) DO UPDATE SET val="
                  "(SELECT v FROM scalar_source WHERE false)"))
    (is (nil? (one c "SELECT val FROM scalar_target WHERE key=1"))
        "INSERT SELECT assigns scalar NULL instead of keeping the old value")
    (is (= "21000"
           (sqlstate c (str "UPDATE scalar_target SET val="
                            "(SELECT v FROM scalar_source)::text WHERE key=1"))))
    (is (nil? (one c "SELECT val FROM scalar_target WHERE key=1"))
        "a cardinality error leaves the existing row unchanged")))

(deftest update-uncorrelated-scalar-subquery-is-initialized-once
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE scalar_dml_rows (k int, v double precision)")
    (exec! c "INSERT INTO scalar_dml_rows VALUES (1,0),(1,0),(1,0)")
    (let [calls (atom 0)]
      (with-redefs [fns/sql-random (fn [] (double (swap! calls inc)))]
        (exec! c "UPDATE scalar_dml_rows SET v=(SELECT random())")
        (is (= ["1" "1" "1"]
               (col c 2 "SELECT k,v FROM scalar_dml_rows ORDER BY db_id")))
        (is (= 1 @calls) "an uncorrelated scalar is initialized once per statement")
        (exec! c "UPDATE scalar_dml_rows SET v=(SELECT random())")
        (is (= ["2" "2" "2"]
               (col c 2 "SELECT k,v FROM scalar_dml_rows ORDER BY db_id")))
        (is (= 2 @calls) "a later statement gets a fresh initialization")))))

(deftest distinct-dml-scalar-occurrences-have-distinct-initplans
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE scalar_dml_occurrences (a double precision, b double precision)")
    (exec! c "INSERT INTO scalar_dml_occurrences VALUES (0,0),(0,0)")
    (let [calls (atom 0)]
      (with-redefs [fns/sql-random (fn [] (double (swap! calls inc)))]
        (exec! c (str "UPDATE scalar_dml_occurrences "
                      "SET a=(SELECT random()), b=(SELECT random())"))
        (is (= [["1" "2"] ["1" "2"]]
               (rows c "SELECT a,b FROM scalar_dml_occurrences ORDER BY db_id")))
        (is (= 2 @calls) "each syntactic scalar occurrence owns one InitPlan")))))

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
