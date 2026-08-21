(ns datahike.test.pg-correlated-where-cte-test
  "Three shapes whose inner part depends on something outside it, each of
   which used to answer without ever looking at that dependency:

     WHERE <op> (SELECT agg(…) WHERE inner.x = outer.x)
         evaluated ONCE at translate time and folded to a constant --
         `WHERE 0 > (SELECT min(i) FROM t2 WHERE t2.id <> t.id)` became
         `0 > -3`, the GLOBAL minimum, so every row passed.

     WITH a AS (…), b AS (SELECT … FROM a)
         `relation \"a\" does not exist` -- the CTE name->namespace map
         was only bound after the whole WITH list had been folded.

     FROM t, generate_series(1, 3) g
         `column \"g\" does not exist` -- only the FROM-ITEM position
         knew how to materialise a set-returning function, and the join
         position left its alias unregistered.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn cw-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"cw" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each cw-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/cw?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

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

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE t (id int, n int, s text)")
  (exec! c "INSERT INTO t VALUES (1,10,'a'),(2,NULL,'b'),(3,30,NULL)")
  ;; id 2 has no children: the row every correlated aggregate has to
  ;; answer 0 (count) or NULL (max) for.
  (exec! c "CREATE TABLE c (tid int, v int)")
  (exec! c "INSERT INTO c VALUES (1,7),(1,8),(3,9)"))

(deftest correlated-scalar-subquery-in-where
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["1"] (col c 1 (str "SELECT id FROM t WHERE (SELECT count(*) FROM c "
                               "WHERE c.tid = t.id) > 1 ORDER BY id"))))
    (is (= ["3"] (col c 1 (str "SELECT id FROM t WHERE (SELECT max(v) FROM c "
                               "WHERE c.tid = t.id) = 9 ORDER BY id"))))
    (testing "an empty inner is NULL for max and 0 for count"
      (is (= ["2"] (col c 1 (str "SELECT id FROM t WHERE (SELECT max(v) FROM c "
                                 "WHERE c.tid = t.id) IS NULL ORDER BY id"))))
      (is (= ["2"] (col c 1 (str "SELECT id FROM t WHERE (SELECT count(*) FROM c "
                                 "WHERE c.tid = t.id) = 0 ORDER BY id")))))
    (testing "an outer column on the other side of the comparison"
      (is (= ["1" "3"] (col c 1 (str "SELECT id FROM t WHERE n > (SELECT min(v) FROM c "
                                     "WHERE c.tid = t.id) ORDER BY id")))))
    (testing "NOT keeps only the rows where the predicate is FALSE"
      ;; id 2's subquery is 0, so `0 > 1` is FALSE and NOT keeps it.
      (is (= ["2" "3"] (col c 1 (str "SELECT id FROM t WHERE NOT ((SELECT count(*) "
                                     "FROM c WHERE c.tid = t.id) > 1) ORDER BY id")))))
    (testing "the SELECT-list form still agrees"
      (is (= [["1" "2"] ["2" "0"] ["3" "1"]]
             (rows c (str "SELECT id, (SELECT count(*) FROM c WHERE c.tid = t.id) "
                          "FROM t ORDER BY id")))))))

(deftest cte-referencing-an-earlier-cte
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [["1" "20"] ["2" nil] ["3" "60"]]
           (rows c (str "WITH a AS (SELECT id, n AS v FROM t), "
                        "b AS (SELECT id, v*2 AS w FROM a) "
                        "SELECT id, w FROM b ORDER BY id"))))
    (testing "three deep"
      (is (= [["1" "21"] ["2" nil] ["3" "61"]]
             (rows c (str "WITH a AS (SELECT id, n AS v FROM t), "
                          "b AS (SELECT id, v*2 AS w FROM a), "
                          "d AS (SELECT id, w+1 AS z FROM b) "
                          "SELECT id, z FROM d ORDER BY id")))))
    (testing "and both CTEs joined in the outer query"
      (is (= [["1" "10" "20"] ["2" nil nil] ["3" "30" "60"]]
             (rows c (str "WITH a AS (SELECT id, n AS v FROM t), "
                          "b AS (SELECT id, v*2 AS w FROM a) "
                          "SELECT a.id, a.v, b.w FROM a JOIN b ON a.id = b.id "
                          "ORDER BY a.id")))))))

(deftest set-returning-function-in-join-position
  (with-open [c (jdbc)]
    (seed! c)
    (testing "the alias also names the single column, as in PostgreSQL"
      (is (= [["1" "1"] ["1" "2"] ["2" "1"] ["2" "2"] ["3" "1"] ["3" "2"]]
             (rows c "SELECT t.id, g FROM t, generate_series(1,2) g ORDER BY 1,2"))))
    (testing "AS s(x) renames the column"
      (is (= [["1" "1"] ["1" "2"] ["2" "1"] ["2" "2"] ["3" "1"] ["3" "2"]]
             (rows c "SELECT t.id, x FROM t, generate_series(1,2) AS s(x) ORDER BY 1,2"))))
    (testing "the cross product exists even when nothing references it"
      ;; The alias was never registered, so no entity var was created for
      ;; it and the row-marker anchor pass never saw the relation:
      ;; count(*) answered t's row count.
      (is (= ["9"] (col c 1 "SELECT count(*) FROM t, generate_series(1,3) g"))))
    (testing "unnest too"
      (is (= [["1" "7"] ["1" "8"] ["2" "7"] ["2" "8"] ["3" "7"] ["3" "8"]]
             (rows c "SELECT t.id, u FROM t, unnest(ARRAY[7,8]) u ORDER BY 1,2"))))
    (testing "a CORRELATED argument still takes the per-outer-row path"
      (is (= [["1" "1"] ["2" "1"] ["2" "2"] ["3" "1"] ["3" "2"] ["3" "3"]]
             (rows c (str "SELECT t.id, g FROM t, LATERAL generate_series(1, t.id) g "
                          "ORDER BY 1,2")))))))
