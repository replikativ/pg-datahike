(ns datahike.test.pg-derived-join-test
  "A derived table on the RIGHT of a join, and `SELECT *` across joins.

   Two defects that hid behind each other:

   `FROM (SELECT …) s JOIN t ON …` worked, but `FROM t JOIN (SELECT …) s
   ON …` — the shape every ORM emits — raised `missing FROM-clause
   entry for table \"s\"`. The join branch registered only the storage
   namespace, never the user's alias; the from-item path always
   registered both.

   And `SELECT *` expanded ONLY the default table, so
   `SELECT * FROM t JOIN c` returned a silently narrower row rather
   than an error.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *h* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*h* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *h* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- v [sql] (ffirst (rows sql)))

(defn- seed! []
  (run "CREATE TABLE t (id int, n int)")
  (run "INSERT INTO t VALUES (1,2),(2,3)")
  (run "CREATE TABLE c (tid int, v int)")
  (run "INSERT INTO c VALUES (1,10),(2,20)"))

(deftest derived-table-on-the-right-of-a-join
  (seed!)
  (testing "explicit JOIN ... ON — the ORM shape"
    (is (= [["1" "10"] ["2" "20"]]
           (rows "SELECT t.id, s.v FROM t JOIN (SELECT tid, v FROM c) s
                    ON s.tid = t.id ORDER BY t.id"))))

  (testing "comma join"
    (is (= [["1" "10"] ["1" "20"] ["2" "10"] ["2" "20"]]
           (rows "SELECT t.id, s.v FROM t, (SELECT v FROM c) s
                  ORDER BY t.id, s.v"))))

  (testing "count over it"
    (is (= "4" (v "SELECT count(*) FROM t, (SELECT v FROM c) s"))))

  (testing "the derived table on the LEFT still works"
    (is (= [["10"] ["20"]]
           (rows "SELECT s.v FROM (SELECT tid, v FROM c) s JOIN t
                    ON s.tid = t.id ORDER BY s.v")))))

(deftest select-star-expands-every-relation
  ;; Expanded only the default table, so a join returned a narrower row
  ;; with no indication anything was missing.
  (seed!)
  (testing "an ordinary join"
    (is (= [["1" "2" "1" "10"] ["2" "3" "2" "20"]]
           (rows "SELECT * FROM t JOIN c ON c.tid = t.id ORDER BY 1"))))

  (testing "a comma join"
    (is (= [["1" "2" "1" "10"] ["2" "3" "2" "20"]]
           (rows "SELECT * FROM t, c WHERE c.tid = t.id ORDER BY 1"))))

  (testing "over a derived table"
    (is (= [["1" "2" "1" "10"] ["2" "3" "2" "20"]]
           (rows "SELECT * FROM t JOIN (SELECT tid, v FROM c) s
                    ON s.tid = t.id ORDER BY 1"))))

  (testing "a single relation is unchanged"
    (is (= [["1" "2"] ["2" "3"]] (rows "SELECT * FROM t ORDER BY 1")))))

(deftest redundant-parentheses-around-derived-relations
  (is (= [["1"]]
         (rows "SELECT * FROM ((SELECT 1 AS x)) ss")))
  (is (= [["1" "2"]]
         (rows (str "SELECT * FROM ((SELECT 1 AS x)), "
                    "((SELECT * FROM ((SELECT 2 AS y))))")))))

(deftest unsupported-derived-and-outer-join-shapes-fail-before-datalog
  (testing "duplicate projected names do not leak a Datahike schema update"
    (let [r (run "SELECT count(*) FROM (SELECT 1 AS x, 2 AS x) s")]
      (is (= "0A000" (.-sqlstate ^PgWireServer$QueryResult r)))
      (is (re-find #"duplicate columns" (.-error ^PgWireServer$QueryResult r)))))
  (testing "a predicate-only outer join does not construct nil rule variables"
    (seed!)
    (let [r (run "SELECT t.id, c.v FROM t LEFT JOIN c ON true")]
      (is (= "0A000" (.-sqlstate ^PgWireServer$QueryResult r)))
      (is (re-find #"non-equality outer join"
                   (.-error ^PgWireServer$QueryResult r))))))
