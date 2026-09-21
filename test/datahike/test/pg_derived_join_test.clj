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

(deftest unsupported-derived-shapes-fail-before-datalog
  (testing "duplicate projected names do not leak a Datahike schema update"
    (let [r (run "SELECT count(*) FROM (SELECT 1 AS x, 2 AS x) s")]
      (is (= "0A000" (.-sqlstate ^PgWireServer$QueryResult r)))
      (is (re-find #"duplicate columns" (.-error ^PgWireServer$QueryResult r))))))

(deftest a-predicate-only-outer-join-is-a-nested-loop
  ;; This was refused with 0A000 ("non-equality outer join conditions
  ;; are not supported"). It is now lowered as a nested loop -- see
  ;; datahike.test.pg-outer-join-on-test for the full shape coverage.
  (seed!)
  (is (= [["1" "10"] ["1" "20"] ["2" "10"] ["2" "20"]]
         (rows "SELECT t.id, c.v FROM t LEFT JOIN c ON true ORDER BY 1,2")))
  (is (= [["1" nil] ["2" nil]]
         (rows "SELECT t.id, c.v FROM t LEFT JOIN c ON (c.v > 1000) ORDER BY 1,2"))))

(deftest using-and-natural-joins-are-joins
  ;; `USING (c)` and NATURAL are conditions PostgreSQL synthesises before
  ;; planning, and the pair of columns becomes ONE output column
  ;; (parse_clause.c). Neither happened here: the join was a CROSS
  ;; PRODUCT, and the merged name was reported ambiguous. Expectations
  ;; are PostgreSQL 17's.
  (run "CREATE TABLE ug_a (id int, x int, k int)")
  (run "CREATE TABLE ug_b (id int, y int, k int)")
  (run "INSERT INTO ug_a VALUES (1,10,7),(2,20,8),(3,30,9)")
  (run "INSERT INTO ug_b VALUES (1,100,7),(2,200,99)")
  (testing "the merged columns are the join condition"
    (is (= "2" (v "SELECT count(*) FROM ug_a JOIN ug_b USING (id)")))
    (is (= "1" (v "SELECT count(*) FROM ug_a JOIN ug_b USING (id, k)")))
    (is (= "1" (v "SELECT count(*) FROM ug_a NATURAL JOIN ug_b"))))
  (testing "the merged column is one column, the left side's"
    (is (= [["1" "10" "100"] ["2" "20" "200"]]
           (rows "SELECT id, x, y FROM ug_a JOIN ug_b USING (id) ORDER BY id")))
    (is (= [["1" "7" "10" "100"]]
           (rows "SELECT id, k, x, y FROM ug_a NATURAL JOIN ug_b"))))
  (testing "SELECT * emits it once, first, then each relation's rest"
    (is (= [["1" "10" "7" "100" "7"] ["2" "20" "8" "200" "99"]]
           (rows "SELECT * FROM ug_a JOIN ug_b USING (id) ORDER BY id")))
    (is (= [["1" "7" "10" "100"]] (rows "SELECT * FROM ug_a NATURAL JOIN ug_b")))
    (is (= [["1" "10" "7" "100" "7"] ["2" "20" "8" "200" "99"]]
           (rows "SELECT * FROM ug_a a1 JOIN ug_b b1 USING (id) ORDER BY id"))))
  (testing "a qualified reference still names its own side"
    (is (= [["1" "1"] ["2" "2"]]
           (rows "SELECT ug_a.id, ug_b.id FROM ug_a JOIN ug_b USING (id) ORDER BY 1"))))
  (testing "an ON join is unchanged: both columns, both emitted"
    (is (= [["1" "10" "7" "1" "100" "7"] ["2" "20" "8" "2" "200" "99"]]
           (rows "SELECT * FROM ug_a JOIN ug_b ON ug_a.id = ug_b.id ORDER BY ug_a.id")))))

(deftest a-column-alias-list-renames-a-derived-tables-columns
  ;; `FROM (SELECT …) AS s(a,b)` RENAMES the relation's columns, left to
  ;; right: `SELECT a FROM (SELECT 1 AS x) AS s(a)` reads the renamed
  ;; column, and `x` is gone. Only the `FROM (VALUES …) AS v(a,b)` path
  ;; passed those aliases to the materialiser, so every other derived
  ;; table answered 42703 for its own declared name.
  ;;
  ;; Naming more columns than the relation has is 42P10; naming fewer
  ;; leaves the rest under their own names. Expectations are a
  ;; PostgreSQL 17 oracle's.
  (testing "the alias list renames"
    (is (= "1" (v "SELECT a FROM (SELECT 1 AS x) AS s(a)")))
    (is (= "1" (v "SELECT s.a FROM (SELECT 1 AS x) AS s(a)")))
    (is (= [["1" "2"]] (rows "SELECT a, b FROM (SELECT 1 AS x, 2 AS y) AS s(a,b)"))))
  (testing "fewer aliases than columns leaves the rest alone"
    (is (= [["1" "2"]] (rows "SELECT a, y FROM (SELECT 1 AS x, 2 AS y) AS s(a)"))))
  (testing "more aliases than columns is 42P10"
    (let [r (run "SELECT * FROM (SELECT 1 AS x) AS s(a,b)")]
      (is (= "42P10" (.-sqlstate ^PgWireServer$QueryResult r)))
      (is (re-find #"has 1 columns available but 2 columns specified"
                   (.-error ^PgWireServer$QueryResult r)))))
  (testing "a derived table over a real relation, and VALUES as before"
    (seed!)
    (is (= [["1" "10"] ["2" "20"]]
           (rows "SELECT k, w FROM (SELECT tid, v FROM c) AS s(k,w) ORDER BY 1")))
    (is (= [["1" "2"]] (rows "SELECT * FROM (VALUES (1,2)) AS v(a,b)")))))

