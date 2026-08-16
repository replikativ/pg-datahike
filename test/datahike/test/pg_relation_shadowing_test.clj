(ns datahike.test.pg-relation-shadowing-test
  "A CTE, derived table or table function whose name collides with a real
   table must SHADOW it, not merge with it.

   Speculative relations are materialised into a db as ordinary
   attributes, and the namespace used to be the relation's own name. So
   `WITH t AS (...)` wrote `:t/col` and `:t/db-row-exists` into exactly
   the namespace a real table `t` already occupied, and namespace is what
   every reader keys on. The two relations then merged:

     - scans saw the UNION of both row sets (the row-existence marker
       matched both), so an extra all-NULL row appeared and `count(*)`
       was wrong;
     - `SELECT *` listed the union of both column lists;
     - a reference to a base-table column through the CTE returned
       base-table data, where PostgreSQL raises 42703;
     - and because a SQL PRIMARY KEY becomes `:db.unique/identity`, a
       CTE row whose key matched a base row UPSERTED onto it — so two
       CTE rows with the same key collapsed into one.

   Worst of all, UPDATE and DELETE re-translate their WHERE clause at
   execute time, where the merge made a CTE reference resolve to the
   real table:

       WITH t AS (SELECT 99 AS id)
       DELETE FROM t WHERE id IN (SELECT id FROM t)

   deleted every row of `t` — persisted — where PostgreSQL deletes none.

   The fix gives each speculative relation a synthetic namespace and
   keeps the user's name as an ALIAS, which routes the whole thing
   through the same resolution path as `FROM emp e`.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)]
          ;; Seeded through SQL so the row-existence marker is installed —
          ;; the collision is between markers, so a d/transact-seeded
          ;; table would not reproduce it.
          (.execute *handler* "CREATE TABLE t (id int PRIMARY KEY, name text)")
          (.execute *handler* "INSERT INTO t VALUES (1,'base')")
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (.execute *handler* sql))))

(defn- v [sql] (ffirst (rows sql)))

(defn- tag [sql]
  (.-commandTag ^PgWireServer$QueryResult (.execute *handler* sql)))

;; ---------------------------------------------------------------------------
;; The data-destruction cases — highest priority
;; ---------------------------------------------------------------------------

(deftest cte-shadowing-does-not-delete-the-base-table
  (testing "the CTE supplies the id set; no base row matches it"
    (.execute *handler* "INSERT INTO t VALUES (2,'b'), (5,'z')")
    (is (= "DELETE 0"
           (tag "WITH t AS (SELECT 99 AS id) DELETE FROM t WHERE id IN (SELECT id FROM t)")))
    (is (= [["1"] ["2"] ["5"]] (rows "SELECT id FROM t ORDER BY id"))
        "every row must survive — this used to empty the table")))

(deftest cte-shadowing-does-not-update-the-whole-base-table
  (.execute *handler* "INSERT INTO t VALUES (2,'b'), (5,'z')")
  (is (= "UPDATE 1"
         (tag "WITH t AS (SELECT 1 AS id) UPDATE t SET name='UP' WHERE id IN (SELECT id FROM t)")))
  (is (= [["UP"] ["b"] ["z"]] (rows "SELECT name FROM t ORDER BY id"))
      "only the row the CTE selected"))

(deftest cte-rows-do-not-upsert-onto-base-rows
  (testing "a SQL PRIMARY KEY is :db.unique/identity, so a colliding CTE
            row used to overwrite the base row instead of being its own"
    (is (= [["1" "base"]] (rows "SELECT id, name FROM t ORDER BY id")))
    (is (= [["CTE"]]
           (rows "WITH t AS (SELECT 1 AS id, 'CTE' AS name) SELECT name FROM t")))
    (is (= [["1" "base"]] (rows "SELECT id, name FROM t ORDER BY id"))
        "the base row must be untouched by having read the CTE")))

(deftest two-cte-rows-with-the-same-key-stay-two-rows
  (is (= #{["X"] ["Y"]}
         (set (rows (str "WITH t AS (SELECT 1 AS id,'X' AS name "
                         "UNION ALL SELECT 1 AS id,'Y' AS name) SELECT name FROM t"))))))

;; ---------------------------------------------------------------------------
;; Read-side shadowing
;; ---------------------------------------------------------------------------

(deftest cte-shadows-a-same-named-table
  (testing "only the CTE's rows"
    (is (= [["7"]] (rows "WITH t AS (SELECT 7 AS z) SELECT z FROM t")))
    (is (= [["1"]] (rows "WITH t AS (SELECT 7 AS z) SELECT count(*) FROM t"))
        "the base row used to be counted too"))
  (testing "only the CTE's columns"
    (is (= [["7"]] (rows "WITH t AS (SELECT 7 AS z) SELECT * FROM t")))))

(deftest a-non-colliding-cte-is-unaffected
  (is (= [["7"]] (rows "WITH w AS (SELECT 7 AS z) SELECT z FROM w")))
  (is (= [["7"]] (rows "WITH w AS (SELECT 7 AS z) SELECT z FROM w WHERE z = 7"))))

(deftest the-base-table-is-still-reachable-outside-the-with
  (is (= [["base"]] (rows "SELECT name FROM t")))
  (is (= [["1" "base"]] (rows "SELECT id, name FROM t"))))

(deftest cte-visible-to-an-inner-subquery
  (testing "PostgreSQL scopes a CTE to its own level and every inner one"
    (is (= [["5"]]
           (rows "WITH t AS (SELECT 5 AS z) SELECT z FROM t WHERE z IN (SELECT z FROM t)")))))

;; ---------------------------------------------------------------------------
;; The same bug through the other two materialisers
;; ---------------------------------------------------------------------------

(deftest derived-table-shadows-a-same-named-table
  (is (= [["1"]] (rows "SELECT x FROM (SELECT 1 AS x) AS t")))
  (is (= [["1"]] (rows "SELECT count(*) FROM (SELECT 1 AS x) AS t")))
  (testing "and a non-colliding alias still works"
    (is (= [["1"]] (rows "SELECT x FROM (SELECT 1 AS x) AS s")))
    (is (= [["4"]] (rows "SELECT s.a FROM (SELECT 4 AS a, 5 AS b) s")))))

(deftest table-function-shadows-a-same-named-table
  (is (= [["1"] ["2"] ["3"]] (rows "SELECT * FROM generate_series(1,3) AS t"))))

;; ---------------------------------------------------------------------------
;; SELECT * through an alias — the resolution bug the fix exposed, which
;; predates it: `default-table` is the ALIAS, and it was never resolved
;; to the relation.
;; ---------------------------------------------------------------------------

(deftest select-star-through-a-table-alias
  (testing "returned a row with zero columns"
    (is (= [["1" "base"]] (rows "SELECT * FROM t alias")))
    (is (= [["1" "base"]] (rows "SELECT * FROM t AS alias"))))
  (testing "the qualified form always worked and must keep working"
    (is (= [["1" "base"]] (rows "SELECT alias.* FROM t alias")))
    (is (= [["1" "base"]] (rows "SELECT * FROM t")))))

(deftest select-star-over-speculative-relations
  (is (= [["1" "2"]] (rows "SELECT * FROM (SELECT 1 AS x, 2 AS y) s")))
  (is (= [["1"] ["2"]] (rows "SELECT * FROM unnest(ARRAY[1,2]) AS u")))
  (is (= [["1"] ["2"] ["3"]] (rows "SELECT * FROM generate_series(1,3) AS g"))))

;; ---------------------------------------------------------------------------
;; Writes that should still work
;; ---------------------------------------------------------------------------

(deftest insert-select-from-a-non-colliding-cte
  (is (= "INSERT 0 1"
         (tag "WITH s AS (SELECT 9 AS id) INSERT INTO t (id, name) SELECT id, 'n' FROM s")))
  (is (= [["1"] ["9"]] (rows "SELECT id FROM t ORDER BY id"))))

(deftest delete-using-a-non-colliding-cte
  (.execute *handler* "INSERT INTO t VALUES (2,'b')")
  (is (= "DELETE 1"
         (tag "WITH s AS (SELECT 2 AS id) DELETE FROM t WHERE id IN (SELECT id FROM s)")))
  (is (= [["1"]] (rows "SELECT id FROM t ORDER BY id"))))
