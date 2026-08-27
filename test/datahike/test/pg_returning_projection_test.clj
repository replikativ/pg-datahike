(ns datahike.test.pg-returning-projection-test
  "Application-facing RETURNING semantics admitted from PostgreSQL 17's
   returning.sql simple-cases slice (lines 7-21)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.fns :as fns])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql]
  (.execute *handler* sql))

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))

(deftest postgres-returning-simple-projection-slice
  (run "CREATE TABLE rt (id serial, s text, n int DEFAULT 42)")
  (testing "INSERT RETURNING expands star and evaluates an aliased expression"
    (let [r (run (str "INSERT INTO rt (s,n) VALUES "
                      "('A',DEFAULT),(upper('b'),10) "
                      "RETURNING *, id+n AS total"))]
      (is (= ["id" "s" "n" "total"] (vec (.-columnNames r))))
      (is (= [["1" "A" "42" "43"] ["2" "B" "10" "12"]]
             (mapv vec (.-rows r))))
      (is (= "INSERT 0 2" (.-commandTag r)))))
  (testing "UPDATE DEFAULT and table-star use the post-update row"
    (let [r (run "UPDATE rt SET s=lower(s), n=DEFAULT RETURNING rt.*, id+n AS sum13")]
      (is (= ["id" "s" "n" "sum13"] (vec (.-columnNames r))))
      (is (= [["1" "a" "42" "43"] ["2" "b" "42" "44"]]
             (mapv vec (.-rows r))))
      (is (= "UPDATE 2" (.-commandTag r)))))
  (testing "DELETE RETURNING evaluates functions over the pre-delete row"
    (let [r (run "DELETE FROM rt WHERE id > 1 RETURNING n,s,id,least(id,n)")]
      (is (= ["n" "s" "id" "least"] (vec (.-columnNames r))))
      (is (= [["42" "b" "2" "2"]] (mapv vec (.-rows r))))
      (is (= "DELETE 1" (.-commandTag r))))
    (is (= [["1" "a" "42"]] (rows "SELECT * FROM rt")))))

(deftest returning-subqueries-use-the-command-snapshot
  (run "CREATE TABLE returning_snapshot (id int, v int)")
  (run "INSERT INTO returning_snapshot VALUES (1,10),(2,20)")
  (testing "UPDATE exposes new row columns while its subquery scans old rows"
    (is (= [["1" "11" "30"] ["2" "21" "30"]]
           (mapv vec
                 (.-rows ^PgWireServer$QueryResult
                  (run (str "UPDATE returning_snapshot SET v=v+1 "
                            "RETURNING id,v,(SELECT sum(v) "
                            "FROM returning_snapshot)")))))))
  (testing "INSERT's subquery cannot see the row inserted by its command"
    (is (= [["3" "30" "2"]]
           (mapv vec
                 (.-rows ^PgWireServer$QueryResult
                  (run (str "INSERT INTO returning_snapshot VALUES (3,30) "
                            "RETURNING id,v,(SELECT count(*) "
                            "FROM returning_snapshot)")))))))
  (testing "DELETE uses the same pre-command database for its row and scans"
    (is (= [["1" "11" "3"]]
           (mapv vec
                 (.-rows ^PgWireServer$QueryResult
                  (run (str "DELETE FROM returning_snapshot WHERE id=1 "
                            "RETURNING id,v,(SELECT count(*) "
                            "FROM returning_snapshot)"))))))))

(deftest returning-scalar-subqueries-correlate-to-the-target-row
  (run "CREATE TABLE returning_row (id int, v int)")
  (testing "INSERT and UPDATE expose each post-command target row"
    (is (= [["1" "10" "10"] ["2" "20" "20"]]
           (rows (str "INSERT INTO returning_row AS r VALUES (1,10),(2,20) "
                      "RETURNING id,(SELECT r.v),(SELECT v)"))))
    (is (= [["1" "11" "11"] ["2" "21" "21"]]
           (rows (str "UPDATE returning_row SET v=v+1 "
                      "RETURNING id,(SELECT returning_row.v),(SELECT v)")))))
  (testing "DELETE exposes each pre-command target row"
    (is (= [["1" "11" "11"] ["2" "21" "21"]]
           (rows (str "DELETE FROM returning_row "
                      "RETURNING id,(SELECT returning_row.v),(SELECT v)")))))
  (testing "scalar RETURNING metadata retains the analyzed PostgreSQL type"
    (run "INSERT INTO returning_row VALUES (3,30)")
    (is (= [23 20]
           (vec (.-columnOids
                 ^PgWireServer$QueryResult
                 (run (str "UPDATE returning_row SET v=v "
                           "RETURNING (SELECT returning_row.v),(SELECT 1::bigint)"))))))))

(deftest failing-returning-does-not-commit-a-direct-write
  (run "CREATE TABLE returning_atomic (id int, v int)")
  (run "CREATE TABLE returning_many (v int)")
  (run "INSERT INTO returning_many VALUES (1),(2)")
  (let [insert-result
        (run (str "INSERT INTO returning_atomic VALUES (1,10) "
                  "RETURNING (SELECT v FROM returning_many)"))]
    (is (= "21000" (.-sqlstate insert-result)))
    (is (= [] (rows "SELECT id FROM returning_atomic"))))
  (run "INSERT INTO returning_atomic VALUES (1,10)")
  (let [update-result
        (run (str "UPDATE returning_atomic SET v=20 "
                  "RETURNING (SELECT v FROM returning_many)"))]
    (is (= "21000" (.-sqlstate update-result)))
    (is (= [["10"]] (rows "SELECT v FROM returning_atomic")))))

(deftest returning-commits-the-same-volatile-upsert-value-it-reports
  (run "CREATE TABLE returning_upsert (id int PRIMARY KEY, v double precision)")
  (run "INSERT INTO returning_upsert VALUES (1,0)")
  (let [calls (atom 0)]
    (with-redefs [fns/sql-random (fn [] (double (swap! calls inc)))]
      (is (= [["1"]]
             (rows (str "INSERT INTO returning_upsert VALUES (1,0) "
                        "ON CONFLICT(id) DO UPDATE SET v=random() RETURNING v"))))
      (is (= [["1"]] (rows "SELECT v FROM returning_upsert")))
      (is (= 1 @calls) "the ON CONFLICT transaction function executes once"))))

(deftest returning-subquery-snapshot-in-an-explicit-transaction
  (run "CREATE TABLE returning_tx_snapshot (id int, v int)")
  (run "INSERT INTO returning_tx_snapshot VALUES (1,10)")
  (run "BEGIN")
  (run "INSERT INTO returning_tx_snapshot VALUES (2,20)")
  (testing "prior commands are visible, but the current command is not"
    (is (= [["3" "30" "2"]]
           (mapv vec
                 (.-rows ^PgWireServer$QueryResult
                  (run (str "INSERT INTO returning_tx_snapshot VALUES (3,30) "
                            "RETURNING id,v,(SELECT count(*) "
                            "FROM returning_tx_snapshot)")))))))
  (run "ROLLBACK"))

(deftest target-alias-hides-original-relation-name
  (run "CREATE TABLE dt (id int, n int)")
  (run "INSERT INTO dt VALUES (1,10),(2,30)")
  (let [r (run "DELETE FROM dt d WHERE dt.n > 20")]
    (is (= "42P01" (.-sqlstate r)))
    (is (= [["1"] ["2"]] (rows "SELECT id FROM dt ORDER BY id"))))
  (is (= "DELETE 1" (.-commandTag ^PgWireServer$QueryResult
                     (run "DELETE FROM dt d WHERE d.n > 20"))))
  (run "INSERT INTO dt VALUES (3,40)")
  (let [r (run "UPDATE dt d SET n=50 WHERE dt.id=3")]
    (is (= "42P01" (.-sqlstate r)))
    (is (= [["40"]] (rows "SELECT n FROM dt WHERE id=3"))))
  (testing "a nested SELECT has its own relation-name scope"
    (is (= "DELETE 1"
           (.-commandTag ^PgWireServer$QueryResult
            (run (str "DELETE FROM dt d WHERE d.id=3 AND EXISTS "
                      "(SELECT 1 FROM dt WHERE dt.id=d.id)")))))))

(deftest update-from-missing-relation-is-a-postgres-error
  (run "CREATE TABLE uf (id int, n int)")
  (let [r (run "UPDATE uf SET n=src.n FROM absent_source src WHERE uf.id=src.id")]
    (is (= "42P01" (.-sqlstate r)))
    (is (re-find #"relation \"absent_source\" does not exist" (.-error r)))))

(deftest commit-of-failed-transaction-rolls-back-and-recovers
  (run "CREATE TABLE failed_tx (id int)")
  (run "BEGIN")
  (run "INSERT INTO failed_tx VALUES (1)")
  (is (= "22012" (.-sqlstate ^PgWireServer$QueryResult (run "SELECT 1/0"))))
  (is (= "ROLLBACK" (.-commandTag ^PgWireServer$QueryResult (run "COMMIT"))))
  (is (= [] (rows "SELECT id FROM failed_tx")))
  (is (= [["1"]] (rows "SELECT 1"))))

(deftest postgres-transaction-begin-end-abort-slice
  (run "BEGIN")
  (run "CREATE TABLE xact_slice (a int)")
  (run "INSERT INTO xact_slice VALUES (56),(777)")
  (is (= "COMMIT" (.-commandTag ^PgWireServer$QueryResult (run "END"))))
  (is (= [["777"]] (rows "SELECT a FROM xact_slice WHERE a > 100")))
  (run "BEGIN")
  (run "CREATE TABLE disappear_slice (a int)")
  (run "DELETE FROM xact_slice")
  (is (= [] (rows "SELECT a FROM xact_slice")))
  (is (= "ROLLBACK" (.-commandTag ^PgWireServer$QueryResult (run "ABORT"))))
  (is (= [["56"] ["777"]] (rows "SELECT a FROM xact_slice ORDER BY a")))
  (is (= "42P01" (.-sqlstate ^PgWireServer$QueryResult
                  (run "SELECT * FROM disappear_slice")))))

(deftest postgres-failed-transaction-savepoint-slice
  (run "BEGIN")
  (run "SAVEPOINT keep_me")
  (is (= "22012" (.-sqlstate ^PgWireServer$QueryResult (run "SELECT 1/0"))))
  (is (= "25P02" (.-sqlstate ^PgWireServer$QueryResult
                  (run "RELEASE SAVEPOINT keep_me"))))
  (is (= "ROLLBACK" (.-commandTag ^PgWireServer$QueryResult
                     (run "ROLLBACK TO SAVEPOINT keep_me"))))
  (is (= [["1"]] (rows "SELECT 1")))
  (run "COMMIT"))
