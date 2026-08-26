(ns datahike.test.pg-returning-projection-test
  "Application-facing RETURNING semantics admitted from PostgreSQL 17's
   returning.sql simple-cases slice (lines 7-21)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
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
