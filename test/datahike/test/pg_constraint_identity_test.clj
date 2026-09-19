(ns datahike.test.pg-constraint-identity-test
  "CHECK and FOREIGN KEY constraint names are per table, as in PostgreSQL,
   and a table's constraints are dropped with it. Constraint entities were
   identified by name alone and survived DROP TABLE, so a recreated table
   of the same name enforced a CHECK it no longer declared.

   Expectations are PostgreSQL 17.7's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"ci" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server) *conn* conn] (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- jdbc ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/ci?user=x&password=x&sslmode=disable")))

(defn- run [^Connection c sql]
  (try
    (with-open [st (.createStatement c)]
      (if (.execute st sql)
        (with-open [rs (.getResultSet st)]
          (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc)))
        (.getUpdateCount st)))
    (catch SQLException e
      [:error (.getSQLState e)
       (.getMessage (.getServerErrorMessage ^org.postgresql.util.PSQLException e))])))

(deftest unnamed-checks-are-named-as-postgresql-names-them
  (with-open [c (jdbc)]
    (run c (str "CREATE TABLE na (a int, b int, CHECK (a > 0), CHECK (a < b), CHECK (b > 0), "
                "CHECK (a + b > 0), CONSTRAINT mine CHECK (a <> 7), c int CONSTRAINT cpos CHECK (c > 0))"))
    (is (= ["cpos" "mine" "na_a_check" "na_b_check" "na_check" "na_check1"]
           (run c "SELECT conname FROM pg_constraint WHERE contype = 'c' ORDER BY conname")))))

(deftest constraint-names-are-per-table
  (with-open [c (jdbc)]
    (run c "CREATE TABLE ca (x int CONSTRAINT pos CHECK (x > 0))")
    (run c "CREATE TABLE cb (y int, CONSTRAINT pos CHECK (y > 0))")
    (is (= [:error "23514" "new row for relation \"ca\" violates check constraint \"pos\""]
           (run c "INSERT INTO ca VALUES (-1)")))
    (is (= [:error "23514" "new row for relation \"cb\" violates check constraint \"pos\""]
           (run c "INSERT INTO cb VALUES (-1)")))))

(deftest drop-table-drops-its-constraints
  (with-open [c (jdbc)]
    (run c "CREATE TABLE p (id int PRIMARY KEY)")
    (run c "CREATE TABLE cz (a int CHECK (a > 0), p int REFERENCES p (id))")
    (run c "DROP TABLE cz")
    (run c "CREATE TABLE cz (a int, p int)")
    (testing "neither the CHECK nor the FOREIGN KEY of the dropped table applies"
      (is (= 1 (run c "INSERT INTO cz VALUES (-5, 42)"))))
    (is (= [] (run c "SELECT conname FROM pg_constraint WHERE contype IN ('c', 'f')")))))

(deftest generated-names-follow-choose-constraint-name
  (with-open [c (jdbc)]
    (run c "CREATE TABLE p (id int PRIMARY KEY, k int UNIQUE)")
    (testing "numbered past any constraint of the table, CHECK or FOREIGN KEY"
      (run c (str "CREATE TABLE ch (a int REFERENCES p (id), b int REFERENCES p (id), "
                  "CONSTRAINT ch_a_fkey CHECK (a > 0), c int, FOREIGN KEY (a) REFERENCES p (k))"))
      (is (= ["ch_a_fkey" "ch_a_fkey1" "ch_a_fkey2" "ch_b_fkey"]
             (run c "SELECT conname FROM pg_constraint WHERE contype IN ('c', 'f') ORDER BY conname"))))
    (testing "shortened to 63 bytes, the longer part first"
      (run c (str "CREATE TABLE rv3_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa "
                  "(bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb int "
                  "CHECK (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb > 0), "
                  "CHECK (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb < 9))"))
      (is (= ["rv3_aaaaaaaaaaaaaaaaaaaaaaaa_bbbbbbbbbbbbbbbbbbbbbbbbbbb_check1"
              "rv3_aaaaaaaaaaaaaaaaaaaaaaaa_bbbbbbbbbbbbbbbbbbbbbbbbbbbb_check"]
             (run c "SELECT conname FROM pg_constraint WHERE conname LIKE 'rv3_%' ORDER BY conname"))))
    (testing "two constraints named alike"
      (is (= [:error "42710" "check constraint \"c1\" already exists"]
             (run c "CREATE TABLE d (a int CONSTRAINT c1 CHECK (a > 0), b int CONSTRAINT c1 CHECK (b > 0))"))))))

(deftest dropping-a-referenced-table
  (with-open [c (jdbc)]
    (run c "CREATE TABLE p (id int PRIMARY KEY)")
    (run c "CREATE TABLE ch (id int, p int REFERENCES p (id))")
    (is (= [:error "2BP01" "cannot drop table p because other objects depend on it"]
           (run c "DROP TABLE p")))
    (testing "CASCADE drops the referencing foreign key, not the referencing table"
      (is (= 0 (run c "DROP TABLE p CASCADE")))
      (is (= 1 (run c "INSERT INTO ch VALUES (1, 99)")))
      (is (= [] (run c "SELECT conname FROM pg_constraint WHERE contype = 'f'"))))))

(deftest legacy-name-keyed-constraints-are-migrated
  (with-open [c (jdbc)]
    (run c "CREATE TABLE live (a int)")
    ;; A database written before constraint-key: entities keyed by name
    ;; alone, one of a table that DROP TABLE left behind.
    (d/transact *conn* [{:pg/check-name "live_a_check" :pg/check-table "live" :pg/check-expr "a > 0"}
                        {:pg/check-name "gone_a_check" :pg/check-table "gone" :pg/check-expr "a > 0"}])
    (#'pg/ensure-pg-schema! *conn*)
    (let [db (d/db *conn*)]
      (is (= #{["live_a_check" "live"]}
             (d/q '{:find [?n ?t] :where [[?e :pg/check-conname ?n] [?e :pg/check-table ?t]]} db)))
      (is (empty? (d/q '{:find [?e] :where [[?e :pg/check-name _]]} db))))
    (testing "the live constraint is still enforced; the orphan is gone"
      (run c "CREATE TABLE gone (a int)")
      (is (= 1 (run c "INSERT INTO gone VALUES (-1)")))
      (is (= "23514" (second (run c "INSERT INTO live VALUES (-1)")))))))
