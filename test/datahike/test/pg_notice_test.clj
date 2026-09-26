(ns datahike.test.pg-notice-test
  "NoticeResponse.

   PostgreSQL tells a client about things that are not errors -- a
   DROP IF EXISTS that skipped, a CREATE IF NOT EXISTS that found the
   object already there, later a plpgsql RAISE NOTICE. The wire layer
   had no way to send one, so all of it was silently dropped: 1352
   NOTICE / WARNING / INFO lines across 73 of PostgreSQL's own
   regression files, and every one of them a difference.

   On the wire a NoticeResponse is an ErrorResponse with a different
   type byte. What it is NOT is an error: it does not abort the
   statement and it does not put the transaction into the failed state.
   pgjdbc surfaces one as a `SQLWarning` on the statement.

   Wordings are a PostgreSQL 17 oracle's, including the one place its
   notice and its error disagree: `DROP FUNCTION IF EXISTS f(int)`
   warns about `f(pg_catalog.int4)` and errors about `f(integer)`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLWarning]))

(def ^:dynamic *port* nil)

(defn- nt-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"nt" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each nt-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/nt?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- warnings
  "Every warning `sql` produced, as strings, over the simple protocol."
  [^Connection c sql]
  (with-open [st (.createStatement c)]
    (.execute st sql)
    (loop [^SQLWarning w (.getWarnings st), out []]
      (if w (recur (.getNextWarning w) (conj out (.getMessage w))) out))))

(defn- prepared-warnings
  "The same, over the extended protocol, which buffers its responses
   differently and so has its own ordering to get right."
  [^Connection c sql]
  (with-open [st (.prepareStatement c sql)]
    (.execute st)
    (loop [^SQLWarning w (.getWarnings st), out []]
      (if w (recur (.getNextWarning w) (conj out (.getMessage w))) out))))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(deftest drop-if-exists-says-it-skipped
  (with-open [c (jdbc)]
    (is (= ["table \"nope_t\" does not exist, skipping"]
           (warnings c "DROP TABLE IF EXISTS nope_t")))
    (is (= ["view \"nope_v\" does not exist, skipping"]
           (warnings c "DROP VIEW IF EXISTS nope_v")))
    (is (= ["sequence \"nope_s\" does not exist, skipping"]
           (warnings c "DROP SEQUENCE IF EXISTS nope_s")))
    (is (= ["index \"nope_i\" does not exist, skipping"]
           (warnings c "DROP INDEX IF EXISTS nope_i")))
    (testing "a function's notice names pg_catalog types, unlike its error"
      (is (= ["function nope_f(pg_catalog.int4) does not exist, skipping"]
             (warnings c "DROP FUNCTION IF EXISTS nope_f(int)")))
      (is (= ["function nope_g(pg_catalog.text, pg_catalog.int8) does not exist, skipping"]
             (warnings c "DROP FUNCTION IF EXISTS nope_g(text, bigint)"))))))

(deftest one-notice-per-missing-name
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE present_t (i int)")
    (is (= ["table \"missing_a\" does not exist, skipping"
            "table \"missing_b\" does not exist, skipping"]
           (warnings c "DROP TABLE IF EXISTS missing_a, present_t, missing_b")))))

(deftest create-if-not-exists-says-it-skipped
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ine_t (i int)")
    (is (= ["relation \"ine_t\" already exists, skipping"]
           (warnings c "CREATE TABLE IF NOT EXISTS ine_t (i int)")))
    (testing "and really skipped — the original table is untouched"
      (exec! c "INSERT INTO ine_t VALUES (1)")
      (is (= [["1"]] (rows c "SELECT i FROM ine_t"))))))

(deftest a-notice-is-not-an-error
  (with-open [c (jdbc)]
    (testing "it does not abort the statement"
      (is (= [] (warnings c "CREATE TABLE keep_t (i int)")))
      (is (= ["table \"gone_t\" does not exist, skipping"]
             (warnings c "DROP TABLE IF EXISTS gone_t")))
      (exec! c "INSERT INTO keep_t VALUES (7)")
      (is (= [["7"]] (rows c "SELECT i FROM keep_t"))))
    (testing "and it does not put an open transaction into the failed state"
      (exec! c "BEGIN")
      (is (= ["table \"gone_t\" does not exist, skipping"]
             (warnings c "DROP TABLE IF EXISTS gone_t")))
      ;; A failed transaction would refuse this with 25P02.
      (exec! c "INSERT INTO keep_t VALUES (8)")
      (exec! c "COMMIT")
      (is (= [["7"] ["8"]] (rows c "SELECT i FROM keep_t ORDER BY i"))))))

(deftest notices-reach-the-extended-protocol-too
  ;; The extended path buffers its responses so a held statement's
  ;; CommandComplete can be drained at Sync; a notice written straight
  ;; to the socket would jump ahead of an earlier statement's response.
  (with-open [c (jdbc)]
    (is (= ["table \"nope_x\" does not exist, skipping"]
           (prepared-warnings c "DROP TABLE IF EXISTS nope_x")))
    (exec! c "CREATE TABLE ext_t (i int)")
    (is (= ["relation \"ext_t\" already exists, skipping"]
           (prepared-warnings c "CREATE TABLE IF NOT EXISTS ext_t (i int)")))))

(deftest a-statement-with-nothing-to-say-sends-no-notice
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE real_t (i int)")
    (is (= [] (warnings c "DROP TABLE IF EXISTS real_t")))
    (is (= [] (warnings c "CREATE TABLE IF NOT EXISTS other_t (i int)")))
    (is (= [] (warnings c "SELECT 1")))))
