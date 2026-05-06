(ns datahike.test.pg-chinook-roundtrip-test
  "Full bidirectional roundtrip: real PostgreSQL → pg-datahike → dump
   → real PostgreSQL, using the Chinook fixture (~15.6k rows across
   11 tables with FKs, multi-VALUES INSERTs, NUMERIC, TIMESTAMP, and
   slash-formatted dates).

   Requires:
     - test/fixtures/chinook.sql (the upstream lerocha/chinook-database
       Chinook_PostgreSql.sql, fetched once and committed)
     - a running real-PG container reachable as `pgwire-real-pg`
       (env-overridable). Defaults match the konserve-jdbc-postgres-1
       devcontainer used elsewhere in this repo.

   Skipped (with a `(println …)` notice) when the docker container or
   real PG isn't reachable, so the unit suite still runs everywhere."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.dump :as dump]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

;; ============================================================================
;; Real-PG connection settings — override via env for CI / other layouts.
;; ============================================================================

(def ^:private real-pg-host (or (System/getenv "PGWIRE_REAL_PG_HOST") "127.0.0.1"))
(def ^:private real-pg-port (or (some-> (System/getenv "PGWIRE_REAL_PG_PORT") Integer/parseInt) 5432))
(def ^:private real-pg-user (or (System/getenv "PGWIRE_REAL_PG_USER") "alice"))
(def ^:private real-pg-pass (or (System/getenv "PGWIRE_REAL_PG_PASS") "foo"))
(def ^:private real-pg-admin-db (or (System/getenv "PGWIRE_REAL_PG_DB") "postgres"))

(defn- real-pg-url [^String db]
  (str "jdbc:postgresql://" real-pg-host ":" real-pg-port "/" db
       "?user=" real-pg-user "&password=" real-pg-pass))

(defn- real-pg-available?
  "True if we can open a TCP+auth handshake against the real-PG admin db."
  []
  (try
    (with-open [^Connection c (DriverManager/getConnection (real-pg-url real-pg-admin-db))
                stmt (.createStatement c)]
      (.execute stmt "SELECT 1"))
    true
    (catch Throwable _ false)))

(def ^:private chinook-tables
  ["album" "artist" "customer" "employee" "genre" "invoice"
   "invoice_line" "media_type" "playlist" "playlist_track" "track"])

(defn- preprocess-chinook
  "Strip db-management lines that target a real-PG `\\c chinook` flow:
   we host the schema in our own pre-created datahike DB."
  [^String sql]
  (->> (str/split-lines sql)
       (remove (fn [line]
                 (let [t (str/trim line)
                       low (str/lower-case t)]
                   (or (str/starts-with? low "drop database")
                       (str/starts-with? low "create database")
                       (str/starts-with? t "\\c")
                       (str/starts-with? t "\\C")))))
       (str/join "\n")))

(defn- exec! [^Connection c ^String sql]
  (with-open [stmt (.createStatement c)] (.execute stmt sql)))

(defn- count-tables
  "Return {table-name → row-count} querying every Chinook table."
  [^Connection c]
  (into {}
        (map (fn [t]
               (with-open [stmt (.createStatement c)
                           rs (.executeQuery stmt (str "SELECT count(*) FROM " t))]
                 (.next rs)
                 [t (.getLong rs 1)])))
        chinook-tables))

(defn- execute-multistatement!
  "Run a multi-statement SQL via JDBC. The wire-protocol simple-query
   path splits on `;` server-side, so a single execute works."
  [^Connection c ^String sql]
  (with-open [stmt (.createStatement c)]
    (.execute stmt sql)))

;; ============================================================================
;; Test driver: full source → pg-datahike → dump → target roundtrip.
;; ============================================================================

(def ^:dynamic *fixture-sql* nil)
(def ^:dynamic *baseline-counts* nil)
(def ^:dynamic *real-pg-skip-reason* nil)

(defn- with-real-pg-baseline
  "Set up the real-PG `chinook` baseline DB and a fresh `chinook_rt`
   target. Captures expected row counts from the baseline."
  [f]
  (Class/forName "org.postgresql.Driver")
  (cond
    (not (real-pg-available?))
    (binding [*real-pg-skip-reason* (str "real PG unreachable at "
                                         real-pg-host ":" real-pg-port)]
      (println "[skip] pg-chinook-roundtrip:" *real-pg-skip-reason*)
      (f))

    (not (.exists (io/file "test/fixtures/chinook.sql")))
    (binding [*real-pg-skip-reason* "test/fixtures/chinook.sql missing"]
      (println "[skip] pg-chinook-roundtrip:" *real-pg-skip-reason*)
      (f))

    :else
    (let [fixture (slurp "test/fixtures/chinook.sql")]
      ;; Reset baseline DB
      (with-open [c (DriverManager/getConnection (real-pg-url real-pg-admin-db))]
        (try (exec! c "DROP DATABASE IF EXISTS chinook")    (catch Throwable _))
        (try (exec! c "DROP DATABASE IF EXISTS chinook_rt") (catch Throwable _))
        (exec! c "CREATE DATABASE chinook"))

      ;; Load fixture into the baseline (skip the DROP/CREATE/\c lines —
      ;; we already created the db).
      (with-open [c (DriverManager/getConnection (real-pg-url "chinook"))]
        (execute-multistatement! c (preprocess-chinook fixture)))

      (let [baseline (with-open [c (DriverManager/getConnection (real-pg-url "chinook"))]
                       (count-tables c))]
        (binding [*fixture-sql* fixture
                  *baseline-counts* baseline]
          (try (f)
               (finally
                 (with-open [c (DriverManager/getConnection (real-pg-url real-pg-admin-db))]
                   (try (exec! c "DROP DATABASE IF EXISTS chinook")    (catch Throwable _))
                   (try (exec! c "DROP DATABASE IF EXISTS chinook_rt") (catch Throwable _))))))))))

(use-fixtures :each with-real-pg-baseline)

;; ============================================================================
;; Tests
;; ============================================================================

(deftest chinook-baseline-loads-into-real-pg
  (when-not *real-pg-skip-reason*
    (testing "the upstream Chinook fixture loads cleanly into real PG"
      (is (= 11 (count *baseline-counts*)))
      (is (every? pos? (vals *baseline-counts*)))
      (is (= 8715 (get *baseline-counts* "playlist_track")))
      (is (= 3503 (get *baseline-counts* "track"))))))

(deftest chinook-ingests-into-pg-datahike
  (when-not *real-pg-skip-reason*
    (testing "Chinook SQL ingests into pg-datahike with identical row counts"
      (pg/reset-lock-registry!)
      (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
                 :schema-flexibility :write :keep-history? false}]
        (d/create-database cfg)
        (let [conn (d/connect cfg)
              srv (pg/start-server conn {:port 0})
              port (.getPort ^datahike.pg.PgWireServer (:server srv))]
          (try
            (with-open [c (DriverManager/getConnection
                           (str "jdbc:postgresql://localhost:" port "/datahike"
                                "?user=datahike&password=datahike"))]
              (execute-multistatement! c (preprocess-chinook *fixture-sql*))
              (let [counts (count-tables c)]
                (is (= *baseline-counts* counts)
                    "Per-table row counts must match the real-PG baseline")))
            (finally
              (.stop ^datahike.pg.PgWireServer (:server srv))
              (d/release conn)
              (d/delete-database cfg))))))))

(deftest chinook-full-bidirectional-roundtrip
  (when-not *real-pg-skip-reason*
    (testing "real-PG → pg-datahike → dump → fresh real-PG yields identical state"
      (pg/reset-lock-registry!)
      (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
                 :schema-flexibility :write :keep-history? false}]
        (d/create-database cfg)
        (let [conn (d/connect cfg)
              srv (pg/start-server conn {:port 0})
              port (.getPort ^datahike.pg.PgWireServer (:server srv))]
          (try
            ;; Leg 1: load fixture into pg-datahike
            (with-open [c (DriverManager/getConnection
                           (str "jdbc:postgresql://localhost:" port "/datahike"
                                "?user=datahike&password=datahike"))]
              (execute-multistatement! c (preprocess-chinook *fixture-sql*)))

            ;; Leg 2: dump pg-datahike to portable SQL
            (let [dump-sql (dump/dump-to-string conn)]
              (is (pos? (count dump-sql)) "Dump output is non-empty")
              (is (str/includes? dump-sql "CREATE TABLE") "Dump emits DDL")
              (is (str/includes? dump-sql "INSERT INTO") "Dump emits data")

              ;; Leg 3: replay dump into a FRESH real-PG database
              (with-open [c (DriverManager/getConnection (real-pg-url real-pg-admin-db))]
                (exec! c "CREATE DATABASE chinook_rt"))
              (with-open [c (DriverManager/getConnection (real-pg-url "chinook_rt"))]
                (execute-multistatement! c dump-sql))

              ;; Verify: target counts match baseline
              (with-open [c (DriverManager/getConnection (real-pg-url "chinook_rt"))]
                (let [counts (count-tables c)]
                  (is (= *baseline-counts* counts)
                      "Roundtripped per-table row counts match baseline"))

                ;; Spot-check a multi-table FK join survives the trip.
                (with-open [stmt (.createStatement c)
                            rs (.executeQuery stmt
                                              (str "SELECT t.name, a.title, ar.name FROM track t "
                                                   "JOIN album a ON t.album_id = a.album_id "
                                                   "JOIN artist ar ON a.artist_id = ar.artist_id "
                                                   "WHERE t.track_id = 1"))]
                  (is (.next rs))
                  (is (= "For Those About To Rock (We Salute You)" (.getString rs 1)))
                  (is (= "For Those About To Rock We Salute You" (.getString rs 2)))
                  (is (= "AC/DC" (.getString rs 3))))

                ;; NUMERIC fidelity: sum the invoice totals on both sides.
                (let [target-sum (with-open [stmt (.createStatement c)
                                             rs (.executeQuery stmt "SELECT sum(total) FROM invoice")]
                                   (.next rs)
                                   (.getBigDecimal rs 1))
                      baseline-sum (with-open [c2 (DriverManager/getConnection (real-pg-url "chinook"))
                                               stmt (.createStatement c2)
                                               rs (.executeQuery stmt "SELECT sum(total) FROM invoice")]
                                     (.next rs)
                                     (.getBigDecimal rs 1))]
                  (is (= 0 (.compareTo baseline-sum target-sum))
                      "NUMERIC sums match exactly across the roundtrip"))))
            (finally
              (.stop ^datahike.pg.PgWireServer (:server srv))
              (d/release conn)
              (d/delete-database cfg))))))))
