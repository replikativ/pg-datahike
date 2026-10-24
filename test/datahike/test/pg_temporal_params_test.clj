(ns datahike.test.pg-temporal-params-test
  "A date, time, timestamp or timestamptz parameter bound as TEXT with its
   declared OID -- what pgjdbc does for java.time values and psycopg for
   datetime -- gated against a corpus captured from PostgreSQL 17 under
   three zones (`test/resources/temporal_params_corpus.edn`).

   Such a value was left a string by the wire layer and reached the
   statement as an unknown literal, so it was read late, in UTC, or not
   at all: under Asia/Kolkata `SELECT ?::text` of 10:30+05:30 answered
   in UTC, and a naive timestamp bound as timestamptz was placed in UTC
   instead of the session's zone."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.edn :as edn]
            [clojure.string]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:private corpus
  (:corpus (edn/read-string (slurp "test/resources/temporal_params_corpus.edn"))))

(def ^:dynamic *port* nil)

(defn- server-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"tp" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each server-fixture)

(defn- value [kind ^String iso]
  (case kind
    :local-date-time (java.time.LocalDateTime/parse iso)
    :offset-date-time (java.time.OffsetDateTime/parse iso)
    :local-date (java.time.LocalDate/parse iso)
    :local-time (java.time.LocalTime/parse iso)))

(defn- run [^Connection c sql v]
  (try
    (with-open [st (.prepareStatement c sql)]
      (.setObject st 1 v)
      (with-open [rs (.executeQuery st)]
        (.next rs)
        [(.getString rs 1) nil]))
    (catch SQLException e [nil (.getSQLState e)])))

(def ^:private money-corpus
  (:corpus (edn/read-string (slurp "test/resources/money_array_corpus.edn"))))

(deftest money-as-an-array-element
  ;; The same gap from the other side: an array element is read by its
  ;; type's input function, and money had none registered, so the token's
  ;; text was kept. Captured from PostgreSQL 17.
  (with-open [c (DriverManager/getConnection
                 (str "jdbc:postgresql://127.0.0.1:" *port*
                      "/tp?user=x&password=x&sslmode=disable"))]
    (doseq [[sql want err] money-corpus]
      (let [[got got-err] (try (with-open [st (.createStatement c)
                                           rs (.executeQuery st sql)]
                                 (.next rs) [(.getString rs 1) nil])
                               (catch SQLException e [nil (.getMessage e)]))]
        (if err
          (is (and got-err (clojure.string/includes? got-err err)) (str sql " -> " got " / " got-err))
          (is (= want got) (str sql " -> " got-err)))))))

(deftest the-corpus-agrees-with-postgresql
  (is (<= 100 (count corpus)) "the corpus is actually loaded")
  (let [bad (atom [])]
    (doseq [[zone rows] (group-by first corpus)]
      (with-open [c (DriverManager/getConnection
                     (str "jdbc:postgresql://127.0.0.1:" *port*
                          "/tp?user=x&password=x&sslmode=disable&binaryTransfer=false"))]
        (with-open [st (.createStatement c)] (.execute st (str "SET TimeZone = '" zone "'")))
        (doseq [[_ kind iso sql want-v want-err :as row] rows]
          (let [[got-v got-err] (run c sql (value kind iso))]
            (when-not (if want-err (= want-err got-err) (= want-v got-v))
              (swap! bad conj [row [got-v got-err]]))))))
    (doseq [[row got] @bad] (println " " row "\n    got:" got))
    (is (empty? @bad) (str (count @bad) " of " (count corpus) " corpus entries disagree"))))
