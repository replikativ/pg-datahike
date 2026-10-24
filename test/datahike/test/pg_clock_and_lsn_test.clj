(ns datahike.test.pg-clock-and-lsn-test
  "Two regressions the 2026-10-23 pg_regress re-measure found, gated
   against a corpus captured from PostgreSQL 17 under
   America/Los_Angeles (`test/resources/clock_and_lsn_corpus.edn`).

   The SQL value functions -- current_time, localtime, localtimestamp,
   current_date -- read the statement's clock in UTC whatever TimeZone
   said. Once a cast honoured the zone, the regression suite's own
   `now()::time::text = localtime::text` turned false (expressions.sql),
   and every evening in Los Angeles current_date was tomorrow.

   pg_lsn had no input function: the cast kept the text as written, so
   `'16AE7F7'::pg_lsn` was accepted and pg_input_is_valid said true
   (pg_lsn.sql)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/clock_and_lsn_corpus.edn")))

(defn- with-handler [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f (pg/make-query-handler conn {}))
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- run [^PgWireServer$QueryHandler h ^String sql]
  (let [r (.execute h sql)]
    (if (.error ^PgWireServer$QueryResult r)
      {:error (.error ^PgWireServer$QueryResult r)}
      (str/join "\n" (map (fn [row] (str/join "|" (map #(or % "") row)))
                          (.rows ^PgWireServer$QueryResult r))))))

(deftest the-corpus-agrees-with-postgresql
  (is (<= 15 (count (:corpus fixture))) "the corpus is actually loaded")
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (.execute h (str "SET TimeZone = '" (:zone fixture) "'"))
      (doseq [[sql expected err] (:corpus fixture)]
        (let [got (run h sql)]
          (if err
            (is (and (map? got) (str/includes? (str (:error got)) err)) (str sql " -> " got))
            (is (= expected got) sql)))))))
