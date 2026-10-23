(ns datahike.test.pg-to-timestamp-test
  "to_date and to_timestamp(text, text) -- formatting.c's DCH_from_char and
   do_to_timestamp, ported in datahike.pg.datetime.format -- gated against
   a corpus captured from a real PostgreSQL 17 under America/Los_Angeles,
   the zone pg_regress runs in (`test/resources/to_timestamp_corpus.edn`):
   the regression suite's own calls, errors included.

   Neither existed: to_date answered `function to_date does not exist`
   and to_timestamp accepted only the epoch form."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/to_timestamp_corpus.edn")))

(defn- with-handler [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f (pg/make-query-handler conn {}))
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- run
  "Every row, as psql -At prints them: rows by newline, columns by |."
  [^PgWireServer$QueryHandler h ^String sql]
  (let [r (.execute h sql)]
    (if (.error ^PgWireServer$QueryResult r)
      {:error (.error ^PgWireServer$QueryResult r)}
      (str/join "\n" (map (fn [row] (str/join "|" (map #(or % "") row)))
                          (.rows ^PgWireServer$QueryResult r))))))

(deftest the-corpus-agrees-with-postgresql
  (let [corpus (:corpus fixture)]
    (is (<= 100 (count corpus)) "the corpus is actually loaded")
    (with-handler
      (fn [^PgWireServer$QueryHandler h]
        (.execute h (str "SET timezone = '" (:zone fixture) "'"))
        (let [bad (atom [])]
          (doseq [[sql expected err] corpus]
            (let [got (run h sql)]
              (cond
                (some? err) (when-not (and (map? got) (str/includes? (str (:error got)) err))
                              (swap! bad conj [sql (str "error: " err) got]))
                (map? got) (swap! bad conj [sql expected (str "ERROR " (:error got))])
                :else (when-not (= expected got) (swap! bad conj [sql expected got])))))
          (doseq [[sql want got] (take 60 @bad)]
            (println " " sql "\n    want:" want "\n    got :" got))
          (is (empty? @bad)
              (str (count @bad) " of " (count corpus) " corpus entries disagree")))))))
