(ns datahike.test.pg-to-char-test
  "to_char over timestamps, timestamptz, dates, times and intervals --
   formatting.c's DCH side, ported in datahike.pg.datetime.format --
   gated against a corpus captured from a real PostgreSQL 17 under two
   zones (`test/resources/to_char_corpus.edn`): every pattern the
   regression suite uses, and more.

   to_char over any of these raised 0A000 before."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private corpus
  (:corpus (edn/read-string (slurp "test/resources/to_char_corpus.edn"))))

(defn- with-handler [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f (pg/make-query-handler conn {}))
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- scalar [^PgWireServer$QueryHandler h ^String expr]
  (let [r (.execute h (str "SELECT " expr))]
    (if (.error ^PgWireServer$QueryResult r)
      {:error (.error ^PgWireServer$QueryResult r)}
      (let [rows (.rows ^PgWireServer$QueryResult r)]
        (if (zero? (alength rows)) {:error "no rows"}
            (or (aget ^"[Ljava.lang.String;" (aget rows 0) 0) ""))))))

(deftest the-corpus-agrees-with-postgresql
  (is (<= 500 (count corpus)) "the corpus is actually loaded")
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (let [bad (atom [])]
        (doseq [[zone expr expected err] corpus]
          (.execute h (str "SET timezone = '" zone "'"))
          (let [got (scalar h expr)]
            (cond
              (some? err) (when-not (and (map? got) (str/includes? (str (:error got)) err))
                            (swap! bad conj [zone expr (str "error: " err) got]))
              (map? got) (swap! bad conj [zone expr expected (str "ERROR " (:error got))])
              :else (when-not (= expected got) (swap! bad conj [zone expr expected got])))))
        (doseq [[zone expr want got] (take 40 @bad)]
          (println " " zone expr "\n    want:" want "\n    got :" got))
        (is (empty? @bad)
            (str (count @bad) " of " (count corpus) " corpus entries disagree"))))))
