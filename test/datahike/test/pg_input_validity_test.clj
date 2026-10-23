(ns datahike.test.pg-input-validity-test
  "pg_input_is_valid and pg_input_error_info answer through each type's
   own input function, gated against a corpus captured from a real
   PostgreSQL 17 (`test/resources/input_validity_corpus.edn`).

   They knew a short table of types and answered FALSE for every other
   one: a valid date, timestamp, time, money, interval, point, macaddr,
   bytea or array was reported invalid. An unknown type name is 42704,
   and an array is checked by the strict array input."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/input_validity_corpus.edn")))

(def ^:private corpus (:corpus fixture))

(defn- with-handler [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f (pg/make-query-handler conn {}))
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- scalar
  "Run `SELECT <expr>` and return the rendered text, or {:error msg}."
  [^PgWireServer$QueryHandler h ^String expr]
  (let [r (.execute h (str "SELECT " expr))]
    (if (.error ^PgWireServer$QueryResult r)
      {:error (.error ^PgWireServer$QueryResult r)}
      (let [rows (.rows ^PgWireServer$QueryResult r)]
        (if (zero? (alength rows))
          {:error "no rows"}
          ;; Every column, joined as psql -A joins them.
          (str/join "|" (map #(or % "") (aget rows 0))))))))

(deftest the-corpus-agrees-with-postgresql
  (is (<= 20 (count corpus)) "the corpus is actually loaded")
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql (:setup fixture)] (.execute h ^String sql))
      (let [bad (atom [])]
        (doseq [[expr expected err] corpus]
          (let [got (scalar h expr)]
            (cond
              (some? err)
              (when-not (and (map? got) (str/includes? (str (:error got)) err))
                (swap! bad conj [expr (str "error: " err)
                                 (if (map? got) (str (:error got)) (str got))]))
              (map? got) (swap! bad conj [expr expected (str "ERROR " (:error got))])
              :else (when-not (= expected got) (swap! bad conj [expr expected got])))))
        (doseq [[expr want got] @bad]
          (println " " expr "\n    want:" want "\n    got :" got))
        (is (empty? @bad)
            (str (count @bad) " of " (count corpus) " corpus entries disagree"))))))
