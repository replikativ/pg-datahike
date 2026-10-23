(ns datahike.test.pg-rendering-test
  "Values rendered as text and as json by their TYPE, gated against a
   corpus captured from a real PostgreSQL 17
   (`test/resources/rendering_corpus.edn`, setup included).

   `->pg-text` was not total: a bool, an array, a bit string and a record
   fell to `str`, so `concat(true, 'x')` was `truex` (PostgreSQL: `tx`,
   bool's output function) and `concat(ARRAY[1,2], 'x')` printed the
   Java identity. record_out escaped a quote with a backslash where
   PostgreSQL doubles it. And the json constructors and aggregates
   converted by CLASS, so a money was a bare number, a date in json_agg
   a timestamp, a timestamptz lost its offset, `infinity` printed year
   292278994, and a bytea its Java identity."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/rendering_corpus.edn")))

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
          (aget ^"[Ljava.lang.String;" (aget rows 0) 0))))))

(deftest the-corpus-agrees-with-postgresql
  (is (<= 25 (count corpus)) "the corpus is actually loaded")
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
