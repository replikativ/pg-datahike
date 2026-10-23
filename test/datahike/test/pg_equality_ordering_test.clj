(ns datahike.test.pg-equality-ordering-test
  "SQL equality and ordering where Clojure's disagree with PostgreSQL's,
   gated against a corpus captured from a real PostgreSQL 17
   (`test/resources/equality_ordering_corpus.edn`, setup included).

   Each was a silent wrong answer: `f NOT IN ('NaN')` was true for a NaN
   row; `SELECT 1 UNION SELECT 1.0` was two rows; a set operation inside
   IN compared with `clojure.set`, so `'1 mon' IN (SELECT '30 days'
   INTERSECT SELECT '1 mon')` was false; an aggregate's ORDER BY took one
   direction for every key and always put NULLs last; arrays could not
   be compared at all; and an interval column stored the text as typed,
   so '24 hours' read back as `24 hours` rather than `24:00:00`."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/equality_ordering_corpus.edn")))

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
