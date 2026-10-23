(ns datahike.test.pg-format-test
  "format() as varlena.c's text_format and quote_ident as ruleutils.c's
   quote_identifier, gated against a corpus captured from a real
   PostgreSQL 17 (`test/resources/format_corpus.edn`).

   format() read only %s, %I, %L and %%: argument positions, widths, `-`
   and `*` were printed literally, too few arguments silently gave '',
   an unknown specifier was echoed, a NULL identifier was \"\", and %I
   quoted every name. quote_ident knew a dozen keywords of PostgreSQL's
   several hundred, and took `$` as safe."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/format_corpus.edn")))

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
