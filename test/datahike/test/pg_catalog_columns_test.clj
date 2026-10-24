(ns datahike.test.pg-catalog-columns-test
  "The catalog columns psql's \\d, \\d+, \\df, \\dt, \\di and the sanity
   regression files read, gated against a corpus captured from
   PostgreSQL 17 (`test/resources/catalog_columns_corpus.edn`).

   Once an unknown catalog column raised 42703 instead of answering NULL
   (#306), every query naming one of these stopped running: psql's \\d
   reported `column \"reltoastrelid\" does not exist`, \\df `prokind`,
   type_sanity `typinput`, drop_operator `oprcom`. The pg_regress
   re-measure of 2026-10-23 counted them -- psql.sql alone lost 94 lines
   to it. They are served now with the values PostgreSQL gives, the type
   and operator ones from the generated registry.

   Two wrong answers surfaced on the way and are pinned here too:
   pg_get_constraintdef / pg_get_indexdef / pg_get_triggerdef were inner
   joins, so a NULL oid REMOVED THE ROW -- \\d listed no indexes at all --
   and pg_constraint.conindid was unset, so a key constraint never met
   its index."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(def ^:private fixture
  (edn/read-string (slurp "test/resources/catalog_columns_corpus.edn")))

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
      (str "ERROR " (.error ^PgWireServer$QueryResult r))
      (str/join "\n" (map (fn [row] (str/join "|" (map #(or % "") row)))
                          (.rows ^PgWireServer$QueryResult r))))))

(deftest the-corpus-agrees-with-postgresql
  (is (<= 10 (count (:corpus fixture))) "the corpus is actually loaded")
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql (:setup fixture)]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (doseq [[sql expected] (:corpus fixture)]
        (is (= expected (run h sql)) sql)))))
