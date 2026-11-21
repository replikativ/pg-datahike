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
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandler PgWireServer$QueryResult]))

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

(deftest ordinary-constraint-flags-match-postgresql-18
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (doseq [sql ["CREATE TABLE parent (id integer PRIMARY KEY)"
                   "CREATE TABLE child (id integer PRIMARY KEY, pid integer UNIQUE REFERENCES parent(id), amount integer CHECK (amount > 0))"]]
        (is (nil? (.error (.execute h ^String sql))) sql))
      (is (= "c|t|0|f\nf|t|0|f\np|t|0|f\nu|t|0|f"
             (run h "SELECT contype, conenforced, conparentid, conperiod
                       FROM pg_catalog.pg_constraint
                       WHERE conrelid = 'child'::regclass AND contype <> 'n'
                       ORDER BY contype")))
      (is (= "boolean|oid|boolean"
             (run h "SELECT pg_typeof(conenforced), pg_typeof(conparentid), pg_typeof(conperiod)
                       FROM pg_catalog.pg_constraint WHERE contype = 'c'"))))))

(deftest partition-ancestors-of-ordinary-relations
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (is (nil? (.error (.execute h "CREATE TABLE ordinary (id integer)"))))
      (doseq [sql ["SELECT * FROM pg_catalog.pg_partition_ancestors('ordinary')"
                   "SELECT * FROM pg_partition_ancestors('ordinary') WITH ORDINALITY AS a(relid, depth)"
                   "SELECT pg_partition_ancestors('ordinary')"
                   "SELECT pg_partition_ancestors(NULL)"]]
        (is (= "" (run h sql)) sql))
      (is (= "t" (run h "SELECT 'ordinary'::pg_catalog.regclass IN
                                 (SELECT pg_catalog.pg_partition_ancestors('ordinary')
                                  UNION ALL VALUES ('ordinary'::pg_catalog.regclass))")))
      (is (= "42P01" (.sqlstate (.execute h "SELECT * FROM pg_partition_ancestors('missing_table')"))))
      (is (= "42883" (.sqlstate (.execute h "SELECT * FROM pg_partition_ancestors()"))))
      (is (= "42883" (.sqlstate (.execute h "SELECT pg_partition_ancestors(true)"))))
      (let [^PgWireServer$QueryResult result
            (.execute h "SELECT * FROM pg_partition_ancestors('ordinary') WITH ORDINALITY AS a(relid, depth)")]
        (is (= ["relid" "depth"] (vec (.-columnNames result))))
        (is (= [2205 20] (vec (.-columnOids result)))))
      (is (= "f|t" (run h "SELECT 1 IN (SELECT generate_series(1, 3) LIMIT 1 OFFSET 1),
                                 2 IN (SELECT generate_series(1, 3) LIMIT 1 OFFSET 1)"))))))

(deftest not-null-catalog-follows-the-advertised-major
  (with-handler
    (fn [^PgWireServer$QueryHandler h]
      (let [pg18? (>= (parse-long PgWireServer/SERVER_VERSION_NUM) 180000)
            sql "SELECT c.conname, a.attname, c.connoinherit, c.conislocal,
                        c.coninhcount <> 0, c.convalidated, pg_typeof(c.conkey[1])
                   FROM pg_catalog.pg_constraint c JOIN pg_catalog.pg_attribute a
                     ON (a.attrelid = c.conrelid AND a.attnum = c.conkey[1])
                  WHERE c.contype = 'n' AND c.conrelid = 'described'::pg_catalog.regclass
                  ORDER BY a.attnum"]
        (is (nil? (.error (.execute h "CREATE TABLE described (id integer PRIMARY KEY, label text NOT NULL, notes text)"))))
        (is (= (if pg18?
                 "described_id_not_null|id|f|t|f|t|smallint\ndescribed_label_not_null|label|f|t|f|t|smallint"
                 "")
               (run h sql)))
        (is (nil? (.error (.execute h "ALTER TABLE described DROP COLUMN label"))))
        (is (= (if pg18? "described_id_not_null|id|f|t|f|t|smallint" "") (run h sql)))))))
