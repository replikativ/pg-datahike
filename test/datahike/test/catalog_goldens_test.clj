(ns datahike.test.catalog-goldens-test
  "Golden-file regression tests for the PG catalog surface.

   Records the rows every `datahike.pg` handler produces for the exact
   SQL that pgjdbc, Hibernate, Rails, and other PG clients issue at
   connection/boot time — the `DatabaseMetaData` probe set, the
   information_schema queries, and the handful of catalog SELECTs that
   come along for the ride.

   Storage: `test/goldens/<probe-name>.edn`. On first run (or when a
   probe genuinely changes), regenerate by deleting the golden file
   (or running with `:regenerate true` from a REPL) — the test
   re-writes the golden and prints a diff. Every subsequent run
   compares byte-for-byte. Catches silent regressions in pg_class /
   pg_attribute / pg_index / pg_type projection that unit tests miss
   because they don't know which exact 47 columns Hibernate wants.

   The probes are named, not numbered, so a regression reads as
   `getTables-all-public` rather than `goldens/43.edn`. Names are the
   file-safe name of the test case."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Probe helpers

(defn- goldens-dir
  "Resolve the on-disk goldens directory. `test/goldens` for the
   standard repo layout; override with DATAHIKE_PG_GOLDENS_DIR if the
   test is run from elsewhere."
  []
  (io/file (or (System/getenv "DATAHIKE_PG_GOLDENS_DIR")
               "test/goldens")))

(defn- golden-path [probe-name]
  (io/file (goldens-dir) (str probe-name ".edn")))

(defn- result->plain
  "Turn a QueryResult into a plain Clojure map the golden can store.
   Captures :error and :rows (vector-of-vectors of String cells) —
   column metadata varies across driver versions and doesn't help
   detect semantic regressions, so we skip it."
  [^PgWireServer$QueryResult r]
  {:error (.error r)
   :rows  (mapv vec (.rows r))})

(defn- read-golden [probe-name]
  (let [^java.io.File f (golden-path probe-name)]
    (when (.exists f)
      (edn/read-string (slurp f)))))

(defn- write-golden! [probe-name data]
  (let [f (golden-path probe-name)]
    (io/make-parents f)
    (spit f (with-out-str (pp/pprint data)))))

(defn- diff-rows
  "Return a human-readable diff of two result-maps, or nil if equal."
  [expected actual]
  (when (not= expected actual)
    (let [er (:rows expected) ar (:rows actual)
          extra (remove (set er) ar)
          missing (remove (set ar) er)]
      (cond-> {:path :rows}
        (seq extra)   (assoc :unexpected extra)
        (seq missing) (assoc :missing missing)
        (not= (:error expected) (:error actual))
        (assoc :error [(:error expected) (:error actual)])))))

(defn check-probe!
  "Core runner: execute `sql` against `handler`, compare to the stored
   golden. Returns `{:status :ok|:missing-golden|:mismatch :diff ...}`.
   When the golden is missing, writes it and reports :missing-golden
   — the first test run establishes the baseline and then flags a
   missing golden as an explicit action for the dev.

   When `regenerate?` is true, overwrites the stored golden."
  [handler probe-name sql & [{:keys [regenerate?]}]]
  (let [raw   (.execute ^datahike.pg.PgWireServer$QueryHandler handler sql)
        plain (result->plain raw)]
    (cond
      regenerate?
      (do (write-golden! probe-name plain)
          {:status :regenerated :data plain})

      :else
      (if-let [expected (read-golden probe-name)]
        (if-let [diff (diff-rows expected plain)]
          {:status :mismatch :diff diff :actual plain :expected expected}
          {:status :ok})
        (do (write-golden! probe-name plain)
            {:status :missing-golden :data plain})))))

;; ---------------------------------------------------------------------------
;; Fixture: build a handler against a tiny stable DB

(defn- fresh-handler ^datahike.pg.PgWireServer$QueryHandler []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          ^datahike.pg.PgWireServer$QueryHandler h (pg/make-query-handler conn)]
      ;; Small but representative schema.
      (.execute h "CREATE TABLE person (id INT PRIMARY KEY, name TEXT NOT NULL, age INT)")
      (.execute h "CREATE TABLE address (id INT PRIMARY KEY, person_id INT, city TEXT)")
      (.execute h "CREATE INDEX idx_person_name ON person(name)")
      (.execute h "INSERT INTO person VALUES (1, 'Alice', 30), (2, 'Bob', 25)")
      (.execute h "INSERT INTO address VALUES (10, 1, 'Zurich'), (11, 2, 'Basel')")
      h)))

;; ---------------------------------------------------------------------------
;; Probes — every named test records one canonical catalog SQL.

(def probes
  "Map of probe-name → SQL. Keep grouped by originating client so a
   failure immediately tells you which driver's expectations drifted.

   Add a probe by appending here and running the test — the first run
   writes the baseline."
  {;; pgjdbc DatabaseMetaData calls
   "pgjdbc-getTables-public"
   "SELECT n.nspname AS table_schem, c.relname AS table_name,
           CASE c.relkind WHEN 'r' THEN 'TABLE' WHEN 'v' THEN 'VIEW' END AS table_type
      FROM pg_catalog.pg_class c
      JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
     WHERE n.nspname = 'public' AND c.relkind IN ('r','v')
     ORDER BY table_schem, table_name"

   "pgjdbc-getColumns-person"
   "SELECT a.attname AS column_name, a.atttypid, a.attnotnull, a.attnum
      FROM pg_catalog.pg_class c
      JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid
     WHERE c.relname = 'person' AND a.attnum > 0
     ORDER BY a.attnum"

   "pgjdbc-getPrimaryKeys-person"
   "SELECT a.attname AS column_name
      FROM pg_catalog.pg_class c
      JOIN pg_catalog.pg_index i ON i.indrelid = c.oid
      JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(i.indkey)
     WHERE c.relname = 'person' AND i.indisprimary"

   ;; information_schema — Hibernate / SQLAlchemy / Rails
   "is-tables-public"
   "SELECT table_schema, table_name, table_type
      FROM information_schema.tables
     WHERE table_schema = 'public'
     ORDER BY table_name"

   "is-columns-person"
   "SELECT column_name, data_type, is_nullable, ordinal_position
      FROM information_schema.columns
     WHERE table_name = 'person'
     ORDER BY ordinal_position"

   "is-table-constraints-person"
   "SELECT constraint_type, constraint_name
      FROM information_schema.table_constraints
     WHERE table_name = 'person'
     ORDER BY constraint_type, constraint_name"

   ;; Misc boot probes
   "pg-version"
   "SELECT version()"

   "current-schemas"
   "SELECT current_schemas(false)"})

(deftest catalog-goldens
  (let [h (fresh-handler)]
    (doseq [[probe-name sql] probes]
      (testing probe-name
        (let [{:keys [status diff expected actual]}
              (check-probe! h probe-name sql)]
          (case status
            :ok             (is true (str probe-name ": matches golden"))
            :missing-golden (is true (str probe-name ": baseline written"
                                          " — commit test/goldens/"
                                          probe-name ".edn and re-run"))
            :mismatch       (is false
                                (str probe-name ": diff\n"
                                     "expected rows: " (pr-str (:rows expected)) "\n"
                                     "actual rows:   " (pr-str (:rows actual)) "\n"
                                     "diff:          " (pr-str diff)))))))))
