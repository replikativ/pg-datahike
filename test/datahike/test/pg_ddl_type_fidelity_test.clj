(ns datahike.test.pg-ddl-type-fidelity-test
  "A column's DECLARED SQL type must survive into the catalog and the
   wire, even though Datahike collapses whole families onto one carrier:
   every integer is :db.type/long, every temporal is :db.type/instant.

   `:pg/type` records the declared type at CREATE TABLE and
   `declared-col-oid` already resolved it correctly — but
   `pg_attribute.atttypid` threw that away and re-derived from the
   storage valueType, so a `date` column reported `timestamp without
   time zone` and an `int` column reported `bigint`. Column ORDER had
   the same shape of bug: `column-info` reordered by creation order
   while everything attnum-shaped read the schema map's hash order.

   Not cosmetic. Drivers pick codecs off these OIDs, and on a date
   column pgjdbc with server-side prepares raised
   `Unsupported binary encoding of date` on the second execution.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *h* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*h* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *h* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- v [sql] (ffirst (rows sql)))

(def ^:private cols-sql
  "SELECT a.attname, format_type(a.atttypid, a.atttypmod)
     FROM pg_attribute a JOIN pg_class c ON c.oid = a.attrelid
    WHERE c.relname = '%s' AND a.attnum > 0
    ORDER BY a.attnum")

(deftest declared-types-survive-into-pg-attribute
  (run (str "CREATE TABLE ag (id int, nm text, d date, ts timestamp, "
            "sm smallint, bg bigint, n numeric, iv interval)"))
  (testing "each column reports the type it was DECLARED as, in CREATE TABLE order"
    (is (= [["id" "integer"]
            ["nm" "text"]
            ["d"  "date"]
            ["ts" "timestamp without time zone"]
            ["sm" "smallint"]
            ["bg" "bigint"]
            ["n"  "numeric"]
            ["iv" "interval"]]
           (rows (format cols-sql "ag"))))))

(deftest bit-widths-survive-into-pg-attribute
  (run "CREATE TABLE bit_types (b bit(4), v bit varying(5), u varbit)")
  (is (= [["b" "bit(4)"]
          ["v" "bit varying(5)"]
          ["u" "bit varying"]]
         (rows (format cols-sql "bit_types")))))

(deftest column-order-is-create-table-order
  ;; Was the schema map's hash order, which disagreed with column-info's
  ;; creation order — and drivers key field metadata on attnum.
  (run "CREATE TABLE zz (zebra int, alpha int, middle int)")
  (is (= [["zebra"] ["alpha"] ["middle"]]
         (mapv (fn [r] [(first r)]) (rows (format cols-sql "zz")))))
  (testing "information_schema.columns agrees (after our synthetic db_id)"
    (is (= [["db_id"] ["zebra"] ["alpha"] ["middle"]]
           (mapv (fn [r] [(first r)])
                 (rows "SELECT column_name FROM information_schema.columns
                         WHERE table_name = 'zz' ORDER BY ordinal_position"))))))

(deftest a-date-column-renders-as-a-date
  (run "CREATE TABLE dd (id int, d date, ts timestamp)")
  (run "INSERT INTO dd VALUES (1, '2020-01-01', '2020-01-01 10:00')")
  (testing "the column path, which used to render a timestamp"
    (is (= "2020-01-01" (v "SELECT d FROM dd"))))
  (testing "the cast path was already right and stays right"
    (is (= "2020-01-01" (v "SELECT '2020-01-01'::date"))))
  (testing "a real timestamp keeps its time"
    (is (= "2020-01-01 10:00:00" (v "SELECT ts FROM dd"))))
  (testing "aggregates inherit the column's type"
    (is (= "2020-01-01" (v "SELECT max(d) FROM dd")))
    (is (= "2020-01-01" (v "SELECT min(d) FROM dd"))))
  (testing "SELECT * too"
    (is (= [["1" "2020-01-01" "2020-01-01 10:00:00"]] (rows "SELECT * FROM dd")))))

(deftest information-schema-reports-the-declared-type
  (run "CREATE TABLE ii (id int, d date)")
  (is (= [["id" "integer"] ["d" "date"]]
         (rows "SELECT column_name, data_type FROM information_schema.columns
                 WHERE table_name = 'ii' AND column_name <> 'db_id'
                 ORDER BY ordinal_position"))))

(deftest alter-add-column-records-the-same-hints
  ;; ADD COLUMN recorded :pg/type for json/jsonb ONLY, so a column added
  ;; as date or smallint reported its storage type forever after.
  (run "CREATE TABLE al (id int)")
  (run "ALTER TABLE al ADD COLUMN d date")
  (run "ALTER TABLE al ADD COLUMN sm smallint")
  (run "ALTER TABLE al ADD COLUMN j jsonb")
  (is (= [["id" "integer"] ["d" "date"] ["sm" "smallint"] ["j" "jsonb"]]
         (rows (format cols-sql "al"))))
  (testing "and the value renders accordingly"
    (run "INSERT INTO al VALUES (1, '2020-01-01', 2, '{\"a\":1}')")
    (is (= "2020-01-01" (v "SELECT d FROM al")))))

(deftest array-columns-are-not-double-promoted
  ;; An `int[]` column's :pg/type is already the array name (_int4 ->
  ;; 1007); promoting a cardinality-many column again would give int[][].
  (run "CREATE TABLE ar (id int, xs int[])")
  (is (= [["id" "integer"] ["xs" "integer[]"]]
         (rows (format cols-sql "ar")))))
