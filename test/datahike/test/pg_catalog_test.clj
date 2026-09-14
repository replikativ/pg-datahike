(ns datahike.test.pg-catalog-test
  "Tests for PostgreSQL catalog / information_schema virtual tables.

   PG clients (JDBC, psycopg2, pgAdmin, Odoo) issue many metadata
   probes on connect and during query planning. The pg-server
   materializes these catalogs on demand in sql.clj (around line
   5030+): pg_type, pg_class, pg_tables, pg_attribute, pg_namespace,
   pg_database, pg_proc, information_schema.{columns,tables,sequences}.

   These tests cover the common probe shapes, not exhaustive catalog
   compliance. If a client starts failing on a specific catalog
   lookup, add a regression test here."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as catalog-objects]
            [datahike.pg.server :as pg]
            [datahike.pg.sql :as sql])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def schema
  [{:db/ident :person/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :person/age  :db/valueType :db.type/long   :db/cardinality :db.cardinality/one}])

(def data [{:person/name "Alice" :person/age 30}])

(def ^:dynamic *h* nil)
(def ^:dynamic *conn* nil)

(defn fx [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema)
      (d/transact conn data)
      (try (binding [*h* (pg/make-query-handler conn)
                     *conn* conn]
             (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fx)

(defn- ex [sql]
  (let [^PgWireServer$QueryResult r (.execute *h* sql)]
    {:err (.error r)
     :sqlstate (.sqlstate r)
     :cols (vec (.columnNames r))
     :oids (vec (.columnOids r))
     :rows (mapv vec (.rows r))}))

(defn- rows [sql] (:rows (ex sql)))

(deftest object-registry-backs-table-catalog-lifecycle
  (is (nil? (:err (ex "CREATE TABLE catalog_identity (id integer)"))))
  (let [db (d/db *conn*)
        object (catalog-objects/object-by-identity
                db catalog-objects/pg-class-oid
                catalog-objects/public-namespace-oid "catalog_identity")
        oid (:datahike.pg.object/oid object)]
    (is (<= catalog-objects/first-user-oid oid))
    (is (= [[(str oid)]]
           (rows "SELECT oid FROM pg_class WHERE relname = 'catalog_identity'")))
    (is (nil? (:err (ex "DROP TABLE catalog_identity"))))
    (is (nil? (catalog-objects/object-by-address
               (d/db *conn*) catalog-objects/pg-class-oid oid)))))

(deftest namespace-catalog-comes-from-persistent-registry
  (is (= #{["11" "pg_catalog"] ["2200" "public"]}
         (set (rows "SELECT oid, nspname FROM pg_namespace")))))

(deftest create-table-if-not-exists-does-not-consume-an-oid
  (is (nil? (:err (ex "CREATE TABLE oid_once (id integer)"))))
  (let [next-before (:datahike.pg.catalog/next-oid
                     (catalog-objects/catalog-entity (d/db *conn*)))]
    (is (nil? (:err (ex "CREATE TABLE IF NOT EXISTS oid_once (id integer)"))))
    (is (= next-before
           (:datahike.pg.catalog/next-oid
            (catalog-objects/catalog-entity (d/db *conn*)))))
    (is (nil? (:err (ex "CREATE TABLE oid_after_noop (id integer)"))))
    (is (= next-before
           (Long/parseLong
            (ffirst (rows (str "SELECT oid FROM pg_class "
                               "WHERE relname = 'oid_after_noop'"))))))))

(deftest table-columns-have-durable-subobject-addresses
  (is (nil? (:err (ex "CREATE TABLE addressed_columns (a integer, b text)"))))
  (let [oid (Long/parseLong
             (ffirst (rows (str "SELECT oid FROM pg_class "
                                "WHERE relname = 'addressed_columns'"))))]
    (is (= [["a" "1"] ["b" "2"]]
           (rows (str "SELECT attname, attnum FROM pg_attribute "
                      "WHERE attrelid = " oid " ORDER BY attnum"))))
    (is (nil? (:err (ex "ALTER TABLE addressed_columns ADD COLUMN c bigint"))))
    (is (= [["a" "1"] ["b" "2"] ["c" "3"]]
           (rows (str "SELECT attname, attnum FROM pg_attribute "
                      "WHERE attrelid = " oid " ORDER BY attnum"))))
    (is (= [1 2 3]
           (mapv :datahike.pg.column/attnum
                 (catalog-objects/columns-by-relation (d/db *conn*) oid true))))))

(deftest column-drop-readd-and-rename-preserve-address-history
  (is (nil? (:err (ex (str "CREATE TABLE column_lifecycle "
                           "(a integer, b text, c integer UNIQUE)")))))
  (is (nil? (:err (ex "INSERT INTO column_lifecycle VALUES (1, 'old', 3)"))))
  (let [oid (Long/parseLong
             (ffirst (rows (str "SELECT oid FROM pg_class "
                                "WHERE relname = 'column_lifecycle'"))))]
    (is (nil? (:err (ex "ALTER TABLE column_lifecycle DROP COLUMN b"))))
    (is (= [["a" "1" "f"]
            ["........pg.dropped.2........" "2" "t"]
            ["c" "3" "f"]]
           (rows (str "SELECT attname, attnum, attisdropped FROM pg_attribute "
                      "WHERE attrelid = " oid " ORDER BY attnum"))))
    (is (= [["a" "2"] ["c" "4"]]
           (rows (str "SELECT column_name, ordinal_position "
                      "FROM information_schema.columns "
                      "WHERE table_name = 'column_lifecycle' "
                      "AND column_name <> 'db_id' ORDER BY ordinal_position"))))
    (is (= [["3"]]
           (rows (str "SELECT indkey FROM pg_index "
                      "WHERE indrelid = " oid))))
    (is (nil? (:err (ex "ALTER TABLE column_lifecycle ADD COLUMN b text"))))
    (is (= [["a" "1"] ["c" "3"] ["b" "4"]]
           (rows (str "SELECT attname, attnum FROM pg_attribute "
                      "WHERE attrelid = " oid " AND NOT attisdropped "
                      "ORDER BY attnum"))))
    (is (nil? (:err (ex "INSERT INTO column_lifecycle (a, c, b) VALUES (2, 4, 'new')"))))
    (is (= [["1" "3" nil] ["2" "4" "new"]]
           (rows "SELECT a, c, b FROM column_lifecycle ORDER BY a")))
    (is (nil? (:err (ex "ALTER TABLE column_lifecycle RENAME COLUMN c TO x"))))
    (is (= [["1" "3"] ["2" "4"]]
           (rows "SELECT a, x FROM column_lifecycle ORDER BY a")))
    (is (= [["x" "3"] ["b" "4"]]
           (rows (str "SELECT attname, attnum FROM pg_attribute "
                      "WHERE attrelid = " oid " AND attnum >= 3 "
                      "ORDER BY attnum"))))))

(deftest replacing-a-view-preserves-its-existing-row-type
  (is (nil? (:err (ex "CREATE TABLE replace_base (id integer, n numeric)"))))
  (is (nil? (:err (ex (str "CREATE VIEW replace_view AS "
                           "SELECT id, n::numeric(8,2) AS amount FROM replace_base")))))
  (let [[relation-oid row-type-oid]
        (first (rows (str "SELECT oid, reltype FROM pg_class "
                          "WHERE relname = 'replace_view'")))
        columns-sql (str "SELECT attname, atttypid, atttypmod, attnum "
                         "FROM pg_attribute WHERE attrelid = " relation-oid
                         " ORDER BY attnum")
        original-columns (rows columns-sql)]
    (is (nil? (:err (ex (str "CREATE OR REPLACE VIEW replace_view AS "
                             "SELECT id, n::numeric(8,2) AS amount FROM replace_base")))))
    (is (= [[relation-oid row-type-oid]]
           (rows (str "SELECT oid, reltype FROM pg_class "
                      "WHERE relname = 'replace_view'"))))
    (is (= original-columns (rows columns-sql)))
    (doseq [statement
            ["CREATE OR REPLACE VIEW replace_view AS SELECT id FROM replace_base"
             (str "CREATE OR REPLACE VIEW replace_view AS "
                  "SELECT id AS renamed, n::numeric(8,2) AS amount FROM replace_base")
             (str "CREATE OR REPLACE VIEW replace_view AS "
                  "SELECT id::bigint AS id, n::numeric(8,2) AS amount FROM replace_base")
             (str "CREATE OR REPLACE VIEW replace_view AS "
                  "SELECT id, n::numeric(9,2) AS amount FROM replace_base")]]
      (is (= "42P16" (:sqlstate (ex statement))) statement))
    (is (= original-columns (rows columns-sql)))
    (is (nil? (:err (ex (str "CREATE OR REPLACE VIEW replace_view AS "
                             "SELECT id, n::numeric(8,2) AS amount, "
                             "id + 1 AS next_id FROM replace_base")))))
    (is (= [["id" "1"] ["amount" "2"] ["next_id" "3"]]
           (rows (str "SELECT attname, attnum FROM pg_attribute "
                      "WHERE attrelid = " relation-oid " ORDER BY attnum"))))
    (is (nil? (:err (ex "BEGIN"))))
    (is (nil? (:err (ex (str "CREATE OR REPLACE VIEW replace_view AS "
                             "SELECT id, n::numeric(8,2) AS amount, "
                             "id + 1 AS next_id, id + 2 AS later FROM replace_base")))))
    (is (nil? (:err (ex "ROLLBACK"))))
    (is (= [["id" "1"] ["amount" "2"] ["next_id" "3"]]
           (rows (str "SELECT attname, attnum FROM pg_attribute "
                      "WHERE attrelid = " relation-oid " ORDER BY attnum"))))))

(deftest view-stars-have-a-frozen-output-descriptor
  (is (nil? (:err (ex "CREATE TABLE star_base (id integer, label varchar(12))"))))
  (is (nil? (:err (ex "INSERT INTO star_base VALUES (1, 'one')"))))
  (is (nil? (:err (ex "CREATE VIEW star_view AS SELECT * FROM star_base"))))
  (is (nil? (:err (ex "CREATE OR REPLACE VIEW star_view AS SELECT * FROM star_base"))))
  (is (= [["id" "-1"] ["label" "16"]]
         (rows (str "SELECT a.attname, a.atttypmod FROM pg_attribute a "
                    "JOIN pg_class c ON c.oid = a.attrelid "
                    "WHERE c.relname = 'star_view' ORDER BY a.attnum"))))
  (is (nil? (:err (ex "ALTER TABLE star_base ADD COLUMN later text"))))
  (is (= [["1" "one"]]
         (rows "SELECT * FROM star_view")))
  (is (= [["id"] ["label"]]
         (rows (str "SELECT a.attname FROM pg_attribute a "
                    "JOIN pg_class c ON c.oid = a.attrelid "
                    "WHERE c.relname = 'star_view' ORDER BY a.attnum"))))
  (is (nil? (:err (ex "CREATE VIEW star_source AS SELECT id FROM star_base"))))
  (is (nil? (:err (ex "CREATE VIEW star_dependent AS SELECT * FROM star_source"))))
  (is (nil? (:err (ex (str "CREATE OR REPLACE VIEW star_source AS "
                           "SELECT id, label FROM star_base")))))
  (is (= [["1"]] (rows "SELECT * FROM star_dependent")))
  (is (= [["id"]]
         (rows (str "SELECT a.attname FROM pg_attribute a "
                    "JOIN pg_class c ON c.oid = a.attrelid "
                    "WHERE c.relname = 'star_dependent' ORDER BY a.attnum")))))

(deftest addressable-ddl-objects-have-postgresql-shaped-catalog-identities
  (doseq [ddl ["CREATE TABLE address_table (id integer, payload text)"
               "CREATE TYPE address_pair AS (left_value integer, right_value text)"
               "CREATE DOMAIN address_domain AS integer NOT NULL"
               "CREATE SEQUENCE address_sequence"
               "CREATE VIEW address_view AS SELECT id, payload FROM address_table"
               "CREATE INDEX address_payload_idx ON address_table (id, payload)"]]
    (is (nil? (:err (ex ddl))) ddl))
  (let [[table-oid table-type-oid]
        (mapv parse-long
              (first (rows (str "SELECT oid, reltype FROM pg_class "
                                "WHERE relname = 'address_table'"))))
        [table-type-type-oid table-typrelid]
        (mapv parse-long
              (first (rows (str "SELECT oid, typrelid FROM pg_type "
                                "WHERE typname = 'address_table'"))))
        [composite-class-oid composite-type-oid]
        (mapv parse-long
              (first (rows (str "SELECT oid, reltype FROM pg_class "
                                "WHERE relname = 'address_pair'"))))
        [composite-pg-type-oid composite-typrelid]
        (mapv parse-long
              (first (rows (str "SELECT oid, typrelid FROM pg_type "
                                "WHERE typname = 'address_pair'"))))]
    (is (= table-type-oid table-type-type-oid))
    (is (= table-oid table-typrelid))
    (is (not= table-oid table-type-oid))
    (is (= composite-type-oid composite-pg-type-oid))
    (is (= composite-class-oid composite-typrelid))
    (is (not= composite-class-oid composite-type-oid)))
  (is (= [["d" "23" "t"]]
         (rows (str "SELECT typtype, typbasetype, typnotnull FROM pg_type "
                    "WHERE typname = 'address_domain'"))))
  (is (= #{["address_sequence" "S"]
           ["address_view" "v"]
           ["address_payload_idx" "i"]}
         (set (rows (str "SELECT relname, relkind FROM pg_class WHERE relname IN "
                         "('address_sequence', 'address_view', 'address_payload_idx')")))))
  (is (= [["address_payload_idx" "address_table" "1 2" "f"]]
         (rows (str "SELECT i.relname, t.relname, x.indkey, x.indisunique "
                    "FROM pg_class i JOIN pg_index x ON x.indexrelid = i.oid "
                    "JOIN pg_class t ON t.oid = x.indrelid "
                    "WHERE i.relname = 'address_payload_idx'"))))
  (let [definition (ffirst
                    (rows (str "SELECT indexdef FROM pg_indexes "
                               "WHERE indexname = 'address_payload_idx'")))]
    (is (str/includes? definition
                       "INDEX address_payload_idx ON public.address_table"))
    (is (str/includes? definition "USING btree (id, payload)")))
  (let [[view-oid view-type-oid]
        (mapv parse-long
              (first (rows (str "SELECT oid, reltype FROM pg_class "
                                "WHERE relname = 'address_view'"))))
        [projected-type-oid projected-relation-oid]
        (mapv parse-long
              (first (rows (str "SELECT oid, typrelid FROM pg_type "
                                "WHERE typname = 'address_view'"))))]
    (is (= view-type-oid projected-type-oid))
    (is (= view-oid projected-relation-oid))
    (is (not= view-oid view-type-oid)))
  (doseq [ddl ["DROP INDEX address_payload_idx"
               "DROP VIEW address_view"
               "DROP SEQUENCE address_sequence"
               "DROP DOMAIN address_domain"
               "DROP TYPE address_pair"
               "DROP TABLE address_table"]]
    (is (nil? (:err (ex ddl))) ddl))
  (is (empty? (rows (str "SELECT relname FROM pg_class WHERE relname IN "
                         "('address_table', 'address_pair', 'address_sequence', "
                         "'address_view', 'address_payload_idx')"))))
  (is (empty? (rows (str "SELECT typname FROM pg_type WHERE typname IN "
                         "('address_table', 'address_pair', 'address_domain', "
                         "'address_view')")))))

(deftest user-defined-column-types-retain-their-catalog-oids
  (doseq [ddl ["CREATE TYPE catalog_mood AS ENUM ('low', 'high')"
               "CREATE DOMAIN catalog_mood_domain AS catalog_mood"
               (str "CREATE TYPE catalog_holder AS "
                    "(direct catalog_mood, wrapped catalog_mood_domain)")
               (str "CREATE TABLE catalog_typed "
                    "(direct catalog_mood, wrapped catalog_mood_domain, "
                    "holder catalog_holder)")]]
    (is (nil? (:err (ex ddl))) ddl))
  (let [type-oids
        (into {}
              (map (fn [[name oid]] [name oid]))
              (rows (str "SELECT typname, oid FROM pg_type WHERE typname IN "
                         "('catalog_mood', 'catalog_mood_domain', 'catalog_holder')")))
        mood-oid (get type-oids "catalog_mood")
        domain-oid (get type-oids "catalog_mood_domain")
        holder-oid (get type-oids "catalog_holder")]
    (is (= [[mood-oid]]
           (rows (str "SELECT typbasetype FROM pg_type "
                      "WHERE typname = 'catalog_mood_domain'"))))
    (is (= [["direct" mood-oid] ["wrapped" domain-oid]]
           (rows (str "SELECT a.attname, a.atttypid FROM pg_attribute a "
                      "JOIN pg_class c ON c.oid = a.attrelid "
                      "WHERE c.relname = 'catalog_holder' ORDER BY a.attnum"))))
    (is (= [["direct" mood-oid]
            ["wrapped" domain-oid]
            ["holder" holder-oid]]
           (rows (str "SELECT a.attname, a.atttypid FROM pg_attribute a "
                      "JOIN pg_class c ON c.oid = a.attrelid "
                      "WHERE c.relname = 'catalog_typed' ORDER BY a.attnum"))))))

(deftest explicit-unique-indexes-are-enforced-and-transactional
  (is (nil? (:err (ex "CREATE TABLE explicit_unique (id integer, code text)"))))
  (is (nil? (:err (ex "CREATE UNIQUE INDEX explicit_unique_code ON explicit_unique (code)"))))
  (is (= [["t"]]
         (rows (str "SELECT x.indisunique FROM pg_index x "
                    "JOIN pg_class i ON i.oid = x.indexrelid "
                    "WHERE i.relname = 'explicit_unique_code'"))))
  (is (nil? (:err (ex "INSERT INTO explicit_unique VALUES (1, 'same')"))))
  (is (= "23505" (:sqlstate
                  (ex "INSERT INTO explicit_unique VALUES (2, 'same')"))))
  (is (nil? (:err (ex "BEGIN"))))
  (is (nil? (:err (ex "DROP INDEX explicit_unique_code"))))
  (is (empty? (rows (str "SELECT oid FROM pg_class "
                         "WHERE relname = 'explicit_unique_code'"))))
  (is (nil? (:err (ex "ROLLBACK"))))
  (is (= 1 (count (rows (str "SELECT oid FROM pg_class "
                             "WHERE relname = 'explicit_unique_code'"))))))

(deftest drop-commands-refuse-the-wrong-relation-kind
  (doseq [ddl ["CREATE TABLE kind_table(id int)"
               "CREATE VIEW kind_view AS SELECT id FROM kind_table"
               "CREATE SEQUENCE kind_sequence"
               "CREATE INDEX kind_index ON kind_table(id)"]]
    (is (nil? (:err (ex ddl))) ddl))
  (let [before (into {} (rows (str "SELECT relname,oid FROM pg_class "
                                   "WHERE relname LIKE 'kind_%'")))]
    (doseq [ddl ["DROP TABLE IF EXISTS kind_view"
                 "DROP TABLE IF EXISTS kind_sequence"
                 "DROP VIEW IF EXISTS kind_table"
                 "DROP INDEX IF EXISTS kind_table"]]
      (is (= "42809" (:sqlstate (ex ddl))) ddl))
    (is (= before
           (into {} (rows (str "SELECT relname,oid FROM pg_class "
                               "WHERE relname LIKE 'kind_%'")))))))

(deftest views-freeze-star-shape-and-one-column-typmods
  (doseq [ddl ["CREATE TABLE freeze_left(a int)"
               "CREATE TABLE freeze_right(b int)"
               "INSERT INTO freeze_left VALUES(1)"
               "INSERT INTO freeze_right VALUES(2)"
               (str "CREATE VIEW freeze_joined AS SELECT * FROM freeze_left "
                    "CROSS JOIN freeze_right")
               (str "CREATE VIEW freeze_nested AS SELECT a FROM "
                    "(SELECT * FROM freeze_left CROSS JOIN freeze_right) q")
               "ALTER TABLE freeze_right ADD COLUMN a text"]]
    (is (nil? (:err (ex ddl))) ddl))
  (is (= [["1" "2"]] (rows "SELECT * FROM freeze_joined")))
  (is (= [["1"]] (rows "SELECT * FROM freeze_nested")))
  (is (nil? (:err (ex "CREATE TABLE freeze_typed(label varchar(12))"))))
  (is (nil? (:err (ex "CREATE VIEW freeze_one AS SELECT * FROM freeze_typed"))))
  (is (nil? (:err (ex (str "CREATE OR REPLACE VIEW freeze_one AS "
                           "SELECT label FROM freeze_typed"))))))

(deftest dropped-column-storage-does-not-leak-into-logical-lifecycle
  (doseq [ddl ["CREATE TABLE column_life(a int,b int NOT NULL,pg$att4 int)"
               "CREATE INDEX column_life_b_idx ON column_life(b)"
               "ALTER TABLE column_life DROP COLUMN b"
               "INSERT INTO column_life(a,pg$att4) VALUES(1,4)"
               "ALTER TABLE column_life RENAME COLUMN a TO b"
               "ALTER TABLE column_life ADD COLUMN a int"]]
    (is (nil? (:err (ex ddl))) ddl))
  (is (= [] (rows "SELECT indexname FROM pg_indexes WHERE indexname='column_life_b_idx'")))
  (is (= "42703" (:sqlstate
                  (ex "CREATE INDEX dead_column_idx ON column_life(no_such)"))))
  (is (= [["b" "1" "f"]
          ["........pg.dropped.2........" "2" "t"]
          ["pg$att4" "3" "f"]
          ["a" "4" "f"]]
         (rows (str "SELECT attname,attnum,attisdropped FROM pg_attribute a "
                    "JOIN pg_class c ON c.oid=a.attrelid "
                    "WHERE c.relname='column_life' ORDER BY attnum")))))

(deftest unsigned-composite-oids-cross-java-boundaries-without-post-commit-errors
  (let [catalog (catalog-objects/catalog-entity (d/db *conn*))]
    (d/transact *conn*
                [[:db/add (:db/id catalog)
                  :datahike.pg.catalog/next-oid 2147483648]])
    (is (nil? (:err (ex "CREATE TYPE unsigned_oid_record AS (a int)"))))
    (is (= [["2147483648"]]
           (rows "SELECT oid FROM pg_type WHERE typname='unsigned_oid_record'")))))

(deftest retired-column-names-cannot-reach-retained-storage
  (doseq [ddl ["CREATE TABLE retired_column(a int,b int UNIQUE)"
               "INSERT INTO retired_column VALUES(1,2)"
               "ALTER TABLE retired_column DROP COLUMN b"]]
    (is (nil? (:err (ex ddl))) ddl))
  (doseq [statement ["SELECT b FROM retired_column"
                     "SELECT a FROM retired_column WHERE b=2"
                     "INSERT INTO retired_column(a,b) VALUES(2,3)"
                     "INSERT INTO retired_column(a,b) SELECT 2,3 WHERE false"
                     "UPDATE retired_column SET b=3 WHERE false"
                     "DELETE FROM retired_column WHERE b=2"
                     "DELETE FROM retired_column WHERE a=1 RETURNING b"
                     (str "INSERT INTO retired_column(a) VALUES(2) "
                          "ON CONFLICT(b) DO NOTHING")]]
    (is (= "42703" (:sqlstate (ex statement))) statement))
  (is (= [["1"]] (rows "SELECT a FROM retired_column")))
  (is (nil? (:err (ex "ALTER TABLE retired_column RENAME COLUMN a TO current_a"))))
  (doseq [statement ["SELECT a FROM retired_column"
                     "INSERT INTO retired_column(a) VALUES(2)"
                     "UPDATE retired_column SET a=3"]]
    (is (= "42703" (:sqlstate (ex statement))) statement))
  (is (= [["1"]] (rows "SELECT current_a FROM retired_column"))))

(deftest relation-and-type-namespace-conflicts-fail-before-allocation
  (is (nil? (:err (ex "CREATE TABLE namespace_table (id integer)"))))
  (let [next-oid (:datahike.pg.catalog/next-oid
                  (catalog-objects/catalog-entity (d/db *conn*)))]
    (doseq [[statement state]
            [["CREATE SEQUENCE namespace_table" "42P07"]
             ["CREATE INDEX namespace_table ON namespace_table (id)" "42P07"]
             ["CREATE VIEW namespace_table AS SELECT id FROM namespace_table" "42P07"]
             [(str "CREATE OR REPLACE VIEW namespace_table AS "
                   "SELECT id FROM namespace_table") "42809"]
             ["CREATE TYPE namespace_table AS ENUM ('x')" "42710"]]]
      (is (= state (:sqlstate (ex statement))) statement)
      (is (= next-oid
             (:datahike.pg.catalog/next-oid
              (catalog-objects/catalog-entity (d/db *conn*)))))))
  (is (nil? (:err (ex "CREATE TYPE namespace_type AS ENUM ('x')"))))
  (is (= "42P07" (:sqlstate
                  (ex "CREATE TABLE namespace_type (id integer)"))))
  (is (= "42710" (:sqlstate
                  (ex "CREATE DOMAIN namespace_type AS text"))))
  (is (nil? (:err (ex "CREATE SEQUENCE namespace_sequence"))))
  (is (= "42P07" (:sqlstate
                  (ex "CREATE TABLE namespace_sequence (id integer)"))))
  (is (= "42P07" (:sqlstate
                  (ex "CREATE VIEW namespace_sequence AS SELECT 1 AS id"))))
  (is (= "42809" (:sqlstate (ex "DROP SEQUENCE namespace_table"))))
  (is (= "42P01" (:sqlstate (ex "DROP SEQUENCE missing_sequence"))))
  (is (nil? (:err (ex "DROP SEQUENCE IF EXISTS missing_sequence")))))

;; ============================================================================
;; pg_type — type name → OID probes
;; ============================================================================

(deftest test-pg-type-by-typname
  (testing "pg_type lookup by typname returns a single row with the OID"
    (let [r (ex "SELECT oid, typname FROM pg_type WHERE typname = 'int4'")]
      (is (nil? (:err r)))
      (is (= 1 (count (:rows r))))
      (is (= "23" (first (first (:rows r))))
          "int4 OID must be 23 per pg_type.dat"))))

(deftest catalog-name-columns-use-name-oid
  (testing "NameData catalog fields advertise PostgreSQL's name OID 19"
    (doseq [sql ["SELECT typname FROM pg_type"
                 "SELECT attname FROM pg_attribute"
                 "SELECT nspname FROM pg_namespace"
                 "SELECT rolname FROM pg_roles"
                 "SELECT datname FROM pg_database"
                 "SELECT proname FROM pg_proc"
                 "SELECT relname FROM pg_class"]]
      (is (= [19] (:oids (ex sql))) sql))))

(deftest unknown-pg-prefixed-relation-is-not-a-catalog
  (let [r (ex "SELECT * FROM pg_catalog.pg_databases")]
    (is (= "42P01" (:sqlstate r)))
    (is (re-find #"relation .* does not exist" (or (:err r) "")))))

(deftest test-pg-type-common-oids
  (testing "Common type OIDs present"
    ;; We only materialize a handful of common types. Spot-check three
    ;; that JDBC/psycopg2 probe on every connection.
    (doseq [[typname expected-oid]
            [["bool" "16"] ["int4" "23"] ["text" "25"]]]
      (let [r (ex (str "SELECT oid FROM pg_type WHERE typname = '" typname "'"))]
        (is (nil? (:err r)))
        (is (= expected-oid (first (first (:rows r))))
            (str typname " OID should be " expected-oid))))))

(deftest test-pg-type-in-clause
  (testing "WHERE oid IN (...) against pg_type (common JDBC probe)"
    (let [r (ex "SELECT typname FROM pg_type WHERE oid IN (16, 23, 25)")]
      (is (nil? (:err r)))
      (is (= #{"bool" "int4" "text"}
             (set (map first (:rows r))))))))

;; ============================================================================
;; pg_catalog-qualified access
;; ============================================================================

(deftest test-pg-catalog-prefix
  (testing "pg_catalog.pg_type resolves to the same table"
    (let [r (ex "SELECT oid FROM pg_catalog.pg_type WHERE typname = 'int4'")]
      (is (nil? (:err r)))
      (is (= "23" (first (first (:rows r))))))))

;; ============================================================================
;; information_schema.columns — per-column metadata
;; ============================================================================

(deftest test-information-schema-columns-lists-user-table
  (testing "information_schema.columns lists columns of our user table"
    (let [r (ex "SELECT column_name FROM information_schema.columns WHERE table_name = 'person'")]
      (is (nil? (:err r)))
      (is (seq (:rows r)) "must return at least one row for our `person` table")
      (let [cols (set (map first (:rows r)))]
        (is (contains? cols "name"))
        (is (contains? cols "age"))))))

;; ============================================================================
;; pg_class / pg_tables — table-level metadata
;; ============================================================================

(deftest test-pg-class-lists-user-table
  (testing "pg_class includes our user-defined table with the pg_class column set"
    (let [r (ex "SELECT relname FROM pg_class WHERE relname = 'person'")]
      (is (nil? (:err r)))
      (is (<= 1 (count (:rows r)))))))

(deftest test-pg-tables-lists-user-table
  (testing "pg_tables exposes tablename / schemaname — pgJDBC DatabaseMetaData probe"
    (let [r (ex "SELECT tablename FROM pg_tables WHERE tablename = 'person'")]
      (is (nil? (:err r)))
      (is (<= 1 (count (:rows r))))
      (is (= "person" (first (first (:rows r)))))))

  (testing "pg_tables filter by schemaname"
    (let [r (ex "SELECT schemaname, tablename FROM pg_tables WHERE schemaname = 'public' AND tablename = 'person'")]
      (is (nil? (:err r)))
      (is (= [["public" "person"]] (:rows r))))))

;; ============================================================================
;; pg_attribute — per-column metadata
;; ============================================================================

(deftest test-pg-attribute-of-user-table
  (testing "pg_attribute lists attributes for user tables"
    ;; Common JDBC probe shape: join pg_attribute × pg_class on attrelid.
    ;; Since D2, attrelid is the integer OID; look it up via the
    ;; ::regclass cast (as real PG clients do).
    (let [r (ex "SELECT attname FROM pg_attribute WHERE attrelid = 'person'::regclass")]
      (is (nil? (:err r)))
      (let [names (set (map first (:rows r)))]
        (is (contains? names "name") "pg_attribute must list `name`")
        (is (contains? names "age")  "pg_attribute must list `age`")))))

;; ============================================================================
;; pg_namespace — schemas
;; ============================================================================

(deftest test-pg-namespace
  (testing "pg_namespace returns at least the `public` schema"
    (let [r (ex "SELECT nspname FROM pg_namespace WHERE nspname = 'public'")]
      (is (nil? (:err r)))
      (is (some #(= "public" (first %)) (:rows r))))))

;; ============================================================================
;; pg_indexes / information_schema constraints — schema discovery
;; ============================================================================

(deftest test-pg-indexes-populated
  (testing "pg_indexes returns rows for each :db/unique attribute"
    (let [r (ex "SELECT tablename, indexname FROM pg_indexes ORDER BY indexname")]
      (is (nil? (:err r)))
      (is (= ["tablename" "indexname"] (:cols r)))
      (is (some (fn [[tbl idx]] (and (= "person" tbl)
                                     (= "person_name_key" idx)))
                (:rows r))))))

(deftest test-pg-indexes-projection
  (testing "projection to a single column returns that one column, not all 5"
    (let [r (ex "SELECT indexname FROM pg_indexes")]
      (is (nil? (:err r)))
      (is (= ["indexname"] (:cols r)))
      (is (some #(str/ends-with? (first %) "_key") (:rows r))))))

(deftest test-pg-indexes-where-filter
  (testing "WHERE tablename = 'person' returns only that table's indexes"
    (let [r (ex "SELECT indexname FROM pg_indexes WHERE tablename = 'person'")]
      (is (nil? (:err r)))
      (doseq [[idxname] (:rows r)]
        (is (str/starts-with? idxname "person_")))
      (is (pos? (count (:rows r)))))))

(deftest test-pg-indexes-count
  (testing "count(*) with WHERE returns actual filtered row count"
    (let [r (ex "SELECT count(*) FROM pg_indexes WHERE tablename = 'person'")]
      (is (nil? (:err r)))
      ;; :person/name has :db.unique/identity → one index row for person.
      (is (= "1" (first (first (:rows r))))))))

(deftest test-information-schema-table-constraints
  (testing "table_constraints exposes PRIMARY KEY + UNIQUE"
    (let [r (ex "SELECT constraint_type FROM information_schema.table_constraints")]
      (is (nil? (:err r)))
      (let [types (set (map first (:rows r)))]
        (is (contains? types "PRIMARY KEY") "implicit pkey per table")
        (is (contains? types "UNIQUE") "unique attribute exposed")))))

(deftest test-information-schema-table-constraints-where
  (testing "WHERE table_name = 'person' filters to that table's constraints"
    (let [r (ex "SELECT constraint_name, constraint_type
                  FROM information_schema.table_constraints
                  WHERE table_name = 'person'")]
      (is (nil? (:err r)))
      (doseq [[cname _] (:rows r)]
        (is (str/starts-with? cname "person_")
            (str "got " cname))))))

(deftest test-pg-proc-populated
  (testing "pg_proc exposes common built-in functions by name"
    (let [r (ex "SELECT proname FROM pg_proc WHERE proname = 'now'")]
      (is (nil? (:err r)))
      (is (= 1 (count (:rows r))) "now() must be listed"))
    (let [r (ex "SELECT proname FROM pg_proc WHERE proname = 'jsonb_set'")]
      (is (nil? (:err r)))
      (is (= 1 (count (:rows r)))))
    (let [r (ex "SELECT proname FROM pg_proc WHERE proname = 'does_not_exist_xyz'")]
      (is (nil? (:err r)))
      (is (zero? (count (:rows r))) "unknown proc must not match"))))

(deftest test-catalog-cache-shared-across-handlers
  (testing "two handlers sharing a schema hit the same cache entry"
    (let [cache @#'sql/global-catalog-cache]
      (.clear ^java.util.Map cache)
      (is (nil? (:err (ex "SELECT count(*) FROM pg_class"))))
      (let [size-after-1 (.size ^java.util.Map cache)]
        (is (pos? size-after-1) "first query warms the cache")
        ;; A fresh handler on the SAME conn sees the same user schema
        ;; (same content hash) and must reuse the entry — no new allocation.
        (let [h2 (pg/make-query-handler *conn*)
              before-size (.size ^java.util.Map cache)]
          (.execute h2 "SELECT count(*) FROM pg_class")
          (is (= before-size (.size ^java.util.Map cache))
              "second handler must hit cached entry, not allocate a new one"))))))

(deftest test-cross-catalog-join
  (testing "JOIN between two catalog tables materializes both sides"
    ;; Before round 3: only the FROM item was materialized; JOIN right-items
    ;; got ignored and the translator returned an empty result.
    (let [r (ex "SELECT c.relname, a.attname
                  FROM pg_class c JOIN pg_attribute a ON a.attrelid = c.oid
                  WHERE c.relname = 'person'")]
      (is (nil? (:err r)))
      (is (pos? (count (:rows r)))
          "pg_class JOIN pg_attribute must return at least one row"))
    ;; pg_indexes + information_schema.table_constraints — the two catalogs
    ;; that landed most recently. Both now inject into the same speculative
    ;; db so the JOIN resolves.
    (let [r (ex "SELECT pi.indexname, tc.constraint_type
                  FROM pg_indexes pi
                  JOIN information_schema.table_constraints tc
                    ON pi.tablename = tc.table_name
                  WHERE pi.tablename = 'person'")]
      (is (nil? (:err r)))
      (is (pos? (count (:rows r)))))))

(deftest test-pg-proc-arg-return-types
  (testing "prorettype and proargtypes expose best-effort type signatures"
    (let [r (ex "SELECT prorettype, proargtypes FROM pg_proc WHERE proname = 'upper'")]
      (is (nil? (:err r)))
      (is (= 1 (count (:rows r))))
      (let [[ret args] (first (:rows r))]
        ;; upper returns text (OID 25), takes text (OID 25)
        (is (= "25" ret))
        (is (= "25" args))))
    (let [r (ex "SELECT prorettype, proargtypes FROM pg_proc WHERE proname = 'length'")]
      (is (nil? (:err r)))
      (let [[ret args] (first (:rows r))]
        ;; length returns int4 (OID 23), takes text (OID 25)
        (is (= "23" ret))
        (is (= "25" args))))))

(deftest test-information-schema-key-column-usage
  (testing "key_column_usage exposes the constrained column"
    (let [r (ex "SELECT column_name FROM information_schema.key_column_usage")]
      (is (nil? (:err r)))
      (let [cols (set (map first (:rows r)))]
        (is (contains? cols "name") "UNIQUE on :person/name should surface 'name'")
        (is (contains? cols "db_id") "implicit pkey on db_id")))))

;; ============================================================================
;; Nested catalog references — derived tables + UNIONs.
;;
;; Catalog materialisation previously only fired for top-level PlainSelect
;; FROM/JOIN items, so `SELECT * FROM (SELECT … FROM pg_tables) t` and
;; `SELECT … FROM pg_tables UNION SELECT … FROM pg_views` both returned
;; zero rows. The recursive collector in catalog.clj plus the hoisted
;; materialisation at the top of parse-sql fix this uniformly.
;; ============================================================================

(deftest test-catalog-under-derived-table
  (testing "SELECT * FROM (SELECT … FROM pg_tables) t — inner catalog materialises"
    (let [r (ex "SELECT * FROM (SELECT tablename FROM pg_tables) t")]
      (is (nil? (:err r)))
      (is (contains? (set (map first (:rows r))) "person")))))

(deftest test-catalog-under-union
  (testing "UNION of catalog tables — each branch sees the materialised db"
    (let [r (ex (str "SELECT tablename FROM pg_tables "
                     "UNION SELECT viewname FROM pg_views"))]
      (is (nil? (:err r)))
      (is (contains? (set (map first (:rows r))) "person")))))

(deftest test-catalog-under-three-way-union
  (testing "Triple UNION pg_tables ∪ pg_views ∪ pg_matviews"
    (let [r (ex (str "SELECT schemaname, tablename FROM pg_tables "
                     "UNION SELECT schemaname, viewname FROM pg_views "
                     "UNION SELECT schemaname, matviewname FROM pg_matviews"))]
      (is (nil? (:err r)))
      ;; pg_views + pg_matviews are empty; only the pg_tables row flows.
      (is (= 1 (count (:rows r)))))))

(deftest test-derived-table-over-union-with-where-alias
  ;; Metabase build_privilege_map shape: `FROM (UNION-of-catalogs) t
  ;; WHERE t.schemaname …`. Inner UNION must materialise into the
  ;; derived-table alias so the outer WHERE can resolve `t.schemaname`.
  (testing "WHERE t.<col> binds through a UNION-rooted derived table"
    (let [r (ex (str "SELECT * FROM ("
                     "  SELECT schemaname, tablename FROM pg_tables "
                     "  UNION SELECT schemaname, viewname FROM pg_views"
                     ") t WHERE t.schemaname = 'public'"))]
      (is (nil? (:err r)))
      (is (= 1 (count (:rows r)))))))

;; ============================================================================
;; pg_catalog function stubs — psql `\d` family probes
;;
;; psql's meta-commands (\df \da \dF \l \d+ \sf …) gate their list
;; queries on pg_*_is_visible predicates and project pg_get_*_arguments
;; / pg_table_size / pg_size_pretty / pg_encoding_to_char. Without the
;; stubs the parser succeeds but execution raises "Unknown function";
;; with them, psql renders an empty/zero-valued row instead of an
;; error. These cover the wire-level shapes psql actually emits.
;; ============================================================================

(deftest test-pg-is-visible-stubs-return-true
  (doseq [fn-name ["pg_table_is_visible" "pg_function_is_visible"
                   "pg_type_is_visible" "pg_namespace_is_visible"
                   "pg_ts_config_is_visible" "pg_operator_is_visible"
                   "pg_conversion_is_visible"]]
    (testing fn-name
      (let [r (ex (str "SELECT " fn-name "(0)"))]
        (is (nil? (:err r)) (str fn-name " errored: " (:err r)))
        (is (= "t" (first (first (:rows r)))))))))

(deftest test-pg-get-function-stubs-return-empty-string
  (doseq [fn-name ["pg_get_function_arguments" "pg_get_function_result"
                   "pg_get_functiondef" "pg_get_function_identity_arguments"]]
    (testing fn-name
      (let [r (ex (str "SELECT " fn-name "(0)"))]
        (is (nil? (:err r)))
        (is (= "" (first (first (:rows r)))))))))

(deftest test-pg-table-size-stub-returns-zero
  (let [r (ex "SELECT pg_table_size(0)")]
    (is (nil? (:err r)))
    (is (= "0" (first (first (:rows r)))))))

(deftest test-pg-size-pretty-formatting
  (testing "byte-count formatter matches dbsize.c boundaries"
    (doseq [[bytes expected] [[0       "0 bytes"]
                              [1023    "1023 bytes"]
                              [10239   "10239 bytes"]
                              [10240   "10 kB"]
                              [1048576 "1024 kB"]
                              [10485760 "10 MB"]
                              [10737418240 "10 GB"]]]
      (let [r (ex (str "SELECT pg_size_pretty(" bytes ")"))]
        (is (nil? (:err r)))
        (is (= expected (first (first (:rows r))))
            (str bytes " bytes → " expected))))))

(deftest test-pg-encoding-to-char-returns-utf8
  (let [r (ex "SELECT pg_encoding_to_char(6)")]
    (is (nil? (:err r)))
    (is (= "UTF8" (first (first (:rows r)))))))

(deftest test-psql-df-shape
  ;; Canonical \df SQL — pg_function_is_visible + pg_get_function_*.
  ;; Validates the whole pipeline, not just individual stubs.
  (testing "psql \\df SQL parses and executes against an empty pg_proc"
    (let [r (ex (str "SELECT n.nspname, p.proname,"
                     " pg_catalog.pg_get_function_result(p.oid),"
                     " pg_catalog.pg_get_function_arguments(p.oid)"
                     " FROM pg_catalog.pg_proc p"
                     " LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace"
                     " WHERE pg_catalog.pg_function_is_visible(p.oid)"
                     "   AND n.nspname <> 'pg_catalog'"
                     "   AND n.nspname <> 'information_schema'"
                     " ORDER BY 1, 2"))]
      (is (nil? (:err r)) (str "expected clean parse+execute, got: " (:err r))))))

(deftest test-psql-d-plus-shape
  ;; Canonical \d+ SQL — adds pg_size_pretty(pg_table_size(c.oid))
  ;; on top of \dt's pg_class scan.
  (testing "psql \\d+ SQL parses and renders Size column"
    (let [r (ex (str "SELECT n.nspname, c.relname,"
                     " pg_catalog.pg_size_pretty(pg_catalog.pg_table_size(c.oid)) AS size"
                     " FROM pg_catalog.pg_class c"
                     " LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace"
                     " WHERE c.relkind IN ('r','p','v','m','S','f','')"
                     "   AND pg_catalog.pg_table_is_visible(c.oid)"))]
      (is (nil? (:err r)) (str "got: " (:err r)))
      ;; Every relation reports "0 bytes" — we don't track on-disk size.
      (is (every? (fn [row] (= "0 bytes" (nth row 2)))
                  (:rows r))))))

(deftest test-pg-policy-is-an-empty-real-catalog
  (testing "psql \\d probes row-level policies even when RLS is disabled"
    (let [r (ex (str "SELECT pol.polname, pol.polpermissive, pol.polroles,"
                     " pol.polqual, pol.polwithcheck, pol.polcmd"
                     " FROM pg_catalog.pg_policy pol"
                     " WHERE pol.polrelid = 0 ORDER BY 1"))]
      (is (nil? (:err r)) (str "got: " (:err r)))
      (is (empty? (:rows r))))))

(deftest test-pg-statistic-ext-is-an-empty-real-catalog
  (testing "psql \\d probes extended statistics for every relation"
    (let [r (ex (str "SELECT oid, stxrelid, stxnamespace, stxname,"
                     " 'd' = any(stxkind), stxstattarget"
                     " FROM pg_catalog.pg_statistic_ext"
                     " WHERE stxrelid = 0 ORDER BY stxnamespace, stxname"))]
      (is (nil? (:err r)) (str "got: " (:err r)))
      (is (empty? (:rows r))))))

(deftest test-publication-catalogs-are-empty-real-catalogs
  (testing "psql \\d checks logical-replication membership unconditionally"
    (doseq [sql ["SELECT oid, pubname, puballtables FROM pg_publication"
                 "SELECT oid, pnpubid, pnnspid FROM pg_publication_namespace"
                 "SELECT oid, prpubid, prrelid, prqual, prattrs FROM pg_publication_rel"]]
      (let [r (ex sql)]
        (is (nil? (:err r)) (str sql " got: " (:err r)))
        (is (empty? (:rows r)) sql)))))

(deftest test-pg-attrdef-reflects-persisted-defaults
  (.execute *h* (str "CREATE TABLE catalog_defaults ("
                     "fixed bit(4) DEFAULT B'0101', "
                     "varying varbit(5) DEFAULT '1001')"))
  (let [r (ex (str "SELECT a.attname, pg_get_expr(d.adbin, d.adrelid, true)"
                   " FROM pg_attribute a JOIN pg_attrdef d"
                   " ON d.adrelid = a.attrelid AND d.adnum = a.attnum"
                   " WHERE a.attrelid = (SELECT oid FROM pg_class"
                   " WHERE relname = 'catalog_defaults')"
                   " ORDER BY a.attnum"))]
    (is (nil? (:err r)) (str "got: " (:err r)))
    (is (= [["fixed" "'0101'::\"bit\""]
            ["varying" "'1001'::bit varying"]]
           (:rows r))))
  (testing "the correlated scalar shape used by psql \\d"
    (let [r (ex (str "SELECT a.attname,"
                     " (SELECT pg_get_expr(d.adbin, d.adrelid, true)"
                     " FROM pg_attrdef d"
                     " WHERE d.adrelid = a.attrelid"
                     " AND d.adnum = a.attnum AND a.atthasdef)"
                     " FROM pg_attribute a"
                     " WHERE a.attrelid = (SELECT oid FROM pg_class"
                     " WHERE relname = 'catalog_defaults')"
                     " ORDER BY a.attnum"))]
      (is (nil? (:err r)) (str "got: " (:err r)))
      (is (= [["fixed" "'0101'::\"bit\""]
              ["varying" "'1001'::bit varying"]]
             (:rows r))))))

;; ============================================================================
;; OPERATOR(qual.op) and COLLATE qual.X stripping — psql `\d <table>`.
;;
;; psql emits `relname OPERATOR(pg_catalog.~) '^x$' COLLATE pg_catalog.default`
;; for every "describe one thing by name" command. JSqlParser doesn't
;; accept either construct; we strip them in preprocess-sql so the
;; parser sees `relname ~ '^x$'`.
;; ============================================================================

(deftest test-operator-qualifier-strip
  (testing "OPERATOR(pg_catalog.~) strips to ~"
    (let [r (ex (str "SELECT relname FROM pg_class "
                     "WHERE relname OPERATOR(pg_catalog.~) '^person$'"))]
      (is (nil? (:err r)) (str "got: " (:err r)))
      (is (= ["person"] (mapv first (:rows r))))))
  (testing "Multiple operators in one query"
    (let [r (ex (str "SELECT relname FROM pg_class "
                     "WHERE relname OPERATOR(pg_catalog.~) '^.*$' "
                     "  AND relkind OPERATOR(pg_catalog.=) 'r'"))]
      (is (nil? (:err r))))))

(deftest test-collate-qualifier-strip
  (testing "COLLATE pg_catalog.default is stripped"
    (let [r (ex "SELECT relname FROM pg_class WHERE relname = 'person' COLLATE pg_catalog.default")]
      (is (nil? (:err r)) (str "got: " (:err r)))))
  (testing "COLLATE \"C\" (bare quoted) also stripped"
    (let [r (ex "SELECT relname FROM pg_class WHERE relname = 'person' COLLATE \"C\"")]
      (is (nil? (:err r))))))

;; ============================================================================
;; PG-style typinput: implicit coercion of unknown-string literals
;;
;; Mirrors src/backend/parser/parse_coerce.c:233 — when an unknown
;; (single-quoted) literal lands as the operand of an operator whose
;; other side has a determined type, PG runs the type's typinput. We
;; do the same at translate time when one operand resolves to a
;; Datahike-typed schema attribute. Drives `WHERE c.oid = '16384'`
;; from psql's `\d <table>` family.
;; ============================================================================

(deftest test-string-literal-coerced-to-long-on-equality
  (testing "oid = '<digits>' parses to long and matches"
    (let [pg-class-rows (rows "SELECT oid FROM pg_class WHERE relname = 'person'")
          oid (first (first pg-class-rows))]
      (is (= [[oid]]
             (rows (str "SELECT oid FROM pg_class WHERE oid = '" oid "'")))))))

(deftest test-string-literal-coerced-in-in-clause
  (testing "oid IN ('a','b','c') — each unknown literal coerced"
    (let [oids (set (map first
                         (rows "SELECT oid FROM pg_class WHERE relname IN ('person','pg_class')")))]
      ;; The IN-list literals are digit strings; the typinput should
      ;; long-coerce them and match.
      (let [oid-list (str/join "," (map #(str "'" % "'") oids))
            r (ex (str "SELECT oid FROM pg_class WHERE oid IN (" oid-list ")"))]
        (is (nil? (:err r)))
        (is (= oids (set (map first (:rows r)))))))))

(deftest test-string-literal-coerced-in-between
  (testing "oid BETWEEN '<lo>' AND '<hi>' — both bounds typinput-coerced"
    (let [;; Fetch the actual pg_class oids and bracket them.
          oids (mapv (comp #(Long/parseLong %) first)
                     (rows "SELECT oid FROM pg_class"))
          lo (str (apply min oids))
          hi (str (apply max oids))
          r (ex (str "SELECT relname FROM pg_class "
                     "WHERE oid BETWEEN '" lo "' AND '" hi "'"))]
      (is (nil? (:err r)))
      (is (= (count oids) (count (:rows r)))
          "BETWEEN with quoted bounds should match every row in [min..max]"))))

(deftest test-text-column-comparison-uncoerced
  (testing "text-typed column = 'literal' stays as string equality (no coercion)"
    (let [r (ex "SELECT relname FROM pg_class WHERE relname = 'person'")]
      (is (nil? (:err r)))
      (is (= [["person"]] (:rows r))))))

;; ============================================================================
;; BooleanValue + ParenthesedSelect in WHERE
;;
;; psql `\dC` (list casts) emits `WHERE ((true AND fn1) OR (true AND fn2))`.
;; psql `\dT` emits `WHERE (typrelid=0 OR (SELECT relkind='c' FROM ...))`.
;; ============================================================================

(deftest test-where-boolean-true-no-constraint
  (testing "WHERE true matches all rows"
    (let [r (ex "SELECT relname FROM pg_class WHERE true")]
      (is (nil? (:err r)))
      (is (pos? (count (:rows r)))))))

(deftest test-where-boolean-false-zero-rows
  (testing "WHERE false matches no rows"
    (let [r (ex "SELECT relname FROM pg_class WHERE false")]
      (is (nil? (:err r)))
      (is (= 0 (count (:rows r)))))))

(deftest test-where-true-and-fn-collapses-to-fn
  (testing "(true AND <fn>) OR (true AND <fn>) — psql \\dC shape"
    (let [r (ex (str "SELECT 1 FROM pg_class WHERE "
                     "((true AND pg_table_is_visible(oid)) "
                     " OR (true AND pg_table_is_visible(oid)))"))]
      (is (nil? (:err r))))))

(deftest test-where-scalar-subquery-treated-as-bool
  (testing "WHERE (SELECT relkind = 'r' FROM pg_class WHERE oid = ...) — \\dT shape"
    ;; The inner subquery is correlated — falls back to constant-false
    ;; (no rows match) which is semantically correct since the catalog
    ;; tables involved are empty in our impl. The query parses and
    ;; executes without error; that's the assertion.
    (let [r (ex (str "SELECT relname FROM pg_class WHERE "
                     "(SELECT c.relkind = 'r' FROM pg_class c WHERE c.oid = pg_class.oid)"))]
      (is (nil? (:err r)) (str "got: " (:err r))))))

;; ============================================================================
;; Scalar subquery in projection + ARRAY(SELECT) constructor.
;; psql `\d <table>` and `\dD` rely on these.
;; ============================================================================

(deftest test-scalar-subquery-in-projection
  (testing "Correlated scalar subquery against a non-existent attribute → NULL"
    ;; psql \d <table> projects:
    ;;   (SELECT pg_get_expr(d.adbin, d.adrelid, true) FROM pg_attrdef d
    ;;    WHERE d.adrelid = a.attrelid AND d.adnum = a.attnum AND a.atthasdef)
    ;; We don't model pg_attrdef so the inner produces no rows for any
    ;; outer row → NULL. The translator pre-evaluates and falls back
    ;; to :__null__ on correlation; the outer query parses cleanly.
    (let [r (ex (str "SELECT relname,"
                     " (SELECT 1 FROM pg_class c WHERE c.oid = pg_class.oid AND false) AS x"
                     " FROM pg_class LIMIT 1"))]
      (is (nil? (:err r))))))

(deftest test-array-from-subquery
  (testing "ARRAY(SELECT col FROM tbl) — empty result → empty array"
    (let [r (ex "SELECT array(SELECT relname FROM pg_class WHERE relname = '__no_such__')")]
      (is (nil? (:err r))))))
