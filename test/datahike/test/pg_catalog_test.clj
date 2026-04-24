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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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
     :cols (vec (.columnNames r))
     :rows (mapv vec (.rows r))}))

(defn- rows [sql] (:rows (ex sql)))

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
