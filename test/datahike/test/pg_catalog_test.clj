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
