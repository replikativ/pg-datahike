(ns datahike.test.pg-enum-domain-test
  "Coverage for `CREATE TYPE … AS ENUM` and `CREATE DOMAIN`. Both
   bypass JSqlParser (custom parsers in `datahike.pg.sql.types`) and
   land as registry entities (`:datahike.pg.enum/*`,
   `:datahike.pg.domain/*`). Columns declared with these types lower
   to the appropriate base type for storage but carry the enum/domain
   name in metadata so the dump can re-emit them."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as catalog-objects]
            [datahike.pg.constraints.row :as row-constraints]
            [datahike.pg.dump :as dump]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.row-eval :as row-eval])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn- jdbc-url [port]
  (str "jdbc:postgresql://localhost:" port "/datahike"
       "?user=datahike&password=datahike"))

(defn- enum-domain-fixture [f]
  (Class/forName "org.postgresql.Driver")
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          srv  (pg/start-server conn {:port 0 :compat :pg-dump})
          port (.getPort ^datahike.pg.PgWireServer (:server srv))]
      (try
        (binding [*port* port *conn* conn] (f))
        (finally
          (.stop ^datahike.pg.PgWireServer (:server srv))
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each enum-domain-fixture)

(defn- exec! [^Connection c ^String sql]
  (with-open [stmt (.createStatement c)] (.execute stmt sql)))

(defn- query-rows [^Connection c ^String sql]
  (with-open [stmt (.createStatement c) rs (.executeQuery stmt sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          out)))))

;; ============================================================================
;; ENUM
;; ============================================================================

(deftest create-enum-stores-registry-entity
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (let [vs-ord (ffirst (d/q '[:find ?vs :where
                                [?e :datahike.pg.enum/name "mood"]
                                [?e :datahike.pg.enum/values-ordered ?vs]]
                              (d/db *conn*)))]
      (is (= "sad\nok\nhappy" vs-ord)
          "values stored in declaration order")
      (is (pos? (ffirst (d/q '[:find ?oid :where
                               [?e :datahike.pg.enum/name "mood"]
                               [?e :datahike.pg.enum/oid ?oid]]
                             (d/db *conn*))))
          "enum receives a persistent PostgreSQL type OID"))))

(deftest enum-catalogs-share-a-stable-type-oid
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (let [rows (query-rows
                c
                (str "SELECT t.typname, t.typtype, e.enumlabel, e.enumsortorder "
                     "FROM pg_type t JOIN pg_enum e ON e.enumtypid = t.oid "
                     "WHERE t.typname = 'mood' ORDER BY e.enumsortorder"))]
      (is (= [["mood" "e" "sad" 1.0]
              ["mood" "e" "ok" 2.0]
              ["mood" "e" "happy" 3.0]]
             (mapv vec rows))))
    (is (= [[3]]
           (mapv vec
                 (query-rows
                  c "SELECT COUNT(*) FROM pg_enum WHERE enumtypid = 'mood'::regtype"))))))

(deftest enum-catalog-keeps-positions-notices-and-subscript-labels
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))
              statement (.createStatement c)]
    (exec! c "CREATE TYPE spectrum AS ENUM ('middle', 'end')")
    (exec! c "ALTER TYPE spectrum ADD VALUE 'start' BEFORE 'middle'")
    (exec! c "ALTER TYPE spectrum ADD VALUE 'between' AFTER 'middle'")
    (exec! c "ALTER TYPE spectrum ADD VALUE 'quarter' BEFORE 'between'")
    (exec! c "ALTER TYPE spectrum RENAME VALUE 'quarter' TO 'near-middle'")
    (is (= [["start" 0.0] ["middle" 1.0] ["near-middle" 1.25]
            ["between" 1.5] ["end" 2.0]]
           (query-rows c "SELECT enumlabel, enumsortorder FROM pg_enum
                          WHERE enumtypid = 'spectrum'::regtype ORDER BY enumsortorder")))
    (.execute statement "ALTER TYPE spectrum ADD VALUE IF NOT EXISTS 'start'")
    (let [warning (.getWarnings statement)]
      (is (= "42710" (some-> warning .getSQLState)))
      (is (some->> warning .getMessage (re-find #"already exists, skipping"))))
    (with-open [result (.executeQuery statement "SELECT ('{start,middle,end}'::spectrum[])[2]")]
      (is (= "spectrum" (.getColumnLabel (.getMetaData result) 1)))
      (is (.next result))
      (is (= "middle" (.getString result 1))))))

(deftest dropping-user-types-ignores-retired-tables-and-columns
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE ticket_state AS ENUM ('open', 'closed')")
    (exec! c "CREATE DOMAIN ticket_number AS integer CHECK (VALUE > 0)")
    (exec! c "CREATE TABLE ticket_ledger (id int, state ticket_state, ticket_no ticket_number)")
    (doseq [drop-sql ["DROP TYPE ticket_state" "DROP DOMAIN ticket_number"]]
      (is (= "2BP01" (try (exec! c drop-sql)
                          (catch java.sql.SQLException e (.getSQLState e))))))
    (exec! c "ALTER TABLE ticket_ledger DROP COLUMN ticket_no")
    (exec! c "DROP DOMAIN ticket_number")
    (exec! c "DROP TABLE ticket_ledger")
    (exec! c "DROP TYPE ticket_state")
    (is (= [[0]] (query-rows c "SELECT count(*) FROM pg_type WHERE typname IN ('ticket_state', 'ticket_number')")))))

(deftest enum-catalog-has-no-orphaned-labels
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (testing "the catalog OIDs join directly"
      (is (= [[3]]
             (mapv vec
                   (query-rows
                    c (str "SELECT COUNT(*) FROM pg_enum e JOIN pg_type t "
                           "ON t.oid = e.enumtypid WHERE t.typname = 'mood'"))))))
    (testing "PostgreSQL's cleanup invariant works as a correlated anti-join"
      (is (= []
             (mapv vec
                   (query-rows
                    c (str "SELECT enumlabel FROM pg_enum e WHERE NOT EXISTS "
                           "(SELECT 1 FROM pg_type t WHERE t.oid = e.enumtypid)"))))))
    (testing "and with the outer column left unqualified, as upstream writes it"
      (is (= []
             (mapv vec
                   (query-rows
                    c (str "SELECT enumlabel FROM pg_enum WHERE NOT EXISTS "
                           "(SELECT 1 FROM pg_type WHERE pg_type.oid = enumtypid)"))))))))

(deftest enum-and-table-oids-do-not-collide
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'happy')")
    (exec! c "CREATE TABLE enum_owner (id int)")
    (let [[[enum-oid]] (query-rows c "SELECT oid FROM pg_type WHERE typname = 'mood'")
          [[table-oid]] (query-rows c "SELECT oid FROM pg_class WHERE relname = 'enum_owner'")]
      (is (not= enum-oid table-oid)))))

(deftest create-enum-schema-qualified
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE public.mpaa AS ENUM ('G', 'PG', 'PG-13', 'R', 'NC-17')")
    (let [vs-ord (ffirst (d/q '[:find ?vs :where
                                [?e :datahike.pg.enum/name "mpaa"]
                                [?e :datahike.pg.enum/values-ordered ?vs]]
                              (d/db *conn*)))]
      (is (= "G\nPG\nPG-13\nR\nNC-17" vs-ord)
          "schema prefix stripped, values preserved"))))

(deftest enum-typed-column-stores-as-text
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (exec! c "CREATE TABLE t (id int PRIMARY KEY, m mood)")
    (exec! c "INSERT INTO t VALUES (1, 'happy')")
    (exec! c "INSERT INTO t VALUES (2, 'sad')")
    (let [rows (query-rows c "SELECT id, m FROM t ORDER BY id")]
      (is (= [[1 "happy"] [2 "sad"]] (mapv vec rows))))
    (testing "column metadata records the enum name"
      (let [eid (ffirst (d/q '[:find ?e :where [?e :db/ident :t/m]] (d/db *conn*)))
            ent (into {} (d/entity (d/db *conn*) eid))]
        (is (= "mood" (:datahike.pg/enum-of ent)))
        (is (= :db.type/string (:db/valueType ent)))))))

(deftest registered-enum-remains-a-valid-cast-target
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (is (= [["happy"]] (mapv vec (query-rows c "SELECT 'happy'::mood"))))
    (let [raised (try
                   (query-rows c "SELECT 'angry'::mood")
                   nil
                   (catch java.sql.SQLException e e))]
      (is (= "22P02" (some-> raised .getSQLState)))
      (is (re-find #"invalid input value for enum mood"
                   (or (some-> raised .getMessage) ""))))
    (exec! c "CREATE TABLE enum_cast_source (v text)")
    (exec! c "INSERT INTO enum_cast_source VALUES ('happy'), ('angry')")
    (let [raised (try
                   (query-rows c "SELECT v::mood FROM enum_cast_source")
                   nil
                   (catch java.sql.SQLException e e))]
      (is (= "22P02" (some-> raised .getSQLState))))))

(deftest enum-input-validation-uses-the-registry
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (is (= [[true false]]
           (mapv vec (query-rows
                      c (str "SELECT pg_input_is_valid('happy', 'mood'), "
                             "pg_input_is_valid('angry', 'mood')")))))
    (is (= [["invalid input value for enum mood: \"angry\"" "22P02"]]
           (mapv vec (query-rows
                      c (str "SELECT message, sql_error_code FROM "
                             "pg_input_error_info('angry', 'mood')")))))))

(deftest enum-ordered-helper-functions
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (is (= [["sad" "happy" "{sad,ok,happy}" "{ok,happy}"]]
           (mapv vec
                 (query-rows
                  c (str "SELECT enum_first(NULL::mood), enum_last('ok'::mood), "
                         "enum_range(NULL::mood)::text, "
                         "enum_range('ok'::mood, NULL)::text")))))
    (exec! c "CREATE TABLE enum_helper_source (m mood)")
    (exec! c "INSERT INTO enum_helper_source VALUES ('ok')")
    (is (= [["sad" "happy"]]
           (mapv vec
                 (query-rows c "SELECT enum_first(m), enum_last(m) FROM enum_helper_source"))))))

(deftest alter-enum-add-and-rename-values
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE planets AS ENUM ('venus', 'earth', 'mars')")
    (exec! c "ALTER TYPE planets ADD VALUE 'uranus'")
    (exec! c "ALTER TYPE planets ADD VALUE 'mercury' BEFORE 'venus'")
    (exec! c "ALTER TYPE planets ADD VALUE 'jupiter' AFTER 'mars'")
    (exec! c "ALTER TYPE planets ADD VALUE IF NOT EXISTS 'earth'")
    (exec! c "ALTER TYPE planets RENAME VALUE 'uranus' TO 'neptune'")
    (is (= [["mercury"] ["venus"] ["earth"] ["mars"] ["jupiter"] ["neptune"]]
           (mapv vec
                 (query-rows c (str "SELECT enumlabel FROM pg_enum "
                                    "WHERE enumtypid = 'planets'::regtype "
                                    "ORDER BY enumsortorder")))))
    (is (= [["mercury" "neptune"]]
           (mapv vec
                 (query-rows c "SELECT enum_first(NULL::planets), enum_last(NULL::planets)"))))))

(deftest alter-enum-validates-and-is-transactional
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'happy')")
    (doseq [[sql state]
            [["ALTER TYPE mood ADD VALUE 'sad'" "42710"]
             ["ALTER TYPE mood ADD VALUE 'ok' AFTER 'missing'" "22023"]
             ["ALTER TYPE mood RENAME VALUE 'missing' TO 'ok'" "22023"]]]
      (let [raised (try (exec! c sql) nil (catch java.sql.SQLException e e))]
        (is (= state (some-> raised .getSQLState)))))
    (.setAutoCommit c false)
    (exec! c "ALTER TYPE mood ADD VALUE 'ok' BEFORE 'happy'")
    (.rollback c)
    (.setAutoCommit c true)
    (is (= [["sad"] ["happy"]]
           (mapv vec
                 (query-rows c (str "SELECT enumlabel FROM pg_enum "
                                    "WHERE enumtypid = 'mood'::regtype "
                                    "ORDER BY enumsortorder")))))))

(deftest new-label-on-existing-enum-is-unsafe-until-commit
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'happy')")
    (.setAutoCommit c false)
    (exec! c "ALTER TYPE mood ADD VALUE 'ok'")
    (doseq [sql ["SELECT 'ok'::mood" "SELECT enum_last(NULL::mood)"
                 "SELECT enum_range(NULL::mood)"]]
      (exec! c "SAVEPOINT before_unsafe")
      (let [raised (try (query-rows c sql) nil (catch java.sql.SQLException e e))]
        (is (= "55P04" (some-> raised .getSQLState)))
        (is (str/includes? (or (some-> raised .getMessage) "")
                           "must be committed before they can be used")))
      (exec! c "ROLLBACK TO SAVEPOINT before_unsafe"))
    (.commit c)
    (is (= [["ok"]] (mapv vec (query-rows c "SELECT 'ok'::mood"))))
    (.setAutoCommit c false)
    (exec! c "CREATE TYPE fresh_mood AS ENUM ('sad')")
    (exec! c "ALTER TYPE fresh_mood ADD VALUE 'happy'")
    (is (= [["happy"]]
           (mapv vec (query-rows c "SELECT 'happy'::fresh_mood"))))
    (.rollback c)))

(deftest enum-comparisons-order-and-extrema-use-declaration-order
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE rainbow AS ENUM ('red', 'yellow', 'green', 'blue')")
    (exec! c "CREATE TABLE enum_order (col rainbow)")
    (exec! c "INSERT INTO enum_order VALUES ('green'), ('red'), ('blue'), ('yellow')")
    (is (= [["red"] ["yellow"] ["green"] ["blue"]]
           (mapv vec (query-rows c "SELECT col FROM enum_order ORDER BY col"))))
    (is (= [["red"] ["yellow"] ["green"] ["blue"]]
           (mapv vec (query-rows c "SELECT * FROM enum_order ORDER BY col"))))
    (is (= [["green"] ["blue"]]
           (mapv vec
                 (query-rows c "SELECT col FROM enum_order WHERE col > 'yellow' ORDER BY col"))))
    (is (= [["red" "blue"]]
           (mapv vec (query-rows c "SELECT min(col), max(col) FROM enum_order"))))
    (is (= [["red"] ["yellow"] ["green"] ["blue"]]
           (mapv vec
                 (query-rows
                  c (str "SELECT enumlabel FROM pg_enum "
                         "WHERE enumtypid = 'rainbow'::regtype "
                         "ORDER BY enumlabel::rainbow")))))))

(deftest prepared-enum-projections-keep-type-and-declaration-order
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE rainbow AS ENUM ('red', 'yellow', 'green', 'blue')")
    (exec! c "CREATE DOMAIN note AS text CHECK (VALUE <> '')")
    (exec! c "CREATE TABLE palette (color rainbow, caption note)")
    (exec! c "INSERT INTO palette VALUES ('green', 'g'), ('red', 'r'), ('blue', 'b'), ('yellow', 'y')")
    (doseq [sql ["SELECT color, caption FROM palette ORDER BY color"
                 "SELECT * FROM palette ORDER BY color"
                 "SELECT p.* FROM palette p ORDER BY p.color"
                 "WITH c AS (SELECT * FROM palette) SELECT * FROM c ORDER BY color"
                 "SELECT p.* FROM (SELECT color, caption FROM palette) p(shade, note) ORDER BY shade"]]
      (testing sql
        (with-open [stmt (.prepareStatement c sql)]
          (is (= ["rainbow" "text"] (mapv #(.getColumnTypeName (.getMetaData stmt) %) [1 2])))
          (with-open [rs (.executeQuery stmt)]
            (is (= ["rainbow" "text"] (mapv #(.getColumnTypeName (.getMetaData rs) %) [1 2])))
            (is (= [["red" "r"] ["yellow" "y"] ["green" "g"] ["blue" "b"]]
                   (loop [out []]
                     (if (.next rs)
                       (recur (conj out [(.getObject rs 1) (.getObject rs 2)]))
                       out))))))))
    (with-open [stmt (.prepareStatement c "SELECT 'red'::rainbow AS shade")]
      (is (= "rainbow" (.getColumnTypeName (.getMetaData stmt) 1))))
    (with-open [stmt (.prepareStatement c "SELECT * FROM (SELECT color FROM palette LIMIT 0) p")
                rs (.executeQuery stmt)]
      (is (= "rainbow" (.getColumnTypeName (.getMetaData rs) 1)))
      (is (false? (.next rs))))))

(deftest enum-query-results-preserve-write-types-and-catalog-dependencies
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE workflow_state AS ENUM ('7')")
    (exec! c "CREATE TABLE enum_input (state workflow_state)")
    (exec! c "INSERT INTO enum_input VALUES ('7'), (NULL)")
    (exec! c "CREATE TABLE enum_copy AS SELECT state FROM enum_input")
    (exec! c "CREATE TABLE enum_empty AS SELECT state FROM enum_input WITH NO DATA")
    (exec! c "CREATE TABLE numeric_output (quantity integer)")
    (exec! c "PREPARE store_number (integer) AS INSERT INTO numeric_output VALUES ($1)")
    (doseq [[sql state]
            [["INSERT INTO numeric_output SELECT state FROM enum_input" "42804"]
             ["EXECUTE store_number('7'::workflow_state)" "42804"]
             ["ALTER TABLE numeric_output ALTER COLUMN quantity TYPE bigint USING '7'::workflow_state" "42804"]
             ["INSERT INTO enum_copy VALUES ('wrong')" "22P02"]
             ["INSERT INTO enum_empty VALUES ('wrong')" "22P02"]
             ["SELECT coalesce(state, 'wrong') FROM enum_input" "22P02"]
             ["SELECT CASE WHEN state IS NULL THEN 'wrong' ELSE state END FROM enum_input" "22P02"]
             ["CREATE TABLE invalid_enum_copy AS SELECT coalesce(state, 'wrong') AS state FROM enum_input" "22P02"]]]
      (let [error (try (exec! c sql) nil (catch java.sql.SQLException e e))]
        (is (= state (some-> error .getSQLState)) sql)))
    (is (empty? (query-rows c "SELECT quantity FROM numeric_output")))
    (is (= [[nil]] (query-rows c "SELECT to_regclass('invalid_enum_copy')")))
    (doseq [table ["enum_copy" "enum_empty"]]
      (with-open [stmt (.prepareStatement c (str "SELECT state FROM " table))]
        (is (= "workflow_state" (.getColumnTypeName (.getMetaData stmt) 1)))))
    (exec! c "CREATE VIEW enum_view AS SELECT '7'::workflow_state AS state")
    (exec! c "DROP TABLE enum_input")
    (exec! c "DROP TABLE enum_empty")
    (let [drop-type-error #(try (exec! c "DROP TYPE workflow_state") nil
                                (catch java.sql.SQLException e (.getSQLState e)))]
      (is (= "2BP01" (drop-type-error)))
      (exec! c "DROP TABLE enum_copy")
      (is (= "2BP01" (drop-type-error)))
      (exec! c "DROP VIEW enum_view")
      (is (nil? (drop-type-error))))))

(deftest replacing-an-existing-enum-view-preserves-its-stored-text-type
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE legacy_state AS ENUM ('queued')")
    (exec! c "CREATE TABLE legacy_input (state legacy_state)")
    (exec! c "INSERT INTO legacy_input VALUES ('queued')")
    (exec! c "CREATE VIEW legacy_status AS SELECT state FROM legacy_input")
    (let [db (d/db *conn*)
          view (d/entity db [:datahike.pg/view-name "legacy_status"])
          columns (edn/read-string (:datahike.pg/view-columns view))
          [[oid]] (query-rows c "SELECT 'legacy_status'::regclass::oid")
          column (catalog-objects/column-by-attnum db (long oid) 1)]
      (d/transact *conn* [[:db/add (:db/id view) :datahike.pg/view-columns
                           (pr-str (assoc-in columns [0 :oid] 25))]
                          [:db/add (:db/id column) :datahike.pg.column/type-oid 25]]))
    (exec! c "CREATE OR REPLACE VIEW legacy_status AS SELECT i.state FROM legacy_input i WHERE i.state IS NOT NULL")
    (with-open [stmt (.prepareStatement c "SELECT state FROM legacy_status")]
      (is (= "text" (.getColumnTypeName (.getMetaData stmt) 1))))
    (is (= [["queued"]] (query-rows c "SELECT state FROM legacy_status")))))

(deftest concrete-enum-overload-wins-over-polymorphic-argument
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE rainbow AS ENUM ('red')")
    (exec! c "CREATE TYPE compass AS ENUM ('north')")
    (exec! c "CREATE FUNCTION label(anyenum) RETURNS text LANGUAGE plpgsql AS $$ BEGIN RETURN $1::text || ' generic'; END $$")
    (exec! c "CREATE FUNCTION label(rainbow) RETURNS text LANGUAGE plpgsql AS $$ BEGIN RETURN $1::text || ' color'; END $$")
    (is (= [["red color" "north generic"]]
           (query-rows c "SELECT label('red'::rainbow), label('north'::compass)")))))

(deftest enum-foreign-keys-use-nominal-types-and-default-primary-key
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE rainbow AS ENUM ('red', 'blue')")
    (exec! c "CREATE TYPE bogus AS ENUM ('red', 'blue')")
    (exec! c "CREATE TABLE enum_parent (id rainbow PRIMARY KEY)")
    (exec! c "CREATE TABLE enum_child (parent rainbow REFERENCES enum_parent)")
    (exec! c "INSERT INTO enum_parent VALUES ('red')")
    (exec! c "INSERT INTO enum_child VALUES ('red')")
    (doseq [sql ["INSERT INTO enum_child VALUES ('blue')"
                 "DELETE FROM enum_parent"]]
      (let [raised (try (exec! c sql) nil (catch java.sql.SQLException e e))]
        (is (= "23503" (some-> raised .getSQLState)))))
    (let [raised (try
                   (exec! c "CREATE TABLE enum_bogus (parent bogus REFERENCES enum_parent)")
                   nil
                   (catch java.sql.SQLException e e))]
      (is (= "42804" (some-> raised .getSQLState)))
      (is (str/includes? (some-> raised .getMessage)
                         "cannot be implemented")))))

(deftest enum-type-rename-and-drop-lifecycle
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'happy')")
    (exec! c "CREATE TABLE enum_lifecycle (m mood)")
    (exec! c "ALTER TYPE mood RENAME TO feeling")
    (exec! c "INSERT INTO enum_lifecycle VALUES ('happy')")
    (is (= [["happy"]] (mapv vec (query-rows c "SELECT m FROM enum_lifecycle"))))
    (is (= [[2]]
           (mapv vec
                 (query-rows
                  c "SELECT count(*) FROM pg_enum WHERE enumtypid = 'feeling'::regtype"))))
    (let [raised (try (exec! c "DROP TYPE feeling") nil
                      (catch java.sql.SQLException e e))]
      (is (= "2BP01" (some-> raised .getSQLState))))
    (exec! c "DROP TABLE enum_lifecycle")
    (exec! c "DROP TYPE feeling")
    (is (= [[0]]
           (mapv vec (query-rows c "SELECT count(*) FROM pg_type WHERE typname = 'feeling'"))))
    (exec! c "DROP TYPE IF EXISTS feeling")))

(deftest enum-definition-rejects-duplicate-and-oversized-labels
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (doseq [[sql state]
            [["CREATE TYPE duplicate_mood AS ENUM ('sad', 'ok', 'sad')" "42710"]
             [(str "CREATE TYPE long_mood AS ENUM ('" (apply str (repeat 64 "x")) "')")
              "42622"]]]
      (let [raised (try (exec! c sql) nil (catch java.sql.SQLException e e))]
        (is (= state (some-> raised .getSQLState)))))))

;; ============================================================================
;; DOMAIN
;; ============================================================================

(deftest create-domain-with-check
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN year AS integer CONSTRAINT year_check CHECK ((VALUE >= 1901) AND (VALUE <= 2155))")
    (let [ent (into {} (d/entity (d/db *conn*)
                                 [:datahike.pg.domain/name "year"]))]
      (is (= "integer" (:datahike.pg.domain/base-type ent)))
      (is (= "year_check" (:datahike.pg.domain/check-name ent)))
      (is (str/includes? (:datahike.pg.domain/check-expr ent) ">="))
      (is (str/includes? (:datahike.pg.domain/check-expr ent) "<=")))))

(deftest invalid-domain-clauses-do-not-leave-types-in-the-catalog
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (doseq [[clause state]
            [["CONSTRAINT fk REFERENCES absent(id)" "42601"]
             ["NOT NULL NO INHERIT" "42P17"]
             ["NULL NOT NULL" "42601"]
             ["NOT NULL NULL" "42601"]
             ["DEFAULT 3 DEFAULT 4" "42601"]
             ["DEFAULT NULL DEFAULT 4" "42601"]
             ["UNIQUE" "42601"]
             ["PRIMARY KEY" "42601"]
             ["GENERATED BY DEFAULT AS IDENTITY" "0A000"]
             ["CHECK (VALUE > 0) NO INHERIT" "42P17"]
             ["CHECK (VALUE > 0) DEFERRABLE" "0A000"]]]
      (let [sql (str "CREATE DOMAIN rejected_domain AS integer " clause)
            error (try (exec! c sql) nil (catch java.sql.SQLException e e))]
        (is (= state (some-> error .getSQLState)) sql)))
    (is (empty? (query-rows c "SELECT typname FROM pg_type WHERE typname = 'rejected_domain'")))
    (is (false? (exec! c "CREATE DOMAIN rejected_domain AS integer DEFAULT NULL CHECK (VALUE > 0)")))))

(deftest create-domain-bare-alias
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN big AS bigint")
    (let [ent (into {} (d/entity (d/db *conn*)
                                 [:datahike.pg.domain/name "big"]))]
      (is (= "bigint" (:datahike.pg.domain/base-type ent)))
      (is (nil? (:datahike.pg.domain/check-expr ent))))))

(deftest domain-check-enforces-on-insert
  ;; The CHECK expression on a DOMAIN must reject INSERTs whose value
  ;; falls outside the domain. PG raises 23514 (check_violation).
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN year AS integer CHECK (VALUE >= 1901 AND VALUE <= 2155)")
    (exec! c "CREATE TABLE film (id int PRIMARY KEY, y year)")
    ;; In-range value is accepted.
    (exec! c "INSERT INTO film VALUES (1, 2020)")
    ;; Out-of-range raises 23514 (check_violation).
    (let [raised (try
                   (exec! c "INSERT INTO film VALUES (2, 1850)")
                   nil
                   (catch java.sql.SQLException e e))]
      (is (some? raised) "domain CHECK must reject out-of-range value")
      (is (= "23514" (.getSQLState raised))))
    (let [raised (try
                   (exec! c "INSERT INTO film VALUES (3, 2200)")
                   nil
                   (catch java.sql.SQLException e e))]
      (is (some? raised))
      (is (= "23514" (.getSQLState raised))))
    ;; Only the in-range row landed.
    (let [rows (query-rows c "SELECT id, y FROM film ORDER BY id")]
      (is (= [[1 2020]] (mapv vec rows))))))

(deftest domain-check-enforces-on-direct-cast
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE rainbow AS ENUM ('red', 'green', 'blue', 'purple')")
    (exec! c "CREATE DOMAIN rgb AS rainbow CHECK (VALUE IN ('red', 'green', 'blue'))")
    (is (= [["green"]] (mapv vec (query-rows c "SELECT 'green'::rgb"))))
    (doseq [sql ["SELECT 'purple'::rgb" "SELECT 'purple'::rainbow::rgb"]]
      (let [raised (try (query-rows c sql) nil (catch java.sql.SQLException e e))]
        (is (= "23514" (some-> raised .getSQLState)))
        (is (str/includes? (or (some-> raised .getMessage) "")
                           "value for domain rgb violates check constraint \"rgb_check\""))))))

(deftest drop-domain-lifecycle
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN rgb AS text")
    (exec! c "DROP DOMAIN rgb")
    (let [raised (try (exec! c "SELECT 'red'::rgb") nil
                      (catch java.sql.SQLException e e))]
      (is (= "42704" (some-> raised .getSQLState))))
    (exec! c "DROP DOMAIN IF EXISTS rgb")
    (exec! c "CREATE DOMAIN rgb AS text")
    (exec! c "CREATE TABLE domain_dependent (value rgb)")
    (let [raised (try (exec! c "DROP DOMAIN rgb") nil
                      (catch java.sql.SQLException e e))]
      (is (= "2BP01" (some-> raised .getSQLState))))))

(deftest domain-check-between-enforces
  ;; `BETWEEN` had no clause in the old CHECK interpreter; the :else
  ;; fallback stringified the AST and returned truthy, so any value
  ;; passed. Regression guard.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN smallint_d AS integer CHECK (VALUE BETWEEN 1 AND 100)")
    (exec! c "CREATE TABLE t (id int PRIMARY KEY, n smallint_d)")
    (exec! c "INSERT INTO t VALUES (1, 50)")  ; in-range
    (exec! c "INSERT INTO t VALUES (2, 1)")   ; lower bound, inclusive
    (exec! c "INSERT INTO t VALUES (3, 100)") ; upper bound, inclusive
    (let [raised (try (exec! c "INSERT INTO t VALUES (4, 0)") nil
                      (catch java.sql.SQLException e e))]
      (is (some? raised) "below-range BETWEEN must reject")
      (is (= "23514" (.getSQLState raised))))
    (let [raised (try (exec! c "INSERT INTO t VALUES (5, 101)") nil
                      (catch java.sql.SQLException e e))]
      (is (some? raised) "above-range BETWEEN must reject")
      (is (= "23514" (.getSQLState raised))))
    (let [rows (query-rows c "SELECT id FROM t ORDER BY id")]
      (is (= [[1] [2] [3]] (mapv vec rows))))))

(deftest domain-check-null-passes
  ;; PG 3VL: CHECK that yields UNKNOWN (null comparison) is treated
  ;; as satisfied — the domain-not-null bit is the separate gate.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN nonneg AS integer CHECK (VALUE >= 0)")
    (exec! c "CREATE TABLE t (id int PRIMARY KEY, n nonneg)")
    (exec! c "INSERT INTO t VALUES (1, NULL)")  ; null → CHECK is unknown → ok
    (is (= [[1 nil]] (mapv vec (query-rows c "SELECT id, n FROM t"))))))

(deftest domain-check-evaluates-null-input
  ;; NULL input does not imply an UNKNOWN result: IS NOT NULL yields FALSE.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN present_int AS integer CHECK (VALUE IS NOT NULL)")
    (exec! c "CREATE TABLE t (id int PRIMARY KEY, n present_int)")
    (exec! c "INSERT INTO t VALUES (1, 10)")
    (doseq [sql ["INSERT INTO t VALUES (2, NULL)"
                 "INSERT INTO t (id) VALUES (3)"
                 "INSERT INTO t VALUES (1, NULL) ON CONFLICT (id) DO NOTHING"
                 "UPDATE t SET n = NULL WHERE id = 1"]]
      (let [raised (try (exec! c sql) nil
                        (catch java.sql.SQLException e e))]
        (is (= "23514" (some-> raised .getSQLState)) sql)))
    (is (= [[1 10]] (mapv vec (query-rows c "SELECT id, n FROM t"))))))

(deftest update-validates-domain-and-enum-in-transactions
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN false_only AS boolean NOT NULL CHECK (VALUE = FALSE)")
    (exec! c "CREATE TYPE status AS ENUM ('on', 'off')")
    (exec! c "CREATE TABLE t (id int PRIMARY KEY, b false_only, s status)")
    (exec! c "INSERT INTO t VALUES (1, FALSE, 'on')")
    ;; A FALSE value must reach the domain CHECK as FALSE (not as absent).
    (let [db (d/db *conn*)]
      (is (nil? (row-constraints/validate-pre-arbiter!
                 db "t" {:t/id 1 :t/b false :t/s "on"} (row-constraints/constraint-plan db "t")
                 (row-eval/check-fn db) nil))))
    (doseq [transaction? [false true]
            [sql state] [["UPDATE t SET b = NULL WHERE id = 1" "23502"]
                         ["UPDATE t SET b = TRUE WHERE id = 1" "23514"]
                         ["UPDATE t SET s = 'invalid' WHERE id = 1" "22P02"]]]
      (when transaction? (exec! c "BEGIN"))
      (try
        (let [raised (try (exec! c sql) nil
                          (catch java.sql.SQLException e e))]
          (is (= state (some-> raised .getSQLState))
              (str "transaction=" transaction? ": " sql)))
        (finally (when transaction? (exec! c "ROLLBACK")))))
    (exec! c "UPDATE t SET b = FALSE, s = 'off' WHERE id = 1")
    (is (= [[false "off"]] (mapv vec (query-rows c "SELECT b, s FROM t"))))
    (exec! c "CREATE TABLE nullable_bool (id int PRIMARY KEY, b boolean)")
    (exec! c "INSERT INTO nullable_bool VALUES (1, FALSE)")
    (exec! c "UPDATE nullable_bool SET b = NULL WHERE id = 1")
    (is (= [[nil]] (mapv vec (query-rows c "SELECT b FROM nullable_bool"))))))

(deftest enum-membership-enforces-on-insert
  ;; An ENUM column accepts only declared members. Non-members raise
  ;; 22P02 (invalid_text_representation), matching real PG.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (exec! c "CREATE TABLE p (id int PRIMARY KEY, m mood)")
    (exec! c "INSERT INTO p VALUES (1, 'happy')")
    (let [raised (try
                   (exec! c "INSERT INTO p VALUES (2, 'angry')")
                   nil
                   (catch java.sql.SQLException e e))]
      (is (some? raised) "ENUM rejects non-members")
      (is (= "22P02" (.getSQLState raised))))
    (let [rows (query-rows c "SELECT id, m FROM p ORDER BY id")]
      (is (= 1 (count rows))))))

(deftest enum-membership-null-passes
  ;; NULL into an ENUM column is allowed (column-level NULL stays
  ;; the only gate on nullability).
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE status AS ENUM ('on', 'off')")
    (exec! c "CREATE TABLE s (id int PRIMARY KEY, st status)")
    (exec! c "INSERT INTO s VALUES (1, NULL)")
    (is (= [[1 nil]] (mapv vec (query-rows c "SELECT id, st FROM s"))))))

(deftest create-domain-quoted-name-with-non-ascii
  ;; Pagila's schema includes `CREATE DOMAIN public."bıgınt" AS bigint`
  ;; (Turkish dotless-i). Three quoted-name forms must all land under
  ;; the unquoted name only — the schema prefix is informational.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (testing "schema.\"name\""
      (exec! c "CREATE DOMAIN public.\"bıgınt\" AS bigint")
      (is (= "bigint" (:datahike.pg.domain/base-type
                       (d/entity (d/db *conn*)
                                 [:datahike.pg.domain/name "bıgınt"])))))
    (testing "\"schema\".\"name\""
      (exec! c "CREATE DOMAIN \"public\".\"smolint\" AS smallint")
      (is (= "smallint" (:datahike.pg.domain/base-type
                         (d/entity (d/db *conn*)
                                   [:datahike.pg.domain/name "smolint"])))))
    (testing "\"name\" alone"
      (exec! c "CREATE DOMAIN \"qüöte\" AS text")
      (is (= "text" (:datahike.pg.domain/base-type
                     (d/entity (d/db *conn*)
                               [:datahike.pg.domain/name "qüöte"])))))))

(deftest domain-typed-column-uses-base-type
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN year AS integer CHECK (VALUE > 0)")
    (exec! c "CREATE TABLE film (id int PRIMARY KEY, y year)")
    (exec! c "INSERT INTO film VALUES (1, 2020)")
    (exec! c "INSERT INTO film VALUES (2, 1990)")
    (let [rows (query-rows c "SELECT id, y FROM film ORDER BY id")]
      (is (= [[1 2020] [2 1990]] (mapv vec rows))))
    (testing "column metadata records the domain name + lowered base type"
      (let [eid (ffirst (d/q '[:find ?e :where [?e :db/ident :film/y]]
                             (d/db *conn*)))
            ent (into {} (d/entity (d/db *conn*) eid))]
        (is (= "year" (:datahike.pg/domain-of ent)))
        (is (= :db.type/long (:db/valueType ent)))))))

;; ============================================================================
;; Dump roundtrip — registry entities + column metadata survive a
;; dump/replay cycle.
;; ============================================================================

(deftest dump-emits-create-type-and-create-domain
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'happy')")
    (exec! c "CREATE DOMAIN year AS integer CHECK (VALUE > 0)")
    (exec! c "CREATE TABLE t (id int PRIMARY KEY, m mood, y year)")
    (let [out (dump/dump-to-string *conn*)]
      (testing "dump emits CREATE TYPE / CREATE DOMAIN sections"
        (is (str/includes? out "CREATE TYPE \"mood\" AS ENUM ('sad', 'happy')"))
        (is (str/includes? out "CREATE DOMAIN \"year\" AS integer"))
        (is (str/includes? out "CHECK")))
      (testing "column types refer to the enum/domain name, not the lowered base"
        ;; In the column block: `"m" mood` and `"y" year`, NOT `"m" text` / `"y" bigint`.
        (is (str/includes? out "\"m\" mood"))
        (is (str/includes? out "\"y\" year"))))))

(deftest dump-roundtrip-self-loop-with-enum-and-domain
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (exec! c "CREATE DOMAIN year AS integer CHECK (VALUE >= 1900)")
    (exec! c "CREATE TABLE film (id int PRIMARY KEY, m mood, y year)")
    (exec! c "INSERT INTO film VALUES (1, 'happy', 2020)")
    (exec! c "INSERT INTO film VALUES (2, 'sad',   1990)")
    (let [src-rows (query-rows c "SELECT id, m, y FROM film ORDER BY id")
          sql (dump/dump-to-string *conn*)
          tgt-cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
                   :schema-flexibility :write :keep-history? false}]
      (d/create-database tgt-cfg)
      (let [tgt-conn (d/connect tgt-cfg)
            tgt-srv (pg/start-server tgt-conn {:port 0 :compat :pg-dump})
            tgt-port (.getPort ^datahike.pg.PgWireServer (:server tgt-srv))]
        (try
          (with-open [tc (DriverManager/getConnection (jdbc-url tgt-port))
                      stmt (.createStatement tc)]
            (.execute stmt sql)
            (let [tgt-rows (query-rows tc "SELECT id, m, y FROM film ORDER BY id")]
              (is (= src-rows tgt-rows)
                  "rows match after dump/replay through enum+domain types")))
          (finally
            (.stop ^datahike.pg.PgWireServer (:server tgt-srv))
            (d/release tgt-conn)
            (d/delete-database tgt-cfg)))))))

;; ============================================================================
;; Edge cases
;; ============================================================================

(deftest enum-non-enum-create-type-falls-through
  ;; CREATE TYPE … AS (composite) should silently accept under :pg-dump
  ;; (we don't model composite types). Only AS ENUM is first-class.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE pt AS (x int, y int)")
    ;; No registry entity should be created.
    (is (empty? (d/q '[:find ?n :where [_ :datahike.pg.enum/name ?n]]
                     (d/db *conn*))))))

(deftest assignment-into-a-user-type-is-checked-by-its-name
  ;; DDL lowers an enum column to text and a domain column to its base
  ;; type, so an OID alone cannot tell one enum from another -- every
  ;; enum column accepted any text, and the error came later from the
  ;; enum's input function (22P02) or not at all. PostgreSQL rejects the
  ;; assignment itself (transformAssignedExpr). Expectations are
  ;; PostgreSQL 17's.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TYPE ad_mood AS ENUM ('ok','bad')")
    (exec! c "CREATE DOMAIN ad_dint AS int")
    (exec! c "CREATE TABLE ad (id int PRIMARY KEY, m ad_mood, m2 ad_mood, di ad_dint, t text, i int)")
    (exec! c "INSERT INTO ad VALUES (1, 'ok', 'bad', 5, 'x', 7)")
    (testing "what an enum column takes"
      (exec! c "UPDATE ad SET m = 'ok'")
      (exec! c "UPDATE ad SET m = 'ok'::ad_mood")
      (exec! c "UPDATE ad SET m = m2")
      (exec! c "UPDATE ad SET m = (SELECT m2 FROM ad LIMIT 1)")
      (exec! c "UPDATE ad SET m = CASE WHEN id = 1 THEN m2 END"))
    (testing "and what it does not"
      (doseq [[sql expected]
              [["UPDATE ad SET m = t" "column \"m\" is of type ad_mood but expression is of type text"]
               ["UPDATE ad SET m = 'x'::text" "column \"m\" is of type ad_mood but expression is of type text"]
               ["UPDATE ad SET m = 42" "column \"m\" is of type ad_mood but expression is of type integer"]]]
        (let [e (try (exec! c sql) nil (catch java.sql.SQLException e e))]
          (is (some? e) sql)
          (when e
            (is (= "42804" (.getSQLState ^java.sql.SQLException e)) sql)
            (is (re-find (re-pattern (java.util.regex.Pattern/quote expected))
                         (.getMessage ^java.sql.SQLException e))
                sql)))))
    (testing "the message names the user type on either side"
      (let [e (try (exec! c "UPDATE ad SET i = m") nil (catch java.sql.SQLException e e))]
        (is (re-find #"is of type integer but expression is of type ad_mood" (.getMessage ^java.sql.SQLException e))))
      (let [e (try (exec! c "UPDATE ad SET di = 'abc'::text") nil (catch java.sql.SQLException e e))]
        (is (re-find #"column \"di\" is of type ad_dint" (.getMessage ^java.sql.SQLException e))
            "a domain is named, not its base type")))
    (testing "a domain still takes whatever its base type takes"
      (exec! c "UPDATE ad SET di = i")
      (exec! c "UPDATE ad SET di = 1.5")
      (is (= [[2]] (query-rows c "SELECT di FROM ad"))))))

(deftest an-array-over-a-domain-checks-each-element
  ;; PostgreSQL applies a domain's constraint to every ELEMENT of an
  ;; array over it. The check bound `value` to the array's own text
  ;; instead, so `value > 0` compared a String to 0 and the client got
  ;; `class java.lang.Long cannot be cast to class java.lang.String` --
  ;; for `posint[]`, and even when every element satisfied the domain.
  ;;
  ;; It is PostgreSQL's own `domain` test, and its expectations are a
  ;; PostgreSQL 17 oracle's.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE DOMAIN posint AS int CHECK (value > 0)")
    (exec! c "CREATE TABLE pitable (f1 posint[])")
    (testing "an array whose elements all satisfy the domain goes in"
      (exec! c "INSERT INTO pitable VALUES (array[42])")
      (is (= ["{42}"] (mapv #(str (first %))
                            (query-rows c "SELECT f1 FROM pitable")))))
    (testing "one bad element rejects the whole array, named as the domain's"
      (doseq [sql ["INSERT INTO pitable VALUES (array[-1])"
                   "INSERT INTO pitable VALUES ('{0}')"
                   "INSERT INTO pitable VALUES ('{1,-2}')"]]
        (let [msg (try (exec! c sql) nil
                       (catch java.sql.SQLException e (.getMessage e)))]
          (is (re-find #"value for domain posint violates check constraint \"posint_check\""
                       (str msg))
              sql)
          (is (not (re-find #"(?i)cannot be cast|java\.lang" (str msg)))
              "an internal failure must never reach the client"))))
    (testing "a NULL element is exempt, as a NULL scalar is"
      (exec! c "INSERT INTO pitable VALUES ('{1,NULL,3}')")
      (is (= #{"{42}" "{1,NULL,3}"}
             (set (mapv #(str (first %))
                        (query-rows c "SELECT f1 FROM pitable"))))))))
