(ns datahike.test.pg-enum-domain-test
  "Coverage for `CREATE TYPE … AS ENUM` and `CREATE DOMAIN`. Both
   bypass JSqlParser (custom parsers in `datahike.pg.sql.types`) and
   land as registry entities (`:datahike.pg.enum/*`,
   `:datahike.pg.domain/*`). Columns declared with these types lower
   to the appropriate base type for storage but carry the enum/domain
   name in metadata so the dump can re-emit them."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.dump :as dump]
            [datahike.pg.server :as pg])
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

(deftest domain-check-between-enforces
  ;; `BETWEEN` had no clause in eval-check-predicate; the :else
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
