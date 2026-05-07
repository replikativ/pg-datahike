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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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
          "values stored in declaration order"))))

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
          tgt-cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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
