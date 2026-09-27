(ns datahike.test.pg-roles-test
  "Roles, the encoding functions, and the privilege statements.

   This server has one login identity and enforces no privileges:
   `has_table_privilege` and its relatives answer `true` unconditionally
   and say so. That model decides what these statements may do.

   A ROLE is a real object even so -- it is what `pg_roles` lists, what
   an owner names, and what PostgreSQL's own tests create before doing
   anything else. Its OPTIONS are not: `SUPERUSER`, `NOLOGIN` and the
   rest are read and discarded rather than recorded as something nothing
   honours.

   `GRANT` is accepted: everything is already permitted, so granting
   more permits nothing new. `REVOKE` is refused, and the difference is
   not cosmetic -- it claims to take access away, and a caller told it
   succeeded has been told something false about who can read their
   data. PostgreSQL's own `privileges` test prints 192 `permission
   denied` lines this server cannot produce; that is the size of the gap
   a silent REVOKE would hide.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.sql.classify :as c]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec ^PgWireServer$QueryResult [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (mapv vec (.rows r))))

(defn- err [^PgWireServer$QueryResult r] (some-> (.error r) str))

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (err r)))
    r))

(defn- release! [{:keys [conn cfg]}] (d/release conn) (d/delete-database cfg))

(defmacro with-h [[sym init] & body]
  `(let [~sym ~init] (try ~@body (finally (release! ~sym)))))

(deftest the-encoding-functions
  ;; PostgreSQL's `collate.*` and `copyencoding` open by asking the
  ;; encoding and skip themselves when it is not UTF8, so being unable
  ;; to answer blocked the whole file.
  (with-h [h (fresh-handler)]
    (is (= [["UTF8"]] (rows (ok! h "SELECT getdatabaseencoding()"))))
    (is (= [["UTF8"]] (rows (ok! h "SELECT pg_client_encoding()"))))
    (is (= [["f"]] (rows (ok! h "SELECT getdatabaseencoding() <> 'UTF8'"))))
    (testing "declared `name`, as PostgreSQL declares them"
      (is (= [["name"]] (rows (ok! h "SELECT pg_typeof(getdatabaseencoding())")))))))

(deftest roles-are-objects
  (with-h [h (fresh-handler)]
    (ok! h "CREATE ROLE alpha")
    (testing "PostgreSQL answers CREATE ROLE to CREATE USER, and echoes DROP USER"
      (is (= "CREATE ROLE" (.commandTag (ok! h "CREATE USER beta NOLOGIN"))))
      (is (= "DROP USER" (.commandTag (ok! h "DROP USER beta")))))
    (testing "and they are in pg_roles"
      (ok! h "CREATE ROLE gamma SUPERUSER")
      (is (= [["alpha"] ["gamma"]]
             (rows (ok! h "SELECT rolname FROM pg_roles WHERE rolname IN ('alpha','gamma') ORDER BY rolname")))))
    (testing "a duplicate is 42710 and a missing one 42704"
      (is (str/includes? (err (exec h "CREATE ROLE alpha")) "role \"alpha\" already exists"))
      (is (str/includes? (err (exec h "DROP ROLE nosuch")) "role \"nosuch\" does not exist")))
    (testing "IF EXISTS skips, and several names drop together"
      (ok! h "DROP ROLE IF EXISTS nosuch")
      (ok! h "DROP ROLE alpha, gamma")
      (is (= [] (rows (ok! h "SELECT rolname FROM pg_roles WHERE rolname IN ('alpha','gamma')")))))))

(deftest create-user-mapping-is-not-a-role
  ;; `CREATE USER MAPPING FOR …` is a foreign-data statement. Reading it
  ;; as a role called "mapping" would have been silently wrong.
  (is (not= :create-role-object
            (:kind (c/classify "CREATE USER MAPPING FOR bob SERVER s")))))

(deftest grant-is-accepted-revoke-is-refused
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (testing "GRANT permits nothing new, because everything is permitted"
      (ok! h "GRANT SELECT ON t TO PUBLIC")
      (ok! h "GRANT ALL ON t TO PUBLIC"))
    (testing "REVOKE claims to take access away, and is refused loudly"
      (let [r (exec h "REVOKE SELECT ON t FROM PUBLIC")]
        (is (some? (.error r)))
        (is (str/includes? (err r) "not supported"))))))

(deftest the-point-constructor
  ;; A point LITERAL already worked; only the constructor was missing.
  (with-h [h (fresh-handler)]
    (is (= [["(1,2)"]] (rows (ok! h "SELECT point(1,2)"))))
    (is (= [["(1.5,-2.25)"]] (rows (ok! h "SELECT point(1.5,-2.25)"))))
    (testing "float8, so a whole number prints without its fraction"
      (is (= [["(1,2)"]] (rows (ok! h "SELECT point(1.0,2.0)")))))
    (is (= [["point"]] (rows (ok! h "SELECT pg_typeof(point(1,2))"))))))
