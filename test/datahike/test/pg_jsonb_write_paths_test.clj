(ns datahike.test.pg-jsonb-write-paths-test
  "jsonb canonicalization has to happen on EVERY write path.

   PostgreSQL's `jsonb` is a parsed tree: key order is not preserved,
   duplicate keys collapse last-wins, whitespace is gone. We reproduce
   that by canonicalizing the text on the way in — but only where the
   column is known to be jsonb, and that knowledge lives in `:pg/type`
   on the ident entity rather than in Datahike's `:schema` map.

   `coerce-insert-value` read it with `(get-in schema [attr :pg/type])`,
   which answers nil unless the caller happened to enrich the schema
   first. Exactly one caller did. So canonicalization fired for a
   simple-protocol INSERT with an inline literal, and silently did not
   fire for:

     - a prepared INSERT (the value arrives as a ParamRef and is
       re-coerced at Execute against the raw schema),
     - any UPDATE.

   Which is to say: it fired for `psql`, and not for JDBC `setString`,
   asyncpg, psycopg, node-postgres or any ORM. The existing
   canonicalization test used a `Statement`, which is why this went
   unnoticed.

   A missing `:pg/type` now falls back to the ident-entity query rather
   than reading as \"not jsonb\".

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryResult
            PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*handler* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- v [sql] (ffirst (rows sql)))

;; The canonical form asserted here is PostgreSQL's own: keys ordered
;; length-first then bytewise, `": "` after a key and `", "` between
;; pairs. What this suite is really pinning is that EVERY write path
;; produces it, whatever it is.

(deftest canonicalizes-on-the-literal-insert-path
  (run "CREATE TABLE j1 (id int PRIMARY KEY, p jsonb)")
  (run "INSERT INTO j1 VALUES (1, '{\"b\":2,\"a\":1}')")
  (is (= "{\"a\": 1, \"b\": 2}" (v "SELECT p FROM j1 WHERE id = 1"))))

(deftest canonicalizes-on-the-update-path
  (testing "UPDATE stored the text verbatim — the tx builder coerced
            against the raw schema, where :pg/type is invisible"
    (run "CREATE TABLE j2 (id int PRIMARY KEY, p jsonb)")
    (run "INSERT INTO j2 VALUES (1, '{\"a\":1}')")
    (run "UPDATE j2 SET p = '{\"d\":4,\"c\":3}' WHERE id = 1")
    (is (= "{\"c\": 3, \"d\": 4}" (v "SELECT p FROM j2 WHERE id = 1"))))
  (testing "and duplicate keys still collapse last-wins on UPDATE"
    (run "UPDATE j2 SET p = '{\"a\":1,\"a\":9}' WHERE id = 1")
    (is (= "{\"a\": 9}" (v "SELECT p FROM j2 WHERE id = 1")))))

(deftest canonicalizes-on-the-prepared-insert-path
  (testing "a parameterised INSERT defers coercion to Execute, which ran
            against the raw schema"
    (run "CREATE TABLE j3 (id int PRIMARY KEY, p jsonb)")
    (run "PREPARE ins AS INSERT INTO j3 VALUES ($1, $2)")
    (run "EXECUTE ins (1, '{\"b\":2,\"a\":1}')")
    (is (= "{\"a\": 1, \"b\": 2}" (v "SELECT p FROM j3 WHERE id = 1")))))

(deftest json-is-never-canonicalized-on-any-path
  (testing "PG `json` is the text-faithful type: whitespace, key order
            and duplicate keys all survive, on every path"
    (run "CREATE TABLE j4 (id int PRIMARY KEY, p json)")
    (run "INSERT INTO j4 VALUES (1, '{ \"b\":1,  \"a\":2, \"a\":3 }')")
    (is (= "{ \"b\":1,  \"a\":2, \"a\":3 }" (v "SELECT p FROM j4 WHERE id = 1")))
    (run "UPDATE j4 SET p = '{ \"z\" : 9 }' WHERE id = 1")
    (is (= "{ \"z\" : 9 }" (v "SELECT p FROM j4 WHERE id = 1"))
        "UPDATE must not canonicalize a json column either")))

(deftest non-json-columns-are-unaffected
  (run "CREATE TABLE j5 (id int PRIMARY KEY, t text, n int)")
  (run "INSERT INTO j5 VALUES (1, '{\"b\":2,\"a\":1}', 5)")
  (is (= "{\"b\":2,\"a\":1}" (v "SELECT t FROM j5 WHERE id = 1"))
      "a text column holding JSON-looking text is left alone")
  (run "UPDATE j5 SET t = '{\"z\":1,\"y\":2}' WHERE id = 1")
  (is (= "{\"z\":1,\"y\":2}" (v "SELECT t FROM j5 WHERE id = 1"))))

;; ---------------------------------------------------------------------------
;; Over the wire. `PREPARE`/`EXECUTE` above is the SIMPLE protocol and takes a
;; different route, so it does not reach the Execute-time re-coercion where the
;; hole was — only a real driver's extended-protocol Bind does.
;; ---------------------------------------------------------------------------

(deftest canonicalizes-through-a-real-driver
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          factory (reify PgWireServer$QueryHandlerFactory
                    (create [_] (pg/make-query-handler conn)))
          server (PgWireServer. 0 "127.0.0.1" factory)]
      (.start server)
      (try
        (with-open [c (DriverManager/getConnection
                       (str "jdbc:postgresql://127.0.0.1:" (.getPort server)
                            "/datahike?user=datahike&password=x"
                            "&sslmode=disable&binaryTransfer=false"))]
          (with-open [st (.createStatement c)]
            (.execute st "CREATE TABLE jw (id int PRIMARY KEY, p jsonb)"))
          (testing "parameterised INSERT — the value arrives as a ParamRef and
                    is re-coerced at Execute, which used to see no :pg/type"
            (with-open [ps (.prepareStatement c "INSERT INTO jw VALUES (?, ?)")]
              (.setInt ps 1 1)
              (.setString ps 2 "{\"b\":2,\"a\":1}")
              (.execute ps)))
          (testing "UPDATE through the driver"
            (with-open [st (.createStatement c)]
              (.execute st "UPDATE jw SET p = '{\"d\":4,\"c\":3}' WHERE id = 1")))
          (with-open [st (.createStatement c)
                      rs (.executeQuery st "SELECT p FROM jw WHERE id = 1")]
            (.next rs)
            (is (= "{\"c\": 3, \"d\": 4}" (.getString rs 1))))
          (testing "and a parameterised INSERT that is never updated"
            (with-open [ps (.prepareStatement c "INSERT INTO jw VALUES (?, ?)")]
              (.setInt ps 1 2)
              (.setString ps 2 "{\"z\":1,\"y\":2}")
              (.execute ps))
            (with-open [st (.createStatement c)
                        rs (.executeQuery st "SELECT p FROM jw WHERE id = 2")]
              (.next rs)
              (is (= "{\"y\": 2, \"z\": 1}" (.getString rs 1))))))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))
