(ns datahike.test.pg-dump-interop-test
  "Interop with PostgreSQL's own dump/restore tooling, and the
   INSERT-VALUES expression evaluation that a dump exercises.

   We have always had our OWN dump — `datahike.pg.dump/dump` emits
   portable SQL that replays into either engine. What did not work was
   the standard client tool: `pg_dump`'s FIRST query is
   `SELECT pg_catalog.pg_is_in_recovery()`, so it aborted before doing
   anything, and restoring a real dump failed on its first COPY block
   because pg_dump always schema-qualifies (`COPY public.emp`). The
   COPY half is covered in pg_copy_e2e_test, which drives the wire
   protocol properly.

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
(defn- err [sql] (.-error ^PgWireServer$QueryResult (run sql)))

(deftest pg-dump-preflight-functions
  (testing "pg_is_in_recovery — pg_dump's first query; we are never a standby"
    (is (= "f" (v "SELECT pg_catalog.pg_is_in_recovery()")))
    (is (= "f" (v "SELECT pg_is_in_recovery()"))))

  (testing "acldefault — NULL, so pg_dump emits no GRANT/REVOKE"
    (is (nil? (v "SELECT acldefault('n', 10)"))))

  (testing "a role exists and owns the public schema"
    ;; pg_dump looks the owner up in its role map; a NULL owner resolved
    ;; to OID 0 and it aborted with "role with OID 0 does not exist".
    (is (= [["10" "datahike"]] (rows "SELECT oid, rolname FROM pg_roles")))
    (is (= [["public" "10"]]
           (rows "SELECT nspname, nspowner FROM pg_namespace WHERE nspname='public'")))))

(deftest insert-values-evaluates-function-calls
  ;; `INSERT INTO t VALUES (1, repeat('x',5))` stored the SQL TEXT
  ;; `repeat('x', 5)` — the 14-character string — because extract-value
  ;; fell through to `(str e)`.
  (run "CREATE TABLE iv (id int, s text, n int)")
  (testing "string functions"
    (run "INSERT INTO iv (id,s) VALUES (1, repeat('x',5))")
    (is (= "xxxxx" (v "SELECT s FROM iv WHERE id=1")))
    (run "INSERT INTO iv (id,s) VALUES (2, upper('abc'))")
    (is (= "ABC" (v "SELECT s FROM iv WHERE id=2")))
    (run "INSERT INTO iv (id,s) VALUES (3, concat('a','-','b'))")
    (is (= "a-b" (v "SELECT s FROM iv WHERE id=3"))))

  (testing "a result whose PG type is int4 arrives as an Integer and must widen"
    ;; :db.type/long rejects an Integer, so this failed with
    ;; "invalid input syntax" while abs(-7) — already a Long — passed.
    (run "INSERT INTO iv (id,n) VALUES (4, length('hello'))")
    (is (= "5" (v "SELECT n FROM iv WHERE id=4")))
    (run "INSERT INTO iv (id,n) VALUES (5, abs(-7))")
    (is (= "7" (v "SELECT n FROM iv WHERE id=5"))))

  (testing "an unimplemented function falls back to the old text behaviour"
    ;; Strictly better than storing nil, which would fail the INSERT.
    (run "INSERT INTO iv (id,s) VALUES (6, md5('x'))")
    (is (some? (v "SELECT s FROM iv WHERE id=6")))))

(deftest volatile-functions-are-still-deferred
  ;; The reason this evaluation is safe at PARSE time is that the
  ;; volatile functions are handled ABOVE it and emit markers instead.
  ;; Folding them into a cached parse would freeze them.
  (run "CREATE SEQUENCE s1")
  (run "CREATE TABLE vol (id int, t timestamp, n bigint)")
  (run "INSERT INTO vol VALUES (1, now(), nextval('s1'))")
  (run "INSERT INTO vol VALUES (2, now(), nextval('s1'))")
  (testing "nextval advances per execute rather than repeating"
    (is (= [["1"] ["2"]] (rows "SELECT n FROM vol ORDER BY id"))))
  (testing "now() is not frozen by the parse cache"
    (is (= "2" (v "SELECT count(DISTINCT t) FROM vol")))))
