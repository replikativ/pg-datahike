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

(deftest extension-configuration-columns
  (is (= [["3079" "16384" "vector" "public" "t" "0.8.6" nil nil]]
         (rows "SELECT x.tableoid, x.oid, x.extname, n.nspname, x.extrelocatable,
                       x.extversion, x.extconfig, x.extcondition
                  FROM pg_extension x JOIN pg_namespace n ON n.oid = x.extnamespace")))
  (is (= [["oid[]" "text[]"]]
         (rows "SELECT pg_typeof(extconfig), pg_typeof(extcondition) FROM pg_extension"))))

(deftest namespace-acl-column
  (is (= [["2615" "2200" "public" "10" nil nil]]
         (rows "SELECT n.tableoid, n.oid, n.nspname, n.nspowner, n.nspacl,
                       acldefault('n', n.nspowner) AS acldefault
                  FROM pg_namespace n WHERE n.nspname = 'public'")))
  (let [^PgWireServer$QueryResult result (run "SELECT nspacl FROM pg_namespace")]
    (is (nil? (.-error result)))
    (is (= [1034] (vec (.-columnOids result))))))

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

(deftest vector-objects-belong-to-the-extension
  (is (= [["vector" "vector"]]
         (rows "SELECT e.extname, t.typname
                  FROM pg_extension e JOIN pg_depend d ON d.refobjid = e.oid
                  JOIN pg_type t ON t.oid = d.objid
                 WHERE d.refclassid = 'pg_extension'::regclass
                   AND d.classid = 'pg_type'::regclass AND d.deptype = 'e'")))
  (is (= [["vector_in"]]
         (rows "SELECT p.proname
                  FROM pg_extension e JOIN pg_depend d ON d.refobjid = e.oid
                  JOIN pg_proc p ON p.oid = d.objid
                 WHERE d.classid = 'pg_proc'::regclass AND d.deptype = 'e'
                   AND p.proname = 'vector_in'"))))

(deftest schema-definitions-respect-the-dump-search-path
  (doseq [sql ["CREATE TABLE restore_parent (id int PRIMARY KEY)"
               "CREATE TABLE restore_child (pid int REFERENCES restore_parent)"
               "CREATE VIEW restore_view AS SELECT id FROM restore_parent"
               "CREATE VIEW restore_nested AS SELECT q.pid FROM (SELECT pid FROM restore_child) q"]]
    (is (nil? (err sql)) sql))
  (is (= "SELECT id FROM restore_parent;"
         (v "SELECT pg_get_viewdef(oid) FROM pg_class WHERE relname = 'restore_view'")))
  (is (nil? (err "SELECT pg_catalog.set_config('search_path', '', false)")))
  (is (= "SELECT id FROM public.restore_parent;"
         (v "SELECT pg_get_viewdef(oid) FROM pg_class WHERE relname = 'restore_view'")))
  (is (= "SELECT q.pid FROM (SELECT pid FROM public.restore_child) q;"
         (v "SELECT pg_get_viewdef(oid) FROM pg_class WHERE relname = 'restore_nested'")))
  (is (= "FOREIGN KEY (pid) REFERENCES public.restore_parent (id)"
         (v "SELECT pg_get_constraintdef(oid) FROM pg_constraint WHERE contype = 'f'"))))

(deftest view-definitions-resolve-names-and-oids
  (doseq [sql ["CREATE TABLE view_source (n int)"
               "INSERT INTO view_source VALUES (42)"
               "CREATE VIEW \"Mirror View\" AS SELECT n FROM view_source"]]
    (is (nil? (err sql)) sql))
  (let [definition (v "SELECT pg_get_viewdef('\"Mirror View\"', true)")]
    (is (= "SELECT n FROM view_source;" definition))
    (is (= definition (v "SELECT pg_get_viewdef('\"Mirror View\"'::regclass, false)")))
    (is (nil? (v "SELECT pg_get_viewdef('view_source')")))
    (is (nil? (v "SELECT pg_get_viewdef('\"Mirror View\"', NULL::boolean)")))
    (doseq [[sql state] [["SELECT pg_get_viewdef('23')" "42P01"]
                         ["SELECT pg_get_viewdef('missing_schema.some_view')" "3F000"]
                         ["SELECT 'missing_schema.some_view'::regclass" "42P01"]
                         ["SELECT pg_get_viewdef('\"Mirror View\"', NULL)" "42725"]]]
      (is (= state (.-sqlstate ^PgWireServer$QueryResult (run sql))) sql)))
  (is (= "42P01" (v (str "SELECT sql_error_code FROM pg_input_error_info("
                         "'missing_schema.some_view', 'regclass')"))))
  (is (nil? (err "SET search_path TO pg_catalog")))
  (is (= "42P01" (.-sqlstate ^PgWireServer$QueryResult
                  (run "SELECT pg_get_viewdef('\"Mirror View\"')"))))
  (let [definition (v "SELECT pg_get_viewdef('public.\"Mirror View\"', true)")]
    (is (= "SELECT n FROM public.view_source;" definition))
    (is (nil? (err (str "CREATE VIEW public.restore_view AS " definition))))
    (is (= [["42"]] (rows "TABLE public.restore_view")))
    (is (nil? (err "DROP VIEW public.restore_view")))))
