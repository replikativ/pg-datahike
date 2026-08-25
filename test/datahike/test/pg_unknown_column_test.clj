(ns datahike.test.pg-unknown-column-test
  "A reference to a column that does not exist must raise 42703, not read
   as NULL.

   Datalog binds a missing attribute with `get-else`, so an unresolvable
   name produced a well-formed NULL instead of an error. That turns a
   typo into silent data loss:

       UPDATE t SET a = 1 WHERE nosuchcol = 5   -- updated NOTHING
       SELECT * FROM t WHERE nam = 'x'          -- returned NOTHING
       INSERT INTO t (id) SELECT nosuchcol ...  -- inserted NULLs

   and no client can tell those apart from a genuinely empty result.

   The check can only run where the schema is EXHAUSTIVE, which means
   `:schema-flexibility :write`. Under `:read` a plain scalar attribute
   has no schema entry at all, so every column of a working query would
   look undefined; those databases stay permissive, which is the whole
   point of that mode. Catalog namespaces (pg_*, information_schema) are
   likewise exempt — they are synthesised, not schema-backed.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *conn* nil)

(defn- make-fixture [flexibility seed]
  (fn [f]
    (pg/reset-lock-registry!)
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility flexibility
               :keep-history? false}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (binding [*conn* conn
                    *handler* (pg/make-query-handler conn)]
            (seed)
            (f))
          (finally
            (d/release conn)
            (d/delete-database cfg)))))))

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- v [sql] (ffirst (rows sql)))
(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (run sql))
       (catch Exception e (ex-message e))))
(defn- state [sql]
  (try (.-sqlstate ^PgWireServer$QueryResult (run sql))
       (catch Exception e (:sqlstate (ex-data e)))))

;; ===========================================================================
;; :schema-flexibility :write — the schema is exhaustive, so the check runs
;; ===========================================================================

(defn- seed-write! []
  (run "CREATE TABLE uc (id int PRIMARY KEY, a int, name text)")
  (run "INSERT INTO uc VALUES (1, 10, 'x'), (2, 20, 'y')"))

(deftest ^:eftest/synchronized unknown-column-under-write-flexibility
  ((make-fixture :write seed-write!)
   (fn []
     (testing "projection"
       (is (= "42703" (state "SELECT nosuchcol FROM uc")))
       (is (re-find #"column \"nosuchcol\" does not exist" (or (err "SELECT nosuchcol FROM uc") ""))))

     (testing "the silent-empty-result cases — a WHERE on a typo used to
               match no rows and look like a legitimately empty table"
       (is (= "42703" (state "SELECT * FROM uc WHERE nosuchcol = 5")))
       (is (= "42703" (state "SELECT * FROM uc WHERE nam = 'x'")) "one-character typo"))

     (testing "the silent-data-loss cases"
       (is (= "42703" (state "UPDATE uc SET a = 1 WHERE nosuchcol = 5")))
       (is (= "42703" (state "DELETE FROM uc WHERE nosuchcol = 5")))
       (is (= "42703" (state "UPDATE uc SET nosuchcol = 1 WHERE id = 1")))
       (is (= [["10"] ["20"]] (rows "SELECT a FROM uc ORDER BY id"))
           "and nothing was changed on the way to the error"))

     (testing "IS NULL — read as true for every row, the inverted case"
       (is (= "42703" (state "SELECT id FROM uc WHERE nosuchcol IS NULL")))
       (is (= "42703" (state "SELECT id FROM uc WHERE nosuchcol IS NOT NULL"))))

     (testing "other clause positions"
       (is (= "42703" (state "SELECT id FROM uc ORDER BY nosuchcol")))
       (is (= "42703" (state "SELECT id FROM uc GROUP BY nosuchcol")))
       (is (= "42703" (state "SELECT sum(nosuchcol) FROM uc")))
       (is (= "42703" (state "SELECT a + nosuchcol FROM uc")))
       (is (= "42703" (state "SELECT id FROM uc u JOIN uc w ON u.nosuchcol = w.id"))))

     (testing "qualified by a table alias"
       (is (= "42703" (state "SELECT u.nosuchcol FROM uc u")))
       (is (= "42703" (state "SELECT uc.nosuchcol FROM uc"))))

     (testing "a QUALIFIER that is not in scope is 42P01, as in PostgreSQL
               — the relation is missing, not the column. This bound
               nothing, so the query failed internally and the client saw
               an empty result"
       (is (= "42P01" (state "SELECT other.a FROM uc")))
       (is (= "42P01" (state "SELECT other.nosuch FROM uc")))
       (is (= "42P01" (state "SELECT id FROM uc WHERE other.a = 1"))))

     ;; -- everything that must KEEP working -------------------------------
     (testing "real columns, every spelling"
       (is (= [["10"] ["20"]] (rows "SELECT a FROM uc ORDER BY id")))
       (is (= "10" (v "SELECT uc.a FROM uc WHERE id = 1")))
       (is (= "10" (v "SELECT u.a FROM uc u WHERE u.id = 1")))
       (is (= "10" (v "SELECT A FROM uc WHERE id = 1")) "case-folded"))

     (testing "output-column aliases in ORDER BY and GROUP BY resolve to the
               select list, not to a column — PostgreSQL's documented rule"
       (is (= [["10"] ["20"]] (rows "SELECT a AS x FROM uc ORDER BY x")))
       (is (= #{["10"] ["20"]} (set (rows "SELECT a AS x FROM uc GROUP BY x"))))
       (is (= [["10"] ["20"]] (rows "SELECT a AS x FROM uc ORDER BY x ASC"))))

     (testing "all DML rejects a missing target before lowering"
       (doseq [sql ["INSERT INTO nonesuch VALUES (1)"
                    "UPDATE nonesuch SET a = 1"
                    "DELETE FROM nonesuch"]]
         (is (= "42P01" (state sql)) sql)
         (is (= "relation \"nonesuch\" does not exist" (err sql)) sql)))

     (testing "DELETE requires a target relation"
       (is (= "42601" (state "DELETE FROM")))
       (is (not (re-find #"Cannot invoke|NullPointerException"
                         (or (err "DELETE FROM") "")))))

     (testing "whole-row and star references"
       (is (= [["1" "10" "x"] ["2" "20" "y"]] (rows "SELECT * FROM uc ORDER BY id")))
       (is (= [["1" "10" "x"]] (rows "SELECT u.* FROM uc u WHERE u.id = 1"))))

     (testing "columns of a CTE, derived table and table function — these
               are synthesised relations with no schema entry"
       (is (= [["7"]] (rows "WITH w AS (SELECT 7 AS z) SELECT z FROM w")))
       (is (= [["7"]] (rows "SELECT s.z FROM (SELECT 7 AS z) s")))
       (is (= [["1"] ["2"]] (rows "SELECT * FROM generate_series(1,2) g")))
       (testing "and an unknown column of a CTE is still an error"
         (is (= "42703" (state "WITH w AS (SELECT 7 AS z) SELECT nope FROM w")))))

     (testing "the catalog is synthesised, so it stays permissive"
       (is (some? (rows "SELECT relname FROM pg_class WHERE relname = 'uc'")))
       (is (some? (rows "SELECT table_name FROM information_schema.tables"))))

     (testing "INSERT of a column the table does not have"
       (is (= "42703" (state "INSERT INTO uc (id, nosuchcol) VALUES (9, 1)")))
       (is (= "42703" (state "INSERT INTO uc (id) SELECT nosuchcol FROM uc"))))

     (testing "an inherited column is resolved through the parent, so it
               must not be reported as undefined"
       (run "CREATE TABLE par2 (id int PRIMARY KEY, pname text)")
       (run "CREATE TABLE chi2 (cname text) INHERITS (par2)")
       (run "INSERT INTO chi2 (id, pname, cname) VALUES (1, 'p', 'c')")
       (is (= "p" (v "SELECT pname FROM chi2")))
       (is (= "p" (v "SELECT c.pname FROM chi2 c")))
       (is (= "42703" (state "SELECT c.nosuchcol FROM chi2 c")))))))

;; ===========================================================================
;; :schema-flexibility :read — no exhaustive schema, so no check
;; ===========================================================================

(defn- seed-read! []
  ;; `:read` does not forbid schema entries, it stops REQUIRING them. A
  ;; realistic database has some — a unique key, a ref — and holds the
  ;; rest as plain scalars with no entry at all. That mix is the point:
  ;; `person` is a nameable relation because `:person/id` is declared,
  ;; while `:person/name` and `:person/age` are not in the schema and
  ;; must still resolve.
  (d/transact *conn* [{:db/ident :person/id
                       :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one
                       :db/unique :db.unique/identity}])
  (d/transact *conn* [{:person/id 1 :person/name "Alice" :person/age 30}
                      {:person/id 2 :person/name "Bob" :person/age 40}]))

(deftest ^:eftest/synchronized read-flexibility-stays-permissive
  ((make-fixture :read seed-read!)
   (fn []
     (testing "a plain scalar attribute has NO schema entry under :read —
               enforcing would break every query against such a database"
       (is (nil? (get (:schema (d/db *conn*)) :person/name))
           "the premise of this test")
       (is (some? (get (:schema (d/db *conn*)) :person/id))
           "and `person` is nameable because one attribute IS declared")
       (is (= #{["Alice"] ["Bob"]} (set (rows "SELECT name FROM person"))))
       (is (= [["Alice"]] (rows "SELECT name FROM person WHERE age = 30")))
       (is (= "30" (v "SELECT p.age FROM person p WHERE p.name = 'Alice'"))))

     (testing "and an unknown column keeps its old NULL semantics here,
               which is what :read means"
       (is (= [[nil] [nil]] (rows "SELECT nosuchcol FROM person")))))))
