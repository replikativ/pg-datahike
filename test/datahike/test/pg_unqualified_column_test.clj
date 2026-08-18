(ns datahike.test.pg-unqualified-column-test
  "An unqualified column name is resolved against EVERY relation in
   scope, not just the default table.

   `ctx/resolve-column` used `(or table-alias default-table)`, so a bare
   name was only ever looked for on the first FROM item:

     SELECT tid FROM t, c WHERE c.tid = t.id
       -> ERROR: column \"tid\" does not exist

   And when more than one relation had the name we silently picked the
   default table's — a wrong answer rather than an error. PostgreSQL
   raises 42702 there.

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
(defn- state [sql] (.-sqlstate ^PgWireServer$QueryResult (run sql)))

(defn- seed! []
  (run "CREATE TABLE t (id int, nm text)")
  (run "INSERT INTO t VALUES (1,'a'),(2,'b')")
  (run "CREATE TABLE c (tid int, val int, nm text)")
  (run "INSERT INTO c VALUES (1,10,'x'),(1,11,'y'),(2,20,'z')"))

(deftest unqualified-name-resolves-across-from-items
  (seed!)
  (testing "a comma join"
    (is (= [["1"] ["1"] ["2"]]
           (rows "SELECT tid FROM t, c WHERE c.tid = t.id ORDER BY tid"))))

  (testing "an explicit JOIN"
    (is (= [["10"] ["11"] ["20"]]
           (rows "SELECT val FROM t JOIN c ON c.tid = t.id ORDER BY val"))))

  (testing "mixing columns from both relations, unqualified"
    (is (= [["1" "10"] ["1" "11"] ["2" "20"]]
           (rows "SELECT id, val FROM t JOIN c ON c.tid = t.id ORDER BY id, val"))))

  (testing "in WHERE and ORDER BY, not just the select list"
    (is (= [["11"] ["20"]]
           (rows "SELECT val FROM t JOIN c ON c.tid = t.id WHERE val > 10
                  ORDER BY val")))))

(deftest an-ambiguous-unqualified-name-raises-42702
  ;; `nm` exists on BOTH t and c. We used to answer with t.nm.
  (seed!)
  (is (= "42702" (state "SELECT nm FROM t, c WHERE c.tid = t.id")))
  (is (re-find #"column reference \"nm\" is ambiguous"
               (or (err "SELECT nm FROM t, c WHERE c.tid = t.id") "")))

  (testing "qualifying it resolves the ambiguity, as in PostgreSQL"
    (is (= [["a"] ["a"] ["b"]]
           (rows "SELECT t.nm FROM t, c WHERE c.tid = t.id ORDER BY t.nm")))
    (is (= [["x"] ["y"] ["z"]]
           (rows "SELECT c.nm FROM t, c WHERE c.tid = t.id ORDER BY c.nm")))))

(deftest single-relation-queries-are-unchanged
  ;; The search only runs when the name is unqualified AND more than one
  ;; relation is in scope, so the ordinary case resolves exactly as before.
  (seed!)
  (is (= [["1"] ["2"]] (rows "SELECT id FROM t ORDER BY id")))
  (is (= [["a"] ["b"]] (rows "SELECT nm FROM t ORDER BY nm")))
  (is (= "2" (v "SELECT count(*) FROM t")))
  (testing "a column that exists on NO relation still raises 42703"
    (is (= "42703" (state "SELECT nosuchcol FROM t")))))

(deftest db-id-still-means-the-entity
  ;; db_id is special-cased to the entity var and must not be routed
  ;; through the cross-relation search.
  (seed!)
  (is (= "2" (v "SELECT count(db_id) FROM t"))))

(deftest an-aliased-single-table-is-not-ambiguous
  ;; `table-aliases` registers BOTH `{alias -> name}` and `{name -> name}`
  ;; for ONE from item, so `FROM t x` looks like two relations. Counting
  ;; aliases rather than resolved attributes made every SQLAlchemy
  ;; introspection query fail with `typnamespace is ambiguous` — a
  ;; column that exists on pg_type alone.
  (seed!)
  (is (= [["1"] ["2"]] (rows "SELECT id FROM t x ORDER BY id")))
  (is (= [["a"] ["b"]] (rows "SELECT nm FROM t x ORDER BY nm")))
  (testing "the catalog shape that actually broke"
    (is (some? (v "SELECT count(*) FROM pg_type t WHERE typnamespace = 2200")))))
