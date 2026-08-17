(ns datahike.test.pg-whole-row-test
  "Whole-row references: a bare identifier naming a table in scope is a
   COMPOSITE value in PostgreSQL, not a column.

     SELECT t FROM t        ->  (1,a)
     SELECT to_json(t) …    ->  {\"id\":1,\"nm\":\"a\"}
     SELECT row_to_json(t)  ->  {\"id\":1,\"nm\":\"a\"}
     SELECT json_agg(t) …   ->  [{\"id\":1,…}, {\"id\":2,…}]

   `ctx/relation-in-scope?` already exempted the name from the 42703
   unknown-column check, so these parsed — but nothing produced a value,
   so every one of them silently returned NULL. `json_agg(t)` was the
   worst of it: `[null, null]`.

   This is also the shape PostgREST emits on every read, which is why it
   is worth more than its size.

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

(defn- seed! []
  (run "CREATE TABLE wr (id int, nm text)")
  (run "INSERT INTO wr VALUES (1,'a'),(2,'b')"))

(deftest whole-row-is-a-composite-not-null
  (seed!)
  (testing "bare table name in the select list"
    (is (= [["(1,a)"] ["(2,b)"]] (rows "SELECT wr FROM wr ORDER BY id"))))

  (testing "through a FROM alias"
    (is (= [["(1,a)"] ["(2,b)"]] (rows "SELECT x FROM wr x ORDER BY id"))))

  (testing "db_id is not a field — PG's whole row has only declared columns"
    (is (= "(1,a)" (v "SELECT wr FROM wr WHERE id = 1")))))

(deftest whole-row-through-the-json-family
  (seed!)
  (testing "to_json / row_to_json render compact objects"
    (is (= [["{\"id\":1,\"nm\":\"a\"}"] ["{\"id\":2,\"nm\":\"b\"}"]]
           (rows "SELECT to_json(wr) FROM wr ORDER BY id")))
    (is (= [["{\"id\":1,\"nm\":\"a\"}"] ["{\"id\":2,\"nm\":\"b\"}"]]
           (rows "SELECT row_to_json(wr) FROM wr ORDER BY id"))))

  (testing "to_jsonb spaces them — the two families differ in punctuation"
    (is (= [["{\"id\": 1, \"nm\": \"a\"}"] ["{\"id\": 2, \"nm\": \"b\"}"]]
           (rows "SELECT to_jsonb(wr) FROM wr ORDER BY id"))))

  (testing "json_agg over whole rows no longer leaks PgRecord internals"
    ;; Was [{":fields": null, ":type-oid": null}, …]: serialize-jsonb
    ;; emitted without normalising, and PgRecord is a defrecord so
    ;; `map?` is true for it.
    (let [s (v "SELECT json_agg(wr) FROM wr")]
      (is (not (re-find #":fields|:type-oid" s)) s)
      (is (re-find #"\"id\"" s) s)
      (is (re-find #"\"nm\"" s) s))))

(deftest array-punctuation-follows-the-family
  (testing "json is compact, jsonb is spaced — for ARRAYS as well as objects"
    (is (= "[1,2]"  (v "SELECT to_json(ARRAY[1,2])")))
    (is (= "[1, 2]" (v "SELECT to_jsonb(ARRAY[1,2])")))))

(deftest a-column-wins-over-the-relation-name
  ;; PostgreSQL resolves a bare identifier as a COLUMN first and only
  ;; then as a relation, so a table with a column of its own name keeps
  ;; the column meaning.
  (run "CREATE TABLE s (s int, other int)")
  (run "INSERT INTO s VALUES (7, 8)")
  (is (= "7" (v "SELECT s FROM s"))))

(deftest row-to-json-requires-a-composite
  (seed!)
  (testing "row_to_json is declared row_to_json(record); PG rejects a scalar"
    (is (re-find #"function row_to_json\(\w+\) does not exist"
                 (or (err "SELECT row_to_json(1)") ""))))
  (testing "to_json takes anyelement and accepts a scalar"
    (is (= "1" (v "SELECT to_json(1)")))))
