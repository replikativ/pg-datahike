(ns datahike.test.pg-cache-isolation-test
  "Schema-derived caches must not leak between databases whose schema
   maps are structurally equal but whose ident-entity metadata
   (:pg/typmod, :pg/check-*, :__seq__ data, …) differs. The old
   WeakHashMap keying compared schema maps with .equals, so the first
   database to populate the cache answered for every equal-schema
   database — see datahike.pg.cache."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [r] (when-not (.error r) (mapv vec (.rows r))))

(defn- release! [{:keys [conn cfg]}]
  (d/release conn)
  (d/delete-database cfg))

(deftest numeric-scale-isolated-across-equal-schemas
  (testing "two DBs, same table shape, different NUMERIC typmod scales"
    ;; Both schema maps carry :db.type/bigdec for m/a and are
    ;; structurally EQUAL; the scale lives in the :pg/typmod ident fact.
    ;; With .equals-keyed caching the second database inherited the
    ;; first one's scale.
    (let [a (fresh-handler)
          b (fresh-handler)]
      (try
        (is (nil? (.error (exec a "CREATE TABLE m(v NUMERIC(10,2))"))))
        (is (nil? (.error (exec b "CREATE TABLE m(v NUMERIC(10,4))"))))
        (is (nil? (.error (exec a "INSERT INTO m(v) VALUES ('1.23456')"))))
        (is (nil? (.error (exec b "INSERT INTO m(v) VALUES ('1.23456')"))))
        (is (= [["1.23"]] (rows (exec a "SELECT v FROM m"))))
        (is (= [["1.2346"]] (rows (exec b "SELECT v FROM m"))))
        (finally
          (release! a)
          (release! b))))))

(deftest identity-cols-isolated-across-equal-schemas
  (testing "IDENTITY sequence data in one DB doesn't leak to another"
    ;; compute-identity-cols reads per-database :__seq__ datoms but is
    ;; cached per schema; two equal schemas must not share the answer.
    (let [a (fresh-handler)
          b (fresh-handler)]
      (try
        (is (nil? (.error (exec a "CREATE TABLE t(id INTEGER GENERATED ALWAYS AS IDENTITY, x INTEGER)"))))
        (is (nil? (.error (exec b "CREATE TABLE t(id INTEGER, x INTEGER)"))))
        (is (nil? (.error (exec a "INSERT INTO t(x) VALUES (7)"))))
        ;; A's id is auto-populated from its sequence…
        (is (= [["1" "7"]] (rows (exec a "SELECT id, x FROM t"))))
        ;; …but B (no IDENTITY) must not auto-populate id from A's cache
        ;; entry: id stays NULL.
        (is (nil? (.error (exec b "INSERT INTO t(x) VALUES (7)"))))
        (is (= [[nil "7"]] (rows (exec b "SELECT id, x FROM t"))))
        (finally
          (release! a)
          (release! b))))))
