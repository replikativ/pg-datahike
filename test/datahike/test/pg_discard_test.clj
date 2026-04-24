(ns datahike.test.pg-discard-test
  "Tests for DISCARD ALL / DISCARD {PLANS,SEQUENCES,TEMP,LOCKS}.

   DISCARD ALL is used by connection pools (pgbouncer, HikariCP, Odoo's
   psycopg2 pool) to reset a connection before returning it to the pool.
   A buggy DISCARD leaves locks held / savepoints active on the next
   checkout, producing cross-request state leaks."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def schema
  [{:db/ident :t/id   :db/valueType :db.type/long   :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :t/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])

(def data
  [{:t/id 1 :t/name "one"}
   {:t/id 2 :t/name "two"}])

(def ^:dynamic *conn* nil)

(defn fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema)
      (d/transact conn data)
      (try (binding [*conn* conn] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ex [h sql]
  (let [^PgWireServer$QueryResult r (.execute h sql)]
    {:err (.error r) :tag (.commandTag r)}))

(deftest test-discard-all-clears-transaction
  (testing "DISCARD ALL aborts an in-progress transaction"
    (let [h (pg/make-query-handler *conn*)]
      (is (nil? (:err (ex h "BEGIN"))))
      (is (= "DISCARD ALL" (:tag (ex h "DISCARD ALL"))))
      ;; After DISCARD ALL we should be out of the tx; a fresh BEGIN works
      (is (= "BEGIN" (:tag (ex h "BEGIN"))))
      (is (= "COMMIT" (:tag (ex h "COMMIT")))))))

(deftest test-discard-all-releases-locks
  (testing "DISCARD ALL releases FOR UPDATE locks so another session can acquire"
    (let [h1 (pg/make-query-handler *conn*)
          h2 (pg/make-query-handler *conn*)]
      ;; h1 acquires a lock on id=1
      (is (nil? (:err (ex h1 "SELECT t.id FROM t WHERE t.id = 1 FOR UPDATE NOWAIT"))))
      ;; h2 sees the conflict
      (is (re-find #"(?i)lock" (:err (ex h2 "SELECT t.id FROM t WHERE t.id = 1 FOR UPDATE NOWAIT"))))
      ;; h1 discards
      (is (= "DISCARD ALL" (:tag (ex h1 "DISCARD ALL"))))
      ;; h2 can now acquire
      (is (nil? (:err (ex h2 "SELECT t.id FROM t WHERE t.id = 1 FOR UPDATE NOWAIT")))))))

(deftest test-discard-all-resets-session-vars
  (testing "DISCARD ALL clears temporal session state (as-of/since/history)"
    (let [h (pg/make-query-handler *conn*)]
      (is (= "SET" (:tag (ex h "SET datahike.as_of = '2020-01-01T00:00:00Z'"))))
      (is (= "DISCARD ALL" (:tag (ex h "DISCARD ALL"))))
      ;; If DISCARD ALL didn't reset as_of, the next SELECT would see 0 rows
      ;; (the DB didn't exist yet in 2020). We inserted `data` just now so
      ;; the default (current) view shows 2 rows.
      (let [h2 (pg/make-query-handler *conn*)
            r  (.execute h2 "SELECT count(*) FROM t")]
        (is (nil? (.error ^PgWireServer$QueryResult r)))))))

(deftest test-discard-scoped-variants-are-noops
  (testing "DISCARD PLANS / SEQUENCES / TEMP / LOCKS are accepted as no-ops"
    (let [h (pg/make-query-handler *conn*)]
      (is (= "DISCARD PLANS"     (:tag (ex h "DISCARD PLANS"))))
      (is (= "DISCARD SEQUENCES" (:tag (ex h "DISCARD SEQUENCES"))))
      (is (= "DISCARD TEMP"      (:tag (ex h "DISCARD TEMP"))))
      (is (= "DISCARD LOCKS"     (:tag (ex h "DISCARD LOCKS")))))))
