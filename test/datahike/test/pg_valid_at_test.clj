(ns datahike.test.pg-valid-at-test
  "Tests for the valid-time SQL session vars: `datahike.valid_at`,
   `datahike.valid_from`, `datahike.valid_to`.

   These vars are parsed by `parse-temporal-set`, stored on the
   handler's session-state atom, and applied by `apply-temporal` —
   `valid-at` calls `d/valid-at` to tag the db with a
   `:datahike/valid-at` marker that vt-aware secondary indices read
   via `sec/search-with-vt` (datahike feature/bitemporal-v1 commit 5).

   We test:
   - parse-temporal-set recognises the three vars
   - apply-temporal threads the parsed Date into `d/valid-at`
   - end-to-end through the handler: SET / SELECT / RESET cycle
     completes without error (no vt-aware index in fixture, so
     queries return normal results — the marker is inert)."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:private parse-temporal-set
  (resolve 'datahike.pg.server/parse-temporal-set))

(def ^:private apply-temporal
  (resolve 'datahike.pg.server/apply-temporal))

(def test-schema
  [{:db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :person/age
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}])

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)

(defn pg-fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          _ (d/transact conn test-schema)
          _ (d/transact conn [{:person/name "Alice" :person/age 30}
                              {:person/name "Bob"   :person/age 25}])
          handler (pg/make-query-handler conn)]
      (try
        (binding [*conn* conn *handler* handler]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each pg-fixture)

(defn- err [^PgWireServer$QueryResult r] (.error r))
(defn- rows [^PgWireServer$QueryResult r] (when-not (.error r) (vec (map vec (.rows r)))))

;; ============================================================================
;; parse-temporal-set

(deftest parse-temporal-set-recognises-valid-time-vars
  (testing "datahike.valid_at"
    (is (= [:valid-at "2024-04-15"]
           (@parse-temporal-set "SET datahike.valid_at = '2024-04-15'"))))
  (testing "datahike.valid_from"
    (is (= [:valid-from "2024-01-01"]
           (@parse-temporal-set "SET datahike.valid_from = '2024-01-01'"))))
  (testing "datahike.valid_to"
    (is (= [:valid-to "2024-07-01"]
           (@parse-temporal-set "SET datahike.valid_to = '2024-07-01'"))))
  (testing "RESET clears the var"
    (is (= [:valid-at nil]
           (@parse-temporal-set "RESET datahike.valid_at"))))
  (testing "SET '' clears the var"
    (is (= [:valid-at nil]
           (@parse-temporal-set "SET datahike.valid_at = ''")))))

;; ============================================================================
;; apply-temporal threads valid-at through d/valid-at

(deftest apply-temporal-marks-db-with-valid-at
  (let [at #inst "2024-04-15"
        session (atom {:valid-at at})
        marked (@apply-temporal (d/db *conn*) session)]
    (testing "session-state :valid-at flows to the db's metadata"
      (is (= at (:datahike/valid-at (meta marked)))))))

(deftest apply-temporal-no-marker-when-valid-at-unset
  (let [session (atom {})
        plain (@apply-temporal (d/db *conn*) session)]
    (testing "no :valid-at in session-state → no marker on db"
      (is (nil? (:datahike/valid-at (meta plain)))))))

(deftest apply-temporal-composes-valid-at-with-as-of
  (let [tx-at  (java.util.Date.)
        vt-at  #inst "2024-04-15"
        session (atom {:as-of tx-at :valid-at vt-at})
        composed (@apply-temporal (d/db *conn*) session)]
    (testing "as-of wraps tx-time AND valid-at marks vt independently"
      ;; as-of produced an AsOfDB. valid-at is metadata on the result.
      (is (= vt-at (:datahike/valid-at (meta composed)))))))

;; ============================================================================
;; End-to-end through the handler

(deftest set-valid-at-survives-roundtrip
  (testing "SET datahike.valid_at = '<iso-instant>' completes without error"
    (let [r (.execute *handler* "SET datahike.valid_at = '2024-04-15T00:00:00Z'")]
      (is (nil? (err r)))))
  (testing "SELECT under valid_at runs (marker is inert without vt-aware index)"
    (let [r (.execute *handler* "SELECT name, age FROM person ORDER BY age")]
      (is (nil? (err r)))
      (is (= 2 (count (rows r))))))
  (testing "RESET datahike.valid_at clears"
    (let [r (.execute *handler* "RESET datahike.valid_at")]
      (is (nil? (err r)))))
  (testing "after RESET, SELECT continues to work"
    (let [r (.execute *handler* "SELECT name FROM person ORDER BY name")]
      (is (nil? (err r)))
      (is (= [["Alice"] ["Bob"]] (rows r))))))
