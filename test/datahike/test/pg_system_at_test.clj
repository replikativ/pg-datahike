(ns datahike.test.pg-system-at-test
  "Tests for the tx-time (SQL:2011 SYSTEM_TIME) SQL session var:
   `datahike.system_at`, a SQL:2011-compliant alias for the existing
   `datahike.as_of` var.

   Both names map to the same internal `:as-of` session-state key
   and route through `d/as-of` on the db. The dual surface lets
   pg-datahike speak SQL:2011 (`SYSTEM_TIME`) while keeping the
   datahike-native naming (`as_of`) for existing consumers.

   We test:
   - parse-temporal-set recognises both names and maps to :as-of
   - apply-temporal threads the parsed Date into d/as-of
   - per-statement override :as-of shadows session-state
   - per-statement override :as-of :all clears any session pin
   - end-to-end through the handler: SET / SELECT / RESET cycle
     completes without error."
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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          _ (d/transact conn test-schema)
          _ (d/transact conn [{:person/name "Alice" :person/age 30}
                              {:person/name "Bob"   :person/age 25}])
          handler (pg/make-query-handler conn)]
      (try
        (binding [*conn* conn
                  *handler* handler]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each pg-fixture)

(defn- err [^PgWireServer$QueryResult r] (.error r))

;; ============================================================================
;; parse-temporal-set

(deftest parse-temporal-set-recognises-system-at
  (testing "datahike.system_at maps to :as-of"
    (is (= [:as-of "2024-04-15T00:00:00Z"]
           (@parse-temporal-set "SET datahike.system_at = '2024-04-15T00:00:00Z'"))))
  (testing "datahike.as_of still maps to :as-of (legacy name)"
    (is (= [:as-of "2024-04-15T00:00:00Z"]
           (@parse-temporal-set "SET datahike.as_of = '2024-04-15T00:00:00Z'"))))
  (testing "RESET datahike.system_at clears via :as-of"
    (is (= [:as-of nil]
           (@parse-temporal-set "RESET datahike.system_at"))))
  (testing "SET '' clears the var"
    (is (= [:as-of nil]
           (@parse-temporal-set "SET datahike.system_at = ''")))))

;; ============================================================================
;; apply-temporal threads as-of through d/as-of

(deftest apply-temporal-marks-db-with-as-of
  (let [tx-at (java.util.Date.)
        session (atom {:as-of tx-at})
        marked (@apply-temporal (d/db *conn*) session)]
    (testing "session :as-of produces an AsOfDB wrapper"
      (is (instance? datahike.db.AsOfDB marked)))))

(deftest apply-temporal-no-as-of-wrapper-when-unset
  (let [session (atom {})
        plain (@apply-temporal (d/db *conn*) session)]
    (testing "no :as-of in session-state → no AsOfDB wrapper"
      (is (not (instance? datahike.db.AsOfDB plain))))))

;; ============================================================================
;; per-statement override shadows session

(deftest apply-temporal-override-as-of-shadows-session
  (let [session-at (java.util.Date. 1000000)
        override-at (java.util.Date. 2000000)
        session (atom {:as-of session-at})
        marked (@apply-temporal (d/db *conn*) session {:as-of override-at})]
    (testing "per-statement :as-of override replaces session :as-of"
      (is (instance? datahike.db.AsOfDB marked))
      ;; AsOfDB has a `time-point` field — verify it picked the override
      (is (= override-at (.-time-point ^datahike.db.AsOfDB marked))))))

(deftest apply-temporal-override-as-of-all-clears-session
  (let [session-at (java.util.Date.)
        session (atom {:as-of session-at})
        plain (@apply-temporal (d/db *conn*) session {:as-of :all})]
    (testing "per-statement :as-of :all clears any session pin for this stmt"
      (is (not (instance? datahike.db.AsOfDB plain)))
      (is (= session-at (:as-of @session))
          "session-state untouched — :as-of pin restored for next stmt"))))

;; ============================================================================
;; End-to-end through the handler

(deftest set-system-at-survives-roundtrip
  (testing "SET datahike.system_at = '<iso-instant>' completes without error"
    (let [r (.execute *handler* "SET datahike.system_at = '2099-01-01T00:00:00Z'")]
      (is (nil? (err r)))))
  (testing "Subsequent SELECT under system_at runs without error"
    (let [r (.execute *handler* "SELECT name, age FROM person ORDER BY age")]
      (is (nil? (err r)))))
  (testing "RESET datahike.system_at clears"
    (let [r (.execute *handler* "RESET datahike.system_at")]
      (is (nil? (err r))))))
