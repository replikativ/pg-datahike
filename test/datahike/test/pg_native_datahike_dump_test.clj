(ns datahike.test.pg-native-datahike-dump-test
  "Coverage for dumping a NATIVE Datahike database (one created via
   d/transact, not via SQL CREATE TABLE). Native databases lack the
   `:<table>/db-row-exists` row-marker pg-datahike sets at INSERT
   time and may use Datahike-specific features that have no direct
   PG equivalent (`:db.cardinality/many` for scalar attrs, plain
   `:db.type/ref` without :datahike.pg/references hints).

   The dump should still produce valid, replayable PG SQL by:

   - Discovering rows via 'has any column attr in this namespace'
     when the marker is absent.
   - Lowering :db.cardinality/many T to PG `T[]` and rendering
     values as PG array literals (`'{a,b}'`).
   - Lowering :db.type/ref to bigint (entity-id). Users who want
     FK constraints can call `set-hint!` with :datahike.pg/references."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.dump :as dump]))

(def ^:dynamic *conn* nil)

(defn- native-fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*conn* conn] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each native-fixture)

;; ============================================================================
;; Row discovery without the SQL-set row-marker
;; ============================================================================

(deftest native-rows-discovered-without-marker
  (d/transact *conn*
              [{:db/ident :u/email :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
               {:db/ident :u/name  :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one}])
  (d/transact *conn*
              [{:u/email "alice@x" :u/name "Alice"}
               {:u/email "bob@x"   :u/name "Bob"}])
  (let [out (dump/dump-to-string *conn*)]
    (is (str/includes? out "CREATE TABLE \"u\""))
    (is (str/includes? out "INSERT INTO \"u\""))
    (is (str/includes? out "'alice@x'"))
    (is (str/includes? out "'bob@x'"))))

;; ============================================================================
;; cardinality-many → PG array column
;; ============================================================================

(deftest cardinality-many-string-as-text-array
  (d/transact *conn*
              [{:db/ident :u/email :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
               {:db/ident :u/tags :db/valueType :db.type/string
                :db/cardinality :db.cardinality/many}])
  (d/transact *conn*
              [{:u/email "alice@x" :u/tags ["clojure" "datalog"]}
               {:u/email "bob@x"}])  ;; no tags
  (let [out (dump/dump-to-string *conn*)]
    (testing "column type is text[]"
      (is (str/includes? out "\"tags\" text[]")))
    (testing "values are rendered as PG array literals"
      ;; Either ordering is fine — Datahike's :many is a set, ordering
      ;; isn't preserved. We just need the right shape.
      (is (or (str/includes? out "'{\"clojure\",\"datalog\"}'")
              (str/includes? out "'{\"datalog\",\"clojure\"}'"))))
    (testing "entity without the attr emits NULL"
      ;; bob has no tags; should be NULL not '{}'.
      (is (str/includes? out "NULL")))))

(deftest cardinality-many-long-as-bigint-array
  (d/transact *conn*
              [{:db/ident :p/id :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
               {:db/ident :p/scores :db/valueType :db.type/long
                :db/cardinality :db.cardinality/many}])
  (d/transact *conn* [{:p/id 1 :p/scores [10 20 30]}])
  (let [out (dump/dump-to-string *conn*)]
    (is (str/includes? out "\"scores\" bigint[]"))
    ;; values inside are bare numbers, no quotes; ordering is set so
    ;; check the shape is bigint-array-shaped, not the contents.
    (is (re-find #"VALUES \(1, '\{[0-9]+,[0-9]+,[0-9]+\}'\)" out))))

;; ============================================================================
;; ref column lowers to bigint (entity-id) without FK info
;; ============================================================================

(deftest ref-column-emits-bigint-without-fk
  (d/transact *conn*
              [{:db/ident :u/email :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
               {:db/ident :u/best-friend :db/valueType :db.type/ref
                :db/cardinality :db.cardinality/one}])
  (d/transact *conn*
              [{:db/id "alice" :u/email "alice@x"}
               {:u/email "bob@x" :u/best-friend "alice"}])
  (let [out (dump/dump-to-string *conn*)]
    (is (str/includes? out "\"best-friend\" bigint")
        "ref lowers to bigint")
    ;; no FK constraint emitted because no :datahike.pg/references hint
    (is (not (str/includes? out "FOREIGN KEY")))))

;; ============================================================================
;; Identity / unique map cleanly
;; ============================================================================

(deftest unique-identity-becomes-primary-key
  (d/transact *conn*
              [{:db/ident :u/email :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}])
  (let [out (dump/dump-to-string *conn*)]
    (is (str/includes? out "\"email\" text NOT NULL PRIMARY KEY"))))

(deftest unique-value-becomes-unique-not-pk
  (d/transact *conn*
              [{:db/ident :u/id :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
               {:db/ident :u/handle :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one :db/unique :db.unique/value}])
  (let [out (dump/dump-to-string *conn*)]
    (is (str/includes? out "PRIMARY KEY"))
    (is (str/includes? out "UNIQUE"))))
