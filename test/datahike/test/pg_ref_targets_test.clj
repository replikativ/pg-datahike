(ns datahike.test.pg-ref-targets-test
  "Direct tests for `datahike.pg.schema/derive-ref-targets` and
   `validate-ref-targets!` — the FK-projection inference that drives
   how `:db.type/ref` columns project at the SQL surface."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.schema :as pgs]))

(defn- make-db
  ([schema] (make-db schema []))
  ([schema data]
   (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
              :schema-flexibility :write
              :keep-history? false}]
     (d/create-database cfg)
     (let [conn (d/connect cfg)]
       (d/transact conn schema)
       (when (seq data) (d/transact conn data))
       (d/db conn)))))

;; ---------------------------------------------------------------------------
;; derive-ref-targets — pure schema-side logic

(deftest convention-matches-namespace
  (testing "ref attr name == target table → :unique/identity in that namespace"
    (let [schema-tx [{:db/ident :customer/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :customer/name :db/valueType :db.type/string
                      :db/cardinality :db.cardinality/one}
                     {:db/ident :order/id      :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/customer :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          schema (:schema (make-db schema-tx))]
      (is (= {:order/customer :customer/id}
             (pgs/derive-ref-targets schema {}))))))

(deftest hint-overrides-convention
  (testing ":datahike.pg/references hint wins over namespace convention"
    (let [schema-tx [{:db/ident :customer/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :customer/sku  :db/valueType :db.type/string
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/buyer  :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          schema (:schema (make-db schema-tx))
          hints  {:order/buyer {:references :customer/sku}}]
      (is (= {:order/buyer :customer/sku}
             (pgs/derive-ref-targets schema hints))
          ":order/buyer would have no convention match (no `buyer` namespace) — hint provides the target"))))

(deftest unmatched-ref-returns-no-entry
  (testing "ref attr with no namespace match and no hint → omitted from result"
    (let [schema-tx [{:db/ident :order/id      :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/buyer   :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          schema (:schema (make-db schema-tx))]
      (is (empty? (pgs/derive-ref-targets schema {}))
          "no `buyer` namespace exists, no hint, so no FK projection target"))))

(deftest cardinality-many-shape
  (testing ":db.cardinality/many refs return [target :many] vector shape"
    ;; Use convention-matching name: :order/tag matches :tag/* namespace.
    ;; (Plural :order/tags would need an explicit hint, since "tags"
    ;; doesn't equal "tag".)
    (let [schema-tx [{:db/ident :tag/id  :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/tag :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/many}]
          schema (:schema (make-db schema-tx))]
      (is (= {:order/tag [:tag/id :many]}
             (pgs/derive-ref-targets schema {}))
          "many refs project as PgArray of target PKs (vs single PK for one)")))

  (testing "explicit hint on plural-name many-ref"
    (let [schema-tx [{:db/ident :tag/id :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/tags :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/many}]
          schema (:schema (make-db schema-tx))]
      (is (= {:order/tags [:tag/id :many]}
             (pgs/derive-ref-targets schema
                                     {:order/tags {:references :tag/id}}))
          ":datahike.pg/references hint resolves the convention mismatch"))))

(deftest cached-result-stable
  (testing "same schema + hints → same result instance (memoization)"
    (let [schema-tx [{:db/ident :customer/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/customer :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          schema (:schema (make-db schema-tx))
          a      (pgs/derive-ref-targets schema {})
          b      (pgs/derive-ref-targets schema {})]
      (is (identical? a b)
          "memoized — second call returns the same instance"))))

;; ---------------------------------------------------------------------------
;; validate-ref-targets! — runtime data validation

(deftest validate-passes-when-data-matches-convention
  (testing "homogeneous data matching the convention's expected namespace passes through"
    (let [schema-tx [{:db/ident :customer/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/id      :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/customer :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          data [{:customer/id 1}
                {:order/id 100 :order/customer [:customer/id 1]}]
          db (make-db schema-tx data)
          targets (pgs/derive-ref-targets (:schema db) {})]
      (is (= targets
             (pgs/validate-ref-targets! db (:schema db) targets))
          "data points only to :customer/* — validation keeps the entry"))))

(deftest validate-drops-polymorphic-ref
  (testing "ref pointing into multiple namespaces is dropped (with warning)"
    (let [schema-tx [{:db/ident :customer/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :supplier/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/id      :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     ;; `customer` is the convention target but we'll
                     ;; deliberately point some orders to suppliers too.
                     {:db/ident :order/customer :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          data [{:customer/id 1}
                {:supplier/id 2}
                {:order/id 100 :order/customer [:customer/id 1]}
                {:order/id 101 :order/customer [:supplier/id 2]}]
          db (make-db schema-tx data)
          targets (pgs/derive-ref-targets (:schema db) {})]
      (is (empty? (pgs/validate-ref-targets! db (:schema db) targets))
          "polymorphic ref is dropped from the validated targets"))))

(deftest validate-drops-namespace-mismatch
  (testing "convention picked X but data points only to Y → drop"
    (let [schema-tx [{:db/ident :customer/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :supplier/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/id      :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     ;; Convention picks `:customer/id` (namespace match)
                     ;; but actual data points to suppliers.
                     {:db/ident :order/customer :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          data [{:supplier/id 2}
                {:order/id 100 :order/customer [:supplier/id 2]}]
          db (make-db schema-tx data)
          targets (pgs/derive-ref-targets (:schema db) {})]
      (is (empty? (pgs/validate-ref-targets! db (:schema db) targets))
          "namespace mismatch — drop the convention-derived entry"))))

(deftest validate-passes-empty-data
  (testing "ref attr defined but no data yet → trust the convention"
    (let [schema-tx [{:db/ident :customer/id   :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/id      :db/valueType :db.type/long
                      :db/cardinality :db.cardinality/one
                      :db/unique :db.unique/identity}
                     {:db/ident :order/customer :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/one}]
          db (make-db schema-tx [])
          targets (pgs/derive-ref-targets (:schema db) {})]
      (is (= targets (pgs/validate-ref-targets! db (:schema db) targets))
          "no entities reference the attr yet — keep the convention guess"))))
