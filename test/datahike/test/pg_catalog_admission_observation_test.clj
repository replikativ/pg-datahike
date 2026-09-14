(ns datahike.test.pg-catalog-admission-observation-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.core :as dc]
            [datahike.pg.catalog.admission :as admission]
            [datahike.pg.constraints.row :as row]
            [datahike.pg.schema :as pgs]))

(def parsed {:type :insert :table "target" :catalog-dependency-shape :literal-insert-v1})

(defn- transact [db data] (:db-after (dc/with db data)))

(defn- attribute [ident type]
  {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})

(defn- fixture []
  (transact
   (dc/empty-db {} {:schema-flexibility :write :attribute-refs? false})
   [(attribute :target/value :db.type/long)
    (attribute :target/db-row-exists :db.type/boolean)
    (attribute :pg/not-null :db.type/boolean)
    (attribute :pg/type :db.type/string)
    (attribute :pg/table-oid :db.type/long)
    (attribute :datahike.pg/for-ident :db.type/keyword)
    (attribute :datahike.pg/column :db.type/string)
    (attribute :datahike.pg/internal-index :db.type/boolean)
    (attribute :datahike.pg/references :db.type/keyword)
    (attribute :datahike.pg/table :db.type/string)
    (attribute :datahike.pg/hidden :db.type/boolean)]))

(deftest capture-can-share-metadata-but-validation-always-rereads
  (let [db (fixture)
        metadata (row/constraint-metadata db "target")
        original row/constraint-metadata
        calls (atom 0)]
    (with-redefs [row/constraint-metadata
                  (fn [& args]
                    (swap! calls inc)
                    (apply original args))]
      (let [certificate (admission/capture db parsed metadata)]
        (is (some? certificate))
        (is (zero? @calls))
        (is (true? (admission/valid? certificate db)))
        (is (= 1 @calls))
        (let [changed (transact db [[:db/add [:db/ident :target/value] :pg/not-null true]])]
          (is (false? (admission/valid? certificate changed)))
          (is (= 2 @calls)))))))

(deftest unrelated-schema-and-row-changes-preserve-observations
  (let [db (fixture) certificate (admission/capture db parsed)]
    (is (some? certificate))
    (is (true? (admission/valid? certificate db)))
    (let [other (transact db [(attribute :other/value :db.type/long)
                              (attribute :other/db-row-exists :db.type/boolean)])
          row (transact other [{:other/value 9 :other/db-row-exists true}])]
      (is (true? (admission/valid? certificate other)))
      (is (true? (admission/valid? certificate row))))))

(deftest target-column-schema-and-hints-remain-dependencies
  (let [db (fixture) certificate (admission/capture db parsed)]
    (is (some? certificate))
    (doseq [data [[(attribute :target/added :db.type/string)]
                  [[:db/add [:db/ident :target/value] :pg/not-null true]]
                  [{:datahike.pg/for-ident :target/value :datahike.pg/column "renamed"}]
                  [{:datahike.pg/for-ident :target/value :datahike.pg/hidden true}]]]
      (is (false? (admission/valid? certificate (transact db data)))))))

(deftest previously-absent-check-and-fk-are-observed
  (let [db (fixture) certificate (admission/capture db parsed)
        checks [(attribute :pg/check-name :db.type/string)
                (attribute :pg/check-table :db.type/string)
                (attribute :pg/check-expr :db.type/string)]
        fks [(attribute :pg/fk-name :db.type/string)
             (attribute :pg/fk-child-table :db.type/string)
             (attribute :pg/fk-child-cols :db.type/string)
             (attribute :pg/fk-parent-table :db.type/string)
             (attribute :pg/fk-parent-cols :db.type/string)]]
    (is (some? certificate))
    (let [declared (transact db (into checks fks))]
      (is (true? (admission/valid? certificate declared))
          "Installing unrelated catalog attributes does not turn absence into a change")
      (is (false? (admission/valid?
                   certificate
                   (transact declared [{:pg/check-name "positive" :pg/check-table "target"
                                        :pg/check-expr "value > 0"}]))))
      (is (false? (admission/valid?
                   certificate
                   (transact declared [(attribute :parent/value :db.type/long)
                                       {:pg/fk-name "target_parent" :pg/fk-child-table "target"
                                        :pg/fk-child-cols "[\"value\"]" :pg/fk-parent-table "parent"
                                        :pg/fk-parent-cols "[\"value\"]"}])))))))

(deftest unsupported-and-unattested-calls-fail-closed
  (let [db (fixture)]
    (is (false? (admission/valid? nil db)))
    (is (nil? (admission/capture db (dissoc parsed :catalog-dependency-shape))))
    (is (nil? (admission/capture db (assoc parsed :type :update))))
    (is (nil? (admission/capture db (assoc parsed :table "missing"))))
    (is (nil? (admission/capture (dc/filter db (fn [_ _] true)) parsed)))
    (let [custom (transact db [[:db/add [:db/ident :target/value] :pg/type "custom_type"]])]
      (is (nil? (admission/capture custom parsed))))))

(deftest exact-column-resolution-is-observed
  (let [db (fixture)
        certificate (admission/capture db parsed)
        original-ci pgs/ci-index]
    (is (some? certificate))
    ;; A same-named registered relation in another namespace can replace these
    ;; entries without changing the public relation or target's raw attributes.
    (doseq [key [:columns :registered-columns]
            replacement [{} {"value" :target/db-row-exists}]]
      (with-redefs [pgs/ci-index
                    (fn [& args]
                      (assoc-in (apply original-ci args) [key "target"] replacement))]
        (is (false? (admission/valid? certificate db)))))
    (with-redefs [pgs/ci-index
                  (fn [& args]
                    (assoc-in (apply original-ci args)
                              [:registered-columns "unrelated"] {"value" :other/value}))]
      (is (true? (admission/valid? certificate db))))))
