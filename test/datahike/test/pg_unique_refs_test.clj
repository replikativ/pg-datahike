(ns datahike.test.pg-unique-refs-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.constraints.unique :as unique]
            [datahike.pg.server :as pg]
            [datahike.pg.types :as types]))

(deftest resolved-attribute-references-trigger-unique-validation
  (doseq [refs? [false true]]
    (testing (str "attribute-refs? " refs?)
      (let [config {:store {:backend :memory :id (random-uuid)}
                    :schema-flexibility :write :attribute-refs? refs?
                    :keep-history? false}
            descriptor {:name "refs_key" :table "refs"
                        :attrs [:refs/value] :keys [{:name "value"}]
                        :marker :refs/db-row-exists
                        :descendant-markers #{:child/db-row-exists}}]
        (d/create-database config)
        (let [conn (d/connect config)]
          (try
            (d/transact conn
                        [{:db/ident :refs/value :db/valueType :db.type/long
                          :db/cardinality :db.cardinality/one :db/index true}
                         {:db/ident :refs/db-row-exists :db/valueType :db.type/boolean
                          :db/cardinality :db.cardinality/one}
                         {:db/ident :child/db-row-exists :db/valueType :db.type/boolean
                          :db/cardinality :db.cardinality/one}])
            (let [report (d/transact conn [{:db/id "a" :refs/value 1 :refs/db-row-exists true}
                                           {:db/id "b" :refs/value 2 :refs/db-row-exists true}])
                  db (:db-after report)
                  b (get (:tempids report) "b")]
              ;; Only catalog discovery is supplied here. Reports, index probes,
              ;; row ownership and uniqueness comparison use actual Datahike DBs.
              (with-redefs [unique/index-descriptors (constantly [descriptor])]
                (is (nil? (unique/validate-report!
                           (d/with db [[:db/add b :refs/value 3]]))))
                (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                                      (unique/validate-report!
                                       (d/with db [[:db/add b :refs/value 1]]))))
                (let [hidden (:db-after (d/with db [[:db/retract b :refs/db-row-exists true]
                                                    [:db/add b :refs/value 1]]))]
                  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                                        (unique/validate-report!
                                         (d/with hidden [[:db/add b :refs/db-row-exists true]])))))
                (let [child (:db-after (d/with db [[:db/add b :child/db-row-exists true]
                                                   [:db/add b :refs/value 1]]))]
                  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                                        (unique/validate-report!
                                         (d/with child [[:db/retract b :child/db-row-exists true]])))))))
            (finally (d/release conn) (d/delete-database config))))))))

(defn- with-ref-column [f]
  (let [config {:store {:backend :memory :id (java.util.UUID/randomUUID)}
                :schema-flexibility :write
                :keep-history? false
                :max-string-length 0}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try
        (pg/reset-lock-registry!)
        (let [handler (pg/make-query-handler conn)]
          (is (nil? (.-error (.execute handler "CREATE TABLE ref_keys (bucket int)"))))
          (let [relation (objects/object-by-identity (d/db conn) objects/pg-class-oid
                                                     objects/public-namespace-oid "ref_keys")]
            (d/transact conn
                        (into [{:db/ident :ref_keys/ref
                                :db/valueType :db.type/ref
                                :db/cardinality :db.cardinality/one
                                :db/index true}
                               {:db/ident :ref-target/first}
                               {:db/ident :ref-target/second}]
                              (objects/create-columns-tx
                               (:datahike.pg.object/oid relation)
                               [{:attnum 2 :name "ref" :storage-ident :ref_keys/ref
                                 :type-oid types/oid-int8}]))))
          (f conn handler))
        (finally
          (d/release conn)
          (d/delete-database config))))))

(defn- ref-row [bucket ref]
  {:ref_keys/db-row-exists true :ref_keys/bucket bucket :ref_keys/ref ref})

(deftest native-ref-unique-index-uses-entity-ids
  (with-ref-column
    (fn [conn handler]
      (is (nil? (.-error (.execute handler "CREATE UNIQUE INDEX ref_key_idx ON ref_keys(ref)"))))
      (let [first-tx (d/transact conn [(assoc (ref-row 1 :ref-target/first) :db/id "row")])
            row-id (get (:tempids first-tx) "row")
            db (d/db conn)
            reference (d/entity db :ref-target/first)
            lookup (unique/conflict-lookup db "ref_keys")]
        (is (some? row-id))
        (testing "lookup refs, entity IDs and entity values name the same key"
          (doseq [key [:ref-target/first [:db/ident :ref-target/first] (:db/id reference) reference]]
            (is (= row-id (lookup [:ref_keys/ref] [key])))))
        (testing "NULL and unresolved candidate references do not match a row"
          (is (nil? (lookup [:ref_keys/ref] [nil])))
          (is (nil? (lookup [:ref_keys/ref] [:ref-target/missing])))
          (is (nil? (.-error (.execute handler "INSERT INTO ref_keys (bucket,ref) VALUES (3,NULL),(3,NULL)")))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                              (d/transact conn [(ref-row 2 (:db/id reference))])))
        (is (some? (:db-after (d/transact conn [(ref-row 2 :ref-target/second)]))))))))

(deftest native-ref-can-be-a-later-indexed-component
  (with-ref-column
    (fn [conn handler]
      (is (nil? (.-error (.execute handler "CREATE UNIQUE INDEX ref_pair_idx ON ref_keys(bucket,ref)"))))
      (is (some? (:db-after (d/transact conn [(ref-row 1 :ref-target/first)
                                              (ref-row 1 :ref-target/second)]))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                            (d/transact conn [(ref-row 1 :ref-target/first)])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                            (d/transact conn [(ref-row 2 :ref-target/first)
                                              (ref-row 2 :ref-target/first)])))
      (is (some? (:db-after (d/transact conn [(ref-row 2 :ref-target/first)])))))))
