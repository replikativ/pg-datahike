(ns datahike.test.pg-unique-refs-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.constraints.unique :as unique]))

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
