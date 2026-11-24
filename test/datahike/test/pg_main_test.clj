(ns datahike.test.pg-main-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.pg.main :as main]
            [datahike.pg.sql.database :as database])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest startup-reopens-file-store-with-persisted-config
  (let [dir (.toFile (Files/createTempDirectory "pgdh-startup-" (make-array FileAttribute 0)))
        template {:store {:backend :file :path (str (io/file dir "{{name}}"))}
                  :schema-flexibility :write :keep-history? true}
        start! (fn [opts]
                 (get (#'main/startup-dbs! opts (database/db-from-template opts) ["orders"])
                      "orders"))]
    (try
      (let [created (start! template)
            original (:config (d/db created))]
        (try
          (d/transact created [{:db/ident :order/number
                                :db/valueType :db.type/long
                                :db/cardinality :db.cardinality/one}
                               {:order/number 3}])
          (finally (d/release created)))
        (let [reopened (start! (assoc template :keep-history? false))]
          (try
            (is (= (get-in original [:store :id]) (get-in (d/db reopened) [:config :store :id])))
            (is (:keep-history? (:config (d/db reopened))))
            (is (= 3 (d/q '[:find ?n . :where [_ :order/number ?n]] (d/db reopened))))
            (finally (d/release reopened)))))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file))))))
