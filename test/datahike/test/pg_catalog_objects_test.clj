(ns datahike.test.pg-catalog-objects-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.schema :as schema]
            [datahike.pg.server :as server])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-catalog [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn objects/schema)
        (d/transact conn (objects/initialization-tx [] objects/first-user-oid))
        (f conn)
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest structural-addresses-and-identities
  (is (= [1259 16384 0] (objects/address 1259 16384)))
  (is (= [1259 16384 3] (objects/address 1259 16384 3)))
  (is (not= (objects/address-key objects/pg-class-oid 16384)
            (objects/address-key objects/pg-type-oid 16384)))
  (is (not= (objects/identity-key objects/pg-proc-oid 2200 "f" [23])
            (objects/identity-key objects/pg-proc-oid 2200 "f" [25]))))

(deftest catalog-initialization-is-idempotent
  (with-catalog
    (fn [conn]
      (let [before (d/db conn)]
        (d/transact conn (objects/initialization-tx [] 99999))
        (is (= objects/catalog-version
               (:datahike.pg.catalog/version
                (objects/catalog-entity (d/db conn)))))
        (is (= objects/first-user-oid
               (:datahike.pg.catalog/next-oid
                (objects/catalog-entity (d/db conn)))))
        (is (= "pg_catalog"
               (:datahike.pg.object/name
                (objects/namespace-by-oid before
                                          objects/pg-catalog-namespace-oid))))
        (is (= objects/public-namespace-oid
               (:datahike.pg.object/oid
                (objects/namespace-by-name (d/db conn) "public"))))))))

(deftest allocation-and-object-creation-are-one-transaction
  (with-catalog
    (fn [conn]
      (let [db (d/db conn)
            {:keys [oid tx-data]} (objects/reserve-user-oid-tx db)
            object-tx (objects/create-object-tx
                       db {:class-oid objects/pg-type-oid
                           :oid oid :kind :enum :name "mood"
                           :namespace-oid objects/public-namespace-oid})]
        (is (= objects/first-user-oid oid))
        (d/transact conn (into tx-data object-tx))
        (is (= oid
               (:datahike.pg.object/oid
                (objects/object-by-identity
                 (d/db conn) objects/pg-type-oid
                 objects/public-namespace-oid "mood"))))
        (is (= (inc oid)
               (:datahike.pg.catalog/next-oid
                (objects/catalog-entity (d/db conn)))))))))

(deftest speculative-allocations-chain-and-rollback
  (with-catalog
    (fn [conn]
      (let [db0 (d/db conn)
            a (objects/reserve-user-oid-tx db0)
            tx-a (into (:tx-data a)
                       (objects/create-object-tx
                        db0 {:class-oid objects/pg-type-oid :oid (:oid a)
                             :kind :enum :name "one"
                             :namespace-oid objects/public-namespace-oid}))
            db1 (:db-after (dc/with db0 tx-a))
            b (objects/reserve-user-oid-tx db1)]
        (is (= (inc (:oid a)) (:oid b)))
        ;; dc/with models an explicit transaction/savepoint.  Discarding the
        ;; speculative value must leave both the object and allocator intact.
        (is (nil? (objects/object-by-identity
                   (d/db conn) objects/pg-type-oid
                   objects/public-namespace-oid "one")))
        (is (= objects/first-user-oid
               (:datahike.pg.catalog/next-oid
                (objects/catalog-entity (d/db conn)))))))))

(deftest same-numeric-oid-is-legal-across-catalog-classes
  (with-catalog
    (fn [conn]
      (let [db (d/db conn)
            table (objects/create-object-tx
                   db {:class-oid objects/pg-class-oid :oid 50000
                       :kind :table :name "same_oid_table"
                       :namespace-oid objects/public-namespace-oid})
            type (objects/create-object-tx
                  db {:class-oid objects/pg-type-oid :oid 50000
                      :kind :enum :name "same_oid_type"
                      :namespace-oid objects/public-namespace-oid})]
        (d/transact conn (into table type))
        (is (= :table (:datahike.pg.object/kind
                       (objects/object-by-address
                        (d/db conn) objects/pg-class-oid 50000))))
        (is (= :enum (:datahike.pg.object/kind
                      (objects/object-by-address
                       (d/db conn) objects/pg-type-oid 50000))))))))

(deftest rename-preserves-address-and-bumps-revision
  (with-catalog
    (fn [conn]
      (let [db (d/db conn)
            tx (objects/create-object-tx
                db {:class-oid objects/pg-type-oid :oid 51000
                    :kind :enum :name "before"
                    :namespace-oid objects/public-namespace-oid})]
        (d/transact conn tx)
        (d/transact conn (objects/rename-object-tx
                          (d/db conn) objects/pg-type-oid 51000 "after" nil))
        (is (nil? (objects/object-by-identity
                   (d/db conn) objects/pg-type-oid
                   objects/public-namespace-oid "before")))
        (let [renamed (objects/object-by-address
                       (d/db conn) objects/pg-type-oid 51000)]
          (is (= "after" (:datahike.pg.object/name renamed)))
          (is (= 2 (:datahike.pg.object/revision renamed))))))))

(deftest search-path-follows-postgresql-lookup-and-create-rules
  (with-catalog
    (fn [conn]
      (let [db (d/db conn)]
        (is (= [objects/pg-catalog-namespace-oid
                objects/public-namespace-oid]
               (objects/resolve-search-path db ["missing" "public"] "role")))
        (is (= [objects/public-namespace-oid
                objects/pg-catalog-namespace-oid]
               (objects/resolve-search-path db ["public" "pg_catalog"] "role")))
        (is (= "public"
               (:datahike.pg.object/name
                (objects/creation-namespace db ["missing" "public"] "role"))))))))

(deftest startup-migrates-and-repairs-colliding-legacy-table-oids
  ;; Java's "Aa" and "BB" hashes collide.  Legacy pg-datahike used that hash
  ;; as a read-time table OID, so this exercises the real upgrade hazard.
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn
                    [{:db/ident :Aa/db-row-exists
                      :db/valueType :db.type/boolean
                      :db/cardinality :db.cardinality/one}
                     {:db/ident :BB/db-row-exists
                      :db/valueType :db.type/boolean
                      :db/cardinality :db.cardinality/one}])
        ((deref (ns-resolve 'datahike.pg.server 'ensure-pg-schema!)) conn)
        (let [db (d/db conn)
              aa (schema/table-oid db "Aa")
              bb (schema/table-oid db "BB")
              next-before (:datahike.pg.catalog/next-oid
                           (objects/catalog-entity db))]
          (is (not= aa bb))
          (is (some #{objects/first-user-oid} [aa bb]))
          (is (true? (:datahike.pg.object/legacy-oid?
                      (objects/object-by-identity
                       db objects/pg-class-oid
                       objects/public-namespace-oid "Aa"))))
          ;; A second startup is a version-checked no-op, not a reseed.
          ((deref (ns-resolve 'datahike.pg.server 'ensure-pg-schema!)) conn)
          (is (= [aa bb next-before]
                 [(schema/table-oid (d/db conn) "Aa")
                  (schema/table-oid (d/db conn) "BB")
                  (:datahike.pg.catalog/next-oid
                   (objects/catalog-entity (d/db conn)))]))
          (is (= #{"Aa" "BB"}
                 (set (schema/table-names (:schema (d/db conn)))))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest startup-migrates-a-pre-oid-enum-registry
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact
         conn
         [{:db/ident :datahike.pg.enum/name
           :db/valueType :db.type/string
           :db/cardinality :db.cardinality/one
           :db/unique :db.unique/identity}
          {:db/ident :datahike.pg.enum/values-ordered
           :db/valueType :db.type/string
           :db/cardinality :db.cardinality/one}
          {:datahike.pg.enum/name "legacy_mood"
           :datahike.pg.enum/values-ordered "sad\nhappy"}])
        ((deref (ns-resolve 'datahike.pg.server 'ensure-pg-schema!)) conn)
        (let [db (d/db conn)
              enum (first (schema/enum-types db))
              object (objects/object-by-identity
                      db objects/pg-type-oid
                      objects/public-namespace-oid "legacy_mood")
              stored-oid (ffirst
                          (d/q '{:find [?oid]
                                 :where [[?enum :datahike.pg.enum/name "legacy_mood"]
                                         [?enum :datahike.pg.enum/oid ?oid]]}
                               db))]
          (is (= (:oid enum) stored-oid
                 (:datahike.pg.object/oid object)))
          (is (true? (:datahike.pg.object/legacy-oid? object))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest object-identities-survive-a-file-backend-reconnect
  (let [root (Files/createTempDirectory
              "pg-datahike-catalog-"
              (make-array FileAttribute 0))
        path (str (.resolve root "store"))
        cfg {:store {:backend :file :path path
                     :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (try
      (let [conn (d/connect cfg)
            handler (server/make-query-handler conn)]
        (.execute handler "CREATE TABLE durable_identity (id integer)")
        (let [oid (schema/table-oid (d/db conn) "durable_identity")
              next-oid (:datahike.pg.catalog/next-oid
                        (objects/catalog-entity (d/db conn)))]
          (d/release conn)
          (let [reopened (d/connect cfg)]
            (try
              ;; Handler construction reruns the version/idempotency gate.
              (server/make-query-handler reopened)
              (is (= oid (schema/table-oid (d/db reopened)
                                           "durable_identity")))
              (is (= next-oid
                     (:datahike.pg.catalog/next-oid
                      (objects/catalog-entity (d/db reopened)))))
              (finally (d/release reopened))))))
      (finally
        (d/delete-database cfg)
        (Files/deleteIfExists root)))))
