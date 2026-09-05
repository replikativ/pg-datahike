(ns datahike.test.pg-unique-index-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.db.interface :as dbi]
            [datahike.tx-preds :as tx-preds]
            [datahike.versioning :as versioning]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.constraints.unique :as unique]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*conn* conn
                  *handler* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- execute [sql]
  (let [^PgWireServer$QueryResult result (.execute *handler* sql)]
    {:state (.-sqlstate result)
     :error (.-error result)
     :rows (mapv vec (.-rows result))}))

(defn- state [sql] (:state (execute sql)))
(defn- rows [sql] (:rows (execute sql)))

(deftest compound-unique-index-is-removable-and-null-distinct
  (is (nil? (state "CREATE TABLE uniq_pair(a int,b int)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_pair_ab ON uniq_pair(a,b)")))
  (is (nil? (state "INSERT INTO uniq_pair VALUES(1,2)")))
  (is (= "23505" (state "INSERT INTO uniq_pair VALUES(1,2)")))
  (is (nil? (state "INSERT INTO uniq_pair VALUES(1,NULL),(1,NULL)")))
  (is (nil? (state "DROP INDEX uniq_pair_ab")))
  (is (nil? (state "INSERT INTO uniq_pair VALUES(1,2)")))
  (is (= [] (rows "SELECT indexname FROM pg_indexes WHERE tablename='uniq_pair'"))))

(deftest failed-create-is-atomic
  (is (nil? (state "CREATE TABLE uniq_existing(a int)")))
  (is (nil? (state "INSERT INTO uniq_existing VALUES(1),(1)")))
  (let [next-oid (:datahike.pg.catalog/next-oid
                  (objects/catalog-entity (d/db *conn*)))]
    (is (= "23505"
           (state "CREATE UNIQUE INDEX uniq_existing_a ON uniq_existing(a)")))
    (is (= next-oid
           (:datahike.pg.catalog/next-oid
            (objects/catalog-entity (d/db *conn*)))))
    (is (= [] (rows "SELECT indexname FROM pg_indexes WHERE indexname='uniq_existing_a'")))))

(deftest populated-index-builds-avet-in-the-catalog-transaction
  (is (nil? (state "CREATE TABLE uniq_backfill(a int)")))
  (is (nil? (state "INSERT INTO uniq_backfill VALUES(1),(2),(3)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_backfill_a ON uniq_backfill(a)")))
  (let [db (d/db *conn*)]
    (is (true? (get-in db [:schema :uniq_backfill/a :db/index])))
    (is (= #{1 2 3}
           (into #{} (map :v) (d/datoms db :avet :uniq_backfill/a))))))

(deftest rolled-back-index-build-does-not-leak-backfill-authorization
  (doseq [sql ["CREATE TABLE uniq_backfill_rollback(a int)"
               "INSERT INTO uniq_backfill_rollback VALUES(1),(2)"
               "BEGIN"
               "CREATE UNIQUE INDEX uniq_backfill_rollback_a ON uniq_backfill_rollback(a)"
               "ROLLBACK"]]
    (is (nil? (state sql)) sql))
  (is (not (true? (get-in (d/db *conn*)
                          [:schema :uniq_backfill_rollback/a :db/index]))))
  (is (thrown? clojure.lang.ExceptionInfo
               (d/transact *conn*
                           [{:db/ident :uniq_backfill_rollback/a
                             :db/index true}]))))

(deftest unsupported-unique-index-semantics-fail-before-allocation
  (is (nil? (state "CREATE TABLE uniq_syntax(a text,b int)")))
  (doseq [statement ["CREATE UNIQUE INDEX uniq_partial ON uniq_syntax(a) WHERE a"
                     "CREATE UNIQUE INDEX uniq_opclass ON uniq_syntax(a text_ops)"
                     "CREATE UNIQUE INDEX uniq_hash ON uniq_syntax USING hash(a)"]]
    (let [before (:datahike.pg.catalog/next-oid
                  (objects/catalog-entity (d/db *conn*)))]
      (is (= "0A000" (state statement)) statement)
      (is (= before (:datahike.pg.catalog/next-oid
                     (objects/catalog-entity (d/db *conn*)))))))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_desc ON uniq_syntax(a DESC)"))))

(deftest invalid-unique-descriptor-mutations-are-rejected
  (is (nil? (state "CREATE TABLE uniq_catalog(a int)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_catalog_a ON uniq_catalog(a)")))
  (let [index (objects/object-by-identity
               (d/db *conn*) objects/pg-class-oid
               objects/public-namespace-oid "uniq_catalog_a")
        table (objects/object-by-identity
               (d/db *conn*) objects/pg-class-oid
               objects/public-namespace-oid "uniq_catalog")]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"invalid unique index descriptor"
         (d/transact *conn*
                     [[:db/retract (:db/id index)
                       :datahike.pg.index/method "btree"]])))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"invalid unique index descriptor"
         (d/transact *conn*
                     [[:db/add (:db/id index)
                       :datahike.pg.object/class-oid objects/pg-type-oid]])))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"invalid unique index descriptor"
         (d/transact *conn*
                     [[:db/add (:db/id table)
                       :datahike.pg.object/class-oid objects/pg-type-oid]]))))
  (is (nil? (state "INSERT INTO uniq_catalog VALUES(1)")))
  (is (= "23505" (state "INSERT INTO uniq_catalog VALUES(1)"))))

(deftest failed-create-unique-aborts-an-explicit-transaction
  (doseq [sql ["CREATE TABLE uniq_ddl_tx(a int)"
               "INSERT INTO uniq_ddl_tx VALUES(1),(1)"
               "BEGIN"]]
    (is (nil? (state sql))))
  (is (= "23505" (state "CREATE UNIQUE INDEX uniq_ddl_tx_a ON uniq_ddl_tx(a)")))
  (is (= "25P02" (state "SELECT 1")))
  (is (nil? (state "ROLLBACK")))
  (is (= [["1"] ["1"]] (rows "SELECT a FROM uniq_ddl_tx"))))

(deftest equivalent-indexes-enforce-until-the-last-drop
  (is (nil? (state "CREATE TABLE uniq_twice(a int)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_twice_a1 ON uniq_twice(a)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_twice_a2 ON uniq_twice(a)")))
  (is (nil? (state "INSERT INTO uniq_twice VALUES(1)")))
  (is (nil? (state "DROP INDEX uniq_twice_a1")))
  (is (= "23505" (state "INSERT INTO uniq_twice VALUES(1)")))
  (is (nil? (state "DROP INDEX uniq_twice_a2")))
  (is (nil? (state "INSERT INTO uniq_twice VALUES(1)"))))

(deftest explicit-index-arbitrates-on-conflict
  (is (nil? (state "CREATE TABLE uniq_upsert(a int,b int,v text)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_upsert_ab ON uniq_upsert(a,b)")))
  (is (nil? (state "INSERT INTO uniq_upsert VALUES(1,2,'old')")))
  (is (nil? (state (str "INSERT INTO uniq_upsert VALUES(1,2,'new') "
                        "ON CONFLICT (a,b) DO UPDATE SET v=EXCLUDED.v"))))
  (is (= [["1" "2" "new"]] (rows "SELECT * FROM uniq_upsert")))
  (is (nil? (state (str "INSERT INTO uniq_upsert VALUES(1,2,'ignored') "
                        "ON CONFLICT DO NOTHING"))))
  (is (= [["new"]] (rows "SELECT v FROM uniq_upsert"))))

(deftest violations-fail-at-the-statement-inside-a-transaction
  (doseq [sql ["CREATE TABLE uniq_tx(a int)"
               "CREATE UNIQUE INDEX uniq_tx_a ON uniq_tx(a)"
               "BEGIN"
               "INSERT INTO uniq_tx VALUES(1)"]]
    (is (nil? (state sql))))
  (is (= "23505" (state "INSERT INTO uniq_tx VALUES(1)")))
  (is (nil? (state "ROLLBACK")))
  (is (= [] (rows "SELECT * FROM uniq_tx"))))

(deftest writer-gate-covers-native-datahike-writes
  (is (nil? (state "CREATE TABLE uniq_native(a int)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_native_a ON uniq_native(a)")))
  (d/transact *conn* [{:uniq_native/a 1 :uniq_native/db-row-exists true}])
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"unique violation"
       (d/transact *conn* [{:uniq_native/a 1 :uniq_native/db-row-exists true}])))
  ;; A rejected publication must not poison the writer chain.
  (d/transact *conn* [{:uniq_native/a 2 :uniq_native/db-row-exists true}])
  (is (= [["1"] ["2"]] (rows "SELECT a FROM uniq_native ORDER BY a"))))

(deftest writer-gate-checks-rows-that-enter-a-table-by-marker
  (is (nil? (state "CREATE TABLE uniq_marker(a int)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_marker_a ON uniq_marker(a)")))
  (d/transact *conn* [{:uniq_marker/a 1 :uniq_marker/db-row-exists true}
                      {:db/id "unmarked" :uniq_marker/a 1}])
  (let [unmarked (ffirst
                  (d/q '{:find [?e]
                         :where [[?e :uniq_marker/a 1]
                                 (not [?e :uniq_marker/db-row-exists true])]}
                       (d/db *conn*)))]
    (is (some? unmarked))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"unique violation"
         (d/transact *conn* [[:db/add unmarked
                              :uniq_marker/db-row-exists true]])))))

(deftest updates-are-checked-against-explicit-indexes
  (doseq [sql ["CREATE TABLE uniq_update(a int,b int,v text)"
               "CREATE UNIQUE INDEX uniq_update_ab ON uniq_update(a,b)"
               "INSERT INTO uniq_update VALUES(1,1,'left'),(2,2,'right')"]]
    (is (nil? (state sql))))
  (is (= "23505"
         (state "UPDATE uniq_update SET a=1,b=1 WHERE v='right'")))
  (is (= [["1" "1" "left"] ["2" "2" "right"]]
         (rows "SELECT a,b,v FROM uniq_update ORDER BY a")))
  (is (nil? (state "UPDATE uniq_update SET a=3,b=3 WHERE v='right'")))
  (is (= [["1" "1"] ["3" "3"]]
         (rows "SELECT a,b FROM uniq_update ORDER BY a"))))

(deftest byte-array-keys-use-value-equality
  (is (nil? (state "CREATE TABLE uniq_bytes(payload bytea)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_bytes_payload ON uniq_bytes(payload)")))
  (is (nil? (state "INSERT INTO uniq_bytes VALUES('\\x0102')")))
  (is (= "23505" (state "INSERT INTO uniq_bytes VALUES('\\x0102')")))
  (is (nil? (state "INSERT INTO uniq_bytes VALUES('\\x0103')"))))

(deftest scalar-float-nan-keys-use-postgresql-equality
  (doseq [[table type] [["uniq_nan4" "real"]
                        ["uniq_nan8" "double precision"]]]
    (is (nil? (state (str "CREATE TABLE " table "(id int PRIMARY KEY, payload " type ")"))))
    (is (nil? (state (str "CREATE UNIQUE INDEX " table "_payload ON "
                          table "(payload)"))))
    (is (nil? (state (str "INSERT INTO " table " VALUES(1, 'NaN')"))))
    (is (= "23505" (state (str "INSERT INTO " table " VALUES(2, 'NaN')"))))
    (is (nil? (state (str "INSERT INTO " table " VALUES(2, 0)"))))
    (let [id-attr (keyword table "id")
          payload-attr (keyword table "payload")
          eid (:db/id (d/entity (d/db *conn*) [id-attr 2]))]
      (is (some? eid))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"unique violation"
           (d/transact *conn* [[:db/add eid payload-attr Double/NaN]]))))
    (is (= "23505" (state (str "INSERT INTO " table " VALUES(3, -0.0)"))))
    (is (= [["1" "NaN"] ["2" "0"]]
           (rows (str "SELECT id,payload FROM " table " ORDER BY id"))))))

(deftest composite-float-nan-arbitration-remains-null-distinct
  (is (nil? (state (str "CREATE TABLE uniq_nan_pair("
                        "id int, payload double precision, discriminator int, note text)"))))
  (is (nil? (state (str "CREATE UNIQUE INDEX uniq_nan_pair_key "
                        "ON uniq_nan_pair(payload,discriminator)"))))
  (is (nil? (state "INSERT INTO uniq_nan_pair VALUES(1,'NaN',1,'old')")))
  (is (nil? (state (str "INSERT INTO uniq_nan_pair VALUES(2,'NaN',1,'new') "
                        "ON CONFLICT(payload,discriminator) DO UPDATE "
                        "SET note=excluded.note"))))
  (is (= [["1" "NaN" "1" "new"]]
         (rows (str "SELECT id,payload,discriminator,note FROM uniq_nan_pair "
                    "WHERE discriminator=1"))))
  (is (nil? (state (str "INSERT INTO uniq_nan_pair VALUES "
                        "(3,'NaN',NULL,'left'),(4,'NaN',NULL,'right') "
                        "ON CONFLICT(payload,discriminator) DO NOTHING"))))
  (is (= [["3" "left"] ["4" "right"]]
         (rows (str "SELECT id,note FROM uniq_nan_pair "
                    "WHERE discriminator IS NULL ORDER BY id")))))

(deftest concurrent-writers-have-one-commit-winner
  (is (nil? (state "CREATE TABLE uniq_race(a int)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_race_a ON uniq_race(a)")))
  (let [left (pg/make-query-handler *conn*)
        right (pg/make-query-handler *conn*)
        start (promise)
        race (fn [handler]
               (future
                 @start
                 (let [^PgWireServer$QueryResult result
                       (.execute handler "INSERT INTO uniq_race VALUES(1)")]
                   (.-sqlstate result))))
        results [(race left) (race right)]]
    (deliver start true)
    (is (= #{nil "23505"} (set (map deref results))))
    (is (= [["1"]] (rows "SELECT a FROM uniq_race")))))

(deftest concurrent-handler-admission-scans-a-branch-once
  (let [admissions (atom {})
        scans (atom 0)
        validate-db! unique/validate-db!]
    (with-redefs [unique/validate-db! (fn [db]
                                        (swap! scans inc)
                                        (validate-db! db))]
      (let [start (promise)
            attempts (doall
                      (repeatedly 32
                                  #(future
                                     @start
                                     (#'pg/admit-unique-index-enforcement!
                                      *conn* admissions))))]
        (deliver start true)
        (is (every? true? (map deref attempts)))
        (is (= 1 @scans))))))

(deftest remote-writer-admission-is-rejected-before-caching
  (let [admissions (atom {})
        remote-conn (atom (assoc-in (d/db *conn*)
                                    [:config :writer :backend] :kabel))]
    (try
      (#'pg/admit-unique-index-enforcement! remote-conn admissions)
      (is false "remote writer admission should fail")
      (catch clojure.lang.ExceptionInfo e
        (is (= "55000" (:sqlstate (ex-data e))))
        (is (= :kabel (:writer-backend (ex-data e))))))
    (is (empty? @admissions))))

(deftest failed-admission-is-evicted-and-can-be-retried
  (let [admissions (atom {})
        attempts (atom 0)
        validate-db! unique/validate-db!]
    (with-redefs [unique/validate-db!
                  (fn [db]
                    (if (= 1 (swap! attempts inc))
                      (throw (ex-info "injected admission failure" {}))
                      (validate-db! db)))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"injected admission failure"
           (#'pg/admit-unique-index-enforcement! *conn* admissions)))
      (is (empty? @admissions))
      (is (true? (#'pg/admit-unique-index-enforcement!
                  *conn* admissions)))
      (is (= 2 @attempts))
      (is (= 1 (count @admissions))))))

(deftest inherited-tables-have-separate-unique-domains
  (doseq [sql ["CREATE TABLE uniq_parent(a int)"
               "CREATE TABLE uniq_child(note text) INHERITS(uniq_parent)"
               "CREATE UNIQUE INDEX uniq_parent_a ON uniq_parent(a)"
               "CREATE UNIQUE INDEX uniq_child_a ON uniq_child(a)"
               "INSERT INTO uniq_parent VALUES(1)"
               "INSERT INTO uniq_child(a,note) VALUES(1,'child')"]]
    (is (nil? (state sql))))
  (is (= "23505" (state "INSERT INTO uniq_child(a,note) VALUES(1,'again')")))
  (is (= "23505" (state "INSERT INTO uniq_parent VALUES(1)"))))

(deftest creating-and-dropping-an-index-does-not-change-a-primary-key
  (is (nil? (state "CREATE TABLE uniq_pk(id int PRIMARY KEY)")))
  (is (nil? (state "CREATE UNIQUE INDEX uniq_pk_extra ON uniq_pk(id)")))
  (is (nil? (state "DROP INDEX uniq_pk_extra")))
  (is (= [["t"]]
         (rows (str "SELECT indisprimary FROM pg_index "
                    "WHERE indrelid=(SELECT oid FROM pg_class WHERE relname='uniq_pk')"))))
  (is (= "23505" (state "INSERT INTO uniq_pk VALUES(1),(1)"))))

(defn- legacy-database
  ([f] (legacy-database {} f))
  ([index-fields f]
   (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
              :schema-flexibility :write :keep-history? false
              :max-string-length 0}]
     (d/create-database cfg)
     (let [conn (d/connect cfg)]
       (try
         (d/transact conn
                     [{:db/ident :legacy/a :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :legacy/b :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :legacy/db-row-exists :db/valueType :db.type/boolean
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :datahike.pg.index/table :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :datahike.pg.index/method :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}])
         (d/transact conn
                     [(merge {:db/ident :datahike.pg.index/legacy_ab
                              :datahike.pg.index/table "legacy"
                              :datahike.pg.index/method "btree"}
                             index-fields)
                      {:legacy/a 1 :legacy/b 2 :legacy/db-row-exists true}
                      {:legacy/a 1 :legacy/b 2 :legacy/db-row-exists true}])
         (f conn cfg)
         (finally
           (d/release conn)
           (d/delete-database cfg)
           (tx-preds/unregister-tx-pred! (get-in cfg [:store :id]) unique/predicate-id)))))))

(deftest legacy-index-startup-preserves-identity-without-inventing-keys
  (legacy-database
   (fn [conn _]
     (let [eid (:db/id (d/entity (d/db conn) :datahike.pg.index/legacy_ab))
           handler (pg/make-query-handler conn)]
       (binding [*conn* conn *handler* handler]
         (let [db (d/db conn)
               index (d/entity db :datahike.pg.index/legacy_ab)
               object (objects/object-by-identity
                       db objects/pg-class-oid objects/public-namespace-oid "legacy_ab")]
           (is (= eid (:db/id object) (:db/id index)))
           (is (true? (:datahike.pg.index/legacy-incomplete? index)))
           (is (false? (:datahike.pg.index/unique? index)))
           (is (= "[]" (:datahike.pg.index/keys index)))
           (is (= [] (unique/index-descriptors db))))
         (is (= [["f" "f"]]
                (rows (str "SELECT indisunique,indisvalid FROM pg_index "
                           "WHERE indexrelid=(SELECT oid FROM pg_class WHERE relname='legacy_ab')"))))
         (is (re-find #"legacy key metadata unavailable"
                      (ffirst (rows "SELECT indexdef FROM pg_indexes WHERE indexname='legacy_ab'"))))
         (is (nil? (state "INSERT INTO legacy VALUES(1,2)")))
         (is (nil? (state "DROP INDEX legacy_ab")))
         (is (nil? (state "CREATE INDEX legacy_ab ON legacy(a,b)")))
         (is (not (:datahike.pg.index/legacy-incomplete?
                   (d/entity (d/db conn) :datahike.pg.index/legacy_ab))))
         (is (nil? (state "DROP INDEX legacy_ab")))
         (is (= "23505" (state "CREATE UNIQUE INDEX legacy_ab ON legacy(a,b)"))))))))

(deftest historical-legacy-branch-migrates-under-an-installed-store-predicate
  (legacy-database
   (fn [conn cfg]
     (versioning/branch! conn :db :old)
     (pg/make-query-handler conn)
     (let [branch (d/connect (assoc cfg :branch :old))
           publications (atom [])
           store-id (get-in cfg [:store :id])]
       (try
         (tx-preds/register-tx-pred!
          store-id ::migration-observer
          (fn [{:keys [db-after]}]
            (when (= :old (:branch (dbi/-config db-after)))
              (swap! publications conj db-after)
              (unique/validate-db! db-after))))
         (pg/make-query-handler branch)
         (is (= 1 (count @publications)))
         (is (= objects/catalog-version
                (:datahike.pg.catalog/version (objects/catalog-entity (d/db branch)))))
         (is (true? (:datahike.pg.index/legacy-incomplete?
                     (d/entity (d/db branch) :datahike.pg.index/legacy_ab))))
         (finally
           (tx-preds/unregister-tx-pred! store-id ::migration-observer)
           (d/release branch)))))))

(deftest legacy-index-recovers-only-durable-secondary-key-order
  (legacy-database
   {:db.secondary/attrs [:legacy/b :legacy/a]}
   (fn [conn _]
     (let [eid (:db/id (d/entity (d/db conn) :datahike.pg.index/legacy_ab))]
       ;; Retained secondary metadata is enough to recover exact keys; no
       ;; optional adapter needs to be present merely to migrate its catalog.
       (pg/make-query-handler conn)
       (let [index (d/entity (d/db conn) eid)]
         (is (false? (:datahike.pg.index/legacy-incomplete? index)))
         (is (false? (:datahike.pg.index/unique? index)))
         (is (= "[{:name \"b\", :attnum 2, :storage-ident :legacy/b} {:name \"a\", :attnum 1, :storage-ident :legacy/a}]"
                (:datahike.pg.index/keys index))))))))

(deftest legacy-catalog-upgrades-preserve-the-index-payload-entity
  (doseq [version [1 2]]
    (legacy-database
     (fn [conn cfg]
       (d/transact conn objects/schema)
       (d/transact
        conn
        (objects/initialization-tx
         [{:tempid "legacy-table" :class-oid objects/pg-class-oid
           :oid 17000 :kind :table :name "legacy"
           :namespace-tempid "catalog-namespace-public"}]
         (if (= 2 version)
           [{:relation-oid 17000
             :columns [{:attnum 1 :name "a" :storage-ident :legacy/a :type-oid 23}
                       {:attnum 2 :name "b" :storage-ident :legacy/b :type-oid 23}]}]
           [])
         17010))
       (d/transact conn [[:db/add (:db/id (objects/catalog-entity (d/db conn)))
                          :datahike.pg.catalog/version version]])
       (let [eid (:db/id (d/entity (d/db conn) :datahike.pg.index/legacy_ab))
             publications (atom 0)
             store-id (get-in cfg [:store :id])]
         (try
           (tx-preds/register-tx-pred!
            store-id ::migration-observer
            (fn [{:keys [db-after]}]
              (swap! publications inc)
              (unique/validate-db! db-after)))
           (pg/make-query-handler conn)
           (is (= 1 @publications) (str "catalog v" version))
           (is (= eid (:db/id (objects/object-by-identity
                               (d/db conn) objects/pg-class-oid
                               objects/public-namespace-oid "legacy_ab"))))
           (finally (tx-preds/unregister-tx-pred! store-id ::migration-observer))))))))

(deftest marked-legacy-descriptors-cannot-claim-uniqueness-or-conceal-modern-corruption
  (legacy-database
   (fn [conn _]
     (pg/make-query-handler conn)
     (let [eid (:db/id (d/entity (d/db conn) :datahike.pg.index/legacy_ab))]
       (doseq [mutation [[[:db/add eid :datahike.pg.index/unique? true]]
                         [[:db/retract eid :datahike.pg.index/legacy-incomplete? true]]]]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo
                               #"invalid unique index descriptor"
                               (d/transact conn mutation))))))))

(deftest recreated-store-id-cannot-reuse-an-old-writer-admission
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}
        admissions (atom {})]
    (dotimes [_ 2]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (pg/make-query-handler conn {:unique-admissions admissions})
          (is (= objects/catalog-version
                 (:datahike.pg.catalog/version (objects/catalog-entity (d/db conn)))))
          (finally (d/release conn) (d/delete-database cfg)))))
    (tx-preds/unregister-tx-pred! (get-in cfg [:store :id]) unique/predicate-id)))

(deftest removing-a-database-evicts-its-branch-admissions
  (let [admissions (atom {})
        server {:registry-atom (atom {"example" *conn*})
                :unique-admissions admissions}]
    (pg/add-database! server "example" *conn*)
    (is (seq @admissions))
    (pg/remove-database! server "example")
    (is (empty? @admissions))))

(deftest sql-drop-database-evicts-its-admission
  (let [admissions (atom {})
        registry (atom {"main" *conn* "dropme" *conn*})
        handler (pg/make-query-handler
                 *conn* {:db-name "main" :unique-admissions admissions
                         :registry-atom registry
                         :on-delete-database (fn [& _])})]
    (is (seq @admissions))
    (binding [*handler* handler]
      (is (nil? (state "DROP DATABASE dropme"))))
    (is (not (contains? @registry "dropme")))
    (is (empty? @admissions))))

(deftest creating-a-branch-does-not-admit-a-moving-source-snapshot
  (let [admissions (atom {})
        handler (pg/make-query-handler *conn* {:unique-admissions admissions})]
    (binding [*handler* handler]
      (is (nil? (state "SELECT datahike.create_branch('not_yet_admitted','db')"))))
    (is (not-any? #(= :not_yet_admitted (second %)) (keys @admissions)))))

(deftest deleting-a-branch-evicts-only-that-branch-admission
  (let [admissions (atom {})
        handler (pg/make-query-handler *conn* {:unique-admissions admissions})
        store-id (get-in (dbi/-config (d/db *conn*)) [:store :id])]
    (binding [*handler* handler]
      (is (nil? (state "SELECT datahike.create_branch('evict_me','db')")))
      (let [branch-conn (d/connect (assoc (dbi/-config (d/db *conn*))
                                          :branch :evict_me))]
        (try
          (is (true? (#'pg/admit-unique-index-enforcement!
                      branch-conn admissions)))
          (finally
            (d/release branch-conn))))
      (is (contains? @admissions [store-id :db]))
      (is (contains? @admissions [store-id :evict_me]))
      (is (nil? (state "SELECT datahike.delete_branch('evict_me')")))
      (is (contains? @admissions [store-id :db]))
      (is (not (contains? @admissions [store-id :evict_me]))))))

(deftest disjoint-entity-rebase-rejects-a-new-unique-key-conflict
  (doseq [sql ["CREATE TABLE uniq_rebase(a int)"
               "CREATE UNIQUE INDEX uniq_rebase_a ON uniq_rebase(a)"
               "INSERT INTO uniq_rebase VALUES(1)"]]
    (is (nil? (state sql))))
  (let [before (d/db *conn*)
        eid (ffirst (d/q '{:find [?e] :where [[?e :uniq_rebase/a 1]]} before))
        buffer [[:db/add eid :uniq_rebase/a 2]]
        speculative (:db-after (dc/with before buffer))
        tx-state (atom {:speculative-db speculative :tx-buffer buffer
                        :begin-max-tx (:max-tx before) :eid->tempid {}
                        :tx-options {}})]
    (is (nil? (state "INSERT INTO uniq_rebase VALUES(2)")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                          (#'pg/rebase-tx-state! *conn* tx-state)))
    (is (identical? speculative (:speculative-db @tx-state)))))

(deftest index-backfill-options-follow-savepoint-rollback-and-commit
  (doseq [sql ["CREATE TABLE uniq_savepoint(a int,b int)"
               "INSERT INTO uniq_savepoint VALUES(1,10),(2,20)"
               "BEGIN"
               "SAVEPOINT before_index"
               "CREATE UNIQUE INDEX uniq_savepoint_a ON uniq_savepoint(a)"
               "ROLLBACK TO SAVEPOINT before_index"
               "COMMIT"]]
    (is (nil? (state sql)) sql))
  (is (not (get-in (d/db *conn*) [:schema :uniq_savepoint/a :db/index])))
  (doseq [sql ["BEGIN"
               "CREATE UNIQUE INDEX uniq_savepoint_a ON uniq_savepoint(a)"
               "SAVEPOINT after_index"]]
    (is (nil? (state sql)) sql))
  (is (= "23505" (state "INSERT INTO uniq_savepoint VALUES(1,30)")))
  (is (nil? (state "ROLLBACK TO SAVEPOINT after_index")))
  (is (nil? (state "COMMIT")))
  (is (= #{1 2} (into #{} (map :v) (d/datoms (d/db *conn*) :avet :uniq_savepoint/a))))
  (is (= "23505" (state "INSERT INTO uniq_savepoint VALUES(1,30)")))
  (is (not (:allow-index-backfill? (dbi/-config (d/db *conn*)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (d/transact *conn* [{:db/ident :uniq_savepoint/b :db/index true}]))))
