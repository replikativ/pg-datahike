(ns datahike.test.pg-unique-index-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.db.interface :as dbi]
            [datahike.tx-preds :as tx-preds]
            [datahike.versioning :as versioning]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.constraints.unique :as unique]
            [datahike.pg.dump :as dump]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.catalog-constraint-read :as constraint-read]
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

(deftest postgres-alter-table-adds-a-composite-unique-constraint
  (doseq [sql ["CREATE TABLE atacc1 (test int, test2 int)"
               "ALTER TABLE atacc1 ADD CONSTRAINT atacc_test1 UNIQUE (test, test2)"
               "INSERT INTO atacc1 (test,test2) VALUES (4,4)"]]
    (is (nil? (state sql)) sql))
  (is (= "23505" (state "INSERT INTO atacc1 (test,test2) VALUES (4,4)")))
  (doseq [sql ["INSERT INTO atacc1 (test,test2) VALUES (4,5)"
               "INSERT INTO atacc1 (test,test2) VALUES (5,4)"
               "INSERT INTO atacc1 (test,test2) VALUES (5,5)"]]
    (is (nil? (state sql)) sql))
  (is (= [["4" "4"] ["4" "5"] ["5" "4"] ["5" "5"]]
         (rows "SELECT test,test2 FROM atacc1 ORDER BY test,test2")))
  (is (nil? (state "DROP TABLE atacc1")))
  (is (= [] (rows "SELECT indexname FROM pg_indexes WHERE indexname='atacc_test1'"))))

(deftest odoo-alter-added-key-arbitrates-field-upserts
  (doseq [sql ["CREATE TABLE ir_model_fields (id int PRIMARY KEY, model varchar NOT NULL, name varchar NOT NULL, changed int)"
               "BEGIN"
               "INSERT INTO ir_model_fields VALUES (1, 'res.users', 'login', 0), (2, 'res.users', 'active', 0)"
               "ALTER TABLE ir_model_fields ADD CONSTRAINT ir_model_fields_name_unique UNIQUE (model,name)"
               "COMMENT ON CONSTRAINT ir_model_fields_name_unique ON ir_model_fields IS 'UNIQUE(model, name)'"
               "COMMIT"]]
    (let [result (execute sql)]
      (is (nil? (:state result)) (str sql " " (:error result)))))
  (is (= [["1" "1"]]
         (rows "INSERT INTO ir_model_fields VALUES (3, 'res.users', 'login', 1)
                ON CONFLICT (model, name) DO UPDATE SET changed=EXCLUDED.changed RETURNING id, changed")))
  (is (= [["1" "2"]]
         (rows "INSERT INTO ir_model_fields VALUES (4, 'res.users', 'login', 2)
                ON CONFLICT ON CONSTRAINT ir_model_fields_name_unique
                DO UPDATE SET changed=EXCLUDED.changed RETURNING id, changed")))
  (is (= "23505" (state "INSERT INTO ir_model_fields VALUES (4, 'res.users', 'login', 3)")))
  (is (= "23505" (state "UPDATE ir_model_fields SET name='login' WHERE id=2")))
  (is (= [["1" "login" "2"] ["2" "active" "0"]]
         (rows "SELECT id,name,changed FROM ir_model_fields ORDER BY id")))
  (is (= [["u" "UNIQUE (model, name)" "t"]]
         (rows "SELECT c.contype, pg_get_constraintdef(c.oid), i.indisunique
                FROM pg_constraint c JOIN pg_index i ON i.indexrelid=c.conindid
                WHERE c.conname='ir_model_fields_name_unique'")))
  (is (= [["model" "1" nil] ["name" "2" nil]]
         (rows "SELECT column_name, ordinal_position, position_in_unique_constraint
                FROM information_schema.key_column_usage
                WHERE constraint_name='ir_model_fields_name_unique' ORDER BY ordinal_position")))
  (is (= [["UNIQUE"]]
         (rows "SELECT constraint_type FROM information_schema.table_constraints
                WHERE constraint_name='ir_model_fields_name_unique'")))
  (is (re-find #"ADD CONSTRAINT \"ir_model_fields_name_unique\" UNIQUE"
               (dump/dump-to-string *conn* {:sections #{:schema}})))
  (is (= "2BP01" (state "DROP INDEX ir_model_fields_name_unique")))
  (is (nil? (state "CREATE TABLE field_reference (model varchar,name varchar, FOREIGN KEY (model,name) REFERENCES ir_model_fields(model,name))")))
  (is (= "2BP01" (state "ALTER TABLE ir_model_fields DROP CONSTRAINT ir_model_fields_name_unique")))
  (is (nil? (state "DROP TABLE field_reference")))
  (is (nil? (state "ALTER TABLE ir_model_fields DROP CONSTRAINT ir_model_fields_name_unique")))
  (is (= [] (rows "SELECT indexname FROM pg_indexes WHERE indexname='ir_model_fields_name_unique'")))
  (is (nil? (state "INSERT INTO ir_model_fields VALUES (4, 'res.users', 'login', 3)"))))

(deftest composite-unique-add-validates-old-rows-and-rolls-back
  (doseq [sql ["CREATE TABLE populated (a int,b int)"
               "INSERT INTO populated VALUES (1,2),(1,2),(1,NULL),(1,NULL)"]]
    (is (nil? (state sql)) sql))
  (let [oid-before (:datahike.pg.catalog/next-oid (objects/catalog-entity (d/db *conn*)))]
    (is (= "23505" (state "ALTER TABLE populated ADD UNIQUE (a,b)")))
    (is (= oid-before (:datahike.pg.catalog/next-oid (objects/catalog-entity (d/db *conn*)))))
    (is (= [] (rows "SELECT indexname FROM pg_indexes WHERE tablename='populated'"))))
  (is (= "42703" (state "ALTER TABLE populated ADD UNIQUE (a,missing)")))
  (is (= "42701" (state "ALTER TABLE populated ADD UNIQUE (a,a)")))
  (is (= "0A000" (state "ALTER TABLE populated ADD CONSTRAINT deferred_key UNIQUE (a,b) DEFERRABLE")))
  (doseq [sql ["DELETE FROM populated WHERE b=2"
               "INSERT INTO populated VALUES (1,2)"
               "BEGIN"
               "ALTER TABLE populated ADD UNIQUE (a,b), ADD COLUMN c int, ADD UNIQUE (b,c)"]]
    (let [result (execute sql)]
      (is (nil? (:state result)) (str sql " " (:error result)))))
  (is (= [["populated_a_b_key"] ["populated_b_c_key"]]
         (rows "SELECT indexname FROM pg_indexes WHERE tablename='populated' ORDER BY indexname")))
  (is (= "23505" (state "INSERT INTO populated (a,b) VALUES (1,2)")))
  (is (nil? (state "ROLLBACK")))
  (is (= [] (rows "SELECT indexname FROM pg_indexes WHERE tablename='populated'")))
  (is (nil? (state "ALTER TABLE populated ADD UNIQUE (a,b)")))
  (is (nil? (state "INSERT INTO populated VALUES (1,NULL),(1,NULL)")))
  (is (= "23505" (state "INSERT INTO populated VALUES (1,2)"))))

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

(deftest postgres-composite-index-inference-accepts-both-column-orders
  (doseq [sql ["CREATE TABLE insertconflicttest (key int, fruit text)"
               "CREATE UNIQUE INDEX comp_key_index ON insertconflicttest (key,fruit)"
               "INSERT INTO insertconflicttest VALUES (7, 'Raspberry')
                ON CONFLICT (key,fruit) DO UPDATE SET fruit=EXCLUDED.fruit"
               "INSERT INTO insertconflicttest VALUES (8, 'Lime')
                ON CONFLICT (fruit,key) DO UPDATE SET fruit=EXCLUDED.fruit"]]
    (is (nil? (state sql)) sql))
  (is (= "42P10"
         (state "INSERT INTO insertconflicttest VALUES (9, 'Banana')
                 ON CONFLICT (key) DO UPDATE SET fruit=EXCLUDED.fruit")))
  (is (= [["7" "Raspberry"] ["8" "Lime"]]
         (rows "SELECT key,fruit FROM insertconflicttest ORDER BY key"))))

(deftest composite-arbitration-sees-an-earlier-key-change
  (doseq [sql ["CREATE TABLE moving_key(module text, k int, dest int, UNIQUE(module,k))"
               "INSERT INTO moving_key VALUES ('base',1,7)"]]
    (is (nil? (state sql)) sql))
  ;; The first candidate moves the existing row to key 2. Key 1 is free
  ;; for the next candidate even though it exists on the statement basis.
  (let [result (execute "INSERT INTO moving_key VALUES ('base',1,2),('base',1,9)
                         ON CONFLICT (module,k) DO UPDATE SET k=excluded.dest
                         RETURNING k,dest")]
    (is (nil? (:state result)))
    (is (= [["2" "7"] ["1" "9"]] (:rows result))))
  (is (= [["1" "9"] ["2" "7"]] (rows "SELECT k,dest FROM moving_key ORDER BY k"))))

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

(deftest native-batch-validates-composite-keys-with-a-shared-leading-value
  (is (nil? (state "CREATE TABLE batch_keys (a int, b int)")))
  (is (nil? (state "CREATE UNIQUE INDEX batch_ab ON batch_keys(a,b)")))
  (let [row (fn [b] {:batch_keys/db-row-exists true :batch_keys/a 1 :batch_keys/b b})]
    (d/transact *conn* [(row 1) (row 2)])
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                          (d/transact *conn* [(row 3) (row 2)])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unique violation"
                          (d/transact *conn* [(row 3) (row 3)])))
    (is (= [["1"] ["2"]] (rows "SELECT b FROM batch_keys ORDER BY b")))))

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
    (is (= "23505" (state (str "INSERT INTO " table " VALUES(3, -0.0)"))))
    (is (= [["1" "NaN"] ["2" "0"]]
           (rows (str "SELECT id,payload FROM " table " ORDER BY id"))))))

;; Kept executable but outside the ordinary release gate. Released Datahike's
;; index comparator currently treats a finite scalar and NaN as the same sort
;; key, so the native restatement is discarded before the transaction
;; predicate can inspect it. Fixing that changes persisted index ordering and
;; belongs to the Datahike 1.0 migration decision (datahike#1075), not to a
;; pgwire-only workaround. Remove the skip in a migration worktree to use this
;; as the focused acceptance test.
(deftest ^:kaocha/skip native-finite-to-nan-restatement-enforces-sql-uniqueness
  (doseq [[table type] [["uniq_native_nan4" "real"]
                        ["uniq_native_nan8" "double precision"]]]
    (is (nil? (state (str "CREATE TABLE " table
                          "(id int PRIMARY KEY, payload " type ")"))))
    (is (nil? (state (str "CREATE UNIQUE INDEX " table "_payload ON "
                          table "(payload)"))))
    (is (nil? (state (str "INSERT INTO " table " VALUES(1, 'NaN'),(2, 0)"))))
    (let [id-attr (keyword table "id")
          payload-attr (keyword table "payload")
          eid (:db/id (d/entity (d/db *conn*) [id-attr 2]))]
      (is (some? eid))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"unique violation"
           (d/transact *conn* [[:db/add eid payload-attr Double/NaN]]))))))

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

(deftest admission-uses-one-transaction-and-cache-hits-do-not-write
  (let [admissions (atom {})
        commits (atom [])]
    (d/listen-commits *conn* ::admission-commits #(swap! commits conj %))
    (try
      (with-redefs-fn
        (if-let [barrier (ns-resolve 'datahike.api 'writer-barrier)]
          {barrier (fn [& _]
                     (throw (ex-info "admission must not use a barrier" {})))}
          {})
        (fn []
          (is (true? (#'pg/admit-unique-index-enforcement! *conn* admissions)))
          (is (= 1 (count @commits)))
          (is (true? (#'pg/admit-unique-index-enforcement! *conn* admissions)))
          (is (= 1 (count @commits)))))
      (finally (d/unlisten-commits *conn* ::admission-commits)))))

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

(deftest failed-admission-does-not-publish-legacy-migration
  (legacy-database
   (fn [conn _]
     (let [before (d/db conn)
           admissions (atom {})]
       (with-redefs [unique/validate-db! (fn [_]
                                           (throw (ex-info "reject migrated candidate" {})))]
         (is (thrown-with-msg?
              clojure.lang.ExceptionInfo #"reject migrated candidate"
              (#'pg/admit-unique-index-enforcement! conn admissions))))
       (is (empty? @admissions))
       (is (= (get-in before [:meta :datahike/commit-id])
              (get-in (d/db conn) [:meta :datahike/commit-id])))
       (is (= (objects/catalog-entity before)
              (objects/catalog-entity (d/db conn)))))
     (is (some? (pg/make-query-handler conn))))))

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

(deftest new-table-index-includes-earlier-writes-at-commit
  ;; PostgreSQL 18 alter_table.sql:2349–2351, grouped as one migration.
  (doseq [sql ["BEGIN"
               "CREATE TABLE ataddindex(f1 INT)"
               "INSERT INTO ataddindex VALUES (42), (43)"
               "CREATE UNIQUE INDEX ataddindexi0 ON ataddindex(f1)"]]
    (is (nil? (state sql)) sql))
  (let [lookup "SELECT f1 FROM ataddindex WHERE f1 = 42"]
    (is (= [["42"]] (rows lookup)))
    (is (nil? (state "COMMIT")))
    (binding [*handler* (pg/make-query-handler *conn*)]
      (is (= [["42"] ["43"]] (rows "SELECT f1 FROM ataddindex ORDER BY f1")))
      (is (= [["42"]] (rows lookup)))
      (is (= "23505" (state "INSERT INTO ataddindex VALUES (42)"))))))

(deftest odoo-external-ids-remain-searchable-after-bootstrap
  (doseq [sql ["BEGIN"
               "CREATE TABLE ir_model_data(module varchar, name varchar, model varchar, res_id int)"
               "INSERT INTO ir_model_data VALUES ('base','group_user','res.groups',1), ('base','module_category_accounting','ir.module.category',1)"
               "CREATE UNIQUE INDEX ir_model_data_module_name ON ir_model_data(module,name)"
               "CREATE INDEX ir_model_data_model_res_id ON ir_model_data(model,res_id)"
               "COMMIT"]]
    (is (nil? (state sql)) sql))
  (binding [*handler* (pg/make-query-handler *conn*)]
    (is (= [["res.groups" "1"]]
           (rows "SELECT model,res_id FROM ir_model_data WHERE module='base' AND name='group_user'")))
    (is (= [["1"]]
           (rows "SELECT res_id FROM ir_model_data WHERE name='module_category_accounting' AND module='base' AND model='ir.module.category'")))
    (is (= [["2"]]
           (rows "INSERT INTO ir_model_data VALUES ('base','group_user','res.groups',2)
                  ON CONFLICT(module,name) DO UPDATE SET res_id=EXCLUDED.res_id RETURNING res_id")))
    (is (= [["2"]] (rows "SELECT count(*) FROM ir_model_data")))))

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

(deftest a-hash-index-is-accepted-as-equality-metadata
  ;; A PostgreSQL hash index serves EQUALITY and nothing else, which is
  ;; exactly what Datahike's native AVET/AEVT already serve -- so the
  ;; declaration is compatibility metadata, as a btree one already was.
  ;;
  ;; Rejecting it outright with `index method hash is not supported for
  ;; this column` made `CREATE INDEX ... USING HASH` fail, and uuid.sql
  ;; then counted two `guid%` indexes in pg_class where PostgreSQL
  ;; counts three. That one refusal was uuid's entire remaining gap.
  (is (nil? (state "CREATE TABLE hx (guid_field uuid, id int)")))
  (is (nil? (state "CREATE INDEX hx_btree ON hx USING BTREE (guid_field)")))
  (is (nil? (state "CREATE INDEX hx_hash ON hx USING HASH (guid_field)")))
  (testing "and both are visible in pg_class, as PostgreSQL's own count query reads them"
    (is (= [["2"]]
           (rows (str "SELECT count(*) FROM pg_class"
                      " WHERE relkind='i' AND relname LIKE 'hx%'")))))
  (testing "a UNIQUE hash index is refused, in PostgreSQL's words"
    (let [r (execute "CREATE UNIQUE INDEX hx_uh ON hx USING HASH (guid_field)")]
      (is (= "0A000" (:state r)))
      (is (= "access method \"hash\" does not support unique indexes" (:error r)))))
  (testing "a unique BTREE index is still accepted"
    (is (nil? (state "CREATE UNIQUE INDEX hx_ub ON hx USING BTREE (guid_field)"))))
  (testing "an unknown access method is still refused"
    (is (= "0A000" (state "CREATE INDEX hx_brin ON hx USING BRIN (id)")))))

(deftest named-composite-key-definition-follows-transactional-ddl
  (let [probe (str "SELECT COALESCE(d.description, pg_get_constraintdef(c.oid)) "
                   "FROM pg_constraint c JOIN pg_class t ON t.oid=c.conrelid "
                   "LEFT JOIN pg_description d ON c.oid=d.objoid "
                   "WHERE t.relname='probe' AND conname='probe_key' "
                   "AND t.relnamespace=current_schema::regnamespace")
        prepared (.parse *handler* probe (int-array 0))
        read-definition (fn []
                          (let [^PgWireServer$QueryResult r
                                (.executePrepared *handler* prepared (object-array [nil]))]
                            (is (nil? (.-error r)) (.-error r))
                            (mapv vec (.-rows r))))]
    (is (= [] (read-definition)))
    (is (nil? (state "CREATE TABLE probe(id int, name text, CONSTRAINT probe_key UNIQUE(id, name))")))
    (let [db @*conn*
          plan (:catalog-plan (sql/parse-sql probe (:schema db) db true))]
      (is (= {:relation-name "probe" :constraint-name "probe_key"} plan))
      (is (some? (constraint-read/query-db db plan))))
    (is (= [["UNIQUE (id, name)"]] (read-definition)))
    (is (= [["UNIQUE (id, name)"]]
           (rows "SELECT pg_get_constraintdef(c.oid) FROM pg_constraint c WHERE c.conname = 'probe_key'")))
    (is (nil? (state "CREATE SCHEMA archive")))
    (is (nil? (state "CREATE TABLE archive.probe(x int, y int, CONSTRAINT probe_key PRIMARY KEY(x,y))")))
    (is (nil? (state "SET search_path TO archive, public")))
    (is (= [["PRIMARY KEY (x, y)"]] (read-definition)))
    (is (nil? (state "SET search_path TO public")))
    (is (nil? (state "BEGIN")))
    (is (nil? (state "SAVEPOINT before_drop")))
    (is (nil? (state "DROP TABLE probe")))
    (is (= [] (read-definition)))
    (is (nil? (state "ROLLBACK TO SAVEPOINT before_drop")))
    (is (= [["UNIQUE (id, name)"]] (read-definition)))
    (is (nil? (state "COMMIT")))))
