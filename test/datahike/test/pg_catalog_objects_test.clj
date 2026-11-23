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

(deftest schema-ddl-uses-durable-namespace-identities
  (with-catalog
    (fn [conn]
      (with-open [handler (server/make-query-handler conn)]
        (let [run #(.execute handler %)
              exec! (fn [sql] (is (nil? (.error (run sql))) sql))
              rows #(mapv vec (.rows (run %)))
              oid-for (fn [name]
                        (:datahike.pg.object/oid (objects/namespace-by-name (d/db conn) name)))]
          (exec! "CREATE SCHEMA \"Billing Data\"")
          (exec! "CREATE SCHEMA archive")
          (let [oid (oid-for "Billing Data")]
            (is (>= oid objects/first-user-oid))
            (is (= [[(str oid)]] (rows "SELECT oid FROM pg_namespace WHERE nspname = 'Billing Data'")))
            (is (= "42P06" (.sqlstate (run "CREATE SCHEMA \"Billing Data\""))))
            (is (= "42939" (.sqlstate (run "CREATE SCHEMA pg_reserved"))))
            (exec! "CREATE SCHEMA IF NOT EXISTS \"Billing Data\"")
            (exec! "SET search_path TO missing, \"Billing Data\", public")
            (is (= [["Billing Data" "Billing Data" "{\"Billing Data\",public}"]]
                   (rows "SELECT current_schema(), current_schema, current_schemas(false)::text")))
            (is (= [["{pg_catalog,\"Billing Data\",public}"]]
                   (rows "SELECT current_schemas(true)::text")))
            (exec! "BEGIN")
            (exec! "ALTER SCHEMA \"Billing Data\" RENAME TO temporary_billing")
            (is (= [[(str oid)]] (rows "SELECT oid FROM pg_namespace WHERE nspname = 'temporary_billing'")))
            (is (= [["public"]] (rows "SELECT current_schema()")))
            (exec! "ROLLBACK")
            (is (= oid (oid-for "Billing Data")))
            (is (nil? (oid-for "temporary_billing")))
            (exec! "ALTER SCHEMA \"Billing Data\" RENAME TO billing")
            (is (= oid (oid-for "billing")))
            (is (= "3F000" (.sqlstate (run "DROP SCHEMA billing, missing"))))
            (is (= oid (oid-for "billing")))
            (exec! "DROP SCHEMA IF EXISTS billing, missing, archive RESTRICT")
            (is (nil? (oid-for "billing")))
            (is (nil? (oid-for "archive")))
            (exec! "CREATE SCHEMA ÄMixed")
            (is (some? (oid-for "Ämixed")))))))))

(deftest scoped-relations-preserve-namespace-and-storage-identity
  (with-catalog
    (fn [conn]
      (with-open [handler (server/make-query-handler conn)]
        (let [run #(.execute handler %)
              exec! (fn [sql] (is (nil? (.error (run sql))) sql))
              rows #(mapv vec (.rows (run %)))]
          (doseq [sql ["CREATE SCHEMA billing"
                       "CREATE SCHEMA archive"
                       "CREATE TABLE billing.pg_namespace (label text)"
                       "INSERT INTO billing.pg_namespace VALUES ('customer catalog')"
                       "CREATE TABLE public.events (id int PRIMARY KEY, label text)"
                       "CREATE TABLE billing.events (id int PRIMARY KEY, label text)"
                       "INSERT INTO public.events VALUES (1, 'public')"
                       "INSERT INTO billing.events VALUES (1, 'billing')"
                       "CREATE UNIQUE INDEX events_label ON public.events (label)"
                       "CREATE UNIQUE INDEX events_label ON billing.events (label)"
                       "CREATE TABLE archive.entries (id int REFERENCES billing.events ON DELETE CASCADE)"
                       "INSERT INTO archive.entries VALUES (1)"
                       "SET search_path TO billing, public"]]
            (exec! sql))
          (is (= [["billing"]] (rows "SELECT label FROM events")))
          (is (= [["customer catalog"]] (rows "SELECT label FROM billing.pg_namespace")))
          (exec! "SET search_path TO billing, pg_catalog, public")
          (is (= [["customer catalog"]] (rows "SELECT label FROM pg_namespace")))
          (exec! "SET search_path TO billing, public")
          (is (= [["public"]] (rows "SELECT label FROM public.events")))
          (is (= [["t"]] (rows "SELECT 'billing.events_label'::regclass::oid <> 'public.events_label'::regclass::oid")))
          (is (= "23505" (.sqlstate (run "INSERT INTO billing.events VALUES (2, 'billing')"))))
          (is (= "23503" (.sqlstate (run "INSERT INTO archive.entries VALUES (2)"))))
          (is (= [["archive" "FOREIGN KEY (id) REFERENCES events (id) ON DELETE CASCADE"]]
                 (rows "SELECT n.nspname, pg_catalog.pg_get_constraintdef(c.oid) FROM pg_catalog.pg_constraint c
                          JOIN pg_catalog.pg_namespace n ON n.oid = c.connamespace
                         WHERE c.conrelid = 'archive.entries'::regclass::oid AND c.contype = 'f'")))
          (doseq [sql ["SELECT * FROM pg_catalog.events"
                       "INSERT INTO information_schema.events VALUES (2, 'wrong')"
                       "COPY pg_catalog.events FROM STDIN"
                       "COPY pg_temp.events FROM STDIN"]]
            (is (= "42P01" (.sqlstate (run sql))) sql))
          (exec! "SET search_path TO archive")
          (doseq [sql ["SELECT * FROM events"
                       "UPDATE events SET label = 'wrong'"
                       "ALTER TABLE events ADD COLUMN wrong int"
                       "DROP TABLE events"
                       "TRUNCATE events"]]
            (is (= "42P01" (.sqlstate (run sql))) sql))
          (is (= [["public"]] (rows "SELECT label FROM public.events")))
          (is (= [["public"]] (rows "SELECT nspname FROM pg_namespace WHERE nspname = 'public'")))
          (is (= [["1"] ["2"] ["3"]]
                 (rows "WITH RECURSIVE nums AS (SELECT 1 AS n UNION ALL SELECT n+1 FROM nums WHERE n < 3)
                        SELECT n FROM nums ORDER BY n")))
          (let [[[table-oid index-oid]]
                (rows "SELECT 'billing.events'::regclass::oid, 'billing.events_label'::regclass::oid")]
            (exec! "ALTER TABLE billing.events SET SCHEMA archive")
            (is (= [[table-oid index-oid]]
                   (rows "SELECT 'archive.events'::regclass::oid, 'archive.events_label'::regclass::oid")))
            (is (= "42P01" (.sqlstate (run "SELECT * FROM billing.events"))))
            (is (= [["billing"]] (rows "SELECT label FROM archive.events")))
            (is (= [["billing"]] (rows "SELECT label FROM events"))))
          (exec! "SET search_path TO billing, public")
          (is (= [["public"]] (rows "SELECT label FROM events")))
          (is (= [["archive" "FOREIGN KEY (id) REFERENCES archive.events (id) ON DELETE CASCADE"]]
                 (rows "SELECT n.nspname, pg_catalog.pg_get_constraintdef(c.oid) FROM pg_catalog.pg_constraint c
                          JOIN pg_catalog.pg_namespace n ON n.oid = c.connamespace
                         WHERE c.conrelid = 'archive.entries'::regclass::oid AND c.contype = 'f'")))
          (exec! "ALTER TABLE archive.events ADD COLUMN amount int DEFAULT 5")
          (is (= [["billing" "5"]] (rows "SELECT label, amount FROM archive.events")))
          (is (= [["billing" "7"]]
                 (rows "UPDATE archive.events SET amount = 7 WHERE id = 1 RETURNING label, amount")))
          (exec! "DELETE FROM archive.events WHERE id = 1")
          (is (= [["0"]] (rows "SELECT count(*) FROM archive.entries")))
          (exec! "DROP INDEX archive.events_label")
          (is (= [["t"]] (rows "SELECT to_regclass('public.events_label') IS NOT NULL")))
          (exec! "DROP TABLE archive.entries, archive.events")
          (is (= [["public"]] (rows "SELECT label FROM public.events"))))))))

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
          (is (= 51000 (:datahike.pg.object/oid
                        (objects/object-by-identity
                         (d/db conn) objects/pg-type-oid
                         objects/public-namespace-oid "after"))))
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
                      :db/cardinality :db.cardinality/one}
                     ;; Its Java hash is Integer/MIN_VALUE; Math/abs remains
                     ;; negative and used to leak that value into pg_class.
                     {:db/ident :polygenelubricants/db-row-exists
                      :db/valueType :db.type/boolean
                      :db/cardinality :db.cardinality/one}])
        ((deref (ns-resolve 'datahike.pg.server 'ensure-pg-schema!)) conn)
        (let [db (d/db conn)
              aa (schema/table-oid db "Aa")
              bb (schema/table-oid db "BB")
              minimum-hash (schema/table-oid db "polygenelubricants")
              next-before (:datahike.pg.catalog/next-oid
                           (objects/catalog-entity db))]
          (is (not= aa bb))
          (is (every? objects/valid-user-oid? [aa bb minimum-hash]))
          (is (true? (:datahike.pg.object/legacy-oid?
                      (objects/object-by-identity
                       db objects/pg-class-oid
                       objects/public-namespace-oid "Aa"))))
          ;; A second startup is a version-checked no-op, not a reseed.
          ((deref (ns-resolve 'datahike.pg.server 'ensure-pg-schema!)) conn)
          (is (= [aa bb minimum-hash next-before]
                 [(schema/table-oid (d/db conn) "Aa")
                  (schema/table-oid (d/db conn) "BB")
                  (schema/table-oid (d/db conn) "polygenelubricants")
                  (:datahike.pg.catalog/next-oid
                   (objects/catalog-entity (d/db conn)))]))
          (is (= #{"Aa" "BB" "polygenelubricants"}
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

(deftest catalog-v1-upgrade-backfills-column-subaddresses-atomically
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (let [handler (server/make-query-handler conn)]
          (.execute handler "CREATE TABLE v1_columns (first integer, second text)")
          (let [db (d/db conn)
                oid (schema/table-oid db "v1_columns")
                column-rows (objects/columns-by-relation db oid)]
            ;; Construct the exact durable shape of catalog v1: object rows
            ;; and allocator exist, but no column-subaddress rows do.
            (d/transact conn
                        (into [[:db/add (:db/id (objects/catalog-entity db))
                                :datahike.pg.catalog/version 1]]
                              (map (fn [column]
                                     [:db/retractEntity (:db/id column)]))
                              column-rows))
            ((deref (ns-resolve 'datahike.pg.server 'ensure-pg-schema!)) conn)
            (is (= objects/catalog-version
                   (:datahike.pg.catalog/version
                    (objects/catalog-entity (d/db conn)))))
            (is (= [["first" 1] ["second" 2]]
                   (mapv (juxt :datahike.pg.column/name
                               :datahike.pg.column/attnum)
                         (objects/columns-by-relation (d/db conn) oid true))))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest catalog-v1-upgrade-rekeys-columns-when-a-relation-oid-is-invalid
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (let [handler (server/make-query-handler conn)]
          (.execute handler "CREATE TABLE invalid_oid(first integer, second text)")
          (let [db (d/db conn)
                relation (objects/object-by-identity
                          db objects/pg-class-oid objects/public-namespace-oid
                          "invalid_oid")
                old-oid (:datahike.pg.object/oid relation)
                old-address (:datahike.pg.object/address-key relation)
                marker-eid (ffirst
                            (d/q '{:find [?marker]
                                   :where [[?marker :db/ident
                                            :invalid_oid/db-row-exists]]}
                                 db))
                columns (objects/columns-by-relation db old-oid)]
            ;; Exact v1 shape plus an address-unsafe legacy table OID. v1 has
            ;; no durable column rows; v1→v2 first creates them at OID 42 and
            ;; v2→v3 must then move, rather than duplicate, those entities.
            (d/transact
             conn
             (into [[:db/retract (:db/id relation)
                     :datahike.pg.object/address-key old-address]
                    [:db/add (:db/id relation)
                     :datahike.pg.object/address-key
                     (objects/address-key objects/pg-class-oid 42)]
                    [:db/add (:db/id relation) :datahike.pg.object/oid 42]
                    [:db/add marker-eid :pg/table-oid 42]
                    [:db/add (:db/id (objects/catalog-entity db))
                     :datahike.pg.catalog/version 1]]
                   (map (fn [column] [:db/retractEntity (:db/id column)]))
                   columns))
            ((deref (ns-resolve 'datahike.pg.server 'ensure-pg-schema!)) conn)
            (let [upgraded (d/db conn)
                  new-oid (schema/table-oid upgraded "invalid_oid")
                  upgraded-columns (objects/columns-by-relation upgraded new-oid)]
              (is (objects/valid-user-oid? new-oid))
              (is (not= 42 new-oid))
              (is (= [["first" 1] ["second" 2]]
                     (mapv (juxt :datahike.pg.column/name
                                 :datahike.pg.column/attnum)
                           upgraded-columns)))
              (is (nil? (objects/column-by-attnum upgraded 42 1)))
              (is (= new-oid
                     (ffirst
                      (d/q '{:find [?oid]
                             :where [[?marker :db/ident
                                      :invalid_oid/db-row-exists]
                                     [?marker :pg/table-oid ?oid]]}
                           upgraded)))))))
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

(deftest retired-table-generations-survive-a-file-backend-reconnect
  (let [root (Files/createTempDirectory "pg-datahike-table-history-" (make-array FileAttribute 0))
        cfg {:store {:backend :file :path (str (.resolve root "store"))
                     :id (random-uuid)}
             :schema-flexibility :write :keep-history? true}]
    (d/create-database cfg)
    (try
      (let [conn (d/connect cfg)
            before (try
                     (with-open [handler (server/make-query-handler conn)]
                       (doseq [sql ["CREATE TABLE ledger (id int)" "INSERT INTO ledger VALUES (1)"]]
                         (is (nil? (.error (.execute handler sql))) sql))
                       (let [db (d/db conn)
                             snapshot (.toInstant ^java.util.Date (:db/txInstant (d/entity db (:max-tx db))))]
                         (doseq [sql ["DROP TABLE ledger"
                                      "CREATE TABLE ledger (id text)"
                                      "INSERT INTO ledger VALUES ('current')"]]
                           (is (nil? (.error (.execute handler sql))) sql))
                         snapshot))
                     (finally (d/release conn)))
            reopened (d/connect cfg)]
        (try
          (with-open [handler (server/make-query-handler reopened)]
            (is (= [["current"]] (mapv vec (.rows (.execute handler "SELECT id FROM ledger")))))
            (is (nil? (.error (.execute handler (str "SET datahike.as_of = '" before "'")))))
            (let [result (.execute handler "SELECT id FROM ledger")]
              (is (= [["1"]] (mapv vec (.rows result))))
              (is (= [23] (vec (.-columnOids result))))))
          (finally (d/release reopened))))
      (finally
        (d/delete-database cfg)
        (Files/deleteIfExists root)))))

(deftest table-schema-move-preserves-historical-namespace-after-reopen
  (let [root (Files/createTempDirectory "pg-datahike-schema-history-" (make-array FileAttribute 0))
        cfg {:store {:backend :file :path (str (.resolve root "store")) :id (random-uuid)}
             :schema-flexibility :write :keep-history? true}]
    (d/create-database cfg)
    (try
      (let [conn (d/connect cfg)
            [snapshot oid]
            (try
              (with-open [handler (server/make-query-handler conn)]
                (doseq [sql ["CREATE SCHEMA billing"
                             "CREATE SCHEMA archive"
                             "CREATE TABLE billing.invoices (id int PRIMARY KEY)"
                             "INSERT INTO billing.invoices VALUES (1)"]]
                  (is (nil? (.error (.execute handler sql))) sql))
                (let [db (d/db conn)
                      snapshot (.toInstant ^java.util.Date (:db/txInstant (d/entity db (:max-tx db))))
                      oid (ffirst (.rows (.execute handler "SELECT 'billing.invoices'::regclass::oid")))]
                  (doseq [sql ["ALTER TABLE billing.invoices SET SCHEMA archive"
                               "UPDATE archive.invoices SET id = 2"]]
                    (is (nil? (.error (.execute handler sql))) sql))
                  [snapshot oid]))
              (finally (d/release conn)))
            reopened (d/connect cfg)]
        (try
          (with-open [handler (server/make-query-handler reopened)]
            (let [run #(.execute handler %)
                  rows #(mapv vec (.rows (run %)))]
              (is (= [[oid "2"]] (rows "SELECT 'archive.invoices'::regclass::oid, id FROM archive.invoices")))
              (is (= "42P01" (.sqlstate (run "SELECT id FROM billing.invoices"))))
              (is (nil? (.error (run (str "SET datahike.as_of = '" snapshot "'")))))
              (is (= [[oid "1"]] (rows "SELECT 'billing.invoices'::regclass::oid, id FROM billing.invoices")))
              (is (= "42P01" (.sqlstate (run "SELECT id FROM archive.invoices"))))))
          (finally (d/release reopened))))
      (finally
        (d/delete-database cfg)
        (Files/deleteIfExists root)))))
