(ns datahike.test.pg-catalog-cache-basis-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.schema :as schema]
            [datahike.pg.server :as pg]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.catalog :as catalog]
            [datahike.pg.sql.catalog-read :as catalog-read]
            [datahike.pg.sql.stmt :as stmt])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- with-db [f]
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try
        (d/transact conn [{:db/ident :cache_test/value
                           :db/valueType :db.type/string
                           :db/cardinality :db.cardinality/one}
                          {:db/ident :pg/typmod
                           :db/valueType :db.type/long
                           :db/cardinality :db.cardinality/one}
                          {:db/ident :pg/type
                           :db/valueType :db.type/string
                           :db/cardinality :db.cardinality/one}])
        (f conn)
        (finally (d/release conn) (d/delete-database config))))))

(defn- values [db]
  (set (d/q '[:find [?v ...] :where [_ :cache_test/value ?v]] db)))

(deftest enriched-catalogs-preserve-the-current-user-snapshot
  (binding [sql/*catalog-cache* (java.util.HashMap.)]
    (with-db
      (fn [conn]
        (d/transact conn [{:db/id 10000 :cache_test/value "first"}])
        (let [before @conn
              enriched (sql/enrich-db-with-catalogs before (dbi/-schema before) ["pg_class"])]
          (is (= #{"first"} (values enriched)))
          (d/transact conn [[:db/add 10000 :cache_test/value "second"]])
          (let [after @conn]
            (is (= #{"second"}
                   (values (sql/enrich-db-with-catalogs after (dbi/-schema after) ["pg_class"])))))
          (is (= #{"first"} (values enriched))))))
    (with-db
      (fn [conn]
        (d/transact conn [{:cache_test/value "other database"}])
        (is (= #{"other database"}
               (values (sql/enrich-db-with-catalogs @conn (dbi/-schema @conn) ["pg_class"]))))))))

(deftest native-typmod-change-invalidates-enrichment-without-global-clear
  (with-db
    (fn [conn]
      (d/transact conn [[:db/add [:db/ident :cache_test/value] :pg/typmod 12]])
      (let [before @conn
            schema (dbi/-schema before)]
        (is (= 12 (get-in (stmt/enrich-schema-with-pg-array-meta schema before)
                          [:cache_test/value :pg/typmod])))
        (d/transact conn [[:db/add [:db/ident :cache_test/value] :pg/typmod 24]])
        (is (= schema (dbi/-schema @conn)))
        (is (= 24 (get-in (stmt/enrich-schema-with-pg-array-meta schema @conn)
                          [:cache_test/value :pg/typmod])))))))

(deftest translation-cache-tracks-catalog-but-not-ordinary-row-changes
  (binding [sql/*parse-cache* (java.util.HashMap.)]
    (with-db
      (fn [conn]
        (let [text "SELECT value FROM cache_test"
              before (sql/parse-sql text (dbi/-schema @conn) @conn)]
          (is (= :select (:type before)))
          (d/transact conn [{:cache_test/value "new row"}])
          (is (identical? before (sql/parse-sql text (dbi/-schema @conn) @conn)))
          (d/transact conn [[:db/add [:db/ident :cache_test/value] :pg/typmod 16]])
          (is (not (identical? before (sql/parse-sql text (dbi/-schema @conn) @conn)))))
        (is (= :select (:type (sql/parse-sql "SELECT value FROM cache_test"
                                             (dbi/-schema @conn)))))))))

(deftest extension-catalog-rows-are-not-assumed-to-be-metadata-only
  (let [name "pg_cache_basis_extension"]
    (catalog/register-catalog-table!
     name {:schema [{:db/ident :pg_cache_basis_extension/value
                     :db/valueType :db.type/string
                     :db/cardinality :db.cardinality/one}]
           :data-fn (fn [_ db]
                      (mapv #(hash-map :pg_cache_basis_extension/value %) (values db)))})
    (try
      (binding [sql/*catalog-cache* (java.util.HashMap.)]
        (with-db
          (fn [conn]
            (doseq [value ["before" "after"]]
              (d/transact conn [[:db/add 10000 :cache_test/value value]])
              (let [enriched (sql/enrich-db-with-catalogs @conn (dbi/-schema @conn) [name])]
                (is (= #{value}
                       (set (d/q '[:find [?v ...] :where [_ :pg_cache_basis_extension/value ?v]]
                                 enriched)))))))))
      (finally (catalog/unregister-catalog-table! name)))))

(deftest built-in-catalog-reuse-is-local-to-the-simple-statement
  (with-db
    (fn [conn]
      (let [^PgWireServer$QueryHandler handler (pg/make-query-handler conn)
            query (str "SELECT c.relname FROM pg_class c JOIN pg_attribute a "
                       "ON a.attrelid = c.oid WHERE a.attname = 'new_column' "
                       "AND c.relname = 'new_relation'")
            enrich sql/enrich-db-with-catalogs
            calls (atom 0)]
        (with-redefs [sql/enrich-db-with-catalogs (fn [db schema catalogs]
                                                    (swap! calls inc)
                                                    (enrich db schema catalogs))]
          (let [^PgWireServer$QueryResult before (.execute handler query)]
            (is (nil? (.-error before)))
            (is (empty? (.-rows before)))
            (is (= 1 @calls)))
          (let [prepared (.parse handler query (int-array 0))
                ^PgWireServer$QueryResult created (.execute handler "CREATE TABLE new_relation(new_column int)")
                ^PgWireServer$QueryResult after (.executePrepared handler prepared (object-array [nil]))]
            (is (nil? (.-error created)))
            (is (nil? (.-error after)))
            (is (= [["new_relation"]] (mapv vec (.-rows after))))))))))

(deftest scoped-foreign-key-catalog-reads-use-the-execution-snapshot
  (with-db
    (fn [conn]
      (with-open [^PgWireServer$QueryHandler h (pg/make-query-handler conn)]
        (let [execute (fn [sql]
                        (let [^PgWireServer$QueryResult result (.execute h sql)]
                          (is (nil? (.-error result)) sql)
                          (mapv vec (.-rows result))))
              query (str "SELECT f.conname, p.relname, a.attname FROM pg_constraint f "
                         "JOIN pg_class c ON c.oid = f.conrelid "
                         "JOIN pg_class p ON p.oid = f.confrelid "
                         "JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = f.conkey[1] "
                         "WHERE f.contype = 'f' AND c.relname IN ('child','also_absent') "
                         "AND c.relnamespace = current_schema::regnamespace")]
          (doseq [ddl ["CREATE TABLE parent (id int PRIMARY KEY)"
                       "CREATE TABLE child (p int CONSTRAINT public_fk REFERENCES parent)"
                       "CREATE SCHEMA audit"
                       "CREATE TABLE audit.child (p int CONSTRAINT audit_fk REFERENCES public.parent)"]]
            (execute ddl))
          (is (= #{"child" "also_absent"}
                 (:catalog-child-names (sql/parse-sql query (:schema @conn) @conn true))))
          (is (nil? (:catalog-read? (sql/parse-sql query (:schema @conn) @conn))))
          (is (every? #(get-in (:schema @conn) [% :db/index]) [:pg/fk-child-table :pg/fk-parent-table]))
          (let [two-relations (str "SELECT k.conname FROM pg_class t JOIN pg_constraint k "
                                   "ON k.conrelid = t.oid WHERE t.relname = 'child' AND k.contype = 'f'")]
            (is (= #{"child"} (:catalog-child-names (sql/parse-sql two-relations (:schema @conn) @conn true))))
            (is (= #{["public_fk"] ["audit_fk"]} (set (execute two-relations)))))
          (let [plan (.parse h query (int-array 0))
                read-plan (fn []
                            (let [^PgWireServer$QueryResult result
                                  (.executePrepared h plan (object-array [nil]))]
                              (is (nil? (.-error result)))
                              (is (= [19 19 19] (vec (.-columnOids result))))
                              (set (map vec (.-rows result)))))]
            (is (= #{["public_fk" "parent" "p"]} (read-plan)))
            (execute "SET search_path TO audit, public")
            (is (= #{["audit_fk" "parent" "p"]} (read-plan)))
            (execute "BEGIN")
            (execute "SAVEPOINT before_fk")
            (execute "ALTER TABLE audit.child ADD CONSTRAINT new_fk FOREIGN KEY (p) REFERENCES public.parent")
            (is (= #{["audit_fk" "parent" "p"] ["new_fk" "parent" "p"]} (read-plan)))
            (execute "ROLLBACK TO SAVEPOINT before_fk")
            (is (= #{["audit_fk" "parent" "p"]} (read-plan)))
            (execute "COMMIT"))
          (let [scan "SELECT conname FROM pg_constraint WHERE contype = 'f'"]
            (is (= #{["public_fk"] ["audit_fk"]} (set (execute scan)))))
          (let [independent (str "SELECT a.attname FROM pg_class c "
                                 "JOIN pg_constraint f ON c.oid = f.conrelid "
                                 "CROSS JOIN pg_attribute a WHERE c.relname = 'child' AND f.contype = 'f'")]
            (is (nil? (:catalog-child-names (sql/parse-sql independent (:schema @conn) @conn true))))
            (is (some #{["id"]} (execute independent))))
          (execute "SET search_path TO public")
          (is (some? (catalog-read/query-db @conn #{"child"})))
          (schema/set-hint! conn :child/p {:hidden true})
          (is (nil? (catalog-read/query-db @conn #{"child"})))
          (is (empty? (execute query))))))))

(deftest foreign-key-attnums-use-live-names
  (with-db
    (fn [conn]
      (with-open [^PgWireServer$QueryHandler h (pg/make-query-handler conn)]
        (doseq [sql ["CREATE TABLE p (id int PRIMARY KEY)"
                     "CREATE TABLE c (\"........pg.dropped.2........\" int REFERENCES p, spare int)"
                     "ALTER TABLE c DROP COLUMN spare"]]
          (is (nil? (.-error ^PgWireServer$QueryResult (.execute h sql)))))
        (let [sql (str "SELECT f.conkey, a.attnum FROM pg_constraint f "
                       "JOIN pg_class c ON c.oid=f.conrelid "
                       "JOIN pg_attribute a ON a.attrelid=c.oid AND a.attnum=f.conkey[1] "
                       "WHERE f.contype='f' AND c.relname='c'")
              ^PgWireServer$QueryResult result (.execute h sql)]
          (is (nil? (.-error result)))
          (is (= [["{1}" "1"]] (mapv vec (.-rows result)))))
        (is (nil? (.-error ^PgWireServer$QueryResult
                   (.execute h "CREATE TABLE pg (p int REFERENCES p)"))))
        (is (nil? (catalog-read/query-db @conn #{"pg"})))))))
