(ns datahike.test.pg-catalog-cache-basis-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.server :as pg]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.catalog :as catalog]
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
