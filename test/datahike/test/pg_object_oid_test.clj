(ns datahike.test.pg-object-oid-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.server :as pg])
  (:import [java.io StringReader]
           [java.sql Connection DriverManager SQLException]
           [org.postgresql.core BaseConnection]
           [org.postgresql.copy CopyManager]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [f]
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
               :schema-flexibility :write :keep-history? false :max-string-length 0}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            srv (pg/start-server {"objects" conn} {:port 0 :host "127.0.0.1"})]
        (try (binding [*port* (.getPort (:server srv)) *conn* conn] (f))
             (finally (pg/stop-server srv) (d/release conn) (d/delete-database cfg)))))))

(defn- connect ^Connection [simple?]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/objects?user=x&password=x&sslmode=disable&binaryTransfer=false"
        (when simple? "&preferQueryMode=simple"))))

(defn- result-rows [^java.sql.ResultSet rs]
  (let [n (.getColumnCount (.getMetaData rs))]
    (loop [out []]
      (if (.next rs)
        (recur (conj out (mapv #(.getString rs (int %)) (range 1 (inc n))))) out))))

(defn- query [^Connection c q]
  (try (with-open [st (.prepareStatement c q) rs (.executeQuery st)]
         {:rows (result-rows rs)})
       (catch SQLException e {:sqlstate (.getSQLState e)})))

(defn- execute! [^Connection c q]
  (with-open [st (.createStatement c)] (.execute st q)))

(deftest captured-postgresql-object-alias-corpus
  (let [cases (:cases (edn/read-string (slurp "test/resources/object_oid_corpus.edn")))]
    (is (= 77 (count cases)))
    (doseq [simple? [true false]]
      (with-open [c (connect simple?)]
        (doseq [[sql expected] cases]
          (is (= expected (query c sql)) (str simple? " " sql)))))))

(deftest builtin-identities-and-errors-over-both-protocols
  (doseq [simple? [true false]]
    (with-open [c (connect simple?)]
      (doseq [[sql expected]
              [["SELECT regclass('pg_class')::oid, 'pg_catalog.pg_class'::regclass::text" {:rows [["1259" "pg_class"]]}]
               ["SELECT regnamespace('pg_catalog')::oid, 'public'::regnamespace::text" {:rows [["11" "public"]]}]
               ["SELECT to_regclass('missing_object'), to_regnamespace('missing_schema')" {:rows [[nil nil]]}]
               ["SELECT 'missing_object'::regclass" {:sqlstate "42P01"}]
               ["SELECT 'missing_schema.pg_class'::regclass" {:sqlstate "42P01"}]
               ["SELECT 'missing_schema'::regnamespace" {:sqlstate "3F000"}]
               ["SELECT 'public.pg_catalog'::regnamespace" {:sqlstate "42602"}]
               ["SELECT to_regnamespace('public.pg_catalog')" {:rows [[nil]]}]
               ["SELECT 'otherdb.pg_catalog.pg_class'::regclass" {:sqlstate "0A000"}]
               ["SELECT to_regclass('otherdb.pg_catalog.pg_class')" {:sqlstate "0A000"}]
               ["SELECT 'objects.pg_catalog.pg_class'::regclass::oid" {:rows [["1259"]]}]
               ["SELECT '-'::regclass::text, '0'::regnamespace::text, '4294967295'::regclass::text" {:rows [["-" "-" "4294967295"]]}]
               ["SELECT (-1)::regclass::oid, '999999'::regnamespace::text" {:rows [["4294967295" "999999"]]}]
               ["SELECT pg_input_is_valid('missing_object', 'regclass'), pg_input_is_valid('missing_schema', 'regnamespace')" {:rows [["f" "f"]]}]
               ["SELECT sql_error_code FROM pg_input_error_info('missing_object', 'regclass')" {:rows [["42P01"]]}]
               ["SELECT sql_error_code FROM pg_input_error_info('missing_schema', 'regnamespace')" {:rows [["3F000"]]}]
               ["SELECT c.oid, n.oid FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE c.oid = 'pg_class'::regclass" {:rows [["1259" "11"]]}]
               ["WITH x AS (SELECT 1 AS id) SELECT to_regclass('x')" {:rows [[nil]]}]]]
        (is (= expected (query c sql)) (str simple? " " sql))))))

(deftest all-supported-relation-kinds-use-projected-identities
  (with-open [c (connect false)]
    (doseq [sql ["CREATE TABLE oo_table (id integer PRIMARY KEY, v text UNIQUE)"
                 "CREATE VIEW oo_view AS SELECT id FROM oo_table"
                 "CREATE MATERIALIZED VIEW oo_mat AS SELECT id FROM oo_table"
                 "CREATE SEQUENCE oo_seq"
                 "CREATE TYPE oo_comp AS (v integer)"
                 "CREATE INDEX oo_explicit ON oo_table(v)"]]
      (execute! c sql))
    (doseq [name ["oo_table" "oo_view" "oo_mat" "oo_seq" "oo_comp"
                  "oo_explicit" "oo_table_pkey" "oo_table_v_key"]]
      (is (= {:rows [[name "t"]]}
             (query c (str "SELECT relname, oid = '" name "'::regclass::oid FROM pg_class WHERE oid = '" name "'::regclass::oid"))) name))
    (execute! c "CREATE TABLE \"OoMixed\" (v integer)")
    (is (= {:rows [["\"OoMixed\""]]}
           (query c "SELECT 'public.\"OoMixed\"'::regclass")))
    (execute! c "SET search_path TO pg_catalog")
    (is (= {:rows [[nil "public.oo_table"]]}
           (query c "SELECT to_regclass('oo_table'), 'public.oo_table'::regclass")))))

(deftest native-inferred-relations-remain-addressable
  (d/transact *conn* [{:db/ident :native_item/id :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                      {:db/ident :Oo.Dot/v :db/valueType :db.type/long
                       :db/cardinality :db.cardinality/one}])
  (with-open [c (connect false)]
    (is (= {:rows [["native_item" "t"]]}
           (query c "SELECT relname, oid = 'native_item'::regclass::oid FROM pg_class WHERE relname = 'native_item'")))
    (is (= {:rows [["t"]]}
           (query c "SELECT oid = 'native_item_pkey'::regclass::oid FROM pg_class WHERE relname = 'native_item_pkey'")))
    (is (= {:rows [["\"Oo.Dot\""]]}
           (query c "SELECT 'public.\"Oo.Dot\"'::regclass")))))

(deftest runtime-lookups-observe-ddl-while-stored-oids-keep-their-identity
  (with-open [c (connect false)]
    (execute! c "CREATE TABLE oo_live (v integer)")
    (let [old (-> (query c "SELECT 'oo_live'::regclass::oid") :rows ffirst)]
      (execute! c "CREATE TABLE oo_refs (r regclass, n regnamespace)")
      (execute! c "INSERT INTO oo_refs VALUES ('oo_live', 'public')")
      (with-open [st (.prepareStatement c "SELECT to_regclass(?)::oid")
                  literal (.prepareStatement c "SELECT 'oo_live'::regclass::oid")]
        (.setString st 1 "oo_live")
        (let [read-rows #(with-open [rs (.executeQuery st)] (result-rows rs))]
          (dotimes [_ 6] (is (= [[old]] (read-rows))))
          (dotimes [_ 6]
            (with-open [rs (.executeQuery literal)]
              (is (= [[old]] (result-rows rs)))))
          (is (= {:sqlstate "0A000"}
                 (query c "ALTER TABLE oo_live RENAME TO oo_renamed")))
          (execute! c "DROP TABLE oo_live")
          (is (= [[nil]] (read-rows)))
          (is (= "42P01" (try (with-open [rs (.executeQuery literal)] (result-rows rs))
                              (catch SQLException e (.getSQLState e)))))
          (is (= {:rows [[old "public"]]} (query c "SELECT r,n FROM oo_refs")))
          (execute! c "CREATE TABLE oo_live (v integer)")
          (is (not= [[old]] (read-rows)))
          (with-open [rs (.executeQuery literal)]
            (is (not= [[old]] (result-rows rs))))
          (.setAutoCommit c false)
          (execute! c "DROP TABLE oo_live")
          (is (= [[nil]] (read-rows)))
          (.rollback c)
          (is (some? (ffirst (read-rows))))
          (.setAutoCommit c true))))))

(deftest alias-arrays-copy-and-typed-rendering
  (with-open [c (connect false)]
    (execute! c "CREATE TABLE oo_copy (r regclass, n regnamespace, a regclass[])")
    (let [copy (CopyManager. ^BaseConnection (.unwrap c BaseConnection))]
      (is (= 1 (.copyIn copy "COPY oo_copy FROM STDIN"
                        (StringReader. "pg_class\tpublic\t{pg_class,-}")))))
    (is (= {:rows [["pg_class" "public" "{pg_class,-}"]]}
           (query c "SELECT r,n,a FROM oo_copy")))
    (is (= {:rows [["pg_class" "\"public\"" "'pg_class'"]]}
           (query c "SELECT x, to_json('public'::regnamespace), quote_literal('pg_class'::regclass) FROM unnest(ARRAY['pg_class']::regclass[]) s(x)")))
    (is (= {:rows [["{public,pg_catalog}"]]}
           (query c "SELECT '{public,pg_catalog}'::regnamespace[]")))))

(deftest temporary-identities-have-an-explicit-boundary
  (with-open [a (connect false) b (connect false)]
    (execute! a "CREATE TABLE oo_temp (v integer)")
    (execute! a "CREATE TEMP TABLE oo_temp (v integer)")
    (is (= {:sqlstate "0A000"} (query a "SELECT 'oo_temp'::regclass")))
    (is (= {:sqlstate "0A000"} (query a "SELECT to_regclass('pg_temp.oo_temp')")))
    (is (= {:rows [["oo_temp"]]} (query b "SELECT 'oo_temp'::regclass")))
    (is (= {:rows [["public.oo_temp"]]} (query a "SELECT 'public.oo_temp'::regclass")))
    (is (= {:rows [[nil]]} (query a "SELECT to_regnamespace('pg_temp')")))))

(deftest stored-identities-render-current-registry-names
  (with-open [c (connect false)]
    (execute! c "CREATE TABLE oo_before (v integer)")
    (let [oid (Long/parseLong (-> (query c "SELECT 'oo_before'::regclass::oid") :rows ffirst))]
      (d/transact *conn* (objects/rename-object-tx @*conn* objects/pg-class-oid oid "oo_after" nil))
      (is (= {:rows [["oo_after"]]}
             (query c (str "SELECT " oid "::oid::regclass"))))
      (is (= {:rows [[nil "oo_after"]]}
             (query c "SELECT to_regclass('oo_before'), 'oo_after'::regclass")))
      (is (= {:rows [["oo_after"]]}
             (query c (str "SELECT relname FROM pg_class WHERE oid = " oid)))))))
