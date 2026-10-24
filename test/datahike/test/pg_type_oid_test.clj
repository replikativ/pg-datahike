(ns datahike.test.pg-type-oid-test
  "Type OID aliases checked against PostgreSQL protocol text and SQLSTATEs."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.io StringReader]
           [java.sql Connection DriverManager SQLException]
           [org.postgresql.core BaseConnection]
           [org.postgresql.copy CopyManager]
           [org.postgresql.util PGobject]))

(def ^:dynamic *port* nil)

(use-fixtures :each
  (fn [f]
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
               :schema-flexibility :write :keep-history? false :max-string-length 0}]
      (d/create-database cfg)
      (let [conn (d/connect cfg)
            srv (pg/start-server {"types" conn} {:port 0 :host "127.0.0.1"})]
        (try (binding [*port* (.getPort (:server srv))] (f))
             (finally (pg/stop-server srv) (d/release conn) (d/delete-database cfg)))))))

(defn- connect ^Connection [simple?]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/types?user=x&password=x&sslmode=disable&binaryTransfer=false"
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

(deftest captured-postgresql-type-alias-corpus
  (let [fixture (edn/read-string (slurp "test/resources/type_oid_corpus.edn"))
        cases (:cases fixture)]
    (is (= "REL_17_7" (:source-ref fixture)))
    (is (seq cases) "an empty corpus must not pass")
    (doseq [simple? [true false]]
      (with-open [c (connect simple?)]
        (doseq [[sql expected] cases]
          (is (= expected (query c sql)) (str (if simple? "simple " "extended ") sql)))))))

(deftest persistent-user-types-use-projected-identities
  (with-open [c (connect false)]
    (doseq [sql ["CREATE TYPE to_enum AS ENUM ('a')"
                 "CREATE DOMAIN to_domain AS integer"
                 "CREATE TYPE to_composite AS (v integer)"
                 "CREATE TABLE to_row (v integer)"
                 "CREATE VIEW to_view AS SELECT v FROM to_row"
                 "CREATE MATERIALIZED VIEW to_mat AS SELECT v FROM to_row"
                 "CREATE TYPE \"To.Dot\" AS ENUM ('a')"]]
      (execute! c sql)))
  (doseq [simple? [true false]]
    (with-open [c (connect simple?)]
      (doseq [name ["to_enum" "to_domain" "to_composite" "to_row" "to_view" "to_mat"]]
        (is (= {:rows [[name "t" name "public"]]}
               (query c (str "SELECT t.typname, t.oid = '" name "'::regtype::oid, '" name
                             "'::regtype::text, n.nspname FROM pg_type t JOIN pg_namespace n ON n.oid = t.typnamespace WHERE t.oid = '"
                             name "'::regtype::oid"))) name))
      (is (= {:rows [["\"To.Dot\""]]}
             (query c "SELECT 'public.\"To.Dot\"'::regtype")))
      (is (= {:rows [["to_composite" "t"] ["to_mat" "t"] ["to_row" "t"] ["to_view" "t"]]}
             (query c "SELECT t.typname, t.typrelid = c.oid FROM pg_type t JOIN pg_class c ON c.reltype = t.oid WHERE t.typname IN ('to_row','to_view','to_mat','to_composite') ORDER BY t.typname")))
      (execute! c "SET search_path TO pg_catalog")
      (is (= {:rows [[nil "public.to_enum" "public.to_domain" "public.\"To.Dot\""]]}
             (query c "SELECT to_regtype('to_enum'), 'public.to_enum'::regtype, 'public.to_domain'::regtype, 'public.\"To.Dot\"'::regtype"))))))

(deftest type-search-path-preserves-sql-keyword-and-quoted-name-rules
  (with-open [c (connect false)]
    (execute! c "CREATE TYPE int4 AS ENUM ('a')")
    (execute! c "CREATE TYPE \"integer\" AS ENUM ('a')"))
  (doseq [simple? [true false]]
    (with-open [c (connect simple?)]
      (execute! c "SET search_path TO public, pg_catalog")
      (is (= {:rows [["t" "t" "t" "integer" "int4" "\"integer\""]]}
             (query c "SELECT 'int4'::regtype::oid = 'public.int4'::regtype::oid, 'integer'::regtype::oid = 23, '\"integer\"'::regtype::oid = 'public.\"integer\"'::regtype::oid, 23::regtype, 'public.int4'::regtype, 'public.\"integer\"'::regtype")))
      (execute! c "SET search_path TO pg_catalog, public")
      (is (= {:rows [["t" "public.int4" "\"integer\""]]}
             (query c "SELECT 'int4'::regtype::oid = 23, 'public.int4'::regtype, 'public.\"integer\"'::regtype"))))))

(deftest runtime-lookups-observe-type-ddl-and-stored-oids-retain-identity
  (with-open [c (connect false)]
    (execute! c "CREATE TYPE to_live AS ENUM ('a')")
    (execute! c "CREATE TABLE to_refs (t regtype)")
    (execute! c "INSERT INTO to_refs VALUES ('to_live')")
    (let [old (get-in (query c "SELECT 'to_live'::regtype::oid") [:rows 0 0])]
      (with-open [st (.prepareStatement c "SELECT to_regtype(?)::oid")]
        (let [lookup (fn [name]
                       (.setString st 1 name)
                       (with-open [rs (.executeQuery st)] (result-rows rs)))]
          (dotimes [_ 6] (is (= [[old]] (lookup "to_live"))))
          (execute! c "ALTER TYPE to_live RENAME TO to_renamed")
          (is (= [[nil]] (lookup "to_live")))
          (is (= [[old]] (lookup "to_renamed")))
          (is (= {:rows [[old "to_renamed"]]}
                 (query c "SELECT t::oid, t FROM to_refs")))
          (execute! c "DROP TYPE to_renamed")
          (is (= [[nil]] (lookup "to_renamed")))
          (is (= {:rows [[old old]]} (query c "SELECT t::oid, t FROM to_refs")))
          (execute! c "CREATE TYPE to_live AS ENUM ('b')")
          (let [new (ffirst (lookup "to_live"))]
            (is (some? new))
            (is (not= old new))
            (is (= {:rows [[old old]]} (query c "SELECT t::oid, t FROM to_refs")))
            (.setAutoCommit c false)
            (execute! c "DROP TYPE to_live")
            (is (= [[nil]] (lookup "to_live")))
            (.rollback c)
            (is (= [[new]] (lookup "to_live")))
            (execute! c "SAVEPOINT type_before_drop")
            (execute! c "DROP TYPE to_live")
            (is (= [[nil]] (lookup "to_live")))
            (execute! c "ROLLBACK TO SAVEPOINT type_before_drop")
            (is (= [[new]] (lookup "to_live")))
            (.rollback c)
            (.setAutoCommit c true)))))))

(deftest typed-parameters-and-type-alias-arrays
  (with-open [c (connect false)]
    (with-open [st (.prepareStatement c "SELECT ?")]
      (let [v (doto (PGobject.) (.setType "regtype") (.setValue "int4"))]
        (.setObject st 1 v)
        (with-open [rs (.executeQuery st)]
          (is (= "regtype" (.getColumnTypeName (.getMetaData rs) 1)))
          (is (= [["integer"]] (result-rows rs))))))
    (with-open [st (.prepareStatement c "SELECT ?")]
      (let [a (.createArrayOf c "regtype" (into-array Object ["int4" "text"]))]
        (try (.setArray st 1 a)
             (with-open [rs (.executeQuery st)]
               (is (= [["{integer,text}"]] (result-rows rs))))
             (finally (.free a)))))
    (execute! c "CREATE TABLE to_arrays (t regtype, a regtype[])")
    (execute! c "INSERT INTO to_arrays VALUES ('varchar', ARRAY['int4','text']::regtype[])")
    (is (= {:rows [["character varying" "{integer,text}"]]}
           (query c "SELECT t, a FROM (SELECT t,a FROM to_arrays) s")))
    (is (= {:rows [["1043" "2206"]]}
           (query c "SELECT t::oid, pg_typeof(t)::oid FROM to_arrays")))))

(deftest copy-input-resolves-types-in-the-statement-context
  (with-open [c (connect false)]
    (execute! c "CREATE TYPE to_copy_enum AS ENUM ('a')")
    (execute! c "CREATE TABLE to_copy (t regtype, a regtype[])")
    (let [copy (CopyManager. ^BaseConnection (.unwrap c BaseConnection))]
      (is (= 1 (.copyIn copy "COPY to_copy FROM STDIN"
                        (StringReader. "to_copy_enum\t{int4,to_copy_enum,-}"))))
      (is (= {:rows [["to_copy_enum" "{integer,to_copy_enum,-}"]]}
             (query c "SELECT t,a FROM to_copy")))
      (execute! c "SET search_path TO pg_catalog")
      (is (= "42704"
             (try (.copyIn copy "COPY public.to_copy FROM STDIN"
                           (StringReader. "to_copy_enum\t{int4,to_copy_enum,-}"))
                  nil (catch SQLException e (.getSQLState e)))))
      (is (= {:rows [["1"]]} (query c "SELECT count(*) FROM public.to_copy")))
      (is (= 1 (.copyIn copy "COPY public.to_copy FROM STDIN"
                        (StringReader. "public.to_copy_enum\t{int4,public.to_copy_enum,-}"))))
      (is (= {:rows [["public.to_copy_enum" "{integer,public.to_copy_enum,-}"]
                     ["public.to_copy_enum" "{integer,public.to_copy_enum,-}"]]}
             (query c "SELECT t,a FROM public.to_copy"))))))

(deftest temporary-row-types-have-an-explicit-alias-boundary
  (with-open [a (connect false) b (connect false)]
    (execute! a "CREATE TABLE to_temp (v integer)")
    (execute! a "CREATE TEMP TABLE to_temp (v integer)")
    (is (= {:sqlstate "0A000"} (query a "SELECT 'to_temp'::regtype")))
    (is (= {:sqlstate "0A000"} (query a "SELECT to_regtype('pg_temp.to_temp')")))
    (is (= {:rows [["to_temp"]]} (query b "SELECT 'to_temp'::regtype")))
    (is (= {:rows [["public.to_temp"]]} (query a "SELECT 'public.to_temp'::regtype")))))

(deftest pg-typeof-preserves-effectful-arguments-and-wire-types
  (doseq [simple? [true false]]
    (with-open [c (connect simple?) observer (connect true)]
      (let [suffix (if simple? "simple" "extended")
            sequence-name (str "to_effect_" suffix)
            table-name (str "to_effect_rows_" suffix)
            value-expr (str "pg_typeof(nextval('" sequence-name "'))")
            shaped-query (fn [sql]
                           (with-open [st (.prepareStatement c sql)
                                       rs (.executeQuery st)]
                             (let [md (.getMetaData rs)]
                               {:types (mapv #(.getColumnTypeName md (int %))
                                             (range 1 (inc (.getColumnCount md))))
                                :rows (result-rows rs)})))
            assert-sequence (fn [expected]
                              (is (= {:rows [[(str expected) "t"]]}
                                     (query observer (str "SELECT last_value, is_called FROM " sequence-name)))))]
        (execute! c (str "CREATE SEQUENCE " sequence-name))
        (execute! c (str "CREATE TABLE " table-name " (v integer)"))
        (execute! c (str "INSERT INTO " table-name " VALUES (1), (2)"))
        (is (= {:types ["regtype"] :rows [["bigint"]]}
               (shaped-query (str "SELECT " value-expr))))
        (assert-sequence 1)
        (is (= {:types ["oid"] :rows [["20"]]}
               (shaped-query (str "SELECT " value-expr "::oid"))))
        (assert-sequence 2)
        (is (= {:types ["regtype" "int4"] :rows [["bigint" "1"]]}
               (shaped-query (str "SELECT " value-expr ", 1"))))
        (assert-sequence 3)
        (is (= {:types ["regtype"] :rows [["bigint"]]}
               (shaped-query (str "SELECT DISTINCT " value-expr))))
        (assert-sequence 4)
        (is (= {:types ["regtype"] :rows [["bigint"]]}
               (shaped-query (str "SELECT " value-expr " ORDER BY 1"))))
        (assert-sequence 5)
        (is (= {:types ["regtype"] :rows [["bigint"]]}
               (shaped-query (str "SELECT " value-expr " FROM " table-name " LIMIT 1"))))
        (assert-sequence 6)
        (is (= {:types ["regtype"] :rows []}
               (shaped-query (str "SELECT " value-expr " FROM " table-name " LIMIT 0"))))
        (assert-sequence 6)
        (is (= {:types ["regtype" "int4"] :rows [["bigint" "1"] ["bigint" "2"]]}
               (shaped-query (str "SELECT " value-expr ", v FROM " table-name " ORDER BY v LIMIT 2"))))
        (assert-sequence 8)
        (is (= {:types ["regtype"] :rows [["bigint"]]}
               (shaped-query (str "SELECT DISTINCT " value-expr " FROM " table-name))))
        (assert-sequence 10)))))

(deftest boolean-parameters-in-type-introspection-predicates
  (doseq [simple? [true false]]
    (with-open [c (connect simple?)]
      (doseq [sql ["SELECT 1 WHERE ?" "SELECT 1 WHERE false OR ? AND true"]]
        (with-open [st (.prepareStatement c sql)]
          (.setBoolean st 1 true)
          (with-open [rs (.executeQuery st)] (is (= [["1"]] (result-rows rs))))
          (.setBoolean st 1 false)
          (with-open [rs (.executeQuery st)] (is (= [] (result-rows rs))))
          (.setNull st 1 java.sql.Types/BOOLEAN)
          (with-open [rs (.executeQuery st)] (is (= [] (result-rows rs))))))
      (with-open [st (.prepareStatement c "SELECT 1 WHERE NOT ?")]
        (.setBoolean st 1 false)
        (with-open [rs (.executeQuery st)] (is (= [["1"]] (result-rows rs))))
        (.setBoolean st 1 true)
        (with-open [rs (.executeQuery st)] (is (= [] (result-rows rs))))
        (.setNull st 1 java.sql.Types/BOOLEAN)
        (with-open [rs (.executeQuery st)] (is (= [] (result-rows rs))))))))

(deftest nonboolean-parameters-are-rejected-in-predicates
  (with-open [c (connect false)]
    (doseq [sql ["SELECT 1 WHERE ?" "SELECT 1 WHERE NOT ?"]]
      (with-open [st (.prepareStatement c sql)]
        (.setInt st 1 1)
        (is (= "42804" (try (.executeQuery st) nil
                            (catch SQLException e (.getSQLState e)))))))))

(deftest deferred-projection-preserves-unknown-set-operation-types
  (doseq [simple? [true false]]
    (with-open [c (connect simple?)]
      (doseq [[literal expected] [["NULL" [["2" nil] ["2" "1"]]]
                                  ["'1'" [["2" "1"] ["2" "1"]]]]]
        (with-open [st (.prepareStatement c (str "SELECT max(v) + min(v), " literal
                                                 " FROM (VALUES (1)) AS r(v) UNION ALL SELECT 2, 1"))
                    rs (.executeQuery st)]
          (is (= ["int4" "int4"]
                 (mapv #(.getColumnTypeName (.getMetaData rs) (int %)) [1 2])))
          (is (= expected (result-rows rs))))))))

(deftest deferred-and-correlated-projections-share-wire-types
  (with-open [c (connect false)]
    (execute! c "CREATE SEQUENCE to_corr_sequence")
    (execute! c "CREATE TABLE to_corr_rows (v integer)")
    (execute! c "INSERT INTO to_corr_rows VALUES (1), (2)"))
  (doseq [simple? [true false]]
    (with-open [c (connect simple?)
                st (.prepareStatement c
                                      "SELECT pg_typeof(nextval('to_corr_sequence')), (SELECT i.v FROM to_corr_rows i WHERE i.v = o.v), o.v FROM to_corr_rows o")
                rs (.executeQuery st)]
      (is (= ["regtype" "int4" "int4"]
             (mapv #(.getColumnTypeName (.getMetaData rs) (int %)) [1 2 3])))
      (is (= [["bigint" "1" "1"] ["bigint" "2" "2"]] (sort-by second (result-rows rs)))))))
