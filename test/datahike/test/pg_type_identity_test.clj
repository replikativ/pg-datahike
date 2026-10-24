(ns datahike.test.pg-type-identity-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.pg.catalog.regoid :as regoid]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.catalog :as catalog])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- with-catalog [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f conn (pg/make-query-handler conn {}))
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- rows [^PgWireServer$QueryHandler h ^String sql]
  (let [^PgWireServer$QueryResult r (.execute h sql)]
    (is (nil? (.error r)) sql)
    (mapv vec (.rows r))))

(def ^:private handler-columns
  [:pg_type/typinput :pg_type/typoutput :pg_type/typreceive :pg_type/typsend
   :pg_type/typmodin :pg_type/typmodout :pg_type/typanalyze :pg_type/typsubscript])

(deftest every-type-handler-has-a-numeric-procedure-identity
  (with-catalog
    (fn [conn h]
      (doseq [sql ["CREATE TYPE ti_enum AS ENUM ('a')"
                   "CREATE TYPE ti_composite AS (v integer)"
                   "CREATE TABLE ti_row (v integer)"
                   "CREATE DOMAIN ti_domain AS integer"
                   "CREATE DOMAIN ti_enum_domain AS ti_enum"
                   "CREATE DOMAIN ti_nested AS ti_enum_domain"
                   "CREATE DOMAIN ti_composite_domain AS ti_composite"]]
        (rows h sql))
      (let [db @conn
            types (catalog/catalog-data-for* "pg_type" {} db)
            procedures (group-by :oid (regoid/routines db))
            bad (for [t types k handler-columns
                      :let [oid (get t k)]
                      :when (or (not (integer? oid))
                                (and (not= 0 oid) (not= 1 (count (get procedures oid)))))]
                  [(:pg_type/typname t) k oid])]
        (is (empty? bad) (pr-str bad))
        (is (every? #(= (if (= "vector" (:pg_type/typname %)) 2200 11)
                        (:pg_type/typnamespace %))
                    (remove #(contains? #{"ti_enum" "ti_composite" "ti_row" "ti_domain"
                                          "ti_enum_domain" "ti_nested" "ti_composite_domain"}
                                        (:pg_type/typname %)) types))))
      (is (= [["ti_composite" "record_in" "public"]
              ["ti_domain" "domain_in" "public"]
              ["ti_enum" "enum_in" "public"]
              ["ti_row" "record_in" "public"]]
             (rows h "SELECT t.typname, p.proname, n.nspname FROM pg_type t JOIN pg_proc p ON p.oid = t.typinput JOIN pg_namespace n ON n.oid = t.typnamespace WHERE t.typname IN ('ti_composite','ti_domain','ti_enum','ti_row') ORDER BY t.typname")))
      (is (= [["t" "t" "-"]]
             (rows h "SELECT t.typoutput = b.typoutput, t.typsend = b.typsend, t.typmodin FROM pg_type t JOIN pg_type b ON b.oid = t.typbasetype WHERE t.typname = 'ti_domain'")))
      (is (= [["ti_composite_domain" "record_out" "record_send"]
              ["ti_enum_domain" "enum_out" "enum_send"]
              ["ti_nested" "enum_out" "enum_send"]]
             (rows h "SELECT t.typname, o.proname, s.proname FROM pg_type t JOIN pg_proc o ON o.oid = t.typoutput JOIN pg_proc s ON s.oid = t.typsend WHERE t.typname IN ('ti_composite_domain','ti_enum_domain','ti_nested') ORDER BY t.typname"))))))

(deftest type-handlers-support-client-oid-comparisons
  (with-catalog
    (fn [_ h]
      ;; pgjdbc TypeInfoCache identifies array types by this comparison.
      (is (= [["_int4" "t" "pg_catalog"]]
             (rows h "SELECT t.typname, t.typinput::oid = p.oid, n.nspname FROM pg_type t JOIN pg_proc p ON t.typinput = p.oid JOIN pg_namespace n ON n.oid = t.typnamespace WHERE t.typname = '_int4' AND t.typinput = 'pg_catalog.array_in'::regproc")))
      (is (= [["t" "-"]]
             (rows h "SELECT typmodin = 0, typmodin FROM pg_type WHERE typname = 'int4'")))
      (rows h "CREATE FUNCTION array_in(v integer) RETURNS integer LANGUAGE sql AS 'SELECT v'")
      (rows h "SET search_path TO public, pg_catalog")
      (is (= [["t"]]
             (rows h "SELECT typinput::oid = 'pg_catalog.array_in'::regproc::oid FROM pg_type WHERE typname = '_int4'"))))))

(deftest vector-handler-signatures-match-the-extension
  (let [procs (into {} (map (juxt :proname identity)) (regoid/routines nil))]
    (is (= {"vector_in" [[2275 26 23] 16383]
            "vector_out" [[16383] 2275]
            "vector_typmod_in" [[1263] 23]
            "vector_recv" [[2281 26 23] 16383]
            "vector_send" [[16383] 17]}
           (into {} (for [name ["vector_in" "vector_out" "vector_typmod_in" "vector_recv" "vector_send"]
                          :let [p (get procs name)]]
                      [name [(:args p) (:ret p)]]))))
    (is (= (long (Math/abs (.hashCode "vector_dims"))) (:oid (get procs "vector_dims"))))))
