(ns datahike.test.pg-explain-producer-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.analysis :as analysis]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]))

(def ^:dynamic *db* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (let [db (d/db conn)]
          (binding [*db* db params/*parse-db* db
                    params/*analysis-only?* true params/*parse-sql* sql/parse-sql]
            (f)))
        (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- values-columns [source]
  (stmt/analyze-values-output (CCJSqlParserUtil/parse source) (:schema *db*) *db*))

(defn- shape [source]
  (let [tf (.getFromItem (CCJSqlParserUtil/parse (str "SELECT * FROM " source)))]
    (stmt/materialize-table-function
     tf (fn [_] (throw (AssertionError. "EXPLAIN invoked an SRF argument evaluator")))
     nil {:schema (:schema *db*) :db *db*})))

(defn- state [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:sqlstate (ex-data e)))))

(deftest values-infer-all-row-types-without-evaluating-cells
  (with-redefs [stmt/eval-values-literal
                (fn [_] (throw (AssertionError. "EXPLAIN evaluated a VALUES cell")))]
    (is (= [{:name "column1" :oid 1700} {:name "column2" :oid 25}]
           (values-columns "VALUES (1,NULL),(2.5,'x')"))))
  (is (= [1007] (mapv :oid (values-columns "VALUES (ARRAY[1]),(ARRAY[2])"))))
  (is (= "42804" (state #(values-columns "VALUES (1),(true)"))))
  (is (= "42601" (state #(values-columns "VALUES (1),(1,2)"))))
  (is (= "42703" (state #(values-columns "VALUES (missing_column)")))))

(deftest values-preserve-only-common-type-modifiers
  (is (= [7] (mapv :typmod (values-columns "VALUES (('a'::varchar(3))),('b'::varchar(3))"))))
  (is (= [7] (mapv :typmod (values-columns "VALUES ('a'::varchar(3)),('b'::varchar(3))"))))
  (is (= [nil] (mapv :typmod (values-columns "VALUES ('a'::varchar(3)),('b'::varchar(5))"))))
  (is (= [nil] (mapv :typmod (values-columns "VALUES ('a'::varchar(3)),('b'::text)")))))

(deftest series-result-types-use-declared-overloads-without-generating-rows
  (is (= {:oids [20] :pg-types ["int8"] :rows []}
         (select-keys (shape "generate_series(1::bigint,2)") [:oids :pg-types :rows])))
  (is (= [1700] (:oids (shape "generate_series(1,2,0.5)"))))
  (is (= [1114] (:oids (shape "generate_series(timestamp '2020-01-01',timestamp '2020-01-02',interval '1 day')"))))
  (is (= "42725" (state #(shape "generate_series('1','2')"))))
  (is (= "42725" (state #(shape "generate_series(1::smallint,2::smallint)"))))
  (is (= "42725" (state #(shape "generate_series(1::smallint,2::smallint,1::smallint)"))))
  (is (= "42883" (state #(shape "generate_series(1::float8,2::float8)")))))

(deftest from-functions-expose-columns-and-ordinality-without-running
  (is (= {:oids [23 20] :rows [] :aliases ["unnest" "ordinality"]}
         (select-keys (shape "unnest(ARRAY[1,2]) WITH ORDINALITY") [:oids :rows :aliases])))
  (is (= [25 3802] (:oids (shape "jsonb_each('{}')")))))

(deftest known-invalid-or-unresolved-function-shapes-fail-before-evaluation
  (is (= "42883" (state #(shape "jsonb_each(1)"))))
  (is (= "42883" (state #(shape "pg_get_keywords(1)"))))
  (is (= "42725" (state #(shape "unnest(NULL)"))))
  (is (= "42883" (state #(shape "unnest(1)")))))
