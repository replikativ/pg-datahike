(ns datahike.test.pg-regoid-type-test
  "Routine argument TypeNames: values and SQLSTATEs captured from PostgreSQL
   17.10; grammar and typmod limits checked against REL_17_7 sources."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.catalog.regoid :as regoid]
            [datahike.pg.errors :as errors]))

(def ^:private type-cases
  [["double   precision" 701]
   ["double\nprecision" 701]
   ["numeric(10,2)" 1700]
   ["numeric(nope)" "22P02"]
   ["numeric(1.5)" "22P02"]
   ["numeric(0)" "22023"]
   ["numeric(1001)" "22023"]
   ["numeric(10,-1001)" "22023"]
   ["numeric(1,2,3)" "22023"]
   ["numeric(1 + 1)" "42601"]
   ["numeric('10', '2')" 1700]
   ["numeric(\"nope\")" "22P02"]
   ["integer(3)" "42601"]
   ["integer[3][2]" 1007]
   ["integer ARRAY[3]" 1007]
   ["integer ARRAY" 1007]
   ["integer[-1]" "42601"]
   ["integer[2147483648]" "42601"]
   ["integer ARRAY[]" "42601"]
   ["timestamp(3) with time zone" 1184]
   ["timestamp with time zone(3)" "42601"]
   ["timestamp(-1)" "42601"]
   ["\"timestamp\"(-1)" "22023"]
   ["timestamp(7)" 1114]
   ["varchar(0)" "22023"]
   ["varchar(nope)" "42601"]
   ["\"varchar\"(nope)" "22P02"]
   ["char(3)" 1042]
   ["\"char\"" 18]
   ["\"char\"(3)" "42601"]
   ["\"numeric(nope)\"" "42704"]
   ["float(24)" 700]
   ["float(25)" 701]
   ["float(0)" "22023"]
   ["float(54)" "22023"]
   ["float(-1)" "42601"]
   ["interval day to second(3)" 1186]
   ["interval(3) day" "42601"]
   ["interval(3)" 1186]
   ["\"interval\"(3)" "22023"]
   ["\"interval\"(32767,3)" 1186]
   ["numeric(10,2)[]" 1231]
   ["national character varying(3)" 1043]
   ["char varying(3)" 1043]
   ["numeric(/* x */10,2)" 1700]
   ["SETOF integer" "42601"]
   ["int4[][][][][][][]" 1007]])

(defn- result [f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (first (errors/classify-exception e)))))

(deftest argument-type-input
  (doseq [[spelling expected] type-cases]
    (is (= expected (result #(regoid/type-oid nil spelling))) spelling)))

(deftest signature-type-validation
  (is (= 1397 (regoid/input "regprocedure" "abs(integer)")))
  (is (= 1395 (regoid/input "regprocedure" "abs(double   precision)")))
  (is (= "22P02" (result #(regoid/input "regprocedure" "abs(numeric(nope))"))))
  (testing "type parser/typmod errors remain hard within to_regprocedure"
    (is (= "22P02" (result #(regoid/input-or-nil "regprocedure" "abs(numeric(nope))"))))
    (is (= "22023" (result #(regoid/input-or-nil "regprocedure" "abs(numeric(0))"))))
    (is (= "42601" (result #(regoid/input-or-nil "regprocedure" "abs(integer(3))")))))
  (testing "missing identities and the signature scanner's own errors are soft"
    (is (nil? (regoid/input-or-nil "regprocedure" "abs(no_such_type)")))
    (is (nil? (regoid/input-or-nil "regprocedure" "abs(integer")))))

(deftest extension-identities-remain-visible
  (let [extensions (filter #(= 2200 (:namespace-oid %)) (regoid/routines nil))]
    (is (= #{"vector_dims" "vector_norm" "l2_distance" "vector_l2_squared_distance"
             "l1_distance" "inner_product" "cosine_distance"}
           (set (map :proname extensions))))
    (doseq [{:keys [oid proname]} extensions]
      (is (= oid (regoid/input "regproc" proname)))
      (is (= oid (Math/abs (.hashCode ^String proname)))))))

(deftest type-lexical-boundaries
  (doseq [s ["\"numeric" "\"numeric\"\"" "\"\""
             "numeric('10)" "numeric(E'10\\')" "numeric($t$10)"
             "numeric/*" "numeric/* outer /* inner */"]]
    (is (= "42601" (result #(regoid/type-oid nil s))) s))
  (is (= 1700 (regoid/type-oid nil "numeric/* outer /* inner */ */")))
  (is (= 1700 (regoid/type-oid nil "numeric($t$10$t$)"))))

(deftest scalar-vector-and-unrelated-malformed-routines
  (is (= 16383 (regoid/type-oid nil "vector")))
  (is (= 16383 (regoid/type-oid nil "public.vector(3)")))
  (is (= "42704" (result #(regoid/type-oid nil "pg_catalog.vector"))))
  (is (= "42704" (result #(regoid/type-oid nil "vector[]"))))
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn
                    (into [{:db/ident :datahike.pg.function/oid
                            :db/valueType :db.type/long :db/cardinality :db.cardinality/one}]
                          (for [a [:name :arg-types :return-type]]
                            {:db/ident (keyword "datahike.pg.function" (name a))
                             :db/valueType :db.type/string :db/cardinality :db.cardinality/one})))
        (d/transact conn
                    [{:datahike.pg.function/oid 20001
                      :datahike.pg.function/name "rg_broken"
                      :datahike.pg.function/arg-types "[\"no_such_type\"]"
                      :datahike.pg.function/return-type "{:type \"integer\"}"}
                     {:datahike.pg.function/oid 20002
                      :datahike.pg.function/name "rg_vector"
                      :datahike.pg.function/arg-types "[\"vector\"]"
                      :datahike.pg.function/return-type "{:type \"vector\"}"}])
        (binding [regoid/*db* (d/db conn)]
          (is (= 1299 (regoid/input "regproc" "now")))
          (is (= "999999" (regoid/output 24 999999 str)))
          (is (= 20002 (regoid/input "regproc" "rg_vector")))
          (is (= "rg_vector" (regoid/output 24 20002 str)))
          (is (= "42704" (result #(regoid/input "regproc" "rg_broken"))))
          (is (= "42704" (result #(regoid/routines regoid/*db*)))))
        (finally (d/release conn) (d/delete-database cfg))))))
