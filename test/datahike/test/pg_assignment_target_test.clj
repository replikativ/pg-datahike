(ns datahike.test.pg-assignment-target-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.pg.sql.assignment-target :as target])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.expression NullValue RangeExpression ArrayConstructor]
           [net.sf.jsqlparser.schema Column]))

(defn- update-target [sql]
  (-> (CCJSqlParserUtil/parse sql) .getUpdateSets first .getColumns first))

(deftest targets-retain-structure-and-expression-identities
  (let [column (update-target "UPDATE t SET a[?]=?")
        descriptor (target/describe column)]
    (is (= "a" (:column descriptor)))
    (is (= :array-element (:target-kind descriptor)))
    (is (= 23 (target/value-oid descriptor 1007)))
    (is (= 1 (-> descriptor :dimensions first :index .getIndex))))
  (let [descriptor (target/describe (update-target "UPDATE t SET a[?:?]=?"))]
    (is (= :array-slice (:target-kind descriptor)))
    (is (= 1007 (target/value-oid descriptor 1007)))
    (is (= [2 1] (mapv #(-> % :expression .getIndex)
                       (target/bound-expressions descriptor))))))

(deftest omitted-bounds-and-explicit-null-remain-distinct
  (let [column (doto (Column. "a")
                 (.setArrayConstructor
                  (ArrayConstructor. (into-array net.sf.jsqlparser.expression.Expression
                                                 [(RangeExpression. nil (NullValue.))]))))
        descriptor (target/describe column)
        dimension (first (:dimensions descriptor))]
    (is (not (contains? dimension :lower)))
    (is (instance? NullValue (:upper dimension)))
    (is (= [[:dimensions 0 :upper]]
           (mapv :path (target/bound-expressions descriptor))))))

(deftest only-partial-targets-may-repeat-a-base-column
  (let [partial (target/describe (update-target "UPDATE t SET a[2]=3"))
        whole (target/describe (update-target "UPDATE t SET a=3"))]
    (is (= [partial partial] (target/validate-duplicates! [partial partial] "42601")))
    (doseq [state ["42601" "42701"]]
      (is (= state (try (target/validate-duplicates! [partial whole] state)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:sqlstate (ex-data e)))))))))

(deftest signed-json-shaped-slices-apply-the-sign-only-to-the-lower-bound
  (let [target (target/describe (update-target "UPDATE t SET a[-1:2]=ARRAY[1,2,3,4]"))
        dimension (first (:dimensions target))]
    (is (= :slice (:kind dimension)))
    (is (= "-1" (str (:lower dimension))))
    (is (= "2" (str (:upper dimension))))))
