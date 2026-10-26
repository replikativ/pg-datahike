(ns datahike.test.pg-array-assign-kernel-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.arrays :as arrays]
            [datahike.pg.arrays.assign :as assignment]))

(defn- array [values] (arrays/array :int4 values))
(defn- index-path [& values] (mapv #(hash-map :kind :element :index %) values))
(defn- slice-path [& bounds]
  (mapv (fn [[lower upper]] {:kind :slice :lower lower :upper upper}) bounds))
(defn- assign-text [base path rhs]
  (arrays/to-pg-text (assignment/assign base :int4 path rhs)))
(defn- state [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:sqlstate (ex-data e)))))

(deftest element-assignment-preserves-complete-values
  (doseq [[expected base path rhs]
          [["{1,22,NULL,4}" (array [1 2 nil 4]) (index-path 2) 22]
           ["{1,2,NULL,4,NULL,66}" (array [1 2 nil 4]) (index-path 6) 66]
           ["[-3:4]={9,NULL,NULL,NULL,1,2,NULL,4}" (array [1 2 nil 4]) (index-path -3) 9]
           ["{1,NULL,NULL,4}" (array [1 2 nil 4]) (index-path 2) :__null__]
           ["[2:2]={22}" nil (index-path 2) 22]
           ["[-4:-4]={5}" nil (index-path -4) 5]
           ["[2:2]={NULL}" nil (index-path 2) nil]
           ["[0:0]={7}" (array []) (index-path 0) 7]
           ["{{1,22},{3,4}}" (array [[1 2] [3 4]]) (index-path 1 2) 22]
           ["[2:2][3:3]={{9}}" nil (index-path 2 3) 9]]]
    (is (= expected (assign-text base path rhs)) [path rhs])))

(deftest slice-assignment-preserves-unaffected-values
  (doseq [[expected base path rhs]
          [["{1,8,9,4}" (array [1 2 3 4]) (slice-path [2 3]) (array [8 9])]
           ["{1,8,9,4}" (array [1 2 3 4]) (slice-path [2 3]) (array [8 9 10])]
           ["{1,8,9,4}" (array [1 2 3 4]) (slice-path [2 3]) (array [[8 9] [10 11]])]
           ["{1,8,9,4}" (array [1 2 3 4]) (slice-path [2 3])
            (arrays/array :int4 [8 9] [2] [-4])]
           ["[-2:4]={8,9,NULL,1,2,3,4}" (array [1 2 3 4]) (slice-path [-2 -1]) (array [8 9])]
           ["{1,2,3,4,NULL,8,9}" (array [1 2 3 4]) (slice-path [6 7]) (array [8 9])]
           ["{8,9,3,4}" (array [1 2 3 4]) [{:kind :slice :upper 2}] (array [8 9])]
           ["{1,2,8,9}" (array [1 2 3 4]) [{:kind :slice :lower 3}] (array [8 9])]
           ["{8,9,10,11}" (array [1 2 3 4]) [{:kind :slice}] (array [8 9 10 11])]
           ["{{8,9},{3,4}}" (array [[1 2] [3 4]]) (slice-path [1 1]) (array [8 9])]
           ["{{1,8},{3,4}}" (array [[1 2] [3 4]])
            [{:kind :element :index 1} {:kind :slice :lower 2 :upper 2}] (array [[8]])]
           ["[2:3][4:5]={{1,2},{3,4}}" nil (slice-path [2 3] [4 5]) (array [1 2 3 4 5])]
           ["[2:3]={8,9}" nil (slice-path [2 3]) (array [8 9])]
           ["{}" nil (slice-path [2 1]) (array [])]
           ["{}" nil (slice-path [3 1]) nil]]]
    (is (= expected (assign-text base path rhs)) [path rhs])))

(deftest array-assignment-native-error-states
  (doseq [[code base path rhs]
          [["2202E" (array [[1 2] [3 4]]) (index-path 1) 9]
           ["2202E" (array [[1 2] [3 4]]) (index-path 1 2 3) 9]
           ["2202E" (array [[1 2] [3 4]]) (index-path 0 1) 9]
           ["2202E" (array [[1 2] [3 4]]) (index-path 1 3) 9]
           ["2202E" (array [[1 2] [3 4]]) (slice-path [0 1]) (array [7 8 9 10])]
           ["2202E" (array [[1 2] [3 4]])
            [{:kind :slice :lower 1 :upper 1} {:kind :element :index 2}] (array [[9]])]
           ["2202E" (array [1 2 3]) (slice-path [1 3]) (array [8 9])]
           ["2202E" (array [1 2]) (slice-path [2 1]) (array [])]
           ["2202E" nil [{:kind :slice}] (array [8 9])]
           ["2202E" (array []) [{:kind :slice :lower 1}] (array [8 9])]
           ["2202E" nil [{:kind :slice :upper 2}] (array [8 9])]
           ["54000" nil (slice-path [3 1]) (array [])]
           ["54000" nil (index-path 2147483647) 1]
           ["54000" nil (apply index-path (repeat 7 1)) 1]
           ["22004" (array [1 2]) (slice-path [nil 1]) nil]
           ["22004" nil [{:kind :slice :upper :__null__}] nil]]]
    (is (= code (state #(assignment/assign base :int4 path rhs))) [path rhs])))

(deftest null-source-skips-mutation-geometry
  (let [base (array [1 2])]
    (is (identical? base (assignment/assign base :int4 (slice-path [3 1]) nil)))
    (is (= "22004" (state #(assignment/check-subscripts! (index-path nil)))))
    (is (= "22003" (state #(assignment/check-subscripts! (index-path 2147483648)))))))

(deftest assignment-does-not-mutate-shared-input
  (let [base (arrays/array :int4 [[1 2] [3 4]] [2 2] [-2 5])
        result (assignment/assign base :int4 (index-path -1 6) 8)]
    (is (= [-2 5] (:lbounds result)))
    (is (= [[1 2] [3 8]] (:elements result)))
    (is (= [[1 2] [3 4]] (:elements base))))
  (let [base (array [1 2 3 4])]
    (assignment/assign base :int4 (slice-path [2 3]) (array [8 9]))
    (is (= [1 2 3 4] (:elements base))))
  (testing "nested PgArray children are read as containers, not scalar records"
    (let [base (arrays/array :int4 [(array [1 2]) (array [3 4])])]
      (is (= "{{1,2},{3,8}}" (assign-text base (index-path 2 2) 8))))))

(deftest pathological-allocation-is-rejected-before-construction
  (let [base (array [1 2 3])]
    (with-redefs-fn {#'assignment/max-structure-slots 64}
      (fn []
        (is (= "54000" (state #(assignment/assign base :int4 (index-path 1000000) 9))))
        (is (= "54000" (state #(assignment/assign nil :int4 (slice-path [1 20] [1 1]) (array (range 20))))))))
    (is (= [1 2 3] (:elements base)))))
