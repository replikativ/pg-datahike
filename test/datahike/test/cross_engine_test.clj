(ns datahike.test.cross-engine-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.test.cross-engine :as cross]))

(deftest differential-result-contract
  (testing "matching errors require the same SQLSTATE and structured fields"
    (is (nil? (cross/diff-result
               {:error "reference wording" :sqlstate "23505"
                :diagnostics {:detail "reference detail"}
                :fields {:constraint "items_pkey"}}
               {:error "target wording" :sqlstate "23505"
                :diagnostics {:detail "target detail"}
                :fields {:constraint "items_pkey"}}
               "nosort")))
    (is (= :sqlstate-differ
           (:kind (cross/diff-result
                   {:error "bad" :sqlstate "23505" :fields {}}
                   {:error "bad" :sqlstate "XX000" :fields {}}
                   "nosort"))))
    (is (= :error-fields-differ
           (:kind (cross/diff-result
                   {:error "bad" :sqlstate "23505"
                    :fields {:constraint "items_pkey"}}
                   {:error "bad" :sqlstate "23505" :fields {}}
                   "nosort")))))

  (testing "result metadata is part of compatibility"
    (is (= :metadata-differ
           (:kind (cross/diff-result
                   {:rows [["1"]]
                    :metadata [{:label "n" :type-name "int4" :jdbc-type 4}]}
                   {:rows [["1"]]
                    :metadata [{:label "n" :type-name "int8" :jdbc-type -5}]}
                   "nosort")))))

  (testing "DML update counts and result kinds cannot silently diverge"
    (is (= :update-count-differ
           (:kind (cross/diff-result {:updated 1} {:updated 0} "nosort"))))
    (is (= :result-kind-differ
           (:kind (cross/diff-result
                   {:rows [] :metadata []} {:updated 0} "nosort")))))

  (testing "declared row ordering remains the only row normalization"
    (is (nil? (cross/diff-result
               {:rows [["b"] ["a"]] :metadata []}
               {:rows [["a"] ["b"]] :metadata []}
               "rowsort")))
    (is (= :rows-differ
           (:kind (cross/diff-result
                   {:rows [["b"] ["a"]] :metadata []}
                   {:rows [["a"] ["b"]] :metadata []}
                   "nosort"))))))

(deftest oracle-control-flow-contract
  (testing "a statement divergence increments the failure count"
    (let [result (cross/compare-spec-results
                  {:passed 0 :failed 0 :diffs []}
                  {:type :statement :expect :ok :sql "UPDATE t SET n = 1"}
                  {:updated 1} {:updated 0})]
      (is (= 1 (:failed result)))
      (is (= :update-count-differ (-> result :diffs first :kind)))))

  (testing "the PostgreSQL oracle must satisfy the fixture declaration"
    (let [both-error {:error "same" :sqlstate "42601" :fields {}}
          result (cross/compare-spec-results
                  {:passed 0 :failed 0 :diffs []}
                  {:type :statement :expect :ok :sql "CREATE TABLE t()"}
                  both-error both-error)]
      (is (= 1 (:failed result)))
      (is (= :reference-expectation-mismatch
             (-> result :diffs first :kind)))))

  (testing "declared query rows are checked after SQLLogic formatting"
    (let [base {:type :query :types "IR" :sort "rowsort"
                :sql "SELECT 1, 2.5"}
          matching (cross/compare-spec-results
                    {:passed 0 :failed 0 :diffs []}
                    (assoc base :expected ["1\t2.500000"])
                    {:rows [["1" "2.5"]] :metadata []}
                    {:rows [["1" "2.5"]] :metadata []})
          stale (cross/compare-spec-results
                 {:passed 0 :failed 0 :diffs []}
                 (assoc base :expected ["1\t3.000000"])
                 {:rows [["1" "2.5"]] :metadata []}
                 {:rows [["1" "2.5"]] :metadata []})]
      (is (= 1 (:passed matching)))
      (is (= 1 (:failed stale)))
      (is (= :reference-expectation-mismatch
             (-> stale :diffs first :kind)))))

  (testing "notice order and content are part of successful behavior"
    (is (= :warnings-differ
           (:kind (cross/diff-result
                   {:updated 1 :warnings [{:message "NOTICE: before"}]}
                   {:updated 1 :warnings []}
                   "nosort"))))
    (is (= :warnings-differ
           (:kind (cross/diff-result
                   {:error "boom" :sqlstate "P0001" :fields {}
                    :warnings [{:message "NOTICE: before"}]}
                   {:error "boom" :sqlstate "P0001" :fields {}
                    :warnings []}
                   "nosort"))))))
