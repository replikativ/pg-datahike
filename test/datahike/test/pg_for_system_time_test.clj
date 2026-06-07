(ns datahike.test.pg-for-system-time-test
  "Tests for SQL:2011 `FOR SYSTEM_TIME …` SELECT-side preprocessor.

   The preprocessor strips `FOR SYSTEM_TIME <spec>` / `FOR ALL
   SYSTEM_TIME` clauses from SELECT SQL and returns a side-channel
   override map the handler threads into `apply-temporal` for THIS
   statement only — equivalent to a per-statement
   `SET datahike.system_at = ...` that auto-resets after the query.

   Only AS OF and ALL are supported on the SYSTEM_TIME axis;
   BETWEEN / FROM-TO are rejected with a clear error (datahike's
   `d/as-of` takes a single time-point).

   The two temporal axes compose: a single statement may carry both
   `FOR SYSTEM_TIME AS OF …` and `FOR VALID_TIME AS OF …` and the
   preprocessor returns both keys on the override map."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.temporal :as temporal])
  (:import [java.util Date]))

;; ===========================================================================
;; SELECT-side FOR SYSTEM_TIME preprocessor
;; ===========================================================================

(deftest preprocess-for-system-time-as-of-extracts-date
  (testing "AS OF clause stripped + override carries the parsed Date"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR SYSTEM_TIME AS OF '2024-04-15T00:00:00Z'")]
      (is (re-find #"^SELECT \* FROM person\s*$" sql))
      (is (not (re-find #"(?i)FOR\s+SYSTEM_TIME" sql)))
      (is (= {:as-of (Date. 1713139200000)} override)))))

(deftest preprocess-for-system-time-as-of-iso-date-only
  (testing "Date-only literal pads to midnight UTC"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR SYSTEM_TIME AS OF '2024-04-15'")]
      (is (not (re-find #"(?i)FOR\s+SYSTEM_TIME" sql)))
      (is (= {:as-of (Date. 1713139200000)} override)))))

(deftest preprocess-for-all-system-time
  (testing "FOR ALL SYSTEM_TIME yields {:as-of :all}"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR ALL SYSTEM_TIME")]
      (is (= {:as-of :all} override))
      (is (not (re-find #"(?i)FOR\s+ALL" sql))))))

(deftest preprocess-for-system-time-all-alias
  (testing "FOR SYSTEM_TIME ALL is an alias for FOR ALL SYSTEM_TIME"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR SYSTEM_TIME ALL")]
      (is (= {:as-of :all} override))
      (is (not (re-find #"(?i)FOR\s+SYSTEM_TIME" sql))))))

(deftest preprocess-system-time-clause-before-where
  (testing "WHERE following the clause terminates the AS OF expression"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR SYSTEM_TIME AS OF '2024-04-15' WHERE age > 18")]
      (is (re-find #"WHERE age > 18" sql))
      (is (not (re-find #"(?i)FOR\s+SYSTEM_TIME" sql)))
      (is (instance? Date (:as-of override))))))

(deftest preprocess-system-time-clause-before-order-by
  (testing "ORDER BY following the clause terminates the AS OF expression"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT name FROM person FOR SYSTEM_TIME AS OF '2024-04-15' ORDER BY name")]
      (is (re-find #"ORDER BY name" sql))
      (is (not (re-find #"(?i)FOR\s+SYSTEM_TIME" sql))))))

(deftest preprocess-rejects-multi-system-time-clauses
  (testing "Two FOR SYSTEM_TIME clauses in one statement throw"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"More than one FOR SYSTEM_TIME clause"
         (temporal/preprocess
          (str "SELECT a.id, b.id FROM a FOR SYSTEM_TIME AS OF '2024-01-01' "
               "JOIN b FOR SYSTEM_TIME AS OF '2024-06-01' ON a.id = b.id"))))))

(deftest preprocess-rejects-for-system-time-between
  (testing "FOR SYSTEM_TIME BETWEEN throws a helpful error"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"FOR SYSTEM_TIME BETWEEN is not yet supported"
         (temporal/preprocess
          "SELECT * FROM person FOR SYSTEM_TIME BETWEEN '2024-01-01' AND '2024-06-01'")))))

(deftest preprocess-rejects-for-system-time-from-to
  (testing "FOR SYSTEM_TIME FROM…TO throws a helpful error"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"FOR SYSTEM_TIME FROM TO is not yet supported"
         (temporal/preprocess
          "SELECT * FROM person FOR SYSTEM_TIME FROM '2024-01-01' TO '2024-06-01'")))))

;; ===========================================================================
;; Multi-axis composition: FOR SYSTEM_TIME + FOR VALID_TIME in one statement
;; ===========================================================================

(deftest preprocess-system-time-and-valid-time-compose
  (testing "Both axes pinned together produces both override keys"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR SYSTEM_TIME AS OF '2024-04-15' FOR VALID_TIME AS OF '2024-06-01'")]
      (is (not (re-find #"(?i)FOR\s+(SYSTEM|VALID)_TIME" sql)))
      (is (= {:as-of (Date. 1713139200000)
              :valid-at (Date. 1717200000000)}
             override)))))

(deftest preprocess-valid-time-and-system-time-compose-reverse-order
  (testing "VALID_TIME first then SYSTEM_TIME — same axis-keys"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR VALID_TIME AS OF '2024-06-01' FOR SYSTEM_TIME AS OF '2024-04-15'")]
      (is (not (re-find #"(?i)FOR\s+(SYSTEM|VALID)_TIME" sql)))
      (is (= {:as-of (Date. 1713139200000)
              :valid-at (Date. 1717200000000)}
             override)))))

(deftest preprocess-system-time-all-with-valid-time-as-of
  (testing "Clear SYSTEM_TIME pin + set VALID_TIME for this stmt"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person FOR ALL SYSTEM_TIME FOR VALID_TIME AS OF '2024-06-01'")]
      (is (not (re-find #"(?i)FOR\s+(SYSTEM|VALID|ALL)" sql)))
      (is (= {:as-of :all
              :valid-at (Date. 1717200000000)}
             override)))))

;; ===========================================================================
;; Robustness against false matches
;; ===========================================================================

(deftest preprocess-ignores-system-time-inside-string-literal
  (testing "Quoted phrase containing FOR SYSTEM_TIME is not a clause"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person WHERE note = 'pinned FOR SYSTEM_TIME AS OF check'")]
      (is (= "SELECT * FROM person WHERE note = 'pinned FOR SYSTEM_TIME AS OF check'" sql))
      (is (nil? override)))))

(deftest preprocess-handles-mixed-case-system-time
  (testing "Mixed-case `For System_Time` keyword recognised"
    (let [{:keys [sql override]}
          (temporal/preprocess
           "SELECT * FROM person For System_Time As Of '2024-04-15'")]
      (is (not (re-find #"(?i)System_Time" sql)))
      (is (= {:as-of (Date. 1713139200000)} override)))))
