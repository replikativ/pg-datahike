(ns datahike.test.pg-aggregate-extras-test
  "Direct tests for ordered-set aggregates (`filter-percentile-cont`,
   `filter-percentile-disc`, `filter-mode`) and the BigDecimal-precision
   variants (`filter-sum-numeric`, `filter-avg-numeric`)."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.sql.fns :as fns]))

;; ---------------------------------------------------------------------------
;; PERCENTILE_CONT — pair-aggregate input [[p val] ...]

(deftest percentile-cont-basic
  (testing "median of 4 evenly-spaced values"
    (is (= 2.5
           (fns/filter-percentile-cont
            [[0.5 1] [0.5 2] [0.5 3] [0.5 4]]))
        "median of [1 2 3 4] = (2+3)/2 = 2.5"))

  (testing "arbitrary percentile via linear interpolation"
    ;; p25 of [1 2 3 4]: idx = 0.25 * (n-1) = 0.75, lo=0, hi=1,
    ;; frac=0.75 → 1*0.25 + 2*0.75 = 1.75
    (is (= 1.75
           (fns/filter-percentile-cont
            [[0.25 1] [0.25 2] [0.25 3] [0.25 4]])))))

(deftest percentile-cont-edge-cases
  (testing "single value → that value"
    (is (= 42
           (fns/filter-percentile-cont [[0.5 42]]))))

  (testing "empty → :__null__"
    (is (= :__null__ (fns/filter-percentile-cont []))))

  (testing "all-null pairs → :__null__"
    (is (= :__null__
           (fns/filter-percentile-cont [[0.5 :__null__] [0.5 nil]]))))

  (testing "p=0 → first sorted value (cast to double via interpolation arithmetic)"
    (is (= 1.0
           (fns/filter-percentile-cont
            [[0.0 3] [0.0 1] [0.0 2]]))))

  (testing "p=1 → last sorted value"
    (is (= 3.0
           (fns/filter-percentile-cont
            [[1.0 3] [1.0 1] [1.0 2]])))))

;; ---------------------------------------------------------------------------
;; PERCENTILE_DISC — discrete (no interpolation)

(deftest percentile-disc-basic
  (testing "discrete median picks the lower middle, no interpolation"
    (is (= 2
           (fns/filter-percentile-disc
            [[0.5 1] [0.5 2] [0.5 3] [0.5 4]]))
        "p_disc(0.5) on 4 sorted values: ceil(0.5 * 4) = 2 → vs[1] = 2"))

  (testing "p=0 returns first sorted value"
    (is (= 1
           (fns/filter-percentile-disc
            [[0.0 1] [0.0 2] [0.0 3]]))))

  (testing "p=1 returns last"
    (is (= 3
           (fns/filter-percentile-disc
            [[1.0 1] [1.0 2] [1.0 3]])))))

;; ---------------------------------------------------------------------------
;; MODE — most frequent, ascending tiebreak

(deftest mode-basic
  (testing "most frequent value"
    (is (= 2
           (fns/filter-mode [1 2 2 3 2]))))

  (testing "tie broken by ascending sort"
    (is (= "a"
           (fns/filter-mode ["a" "b" "a" "b"]))))

  (testing "skips :__null__ and nil"
    (is (= 1
           (fns/filter-mode [1 :__null__ nil 1 2]))))

  (testing "empty → :__null__"
    (is (= :__null__ (fns/filter-mode [])))))

;; ---------------------------------------------------------------------------
;; SUM-NUMERIC — BigDecimal-precision SUM(int8) / SUM(numeric)

(deftest sum-numeric-preserves-precision
  (testing "BigDecimal sum of integer inputs"
    (is (= (java.math.BigDecimal/valueOf 60)
           (fns/filter-sum-numeric [10 20 30]))))

  (testing "mixed BigDecimal + Long"
    (is (= (java.math.BigDecimal. "60.50")
           (fns/filter-sum-numeric
            [(java.math.BigDecimal. "10.50") 20 30]))))

  (testing "skips :__null__"
    (is (= (java.math.BigDecimal/valueOf 30)
           (fns/filter-sum-numeric [10 :__null__ 20 nil]))))

  (testing "empty → :__null__"
    (is (= :__null__ (fns/filter-sum-numeric []))))

  (testing "doesn't overflow on values that exceed Long range"
    ;; Long/MAX_VALUE = 9_223_372_036_854_775_807
    ;; Sum of two MAX would overflow Long; BigDecimal handles fine.
    (let [m Long/MAX_VALUE
          result (fns/filter-sum-numeric [m m m])]
      (is (= 0 (.compareTo ^java.math.BigDecimal result
                           (.multiply
                            (java.math.BigDecimal/valueOf m)
                            (java.math.BigDecimal/valueOf 3))))))))

;; ---------------------------------------------------------------------------
;; AVG-NUMERIC — BigDecimal-precision AVG(int*) / AVG(numeric)

(deftest avg-numeric-preserves-precision
  (testing "AVG of [10 20 30] = exactly 20"
    (let [r (fns/filter-avg-numeric [10 20 30])]
      (is (instance? java.math.BigDecimal r))
      (is (zero? (.compareTo ^java.math.BigDecimal r
                             (java.math.BigDecimal/valueOf 20))))))

  (testing "AVG of cents preserves fractional part (no float rounding)"
    ;; Sum 100 + 200 + 250 = 550. AVG = 183.3333...
    ;; A Double-based AVG might lose digits; BigDecimal at scale 16
    ;; preserves them.
    (let [r (fns/filter-avg-numeric [100 200 250])]
      (is (instance? java.math.BigDecimal r))
      ;; Render check: scale should be 16, value = 183.333...3 exactly.
      (is (= 16 (.scale ^java.math.BigDecimal r))
          (str "scale: " (.scale ^java.math.BigDecimal r)))))

  (testing "skips :__null__"
    (let [r (fns/filter-avg-numeric [10 :__null__ 30])]
      (is (zero? (.compareTo ^java.math.BigDecimal r
                             (java.math.BigDecimal/valueOf 20))))))

  (testing "empty → :__null__"
    (is (= :__null__ (fns/filter-avg-numeric [])))))
