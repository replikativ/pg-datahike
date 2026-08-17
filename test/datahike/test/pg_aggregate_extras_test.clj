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

;; ---------------------------------------------------------------------------
;; MIN/MAX ordering — every type SQL orders, not just numbers

(deftest min-max-orders-non-numeric-types
  (testing "text compares as text, not as a number"
    (is (= "apple" (fns/filter-min ["pear" "apple" "fig"])))
    (is (= "pear"  (fns/filter-max ["pear" "apple" "fig"]))))

  (testing "dates and timestamps"
    (let [d1 (java.time.LocalDate/parse "2020-01-01")
          d2 (java.time.LocalDate/parse "2021-06-15")]
      (is (= d1 (fns/filter-min [d2 d1])))
      (is (= d2 (fns/filter-max [d2 d1]))))
    (let [t1 (java.util.Date. 1000000000000)
          t2 (java.util.Date. 2000000000000)]
      (is (= t1 (fns/filter-min [t2 t1])))
      (is (= t2 (fns/filter-max [t2 t1])))))

  (testing "numbers still work, including mixed width and BigDecimal"
    (is (= 1 (fns/filter-min [3 1 2])))
    (is (= 3 (fns/filter-max [3 1 2])))
    (is (= 0 (compare 2.5M (fns/filter-max [1.5M 2.5M])))))

  (testing "NULL sentinels are skipped; all-NULL is NULL"
    (is (= "a" (fns/filter-min [:__null__ "a" :__null__])))
    (is (= :__null__ (fns/filter-max [:__null__ :__null__])))
    (is (= :__null__ (fns/filter-min []))))

  (testing "bytea orders by unsigned byte value, as PG's byteacmp does"
    ;; byte[] is not Comparable, so plain `compare` would throw. The
    ;; unsigned part matters: (byte -1) is 0xFF and must sort ABOVE
    ;; 0x01, where Java's signed byte comparison would put it below.
    (is (= [0x01] (vec (fns/filter-min [(byte-array [0x01]) (byte-array [-1])]))))
    (is (= [-1]   (vec (fns/filter-max [(byte-array [0x01]) (byte-array [-1])])))))

  (testing "a single value needs no comparison and is returned as-is"
    (is (= "only" (fns/filter-max ["only"])))))

(deftest min-max-rejects-types-postgres-has-no-aggregate-for
  ;; PostgreSQL has no max(bool) and no max(uuid) on ANY release,
  ;; master included. Raise 42883 as PG does rather than letting a
  ;; ClassCastException escape. bytea is deliberately NOT here — it has
  ;; an aggregate upstream and we track upstream.
  (doseq [[tname vs] {"boolean" [true false]
                      "uuid"    [(java.util.UUID/randomUUID) (java.util.UUID/randomUUID)]}]
    (testing (str "max(" tname ") raises undefined_function")
      ;; :error is the codebase's error key; errors.clj maps
      ;; :undefined-function to SQLSTATE 42883 at the wire boundary.
      (let [e (is (thrown? clojure.lang.ExceptionInfo (fns/filter-max vs)))
            d (ex-data e)]
        (is (= :undefined-function (:error d)) (str tname ": " (pr-str d)))
        (is (= (str "max(" tname ")") (:function d)))))))

;; ---------------------------------------------------------------------------
;; GREATEST / LEAST — scalar, not aggregates, and defined over any ordering

(deftest greatest-least-order-non-numeric-types
  (let [greatest (get fns/sql-fn->clj-fn "greatest")
        least    (get fns/sql-fn->clj-fn "least")]
    (is (= "b" (greatest "a" "b")))
    (is (= "a" (least "a" "b")))
    (is (= 2 (greatest 1 2)))
    (is (= 1 (least 1 2)))
    (let [d1 (java.time.LocalDate/parse "2020-01-01")
          d2 (java.time.LocalDate/parse "2021-01-01")]
      (is (= d2 (greatest d1 d2)))
      (is (= d1 (least d1 d2))))))

;; ---------------------------------------------------------------------------
;; string_agg — a real aggregate, folding [value delimiter] pairs

(deftest string-agg-folds-the-whole-group
  (testing "joins with the delimiter carried on each pair"
    (is (= "a,b,c" (fns/filter-string-agg [["a" ","] ["b" ","] ["c" ","]])))
    (is (= "a-b"   (fns/filter-string-agg [["a" "-"] ["b" "-"]]))))

  (testing "NULL inputs are skipped, not stringified"
    ;; Was `(str v)` per row, which rendered the sentinel.
    (is (= "a,b" (fns/filter-string-agg [["a" ","] [:__null__ ","] ["b" ","]])))
    (is (= "a"   (fns/filter-string-agg [[nil ","] ["a" ","]]))))

  (testing "an all-NULL or empty group is SQL NULL"
    (is (= :__null__ (fns/filter-string-agg [[:__null__ ","]])))
    (is (= :__null__ (fns/filter-string-agg []))))

  (testing "a NULL delimiter joins with nothing rather than rendering"
    (is (= "ab" (fns/filter-string-agg [["a" :__null__] ["b" :__null__]]))))

  (testing "non-string values are stringified, as PG's text coercion does"
    (is (= "1,2" (fns/filter-string-agg [[1 ","] [2 ","]])))))

(deftest string-agg-ordered
  ;; Triples are [sort-key value delimiter].
  (testing "ascending"
    (is (= "a,b,c" (fns/filter-string-agg-ordered
                    [["b" "b" ","] ["a" "a" ","] ["c" "c" ","]]))))
  (testing "descending"
    (is (= "c,b,a" (fns/filter-string-agg-ordered-desc
                    [["b" "b" ","] ["a" "a" ","] ["c" "c" ","]]))))
  (testing "a NULL sort key sorts last, rather than throwing"
    ;; akey-compare knew `nil` but not the :__null__ sentinel a nullable
    ;; ORDER BY column actually binds to, so this threw
    ;; `Keyword cannot be cast to String`. array_agg shared the bug.
    (is (= "a,b" (fns/filter-string-agg-ordered
                  [[:__null__ nil ","] ["a" "a" ","] ["b" "b" ","]])))))
