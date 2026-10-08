(ns datahike.test.pg-interval-arith-test
  "Interval arithmetic: the `oprcode` functions behind `timestamp ±
   interval`, `interval ± interval`, `-interval`, `interval * n`,
   `interval / n` and `time ± interval`.

   These are the unit tests for the implementation; the SQL-level
   behaviour is covered by `pg_date_arithmetic_test` and by the
   differential corpus.

   Every expectation is the oracle's own answer. The month-clamp cases
   matter most: `'2001-01-31' + '1 mon'` and `+ '30 days'` are
   cmp-EQUAL intervals that must give different days, which is the
   concrete reason `interval_cmp_value` cannot be the carrier's
   `.equals`."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.interval.arith :as ia]
            [datahike.pg.interval.core :as ic]
            [datahike.pg.types :as types]))

(defn- iv [s] (ic/interval-in s))
(defn- s'
  "The PostgreSQL text, via the single rendering funnel. NOT `str`: the
   infinity sentinels are `java.util.Date`s whose `toString` is a Java
   date, and only `->pg-text` knows they mean `infinity`."
  [x] (types/->pg-text x))

(def ^:private ts-conv
  "The conversion `timestamp-plus` needs, over `LocalDateTime`."
  {:ts->fields
   (fn [^java.time.LocalDateTime t]
     {:year (.getYear t) :mon (.getMonthValue t) :mday (.getDayOfMonth t)
      :usec-of-day (quot (.toNanoOfDay (.toLocalTime t)) 1000)})})

(defn- plus [^String ts ^String ivs]
  (let [t (java.time.LocalDateTime/parse ts)]
    (str (ia/timestamp-plus
          t (iv ivs)
          (assoc ts-conv :fields->ts
                 (fn [{:keys [year mon mday usec-of-day]} m]
                   (-> (java.time.LocalDate/of (int year) (int mon) (int mday))
                       (.atStartOfDay)
                       (.plusNanos (* 1000 (+ (long usec-of-day) (long m)))))))))))

(deftest the-three-stages-have-different-semantics
  ;; `timestamp_pl_interval` applies months, then days, then
  ;; microseconds, and only the MONTH stage clamps the day to the
  ;; target month's length. Oracle:
  ;;   '2001-01-31'::timestamp + '1 mon'   => 2001-02-28 00:00:00
  ;;   '2001-01-31'::timestamp + '30 days' => 2001-03-02 00:00:00
  ;; Those two intervals are cmp-EQUAL, which is precisely why
  ;; `interval_cmp_value` must not be used for arithmetic.
  (is (= "2001-02-28T00:00" (plus "2001-01-31T00:00" "1 mon")))
  (is (= "2001-03-02T00:00" (plus "2001-01-31T00:00" "30 days")))
  (is (= "2001-03-31T00:00" (plus "2001-01-31T00:00" "2 mons")))
  (is (= "2001-04-01T00:00" (plus "2001-01-31T00:00" "60 days")))
  (testing "a leap year, where the clamp lands differently"
    (is (= "2000-02-29T00:00" (plus "2000-01-31T00:00" "1 mon")))
    (is (= "2000-03-01T00:00" (plus "2000-01-31T00:00" "30 days"))))
  (testing "subtraction clamps the same way, because it is addition of
            the negation"
    (is (= "2001-02-28T00:00"
           (str (ia/timestamp-minus
                 (java.time.LocalDateTime/parse "2001-03-31T00:00") (iv "1 mon")
                 (assoc ts-conv :fields->ts
                        (fn [{:keys [year mon mday usec-of-day]} m]
                          (-> (java.time.LocalDate/of (int year) (int mon) (int mday))
                              (.atStartOfDay)
                              (.plusNanos (* 1000 (+ (long usec-of-day)
                                                     (long m))))))))))))
  (testing "and the microsecond stage is plain addition"
    (is (= "2001-01-31T02:00" (plus "2001-01-31T00:00" "2 hours")))
    (is (= "2001-02-01T00:00" (plus "2001-01-31T23:00" "1 hour")))))

(deftest interval-plus-minus-and-negation
  (is (= "1 mon 1 day" (s' (ia/add (iv "1 mon") (iv "1 day")))))
  (is (= "1 mon -1 days" (s' (ia/subtract (iv "1 mon") (iv "1 day")))))
  (is (= "-1 days" (s' (ia/negate (iv "1 day")))))
  (is (= "-1 mons -2 days -03:04:05" (s' (ia/negate (iv "1 mon 2 days 03:04:05")))))
  (is (= "00:00:00" (s' (ia/subtract (iv "1 day") (iv "1 day")))))
  (testing "cmp-equal operands give DIFFERENT results, which is the
            whole reason the cmp equivalence is not a congruence for
            arithmetic"
    (is (= "1 mon 1 day" (s' (ia/add (iv "1 mon") (iv "1 day")))))
    (is (= "31 days" (s' (ia/add (iv "30 days") (iv "1 day")))))
    (is (ic/value-eq? (iv "1 mon") (iv "30 days"))
        "they are still VALUE-equal, under the opclass equality")
    (is (not= (iv "1 mon") (iv "30 days"))
        "and NOT `.equals`-equal, which is what keeps caches honest")))

(deftest multiply-and-divide-cascade-fractions-downward
  ;; `interval_mul`: a fraction of a month becomes days at 30 per
  ;; month, and a fraction of a day becomes seconds. Oracle:
  ;;   '1 mon'::interval * 0.5  => 15 days
  ;;   '1 day'::interval * 1.5  => 1 day 12:00:00
  (is (= "2 days" (s' (ia/multiply (iv "1 day") 2.0))))
  (is (= "15 days" (s' (ia/multiply (iv "1 mon") 0.5))))
  (is (= "1 day 12:00:00" (s' (ia/multiply (iv "1 day") 1.5))))
  (is (= "12:00:00" (s' (ia/divide (iv "1 day") 2.0))))
  (is (= "-1 days" (s' (ia/multiply (iv "1 day") -1.0))))
  (testing "division by zero is 22012, not an interval error"
    (is (thrown? clojure.lang.ExceptionInfo (ia/divide (iv "1 day") 0.0))))
  (testing "and NaN is an interval overflow"
    (is (thrown? clojure.lang.ExceptionInfo (ia/multiply (iv "1 day") Double/NaN)))))

(deftest a-time-takes-the-microseconds-only
  ;; `time_pl_interval` (date.c:1745-1762) adds `span->time` and wraps
  ;; into one day. Months and days are ignored -- a time has nowhere to
  ;; put them. Oracle: '12:00:00'::time + '1 mon' => 12:00:00
  (let [h (fn [usec] (str (java.time.LocalTime/ofNanoOfDay (* 1000 usec))))
        noon (* 12 3600000000)]
    (is (= "14:00" (h (ia/time-plus noon (iv "2 hours")))))
    (is (= "12:00" (h (ia/time-plus noon (iv "1 mon")))))
    (is (= "12:00" (h (ia/time-plus noon (iv "30 days")))))
    (testing "and it WRAPS rather than overflowing the day"
      (is (= "01:00" (h (ia/time-plus (* 23 3600000000) (iv "2 hours")))))
      (is (= "23:00" (h (ia/time-minus (* 1 3600000000) (iv "2 hours"))))))
    (testing "an infinite interval has no time to add"
      (is (thrown? clojure.lang.ExceptionInfo
                   (ia/time-plus noon (ic/interval-in "infinity")))))))

(deftest the-infinity-rules
  ;; `interval_pl`: two infinities of OPPOSITE sign are an error and
  ;; otherwise the infinity wins.
  (let [pos (ic/interval-in "infinity") neg (ic/interval-in "-infinity")]
    (is (= "infinity" (s' (ia/add pos (iv "1 day")))))
    (is (= "-infinity" (s' (ia/add neg (iv "1 day")))))
    (is (= "infinity" (s' (ia/add pos pos))))
    (is (thrown? clojure.lang.ExceptionInfo (ia/add pos neg)))
    (is (= "-infinity" (s' (ia/negate pos))))
    (is (= "infinity" (s' (ia/negate neg))))
    (testing "times zero is an error, and times a negative flips it"
      (is (thrown? clojure.lang.ExceptionInfo (ia/multiply pos 0.0)))
      (is (= "-infinity" (s' (ia/multiply pos -2.0))))
      (is (= "infinity" (s' (ia/multiply pos 2.0)))))))
