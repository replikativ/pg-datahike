(ns datahike.test.pg-interval-decode-test
  "`DecodeInterval`, `DecodeISO8601Interval`, the `Adjust*` helpers and
   the carrier.

   Expectations that are not obvious were checked against the running
   oracle and its answer is quoted."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.datetime.lex :as lex]
            [datahike.pg.interval.core :as ic]
            [datahike.pg.interval.decode :as ivd]))

(defn- d
  "Decode, as `[months days micros]`, or the dterr keyword."
  [s]
  (try (let [r (ivd/decode-interval (lex/tokenize s 256))]
         (if (= :delta (:dtype r))
           [(:months r) (:days r) (:micros r)]
           (:dtype r)))
       (catch clojure.lang.ExceptionInfo e (::lex/dterr (ex-data e)))))

(defn- iso [s]
  (try (let [r (ivd/decode-iso8601-interval s)] [(:months r) (:days r) (:micros r)])
       (catch clojure.lang.ExceptionInfo e (::lex/dterr (ex-data e)))))

(defn- out [months days micros] (str (ic/interval months days micros)))

(deftest the-loop-runs-right-to-left
  ;; Units FOLLOW their values, so `'1 day'` reads `day` then `1`.
  ;; Nothing else in the datetime family works this way.
  (is (= [0 1 0] (d "1 day")))
  (is (= [1 0 0] (d "1 mon")))
  (is (= [0 30 0] (d "30 days")))
  (is (= [14 0 0] (d "1 year 2 mons")))
  (is (= [0 7 0] (d "1 week")))
  (testing "two units with no value between them is an error, which is
            what `parsing_unit_val` is for"
    (is (= :bad-format (d "1 day 2 day")))
    (is (= :bad-format (d "day day"))))
  (testing "and a bare unit with nothing to apply it to"
    (is (= :bad-format (d "day")))))

(deftest a-time-field-claims-the-whole-time-family
  ;; `DecodeTimeCommon` sets `*tmask = DTK_TIME_M`. With an empty tmask
  ;; `'24:00:00'` reached the end with nothing claimed and was refused
  ;; as `fmask == 0` -- which is how I found this.
  (is (= [0 0 86400000000] (d "24:00:00")))
  (is (= [0 0 2592000000000] (d "720:00:00")))
  (is (= [0 0 3723456789] (d "01:02:03.456789")))
  (testing "DTK_HOUR hands DAY to the next field, which is what makes
            `'1 2:03:04'` one DAY and two hours"
    (is (= [0 1 7384000000] (d "1 2:03:04")))
    (is (= [0 -1 7384000000] (d "-1 2:03:04")))))

(deftest the-sql-years-months-form-overrides-the-type
  (is (= [14 0 0] (d "1-2")))
  (is (= [-14 0 0] (d "-1-2")))
  (testing "the month part must be 0..11, and out of range is
            FIELD_OVERFLOW rather than a format error -- so
            `'1-13'::interval` is 22015, not 22007"
    (is (= :field-overflow (d "1-13")))
    (is (= :field-overflow (d "1-12")))))

(deftest a-signed-field-is-a-time-only-when-it-has-a-colon
  ;; The DTK_TZ arm FALLS THROUGH to the number case without one,
  ;; which is why `'-01:00'` is a negative hour and `'+1'` is a value.
  (is (= [0 0 -3600000000] (d "-01:00")))
  (is (= [0 0 3600000000] (d "+01:00")))
  (is (= [0 0 3720000000] (d "1:2")))
  (is (= [0 0 1000000] (d "+1"))))

(deftest the-fractional-helpers
  ;; AdjustFractDays puts the INTEGER part of the scaled fraction into
  ;; whole days and only the remainder into microseconds.
  (is (= [1 15 0] (d "1.5 months")))
  (is (= [18 0 0] (d "1.5 years")))
  (is (= [0 3 43200000000] (d "0.5 weeks")))
  (is (= [0 10 43200000000] (d "1.5 weeks")))
  (is (= [0 0 1500000] (d "1.5 sec")))
  (testing "AdjustFractYears rounds half to EVEN (`rint`, not `round`),
            so `0.5 mons` must not gain a month"
    (is (= [6 0 0] (d "0.5 years")))
    (is (= [0 15 0] (d "0.5 mons")))))

(deftest ago-and-the-reserved-words
  (is (= [0 -1 0] (d "@ 1 day ago")))
  (is (= [-14 0 0] (d "@ 1 year 2 mons ago")))
  (is (= :late (d "infinity")))
  (is (= :early (d "-infinity")))
  (testing "AGO is legal ONLY as the last field"
    (is (= :bad-format (d "1 day ago 2 hours"))))
  (testing "and so is a RESERV word, so `infinity ago` is an error"
    (is (= :bad-format (d "infinity ago"))))
  (testing "a literal with nothing claimed at all"
    (is (= :bad-format (d "ago")))
    (is (= :bad-format (d "@")))))

(deftest the-typmod-decides-what-a-bare-number-means
  ;; `'1'::interval` is one SECOND and `interval '1' hour` is one HOUR,
  ;; and the only difference is the `range` the caller passes.
  (let [dr (fn [s r] (let [x (ivd/decode-interval (lex/tokenize s 256) r false)]
                       [(:months x) (:days x) (:micros x)]))]
    (is (= [0 0 1000000] (dr "1" ivd/full-range)))
    (is (= [0 0 3600000000] (dr "1" #{:hour})))
    (is (= [0 0 60000000] (dr "1" #{:minute})))
    (is (= [0 1 0] (dr "1" #{:day})))
    (is (= [1 0 0] (dr "1" #{:month})))
    (is (= [12 0 0] (dr "1" #{:year})))
    (testing "a field SET picks its least significant member"
      (is (= [0 0 60000000] (dr "1" #{:hour :minute})))
      (is (= [0 0 1000000] (dr "1" #{:day :hour :minute :second}))))))

(deftest units-that-resolve-and-are-still-refused
  ;; DTK_QUARTER and the timezone units reach `DecodeInterval`'s
  ;; `default`. Oracle: `'1 qtr'::interval` is 22007.
  (is (= :bad-format (d "1 qtr")))
  (is (= :bad-format (d "1 quarter")))
  (is (= :bad-format (d "1 timezone")))
  (is (= :bad-format (d "1 timezone_hour")))
  (is (= :bad-format (d "1 j")))
  (testing "while the fallback units that DO have arms work, including
            the `m`/`mm` pair that resolves through two tables"
    (is (= [0 0 60000000] (d "1 m")))
    (is (= [0 0 60000000] (d "1 mm")))
    (is (= [0 0 60000000] (d "1 min")))
    (is (= [12000 0 0] (d "1 millenniumXYZ")))))

(deftest iso-8601-designator-and-alternative-forms
  (is (= [14 3 14706000000] (iso "P1Y2M3DT4H5M6S")))
  (is (= [144 0 0] (iso "P12Y")))
  (is (= [0 0 0] (iso "PT0S")))
  (is (= [0 1 43200000000] (iso "P1.5D")))
  (is (= [6 0 0] (iso "P0.5Y")))
  (is (= [0 7 0] (iso "P1W")))
  (is (= [0 10 43200000000] (iso "P1.5W")))
  (is (= [0 -1 7384000000] (iso "P-1DT2H3M4S")))
  (testing "the ALTERNATIVE basic forms, selected by field WIDTH --
            eight digits before T and six after"
    (is (= [14 3 0] (iso "P00010203")))
    (is (= [14 3 14706000000] (iso "P00010203T040506")))
    (is (= [0 0 14706000000] (iso "PT040506"))))
  (testing "and the alternative EXTENDED forms"
    (is (= [14 3 0] (iso "P0001-02-03")))
    (is (= [14 3 14706000000] (iso "P0001-02-03T04:05:06")))
    (is (= [0 0 14706000000] (iso "PT04:05:06"))))
  (testing "refusals"
    (is (= :bad-format (iso "P")))
    (is (= :bad-format (iso "P1X")))
    (is (= :bad-format (iso "PT1X")))
    (is (= :bad-format (iso "1Y"))))
  (testing "a repeated designator is NOT refused -- the `havefield`
            guard sits only in the alternative-format arms, so
            `'P1Y1Y'::interval` is 2 years on the oracle too"
    (is (= [24 0 0] (iso "P1Y1Y")))))

(deftest the-carrier-owns-equality-ordering-and-hashing
  (let [i ic/interval]
    (testing "a month is 30 days and a day is 24 hours, so these are
              EQUAL -- and the hash agrees, which is what makes GROUP
              BY and DISTINCT correct without any comparator"
      (is (= (i 1 0 0) (i 0 30 0)))
      (is (= (i 0 1 0) (i 0 0 86400000000)))
      (is (= (i 12 0 0) (i 0 360 0)))
      (is (= (i 1 -30 0) (i 0 0 0)))
      (is (= (hash (i 1 0 0)) (hash (i 0 30 0))))
      (is (= (hash (i 12 0 0)) (hash (i 0 360 0)))))
    (testing "near-misses stay distinct"
      (is (not= (i 1 0 0) (i 0 29 0)))
      (is (not= (i 1 0 0) (i 0 31 0)))
      (is (not= (i 12 0 0) (i 0 365 0))))
    (testing "so the Clojure collections that no comparator can reach
              are correct by construction"
      (is (= 2 (count (set [(i 1 0 0) (i 0 30 0) (i 0 29 0)]))))
      (is (= 2 (count (group-by identity [(i 1 0 0) (i 0 30 0) (i 0 29 0)]))))
      (is (= 2 (count (distinct [(i 1 0 0) (i 0 30 0) (i 0 29 0)]))))
      (is (contains? (set [(i 0 30 0)]) (i 1 0 0))))
    (testing "Comparable, which is what `fns/order-cmp`'s existing
              `:else (compare a b)` already routes to"
      (is (neg? (compare (i 0 29 0) (i 1 0 0))))
      (is (pos? (compare (i 0 31 0) (i 1 0 0))))
      (is (zero? (compare (i 1 0 0) (i 0 30 0))))
      (is (= [(i 0 29 0) (i 1 0 0) (i 0 31 0)]
             (sort [(i 0 31 0) (i 1 0 0) (i 0 29 0)]))))
    (testing "and the span is computed in 128 bits, because
              `month * 30 * 86400000000` overflows int64"
      (is (= (i 2147483647 0 0) (i 2147483647 0 0)))
      (is (pos? (compare (i 2147483647 0 0) (i 2147483646 0 0)))))))

(deftest interval-out-under-the-postgres-style
  ;; Every expectation here was read off the oracle:
  ;;   set intervalstyle='postgres';
  ;;   select '1 mon'::interval, '30 days'::interval, …
  (is (= "1 mon" (out 1 0 0)))
  (is (= "30 days" (out 0 30 0)))
  (is (= "1 year 2 mons 3 days 04:05:06" (out 14 3 14706000000)))
  (is (= "00:00:00" (out 0 0 0)))
  (is (= "-1 years" (out -12 0 0)))
  (is (= "-01:00:00" (out 0 0 -3600000000)))
  (is (= "1 day" (out 0 1 0)))
  (is (= "24:00:00" (out 0 0 86400000000)))
  (is (= "01:02:03.456789" (out 0 0 3723456789)))
  (testing "the `is_before` rule, which the C's own comment calls
            bizarre: each nonzero field sets it for the NEXT one only,
            so a positive field after a negative one gets an explicit
            `+`. `-1 days +02:03:04`, not `-1 days 02:03:04`."
    (is (= "-1 days +02:03:04" (out 0 -1 7384000000)))
    (is (= "1 day -02:03:04" (out 0 1 -7384000000))))
  (testing "a singular unit has no `s`"
    (is (= "1 mon" (out 1 0 0)))
    (is (= "2 mons" (out 2 0 0)))
    (is (= "1 year" (out 12 0 0)))
    (is (= "1 mon -30 days" (out 1 -30 0)))))
