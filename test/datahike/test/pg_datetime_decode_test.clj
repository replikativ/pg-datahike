(ns datahike.test.pg-datetime-decode-test
  "The leaf decoders -- `date2j`/`j2date`, `ValidateDate`,
   `DecodeTimezone`, `DecodeTimeCommon`.

   Pure functions over one field each, so these are tables. Where a rule
   is surprising the expectation was CHECKED against the running 17.7
   oracle rather than derived from my reading of the C, and the oracle's
   answer is quoted in the comment."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.datetime.decode :as dec]
            [datahike.pg.datetime.lex :as lex]))

(defn- caught [f] (try (f) (catch clojure.lang.ExceptionInfo e
                             (::lex/dterr (ex-data e)))))

(deftest julian-day-conversions-round-trip
  ;; date2j is the hinge of everything: day-of-year resolution, the
  ;; weekday, and the whole timestamp range depend on it.
  (is (= 2451545 (dec/date2j 2000 1 1)))
  (is (= [2000 1 1] (dec/j2date 2451545)))
  (is (= 2440588 (dec/date2j 1970 1 1)))
  (is (= [1970 1 1] (dec/j2date 2440588)))
  (testing "POSTGRES_EPOCH_JDATE, the zero of the internal representation"
    (is (= 2451545 (dec/date2j 2000 1 1))))
  (testing "round-trips across the whole supported range, including BC,
            where the year is negative and date2j's integer division
            must still truncate the same way"
    (doseq [[y m d] [[1 1 1] [0 1 1] [-4713 11 24] [1582 10 15]
                     [1900 2 28] [2000 2 29] [5874897 12 31]]]
      (is (= [y m d] (dec/j2date (dec/date2j y m d))) (str y "-" m "-" d))))
  (testing "j2day: 2000-01-01 was a Saturday (6)"
    (is (= 6 (dec/j2day (dec/date2j 2000 1 1))))
    (is (= 0 (dec/j2day (dec/date2j 2000 1 2))))))

(deftest leap-years-use-the-proleptic-year
  ;; `isleap` is applied to the year as PostgreSQL STORES it, where 1 BC
  ;; is year 0. So year 0 is a leap year and 29 February 1 BC exists.
  (is (dec/leap-year? 2000))
  (is (not (dec/leap-year? 1900)))
  (is (dec/leap-year? 1996))
  (is (dec/leap-year? 0))
  (is (= 29 (dec/days-in-month 2000 2)))
  (is (= 28 (dec/days-in-month 1900 2)))
  (is (= 29 (dec/days-in-month 0 2))))

(deftest validate-date-applies-the-year-rules-in-order
  (let [v (fn [tm fields opts] (caught #(dec/validate-date tm fields opts)))
        ymd #{:year :month :day}]
    (testing "two digits: <70 is 2000s, <100 is 1900s"
      (is (= 1970 (:year (dec/validate-date {:year 70 :mon 1 :mday 1} ymd
                                            {:two-digits? true}))))
      (is (= 2069 (:year (dec/validate-date {:year 69 :mon 1 :mday 1} ymd
                                            {:two-digits? true}))))
      (is (= 2000 (:year (dec/validate-date {:year 0 :mon 1 :mday 1} ymd
                                            {:two-digits? true}))))
      (is (= 1999 (:year (dec/validate-date {:year 99 :mon 1 :mday 1} ymd
                                            {:two-digits? true})))))
    (testing "BC comes FIRST in the chain, so a two-digit BC year is NOT
              expanded -- `70 BC` is 70 BC, not 1970 BC. 1 BC is stored
              as year 0."
      (is (= -69 (:year (dec/validate-date {:year 70 :mon 1 :mday 1} ymd
                                           {:two-digits? true :bc? true}))))
      (is (= 0 (:year (dec/validate-date {:year 1 :mon 1 :mday 1} ymd
                                         {:bc? true}))))
      (is (= -1 (:year (dec/validate-date {:year 2 :mon 1 :mday 1} ymd
                                          {:bc? true})))))
    (testing "there is no year zero in AD/BC notation"
      (is (= :field-overflow (v {:year 0 :mon 1 :mday 1} ymd {})))
      (is (= :field-overflow (v {:year 0 :mon 1 :mday 1} ymd {:bc? true})))
      (is (= :field-overflow (v {:year -1 :mon 1 :mday 1} ymd {}))))
    (testing "a JULIAN year skips the check entirely, because a Julian
              day legitimately lands on year 0 and below"
      (is (= 0 (:year (dec/validate-date {:year 0 :mon 1 :mday 1} ymd
                                         {:julian? true})))))
    (testing "the month-length check needs year, month AND day, and is
              FIELD_OVERFLOW rather than MD_FIELD_OVERFLOW -- `Feb 29`
              is unlikely to be a field-order mistake, so the error
              should not suggest a DateStyle fix"
      (is (= :field-overflow (v {:year 1997 :mon 2 :mday 29} ymd {})))
      (is (map? (v {:year 2000 :mon 2 :mday 29} ymd {})))
      (is (= :field-overflow (v {:year 1997 :mon 4 :mday 31} ymd {}))))
    (testing "while an out-of-range month or day IS MD_FIELD_OVERFLOW"
      (is (= :md-field-overflow (v {:year 1997 :mon 13 :mday 1} ymd {})))
      (is (= :md-field-overflow (v {:year 1997 :mon 1 :mday 32} ymd {})))
      (is (= :md-field-overflow (v {:year 1997 :mon 1 :mday 0} ymd {}))))
    (testing "a day-of-year is resolved AFTER the year rules, because it
              needs the corrected year to know the February length"
      (is (= {:year 1997 :mon 2 :mday 7 :yday 38}
             (select-keys (dec/validate-date {:year 1997 :yday 38} #{:year :doy} {})
                          [:year :mon :mday :yday])))
      (is (= [2000 12 31]
             ((juxt :year :mon :mday)
              (dec/validate-date {:year 2000 :yday 366} #{:year :doy} {})))))))

(deftest decode-timezone-splits-on-LENGTH-not-value
  (let [tz (fn [s] (caught #(dec/decode-timezone s)))]
    (testing "the result is SECONDS WEST -- `+05` is -18000, because the
              C stores `*tzp = -tz` (datetime.c:3060)"
      (is (= -18000 (tz "+05")))
      (is (= 28800 (tz "-08"))))
    (testing "with no colon, a string LONGER THAN THREE characters
              splits as hhmm. So `+100` is ONE hour -- four characters,
              hr=1 min=0 -- and the oracle agrees:
                '2000-01-01 12:00:00+100'::timestamptz => 11:00:00+00"
      (is (= -19800 (tz "+0530")))
      (is (= -3600 (tz "+100")))
      (is (= -18000 (tz "+05")))
      (is (= 0 (tz "+0"))))
    (testing "colons give hh:mm and hh:mm:ss"
      (is (= -19800 (tz "+05:30")))
      (is (= -19830 (tz "+05:30:30")))
      (is (= 0 (tz "+00:00:00"))))
    (testing "MAX_TZDISP_HOUR is 15, and over it is TZDISP_OVERFLOW --
              which becomes 22009, not 22007"
      (is (= -57599 (tz "+15:59:59")))
      (is (= :tzdisp-overflow (tz "+16")))
      (is (= :tzdisp-overflow (tz "-16")))
      (is (= :tzdisp-overflow (tz "+05:60")))
      (is (= :tzdisp-overflow (tz "+05:30:60"))))
    (testing "and anything left over is a FORMAT error. The lexer hands
              the whole of `+05.5` here precisely so it is refused:
                '… +05.5'::timestamptz => invalid input syntax"
      (is (= :bad-format (tz "+05.5")))
      (is (= :bad-format (tz "+05x")))
      (is (= :bad-format (tz "05"))))))

(deftest decode-time-and-the-field-shift
  (let [t (fn [s] (caught #(dec/decode-time s)))]
    (is (= {:hour 4 :min 5 :sec 6 :usec 0} (t "04:05:06")))
    (is (= {:hour 4 :min 5 :sec 0 :usec 0} (t "04:05")))
    (is (= {:hour 4 :min 5 :sec 6 :usec 123456} (t "04:05:06.123456")))
    (testing "`hh:mm.ff` is NOT a fraction of a minute -- the C shifts
              the fields down, so `04:05.5` is 00:04:05.5, and the
              oracle agrees: '04:05.5'::time => 00:04:05.5"
      (is (= {:hour 0 :min 4 :sec 5 :usec 500000} (t "04:05.5"))))
    (testing "seconds may be exactly 60 (`> SECS_PER_MINUTE` is the
              failure, not `>=`); minutes may not"
      (is (= {:hour 23 :min 59 :sec 60 :usec 0} (t "23:59:60")))
      (is (= :field-overflow (t "23:60:00"))))
    (testing "the hour has NO upper bound HERE. 24:00:00 is legal and
              25:00:00 is rejected by the CALLER, not by this function
              -- putting the limit here would reject 24:00:00 too"
      (is (= {:hour 24 :min 0 :sec 0 :usec 0} (t "24:00:00")))
      (is (= {:hour 25 :min 0 :sec 0 :usec 0} (t "25:00:00")))
      (is (= {:hour 99 :min 0 :sec 0 :usec 0} (t "99:00:00"))))
    (testing "a colon is required after the hour"
      (is (= :bad-format (t "0405")))
      (is (= :bad-format (t "04")))
      (is (= :bad-format (t "04:05:06:07")))
      (is (= :bad-format (t "04:05:06x"))))))
