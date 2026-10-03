(ns datahike.test.pg-datetime-parse-test
  "`DecodeDateTime` and `DecodeTimeOnly`, the two state machines.

   These test the DECODED fields, not a rendered value -- zone
   resolution and the carrier conversion come later and would hide
   which step was wrong. Expectations that are not obvious were checked
   against the running oracle and its answer is quoted."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.datetime.lex :as lex]
            [datahike.pg.datetime.parse :as parse]))

(def ^:private ctx
  {:date-order :mdy :zone? true
   :now {:tm {:year 2026 :mon 10 :mday 3 :hour 7 :min 8 :sec 9 :usec 0} :west 0}})

(defn- dt
  ([s] (dt s ctx))
  ([s c] (try (let [r (parse/decode-datetime (lex/tokenize s) c)]
                (assoc (select-keys (:tm r) [:year :mon :mday :hour :min :sec :usec])
                       :dtype (:dtype r) :zone (:zone r)))
              (catch clojure.lang.ExceptionInfo e (::lex/dterr (ex-data e))))))

(defn- to
  ([s] (to s ctx))
  ([s c] (try (let [r (parse/decode-time-only (lex/tokenize s) c)]
                (assoc (select-keys (:tm r) [:hour :min :sec :usec])
                       :zone (:zone r)))
              (catch clojure.lang.ExceptionInfo e (::lex/dterr (ex-data e))))))

(defn- ymd [s] ((juxt :year :mon :mday) (dt s)))
(defn- hms [s] ((juxt :hour :min :sec) (to s)))

(deftest the-same-digits-mean-different-things-to-the-two-machines
  ;; The reason these are two functions and not one. `DecodeTimeOnly`
  ;; reaches DecodeNumberField at `flen > 4` and passes
  ;; `fmask | DTK_DATE_M` on every call; `DecodeDateTime` uses
  ;; `flen >= 6` and the real fmask.
  (testing "040506 is 2004-05-06 as a date and 04:05:06 as a time.
            The year is 2004 and not 0004 because the positional split
            leaves a TWO-digit year, which ValidateDate then expands:
              select '040506'::date, '040506'::time
              =>  2004-05-06 | 04:05:06"
    (is (= [2004 5 6] (ymd "040506")))
    (is (= [4 5 6] (hms "040506"))))
  (testing "and 0405 is a time but not a date"
    (is (= [4 5 0] (hms "0405")))
    (is (= :bad-format (dt "0405")))))

(deftest decode-datetime-places-fields-by-what-is-already-known
  (is (= [2001 2 3] (ymd "2001-02-03")))
  (is (= [1997 2 10] (ymd "Mon Feb 10 17:32:01 1997")))
  (is (= [1997 2 10] (ymd "10 Feb 1997")))
  (is (= [1997 2 10] (ymd "Feb 10 1997")))
  (is (= [1997 2 10] (ymd "1997-Feb-10")))
  (is (= [1999 1 8] (ymd "08-Jan-99")))
  (is (= [1999 1 8] (ymd "jan 8 99")))
  (testing "the Postgres output style, which timestamp.sql seeds from"
    (is (= [1997 2 10] (ymd "Mon Feb 10 17:32:01 1997 PST")))
    (is (= [17 32 1] ((juxt :hour :min :sec) (dt "Mon Feb 10 17:32:01 1997 PST")))))
  (testing "a day-of-year, which needs the year first"
    (is (= [1997 2 7] (ymd "1997.038")))
    (is (= [2000 12 31] (ymd "2000 366"))))
  (testing "Julian days, with and without a time"
    (is (= [1999 1 8] (ymd "J2451187")))
    (is (= [1999 1 8] (ymd "J 2451187")))
    (is (= [1999 1 8 4 5 6]
           ((juxt :year :mon :mday :hour :min :sec) (dt "J2451187 04:05:06")))))
  (testing "a weekday is silently DROPPED, and `at` and `on` with it"
    (is (= [2000 1 1] (ymd "2000-01-01 Thursday")))
    (is (= [2000 1 1 12 0 0]
           ((juxt :year :mon :mday :hour :min :sec) (dt "2000-01-01 at 12:00")))))
  (testing "BC, where 1 BC is stored as year 0"
    (is (= [-2000 2 3] (ymd "2001-02-03 BC")))
    (is (= [0 1 1] (ymd "0001-01-01 BC")))))

(deftest a-field-may-not-claim-what-another-already-claimed
  ;; `if (tmask & fmask) return DTERR_BAD_FORMAT`. This single rule is
  ;; what makes these errors rather than last-one-wins.
  (is (= :bad-format (dt "2001-02-03 +05 +06")))
  (is (= :bad-format (dt "2001-02-03 2001-02-03")))
  (is (= :bad-format (dt "Feb Feb 10 1997")))
  (is (= :bad-format (dt "04:05:06 04:05:06"))))

(deftest a-dangling-prefix-is-an-error
  ;; `ptype` must be consumed by the following field
  ;; (datetime.c:1498-1499).
  (is (= :bad-format (dt "2001-02-03 J")))
  (is (= :bad-format (dt "J")))
  (is (= :bad-format (dt "2001-02-03T"))))

(deftest decode-time-only-is-stricter-about-what-a-literal-may-contain
  (is (= [4 5 6] (hms "04:05:06")))
  (is (= [0 0 0] (hms "allballs")))
  (testing "24:00:00 and 23:59:60 both decode -- the overflow check is
            on the TOTAL, so neither exceeds a day"
    (is (= [24 0 0] (hms "24:00:00")))
    (is (= [23 59 60] (hms "23:59:60")))
    (is (= :field-overflow (to "24:00:00.001")))
    (is (= :field-overflow (to "25:00:00"))))
  (testing "AM/PM, where 12 AM is midnight and 12 PM is noon"
    (is (= [0 0 0] (hms "12:00 AM")))
    (is (= [12 0 0] (hms "12:00 PM")))
    (is (= [23 59 0] (hms "11:59 PM")))
    (is (= :field-overflow (to "13:00:00 PM"))))
  (testing "only `now` and `allballs` are allowed RESERV words -- there
            is no infinite time of day"
    (is (= [7 8 9] (hms "now")))
    (is (= :bad-format (to "today")))
    (is (= :bad-format (to "infinity")))
    (is (= :bad-format (to "epoch"))))
  (testing "and there is no MONTH or DOW case at all"
    (is (= :bad-format (to "jan")))
    (is (= :bad-format (to "monday")))
    (is (= :bad-format (to "04:05:06 jan"))))
  (testing "a leading date is accepted and discarded, but only when the
            field list looks like a date followed by a time"
    (is (= [4 5 6] (hms "2001-02-03 04:05:06")))))

(deftest a-caller-that-takes-no-zone-refuses-every-zone
  ;; The C's `tzp == NULL`, which is how `date_in` and `time_in` differ
  ;; from their -tz counterparts.
  (let [nz (assoc ctx :zone? false)]
    (is (= :bad-format (dt "2001-02-03 04:05:06+05" nz)))
    (is (= :bad-format (dt "2001-02-03 04:05:06 PST" nz)))
    (is (= :bad-format (dt "2001-02-03 America/New_York" nz)))
    (testing "but a Julian day is NOT refused -- the tzp check sits in
              the DTK_DATE arm, and `J2451187` lexes as a STRING plus a
              NUMBER, which takes the DTK_NUMBER arm instead. The
              oracle agrees: select 'J2451187'::date => 1999-01-08"
      (is (= [1999 1 8] ((juxt :year :mon :mday) (dt "J2451187" nz)))))
    (testing "while the zone literals are fine when the caller takes one"
      (is (map? (dt "2001-02-03 04:05:06+05")))
      (is (map? (dt "2001-02-03 04:05:06 PST"))))))

(deftest the-zone-is-recorded-but-not-resolved-during-the-walk
  ;; An offset is a function of the INSTANT, so an abbreviation, a zone
  ;; name and the session default all need the validated date. The walk
  ;; records which zone; resolution happens after.
  (is (= {:kind :offset :west -19800} (:zone (dt "2001-02-03 04:05:06+05:30"))))
  (is (= {:kind :offset :west 28800} (:zone (dt "2001-02-03 04:05:06 PST")))
      "an abbreviation is a FIXED offset, resolved from the table")
  (is (= {:kind :named :name "america/new_york"}
         (:zone (dt "2000-01-01 12:00:00 America/New_York"))))
  (is (= {:kind :named :name "pst8pdt"}
         (:zone (dt "2000-01-01 12:00:00 PST8PDT")))
      "PST8PDT is a zone NAME, not an abbreviation -- it observes DST")
  (is (= {:kind :session} (:zone (dt "2001-02-03 04:05:06")))))

(deftest the-reserved-words-read-the-clock
  (is (= [2026 10 3] (ymd "today")))
  (is (= [2026 10 4] (ymd "tomorrow")))
  (is (= [2026 10 2] (ymd "yesterday")))
  (is (= :epoch (:dtype (dt "epoch"))))
  (is (= :late (:dtype (dt "infinity"))))
  (is (= :early (:dtype (dt "-infinity"))))
  (is (= :late (:dtype (dt "+infinity")))))
