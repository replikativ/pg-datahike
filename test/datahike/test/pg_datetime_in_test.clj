(ns datahike.test.pg-datetime-in-test
  "Zone resolution and the five input functions.

   The sharp edges here were all found by diffing against the oracle,
   not by reading, so each one quotes the query that settled it."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.datetime.in :as in]
            [datahike.pg.datetime.zone :as zone]))

(def ^:private ctx
  {:date-order :mdy
   :now {:tm {:year 2026 :mon 10 :mday 3 :hour 7 :min 8 :sec 9 :usec 0} :west 0}
   :session-zone (zone/resolve-zone-name "UTC")})

(defn- f [g s] (try (let [r (g s ctx)] (merge {:kind (:kind r)} (in/->fields r)
                                              (select-keys r [:west])))
                    (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))

(deftest the-ambiguity-rule-is-the-opposite-of-java-times
  ;; datetime.c:1697-1706. In a GAP take the BEFORE offset; in an
  ;; OVERLAP take the AFTER one. `ZonedDateTime/of` does the reverse in
  ;; both cases, so using it is an hour wrong twice a year, silently.
  (let [ny (zone/resolve-zone-name "America/New_York")
        off (fn [y m d h mi] (zone/determine-offset
                              {:year y :mon m :mday d :hour h :min mi :sec 0} ny))]
    (testing "unambiguous times, as a control"
      (is (= 18000 (off 2000 1 1 12 0)))
      (is (= 14400 (off 2000 7 1 12 0))))
    (testing "a GAP: 2024-03-10 02:30 never happened. PostgreSQL keeps
              the local fields and uses EST, the BEFORE offset:
                '2024-03-10 02:30:00 America/New_York'::timestamptz
                => 2024-03-10 07:30:00+00"
      (is (= 18000 (off 2024 3 10 2 30)))
      (is (= -14400 (.getTotalSeconds
                     (.getOffset (java.time.ZonedDateTime/of
                                  (java.time.LocalDateTime/of 2024 3 10 2 30) ny))))
          "java.time would take EDT instead"))
    (testing "an OVERLAP: 2024-11-03 01:30 happened twice. PostgreSQL
              takes EST, the AFTER offset:
                '2024-11-03 01:30:00 America/New_York'::timestamptz
                => 2024-11-03 06:30:00+00"
      (is (= 18000 (off 2024 11 3 1 30)))
      (is (= -14400 (.getTotalSeconds
                     (.getOffset (java.time.ZonedDateTime/of
                                  (java.time.LocalDateTime/of 2024 11 3 1 30) ny))))
          "java.time would take EDT, the earlier offset"))))

(deftest a-posix-zone-name-inverts-its-sign
  ;; In a POSIX TZ string the offset is what you ADD to local time to
  ;; reach UTC, so `GMT+8` is eight hours BEHIND UTC. Java reads it the
  ;; other way, so routing these through ZoneId is sixteen hours out
  ;; with no error.
  ;;   '2000-01-01 12:00:00 GMT+8'::timestamptz  =>  2000-01-01 20:00:00+00
  (is (= 28800 (zone/posix-offset "gmt+8")))
  (is (= 28800 (zone/posix-offset "pst+8")))
  (is (= -18000 (zone/posix-offset "utc-5")))
  (testing "Java reads the same string as EAST of Greenwich -- the
            opposite side -- so routing `gmt+8` through ZoneId lands
            sixteen hours away"
    (is (= 28800 (.getTotalSeconds
                  (.getOffset (.getRules (java.time.ZoneId/of "GMT+8"))
                              java.time.Instant/EPOCH))))
    (is (= 28800 (zone/posix-offset "gmt+8"))
        "ours is seconds WEST, so the same number means the other side"))
  (testing "and it does not shadow real zone names, which have a second
            alpha run or a slash and go to tzdb instead"
    (is (nil? (zone/posix-offset "pst8pdt")))
    (is (nil? (zone/posix-offset "est5edt")))
    (is (nil? (zone/posix-offset "etc/gmt+5")))
    (is (some? (zone/resolve-zone-name "pst8pdt")))))

(deftest all-five-input-functions-accept-a-zone
  ;; `date_in` passes `&tzp` (date.c:134) and `time_in` passes `&tz`
  ;; (date.c:1399) -- every input function takes a zone and the ones
  ;; with nowhere to put it discard it. I had date and time REFUSING,
  ;; on the assumption that a type without a zone would not take one.
  ;;   '2000-01-01 12:00:00 PST'::date  =>  2000-01-01
  ;;   '2000-01-01 12:00:00 PST'::time  =>  12:00:00
  (is (= {:kind :date :year 2000 :mon 1 :mday 1}
         (f in/date-in "2000-01-01 12:00:00 PST")))
  (is (= 12 (:hour (f in/time-in "2000-01-01 12:00:00 PST"))))
  (testing "but date_in still runs the whole timestamp decoder, so the
            time it then discards is validated first:
              '2001-02-03 25:00:00'::date  =>  22008"
    (is (= :datetime-field-overflow (f in/date-in "2001-02-03 25:00:00")))
    (is (= {:kind :date :year 2001 :mon 2 :mday 3}
           (f in/date-in "2001-02-03 23:00:00")))))

(deftest normalization-falls-out-of-the-canonical-form
  ;; `tm2timestamp` is `date * USECS_PER_DAY + time2t(...)` with no
  ;; special cases, so hour 24 and second 60 carry into the day.
  (is (= {:year 2001 :mon 2 :mday 4 :hour 0 :min 0 :sec 0 :usec 0 :kind :timestamp}
         (f in/timestamp-in "2001-02-03 24:00:00")))
  (is (= {:year 2001 :mon 2 :mday 4 :hour 0 :min 0 :sec 0 :usec 0 :kind :timestamp}
         (f in/timestamp-in "2001-02-03 23:59:60")))
  (testing "a TIME has no day to carry into, so 24:00:00 stands"
    (is (= 24 (:hour (f in/time-in "24:00:00"))))
    (is (= 24 (:hour (f in/time-in "23:59:60")))))
  (testing "microseconds survive -- the parser is not the lossy step"
    (is (= 123456 (:usec (f in/timestamp-in "2001-02-03 04:05:06.123456"))))
    (is (= 123456 (:usec (f in/time-in "04:05:06.123456"))))))

(deftest the-three-overflow-sqlstates-are-distinct
  ;; Routinely got wrong: a zone past +/-15 is 22009, not 22008.
  (is (= :invalid-tz-displacement (f in/timestamptz-in "2001-02-03 04:05:06+16")))
  (is (= :invalid-tz-displacement (f in/timestamptz-in "2001-02-03 04:05:06-16")))
  (is (= :datetime-field-overflow (f in/date-in "1997-02-29")))
  (is (= :datetime-field-overflow (f in/date-in "1997-13-01")))
  (is (= :invalid-datetime-format (f in/date-in "garbage")))
  (testing "and a zone that is not a zone fails differently depending on
            how it LEXED. A DATE field gives 22023 and a STRING gives
            22007 -- datetime.c:1109 against :1434:
              'Feb-10-1997'::time              =>  22023
              '2000-01-01 12:00:00 IDLW'::date =>  22007"
    (is (= :invalid-parameter-value (f in/time-in "Feb-10-1997")))
    (is (= :invalid-datetime-format (f in/date-in "2000-01-01 12:00:00 IDLW"))))
  (testing "and a date past the end of the range is caught before the
            multiply overflows a long, not after"
    (is (= :datetime-field-overflow (f in/timestamp-in "5874897-12-31")))))

(deftest a-timestamp-parses-its-zone-and-then-discards-it
  ;; The zone must still be VALIDATED even though it is thrown away,
  ;; which is why `pg_tzset` runs during the walk rather than at
  ;; resolution. Otherwise an unknown word is silently ignored.
  (is (= :invalid-datetime-format (f in/timestamp-in "2001-02-03 garbage")))
  (is (= :invalid-datetime-format (f in/timestamp-in "2001-02-03 04:05:06 banana")))
  (testing "while a real zone is parsed and the local time kept as given"
    (is (= 4 (:hour (f in/timestamp-in "2001-02-03 04:05:06+05:30:30"))))
    (is (= 6 (:sec (f in/timestamp-in "2001-02-03 04:05:06+05:30:30"))))))

(deftest an-abbreviation-is-a-fixed-offset-and-a-name-is-a-rule
  ;; The distinction the token tables exist for.
  ;; `timestamptz` folds the offset INTO the value, so the offset shows
  ;; up as the instant rather than as a field: noon PST is 20:00 UTC.
  (is (= {:year 2000 :mon 1 :mday 1 :hour 20 :min 0 :sec 0 :usec 0}
         (in/->fields (in/timestamptz-in "2000-01-01 12:00:00 PST" ctx))))
  (is (= {:year 2000 :mon 7 :mday 1 :hour 20 :min 0 :sec 0 :usec 0}
         (in/->fields (in/timestamptz-in "2000-07-01 12:00:00 PST" ctx)))
      "PST is -08:00 in July too -- it is an abbreviation, not a zone")
  (let [jan (in/timestamptz-in "2000-01-01 12:00:00 PST8PDT" ctx)
        jul (in/timestamptz-in "2000-07-01 12:00:00 PST8PDT" ctx)]
    (is (not= (mod (:usec jan) 86400000000) (mod (:usec jul) 86400000000))
        "PST8PDT is a zone NAME and does observe DST")))

(deftest a-time-with-a-zone-needs-a-date-unless-the-zone-is-fixed
  ;; An offset is a function of the instant, so a time alone can only
  ;; take a zone whose answer does not depend on the date.
  (is (= 0 (:west (in/timetz-in "12:00:00 UTC" ctx))))
  (is (= 28800 (:west (in/timetz-in "12:00:00 PST" ctx))))
  (is (= -19800 (:west (in/timetz-in "12:00:00+05:30" ctx)))))
