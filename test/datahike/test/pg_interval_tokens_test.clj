(ns datahike.test.pg-interval-tokens-test
  "`deltatktbl` and `DecodeUnits`, against the pinned PostgreSQL 17.7.

   Transcribed data, so these check the transcription and the
   invariants the lookup relies on. The prefix rule in particular is
   asserted rather than assumed: it is the one place where a plain map
   lookup differs from `datebsearch`, and the datetime tables' own
   test asserts the OPPOSITE property for them."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.datetime.tokens :as dt]
            [datahike.pg.interval.tokens :as it]))

(deftest deltatktbl-is-transcribed-whole
  (is (= 61 (count it/deltatktbl)))
  (is (= {:units 59 :ago 1 :ignore-dtf 1}
         (frequencies (map first (vals it/deltatktbl)))))
  (testing "the fifteen macro-spelled tokens resolved to their text.
            Transcribing them as the macro NAMES would stop `day`,
            `hour`, `month`, `year`, `ago` and ten more parsing at all."
    (is (= [:ago 0] (it/decode-units "ago")))
    (is (= [:units :day] (it/decode-units "day")))
    (is (= [:units :hour] (it/decode-units "hour")))
    (is (= [:units :month] (it/decode-units "month")))
    (is (= [:units :year] (it/decode-units "year")))
    (is (= [:units :week] (it/decode-units "week")))
    (is (= [:units :decade] (it/decode-units "decade")))
    (is (= [:units :century] (it/decode-units "century")))
    (is (= [:units :millennium] (it/decode-units "millennium")))
    (is (= [:units :second] (it/decode-units "second")))
    (is (= [:units :minute] (it/decode-units "minute")))
    (is (= [:units :quarter] (it/decode-units "quarter")))
    (is (= [:units :microsec] (it/decode-units "usecond")))
    (is (= [:units :millisec] (it/decode-units "msecond")))
    (is (= [:units :tz] (it/decode-units "timezone"))))
  (testing "the one IGNORE_DTF token, the relative prefix"
    (is (= [:ignore-dtf 0] (it/decode-units "@")))))

(deftest the-ten-character-keys-are-real-prefixes
  ;; `datebsearch` compares `strncmp(key, token, TOKMAXLEN)` with
  ;; TOKMAXLEN = 10, so an entry of exactly ten characters matches any
  ;; input beginning with it. FIVE keys here are ten characters -- one
  ;; more than I first counted -- and the oracle confirms each:
  ;;   '1 millenniumXYZ'::interval   => 1000 years
  ;;   '2 microsecondsXX'::interval  => 00:00:00.000002
  ;;   '1 millisecondXYZ'::interval  => 00:00:00.001
  (is (= 10 it/tokmaxlen))
  (is (= #{"microsecon" "millennium" "millisecon" "timezone_h" "timezone_m"}
         (into #{} (filter #(= 10 (count %))) (keys it/deltatktbl))))
  (testing "a ten-character entry prefix-matches"
    (is (= [:units :millennium] (it/decode-units "millenniumxyz")))
    (is (= [:units :microsec] (it/decode-units "microsecondsxx")))
    (is (= [:units :millisec] (it/decode-units "millisecondxyz")))
    (is (= [:units :tz-hour] (it/decode-units "timezone_hxx"))))
  (testing "a SHORTER entry cannot, because its NUL terminator falls
            inside the ten compared bytes -- which is why `days` has to
            be its own entry rather than matching `day`"
    (is (nil? (it/decode-units "dayx")))
    (is (nil? (it/decode-units "hourx")))
    (is (= [:units :day] (it/decode-units "days")))
    (is (= [:units :hour] (it/decode-units "hours"))))
  (testing "and a plain map lookup is therefore NOT equivalent here,
            where it is for both datetime tables"
    (is (dt/prefix-safe? dt/datetktbl))
    (is (dt/prefix-safe? dt/zone-abbrevs))
    (is (not (dt/prefix-safe? it/deltatktbl)))))

(deftest the-three-tables-disagree-on-purpose
  ;; `DecodeInterval`'s DTK_STRING arm tries DecodeUnits and then falls
  ;; back to DecodeSpecial, so both routes are reachable and the
  ;; disagreement is observable.
  (testing "`m` is MINUTE here and MONTH in datetktbl"
    (is (= [:units :minute] (it/decode-units "m")))
    (is (= [:units :month] (dt/special-token "m"))))
  (testing "`mm` is only in datetktbl, so it resolves only via the
            fallback -- and the oracle agrees `'1 mm'::interval` is
            00:01:00, a MINUTE, because datetktbl's `mm` is DTK_MINUTE"
    (is (nil? (it/decode-units "mm")))
    (is (= [:units :minute] (it/unit-or-special "mm"))))
  (testing "`j` likewise resolves only via the fallback -- though
            DTK_JULIAN has no DecodeInterval arm either, so
            `'1 j'::interval` is 22007 on the oracle. Resolving is not
            the same as being legal; see the test below."
    (is (nil? (it/decode-units "j")))
    (is (= [:units :julian] (it/unit-or-special "j"))))
  (testing "the fallback must not be merged or reordered: consulting
            datetktbl first would make `m` a month"
    (is (= [:units :minute] (it/unit-or-special "m")))))

(deftest units-that-resolve-and-are-still-errors
  ;; DTK_QUARTER and the three timezone units have no arm in
  ;; DecodeInterval's switch, so they reach its `default` and raise
  ;; 22007 even though the lookup succeeds. A decoder that trusts this
  ;; table would accept all of them. Verified on the oracle:
  ;;   '1 qtr'::interval            => 22007
  ;;   '1 timezone_hXX'::interval   => 22007
  (is (= [:units :quarter] (it/decode-units "qtr")))
  (is (= [:units :quarter] (it/decode-units "quarter")))
  (is (= [:units :tz] (it/decode-units "timezone")))
  (is (= [:units :tz-hour] (it/decode-units "timezone_hour")))
  (is (= [:units :tz-minute] (it/decode-units "timezone_minute"))))

(deftest keys-are-lowercase
  (is (every? #(= % (dt/ascii-lower %)) (keys it/deltatktbl))))
