(ns datahike.test.pg-datetime-tokens-test
  "The two token tables, against the pinned PostgreSQL 17.7.

   These are transcribed data, so the tests check the transcription and
   the INVARIANTS the lookup relies on -- not behaviour. An entry count
   that drifts after a PostgreSQL bump should fail here, loudly, rather
   than show up as one literal quietly parsing differently."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.datetime.tokens :as tok]))

(deftest datetktbl-is-transcribed-whole
  ;; datetime.c:105-179. 72 entries, and the type distribution is a
  ;; cheap fingerprint of the whole table.
  (is (= 72 (count tok/datetktbl)))
  (is (= {:month 24 :dow 18 :units 13 :reserv 9 :ignore-dtf 2
          :ampm 2 :adbc 2 :isotime 1 :dtzmod 1}
         (frequencies (map first (vals tok/datetktbl)))))
  (testing "the nine tokens whose C spelling is a MACRO resolved to their
            text -- transcribing them as the macro names would stop every
            one of these words parsing"
    (is (= [:reserv :late] (tok/special-token "infinity")))
    (is (= [:reserv :late] (tok/special-token "+infinity")))
    (is (= [:reserv :early] (tok/special-token "-infinity")))
    (is (= [:reserv :epoch] (tok/special-token "epoch")))
    (is (= [:reserv :now] (tok/special-token "now")))
    (is (= [:reserv :today] (tok/special-token "today")))
    (is (= [:reserv :tomorrow] (tok/special-token "tomorrow")))
    (is (= [:reserv :yesterday] (tok/special-token "yesterday")))
    (is (= [:adbc :ad] (tok/special-token "ad")))
    (is (= [:adbc :bc] (tok/special-token "bc"))))
  (testing "months and weekdays carry their NUMBER"
    (is (= [:month 1] (tok/special-token "jan")))
    (is (= [:month 12] (tok/special-token "december")))
    (is (= [:dow 1] (tok/special-token "monday"))))
  (testing "the two IGNORE_DTF tokens, which never touch fmask"
    (is (= :ignore-dtf (first (tok/special-token "at"))))
    (is (= :ignore-dtf (first (tok/special-token "on")))))
  (testing "and `dst`, the DTZMOD that modifies a zone rather than being one"
    (is (= [:dtzmod 3600] (tok/special-token "dst")))))

(deftest the-abbreviation-table-is-a-different-table
  ;; datetime.c:100-103: "The static table contains no TZ, DTZ, or DYNTZ
  ;; entries; rather those are loaded from configuration files." The
  ;; abbreviations come from src/timezone/tznames/Default.
  (is (= 195 (count tok/zone-abbrevs)))
  (is (= {:tz 97 :dtz 48 :dyntz 50}
         (frequencies (map :type (vals tok/zone-abbrevs)))))
  (testing "an abbreviation is a FIXED OFFSET -- `pst` is -08:00 in July
            as well as in January, which is the whole reason this table
            exists rather than a ZoneId/of lookup"
    (is (= {:type :tz :offset -28800} (tok/zone-abbrev "pst")))
    (is (= {:type :dtz :offset -25200} (tok/zone-abbrev "pdt")))
    (is (= {:type :tz :offset 0} (tok/zone-abbrev "utc")))
    (is (= {:type :tz :offset -18000} (tok/zone-abbrev "est")))
    (is (= {:type :dtz :offset -14400} (tok/zone-abbrev "edt"))))
  (testing "a DYNTZ names a zone to resolve at the instant, rather than
            carrying an offset of its own"
    (is (= {:type :dyntz :zone "America/Argentina/Buenos_Aires"}
           (tok/zone-abbrev "art")))
    (is (= {:type :dyntz :zone "America/Santiago"} (tok/zone-abbrev "clt")))
    (is (every? #(and (:zone %) (nil? (:offset %)))
                (filter #(= :dyntz (:type %)) (vals tok/zone-abbrevs)))))
  (testing "the two tables do not collide, so the abbrev-first precedence
            at datetime.c:1304-1309 is currently a no-op -- it stops being
            one as soon as timezone_abbreviations is settable, which is
            why the order is written down"
    (is (empty? (filter (set (keys tok/datetktbl)) (keys tok/zone-abbrevs))))))

(deftest a-map-lookup-is-equivalent-to-datebsearch
  ;; `datebsearch` (datetime.c:4153-4187) compares with
  ;; `strncmp(key, token, TOKMAXLEN)` where TOKMAXLEN is 10 -- a PREFIX
  ;; match, which is how `millisecond` and `milliseconds` both reach the
  ;; one "millisecon" entry. A map lookup is not equivalent in general;
  ;; it is equivalent for THESE tables only because no key reaches ten
  ;; characters. That is a property of the DATA, so it is asserted.
  (is (tok/prefix-safe? tok/datetktbl))
  (is (tok/prefix-safe? tok/zone-abbrevs))
  (is (= 9 (apply max (map count (keys tok/datetktbl)))))
  (is (= 6 (apply max (map count (keys tok/zone-abbrevs))))))

(deftest keys-are-lowercase-and-the-fold-is-ascii-only
  ;; `ParseDateTime` lowercases every alpha run with `pg_tolower`, which
  ;; is ASCII-only (pgstrcasecmp.c:122-129), and tzparser.c:79-83
  ;; lowercases every abbreviation with the comment "must match
  ;; datetime.c's conversion".
  (is (every? #(= % (tok/ascii-lower %)) (keys tok/datetktbl)))
  (is (every? #(= % (tok/ascii-lower %)) (keys tok/zone-abbrevs)))
  (is (= "pst8pdt" (tok/ascii-lower "PST8PDT")))
  (testing "digits and punctuation are untouched"
    (is (= "gmt+8" (tok/ascii-lower "GMT+8"))))
  (testing "and a non-ASCII character is left alone, where
            clojure.string/lower-case would fold it by locale"
    (is (= "İ" (tok/ascii-lower "İ")))))
