(ns datahike.test.pg-datetime-lex-test
  "`ParseDateTime` (datetime.c:753-977), the datetime lexer.

   Pure function, so these are a table. The cases come from the rule
   table in the C's own doc comment (datetime.c:740-752) and from each
   branch of the scanner -- the point is to pin the SHAPE of the output,
   because every later decision is made on the field types and the
   decoder cannot recover from a mis-lex."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.datetime.lex :as lex]))

(defn- lx [s] (mapv (juxt :text :type) (lex/tokenize s)))

(deftest the-six-field-types
  (testing "ISO: a date and a time, two fields"
    (is (= [["2001-02-03" :date] ["04:05:06" :time]]
           (lx "2001-02-03 04:05:06"))))
  (testing "a time run eats digits, colons AND dots, so the fraction
            rides along in the one field"
    (is (= [["04:05:06.5" :time]] (lx "04:05:06.5")))
    (is (= [["04:05" :time]] (lx "04:05"))))
  (testing "a bare run of digits is a NUMBER, whatever it turns out to mean"
    (is (= [["040506" :number]] (lx "040506")))
    (is (= [["1997" :number]] (lx "1997"))))
  (testing "a word is a STRING -- months and zone abbreviations both"
    (is (= [["feb" :string]] (lx "Feb")))
    (is (= [["pst" :string]] (lx "PST")))
    (is (= [["mon" :string] ["feb" :string] ["10" :number]
            ["17:32:01" :time] ["1997" :number] ["pst" :string]]
           (lx "Mon Feb 10 17:32:01 1997 PST"))))
  (testing "a signed number is a TZ; a signed word is a SPECIAL"
    (is (= [["+05" :tz]] (lx "+05")))
    (is (= [["+05:30:30" :tz]] (lx "+05:30:30")))
    (is (= [["-infinity" :special]] (lx "-infinity"))))
  (testing "and a word with a delimiter in it is a DATE, which is how a
            zone NAME arrives"
    (is (= [["america/new_york" :date]] (lx "America/New_York")))
    (is (= [["gmt+8" :date]] (lx "GMT+8")))
    (is (= [["etc/gmt+5" :date]] (lx "Etc/GMT+5")))))

(deftest a-zone-name-keeps-its-underscore
  ;; The DTK_DATE run includes `/ _ . :` as well as `+ -` and alnum
  ;; (datetime.c:894-897). Without `_` the field splits at the
  ;; underscore and the zone stops resolving -- `america/new` is not a
  ;; zone, and the oracle says so.
  (is (= [["america/new_york" :date]] (lx "America/New_York")))
  (is (= [["2000-01-01" :date] ["12:00:00" :time] ["america/new_york" :date]]
         (lx "2000-01-01 12:00:00 America/New_York"))))

(deftest pst8pdt-lexes-as-a-zone-name-not-an-abbreviation
  ;; A word followed by a digit is a date ONLY IF the word is not in
  ;; `datetktbl` (datetime.c:881-887). `pst` is NOT in datetktbl -- it is
  ;; in the separate abbreviation table -- so `pst8pdt` is one DATE
  ;; field and resolves through pg_tzset, which is why it observes DST
  ;; while a bare `PST` does not.
  (is (= [["pst8pdt" :date]] (lx "PST8PDT")))
  (testing "while a word that IS in datetktbl stops before the digit"
    (is (= [["jan" :string] ["8" :number]] (lx "Jan 8")))
    (is (= [["j" :string] ["2451187" :number]] (lx "J2451187")))))

(deftest the-dotted-forms-take-three-different-paths
  ;; The first `.` leaves the field a NUMBER and only a MATCHING second
  ;; delimiter promotes it to a DATE (datetime.c:810-825). Three
  ;; near-identical strings, three shapes.
  (is (= [["2000.01" :number]] (lx "2000.01")))
  (is (= [["2000.01.01" :date]] (lx "2000.01.01")))
  (is (= [["2000.01" :number] ["-01" :tz]] (lx "2000.01-01")))
  (testing "which is what keeps `yy.ddd` a NUMBER, so the day-of-year
            rule can still see it"
    (is (= [["1997.038" :number]] (lx "1997.038")))))

(deftest punctuation-is-discarded-as-a-delimiter
  ;; datetime.c:929-934. So a double-quoted zone in a LITERAL is not a
  ;; quoted anything: the quotes are punctuation and vanish, and the
  ;; field lexes exactly as the bare word. There is no quoted-zone
  ;; concept at the literal level at all.
  (is (= (lx "2000-01-01 12:00 PST") (lx "2000-01-01 12:00 \"PST\"")))
  (is (= [["2001-02-03" :date] ["04:05:06" :time]] (lx "2001-02-03, 04:05:06")))
  (testing "and the ISO `T` is a word, not a separator"
    (is (= [["2001-02-03" :date] ["t" :string] ["04:05:06" :time]]
           (lx "2001-02-03T04:05:06")))))

(deftest what-the-lexer-refuses
  (testing "a sign followed by neither a digit nor a letter"
    (is (thrown? clojure.lang.ExceptionInfo (lex/tokenize "+"))))
  (testing "a non-ASCII, non-punctuation character"
    (is (thrown? clojure.lang.ExceptionInfo (lex/tokenize "é"))))
  (testing "more than MAXDATEFIELDS fields"
    (is (thrown? clojure.lang.ExceptionInfo
                 (lex/tokenize (clojure.string/join " " (repeat 26 "1"))))))
  (testing "but an empty string lexes to nothing -- the DECODER rejects
            it, not the lexer"
    (is (= [] (lx "")))
    (is (= [] (lx "   ")))
    (is (= [] (lx "@@@")))))

(deftest alpha-runs-are-lowercased-and-digits-are-not
  (is (= [["january" :string]] (lx "JANUARY")))
  (is (= [["america/new_york" :date]] (lx "AMERICA/NEW_YORK")))
  (is (= [["2001-02-03" :date]] (lx "2001-02-03")))
  (testing "a non-ASCII letter is refused. `isalpha` in the C locale is
            false for a byte over 127, so the character falls through
            every branch to the final else. The oracle rejects the
            literal too -- `'2000-01-01 Été'::timestamp` is 22007 --
            though possibly at DECODE rather than at lex. That
            distinction is not observable from SQL, so this pins only
            that we refuse it, not where."
    (is (thrown? clojure.lang.ExceptionInfo (lex/tokenize "Été")))))
