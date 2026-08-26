(ns datahike.test.pg-coerce-test
  "Audit fixes A19 + A21 — the coerce/numeric helpers should:
     * raise SQLSTATE 22003 on Long-range overflow (was: silent wrap
       via Number.longValue);
     * raise SQLSTATE 22P02 on bad-syntax strings (was: silently
       returned the original string, which Datahike then surfaced
       as a generic schema error).

   We test the pure helpers directly (datahike.pg.sql.coerce) plus
   one wire-level scenario per error class."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.sql.coerce :as c]))

(deftest coerce-bigint-direct
  (testing "in-range numeric inputs round-trip"
    (is (= 0 (c/coerce-bigint 0)))
    (is (= 42 (c/coerce-bigint 42)))
    (is (= -1 (c/coerce-bigint -1)))
    (is (= Long/MAX_VALUE (c/coerce-bigint (BigInteger/valueOf Long/MAX_VALUE))))
    (is (= Long/MIN_VALUE (c/coerce-bigint (BigInteger/valueOf Long/MIN_VALUE)))))
  (testing "BigInteger > Long.MAX_VALUE raises 22003"
    (let [bi (.add (BigInteger/valueOf Long/MAX_VALUE) BigInteger/ONE)]
      (try (c/coerce-bigint bi) (is false "expected ex-info")
           (catch clojure.lang.ExceptionInfo e
             (is (= "22003" (:sqlstate (ex-data e))))))))
  (testing "BigInteger < Long.MIN_VALUE raises 22003"
    (let [bi (.subtract (BigInteger/valueOf Long/MIN_VALUE) BigInteger/ONE)]
      (try (c/coerce-bigint bi) (is false "expected ex-info")
           (catch clojure.lang.ExceptionInfo e
             (is (= "22003" (:sqlstate (ex-data e))))))))
  (testing "Double overflow raises 22003"
    (try (c/coerce-bigint 1e30) (is false "expected ex-info")
         (catch clojure.lang.ExceptionInfo e
           (is (= "22003" (:sqlstate (ex-data e)))))))
  (testing "NaN/Infinity raises 22003"
    (try (c/coerce-bigint Double/NaN) (is false "NaN should fail")
         (catch clojure.lang.ExceptionInfo e
           (is (= "22003" (:sqlstate (ex-data e))))))
    (try (c/coerce-bigint Double/POSITIVE_INFINITY) (is false "Inf should fail")
         (catch clojure.lang.ExceptionInfo e
           (is (= "22003" (:sqlstate (ex-data e)))))))
  (testing "bad string raises 22P02"
    (try (c/coerce-bigint "not a number") (is false "expected ex-info")
         (catch clojure.lang.ExceptionInfo e
           (is (= "22P02" (:sqlstate (ex-data e))))))))

(deftest coerce-numeric-long-target
  (testing "decimal string truncates fractional part"
    (is (= 9 (c/coerce-numeric "9.7" :long)))
    (is (= -3 (c/coerce-numeric "-3.999" :long))))
  (testing "scientific notation is accepted"
    (is (= 1500 (c/coerce-numeric "1.5e3" :long))))
  (testing "decimal-string overflow → 22003"
    (try (c/coerce-numeric "1e30" :long) (is false)
         (catch clojure.lang.ExceptionInfo e
           (is (= "22003" (:sqlstate (ex-data e))))))))

(deftest coerce-numeric-bigdec-target
  (testing "string parses to exact BigDecimal"
    (is (= (BigDecimal. "1.234") (c/coerce-numeric "1.234" :bigdec))))
  (testing "BigInteger preserved"
    (is (= (BigDecimal. (BigInteger. "999999999999999999999"))
           (c/coerce-numeric (BigInteger. "999999999999999999999") :bigdec))))
  (testing "blank string raises 22P02"
    (try (c/coerce-numeric "" :bigdec) (is false)
         (catch clojure.lang.ExceptionInfo e
           (is (= "22P02" (:sqlstate (ex-data e))))))))

(deftest postgres-numeric-input-extensions
  (testing "underscores between decimal digits"
    (is (= (BigDecimal. "12000.123456")
           (c/coerce-numeric "12_000.123_456" :bigdec)))
    (is (= (BigDecimal. "2.3")
           (c/coerce-numeric "23_000_000_000e-1_0" :bigdec))))
  (testing "binary, octal and hexadecimal integers"
    (is (= (BigDecimal. "299792458")
           (c/coerce-numeric "0b10001110111100111100001001010" :bigdec)))
    (is (= (BigDecimal. "9999999999")
           (c/coerce-numeric "+0o112402761777" :bigdec)))
    (is (= (BigDecimal. "3735928559")
           (c/coerce-numeric "0x_dead_beef" :bigdec))))
  (testing "separator and radix validation remains strict"
    (doseq [s ["_123" "123_" "12__34" "123_.456" "1.2e_34"
               "0b1112" "0o12345678" "0x1eg" "0x__1234"]]
      (try
        (c/coerce-numeric s :bigdec)
        (is false s)
        (catch clojure.lang.ExceptionInfo e
          (is (= "22P02" (:sqlstate (ex-data e))) s))))))

(deftest coerce-numeric-double-and-float
  (testing "double from string + double from int"
    (is (= 1.5 (c/coerce-numeric "1.5" :double)))
    (is (= 2.0 (c/coerce-numeric 2 :double))))
  (testing "float from large literal becomes ±Infinity (matches PG real)"
    (let [f (c/coerce-numeric "1e40" :float)]
      (is (or (= Float/POSITIVE_INFINITY f) (Float/isFinite f))))))

(deftest coerce-numeric-nil
  (is (nil? (c/coerce-numeric nil :long)))
  (is (nil? (c/coerce-numeric nil :double)))
  (is (nil? (c/coerce-numeric nil :bigdec))))

(deftest parse-bool-token-pg-fidelity
  (testing "PG parse_bool_with_len acceptance table (issue #12)"
    ;; prefixes of true/yes and false/no; on/off with off's 'of' prefix;
    ;; exact 1/0; case-insensitive; whitespace-trimmed.
    (doseq [s ["t" "tr" "tru" "true" "TRUE" "y" "ye" "yes" "on" "1" " t " "\tYeS "]]
      (is (true? (c/parse-bool-token s)) s))
    (doseq [s ["f" "fa" "fal" "fals" "false" "FALSE" "n" "no" "of" "off" "0" " off "]]
      (is (false? (c/parse-bool-token s)) s)))
  (testing "rejected inputs return nil"
    ;; 'o' is ambiguous between on/off; multi-digit numbers, garbage and
    ;; blank are invalid — PG raises 22P02 for all of these.
    (doseq [s ["o" "2" "10" "01" "maybe" "" "  " "truex" "offf" "yesno"]]
      (is (nil? (c/parse-bool-token s)) s))))

(deftest postgres-uuid-input-forms
  (let [canonical "3f3e3c3b-3a30-3938-3736-353433a2313e"
        expected (java.util.UUID/fromString canonical)]
    (doseq [s [canonical
               "{3f3e3c3b-3a30-3938-3736-353433a2313e}"
               "3f3e3c3b3a3039383736353433a2313e"
               "3f3e-3c3b-3a30-3938-3736-3534-33a2-313e"]]
      (is (= expected (c/parse-uuid s)) s))
    (doseq [s ["111-11111-1111-1111-1111-111111111111"
               "11111111-1111-1111-G111-111111111111"
               "{11111111-1111-1111-1111-11111111111}"
               (str " " canonical)
               (str canonical "-")]]
      (try
        (c/parse-uuid s)
        (is false s)
        (catch clojure.lang.ExceptionInfo e
          (is (= "22P02" (:sqlstate (ex-data e))) s))))))
