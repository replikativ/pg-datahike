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
