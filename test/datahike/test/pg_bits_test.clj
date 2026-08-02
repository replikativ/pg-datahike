(ns datahike.test.pg-bits-test
  "BIT / BIT VARYING values (issue #19).

   A bit value used to be a bare String of '0'/'1' characters. The digits
   were right, but the type reported as `text` (OID 25) instead of `bit`
   (1560), so `pg_typeof(0::bit)` answered `text`.

   The width is part of the value — PG compares bit strings with a memcmp
   of the left-aligned bytes and then breaks ties on length (varbit.c:817)
   — so the wrapper carries it, and `B'101'` is NOT equal to `B'10100000'`.

   Every expectation below was captured from PostgreSQL 17.10."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.bits :as b]
            [datahike.pg.errors :as errors]
            [datahike.pg.types :as types]))

(defn- txt [x] (b/to-pg-text x))

(deftest test-type-identity
  (testing "a bit value reports as bit / bit varying, not text"
    (is (= 1560 (types/infer-oid-from-value (b/parse-bit-literal "101"))))
    (is (= 1562 (types/infer-oid-from-value (b/parse-bit-literal "101" true))))
    (is (= "bit" (get types/oid->pg-name 1560)))
    (is (= "bit varying" (get types/oid->pg-name 1562)))))

(deftest test-text-io
  (testing "output is always the digit run — never hex, never a B prefix"
    (is (= "1010" (txt (b/parse-bit-literal "1010"))))
    (is (= "1010" (txt (b/parse-bit-literal "b1010"))))
    (is (= "1010" (txt (b/parse-bit-literal "B1010")))))

  (testing "hex expands to FOUR bits per digit, leading zeros included"
    ;; X'1F' is an 8-bit value, not 5. Getting this wrong changes
    ;; length(), octet_length() and the sort position.
    (is (= "00011111" (txt (b/parse-bit-literal "x1F"))))
    (is (= "000111111111" (txt (b/parse-bit-literal "X1FF"))))
    (is (= 8 (b/width (b/parse-bit-literal "x1F")))))

  (testing "an invalid digit is 22P02 with PG's wording"
    (doseq [[input kind] [["102" "binary"] ["xZZ" "hexadecimal"]]]
      (let [e (try (b/parse-bit-literal input) nil (catch Exception e e))]
        (is (some? e))
        (is (= "22P02" (first (errors/classify-exception e))))
        (is (= (str "\"" (if (= kind "binary") "2" "Z") "\" is not a valid " kind " digit")
               (second (errors/classify-exception e))))))))

(deftest test-width-is-part-of-the-value
  (testing "different widths are never equal, even with the same digits"
    (is (not= (b/parse-bit-literal "101") (b/parse-bit-literal "10100000"))))

  (testing "ordering is content first, then width"
    ;; B'0' < B'00' < B'000' and B'1' > B'0111111'.
    (is (neg? (b/compare-bits (b/parse-bit-literal "0") (b/parse-bit-literal "00"))))
    (is (neg? (b/compare-bits (b/parse-bit-literal "00") (b/parse-bit-literal "000"))))
    (is (pos? (b/compare-bits (b/parse-bit-literal "1") (b/parse-bit-literal "0111111")))))

  (testing "length / octet_length"
    (is (= 3 (b/width (b/parse-bit-literal "101"))))
    (is (= 2 (b/octet-length (b/parse-bit-literal "1011111011"))))))

(deftest test-width-coercion
  (testing "bit(n) zero-pads on the RIGHT, and truncates on the right"
    (is (= "10000" (txt (b/coerce-width (b/parse-bit-literal "10") 5 true))))
    (is (= "10101111101"
           (txt (b/coerce-width (b/parse-bit-literal "101011111010") 11 true)))))

  (testing "bit varying(n) truncates but NEVER pads"
    (is (= "10" (txt (b/coerce-width (b/parse-bit-literal "10" true) 5 true))))
    (is (= "10101111101"
           (txt (b/coerce-width (b/parse-bit-literal "101011111010" true) 11 true)))))

  (testing "an implicit (assignment) coercion raises instead of reshaping"
    (let [e (try (b/coerce-width (b/parse-bit-literal "10") 5 false) nil
                 (catch Exception e e))]
      (is (= ["22026" "bit string length 2 does not match type bit(5)"]
             (vec (take 2 (errors/classify-exception e))))))
    (let [e (try (b/coerce-width (b/parse-bit-literal "101011111010" true) 11 false) nil
                 (catch Exception e e))]
      (is (= ["22001" "bit string too long for type bit varying(11)"]
             (vec (take 2 (errors/classify-exception e))))))))

(deftest test-integer-conversions
  (testing "int -> bit(n) keeps the RIGHTMOST n bits"
    (is (= "1101" (txt (b/from-integer 29 4))))
    (is (= "0000101100" (txt (b/from-integer 44 10))))
    ;; bare `bit` means bit(1), i.e. only the least significant bit
    (is (= "1" (txt (b/from-integer 29 1)))))

  (testing "int -> bit(n) sign-extends on the left for negatives"
    ;; Rendering the binary string and padding the wrong end gives the
    ;; wrong bits for every negative value.
    (is (= "111111010100" (txt (b/from-integer -44 12)))))

  (testing "bit -> int reinterprets the bits, it does not range-check"
    (is (= 5 (b/to-long (b/parse-bit-literal "101"))))
    (is (= 4294967295 (b/to-long (b/parse-bit-literal (apply str (repeat 32 \1))))))
    (is (= -1 (b/to-long (b/parse-bit-literal (apply str (repeat 64 \1))))))))
