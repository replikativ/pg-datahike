(ns datahike.test.pg-numeric-format-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.numeric-format :as fmt]
            [datahike.pg.server :as pg]
            [datahike.pg.types :as types])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)] (f))
        (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- result [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (result sql))))

(deftest numeric-picture-output
  (testing "9, 0, signs, fill, ordinal, scientific, scaling and Roman"
    (is (= "    .00" (fmt/to-char 0M "999.99")))
    (is (= "  12.30" (fmt/to-char 12.3M "999.99")))
    (is (= " -12.30" (fmt/to-char -12.3M "999.99")))
    (is (= "12.3" (fmt/to-char 12.3M "FM999.99")))
    (is (= " 0012" (fmt/to-char 12M "0000")))
    (is (= " <12>" (fmt/to-char -12M "999PR")))
    (is (= "<12>" (fmt/to-char -12M "FM999PR")))
    (is (= " +12" (fmt/to-char 12M "S999")))
    (is (= " +3" (fmt/to-char 3M "S 9")))
    (is (= " +3" (fmt/to-char 3M "FMS 9")))
    (is (= " -3" (fmt/to-char -3M "S 9")))
    (is (= "21ST" (fmt/to-char 21M "FM999TH")))
    (is (= " 1.23e+03" (fmt/to-char 1234M "9.99EEEE")))
    (is (= " 1200" (fmt/to-char 12M "99V99")))
    (is (= "            XIV" (fmt/to-char 14M "RN"))))
  (testing "float pictures honor PostgreSQL's source precision budget"
    (is (= "4200000000"
           (fmt/to-char (float 4200000000) "9999999999.99MI")))
    (is (= " ##."
           (fmt/to-char (float 4200000000) "MI99.99")))
    (is (= "##########.####"
           (fmt/to-char (double 12345678901) "FM9999999999D9999900000000000000000")))))

(deftest numeric-picture-input
  (is (= 12345.67M (fmt/to-number "12,345.67" "99G999D99")))
  (is (= -12.3M (fmt/to-number "<12.3>" "99D9PR")))
  (is (= 12.340000000000000000M (fmt/to-number "1234" "99V99")))
  (testing "the newer PostgreSQL Roman input support round-trips its domain"
    (is (every? true?
                (for [n (range 1 4000)]
                  (= (bigdec n)
                     (fmt/to-number (fmt/to-char n "FMRN") "RN")))))))

(deftest numeric-to-pg-lsn
  ;; `%X/%X`, as pg_lsn_out does -- NOT zero padded. This asserted
  ;; `0/016AE7F8`, which is our old rendering rather than PostgreSQL's:
  ;; the expectation was copied from what we produced instead of from
  ;; the oracle, so it locked the bug in. PostgreSQL 17 answers
  ;; `0/16AE7F8`, and `pg_lsn(0::numeric)` is `0/0`.
  (is (= [["0/16AE7F8" "0/0" "pg_lsn"]]
         (rows "SELECT pg_lsn(23783416::numeric),
                       pg_lsn(0::numeric),
                       pg_typeof(pg_lsn(0::numeric))")))
  (is (= [["FFFFFFFF/FFFFFFFF"]]
         (rows "SELECT pg_lsn(18446744073709551615::numeric)")))
  (doseq [[sql message state]
          [["SELECT pg_lsn(-1::numeric)" "pg_lsn out of range" "22023"]
           ["SELECT pg_lsn(18446744073709551616::numeric)" "pg_lsn out of range" "22023"]
           ["SELECT pg_lsn('NaN'::numeric)" "cannot convert NaN to pg_lsn" "0A000"]]]
    (let [e (.-error ^PgWireServer$QueryResult (result sql))]
      (is (re-find (re-pattern message) e))
      (is (= state (.-sqlstate ^PgWireServer$QueryResult (result sql))))))
  (is (= types/oid-pg-lsn
         (types/infer-oid-from-value
          (types/pg-lsn java.math.BigInteger/ZERO)))))

(deftest pg-lsn-arithmetic
  ;; `lsn - lsn` is the NUMERIC distance between two WAL addresses;
  ;; `lsn ± numeric` moves an address. Both reached generic JVM
  ;; arithmetic, which was handed the `X/Y` text and raised
  ;; `class java.lang.String cannot be cast to java.lang.Number` at the
  ;; client -- an internal failure for an operator PostgreSQL has.
  ;; Every case below is PostgreSQL 17's, including the messages.
  (is (= [["-1" "1"]]
         (rows "SELECT '0/16AE7F7'::pg_lsn - '0/16AE7F8'::pg_lsn,
                       '0/16AE7F8'::pg_lsn - '0/16AE7F7'::pg_lsn")))
  (testing "addition is commutative, subtraction of a count is not"
    (is (= [["0/16AE807" "0/16AE807" "0/16AE7E7"]]
           (rows "SELECT '0/16AE7F7'::pg_lsn + 16::numeric,
                         16::numeric + '0/16AE7F7'::pg_lsn,
                         '0/16AE7F7'::pg_lsn - 16::numeric"))))
  (testing "the ends of the range hold, and going past them is an error"
    (is (= [["FFFFFFFF/FFFFFFFF" "0/0"]]
           (rows "SELECT 'FFFFFFFF/FFFFFFFE'::pg_lsn + 1::numeric,
                         '0/1'::pg_lsn - 1::numeric")))
    (doseq [sql ["SELECT 'FFFFFFFF/FFFFFFFE'::pg_lsn + 2::numeric"
                 "SELECT '0/1'::pg_lsn - 2::numeric"]]
      (is (re-find #"pg_lsn out of range"
                   (.-error ^PgWireServer$QueryResult (result sql))) sql)))
  (testing "NaN names the operator, as PostgreSQL does"
    (is (re-find #"cannot add NaN to pg_lsn"
                 (.-error ^PgWireServer$QueryResult
                  (result "SELECT '0/16AE7F7'::pg_lsn + 'NaN'::numeric"))))
    (is (re-find #"cannot subtract NaN from pg_lsn"
                 (.-error ^PgWireServer$QueryResult
                  (result "SELECT '0/16AE7F7'::pg_lsn - 'NaN'::numeric")))))
  (testing "a round trip through the whole range"
    (is (= [["FFFFFFFF/FFFFFFFF" "0/0"]]
           (rows "SELECT '0/0'::pg_lsn + ('FFFFFFFF/FFFFFFFF'::pg_lsn - '0/0'::pg_lsn),
                         'FFFFFFFF/FFFFFFFF'::pg_lsn
                           - ('FFFFFFFF/FFFFFFFF'::pg_lsn - '0/0'::pg_lsn)")))))
