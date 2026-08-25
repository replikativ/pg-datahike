(ns datahike.test.pg-numeric-width-test
  "Declared numeric widths were advertised but never enforced.

   The catalog tracked `smallint` / `integer` / `numeric(5,2)` correctly
   and RowDescription reported the right OIDs -- but the value path
   ignored all of it, so a column could hold values that do not fit the
   type it claims:

     CREATE TABLE t (s smallint);
     INSERT INTO t VALUES (100000);   -- stored; PostgreSQL raises 22003

   That is persistent corruption, not a display problem: a client sized
   for int2 reads garbage. Casts had the same hole (`99999999999::int4`
   passed through), and PostgreSQL's rounding rules were missing on both
   paths -- it ROUNDS on the way to an integer rather than truncating,
   and rounds float and numeric sources DIFFERENTLY.

   Expectations here are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn width-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"w" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each width-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/w?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE t (id int primary key, s smallint, i integer, b bigint, p numeric(5,2))")
  (exec! c "INSERT INTO t VALUES (1, 10, 20, 30, 1.25)"))

(deftest casts-range-check-the-target-width
  (with-open [c (jdbc)]
    (testing "every integer target used to collapse to Java long, so only
              the int8 range was ever checked"
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (one c "SELECT 99999999999::int8::int4")))
      (is (thrown-with-msg? SQLException #"smallint out of range"
                            (one c "SELECT 100000::int4::int2")))
      (is (thrown-with-msg? SQLException #"smallint out of range"
                            (one c "SELECT 32768.0::float8::int2")))
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (one c "SELECT 2147483648.0::float8::int4"))))))

(deftest integer-casts-round-and-the-two-sources-round-differently
  (with-open [c (jdbc)]
    (testing "float -> int is rint, half to EVEN (float.c dtoi4)"
      (is (= "0" (one c "SELECT 0.5::float8::int4")))
      (is (= "2" (one c "SELECT 1.5::float8::int4")))
      (is (= "2" (one c "SELECT 2.5::float8::int4")))
      (is (= "4" (one c "SELECT 3.5::float8::int4")))
      (is (= "-2" (one c "SELECT (-1.5)::float8::int4"))))
    (testing "numeric -> int is half AWAY FROM ZERO (numeric.c round_var)
              -- the same input gives a different answer, which is not a
              detail we get to smooth over"
      (is (= "1" (one c "SELECT 0.5::numeric::int4")))
      (is (= "2" (one c "SELECT 1.5::numeric::int4")))
      (is (= "3" (one c "SELECT 2.5::numeric::int4")))
      (is (= "-3" (one c "SELECT (-2.5)::numeric::int4"))))))

(deftest numeric-typmod-applies-on-cast
  (with-open [c (jdbc)]
    (is (= "123.5" (one c "SELECT cast(123.456 as numeric(10,1))")))
    (is (= "123" (one c "SELECT 123.456::numeric(10)"))
        "a modifier with no scale means scale 0")
    (is (thrown-with-msg? SQLException #"numeric field overflow"
                          (one c "SELECT 123456::numeric(5,2)"))
        "precision was decoded and then discarded, so 22003 never fired")
    (is (thrown-with-msg? SQLException #"numeric field overflow"
                          (one c "SELECT 1000::numeric(3,0)")))))

(deftest negative-scale-numeric-typmods
  (with-open [c (jdbc)]
    (testing "casts round to positions left of the decimal point"
      (is (= "12000" (one c "SELECT 12345::numeric(3,-3)")))
      (is (= "-12000" (one c "SELECT (-12345)::numeric(3,-3)")))
      (is (= "0" (one c "SELECT scale(12345::numeric(3,-3))"))
          "negative typmod scale controls rounding, not display scale"))
    (exec! c "CREATE TABLE neg_scale (n numeric(3,-3))")
    (exec! c "INSERT INTO neg_scale VALUES (12345), (654321)")
    (is (= "12000" (one c "SELECT min(n) FROM neg_scale")))
    (is (= "numeric(3,-3)"
           (one c (str "SELECT format_type(a.atttypid, a.atttypmod) "
                       "FROM pg_attribute a JOIN pg_class c ON c.oid = a.attrelid "
                       "WHERE c.relname = 'neg_scale' AND a.attname = 'n'"))))
    (is (thrown-with-msg? SQLException #"numeric field overflow"
                          (exec! c "INSERT INTO neg_scale VALUES (999500000)")))))

(deftest numeric-typmods-support-scale-greater-than-precision
  (with-open [c (jdbc)]
    (is (= "0.000123"
           (one c "SELECT 0.0001234::numeric(3,6)")))
    (is (thrown-with-msg? SQLException #"numeric field overflow"
                          (one c "SELECT 0.0009995::numeric(3,6)")))
    (is (thrown-with-msg? SQLException #"absolute value less than 1\."
                          (one c "SELECT 0.9995::numeric(3,3)"))
        "a zero limit exponent is rendered as 1, not 10^0")
    (exec! c (str "CREATE TABLE mixed_scales ("
                  "millions numeric(3,-6), thousands numeric(3,-3), "
                  "units numeric(3,0), thousandths numeric(3,3), "
                  "millionths numeric(3,6))"))
    (exec! c (str "INSERT INTO mixed_scales VALUES "
                  "(123456, 123, 0.123, 0.000123, 0.000000123)"))
    (is (= "0.000000"
           (one c "SELECT millionths FROM mixed_scales")))))

(deftest integer-literals-wider-than-int8-become-exact-numeric
  (with-open [c (jdbc)]
    (is (= "9223372036854775808"
           (one c "SELECT 9223372036854775808")))
    (is (= "999999999999999999999"
           (one c (str "SELECT mod(999999999999999999999::numeric,"
                       "1000000000000000000000)"))))
    (is (= "-9999999999999999999999"
           (one c (str "SELECT div(-9999999999999999999999::numeric,"
                       "1000000000000000000000)*1000000000000000000000 + "
                       "mod(-9999999999999999999999::numeric,"
                       "1000000000000000000000)"))))))

(deftest postgres-numeric-text-input-extensions
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE numeric_input_extensions (n numeric)")
    (exec! c (str "INSERT INTO numeric_input_extensions VALUES "
                  "('12_000.123_456'), "
                  "('0b10001110111100111100001001010'), "
                  "('+0o112402761777'), ('-0x_dead_beef')"))
    (is (= "12000.123456"
           (one c "SELECT n FROM numeric_input_extensions WHERE n = 12000.123456")))
    (is (= "299792458"
           (one c "SELECT n FROM numeric_input_extensions WHERE n = 299792458")))
    (is (thrown-with-msg? SQLException #"invalid input syntax for numeric"
                          (one c "SELECT '0x1eg'::numeric")))))

(deftest writes-enforce-the-declared-width
  (with-open [c (jdbc)]
    (seed! c)
    (testing "INSERT"
      (is (thrown-with-msg? SQLException #"smallint out of range"
                            (exec! c "INSERT INTO t (id, s) VALUES (2, 100000)")))
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (exec! c "INSERT INTO t (id, i) VALUES (3, 99999999999)")))
      (is (thrown-with-msg? SQLException #"numeric field overflow"
                            (exec! c "INSERT INTO t (id, p) VALUES (5, 1000.5)"))))
    (testing "UPDATE -- which reached the coercion by a different route and
              enforced none of it"
      (is (thrown-with-msg? SQLException #"smallint out of range"
                            (exec! c "UPDATE t SET s = 100000 WHERE id = 1"))))
    (testing "the rows that failed left nothing behind"
      (is (= "1" (one c "SELECT count(*) FROM t"))))))

(deftest writes-round-to-the-declared-scale
  (with-open [c (jdbc)]
    (seed! c)
    (exec! c "UPDATE t SET p = 7.129 WHERE id = 1")
    (is (= "7.13" (one c "SELECT p FROM t WHERE id = 1"))
        "numeric(5,2) rounds on assignment; only the INSERT translator
         used to see the declared scale")
    (exec! c "UPDATE t SET p = 7 WHERE id = 1")
    (is (= "7.00" (one c "SELECT p FROM t WHERE id = 1")) "and pads")
    (exec! c "INSERT INTO t (id, i) VALUES (9, 2.5)")
    (is (= "3" (one c "SELECT i FROM t WHERE id = 9"))
        "a write to an integer column rounds, it does not truncate")))

(deftest a-failed-update-leaves-the-row-alone
  (with-open [c (jdbc)]
    (seed! c)
    (is (thrown-with-msg? SQLException #"division by zero"
                          (exec! c "UPDATE t SET i = 1/0 WHERE id = 1"))
        "the divide-by-zero guard yielded nil, which the caller reads as
         SET col = NULL -- so this reported success and RETRACTED i")
    (is (= "20" (one c "SELECT i FROM t WHERE id = 1"))
        "and the column still holds its original value")))

(deftest update-arithmetic-uses-sql-division
  (with-open [c (jdbc)]
    (seed! c)
    (exec! c "UPDATE t SET i = 7/2 WHERE id = 1")
    (is (= "3" (one c "SELECT i FROM t WHERE id = 1"))
        "integer division, not the Ratio 7/2 that plain `/` produces")))
