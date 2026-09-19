(ns datahike.test.pg-input-test
  "Scalar input functions (datahike.pg.input) and the paths that use them.

   `oracle-cases` is PostgreSQL 17.7's answer to `SELECT (?::text)::<type>::text`
   for each input: the text it prints, or the message and SQLSTATE it
   raises. It was generated from the oracle, not from this code."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.errors :as errors]
            [datahike.pg.input :as input]
            [datahike.pg.server :as pg]
            [datahike.pg.types :as types])
  (:import [java.sql Connection DriverManager SQLException]))

(def oracle-cases
  "[type input text] or [type input message sqlstate]."
  [["bool" "t" "true"]
   ["bool" "true" "true"]
   ["bool" "TRUE" "true"]
   ["bool" " yes " "true"]
   ["bool" "y" "true"]
   ["bool" "ye" "true"]
   ["bool"
    "yess"
    "invalid input syntax for type boolean: \"yess\""
    "22P02"]
   ["bool" "on" "true"]
   ["bool" "o" "invalid input syntax for type boolean: \"o\"" "22P02"]
   ["bool" "of" "false"]
   ["bool" "off" "false"]
   ["bool"
    "offf"
    "invalid input syntax for type boolean: \"offf\""
    "22P02"]
   ["bool" "1" "true"]
   ["bool" "0" "false"]
   ["bool" "10" "invalid input syntax for type boolean: \"10\"" "22P02"]
   ["bool" "" "invalid input syntax for type boolean: \"\"" "22P02"]
   ["bool" " " "invalid input syntax for type boolean: \" \"" "22P02"]
   ["bool" "f" "false"]
   ["bool" "fa" "false"]
   ["bool" "no" "false"]
   ["bool" "n" "false"]
   ["bool" "nO" "false"]
   ["bool"
    "tr ue"
    "invalid input syntax for type boolean: \"tr ue\""
    "22P02"]
   ["bool" "\ttrue\n" "true"]
   ["bool"
    "truex"
    "invalid input syntax for type boolean: \"truex\""
    "22P02"]
   ["float4" "1.5" "1.5"]
   ["float4" "3.4e38" "3.4e+38"]
   ["float4" "3.5e38" "\"3.5e38\" is out of range for type real" "22003"]
   ["float4" "1e-45" "1e-45"]
   ["float4" "1e-50" "\"1e-50\" is out of range for type real" "22003"]
   ["float4" "NaN" "NaN"]
   ["float4" "-inf" "-Infinity"]
   ["float4" "0x1p3" "8"]
   ["float4" "1d" "invalid input syntax for type real: \"1d\"" "22P02"]
   ["float4" "7e-46" "\"7e-46\" is out of range for type real" "22003"]
   ["float8" "1" "1"]
   ["float8" "1.5" "1.5"]
   ["float8" "-1.5e10" "-15000000000"]
   ["float8" ".5" "0.5"]
   ["float8" "5." "5"]
   ["float8"
    "."
    "invalid input syntax for type double precision: \".\""
    "22P02"]
   ["float8"
    "1e"
    "invalid input syntax for type double precision: \"1e\""
    "22P02"]
   ["float8" "1e308" "1e+308"]
   ["float8"
    "1e309"
    "\"1e309\" is out of range for type double precision"
    "22003"]
   ["float8"
    "-1e309"
    "\"-1e309\" is out of range for type double precision"
    "22003"]
   ["float8"
    "1e-400"
    "\"1e-400\" is out of range for type double precision"
    "22003"]
   ["float8" "4.9e-324" "5e-324"]
   ["float8"
    "2e-324"
    "\"2e-324\" is out of range for type double precision"
    "22003"]
   ["float8" "NaN" "NaN"]
   ["float8" "nan" "NaN"]
   ["float8" "-NaN" "NaN"]
   ["float8" "Infinity" "Infinity"]
   ["float8" "-Infinity" "-Infinity"]
   ["float8" "+infinity" "Infinity"]
   ["float8" "inf" "Infinity"]
   ["float8" "-inf" "-Infinity"]
   ["float8"
    "infinit"
    "invalid input syntax for type double precision: \"infinit\""
    "22P02"]
   ["float8" "0x10" "16"]
   ["float8" "0x1p3" "8"]
   ["float8" "0x1.8p1" "3"]
   ["float8"
    "1_000"
    "invalid input syntax for type double precision: \"1_000\""
    "22P02"]
   ["float8"
    "1d"
    "invalid input syntax for type double precision: \"1d\""
    "22P02"]
   ["float8"
    "1f"
    "invalid input syntax for type double precision: \"1f\""
    "22P02"]
   ["float8" " 1.5 " "1.5"]
   ["float8"
    "abc"
    "invalid input syntax for type double precision: \"abc\""
    "22P02"]
   ["float8"
    ""
    "invalid input syntax for type double precision: \"\""
    "22P02"]
   ["float8"
    "0x"
    "invalid input syntax for type double precision: \"0x\""
    "22P02"]
   ["float8" "1e+5" "100000"]
   ["float8" "00012" "12"]
   ["float8" "+.5e-3" "0.0005"]
   ["float8" "0x1e" "30"]
   ["float8" "0x1e3" "483"]
   ["float8" "0x0p5" "0"]
   ["float8" "0x.8" "0.5"]
   ["float8" "1e-320" "1e-320"]
   ["float8" "0e-400" "0"]
   ["float8" "0.0e999999" "0"]
   ["float8" "-0" "-0"]
   ["float8" "1.7976931348623157e308" "1.7976931348623157e+308"]
   ["float8"
    "1.7976931348623159e308"
    "\"1.7976931348623159e308\" is out of range for type double precision"
    "22003"]
   ["int2" "0" "0"]
   ["int2" "32767" "32767"]
   ["int2"
    "32768"
    "value \"32768\" is out of range for type smallint"
    "22003"]
   ["int2" "-32768" "-32768"]
   ["int2"
    "-32769"
    "value \"-32769\" is out of range for type smallint"
    "22003"]
   ["int2" " 12 " "12"]
   ["int2" "+5" "5"]
   ["int2"
    "1.5"
    "invalid input syntax for type smallint: \"1.5\""
    "22P02"]
   ["int2"
    "1e3"
    "invalid input syntax for type smallint: \"1e3\""
    "22P02"]
   ["int2" "0x7FFF" "32767"]
   ["int2"
    "0x8000"
    "value \"0x8000\" is out of range for type smallint"
    "22003"]
   ["int2" "-0x8000" "-32768"]
   ["int2" "0o17" "15"]
   ["int2" "0b101" "5"]
   ["int2" "1_000" "1000"]
   ["int2" "_1" "invalid input syntax for type smallint: \"_1\"" "22P02"]
   ["int2" "1_" "invalid input syntax for type smallint: \"1_\"" "22P02"]
   ["int2"
    "1__0"
    "invalid input syntax for type smallint: \"1__0\""
    "22P02"]
   ["int2" "0x_1" "1"]
   ["int2" "0x1_F" "31"]
   ["int2"
    "abc"
    "invalid input syntax for type smallint: \"abc\""
    "22P02"]
   ["int2" "" "invalid input syntax for type smallint: \"\"" "22P02"]
   ["int2" "-" "invalid input syntax for type smallint: \"-\"" "22P02"]
   ["int2" "0x" "invalid input syntax for type smallint: \"0x\"" "22P02"]
   ["int2"
    "12abc"
    "invalid input syntax for type smallint: \"12abc\""
    "22P02"]
   ["int2" "٣" "invalid input syntax for type smallint: \"٣\"" "22P02"]
   ["int4" "2147483647" "2147483647"]
   ["int4"
    "2147483648"
    "value \"2147483648\" is out of range for type integer"
    "22003"]
   ["int4" "-2147483648" "-2147483648"]
   ["int4"
    "-2147483649"
    "value \"-2147483649\" is out of range for type integer"
    "22003"]
   ["int4" "0x7FFFFFFF" "2147483647"]
   ["int4" "0X1f" "31"]
   ["int4" "1_000_000" "1000000"]
   ["int4" "  -12  " "-12"]
   ["int4"
    "- 1"
    "invalid input syntax for type integer: \"- 1\""
    "22P02"]
   ["int4"
    "+-1"
    "invalid input syntax for type integer: \"+-1\""
    "22P02"]
   ["int4"
    "1 2"
    "invalid input syntax for type integer: \"1 2\""
    "22P02"]
   ["int4" "0B11" "3"]
   ["int4" "0O7" "7"]
   ["int4"
    "0o8"
    "invalid input syntax for type integer: \"0o8\""
    "22P02"]
   ["int4" "007" "7"]
   ["int4"
    "1.0"
    "invalid input syntax for type integer: \"1.0\""
    "22P02"]
   ["int8" "9223372036854775807" "9223372036854775807"]
   ["int8"
    "9223372036854775808"
    "value \"9223372036854775808\" is out of range for type bigint"
    "22003"]
   ["int8" "-9223372036854775808" "-9223372036854775808"]
   ["int8"
    "-9223372036854775809"
    "value \"-9223372036854775809\" is out of range for type bigint"
    "22003"]
   ["int8" "0x7fffffffffffffff" "9223372036854775807"]
   ["int8" "1_2_3" "123"]
   ["int8"
    "99999999999999999999999"
    "value \"99999999999999999999999\" is out of range for type bigint"
    "22003"]
   ["numeric" "1" "1"]
   ["numeric" "1.50" "1.50"]
   ["numeric" " 1.5 " "1.5"]
   ["numeric" "-0" "0"]
   ["numeric" "1e3" "1000"]
   ["numeric" "1.5e-3" "0.0015"]
   ["numeric" "1E+2" "100"]
   ["numeric" ".5" "0.5"]
   ["numeric" "5." "5"]
   ["numeric" "." "invalid input syntax for type numeric: \".\"" "22P02"]
   ["numeric" "NaN" "NaN"]
   ["numeric" "nan" "NaN"]
   ["numeric"
    "-NaN"
    "invalid input syntax for type numeric: \"-NaN\""
    "22P02"]
   ["numeric" "Infinity" "Infinity"]
   ["numeric" "-inf" "-Infinity"]
   ["numeric" "+Infinity" "Infinity"]
   ["numeric" "0x1F" "31"]
   ["numeric" "-0x10" "-16"]
   ["numeric" "0o17" "15"]
   ["numeric" "0b101" "5"]
   ["numeric" "1_000" "1000"]
   ["numeric" "1_000.5" "1000.5"]
   ["numeric"
    "1_"
    "invalid input syntax for type numeric: \"1_\""
    "22P02"]
   ["numeric"
    "_1"
    "invalid input syntax for type numeric: \"_1\""
    "22P02"]
   ["numeric"
    "1e"
    "invalid input syntax for type numeric: \"1e\""
    "22P02"]
   ["numeric"
    "abc"
    "invalid input syntax for type numeric: \"abc\""
    "22P02"]
   ["numeric" "" "invalid input syntax for type numeric: \"\"" "22P02"]
   ["numeric"
    "1 2"
    "invalid input syntax for type numeric: \"1 2\""
    "22P02"]
   ["numeric"
    "0x1.8"
    "invalid input syntax for type numeric: \"0x1.8\""
    "22P02"]
   ["numeric" "1e131072" "value overflows numeric format" "22003"]
   ["numeric"
    "12345678901234567890.123456789"
    "12345678901234567890.123456789"]
   ["oid" "0" "0"]
   ["oid" "4294967295" "4294967295"]
   ["oid"
    "4294967296"
    "value \"4294967296\" is out of range for type oid"
    "22003"]
   ["oid" "-1" "4294967295"]
   ["oid" "-2147483648" "2147483648"]
   ["oid"
    "-2147483649"
    "value \"-2147483649\" is out of range for type oid"
    "22003"]
   ["oid" "0x10" "16"]
   ["oid" "010" "8"]
   ["oid" "08" "invalid input syntax for type oid: \"08\"" "22P02"]
   ["oid" "0x" "invalid input syntax for type oid: \"0x\"" "22P02"]
   ["oid" " 7 " "7"]
   ["oid" "1_0" "invalid input syntax for type oid: \"1_0\"" "22P02"]
   ["oid" "+3" "3"]
   ["oid" "abc" "invalid input syntax for type oid: \"abc\"" "22P02"]
   ["oid" "" "invalid input syntax for type oid: \"\"" "22P02"]
   ["oid" "-0" "0"]
   ["uuid"
    "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"
    "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"]
   ["uuid"
    "A0EEBC999C0B4EF8BB6D6BB9BD380A11"
    "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"]
   ["uuid"
    "{a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11}"
    "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"]
   ["uuid"
    "a0ee-bc99-9c0b-4ef8-bb6d-6bb9-bd38-0a11"
    "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"]
   ["uuid"
    "{a0eebc999c0b4ef8bb6d6bb9bd380a11"
    "invalid input syntax for type uuid: \"{a0eebc999c0b4ef8bb6d6bb9bd380a11\""
    "22P02"]
   ["uuid"
    "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a1"
    "invalid input syntax for type uuid: \"a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a1\""
    "22P02"]
   ["uuid"
    " a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"
    "invalid input syntax for type uuid: \" a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11\""
    "22P02"]
   ["uuid"
    "a0eebc999c0b4ef8bb6d6bb9bd380a11-"
    "invalid input syntax for type uuid: \"a0eebc999c0b4ef8bb6d6bb9bd380a11-\""
    "22P02"]
   ["uuid"
    "a0eebc99--9c0b-4ef8-bb6d-6bb9bd380a11"
    "invalid input syntax for type uuid: \"a0eebc99--9c0b-4ef8-bb6d-6bb9bd380a11\""
    "22P02"]
   ["uuid"
    "a0eeb-c99-9c0b-4ef8-bb6d-6bb9bd380a11"
    "invalid input syntax for type uuid: \"a0eeb-c99-9c0b-4ef8-bb6d-6bb9bd380a11\""
    "22P02"]
   ["uuid" "" "invalid input syntax for type uuid: \"\"" "22P02"]])

(def ^:private type-oid
  {"bool" types/oid-bool "int2" types/oid-int2 "int4" types/oid-int4
   "int8" types/oid-int8 "oid" types/oid-oid "float4" types/oid-float4
   "float8" types/oid-float8 "numeric" types/oid-numeric "uuid" types/oid-uuid})

(defn- ours
  "What `input/parse` makes of `s`, in the table's shape."
  [t s]
  (try
    (let [v (input/parse (type-oid t) s)]
      [t s (cond
             (boolean? v) (str v)
             (#{"float4" "float8"} t) v
             :else (types/->pg-text v (type-oid t)))])
    (catch clojure.lang.ExceptionInfo e
      (let [[sqlstate message] (errors/classify-exception e)]
        [t s message sqlstate]))))

(defn- same-float? [t expected v]
  (let [d (double v)]
    (cond
      (Double/isNaN d) (= "NaN" expected)
      (Double/isInfinite d) (= (if (pos? d) "Infinity" "-Infinity") expected)
      (= "float4" t) (= (Float/parseFloat expected) (float v))
      :else (= (Double/parseDouble expected) d))))

(deftest input-functions-match-postgresql
  (doseq [[t s & expected :as row] oracle-cases
          :let [[_ _ & got] (ours t s)]]
    (testing (pr-str [t s])
      (if (and (= 1 (count expected)) (= 1 (count got)) (#{"float4" "float8"} t)
               (number? (first got)))
        (is (same-float? t (first expected) (first got)) (pr-str row))
        (is (= expected got))))))

;; ---------------------------------------------------------------------------
;; The paths: every way text becomes one of these types.

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"in" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :once fixture)

(defn- jdbc ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/in?user=x&password=x&sslmode=disable&stringtype=unspecified")))

(defn- run
  "First column of each row, or [:error sqlstate message]."
  [^Connection c sql & params]
  (try
    (with-open [st (.prepareStatement c sql)]
      (doseq [[i p] (map-indexed vector params)] (.setString st (int (inc i)) p))
      (if (.execute st)
        (with-open [rs (.getResultSet st)]
          (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc)))
        (.getUpdateCount st)))
    (catch SQLException e
      [:error (.getSQLState e) (.getMessage (.getServerErrorMessage ^org.postgresql.util.PSQLException e))])))

(deftest untyped-literals-take-the-other-operands-type
  (with-open [c (jdbc)]
    (run c "CREATE TABLE lit (id int PRIMARY KEY, b bool, i int, s smallint, u uuid)")
    (run c "INSERT INTO lit VALUES (1, true, 5, 1, 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11'), (2, false, 6, 2, NULL)")
    (testing "a coerced false is a value, not 'no coercion' (WHERE b = 'false' found nothing)"
      (is (= ["2"] (run c "SELECT id FROM lit WHERE b = 'false'")))
      (is (= ["2"] (run c "SELECT id FROM lit WHERE b = 'no'")))
      (is (= ["1"] (run c "SELECT id FROM lit WHERE b <> 'off'"))))
    (testing "invalid text is an error, not a comparison that matches nothing"
      (is (= [:error "22P02" "invalid input syntax for type integer: \"abc\""]
             (run c "SELECT id FROM lit WHERE i = 'abc'")))
      (is (= [:error "22P02" "invalid input syntax for type integer: \"x\""]
             (run c "SELECT id FROM lit WHERE i IN ('5', 'x')")))
      (is (= [:error "22P02" "invalid input syntax for type integer: \"5.5\""]
             (run c "SELECT id FROM lit WHERE i BETWEEN '5' AND '5.5'")))
      (is (= [:error "22003" "value \"99999\" is out of range for type smallint"]
             (run c "SELECT id FROM lit WHERE s = '99999'"))))
    (testing "the type's own input syntax"
      (is (= ["2"] (run c "SELECT id FROM lit WHERE i = '0x6'")))
      (is (= ["1"] (run c "SELECT id FROM lit WHERE i = ' 5 '")))
      (is (= ["1"] (run c "SELECT id FROM lit WHERE u = '{a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11}'"))))))

(deftest casts-from-text-read-the-text
  (with-open [c (jdbc)]
    (is (= [:error "22P02" "invalid input syntax for type integer: \"1.5\""]
           (run c "SELECT '1.5'::int")))
    (is (= ["2"] (run c "SELECT 1.5::int")) "a numeric source rounds")
    (is (= ["31"] (run c "SELECT '0x1F'::int")))
    (is (= ["4294967295"] (run c "SELECT '-1'::oid")))
    (is (= [:error "22003" "OID out of range"] (run c "SELECT 4294967296::oid")))
    (is (= [:error "22003" "\"1e400\" is out of range for type double precision"]
           (run c "SELECT '1e400'::float8")))
    (is (= [:error "22003" "value \"99999\" is out of range for type smallint"]
           (run c "SELECT '{1,99999}'::int2[]")))
    (is (= [:error "22P02" "invalid input syntax for type integer: \"abc\""]
           (run c "SELECT '{abc}'::int[]")) "integer arrays keep their width")
    (is (= ["t"] (run c "SELECT ('{1.1}'::real[])[1] = 1.1::real")))
    (is (= ["value \"99999\" is out of range for type smallint"]
           (run c "SELECT message FROM pg_input_error_info('99999', 'int2')")))))

(deftest writes-read-text-with-the-column-type
  (with-open [c (jdbc)]
    (run c "CREATE TABLE w (id int PRIMARY KEY, s smallint, o oid, f float8, b bool, u uuid)")
    (is (= 1 (run c "INSERT INTO w VALUES (1, ' 7 ', '-1', '1e3', ' yes ', '{a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11}')")))
    (is (= [:error "22003" "value \"99999\" is out of range for type smallint"]
           (run c "INSERT INTO w (id, s) VALUES (2, '99999')")))
    (is (= [:error "22P02" "invalid input syntax for type boolean: \"maybe\""]
           (run c "INSERT INTO w (id, b) VALUES (3, 'maybe')")))
    (is (= [:error "22P02" "invalid input syntax for type double precision: \"1d\""]
           (run c "INSERT INTO w (id, f) VALUES (4, '1d')")))
    (is (= 1 (run c "UPDATE w SET b = 'of' WHERE id = 1")))
    (is (= ["7"] (run c "SELECT s FROM w WHERE id = 1")))
    (is (= ["4294967295"] (run c "SELECT o FROM w WHERE id = 1")))
    (is (= ["f"] (run c "SELECT b FROM w WHERE id = 1")))))

(deftest text-parameters-use-the-same-input-functions
  (with-open [c (jdbc)]
    (is (= ["true"] (run c "SELECT (?::bool)::text" "yes")))
    (is (= ["false"] (run c "SELECT (?::bool)::text" "of")))
    (is (= [:error "22P02" "invalid input syntax for type boolean: \"maybe\""]
           (run c "SELECT (?::bool)::text" "maybe"))
        "was false: every bool other than t/true/1")
    (is (= [:error "22003" "value \"99999\" is out of range for type smallint"]
           (run c "SELECT (?::int2)::text" "99999"))
        "was XX000")
    (is (= ["NaN"] (run c "SELECT (?::numeric)::text" "NaN")))
    (is (= ["4294967295"] (run c "SELECT (?::oid)::text" "-1")))))
