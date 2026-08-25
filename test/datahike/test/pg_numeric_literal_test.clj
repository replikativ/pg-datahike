(ns datahike.test.pg-numeric-literal-test
  "An unadorned decimal literal is `numeric` in PostgreSQL, not float8.

   We typed it float8, which is not a display quirk -- it is the wrong
   type, and it made ordinary arithmetic wrong:

     SELECT 0.1 + 0.2   ->  0.30000000000000004   (want 0.3)
     SELECT 1.005 * 100 ->  100.49999999999999    (want 100.500)
     SELECT 1.10        ->  1.1                   (want 1.10)

   The literal has to be rebuilt from the ORIGINAL TOKEN: by the time
   JSqlParser exposes `.getValue` it has already gone through a double,
   losing both the exactness and the scale.

   Division needs PostgreSQL's own rule. BigDecimal refuses to divide
   without an explicit scale (\"Non-terminating decimal expansion\"), and
   the scale is not the operands' -- select_div_scale picks one giving at
   least 16 significant digits, which is why `10.0 / 3` has 16 decimals
   and `1.0 / 3.0` has 20.

   Expectations here are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn num-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"num" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each num-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/num?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE w (id int, n numeric, f float8, r real, i int)")
  (exec! c "INSERT INTO w VALUES (1,1.50,1.5,1.5,10),(2,2.25,2.5,2.5,20),(3,0.10,0.1,0.1,30)"))

(deftest decimal-literals-are-exact
  (with-open [c (jdbc)]
    (testing "the whole point: numeric arithmetic is exact, float8 is not"
      (is (= "0.3" (one c "SELECT 0.1 + 0.2")))
      (is (= "0.3" (one c "SELECT 0.1 * 3")))
      (is (= "100.500" (one c "SELECT 1.005 * 100")))
      (is (= "0.495" (one c "SELECT 1.50 - 1.005"))))))

(deftest decimal-literals-keep-their-scale
  (with-open [c (jdbc)]
    (is (= "1.10" (one c "SELECT 1.10")) "the trailing zero is significant")
    (is (= "100.0" (one c "SELECT 100.0")))
    (is (= "0.0" (one c "SELECT 0.0")))
    (is (= "-1.50" (one c "SELECT -1.50")))
    (is (= "1.23456789012345678901234567890"
           (one c "SELECT 1.23456789012345678901234567890"))
        "arbitrary precision -- a double truncates this at 17 digits")))

(deftest decimal-literals-report-numeric
  (with-open [c (jdbc)]
    (is (= "numeric" (one c "SELECT pg_typeof(1.10)")))
    (is (= "numeric" (one c "SELECT pg_typeof(1e-3)")))
    (is (= "numeric" (one c "SELECT pg_typeof(1.0e3)"))
        "the exponent forms are numeric too")
    (is (= "numeric" (one c "SELECT pg_typeof(1.5 + 1)"))
        "numeric + integer is numeric; only float8 outranks numeric")
    (is (= "double precision" (one c "SELECT pg_typeof(1.5::float8 + 1.5)"))
        "and float8 does outrank it")))

(deftest exponent-literals-render-without-an-exponent
  (with-open [c (jdbc)]
    (testing "1.0e3 parses to a NEGATIVE scale, whose .toString is
              \"1.0E+3\" -- PostgreSQL has no exponent form in numeric output"
      (is (= "1000" (one c "SELECT 1.0e3")))
      (is (= "1000" (one c "SELECT 1e3")))
      (is (= "0.001" (one c "SELECT 1e-3"))))))

(deftest addition-and-multiplication-scales
  (with-open [c (jdbc)]
    (testing "add/subtract take the wider scale, multiply sums them"
      (is (= "5.0" (one c "SELECT 2.5 * 2")))
      (is (= "6.000" (one c "SELECT 2.00 * 3.0")))
      (is (= "1.50750" (one c "SELECT 1.50 * 1.005")))
      (is (= "1.50" (one c "SELECT 3.0 - 1.50")))
      (is (= "0.0" (one c "SELECT -0.5 + 0.5"))))))

(deftest numeric-multiplication-caps-storage-scale
  (with-open [c (jdbc)]
    (testing "PostgreSQL numeric.sql line 1244 rounds an exact product at NUMERIC_DSCALE_MAX"
      (is (= "0.01"
             (one c (str "SELECT trim_scale((0.1 - 2e-16383)"
                         " * (0.1 - 3e-16383))"))))
      (is (= "16383"
             (one c (str "SELECT scale((0.1 - 2e-16383)"
                         " * (0.1 - 3e-16383))")))))))

(deftest division-uses-select-div-scale
  (with-open [c (jdbc)]
    (testing "not the operands' scale -- at least 16 significant digits"
      (is (= "3.3333333333333333" (one c "SELECT 10.0 / 3")))
      (is (= "14.2857142857142857" (one c "SELECT 100.0 / 7")))
      (is (= "3.0000000000000000" (one c "SELECT 1.5 / 0.5")))
      (is (= "2.5000000000000000" (one c "SELECT 5 / 2.0"))
          "numeric / integer is numeric"))
    (testing "a quotient below 1 gets four more decimals -- the estimated
              quotient weight drives the count, not the inputs"
      (is (= "0.33333333333333333333" (one c "SELECT 1.0 / 3.0"))))
    (testing "integer / integer is still integer division"
      (is (= "0" (one c "SELECT 1 / 3"))))))

(deftest numeric-equality-across-types
  (with-open [c (jdbc)]
    (seed! c)
    (testing "PostgreSQL resolves a cross-type comparison by promoting;
              Clojure's = is type-sensitive for numbers, so each of these
              answered no rows"
      (is (= ["1"] (col c "SELECT id FROM w WHERE n = 1.50")))
      (is (= ["1"] (col c "SELECT id FROM w WHERE n = 1.5"))
          "scale is not part of numeric equality")
      (is (= ["1"] (col c "SELECT id FROM w WHERE f = 1.5"))
          "a numeric literal against a float8 column")
      (is (= ["1"] (col c "SELECT id FROM w WHERE r = 1.5")))
      (is (= ["1"] (col c "SELECT id FROM w WHERE i = 10.0"))
          "a decimal literal against an integer column")
      (is (= ["1" "3"] (col c "SELECT id FROM w WHERE n = f ORDER BY id"))
          "column to column, numeric against float8 -- rows 1 and 3 have
           1.50 = 1.5 and 0.10 = 0.1"))))

(deftest a-constant-that-cannot-match-matches-nothing
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [] (col c "SELECT id FROM w WHERE i = 10.5"))
        "10.5 is not exactly representable as an integer, so it equals no
         row -- the index seek uses a key nothing equals rather than
         rounding to 10 and matching the wrong row")))

(deftest numeric-in-list-and-ordering
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["2"] (col c "SELECT count(*) FROM w WHERE n IN (1.50, 0.10)"))
        "IN is set-membership, which is =-based and inherited the same blindness")
    (is (= ["1"] (col c "SELECT count(*) FROM w WHERE n BETWEEN 0.5 AND 2.0"))
        "only 1.50 falls in the range")
    (is (= ["0.10" "1.50" "2.25"] (col c "SELECT n FROM w ORDER BY n")))))

(deftest numeric-literals-insert-into-any-numeric-column
  (with-open [c (jdbc)]
    (seed! c)
    (testing "a numeric literal has to narrow to the column's stored type;
              float8/real/int columns all take one"
      (is (= ["1.50" "2.25" "0.10"] (col c "SELECT n FROM w ORDER BY id")))
      (is (= ["1.5" "2.5" "0.1"] (col c "SELECT f FROM w ORDER BY id")))
      (is (= ["10" "20" "30"] (col c "SELECT i FROM w ORDER BY id"))))))

(deftest aggregates-over-numeric-keep-scale
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "3.85" (one c "SELECT sum(n) FROM w")))
    (is (= "0.10" (one c "SELECT min(n) FROM w")))
    (is (= "2.25" (one c "SELECT max(n) FROM w")))
    (is (= "1.28333333333333333333" (one c "SELECT sum(n) / count(*) FROM w"))
        "division under an aggregate uses the same scale rule -- 20
         decimals here because the quotient is below 1")))

(deftest exponent-literals-have-no-negative-scale
  (with-open [c (jdbc)]
    (testing "PostgreSQL's numeric never carries a negative display scale;
              BigDecimal does -- `1e2` parses to unscaled 1 at scale -2.
              Multiply is the operator that propagates it, since its result
              scale is s1+s2 while add/subtract take a max and divide
              clamps at zero."
      (is (= "125.00" (one c "SELECT 1e2 * 1.25")))
      (is (= "3750.0" (one c "SELECT 1.5e3 * 2.5")))
      (is (= "150.0000000" (one c "SELECT 1e-7 * 1.5e9")))
      (is (= "5000000000.0" (one c "SELECT 1e10 * 0.5"))))
    (testing "the other operators were already unaffected"
      (is (= "101.25" (one c "SELECT 1e2 + 1.25")))
      (is (= "25.0000000000000000" (one c "SELECT 1e2 / 4"))
          "division clamps the negative scale at zero, then select_div_scale
           applies as usual"))))

(deftest mod-promotes-against-a-numeric-operand
  (with-open [c (jdbc)]
    (is (= "1.0" (one c "SELECT mod(1, 3.0)"))
        "the mod FUNCTION was wired to bare `rem`, which neither promotes
         the integer operand nor raises PostgreSQL's division-by-zero")
    (is (= "1.5" (one c "SELECT mod(5.5, 2)")))
    (is (= "1.00" (one c "SELECT mod(5.5, 2.25)")))
    (is (= "1" (one c "SELECT mod(7, 2)")))
    (is (= "-1" (one c "SELECT mod(-7, 2)")))
    (is (thrown-with-msg? java.sql.SQLException #"division by zero"
                          (one c "SELECT mod(1, 0)")))))

(deftest arithmetic-over-a-numeric-aggregate-stays-exact
  (with-open [c (jdbc)]
    (seed! c)
    (testing "the compound-aggregate path selects the runtime variant by
              input type, like the plain and FILTER paths -- it used to
              always take float8, so avg(n) was exact but avg(n)*1 was not"
      (is (= "1.28333333333333333333" (one c "SELECT avg(n) FROM w")))
      (is (= "1.28333333333333333333" (one c "SELECT avg(n) * 1 FROM w")))
      (is (= "7.70" (one c "SELECT sum(n) * 2 FROM w"))))))

(deftest decimal-expressions-report-numeric-to-a-driver
  (with-open [c (jdbc)]
    (testing "the simple protocol rewrites literals to $N before the
              translator sees them, so expr-oid cannot type them and the
              OID comes from the runtime value. A BigDecimal had no branch
              there and reported text; and promoted-numeric claimed int8
              from ONE typed side, so `i4 + 1.0` advertised int8 over the
              text 11.0 -- psycopg2 raised ValueError on int(\"11.0\")."
      (exec! c "CREATE TABLE d (id int, i4 int)")
      (exec! c "INSERT INTO d VALUES (1, 10)")
      (with-open [st (.createStatement c)
                  rs (.executeQuery st "SELECT i4 + 1.0 FROM d")]
        (is (= "numeric" (.getColumnTypeName (.getMetaData rs) 1)))
        (.next rs)
        (is (= "11.0" (.getString rs 1))))
      (with-open [st (.createStatement c)
                  rs (.executeQuery st "SELECT 2.0")]
        (is (= "numeric" (.getColumnTypeName (.getMetaData rs) 1)))))))
