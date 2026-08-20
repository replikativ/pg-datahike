(ns datahike.test.pg-numeric-transcendentals-test
  "sqrt / exp / ln / log / log10 / power over `numeric`.

   PostgreSQL declares BOTH a float8 and a numeric overload of each, and
   function resolution picks the numeric one whenever an argument is
   numeric. We answered float8 for all six -- wrong in two ways at once:
   the reported type, and the precision. `2.0 ^ 10` is
   1024.0000000000000 in PostgreSQL, a scale that says how many digits
   are meaningful; we answered 1024.

   The scale is not the operands'. Each function has its own rule in
   numeric.c, all of the shape \"enough digits for
   NUMERIC_MIN_SIG_DIGITS significant figures, but never fewer than the
   input already shows\" -- numeric_sqrt halves the weight,
   numeric_exp scales by log10(e), numeric_ln uses estimate_ln_dweight,
   power_var_int uses exp*log10(base).

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn nt-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"t" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each nt-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/t?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest a-numeric-argument-selects-the-numeric-overload
  (with-open [c (jdbc)]
    (is (= "numeric" (one c "SELECT pg_typeof(sqrt(2.0))")))
    (is (= "double precision" (one c "SELECT pg_typeof(sqrt(2))"))
        "all-integer arguments stay float8 -- float8 is the preferred
         type in the NUMERIC category")
    (is (= "numeric" (one c "SELECT pg_typeof(power(2.0,3))"))
        "ONE numeric argument is enough")
    (is (= "numeric" (one c "SELECT pg_typeof(exp(1.0))")))))

(deftest each-function-carries-its-own-scale
  (with-open [c (jdbc)]
    (is (= "1.414213562373095" (one c "SELECT sqrt(2.0)")))
    (is (= "2.000000000000000" (one c "SELECT sqrt(4.0)")))
    (is (= "2.7182818284590452" (one c "SELECT exp(1.0)")))
    (is (= "0.6931471805599453" (one c "SELECT ln(2.0)")))
    (is (= "2.0000000000000000" (one c "SELECT log(100.0)")))
    (is (= "3.0000000000000000" (one c "SELECT log10(1000.0)")))
    (is (= "1024.0000000000000" (one c "SELECT power(2.0,10)")))))

(deftest the-power-operator-resolves-the-same-way
  (with-open [c (jdbc)]
    (is (= "1024.0000000000000" (one c "SELECT 2.0 ^ 10"))
        "`^` is numeric_power when an operand is numeric, like power()")
    (is (= "1024" (one c "SELECT 2 ^ 10")) "and float8 otherwise")
    (is (= "2.0000000000000000" (one c "SELECT power(4.0,0.5)"))
        "a non-integer exponent goes through exp(y*ln(x))")))

(deftest two-argument-log-is-always-numeric
  (with-open [c (jdbc)]
    (is (= "6.0000000000000000" (one c "SELECT log(2,64)"))
        "PostgreSQL has no float8 two-argument log, so even integer
         arguments coerce to numeric")
    (is (= "numeric" (one c "SELECT pg_typeof(log(2,64))")))))

(deftest domain-errors-keep-their-own-sqlstates
  (with-open [c (jdbc)]
    (is (thrown-with-msg? SQLException #"cannot take logarithm of zero"
                          (one c "SELECT ln(0.0)")))
    (is (thrown-with-msg? SQLException #"cannot take logarithm of a negative number"
                          (one c "SELECT ln(-1.0)")))
    (is (thrown-with-msg? SQLException #"cannot take logarithm of zero"
                          (one c "SELECT log(0.0,10.0)"))
        "the BASE is what is at fault here, and the message has to say so")
    (is (thrown-with-msg? SQLException #"division by zero"
                          (one c "SELECT log(1.0,10.0)"))
        "ln(1) is 0, so log base 1 divides by zero rather than erroring
         as a logarithm")
    (is (thrown-with-msg? SQLException #"cannot take square root of a negative"
                          (one c "SELECT sqrt(-1.0)")))
    (is (thrown-with-msg? SQLException #"zero raised to a negative power"
                          (one c "SELECT power(0.0,-1.0)")))
    (is (thrown-with-msg? SQLException #"negative number raised to a non-integer"
                          (one c "SELECT power(-1.0,0.5)")))))
