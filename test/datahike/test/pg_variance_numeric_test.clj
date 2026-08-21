(ns datahike.test.pg-variance-numeric-test
  "STDDEV / VARIANCE over int2, int4, int8 and numeric return NUMERIC in
   PostgreSQL -- only the float4 / float8 overloads return float8
   (pg_aggregate.dat). Computing all of them in double was wrong twice:

     stddev(sm)   26754.55172956557    PG 26754.55172957
     stddev(b)    ArithmeticException  PG 5325116328314171703
     stddev_pop(x) with a NULL in the group -- ClassCastException

   The numeric path is a port of `numeric_stddev_internal` (numeric.c)
   and the float path of the Youngs-Cramer accumulator (float.c), so
   both answer what PostgreSQL answers rather than what a textbook
   formula in double gives. Every expectation is a PostgreSQL 17
   oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn vn-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"vn" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each vn-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/vn?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- seed! [^Connection c]
  (exec! c (str "CREATE TABLE sv (i int, b bigint, n numeric(9,3), f float8, "
                "r real, sm smallint)"))
  ;; b spans the int8 range: the double accumulator overflowed on it.
  (exec! c (str "INSERT INTO sv VALUES "
                "(1,10,1.500,1.5,1.1,3),"
                "(2,9223372036854775807,2.250,2.5,2.2,-4),"
                "(3,-20,0.000,0.5,3.3,32767),"
                "(NULL,NULL,NULL,NULL,NULL,NULL)")))

(deftest integer-input-is-numeric
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["1.00000000000000000000"] (col c 1 "SELECT stddev(i) FROM sv")))
    (is (= ["1.00000000000000000000"] (col c 1 "SELECT variance(i) FROM sv")))
    (is (= ["0.81649658092772603273"] (col c 1 "SELECT stddev_pop(i) FROM sv")))
    (is (= ["0.66666666666666666667"] (col c 1 "SELECT var_pop(i) FROM sv")))
    (is (= ["18918.32526943"] (col c 1 "SELECT stddev(sm) FROM sv"))
        "smallint too -- the digit count comes from select_div_scale")
    (is (= ["numeric"] (col c 1 "SELECT pg_typeof(stddev(i)) FROM sv LIMIT 1")))))

(deftest bigint-input-does-not-overflow
  (with-open [c (jdbc)]
    (seed! c)
    ;; `(reduce + 0 …)` over values near Long/MAX_VALUE threw; PostgreSQL
    ;; widens to numeric and answers exactly.
    (is (= ["5325116328314171703"] (col c 1 "SELECT stddev(b) FROM sv")))))

(deftest numeric-input-keeps-its-digits
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["1.1456439237389600"] (col c 1 "SELECT stddev(n) FROM sv")))
    (is (= ["1.3125000000000000"] (col c 1 "SELECT var_samp(n) FROM sv")))))

(deftest float-input-stays-float8
  (with-open [c (jdbc)]
    (seed! c)
    ;; The Youngs-Cramer sequence, not the two-pass mean formula: the
    ;; two differ in the last place.
    (is (= ["1"] (col c 1 "SELECT stddev(f) FROM sv")))
    (is (= ["0.6666666666666666"] (col c 1 "SELECT var_pop(f) FROM sv")))
    (is (= ["0.816496580927726"] (col c 1 "SELECT stddev_pop(f) FROM sv")))
    (is (= ["1.0999999642372138"] (col c 1 "SELECT stddev(r) FROM sv"))
        "real widens to float8, as PG's aggregate does")
    (is (= ["double precision"] (col c 1 "SELECT pg_typeof(stddev(f)) FROM sv LIMIT 1")))))

(deftest nulls-and-small-groups
  (with-open [c (jdbc)]
    (seed! c)
    (testing "a NULL in the group is skipped, not cast to Number"
      ;; stddev_pop / var_pop routed to Datalog's raw aggregates, which
      ;; cast every element: the `:__null__` sentinel raised
      ;; "class clojure.lang.Keyword cannot be cast to class Number".
      (is (= ["0.81649658092772603273"] (col c 1 "SELECT stddev_pop(i) FROM sv"))))
    (testing "sample forms are NULL below two values, population is 0"
      (is (= [nil] (col c 1 "SELECT stddev(i) FROM sv WHERE i = 1")))
      (is (= ["0"] (col c 1 "SELECT var_pop(i) FROM sv WHERE i = 1"))))
    (testing "an empty group is NULL either way"
      (is (= [nil] (col c 1 "SELECT stddev(i) FROM sv WHERE i > 100")))
      (is (= [nil] (col c 1 "SELECT var_pop(i) FROM sv WHERE i > 100"))))))

(deftest windowed-and-filtered-forms-agree
  (with-open [c (jdbc)]
    (seed! c)
    ;; The window engine and the FILTER path resolve the aggregate
    ;; through the same map and the same precision rule, so they pick up
    ;; the numeric runtime without knowing about it.
    (is (= ["1.00000000000000000000"] (col c 1 "SELECT stddev(i) FILTER (WHERE i > 0) FROM sv")))
    (is (= [nil "0.53033008588991064330" "1.1456439237389600" "1.1456439237389600"]
           (col c 1 "SELECT stddev(n) OVER (ORDER BY i) FROM sv ORDER BY i")))))
