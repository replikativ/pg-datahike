(ns datahike.test.pg-math-functions-test
  "Math functions PostgreSQL 17 has and we did not.

   Fifteen names, each of which used to answer `42883 function X does
   not exist`.

   The degree-trigonometry family is the interesting one. It is NOT
   `sin(x * pi/180)`: PostgreSQL divides through by the function's own
   value at a reference angle, and that division CANCELS the libm error
   at the endpoints (float.c sind_0_to_30, cosd_0_to_60, asind_q1). So
   `sind(30)` is EXACTLY 0.5, `tand(45)` exactly 1 and `asind(0.5)`
   exactly 30 on any platform -- and those are precisely the values
   anyone checks. A derived implementation misses every one, which is
   why this is a port rather than a rewrite.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn mf-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"m" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each mf-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/m?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest degree-trig-is-exact-at-its-endpoints
  (with-open [c (jdbc)]
    (testing "the whole point of PostgreSQL's construction"
      (is (= "0.5" (one c "SELECT sind(30)")))
      (is (= "0" (one c "SELECT sind(0)")))
      (is (= "1" (one c "SELECT sind(90)")))
      (is (= "0.5" (one c "SELECT cosd(60)")))
      (is (= "1" (one c "SELECT cosd(0)")))
      (is (= "1" (one c "SELECT tand(45)")))
      (is (= "1" (one c "SELECT cotd(45)")))
      (is (= "30" (one c "SELECT asind(0.5)")))
      (is (= "60" (one c "SELECT acosd(0.5)")))
      (is (= "45" (one c "SELECT atand(1)")))
      (is (= "45" (one c "SELECT atan2d(1,1)"))))
    (testing "and range reduction keeps the exactness"
      (is (= "0" (one c "SELECT sind(180)")))
      (is (= "-1" (one c "SELECT cosd(180)")))
      (is (= "0" (one c "SELECT tand(0)"))))))

(deftest degree-trig-domain-errors
  (with-open [c (jdbc)]
    (is (thrown-with-msg? SQLException #"input is out of range"
                          (one c "SELECT asind(2)")))
    (is (thrown-with-msg? SQLException #"input is out of range"
                          (one c "SELECT acosd(2)")))
    (is (thrown-with-msg? SQLException #"input is out of range"
                          (one c "SELECT sind('Infinity'::float8)"))
        "POSIX: NaN in gives NaN out, but an infinite input is an error")
    (is (= "NaN" (one c "SELECT sind('NaN'::float8)")))))

(deftest div-truncates-toward-zero
  (with-open [c (jdbc)]
    (is (= "2" (one c "SELECT div(9,4)")))
    (is (= "-3" (one c "SELECT div(-7,2)")) "toward zero, not toward -inf")
    (is (= "2" (one c "SELECT div(9.5,4.1)")))
    (is (thrown-with-msg? SQLException #"division by zero"
                          (one c "SELECT div(1,0)")))
    (testing "and it is NOT the `/` operator, which carries
              select_div_scale's scale"
      (is (= "2.3170731707317073" (one c "SELECT 9.5 / 4.1"))))))

(deftest factorial-and-scale-inspectors
  (with-open [c (jdbc)]
    (is (= "120" (one c "SELECT factorial(5)")))
    (is (= "1" (one c "SELECT factorial(0)")))
    (is (thrown-with-msg? SQLException #"factorial of a negative number"
                          (one c "SELECT factorial(-1)")))
    (is (= "4" (one c "SELECT scale(8.4100)")) "the DECLARED scale")
    (is (= "2" (one c "SELECT min_scale(8.4100)")) "the scale still needed")
    (is (= "8.41" (one c "SELECT trim_scale(8.4100)")))))

(deftest erf-and-erfc
  (with-open [c (jdbc)]
    (is (= "0" (one c "SELECT erf(0)")))
    (is (= "1" (one c "SELECT erfc(0)")))
    (testing "PostgreSQL calls libm here, so we are matching glibc rather
              than a specification -- agreement is to ~1 ulp, not exact"
      (let [v (Double/parseDouble (one c "SELECT erf(1)"))]
        (is (< (Math/abs (- v 0.8427007929497149)) 1e-15)))
      (let [v (Double/parseDouble (one c "SELECT erfc(1)"))]
        (is (< (Math/abs (- v 0.15729920705028513)) 1e-15))))))

(deftest result-types
  (with-open [c (jdbc)]
    (is (= "double precision" (one c "SELECT pg_typeof(sind(30))")))
    (is (= "numeric" (one c "SELECT pg_typeof(div(9,4))")))
    (is (= "numeric" (one c "SELECT pg_typeof(factorial(5))")))
    (is (= "integer" (one c "SELECT pg_typeof(scale(8.41))")))))
