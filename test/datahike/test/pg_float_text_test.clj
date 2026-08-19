(ns datahike.test.pg-float-text-test
  "PostgreSQL's text form for float8 and float4.

   PostgreSQL prints shortest-round-trip digits (float.c
   float8out_internal, via Ryu, whenever extra_float_digits > 0 -- its
   default is 1). Java's Double.toString since JDK 19 produces
   shortest-round-trip digits too, so the DIGITS already agreed; the
   presentation did not, in three ways:

     - the fixed-vs-scientific threshold: PostgreSQL uses fixed point iff
       the scientific exponent is in [-4, 15) for float8 and [-4, 6) for
       float4; Java switches at 1e7 / 1e-3
     - the exponent spelling: `1e+300`, `1e-05` vs Java's `1.0E300`
     - Java keeps two significant digits minimum (`1.0E7`), PostgreSQL's
       mantissa is minimal (`1e+15`)

   So every float above ~1e7 or below 1e-4 went out in a syntax
   PostgreSQL never emits. Expectations here are a PG 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn ft-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"ft" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each ft-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/ft?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest float8-fixed-point-threshold
  (with-open [c (jdbc)]
    (testing "fixed point up to but not including 1e15"
      (is (= "10000000" (one c "SELECT 1e7::float8")))
      (is (= "100000000000000" (one c "SELECT 1e14::float8")))
      (is (= "12345678.5" (one c "SELECT 12345678.5::float8")))
      (is (= "123456789.25" (one c "SELECT 123456789.25::float8"))))
    (testing "scientific from 1e15 up"
      (is (= "1e+15" (one c "SELECT 1e15::float8")))
      (is (= "1e+20" (one c "SELECT 1e20::float8")))
      (is (= "1e+300" (one c "SELECT 1e300::float8"))))
    (testing "and down to 1e-4, scientific below"
      (is (= "0.0001" (one c "SELECT 1e-4::float8")))
      (is (= "1e-05" (one c "SELECT 1e-5::float8"))
          "two exponent digits, zero-padded"))))

(deftest float8-ordinary-values-unchanged
  (with-open [c (jdbc)]
    (is (= "0.1" (one c "SELECT 0.1::float8")))
    (is (= "0.3333333333333333" (one c "SELECT (1.0/3.0)::float8")))
    (is (= "100" (one c "SELECT 100.0::float8")) "no trailing .0")
    (is (= "-10000000" (one c "SELECT (-1e7)::float8")))
    (is (= "0" (one c "SELECT 0.0::float8")))))

(deftest float4-has-a-narrower-threshold
  (with-open [c (jdbc)]
    (testing "real switches to scientific at 1e6, not 1e15"
      (is (= "100000" (one c "SELECT 1e5::real")))
      (is (= "1e+06" (one c "SELECT 1e6::real")))
      (is (= "123456.7" (one c "SELECT 123456.7::real"))))
    (is (= "0.0001" (one c "SELECT 1e-4::real")))
    (is (= "1e-05" (one c "SELECT 1e-5::real")))))

(deftest float-text-in-arrays-and-casts
  (with-open [c (jdbc)]
    (testing "array_out calls the element type's own output function"
      (is (= "{10000000,1e-05}"
             (one c "SELECT ARRAY[1e7::float8, 1e-5::float8]"))))
    (testing "and every other route to text uses the same form"
      (is (= "10000000" (one c "SELECT (1e7::float8)::text")))
      (is (= "v10000000" (one c "SELECT 'v' || 1e7::float8")))
      (is (= "10000000x" (one c "SELECT concat(1e7::float8, 'x')"))))))
