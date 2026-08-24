(ns datahike.test.pg-float4-overflow-test
  "`real` is a distinct type, and float arithmetic can fail.

   Two independent gaps, both silent wrong answers:

   float4 was float8 in every expression, so `r + r` on 1.1::real gave
   2.200000047683716 -- the answer you get by widening both operands to
   double first -- where PostgreSQL computes AT float4 precision
   (float.h float4_pl) and gives 2.2.

   And IEEE-754 returns an infinity or a zero where PostgreSQL raises:
   `1e308::float8 * 10` was Infinity, which then propagated through the
   rest of the query as a value. PostgreSQL aborts with 22003.

   A third, smaller one: casting a float to numeric is NOT
   shortest-round-trip. numeric.c float8_numeric prints `%.15g` (`%.6g`
   for float4) and parses that, which is why
   `(0.1::float8 + 0.2::float8)::numeric` is 0.3 and not
   0.30000000000000004.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn f4-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"f" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each f4-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/f?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE t (id int, r real, d float8)")
  (exec! c "INSERT INTO t VALUES (1, 1.1, 1.1)"))

(deftest real-is-a-distinct-type
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "real" (one c "SELECT pg_typeof(1.1::real)")))
    (is (= "real" (one c "SELECT pg_typeof(r) FROM t")))
    (is (= "real" (one c "SELECT pg_typeof(r+r) FROM t")))
    (testing "but there is no float4-with-anything-else operator, so a
              mixed expression resolves to float8 -- `real * 2` is
              double precision in PostgreSQL"
      (is (= "double precision" (one c "SELECT pg_typeof(r*2) FROM t")))
      (is (= "double precision" (one c "SELECT pg_typeof(r+d) FROM t"))))))

(deftest float4-arithmetic-computes-at-float4-precision
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "2.2" (one c "SELECT r+r FROM t"))
        "widening both operands to double first gives 2.200000047683716")
    (is (= "1.1" (one c "SELECT sum(r) FROM t"))
        "sum(float4) accumulates via float4pl, so the running total stays
         a float4")
    (testing "and widening is still visible where PostgreSQL widens"
      (is (= "1.100000023841858" (one c "SELECT 1.1::real::float8")))
      (is (= "f" (one c "SELECT 1.1::real = 1.1::float8"))))))

(deftest arithmetic-coerces-unknown-string-literals
  (with-open [c (jdbc)]
    (seed! c)
    (testing "a column resolves the quoted operand's unknown type"
      (is (= "-11" (one c "SELECT r * '-10' FROM t")))
      (is (= "-8.9" (one c "SELECT r + '-10' FROM t")))
      (is (= "-0.11" (one c "SELECT r / '-10' FROM t")))
      (is (= "11.1" (one c "SELECT r - '-10' FROM t"))))
    (testing "resolution is symmetric"
      (is (= "-11" (one c "SELECT '-10' * d FROM t"))))
    (testing "the same resolution applies to PostgreSQL's power operator"
      (is (= "double precision" (one c "SELECT pg_typeof(d ^ '2.0') FROM t")))
      (is (= "1.2100000000000002" (one c "SELECT d ^ '2.0' FROM t"))))))

(deftest float-overflow-and-underflow-raise
  (with-open [c (jdbc)]
    (testing "a result that came out infinite from finite inputs is 22003,
              not an Infinity that propagates through the query"
      (is (thrown-with-msg? SQLException #"value out of range: overflow"
                            (one c "SELECT 1e308::float8 * 10")))
      (is (thrown-with-msg? SQLException #"value out of range: overflow"
                            (one c "SELECT 1e308::float8 + 1e308::float8")))
      (is (thrown-with-msg? SQLException #"value out of range: overflow"
                            (one c "SELECT (-1e308)::float8 - 1e308::float8"))))
    (testing "and multiply underflows to zero from non-zero inputs"
      (is (thrown-with-msg? SQLException #"value out of range: underflow"
                            (one c "SELECT 1e-320::float8 * 1e-10"))))
    (testing "ordinary arithmetic is unaffected"
      (is (= "3" (one c "SELECT 1e0::float8 + 2e0::float8")))
      (is (= "0" (one c "SELECT 0.0::float8 * 5"))))))

(deftest float-to-numeric-uses-postgres-precision
  (with-open [c (jdbc)]
    (testing "%.15g for float8 -- the cast deliberately drops digits
              beyond the type's guaranteed precision"
      (is (= "0.3" (one c "SELECT (0.1::float8 + 0.2::float8)::numeric")))
      (is (= "0.333333333333333" (one c "SELECT (1.0/3.0)::float8::numeric")))
      (is (= "0.1" (one c "SELECT 0.1::float8::numeric"))))
    (testing "and %.6g for float4"
      (is (= "1.1" (one c "SELECT 1.1::real::numeric"))))))
