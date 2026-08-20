(ns datahike.test.pg-numeric-special-test
  "NaN and +-Infinity as NUMERIC.

   PostgreSQL has had all three since 14, and none of them could exist
   here: `'NaN'::numeric` raised 22P02, because BigDecimal has no
   representation for any of them. They are carried by a small record
   instead -- the same shape PgBit and PgArray already use for values
   Clojure has no native equivalent of.

   Arithmetic involving one routes through double, which is exact for
   every case that can arise: `Inf - Inf` is NaN, `Inf * 0` is NaN,
   `x / Inf` is 0. Ordering is PostgreSQL's total order -- NaN above
   everything, then +Infinity, then the finite values, then -Infinity.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn ns-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"n" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each ns-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/n?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest numeric-specials-parse-and-render
  (with-open [c (jdbc)]
    (is (= "NaN" (one c "SELECT 'NaN'::numeric")))
    (is (= "Infinity" (one c "SELECT 'Infinity'::numeric")))
    (is (= "-Infinity" (one c "SELECT '-Infinity'::numeric")))
    (testing "the same spellings numeric_in accepts, which are float8in's"
      (is (= "NaN" (one c "SELECT 'nan'::numeric")))
      (is (= "Infinity" (one c "SELECT 'inf'::numeric"))))
    (is (= "numeric" (one c "SELECT pg_typeof('NaN'::numeric)")))))

(deftest arithmetic-follows-ieee
  (with-open [c (jdbc)]
    (is (= "NaN" (one c "SELECT 'NaN'::numeric + 1")))
    (is (= "NaN" (one c "SELECT 'NaN'::numeric * 0")))
    (is (= "NaN" (one c "SELECT 'Infinity'::numeric - 'Infinity'::numeric")))
    (is (= "Infinity" (one c "SELECT 'Infinity'::numeric * 2")))
    (testing "a defrecord IS a map, and translate-binary-arith used
              `map?` to spot an aggregate marker -- so these came back as
              the printed form of a compound-aggregate descriptor"
      (is (not (re-find #"compound-agg" (str (one c "SELECT 'NaN'::numeric + 1"))))))))

(deftest ordering-is-postgres-total-order
  (with-open [c (jdbc)]
    (is (= "t" (one c "SELECT 'NaN'::numeric = 'NaN'::numeric")))
    (is (= "t" (one c "SELECT 'NaN'::numeric > 1000000")))
    (is (= "t" (one c "SELECT 'Infinity'::numeric > 1e308")))
    (is (= "f" (one c "SELECT '-Infinity'::numeric > 0")))))

(deftest scalar-functions-pass-specials-through
  (with-open [c (jdbc)]
    (is (= "Infinity" (one c "SELECT round('Infinity'::numeric)")))
    (is (= "NaN" (one c "SELECT abs('NaN'::numeric)")))
    (is (= "Infinity" (one c "SELECT abs('-Infinity'::numeric)")))
    (is (= "NaN" (one c "SELECT sign('NaN'::numeric)")))
    (is (= "1" (one c "SELECT sign('Infinity'::numeric)")))
    (testing "scale has nothing to report for a special"
      (is (nil? (one c "SELECT scale('NaN'::numeric)"))))
    (testing "and the optional second argument still works -- the
              pass-through wrapper has to be variadic"
      (is (= "1.01" (one c "SELECT round(1.005::numeric, 2)"))))))

(deftest casts-out-of-a-special
  (with-open [c (jdbc)]
    (is (= "NaN" (one c "SELECT 'NaN'::numeric::float8")))
    (is (= "Infinity" (one c "SELECT 'Infinity'::numeric::float8")))
    (is (thrown-with-msg? SQLException #"cannot convert NaN to integer"
                          (one c "SELECT 'NaN'::numeric::int")))
    (is (thrown-with-msg? SQLException #"cannot convert infinity to integer"
                          (one c "SELECT 'Infinity'::numeric::int")))))
