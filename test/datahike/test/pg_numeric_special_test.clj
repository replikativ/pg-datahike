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

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [out []]
      (if (.next rs)
        (recur (conj out [(.getString rs 1) (.getString rs 2)]))
        out))))

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
    (testing "quoted unknown operands resolve from a cast expression's OID"
      (is (= "NaN" (one c "SELECT 'NaN'::numeric / '0'")))
      (is (= "NaN" (one c "SELECT 'NaN'::numeric % '0'")))
      (is (zero? (bigdec (one c "SELECT '1'::numeric / 'Infinity'"))))
      (let [e (is (thrown? SQLException
                           (one c "SELECT 'Infinity'::numeric / '0'")))]
        (is (= "22012" (.getSQLState ^SQLException e)))))
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
    (testing "numeric transcendental overloads retain special values"
      (is (= "Infinity" (one c "SELECT sqrt('Infinity'::numeric)")))
      (is (= "0" (one c "SELECT exp('-Infinity'::numeric)")))
      (is (= "Infinity" (one c "SELECT ln('Infinity'::numeric)")))
      (is (= "-Infinity" (one c "SELECT power('-Infinity'::numeric, 3)")))
      (is (thrown-with-msg? SQLException #"cannot take square root"
                            (one c "SELECT sqrt('-Infinity'::numeric)"))))
    (testing "homogeneous function calls resolve unknown arguments"
      (is (= "NaN" (one c "SELECT div('NaN'::numeric, '0')")))
      (is (thrown-with-msg? SQLException #"division"
                            (one c "SELECT div('Infinity'::numeric, '0')")))
      (is (thrown-with-msg? SQLException #"negative"
                            (one c "SELECT log('-Infinity'::numeric, '10')"))))
    (testing "width_bucket accepts special operands but rejects special bounds"
      (is (= "11" (one c "SELECT width_bucket('Infinity'::numeric, 1, 10, 10)")))
      (is (= "0" (one c "SELECT width_bucket('-Infinity'::numeric, 1, 10, 10)")))
      (is (= "889" (one c "SELECT width_bucket('NaN', 3.0, 4.0, 888)")))
      (is (thrown-with-msg? SQLException #"bounds must be finite"
                            (one c "SELECT width_bucket(0::numeric, 'Infinity'::numeric, 5, 10)"))))
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

(deftest numeric-specials-persist-in-tables
  (with-open [c (jdbc)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE measurements (id int PRIMARY KEY, n numeric(20,2))")
      ;; This is the first transaction-breaking statement in PostgreSQL's
      ;; numeric regression test. Keep it in an explicit transaction so a
      ;; rejected special cannot hide behind an auto-commit boundary.
      (.execute st "BEGIN")
      (.executeUpdate st (str "INSERT INTO measurements VALUES "
                              "(1, 'NaN'), (2, 'Infinity'), "
                              "(3, '-Infinity'), (4, 1.25)"))
      (.execute st "COMMIT"))
    (testing "star projection decodes the at-rest representation"
      (is (= [["1" "NaN"] ["2" "Infinity"] ["4" "1.25"] ["3" "-Infinity"]]
             (rows c "SELECT * FROM measurements ORDER BY n DESC"))))
    (testing "unknown literals use numeric typinput and an indexable stored key"
      (is (= "1" (one c "SELECT id FROM measurements WHERE n = 'NaN'"))))
    (testing "UPDATE expressions receive SQL values, not storage sentinels"
      (with-open [st (.createStatement c)]
        (.executeUpdate st "UPDATE measurements SET n = n + 1 WHERE id = 2"))
      (is (= "Infinity" (one c "SELECT n FROM measurements WHERE id = 2")))
      (with-open [st (.createStatement c)]
        (.executeUpdate st "UPDATE measurements SET n = n * 0 WHERE id = 2"))
      (is (= "NaN" (one c "SELECT n FROM measurements WHERE id = 2"))))))

(deftest values-common-type-preserves-numeric-specials
  (with-open [c (jdbc)]
    (is (= [["NaN" "NaN"] ["Infinity" "Infinity"] ["0" "0"]
            ["-Infinity" "-Infinity"]]
           (rows c (str "WITH v(x) AS (VALUES ('0'::numeric), ('inf'), "
                        "('-inf'), ('nan')) "
                        "SELECT x, x::text FROM v ORDER BY x DESC"))))
    (testing "each CTE occurrence has its own column-definition alias"
      (is (= [["Infinity" "NaN"]]
             (rows c (str "WITH v(x) AS (VALUES ('0'::numeric), ('inf'), ('nan')) "
                          "SELECT x1, x2 FROM v AS v1(x1), v AS v2(x2) "
                          "WHERE x1 = 'inf'::numeric AND x2 = 'nan'::numeric")))))))
