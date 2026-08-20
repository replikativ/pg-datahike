(ns datahike.test.pg-random-test
  "random(), setseed() and random_normal().

   PostgreSQL's PRNG is ported (src/common/pg_prng.c: xoroshiro128** with
   a splitmix64 seeder) rather than substituted for java.util.Random,
   and the reason is testability: with the algorithm shared,
   `SETSEED(0.5)` followed by N draws is ONE EXACT SEQUENCE, so this can
   be a differential against a real PostgreSQL rather than a smoke test
   that only checks the range. The literals below are that PostgreSQL's
   output.

   The other half is that `random()` is VOLATILE. A datalog function
   binding with no inputs is evaluated once for the whole query, so
   `SELECT random() FROM t` handed every row the same draw."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn r-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"r" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each r-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/r?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc))))

(def ^:private seed-half-sequence
  "PostgreSQL 17's first five draws after SETSEED(0.5)."
  ["0.9851677175347999" "0.825301858027981" "0.12974610012450416"
   "0.16356291958601088" "0.6476186144084"])

(deftest seeded-sequence-matches-postgres-exactly
  (with-open [c (jdbc)]
    (one c "SELECT setseed(0.5)")
    (is (= seed-half-sequence
           (mapv (fn [_] (one c "SELECT random()")) (range 5)))
        "bit-for-bit, which is only possible because the generator is
         PostgreSQL's own rather than an equivalent-quality substitute")))

(deftest random-is-volatile-per-row
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int)")
    (exec! c "INSERT INTO t VALUES (1),(2),(3),(4),(5)")
    (one c "SELECT setseed(0.5)")
    (is (= seed-half-sequence (col c "SELECT random() FROM t ORDER BY id"))
        "one draw per ROW. A zero-argument datalog binding is evaluated
         once for the whole query, so this used to repeat a single draw
         five times")))

(deftest setseed-validates-its-range
  (with-open [c (jdbc)]
    (testing "PostgreSQL restricts the argument to [-1,1] and rejects NaN"
      (is (thrown-with-msg? SQLException #"out of allowed range"
                            (one c "SELECT setseed(2)")))
      (is (thrown-with-msg? SQLException #"out of allowed range"
                            (one c "SELECT setseed(-1.5)")))
      (is (thrown-with-msg? SQLException #"out of allowed range"
                            (one c "SELECT setseed('NaN'::float8)"))))
    (testing "the endpoints are allowed"
      (is (= "" (one c "SELECT setseed(1)")))
      (is (= "" (one c "SELECT setseed(-1)")))
      (is (= "" (one c "SELECT setseed(0)"))
          "void renders as one EMPTY row -- returning nil would have
           filtered the row away entirely"))))

(deftest reseeding-restarts-the-same-sequence
  (with-open [c (jdbc)]
    (one c "SELECT setseed(0.5)")
    (let [first-run (mapv (fn [_] (one c "SELECT random()")) (range 3))]
      (one c "SELECT setseed(0.5)")
      (is (= first-run (mapv (fn [_] (one c "SELECT random()")) (range 3)))))))

(deftest random-and-random-normal-shape
  (with-open [c (jdbc)]
    (is (= "double precision" (one c "SELECT pg_typeof(random())")))
    (one c "SELECT setseed(0.5)")
    (let [vs (mapv (fn [_] (Double/parseDouble (one c "SELECT random()"))) (range 20))]
      (is (every? #(and (>= % 0.0) (< % 1.0)) vs) "half-open [0,1)")
      (is (= 20 (count (distinct vs)))))
    (is (some? (one c "SELECT random_normal(0,1)")))))
