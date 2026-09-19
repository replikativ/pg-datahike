(ns datahike.test.pg-type-output-test
  "Values render, parse and convert by their SQL TYPE, not by their JVM class.

   Several PostgreSQL types share one carrier here: money and numeric are
   both BigDecimal, time columns keep text, and date, timestamp and
   timestamptz are all instants. Renderers that dispatched on the value's
   class printed money without `$`, `10:00` for a time, `.120` for a
   fraction and a timestamp array element via java.util.Date.toString in
   the JVM's zone. AT TIME ZONE was an identity, a bare `(a, b)` leaked
   query variable names, and time/money input accepted garbage.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"tv" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- jdbc ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/tv?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- state [^Connection c sql]
  (try (exec! c sql) nil (catch SQLException e (.getSQLState e))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE tv (id int, t time, ttz timetz, tz timestamptz, ts timestamp, m money)")
  (exec! c (str "INSERT INTO tv VALUES (1, '10:00', '10:00+02', '2020-01-01 10:00:00.5+00:00', "
                "'2020-01-01 10:00:00.12', -1234567.891), (2, NULL, NULL, NULL, NULL, NULL)")))

(deftest columns-render-by-type
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [["10:00:00" "10:00:00+02" "2020-01-01 10:00:00.5+00" "2020-01-01 10:00:00.12"
             "-$1,234,567.89"]]
           (rows c "SELECT t, ttz, tz, ts, m FROM tv WHERE id = 1")))
    (is (= [["time with time zone"]] (rows c "SELECT pg_typeof(ttz) FROM tv WHERE id = 1"))
        "a `timetz` column is declared as such, not text")
    (testing "every value->text path agrees: casts, ||, concat"
      (is (= [["10:00:00" "2020-01-01 10:00:00.12" "-$1,234,567.89" "x-$1,234,567.89"
               "10:00:00|-$1,234,567.89"]]
             (rows c "SELECT t::text, ts::text, m::text, 'x' || m, concat(t, '|', m) FROM tv WHERE id = 1"))))
    (testing "array elements and record fields use their type's output"
      (is (= [["{\"2020-01-01 10:00:00.12\"}" "{\"-$1,234,567.89\"}"
               "(10:00:00,\"-$1,234,567.89\")" "(10:00:00,\"-$1,234,567.89\")" "record"]]
             (rows c "SELECT ARRAY[ts], ARRAY[m], ROW(t, m), (t, m)::text, pg_typeof((t, m)) FROM tv WHERE id = 1"))))
    (testing "SQL NULL never renders as the internal sentinel"
      (is (= [["|" "{NULL}"]] (rows c "SELECT concat(t, '|'), ARRAY[t] FROM tv WHERE id = 2"))))))

(deftest bare-parenthesised-list-is-a-row
  (with-open [c (jdbc)]
    ;; The numeric literals are templated to $N on the simple-query path;
    ;; this answered ["?p1", "?p2"].
    (is (= [["(1,2)" "(1,2)"]] (rows c "SELECT (1,2), (1,2)::text")))))

(deftest at-time-zone
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [["2020-01-01 15:00:00.12+00" "2020-01-01 15:30:00.5" "2020-01-01 15:00:00.12+00"
             "timestamp with time zone" "timestamp without time zone" "08:00:00+00"]]
           (rows c (str "SELECT ts AT TIME ZONE 'America/New_York', tz AT TIME ZONE 'Asia/Kolkata', "
                        "ts AT TIME ZONE '+05', pg_typeof(ts AT TIME ZONE 'UTC'), "
                        "pg_typeof(tz AT TIME ZONE 'UTC'), ttz AT TIME ZONE 'UTC' FROM tv WHERE id = 1"))))
    (testing "a chain is left-associative"
      (is (= [["2020-01-01 11:00:00.12"]]
             (rows c "SELECT ts AT TIME ZONE 'UTC' AT TIME ZONE 'Europe/Berlin' FROM tv WHERE id = 1"))))
    (is (= "22023" (state c "SELECT ts AT TIME ZONE 'Mars/Olympus' FROM tv")))))

(deftest time-and-money-input
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [["13:02:03.5" "10:00:00.123457" "10:00:00-03:30" "$1,234.50" "-$12.00"]]
           (rows c "SELECT '1:2:3.5 PM'::time, '10:00:00.1234567'::time, '10:00-03:30'::timetz, '$1,234.5'::money, '(12)'::money")))
    (testing "invalid input raises instead of passing through"
      (is (= "22007" (state c "SELECT 'garbage'::time")))
      (is (= "22008" (state c "SELECT '25:00'::time")))
      (is (= "22P02" (state c "SELECT '2020-01-01'::money")))
      (is (= "22P02" (state c "SELECT '1-2'::money"))))
    (testing "writes run the input function: validated and canonical"
      (is (= "22008" (state c "INSERT INTO tv (id, t) VALUES (3, '25:00')")))
      (is (= "22007" (state c "INSERT INTO tv (id, ttz) VALUES (3, 'garbage')")))
      (exec! c "INSERT INTO tv (id, t) VALUES (4, '2:05 PM')")
      (exec! c "UPDATE tv SET t = '1:2:3' WHERE id = 1")
      (is (= [["1" "01:02:03"] ["4" "14:05:00"]]
             (rows c "SELECT id, t FROM tv WHERE t IS NOT NULL ORDER BY id"))))))

(deftest end-of-day-time
  ;; 24:00:00 is a valid time; pgjdbc sends LocalTime.MAX as it
  ;; (PreparedStatementTest.testLocalTimeMax). java.time cannot hold it, so
  ;; it travels as its canonical text.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE eod (id int, t time, tz timetz)")
    (exec! c "INSERT INTO eod VALUES (1, '24:00', '24:00+02'), (2, '12:00', '00:00')")
    (is (= [["2" "12:00:00" "00:00:00+00"] ["1" "24:00:00" "24:00:00+02"]]
           (rows c "SELECT id, t, tz FROM eod ORDER BY t")))
    (with-open [ps (.prepareStatement c "INSERT INTO eod (id, t) VALUES (3, ?)")]
      (.setObject ps 1 java.time.LocalTime/MAX)
      (.executeUpdate ps))
    (with-open [st (.createStatement c) rs (.executeQuery st "SELECT t FROM eod WHERE id = 3")]
      (is (.next rs))
      (is (= java.time.LocalTime/MAX (.getObject rs 1 java.time.LocalTime))))
    (is (= "22008" (state c "SELECT '24:00:01'::time")))))
