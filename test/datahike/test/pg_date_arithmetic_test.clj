(ns datahike.test.pg-date-arithmetic-test
  "`date + integer`, `date - integer` and `date - date` are their own
   operators in PostgreSQL (date_pli / date_mii / date_mi), not the
   numeric ones. Routing them through the numeric path leaked a raw
   ClassCastException -- \"class java.util.Date cannot be cast to class
   java.lang.Number\" -- so ordinary date arithmetic was an error.

   Which operand is a date has to be settled by the TRANSLATOR from the
   declared column type: Datahike stores `date` and `timestamp` columns
   alike as java.util.Date, so a runtime type test cannot tell them
   apart, and PostgreSQL answers the two differently (`timestamp -
   timestamp` is an interval, `date - date` is a count of days).

   Expectations here are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn date-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"dates" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each date-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/dates?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- sqlstate [^Connection c sql]
  (try
    (one c sql)
    nil
    (catch SQLException e (.getSQLState e))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ev (id int, d date, ts timestamp)")
  (exec! c "INSERT INTO ev VALUES (1,'2020-01-01','2020-01-01 10:00')"))

(deftest date-plus-minus-integer-on-a-column
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "2020-01-02" (one c "SELECT d + 1 FROM ev")))
    (is (= "2019-12-31" (one c "SELECT d - 1 FROM ev")))
    (is (= "2020-01-02" (one c "SELECT 1 + d FROM ev"))
        "addition commutes; PostgreSQL has no `integer - date`")
    (is (= "2020-01-02" (one c "SELECT d + id FROM ev"))
        "the integer operand may be a column rather than a literal")))

(deftest date-minus-date-is-a-count-of-days
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "0" (one c "SELECT d - d FROM ev")))
    (is (= "60" (one c "SELECT '2020-03-01'::date - '2020-01-01'::date"))
        "not an interval -- date_mi yields a plain integer")))

(deftest date-arithmetic-on-cast-literals
  (with-open [c (jdbc)]
    (is (= "2020-01-02" (one c "SELECT '2020-01-01'::date + 1")))
    (is (= "2019-12-25" (one c "SELECT '2020-01-01'::date - 7")))
    (is (= "2020-01-01" (one c "SELECT '2020-01-01'::date + 0")))
    (testing "the calendar, not a fixed 86400s day"
      (is (= "2021-01-01" (one c "SELECT '2020-12-31'::date + 1")))
      (is (= "2020-02-29" (one c "SELECT '2020-02-28'::date + 1")) "leap year")
      (is (= "2019-03-01" (one c "SELECT '2019-02-28'::date + 1")) "non-leap year"))))

(deftest date-arithmetic-reports-the-right-type
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "date" (one c "SELECT pg_typeof(d + 1) FROM ev"))
        "numeric promotion would say int8, and a binary-format client would
         then fail to decode the `2020-01-02` the renderer emits")
    (is (= "integer" (one c "SELECT pg_typeof(d - d) FROM ev")))))

(deftest date-arithmetic-outside-the-select-list
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "2020-01-02" (one c "SELECT d + 1 FROM ev WHERE d + 1 > '2020-01-01'::date"))
        "the same lowering has to happen in WHERE")
    (is (= "2020-01-02" (one c "SELECT max(d + 1) FROM ev"))
        "and under an aggregate")))

(deftest timestamp-columns-keep-their-own-semantics
  (with-open [c (jdbc)]
    (seed! c)
    (is (thrown? Exception (one c "SELECT ts + 1 FROM ev"))
        "PostgreSQL has no `timestamp + integer` operator either; the point
         is that a timestamp column must NOT quietly acquire date
         semantics just because it is stored as a java.util.Date")))

(deftest temporal-subtraction-produces-an-interval
  (with-open [c (jdbc)]
    (is (= "1 day 02:02:02"
           (one c (str "SELECT timestamp '2020-01-02 03:04:05' "
                       "- timestamp '2020-01-01 01:02:03'"))))
    (is (= "-1 days -02:02:02"
           (one c (str "SELECT timestamp '2020-01-01 01:02:03' "
                       "- timestamp '2020-01-02 03:04:05'"))))
    (is (= "02:02:02"
           (one c "SELECT time '03:04:05' - time '01:02:03'")))
    (is (= "interval"
           (one c (str "SELECT pg_typeof(timestamp '2020-01-02 03:04:05' "
                       "- timestamp '2020-01-01 01:02:03')"))))
    (is (= "interval"
           (one c "SELECT pg_typeof(time '03:04:05' - time '01:02:03')")))))

(deftest unsupported-temporal-families-do-not-leak-host-casts
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE spans (span interval)")
    (exec! c "INSERT INTO spans VALUES ('1 day')")
    (is (= "interval" (one c "SELECT pg_typeof(span) FROM spans")))
    (is (= "42883" (sqlstate c "SELECT time '03:04:05' + time '01:02:03'")))
    (is (= "42883" (sqlstate c "SELECT -('1 day'::interval)")))
    (is (= "42883" (sqlstate c "SELECT '1 day'::interval * 2")))
    (is (= "42883" (sqlstate c "SELECT '1 day'::interval - '1 day'::interval")))
    (is (= "42883" (sqlstate c "SELECT -span FROM spans")))
    (is (= "42883" (sqlstate c "SELECT span - span FROM spans")))
    (is (= "0A000"
           (sqlstate c "SELECT to_char(timestamp '2020-01-01', 'YYYY-MM-DD')")))
    (is (= "0A000" (sqlstate c "SELECT to_char('1 day'::interval, 'YYYY')")))))
