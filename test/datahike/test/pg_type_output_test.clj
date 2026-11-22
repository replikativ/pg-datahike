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
    (testing "converting either timestamp kind preserves microseconds"
      (is (= [["2024-03-01 00:30:00.654321+00" "2024-03-01 01:30:00.654321"]]
             (rows c (str "SELECT timestamp '2024-03-01 00:30:00.654321' AT TIME ZONE 'UTC', "
                          "timestamptz '2024-03-01 00:30:00.654321+00' AT TIME ZONE 'Europe/Berlin'")))))
    (testing "named zones use the PostgreSQL gap and overlap offsets"
      (is (= [["2024-11-03 06:30:00.123456+00" "2024-03-10 07:30:00.123456+00"]]
             (rows c (str "SELECT TIMESTAMP '2024-11-03 01:30:00.123456' AT TIME ZONE 'America/New_York', "
                          "TIMESTAMP '2024-03-10 02:30:00.123456' AT TIME ZONE 'america/new_york'")))))
    (testing "fixed abbreviations do not acquire a region's summer offset"
      (is (= [["2024-07-01 20:00:00+00" "2024-07-01 19:00:00+00" "08:00:00-04"]]
             (rows c (str "SELECT TIMESTAMP '2024-07-01 12:00' AT TIME ZONE 'PST', "
                          "TIMESTAMP '2024-07-01 12:00' AT TIME ZONE 'America/Los_Angeles', "
                          "TIMETZ '12:00+00' AT TIME ZONE 'EDT'")))))
    (testing "the session offset reported by PostgreSQL is a valid zone"
      (exec! c "SET TIME ZONE 2.5")
      (is (= [["<+02:30>-02:30" "10:30:00+02:30" "2020-01-01 12:30:00.5"]]
             (rows c (str "SELECT current_setting('TimeZone'), "
                          "ttz AT TIME ZONE current_setting('TimeZone'), "
                          "tz AT TIME ZONE current_setting('TimeZone') FROM tv WHERE id = 1"))))
      (exec! c "SET TIME ZONE '<-07:30>+07:30'")
      (is (= [["<-07:30>+07:30" "00:30:00-07:30"]]
             (rows c (str "SELECT current_setting('TimeZone'), "
                          "ttz AT TIME ZONE current_setting('TimeZone') FROM tv WHERE id = 1")))))
    (is (= "22023" (state c "SELECT ts AT TIME ZONE '<EAST>-02:60' FROM tv")))
    (is (= "22023" (state c "SELECT ts AT TIME ZONE 'Mars/Olympus' FROM tv")))))

(deftest timezone-coerces-date-and-untyped-timestamp-operands
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 'America/New_York'")
    (is (= [["2024-03-10 05:00:00" "2024-03-10 05:00:00"]]
           (rows c (str "SELECT date '2024-03-10' AT TIME ZONE 'UTC', "
                        "timezone('UTC', date '2024-03-10')"))))
    (is (= [["2024-03-10 01:30:00.123456" "2024-03-10 01:30:00.123456"]]
           (rows c (str "SELECT ('2024-03-10 03:30:00.123456+02') AT TIME ZONE 'UTC', "
                        "timezone('UTC', '2024-03-10 03:30:00.123456+02')"))))
    (exec! c "CREATE TABLE timezone_days (d date)")
    (exec! c "INSERT INTO timezone_days VALUES ('2024-03-10'), (NULL)")
    (exec! c "PREPARE zone_days AS SELECT d AT TIME ZONE 'UTC' FROM timezone_days ORDER BY d")
    (is (= [["2024-03-10 05:00:00"] [nil]] (rows c "EXECUTE zone_days")))
    (exec! c "BEGIN")
    (exec! c "SET LOCAL TIME ZONE 'Europe/Paris'")
    (is (= [["2024-03-09 23:00:00"] [nil]] (rows c "EXECUTE zone_days")))
    (exec! c "ROLLBACK")
    (is (= [["2024-03-10 05:00:00"] [nil]] (rows c "EXECUTE zone_days")))))

(deftest historical-time-zone-abbreviations
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 'UTC'")
    (testing "wall-clock conversion at a gap and an overlap"
      (doseq [[day cases]
              [["2011-03-27" [["00:00:00" 3] ["01:00:00" 3] ["01:59:59" 3]
                              ["02:00:00" 4] ["02:00:01" 4] ["02:59:59" 4]
                              ["03:00:00" 4] ["03:00:01" 4] ["04:00:00" 4]]]
               ["2014-10-26" [["00:00:00" 4] ["00:59:59" 4] ["01:00:00" 3]
                              ["01:00:01" 3] ["02:00:00" 3]]]]
              [time hours] cases]
        (let [local (str day " " time)
              result (-> (java.time.LocalDateTime/parse (str day "T" time))
                         (.minusHours hours)
                         (.format (java.time.format.DateTimeFormatter/ofPattern "uuuu-MM-dd HH:mm:ss")))]
          (is (= [[(str result "+00")]]
                 (rows c (str "SELECT timestamp '" local "' AT TIME ZONE 'MSK'"))) local))))
    (testing "instant conversion uses the offset on the correct side of the transition"
      (doseq [[day cases]
              [["2011-03-26" [["21:00:00" 3] ["22:00:00" 3] ["22:59:59" 3]
                              ["23:00:00" 4] ["23:00:01" 4] ["23:59:59" 4]]]
               ["2011-03-27" [["00:00:00" 4]]]
               ["2014-10-25" [["21:00:00" 4] ["21:59:59" 4] ["22:00:00" 3]
                              ["22:00:01" 3] ["23:00:00" 3]]]]
              [time hours] cases]
        (let [utc (str day " " time)
              result (-> (java.time.LocalDateTime/parse (str day "T" time))
                         (.plusHours hours)
                         (.format (java.time.format.DateTimeFormatter/ofPattern "uuuu-MM-dd HH:mm:ss")))]
          (is (= [[result]]
                 (rows c (str "SELECT timestamptz '" utc " UTC' AT TIME ZONE 'MSK'"))) utc))))
    (is (= [["2014-10-25 20:00:00+00" "2014-10-25 22:00:00+00"]]
           (rows c "SELECT make_timestamptz(2014, 10, 26, 0, 0, 0, 'MSK'), make_timestamptz(2014, 10, 26, 1, 0, 0, 'MSK')")))
    (testing "the first recorded meaning and the most recent one precede the zone's own offset"
      (is (= [["1899-12-31 21:00:00.123456+00" "1899-12-31 21:29:43.123456+00"
               "2010-06-30 21:00:00+00" "2010-06-30 20:00:00+00"]]
             (rows c (str "SELECT timestamp '1900-01-01 00:00:00.123456' AT TIME ZONE 'msk', "
                          "timestamp '1900-01-01 00:00:00.123456' AT TIME ZONE 'Europe/Moscow', "
                          "timestamptz '2010-07-01 00:00 MSK', "
                          "date_trunc('day', timestamptz '2010-07-01 12:00 UTC', 'MSK')"))))
      (is (= [["2010-06-30 21:00:00+00" "2024-01-01 04:00:00+00"]]
             (rows c (str "SELECT to_timestamp('2010-07-01 00:00:00 MSK', 'YYYY-MM-DD HH24:MI:SS TZ'), "
                          "timestamp '2024-01-01' AT TIME ZONE 'VET'"))))
      (with-open [stmt (.createStatement c)
                  rs (.executeQuery stmt "SELECT timestamp '2010-07-01' AT TIME ZONE 'MSK'")]
        (is (= "timezone" (.getColumnLabel (.getMetaData rs) 1)))))))

(deftest at-time-zone-with-interval
  (with-open [c (jdbc)]
    (is (= [["11:45:03.123456+00" "01:45:03.123456-10"
             "2024-02-29 22:00:00.654321+00" "2024-03-01 03:00:00.654321"]]
           (rows c (str "SELECT TIMETZ '17:15:03.123456+05:30' AT TIME ZONE INTERVAL '00:00', "
                        "TIMETZ '17:15:03.123456+05:30' AT TIME ZONE INTERVAL '-10:00', "
                        "TIMESTAMP '2024-03-01 00:30:00.654321' AT TIME ZONE INTERVAL '+02:30', "
                        "TIMESTAMPTZ '2024-03-01 00:30:00.654321+00' AT TIME ZONE INTERVAL '+02:30'"))))
    (testing "a seconds offset truncates its fraction and can exceed the Java zone range"
      (is (= [["14:31:02+20:01:02" "18:30:00+00"]]
             (rows c (str "SELECT TIMETZ '24:00+05:30' AT TIME ZONE INTERVAL '+20:01:02.5', "
                          "(TIMETZ '24:00+05:30' AT TIME ZONE INTERVAL '+20:01:02.5') AT TIME ZONE 'UTC'")))))
    (testing "a stored interval resolves to the same overload"
      (exec! c "CREATE TABLE intervals (z interval, t timetz)")
      (exec! c "INSERT INTO intervals VALUES ('-10:00', '10:00:00.123456+02')")
      (is (= [["22:00:00.123456-10" "22:00:00.123456-10" "22:00:00.123456-10" "22:00:00.123456-10"]]
             (rows c (str "SELECT t AT TIME ZONE 'UTC+10', t AT TIME ZONE INTERVAL '-10:00', "
                          "t AT TIME ZONE z, timezone(z, t) FROM intervals ORDER BY t")))))
    (is (= [[nil "infinity"]]
           (rows c (str "SELECT TIMETZ '00:00+00' AT TIME ZONE NULL::interval, "
                        "TIMESTAMP 'infinity' AT TIME ZONE INTERVAL '1 day'"))))
    (doseq [zone ["1 day" "1 month" "infinity"]]
      (is (= "22023" (state c (str "SELECT TIMETZ '00:00+00' AT TIME ZONE INTERVAL '" zone "'")))))))

(deftest fixed-expression-zone-offsets-match-postgres
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 'UTC'")
    (is (= [["2020-05-03 03:04:05.123456+00" "2020-05-01 03:04:05.123456"
             "03:04:05.123456-168" "2020-05-02 00:00:00+00"
             "2020-05-02 04:06:08.123456+00" "2020-05-01 21:34:05.123456+00"]]
           (rows c (str "SELECT timestamp '2020-05-02 03:04:05.123456' AT TIME ZONE 'UTC+24', "
                        "timestamptz '2020-05-02 03:04:05.123456+00' AT TIME ZONE 'UTC+24', "
                        "timetz '03:04:05.123456+00' AT TIME ZONE 'UTC+167:59:60', "
                        "date_trunc('day', timestamptz '2020-05-02 03:04:05.123456+00', 'UTC+24'), "
                        "timezone('UTC+1:2:3', timestamp '2020-05-02 03:04:05.123456'), "
                        "make_timestamptz(2020, 5, 2, 3, 4, 5.123456, '+0530')"))))
    (doseq [zone ["UTC+168" "UTC+24:60" "UTC+24:10:61" "+0530" "UTC "]]
      (is (= "22023" (state c (str "SELECT timestamp '2020-05-02' AT TIME ZONE '" zone "'")))
          (pr-str zone)))
    (doseq [zone ["24" "+24" "-24:30:1"]]
      (is (= "22023" (state c (str "SELECT make_timestamptz(2020, 5, 2, 0, 0, 0, '" zone "')")))
          (pr-str zone)))
    (with-open [stmt (.prepareStatement c
                                        (str "SELECT timezone(?, timestamp '2020-05-02 03:04:05.123456'), "
                                             "make_timestamptz(2020, 5, 2, 3, 4, 5.123456, ?)"))]
      (.setString stmt 1 "+05:30")
      (.setString stmt 2 "+05:30")
      (with-open [result (.executeQuery stmt)]
        (is (.next result))
        (is (= ["2020-05-02 08:34:05.123456+00" "2020-05-01 21:34:05.123456+00"]
               (mapv #(.getString result (int %)) [1 2])))))))

(deftest insert-timezone-expressions-use-the-execution-clock
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE clock_writes (id int, a timestamp, b timestamp, rendered text)")
    (exec! c (str "PREPARE insert_clock (int, text) AS INSERT INTO clock_writes VALUES "
                  "($1, now() AT TIME ZONE $2, timezone($2, now()), (now() AT TIME ZONE $2)::text)"))
    (exec! c "BEGIN")
    (exec! c "EXECUTE insert_clock(1, 'Asia/Kolkata')")
    (is (= [["t" "t" "t"]]
           (rows c (str "SELECT a = b, a = timezone('Asia/Kolkata', now()), rendered = a::text "
                        "FROM clock_writes WHERE id = 1"))))
    (exec! c "COMMIT")
    (exec! c "SELECT pg_sleep(0.025)")
    (exec! c "EXECUTE insert_clock(2, 'Asia/Kolkata')")
    (is (= [["t"]]
           (rows c (str "SELECT later.a > earlier.a FROM clock_writes earlier "
                        "CROSS JOIN clock_writes later WHERE earlier.id = 1 AND later.id = 2"))))
    (is (= "22023" (state c "EXECUTE insert_clock(3, 'Nonsense/Zone')")))
    (with-open [st (.prepareStatement c
                                      (str "INSERT INTO clock_writes VALUES "
                                           "(3, now() AT TIME ZONE ?, timezone(?, now()), "
                                           "(now() AT TIME ZONE ?)::text)"))]
      (doseq [index [1 2 3]]
        (.setString st index "Asia/Kolkata"))
      (is (= 1 (.executeUpdate st))))
    (is (= [["t" "t"]] (rows c "SELECT a = b, rendered = a::text FROM clock_writes WHERE id = 3")))))

(deftest timezone-uses-the-current-session-setting
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 2.5")
    (is (= [["2024-03-01 00:30:00.654321+02:30" "2024-03-01 03:00:00.654321"
             "14:15:03.123456+02:30" "12:00:00+02:30" "2024-07-01 00:00:00"
             "2024-07-01 03:00:00" nil "timestamp with time zone"]]
           (rows c (str "SELECT timezone(TIMESTAMP '2024-03-01 00:30:00.654321'), "
                        "timezone(TIMESTAMPTZ '2024-03-01 00:30:00.654321+00'), "
                        "timezone(TIMETZ '17:15:03.123456+05:30'), timezone(TIME '12:00'), "
                        "timezone(DATE '2024-07-01'), timezone('2024-07-01 01:30+01'), "
                        "timezone(NULL::timestamp), pg_typeof(timezone(TIMESTAMP '2024-03-01 00:30'))"))))
    (exec! c "PREPARE local_tz AS SELECT timezone(TIMETZ '17:15:03.123456+05:30')")
    (is (= [["14:15:03.123456+02:30"]] (rows c "EXECUTE local_tz")))
    (exec! c "SET TIME ZONE '-7.5'")
    (is (= [["04:15:03.123456-07:30"]] (rows c "EXECUTE local_tz")))
    (exec! c "RESET TIME ZONE")
    (is (= [["11:45:03.123456+00"]] (rows c "EXECUTE local_tz")))
    (is (= "42883" (state c "SELECT timezone(42)")))
    (is (= "42883" (state c "SELECT timezone('12:00'::text)")))))

(deftest timezone-coerces-plain-time-in-the-current-session-zone
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE local_times (t time)")
    (exec! c "INSERT INTO local_times VALUES ('12:00:00.123456')")
    (exec! c (str "PREPARE t_conv AS SELECT t AT TIME ZONE INTERVAL '00:00', "
                  "timezone('UTC', t), pg_typeof(t AT TIME ZONE 'UTC') FROM local_times"))
    (exec! c "SET TIME ZONE 2.5")
    (is (= [["09:30:00.123456+00" "09:30:00.123456+00" "time with time zone"]]
           (rows c "EXECUTE t_conv")))
    (exec! c "SET TIME ZONE '-7.5'")
    (is (= [["19:30:00.123456+00" "19:30:00.123456+00" "time with time zone"]]
           (rows c "EXECUTE t_conv")))
    (is (= [[nil]] (rows c "SELECT NULL::time AT TIME ZONE INTERVAL '00:00'")))))

(deftest prepared-timezone-overloads
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 'America/New_York'")
    (doseq [[sql expected] [["SELECT timezone((?))" ["timestamptz"]]
                            ["SELECT timezone((?), (?))" ["text" "timestamptz"]]
                            ["SELECT (?) AT TIME ZONE (?) AT TIME ZONE (?)" ["timestamptz" "text" "text"]]]]
      (with-open [st (.prepareStatement c sql)]
        (let [meta (.getParameterMetaData st)]
          (is (= expected (mapv #(.getParameterTypeName meta (int %))
                                (range 1 (inc (.getParameterCount meta)))))))))
    (exec! c "PREPARE tz_session AS SELECT timezone(($1))")
    (exec! c "PREPARE tz_named AS SELECT timezone(($1), ($2))")
    (exec! c "PREPARE tz_chain AS SELECT ($1) AT TIME ZONE ($2) AT TIME ZONE ($3)")
    (is (= [["2024-03-09 20:30:00.123456"]]
           (rows c "EXECUTE tz_session ('2024-03-10 03:30:00.123456+02')")))
    (is (= [["2024-03-10 01:30:00.123456"]]
           (rows c "EXECUTE tz_named ('UTC', '2024-03-10 03:30:00.123456+02')")))
    (is (= [["2024-03-09 19:30:00.123456-05"]]
           (rows c "EXECUTE tz_chain ('2024-03-10 03:30:00.123456+02', 'UTC', 'Europe/Berlin')")))
    (exec! c "BEGIN")
    (exec! c "SET LOCAL TIME ZONE 'Asia/Tokyo'")
    (is (= [["2024-03-10 10:30:00.123456"]]
           (rows c "EXECUTE tz_session ('2024-03-10 03:30:00.123456+02')")))
    (exec! c "ROLLBACK")
    (is (= [["2024-03-09 20:30:00.123456"]]
           (rows c "EXECUTE tz_session ('2024-03-10 03:30:00.123456+02')")))
    (is (= [["2023-12-31 17:30:00-05" "2023-12-31 17:30:00-05"]]
           (rows c (str "SELECT timezone(TIME '01:30', TIMESTAMP '2024-01-01'), "
                        "TIMESTAMP '2024-01-01' AT TIME ZONE TIME '01:30'"))))
    (doseq [sql ["SELECT timezone(12, TIMESTAMP '2024-01-01')"
                 "SELECT timezone('UTC', '2024-01-01'::text)"
                 "SELECT '2024-01-01'::text AT TIME ZONE 'UTC'"
                 "SELECT TIMESTAMP '2024-01-01' AT TIME ZONE 12"]]
      (is (= "42883" (state c sql)) sql))))

(deftest time-expression-metadata
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE empty_times (t time)")
    (is (= "42725" (state c "SELECT t + time '00:01' FROM empty_times")))
    (exec! c "CREATE TABLE empty_zoned_times (t timetz)")
    (is (= "42883" (state c "SELECT t + time with time zone '00:01' FROM empty_zoned_times")))
    (with-open [st (.createStatement c)
                rs (.executeQuery st "SELECT EXTRACT(SECOND FROM TIME '13:30:25.575401')")]
      (is (= "extract" (.getColumnLabel (.getMetaData rs) 1)))
      (is (.next rs))
      (is (= "25.575401" (.getString rs 1))))))

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

(deftest timetz-order-and-view-output
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE zoned_times (id int, t timetz)")
    (exec! c (str "INSERT INTO zoned_times VALUES (1, '07:07-08'), (2, '08:08-04'), "
                  "(3, '10:00+01'), (4, '09:00+00'), (5, '24:00+00'), (6, NULL)"))
    (is (= [["3"] ["4"] ["2"] ["1"] ["5"] ["6"]]
           (rows c "SELECT id FROM zoned_times ORDER BY t")))
    (is (= [["6" nil] ["5" "24:00:00+00"] ["1" "07:07:00-08"]]
           (rows c "SELECT id, t AS x FROM zoned_times ORDER BY x DESC NULLS FIRST LIMIT 3")))
    (doseq [columns ["id, t" "*"]]
      (is (= [["3" "10:00:00+01"] ["4" "09:00:00+00"]]
             (rows c (str "SELECT " columns " FROM zoned_times ORDER BY 2 LIMIT 2")))))
    (is (= [["3" "10:00:00+01" "10:00:00+01"]]
           (rows c "SELECT *, t AS \"Local Clock\" FROM zoned_times ORDER BY \"Local Clock\" LIMIT 1")))
    (is (= [["t"]] (rows c "SELECT '10:00+01'::timetz < '09:00+00'::timetz")))
    (exec! c (str "CREATE VIEW zoned_view AS SELECT id, timezone('UTC', t) AS z "
                  "FROM zoned_times WHERE id IN (1, 2) ORDER BY t"))
    (is (= [["2" "12:08:00+00" "time with time zone"]
            ["1" "15:07:00+00" "time with time zone"]]
           (rows c "SELECT id, z, pg_typeof(z) FROM zoned_view")))))

(deftest time-precision-comparison-and-fixed-width-text
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE typed (id int, t time(3), tz timetz(6), fixed char(4))")
    (exec! c (str "INSERT INTO typed VALUES "
                  "(1, '23:59:59.999500', '14:00:00.123456+04', 'a'), "
                  "(2, '00:00:00', '24:00:00+02', 'b'), "
                  "(3, '00:00:00.000500', '00:00:00+02', 'c')"))
    (is (= [["24:00:00" "14:00:00.123456+04" "a   " "a"]
            ["00:00:00" "24:00:00+02" "b   " "b"]
            ["00:00:00.001" "00:00:00+02" "c   " "c"]]
           (rows c "SELECT t, tz, fixed, fixed::text FROM typed ORDER BY id")))
    (is (= [["2"]] (rows c "SELECT id FROM typed WHERE tz = '24:00+02'")))
    (is (= [["1"]] (rows c "SELECT id FROM typed WHERE t = CAST('24:00' AS time)")))
    (is (= [["1"] ["3"]]
           (rows c "SELECT id FROM typed WHERE tz < CAST('12:00:00+00' AS timetz) ORDER BY id")))
    (is (= "42883" (state c "SELECT id FROM typed WHERE tz = CAST('24:00+02' AS text)")))
    (with-open [ps (.prepareStatement c "SELECT id FROM typed WHERE tz = CAST(? AS timetz)")]
      (.setString ps 1 "14:00:00.123456+04")
      (with-open [rs (.executeQuery ps)]
        (is (.next rs))
        (is (= 1 (.getInt rs 1)))
        (is (false? (.next rs)))))))

(deftest bytea-survives-a-derived-relation
  ;; A derived relation re-infers each column's type from the VALUES it
  ;; carries. A byte[] matched no category, fell through to the
  ;; `:db.type/string` arm -- whose coercer is `str` -- and the column
  ;; was stored as the Java identity. Same defect a `PgArray` had.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tb (id int, b bytea)")
    (exec! c "INSERT INTO tb VALUES (1, '\\x0001ff'::bytea)")
    (is (= [["\\x0001ff"]] (rows c "SELECT b::text FROM tb"))
        "the base case was never broken")
    (testing "through a derived relation"
      (is (= [["\\x0001ff"]] (rows c "SELECT b::text FROM (SELECT b FROM tb) s")))
      (is (= [["\\x0001ff"]] (rows c "SELECT s.b::text FROM (SELECT b FROM tb) s")))
      (is (= [["bytea"]] (rows c "SELECT pg_typeof(b) FROM (SELECT b FROM tb) s"))
          "derived metadata preserves the bytea type as well as its value"))))

(deftest text-order-matches-the-collation-we-report
  ;; Text comparison here is `fns/order-cmp`'s `compare`, i.e.
  ;; `String.compareTo`, which is byte order: exactly what PostgreSQL
  ;; gives under `COLLATE "C"` and NOT what it gives under a locale.
  ;; Reporting `en_US.UTF-8` while sorting as `C` told every client that
  ;; reads `datcollate` something untrue. These two assertions have to
  ;; move together -- the claim is only honest while the order matches.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tc (t text)")
    (exec! c "INSERT INTO tc VALUES ('a'), ('B'), ('b'), ('A')")
    (is (= [["A"] ["B"] ["a"] ["b"]] (rows c "SELECT t FROM tc ORDER BY t"))
        "byte order; a locale-initialised cluster answers a,A,b,B")
    (is (= [["C" "C"]]
           (rows c (str "SELECT datcollate, datctype FROM pg_database"
                        " WHERE datname = current_database()")))
        "and the catalog reports the collation we actually implement")))

(deftest session-timezone-renders-timestamptz-only
  ;; The session TimeZone is read at exactly two boundaries: timestamptz
  ;; OUTPUT rendering, and INPUT of text carrying no explicit offset.
  ;;
  ;; `timestamp` and `date` must NEVER move. They are wall-clock values,
  ;; PostgreSQL does not apply TimeZone to them, and they share a
  ;; carrier with timestamptz here -- so the TYPE has to decide, not the
  ;; carrier. Applying the zone uniformly would silently shift every
  ;; stored timestamp, which is the whole reason this test exists.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tzr (ts timestamp, tstz timestamptz, d date)")
    (exec! c (str "INSERT INTO tzr VALUES ('2026-06-15 12:00:00',"
                  "'2026-06-15 12:00:00+00','2026-06-15')"))
    (let [read-all #(first (rows c "SELECT ts, tstz, d FROM tzr"))]
      (testing "the default is UTC and renders exactly as before"
        (is (= ["2026-06-15 12:00:00" "2026-06-15 12:00:00+00" "2026-06-15"]
               (read-all))))
      (testing "a named zone shifts ONLY the timestamptz, and is DST-aware"
        ;; June in Berlin is CEST, +02 -- a ZoneOffset cached per session
        ;; would have answered +01 here. The offset must be resolved at
        ;; the instant being rendered.
        (exec! c "SET timezone = 'Europe/Berlin'")
        (is (= ["2026-06-15 12:00:00" "2026-06-15 14:00:00+02" "2026-06-15"]
               (read-all))))
      (testing "and in January the same zone is +01"
        (exec! c "INSERT INTO tzr VALUES (NULL,'2026-01-15 12:00:00+00',NULL)")
        (is (= [["2026-01-15 13:00:00+01"]]
               (rows c (str "SELECT tstz FROM tzr WHERE tstz"
                            " < '2026-02-01 00:00:00+00'")))))
      (testing "a numeric zone uses PostgreSQL's POSIX sign"
        (exec! c "SET timezone = '+02:00'")
        (is (= "2026-06-15 10:00:00-02" (second (read-all))))
        (exec! c "SET timezone = '-05:30'")
        (is (= "2026-06-15 17:30:00+05:30" (second (read-all)))))
      (testing "RESET goes back to UTC, and nothing else moved throughout"
        (exec! c "RESET timezone")
        (is (= ["2026-06-15 12:00:00" "2026-06-15 12:00:00+00" "2026-06-15"]
               (read-all)))))
    (testing "INPUT: a literal with no offset is read in the session zone"
      (exec! c "SET timezone = 'Europe/Berlin'")
      (is (= [["2026-06-15 12:00:00+02" "2026-06-15 14:00:00+02"]]
             (rows c (str "SELECT '2026-06-15 12:00:00'::timestamptz,"
                          " '2026-06-15 12:00:00+00'::timestamptz")))
          "no offset means Berlin local; an explicit one is honoured, then rendered in Berlin")
      (testing "a plain timestamp literal is unaffected by the zone"
        (is (= [["2026-06-15 12:00:00"]]
               (rows c "SELECT '2026-06-15 12:00:00'::timestamp")))))))

(deftest the-session-zone-reaches-the-datetime-functions
  ;; #319 applied TimeZone to timestamptz input and output only, and
  ;; everything that reads a timestamptz's calendar ran in UTC -- which
  ;; nothing noticed while the zone was UTC. Every expectation is
  ;; PostgreSQL 17's under America/New_York.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tzf (tstz timestamptz, ts timestamp, d date)")
    (exec! c "INSERT INTO tzf VALUES ('2026-06-15 02:30+00', '2026-06-15 02:30', '2026-06-15')")
    (exec! c "SET timezone = 'America/New_York'")
    (let [one (fn [sql] (first (first (rows c sql))))]
      (testing "a timestamptz's fields are its wall clock in the zone"
        (is (= ["22" "14" "0" "-14400"]
               (first (rows c (str "SELECT extract(hour from tstz), extract(day from tstz),"
                                   " date_part('dow', tstz), extract(timezone from tstz)"
                                   " FROM tzf")))))
        (is (= "2" (one "SELECT extract(hour from ts) FROM tzf"))
            "a timestamp's are its own"))
      (testing "date_trunc truncates on that wall clock, or in the zone it is given"
        (is (= "2026-06-14 00:00:00-04" (one "SELECT date_trunc('day', tstz) FROM tzf")))
        (is (= "2026-06-01 00:00:00-04" (one "SELECT date_trunc('month', tstz) FROM tzf")))
        (is (= "2026-06-14 11:00:00-04"
               (one "SELECT date_trunc('day', tstz, 'Asia/Tokyo') FROM tzf"))))
      (testing "the casts between a point in time and a wall clock"
        (is (= ["2026-06-14" "22:30:00" "2026-06-14 22:30:00"]
               (first (rows c "SELECT tstz::date, tstz::time, tstz::timestamp FROM tzf"))))
        (is (= "2026-06-15 02:30:00-04" (one "SELECT ts::timestamptz FROM tzf")))
        (is (= "2026-06-15 00:00:00-04" (one "SELECT d::timestamptz FROM tzf")))
        (is (= "2026-06-14" (one "SELECT date(tstz) FROM tzf"))))
      (testing "an interval's days keep the wall clock across DST; its hours do not"
        (is (= "2026-03-08 12:00:00-04"
               (one "SELECT '2026-03-07 12:00-05'::timestamptz + interval '1 day'")))
        (is (= "2026-03-08 13:00:00-04"
               (one "SELECT '2026-03-07 12:00-05'::timestamptz + interval '24 hours'")))
        (is (= "2026-11-01 01:30:00-05"
               (one "SELECT '2026-10-31 01:30-04'::timestamptz + interval '1 day'"))
            "a repeated hour takes the later offset")
        (is (= "timestamp with time zone"
               (one "SELECT pg_typeof(tstz + interval '1 day') FROM tzf"))
            "and the result is typed; it was reported as bigint"))
      (testing "a timestamptz LITERAL is read too -- extract gave NULL in any zone"
        (is (= "22" (one "SELECT extract(hour from '2026-06-15 02:00+00'::timestamptz)")))
        (is (= "2026-06-14 00:00:00-04"
               (one "SELECT date_trunc('day', timestamptz '2026-06-15 02:00+00')"))))
      (testing "date + interval is a timestamp"
        (is (= "2026-01-02 00:00:00" (one "SELECT date '2026-01-01' + interval '1 day'")))))))

(deftest a-timestamptz-compares-with-a-wall-clock-in-the-session-zone
  ;; PostgreSQL casts the timestamp (or date) to timestamptz, placing it in
  ;; the session zone, before comparing. We compared its UTC reading, and a
  ;; timestamptz LITERAL -- folded to an OffsetDateTime -- compared with
  ;; nothing at all: `>` raised 42883 and `=` answered false. Every
  ;; expectation is PostgreSQL 17's under America/New_York.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tzc (tstz timestamptz, ts timestamp, d date)")
    (exec! c "INSERT INTO tzc VALUES ('2026-06-15 02:30+00', '2026-06-14 22:30', '2026-06-15')")
    (exec! c "SET timezone = 'America/New_York'")
    (let [one (fn [sql] (first (first (rows c sql))))]
      (is (= "t" (one "SELECT '2026-06-15 02:00+00'::timestamptz = '2026-06-14 22:00'::timestamp")))
      (is (= "t" (one "SELECT '2026-06-14 22:00'::timestamp = '2026-06-15 02:00+00'::timestamptz")))
      (is (= "t" (one "SELECT '2026-06-15 02:00+00'::timestamptz > '2026-06-14 21:00'::timestamp")))
      (is (= "t" (one "SELECT '2026-06-15 02:00+00'::timestamptz < date '2026-06-15'"))
          "the date is midnight in New York, four hours after 00:00 UTC")
      (is (= "1" (one "SELECT count(*) FROM tzc WHERE tstz = ts"))
          "two columns: not unified on the stored value")
      (is (= "1" (one "SELECT count(*) FROM tzc WHERE ts = tstz")))
      (is (= "1" (one "SELECT count(*) FROM tzc WHERE tstz < d")))
      (is (= "1" (one "SELECT count(*) FROM tzc WHERE tstz = '2026-06-14 22:30'::timestamp"))))))

(deftest postgres-date-trunc-slices
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 'UTC'")
    (exec! c "SET DateStyle TO ISO, MDY")
    (doseq [[source sql sqlstate text]
            [["date.sql:315" "SELECT DATE_TRUNC('MILLENNIUM', TIMESTAMP '1970-03-20 04:30:00.00000');" "00000" "1001-01-01 00:00:00"]
             ["date.sql:316" "SELECT DATE_TRUNC('MILLENNIUM', DATE '1970-03-20');" "00000" "1001-01-01 00:00:00+00"]
             ["date.sql:317" "SELECT DATE_TRUNC('CENTURY', TIMESTAMP '1970-03-20 04:30:00.00000');" "00000" "1901-01-01 00:00:00"]
             ["date.sql:318" "SELECT DATE_TRUNC('CENTURY', DATE '1970-03-20');" "00000" "1901-01-01 00:00:00+00"]
             ["date.sql:319" "SELECT DATE_TRUNC('CENTURY', DATE '2004-08-10');" "00000" "2001-01-01 00:00:00+00"]
             ["date.sql:320" "SELECT DATE_TRUNC('CENTURY', DATE '0002-02-04');" "00000" "0001-01-01 00:00:00+00"]
             ["date.sql:321" "SELECT DATE_TRUNC('CENTURY', DATE '0055-08-10 BC');" "00000" "0100-01-01 00:00:00+00 BC"]
             ["date.sql:322" "SELECT DATE_TRUNC('DECADE', DATE '1993-12-25');" "00000" "1990-01-01 00:00:00+00"]
             ["date.sql:323" "SELECT DATE_TRUNC('DECADE', DATE '0004-12-25');" "00000" "0001-01-01 00:00:00+00 BC"]
             ["date.sql:324" "SELECT DATE_TRUNC('DECADE', DATE '0002-12-31 BC');" "00000" "0011-01-01 00:00:00+00 BC"]
             ["timestamp.sql:177" "SELECT date_trunc( 'week', timestamp '2004-02-29 15:44:17.71393' ) AS week_trunc;" "00000" "2004-02-23 00:00:00"]
             ["timestamp.sql:178" "SELECT date_trunc( 'week', timestamp 'infinity' ) AS inf_trunc;" "00000" "infinity"]
             ["timestamp.sql:179" "SELECT date_trunc( 'timezone', timestamp '2004-02-29 15:44:17.71393' ) AS notsupp_trunc;" "0A000" nil]
             ["timestamp.sql:180" "SELECT date_trunc( 'timezone', timestamp 'infinity' ) AS notsupp_inf_trunc;" "0A000" nil]
             ["timestamp.sql:181" "SELECT date_trunc( 'ago', timestamp 'infinity' ) AS invalid_trunc;" "22023" nil]
             ["timestamptz.sql:219" "SELECT date_trunc( 'week', timestamp with time zone '2004-02-29 15:44:17.71393' ) AS week_trunc;" "00000" "2004-02-23 00:00:00+00"]
             ["timestamptz.sql:220" "SELECT date_trunc( 'week', timestamp with time zone 'infinity' ) AS inf_trunc;" "00000" "infinity"]
             ["timestamptz.sql:221" "SELECT date_trunc( 'timezone', timestamp with time zone '2004-02-29 15:44:17.71393' ) AS notsupp_trunc;" "0A000" nil]
             ["timestamptz.sql:222" "SELECT date_trunc( 'timezone', timestamp with time zone 'infinity' ) AS notsupp_inf_trunc;" "0A000" nil]
             ["timestamptz.sql:223" "SELECT date_trunc( 'ago', timestamp with time zone 'infinity' ) AS invalid_trunc;" "22023" nil]
             ["timestamptz.sql:225" "SELECT date_trunc('day', timestamp with time zone '2001-02-16 20:38:40+00', 'Australia/Sydney') as sydney_trunc;" "00000" "2001-02-16 13:00:00+00"]
             ["timestamptz.sql:226" "SELECT date_trunc('day', timestamp with time zone '2001-02-16 20:38:40+00', 'GMT') as gmt_trunc;" "00000" "2001-02-16 00:00:00+00"]
             ["timestamptz.sql:227" "SELECT date_trunc('day', timestamp with time zone '2001-02-16 20:38:40+00', 'VET') as vet_trunc;" "00000" "2001-02-16 04:00:00+00"]
             ["timestamptz.sql:228" "SELECT date_trunc('timezone', timestamp with time zone 'infinity', 'GMT') AS notsupp_zone_trunc;" "0A000" nil]
             ["timestamptz.sql:229" "SELECT date_trunc( 'week', timestamp with time zone 'infinity', 'GMT') AS inf_zone_trunc;" "00000" "infinity"]
             ["timestamptz.sql:230" "SELECT date_trunc('ago', timestamp with time zone 'infinity', 'GMT') AS invalid_zone_trunc;" "22023" nil]]]
      (if (= sqlstate "00000")
        (is (= [[text]] (rows c sql)) source)
        (is (= sqlstate (state c sql)) source)))))

(deftest date-trunc-types-and-precision-match-postgres
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 'UTC'")
    (is (= [["2020-03-15 12:34:56.123" "timestamp without time zone"
             "2020-03-15 12:34:56.123456+00" "timestamp with time zone"]]
           (rows c (str "SELECT date_trunc('milliseconds', timestamp '2020-03-15 12:34:56.123456'), "
                        "pg_typeof(date_trunc('milliseconds', timestamp '2020-03-15 12:34:56.123456')), "
                        "date_trunc('microseconds', timestamptz '2020-03-15 12:34:56.123456+00'), "
                        "pg_typeof(date_trunc('microseconds', timestamptz '2020-03-15 12:34:56.123456+00'))"))))
    (is (= [["2020-11-01 05:00:00+00" "2020-11-01 06:00:00+00" "2020-10-03 15:00:00+00"]]
           (rows c (str "SELECT date_trunc('hour', timestamptz '2020-11-01 01:30:20-04', 'America/New_York'), "
                        "date_trunc('hour', timestamptz '2020-11-01 01:30:20-05', 'America/New_York'), "
                        "date_trunc('hour', timestamptz '2020-10-04 02:50:00+11', 'Australia/Lord_Howe')"))))
    (is (= [["12:34:00" "interval" "24:00:00" "infinity" nil]]
           (rows c (str "SELECT date_trunc('minute', time '12:34:56'), "
                        "pg_typeof(date_trunc('minute', time '12:34:56')), "
                        "date_trunc('minute', time '24:00:00'), date_trunc('day', interval 'infinity'), "
                        "date_trunc('day', now(), NULL)"))))
    (doseq [[expected sql] [["42725" "SELECT date_trunc('day', '2020-03-15 12:34:56')"]
                            ["42883" "SELECT date_trunc('day', '2020-03-15 12:34:56'::text)"]
                            ["42883" "SELECT date_trunc('day', 1584275696)"]
                            ["42883" "SELECT date_trunc('day', interval '2 days', 'UTC')"]
                            ["42883" "SELECT date_trunc('minute', timetz '12:34:56+02')"]
                            ["22023" "SELECT date_trunc('day', timestamptz 'infinity', 'Frogs/Unknown')"]
                            ["22023" "SELECT date_trunc('day', timestamptz '2020-03-15 12:34', ' UTC ')"]
                            ["22007" "SELECT date_trunc(NULL, 'invalid', 'UTC')"]
                            ["22007" "SELECT date_trunc('day', 'invalid', NULL)"]
                            ["0A000" "SELECT date_trunc('week', interval 'infinity')"]]]
      (is (= expected (state c sql)) sql))
    (with-open [stmt (.prepareStatement c "SELECT pg_catalog.date_trunc(?, ?, ?)")]
      (let [metadata (.getParameterMetaData stmt)]
        (is (= ["text" "timestamptz" "text"]
               (mapv #(.getParameterTypeName metadata %) [1 2 3])))))
    (exec! c "CREATE TABLE trunc_params (ts timestamp, tz timestamptz)")
    (exec! c "INSERT INTO trunc_params VALUES ('2020-03-15 02:01', '2020-03-15 02:01+00'), (NULL, NULL)")
    (with-open [stmt (.prepareStatement c
                                        (str "SELECT date_trunc(?, ts, ?), date_trunc(?, tz) "
                                             "FROM trunc_params ORDER BY ts NULLS FIRST"))]
      (.setPrepareThreshold ^org.postgresql.PGStatement stmt 1)
      (.setString stmt 1 "day")
      (.setString stmt 2 "Asia/Kolkata")
      (.setString stmt 3 "day")
      (is (= "timestamptz" (.getColumnTypeName (.getMetaData stmt) 1)))
      (with-open [result (.executeQuery stmt)]
        (is (.next result))
        (is (nil? (.getString result 1)))
        (is (.next result))
        (is (= "2020-03-14 18:30:00+00" (.getString result 1)))
        (is (= "2020-03-15 00:00:00+00" (.getString result 2))))
      (exec! c "SET TIME ZONE 'Asia/Tokyo'")
      (.setNull stmt 2 java.sql.Types/VARCHAR)
      (with-open [result (.executeQuery stmt)]
        (is (.next result))
        (is (nil? (.getString result 1)))
        (is (.next result))
        (is (nil? (.getString result 1)))
        (is (= "2020-03-15 00:00:00+09" (.getString result 2)))))))

(deftest date-trunc-prepared-input-uses-the-right-session-zone
  (with-open [c (jdbc)]
    (exec! c "SET TIME ZONE 'UTC'")
    (exec! c "PREPARE trunc_literal AS SELECT date_trunc('hour', '2020-03-15 02:01', 'UTC')")
    (exec! c "PREPARE trunc_bound AS SELECT date_trunc($1, $2, $3)")
    (exec! c "SET DateStyle TO ISO, MDY")
    (exec! c "PREPARE trunc_style AS SELECT date_trunc('day', '03/04/2020', 'UTC')")
    (exec! c "SET TIME ZONE 'Asia/Tokyo'")
    (exec! c "SET DateStyle TO ISO, DMY")
    (is (= [["2020-03-15 11:00:00+09"]] (rows c "EXECUTE trunc_literal")))
    (is (= [["2020-03-15 02:00:00+09"]] (rows c "EXECUTE trunc_bound('hour', '2020-03-15 02:01', 'UTC')")))
    (exec! c "CREATE TABLE trunc_catalog_change (id int)")
    (is (= [["2020-03-15 11:00:00+09"]] (rows c "EXECUTE trunc_literal")))
    (is (= [["2020-03-04 09:00:00+09"]] (rows c "EXECUTE trunc_style")))))
