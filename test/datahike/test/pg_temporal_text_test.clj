(ns datahike.test.pg-temporal-text-test
  "Rendering a temporal value to TEXT.

   The wire renderer knew PostgreSQL's temporal text formats, but it kept
   its own private copy of them -- so every route to text that was not
   the wire (`::text`, `CAST(… AS varchar)`, `||`, `concat()`) fell
   through to Clojure's `str` and emitted java.util.Date.toString:

     SELECT ts::text  ->  Wed Jan 01 02:00:00 PST 2020
                          (want 2020-01-01 10:00:00)

   That is not merely misformatted. It carries the JVM's default time
   zone and locale, so the same query answered differently on different
   machines -- which is why these tests assert exact strings.

   Telling a `date` from a `timestamp` is the whole difficulty: Datahike
   has only :db.type/instant, so both columns arrive as java.util.Date at
   UTC and the value itself says nothing. The declared type has to come
   from the translator.

   Expectations here are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn text-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"txt" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each text-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/txt?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ev (id int, d date, ts timestamp)")
  (exec! c "INSERT INTO ev VALUES (1,'2020-01-01','2020-01-01 10:00')"))

(deftest cast-to-text-uses-the-declared-column-type
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "2020-01-01 10:00:00" (one c "SELECT ts::text FROM ev")))
    (is (= "2020-01-01" (one c "SELECT d::text FROM ev"))
        "a date drops the time part -- and both columns are a java.util.Date
         at this point, so only the declared type can say so")
    (testing "CAST(… AS varchar) is the same conversion"
      (is (= "2020-01-01 10:00:00" (one c "SELECT CAST(ts AS varchar) FROM ev")))
      (is (= "2020-01-01" (one c "SELECT CAST(d AS varchar) FROM ev"))))))

(deftest cast-to-text-of-a-cast-literal
  (with-open [c (jdbc)]
    (is (= "2020-01-01" (one c "SELECT '2020-01-01'::date::text"))
        "a ::date cast yields a LocalDate, which needs no type hint")
    (is (= "2020-01-01 10:00:00" (one c "SELECT '2020-01-01 10:00'::timestamp::text")))))

(deftest concatenation-renders-temporals-the-postgres-way
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "2020-01-01x" (one c "SELECT concat(d, 'x') FROM ev")))
    (is (= "v2020-01-01" (one c "SELECT 'v' || d FROM ev")))
    (is (= "2020-01-01v" (one c "SELECT d || 'v' FROM ev"))
        "either side of || may be the temporal one")
    (is (= "2020-01-01 10:00:00x" (one c "SELECT concat(ts, 'x') FROM ev")))
    (is (= "1-2020-01-01" (one c "SELECT concat(id, '-', d) FROM ev"))
        "concat takes more than two arguments, each with its own type")))

(deftest text-rendering-composes-with-other-string-functions
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "10" (one c "SELECT length(d::text) FROM ev"))
        "the length of 2020-01-01, not of a Date.toString")
    (is (= "2020-01-01z" (one c "SELECT d::text || 'z' FROM ev")))
    (is (= "2020-01-01A" (one c "SELECT upper(concat(d,'a')) FROM ev")))))

(deftest concat-operator-is-strict-and-concat-function-is-not
  (with-open [c (jdbc)]
    (seed! c)
    (testing "|| yields NULL if either operand is NULL"
      (is (nil? (one c "SELECT 'a' || NULL")))
      (is (nil? (one c "SELECT NULL || 'a'")))
      (is (nil? (one c "SELECT 'a' || NULL || 'b'")))
      (is (nil? (one c "SELECT d || NULL FROM ev"))))
    (testing "concat() ignores its NULL arguments -- this is the whole
              reason PostgreSQL ships both"
      (is (= "ab" (one c "SELECT concat('a', NULL, 'b')")))
      (is (= "a" (one c "SELECT concat('a', NULL)"))))
    (testing "the other || overloads stay strict too"
      (is (nil? (one c "SELECT '{\"a\":1}'::jsonb || NULL"))))))

(deftest wire-rendering-of-a-bare-column-is-unchanged
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "2020-01-01 10:00:00" (one c "SELECT ts FROM ev")))
    (is (= "2020-01-01" (one c "SELECT d FROM ev")))))

(deftest datestyle-chooses-the-output-format
  ;; `DateStyle` is a session setting, and it was accepted and then
  ;; ignored: every date and timestamp rendered ISO whatever the session
  ;; asked for. PostgreSQL's own regression suite runs under
  ;; `Postgres, MDY` -- pg_regress passes it in the STARTUP packet, never
  ;; as a SET -- so every date in that whole suite was in the wrong
  ;; style, and the files that are mostly dates diverged on nearly every
  ;; line.
  ;;
  ;; The setting is a display style plus a field order, and the halves
  ;; move independently: `SET datestyle = 'DMY'` changes only the order.
  ;; Expectations are a PostgreSQL 17 oracle's.
  (with-open [c (jdbc)]
    (testing "ISO is the default"
      (is (= "2000-04-01" (one c "SELECT '2000-04-01'::date")))
      (is (= "2016-09-01 12:00:00" (one c "SELECT '2016-09-01 12:00:00'::timestamp")))
      (is (= "ISO, MDY" (one c "SHOW DateStyle"))))
    (testing "Postgres style, and the field order it reads"
      (exec! c "SET DateStyle = 'Postgres, MDY'")
      (is (= "04-01-2000" (one c "SELECT '2000-04-01'::date")))
      (is (= "Thu Sep 01 12:00:00 2016" (one c "SELECT '2016-09-01 12:00:00'::timestamp"))
          "the Postgres style names the weekday and the month")
      (is (= "Postgres, MDY" (one c "SHOW DateStyle")))
      (exec! c "SET DateStyle = 'Postgres, DMY'")
      (is (= "01-04-2000" (one c "SELECT '2000-04-01'::date"))))
    (testing "SQL and German"
      (exec! c "SET DateStyle = 'SQL, MDY'")
      (is (= "04/01/2000" (one c "SELECT '2000-04-01'::date")))
      (exec! c "SET DateStyle = 'German'")
      (is (= "01.04.2000" (one c "SELECT '2000-04-01'::date"))))
    (testing "the halves move independently"
      (exec! c "SET DateStyle = 'ISO, MDY'")
      (exec! c "SET datestyle = 'DMY'")
      (is (= "ISO, DMY" (one c "SHOW DateStyle")) "only the order changed"))
    (testing "RESET returns to the default"
      (exec! c "RESET DateStyle")
      (is (= "ISO, MDY" (one c "SHOW DateStyle")))
      (is (= "2000-04-01" (one c "SELECT '2000-04-01'::date"))))
    (testing "a column renders in the session's style too, not only a literal"
      (seed! c)
      (exec! c "SET DateStyle = 'Postgres, MDY'")
      (is (= "01-01-2020" (one c "SELECT d FROM ev WHERE id = 1")))
      (is (= "Wed Jan 01 10:00:00 2020" (one c "SELECT ts FROM ev WHERE id = 1")))
      (exec! c "RESET DateStyle"))))

(defn- err-of [^Connection c sql]
  (try (one c sql) nil
       (catch java.sql.SQLException e [(.getSQLState e) (.getMessage e)])))

(deftest an-impossible-date-is-rejected-not-rolled
  ;; `DateTimeFormatter` resolves SMART by default, which quietly moves
  ;; 1997-04-31 to the 30th and 1997-02-29 to the 28th. PostgreSQL
  ;; rejects both. Worse, a date whose fields could not be resolved at
  ;; all fell through the cast's passthrough and came back as its own
  ;; text -- `'1997-13-01'::date` answered the string `1997-13-01`, a
  ;; date with a thirteenth month.
  (with-open [c (jdbc)]
    (doseq [s ["1997-02-29" "1997-04-31" "1997-13-01" "1997-00-01" "1997-01-32"]]
      (let [[state msg] (err-of c (str "SELECT '" s "'::date"))]
        (is (= "22008" state) (str s " => " msg))
        (is (re-find (re-pattern (str "date/time field value out of range: \"" s "\"")) (str msg)))))
    (testing "a real date, and a real leap day, still parse"
      (is (= "1997-02-28" (one c "SELECT '1997-02-28'::date")))
      (is (= "2000-02-29" (one c "SELECT '2000-02-29'::date")))
      (is (= "2024-02-29" (one c "SELECT '2024-02-29'::date"))))
    (testing "text that is not a date at all is 22007, not 22008"
      (is (= "22007" (first (err-of c "SELECT 'garbage'::date")))))))

(deftest a-bc-date-keeps-its-era
  ;; LocalDate counts proleptically -- 1 BC is year 0, 2 BC is -1 --
  ;; while PostgreSQL writes the year of the era and a BC suffix. The
  ;; era was being dropped on the way in and on the way out.
  (with-open [c (jdbc)]
    (is (= "2040-04-10 BC" (one c "SELECT '2040-04-10 BC'::date")))
    (is (= "0001-01-01 BC" (one c "SELECT '0001-01-01 BC'::date")))
    (testing "and AD is the default, written without a suffix"
      (is (= "2040-04-10" (one c "SELECT '2040-04-10 AD'::date")))
      (is (= "2040-04-10" (one c "SELECT '2040-04-10'::date"))))))

(deftest a-date-column-writes-through-the-date-input-function
  ;; The cast rejected `'1997-02-29'` while the WRITE stored the 28th:
  ;; `INSERT` fell through to the lenient timestamp parser instead of
  ;; running PostgreSQL's date input function, so the two spellings of
  ;; one value disagreed. `time` and `timetz` columns already routed
  ;; their writes through the cast; a date did not.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE dt (f1 date)")
    (testing "an impossible date is refused, not rolled into the next month"
      (doseq [s ["1997-02-29" "1997-04-31" "1997-13-01"]]
        (let [[state msg] (err-of c (str "INSERT INTO dt VALUES ('" s "')"))]
          (is (= "22008" state) (str s " => " msg)))))
    (testing "the write and the cast now give the SAME answer, which is the point"
      (is (= (second (err-of c "INSERT INTO dt VALUES ('1997-02-29')"))
             (second (err-of c "SELECT '1997-02-29'::date")))))
    (testing "a BC date, which the cast accepted and the write refused"
      (exec! c "INSERT INTO dt VALUES ('2040-04-10 BC')")
      (is (= "2040-04-10 BC" (one c "SELECT f1 FROM dt"))))
    (testing "and an ordinary date still goes in"
      (exec! c "INSERT INTO dt VALUES ('1997-02-28')")
      (is (= "1997-02-28" (one c "SELECT f1 FROM dt WHERE f1 = '1997-02-28'"))))))

(deftest iso-basic-timestamps-and-at-time-zone-failure
  ;; PostgreSQL reads ISO 8601's BASIC spelling -- no separators --
  ;; and nothing here did. `'19970210 173201'::timestamp` answered with
  ;; its own text: a value that is not a timestamp, in a timestamp
  ;; column, because the parser returns its input when nothing matches.
  ;;
  ;; `AT TIME ZONE` then dereferenced that failure. A string operand
  ;; produced nil from the wall-clock conversion and the client got
  ;; `Cannot invoke "java.time.LocalDateTime.atZone(…)" because the
  ;; return value of "clojure.lang.IFn.invoke(Object)" is null` --
  ;; fifteen such lines in `timestamptz` alone, all from this one gap.
  ;;
  ;; Expectations are a PostgreSQL 17 oracle's.
  (with-open [c (jdbc)]
    (testing "the basic spelling parses, in a timestamp and in a date"
      (is (= "1997-02-10 17:32:01" (one c "SELECT '19970210 173201'::timestamp")))
      (is (= "1997-02-10" (one c "SELECT '19970210'::date")))
      (is (= "1997-02-10 17:32:01" (one c "SELECT '19970210T173201'::timestamp"))))
    (testing "AT TIME ZONE over one of them shifts the wall clock"
      (is (= "1997-02-10 12:32:01"
             (one c "SELECT '19970210 173201' AT TIME ZONE 'America/New_York'"))))
    (testing "and a value that is not a timestamp is 22007, not an internal error"
      (let [[state msg] (err-of c "SELECT 'garbage' AT TIME ZONE 'America/New_York'")]
        (is (= "22007" state) msg)
        (is (re-find #"invalid input syntax for type timestamp with time zone"
                     (str msg)))
        (is (not (re-find #"(?i)cannot invoke|java\.lang|clojure\." (str msg)))
            "an internal failure must never reach the client")))))
