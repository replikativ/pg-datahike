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

(deftest a-named-zone-inside-a-timestamptz-literal
  ;; `'1997-02-10 17:32:01 America/New_York'::timestamptz` is 22:32:01
  ;; UTC. The zone was parsed away and the wall clock kept, so it read
  ;; back as 17:32:01+00 -- five hours out, reported as success -- and
  ;; the extended spelling did not parse at all and passed its own text
  ;; through as a timestamptz value.
  ;;
  ;; A plain `timestamp` keeps the fields and DROPS the zone, which is
  ;; the opposite behaviour from the same text, and is PostgreSQL's.
  ;; Expectations are a PostgreSQL 17 oracle's.
  (with-open [c (jdbc)]
    (testing "the zone is applied, and daylight saving with it"
      (is (= "1997-02-10 22:32:01+00"
             (one c "SELECT '1997-02-10 17:32:01 America/New_York'::timestamptz")))
      (is (= "1997-07-10 21:32:01+00"
             (one c "SELECT '1997-07-10 17:32:01 America/New_York'::timestamptz"))
          "July is daylight time, so the same wall clock is an hour earlier in UTC")
      (is (= "1997-02-11 01:32:01+00"
             (one c "SELECT '1997-02-10 17:32:01 PST'::timestamptz"))
          "an abbreviation resolves too")
      (is (= "1997-02-10 22:32:01+00"
             (one c "SELECT '19970210 173201 America/New_York'::timestamptz"))
          "including on the ISO basic spelling"))
    (testing "a numeric offset still works, and is POSIX-signed"
      (is (= "1997-02-10 12:32:01+00"
             (one c "SELECT '1997-02-10 17:32:01+05'::timestamptz"))))
    (testing "a plain timestamp keeps the fields and drops the zone"
      (is (= "1997-02-10 17:32:01"
             (one c "SELECT '1997-02-10 17:32:01 America/New_York'::timestamp")))
      (is (= "1997-02-10 17:32:01"
             (one c "SELECT '1997-02-10 17:32:01 PST'::timestamp"))))
    (testing "an unrecognised zone is an error, lowercased as the cast reports it"
      (let [[state msg] (err-of c "SELECT '1997-02-10 17:32:01 Nonsense/Zone'::timestamptz")]
        (is (= "22023" state) msg)
        (is (re-find #"time zone \"nonsense/zone\" not recognized" (str msg))))
      (testing "while AT TIME ZONE keeps what was written -- PostgreSQL differs"
        (is (re-find #"time zone \"Nonsense/Zone\" not recognized"
                     (str (second (err-of c "SELECT now() AT TIME ZONE 'Nonsense/Zone'")))))))))

(deftest a-timestamp-write-and-cast-give-the-same-verdict
  ;; The `date` branch of the INSERT coercion was routed through the
  ;; cast and its `timestamp`/`timestamptz` SIBLING, directly below it,
  ;; was left on the lenient parser. So the two spellings still
  ;; disagreed for every other temporal type.
  ;;
  ;; Underneath that, `parse-timestamp-string` built its formatters
  ;; with `ofPattern`, whose default ResolverStyle/SMART ROLLS an
  ;; impossible field instead of refusing it -- the same defect
  ;; `parse-date-strict` exists to fix, never applied here. And when
  ;; nothing parsed at all, the cast returned its own input: a value
  ;; that is not a timestamp, indistinguishable from one that is.
  ;;
  ;; Expectations, including which SQLSTATE, are a PostgreSQL 17
  ;; oracle's: impossible FIELDS are 22008, text that is not a
  ;; timestamp at all is 22007.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tw (ts timestamp, tz timestamptz)")
    (testing "an impossible date is refused on WRITE, not rolled"
      (doseq [s ["2024-02-30" "2023-02-29" "2024-04-31"]]
        (let [[state msg] (err-of c (str "INSERT INTO tw(ts) VALUES ('" s "')"))]
          (is (= "22008" state) (str s " => " msg)))))
    (testing "and on CAST, with the same message"
      (doseq [s ["2024-02-30" "2023-02-29" "2024-04-31"]]
        (is (= (second (err-of c (str "INSERT INTO tw(ts) VALUES ('" s "')")))
               (second (err-of c (str "SELECT '" s "'::timestamp"))))
            s)))
    (testing "text that is not a timestamp is 22007, and is never stored as itself"
      (let [[state msg] (err-of c "SELECT 'nonsense'::timestamp")]
        (is (= "22007" state) msg)
        (is (re-find #"invalid input syntax for type timestamp" (str msg)))))
    (testing "a named zone is applied on write, as it is on cast"
      (exec! c "INSERT INTO tw(tz) VALUES ('1997-02-10 17:32:01 America/New_York')")
      (is (= "1997-02-10 22:32:01+00"
             (one c "SELECT tz FROM tw WHERE tz IS NOT NULL"))))
    (testing "and an ordinary timestamp still goes in"
      (exec! c "INSERT INTO tw(ts) VALUES ('2024-02-29 10:00:00')")
      (is (= "2024-02-29 10:00:00"
             (one c "SELECT ts FROM tw WHERE ts IS NOT NULL"))))))

;; ============================================================================
;; Reading a temporal value FROM text
;; ============================================================================

(deftest a-month-name-names-a-month-wherever-it-falls
  ;; PostgreSQL's datetime input tokenises before it interprets
  ;; (`datetktbl` in datetime.c), so the month name is recognised in
  ;; any position and the separator does not matter. Matching that
  ;; with a list of patterns means missing one of the spellings, and
  ;; every one of these was `invalid input syntax` here.
  (with-open [c (jdbc)]
    (doseq [s ["Jan 15, 2024" "January 15, 2024" "Jan 15 2024" "15-JAN-2024"
               "15 Jan 2024" "2024-Jan-15" "Jan-15-2024"]]
      (is (= "2024-01-15 00:00:00" (one c (str "SELECT '" s "'::timestamp::text")))
          s))
    (is (= "2024-01-15 10:20:30"
           (one c "SELECT 'Jan 15, 2024 10:20:30'::timestamp::text")))
    (is (= "2024-01-15" (one c "SELECT 'Jan 15, 2024'::date::text")))))

(deftest an-impossible-day-spelled-with-a-month-name-is-out-of-range
  ;; The 22008/22007 split is about the SHAPE of the input, and the
  ;; shape test only knew digits-and-separators: `Feb 30, 2024` names a
  ;; real month, so it is a field out of RANGE, exactly as `2024-02-30`
  ;; is -- while `Foo 30, 2024` is not a date at all.
  (with-open [c (jdbc)]
    (is (thrown-with-msg?
         Exception #"date/time field value out of range: \"Feb 30, 2024\""
         (one c "SELECT 'Feb 30, 2024'::timestamp")))
    (is (thrown-with-msg?
         Exception #"date/time field value out of range: \"Feb 30, 2024\""
         (one c "SELECT 'Feb 30, 2024'::date")))
    (is (thrown-with-msg?
         Exception #"invalid input syntax for type timestamp: \"Foo 30, 2024\""
         (one c "SELECT 'Foo 30, 2024'::timestamp")))))

(deftest copy-reads-a-timestamp-through-the-same-input-function-as-a-cast
  ;; COPY kept a THIRD temporal parser, beside cast.clj's and
  ;; expr.clj's, and it knew fewer spellings than either: `COPY`
  ;; rejected `20240115` while `'20240115'::timestamp` took it.
  ;; PostgreSQL has no such seam -- both call `timestamp_in` -- so
  ;; these assert the two paths agree rather than asserting a format
  ;; list twice.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE cm (id int, t timestamp)")
    (let [cm (org.postgresql.copy.CopyManager. (.unwrap c org.postgresql.core.BaseConnection))]
      (.copyIn cm "COPY cm FROM STDIN"
               (java.io.ByteArrayInputStream.
                (.getBytes (str "1\t20240115\n"
                                "2\tJan 15, 2024 10:20:30\n"
                                "3\t15-JAN-2024\n")
                           "UTF-8"))))
    (is (= "2024-01-15 00:00:00" (one c "SELECT t::text FROM cm WHERE id = 1")))
    (is (= "2024-01-15 10:20:30" (one c "SELECT t::text FROM cm WHERE id = 2")))
    (is (= "2024-01-15 00:00:00" (one c "SELECT t::text FROM cm WHERE id = 3")))
    (testing "and a value neither path accepts raises the type's own error,
              where COPY used to report a flat 22P02"
      (is (thrown-with-msg?
           Exception #"date/time field value out of range"
           (let [cm (org.postgresql.copy.CopyManager.
                     (.unwrap c org.postgresql.core.BaseConnection))]
             (.copyIn cm "COPY cm FROM STDIN"
                      (java.io.ByteArrayInputStream.
                       (.getBytes "9\t2024-02-30\n" "UTF-8")))))))))

(deftest the-three-spellings-of-an-offset-are-one-value
  ;; `pg_dump`'s text format writes a timestamptz with an hour-only
  ;; offset, and other sources write the four-digit one. PostgreSQL
  ;; reads `-08`, `-0800` and `-08:00` as the same instant. COPY knew
  ;; all three only because it carried a private normaliser; folding
  ;; that into the shared parser is what lets the private copy go, and
  ;; `-0800` reached the shared parser as an out-of-range date until
  ;; it did.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tz (id int, t timestamptz)")
    (let [cm (org.postgresql.copy.CopyManager. (.unwrap c org.postgresql.core.BaseConnection))]
      (.copyIn cm "COPY tz FROM STDIN"
               (java.io.ByteArrayInputStream.
                (.getBytes (str "1\t2022-01-28 17:58:52-08\n"
                                "2\t2022-01-28 17:58:52-0800\n"
                                "3\t2022-01-28 17:58:52-08:00\n")
                           "UTF-8"))))
    (is (= 1 (count (set (map #(one c (str "SELECT t::text FROM tz WHERE id = " %))
                              [1 2 3]))))
        "one instant, however the offset is spelled")))

(deftest a-nested-cast-refuses-what-the-bare-cast-refuses
  ;; The constant fold for a cast kept its OWN temporal conversion
  ;; beside `cast-scalar`'s, and so kept its own idea of failure: the
  ;; timestamp branch called the bare parser, which returns its INPUT
  ;; when nothing parses, and the date branch answered nil. A cast
  ;; reaches that fold only when it is NESTED -- a bare
  ;; `'nonsense'::timestamp` goes down another path and always raised
  ;; -- so the two disagreed:
  ;;
  ;;   'nonsense'::timestamp         ERROR            (right)
  ;;   'nonsense'::timestamp::text   'nonsense'       (a value that is
  ;;                                                   not a timestamp,
  ;;                                                   rendered as one)
  ;;   'nonsense'::date::text        NULL
  (with-open [c (jdbc)]
    (doseq [sql ["SELECT 'nonsense'::timestamp::text"
                 "SELECT 'nonsense'::date::text"
                 "SELECT extract(year from 'nonsense'::timestamp)"]]
      (is (thrown-with-msg? Exception #"invalid input syntax for type"
                            (one c sql))
          sql))
    (testing "an impossible field is still told from bad syntax"
      (is (thrown-with-msg?
           Exception #"date/time field value out of range"
           (one c "SELECT '2024-02-30'::date::text"))))
    (testing "and everything that did parse still does"
      (is (= "2024-01-15 00:00:00" (one c "SELECT '2024-01-15'::timestamp::text")))
      (is (= "2024-01-15" (one c "SELECT '2024-01-15'::date::text")))
      (is (= "2024-01-15 10:00:00" (one c "SELECT '2024-01-15 10:00'::timestamp::text")))
      (is (= "2024-01-15 00:00:00" (one c "SELECT 'Jan 15, 2024'::timestamp::text"))))))

(deftest a-column-cast-refuses-what-a-literal-cast-refuses
  ;; The RUNTIME cast -- a column, not a literal -- kept its own
  ;; temporal conversion too, beside the `is-time?` branch next to it
  ;; that already delegated. Same two failure modes, now per row:
  ;;
  ;;   s::timestamp  answered the text `nonsense` for the row that did
  ;;                 not parse, presenting a non-timestamp as one;
  ;;   s::date       answered nil, and nil FILTERS THE ROW in a datalog
  ;;                 function binding -- the row silently vanished from
  ;;                 the result rather than raising.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE tt (id int, s text)")
    (exec! c "INSERT INTO tt VALUES (1,'2024-01-15'),(2,'nonsense')")
    (is (thrown-with-msg?
         Exception #"invalid input syntax for type timestamp"
         (one c "SELECT id, s::timestamp FROM tt ORDER BY id")))
    (is (thrown-with-msg?
         Exception #"invalid input syntax for type date"
         (one c "SELECT id, s::date FROM tt ORDER BY id")))
    (testing "the rows that do parse are unaffected"
      (is (= "2024-01-15" (one c "SELECT s::date::text FROM tt WHERE id = 1")))
      (is (= "2024-01-15 00:00:00"
             (one c "SELECT s::timestamp::text FROM tt WHERE id = 1"))))))

(deftest the-mdy-slash-spelling-survives-a-strict-resolver
  ;; `8/10/7777` is PostgreSQL's MDY DateStyle. The pattern for it was
  ;; the one `ofPattern` site left on year-of-era `y` when the others
  ;; were moved to proleptic `uuuu`, and STRICT resolves `y` only with
  ;; an era field -- so it matched nothing and the cast became an
  ;; error.
  ;;
  ;; pgjdbc's own ResultSetTest.testTimestamp is what caught it, three
  ;; CI runs after the fact, and only in binary mode: the failed cast
  ;; left TEXT where a timestamp OID was advertised, and the driver
  ;; answered `Unsupported binary encoding of timestamp`. Asserting it
  ;; here means the next one fails in seconds rather than in CI.
  (with-open [c (jdbc)]
    (is (= "7777-08-10 00:00:00" (one c "SELECT '8/10/7777'::timestamp::text")))
    (is (= "2017-08-10 00:00:00" (one c "SELECT '8/10/2017'::timestamp::text")))
    (is (= "2017-08-10" (one c "SELECT '8/10/2017'::date::text")))
    (testing "through array_fill/unnest, the shape pgjdbc uses"
      ;; No ::text around it: `unnest(timestamp[])` is not in the
      ;; signature registry yet, which is a separate gap. Reading the
      ;; value is what this test is about.
      (is (some? (one c (str "SELECT unnest(array_fill('8/10/7777'::timestamp, "
                             "ARRAY[3]))")))))
    (testing "and an impossible field in THAT spelling is out of range,
              not bad syntax -- the shape test knew only dashes"
      (is (thrown-with-msg?
           Exception #"date/time field value out of range: \"13/10/2017\""
           (one c "SELECT '13/10/2017'::timestamp")))
      (is (thrown-with-msg?
           Exception #"date/time field value out of range: \"2/30/2017\""
           (one c "SELECT '2/30/2017'::timestamp"))))))

(deftest datestyle-decides-what-a-numeric-date-means
  ;; DateStyle has two halves and only the OUTPUT half was honoured, so
  ;; `SET datestyle TO dmy` changed how a date printed and not how one
  ;; was read. `8/10/2017` was always August 10th.
  ;;
  ;; PostgreSQL's rules (DecodeDateTime): a leading field of FOUR or
  ;; more digits is the year whatever the order, so `2017-08-10` reads
  ;; the same everywhere; otherwise the order assigns the fields, a
  ;; two-digit year gets a century, and the fields are CHECKED rather
  ;; than rolled. Separators carry no meaning -- `8/10/2017` and
  ;; `10-08-2017` differ only in punctuation.
  ;;
  ;; Every expectation here is a PostgreSQL 17 oracle's, taken as a
  ;; matrix over the three orders.
  (with-open [c (jdbc)]
    (let [d (fn [style s] (exec! c (str "SET datestyle TO '" style "'"))
              (one c (str "SELECT '" s "'::date::text")))
          ts (fn [style s] (exec! c (str "SET datestyle TO '" style "'"))
               (one c (str "SELECT '" s "'::timestamp::text")))]
      (testing "MDY, the default"
        (is (= "2017-08-10" (d "ISO, MDY" "8/10/2017")))
        (is (= "1999-01-08" (d "ISO, MDY" "1/8/1999")))
        (is (= "2017-10-08" (d "ISO, MDY" "10-08-2017")))
        (is (= "2017-08-10" (d "ISO, MDY" "2017-08-10"))))
      (testing "DMY swaps the first two fields"
        (is (= "2017-10-08" (d "ISO, DMY" "8/10/2017")))
        (is (= "1999-08-01" (d "ISO, DMY" "1/8/1999")))
        (is (= "2017-08-10" (d "ISO, DMY" "10-08-2017")))
        (is (= "2017-10-13" (d "ISO, DMY" "13/10/2017"))
            "a 13 that cannot be a month is the day here"))
      (testing "a four-digit leading field wins over the order"
        (doseq [style ["ISO, MDY" "ISO, DMY" "ISO, YMD"]]
          (is (= "2017-08-10" (d style "2017-08-10")) style)))
      (testing "YMD reads the year first, and gives a two-digit one a century"
        (is (= "1999-01-02" (d "ISO, YMD" "99-01-02"))))
      (testing "the timestamp parser follows the same order"
        (is (= "2017-10-08 00:00:00" (ts "ISO, DMY" "8/10/2017")))
        (is (= "2017-08-10 00:00:00" (ts "ISO, DMY" "10-08-2017")))
        (is (= "2017-08-10 00:00:00" (ts "ISO, MDY" "8/10/2017")))
        (is (= "2017-08-10 10:20:30" (ts "ISO, MDY" "2017-08-10 10:20:30"))))
      (testing "and impossible fields are out of range, not bad syntax"
        (exec! c "SET datestyle TO 'ISO, MDY'")
        (doseq [s ["13/10/2017" "2/30/2017" "2024-02-30"]]
          (is (thrown-with-msg?
               Exception #"date/time field value out of range"
               (one c (str "SELECT '" s "'::date"))) s))
        (exec! c "SET datestyle TO 'ISO, YMD'")
        (is (thrown-with-msg?
             Exception #"date/time field value out of range"
             (one c "SELECT '8/10/2017'::date"))
            "day 2017 under YMD")))))
(deftest the-float-sum-guc-is-a-session-setting
  ;; Compensated summation is opt-in, and the GUC has to behave like one:
  ;; SHOW reports it, SET changes it, RESET clears it, and a value that
  ;; is neither is refused rather than silently ignored.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE fs (v float8)")
    (exec! c "INSERT INTO fs VALUES (1e16),(1.0),(1.0),(1.0),(1.0),(-1e16)")
    (is (= "naive" (one c "SHOW pg_datahike.float_sum"))
        "off by default -- differential-fuzz compares against a real
         PostgreSQL, which sums naively")
    (exec! c "SET pg_datahike.float_sum = 'compensated'")
    (is (= "compensated" (one c "SHOW pg_datahike.float_sum")))
    (is (= "4" (one c "SELECT sum(v) FROM fs"))
        "the true total, whatever order the plan produced")
    (exec! c "RESET pg_datahike.float_sum")
    (is (= "naive" (one c "SHOW pg_datahike.float_sum")))
    (testing "an unknown value is refused"
      (is (thrown-with-msg?
           Exception #"pg_datahike.float_sum must be 'naive' or 'compensated'"
           (exec! c "SET pg_datahike.float_sum = 'wrong'"))))
    (testing "RESET ALL clears it too"
      (exec! c "SET pg_datahike.float_sum = 'compensated'")
      (exec! c "RESET ALL")
      (is (= "naive" (one c "SHOW pg_datahike.float_sum"))))))
;; ---------------------------------------------------------------------------
;; The zone in a datetime literal
;;
;; `timestamp without time zone` keeps the fields it was given and DROPS
;; the zone -- datetime.c decodes tzp and then ignores it -- while
;; `timestamptz` converts by it. Only the NAMED spelling was handled
;; here; a numeric offset fell through to a parser that converts
;; whatever the target, so `'2000-01-01 12:00:00+05'::timestamp`
;; answered 07:00:00. Five hours out, reported as success.
;;
;; The zone is read once now (`split-trailing-zone`) and applied by the
;; target, which is also what fixed three timestamptz spellings that
;; used to raise or answer midnight.
;;
;; Every expectation below is a PostgreSQL 17 oracle's.
;; ---------------------------------------------------------------------------

(deftest a-zone-in-the-literal-is-the-targets-to-apply
  (with-open [c (jdbc)]
    (testing "timestamp drops it, whatever the spelling"
      (doseq [lit ["2000-01-01 12:00:00+05"
                   "2000-01-01 12:00:00-05"
                   "2000-01-01 12:00:00+0530"
                   "2000-01-01 12:00:00 +05"
                   "2000-01-01 12:00:00Z"
                   "2000-01-01T12:00:00+05:00"
                   "2000-01-01 12:00:00 PST"
                   "2000-01-01 12:00:00 America/New_York"]]
        (is (= "2000-01-01 12:00:00" (one c (str "SELECT '" lit "'::timestamp::text")))
            lit))
      (is (= "2000-01-01 12:00:00.5"
             (one c "SELECT '2000-01-01 12:00:00.5+03'::timestamp::text"))
          "a fractional second before the offset is still a time field"))
    (testing "timestamptz converts by it"
      (is (= "2000-01-01 07:00:00+00"
             (one c "SELECT '2000-01-01 12:00:00+05'::timestamptz::text")))
      (is (= "2000-01-01 06:30:00+00"
             (one c "SELECT '2000-01-01 12:00:00+05:30'::timestamptz::text")))
      (testing "including the spellings that used to raise"
        ;; A space before the offset, and an offset after HH:MM with no
        ;; seconds, reached neither branch and were reported as invalid
        ;; input for a literal PostgreSQL reads.
        (is (= "2000-01-01 07:00:00+00"
               (one c "SELECT '2000-01-01 12:00:00 +05'::timestamptz::text")))
        (is (= "2000-01-01 10:00:00+00"
               (one c "SELECT '2000-01-01 12:00+02'::timestamptz::text"))))
      (testing "and a date-only literal whose offset was being dropped"
        ;; `2000-09-07 -07` is midnight at -07, which is 07:00 UTC. It
        ;; answered midnight UTC.
        (is (= "2000-09-07 07:00:00+00"
               (one c "SELECT '2000-09-07 -07'::timestamptz::text")))))
    (testing "a bare date is not a zone"
      ;; `2000-01-01` ends in `-01`. An end-anchored zone pattern with no
      ;; guard eats the day, leaves `2000-01`, and the literal then
      ;; parses as nothing at all.
      (is (= "2000-01-01 00:00:00" (one c "SELECT '2000-01-01'::timestamp::text")))
      (is (= "2000-09-07 00:00:00" (one c "SELECT '2000-09-07 -07'::timestamp::text"))
          "the offset is dropped, not subtracted"))))
