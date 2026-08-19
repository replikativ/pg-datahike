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
