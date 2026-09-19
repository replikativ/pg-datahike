(ns datahike.test.pg-check-constraint-test
  "CHECK constraints, domain checks and DEFAULTs, evaluated by the SELECT
   translator (datahike.pg.sql.row-eval).

   The interpreter this replaced treated any expression shape it did not
   know as satisfied and compared only numbers with < and >, so text,
   date, LIKE, regex, IS DISTINCT FROM, JSON and array CHECKs accepted
   every row. Expectations are PostgreSQL 17.7's."
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
          {:keys [server]} (pg/start-server {"ck" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- jdbc ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/ck?user=x&password=x&sslmode=disable")))

(defn- run
  "Rows of the first column, the update count, or [:error sqlstate message]."
  [^Connection c sql]
  (try
    (with-open [st (.createStatement c)]
      (if (.execute st sql)
        (with-open [rs (.getResultSet st)]
          (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc)))
        (.getUpdateCount st)))
    (catch SQLException e
      [:error (.getSQLState e)
       (.getMessage (.getServerErrorMessage ^org.postgresql.util.PSQLException e))])))

(defn- violation [constraint]
  [:error "23514" (str "new row for relation \"ck1\" violates check constraint \"" constraint "\"")])

(defn- domain-violation [domain]
  [:error "23514" (str "value for domain " domain " violates check constraint \"" domain "_check\"")])

(deftest check-constraints-use-sql-semantics
  (with-open [c (jdbc)]
    (run c "CREATE DOMAIN posint AS int CHECK (VALUE > 0)")
    (run c "CREATE DOMAIN shortname AS text CHECK (length(VALUE) <= 3 AND VALUE ~ '^[a-z]+$')")
    (run c (str "CREATE TABLE ck1 (id int PRIMARY KEY, name text CHECK (name > 'm'), "
                "d date CHECK (d < '2030-01-01'), s text CHECK (s LIKE 'a%'), "
                "n numeric CHECK (n IS DISTINCT FROM 13), p posint, sn shortname, "
                "j jsonb CHECK (j ->> 'k' <> 'bad'), arr int[] CHECK (arr[1] > 0), "
                "t time CHECK (t < '12:00'), x int CHECK (x / 0 > 1))"))
    (testing "shapes the old interpreter passed unchecked"
      (is (= (violation "ck1_name_check") (run c "INSERT INTO ck1 (id, name) VALUES (1, 'a')")))
      (is (= 1 (run c "INSERT INTO ck1 (id, name) VALUES (2, 'z')")))
      (is (= (violation "ck1_d_check") (run c "INSERT INTO ck1 (id, d) VALUES (3, '2040-01-01')")))
      (is (= (violation "ck1_s_check") (run c "INSERT INTO ck1 (id, s) VALUES (4, 'b')")))
      (is (= 1 (run c "INSERT INTO ck1 (id, s) VALUES (5, 'abc')")))
      (is (= (violation "ck1_n_check") (run c "INSERT INTO ck1 (id, n) VALUES (6, 13)")))
      (is (= (violation "ck1_j_check") (run c "INSERT INTO ck1 (id, j) VALUES (12, '{\"k\":\"bad\"}')")))
      (is (= (violation "ck1_arr_check") (run c "INSERT INTO ck1 (id, arr) VALUES (13, '{0,1}')")))
      (is (= (violation "ck1_t_check") (run c "INSERT INTO ck1 (id, t) VALUES (14, '13:00')"))))
    (testing "an error inside the expression aborts the statement"
      (is (= [:error "22012" "division by zero"] (run c "INSERT INTO ck1 (id, x) VALUES (15, 1)"))))
    (testing "NULL passes a CHECK"
      (is (= 1 (run c "INSERT INTO ck1 (id, name) VALUES (16, NULL)"))))
    (testing "domains, on writes and on casts"
      (is (= (domain-violation "posint") (run c "INSERT INTO ck1 (id, p) VALUES (7, 0)")))
      (is (= 1 (run c "INSERT INTO ck1 (id, p) VALUES (8, 5)")))
      (is (= (domain-violation "shortname") (run c "INSERT INTO ck1 (id, sn) VALUES (9, 'abcd')")))
      (is (= (domain-violation "shortname") (run c "INSERT INTO ck1 (id, sn) VALUES (10, 'ab1')")))
      (is (= 1 (run c "INSERT INTO ck1 (id, sn) VALUES (11, 'ab')")))
      (is (= ["5"] (run c "SELECT '5'::posint")))
      (is (= (domain-violation "posint") (run c "SELECT '0'::posint"))))
    (testing "UPDATE is checked too"
      (is (= (violation "ck1_name_check") (run c "UPDATE ck1 SET name = 'a' WHERE id = 2"))))
    (is (= ["2" "5" "8" "11" "16"] (run c "SELECT id FROM ck1 ORDER BY id")))))

(deftest defaults-are-read-as-the-column-type
  (with-open [c (jdbc)]
    (run c (str "CREATE TABLE df (id int PRIMARY KEY, s text DEFAULT '123', n numeric DEFAULT 1.50, "
                "f float8 DEFAULT 2, b bool DEFAULT 'yes', i int DEFAULT '7', d date DEFAULT '2020-01-02')"))
    (run c "INSERT INTO df (id) VALUES (1)")
    (is (= ["123"] (run c "SELECT s FROM df")))
    (is (= ["text"] (run c "SELECT pg_typeof(s)::text FROM df")))
    (is (= ["1.50"] (run c "SELECT n FROM df")) "the literal's scale survives")
    (is (= ["t"] (run c "SELECT b FROM df")))
    (is (= ["7"] (run c "SELECT i FROM df")))
    (is (= ["2020-01-02"] (run c "SELECT d FROM df")))
    (testing "AT TIME ZONE folds into now() only for UTC"
      (is (= 0 (run c "CREATE TABLE dz (id int PRIMARY KEY, a timestamp DEFAULT (now() AT TIME ZONE 'UTC'))")))
      (is (= "0A000" (second (run c "CREATE TABLE dz2 (id int PRIMARY KEY, a timestamp DEFAULT (now() AT TIME ZONE 'America/New_York'))")))))))

(deftest on-conflict-checks-and-where-use-sql-semantics
  (with-open [c (jdbc)]
    (run c "CREATE TABLE oc (id int PRIMARY KEY, s text CHECK (s LIKE 'a%'))")
    (run c "INSERT INTO oc VALUES (1, 'ab')")
    (is (= [:error "23514" "new row for relation \"oc\" violates check constraint \"oc_s_check\""]
           (run c "INSERT INTO oc VALUES (1, 'ac') ON CONFLICT (id) DO UPDATE SET s = 'zz'")))
    (is (= 1 (run c "INSERT INTO oc VALUES (1, 'ac') ON CONFLICT (id) DO UPDATE SET s = excluded.s WHERE oc.s LIKE 'a%'"))
        "LIKE in the conflict WHERE was 'unknown', so the row was never updated")
    (is (= ["ac"] (run c "SELECT s FROM oc WHERE id = 1")))))

(deftest check-expressions-type-their-columns-as-declared
  (with-open [c (jdbc)]
    (testing "a quoted identifier (Django quotes every column)"
      (run c "CREATE TABLE q (id int PRIMARY KEY, \"age\" integer CHECK (\"age\" >= 0))")
      (is (= 1 (run c "INSERT INTO q VALUES (1, 5)")))
      (is (= "23514" (second (run c "INSERT INTO q VALUES (2, -1)")))))
    (testing "a subscript indexed by another column"
      (run c "CREATE TABLE s (id int PRIMARY KEY, a int[], i int, CHECK (a[i] > 0))")
      (is (= 1 (run c "INSERT INTO s VALUES (1, '{5,-1}', 1)")))
      (is (= "23514" (second (run c "INSERT INTO s VALUES (2, '{5,-1}', 2)")))))
    (testing "type modifiers: bit(3) is not bit(1)"
      (run c "CREATE TABLE b (id int PRIMARY KEY, b bit(3) CHECK (b = b'101'))")
      (is (= 1 (run c "INSERT INTO b VALUES (1, b'101')")))
      (is (= "23514" (second (run c "INSERT INTO b VALUES (2, b'100')")))))
    (testing "an enum compares in declaration order"
      (run c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
      (run c "CREATE TABLE e (id int PRIMARY KEY, m mood CHECK (m > 'sad'))")
      (is (= 1 (run c "INSERT INTO e VALUES (1, 'happy')")))
      (is (= "23514" (second (run c "INSERT INTO e VALUES (2, 'sad')"))))
      (is (= ["t"] (run c "SELECT 'happy'::mood > 'sad'"))))
    (testing "a subquery is refused when the table is created"
      (is (= [:error "0A000" "cannot use subquery in check constraint"]
             (run c "CREATE TABLE x (a int CHECK (a IN (SELECT 1)))"))))))

(deftest enum-labels-collations-and-typmods
  (with-open [c (jdbc)]
    (run c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (run c "CREATE TABLE t (id int PRIMARY KEY, m mood)")
    (run c "INSERT INTO t VALUES (1, 'ok'), (2, NULL)")
    (testing "a label outside the enum is invalid when the statement is read, rows or not"
      (let [bad [:error "22P02" "invalid input value for enum mood: \"xyz\""]]
        (is (= bad (run c "SELECT id FROM t WHERE m > 'xyz'")))
        (is (= bad (run c "SELECT 'happy'::mood > 'xyz'")))
        (is (= bad (run c "SELECT NULL::mood > 'xyz'")))))
    (is (= ["1"] (run c "SELECT id FROM t WHERE m >= 'sad' ORDER BY id")))
    (testing "COLLATE inside a CHECK"
      (run c "CREATE TABLE co (id int PRIMARY KEY, name text CHECK (name COLLATE \"C\" > 'm'))")
      (is (= 1 (run c "INSERT INTO co VALUES (1, 'z')")))
      (is (= "23514" (second (run c "INSERT INTO co VALUES (2, 'a')")))))
    (testing "precision modifiers on temporal columns"
      (run c "CREATE TABLE tz (id int PRIMARY KEY, ts timestamptz(3) CHECK (ts > '2020-01-01'), tt time(2) CHECK (tt < '12:00'))")
      (is (= 1 (run c "INSERT INTO tz VALUES (1, '2021-01-01 00:00:00.1234+00', '10:00')")))
      (is (= "23514" (second (run c "INSERT INTO tz VALUES (2, '2019-01-01', NULL)"))))
      (is (= "23514" (second (run c "INSERT INTO tz VALUES (3, NULL, '13:00')")))))
    (testing "a quoted mixed-case enum name"
      (run c "CREATE TYPE \"MixedCase\" AS ENUM ('a', 'b')")
      (run c "CREATE TABLE mc (id int PRIMARY KEY, x \"MixedCase\" CHECK (x > 'a'))")
      (is (= 1 (run c "INSERT INTO mc VALUES (1, 'b')")))
      (is (= "23514" (second (run c "INSERT INTO mc VALUES (2, 'a')")))))))

(deftest update-and-domain-definitions-are-validated
  (with-open [c (jdbc)]
    (is (= [:error "0A000" "cannot use subquery in check constraint"]
           (run c "CREATE DOMAIN dq AS int CHECK (VALUE IN (SELECT 1))")))
    (run c "CREATE DOMAIN posint AS int CHECK (VALUE > 0)")
    (run c "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')")
    (run c "CREATE TABLE u (id int PRIMARY KEY, d posint, m mood)")
    (run c "INSERT INTO u VALUES (1, 5, 'ok')")
    (is (= [:error "23514" "value for domain posint violates check constraint \"posint_check\""]
           (run c "UPDATE u SET d = 0 WHERE id = 1")))
    (is (= [:error "22P02" "invalid input value for enum mood: \"nope\""]
           (run c "UPDATE u SET m = 'nope' WHERE id = 1")))
    (testing "a CHECK shape row evaluation cannot substitute fails loudly, not wrongly"
      (run c "CREATE TABLE j (id int PRIMARY KEY, a int, CHECK (JSON_OBJECT('k': a) IS NOT NULL))")
      (is (= "0A000" (second (run c "INSERT INTO j VALUES (1, 1)")))))
    (is (= ["5"] (run c "SELECT d FROM u")))))

(deftest writes-keep-false-and-null
  (with-open [c (jdbc)]
    (run c "CREATE TABLE w (id int PRIMARY KEY, i int, b bool, n numeric)")
    (run c "INSERT INTO w VALUES (1, 1, 'no', NULL), (2, 2, false, 1.5)")
    (testing "INSERT ... SELECT of a NULL (the query engine's sentinel reached the write)"
      (is (= 2 (run c "INSERT INTO w (id, i, b) SELECT id + 100, n, b FROM w"))))
    (testing "a coerced false is written as false, not as its input text"
      (is (= 1 (run c "INSERT INTO w (id, b) VALUES (3, 'of') ON CONFLICT (id) DO UPDATE SET b = excluded.b")))
      (is (= 1 (run c "INSERT INTO w (id, b) VALUES (1, 'yes') ON CONFLICT (id) DO UPDATE SET b = 'no'"))))
    (is (= ["1|1|false" "2|2|false" "3||false" "101||false" "102|2|false"]
           (run c "SELECT id || '|' || coalesce(i::text, '') || '|' || b::text FROM w ORDER BY id")))))
