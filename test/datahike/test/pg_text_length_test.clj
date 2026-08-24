(ns datahike.test.pg-text-length-test
  "varchar(n) / char(n) declared lengths.

   The length was dropped on the floor twice over. A CAST ignored it, so
   `'abcdef'::varchar(4)` answered abcdef where PostgreSQL truncates to
   abcd. And the DDL never recorded it at all -- a `varchar(4)` column
   reported as plain `text` with no character_maximum_length, so nothing
   could enforce it on write either and the column happily held text its
   own declared type forbids.

   PostgreSQL draws a distinction the cast implementation already models
   for `bit`: an EXPLICIT cast truncates silently, an ASSIGNMENT refuses
   with 22001.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn tl-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"l" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each tl-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/l?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest explicit-casts-truncate
  (with-open [c (jdbc)]
    (is (= "abcd" (one c "SELECT 'abcdef'::varchar(4)")))
    (is (= "abcd" (one c "SELECT 'abcdef'::char(4)")))
    (is (= "abcd" (one c "SELECT 'abcdef'::character varying(4)")))
    (is (= "abcd" (one c "SELECT 'abcdef'::bpchar(4)")))
    (is (= "123" (one c "SELECT 12345::varchar(3)"))
        "the length applies to the RENDERED text, whatever the source type")
    (testing "and leave a fitting value alone"
      (is (= "abcd" (one c "SELECT 'abcd'::varchar(4)")))
      (is (= "abcdef" (one c "SELECT 'abcdef'::varchar"))
          "unmodified varchar has no limit at all"))))

(deftest the-declared-length-is-recorded
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int, v varchar(4), t2 text)")
    (is (= "character varying"
           (one c (str "SELECT data_type FROM information_schema.columns "
                       "WHERE table_name='t' AND column_name='v'")))
        "it reported plain `text` before, the length having been dropped
         at CREATE TABLE")
    (is (= "text"
           (one c (str "SELECT data_type FROM information_schema.columns "
                       "WHERE table_name='t' AND column_name='t2'"))))))

(deftest assignments-refuse-rather-than-truncate
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int, v varchar(4), c char(4))")
    (exec! c "INSERT INTO t VALUES (1, 'ab', 'ab')")
    (testing "bpchar is blank-padded at the PostgreSQL result boundary"
      (is (= "ab  " (one c "SELECT c FROM t WHERE id = 1"))))
    (testing "excess spaces are truncated rather than rejected"
      (exec! c "INSERT INTO t VALUES (2, 'abcd    ', 'xy      ')")
      (is (= "abcd" (one c "SELECT v FROM t WHERE id = 2")))
      (is (= "xy  " (one c "SELECT c FROM t WHERE id = 2"))))
    (testing "INSERT"
      (is (thrown-with-msg? SQLException #"value too long for type character varying\(4\)"
                            (exec! c "INSERT INTO t (id, v) VALUES (2, 'abcdef')")))
      (is (thrown-with-msg? SQLException #"value too long for type character\(4\)"
                            (exec! c "INSERT INTO t (id, c) VALUES (3, 'abcdef')"))))
    (testing "and UPDATE, which reaches the coercion by a different route"
      (is (thrown-with-msg? SQLException #"value too long for type character varying\(4\)"
                            (exec! c "UPDATE t SET v = 'xyzzy' WHERE id = 1"))))
    (testing "a fitting value still writes"
      (exec! c "INSERT INTO t (id, v) VALUES (4, 'abcd')")
      (is (= "abcd" (one c "SELECT v FROM t WHERE id = 4"))))
    (testing "and the refused rows left nothing behind"
      (is (= "3" (one c "SELECT count(*)::text FROM t"))))))

(deftest extra-float-digits-is-reported
  (with-open [c (jdbc)]
    (is (= "1" (one c "SHOW extra_float_digits"))
        "SHOW answered an empty string; PostgreSQL's default is 1, and
         above 0 it means shortest-round-trip -- which is what our float
         renderer already produces")
    (is (= "1" (one c "SELECT current_setting('extra_float_digits')"))
        "current_setting raised 42704 for it")))
