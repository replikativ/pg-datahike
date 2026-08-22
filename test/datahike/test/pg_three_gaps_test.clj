(ns datahike.test.pg-three-gaps-test
  "Three conformance gaps the differential fuzzer kept reporting, each
   with its rule taken from the PostgreSQL source:

     0.0 / -1                    printed `0`; -0.0 is a distinct float
                                 value and PostgreSQL prints `-0`
                                 (float.c prints the sign; `zero?` does
                                 not distinguish the two zeros)
     date_trunc('day', ts)       came back with a `+00` offset -- a
                                 DIFFERENT INSTANT for a client that
                                 reads it as local time. Each of the
                                 three overloads returns its SECOND
                                 argument's type (pg_proc.dat)
     jsonb_array_length(scalar)  answered NULL and dropped the row;
                                 jsonfuncs.c raises 22023

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn tg-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"tg" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each tg-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/tg?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- sqlstate [^Connection c sql]
  (try
    (col c 1 sql)
    nil
    (catch org.postgresql.util.PSQLException e (.getSQLState e))))

(defn- seed! [^Connection c]
  ;; Text rendering of timestamptz is session-sensitive. Pin the same
  ;; deterministic zone used by the PostgreSQL oracle for these fixtures.
  (exec! c "SET TIME ZONE 'UTC'")
  (exec! c (str "CREATE TABLE zz (id int, f float8, r real, ts timestamp, "
                "tz timestamptz, d date, js jsonb)"))
  (exec! c (str "INSERT INTO zz VALUES "
                "(1,0.0,0.0,'2020-01-01 10:20:30','2020-01-01 10:20:30+00',"
                "'2020-01-01','[1,2]'),"
                "(2,1.5,1.5,'2021-06-15 00:00:00','2021-06-15 00:00:00+00',"
                "'2021-06-15','{\"a\":1}')")))

(deftest negative-zero-keeps-its-sign
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["-0" "-1.5"] (col c 2 "SELECT id, f / -1 FROM zz ORDER BY id")))
    (is (= ["-0" "-1.5"] (col c 2 "SELECT id, r / -1 FROM zz ORDER BY id"))
        "real prints its sign too")
    (is (= ["-0"] (col c 1 "SELECT -1 * 0.0::float8")))
    (testing "and numeric does NOT -- it normalises -0 away"
      (is (= ["0.00000000000000000000"] (col c 1 "SELECT 0.0::numeric / -1"))))))

(deftest date-trunc-returns-its-argument-type
  (with-open [c (jdbc)]
    (seed! c)
    (testing "a plain timestamp stays offset-less"
      (is (= ["2020-01-01 00:00:00" "2021-06-15 00:00:00"]
             (col c 2 "SELECT id, date_trunc('day', ts) FROM zz ORDER BY id")))
      (is (= ["timestamp without time zone"]
             (col c 1 "SELECT pg_typeof(date_trunc('day', ts)) FROM zz LIMIT 1"))))
    (testing "a timestamptz keeps its offset"
      (is (= ["2020-01-01 00:00:00+00" "2021-06-15 00:00:00+00"]
             (col c 2 "SELECT id, date_trunc('day', tz) FROM zz ORDER BY id")))
      (is (= ["timestamp with time zone"]
             (col c 1 "SELECT pg_typeof(date_trunc('day', tz)) FROM zz LIMIT 1"))))
    (testing "a date has no overload of its own"
      ;; It coerces implicitly to BOTH timestamp and timestamptz, so
      ;; resolution falls to category D's PREFERRED type, timestamptz.
      (is (= ["2020-01-01 00:00:00+00" "2021-06-01 00:00:00+00"]
             (col c 2 "SELECT id, date_trunc('month', d) FROM zz ORDER BY id"))))))

(deftest jsonb-array-length-is-strict
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["3"] (col c 1 "SELECT jsonb_array_length('[1,2,3]'::jsonb)")))
    (is (= ["0"] (col c 1 "SELECT jsonb_array_length('[]'::jsonb)")))
    (is (= [nil] (col c 1 "SELECT jsonb_array_length(NULL::jsonb)"))
        "strict: NULL in, NULL out")
    (testing "a non-array is an error, with PostgreSQL's two messages"
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException #"cannot get array length of a scalar"
           (col c 1 "SELECT jsonb_array_length('null'::jsonb)"))
          "JSON null is a scalar, not SQL NULL")
      (is (= "22023" (sqlstate c "SELECT jsonb_array_length('null'::jsonb)")))
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException #"cannot get array length of a scalar"
           (col c 1 "SELECT jsonb_array_length('5'::jsonb)")))
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException #"cannot get array length of a non-array"
           (col c 1 "SELECT jsonb_array_length('{\"a\":1}'::jsonb)"))))
    (testing "so a column of mixed shapes fails the statement"
      ;; It used to answer NULL, which DROPPED the row: the query
      ;; returned only the rows whose value happened to be an array.
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException #"cannot get array length of a non-array"
           (col c 2 "SELECT id, jsonb_array_length(js) FROM zz ORDER BY id"))))))
