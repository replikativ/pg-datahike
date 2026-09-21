(ns datahike.test.pg-composite-field-test
  "`(expr).field` — a field of a composite value.

   Nothing translated it: every spelling raised `expression of type
   RowGetExpression is not supported`, including the whole-row form
   `(t).col` and the `(f(x)).n` that a record-returning function is
   read with.

   PostgreSQL names an anonymous ROW's fields f1, f2, … and a whole-row
   reference's fields after the table's columns; both are selected the
   same way. Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- cf-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"cf" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each cf-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/cf?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- scalar [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (.next rs)
    (.getString rs 1)))

(defn- state [^Connection c sql]
  (try
    (with-open [st (.createStatement c)] (.execute st sql))
    :no-error
    (catch SQLException e (.getSQLState e))))

(deftest a-field-of-a-composite-value
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE rc (id int, a int, b text)")
    (exec! c "INSERT INTO rc VALUES (1, 5, 'x')")
    (testing "an anonymous ROW, by position"
      (is (= "1" (scalar c "SELECT (row(1,2)).f1")))
      (is (= "2" (scalar c "SELECT (row(1,2)).f2")))
      (is (= "t" (scalar c "SELECT (row(1,'a',true)).f3")))
      (is (= "a" (scalar c "SELECT (row(1,'a',true)).f2"))))
    (testing "a whole-row reference, by column name"
      (is (= "5" (scalar c "SELECT (rc).a FROM rc")))
      (is (= "x" (scalar c "SELECT (rc).b FROM rc")))
      (is (= "1" (scalar c "SELECT (rc).id FROM rc"))))
    (testing "a field that is not there is 42703"
      (is (= "42703" (state c "SELECT (row(1,2)).f3")))
      (is (= "42703" (state c "SELECT (rc).nosuch FROM rc"))))
    (testing "in a predicate and an expression"
      (is (= "1" (scalar c "SELECT 1 WHERE (row(1,2)).f1 = 1")))
      (is (= "3" (scalar c "SELECT (row(1,2)).f1 + (row(1,2)).f2"))))))

(deftest an-int2vector-is-an-array
  ;; `pg_index.indkey` is an int2vector: PostgreSQL writes it
  ;; SPACE-separated (`1`, `1 2`), not in braces, so the array reader
  ;; saw a scalar and `a.attnum = ANY(i.indkey)` matched nothing. That
  ;; is how pgjdbc and Metabase ask which columns an index covers.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ix (id int PRIMARY KEY, v text)")
    (exec! c "CREATE TABLE ix2 (a int, b int, v text)")
    (exec! c "CREATE INDEX ix2_ab ON ix2 (a, b)")
    (testing "the rendering is PostgreSQL's"
      (is (= "1" (scalar c "SELECT i.indkey FROM pg_index i JOIN pg_class c ON c.oid = i.indrelid
                             WHERE c.relname = 'ix' AND i.indisprimary")))
      (is (= "1 2" (scalar c "SELECT i.indkey FROM pg_index i JOIN pg_class c ON c.oid = i.indrelid
                               WHERE c.relname = 'ix2'"))))
    (testing "and it reads as the vector it is"
      (is (= "id" (scalar c "SELECT a.attname FROM pg_index i
                               JOIN pg_class c ON c.oid = i.indrelid
                               JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(i.indkey)
                              WHERE c.relname = 'ix' AND i.indisprimary")))
      (is (= "a" (scalar c "SELECT a.attname FROM pg_index i
                              JOIN pg_class c ON c.oid = i.indrelid
                              JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(i.indkey)
                             WHERE c.relname = 'ix2' ORDER BY a.attnum")))
      (is (= "1 2" (scalar c "SELECT i.indkey FROM pg_index i JOIN pg_class c ON c.oid = i.indrelid
                               WHERE c.relname = 'ix2' AND 2 = ANY(i.indkey)"))))
    (testing "an untyped literal is still not an array"
      ;; PostgreSQL: malformed array literal. What must NOT happen is
      ;; reading `'12'` as a one-element vector and answering true.
      (is (not= "t" (scalar c "SELECT 12 = ANY('12')"))))
    (testing "ordinary arrays are unchanged"
      (is (= "t" (scalar c "SELECT 5 = ANY('{5,6}')")))
      (is (= "t" (scalar c "SELECT 7 = ANY(ARRAY[7,8])")))
      (is (= "f" (scalar c "SELECT 9 = ANY('{5,6}')"))))))
