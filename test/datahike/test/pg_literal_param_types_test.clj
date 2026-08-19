(ns datahike.test.pg-literal-param-types-test
  "Literals keep their type through the plan-cache rewrite.

   To get one cached plan per statement FAMILY, the simple-query path
   rewrites `SELECT 1.5 + 1` into `SELECT $1 + $2` with the values
   alongside -- but it did that BEFORE the translator ran, and it kept
   only the values. Every translate-time type decision then went blind
   against an untyped `$N`:

     SELECT 3 / 2         reported int8   (PostgreSQL: int4)
     SELECT 2147483647+1  answered 2147483648, no overflow check
     SELECT 9223372036854775808   was a hard SQL parse error

   The type now travels with the value, following PostgreSQL's own rule
   (parse_node.c make_const): int4 if it fits int32, else int8, else
   numeric -- which is also what rescues the oversized literal.

   The parse-cache key had to grow the parameter types with it. See
   `templated-shapes-do-not-share-a-plan` below for why that is a
   correctness requirement and not a tidiness one."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn lp-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"lp" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each lp-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/lp?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- ^Connection jdbc-simple
  "pgjdbc uses the EXTENDED protocol even for `Statement`, so reaching the
   simple-query path -- the one the plan-cache rewrite lives on -- needs
   this explicitly."
  []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/lp?user=x&password=x&sslmode=disable&binaryTransfer=false"
        "&preferQueryMode=simple")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- typ [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (.getColumnTypeName (.getMetaData rs) 1)))

(deftest all-literal-arithmetic-is-typed
  (with-open [c (jdbc)]
    (testing "the rewrite used to hide both operands, so nothing could be typed"
      (is (= "int4" (typ c "SELECT 3 / 2")))
      (is (= "int4" (typ c "SELECT 1 + 1")))
      (is (= "int4" (typ c "SELECT 2 * 3")))
      (is (= "numeric" (typ c "SELECT 1.5 + 1")))
      (is (= "int8" (typ c "SELECT 2147483648 + 1"))))))

(deftest all-literal-arithmetic-overflows
  (with-open [c (jdbc)]
    (testing "with both operands untyped, no width governed and these
              silently produced values int4 cannot hold"
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (one c "SELECT 2147483647 + 1")))
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (one c "SELECT 100000 * 100000"))))
    (testing "and a unary sign folds into the constant before typing, as
              PostgreSQL's doNegate does -- so -2147483648 is int4, and
              taking its absolute value is out of range"
      (is (= "-2147483648" (one c "SELECT -2147483648")))
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (one c "SELECT abs(-2147483648)"))))))

(deftest literals-wider-than-int64-become-numeric
  (with-open [c (jdbc-simple)]
    (testing "Long/parseLong threw, the templater bailed, and JSqlParser
              then choked on the raw literal -- a hard parse error for a
              statement PostgreSQL accepts. Fixed on the SIMPLE path,
              where the templater replaces the literal before the parser
              ever sees it; on the extended path the raw literal still
              reaches JSqlParser and this remains an error."
      (is (= "9223372036854775808" (one c "SELECT 9223372036854775808")))
      (is (= "100000000000000000000" (one c "SELECT 100000000000000000000"))))))

(deftest templated-shapes-do-not-share-a-plan
  (with-open [c (jdbc)]
    (testing "`1 + 1` and `1.5 + 1` template to the SAME `$1 + $2`. What
              differs is not only the reported type: the runtime operator
              is chosen at translate time too. Sharing one cache entry
              would hand the second statement the first one's plan."
      (is (= "2" (one c "SELECT 1 + 1")))
      (is (= "2.5" (one c "SELECT 1.5 + 1")))
      (is (= "2" (one c "SELECT 1 + 1")) "still int arithmetic after the numeric one"))
    (testing "and in the other order"
      (is (= "1.5000000000000000" (one c "SELECT 3.0 / 2")))
      (is (= "1" (one c "SELECT 3 / 2")))
      (is (= "1.5000000000000000" (one c "SELECT 3.0 / 2"))))))

(deftest aggregate-plus-constant-templates-cleanly
  (with-open [c (jdbc)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id int, x int)")
      (.execute st "INSERT INTO t VALUES (1, 10), (2, 20)"))
    (testing "a sign after a CLOSING bracket is binary, not unary --
              reading it as unary emitted `SELECT sum(x)$1`, malformed SQL
              that failed the parse and silently fell back, so these very
              common shapes never reached any cache"
      (is (= "31" (one c "SELECT sum(x)+1 FROM t")))
      (is (= "1" (one c "SELECT count(*)-1 FROM t")))
      (is (= "40" (one c "SELECT max(x)*2 FROM t"))))))
