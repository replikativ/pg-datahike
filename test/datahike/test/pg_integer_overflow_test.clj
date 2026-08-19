(ns datahike.test.pg-integer-overflow-test
  "Integer arithmetic at PostgreSQL's declared widths.

   int2 and int4 do not exist at runtime -- Datahike stores every
   integer as a Java long -- so overflow can only be detected against
   the width the TRANSLATOR sees. Without that, an int4 column happily
   produced values its own type cannot hold:

     2147483647::int4 + 1   ->  2147483648   (PostgreSQL: 22003)
     abs(-9223372036854775808) -> itself     -- a NEGATIVE absolute value

   The width travels as a leading constant argument to the runtime op,
   and the op falls back to the generic operator when the values turn
   out not to be integral -- so `int4col + 1.5` still promotes to
   numeric rather than being range-checked as an integer.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn ovf-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"o" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each ovf-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/o?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE t (id int primary key, s smallint, i integer, b bigint)")
  (exec! c "INSERT INTO t VALUES (1, 32767, 2147483647, 9223372036854775807)")
  (exec! c "INSERT INTO t VALUES (2, -32768, -2147483648, -9223372036854775808)")
  (exec! c "INSERT INTO t VALUES (3, 5, 5, 5)"))

(deftest arithmetic-overflows-at-the-declared-width
  (with-open [c (jdbc)]
    (seed! c)
    (is (thrown-with-msg? SQLException #"integer out of range"
                          (one c "SELECT i + 1 FROM t WHERE id = 1")))
    (is (thrown-with-msg? SQLException #"integer out of range"
                          (one c "SELECT i * 2 FROM t WHERE id = 1")))
    (is (thrown-with-msg? SQLException #"integer out of range"
                          (one c "SELECT i - 1 FROM t WHERE id = 2")))
    (is (thrown-with-msg? SQLException #"bigint out of range"
                          (one c "SELECT b + 1 FROM t WHERE id = 1"))
        "int8 overflow used to surface as a raw Java \"long overflow\", so a
         client trapping SQLSTATE 22003 never caught it")))

(deftest a-bare-integer-literal-is-int4-so-smallint-widens
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "32768" (one c "SELECT s + 1 FROM t WHERE id = 1"))
        "smallint + integer resolves to int24pl and yields INT4, so this
         is legal -- taking the column's int2 width would wrongly reject it")
    (is (thrown-with-msg? SQLException #"smallint out of range"
                          (one c "SELECT s * s FROM t WHERE id = 1"))
        "but smallint * smallint IS int2 on both sides")))

(deftest unary-minus-and-abs-are-width-checked
  (with-open [c (jdbc)]
    (seed! c)
    (is (thrown-with-msg? SQLException #"integer out of range"
                          (one c "SELECT -i FROM t WHERE id = 2")))
    (is (thrown-with-msg? SQLException #"smallint out of range"
                          (one c "SELECT -s FROM t WHERE id = 2")))
    (is (thrown-with-msg? SQLException #"integer out of range"
                          (one c "SELECT abs(i) FROM t WHERE id = 2")))
    (is (thrown-with-msg? SQLException #"bigint out of range"
                          (one c "SELECT abs(b) FROM t WHERE id = 2"))
        "Java's abs WRAPS at the minimum, so this returned its own negative
         input -- an absolute value that is negative")))

(deftest division-overflows-only-at-min-over-minus-one
  (with-open [c (jdbc)]
    (seed! c)
    (is (thrown-with-msg? SQLException #"integer out of range"
                          (one c "SELECT i / (-1) FROM t WHERE id = 2")))
    (is (thrown-with-msg? SQLException #"bigint out of range"
                          (one c "SELECT b / (-1) FROM t WHERE id = 2"))
        "at int8 a range check cannot catch this -- the wrapped quotient IS
         in range -- so the division itself has to be the exact one")
    (is (= "-1073741824" (one c "SELECT i / 2 FROM t WHERE id = 2"))
        "ordinary division is unaffected")))

(deftest a-non-integral-operand-leaves-integer-arithmetic
  (with-open [c (jdbc)]
    (seed! c)
    (testing "the declared width stops governing once PostgreSQL has
              promoted -- these must NOT be range-checked as int4"
      (is (= "2147483648.0" (one c "SELECT i + 1.0 FROM t WHERE id = 1")))
      (is (= "2147483648" (one c "SELECT i + 1::bigint FROM t WHERE id = 1"))))))

(deftest integer-literal-and-expression-types
  (with-open [c (jdbc)]
    (seed! c)
    (testing "PostgreSQL types an integer literal as the narrowest of
              int4/int8 that holds it"
      (is (= "integer" (one c "SELECT pg_typeof(1)")))
      (is (= "bigint" (one c "SELECT pg_typeof(2147483648)"))))
    (testing "and arithmetic keeps the wider operand's width -- on row 3,
              whose values do not overflow (pg_typeof still EVALUATES its
              argument, so row 1 would raise before reporting a type)"
      (is (= "integer" (one c "SELECT pg_typeof(1 + 1)")))
      (is (= "smallint" (one c "SELECT pg_typeof(s + s) FROM t WHERE id = 3")))
      (is (= "integer" (one c "SELECT pg_typeof(s + i) FROM t WHERE id = 3")))
      (is (= "bigint" (one c "SELECT pg_typeof(i + b) FROM t WHERE id = 3"))))))
