(ns datahike.test.pg-binary-format-test
  "RowDescription and DataRow must agree on the column's type.

   Describe and Execute computed their OIDs by DIFFERENT precedence:
   describeResult let the parse-time item OID always win, while the
   execute path let it win only when the schema lookup returned -1. When
   the two disagreed the descriptor said one width and the payload was
   another, and a binary-format client read garbage:

     SELECT 1 AS x FROM t   ->  advertised int4, sent 8 bytes  ->  0

   Zero, not one -- the client took the leading 4 bytes of a big-endian
   int8. In TEXT format the same mismatch is invisible, because the
   digits are correct whatever the descriptor claims.

   Every other JDBC fixture in this suite sets `binaryTransfer=false`,
   which is exactly why this went unseen. This one does not."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager PreparedStatement ResultSet]))

(def ^:dynamic *port* nil)

(defn bin-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"b" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each bin-fixture)

(defn- ^Connection jdbc-binary []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/b?user=x&password=x&sslmode=disable"
        ;; The point of this namespace.
        "&binaryTransfer=true&prepareThreshold=1")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- bin-read
  "Execute `sql` enough times that pgjdbc switches to a server-side
   prepared statement and requests BINARY results, then read column 1."
  [^Connection c sql]
  (with-open [^PreparedStatement ps (.prepareStatement c sql)]
    (dotimes [_ 3] (with-open [^ResultSet rs (.executeQuery ps)] (.next rs)))
    (with-open [^ResultSet rs (.executeQuery ps)]
      (.next rs)
      [(.getColumnTypeName (.getMetaData rs) 1) (.getString rs 1)])))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE t (id int, x int)")
  (exec! c "INSERT INTO t VALUES (1, 10)"))

(deftest computed-columns-round-trip-in-binary
  (with-open [c (jdbc-binary)]
    (seed! c)
    (testing "a literal whose alias collides with a real column -- the
              shape where the schema lookup and the parse-time inference
              disagreed"
      (is (= ["int4" "1"] (bin-read c "SELECT 1 AS x FROM t"))))
    (testing "and one that does not collide"
      (is (= ["int4" "1"] (bin-read c "SELECT 1 AS a FROM t"))))
    (testing "arithmetic over a column"
      (is (= ["int4" "1"] (bin-read c "SELECT id*1 AS x FROM t")))
      (is (= ["int4" "11"] (bin-read c "SELECT id+x AS a FROM t")))
      (is (= ["int4" "-1"] (bin-read c "SELECT -id AS a FROM t"))))))

(deftest casts-round-trip-in-binary
  (with-open [c (jdbc-binary)]
    (seed! c)
    (is (= ["text" "1"] (bin-read c "SELECT id::text AS a FROM t")))
    (is (= ["text" "10"] (bin-read c "SELECT x::text AS a FROM t")))
    (is (= ["int8" "1"] (bin-read c "SELECT id::bigint AS a FROM t")))
    (is (= ["int2" "1"] (bin-read c "SELECT id::smallint AS a FROM t")))
    (is (= ["float8" "1.0"] (bin-read c "SELECT id::float8 AS a FROM t")))))

(deftest aggregates-and-scalars-round-trip-in-binary
  (with-open [c (jdbc-binary)]
    (seed! c)
    (is (= ["int8" "1"] (bin-read c "SELECT count(*) AS a FROM t")))
    (is (= ["int8" "10"] (bin-read c "SELECT sum(x) AS a FROM t")))
    (is (= ["int4" "1"] (bin-read c "SELECT max(id) AS a FROM t")))
    (is (= ["bool" "t"] (bin-read c "SELECT true AS a FROM t")))
    (is (= ["bool" "f"] (bin-read c "SELECT id IS NULL AS a FROM t")))
    (is (= ["numeric" "2.5"] (bin-read c "SELECT 2.5 AS a FROM t")))
    (is (= ["int4" "1"] (bin-read c "SELECT id FROM t"))
        "a plain column was always fine -- both paths read it from the schema")))
