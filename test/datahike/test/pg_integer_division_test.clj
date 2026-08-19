(ns datahike.test.pg-integer-division-test
  "`/` over two integers is INTEGER division in PostgreSQL, truncated
   toward zero. Clojure's `/` instead produces an exact Ratio, so the
   answers were wrong in two different ways:

     SELECT 7 / 2           ->  3.5                (want 3)
     SELECT 2147483647 / 2  ->  1.0737418235E9     (want 1073741823)

   the second being a Ratio rendered through double -- neither the right
   value nor a syntax any client can read back.

   SQL arithmetic is evaluated in TWO places: the datalog function
   binding, and a server-side post-processor for expressions over
   aggregates (`sum(v) / 2`). Both are covered here, because `/` was
   wrong in both and the second one could not evaluate most of these
   expressions at all.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn div-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"div" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each div-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/div?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs ^long %) (range 1 (inc n)))))
          acc)))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ag (id int, g text, v int)")
  (exec! c "INSERT INTO ag VALUES (1,'a',10),(2,'a',20),(3,'b',7),(4,'b',3),(5,'b',5)"))

(deftest integer-division-truncates
  (with-open [c (jdbc)]
    (is (= "0" (one c "SELECT 1 / 3")))
    (is (= "3" (one c "SELECT 7 / 2")))
    (is (= "2" (one c "SELECT 10 / 5")) "exact division is unaffected")
    (is (= "0" (one c "SELECT (1 + 2) / 4")))
    (is (= "0" (one c "SELECT 1 / 3 * 3"))
        "truncation happens per operation, as in PostgreSQL")))

(deftest integer-division-truncates-toward-zero-not-negative-infinity
  (with-open [c (jdbc)]
    (testing "this is what separates quot from floor division -- Math/floorDiv
              would answer -4 for the first two"
      (is (= "-3" (one c "SELECT -7 / 2")))
      (is (= "-3" (one c "SELECT 7 / -2")))
      (is (= "3"  (one c "SELECT -7 / -2"))))))

(deftest large-integer-division-stays-an-integer
  (with-open [c (jdbc)]
    (is (= "1073741823" (one c "SELECT 2147483647 / 2"))
        "a Ratio rendered through double gave 1.0737418235E9 -- wrong value
         AND a syntax no client can read back")
    (is (= "4611686018427387903" (one c "SELECT 9223372036854775807 / 2")))))

(deftest mixed-operands-keep-fractional-division
  (with-open [c (jdbc)]
    (is (= "3.5" (one c "SELECT 7 / 2.0"))
        "only integer-over-integer is integer division; the operand types
         pick the operator, as PostgreSQL's function resolution does")
    (is (= "3.5" (one c "SELECT 7.0 / 2")))))

(deftest division-by-zero-still-raises
  (with-open [c (jdbc)]
    (is (thrown? Exception (one c "SELECT 1 / 0"))
        "the zero check runs before dividing, for integers too")))

(deftest arithmetic-over-aggregates-uses-the-same-division
  (with-open [c (jdbc)]
    (seed! c)
    (testing "the second evaluation site -- a server-side post-processor --
              had its own copy of the operators"
      (is (= "22" (one c "SELECT sum(v) / 2 FROM ag")))
      (is (= "9"  (one c "SELECT sum(v) / count(*) FROM ag"))))))

(deftest arithmetic-over-aggregates-accepts-a-constant-operand
  (with-open [c (jdbc)]
    (seed! c)
    (testing "an operand that is not an aggregate has no column to read; it
              used to index the row with a nil index and throw"
      (is (= "46" (one c "SELECT sum(v) + 1 FROM ag")))
      (is (= "-35" (one c "SELECT 10 - sum(v) FROM ag")))
      (is (= "22" (one c "SELECT sum(v) / 2 AS half FROM ag"))))))

(deftest arithmetic-over-aggregates-accepts-count-star
  (with-open [c (jdbc)]
    (seed! c)
    (testing "count(*) translates its argument to the keyword :*, which is
              not a datalog variable -- the whole query used to be rejected"
      (is (= "2"  (one c "SELECT count(*) / 2 FROM ag")))
      (is (= "15" (one c "SELECT count(*) * 3 FROM ag")))
      (is (= "10" (one c "SELECT 2 * count(*) FROM ag"))))))

(deftest arithmetic-over-aggregates-nests
  (with-open [c (jdbc)]
    (seed! c)
    (testing "a flat pair of column indices cannot express `sum(v) * 2 + 1`"
      (is (= "91" (one c "SELECT sum(v) * 2 + 1 FROM ag")))
      (is (= "91" (one c "SELECT (sum(v) * 2) + 1 FROM ag")))
      (is (= "18" (one c "SELECT max(v) - min(v) + 1 FROM ag")))
      (is (= "51" (one c "SELECT sum(v) + count(*) + 1 FROM ag"))))))

(deftest arithmetic-over-aggregates-under-group-by
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [["a" "15"] ["b" "7"]]
           (rows c "SELECT g, sum(v) / 2 FROM ag GROUP BY g ORDER BY g")))
    (is (= [["a" "20"] ["b" "30"]]
           (rows c "SELECT g, count(*) * 10 FROM ag GROUP BY g ORDER BY g")))
    (is (= [["a" "10"] ["b" "4"]]
           (rows c "SELECT g, max(v) - min(v) FROM ag GROUP BY g ORDER BY g")))
    (is (= [["a" "15"] ["b" "5"]]
           (rows c "SELECT g, sum(v) / count(*) FROM ag GROUP BY g ORDER BY g")))))

(deftest arithmetic-over-aggregates-describes-its-real-shape
  (with-open [c (jdbc)]
    (seed! c)
    (testing "the parsed find-aliases describe the HIDDEN per-aggregate
              columns; Execute appends the computed value and drops them, so
              Describe has to report the same reshape. Without it a client
              reading by the advertised column count ran off the end of the
              row -- unreadable over the extended protocol, i.e. every
              client except psql's simple queries."
      (with-open [st (.createStatement c)
                  rs (.executeQuery st "SELECT max(v) - min(v) FROM ag")]
        (let [md (.getMetaData rs)]
          (is (= 1 (.getColumnCount md))
              "one output column, not the two internal __compound_ ones")
          (is (= "?column?" (.getColumnName md 1))
              "PostgreSQL's name for a computed column -- the expression text
               also leaked the $1 left behind by the plan-cache rewrite")
          (.next rs)
          (is (= "17" (.getString rs 1))))))
    (testing "and under GROUP BY, where a real column precedes them"
      (with-open [st (.createStatement c)
                  rs (.executeQuery st "SELECT g, sum(v) / 2 FROM ag GROUP BY g ORDER BY g")]
        (let [md (.getMetaData rs)]
          (is (= 2 (.getColumnCount md)))
          (is (= ["g" "?column?"] [(.getColumnName md 1) (.getColumnName md 2)])))))
    (testing "an explicit alias still wins"
      (with-open [st (.createStatement c)
                  rs (.executeQuery st "SELECT sum(v) / 2 AS half FROM ag")]
        (is (= "half" (.getColumnName (.getMetaData rs) 1)))))))
