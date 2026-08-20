(ns datahike.test.pg-setops-windows-test
  "Silent wrong answers in IN-subqueries, set operations and window
   functions -- the second tranche of differential-fuzzing findings.

   All four were wrong ROWS or wrong ORDER rather than errors, which is
   the class that a client cannot detect:

     WHERE i IN (SELECT k …)     matched the rows where i IS NULL
     … EXCEPT … ORDER BY 1       came back in set/difference's order
     sum(i) OVER () … LIMIT 2    summed only the two surviving rows
     avg(i) OVER ()              answered a double, not PG's numeric

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn sw-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"sw" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each sw-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/sw?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ft (id int, i int, j int, s text, b boolean, f float8, n numeric, d date)")
  (exec! c (str "INSERT INTO ft VALUES "
                "(1,10,20,'aa',true,1.5,1.50,'2020-01-01'),"
                "(2,NULL,20,'bb',false,NULL,2.25,NULL),"
                "(3,10,NULL,NULL,NULL,-0.5,NULL,'2021-06-15'),"
                "(4,-3,0,'dd',true,0.0,0.00,'2019-12-31'),"
                "(5,0,7,'',false,2.5,10,'2022-02-28')"))
  ;; fu.k contains a NULL, which is what makes the IN/NOT IN rules bite.
  (exec! c "CREATE TABLE fu (id int, k int, v text)")
  (exec! c "INSERT INTO fu VALUES (1,10,'ten'),(2,NULL,'null-k'),(3,99,NULL)"))

(deftest in-subquery-null-semantics
  (with-open [c (jdbc)]
    (seed! c)
    ;; A NULL from a SUBQUERY arrives as the `:__null__` sentinel, not nil,
    ;; and only nil was being filtered out of the membership set. So the
    ;; sentinel stayed IN the set and a NULL column matched it.
    (is (= ["1" "3"] (col c 1 "SELECT id FROM ft WHERE i IN (SELECT k FROM fu) ORDER BY id"))
        "row 2 has i IS NULL: UNKNOWN, not a match against the subquery's NULL")
    ;; And the "NULL in the list makes NOT IN empty" rule never fired for a
    ;; subquery, for the same reason.
    (is (= [] (col c 1 "SELECT id FROM ft WHERE i NOT IN (SELECT k FROM fu) ORDER BY id"))
        "the subquery yields a NULL, so NOT IN is UNKNOWN for every row")
    (is (= "0" (one c "SELECT count(*) FROM ft WHERE i NOT IN (SELECT k FROM fu)")))
    (testing "with the NULL excluded from the subquery, both behave normally"
      (is (= ["1" "3"]
             (col c 1 (str "SELECT id FROM ft WHERE i IN "
                           "(SELECT k FROM fu WHERE k IS NOT NULL) ORDER BY id"))))
      (is (= ["4" "5"]
             (col c 1 (str "SELECT id FROM ft WHERE i NOT IN "
                           "(SELECT k FROM fu WHERE k IS NOT NULL) ORDER BY id")))
          "the i-IS-NULL row is still excluded -- NULL NOT IN (…) is UNKNOWN"))))

(deftest set-operations-honour-order-by
  (with-open [c (jdbc)]
    (seed! c)
    ;; The trailing ORDER BY belongs to the whole set operation and was
    ;; being dropped. EXCEPT was the visible case -- clojure.set/difference
    ;; has no order at all -- but UNION was wrong too: it kept first-seen
    ;; order, which happened to put the NULL before 99.
    (is (= ["1" "2" "3" "4" "5" "10" "99" nil]
           (col c 1 "SELECT id FROM ft UNION SELECT k FROM fu ORDER BY 1"))
        "NULL sorts last for ASC")
    (is (= ["1" "2" "3" "4" "5" "10" "99" nil]
           (col c 1 "SELECT id FROM ft UNION ALL SELECT k FROM fu ORDER BY 1")))
    (is (= ["1" "2" "3" "4" "5"]
           (col c 1 "SELECT id FROM ft EXCEPT SELECT k FROM fu ORDER BY 1")))
    (is (= [nil "99" "10" "5" "4" "3" "2" "1"]
           (col c 1 "SELECT id FROM ft UNION SELECT k FROM fu ORDER BY 1 DESC"))
        "DESC was ignored outright; NULL sorts first for DESC")))

(deftest window-functions-see-the-whole-result-not-the-limited-one
  (with-open [c (jdbc)]
    (seed! c)
    ;; PostgreSQL evaluates a window over the full result and only then
    ;; applies OFFSET/LIMIT. We trimmed first, so the window aggregated
    ;; over the surviving rows: `sum(i) OVER () … LIMIT 2` answered 10
    ;; (the first two rows) instead of 17.
    (is (= ["17" "17"] (col c 2 "SELECT id, sum(i) OVER () AS c FROM ft ORDER BY id LIMIT 2")))
    (is (= ["17" "17"]
           (col c 2 "SELECT id, sum(i) OVER () AS c FROM ft ORDER BY id LIMIT 2 OFFSET 1")))
    (is (= ["1" "2" "3"]
           (col c 2 (str "SELECT id, row_number() OVER (ORDER BY id) AS c "
                         "FROM ft ORDER BY id LIMIT 3"))))
    (testing "unlimited is unchanged"
      (is (= ["17" "17" "17" "17" "17"]
             (col c 2 "SELECT id, sum(i) OVER () AS c FROM ft ORDER BY id"))))))

(deftest window-aggregates
  (with-open [c (jdbc)]
    (seed! c)
    (testing "an EMPTY OVER () is still a window, not a plain aggregate"
      ;; is-window? was inferred from the presence of a partition / order /
      ;; frame, so `OVER ()` -- which has none -- fell through to the
      ;; aggregate path and raised "must appear in the GROUP BY clause".
      ;; JSqlParser's AnalyticType says OVER vs FILTER_ONLY vs WITHIN_GROUP.
      (is (= ["17" "17" "17" "17" "17"]
             (col c 2 "SELECT id, sum(i) OVER () AS c FROM ft ORDER BY id"))))
    (testing "COUNT(*) has no argument column"
      ;; AllColumns is not a value expression; translating it put a non-var
      ;; into :find and Datahike rejected the query outright.
      (is (= ["5" "5" "5" "5" "5"]
             (col c 2 "SELECT id, count(*) OVER () AS c FROM ft ORDER BY id")))
      (is (= ["2" "2" "1" "2" "2"]
             (col c 2 "SELECT id, count(*) OVER (PARTITION BY b) AS c FROM ft ORDER BY id"))
          "the b IS NULL row is its own partition")
      (is (= ["4" "4" "4" "4" "4"]
             (col c 2 "SELECT id, count(i) OVER () AS c FROM ft ORDER BY id"))
          "COUNT(col) still ignores NULLs"))
    (testing "AVG over int / numeric is NUMERIC, as it is for the plain aggregate"
      (is (= "4.2500000000000000" (one c "SELECT avg(i) OVER () AS c FROM ft ORDER BY id LIMIT 1")))
      (is (= "3.4375000000000000" (one c "SELECT avg(n) OVER () AS c FROM ft ORDER BY id LIMIT 1")))
      (is (= "0.875" (one c "SELECT avg(f) OVER () AS c FROM ft ORDER BY id LIMIT 1"))
          "over float8 it stays a double"))
    (testing "MIN/MAX are defined over any ordered type, not just numbers"
      (is (= "2022-02-28" (one c "SELECT max(d) OVER () AS c FROM ft ORDER BY id LIMIT 1")))
      (is (= "dd" (one c "SELECT max(s) OVER () AS c FROM ft ORDER BY id LIMIT 1"))))
    (testing "ranking and offset functions are unaffected"
      (is (= ["1" "2" "3" "4" "5"]
             (col c 2 "SELECT id, row_number() OVER (ORDER BY id) AS c FROM ft ORDER BY id")))
      (is (= [nil "10" nil "10" "-3"]
             (col c 2 "SELECT id, lag(i) OVER (ORDER BY id) AS c FROM ft ORDER BY id"))))))
