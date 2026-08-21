(ns datahike.test.pg-window-frames-test
  "The window engine: frames, peers, and the aggregate that computes them.

   Every expectation here is a PostgreSQL 17 oracle's, and every one of
   them was WRONG before -- silently, as a value rather than an error:

     sum(v) OVER (ORDER BY k)          ran over ROWS, not the RANGE peers
     min(v) OVER (ORDER BY id)         NULL on every row
     count(*) OVER (ORDER BY id)       raised NullPointerException
     avg(v) OVER (ORDER BY id)         a double where PG answers NUMERIC
     sum(numeric) OVER ()              lost the column's scale
     ROWS UNBOUNDED PRECEDING          read as the whole partition
     lead(v, 2, -1)                    ran as lead(v)
     ntile(2)                          could not run at all
     percent_rank / cume_dist          NULL for every row
     first_value / last_value          NULL for every row
     array_agg / string_agg OVER (…)   NULL for every row
     agg(x) FILTER (…) OVER (…)        the filter was ignored
     OVER w  (a WINDOW clause)         no partition, no order, no frame
     SELECT … FROM (SELECT … OVER …)   failed to run at all"
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn wf-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"wf" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each wf-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/wf?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

;; k is deliberately TIED (1,1,2,2,3): the peer groups are what separate a
;; RANGE frame from a ROWS frame, and the default frame is RANGE.
(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE wp (id int, g text, k int, v int, n numeric(8,2), r real, s text)")
  (exec! c (str "INSERT INTO wp VALUES "
                "(1,'a',1,10,1.50,1.1,'x'),"
                "(2,'a',1,20,2.25,2.2,'y'),"
                "(3,'a',2,30,3.75,3.3,NULL),"
                "(4,'b',2,40,4.00,4.4,'w'),"
                "(5,'b',3,NULL,NULL,NULL,'z')")))

(deftest default-frame-is-range-over-peers
  (with-open [c (jdbc)]
    (seed! c)
    ;; ORDER BY with no frame clause means RANGE UNBOUNDED PRECEDING AND
    ;; CURRENT ROW: the frame ends at the last PEER, so tied rows share a
    ;; total. Read as ROWS -- as it was -- every tie is wrong.
    (is (= ["30" "30" "100" "100" "100"]
           (col c 2 "SELECT id, sum(v) OVER (ORDER BY k) FROM wp ORDER BY id")))
    (is (= ["2" "2" "4" "4" "5"]
           (col c 2 "SELECT id, count(*) OVER (ORDER BY k) FROM wp ORDER BY id")))
    ;; ROWS asks for exactly the row-by-row running total instead.
    (is (= ["10" "30" "60" "100" "100"]
           (col c 2 (str "SELECT id, sum(v) OVER (ORDER BY k, id ROWS UNBOUNDED PRECEDING) "
                         "FROM wp ORDER BY id")))
        "a lone start bound ends at CURRENT ROW -- not the whole partition")))

(deftest frame-bound-combinations
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["100" "100" "100" "100" "100"]
           (col c 2 (str "SELECT id, sum(v) OVER (ORDER BY id RANGE BETWEEN UNBOUNDED "
                         "PRECEDING AND UNBOUNDED FOLLOWING) FROM wp ORDER BY id"))))
    (is (= ["100" "90" "70" "40" nil]
           (col c 2 (str "SELECT id, sum(v) OVER (ORDER BY id ROWS BETWEEN CURRENT ROW "
                         "AND UNBOUNDED FOLLOWING) FROM wp ORDER BY id"))))
    (is (= ["50" "70" "40" nil nil]
           (col c 2 (str "SELECT id, sum(v) OVER (ORDER BY id ROWS BETWEEN 1 FOLLOWING "
                         "AND 2 FOLLOWING) FROM wp ORDER BY id"))))
    (is (= [nil "10" "30" "50" "70"]
           (col c 2 (str "SELECT id, sum(v) OVER (ORDER BY id ROWS BETWEEN 2 PRECEDING "
                         "AND 1 PRECEDING) FROM wp ORDER BY id")))
        "an end bound is exclusive: 2 PRECEDING .. 1 PRECEDING is two rows")
    (is (= [nil "{10}" "{10,20}" "{20,30}" "{30,40}"]
           (col c 2 (str "SELECT id, array_agg(v) OVER (ORDER BY id ROWS BETWEEN 2 "
                         "PRECEDING AND 1 PRECEDING) FROM wp ORDER BY id")))
        "an EMPTY frame is NULL, not an empty array")
    ;; RANGE with a value offset frames by the sort KEY, not by row count.
    (is (= ["100" "100" "100" "100" "70"]
           (col c 2 (str "SELECT id, sum(v) OVER (ORDER BY k RANGE BETWEEN 1 PRECEDING "
                         "AND 1 FOLLOWING) FROM wp ORDER BY id"))))))

(deftest min-max-over-a-frame
  (with-open [c (jdbc)]
    (seed! c)
    ;; Both answered NULL for every row of any frame but the whole
    ;; partition -- the private implementation gave up on them.
    (is (= ["10" "10" "10" "10" "10"]
           (col c 2 "SELECT id, min(v) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= ["10" "20" "30" "40" "40"]
           (col c 2 "SELECT id, max(v) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= ["x" "x" "x" "w" "w"]
           (col c 2 "SELECT id, min(s) OVER (ORDER BY id) FROM wp ORDER BY id"))
        "MIN/MAX are defined over any ordered type, text included")))

(deftest aggregate-types-match-the-plain-aggregate
  (with-open [c (jdbc)]
    (seed! c)
    ;; PG's SUM(numeric) keeps the scale and AVG(int) is NUMERIC. The
    ;; window engine accumulated in double and answered neither.
    (is (= ["11.50" "11.50" "11.50" "11.50" "11.50"]
           (col c 2 "SELECT id, sum(n) OVER () FROM wp ORDER BY id")))
    (is (= ["1.50" "3.75" "7.50" "11.50" "11.50"]
           (col c 2 "SELECT id, sum(n) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= ["11" "11" "11" "11" "11"]
           (col c 2 "SELECT id, sum(r) OVER () FROM wp ORDER BY id"))
        "sum(real) accumulates AT float4 precision, as float4pl does")
    (is (= ["10.0000000000000000" "15.0000000000000000" "20.0000000000000000"
            "25.0000000000000000" "25.0000000000000000"]
           (col c 2 "SELECT id, avg(v) OVER (ORDER BY id) FROM wp ORDER BY id"))
        "a RUNNING avg is the same numeric answer as the whole-frame one")
    (is (= ["1.8750000000000000" "1.8750000000000000" "2.8750000000000000"
            "2.8750000000000000" "2.8750000000000000"]
           (col c 2 "SELECT id, avg(n) OVER (ORDER BY k) FROM wp ORDER BY id")))))

(deftest collection-aggregates-over-a-window
  (with-open [c (jdbc)]
    (seed! c)
    ;; Every aggregate the engine's private `case` did not name answered
    ;; NULL for every row.
    (is (= ["{10}" "{10,20}" "{10,20,30}" "{10,20,30,40}" "{10,20,30,40,NULL}"]
           (col c 2 "SELECT id, array_agg(v) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= ["x" "x,y" "x,y" "x,y,w" "x,y,w,z"]
           (col c 2 "SELECT id, string_agg(s,',') OVER (ORDER BY id) FROM wp ORDER BY id"))
        "string_agg's delimiter is a CONSTANT -- it must not reach :find")))

(deftest ranking-and-navigation
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["1" "1" "3" "3" "5"]
           (col c 2 "SELECT id, rank() OVER (ORDER BY k) FROM wp ORDER BY id")))
    (is (= ["0" "0" "0.5" "0.5" "1"]
           (col c 2 "SELECT id, percent_rank() OVER (ORDER BY k) FROM wp ORDER BY id")))
    (is (= ["0.4" "0.4" "0.8" "0.8" "1"]
           (col c 2 "SELECT id, cume_dist() OVER (ORDER BY k) FROM wp ORDER BY id")))
    ;; PG fills the LARGER ntile buckets first: 5 rows in 2 buckets is 3/2.
    (is (= ["1" "1" "1" "2" "2"]
           (col c 2 "SELECT id, ntile(2) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= ["30" "40" nil nil nil]
           (col c 2 "SELECT id, lead(v,2,NULL) OVER (ORDER BY id) FROM wp ORDER BY id"))
        "the offset and default arguments were parsed and then dropped")
    (is (= ["-1" "-1" "-1" "10" "20"]
           (col c 2 "SELECT id, lag(v,3,-1) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= [nil "20" "20" "20" "20"]
           (col c 2 "SELECT id, nth_value(v,2) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= ["10" "10" "10" "10" "10"]
           (col c 2 "SELECT id, first_value(v) OVER (ORDER BY id) FROM wp ORDER BY id")))
    (is (= ["10" "20" "30" "40" nil]
           (col c 2 "SELECT id, last_value(v) OVER (ORDER BY id) FROM wp ORDER BY id"))
        "last_value follows the FRAME, which by default ends at the current row")))

(deftest order-by-nulls-placement
  (with-open [c (jdbc)]
    (seed! c)
    ;; NULLS FIRST / LAST was dropped from a window's ORDER BY, and the
    ;; comparator pinned nulls last in BOTH directions.
    (is (= ["2" "3" "4" "5" "1"]
           (col c 2 "SELECT id, rank() OVER (ORDER BY v NULLS FIRST) FROM wp ORDER BY id")))
    (is (= ["4" "3" "2" "1" "5"]
           (col c 2 "SELECT id, rank() OVER (ORDER BY v DESC NULLS LAST) FROM wp ORDER BY id")))
    (is (= ["5" "4" "3" "2" "1"]
           (col c 2 "SELECT id, rank() OVER (ORDER BY v DESC) FROM wp ORDER BY id"))
        "PG's DESC default is NULLS FIRST")))

(deftest named-window-clause
  (with-open [c (jdbc)]
    (seed! c)
    ;; `OVER w` was never resolved against the WINDOW clause, so the window
    ;; had no partition, no order and no frame at all.
    (is (= ["10" "30" "60" "40" "40"]
           (col c 2 (str "SELECT id, sum(v) OVER w FROM wp "
                         "WINDOW w AS (PARTITION BY g ORDER BY id) ORDER BY id"))))
    (is (= ["1" "2" "3" "1" "2"]
           (col c 2 (str "SELECT id, count(*) OVER w FROM wp "
                         "WINDOW w AS (PARTITION BY g ORDER BY id) ORDER BY id"))))))

(deftest filter-over-a-window
  (with-open [c (jdbc)]
    (seed! c)
    ;; The FILTER was parsed and dropped: the aggregate ran over the whole
    ;; frame.
    (is (= [nil nil "30" "70" "70"]
           (col c 2 (str "SELECT id, sum(v) FILTER (WHERE v > 20) OVER (ORDER BY id) "
                         "FROM wp ORDER BY id"))))
    (is (= ["0" "0" "1" "2" "2"]
           (col c 2 (str "SELECT id, count(*) FILTER (WHERE v > 20) OVER (ORDER BY id) "
                         "FROM wp ORDER BY id"))))
    ;; array_agg PRESERVES a NULL value, so an EXCLUDED row has to be
    ;; marked as something other than NULL.
    (is (= ["{10}" "{10,20}" "{10,20,30}" "{10,20,30,40}" "{10,20,30,40}"]
           (col c 2 (str "SELECT id, array_agg(v) FILTER (WHERE v IS NOT NULL) "
                         "OVER (ORDER BY id) FROM wp ORDER BY id"))))))

(deftest filter-on-a-plain-aggregate
  (with-open [c (jdbc)]
    (seed! c)
    ;; The FILTER aggregate dispatch was a four-name `case` defaulting to
    ;; filter-sum, so every other aggregate silently computed a SUM.
    ;; Sorted: a plain (non-window) aggregate's input order is the scan
    ;; order, Datalog's here and PostgreSQL's there. What matters is WHICH
    ;; rows the filter admitted.
    (is (= "1234"
           (apply str (sort (re-seq #"[0-9]"
                                    (first (col c 1 (str "SELECT array_agg(id) FILTER "
                                                         "(WHERE v IS NOT NULL) FROM wp"))))))))
    (is (= "wxy"
           (apply str (sort (re-seq #"[a-z]"
                                    (first (col c 1 (str "SELECT string_agg(s,',') FILTER "
                                                         "(WHERE v IS NOT NULL) FROM wp"))))))))
    ;; count(x) FILTER (WHERE p) counts rows where p AND x IS NOT NULL --
    ;; the old shape emitted 1 for every passing row and counted the NULLs.
    (is (= ["4"] (col c 1 "SELECT count(v) FILTER (WHERE id > 0) FROM wp")))
    (is (= ["5"] (col c 1 "SELECT count(*) FILTER (WHERE id > 0) FROM wp")))))

(deftest window-inside-a-derived-table
  (with-open [c (jdbc)]
    (seed! c)
    ;; The window pass only ever ran at the top level, so the standard
    ;; top-N-per-group idiom did not merely lose the column -- the hidden
    ;; __win_* helper columns reached the materialiser and the query died.
    (is (= ["1" "4"]
           (col c 1 (str "SELECT id FROM (SELECT id, row_number() OVER "
                         "(PARTITION BY g ORDER BY id) rn FROM wp) t "
                         "WHERE rn = 1 ORDER BY id"))))
    (is (= ["10" "30" "60" "100" "100"]
           (col c 2 (str "WITH r AS (SELECT id, sum(v) OVER (ORDER BY id) s FROM wp) "
                         "SELECT id, s FROM r ORDER BY id"))))
    (is (= ["5"] (col c 1 "SELECT count(*) FROM (SELECT sum(v) OVER (ORDER BY id) FROM wp) t")))))

(deftest unknown-window-function-raises
  (with-open [c (jdbc)]
    (seed! c)
    ;; Not silently NULL for every row.
    (testing "an unimplemented window aggregate says so"
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException #"(?i)not supported"
           (col c 2 "SELECT id, bool_and(v > 5) OVER (ORDER BY id) FROM wp ORDER BY id"))))))
