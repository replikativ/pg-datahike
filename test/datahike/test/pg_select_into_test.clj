(ns datahike.test.pg-select-into-test
  "`SELECT … INTO t` and the command tag of `CREATE TABLE … AS`.

   PostgreSQL has two spellings for the same statement:

     CREATE TABLE t AS SELECT …
     SELECT … INTO t          -- and INTO TABLE / INTO TEMP t

   JSqlParser has no INTO clause in a SELECT, so the second spelling
   never parsed. A token rewrite lifts the clause to the front, which
   turns it into the CREATE TABLE AS it means.

   Both spellings report the row count they stored (`SELECT 3`), not
   `CREATE TABLE` -- we reported the latter, and therefore a row count
   of zero, for every CTAS. When IF NOT EXISTS skips the work the tag is
   `CREATE TABLE AS`.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.rewrite :as rw])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec ^PgWireServer$QueryResult [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (mapv vec (.rows r))))

(defn- release! [{:keys [conn cfg]}]
  (d/release conn)
  (d/delete-database cfg))

;; ============================================================================
;; The rewrite itself
;; ============================================================================

(defn- pre [sql] (rw/rewrite sql rw/default-rules))

(deftest select-into-becomes-create-table-as
  (is (= "CREATE TABLE t AS SELECT x  FROM s" (pre "SELECT x INTO t FROM s")))
  (is (= "CREATE TABLE t AS SELECT x  FROM s" (pre "SELECT x INTO TABLE t FROM s")))
  (testing "the persistence words are kept -- INTO TEMP t is a temporary table"
    (is (= "CREATE TEMP TABLE t AS SELECT x  FROM s" (pre "SELECT x INTO TEMP t FROM s")))
    (is (= "CREATE UNLOGGED TABLE t AS SELECT x  FROM s"
           (pre "SELECT x INTO UNLOGGED t FROM s"))))
  (testing "a SELECT with no INTO, and an INTO that is not the outer one"
    (is (= "SELECT x FROM s" (pre "SELECT x FROM s")))
    (is (= "SELECT x FROM (SELECT y INTO_LIKE FROM s) q"
           (pre "SELECT x FROM (SELECT y INTO_LIKE FROM s) q")))))

;; ============================================================================
;; End to end
;; ============================================================================

(deftest select-into-creates-and-fills-the-table
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE src (i int, s text)"))))
      (is (nil? (.error (exec h "INSERT INTO src VALUES (1,'a'),(2,'b'),(3,'c')"))))
      (testing "SELECT … INTO t"
        (let [r (exec h "SELECT i, s INTO dst FROM src WHERE i < 3")]
          (is (nil? (.error r)))
          ;; PostgreSQL tags this with the row count, like the SELECT.
          (is (= "SELECT 2" (.commandTag r))))
        (is (= [["1" "a"] ["2" "b"]] (rows (exec h "SELECT i, s FROM dst ORDER BY i")))))
      (testing "SELECT … INTO TABLE t"
        (is (nil? (.error (exec h "SELECT i INTO TABLE dst2 FROM src"))))
        (is (= [["1"] ["2"] ["3"]] (rows (exec h "SELECT i FROM dst2 ORDER BY i")))))
      (testing "a constant SELECT with no FROM"
        (let [r (exec h "SELECT 9 AS a INTO dst3")]
          (is (nil? (.error r)))
          (is (= "SELECT 1" (.commandTag r))))
        (is (= [["9"]] (rows (exec h "SELECT a FROM dst3")))))
      (finally (release! h)))))

(deftest create-table-as-reports-the-row-count
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE src (i int)"))))
      (is (nil? (.error (exec h "INSERT INTO src VALUES (1),(2),(3)"))))
      (let [r (exec h "CREATE TABLE c1 AS SELECT i FROM src")]
        (is (= "SELECT 3" (.commandTag r))))
      (let [r (exec h "CREATE TABLE c2 AS SELECT i FROM src WHERE i > 9")]
        (is (= "SELECT 0" (.commandTag r))))
      (testing "IF NOT EXISTS on an existing table says CREATE TABLE AS"
        (let [r (exec h "CREATE TABLE IF NOT EXISTS c1 AS SELECT i FROM src")]
          (is (nil? (.error r)))
          (is (= "CREATE TABLE AS" (.commandTag r)))))
      (testing "a plain CREATE TABLE is unchanged"
        (is (= "CREATE TABLE" (.commandTag (exec h "CREATE TABLE c3 (i int)")))))
      (finally (release! h)))))

(deftest ctas-takes-a-column-list-and-a-with-data-tail
  ;; `create_as_target` in PostgreSQL's grammar allows a column list,
  ;; and every CTAS may end `WITH DATA` or `WITH NO DATA`. JSqlParser
  ;; has neither, so `… WITH DATA` was a parse error, and a column list
  ;; reached the schema transaction as a column with no type and came
  ;; back as datahike's "Update not supported for these schema
  ;; attributes" -- an internal message for a statement PostgreSQL just
  ;; runs. It is the same grammar as CREATE MATERIALIZED VIEW's and is
  ;; now read by the same function.
  ;;
  ;; Expectations, including the command tags, are a PostgreSQL 17
  ;; oracle's.
  (let [h (fresh-handler)]
    (try
      (testing "WITH DATA is the default, and says so with the row count"
        (let [r (exec h "CREATE TABLE c1 (a) AS SELECT generate_series(1,3) WITH DATA")]
          (is (nil? (.error r)))
          (is (= "SELECT 3" (.commandTag r))))
        (is (= [["1"] ["2"] ["3"]] (rows (exec h "SELECT * FROM c1 ORDER BY a")))))
      (testing "the column list renames the query's output"
        (let [r (exec h "CREATE TABLE c2 (p, q) AS SELECT 1, 2")]
          (is (nil? (.error r)))
          (is (= "SELECT 1" (.commandTag r))))
        (is (= [["1" "2"]] (rows (exec h "SELECT p, q FROM c2")))))
      (testing "WITH NO DATA creates the table empty, and keeps the DDL tag"
        (let [r (exec h "CREATE TABLE c3 AS SELECT 1 AS x WITH NO DATA")]
          (is (nil? (.error r)))
          (is (= "CREATE TABLE AS" (.commandTag r))))
        (is (= [["0"]] (rows (exec h "SELECT count(*) FROM c3")))))
      (testing "a plain CTAS is untouched by any of this"
        (let [r (exec h "CREATE TABLE c4 AS SELECT 5 AS z")]
          (is (= "SELECT 1" (.commandTag r))))
        (is (= [["5"]] (rows (exec h "SELECT * FROM c4")))))
      (finally (release! h)))))

(deftest ctas-stores-the-rows-in-the-selects-order
  ;; `select_implicit`'s whole remaining gap, and a silent wrong answer
  ;; of a kind nothing downstream can repair: the ORDER BY decides what
  ;; the new table CONTAINS, because a later unordered scan returns the
  ;; stored order.
  ;;
  ;;   SELECT count(b) INTO TABLE t FROM … GROUP BY x.b ORDER BY x.b;
  ;;   SELECT * FROM t;        -- PG: 1 2 3 4   ours: 3 4 1 2
  ;;
  ;; `translate-create-table-as` ran `d/q` and inserted the result
  ;; directly. `d/q` answers with a SET unless the ordering was pushed
  ;; into Datalog, so the rows went in in hash order.
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE src (a int, b int)"))))
      ;; Four groups of different sizes, inserted in an order that is
      ;; neither the group order nor the count order, so a table that
      ;; happens to come out right cannot do so by accident.
      (is (nil? (.error (exec h (str "INSERT INTO src VALUES "
                                     "(1,30),(2,30),(3,30),"
                                     "(4,10),"
                                     "(5,40),(6,40),(7,40),(8,40),"
                                     "(9,20),(10,20)")))))
      (testing "an aggregate INTO, ordered by a column not in the select list"
        (is (nil? (.error (exec h (str "SELECT count(a) AS c INTO TABLE agg "
                                       "FROM src GROUP BY b ORDER BY b")))))
        ;; b ascending is 10,20,30,40 -> counts 1,2,3,4.
        (is (= [["1"] ["2"] ["3"] ["4"]] (rows (exec h "SELECT c FROM agg")))
            "unordered scan returns what was stored, in the SELECT's order"))
      (testing "and descending, so the order cannot be the counts' own"
        (is (nil? (.error (exec h (str "SELECT count(a) AS c INTO TABLE agg_desc "
                                       "FROM src GROUP BY b ORDER BY b DESC")))))
        (is (= [["4"] ["3"] ["2"] ["1"]]
               (rows (exec h "SELECT c FROM agg_desc")))))
      (testing "a non-aggregate CTAS ordered by a visible column"
        (is (nil? (.error (exec h (str "CREATE TABLE ord AS SELECT a FROM src "
                                       "WHERE a <= 4 ORDER BY a DESC")))))
        (is (= [["4"] ["3"] ["2"] ["1"]] (rows (exec h "SELECT a FROM ord")))))
      (testing "ORDER BY with LIMIT trims AFTER ordering, as SQL says"
        (is (nil? (.error (exec h (str "CREATE TABLE top2 AS SELECT a FROM src "
                                       "ORDER BY a DESC LIMIT 2")))))
        (is (= [["10"] ["9"]] (rows (exec h "SELECT a FROM top2")))))
      (finally (release! h)))))

(deftest ctas-applies-offset-and-limit
  ;; Found by the ORDER BY test above, and a separate pre-existing bug:
  ;; the rows came out in the right order and then ALL of them were
  ;; stored. `translate-select` routes the user's LIMIT/OFFSET to one of
  ;; three mutually exclusive pairs and this path read only the one that
  ;; belongs to a set-returning projection, so every ordinary CTAS
  ;; ignored the trim. `exec-select` pushes `:limit`/`:offset` into
  ;; Datalog at execution; CTAS does not go through it.
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE src (a int)"))))
      (is (nil? (.error (exec h (str "INSERT INTO src VALUES "
                                     "(1),(2),(3),(4),(5),(6),(7),(8),(9),(10)")))))
      (testing "LIMIT with a server-side sort (:sql-limit)"
        (is (nil? (.error (exec h (str "CREATE TABLE t1 AS SELECT a FROM src "
                                       "ORDER BY a DESC LIMIT 3")))))
        (is (= [["10"] ["9"] ["8"]] (rows (exec h "SELECT a FROM t1")))))
      (testing "OFFSET and LIMIT together, still ordered"
        (is (nil? (.error (exec h (str "CREATE TABLE t2 AS SELECT a FROM src "
                                       "ORDER BY a OFFSET 4 LIMIT 2")))))
        (is (= [["5"] ["6"]] (rows (exec h "SELECT a FROM t2")))))
      (testing "LIMIT with no ORDER BY (:limit) still trims"
        ;; Which rows is unspecified without ORDER BY; how MANY is not.
        (let [r (exec h "CREATE TABLE t3 AS SELECT a FROM src LIMIT 4")]
          (is (nil? (.error r)))
          (is (= "SELECT 4" (.commandTag r))
              "the tag counts what was stored"))
        (is (= 4 (count (rows (exec h "SELECT a FROM t3"))))))
      (testing "OFFSET past the end stores nothing"
        (is (nil? (.error (exec h (str "CREATE TABLE t4 AS SELECT a FROM src "
                                       "ORDER BY a OFFSET 99")))))
        (is (= [] (rows (exec h "SELECT a FROM t4")))))
      (testing "FETCH … WITH TIES stores the ties, as the SELECT answers"
        ;; Refused while CTAS trimmed its own rows; the query now runs
        ;; through the SELECT pipeline, which knows the sort keys.
        (let [r (exec h (str "CREATE TABLE t5 AS SELECT a FROM src "
                             "ORDER BY a FETCH FIRST 2 ROWS WITH TIES"))]
          (is (nil? (.error r)))
          (is (= (count (rows (exec h "SELECT a FROM src ORDER BY a FETCH FIRST 2 ROWS WITH TIES")))
                 (count (rows (exec h "SELECT a FROM t5")))))))
      (finally (release! h)))))

(deftest a-stored-query-stores-what-the-query-answers
  ;; CREATE TABLE AS ran the query's Datalog itself, and INSERT … SELECT
  ;; re-applied a reduced copy of the passes after it, so both skipped
  ;; whatever came later. Every expectation is PostgreSQL 17's for the
  ;; same statements.
  (let [h (fresh-handler)
        q #(rows (exec h %))]
    (try
      (exec h "CREATE TABLE src (k int, v int)")
      (exec h "INSERT INTO src VALUES (1,10),(1,20),(1,30),(2,5),(3,7),(3,8),(3,9)")
      (testing "CREATE TABLE AS"
        (exec h "CREATE TABLE c1 AS SELECT k, count(*) AS n FROM src GROUP BY k HAVING count(*) > 2")
        (is (= [["1" "3"] ["3" "3"]] (q "SELECT k, n FROM c1 ORDER BY k")) "HAVING kept every group")
        (exec h "CREATE TABLE c2 AS SELECT count(*) AS n FROM src WHERE false")
        (is (= [["0"]] (q "SELECT n FROM c2")) "an aggregate over no input is one row")
        (exec h "CREATE TABLE c3 AS SELECT k, v, row_number() OVER (PARTITION BY k ORDER BY v) AS rn FROM src")
        (is (= [["1" "10" "1"] ["1" "20" "2"] ["1" "30" "3"] ["2" "5" "1"]
                ["3" "7" "1"] ["3" "8" "2"] ["3" "9" "3"]]
               (q "SELECT k, v, rn FROM c3 ORDER BY k, v"))
            "the window column was dropped")
        (exec h "CREATE TABLE c4 AS SELECT DISTINCT ON (k) k, v FROM src ORDER BY k, v DESC")
        (is (= [["1" "30"] ["2" "5"] ["3" "9"]] (q "SELECT k, v FROM c4 ORDER BY k")))
        (exec h "CREATE TABLE c5 AS SELECT k, sum(v) * 2 + 1 AS s FROM src GROUP BY k")
        (is (= [["1" "121"] ["2" "11"] ["3" "49"]] (q "SELECT k, s FROM c5 ORDER BY k"))
            "an expression over an aggregate was stored as `__compound_1`")
        (exec h "CREATE TABLE c7 AS SELECT k, v FROM src UNION SELECT 9, 9")
        (is (= [["8"]] (q "SELECT count(*) FROM c7")) "a set operation was refused")
        (exec h "CREATE TABLE c8 AS WITH t AS (SELECT k FROM src) SELECT DISTINCT k FROM t")
        (is (= [["1"] ["2"] ["3"]] (q "SELECT k FROM c8 ORDER BY k")) "a CTE was not resolved"))
      (testing "INSERT … SELECT"
        (exec h "CREATE TABLE i1 (k int, n bigint)")
        (exec h "INSERT INTO i1 SELECT k, count(*) FROM src GROUP BY k HAVING count(*) > 2")
        (is (= [["1" "3"] ["3" "3"]] (q "SELECT k, n FROM i1 ORDER BY k")))
        (exec h "CREATE TABLE i2 (n bigint)")
        (exec h "INSERT INTO i2 SELECT count(*) FROM src WHERE false")
        (is (= [["0"]] (q "SELECT n FROM i2")))
        (exec h "CREATE TABLE i3 (k int, v int, rn bigint)")
        (exec h "INSERT INTO i3 SELECT k, v, row_number() OVER (PARTITION BY k ORDER BY v) FROM src")
        (is (= [["1" "1"] ["2" "1"] ["3" "1"]]
               (q "SELECT k, min(rn) FROM i3 GROUP BY k ORDER BY k"))
            "this was `INSERT has more target columns than expressions`")
        (exec h "CREATE TABLE i4 (k int, v int)")
        (exec h "INSERT INTO i4 SELECT DISTINCT ON (k) k, v FROM src ORDER BY k, v DESC")
        (is (= [["1" "30"] ["2" "5"] ["3" "9"]] (q "SELECT k, v FROM i4 ORDER BY k")))
        (exec h "CREATE TABLE i5 (k int)")
        (exec h "INSERT INTO i5 SELECT k FROM src LIMIT 2 OFFSET 1")
        (is (= [["2"]] (q "SELECT count(*) FROM i5")) "an unsorted LIMIT inserted every row"))
      (testing "and the SELECT itself: ORDER BY an expression over aggregates, with LIMIT"
        ;; The limit was taken before that sort could run, so the kept
        ;; rows were the first two GROUPS, not the top two.
        (is (= [["1" "20"] ["3" "2"]]
               (q "SELECT k, max(v) - min(v) AS spread FROM src GROUP BY k ORDER BY spread DESC LIMIT 2"))))
      (finally (release! h)))))
