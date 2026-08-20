(ns datahike.test.pg-fuzz-findings-test
  "Divergences found by differential fuzzing against a PostgreSQL 17 oracle.

   A generator emits queries over a fixed NULL-heavy schema -- projections,
   predicates, aggregates, CASE, ORDER BY, casts -- and every result is
   compared against the oracle's. 324 generated queries produced 58
   disagreements, which collapsed to the handful of root causes pinned
   here. Two of them were regressions introduced by the NULL-semantics
   work in #73 and #74, which is the argument for the technique: the
   hand-written batteries that drove those PRs did not catch either.

   Expectations are the oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn fz-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"fz" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fz-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/fz?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col
  "Column `n` of every row, as text; nil = SQL NULL."
  [^Connection c n sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (loop [acc []]
      (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv (fn [^long ix] (.getString rs ix)) (range 1 (inc n)))))
          acc)))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ft (id int, i int, j int, s text, b boolean, f float8, n numeric, d date)")
  (exec! c (str "INSERT INTO ft VALUES "
                "(1,10,20,'aa',true,1.5,1.50,'2020-01-01'),"
                "(2,NULL,20,'bb',false,NULL,2.25,NULL),"
                "(3,10,NULL,NULL,NULL,-0.5,NULL,'2021-06-15'),"
                "(4,-3,0,'dd',true,0.0,0.00,'2019-12-31'),"
                "(5,0,7,'',false,2.5,10,'2022-02-28')")))

(deftest case-with-a-null-branch-keeps-its-row
  (with-open [c (jdbc)]
    (seed! c)
    ;; REGRESSION from #74. The CASE fn returned Clojure nil for a NULL
    ;; branch value, and a datalog binding that yields nil FILTERS THE ROW
    ;; -- so this returned four rows where PostgreSQL returns five. Before
    ;; #74 it returned five rows with the WRONG value (the ELSE), which is
    ;; why the hand-written battery never flagged it.
    (is (= ["1" "2" "3" "4" "5"]
           (col c 1 "SELECT id, CASE WHEN id=1 THEN NULL ELSE 2 END AS c FROM ft ORDER BY id")))
    (is (= [nil "2" "2" "2" "2"]
           (col c 2 "SELECT id, CASE WHEN id=1 THEN NULL ELSE 2 END AS c FROM ft ORDER BY id")))
    (is (= ["9" "2" "2" "2" "2"]
           (col c 2 (str "SELECT id, COALESCE(CASE WHEN id=1 THEN NULL ELSE 2 END, 9) AS c "
                         "FROM ft ORDER BY id")))
        "the NULL must be a real SQL NULL, visible to COALESCE")
    (testing "the properties #74 added still hold"
      (is (= "f" (one c "SELECT CASE WHEN true THEN false ELSE true END")))
      (is (= "1" (one c "SELECT CASE WHEN 1=1 THEN 1 ELSE 2/0 END"))
          "CASE short-circuits: the untaken ELSE must not be evaluated"))))

(deftest not-over-a-variable-free-predicate
  (with-open [c (jdbc)]
    (seed! c)
    ;; A datalog `not` is a NOT-JOIN and needs variables to join on, so a
    ;; ground negation raised "Join variables should not be empty".
    ;; #73 widened this from the standalone `NOT (1 = 1)` to any OR or AND
    ;; containing a constant-only comparison, by splitting the operands
    ;; into separate negations. Ground predicates are now decided at
    ;; translate time instead.
    (is (= [] (col c 1 "SELECT id FROM ft WHERE NOT (1 = 1) ORDER BY id")))
    (is (= ["1" "2" "3" "4" "5"] (col c 1 "SELECT id FROM ft WHERE NOT (1 = 2) ORDER BY id")))
    (is (= [] (col c 1 "SELECT id FROM ft WHERE NOT (i = 10 OR 2.5 <> 0) ORDER BY id")))
    (is (= ["4" "5"] (col c 1 "SELECT id FROM ft WHERE NOT (i = 10 OR 1 = 2) ORDER BY id")))
    (is (= ["1" "2" "3" "4" "5"]
           (col c 1 "SELECT id FROM ft WHERE NOT (0 = -1 AND 1 <= 2.5) ORDER BY id"))
        "a ground CONJUNCTION under NOT, which takes the sql-may? fast path")
    (is (= [] (col c 1 "SELECT id FROM ft WHERE NOT (1 = 1 AND 2 = 2) ORDER BY id")))
    (is (= ["4" "5"] (col c 1 "SELECT id FROM ft WHERE NOT (i = 10 AND 1 = 1) ORDER BY id"))
        "mixed ground and variable operands")))

(deftest aggregate-over-a-constant-argument
  (with-open [c (jdbc)]
    (seed! c)
    ;; `(count 1)` reached Datahike's find-spec parser as a Constant, which
    ;; has no IFindVars implementation -- a raw protocol error. The constant
    ;; is now bound per row, so count/sum see one value per row.
    (is (= "5" (one c "SELECT count(1) FROM ft")))
    (is (= "5" (one c "SELECT count(*) FROM ft")))
    (is (= "5" (one c "SELECT sum(1) FROM ft")))
    (is (= "1" (one c "SELECT min(1) FROM ft")))
    (is (= "-1" (one c "SELECT max(-1) FROM ft")))
    (is (= "2.5000000000000000" (one c "SELECT avg(2.5) FROM ft")))
    (is (= "0" (one c "SELECT count(NULL) FROM ft")) "count ignores NULLs")
    (testing "with no FROM at all -- one row, no entity to vary over"
      (is (= "1" (one c "SELECT count(1)")))
      (is (= "2" (one c "SELECT sum(2)"))))
    (testing "in a GROUP BY and in a scalar subquery"
      (is (= [["20" "2"] ["0" "1"] ["7" "1"] [nil "1"]]
             (rows c "SELECT j, count(1) FROM ft GROUP BY 1 ORDER BY count(1) DESC, j")))
      (is (= ["1" "2" "3" "4" "5"]
             (col c 1 "SELECT id FROM ft WHERE 1 = (SELECT min(1) FROM ft) ORDER BY id"))))
    (testing "column aggregates are unaffected"
      (is (= "4" (one c "SELECT count(i) FROM ft")))
      (is (= "17" (one c "SELECT sum(i) FROM ft")))
      (is (= "3" (one c "SELECT count(DISTINCT i) FROM ft")))
      (is (= "34" (one c "SELECT sum(i*2) FROM ft"))))))

(deftest order-by-nulls-first-and-last
  (with-open [c (jdbc)]
    (seed! c)
    ;; PostgreSQL's DEFAULT is NULLS LAST for ASC and NULLS FIRST for DESC
    ;; (NULL sorts as the largest value) -- which we already did. An
    ;; EXPLICIT clause was parsed and then DISCARDED, so `NULLS FIRST` on an
    ;; ASC sort silently returned the default order.
    (is (= ["2" "4" "5" "1" "3"] (col c 1 "SELECT id FROM ft ORDER BY i ASC NULLS FIRST, id")))
    (is (= ["4" "5" "1" "3" "2"] (col c 1 "SELECT id FROM ft ORDER BY i ASC NULLS LAST, id")))
    (is (= ["2" "1" "3" "5" "4"] (col c 1 "SELECT id FROM ft ORDER BY i DESC NULLS FIRST, id")))
    (is (= ["1" "3" "5" "4" "2"] (col c 1 "SELECT id FROM ft ORDER BY i DESC NULLS LAST, id")))
    (testing "the defaults are unchanged"
      (is (= ["4" "5" "1" "3" "2"] (col c 1 "SELECT id FROM ft ORDER BY i ASC, id")))
      (is (= ["2" "1" "3" "5" "4"] (col c 1 "SELECT id FROM ft ORDER BY i DESC, id"))))
    (testing "per-key, and on an ordinal"
      (is (= ["3" "4" "5" "1" "2"]
             (col c 1 "SELECT id FROM ft ORDER BY j NULLS FIRST, i NULLS LAST, id")))
      (is (= ["2" "4" "5" "1" "3"] (col c 1 "SELECT id, i FROM ft ORDER BY 2 NULLS FIRST, 1"))))))

(deftest integer-to-boolean-cast
  (with-open [c (jdbc)]
    (seed! c)
    ;; PostgreSQL has an int -> bool cast (bool.c int4_bool): zero is false,
    ;; anything else true. We stringified the number and handed it to the
    ;; TEXT parser, which accepts only the exact tokens '1' and '0'.
    (is (= ["t" "t" nil "f" "t"] (col c 2 "SELECT id, j::bool AS c FROM ft ORDER BY id")))
    (is (= "t" (one c "SELECT 20::bool")))
    (is (= "f" (one c "SELECT 0::bool")))
    (is (= "t" (one c "SELECT (-1)::bool"))
        "a parenthesised negative literal must stay a NUMBER through the cast")
    (testing "PostgreSQL still rejects the TEXT '-1', so this must not become lenient"
      (is (thrown? java.sql.SQLException (one c "SELECT '-1'::bool"))))
    (testing "other targets for a parenthesised signed literal"
      (is (= "-1" (one c "SELECT (-1)::int")))
      (is (= "-1.5" (one c "SELECT (-1.5)::numeric")))
      (is (= "-3" (one c "SELECT (-3)::int2"))))))

(deftest having-with-an-inequality
  (with-open [c (jdbc)]
    (seed! c)
    ;; `translate-having-expr` emits Clojure-style operator symbols
    ;; (`not=`), but the evaluator matched on the SQL spellings `'<>` /
    ;; `'!=` -- so `<>` fell through to the `:else =` default and HAVING
    ;; returned exactly the COMPLEMENT of the right groups.
    (is (= [["f" "2"] ["t" "2"]]
           (rows c "SELECT b AS g, count(*) AS n FROM ft GROUP BY 1 HAVING count(*) <> 1 ORDER BY 1")))
    (is (= [["20" "2"]]
           (rows c "SELECT j AS g, count(*) AS n FROM ft GROUP BY 1 HAVING count(*) <> 1 ORDER BY 1")))
    (testing "the operators that already worked"
      (is (= [["f" "2"] ["t" "2"]]
             (rows c "SELECT b AS g, count(*) AS n FROM ft GROUP BY 1 HAVING count(*) = 2 ORDER BY 1")))
      (is (= [["f" "2"] ["t" "2"]]
             (rows c "SELECT b AS g, count(*) AS n FROM ft GROUP BY 1 HAVING count(*) > 1 ORDER BY 1"))))))
