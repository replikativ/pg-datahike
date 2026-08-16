(ns datahike.test.pg-grouping-test
  "GROUP BY semantics.

   Datalog has no grouping clause: `{:find [?dept (count ?e)]}` groups by
   whatever NON-AGGREGATE elements appear in `:find`. The translator
   leaned on that, so the grouping key set was really the set of
   non-aggregate SELECT items and the GROUP BY clause itself was inert.
   `SELECT dept, count(*) … GROUP BY dept` only ever worked because
   projecting the key happened to put it in `:find`. Stop projecting it
   and the grouping vanished:

       SELECT count(*) FROM g GROUP BY dept   -- 5, one group
       SELECT sum(sal)  FROM g GROUP BY dept   -- one total

   where PostgreSQL answers per group. Grouping keys are now appended to
   `:find` as hidden trailing elements and stripped by the wire layer,
   the same mechanism the HAVING-only aggregates already used.

   The converse rule is PostgreSQL's 42803: a select item that is
   neither aggregated nor grouped is an ERROR. Datalog would happily
   group by it instead, so `SELECT sal, count(*) FROM g GROUP BY dept`
   returned five ungrouped rows — a wrong answer wearing the right
   shape. PostgreSQL licenses an ungrouped column when the table's
   PRIMARY KEY is covered by the grouping columns
   (check_functional_grouping in pg_constraint.c), which is what makes
   `SELECT * FROM g GROUP BY id` legal; a SQL PRIMARY KEY is stored as
   the namespace's `:db.unique/identity`, so covering it is the same
   test here.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)]
          (.execute *handler*
                    "CREATE TABLE g (id int PRIMARY KEY, dept text, sal int, reg text)")
          (.execute *handler*
                    (str "INSERT INTO g VALUES (1,'eng',10,'w'),(2,'eng',20,'w'),"
                         "(3,'ops',30,'e'),(4,'ops',40,'w'),(5,'ops',50,'e')"))
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- bag [sql] (frequencies (rows sql)))
(defn- state [sql]
  (try (.-sqlstate ^PgWireServer$QueryResult (run sql))
       (catch Exception e (:sqlstate (ex-data e)))))
(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (run sql))
       (catch Exception e (ex-message e))))

;; ---------------------------------------------------------------------------
;; The regression: a grouping key that is not projected
;; ---------------------------------------------------------------------------

(deftest group-by-without-projecting-the-key
  (testing "one row per group — the GROUP BY used to be inert here, so
            every one of these collapsed to a single group"
    (is (= {["2"] 1 ["3"] 1} (bag "SELECT count(*) FROM g GROUP BY dept")))
    (is (= {["30"] 1 ["120"] 1} (bag "SELECT sum(sal) FROM g GROUP BY dept")))
    (is (= {["20"] 1 ["50"] 1} (bag "SELECT max(sal) FROM g GROUP BY dept")))
    (is (= {["70"] 1 ["80"] 1} (bag "SELECT sum(sal) FROM g GROUP BY reg")))
    (is (= {["2"] 1 ["3"] 1} (bag "SELECT count(DISTINCT sal) FROM g GROUP BY dept"))))

  (testing "multiple aggregates over the same hidden key"
    (is (= {["2" "30"] 1 ["3" "120"] 1}
           (bag "SELECT count(*), sum(sal) FROM g GROUP BY dept"))))

  (testing "a composite grouping key"
    (is (= {["2"] 2 ["1"] 1} (bag "SELECT count(*) FROM g GROUP BY dept, reg"))))

  (testing "one group per distinct value — five singleton groups"
    (is (= {["1"] 5} (bag "SELECT count(*) FROM g GROUP BY sal"))))

  (testing "with WHERE, HAVING and an expression key"
    (is (= {["1"] 1 ["3"] 1} (bag "SELECT count(*) FROM g WHERE sal > 15 GROUP BY dept")))
    (is (= {["2"] 1 ["3"] 1} (bag "SELECT count(*) FROM g GROUP BY dept HAVING count(*) > 1")))
    (is (= {["1"] 5} (bag "SELECT count(*) FROM g GROUP BY sal/10"))))

  (testing "through a table alias"
    (is (= {["2"] 1 ["3"] 1} (bag "SELECT count(*) FROM g e GROUP BY e.dept")))))

(deftest projecting-the-key-still-works
  (testing "the shape that worked by accident must keep working"
    (is (= {["eng" "2"] 1 ["ops" "3"] 1}
           (bag "SELECT dept, count(*) FROM g GROUP BY dept")))
    (is (= {["eng" "w" "2"] 1 ["ops" "e" "2"] 1 ["ops" "w" "1"] 1}
           (bag "SELECT dept, reg, count(*) FROM g GROUP BY dept, reg")))
    (is (= [["eng"] ["ops"]] (rows "SELECT dept FROM g GROUP BY dept ORDER BY dept")))
    (is (= [["eng" "2"] ["ops" "3"]]
           (rows "SELECT dept, count(*) FROM g GROUP BY dept ORDER BY dept")))))

(deftest aggregates-without-group-by-are-unaffected
  (is (= [["5"]] (rows "SELECT count(*) FROM g")))
  (is (= [["150"]] (rows "SELECT sum(sal) FROM g")))
  (is (= [["50"]] (rows "SELECT max(sal) FROM g"))))

(deftest group-by-an-output-column-alias
  (testing "PostgreSQL resolves a bare GROUP BY name as a local FROM
            column first, then an output-column alias
            (findTargetlistEntrySQL92) — so this is legal and groups by
            the aliased expression"
    (is (= {["10"] 1 ["20"] 1 ["30"] 1 ["40"] 1 ["50"] 1}
           (bag "SELECT sal AS x FROM g GROUP BY x")))
    (is (= {["eng"] 1 ["ops"] 1} (bag "SELECT dept AS d FROM g GROUP BY d")))))

;; ---------------------------------------------------------------------------
;; 42803 — a select item that is neither grouped nor aggregated
;; ---------------------------------------------------------------------------

(deftest ungrouped-column-is-an-error
  (testing "used to return five ungrouped rows instead of erroring"
    (is (= "42803" (state "SELECT sal, count(*) FROM g GROUP BY dept")))
    (is (re-find #"must appear in the GROUP BY clause or be used in an aggregate function"
                 (or (err "SELECT sal, count(*) FROM g GROUP BY dept") "")))
    (is (= "42803" (state "SELECT dept, sal FROM g GROUP BY dept")))
    (is (= "42803" (state "SELECT dept, sal FROM g GROUP BY reg"))))

  (testing "an aggregate in the SELECT list groups the whole query, so an
            ungrouped column is an error even with no GROUP BY at all"
    (is (= "42803" (state "SELECT dept, count(*) FROM g"))))

  (testing "the message names the column the way PostgreSQL does"
    (is (re-find #"column \"g\.sal\"" (or (err "SELECT dept, sal FROM g GROUP BY dept") "")))))

(deftest grouping-by-the-primary-key-licenses-every-column
  (testing "PostgreSQL allows an ungrouped column when the table's PRIMARY
            KEY is a subset of the grouping columns — the grouping is a
            no-op for that table. Without this, GROUP BY on a PK would be
            a false 42803."
    (is (= [["eng" "10"] ["eng" "20"] ["ops" "30"] ["ops" "40"] ["ops" "50"]]
           (rows "SELECT dept, sal FROM g GROUP BY id ORDER BY id")))
    (is (= 5 (count (rows "SELECT * FROM g GROUP BY id"))))
    (is (= 5 (count (rows "SELECT id, dept, sum(sal) FROM g GROUP BY id")))))

  (testing "listing every projected column explicitly is the other way to
            satisfy the rule"
    (is (= 5 (count (rows "SELECT dept, sal FROM g GROUP BY dept, sal"))))))

(deftest queries-with-no-aggregate-and-no-group-by-are-not-checked
  (is (= 5 (count (rows "SELECT dept, sal FROM g"))))
  (is (= 5 (count (rows "SELECT * FROM g"))))
  (is (= [["eng"] ["ops"]] (rows "SELECT DISTINCT dept FROM g ORDER BY dept"))))

;; ---------------------------------------------------------------------------
;; Duplicate target columns
;; ---------------------------------------------------------------------------

(deftest duplicate-insert-column-is-42701
  (testing "we built a map from the column list, so the last value
            silently won and the client got INSERT 0 1 for a row it
            never asked for"
    (is (= "42701" (state "INSERT INTO g (id, id) VALUES (91, 92)")))
    (is (re-find #"column \"id\" specified more than once"
                 (or (err "INSERT INTO g (id, id) VALUES (91, 92)") "")))
    (is (= "42701" (state "INSERT INTO g (id, dept, id) VALUES (93,'x',94)")))
    (is (= 5 (count (rows "SELECT id FROM g"))) "and nothing was written"))

  (testing "an ordinary INSERT is unaffected, including the implicit
            column list, which is derived from the schema and cannot
            repeat"
    (is (= 1 (count (rows "INSERT INTO g (id, dept) VALUES (95,'y') RETURNING id"))))
    (is (= 1 (count (rows "INSERT INTO g VALUES (96,'z',1,'w') RETURNING id"))))))

(deftest duplicate-update-assignment-is-42601
  (is (= "42601" (state "UPDATE g SET sal = 1, sal = 2 WHERE id = 1")))
  (is (re-find #"multiple assignments to same column \"sal\""
               (or (err "UPDATE g SET sal = 1, sal = 2 WHERE id = 1") "")))
  (is (= [["10"]] (rows "SELECT sal FROM g WHERE id = 1")) "and nothing was written")
  (testing "distinct targets are unaffected"
    (is (= "UPDATE 1"
           (.-commandTag ^PgWireServer$QueryResult
            (run "UPDATE g SET sal = 11, dept = 'z' WHERE id = 1"))))))
