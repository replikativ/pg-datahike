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

  (testing "a hidden ORDER BY column obeys the same grouping rule"
    (is (= "42803" (state "SELECT count(*) FROM g GROUP BY dept ORDER BY sal")))
    (is (= {["2"] 1 ["3"] 1}
           (bag "SELECT count(*) FROM g GROUP BY dept ORDER BY dept"))))

  (testing "the message names the column the way PostgreSQL does"
    (is (re-find #"column \"g\.sal\"" (or (err "SELECT dept, sal FROM g GROUP BY dept") "")))))

(deftest unqualified-self-join-columns-are-ambiguous
  (testing "relation occurrences, not distinct storage namespaces, determine ambiguity"
    (is (= "42702" (state "SELECT count(*) FROM g x, g y WHERE x.id = y.id GROUP BY dept")))
    (is (= "42702" (state "SELECT count(sal) FROM g x, g y WHERE x.id = y.id GROUP BY x.dept")))
    (is (= {["2"] 1 ["3"] 1}
           (bag "SELECT count(*) FROM g x, g y WHERE x.id = y.id GROUP BY x.dept")))))

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

;; ---------------------------------------------------------------------------
;; AVG result scale — exposed by grouping, not caused by it
;; ---------------------------------------------------------------------------

(deftest avg-numeric-result-scale
  ;; PostgreSQL picks a division result scale via select_div_scale
  ;; (numeric.c): at least NUMERIC_MIN_SIG_DIGITS=16, minus 4 per unit of
  ;; estimated quotient weight, with one extra digit of headroom when the
  ;; dividend's leading digit is <= the divisor's.
  ;;
  ;; That comparison is on base-10000 NumericDigits, and the grouping is
  ;; aligned to the DECIMAL POINT rather than to the leading significant
  ;; digit: 120 is ONE digit of value 120, not 1200. We left-aligned, so
  ;; `sum 120 / count 3` compared 1200 against 3000 instead of 120
  ;; against 3, took the headroom branch, and produced 20 fractional
  ;; digits where PostgreSQL produces 16.
  ;;
  ;; Only visible once GROUP BY worked: before that every group was one
  ;; group, so few sums ever hit the mis-compare.
  (testing "16 digits, the common case"
    (is (= [["15.0000000000000000"]] (rows "SELECT avg(sal) FROM g WHERE dept = 'eng'")))
    (is (= [["40.0000000000000000"]] (rows "SELECT avg(sal) FROM g WHERE dept = 'ops'")))
    (is (= {["15.0000000000000000"] 1 ["40.0000000000000000"] 1}
           (bag "SELECT avg(sal) FROM g GROUP BY dept"))))
  (testing "20 digits where PostgreSQL genuinely produces them — the
            headroom branch is real, it was just being taken too often"
    (.execute *handler* "CREATE TABLE av (id int PRIMARY KEY, v int)")
    (.execute *handler* "INSERT INTO av VALUES (1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1)")
    (is (= [["1.00000000000000000000"]] (rows "SELECT avg(v) FROM av")))))

;; ---------------------------------------------------------------------------
;; ORDER BY over aggregates
;;
;; Fixture: eng {10,20}, ops {30,40,50}.
;; ---------------------------------------------------------------------------

(deftest order-by-an-aggregate
  (testing "written out in ORDER BY — refused with 0A000 even when the
            aggregate WAS projected, because the ORDER BY translator saw
            only an {:aggregate} marker and never asked whether the
            projection had already emitted it"
    (is (= [["eng" "2"] ["ops" "3"]]
           (rows "SELECT dept, count(*) FROM g GROUP BY dept ORDER BY count(*)")))
    (is (= [["ops" "3"] ["eng" "2"]]
           (rows "SELECT dept, count(*) FROM g GROUP BY dept ORDER BY count(*) DESC")))
    (is (= [["eng" "30"] ["ops" "120"]]
           (rows "SELECT dept, sum(sal) FROM g GROUP BY dept ORDER BY sum(sal)"))))

  (testing "and when it is NOT projected — PostgreSQL allows that too, so
            the aggregate is synthesized and rides on :hidden-count"
    (is (= [["eng"] ["ops"]]
           (rows "SELECT dept FROM g GROUP BY dept ORDER BY count(*)")))
    (is (= [["ops"] ["eng"]]
           (rows "SELECT dept FROM g GROUP BY dept ORDER BY sum(sal) DESC"))))

  (testing "by the aggregate's output alias — COUNT(*) emits the bare
            `count` symbol, which the materialise-allowlist never
            covered, so this became the where-clause [(count ?e) ?v] and
            Datalog called clojure.core/count on an entity id:
            \"count not supported on this type: Long\""
    (is (= [["ops" "3"] ["eng" "2"]]
           (rows "SELECT dept, count(*) c FROM g GROUP BY dept ORDER BY c DESC")))
    (is (= [["eng" "2"] ["ops" "3"]]
           (rows "SELECT dept, count(*) c FROM g GROUP BY dept ORDER BY c")))
    (testing "SUM always worked — it emits the qualified filter-sum, which
              WAS on the allowlist. That asymmetry is the bug."
      (is (= [["eng" "30"] ["ops" "120"]]
             (rows "SELECT dept, sum(sal) s FROM g GROUP BY dept ORDER BY s")))))

  (testing "combined with HAVING and LIMIT"
    (is (= [["eng" "2"] ["ops" "3"]]
           (rows "SELECT dept, count(*) FROM g GROUP BY dept HAVING count(*) > 1 ORDER BY count(*)")))
    (is (= [["ops" "3"]]
           (rows "SELECT dept, count(*) FROM g GROUP BY dept ORDER BY count(*) DESC LIMIT 1")))))

;; ---------------------------------------------------------------------------
;; Select-list ordinals
;; ---------------------------------------------------------------------------

(deftest order-by-ordinal
  (testing "a bare integer is a 1-based ordinal into the select list.
            The simple-query path templates bare number literals to $N
            for plan-cache stability, which turned `ORDER BY 2` into
            `ORDER BY $1` — sorting every row by the same constant. The
            sort was silently dropped."
    (is (= [["eng" "10"] ["eng" "20"] ["ops" "30"] ["ops" "40"] ["ops" "50"]]
           (rows "SELECT dept, sal FROM g ORDER BY 2")))
    (is (= [["ops" "50"] ["ops" "40"] ["ops" "30"] ["eng" "20"] ["eng" "10"]]
           (rows "SELECT dept, sal FROM g ORDER BY 2 DESC")))
    (is (= [["eng" "10"] ["eng" "20"] ["ops" "30"] ["ops" "40"] ["ops" "50"]]
           (rows "SELECT dept, sal FROM g ORDER BY 1, 2"))))

  (testing "out of range"
    (is (= "42P10" (state "SELECT dept, sal FROM g ORDER BY 3")))
    (is (re-find #"ORDER BY position 3 is not in select list"
                 (or (err "SELECT dept, sal FROM g ORDER BY 3") "")))
    (is (= "42P10" (state "SELECT dept, sal FROM g ORDER BY 0"))))

  (testing "values elsewhere in the statement must STILL be templated —
            the fix has to be positional, not a blanket opt-out"
    (is (= [["eng" "20"] ["ops" "30"] ["ops" "40"] ["ops" "50"]]
           (rows "SELECT dept, sal FROM g WHERE sal > 15 ORDER BY 2")))
    (is (= [["eng" "10"] ["eng" "20"] ["ops" "30"]]
           (rows "SELECT dept, sal FROM g ORDER BY 2 LIMIT 3")))
    (testing "and a number that is part of an expression is a VALUE, not
              an ordinal"
      (is (= [["eng" "10"] ["eng" "20"] ["ops" "30"] ["ops" "40"] ["ops" "50"]]
             (rows "SELECT dept, sal FROM g ORDER BY sal + 2"))))))

(deftest group-by-ordinal
  (is (= {["eng" "2"] 1 ["ops" "3"] 1}
         (bag "SELECT dept, count(*) FROM g GROUP BY 1")))
  (is (= [["ops" "3"] ["eng" "2"]]
         (rows "SELECT dept, count(*) FROM g GROUP BY 1 ORDER BY 2 DESC")))
  (is (= [["eng" "30"] ["ops" "120"]]
         (rows "SELECT dept, sum(sal) FROM g GROUP BY 1 ORDER BY 2")))
  (testing "out of range, and an ordinal pointing AT an aggregate"
    (is (= "42P10" (state "SELECT dept, sal FROM g GROUP BY 5")))
    (is (= "42803" (state "SELECT count(*) FROM g GROUP BY 1")))
    (is (re-find #"aggregate functions are not allowed in GROUP BY"
                 (or (err "SELECT count(*) FROM g GROUP BY 1") "")))))

(deftest non-integer-constant-is-rejected
  (testing "only an INTEGER constant is an ordinal; PostgreSQL rejects
            every other bare constant rather than sorting or grouping
            every row by the same value"
    (is (= "42601" (state "SELECT dept, sal FROM g ORDER BY 'abc'")))
    (is (re-find #"non-integer constant in ORDER BY"
                 (or (err "SELECT dept, sal FROM g ORDER BY 'abc'") "")))
    (is (= "42601" (state "SELECT dept, sal FROM g ORDER BY NULL")))
    (is (= "42601" (state "SELECT dept, sal FROM g ORDER BY 1.5")))
    (is (= "42601" (state "SELECT dept FROM g GROUP BY 'abc'")))
    (is (= "42601" (state "SELECT dept FROM g GROUP BY dept, 'x'")))))
