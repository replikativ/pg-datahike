(ns datahike.test.pg-lateral-srf-test
  "Correlated set-returning functions — `FROM t, LATERAL generate_series(1, t.n)`.

   doc/design-alignment.md listed this as outside the single-query model:
   PostgreSQL evaluates LATERAL as a parameterized nested loop, and a
   flat `:where` was said not to express per-outer-row evaluation.

   That is not so. Datahike's `bind-by-fn` (query.cljc) applies the
   function ONCE PER PRODUCTION TUPLE and expands the result through the
   binding form:

     (for [tuple (:tuples production)
           :let  [val (tuple-fn tuple)]
           :when (not (nil? val))]
       (prod-rel (rel/->Relation (:attrs production) [tuple])
                 (in->rel binding val)))

   So `[(f ?n) [[?v ?ord]]]` already IS the nested loop, inside the
   ordinary flat `:where` — no second query, no speculative data, and
   the parse cache and fast-select lanes stay available.

   Verified against the engine before building on it, including that
   both the legacy and planner engines agree and that clause ORDER does
   not matter (the planner defers the bind until its inputs are bound).

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *h* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*h* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *h* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- v [sql] (ffirst (rows sql)))

(defn- seed! []
  (run "CREATE TABLE t (id int, n int)")
  (run "INSERT INTO t VALUES (1,2),(2,3)"))

(deftest correlated-generate-series
  (seed!)
  (testing "one row per element, per outer row"
    (is (= [["1" "1"] ["1" "2"] ["2" "1"] ["2" "2"] ["2" "3"]]
           (rows "SELECT t.id, g.g FROM t, LATERAL generate_series(1, t.n) AS g
                  ORDER BY t.id, g.g"))))

  (testing "JOIN LATERAL ... ON true is the same relation"
    (is (= [["1" "1"] ["1" "2"] ["2" "1"] ["2" "2"] ["2" "3"]]
           (rows "SELECT t.id, g.g FROM t JOIN LATERAL generate_series(1, t.n) AS g ON true
                  ORDER BY t.id, g.g"))))

  (testing "AS g(x) renames the column"
    (is (= [["1" "1"] ["1" "2"] ["2" "1"] ["2" "2"] ["2" "3"]]
           (rows "SELECT t.id, g.x FROM t, LATERAL generate_series(1, t.n) AS g(x)
                  ORDER BY t.id, g.x"))))

  (testing "the outer row is multiplied even when only it is projected"
    (is (= [["1"] ["1"] ["2"] ["2"] ["2"]]
           (rows "SELECT t.id FROM t, LATERAL generate_series(1, t.n) AS g ORDER BY t.id")))))

(deftest correlated-srf-composes-with-the-rest-of-the-query
  (seed!)
  (testing "count(*) counts the expanded rows, not the outer ones"
    (is (= "5" (v "SELECT count(*) FROM t, LATERAL generate_series(1, t.n) AS g"))))

  (testing "WHERE filters the generated column"
    (is (= [["1" "2"] ["2" "2"] ["2" "3"]]
           (rows "SELECT t.id, g.g FROM t, LATERAL generate_series(1, t.n) AS g
                  WHERE g.g > 1 ORDER BY t.id, g.g"))))

  (testing "aggregates over the generated column"
    (is (= "9" (v "SELECT sum(g.g) FROM t, LATERAL generate_series(1, t.n) AS g"))))

  (testing "GROUP BY the outer column"
    (is (= [["1" "2"] ["2" "3"]]
           (rows "SELECT t.id, count(*) FROM t, LATERAL generate_series(1, t.n) AS g
                  GROUP BY t.id ORDER BY t.id"))))

  (testing "a correlated argument in every position"
    (is (= "2" (v "SELECT count(*) FROM t, LATERAL generate_series(t.n, t.n) AS g")))))

(deftest duplicates-are-not-collapsed
  ;; Datalog is set-based: a function returning [x x x] yields ONE row
  ;; without an ordinality var in `:with`. Verified directly against the
  ;; engine — #{[a 7]} versus [[a 7] [a 7] [a 7]] — which is why the
  ;; binding carries an ordinality it never projects.
  (run "CREATE TABLE d (id int, s text)")
  (run "INSERT INTO d VALUES (1,'x,x,x'),(2,'y,y')")
  (is (= "5" (v "SELECT count(*) FROM d, LATERAL string_to_table(d.s, ',') AS p")))
  (is (= [["x"] ["x"] ["x"]]
         (rows "SELECT p.p FROM d, LATERAL string_to_table(d.s, ',') AS p WHERE d.id = 1"))))

(deftest an-empty-result-eliminates-the-outer-row
  ;; PostgreSQL's INNER LATERAL semantics, and what the engine already
  ;; does: an empty collection binding drops the outer tuple.
  (seed!)
  (run "INSERT INTO t VALUES (3,0)")
  (testing "the n=0 row contributes nothing and does not survive"
    (is (= "5" (v "SELECT count(*) FROM t, LATERAL generate_series(1, t.n) AS g")))
    (is (= [["1"] ["2"]]
           (rows "SELECT DISTINCT t.id FROM t, LATERAL generate_series(1, t.n) AS g
                  ORDER BY t.id")))))

(deftest constant-arguments-still-take-the-materialise-once-path
  ;; A constant EXPRESSION argument must not be mistaken for a
  ;; correlated one: `srf-const-eval` answers ::corr for both, so the
  ;; discriminator is syntactic — a Column argument.
  (is (= [["1"] ["2"] ["3"]]
         (rows "SELECT * FROM generate_series(1, array_upper(current_schemas(false), 1) + 2)")))
  (is (= [["1"] ["2"]] (rows "SELECT * FROM generate_series(1,2)"))))

;; ---------------------------------------------------------------------------
;; LATERAL SUBQUERIES — `JOIN LATERAL (SELECT …) s ON true`

(defn- seed2! []
  (run "CREATE TABLE c (tid int, v int)")
  (run "INSERT INTO c VALUES (1,10),(1,11),(2,20)"))

(deftest correlated-lateral-subquery
  ;; Same mechanism as a correlated SRF — the inner runs once per outer
  ;; row through a function binding — except the "function" is a SQL
  ;; subquery executed with *from-bindings* holding the outer values.
  (seed!) (seed2!)
  (testing "JOIN LATERAL ... ON true"
    (is (= [["1" "10"] ["1" "11"] ["2" "20"]]
           (rows "SELECT t.id, s.v FROM t JOIN LATERAL
                    (SELECT v FROM c WHERE c.tid = t.id) s ON true
                  ORDER BY t.id, s.v"))))

  (testing "the comma form is the same relation"
    (is (= [["1" "10"] ["1" "11"] ["2" "20"]]
           (rows "SELECT t.id, s.v FROM t, LATERAL
                    (SELECT v FROM c WHERE c.tid = t.id) s
                  ORDER BY t.id, s.v"))))

  (testing "aggregates over the lateral column"
    (is (= "3"  (v "SELECT count(*) FROM t, LATERAL
                      (SELECT v FROM c WHERE c.tid = t.id) s")))
    (is (= "41" (v "SELECT sum(s.v) FROM t, LATERAL
                      (SELECT v FROM c WHERE c.tid = t.id) s"))))

  (testing "an aggregate INSIDE the inner"
    (is (= [["1" "11"] ["2" "20"]]
           (rows "SELECT t.id, s.mx FROM t, LATERAL
                    (SELECT max(v) AS mx FROM c WHERE c.tid = t.id) s
                  ORDER BY t.id"))))

  (testing "multiple aliased inner columns"
    (is (= [["1" "1" "10"] ["1" "1" "11"] ["2" "2" "20"]]
           (rows "SELECT t.id, s.a, s.b FROM t, LATERAL
                    (SELECT tid AS a, v AS b FROM c WHERE c.tid = t.id) s
                  ORDER BY t.id, s.b"))))

  (testing "WHERE, GROUP BY and DISTINCT compose"
    (is (= [["1" "11"] ["2" "20"]]
           (rows "SELECT t.id, s.v FROM t, LATERAL
                    (SELECT v FROM c WHERE c.tid = t.id) s
                  WHERE s.v > 10 ORDER BY t.id, s.v")))
    (is (= [["1" "2"] ["2" "1"]]
           (rows "SELECT t.id, count(*) FROM t, LATERAL
                    (SELECT v FROM c WHERE c.tid = t.id) s
                  GROUP BY t.id ORDER BY t.id")))
    (is (= [["1"] ["2"]]
           (rows "SELECT DISTINCT t.id FROM t, LATERAL
                    (SELECT v FROM c WHERE c.tid = t.id) s ORDER BY t.id")))))

(deftest an-empty-inner-eliminates-the-outer-row
  (seed!) (seed2!)
  (run "INSERT INTO t VALUES (9,1)")
  ;; tid=9 has no rows in c, so PostgreSQL drops it for an INNER lateral.
  (is (= "3" (v "SELECT count(*) FROM t, LATERAL
                   (SELECT v FROM c WHERE c.tid = t.id) s")))
  (is (= [["1"] ["2"]]
         (rows "SELECT DISTINCT t.id FROM t, LATERAL
                  (SELECT v FROM c WHERE c.tid = t.id) s ORDER BY t.id"))))

(deftest outer-lateral-subquery
  ;; An OUTER lateral keeps the outer row with NULLs when the inner
  ;; produces nothing. An empty collection binding DROPS it -- that is
  ;; the inner-join semantics the rest of this relies on -- so the ROW
  ;; PRODUCER supplies the missing row instead: one tuple of NULLs,
  ;; which is exactly what LEFT JOIN LATERAL … ON TRUE means.
  ;;
  ;; (The or-join construction the other OUTER joins use cannot be
  ;; applied here: it reached the fn-binding clause and raised the
  ;; datalog-internal `Cannot parse rule-vars`, which is why this was
  ;; refused outright before.)
  (seed!) (seed2!)
  (run "INSERT INTO t VALUES (3,1)")
  (testing "an outer row with no inner rows survives, NULL-extended"
    (is (= [["1" "10"] ["1" "11"] ["2" "20"] ["3" nil]]
           (rows "SELECT t.id, s.v FROM t LEFT JOIN LATERAL
                    (SELECT v FROM c WHERE c.tid = t.id) s ON true
                  ORDER BY t.id, s.v"))))
  (testing "an aggregate inner is still ONE row -- count is 0, not NULL"
    (is (= [["1" "2"] ["2" "1"] ["3" "0"]]
           (rows "SELECT t.id, s.c FROM t LEFT JOIN LATERAL
                    (SELECT count(*) AS c FROM c WHERE c.tid = t.id) s ON true
                  ORDER BY t.id"))))
  (testing "INNER LATERAL still eliminates the childless row"
    (is (= [["1" "10"] ["1" "11"] ["2" "20"]]
           (rows "SELECT t.id, s.v FROM t JOIN LATERAL
                    (SELECT v FROM c WHERE c.tid = t.id) s ON true
                  ORDER BY t.id, s.v")))))

(deftest outer-lateral-with-a-join-condition-is-refused
  ;; ON TRUE only. With a real condition a row the condition rejects
  ;; still has to survive as NULLs, and a producer that has already
  ;; emitted its rows cannot tell that from a match. Refuse rather than
  ;; answer wrongly.
  ;;
  ;; Only LEFT is meaningful at all: PostgreSQL rejects RIGHT and FULL
  ;; LATERAL outright ("invalid reference to FROM-clause entry"),
  ;; because the inner cannot reference a table it is outer-joined from
  ;; the left of.
  (seed!) (seed2!)
  (let [e (.-error ^PgWireServer$QueryResult
           (run "SELECT t.id FROM t LEFT JOIN LATERAL
                           (SELECT v FROM c WHERE c.tid = t.id) s ON s.v > 10"))]
    (is (re-find #"OUTER JOIN LATERAL" (or e "")))))

(deftest an-uncorrelated-derived-table-is-untouched
  ;; Only a LATERAL whose inner references an outer column takes the new
  ;; path; a plain derived table still materialises once.
  (seed2!)
  (is (= [["10"] ["11"] ["20"]]
         (rows "SELECT s.v FROM (SELECT v FROM c) s ORDER BY s.v"))))
