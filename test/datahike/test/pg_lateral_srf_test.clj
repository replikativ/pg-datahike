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
