(ns datahike.test.pg-sql-cte-test
  "WITH / WITH RECURSIVE end-to-end through the pgwire server.

   - Plain non-recursive CTE on every DML path (SELECT/UPDATE/DELETE/INSERT)
     is materialised at parse-sql time and the outer statement sees the
     CTE rows as a virtual `:<cte>/<col>` table.

   - WITH RECURSIVE in SELECT goes through `translate-recursive-cte` →
     `materialize-recursive-cte!`, which runs the lowered Datalog rule
     against the speculative db and persists the fixed-point rows as
     virtual attrs the outer SELECT then reads.

   - Data-modifying CTE bodies (`WITH x AS (INSERT … RETURNING …)`) are
     skipped at parse time rather than crashing on JSqlParser's
     ParenthesedInsert → ParenthesedSelect cast.

   The recursive path needs the datahike query planner enabled. We
   bind `*disable-planner* false` inside `materialize-recursive-cte!`
   itself (and `server.execute` also binds it at the handler entry),
   so the deftests don't have to set the env var."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn cte-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      ;; Seed through SQL DDL/DML — the representative path: tables created
      ;; via SQL carry the row-marker attribute UPDATE/DELETE row matching
      ;; anchors on. (Seeding via d/transact leaves the marker out, and the
      ;; query engine now — correctly — rejects the resulting anchorless
      ;; get-else-only row-match queries; see #923-era strictness.)
      (let [h (pg/make-query-handler conn)]
        (doseq [sql ["CREATE TABLE emp (id INTEGER PRIMARY KEY, name TEXT, dept TEXT, salary DOUBLE PRECISION)"
                     "CREATE TABLE node (id INTEGER PRIMARY KEY, parent INTEGER, name TEXT)"
                     "INSERT INTO emp (id, name, dept, salary) VALUES (1, 'Alice', 'Eng', 90000.0), (2, 'Bob', 'Eng', 85000.0), (3, 'Carol', 'Sales', 70000.0), (4, 'Dave', 'Sales', 75000.0), (5, 'Eve', 'Eng', 95000.0)"
                     ;; Small tree: root(1) -> a(2),b(3); a -> a1(4),a2(5)
                     "INSERT INTO node (id, parent, name) VALUES (1, 0, 'root'), (2, 1, 'a'), (3, 1, 'b'), (4, 2, 'a1'), (5, 2, 'a2')"]]
          (let [r (.execute h sql)]
            (when (.-error r)
              (throw (ex-info "fixture seed failed" {:sql sql :err (.-error r)}))))))
      (let [{:keys [server]} (pg/start-server {"cte" conn} {:port 0})]
        (try
          (binding [*conn* conn *port* (.getPort server)]
            (f))
          (finally
            (.stop server)
            (d/release conn)
            (d/delete-database cfg)))))))

(use-fixtures :each cte-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/cte?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- rows
  "Run a SELECT and collect each row as a vector of column values
   read positionally as strings (`getString`). String form keeps the
   golden-value assertions stable across JDBC type plumbing."
  [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs ^long %) (range 1 (inc n)))))
          acc)))))

(defn- update-count ^long [^Connection c sql]
  (with-open [st (.createStatement c)]
    (.executeUpdate st sql)))

(deftest create-table-as-select-materializes-schema-and-rows
  (with-open [c (jdbc)]
    (is (zero? (update-count
                c "CREATE TABLE eng_copy AS
                     SELECT id, name FROM emp WHERE dept = 'Eng'")))
    (is (= [["1" "Alice"] ["2" "Bob"] ["5" "Eve"]]
           (rows c "SELECT id, name FROM eng_copy ORDER BY id")))
    (is (= [["integer" "text"]]
           (rows c "SELECT pg_typeof(id), pg_typeof(name) FROM eng_copy LIMIT 1")))))

(deftest create-table-as-aggregate-result
  (with-open [c (jdbc)]
    (is (zero? (update-count
                c "CREATE TABLE dept_counts AS
                     SELECT dept, count(*) AS n FROM emp GROUP BY dept")))
    (is (= [["Eng" "3"] ["Sales" "2"]]
           (rows c "SELECT dept, n FROM dept_counts ORDER BY dept")))))

;; ---------------------------------------------------------------------------
;; Plain (non-recursive) CTE lifted to every DML path
;; ---------------------------------------------------------------------------

(deftest with-on-select-still-works
  ;; Regression for the existing PlainSelect path — the WITH lift refactor
  ;; must not change behaviour here.
  (with-open [c (jdbc)]
    (is (= [["Eve"   "95000"]
            ["Alice" "90000"]
            ["Bob"   "85000"]]
           (rows c "WITH eng AS (SELECT name, salary FROM emp WHERE dept = 'Eng')
                    SELECT name, salary FROM eng ORDER BY salary DESC")))))

(deftest explain-is-a-non-executing-api-boundary
  (with-open [c (jdbc)
              st (.createStatement c)]
    (.execute st "BEGIN")
    (with-open [rs (.executeQuery st "EXPLAIN (VERBOSE, COSTS OFF) SELECT * FROM emp")]
      (is (= "QUERY PLAN" (.. rs getMetaData (getColumnLabel 1))))
      (is (.next rs))
      (is (re-find #"^Datahike Query " (.getString rs 1))))
    ;; A supported EXPLAIN must not poison the surrounding transaction.
    (is (= [["5"]] (rows c "SELECT count(*) FROM emp")))
    (.execute st "COMMIT")
    (let [e (is (thrown? SQLException
                         (rows c "EXPLAIN (ANALYZE TRUE) SELECT * FROM emp")))]
      (is (= "0A000" (.getSQLState ^SQLException e))))))

(deftest postgres-explain-costs-off-slice
  ;; PostgreSQL 17 src/test/regress/sql/explain.sql:64. Keep the literal-only
  ;; form from issue #93: JSqlParser rejects PostgreSQL's parenthesized EXPLAIN
  ;; options unless the compatibility prefix is stripped before AST parsing.
  (with-open [c (jdbc)
              st (.createStatement c)
              rs (.executeQuery st "EXPLAIN (COSTS OFF) SELECT 42")]
    (is (= "QUERY PLAN" (.. rs getMetaData (getColumnLabel 1))))
    (is (.next rs))
    (is (re-find #"^Datahike Query " (.getString rs 1)))
    (is (false? (.next rs)))))

(deftest values-cte-materializes-declared-columns
  ;; PostgreSQL's scalar regression tests use this shape heavily for
  ;; tables of bit patterns. Values is a Select body in JSqlParser, but it
  ;; is not a PlainSelect; force-casting it in materialize-set-op! leaked a
  ;; ClassCastException instead of producing the CTE rows.
  (with-open [c (jdbc)]
    (is (= [["1" "a"] ["2" "b"]]
           (rows c "WITH testdata(bits, label) AS
                      (VALUES (2, 'b'), (1, 'a'))
                    SELECT bits, label FROM testdata ORDER BY bits")))
    (is (= [["bit" "1"] ["bit" "2"]]
           (rows c "WITH testdata(bits) AS
                      (VALUES (x'00000001'), (x'00000002'))
                    SELECT pg_typeof(bits), bits::integer
                    FROM testdata ORDER BY bits")))))

(deftest with-on-delete-resolves-cte-in-where
  ;; Before the materialize-withs! lift, parse-sql only ran the WITH-list
  ;; fold inside the PlainSelect branch. WITH x AS (…) DELETE … WHERE
  ;; id IN (SELECT … FROM x) parsed but the WHERE subquery couldn't
  ;; resolve the CTE alias at execute time, surfacing as a "Cannot
  ;; resolve any more clauses" error. Now the CTE rides on :enriched-db
  ;; and build-delete-tx queries against it.
  (with-open [c (jdbc)]
    (is (= 2
           (update-count
            c "WITH src AS (SELECT id FROM emp WHERE dept = 'Sales')
               DELETE FROM emp WHERE id IN (SELECT id FROM src)")))
    (is (= [["Alice"] ["Bob"] ["Eve"]]
           (rows c "SELECT name FROM emp ORDER BY name")))))

(deftest with-on-update-resolves-cte-in-where
  (with-open [c (jdbc)]
    (is (= 2
           (update-count
            c "WITH src AS (SELECT id FROM emp WHERE dept = 'Sales')
               UPDATE emp SET dept = 'Retail' WHERE id IN (SELECT id FROM src)")))
    (is (= [["Carol" "Retail"] ["Dave" "Retail"]]
           (rows c "SELECT name, dept FROM emp WHERE dept = 'Retail' ORDER BY name")))))

(deftest data-modifying-cte-rejected-as-unsupported
  ;; JSqlParser's WithItem.getSelect() unsafely casts to ParenthesedSelect,
  ;; so naively walking a WITH list with a DML body throws a CCE that
  ;; surfaces as XX000. Our materialize-withs! uses .getParenthesedStatement
  ;; instead.
  ;;
  ;; PostgreSQL RUNS a data-modifying CTE and feeds its RETURNING rows to
  ;; the outer query. We don't implement that. Skipping the item silently
  ;; left the CTE unmaterialised, so the outer SELECT returned zero rows
  ;; and looked like a legitimately empty result — the same class of
  ;; silent wrong answer as an unresolvable column. Say so instead.
  (with-open [c (jdbc)]
    (let [e (is (thrown? org.postgresql.util.PSQLException
                         (rows c "WITH i AS (INSERT INTO emp(id, name, dept, salary)
                                             VALUES (99, 'Z', 'Eng', 1.0) RETURNING id)
                                  SELECT id FROM i")))]
      (is (= "0A000" (.getSQLState ^org.postgresql.util.PSQLException e)))
      (is (re-find #"data-modifying" (.getMessage ^org.postgresql.util.PSQLException e))))
    ;; Side-effect rule: the INSERT inside the CTE must not have run.
    (is (empty? (rows c "SELECT id FROM emp WHERE id = 99")))

    ;; PostgreSQL permits the RECURSIVE keyword even when the CTE body is
    ;; data-modifying rather than recursive. JSqlParser still marks the item
    ;; recursive, so this must take the same explicit unsupported path before
    ;; recursive SELECT-shape validation touches WithItem.getSelect().
    (let [e (is (thrown? org.postgresql.util.PSQLException
                         (rows c "WITH RECURSIVE i AS (
                                    INSERT INTO emp(id, name, dept, salary)
                                    VALUES (100, 'Y', 'Eng', 2.0) RETURNING id
                                  ) SELECT id FROM i")))]
      (is (= "0A000" (.getSQLState ^org.postgresql.util.PSQLException e)))
      (is (re-find #"data-modifying" (.getMessage ^org.postgresql.util.PSQLException e))))
    (is (empty? (rows c "SELECT id FROM emp WHERE id = 100")))))

(deftest cte-names-are-not-writable-relations
  ;; PostgreSQL with.sql's `WITH with_test AS (...) INSERT INTO with_test`
  ;; resolves the INSERT target only among stored relations. A CTE can still
  ;; be the SELECT source, and a same-named physical table remains the target.
  (with-open [c (jdbc)]
    (let [e (is (thrown? SQLException
                         (update-count c "WITH phantom AS (SELECT 42)
                                          INSERT INTO phantom VALUES (1)")))]
      (is (= "42P01" (.getSQLState ^SQLException e))))
    (is (zero? (update-count c "CREATE TABLE shadowed (i INTEGER)")))
    (is (= 1 (update-count c "WITH shadowed AS (SELECT 42 AS i)
                               INSERT INTO shadowed SELECT * FROM shadowed")))
    (is (= [["42"]] (rows c "SELECT i FROM shadowed")))))

;; ---------------------------------------------------------------------------
;; WITH RECURSIVE in SELECT
;; ---------------------------------------------------------------------------

(deftest with-recursive-range
  ;; Anchor is a scanless `(SELECT 1)` — the case that requires the
  ;; planner's fixpoint executor (datahike PR #825) instead of the
  ;; legacy engine's recursive-rule path.
  (with-open [c (jdbc)]
    (is (= [["1"] ["2"] ["3"] ["4"] ["5"]]
           (rows c "WITH RECURSIVE t(n) AS (SELECT 1 UNION ALL SELECT n+1 FROM t WHERE n < 5)
                    SELECT n FROM t ORDER BY n")))))

(deftest postgres-recursive-scalar-values-anchor
  ;; PostgreSQL 17 src/test/regress/sql/with.sql:26-33. VALUES is a SELECT
  ;; body in JSqlParser, including when it is used as a scalar subquery in a
  ;; projection. Lower its sole expression directly so the recursive anchor
  ;; binds n instead of leaking an unbound CTE entity variable.
  (with-open [c (jdbc)]
    (is (= [["1"] ["2"] ["3"] ["4"] ["5"]]
           (rows c "WITH RECURSIVE t(n) AS (
                      SELECT (VALUES(1))
                      UNION ALL
                      SELECT n+1 FROM t WHERE n < 5
                    ) SELECT * FROM t")))
    (is (= [["1"]] (rows c "SELECT (VALUES(1))")))
    (let [e (is (thrown? SQLException
                         (rows c "SELECT (VALUES(1), (2))")))]
      (is (= "21000" (.getSQLState ^SQLException e))))
    (let [e (is (thrown? SQLException
                         (rows c "SELECT (VALUES(1, 2))")))]
      (is (= "42601" (.getSQLState ^SQLException e))))))

(deftest demand-limited-unbounded-recursion-fails-before-materialization
  (with-open [c (jdbc)]
    (let [e (is (thrown? SQLException
                         (rows c "WITH RECURSIVE t(n) AS (
                                    SELECT 1 UNION SELECT n+1 FROM t
                                  ) SELECT n FROM t LIMIT 10")))]
      (is (= "0A000" (.getSQLState ^SQLException e)))
      (is (re-find #"demand-driven recursive CTE"
                   (.getMessage ^SQLException e))))))

(deftest postgres-recursive-set-operation-shape-slice
  ;; PostgreSQL with.sql lines 916-924. A recursive reference is legal only
  ;; under UNION [ALL]; INTERSECT/EXCEPT must fail before fixed-point lowering.
  (with-open [c (jdbc)]
    (doseq [op ["INTERSECT" "INTERSECT ALL" "EXCEPT"]]
      (let [e (is (thrown? SQLException
                           (rows c (str "WITH RECURSIVE x(n) AS "
                                        "(SELECT 1 " op " SELECT n+1 FROM x) "
                                        "SELECT * FROM x"))))]
        (is (= "42601" (.getSQLState ^SQLException e)))
        (is (re-find #"does not have the form non-recursive-term UNION"
                     (.getMessage ^SQLException e)))))))

(deftest recursive-keyword-does-not-make-every-cte-recursive
  ;; WITH RECURSIVE permits ordinary CTE items; fixed-point lowering applies
  ;; only when an item references itself.
  (with-open [c (jdbc)]
    (is (= [["1"]]
           (rows c "WITH RECURSIVE x(n) AS (SELECT 1),
                          y(n) AS (SELECT * FROM x)
                    SELECT * FROM y")))
    ;; PostgreSQL with.sql lines 936-942 additionally nests a WITH inside the
    ;; ordinary item. That scope is not implemented yet, but it must not be
    ;; misclassified as recursive or reported as a nonexistent relation.
    (let [e (is (thrown? SQLException
                         (rows c "WITH RECURSIVE x(n) AS (
                                    WITH x1 AS (SELECT 1 AS n)
                                    SELECT 0 UNION SELECT * FROM x1
                                  ) SELECT * FROM x")))]
      (is (= "0A000" (.getSQLState ^SQLException e)))
      (is (re-find #"nested WITH" (.getMessage ^SQLException e))))))

(deftest malformed-recursive-self-references-fail-before-lowering
  ;; PostgreSQL with.sql lines 930-935. These previously reached Datahike as
  ;; queries containing unbound CTE entity vars.
  (with-open [c (jdbc)]
    (let [e (is (thrown? SQLException
                         (rows c "WITH RECURSIVE x(n) AS (SELECT n FROM x)
                                  SELECT * FROM x")))]
      (is (= "42601" (.getSQLState ^SQLException e)))
      (is (re-find #"does not have the form non-recursive-term UNION"
                   (.getMessage ^SQLException e))))
    (let [e (is (thrown? SQLException
                         (rows c "WITH RECURSIVE x(n) AS (
                                    SELECT n FROM x UNION ALL SELECT 1
                                  ) SELECT * FROM x")))]
      (is (= "42P19" (.getSQLState ^SQLException e)))
      (is (re-find #"must not appear within its non-recursive term"
                   (.getMessage ^SQLException e))))))

(deftest unsupported-recursive-shape-does-not-leak-invalid-datalog
  ;; Forward CTE dependencies under WITH RECURSIVE are legal PostgreSQL but
  ;; not yet available to either fixed-point compiler. Keep that capability
  ;; gap explicit instead of leaving x unmaterialised and leaking
  ;; "Query for unknown vars" from Datahike.
  (with-open [c (jdbc)]
    (let [e (is (thrown? SQLException
                         (rows c "WITH RECURSIVE
                                    x(id) AS (SELECT * FROM y UNION ALL
                                              SELECT id+1 FROM x WHERE id < 5),
                                    y(id) AS (VALUES (1))
                                  SELECT * FROM x")))]
      (is (= "0A000" (.getSQLState ^SQLException e)))
      (is (re-find #"recursive CTE shape" (.getMessage ^SQLException e))))))

(deftest postgres-recursive-outer-join-slice
  ;; PostgreSQL with.sql lines 977-992. The recursive relation may not occupy
  ;; the nullable side of an outer join during fixed-point evaluation.
  (with-open [c (jdbc)]
    (doseq [recursive-from ["y LEFT JOIN x ON x.n = y.id"
                            "x RIGHT JOIN node y ON x.n = y.id"
                            "x FULL JOIN node y ON x.n = y.id"]]
      (let [e (is (thrown? SQLException
                           (rows c (str "WITH RECURSIVE x(n) AS ("
                                        "SELECT 1 UNION ALL "
                                        "SELECT x.n+1 FROM " recursive-from
                                        " WHERE x.n < 10) SELECT * FROM x"))))]
        (is (= "42P19" (.getSQLState ^SQLException e)))
        (is (re-find #"recursive reference to query \"x\" must not appear within an outer join"
                     (.getMessage ^SQLException e)))))))

(deftest postgres-recursive-clause-restrictions-slice
  ;; PostgreSQL with.sql lines 1000-1017. These clauses are rejected during
  ;; recursive-query validation, before a potentially unbounded fixed point.
  (with-open [c (jdbc)]
    (doseq [[term state message]
            [["SELECT count(*) FROM x" "42803" "aggregate functions"]
             ["SELECT n+1 FROM x WHERE n < 3 ORDER BY 1" "0A000" "ORDER BY"]
             ["SELECT n+1 FROM x WHERE n < 3 LIMIT 2 OFFSET 1" "0A000" "OFFSET"]
             ["SELECT n+1 FROM x WHERE n < 3 FOR UPDATE" "0A000" "FOR UPDATE/SHARE"]]]
      (testing term
        (let [e (try
                  (rows c (str "WITH RECURSIVE x(n) AS "
                               "(SELECT 1 UNION ALL " term ") "
                               "SELECT * FROM x"))
                  nil
                  (catch SQLException e e))]
          (is (some? e))
          (when e
            (is (= state (.getSQLState ^SQLException e)))
            (is (re-find (re-pattern message) (.getMessage ^SQLException e)))))))))

(deftest with-recursive-update-from-cte
  ;; Lock-in for the UPDATE WITH RECURSIVE path with a 2-column CTE — the
  ;; shape that previously triggered datahike's delta-driven-expand fast
  ;; path. That shortcut bypassed the rec body's :function ops (sql-+ on
  ;; depth, get-else for nullable columns) and silently emitted
  ;; `[entity-id, propagated]` tuples, so children's depth never updated.
  ;; Tightened in datahike PR #826.
  (with-open [c (jdbc)]
    (.executeUpdate (.createStatement c) "ALTER TABLE node ADD COLUMN depth INTEGER")
    (.executeUpdate
     (.createStatement c)
     "WITH RECURSIVE walk(id, depth) AS (
        SELECT id, 0 FROM node WHERE parent = 0
        UNION ALL
        SELECT n.id, w.depth + 1 FROM node n
          JOIN walk w ON n.parent = w.id
      )
      UPDATE node SET depth = walk.depth FROM walk WHERE node.id = walk.id")
    (is (= [["1" "0"]
            ["2" "1"]
            ["3" "1"]
            ["4" "2"]
            ["5" "2"]]
           (rows c "SELECT id, depth FROM node ORDER BY id")))))

(deftest with-recursive-tree-depth
  ;; Real-world shape: tree traversal with depth annotation. Anchor scans
  ;; the parent==0 row (data-pattern base case). Recursive branch joins
  ;; node against the previous iteration via t.id.
  (with-open [c (jdbc)]
    (is (= [["1" "root" "0"]
            ["2" "a"    "1"]
            ["3" "b"    "1"]
            ["4" "a1"   "2"]
            ["5" "a2"   "2"]]
           (rows c "WITH RECURSIVE tree(id, name, depth) AS (
                      SELECT id, name, 0 FROM node WHERE parent = 0
                      UNION ALL
                      SELECT n.id, n.name, t.depth+1 FROM node n
                        JOIN tree t ON n.parent = t.id
                    )
                    SELECT id, name, depth FROM tree ORDER BY id")))))
