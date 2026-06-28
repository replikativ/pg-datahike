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
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn cte-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn
                  [{:db/ident :emp/id     :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                   {:db/ident :emp/name   :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :emp/dept   :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :emp/salary :db/valueType :db.type/double
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :node/id     :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                   {:db/ident :node/parent :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :node/name   :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}])
      (d/transact conn
                  [{:emp/id 1 :emp/name "Alice" :emp/dept "Eng"   :emp/salary 90000.0}
                   {:emp/id 2 :emp/name "Bob"   :emp/dept "Eng"   :emp/salary 85000.0}
                   {:emp/id 3 :emp/name "Carol" :emp/dept "Sales" :emp/salary 70000.0}
                   {:emp/id 4 :emp/name "Dave"  :emp/dept "Sales" :emp/salary 75000.0}
                   {:emp/id 5 :emp/name "Eve"   :emp/dept "Eng"   :emp/salary 95000.0}])
      (d/transact conn
                  ;; Small tree: root(1) → a(2),b(3); a → a1(4),a2(5)
                  [{:node/id 1 :node/parent 0 :node/name "root"}
                   {:node/id 2 :node/parent 1 :node/name "a"}
                   {:node/id 3 :node/parent 1 :node/name "b"}
                   {:node/id 4 :node/parent 2 :node/name "a1"}
                   {:node/id 5 :node/parent 2 :node/name "a2"}])
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

(deftest data-modifying-cte-skipped-cleanly
  ;; JSqlParser's WithItem.getSelect() unsafely casts to ParenthesedSelect,
  ;; so naively walking a WITH list with a DML body throws a CCE that
  ;; surfaces as XX000. Our materialize-withs! uses .getParenthesedStatement
  ;; instead and skips DML-bodied items rather than crashing. The CTE then
  ;; isn't materialised, so the outer SELECT's reference to `i` resolves
  ;; against an empty/missing virtual table — execute returns 0 rows
  ;; instead of an internal error.
  (with-open [c (jdbc)]
    (let [r (rows c "WITH i AS (INSERT INTO emp(id, name, dept, salary)
                                VALUES (99, 'Z', 'Eng', 1.0) RETURNING id)
                     SELECT id FROM i")]
      ;; Currently degrades to 0 rows; the alternative (a clean 0A000)
      ;; would be a follow-up. The contract this test enforces is
      ;; "doesn't crash with a Java type cast as the user-facing error."
      (is (vector? r))
      ;; Side-effect rule: the INSERT inside the CTE must not have run.
      ;; If it had, emp would contain id=99. PG's data-modifying CTEs
      ;; do execute the inner DML; pg-datahike does not yet.
      (is (empty? (rows c "SELECT id FROM emp WHERE id = 99"))))))

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
