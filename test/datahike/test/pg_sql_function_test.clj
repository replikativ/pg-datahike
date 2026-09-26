(ns datahike.test.pg-sql-function-test
  "`CREATE FUNCTION … LANGUAGE sql`, and what a call to one becomes.

   A call is INLINED, which is what PostgreSQL does too: `inline_function`
   in optimizer/util/clauses.c substitutes the arguments into the body's
   expression and never reaches the SQL-function executor. Inlining is
   not an optimisation here, it is the only option -- a statement is
   lowered to ONE Datalog query, so a per-row callout to a nested
   executor has nowhere to live.

   Where PostgreSQL substitutes the argument's AST, this binds the
   argument's TRANSLATED value. An argument used twice in the body is
   therefore evaluated once, which is why `vdiff(random())` is zero here
   and why PostgreSQL has to refuse to inline that case.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
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

(defn- err [^PgWireServer$QueryResult r] (some-> (.error r) str))

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (err r)))
    r))

(defn- release! [{:keys [conn cfg]}]
  (d/release conn)
  (d/delete-database cfg))

(defmacro with-h
  "A handler on a fresh in-memory database, released afterwards."
  [[sym init] & body]
  `(let [~sym ~init]
     (try ~@body (finally (release! ~sym)))))

(def ^:private dq "$$")

(defn- create-fn [name args ret body & [attrs]]
  (str "CREATE FUNCTION " name "(" args ") RETURNS " ret
       " AS " dq body dq " LANGUAGE sql " (or attrs "")))

;; ============================================================================
;; The definition
;; ============================================================================

(deftest create-and-drop
  (with-h [h (fresh-handler)]
    (ok! h (create-fn "iadd" "a int, b int" "int" "SELECT a + b"))
    (testing "a second definition of the same name and arity is 42723"
      (let [r (exec h (create-fn "iadd" "a int, b int" "int" "SELECT a + b"))]
        (is (some? (.error r)))
        (is (str/includes? (err r) "already exists"))))
    (testing "OR REPLACE replaces the body"
      (is (= [["5"]] (rows (ok! h "SELECT iadd(2,3)"))))
      (ok! h (str "CREATE OR REPLACE FUNCTION iadd(a int, b int) RETURNS int AS "
                  dq "SELECT a + b + 100" dq " LANGUAGE sql"))
      (is (= [["105"]] (rows (ok! h "SELECT iadd(2,3)")))))
    (testing "DROP FUNCTION, and the 42883 it raises for a missing one"
      (is (= "DROP FUNCTION" (.commandTag (ok! h "DROP FUNCTION iadd(int, int)"))))
      (let [r (exec h "DROP FUNCTION iadd(int, int)")]
        (is (some? (.error r)))
        ;; PostgreSQL names the resolved argument types, so `int` prints
        ;; as `integer`.
        (is (str/includes? (err r) "function iadd(integer, integer) does not exist")))
      (ok! h "DROP FUNCTION IF EXISTS iadd(int, int)"))
    (testing "a function of no arguments"
      (ok! h (create-fn "one" "" "int" "SELECT 1"))
      (is (= [["1"]] (rows (ok! h "SELECT one()"))))
      (ok! h "DROP FUNCTION one()"))))

;; ============================================================================
;; The call
;; ============================================================================

(deftest a-call-is-inlined
  (with-h [h (fresh-handler)]
    (ok! h (create-fn "iadd" "a int, b int" "int" "SELECT a + b"))
    (ok! h (create-fn "dbl" "x int" "int" "SELECT $1 * 2"))
    (testing "by parameter name and by position"
      (is (= [["5"]] (rows (ok! h "SELECT iadd(2,3)"))))
      (is (= [["42"]] (rows (ok! h "SELECT dbl(21)")))))
    (testing "in a larger expression, and nested"
      (is (= [["6"]] (rows (ok! h "SELECT iadd(2,3) + 1"))))
      (is (= [["7"]] (rows (ok! h "SELECT iadd(iadd(1,2), 4)")))))
    (testing "NULL propagates through a non-strict body"
      (is (= [[nil]] (rows (ok! h "SELECT iadd(NULL, 3)")))))
    (testing "per row, in the select list and in WHERE"
      (ok! h "CREATE TABLE t (i int)")
      (ok! h "INSERT INTO t VALUES (1),(2),(3)")
      (is (= [["1" "11"] ["2" "12"] ["3" "13"]]
             (rows (ok! h "SELECT i, iadd(i,10) FROM t ORDER BY i"))))
      (is (= [["2"] ["3"]]
             (rows (ok! h "SELECT i FROM t WHERE iadd(i,0) > 1 ORDER BY i")))))
    (testing "the wrong number of arguments is 42883, not a mis-call"
      (let [r (exec h "SELECT iadd(1)")]
        (is (some? (.error r)))
        (is (str/includes? (err r) "does not exist"))))))

(deftest an-argument-used-twice-is-evaluated-once
  ;; PostgreSQL refuses to INLINE this (a repeated parameter whose
  ;; argument is volatile) and calls the function instead, which
  ;; evaluates the argument once. Binding the translated value gets the
  ;; same answer without the analysis.
  (with-h [h (fresh-handler)]
    (ok! h (create-fn "vdiff" "x float8" "float8" "SELECT $1 - $1"))
    (is (= [["0"]] (rows (ok! h "SELECT vdiff(random())"))))
    (is (= [["t"]] (rows (ok! h "SELECT vdiff(random()) = 0"))))))

;; ============================================================================
;; STRICT
;; ============================================================================

(deftest strict-answers-null-for-a-null-argument
  (with-h [h (fresh-handler)]
    (testing "a body that ignores its parameter still answers NULL"
      ;; Inlining alone would answer 'const'. This is the case
      ;; PostgreSQL's inline_function refuses over ("must use all of the
      ;; function parameters").
      (ok! h (create-fn "sconst" "x int" "text" "SELECT 'const'" "STRICT"))
      (is (= [[nil]] (rows (ok! h "SELECT sconst(NULL)"))))
      (is (= [["const"]] (rows (ok! h "SELECT sconst(1)")))))
    (testing "a non-strict construct in a strict body"
      ;; The other half of inline_function's refusal
      ;; ("contain_nonstrict_functions"). We inline it and guard.
      (ok! h (create-fn "scoal" "x int" "int" "SELECT coalesce($1, 99)" "STRICT"))
      (is (= [[nil]] (rows (ok! h "SELECT scoal(NULL)"))))
      (is (= [["5"]] (rows (ok! h "SELECT scoal(5)")))))
    (testing "without STRICT the same body sees the NULL"
      (ok! h (create-fn "ncoal" "x int" "int" "SELECT coalesce($1, 99)"))
      (is (= [["99"]] (rows (ok! h "SELECT ncoal(NULL)")))))
    (testing "RETURNS NULL ON NULL INPUT is STRICT spelled out"
      (ok! h (create-fn "sspelled" "x int" "text" "SELECT 'c'"
                        "RETURNS NULL ON NULL INPUT"))
      (is (= [[nil]] (rows (ok! h "SELECT sspelled(NULL)")))))
    (testing "CALLED ON NULL INPUT is the default"
      (ok! h (create-fn "called" "x int" "text" "SELECT 'c'" "CALLED ON NULL INPUT"))
      (is (= [["c"]] (rows (ok! h "SELECT called(NULL)")))))))

;; ============================================================================
;; A body reading from a table
;; ============================================================================

(deftest a-body-with-a-from-clause
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE vt (i int)")
    (ok! h "INSERT INTO vt VALUES (7),(9)")
    (testing "with no arguments it splices whole, as a scalar subquery"
      (ok! h (create-fn "topi" "" "int" "SELECT max(i) FROM vt"))
      (is (= [["9"]] (rows (ok! h "SELECT topi()")))))
    (testing "with arguments it is refused, cleanly"
      ;; The subquery is translated by a runtime closure that runs
      ;; outside the dynamic binding holding the arguments, so they
      ;; would have to be substituted into the AST instead. Until then
      ;; this is 0A000, never a wrong answer.
      (ok! h (create-fn "above" "n int" "int" "SELECT count(*) FROM vt WHERE i > n"))
      (let [r (exec h "SELECT above(8)")]
        (is (some? (.error r)))
        (is (str/includes? (err r) "cannot inline"))))))

(deftest a-recursive-function-is-refused
  (with-h [h (fresh-handler)]
    (ok! h (create-fn "rec1" "n int" "int" "SELECT rec1(n)"))
    (let [r (exec h "SELECT rec1(1)")]
      (is (some? (.error r)))
      ;; PostgreSQL reaches 54001 stack depth exceeded; we decline to
      ;; expand rather than recursing until the JVM stack goes.
      (is (str/includes? (err r) "recursive")))))

(deftest a-user-function-does-not-shadow-a-builtin
  (with-h [h (fresh-handler)]
    ;; Resolution is on name and arity, which cannot tell overloads
    ;; apart the way PostgreSQL's type-based resolution does. So the
    ;; registry is consulted LAST and nothing built in can be captured.
    (ok! h (create-fn "upper" "x text" "text" "SELECT 'captured'"))
    (is (= [["AB"]] (rows (ok! h "SELECT upper('ab')"))))))

;; ============================================================================
;; Overloading
;; ============================================================================

(deftest two-overloads-of-the-same-arity
  ;; PostgreSQL overloads on argument TYPES. Keying the registry on name
  ;; and arity alone made `dfn(int,int)` and `dfn(text,text)` collide,
  ;; and a call then inlined whichever body happened to be stored:
  ;; `dfn('Hi','City')` ran `$1 + $2` over two strings and surfaced a raw
  ;; ClassCastException. PostgreSQL's own `polymorphism` test is what
  ;; caught it.
  (with-h [h (fresh-handler)]
    (ok! h (create-fn "dfn" "a int, b int" "int" "select $1 + $2"))
    (ok! h (create-fn "dfn" "a text, b text" "text" "select $1 || ', ' || $2"))
    (testing "each call reaches the overload whose types it fits"
      (is (= [["30"]] (rows (ok! h "SELECT dfn(10, 20)"))))
      ;; Both arguments are untyped literals, so both overloads could
      ;; take them; PostgreSQL resolves unknown to text.
      (is (= [["Hi, City"]] (rows (ok! h "SELECT dfn('Hi', 'City')")))))
    (testing "dropping one leaves the other resolvable"
      (ok! h "DROP FUNCTION dfn(int, int)")
      (is (= [["a, b"]] (rows (ok! h "SELECT dfn('a','b')"))))
      (ok! h "DROP FUNCTION dfn(text, text)")
      (is (some? (.error (exec h "SELECT dfn('a','b')")))))))

(deftest a-signature-that-already-exists-is-42723
  (with-h [h (fresh-handler)]
    (ok! h (create-fn "ex1" "a int" "int" "SELECT $1"))
    (let [r (exec h (create-fn "ex1" "a int" "int" "SELECT $1 + 1"))]
      (is (str/includes? (err r) "already exists")))
    (testing "a different argument type at the same arity is a new function"
      (ok! h (create-fn "ex1" "a text" "text" "SELECT $1")))))

;; ============================================================================
;; Set-returning functions as relations
;; ============================================================================

(deftest a-setof-function-is-a-relation
  ;; `FROM f(args)` is rewritten into the derived table it means, which
  ;; is what PostgreSQL's `inline_set_returning_function` does: it pulls
  ;; the body into the range table as a subquery. The arguments are
  ;; substituted into the body's source, because a FROM-clause
  ;; function's arguments are evaluated once, before its rows exist.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int, s text)")
    (ok! h "INSERT INTO t VALUES (1,'a'),(2,'b'),(3,'c')")
    (testing "RETURNS SETOF <scalar> — one column, named for the call site"
      (ok! h (str "CREATE FUNCTION above(m int) RETURNS SETOF int AS "
                  dq "SELECT i FROM t WHERE i > m ORDER BY i" dq " LANGUAGE sql"))
      (let [r (ok! h "SELECT * FROM above(1) ORDER BY 1")]
        (is (= [["2"] ["3"]] (rows r)))
        (is (= ["above"] (vec (.columnNames r)))))
      (testing "an alias renames it"
        (is (= ["z"] (vec (.columnNames (ok! h "SELECT * FROM above(1) AS z ORDER BY 1"))))))
      (is (= [["3"]] (rows (ok! h "SELECT count(*) FROM above(0)")))))
    (testing "RETURNS TABLE (…) — the declared names, not the body's"
      (ok! h (str "CREATE FUNCTION pairs(m int) RETURNS TABLE (a int, b text) AS "
                  dq "SELECT i, s FROM t WHERE i >= m ORDER BY i" dq " LANGUAGE sql"))
      (let [r (ok! h "SELECT * FROM pairs(2) ORDER BY a")]
        (is (= [["2" "b"] ["3" "c"]] (rows r)))
        (is (= ["a" "b"] (vec (.columnNames r)))))
      (is (= [["2"] ["3"]] (rows (ok! h "SELECT a FROM pairs(2) ORDER BY a")))))
    (testing "RETURNS SETOF <table> keeps the table's own column names"
      (ok! h (str "CREATE FUNCTION allrows() RETURNS SETOF t AS "
                  dq "SELECT * FROM t ORDER BY i" dq " LANGUAGE sql"))
      (let [r (ok! h "SELECT * FROM allrows() ORDER BY i")]
        (is (= [["1" "a"] ["2" "b"] ["3" "c"]] (rows r)))
        (is (= ["i" "s"] (vec (.columnNames r))))))
    (testing "joined to a table, not only as the sole FROM item"
      (is (= [["2" "2"] ["3" "3"]]
             (rows (ok! h "SELECT x.i, z.a FROM t x JOIN pairs(2) z ON z.a = x.i ORDER BY 1")))))
    (testing "an expression argument is substituted, parenthesised"
      (is (= [["3"]] (rows (ok! h "SELECT * FROM above(1 + 1) ORDER BY 1")))))))

(deftest an-argument-that-cannot-be-duplicated-is-refused
  ;; The FROM rewrite substitutes the argument's SOURCE, so a parameter
  ;; used twice writes the argument twice. That is safe for a literal or
  ;; a column and not for anything that would then be evaluated twice --
  ;; the hazard `inline_function` declines over.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE t (i int)")
    (ok! h "INSERT INTO t VALUES (1),(2),(3)")
    (ok! h (str "CREATE FUNCTION twice(m int) RETURNS SETOF int AS "
                dq "SELECT i FROM t WHERE i > m AND i > m" dq " LANGUAGE sql"))
    (testing "a literal is fine to write twice"
      (is (= [["2"] ["3"]] (rows (ok! h "SELECT * FROM twice(1) ORDER BY 1")))))
    (testing "a subquery is not"
      (let [r (exec h "SELECT * FROM twice((SELECT min(i) FROM t))")]
        (is (some? (.error r)))
        (is (str/includes? (err r) "SQL function twice as a relation"))))))
