(ns datahike.test.pg-column-naming-test
  "Output column names — the RowDescription field name for a SELECT item.

   We used to answer with whatever was nearest to hand: the Datalog
   variable the projection compiled to (`p1`, `v1`, `v2`), or the
   expression's own SQL text (`B'1001000'`, `B'101'::varbit`, `$1#`).
   No PostgreSQL client would ever see those, and anything that keys
   results by column name — every ORM, every `row['name']` access —
   reads them.

   PostgreSQL's rule is `FigureColname` (parse_target.c). It is not
   'the expression text' and not 'always ?column?'; it is a small set of
   node-specific rules with a two-level notion of confidence, which is
   what makes these three differ:

       a::text        -> a       (the column's good name survives)
       1::int8::text  -> text    (a cast's type name is second-best, so
                                  the outer cast overrides it)
       1::int         -> int4    (the INTERNAL type name, not `int`)

   Transcribed from parse_target.c and checked case by case against
   PostgreSQL 17."
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
          (.execute *handler* "CREATE TABLE nm (id int PRIMARY KEY, name text, n int)")
          (.execute *handler* "INSERT INTO nm VALUES (1, 'a', 10)")
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- cols [sql]
  (vec (.-columnNames ^PgWireServer$QueryResult (.execute *handler* sql))))

(defn- col [sql] (first (cols sql)))

;; ---------------------------------------------------------------------------
;; The default
;; ---------------------------------------------------------------------------

(deftest unnamed-expressions-are-column-placeholder
  (testing "literals of every kind"
    (is (= "?column?" (col "SELECT 1")))
    (is (= "?column?" (col "SELECT 1.5")))
    (is (= "?column?" (col "SELECT 'abc'")))
    (is (= "?column?" (col "SELECT true")))
    (is (= "?column?" (col "SELECT NULL")))
    (testing "including bit-string literals, which named themselves"
      (is (= "?column?" (col "SELECT B'1001000'")))
      (is (= "?column?" (col "SELECT X'4A'")))))
  (testing "operators — arithmetic, bitwise, concat"
    (is (= "?column?" (col "SELECT 1+1")))
    (is (= "?column?" (col "SELECT 5 & 3")))
    (is (= "?column?" (col "SELECT ~1")))
    (is (= "?column?" (col "SELECT 2 ^ 3")))
    (is (= "?column?" (col "SELECT 'a' || 'b'")))
    (is (= "?column?" (col "SELECT id+1 FROM nm"))))
  (testing "parens are not a node — `(1)` is just `1`"
    (is (= "?column?" (col "SELECT (1)")))))

;; ---------------------------------------------------------------------------
;; Explicit aliases
;; ---------------------------------------------------------------------------

(deftest explicit-alias-wins
  (is (= "foo" (col "SELECT 1 AS foo")))
  (is (= "ident" (col "SELECT id AS ident FROM nm"))))

(deftest unquoted-alias-is-down-cased
  (testing "PG's lexer folds every unquoted identifier"
    (is (= "foo" (col "SELECT 1 AS Foo")))
    (is (= "foo" (col "SELECT 1 AS FOO"))))
  (testing "a quoted alias keeps its case"
    (is (= "Foo" (col "SELECT 1 AS \"Foo\"")))))

;; ---------------------------------------------------------------------------
;; Columns and functions
;; ---------------------------------------------------------------------------

(deftest column-reference-uses-the-last-component
  (is (= "id" (col "SELECT id FROM nm")))
  (is (= "id" (col "SELECT nm.id FROM nm")) "never `nm.id`"))

(deftest function-call-uses-the-function-name
  (is (= "abs" (col "SELECT abs(-1)")))
  (is (= "upper" (col "SELECT upper(name) FROM nm")))
  (is (= "pg_typeof" (col "SELECT pg_typeof(1)")))
  (is (= "length" (col "SELECT length('ab')")))
  (testing "aggregates"
    (is (= "count" (col "SELECT count(*) FROM nm")))
    (is (= "sum" (col "SELECT sum(n) FROM nm")))
    (is (= "max" (col "SELECT max(n) FROM nm"))))
  (testing "the constructs PG special-cases to look like functions"
    (is (= "coalesce" (col "SELECT coalesce(1, 2)")))
    (is (= "greatest" (col "SELECT greatest(1,2)")))
    (is (= "least" (col "SELECT least(1,2)")))
    (is (= "nullif" (col "SELECT nullif(1,2)")))))

(deftest bare-keyword-value-functions
  (is (= "current_date" (col "SELECT current_date")))
  (is (= "current_timestamp" (col "SELECT current_timestamp")))
  (is (= "now" (col "SELECT now()"))))

;; ---------------------------------------------------------------------------
;; Casts — the interesting rule
;; ---------------------------------------------------------------------------

(deftest cast-names-use-the-internal-type-name
  (testing "`int` is spelled int4 in pg_type, and that is the name"
    (is (= "int4" (col "SELECT 1::int")))
    (is (= "int4" (col "SELECT CAST(1 AS int)")) "spelling of the cast is irrelevant")
    (is (= "int8" (col "SELECT 1::bigint")))
    (is (= "int2" (col "SELECT 1::smallint")))
    (is (= "varchar" (col "SELECT 'x'::character varying")))
    (is (= "bit" (col "SELECT B'101'::bit(3)")) "the length is not part of the name")
    (is (= "varbit" (col "SELECT B'101'::varbit"))))
  (testing "a type that is already its own internal name"
    (is (= "text" (col "SELECT 1::text")))))

(deftest cast-keeps-a-good-operand-name
  (testing "a column's name survives the cast"
    (is (= "id" (col "SELECT id::text FROM nm"))))
  (testing "but a cast's own type name is only second-best, so an outer
            cast replaces it"
    (is (= "text" (col "SELECT 1::int8::text")))))

;; ---------------------------------------------------------------------------
;; CASE, subqueries, constructors
;; ---------------------------------------------------------------------------

(deftest case-expressions
  (testing "`case` unless the ELSE arm supplies a good name"
    (is (= "case" (col "SELECT CASE WHEN true THEN 1 ELSE 2 END")))
    (is (= "case" (col "SELECT CASE WHEN true THEN 1 END")) "no ELSE at all")
    (is (= "case" (col "SELECT CASE WHEN n>0 THEN 1 ELSE 2 END FROM nm"))))
  (testing "an ELSE column reference is a good name and is inherited"
    (is (= "name" (col "SELECT CASE WHEN n>0 THEN 'x' ELSE name END FROM nm")))))

(deftest scalar-subquery-takes-its-inner-column-name
  (is (= "id" (col "SELECT (SELECT id FROM nm)")))
  (is (= "?column?" (col "SELECT (SELECT 1 FROM nm)"))
      "an unnamed inner column propagates the placeholder"))

(deftest array-constructor
  (is (= "array" (col "SELECT ARRAY[1,2]"))))

;; ---------------------------------------------------------------------------
;; Multi-column and set operations
;; ---------------------------------------------------------------------------

(deftest star-expands-to-the-tables-column-names
  (is (= ["id" "name" "n"] (cols "SELECT * FROM nm"))))

(deftest multiple-items-are-named-independently
  (is (= ["id" "name"] (cols "SELECT id, name FROM nm")))
  (is (= ["id" "?column?" "upper"] (cols "SELECT id, n+1, upper(name) FROM nm"))))

(deftest set-operations-take-the-leftmost-branch
  (is (= "a" (col "SELECT id AS a FROM nm UNION SELECT n AS b FROM nm"))
      "the right branch's alias is discarded"))
