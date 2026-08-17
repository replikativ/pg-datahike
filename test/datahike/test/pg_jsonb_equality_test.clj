(ns datahike.test.pg-jsonb-equality-test
  "PostgreSQL's jsonb `=` compares VALUES and is numeric-scale
   INSENSITIVE: `'1.00'::jsonb = '1'::jsonb` is TRUE even though the two
   render differently, because jsonb keeps display scale on purpose
   while `numeric_eq` ignores it.

   `jsonb-eq?` was already written and correct, but was reachable from
   exactly one place — the WHERE path taken when the right operand is a
   BARE literal. So `j = '1'` was right while `j = '1'::jsonb`,
   `'1'::jsonb = j`, `a.j = b.j` and a comparison in the SELECT list all
   lowered to `=` on the canonical text and answered false.

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
  (run "CREATE TABLE je (id int, j jsonb)")
  ;; 1.00 and 1 are the SAME jsonb value; {"a":1.0} and {"a":1} likewise.
  (run "INSERT INTO je VALUES (1,'1.00'),(2,'1'),(3,'{\"a\":1.0}'),(4,'{\"a\":1}')"))

(deftest scale-insensitive-equality-in-every-operand-shape
  (seed!)
  (testing "bare literal on the right — the one shape that already worked"
    (is (= "2" (v "SELECT count(*) FROM je WHERE j = '1'"))))

  (testing "an explicit ::jsonb cast, which is a CastExpression not a literal"
    (is (= "2" (v "SELECT count(*) FROM je WHERE j = '1'::jsonb")))
    (is (= "2" (v "SELECT count(*) FROM je WHERE j = '{\"a\":1}'::jsonb"))))

  (testing "reversed operand order"
    (is (= "2" (v "SELECT count(*) FROM je WHERE '1'::jsonb = j"))))

  (testing "<> is the complement, not not= on the text"
    (is (= "2" (v "SELECT count(*) FROM je WHERE j <> '1'::jsonb")))))

(deftest equality-in-value-position
  (seed!)
  (testing "two literals in the select list"
    (is (= "t" (v "SELECT '1.00'::jsonb = '1'::jsonb")))
    (is (= "f" (v "SELECT '1.00'::jsonb <> '1'::jsonb")))
    (is (= "t" (v "SELECT '{\"a\":1.0}'::jsonb = '{\"a\":1}'::jsonb"))))

  (testing "a column compared in the select list"
    (is (= [["t"] ["t"] ["f"] ["f"]]
           (rows "SELECT j = '1'::jsonb FROM je ORDER BY id")))))

(deftest joins-on-jsonb-keys
  (seed!)
  ;; An equi-join must NOT unify on a shared logic var here: that makes
  ;; the join key text equality, so 1.00 and 1 fail to match. Both rows
  ;; of each equal pair join both ways -> 2*2 + 2*2 = 8.
  (testing "explicit JOIN ON"
    (is (= "8" (v "SELECT count(*) FROM je a JOIN je b ON a.j = b.j"))))
  (testing "implicit join in WHERE"
    (is (= "8" (v "SELECT count(*) FROM je a, je b WHERE a.j = b.j")))))

(deftest text-equality-remains-the-fast-path
  (seed!)
  (testing "identical canonical text still compares equal"
    (is (= "t" (v "SELECT '{\"a\":1}'::jsonb = '{\"a\":1}'::jsonb"))))
  (testing "genuinely different values are still unequal"
    (is (= "f" (v "SELECT '1'::jsonb = '2'::jsonb")))
    (is (= "f" (v "SELECT '{\"a\":1}'::jsonb = '{\"b\":1}'::jsonb")))
    (is (= "0" (v "SELECT count(*) FROM je WHERE j = '9'::jsonb")))))

(deftest non-jsonb-columns-are-untouched
  (run "CREATE TABLE tt (id int, s text, n numeric)")
  (run "INSERT INTO tt VALUES (1,'a',1.00),(2,'b',1)")
  (testing "text equality is still plain ="
    (is (= "1" (v "SELECT count(*) FROM tt WHERE s = 'a'")))
    (is (= "1" (v "SELECT count(*) FROM tt WHERE s <> 'a'"))))
  (testing "an equi-join on a non-jsonb key still unifies"
    (is (= "2" (v "SELECT count(*) FROM tt a JOIN tt b ON a.id = b.id")))))
