(ns datahike.test.pg-null-where-test
  "`NOT` in a WHERE clause, and the UNKNOWN rows it must drop.

   PostgreSQL has no three-valued evaluator. A comparison with a NULL
   operand yields UNKNOWN, BoolExpr combines UNKNOWN by the Kleene
   tables, and the qual boundary collapses \"not TRUE\" to \"reject\"
   (execExprInterp.c, EEOP_QUAL). So `WHERE NOT p` keeps exactly the rows
   where p is FALSE -- never the rows where p is UNKNOWN.

   A datalog `(not <goal>)` is set COMPLEMENT: it keeps the FALSE rows
   AND the UNKNOWN rows. That difference -- exactly the UNKNOWN set -- is
   what these tests pin. It was not a cosmetic divergence:

     DELETE FROM up WHERE NOT (a = 10 AND b = 20)

   deleted every row whose a or b was NULL; PostgreSQL deletes none of
   them. So the bug silently destroyed data.

   The translation now descends into the negated expression and builds
   its FALSE-set directly (translate-predicate-false), rather than
   complementing its TRUE-set.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn nw-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"nw" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each nw-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/nw?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- ids
  "The id column of every row the query returns, in order."
  [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (loop [acc []]
      (if (.next rs) (recur (conj acc (.getInt rs 1))) acc))))

(defn- seed!
  "Four rows covering every NULL pattern over two comparable columns:
   both present, left NULL, right NULL, both NULL."
  [^Connection c]
  (exec! c "CREATE TABLE tvl (id int, a int, b int, s text)")
  (exec! c (str "INSERT INTO tvl VALUES "
                "(1,10,20,'aa'),(2,NULL,20,'bb'),(3,10,NULL,NULL),(4,NULL,NULL,'dd')")))

(deftest not-over-a-conjunction-drops-unknown-rows
  (with-open [c (jdbc)]
    (seed! c)
    (testing "the shape that used to return every NULL row"
      ;; row 1 is TRUE -> excluded. Rows 2,3,4 are UNKNOWN, not FALSE,
      ;; because a conjunction with a NULL operand and no FALSE conjunct
      ;; is UNKNOWN. Nothing is FALSE, so nothing comes back.
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND b = 20) ORDER BY id"))))
    (testing "a FALSE conjunct makes the conjunction FALSE whatever the other operand is"
      ;; row 2: a IS NULL (UNKNOWN) but s = 'bb' <> 'aa' is FALSE, and
      ;; UNKNOWN AND FALSE is FALSE. Row 4 likewise. Row 3's s IS NULL,
      ;; so both conjuncts are UNKNOWN.
      (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND s = 'aa') ORDER BY id"))))
    (testing "three conjuncts"
      (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND b = 20 AND s = 'aa') ORDER BY id"))))
    (testing "a repeated conjunct is not a special case"
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND a = 10) ORDER BY id"))))))

(deftest not-over-a-disjunction
  (with-open [c (jdbc)]
    (seed! c)
    ;; NOT (x OR y) is TRUE only when both are FALSE, so both operands
    ;; must be non-NULL. Row 1 has a=10 TRUE; rows 2,3,4 are UNKNOWN.
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 OR b = 20) ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (s = 'aa' OR s = 'cc') ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT ((a = 10 AND b = 20) OR s = 'dd') ORDER BY id"))
        "row 2 is UNKNOWN OR FALSE = UNKNOWN, so it is not in the FALSE set either")))

(deftest not-over-a-single-comparison
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10) ORDER BY id"))
        "a IS NULL is UNKNOWN, not FALSE")
    (is (= [1 3] (ids c "SELECT id FROM tvl WHERE NOT (a <> 10) ORDER BY id")))
    (is (= [1 3] (ids c "SELECT id FROM tvl WHERE NOT NOT (a = 10) ORDER BY id"))
        "double negation is the identity")))

(deftest not-over-two-valued-tests-keeps-every-row
  (with-open [c (jdbc)]
    (seed! c)
    ;; IS NULL / IS NOT NULL are never UNKNOWN, so NOT over them is plain
    ;; complement -- and must NOT acquire a null guard, or it would drop
    ;; exactly the rows it is asked to keep.
    (is (= [1 3] (ids c "SELECT id FROM tvl WHERE NOT (a IS NULL) ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (a IS NOT NULL) ORDER BY id")))
    (is (= [2] (ids c "SELECT id FROM tvl WHERE NOT (a IS NOT NULL) AND b IS NOT NULL ORDER BY id"))
        "mixing a 2-valued NOT with another predicate")))

(deftest not-over-a-computed-operand
  (with-open [c (jdbc)]
    (seed! c)
    (testing "SQL operators and functions are strict: the result is NULL if an input is"
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a + b = 30) ORDER BY id")))
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a + 1 = 11) ORDER BY id")))
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a * b > 100) ORDER BY id")))
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (abs(a) = 10) ORDER BY id")))
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a::text = '10') ORDER BY id")))
      (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (upper(s) = 'AA') ORDER BY id")))
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (length(s) = 2) ORDER BY id"))
          "row 4's s is 'dd', length 2 -> TRUE; row 3's is NULL -> UNKNOWN"))
    (testing "a computed operand mixed into a conjunction"
      (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND b + 1 = 21) ORDER BY id")))
      (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (upper(s) = 'AA' AND a = 10) ORDER BY id"))))
    (testing "coalesce is not strict, so its result is never UNKNOWN"
      (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (coalesce(a, 0) = 10) ORDER BY id"))))))

(deftest inequality-drops-unknown-rows-without-any-not
  (with-open [c (jdbc)]
    (seed! c)
    ;; `<>` was `(not (sql-eq? a b))`, and sql-eq? answers false for a
    ;; NULL operand -- so negating it turned UNKNOWN into TRUE. No `NOT`
    ;; keyword is involved; the bug was in the operator itself.
    (is (= [] (ids c "SELECT id FROM tvl WHERE a + b <> 30 ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE upper(s) <> 'AA' ORDER BY id")))
    (is (= [1 3] (ids c "SELECT id FROM tvl WHERE a <> 99 ORDER BY id"))
        "the two non-NULL rows are TRUE; the NULL ones stay UNKNOWN")))

(deftest not-over-pattern-and-range-tests
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (s LIKE 'a%') ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE s NOT LIKE 'a%' ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a BETWEEN 1 AND 20) ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE a NOT BETWEEN 1 AND 20 ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a IN (10, 99)) ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE a NOT IN (10, 99) ORDER BY id")))))

(deftest aggregates-over-a-negated-qual
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "0" (one c "SELECT count(*) FROM tvl WHERE NOT (a = 10 AND b = 20)")))
    (is (= "2" (one c "SELECT count(*) FROM tvl WHERE NOT (a = 10 AND s = 'aa')")))))

(deftest delete-and-update-do-not-touch-unknown-rows
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE up (id int PRIMARY KEY, a int, b int)")
    (exec! c "INSERT INTO up VALUES (1,10,20),(2,NULL,20),(3,10,NULL)")
    (testing "DELETE -- the data-loss case"
      (exec! c "DELETE FROM up WHERE NOT (a = 10 AND b = 20)")
      (is (= [1 2 3] (ids c "SELECT id FROM up ORDER BY id"))
          "rows 2 and 3 are UNKNOWN, not FALSE: PostgreSQL deletes nothing"))
    (testing "UPDATE"
      (exec! c "UPDATE up SET b = 0 WHERE NOT (a = 10)")
      (is (= "20" (one c "SELECT b FROM up WHERE id = 2"))
          "row 2's a IS NULL -> UNKNOWN -> not updated"))))

(deftest conjunctions-whose-conjuncts-have-no-fast-form
  ;; A conjunction of plain binary comparisons collapses to ONE datalog
  ;; negation over `sql-may?` calls. Anything else -- LIKE, IN, BETWEEN,
  ;; IS NULL, EXISTS, a nested NOT -- has no such form, so the
  ;; translation rolls back its attempt and takes the De Morgan path
  ;; instead. Both paths must agree with PostgreSQL, and the rollback
  ;; must not leave the first attempt's binding clauses behind.
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND s LIKE 'a%') ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (s LIKE 'a%' AND a = 10) ORDER BY id"))
        "the un-fast conjunct first, so the rollback happens on the first item")
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND s IN ('aa','zz')) ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND b BETWEEN 1 AND 30) ORDER BY id")))
    (is (= [1 2] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND b IS NULL) ORDER BY id")))
    (is (= [1 2 3] (ids c "SELECT id FROM tvl WHERE NOT (a IS NULL AND b IS NULL) ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND s LIKE 'a%' AND b = 20) ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND (s LIKE 'a%' OR b = 20)) ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT ((a = 10 AND b = 20) AND s LIKE 'a%') ORDER BY id")))
    (is (= [2 4] (ids c "SELECT id FROM tvl WHERE NOT (upper(s) LIKE 'A%' AND a = 10) ORDER BY id")))
    (is (= [1 2] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND NOT (b = 20)) ORDER BY id")))
    (is (= [] (ids c (str "SELECT id FROM tvl WHERE NOT (a = 10 AND EXISTS "
                          "(SELECT 1 FROM tvl t2 WHERE t2.id = 1)) ORDER BY id"))))
    (is (= "2" (one c "SELECT count(*) FROM tvl WHERE NOT (a = 10 AND s LIKE 'a%')")))))

(deftest a-negated-conjunction-composed-with-other-quals
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [] (ids c "SELECT id FROM tvl WHERE NOT (a = 10 AND b = 20) AND s IS NOT NULL ORDER BY id")))
    (is (= [] (ids c "SELECT id FROM tvl WHERE s IS NOT NULL AND NOT (a = 10 AND b = 20) ORDER BY id"))
        "order of the surrounding conjuncts does not matter")))
