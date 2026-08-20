(ns datahike.test.pg-null-value-test
  "SQL's three-valued logic in VALUE position -- projections, CASE tests,
   FILTER conditions, casts.

   The companion to pg-null-where-test, and the mirror image of it. In a
   WHERE clause PostgreSQL collapses UNKNOWN to \"reject\" at the qual
   boundary, so a comparison there may answer plain FALSE. Everywhere
   else it may not:

     SELECT a = 10 FROM t     -- a IS NULL  ->  NULL, not false
     SELECT true AND NULL     ->  NULL
     SELECT false AND NULL    ->  FALSE     (Kleene: FALSE dominates)

   NULL travels through this codebase as the `:__null__` sentinel rather
   than nil, because a datalog function binding that yields nil FILTERS
   THE ROW. The sentinel is TRUTHY, which is what made every one of these
   wrong in the same direction: `and`/`or`/`not`/`when` all read UNKNOWN
   as TRUE. So the three-valued operators are separate symbols from the
   two-valued WHERE ones, and the two places that must collapse UNKNOWN
   back to \"no\" -- a CASE test and a FILTER condition -- do it with an
   explicit `true?`.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn nv-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"nv" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each nv-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/nv?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one
  "First column of the first row, as text. nil means SQL NULL."
  [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col2
  "The second column of every row, in order, as text. nil = SQL NULL."
  [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (loop [acc []]
      (if (.next rs) (recur (conj acc (.getString rs 2))) acc))))

(defn- ids [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (loop [acc []]
      (if (.next rs) (recur (conj acc (.getInt rs 1))) acc))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE tvl (id int, a int, b int, s text)")
  (exec! c (str "INSERT INTO tvl VALUES "
                "(1,10,20,'aa'),(2,NULL,20,'bb'),(3,10,NULL,NULL),(4,NULL,NULL,'dd')"))
  ;; Every (f, g) pair over {true, false, NULL} -- the full Kleene table.
  (exec! c "CREATE TABLE bt (id int, f boolean, g boolean)")
  (exec! c (str "INSERT INTO bt VALUES (1,true,true),(2,true,false),(3,true,NULL),"
                "(4,false,true),(5,false,false),(6,false,NULL),"
                "(7,NULL,true),(8,NULL,false),(9,NULL,NULL)")))

(deftest null-literal-casts-to-null-not-to-its-own-name
  (with-open [c (jdbc)]
    ;; The table-free constant folder stringified the NULL literal, so the
    ;; cast then PARSED the text "NULL": `NULL::bool` raised "invalid input
    ;; syntax for type boolean" and `NULL::text` answered the STRING 'NULL'.
    (is (nil? (one c "SELECT NULL")))
    (is (nil? (one c "SELECT NULL::bool")))
    (is (nil? (one c "SELECT NULL::int")))
    (is (nil? (one c "SELECT NULL::text")))
    (is (= "t" (one c "SELECT NULL::bool IS NULL")))))

(deftest kleene-truth-tables-for-literals
  (with-open [c (jdbc)]
    (is (nil? (one c "SELECT NOT NULL::bool")) "negation cannot resolve an unknown")
    (testing "AND -- FALSE dominates"
      (is (nil? (one c "SELECT true AND NULL::bool")))
      (is (= "f" (one c "SELECT false AND NULL::bool"))
          "false whatever the unknown operand turns out to be")
      (is (nil? (one c "SELECT NULL::bool AND NULL::bool"))))
    (testing "OR -- TRUE dominates"
      (is (= "t" (one c "SELECT true OR NULL::bool")))
      (is (nil? (one c "SELECT false OR NULL::bool")))
      (is (nil? (one c "SELECT NULL::bool OR NULL::bool"))))))

(deftest kleene-truth-tables-over-columns
  (with-open [c (jdbc)]
    (seed! c)
    ;; Rows 1-9 are (t,t) (t,f) (t,N) (f,t) (f,f) (f,N) (N,t) (N,f) (N,N).
    (is (= ["t" "f" nil "f" "f" "f" nil "f" nil]
           (col2 c "SELECT id, f AND g FROM bt ORDER BY id")))
    (is (= ["t" "t" "t" "t" "f" nil "t" nil nil]
           (col2 c "SELECT id, f OR g FROM bt ORDER BY id")))
    (is (= ["f" "f" "f" "t" "t" "t" nil nil nil]
           (col2 c "SELECT id, NOT f FROM bt ORDER BY id")))))

(deftest comparisons-in-value-position-yield-null
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["t" nil "t" nil] (col2 c "SELECT id, a = 10 FROM tvl ORDER BY id")))
    (is (= ["f" nil "f" nil] (col2 c "SELECT id, a <> 10 FROM tvl ORDER BY id"))
        "`<>` was `not=` on the sentinel, which answered TRUE for a NULL")
    (is (= ["t" nil "t" nil] (col2 c "SELECT id, a > 5 FROM tvl ORDER BY id")))
    (is (= ["f" nil nil nil] (col2 c "SELECT id, a = b FROM tvl ORDER BY id")))
    (is (= ["f" nil "f" nil] (col2 c "SELECT id, NOT (a = 10) FROM tvl ORDER BY id")))
    (testing "IS NULL is 2-valued and must NOT become UNKNOWN"
      (is (= ["f" "t" "f" "t"] (col2 c "SELECT id, a IS NULL FROM tvl ORDER BY id"))))))

(deftest nested-boolean-expressions-in-a-projection
  (with-open [c (jdbc)]
    (seed! c)
    ;; Datahike rejects a nested form as a function argument, so these
    ;; used to fail outright; each sub-expression now gets its own clause.
    (is (= ["t" nil nil nil]
           (col2 c "SELECT id, (a = 10) AND (b = 20) FROM tvl ORDER BY id")))
    (is (= ["t" "t" "t" nil]
           (col2 c "SELECT id, (a = 10) OR (b = 20) FROM tvl ORDER BY id"))
        "row 2: UNKNOWN OR TRUE is TRUE")))

(deftest pattern-range-and-membership-in-value-position
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["t" "f" nil "f"] (col2 c "SELECT id, s LIKE 'a%' FROM tvl ORDER BY id")))
    (is (= ["t" nil "t" nil] (col2 c "SELECT id, a IN (10,99) FROM tvl ORDER BY id")))
    (is (= ["t" nil "t" nil] (col2 c "SELECT id, a BETWEEN 1 AND 20 FROM tvl ORDER BY id")))
    (testing "a NULL in the IN-list makes a MISS unknown, not false"
      ;; 3 might have equalled the unknown element, so the answer is NULL.
      (is (nil? (one c "SELECT 3 IN (1, NULL)")))
      (is (= "t" (one c "SELECT 1 IN (1, NULL)")) "a hit is TRUE regardless"))))

(deftest case-takes-a-branch-only-when-its-test-is-true
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "y" (one c "SELECT CASE WHEN true THEN 'y' ELSE 'n' END")))
    (is (= "n" (one c "SELECT CASE WHEN false THEN 'y' ELSE 'n' END")))
    (is (= "n" (one c "SELECT CASE WHEN NULL::bool THEN 'y' ELSE 'n' END"))
        "UNKNOWN is not TRUE; the sentinel is truthy, so this took the branch")
    (is (= ["y" "n" "y" "n"]
           (col2 c "SELECT id, CASE WHEN a = 10 THEN 'y' ELSE 'n' END FROM tvl ORDER BY id")))
    (is (= ["y" "y" "y" "n" "n" "n" "n" "n" "n"]
           (col2 c "SELECT id, CASE WHEN f THEN 'y' ELSE 'n' END FROM bt ORDER BY id")))
    (testing "no ELSE means NULL"
      (is (= ["y" "y" "y" nil nil nil nil nil nil]
             (col2 c "SELECT id, CASE WHEN f THEN 'y' END FROM bt ORDER BY id"))))))

(deftest case-returns-a-matched-branch-even-when-its-value-is-falsy
  (with-open [c (jdbc)]
    ;; `(or (some …) else)` could not distinguish "no branch matched" from
    ;; "a branch matched and produced FALSE", so the ELSE won.
    (is (= "f" (one c "SELECT CASE WHEN true THEN false ELSE true END")))
    (is (nil? (one c "SELECT CASE WHEN true THEN NULL::int ELSE 1 END")))
    (is (= "0" (one c "SELECT CASE WHEN true THEN 0 ELSE 1 END")))))

(deftest case-short-circuits
  (with-open [c (jdbc)]
    ;; Only the taken branch is evaluated -- so a division by zero in an
    ;; untaken branch must not raise. Fixing the falsy-branch bug above by
    ;; seeding a reduce with the ELSE value broke exactly this.
    (is (= "1" (one c "SELECT CASE WHEN 1=1 THEN 1 ELSE 2/0 END")))
    (is (= "42" (one c "SELECT CASE WHEN 1=0 THEN 1/0 WHEN 1=1 THEN 42 ELSE 2/0 END")))))

(deftest filter-counts-a-row-only-when-its-condition-is-true
  (with-open [c (jdbc)]
    (seed! c)
    (is (= "2" (one c "SELECT count(*) FILTER (WHERE a = 10) FROM tvl"))
        "the two NULL rows are UNKNOWN, not TRUE")
    (is (= "2" (one c "SELECT count(*) FILTER (WHERE a IS NULL) FROM tvl")))))

(deftest boolean-tests-are-two-valued
  (with-open [c (jdbc)]
    (seed! c)
    (testing "value position"
      (is (= ["t" "t" "t" "f" "f" "f" "f" "f" "f"]
             (col2 c "SELECT id, f IS TRUE FROM bt ORDER BY id")))
      (is (= ["f" "f" "f" "t" "t" "t" "t" "t" "t"]
             (col2 c "SELECT id, f IS NOT TRUE FROM bt ORDER BY id")))
      (is (= ["f" "f" "f" "t" "t" "t" "f" "f" "f"]
             (col2 c "SELECT id, f IS FALSE FROM bt ORDER BY id")))
      (is (= ["t" "t" "t" "f" "f" "f" "t" "t" "t"]
             (col2 c "SELECT id, f IS NOT FALSE FROM bt ORDER BY id")))
      (is (= ["f" "f" "f" "f" "f" "f" "t" "t" "t"]
             (col2 c "SELECT id, f IS UNKNOWN FROM bt ORDER BY id")))
      (is (= ["t" "t" "t" "t" "t" "t" "f" "f" "f"]
             (col2 c "SELECT id, f IS NOT UNKNOWN FROM bt ORDER BY id"))))
    (testing "WHERE position"
      (is (= [1 2 3] (ids c "SELECT id FROM bt WHERE f IS TRUE ORDER BY id")))
      (is (= [4 5 6 7 8 9] (ids c "SELECT id FROM bt WHERE f IS NOT TRUE ORDER BY id")))
      (is (= [4 5 6] (ids c "SELECT id FROM bt WHERE f IS FALSE ORDER BY id")))
      (is (= [7 8 9] (ids c "SELECT id FROM bt WHERE f IS UNKNOWN ORDER BY id"))))))

(deftest is-distinct-from-is-the-null-aware-inequality
  (with-open [c (jdbc)]
    (seed! c)
    (testing "value position"
      (is (= ["f" "t" "f" "t"] (col2 c "SELECT id, a IS DISTINCT FROM 10 FROM tvl ORDER BY id")))
      (is (= ["t" "f" "t" "f"] (col2 c "SELECT id, a IS NOT DISTINCT FROM 10 FROM tvl ORDER BY id"))))
    (testing "WHERE position"
      (is (= [2 4] (ids c "SELECT id FROM tvl WHERE a IS DISTINCT FROM 10 ORDER BY id")))
      (is (= [2 4] (ids c "SELECT id FROM tvl WHERE a IS NOT DISTINCT FROM NULL ORDER BY id"))
          "two NULLs are NOT distinct"))))

(deftest greatest-and-least-skip-nulls
  (with-open [c (jdbc)]
    (seed! c)
    ;; PostgreSQL compiles these to a MinMaxExpr, which is NOT strict --
    ;; almost alone among SQL functions. A blanket null-safe wrapper over
    ;; the function table short-circuited them to NULL.
    (is (= "5" (one c "SELECT greatest(NULL, 5)")))
    (is (= "5" (one c "SELECT least(NULL, 5)")))
    (is (nil? (one c "SELECT greatest(NULL::int, NULL)")) "NULL only when every input is")
    (is (= ["10" "5" "10" "5"] (col2 c "SELECT id, greatest(a, 5) FROM tvl ORDER BY id")))
    (is (= ["5" "5" "5" "5"] (col2 c "SELECT id, least(a, 5) FROM tvl ORDER BY id")))))

(deftest casting-null-yields-null-and-keeps-the-row
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE cn (id int, n int, t text, d date, f float8)")
    (exec! c (str "INSERT INTO cn VALUES (1,10,'20','2020-01-01',1.5),"
                  "(2,NULL,NULL,NULL,NULL)"))
    ;; A runtime cast returning nil DROPS THE ROW -- a datalog binding that
    ;; yields nil filters -- so `SELECT n::int FROM cn` returned one row
    ;; where PostgreSQL returns two. The temporal casts had the opposite
    ;; bug: they only guarded with `(when v …)`, and the sentinel is
    ;; truthy, so they fell through to their string parser and emitted the
    ;; literal text ":__null__" to the client.
    (is (= ["10" nil] (col2 c "SELECT id, n::int FROM cn ORDER BY id")))
    (is (= ["10" nil] (col2 c "SELECT id, n::text FROM cn ORDER BY id")))
    (is (= ["10" nil] (col2 c "SELECT id, n::float8 FROM cn ORDER BY id")))
    (is (= ["10" nil] (col2 c "SELECT id, n::numeric FROM cn ORDER BY id")))
    (is (= ["20" nil] (col2 c "SELECT id, t::int FROM cn ORDER BY id")))
    (is (= ["11" nil] (col2 c "SELECT id, (n + 1)::int FROM cn ORDER BY id")))
    (is (= ["2" nil] (col2 c "SELECT id, f::int FROM cn ORDER BY id")))
    (is (= ["2020-01-01" nil] (col2 c "SELECT id, d::text FROM cn ORDER BY id")))
    (is (= ["2020-01-01 00:00:00" nil]
           (col2 c "SELECT id, d::timestamp FROM cn ORDER BY id")))))

(deftest a-projected-boolean-can-be-cast
  (with-open [c (jdbc)]
    (seed! c)
    ;; true -> 1, false -> 0, and UNKNOWN stays NULL. cast-to-integer had
    ;; no Boolean case, so this raised "cannot coerce class
    ;; java.lang.Boolean to bigint".
    (is (= ["1" nil "1" nil] (col2 c "SELECT id, (a = 10)::int FROM tvl ORDER BY id")))
    (is (= "1" (one c "SELECT true::int")))
    (is (= "0" (one c "SELECT false::int")))))

(deftest coalesce-collapses-unknown
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["t" "f" "t" "f"]
           (col2 c "SELECT id, coalesce(a = 10, false) FROM tvl ORDER BY id"))
        "the comparison is UNKNOWN for the NULL rows, and coalesce sees it as NULL")))
