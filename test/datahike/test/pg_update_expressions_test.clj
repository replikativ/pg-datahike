(ns datahike.test.pg-update-expressions-test
  "Expressions in `UPDATE … SET`, and the two fallbacks that silently
   corrupted data.

   `eval-update-expr` is a SEPARATE evaluator from the one the SELECT path
   uses -- it runs per ENTITY against a materialised entity-map rather than
   as a datalog clause over the relation -- and it knew 22 AST node types
   where `translate-expr` knows 51. Both of its fallbacks turned the
   unknown remainder into data rather than an error:

     :else (str value-expr)   ->  the SQL SOURCE TEXT went into the column
     eval-update-cond's :else ->  truthiness of that text, so every
                                  unrecognised predicate was TRUE

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn ue-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"ue" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each ue-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/ue?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- update-count [^Connection c sql]
  (with-open [st (.createStatement c)] (.executeUpdate st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv (fn [^long ix] (.getString rs ix)) (range 1 (inc n)))))
          acc)))))

(defn- fresh! [^Connection c]
  (exec! c "DROP TABLE IF EXISTS w")
  (exec! c "CREATE TABLE w (id int, n int, s text, b boolean, a int[], j jsonb, d date)")
  (exec! c (str "INSERT INTO w VALUES "
                "(1,10,'aa',true,'{1,2}','{\"k\":1}','2020-01-01'),"
                "(2,NULL,NULL,NULL,NULL,NULL,NULL)")))

(defn- col-after
  "Run `sql`, then read column `n` of both rows."
  [^Connection c n sql]
  (fresh! c)
  (exec! c sql)
  (mapv #(nth % (dec n)) (rows c "SELECT id, n, s, b, a, j, d FROM w ORDER BY id")))

(deftest unknown-expressions-no-longer-become-column-data
  (with-open [c (jdbc)]
    ;; Each of these previously wrote the SQL source text of the expression
    ;; into the column, with no error raised.
    (is (= ["{7,8}" "{7,8}"] (col-after c 5 "UPDATE w SET a = ARRAY[7,8]"))
        "was the string \"ARRAY[7, 8]\" in an int[] column")
    (is (= ["" nil] (col-after c 3 "UPDATE w SET s = trim(BOTH 'a' FROM s)"))
        "was \"Trim( BOTH 'a' FROM s )\"; trimming 'a' from 'aa' leaves the EMPTY string")
    (is (= ["true" nil] (col-after c 3 "UPDATE w SET s = (n IN (10,20))::text"))
        "was \"n IN (10, 20)\"")
    (is (= ["false" "true"] (col-after c 3 "UPDATE w SET s = (s IS NULL)::text"))
        "was \"s IS NULL\"")))

(deftest expressions-that-were-silent-no-ops
  (with-open [c (jdbc)]
    ;; These raised nothing AND changed nothing: the stringified text failed
    ;; the column's coercion and the old value simply stayed.
    (is (= ["1" "1"] (col-after c 2 "UPDATE w SET n = 7 % 3")))
    (is (= ["2020" nil] (col-after c 2 "UPDATE w SET n = extract(year FROM d)")))
    (is (= ["1" nil] (col-after c 2 "UPDATE w SET n = a[1]"))
        "`a[1]` parses as a Column carrying an array constructor")
    (is (= ["a" nil] (col-after c 3 "UPDATE w SET s = substring(s from 1 for 1)"))
        "the SQL keyword call form leaves .getParameters empty")))

(deftest date-arithmetic-shifts-days
  (with-open [c (jdbc)]
    ;; A date column is a java.util.Date, which the numeric path turned into
    ;; nothing -- so `d + 1` WIPED the column instead of advancing it.
    (is (= ["2020-01-02" nil] (col-after c 7 "UPDATE w SET d = d + 1")))
    (is (= ["2019-12-31" nil] (col-after c 7 "UPDATE w SET d = d - 1")))
    (is (= ["2020-01-31" nil] (col-after c 7 "UPDATE w SET d = d + 30")))))

(deftest unrecognised-conditions-were-unconditionally-true
  (with-open [c (jdbc)]
    ;; eval-update-cond's fallback took the TRUTHINESS of eval-update-expr's
    ;; output -- and that output was the expression's SQL text, which is a
    ;; non-empty string. So every predicate it did not recognise was TRUE.
    (is (= ["else" "else"]
           (col-after c 3 "UPDATE w SET s = CASE WHEN s LIKE 'zzz%' THEN 'TAKEN' ELSE 'else' END")))
    (is (= ["else" "else"]
           (col-after c 3 (str "UPDATE w SET s = CASE WHEN n BETWEEN 100 AND 200 "
                               "THEN 'TAKEN' ELSE 'else' END"))))
    (is (= ["else" "else"]
           (col-after c 3 "UPDATE w SET s = CASE WHEN n IN (777,888) THEN 'TAKEN' ELSE 'else' END")))
    (is (= ["else" "else"]
           (col-after c 3 (str "UPDATE w SET s = CASE WHEN j @> '{\"k\":999}' "
                               "THEN 'TAKEN' ELSE 'else' END"))))
    (testing "and the ones that DO match still match"
      (is (= ["TAKEN" "else"]
             (col-after c 3 "UPDATE w SET s = CASE WHEN s LIKE 'a%' THEN 'TAKEN' ELSE 'else' END")))
      (is (= ["TAKEN" "else"]
             (col-after c 3 (str "UPDATE w SET s = CASE WHEN n BETWEEN 5 AND 20 "
                                 "THEN 'TAKEN' ELSE 'else' END"))))
      (is (= ["TAKEN" "else"]
             (col-after c 3 (str "UPDATE w SET s = CASE WHEN j @> '{\"k\":1}' "
                                 "THEN 'TAKEN' ELSE 'else' END")))))))

(deftest jsonb-extraction-yields-real-null
  (with-open [c (jdbc)]
    ;; The `:__null__` sentinel reached the column, so a row without the key
    ;; got the literal text ":__null__".
    (is (= ["1" nil] (col-after c 3 "UPDATE w SET s = j->>'k'")))
    (is (= [nil nil] (col-after c 3 "UPDATE w SET s = j->>'missing'")))))

(deftest the-forms-that-already-worked-still-do
  (with-open [c (jdbc)]
    (is (= ["11" nil] (col-after c 2 "UPDATE w SET n = n + 1")))
    (is (= ["10" "0"] (col-after c 2 "UPDATE w SET n = coalesce(n, 0)")))
    (is (= ["AA" nil] (col-after c 3 "UPDATE w SET s = upper(s)")))
    (is (= ["1" "9"] (col-after c 2 "UPDATE w SET n = CASE WHEN n > 5 THEN 1 ELSE 9 END")))
    (is (= ["aax" nil] (col-after c 3 "UPDATE w SET s = s || 'x'")))
    (is (= ["t" nil] (col-after c 4 "UPDATE w SET b = (n > 5)")))
    (is (= ["5" "5"] (col-after c 2 "UPDATE w SET n = 5")))))

(deftest update-from-an-ordinary-table
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE upd_target (id int, v int)")
    (exec! c "CREATE TABLE upd_source (id int, flag int, delta int)")
    (exec! c "INSERT INTO upd_target VALUES (1,4),(2,8),(3,-9),(4,-12)")
    (exec! c (str "INSERT INTO upd_source VALUES "
                  "(1,1,-1),(2,2,-2),(3,3,-3),(4,2,-4),(5,1,NULL),(6,NULL,-6)"))
    (is (= 1
           (update-count
            c
            (str "UPDATE upd_target SET v = CASE WHEN s.flag >= 2 "
                 "THEN 2 * delta ELSE 3 * delta END "
                 "FROM upd_source s WHERE delta = -upd_target.v"))))
    (is (= [["1" "-8"] ["2" "8"] ["3" "-9"] ["4" "-12"]]
           (rows c "SELECT id,v FROM upd_target ORDER BY id")))
    (testing "an unqualified name owned by target and source is ambiguous"
      (let [e (try
                (exec! c (str "UPDATE upd_target SET v = 0 FROM upd_source s "
                              "WHERE id = s.id"))
                nil
                (catch java.sql.SQLException e e))]
        (is (some? e))
        (is (= "42702" (.getSQLState ^java.sql.SQLException e)))))))

(deftest postgres-update-from-values-and-qualified-target
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE update_values (a int DEFAULT 10, b int, c text)")
    (exec! c "INSERT INTO update_values VALUES (10,20,'foo'),(10,30,NULL)")
    (is (= 1 (update-count
              c
              (str "UPDATE update_values SET a=v.i "
                   "FROM (VALUES(100,20)) AS v(i,j) "
                   "WHERE update_values.b=v.j"))))
    (is (= [["100" "20" "foo"] ["10" "30" nil]]
           (rows c "SELECT * FROM update_values ORDER BY b")))
    (is (= 1 (update-count
              c
              (str "UPDATE update_values SET (c,b,a)=('bugle',b+11,DEFAULT) "
                   "WHERE c='foo'"))))
    (is (= [["10" "31" "bugle"] ["10" "30" nil]]
           (rows c "SELECT * FROM update_values ORDER BY b DESC")))
    (let [before (rows c "SELECT * FROM update_values ORDER BY b")
          e (try
              (exec! c (str "UPDATE update_values SET (b,a)="
                            "(SELECT a,b FROM update_values LIMIT 1)"))
              nil
              (catch java.sql.SQLException e e))]
      (is (some? e))
      (is (= "0A000" (.getSQLState ^java.sql.SQLException e)))
      (is (= before (rows c "SELECT * FROM update_values ORDER BY b"))))
    (let [e (try
              (exec! c "UPDATE update_values t SET t.b=t.b+10 WHERE t.a=10")
              nil
              (catch java.sql.SQLException e e))]
      (is (some? e))
      (is (= "42703" (.getSQLState ^java.sql.SQLException e)))
      (is (re-find #"SET target columns cannot be qualified"
                   (.getMessage ^java.sql.SQLException e))))))

(defn- sqlstate [^Connection c sql]
  (try (exec! c sql) nil
       (catch java.sql.SQLException e (.getSQLState e))))

(deftest set-is-computed-by-the-update-query
  ;; PostgreSQL plans an UPDATE as a query over the target whose target
  ;; list computes the new values. Expectations are PostgreSQL 17's.
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE us")
    (exec! c (str "CREATE TABLE u (id int PRIMARY KEY, x int, y int, v varchar(3), "
                  "q int DEFAULT nextval('us'), k int DEFAULT 7, ts timestamptz, a int[], j jsonb)"))
    (exec! c "INSERT INTO u (id, x, y, a, j) VALUES (1,10,1,'{1,2}','{\"k\":1}'), (2,20,0,NULL,NULL), (3,30,2,'{3}','[]')")
    (testing "subqueries read the pre-statement snapshot"
      (exec! c "UPDATE u SET x = (SELECT max(x) FROM u) + 1 WHERE id < 3")
      (is (= [["1" "31"] ["2" "31"] ["3" "30"]] (rows c "SELECT id, x FROM u ORDER BY id"))))
    (testing "WHERE filters before SET is evaluated"
      (is (= 2 (update-count c "UPDATE u SET x = 100 / y WHERE y <> 0")))
      (is (= [["1" "100"] ["2" "31"] ["3" "50"]] (rows c "SELECT id, x FROM u ORDER BY id"))))
    (testing "a correlated subquery reads the row"
      (exec! c "UPDATE u SET y = (SELECT x FROM u v WHERE v.id = u.id + 1)")
      (is (= [["1" "31"] ["2" "50"] ["3" nil]] (rows c "SELECT id, y FROM u ORDER BY id"))))
    (testing "the assignment cast follows the expression's type"
      (exec! c "UPDATE u SET x = 1.6 WHERE id = 1")
      (is (= [["2"]] (rows c "SELECT x FROM u WHERE id = 1")))
      (exec! c "UPDATE u SET x = 2.5 WHERE id = 1")
      (is (= [["3"]] (rows c "SELECT x FROM u WHERE id = 1")))
      (exec! c "UPDATE u SET x = 2.5::float8 WHERE id = 1")
      (is (= [["2"]] (rows c "SELECT x FROM u WHERE id = 1")))
      (is (= "42804" (sqlstate c "UPDATE u SET x = '1.6'::text WHERE false")))
      (is (= "22001" (sqlstate c "UPDATE u SET v = 'abcd' WHERE id = 1"))))
    (testing "DEFAULT is the column default, a sequence advancing per row"
      (exec! c "UPDATE u SET q = DEFAULT, k = DEFAULT")
      (is (= [["3" "1"]] (rows c "SELECT count(DISTINCT q), count(DISTINCT k) FROM u"))))
    (testing "now() is the statement's time"
      (exec! c "UPDATE u SET ts = now()")
      (is (= [["1"]] (rows c "SELECT count(DISTINCT ts) FROM u"))))
    (testing "array and jsonb operators are the SELECT translator's"
      (exec! c "UPDATE u SET a = a || 9, j = CASE WHEN j @> '{\"k\":1}' THEN j || '{\"m\":2}' ELSE j END")
      (is (= [["1" "{1,2,9}" "{\"k\": 1, \"m\": 2}"] ["2" "{9}" nil] ["3" "{3,9}" "[]"]]
             (rows c "SELECT id, a, j FROM u ORDER BY id"))))
    (testing "a volatile function is evaluated per row"
      (exec! c "ALTER TABLE u ADD COLUMN g uuid")
      (exec! c "UPDATE u SET g = gen_random_uuid()")
      (is (= [["3"]] (rows c "SELECT count(DISTINCT g) FROM u"))))
    (testing "an untyped operand beside an array takes the array's type"
      (is (= [["1" "{1,2,9}" "{1,2,9,4,5}"] ["2" "{9}" "{9,4,5}"] ["3" "{3,9}" "{3,9,4,5}"]]
             (rows c "SELECT id, a || NULL, a || '{4,5}' FROM u ORDER BY id"))))
    (testing "what the SET list may not contain"
      (is (= "42803" (sqlstate c "UPDATE u SET x = count(*)")))
      (is (= "42P20" (sqlstate c "UPDATE u SET x = row_number() OVER ()")))
      (is (= "42703" (sqlstate c "UPDATE u SET nosuch = 1"))))))

(deftest set-plan-over-a-temp-table
  ;; A temp table is stored under a per-session name; the UPDATE's text
  ;; still qualifies columns with the name it was given.
  (with-open [c (jdbc)]
    (exec! c "CREATE TEMP TABLE tt (id int PRIMARY KEY, x int)")
    (exec! c "INSERT INTO tt VALUES (1, 1), (2, 2)")
    (exec! c "UPDATE tt SET x = tt.x + 10 WHERE tt.id = 1")
    (exec! c "UPDATE tt t SET x = t.x + 100 WHERE t.id = 2")
    (is (= [["1" "11"] ["2" "102"]] (rows c "SELECT id, x FROM tt ORDER BY id")))))

(deftest update-from-is-one-joined-query
  ;; PostgreSQL plans UPDATE ... FROM as an ordinary join whose target
  ;; list computes the SET values, so the list is evaluated for EVERY
  ;; joined pair and a target row several pairs match is updated once.
  ;; Expectations are PostgreSQL 17's.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE uf (id int PRIMARY KEY, x int)")
    (exec! c "CREATE TABLE ua (id int, v int, tag text)")
    (exec! c "CREATE TABLE ub (id int, w int)")
    (exec! c "INSERT INTO uf VALUES (1,10),(2,20),(3,30)")
    (exec! c "INSERT INTO ua VALUES (1,5,'a'),(1,7,'b'),(2,6,'c')")
    (exec! c "INSERT INTO ub VALUES (1,100),(2,200)")
    (testing "a target matched by several source rows is updated once"
      (is (= 2 (update-count c "UPDATE uf t SET x = t.x + a.v FROM ua a WHERE a.id = t.id")))
      (is (= [["1" "15"] ["2" "26"] ["3" "30"]] (rows c "SELECT id, x FROM uf ORDER BY id"))))
    (testing "SET is evaluated for every pair, including the discarded ones"
      (exec! c "INSERT INTO ua VALUES (2,0,'d')")
      (is (= "22012" (sqlstate c "UPDATE uf t SET x = 100 / a.v FROM ua a WHERE a.id = t.id")))
      (exec! c "DELETE FROM ua WHERE tag = 'd'"))
    (testing "several relations, a join, a subquery and VALUES all serve as the source"
      (exec! c "UPDATE uf t SET x = a.v + b.w FROM ua a, ub b WHERE a.id = t.id AND b.id = t.id AND a.tag = 'a'")
      (is (= [["1" "105"] ["2" "26"]] (rows c "SELECT id, x FROM uf WHERE id < 3 ORDER BY id")))
      (exec! c "UPDATE uf t SET x = a.v FROM ua a JOIN ub b ON b.id = a.id WHERE a.id = t.id AND a.tag = 'a'")
      (is (= [["1" "5"]] (rows c "SELECT id, x FROM uf WHERE id = 1")))
      (exec! c "UPDATE uf t SET x = j.v FROM (SELECT id, v * 2 AS v FROM ua WHERE tag = 'a') j WHERE j.id = t.id")
      (is (= [["1" "10"]] (rows c "SELECT id, x FROM uf WHERE id = 1")))
      (exec! c "UPDATE uf t SET x = s.a FROM (VALUES (3, 99)) AS s(i, a) WHERE s.i = t.id")
      (is (= [["3" "99"]] (rows c "SELECT id, x FROM uf WHERE id = 3"))))
    (testing "the target itself may be the source"
      (exec! c "UPDATE uf t SET x = 7 FROM (SELECT id FROM uf WHERE id = 2) z WHERE z.id = t.id")
      (is (= [["2" "7"]] (rows c "SELECT id, x FROM uf WHERE id = 2"))))
    (testing "a derived source is re-read on every execution"
      ;; It is materialised into a snapshot at translate time, so a
      ;; cached plan would answer from the db it was translated on.
      (exec! c "UPDATE uf t SET x = j.v FROM (SELECT id, v FROM ua WHERE tag = 'a') j WHERE j.id = t.id")
      (is (= [["1" "5"]] (rows c "SELECT id, x FROM uf WHERE id = 1")))
      (exec! c "UPDATE ua SET v = v * 10 WHERE tag = 'a'")
      (exec! c "UPDATE uf t SET x = j.v FROM (SELECT id, v FROM ua WHERE tag = 'a') j WHERE j.id = t.id")
      (is (= [["1" "50"]] (rows c "SELECT id, x FROM uf WHERE id = 1"))))
    (testing "a source visible under the target's own name is 42712"
      ;; PostgreSQL puts the target in the range table with the rest.
      (is (= "42712" (sqlstate c "UPDATE uf SET x = 1 FROM uf")))
      (is (= "42712" (sqlstate c "UPDATE uf a SET x = 1 FROM ua a WHERE a.id = 1")))
      (testing "but the target's name is free once the target is aliased"
        (exec! c "UPDATE uf q SET x = uf.x + 1 FROM uf WHERE uf.id = q.id AND q.id = 1")
        (is (= [["1" "51"]] (rows c "SELECT id, x FROM uf WHERE id = 1")))))
    (testing "an unknown source relation is 42P01"
      (is (= "42P01" (sqlstate c "UPDATE uf t SET x = 1 FROM nosuch s WHERE s.id = t.id"))))))
