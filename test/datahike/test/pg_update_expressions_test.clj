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
