(ns datahike.test.pg-dml-expressions-test
  "Expressions on the WRITE paths: `UPDATE … SET col = <expr>` and
   `INSERT … SELECT`.

   Found by extending the differential fuzzer past SELECT. A mutation is
   only comparable if both servers start from the same state, so each
   sample re-seeds both sides, runs the statement, and compares the
   resulting TABLE as well as the reported outcome. That surface had never
   been fuzzed, and it held two bugs that a read-only generator could not
   reach.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn dml-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"dm" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each dml-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/dm?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv (fn [^long ix] (.getString rs ix)) (range 1 (inc n)))))
          acc)))))

(defn- fresh!
  "Re-create the table so each assertion starts from the same state."
  [^Connection c]
  (exec! c "DROP TABLE IF EXISTS z")
  (exec! c "CREATE TABLE z (id int, i int, s text)")
  (exec! c "INSERT INTO z VALUES (1,10,'a'),(2,NULL,NULL)"))

(defn- after
  "Run `sql`, then return the table."
  [^Connection c sql]
  (fresh! c)
  (exec! c sql)
  (rows c "SELECT id, i, s FROM z ORDER BY id"))

(deftest update-set-function-call
  (with-open [c (jdbc)]
    ;; The UPDATE SET evaluator knew only literals, arithmetic, now() and
    ;; concat(); everything else fell through to `(str value-expr)`, so the
    ;; SQL TEXT of the call was written into the column -- and for a numeric
    ;; column it raised "invalid input syntax for numeric: \"abs(i)\"".
    (is (= [["1" "10" "a"] ["2" "0" nil]] (after c "UPDATE z SET i = coalesce(i, 0)")))
    (is (= [["1" "10" "a"] ["2" nil nil]] (after c "UPDATE z SET i = abs(i)"))
        "and a strict function still propagates NULL")
    (is (= [["1" "10" "A"] ["2" nil nil]] (after c "UPDATE z SET s = upper(s)")))
    (is (= [["1" "10" "a"] ["2" "5" nil]] (after c "UPDATE z SET i = greatest(i, 5)"))
        "greatest is non-strict here too")
    (is (= [["1" nil "a"] ["2" nil nil]] (after c "UPDATE z SET i = nullif(i, 10)")))
    (is (= [["1" "1" "a"] ["2" "1" nil]] (after c "UPDATE z SET i = length('x')")))))

(deftest update-set-case-expression
  (with-open [c (jdbc)]
    ;; CASE had no branch at all, so the whole expression was stringified.
    (is (= [["1" "1" "a"] ["2" "2" nil]]
           (after c "UPDATE z SET i = CASE WHEN i <= 10 THEN 1 ELSE 2 END")))
    (is (= [["1" "10" "a"] ["2" "0" nil]]
           (after c "UPDATE z SET i = CASE WHEN s IS NULL THEN 0 ELSE i END")))
    (testing "a branch is taken only when its test is TRUE"
      ;; row 2's i IS NULL, so `i > 0` is UNKNOWN -- not TRUE, so the ELSE wins.
      (is (= [["1" "1" "a"] ["2" "9" nil]]
             (after c "UPDATE z SET i = CASE WHEN i > 0 THEN 1 ELSE 9 END"))))
    (testing "no ELSE means NULL"
      (is (= [["1" "1" "a"] ["2" nil nil]]
             (after c "UPDATE z SET i = CASE WHEN i > 0 THEN 1 END"))))
    (testing "simple CASE, on a switch expression"
      (is (= [["1" "7" "a"] ["2" "8" nil]]
             (after c "UPDATE z SET i = CASE id WHEN 1 THEN 7 ELSE 8 END"))))))

(deftest insert-select-carries-nulls
  (with-open [c (jdbc)]
    ;; A row coming FROM A SELECT carries SQL NULL as the `:__null__`
    ;; sentinel, and the sentinel reached the insert coercion as a value --
    ;; so `INSERT INTO t SELECT …` raised "invalid input syntax for column"
    ;; the moment ANY selected value was NULL. That is every bulk copy of a
    ;; table with a nullable column.
    (is (= [["1" "10" "a"] ["2" nil nil] ["101" "10" nil] ["102" nil nil]]
           (after c "INSERT INTO z (id, i) SELECT id + 100, i FROM z")))
    (is (= [["1" "10" "a"] ["2" nil nil] ["101" "10" "a"] ["102" nil nil]]
           (after c "INSERT INTO z (id, i, s) SELECT id + 100, i, s FROM z")))
    (testing "with a WHERE that filters the NULL row out, which already worked"
      (is (= [["1" "10" "a"] ["2" nil nil] ["101" "10" nil]]
             (after c "INSERT INTO z (id, i) SELECT id + 100, i FROM z WHERE i = 10"))))
    (testing "an explicit NULL in VALUES was never affected"
      (is (= [["1" "10" "a"] ["2" nil nil] ["9" nil nil]]
             (after c "INSERT INTO z (id, i) VALUES (9, NULL)"))))))
