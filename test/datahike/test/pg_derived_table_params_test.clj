(ns datahike.test.pg-derived-table-params-test
  "A parameter inside a derived table or a CTE.

   A relation the translator has to MATERIALISE -- a derived table, a
   CTE, a set operation -- is produced by running its body and storing
   the rows in a speculative db. That happens while the statement is
   PARSED, which under the extended protocol is before Bind: the body
   saw placeholders rather than values, matched nothing, and the
   relation came out empty. The statement then answered no rows, for
   every binding, with no error:

     SELECT * FROM (SELECT id FROM t WHERE id = ?) x      0 rows, always
     WITH x AS (SELECT id FROM t WHERE id = ?) SELECT * FROM x   same

   Literals were fine, which is why this survived: it needs a bound
   parameter AND a materialised relation, and most tests use one or the
   other. Metabase's column introspection uses both.

   The statement is now marked at parse and re-parsed at Execute with
   `params/*bound-params*` in scope, where the translator resolves the
   placeholders inline -- the mechanism runtime subqueries already use.
   Such a plan is not cached: the one built at Parse holds an empty
   relation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn- dp-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"dp" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each dp-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/dp?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows
  "The rows of `sql` with `params` bound, over the extended protocol."
  [^Connection c sql & params]
  (with-open [st (.prepareStatement c sql)]
    (dotimes [i (count params)]
      (.setObject st (int (inc i)) (nth params i)))
    (with-open [rs (.executeQuery st)]
      (let [n (.getColumnCount (.getMetaData rs))]
        (loop [acc []]
          (if (.next rs)
            (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
            acc))))))

(deftest a-parameter-inside-a-materialised-relation
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE dt (id int, s text)")
    (exec! c "INSERT INTO dt VALUES (1,'a'),(2,'b'),(3,'c')")
    (testing "a derived table"
      (is (= [["1"]] (rows c "SELECT * FROM (SELECT id FROM dt WHERE id = ?) x" (int 1))))
      (is (= [["2"]] (rows c "SELECT x.id FROM (SELECT id FROM dt WHERE id = ?) x" (int 2))))
      (is (= [] (rows c "SELECT * FROM (SELECT id FROM dt WHERE id = ?) x" (int 99))))
      ;; The outer ORDER BY is the one that orders the result: PostgreSQL
      ;; does not promise a subquery's own ORDER BY survives.
      (is (= [["1" "a"] ["2" "b"]]
             (rows c "SELECT * FROM (SELECT id, s FROM dt WHERE id <= ?) x ORDER BY 1" (int 2)))))
    (testing "a CTE"
      (is (= [["3"]] (rows c "WITH x AS (SELECT id FROM dt WHERE id = ?) SELECT * FROM x" (int 3))))
      (is (= [["1"] ["2"]]
             (rows c "WITH x AS (SELECT id FROM dt WHERE id < ?) SELECT * FROM x ORDER BY id" (int 3)))))
    (testing "two parameters, and one used twice"
      (is (= [["2"]]
             (rows c "SELECT * FROM (SELECT id FROM dt WHERE id >= ? AND id <= ?) x"
                   (int 2) (int 2))))
      (is (= [["2"]]
             (rows c "SELECT * FROM (SELECT id FROM dt WHERE id = ? OR id = ?) x"
                   (int 2) (int 2)))))
    (testing "the same statement, twice, with different bindings"
      ;; The parse-time plan holds an EMPTY relation, so it must not be
      ;; cached and handed to the next execution.
      (is (= [["1"]] (rows c "SELECT * FROM (SELECT id FROM dt WHERE id = ?) x" (int 1))))
      (is (= [["3"]] (rows c "SELECT * FROM (SELECT id FROM dt WHERE id = ?) x" (int 3))))
      (is (= [["1"]] (rows c "SELECT * FROM (SELECT id FROM dt WHERE id = ?) x" (int 1)))))
    (testing "literals, which always worked, still do"
      (is (= [["1"]] (rows c "SELECT * FROM (SELECT id FROM dt WHERE id = 1) x")))
      (is (= [["3"]] (rows c "WITH x AS (SELECT id FROM dt WHERE id = 3) SELECT * FROM x"))))))
