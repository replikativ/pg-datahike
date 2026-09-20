(ns datahike.test.pg-outer-join-on-test
  "An outer join applies EVERY condition of its ON clause.

   The lowering kept one: `ref-info` was a single map that each
   condition `reset!`, so `ON (b.x = a.x AND b.y = a.y)` joined on `y`
   alone -- and answered rows that satisfy neither pair, silently.
   Around that, two more holes in the same construction:

     ON (b.x = a.x AND b.y = 1)    dropped the unmatched left row
                                   instead of null-extending it: the
                                   unmatched branch negated the key
                                   pattern alone, so a left row whose
                                   KEY matched but whose predicate did
                                   not fell out of both branches.

     ON (b.x = a.x AND b.y > a.y)  answered nothing at all: `a.y` was
                                   not in the or-join's head, so inside
                                   the branch it was a fresh unbound
                                   variable.

   RIGHT and FULL are rewritten into this same path, so they were wrong
   the same way. One condition, and INNER JOIN with the same ON, were
   always right.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn- oj-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"oj" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each oj-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/oj?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows
  "Every row as a vector of strings, NULL as nil."
  [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE la (x int, y int)")
  (exec! c "CREATE TABLE lb (x int, y int, v text)")
  (exec! c "INSERT INTO la VALUES (1,1),(1,2),(2,1)")
  (exec! c "INSERT INTO lb VALUES (1,1,'p'),(1,9,'q'),(2,2,'r')"))

(deftest every-on-condition-is-part-of-the-join
  (with-open [c (jdbc)]
    (seed! c)
    (testing "two equalities: both, not just the last one"
      (is (= [["1" "1" "p"] ["1" "2" nil] ["2" "1" nil]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        LEFT JOIN lb b ON (b.x = a.x AND b.y = a.y)
                      ORDER BY 1,2,3"))))
    (testing "an equality against a constant null-extends, it does not drop the row"
      (is (= [["1" "1" "p"] ["1" "2" "p"] ["2" "1" nil]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        LEFT JOIN lb b ON (b.x = a.x AND b.y = 1)
                      ORDER BY 1,2,3"))))
    (testing "the condition order does not change the answer"
      (is (= [["1" "1" "p"] ["1" "2" "p"] ["2" "1" nil]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        LEFT JOIN lb b ON (b.y = 1 AND b.x = a.x)
                      ORDER BY 1,2,3"))))
    (testing "an inequality reading a left column that is not a join key"
      (is (= [["1" "1" "q"] ["1" "2" "q"] ["2" "1" "r"]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        LEFT JOIN lb b ON (b.x = a.x AND b.y > a.y)
                      ORDER BY 1,2,3"))))
    (testing "a condition over the left side only"
      (is (= [["1" "1" "p"] ["1" "1" "q"] ["1" "2" nil] ["2" "1" "r"]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        LEFT JOIN lb b ON (b.x = a.x AND a.y = 1)
                      ORDER BY 1,2,3"))))
    (testing "one condition, and INNER with the same ON, are unchanged"
      (is (= [["1" "1" "p"] ["1" "1" "q"] ["1" "2" "p"] ["1" "2" "q"] ["2" "1" "r"]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        LEFT JOIN lb b ON (b.x = a.x)
                      ORDER BY 1,2,3")))
      (is (= [["1" "1" "p"]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        JOIN lb b ON (b.x = a.x AND b.y = a.y)
                      ORDER BY 1,2,3"))))
    (testing "RIGHT and FULL are rewritten into the same path"
      (is (= [["1" "1" "p"] [nil nil "q"] [nil nil "r"]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        RIGHT JOIN lb b ON (b.x = a.x AND b.y = a.y)
                      ORDER BY 1,2,3")))
      ;; FULL JOIN is right over the simple protocol --
      ;;   1|1|p  1|2|(null)  2|1|(null)  (null)|(null)|q  (null)|(null)|r
      ;; -- and cannot be asserted here: over the EXTENDED protocol this
      ;; driver uses, every FULL JOIN fails with "Received resultset
      ;; tuples, but no field structure for them", with or without a
      ;; second ON condition. Its own item in doc/consolidation-plan.md.
      )))

(deftest three-conditions-and-a-catalog-shape
  (with-open [c (jdbc)]
    (seed! c)
    (testing "three conditions"
      (is (= [["1" "1" "p"] ["1" "2" nil] ["2" "1" nil]]
             (rows c "SELECT a.x, a.y, b.v FROM la a
                        LEFT JOIN lb b ON (b.x = a.x AND b.y = a.y AND b.v = 'p')
                      ORDER BY 1,2,3"))))
    (testing "the shape pgjdbc's column metadata query joins on"
      ;; `LEFT JOIN pg_attrdef d ON (d.adrelid = a.attrelid AND d.adnum =
      ;; a.attnum)` -- the join that multiplied one row per column into
      ;; twenty, and is why the field-metadata catalog probe exists.
      (exec! c "CREATE TABLE cols (id int, nm text)")
      (let [n (count (rows c "SELECT a.attnum FROM pg_catalog.pg_attribute a
                                WHERE a.attrelid = 'cols'::regclass"))]
        (is (pos? n))
        (is (= n (count (rows c "SELECT a.attnum, d.adnum
                                   FROM pg_catalog.pg_attribute a
                                   LEFT JOIN pg_catalog.pg_attrdef d
                                     ON (d.adrelid = a.attrelid AND d.adnum = a.attnum)
                                  WHERE a.attrelid = 'cols'::regclass"))))))))
