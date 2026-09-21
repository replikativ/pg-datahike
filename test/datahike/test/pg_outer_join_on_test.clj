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

(defn- prepared-rows
  "`rows`, over the extended protocol -- the one every driver uses."
  [^Connection c sql]
  (with-open [st (.prepareStatement c sql) rs (.executeQuery st)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- state
  "The SQLSTATE of a statement that must fail."
  [^Connection c sql]
  (try
    (with-open [st (.createStatement c)] (.execute st sql))
    :no-error
    (catch java.sql.SQLException e (.getSQLState e))))

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

(deftest a-condition-over-the-nullable-side
  ;; A column of the nullable side read by a WHERE, or by a condition the
  ;; join defers, was read OUTSIDE the or-join -- a `get-else` on the
  ;; right ENTITY variable, which the unmatched branch grounds to
  ;; `:__null__`. The client got datalog's "Bad format for entity-id in
  ;; pattern" (or "Cannot resolve any more clauses") as XX000.
  ;;
  ;; The READ belongs inside the join; the predicate stays outside, where
  ;; it filters the null-extended rows -- which is how PostgreSQL reduces
  ;; an outer join to an inner one.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE na (id int, v int)")
    (exec! c "CREATE TABLE nb (id int, w text)")
    (exec! c "INSERT INTO na VALUES (1,10),(2,20),(3,30)")
    (exec! c "INSERT INTO nb VALUES (1,'one'),(2,NULL)")
    (testing "a WHERE over the nullable side"
      (is (= [["1" "one"]]
             (rows c "SELECT na.id, nb.w FROM na LEFT JOIN nb ON (na.id = nb.id)
                       WHERE nb.w IS NOT NULL ORDER BY 1")))
      (is (= [["2" nil] ["3" nil]]
             (rows c "SELECT na.id, nb.w FROM na LEFT JOIN nb ON (na.id = nb.id)
                       WHERE nb.w IS NULL ORDER BY 1"))))
    (testing "a condition over the nullable side inside the ON"
      (is (= [["1" nil] ["2" nil] ["3" nil]]
             (rows c "SELECT na.id, nb.w FROM na LEFT JOIN nb ON (na.id = nb.id AND nb.w IS NULL)
                      ORDER BY 1")))
      (is (= [["1" "one"] ["2" nil] ["3" nil]]
             (rows c "SELECT na.id, nb.w FROM na LEFT JOIN nb ON (na.id = nb.id AND nb.w IS NOT NULL)
                      ORDER BY 1"))))
    (testing "and on the other side of a RIGHT join"
      (is (= [["1" "one"] ["2" nil]]
             (rows c "SELECT na.id, nb.w FROM na RIGHT JOIN nb ON (na.id = nb.id AND na.v IS NOT NULL)
                      ORDER BY 1"))))))

(deftest a-full-join-answers-both-sides-every-time
  ;; Three defects, one query:
  ;;
  ;;   * the FULL -> LEFT rewrite mutated the AST in place, and the AST
  ;;     cache hands out the same object per SQL string, so the FIRST
  ;;     execution answered a FULL JOIN and every later one a plain LEFT
  ;;     JOIN -- the right-only rows disappeared once the statement had
  ;;     been run before;
  ;;   * the halves were combined by removing from the second every row
  ;;     equal to one in the first, which loses a right-only row that
  ;;     happens to equal a left row's projection;
  ;;   * `describeResult` did not know the shape, so over the extended
  ;;     protocol the client met DataRows with no RowDescription
  ;;     ("Received resultset tuples, but no field structure for them")
  ;;     and every later statement on that connection failed too.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE fa (id int, k int)")
    (exec! c "CREATE TABLE fb (id int, w text)")
    (exec! c "INSERT INTO fa VALUES (1,10),(2,20)")
    (exec! c "INSERT INTO fb VALUES (10,'ten'),(30,'thirty')")
    ;; sorted: a NULL left id orders first
    (let [expected [[nil "thirty"] ["1" "ten"] ["2" nil]]]
      (testing "both sides, and the same answer every time"
        (dotimes [_ 3]
          (is (= expected
                 (sort (rows c "SELECT fa.id, fb.w FROM fa FULL JOIN fb ON (fa.k = fb.id)"))))))
      (testing "over a prepared statement, which is how every driver asks"
        (is (= expected
               (sort (prepared-rows c "SELECT fa.id, fb.w FROM fa FULL JOIN fb ON (fa.k = fb.id)")))))
      (testing "and the connection still works afterwards"
        (is (= [["1"]] (rows c "SELECT 1")))))
    (testing "what cannot be answered is refused, not answered twice"
      ;; `count(*)` used to return one row per half -- 5 and 4 where
      ;; PostgreSQL says 7.
      (doseq [sql ["SELECT count(*) FROM fa FULL JOIN fb ON (fa.k = fb.id)"
                   "SELECT DISTINCT fa.id FROM fa FULL JOIN fb ON (fa.k = fb.id)"
                   "SELECT fa.id FROM fa FULL JOIN fb ON (fa.k = fb.id) LIMIT 2"]]
        (is (= "0A000" (state c sql)) sql)))))

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
