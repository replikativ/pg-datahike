(ns datahike.test.pg-array-assignment-order-test
  "Observable PostgreSQL ordering for subscripted writes.

   These controls are intentionally small: sequence values distinguish target
   bounds, RHS expressions, source projections, and physical target columns."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"assignment_order" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/assignment_order?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [s (.createStatement c)] (.execute s sql)))

(defn- rows [^Connection c sql]
  (with-open [s (.createStatement c), r (.executeQuery s sql)]
    (let [n (.getColumnCount (.getMetaData r))]
      (loop [out []]
        (if (.next r)
          (recur (conj out (mapv #(.getString r (int %)) (range 1 (inc n)))))
          out)))))

(defn- state [^Connection c sql]
  (try (exec! c sql) nil (catch SQLException e (.getSQLState e))))

(defn- reset-sequence! [^Connection c]
  (rows c "SELECT setval('assignment_order_seq',1,false)"))

(deftest bounds-run-upper-then-lower-then-rhs
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c "CREATE TABLE assignment_order_rows (a int[])")
    (exec! c "INSERT INTO assignment_order_rows VALUES (ARRAY[0,0,0])")
    (exec! c (str "UPDATE assignment_order_rows SET "
                  "a[nextval('assignment_order_seq')::int:"
                  "(nextval('assignment_order_seq')+1)::int]="
                  "ARRAY[nextval('assignment_order_seq')::int]"))
    (is (= [["{0,3,0}"]] (rows c "SELECT a FROM assignment_order_rows")))
    (is (= [["3"]] (rows c "SELECT last_value FROM assignment_order_seq")))))

(deftest multidimensional-bounds-run-all-uppers-before-all-lowers
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c "CREATE TABLE assignment_order_rows (a int[])")
    (exec! c "INSERT INTO assignment_order_rows VALUES (ARRAY[[0]])")
    ;; In executor order these four expressions all produce 1:
    ;; upper dim1 (seq1), upper dim2 (seq2), lower dim1 (seq3), lower dim2
    ;; (seq4). Lexical lower/upper evaluation would produce different bounds.
    (exec! c (str "UPDATE assignment_order_rows SET "
                  "a[(nextval('assignment_order_seq')-2)::int:"
                  "nextval('assignment_order_seq')::int]"
                  "[(nextval('assignment_order_seq')-3)::int:"
                  "(nextval('assignment_order_seq')-1)::int]="
                  "ARRAY[[nextval('assignment_order_seq')::int]]"))
    (is (= [["{{5}}"]] (rows c "SELECT a FROM assignment_order_rows")))
    (is (= [["5"]] (rows c "SELECT last_value FROM assignment_order_seq")))))

(deftest supplied-null-bound-stops-before-rhs-after-other-bounds
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c "CREATE TABLE assignment_order_rows (a int[])")
    (exec! c "INSERT INTO assignment_order_rows VALUES (ARRAY[1,2])")
    (is (= "22004"
           (state c (str "UPDATE assignment_order_rows SET "
                         "a[NULL:nextval('assignment_order_seq')::int]="
                         "ARRAY[nextval('assignment_order_seq')::int]"))))
    ;; The upper expression ran; the RHS did not.
    (is (= [["1" "t"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_seq")))))

(deftest structural-slice-errors-occur-after-rhs
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c "CREATE TABLE assignment_order_rows (a int[])")
    (exec! c "INSERT INTO assignment_order_rows VALUES (ARRAY[1,2])")
    (is (= "2202E"
           (state c (str "UPDATE assignment_order_rows SET a[3:2]="
                         "ARRAY[nextval('assignment_order_seq')::int]"))))
    (is (= [["1" "t"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_seq")))
    ;; PostgreSQL's NULL-slice early return precedes the reversed-range check.
    (exec! c "UPDATE assignment_order_rows SET a[3:2]=NULL")
    (is (= [["{1,2}"]] (rows c "SELECT a FROM assignment_order_rows")))))

(deftest repeated-partials-interleave-each-target-and-rhs
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c "CREATE TABLE assignment_order_rows (a int[])")
    (exec! c "INSERT INTO assignment_order_rows VALUES (ARRAY[0,0,0])")
    (exec! c (str "UPDATE assignment_order_rows SET "
                  "a[(nextval('assignment_order_seq')*0+1)::int]="
                  "nextval('assignment_order_seq')::int,"
                  "a[(nextval('assignment_order_seq')*0+2)::int]="
                  "nextval('assignment_order_seq')::int"))
    (is (= [["{2,4,0}"]] (rows c "SELECT a FROM assignment_order_rows")))
    (is (= [["4"]] (rows c "SELECT last_value FROM assignment_order_seq")))))

(deftest assignment-groups-run-in-physical-column-order
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c "CREATE TABLE assignment_order_rows (id int,a int[],b int[])")
    (exec! c "INSERT INTO assignment_order_rows VALUES (1,ARRAY[0,0],ARRAY[0,0])")
    ;; SQL names b first, but PostgreSQL evaluates target columns by attnum.
    (exec! c (str "UPDATE assignment_order_rows SET "
                  "b[1]=nextval('assignment_order_seq')::int,"
                  "a[1]=nextval('assignment_order_seq')::int,"
                  "a[2]=nextval('assignment_order_seq')::int WHERE id=1"))
    (is (= [["{1,2}" "{3,0}"]]
           (rows c "SELECT a,b FROM assignment_order_rows")))
    (is (= [["3"]] (rows c "SELECT last_value FROM assignment_order_seq")))))

(deftest values-and-insert-select-have-distinct-source-order
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c "CREATE TABLE assignment_order_rows (id int,a int[])")
    ;; VALUES is nested inside the assignment: target index, then RHS.
    (exec! c (str "INSERT INTO assignment_order_rows "
                  "(id,a[nextval('assignment_order_seq')::int]) "
                  "VALUES (1,nextval('assignment_order_seq')::int)"))
    (is (= [["{2}"]] (rows c "SELECT a FROM assignment_order_rows WHERE id=1")))
    (reset-sequence! c)
    ;; INSERT SELECT materializes its source cell before target assignment.
    (exec! c (str "INSERT INTO assignment_order_rows "
                  "(id,a[nextval('assignment_order_seq')::int]) "
                  "SELECT 2,nextval('assignment_order_seq')::int"))
    (is (= [["[2:2]={1}"]]
           (rows c "SELECT a FROM assignment_order_rows WHERE id=2")))
    (reset-sequence! c)
    (is (= "22004"
           (state c (str "INSERT INTO assignment_order_rows (id,a[NULL]) "
                         "VALUES (3,nextval('assignment_order_seq')::int)"))))
    (is (= [["1" "f"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_seq")))
    (reset-sequence! c)
    (is (= "22004"
           (state c (str "INSERT INTO assignment_order_rows (id,a[NULL]) "
                         "SELECT 4,nextval('assignment_order_seq')::int"))))
    (is (= [["1" "t"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_seq")))))

(deftest mixed-insert-targets-run-in-physical-column-order
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_array_seq")
    (exec! c "CREATE SEQUENCE assignment_order_scalar_seq")
    (exec! c "CREATE TABLE partial_first (a int[],b int)")
    ;; Both source expressions are volatile, so the scalar error cannot be
    ;; constant-folded before executor target ordering. With a first, both
    ;; sequence calls happen before b's division error.
    (is (= "22012"
           (state c (str "INSERT INTO partial_first (a[1],b) VALUES ("
                         "nextval('assignment_order_array_seq')::int,"
                         "1/(nextval('assignment_order_scalar_seq')-1))"))))
    (is (= [["1" "t"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_array_seq")))
    (is (= [["1" "t"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_scalar_seq")))
    (rows c "SELECT setval('assignment_order_array_seq',1,false)")
    (rows c "SELECT setval('assignment_order_scalar_seq',1,false)")
    (exec! c "CREATE TABLE whole_first (b int,a int[])")
    ;; Reversing physical column order makes b's volatile division error
    ;; suppress a's expression, independent of lexical target-list order.
    (is (= "22012"
           (state c (str "INSERT INTO whole_first (a[1],b) VALUES ("
                         "nextval('assignment_order_array_seq')::int,"
                         "1/(nextval('assignment_order_scalar_seq')-1))"))))
    (is (= [["1" "f"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_array_seq")))
    (is (= [["1" "t"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_scalar_seq")))))

(deftest partial-insert-suppresses-the-base-column-default
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE assignment_order_seq")
    (exec! c (str "CREATE TABLE assignment_order_defaults ("
                  "a int[] DEFAULT ARRAY[nextval('assignment_order_seq')::int])"))
    (exec! c "INSERT INTO assignment_order_defaults (a[2]) VALUES (7)")
    (is (= [["[2:2]={7}"]]
           (rows c "SELECT a FROM assignment_order_defaults")))
    (is (= [["1" "f"]]
           (rows c "SELECT last_value,is_called FROM assignment_order_seq")))))
