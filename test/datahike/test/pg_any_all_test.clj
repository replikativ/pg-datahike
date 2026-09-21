(ns datahike.test.pg-any-all-test
  "`x <op> ANY(arr)` / `x <op> ALL(arr)` — one construct, one lowering.

   It had three runtimes. The WHERE path built a two-valued predicate out
   of `clojure.core`'s operators; the value path had two copies for `=`
   and `<>` and recognised no other operator; and the equality WHERE
   branch had a fourth of its own, over `pg-arr/member?`. They disagreed
   with PostgreSQL and with each other:

     SELECT v > ANY(arr) FROM t      42883, `function any(integer[]) does
                                     not exist` — only = and <> were
                                     recognised in value position
     WHERE v > ANY(arr)              XX000, ClassCastException, the moment
                                     an element was NULL
     WHERE v > ALL(ARRAY[1,NULL])    XX000, NullPointerException — the
                                     literal expansion emitted (> ?v nil)
     SELECT v <> ANY(i.indkey)       matched nothing: only the `=` copy
                                     read an int2vector
     SELECT 12 = ANY('12')           NULL, where PostgreSQL raises 22P02

   Now one Kleene runtime serves every operator in both positions, with
   one array reader and one literal expansion. Expectations are a
   PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- aa-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"aa" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each aa-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/aa?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- state [^Connection c sql]
  (try (exec! c sql) :no-error (catch SQLException e (.getSQLState e))))

(defn- error [^Connection c sql]
  (try (exec! c sql) "" (catch SQLException e (.getMessage e))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE q (id int, v int, arr int[])")
  (exec! c (str "INSERT INTO q VALUES (1,3,ARRAY[1,2,3]),(2,9,ARRAY[1,NULL]),"
                "(3,NULL,ARRAY[5]),(4,7,ARRAY[]::int[]),(5,7,NULL)")))

(deftest every-operator-in-value-position
  (with-open [c (jdbc)]
    (seed! c)
    ;; Row 2 holds a NULL element, row 3 a NULL scalar, row 4 an EMPTY
    ;; array and row 5 a NULL array. An empty array settles the answer
    ;; even for a NULL scalar -- there is no element to be unknown about.
    (is (= [["1" "t" "f" "t" "t" "f" "f"]
            ["2" nil nil "t" nil nil "f"]
            ["3" nil nil nil nil nil nil]
            ["4" "f" "t" "f" "t" "f" "t"]
            ["5" nil nil nil nil nil nil]]
           (rows c "SELECT id, v = ANY(arr), v <> ALL(arr), v > ANY(arr),
                           v >= ALL(arr), v < ANY(arr), v <= ALL(arr)
                      FROM q ORDER BY id")))))

(deftest every-operator-in-where-position
  (with-open [c (jdbc)]
    (seed! c)
    (testing "UNKNOWN is rejected, as PostgreSQL rejects it at the qual"
      (is (= [["1"]] (rows c "SELECT id FROM q WHERE v = ANY(arr) ORDER BY id")))
      (is (= [["4"]] (rows c "SELECT id FROM q WHERE v <> ALL(arr) ORDER BY id")))
      (is (= [["1"] ["2"]] (rows c "SELECT id FROM q WHERE v > ANY(arr) ORDER BY id")))
      (is (= [["1"] ["4"]] (rows c "SELECT id FROM q WHERE v >= ALL(arr) ORDER BY id")))
      (is (= [["4"]] (rows c "SELECT id FROM q WHERE v <= ALL(arr) ORDER BY id"))))))

(deftest literal-arrays-holding-a-null
  (with-open [c (jdbc)]
    (seed! c)
    (testing "value position is Kleene"
      (is (= [["t" nil nil]]
             (rows c "SELECT 3 > ANY(ARRAY[1,NULL]), 3 > ALL(ARRAY[1,NULL]), 3 = ANY(ARRAY[1,NULL])"))))
    (testing "an empty array settles it, a NULL array does not"
      (is (= [["f" "t"]]
             (rows c "SELECT NULL::int > ANY(ARRAY[]::int[]), NULL::int > ALL(ARRAY[]::int[])")))
      (is (= [[nil "t" "t"]]
             (rows c "SELECT 1 = ANY(NULL::int[]), 1 = ANY('{1,2}'), 1 = ALL('{1,1}')"))))
    (testing "WHERE over a literal holding a NULL"
      ;; `> ALL` can never be TRUE with a NULL element, so no row survives;
      ;; `> ANY` keeps the rows a real element settles.
      (is (= [] (rows c "SELECT id FROM q WHERE v > ALL(ARRAY[1,NULL]) ORDER BY id")))
      (is (= [["1"] ["2"] ["4"] ["5"]]
             (rows c "SELECT id FROM q WHERE v > ANY(ARRAY[1,NULL]) ORDER BY id"))))))

(deftest an-int2vector-operand-reads-as-a-vector
  ;; `pg_index.indkey` is written space-separated. Every operator now
  ;; reads it the same way; only the `=` copy did before.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ix (id int PRIMARY KEY, s text)")
    (is (= [["id"]]
           (rows c "SELECT a.attname FROM pg_catalog.pg_attribute a
                      JOIN pg_catalog.pg_index i ON (a.attrelid = i.indrelid)
                      JOIN pg_catalog.pg_class c ON (c.oid = i.indrelid)
                     WHERE c.relname = 'ix' AND a.attnum = ANY(i.indkey)
                     ORDER BY 1")))
    (is (= [["s"]]
           (rows c "SELECT a.attname FROM pg_catalog.pg_attribute a
                      JOIN pg_catalog.pg_index i ON (a.attrelid = i.indrelid)
                      JOIN pg_catalog.pg_class c ON (c.oid = i.indrelid)
                     WHERE c.relname = 'ix' AND a.attnum <> ALL(i.indkey)
                     ORDER BY 1")))))

(deftest an-operand-that-is-not-an-array-is-22P02
  (with-open [c (jdbc)]
    (is (= "22P02" (state c "SELECT 12 = ANY('12')")))
    (is (re-find #"malformed array literal: \"12\"" (error c "SELECT 12 = ANY('12')")))
    (testing "a NULL operand is still NULL, not an error"
      (is (= [[nil]] (rows c "SELECT 12 = ANY(NULL::int[])"))))))
