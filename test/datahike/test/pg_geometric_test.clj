(ns datahike.test.pg-geometric-test
  "PostgreSQL's geometric types (geo_ops.c).

   They were accepted as whatever text was written: `'garbage'::point`
   answered `garbage` -- a value that is not a point, in a point column,
   reported as success -- and no spelling was canonicalised, so
   `'(1,2),(3,4)'::box` and `'(3,4),(1,2)'::box` were different text for
   the same box.

   `path` could not be spelled at all: JSqlParser reserves the word, so
   `CREATE TABLE t (p path)` was a syntax error and the file that
   declared one lost every statement after it.

   Only `point` had an OID here, so `format_type` reported `text` for
   the other six and every catalog join that asks a column's type got
   the wrong answer.

   This covers input, output and identity. The operators (`&&`, `<->`,
   `~=`, `@>`) are a separate matter.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"geo" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/geo?user=x&password=x&sslmode=disable")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- state-of [^Connection c sql]
  (try (one c sql) nil (catch SQLException e (.getSQLState e))))

(deftest every-spelling-canonicalises
  (with-open [c (jdbc)]
    (testing "point takes parens or not, and prints the shortest number"
      (is (= "(1,2)" (one c "SELECT '1,2'::point::text")))
      (is (= "(1,2)" (one c "SELECT ' ( 1 , 2 ) '::point::text")))
      (is (= "(1.5,2)" (one c "SELECT '(1.50,2)'::point::text")))
      (is (= "(100,2)" (one c "SELECT '(1e2,2)'::point::text"))))
    (testing "box sorts its corners, so one box is one value"
      ;; box_in prints the upper-right first. Without that the same box
      ;; written two ways compared and grouped as two.
      (is (= "(3,4),(1,2)" (one c "SELECT '(1,2),(3,4)'::box::text")))
      (is (= "(3,4),(1,2)" (one c "SELECT '(3,4),(1,2)'::box::text")))
      (is (= "(3,4),(1,2)" (one c "SELECT '1,2,3,4'::box::text"))))
    (testing "lseg and circle have one output form each"
      (is (= "[(1,2),(3,4)]" (one c "SELECT '(1,2),(3,4)'::lseg::text")))
      (is (= "[(1,2),(3,4)]" (one c "SELECT '((1,2),(3,4))'::lseg::text")))
      (is (= "<(1,2),3>" (one c "SELECT '1,2,3'::circle::text")))
      (is (= "<(1,2),3>" (one c "SELECT '((1,2),3)'::circle::text"))))
    (testing "a path keeps whether it is open or closed"
      (is (= "((1,2))" (one c "SELECT '(1,2)'::path::text")))
      (is (= "[(0,0),(1,1)]" (one c "SELECT '[(0,0),(1,1)]'::path::text")))
      (is (= "((0,0),(1,1))" (one c "SELECT '((0,0),(1,1))'::path::text"))))
    (testing "a line from two points becomes its equation"
      (is (= "{1,2,3}" (one c "SELECT '{1,2,3}'::line::text")))
      (is (= "{1,-1,1}" (one c "SELECT '(1,2),(3,4)'::line::text"))))))

(deftest text-that-is-not-a-shape-is-refused
  (with-open [c (jdbc)]
    ;; Each of these answered with its own input before.
    (is (= "22P02" (state-of c "SELECT 'garbage'::point")))
    (is (= "22P02" (state-of c "SELECT '(1,2'::point")))
    (is (= "22P02" (state-of c "SELECT '(1,2,3)'::point")))
    (is (= "22P02" (state-of c "SELECT 'nonsense'::box")))
    (is (= "22P02" (state-of c "SELECT '(1,2)'::lseg"))
        "an lseg needs two points")))

(deftest path-is-spellable
  ;; JSqlParser reserves the word, so every position it can appear in
  ;; was a syntax error: a column type, a `::` cast and a CAST target.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int, p path)")
    (exec! c "INSERT INTO t VALUES (1, '((1,2),(3,4))')")
    (is (= "((1,2),(3,4))" (one c "SELECT p::text FROM t")))
    (is (= "[(1,2),(3,4)]" (one c "SELECT '[(1,2),(3,4)]'::path::text")))
    (is (= "((1,2))" (one c "SELECT CAST('(1,2)' AS path)::text")))
    (testing "and `path` is still an ordinary column name"
      (exec! c "CREATE TABLE u (path text)")
      (exec! c "INSERT INTO u VALUES ('/tmp')")
      (is (= "/tmp" (one c "SELECT path FROM u"))))))

(deftest the-catalog-reports-the-column-type
  ;; Only `point` was mapped to its OID, so `format_type` answered
  ;; `text` for the other six -- and pg_attribute.atttypid, which is
  ;; what a driver reads for ResultSetMetaData, said 25.
  (with-open [c (jdbc)]
    (exec! c (str "CREATE TABLE g (p point, l line, s lseg, b box,"
                  " h path, y polygon, c circle)"))
    (is (= "point,line,lseg,box,path,polygon,circle"
           (one c (str "SELECT string_agg(format_type(atttypid, atttypmod), ','"
                       " ORDER BY attnum) FROM pg_attribute"
                       " WHERE attrelid = 'g'::regclass AND attnum > 0"))))))

(deftest a-written-shape-reads-back-canonical
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE g (id int, p point, b box, h path)")
    (exec! c "INSERT INTO g VALUES (1, '1,2', '(1,2),(3,4)', '[(0,0),(1,1)]')")
    (is (= "(1,2)|(3,4),(1,2)|[(0,0),(1,1)]"
           (one c "SELECT p::text || '|' || b::text || '|' || h::text FROM g")))
    (testing "and a bad one does not get written at all"
      (is (= "22P02" (state-of c "INSERT INTO g VALUES (2, 'garbage', NULL, NULL)")))
      (is (= "1" (one c "SELECT count(*)::text FROM g"))))))

;; ============================================================================
;; Coordinates are float8, and nothing less
;; ============================================================================

(deftest coordinates_are_float8_in_and_out
  ;; `pair_decode` is `float8in` and `pair_encode` is `float8out`. This
  ;; had a private renderer instead, and being private is what made it
  ;; wrong: an integral double went through `(long d)`, so `1e+300`
  ;; printed as 9223372036854775807, and there was no scientific
  ;; notation at all. `point_tbl` -- which `test_setup` builds and seven
  ;; other regression files read -- has both `(1e+300,Inf)` and
  ;; `(1e-300,-1e-300)`, so the file lost every statement after it.
  (with-open [c (jdbc)]
    (testing "the fixed-vs-scientific threshold is float8out's"
      (is (= "(1e+300,1)" (one c "select '(1e+300,1)'::point")))
      (is (= "(1e+15,100000000000000)" (one c "select '(1e15,1e14)'::point")))
      (is (= "(1e-300,1e-05)" (one c "select '(1e-300,0.00001)'::point")))
      (is (= "(1.2345678901234567e+19,-0)"
             (one c "select '(12345678901234567890,-0.0)'::point"))))
    (testing "the non-finite spellings float8in accepts"
      ;; `Inf` is the one Java's parseDouble does NOT know, and it is
      ;; how PostgreSQL's own fixture writes it.
      (is (= "(1e+300,Infinity)" (one c "select '(1e+300,Inf)'::point")))
      (is (= "(-Infinity,NaN)" (one c "select '(-inf,nan)'::point")))
      (is (= "(Infinity,-Infinity)" (one c "select '(infinity,-infinity)'::point"))))
    (testing "strtod reads hex, and float8in is strtod"
      (is (= "(16,2)" (one c "select '(0x10,2)'::point"))))
    (testing "what float8in refuses"
      ;; Java's parseDouble takes a `d`/`f` suffix; PostgreSQL does not.
      (is (= "22P02" (state-of c "select '(1d,2)'::point")))
      (is (= "22P02" (state-of c "select '(1f,2)'::point")))
      (is (= "22P02" (state-of c "select '(1e,2)'::point"))))
    (testing "it is float8out everywhere a coordinate is printed"
      (is (= "(1e+300,2),(0,0)" (one c "select '((0,0),(1e300,2))'::box")))
      (is (= "[(1e+300,Infinity),(0,0)]"
             (one c "select '[(1e+300,Inf),(0,0)]'::lseg")))
      (is (= "<(1e+16,2),300000>" (one c "select '<(1e16,2),3e5>'::circle"))))))
