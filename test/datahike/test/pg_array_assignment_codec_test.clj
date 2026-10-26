(ns datahike.test.pg-array-assignment-codec-test
  "Array leaf assignment codecs, including values whose scalar storage differs."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]
           [org.postgresql PGStatement]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"codecs" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/codecs?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [s (.createStatement c)] (.execute s sql)))

(defn- rows [^Connection c sql]
  (with-open [s (.createStatement c) r (.executeQuery s sql)]
    (let [n (.getColumnCount (.getMetaData r))]
      (loop [out []]
        (if (.next r)
          (recur (conj out (mapv #(.getString r (int %)) (range 1 (inc n)))))
          out)))))

(defn- state [^Connection c sql]
  (try (exec! c sql) nil (catch SQLException e (.getSQLState e))))

(deftest numeric-array-specials-remain-sql-values-through-every-write-path
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE specials (a numeric[])")
    (exec! c "INSERT INTO specials VALUES (ARRAY['NaN'::numeric,'Infinity'::numeric,'-Infinity'::numeric])")
    (is (= [["{NaN,Infinity,-Infinity}"]] (rows c "SELECT a::text FROM specials")))
    (exec! c "UPDATE specials SET a[2]='NaN',a[3]='Infinity'")
    (is (= [["{NaN,NaN,Infinity}"]] (rows c "SELECT a::text FROM specials")))
    (exec! c "UPDATE specials SET a[1:2]=ARRAY['-Infinity'::numeric,'Infinity'::numeric]")
    (is (= [["{-Infinity,Infinity,Infinity}"]]
           (rows c "SELECT a::text FROM specials")))))

(deftest numeric-array-typmods-apply-to-whole-elements-and-slices
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE decimals (a numeric(5,2)[])")
    (exec! c "INSERT INTO decimals VALUES (ARRAY[1.235])")
    (is (= [["{1.24}"]] (rows c "SELECT a::text FROM decimals")))
    (exec! c "UPDATE decimals SET a[1]=2.345")
    (exec! c "UPDATE decimals SET a[2:3]=ARRAY[3.456,4.567]")
    (is (= [["{2.35,3.46,4.57}"]] (rows c "SELECT a::text FROM decimals")))
    (is (= "22003" (state c "UPDATE decimals SET a[2]=1000")))
    (is (= [["{2.35,3.46,4.57}"]] (rows c "SELECT a::text FROM decimals")))))

(deftest character-array-assignment-preserves-padding-and-enforces-length
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE characters (v varchar(3)[],c char(3)[])")
    (exec! c "INSERT INTO characters VALUES (ARRAY['a'],ARRAY['x'])")
    (is (= [["{a}" "{\"x  \"}"]] (rows c "SELECT v::text,c::text FROM characters")))
    (exec! c "UPDATE characters SET v[1]='ab',c[1]='y'")
    (exec! c "UPDATE characters SET v[2:3]=ARRAY['abc','ab '],c[2:3]=ARRAY['ab','abc']")
    (is (= [["{ab,abc,\"ab \"}" "{\"y  \",\"ab \",abc}"]]
           (rows c "SELECT v::text,c::text FROM characters")))
    (is (= "22001" (state c "UPDATE characters SET v[1]='abcd'")))
    (is (= "22001" (state c "UPDATE characters SET c[1:2]=ARRAY['abcd','a']")))
    (is (= [["{ab,abc,\"ab \"}" "{\"y  \",\"ab \",abc}"]]
           (rows c "SELECT v::text,c::text FROM characters")))))

(deftest whole-array-text-retains-explicit-lower-bounds-and-rejects-scalars
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE bounded (a int[])")
    (exec! c "INSERT INTO bounded VALUES ('[-2:0]={1,2,3}')")
    (exec! c "UPDATE bounded SET a[-1]=9")
    (is (= [["[-2:0]={1,9,3}" "-2" "0"]]
           (rows c "SELECT a::text,array_lower(a,1),array_upper(a,1) FROM bounded")))
    (is (= "42804" (state c "INSERT INTO bounded SELECT 7 WHERE false")))
    (is (= "22P02" (state c "INSERT INTO bounded VALUES ('not an array')")))
    (is (= "42601" (state c "UPDATE bounded SET a[1,2]=7")))
    (is (= [["[-2:0]={1,9,3}"]] (rows c "SELECT a::text FROM bounded")))))

(deftest real-array-assignment-rounds-and-rejects-float-overflow-and-underflow
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE floats (a real[])")
    (exec! c "INSERT INTO floats VALUES (ARRAY[1.1::float8])")
    (is (= [["{1.1}"]] (rows c "SELECT a::text FROM floats")))
    (is (= "22003" (state c "UPDATE floats SET a[1]=1e39::float8")))
    (is (= "22003" (state c "UPDATE floats SET a[1:1]=ARRAY[1e-50::float8]")))
    (is (= "22003" (state c "UPDATE floats SET a=ARRAY[1e39::float8]")))
    (is (= [["{1.1}"]] (rows c "SELECT a::text FROM floats")))))

(deftest bit-array-typmods-apply-to-elements-and-slices
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE bit_arrays (b bit(3)[],v varbit(3)[])")
    (exec! c "INSERT INTO bit_arrays VALUES (ARRAY[B'101'],ARRAY[B'1'])")
    (is (= [["{101}" "{1}"]] (rows c "SELECT b::text,v::text FROM bit_arrays")))
    (is (= "22026" (state c "UPDATE bit_arrays SET b[1]=B'1'")))
    (is (= "22001" (state c "UPDATE bit_arrays SET v[1:1]=ARRAY[B'1010']")))
    (exec! c "UPDATE bit_arrays SET b[1:2]=ARRAY[B'010',B'111'],v[2]=B'10'")
    (is (= [["{010,111}" "{1,10}"]] (rows c "SELECT b::text,v::text FROM bit_arrays")))))

(deftest unmodified-character-and-bit-arrays-use-their-sql-default-width
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE implicit_widths (c char[],b bit[],v varbit[],p bpchar[])")
    (exec! c "INSERT INTO implicit_widths VALUES (ARRAY['x'],ARRAY[B'1'],ARRAY[B'101'],ARRAY['longer'])")
    (is (= [["{x}" "{1}" "{101}" "{longer}"]]
           (rows c "SELECT c::text,b::text,v::text,p::text FROM implicit_widths")))
    (is (= "22001" (state c "UPDATE implicit_widths SET c[1]='xy'")))
    (is (= "22026" (state c "UPDATE implicit_widths SET b[1]=B'10'")))
    (exec! c "UPDATE implicit_widths SET v[1]=B'101010',p[1]='unbounded'")
    (is (= [["{101010}" "{unbounded}"]]
           (rows c "SELECT v::text,p::text FROM implicit_widths")))))

(deftest insert-select-star-types-stay-aligned-with-trailing-unknown-literals
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE text_source (id int,v text)")
    (exec! c "CREATE TABLE typed_source (id int,v int)")
    (exec! c "CREATE TABLE star_target (id int,a int[],tail text)")
    (is (= "42804" (state c "INSERT INTO star_target(id,a[1],tail) SELECT *, 'x' FROM text_source")))
    (exec! c "INSERT INTO text_source VALUES (1,'2')")
    (is (= "42804" (state c "INSERT INTO star_target(id,a[1],tail) SELECT *, 'x' FROM text_source")))
    (is (= [["0"]] (rows c "SELECT count(*) FROM star_target")))
    (exec! c "INSERT INTO typed_source VALUES (2,3)")
    (exec! c "INSERT INTO star_target(id,a[1],tail) SELECT typed_source.*, 'x' FROM typed_source")
    (is (= [["2" "{3}" "x"]] (rows c "SELECT id,a::text,tail FROM star_target")))))

(deftest insert-select-unknown-literals-take-the-partial-target-type
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE unknown_source (a int[])")
    (exec! c "INSERT INTO unknown_source(a[1]) SELECT '3' WHERE false")
    (is (= [["0"]] (rows c "SELECT count(*) FROM unknown_source")))
    (exec! c "INSERT INTO unknown_source(a[1]) SELECT '3'")
    (exec! c "INSERT INTO unknown_source(a[1:2]) SELECT '{1,2}'")
    (is (= [["{3}"] ["{1,2}"]] (rows c "SELECT a::text FROM unknown_source ORDER BY a[1] DESC")))))

(deftest explicit-casts-keep-the-declared-jdbc-parameter-type
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE cast_parameters (a int[])")
    (with-open [s (.prepareStatement c "INSERT INTO cast_parameters VALUES (?::int[])")]
      (.setPrepareThreshold ^PGStatement s 1)
      (.setString s 1 "{1,2,3}")
      (is (= 1 (.executeUpdate s))))
    (with-open [s (.prepareStatement c "UPDATE cast_parameters SET a[?::int]=?::int")]
      (.setPrepareThreshold ^PGStatement s 1)
      (doseq [[index value] [["2" "9"] ["3" "8"]]]
        (.setString s 1 index)
        (.setString s 2 value)
        (is (= 1 (.executeUpdate s)))))
    (is (= [["{1,9,8}"]] (rows c "SELECT a::text FROM cast_parameters")))))

(deftest temporary-table-array-targets-share-the-same-codecs-and-scopes
  (with-open [c (jdbc)]
    (exec! c "CREATE TEMP TABLE temporary_arrays(id int PRIMARY KEY,a numeric(5,2)[])")
    (exec! c "INSERT INTO temporary_arrays(id,a[2]) VALUES(1,1.235)")
    (exec! c "UPDATE temporary_arrays SET a[3]=2.345 WHERE id=1")
    (exec! c "INSERT INTO temporary_arrays(id,a[1]) SELECT 2,4.567")
    (is (= [["1" "[2:3]={1.24,2.35}"] ["2" "{4.57}"]]
           (rows c "SELECT id,a::text FROM temporary_arrays ORDER BY id")))
    (is (= [["[2:3]={5.68,1.24}"]]
           (rows c (str "INSERT INTO temporary_arrays VALUES(1,ARRAY[5.678]) "
                        "ON CONFLICT(id) DO UPDATE SET a[2]=excluded.a[1],"
                        "a[3]=temporary_arrays.a[2] RETURNING a::text"))))
    (is (= [["1" "[2:3]={5.68,1.24}"] ["2" "{4.57}"]]
           (rows c "SELECT id,a::text FROM temporary_arrays ORDER BY id")))))
