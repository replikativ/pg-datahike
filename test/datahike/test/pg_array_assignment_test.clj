(ns datahike.test.pg-array-assignment-test
  "Subscripted write targets, checked against PostgreSQL17 and18."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException Types]
           [org.postgresql PGStatement]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"assignment" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/assignment?user=x&password=x&sslmode=disable&binaryTransfer=false")))

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

(deftest element-writes-preserve-the-container-and-extend-with-null-gaps
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE aa (a int[])")
    (exec! c "INSERT INTO aa VALUES (ARRAY[1,2,NULL,4])")
    (exec! c "UPDATE aa SET a[2]=22")
    (is (= [["{1,22,NULL,4}"]] (rows c "SELECT a FROM aa")))
    (exec! c "UPDATE aa SET a[6]=66")
    (is (= [["{1,22,NULL,4,NULL,66}" "1" "6"]]
           (rows c "SELECT a,array_lower(a,1),array_upper(a,1) FROM aa")))
    (exec! c "UPDATE aa SET a=NULL")
    (exec! c "UPDATE aa SET a[2]=22")
    (is (= [["[2:2]={22}" "2" "2"]]
           (rows c "SELECT a,array_lower(a,1),array_upper(a,1) FROM aa")))))

(deftest slice-and-repeated-partial-targets-use-old-row-expressions
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE aa (a int[])")
    (exec! c "INSERT INTO aa VALUES (ARRAY[1,2,3,4])")
    (exec! c "UPDATE aa SET a[2:3]=ARRAY[8,9]")
    (is (= [["{1,8,9,4}"]] (rows c "SELECT a FROM aa")))
    (exec! c "UPDATE aa SET a=ARRAY[1,2,3,4]")
    (exec! c "UPDATE aa SET a[1]=a[2],a[2]=a[1]")
    (is (= [["{2,1,3,4}"]] (rows c "SELECT a FROM aa")))
    (exec! c "UPDATE aa SET a[1]=3,a[a[1]]=9")
    (is (= [["{3,9,3,4}"]] (rows c "SELECT a FROM aa")))
    (is (= "42601" (state c "UPDATE aa SET a=ARRAY[1],a[2]=2")))))

(deftest partial-insert-builds-a-container-without-the-column-default
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ai (a int[] DEFAULT ARRAY[99])")
    (exec! c "INSERT INTO ai (a[3]) VALUES (33)")
    (is (= [["[3:3]={33}"]] (rows c "SELECT a FROM ai")))
    (exec! c "DELETE FROM ai")
    (exec! c "INSERT INTO ai (a[1],a[3]) VALUES (11,33)")
    (is (= [["{11,NULL,33}"]] (rows c "SELECT a FROM ai")))
    (is (= "42701" (state c "INSERT INTO ai (a,a[2]) VALUES (ARRAY[1],2)")))
    (is (= "0A000" (state c "INSERT INTO ai (a[2]) VALUES (DEFAULT)")))))

(deftest array-assignment-validation-is-atomic-and-sees-the-final-container
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ac (id int PRIMARY KEY,a int[] CHECK(a[1]=a[2]))")
    (exec! c "INSERT INTO ac VALUES (1,ARRAY[2,2]),(2,ARRAY[3,3])")
    (exec! c "UPDATE ac SET a[1]=4,a[2]=4")
    (is (= [["1" "{4,4}"] ["2" "{4,4}"]]
           (rows c "SELECT id,a FROM ac ORDER BY id")))
    (is (= "22004" (state c "UPDATE ac SET a[NULL]=9")))
    (is (= [["1" "{4,4}"] ["2" "{4,4}"]]
           (rows c "SELECT id,a FROM ac ORDER BY id")))
    (is (= "42804" (state c "UPDATE ac SET a=7 WHERE false")))
    (is (= "42804" (state c "INSERT INTO ac VALUES (3,7)")))))

(deftest describe-distinguishes-element-and-slice-parameters
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ap (a int[])")
    (with-open [s (.prepareStatement c "UPDATE ap SET a[?]=?")]
      (.setPrepareThreshold ^PGStatement s 1)
      (let [m (.getParameterMetaData s)]
        (is (= Types/INTEGER (.getParameterType m 1)))
        (is (= Types/INTEGER (.getParameterType m 2)))))
    (with-open [s (.prepareStatement c "UPDATE ap SET a[?:?]=?")]
      (let [m (.getParameterMetaData s)]
        (is (= [Types/INTEGER Types/INTEGER Types/ARRAY]
               (mapv #(.getParameterType m (int %)) [1 2 3])))))))

(deftest unsupported-raw-containers-never-become-whole-column-writes
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE raw_array (p point)")
    (exec! c "INSERT INTO raw_array VALUES ('(1,2)')")
    (is (= "0A000" (state c "UPDATE raw_array SET p[0]=9")))
    (is (= [["(1,2)"]] (rows c "SELECT p FROM raw_array")))))

(deftest multidimensional-and-open-targets-keep-runtime-shape
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE am (a int[])")
    (exec! c "INSERT INTO am VALUES (NULL)")
    (exec! c "UPDATE am SET a[2][3]=9")
    (is (= [["[2:2][3:3]={{9}}"]] (rows c "SELECT a FROM am")))
    (exec! c "UPDATE am SET a=ARRAY[[1,2],[3,4]]")
    (exec! c "UPDATE am SET a[1][2:2]=ARRAY[[8]]")
    (is (= [["{{1,8},{3,4}}"]] (rows c "SELECT a FROM am")))
    (is (= "2202E" (state c "UPDATE am SET a[1:1][2]=ARRAY[[9]]")))
    (exec! c "UPDATE am SET a=ARRAY[1,2,3,4]")
    (exec! c "UPDATE am SET a[:2]=ARRAY[8,9]")
    (exec! c "UPDATE am SET a[3:]=ARRAY[7,6]")
    (is (= [["{8,9,7,6}"]] (rows c "SELECT a FROM am")))
    (exec! c "UPDATE am SET a=NULL")
    (is (= "2202E" (state c "UPDATE am SET a[:2]=ARRAY[8,9]")))))

(deftest named-prepared-array-assignment-reads-the-current-container
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE named_array (a int[])")
    (exec! c "INSERT INTO named_array VALUES (ARRAY[1,2])")
    (with-open [s (.prepareStatement c "UPDATE named_array SET a[?]=?")]
      (.setPrepareThreshold ^PGStatement s 1)
      (let [write (fn [index value]
                    (.setInt s 1 index) (.setInt s 2 value)
                    (is (= 1 (.executeUpdate s))))]
        (write 2 9)
        (is (= [["{1,9}"]] (rows c "SELECT a FROM named_array")))
        (write 4 7)
        (is (= [["{1,9,NULL,7}"]] (rows c "SELECT a FROM named_array")))
        (exec! c "UPDATE named_array SET a=NULL")
        (write -2 6)
        (is (= [["[-2:-2]={6}"]] (rows c "SELECT a FROM named_array")))
        (.setAutoCommit c false)
        (write -1 8)
        (is (= [["[-2:-1]={6,8}"]] (rows c "SELECT a FROM named_array")))
        (.rollback c)
        (is (= [["[-2:-2]={6}"]] (rows c "SELECT a FROM named_array")))))))

(deftest prepare-rejects-conflicting-and-incompatible-assignment-parameters
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE narrow_array (a smallint[])")
    (is (= "42P08" (state c "PREPARE shared_index AS UPDATE narrow_array SET a[$1]=$1")))
    (is (= "42P08" (state c "PREPARE shared_slice AS UPDATE narrow_array SET a[$1:$2]=$1")))
    (is (= "42804" (state c "PREPARE text_index(text) AS UPDATE narrow_array SET a[$1]=2")))
    (is (= "42804" (state c (str "PREPARE temporal_rhs(timestamp) AS UPDATE narrow_array "
                                 "SET a[1]=$1"))))))

(deftest conflict-partials-fold-old-target-and-excluded-into-one-final-array
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE conflict_array (id int PRIMARY KEY,a int[] CHECK(a[1]<a[2]))")
    (exec! c "INSERT INTO conflict_array VALUES(1,ARRAY[1,2])")
    (is (= [["{2,9}"]]
           (rows c (str "INSERT INTO conflict_array VALUES(1,ARRAY[8,9]) "
                        "ON CONFLICT(id) DO UPDATE SET a[1]=conflict_array.a[2],"
                        "a[2]=excluded.a[2] RETURNING a"))))
    (is (= "23514"
           (state c (str "INSERT INTO conflict_array VALUES(1,ARRAY[3,4]) "
                         "ON CONFLICT(id) DO UPDATE SET a[1]=20,a[2]=10"))))
    (is (= [["{2,9}"]] (rows c "SELECT a FROM conflict_array")))
    (is (= "42601"
           (state c (str "INSERT INTO conflict_array VALUES(1,ARRAY[3,4]) "
                         "ON CONFLICT(id) DO UPDATE SET a=excluded.a,a[1]=1"))))
    (exec! c "CREATE SEQUENCE conflict_array_seq")
    (exec! c (str "INSERT INTO conflict_array VALUES(1,ARRAY[3,4]) "
                  "ON CONFLICT(id) DO UPDATE SET a[NULL]=nextval('conflict_array_seq') "
                  "WHERE false"))
    (is (= [["f"]] (rows c "SELECT is_called FROM conflict_array_seq")))))

(deftest insert-select-parameters-and-explicit-casts-retain-their-source-types
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE parameter_array(a int[])")
    (with-open [s (.prepareStatement c "INSERT INTO parameter_array(a[?]) SELECT ?")]
      (let [m (.getParameterMetaData s)]
        (is (= [Types/INTEGER Types/INTEGER]
               (mapv #(.getParameterType m %) [1 2]))))
      (.setInt s 1 2) (.setInt s 2 7) (.executeUpdate s))
    (with-open [s (.prepareStatement c "UPDATE parameter_array SET a[?::int]=?::int")]
      (.setString s 1 "3") (.setString s 2 "8") (.executeUpdate s))
    (is (= [["[2:3]={7,8}"]] (rows c "SELECT a FROM parameter_array")))))

(deftest partial-target-analysis-rejects-known-bad-types-without-effects
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE analysis_array(id int PRIMARY KEY,a int[])")
    (exec! c "CREATE SEQUENCE analysis_array_seq")
    (is (= "42804" (state c "PREPARE bad_static_index AS UPDATE analysis_array SET a['1'::text]=2")))
    (is (= "42804" (state c "PREPARE bad_static_rhs AS UPDATE analysis_array SET a[1]=TIMESTAMP '2020-01-01'")))
    (exec! c (str "PREPARE lazy_assignment AS UPDATE analysis_array "
                  "SET a[nextval('analysis_array_seq')::int]=nextval('analysis_array_seq')::int"))
    (is (= [["f"]] (rows c "SELECT is_called FROM analysis_array_seq")))))

(deftest conflict-assignment-parameters-survive-named-plan-reuse
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE conflict_parameter(id int PRIMARY KEY,a int[])")
    (exec! c "INSERT INTO conflict_parameter VALUES(1,ARRAY[1,2])")
    (with-open [s (.prepareStatement c
                                     (str "INSERT INTO conflict_parameter VALUES(1,ARRAY[9,9]) "
                                          "ON CONFLICT(id) DO UPDATE SET a[?]=?"))]
      (.setPrepareThreshold ^PGStatement s 1)
      (.setInt s 1 2) (.setInt s 2 8) (.executeUpdate s)
      (is (= [["{1,8}"]] (rows c "SELECT a FROM conflict_parameter")))
      (.setInt s 1 4) (.setInt s 2 6) (.executeUpdate s)
      (is (= [["{1,8,NULL,6}"]] (rows c "SELECT a FROM conflict_parameter"))))))

(deftest temporary-array-targets-analyze-the-owned-physical-schema
  (with-open [c (jdbc)]
    (exec! c "CREATE TEMP TABLE temp_assignment(a int[])")
    (exec! c "INSERT INTO temp_assignment VALUES(ARRAY[1,2])")
    (exec! c "UPDATE temp_assignment SET a[-1]=9")
    (is (= [["[-1:2]={9,NULL,1,2}"]]
           (rows c "SELECT a FROM temp_assignment")))
    (with-open [s (.prepareStatement c "UPDATE temp_assignment SET a[?::int]=?::int")]
      (.setString s 1 "2") (.setString s 2 "8") (.executeUpdate s))
    (is (= [["[-1:2]={9,NULL,1,8}"]]
           (rows c "SELECT a FROM temp_assignment")))))
