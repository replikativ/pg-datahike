(ns datahike.test.pg-case-regression-test
  "Application-facing slices from PostgreSQL 17.7 `case.sql`.

   The upstream table queries deliberately omit ORDER BY. PostgreSQL's output
   order there is a plan accident, so these gates compare bags while retaining
   row values, NULLs, and multiplicity."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"case" conn} {:port 0})]
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
        "/case?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- update-count [^Connection c sql]
  (with-open [st (.createStatement c)] (.executeUpdate st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs %) (range 1 (inc n)))))
          acc)))))

(defn- bag [^Connection c sql]
  (frequencies (rows c sql)))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE case_tbl (i integer, f double precision)")
  (exec! c "CREATE TABLE case2_tbl (i integer, j integer)")
  (exec! c (str "INSERT INTO case_tbl VALUES "
                "(1,10.1),(2,20.2),(3,-30.3),(4,NULL)"))
  (exec! c (str "INSERT INTO case2_tbl VALUES "
                "(1,-1),(2,-2),(3,-3),(2,-4),(1,NULL),(NULL,-6)")))

(deftest postgres-case-scalar-lazy-slice
  ;; PostgreSQL 17.7 src/test/regress/sql/case.sql:32-68.
  (with-open [c (jdbc)]
    (is (= [["3" "3"]]
           (rows c "SELECT '3', CASE WHEN 1 < 2 THEN 3 END")))
    (is (= [["<NULL>" nil]]
           (rows c "SELECT '<NULL>', CASE WHEN 1 > 2 THEN 3 END")))
    (is (= [["3" "3"]]
           (rows c "SELECT '3', CASE WHEN 1 < 2 THEN 3 ELSE 4 END")))
    (is (= [["4" "4"]]
           (rows c "SELECT '4', CASE WHEN 1 > 2 THEN 3 ELSE 4 END")))
    (is (= [["6" "6"]]
           (rows c (str "SELECT '6', CASE WHEN 1 > 2 THEN 3 "
                        "WHEN 4 < 5 THEN 6 ELSE 7 END"))))
    (is (= [["7" nil]]
           (rows c "SELECT '7', CASE WHEN random() < 0 THEN 1 END")))
    (is (= [["1"]]
           (rows c "SELECT CASE WHEN 1=0 THEN 1/0 WHEN 1=1 THEN 1 ELSE 2/0 END")))
    (is (= [["1"]]
           (rows c "SELECT CASE 1 WHEN 0 THEN 1/0 WHEN 1 THEN 1 ELSE 2/0 END")))))

(deftest postgres-case-table-null-functions-slice
  ;; PostgreSQL 17.7 src/test/regress/sql/case.sql:74-138.
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [["1"]] (rows c "SELECT CASE 'a' WHEN 'a' THEN 1 ELSE 2 END")))
    (is (= {[nil] 2 ["3"] 1 ["4"] 1}
           (bag c "SELECT CASE WHEN i >= 3 THEN i END FROM case_tbl")))
    (is (= {["1"] 1 ["2"] 1 ["6"] 1 ["8"] 1}
           (bag c "SELECT CASE WHEN i >= 3 THEN i+i ELSE i END FROM case_tbl")))
    (is (= {["1" "one"] 1 ["2" "two"] 1
            ["3" "big"] 1 ["4" "big"] 1}
           (bag c (str "SELECT i, CASE WHEN i < 0 THEN 'small' "
                       "WHEN i = 0 THEN 'zero' WHEN i = 1 THEN 'one' "
                       "WHEN i = 2 THEN 'two' ELSE 'big' END FROM case_tbl"))))
    (is (= {["one"] 1 ["two"] 1 ["big"] 2}
           (bag c (str "SELECT CASE WHEN (i < 0 OR i < 0) THEN 'small' "
                       "WHEN (i = 0 OR i = 0) THEN 'zero' "
                       "WHEN (i = 1 OR i = 1) THEN 'one' "
                       "WHEN (i = 2 OR i = 2) THEN 'two' ELSE 'big' END "
                       "FROM case_tbl"))))
    (is (= {["4" nil] 1}
           (bag c "SELECT * FROM case_tbl WHERE COALESCE(f,i) = 4")))
    (is (= {} (bag c "SELECT * FROM case_tbl WHERE NULLIF(f,i) = 2")))
    (is (= {["10.1"] 6 ["20.2"] 6 ["-30.3"] 6
            ["1"] 2 ["2"] 2 ["3"] 1 ["-6"] 1}
           (bag c "SELECT COALESCE(a.f,b.i,b.j) FROM case_tbl a, case2_tbl b")))
    (is (= {["4" nil "2" "-2"] 1 ["4" nil "2" "-4"] 1}
           (bag c (str "SELECT * FROM case_tbl a, case2_tbl b "
                       "WHERE COALESCE(a.f,b.i,b.j) = 2"))))
    (is (= {[nil "1"] 2 ["2" "1"] 2 ["3" "1"] 2 ["4" "1"] 2
            ["1" "2"] 2 [nil "2"] 2 ["3" "2"] 2 ["4" "2"] 2
            ["1" "3"] 1 ["2" "3"] 1 [nil "3"] 1 ["4" "3"] 1
            ["1" nil] 1 ["2" nil] 1 ["3" nil] 1 ["4" nil] 1}
           (bag c "SELECT NULLIF(a.i,b.i), NULLIF(b.i,4) FROM case_tbl a, case2_tbl b")))
    (is (= {["4" nil "2" "-2"] 1 ["4" nil "2" "-4"] 1}
           (bag c (str "SELECT * FROM case_tbl a, case2_tbl b "
                       "WHERE COALESCE(f,b.i) = 2"))))))

(deftest postgres-case-update-from-slice
  ;; PostgreSQL 17.7 src/test/regress/sql/case.sql:155-173.
  (with-open [c (jdbc)]
    (seed! c)
    (is (= 4 (update-count c (str "UPDATE case_tbl SET i = CASE WHEN i >= 3 "
                                  "THEN -i ELSE 2*i END"))))
    (is (= {["2" "10.1"] 1 ["4" "20.2"] 1
            ["-3" "-30.3"] 1 ["-4" nil] 1}
           (bag c "SELECT * FROM case_tbl")))
    (is (= 4 (update-count c (str "UPDATE case_tbl SET i = CASE WHEN i >= 2 "
                                  "THEN 2*i ELSE 3*i END"))))
    (is (= {["4" "10.1"] 1 ["8" "20.2"] 1
            ["-9" "-30.3"] 1 ["-12" nil] 1}
           (bag c "SELECT * FROM case_tbl")))
    (is (= 1
           (update-count c (str "UPDATE case_tbl SET i = CASE WHEN b.i >= 2 "
                                "THEN 2*j ELSE 3*j END FROM case2_tbl b "
                                "WHERE j = -case_tbl.i"))))
    (is (= {["-8" "10.1"] 1 ["8" "20.2"] 1
            ["-9" "-30.3"] 1 ["-12" nil] 1}
           (bag c "SELECT * FROM case_tbl")))))
