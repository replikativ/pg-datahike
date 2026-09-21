(ns datahike.test.pg-like-pattern-test
  "A LIKE pattern that is not a literal.

   In value position the pattern was compiled to a regex at TRANSLATE
   time, whatever it was. A column, a parameter or NULL arrives there as
   a logic variable or the null sentinel, and `(str …)` of those
   compiles to a regex matching their printed form and nothing else:

     SELECT s LIKE p FROM t      false for every row, whatever p held
     SELECT 'a' LIKE NULL        false, where PostgreSQL answers NULL

   The WHERE path has always deferred a non-literal pattern to a per-row
   matcher -- pgjdbc rewrites every pattern literal to a parameter under
   the extended protocol, so it had to. This is the same expression in
   the other position.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn- lk-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"lk" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each lk-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/lk?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- param-rows [^Connection c sql param]
  (with-open [st (.prepareStatement c sql)]
    (if (nil? param)
      (.setNull st 1 java.sql.Types/VARCHAR)
      (.setString st 1 param))
    (with-open [rs (.executeQuery st)]
      (let [n (.getColumnCount (.getMetaData rs))]
        (loop [acc []]
          (if (.next rs)
            (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
            acc))))))

(deftest a-pattern-that-is-not-a-literal
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE lk (id int, s text, p text)")
    (exec! c "INSERT INTO lk VALUES (1,'abc','a%'),(2,'xyz','z%'),(3,'abc',NULL),(4,NULL,'a%')")
    (testing "a column as the pattern, in value position"
      (is (= [["1" "t"] ["2" "f"] ["3" nil] ["4" nil]]
             (rows c "SELECT id, s LIKE p FROM lk ORDER BY id")))
      (is (= [["1" "f"] ["2" "t"] ["3" nil] ["4" nil]]
             (rows c "SELECT id, s NOT LIKE p FROM lk ORDER BY id")))
      (is (= [["1" "t"] ["2" "f"] ["3" nil] ["4" nil]]
             (rows c "SELECT id, s ILIKE p FROM lk ORDER BY id"))))
    (testing "NULL as the pattern is NULL, not false"
      (is (= [[nil]] (rows c "SELECT 'a' LIKE NULL")))
      (is (= [["t"]] (rows c "SELECT ('a' LIKE NULL) IS NULL")))
      ;; the subject being NULL was already right
      (is (= [[nil]] (rows c "SELECT NULL LIKE 'a%'"))))
    (testing "a parameter as the pattern -- what every driver sends"
      (is (= [["1"] ["3"]] (param-rows c "SELECT id FROM lk WHERE s LIKE ? ORDER BY id" "a%")))
      (is (= [["1" "t"] ["2" "f"] ["3" "t"] ["4" nil]]
             (param-rows c "SELECT id, s LIKE ? FROM lk ORDER BY id" "a%")))
      (is (= [["1" nil] ["2" nil] ["3" nil] ["4" nil]]
             (param-rows c "SELECT id, s LIKE ? FROM lk ORDER BY id" nil))))
    (testing "a literal pattern is unchanged, escapes included"
      (is (= [["1" "t"] ["2" "f"] ["3" "t"] ["4" nil]]
             (rows c "SELECT id, s LIKE 'a%' FROM lk ORDER BY id")))
      (is (= [["1" "t"] ["2" "f"] ["3" "t"] ["4" nil]]
             (rows c "SELECT id, s LIKE 'a_c' FROM lk ORDER BY id")))
      (is (= [["t"]] (rows c "SELECT 'a%c' LIKE 'a\\%c' ESCAPE '\\'")))
      (is (= [["t"]] (rows c "SELECT 'a%c' LIKE 'a$%c' ESCAPE '$'"))))))
