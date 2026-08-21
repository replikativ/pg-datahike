(ns datahike.test.pg-correlated-subquery-test
  "Correlated scalar subqueries in the SELECT list, and the
   aggregate-over-an-empty-relation rule they depend on.

   `SELECT p.id, (SELECT count(*) FROM ch WHERE ch.pid = p.id) FROM p` is
   one of the most common shapes in application SQL, and it was WRONG for
   every row: the deferral machinery detected the correlation and ran the
   inner per outer row, but the inner TRANSLATOR turned `ch.pid = p.id`
   into an implicit JOIN against the relation `p` -- adding p to the
   inner FROM -- so the correlation predicate dissolved and every row got
   the same uncorrelated answer.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn cs-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"cs" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each cs-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/cs?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE p (id int, nm text)")
  (exec! c "CREATE TABLE ch (id int, pid int)")
  (exec! c "INSERT INTO p VALUES (1,'a'),(2,'b'),(3,'c')")
  ;; p=1 has two children, p=2 has one, p=3 has none -- so the correlated
  ;; count differs per row AND one row exercises the empty-aggregate rule.
  (exec! c "INSERT INTO ch VALUES (1,1),(2,1),(3,2)"))

(deftest correlated-scalar-subquery-in-the-select-list
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["2" "1" "0"]
           (col c 2 "SELECT id, (SELECT count(*) FROM ch WHERE ch.pid = p.id) AS c FROM p ORDER BY id"))
        "the count must differ per outer row, and be 0 where there are none")
    (is (= ["2" "1" "0"]
           (col c 2 (str "SELECT id, (SELECT count(*) FROM ch c2 WHERE c2.pid = p.id) AS c "
                         "FROM p ORDER BY id")))
        "with the inner table aliased")
    (is (= ["2" "3" nil]
           (col c 2 "SELECT id, (SELECT max(id) FROM ch WHERE ch.pid = p.id) AS c FROM p ORDER BY id"))
        "a non-COUNT aggregate over no rows is NULL")
    (testing "an explicit outer alias"
      (is (= ["2" "1" "0"]
             (col c 2 (str "SELECT id, (SELECT count(*) FROM ch WHERE ch.pid = o.id) AS c "
                           "FROM p o ORDER BY id")))))
    (testing "self-correlation -- the same table on both sides"
      (is (= ["a" "b" "c"]
             (col c 2 "SELECT id, (SELECT nm FROM p p2 WHERE p2.id = p.id) AS c FROM p ORDER BY id"))))
    (testing "the outer correlation value may itself be NULL"
      (exec! c "INSERT INTO p VALUES (4, NULL)")
      (is (= ["2" "1" "0" "0"]
             (col c 2 (str "SELECT id, (SELECT count(*) FROM ch WHERE ch.pid = p.id) AS c "
                           "FROM p ORDER BY id")))
          "no child matches a NULL parent id, so the count is 0"))))

(deftest aggregate-over-an-empty-relation-in-a-scalar-subquery
  (with-open [c (jdbc)]
    (seed! c)
    ;; SQL requires an aggregate with no GROUP BY to produce ONE row even
    ;; when nothing matches -- COUNT is 0, everything else NULL. Datalog
    ;; returns no rows at all, so the row has to be synthesised. exec-select
    ;; already did that for the top-level result; each of the THREE scalar
    ;; subquery evaluators ran d/q directly and answered NULL.
    (is (= "0" (one c "SELECT (SELECT count(*) FROM ch WHERE pid = 999)")))
    (is (nil? (one c "SELECT (SELECT min(id) FROM ch WHERE pid = 999)")))
    (is (= "3" (one c "SELECT (SELECT count(*) FROM ch)")))
    (testing "the top-level forms, which already worked"
      (is (= "0" (one c "SELECT count(*) FROM ch WHERE pid = 999")))
      (is (nil? (one c "SELECT min(id) FROM ch WHERE pid = 999"))))
    (testing "but a GROUP BY means zero matching GROUPS, hence zero rows"
      (is (= [] (col c 1 "SELECT pid FROM ch WHERE pid = 999 GROUP BY 1"))))))
