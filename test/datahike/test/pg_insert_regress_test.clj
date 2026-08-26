(ns datahike.test.pg-insert-regress-test
  "Application-facing slices admitted from PostgreSQL 17's insert.sql."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql]
  (.execute *handler* sql))

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))

(deftest postgres-insert-default-and-row-width-slice
  (run "CREATE TABLE ir (a int, b int NOT NULL, c text DEFAULT 'testing')")
  (is (= "23502" (.-sqlstate ^PgWireServer$QueryResult
                  (run "INSERT INTO ir (a,b,c) VALUES (DEFAULT,DEFAULT,DEFAULT)"))))
  (run "INSERT INTO ir (b,c) VALUES (3,DEFAULT)")
  (run "INSERT INTO ir (a,b,c) VALUES (DEFAULT,5,DEFAULT)")
  (run "INSERT INTO ir VALUES (DEFAULT,5,'test')")
  (run "INSERT INTO ir VALUES (DEFAULT,7)")
  (is (= [[nil "3" "testing"] [nil "5" "testing"]
          [nil "5" "test"] [nil "7" "testing"]]
         (rows "SELECT * FROM ir")))
  (doseq [[sql message]
          [["INSERT INTO ir (a,b,c) VALUES (DEFAULT,DEFAULT)"
            "more target columns than expressions"]
           ["INSERT INTO ir (a,b,c) VALUES (1,2)"
            "more target columns than expressions"]
           ["INSERT INTO ir (a) VALUES (1,2)"
            "more expressions than target columns"]
           ["INSERT INTO ir (a) VALUES (DEFAULT,DEFAULT)"
            "more expressions than target columns"]]]
    (let [r (run sql)]
      (is (= "42601" (.-sqlstate ^PgWireServer$QueryResult r)))
      (is (re-find (re-pattern message) (.-error ^PgWireServer$QueryResult r)))))
  (is (= 4 (count (rows "SELECT * FROM ir"))))
  (run (str "INSERT INTO ir VALUES (10,20,'40'),(-1,2,DEFAULT),"
            "((SELECT 2),(SELECT i FROM (VALUES (3)) AS foo(i)),'values are fun!')"))
  (is (= [["10" "20" "40"] ["2" "3" "values are fun!"]
          ["-1" "2" "testing"]]
         (rows "SELECT * FROM ir WHERE a IS NOT NULL ORDER BY a DESC"))))
