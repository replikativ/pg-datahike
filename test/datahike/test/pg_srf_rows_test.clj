(ns datahike.test.pg-srf-rows-test
  "Set-returning functions in FROM have to produce SCANNABLE rows.

   A table function is materialised into a virtual table in a
   speculative db. Two things were missing, and both failed silently —
   the queries were well-formed and answered, just with nothing in them.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*handler* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))

(deftest virtual-tables-carry-a-row-marker
  (testing "without a row-existence marker `count(*)` and `SELECT *` had
            no column to enumerate, so a scan returned ZERO rows even
            though the rows were there — `count(g)` on the same query
            correctly answered 10. sequence->virtual-table already
            transacts one and its comment names this exact hazard."
    (is (= [["10"]] (rows "SELECT count(*) FROM generate_series(1,10) g")))
    (is (= [["10"]] (rows "SELECT count(g) FROM generate_series(1,10) g")))
    (is (= [["3"]] (rows "SELECT count(*) FROM unnest(ARRAY[1,2,3]) u")))
    (is (= [["1"] ["2"] ["3"]] (rows "SELECT * FROM generate_series(1,3) g")))))

(deftest insert-select-from-a-set-returning-function
  (testing "the source query ran against the PLAIN db, where the virtual
            table does not exist, so the standard bulk-load idiom
            silently reported INSERT 0 0 while the same SELECT on its own
            returned its rows"
    (run "CREATE TABLE bulk (id int PRIMARY KEY, v int)")
    (is (= "INSERT 0 5"
           (.-commandTag ^PgWireServer$QueryResult
            (run "INSERT INTO bulk SELECT g, g*2 FROM generate_series(1,5) g"))))
    (is (= [["5"]] (rows "SELECT count(*) FROM bulk")))
    (is (= [["10"]] (rows "SELECT v FROM bulk WHERE id = 5"))))
  (testing "and the SELECT alone still works, which is what made the
            no-op hard to notice"
    (is (= 5 (count (rows "SELECT g, g*2 FROM generate_series(1,5) g"))))))
