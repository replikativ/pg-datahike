(ns datahike.test.pg-group-by-test
  "GROUP BY semantics through the pgwire server.

   - GROUP BY without an aggregate must deduplicate by the grouped columns.
     translate-select used to add the FROM table's entity var to `:find`
     for stable insertion-order sorting; that worked for the no-GROUP-BY
     case but collapsed dedup when GROUP BY was present (set-semantics
     keys on the full :find tuple, and each row has a unique eid).

   - HAVING that references an aggregate not in the SELECT list still
     needs the aggregate computed. translate-select used to attach a
     :having spec with :col-idx nil, which the server's apply-having
     then dropped silently.

   Both fixes are local to translate-select; covered end-to-end here."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn group-by-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn
                  [{:db/ident :sales/region :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :sales/amount :db/valueType :db.type/double
                    :db/cardinality :db.cardinality/one}])
      (d/transact conn
                  [{:sales/region "east" :sales/amount 100.0}
                   {:sales/region "east" :sales/amount 200.0}
                   {:sales/region "east" :sales/amount  50.0}
                   {:sales/region "west" :sales/amount 300.0}
                   {:sales/region "west" :sales/amount 400.0}])
      (let [{:keys [server]} (pg/start-server {"groupby" conn} {:port 0})]
        (try
          (binding [*conn* conn *port* (.getPort server)]
            (f))
          (finally
            (.stop server)
            (d/release conn)
            (d/delete-database cfg)))))))

(use-fixtures :each group-by-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/groupby?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs ^long %) (range 1 (inc n)))))
          acc)))))

;; ---------------------------------------------------------------------------
;; GROUP BY without aggregate — deduplicates

(deftest group-by-distinct-rows
  (with-open [c (jdbc)]
    (is (= [["east"] ["west"]]
           (sort (rows c "SELECT region FROM sales GROUP BY region"))))))

;; ---------------------------------------------------------------------------
;; GROUP BY + aggregate already works (regression guard)

(deftest group-by-with-aggregate
  (with-open [c (jdbc)]
    (is (= [["east" "350"] ["west" "700"]]
           (sort
            (rows c "SELECT region, SUM(amount) FROM sales GROUP BY region"))))))

;; ---------------------------------------------------------------------------
;; HAVING with aggregate in SELECT list — already wired, regression guard

(deftest having-with-aggregate-in-select
  (with-open [c (jdbc)]
    (is (= [["west" "700"]]
           (rows c "SELECT region, SUM(amount) FROM sales
                    GROUP BY region HAVING SUM(amount) > 500")))))

;; ---------------------------------------------------------------------------
;; HAVING with aggregate NOT in SELECT — fix #1b

(deftest having-with-aggregate-not-in-select
  (with-open [c (jdbc)]
    (is (= [["west"]]
           (rows c "SELECT region FROM sales
                    GROUP BY region HAVING SUM(amount) > 500")))))
