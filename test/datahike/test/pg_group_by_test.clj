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
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
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

;; ---------------------------------------------------------------------------
;; ORDER BY an aggregate that is not projected
;; ---------------------------------------------------------------------------

(defn- exec!! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(deftest order-by-an-unprojected-aggregate-counts-rows-not-groups
  ;; `GROUP BY c ORDER BY COUNT(c) DESC` sorted arbitrarily, and adding
  ;; `COUNT(c)` to the SELECT list made the same query correct.
  ;;
  ;; Datalog `:find` is a SET. The projection puts the entity id in
  ;; `:with` so the bag survives to the aggregate; the aggregate
  ;; contributed by ORDER BY did not, so the rows collapsed to one per
  ;; distinct group key BEFORE counting and every count came out 1.
  (with-open [c (jdbc)]
    (exec!! c "CREATE TABLE sc (g text, city text)")
    (exec!! c (str "INSERT INTO sc VALUES "
                   "('K-6','A'),('K-6','A'),('K-6','A'),('K-6','A'),('K-6','A'),"
                   "('K-8','A'),('K-8','A'),('K-8','A'),('K-8','A'),"
                   "('9-12','A'),('7-8','A'),('K-12','A'),(NULL,'A')"))
    (is (= [["K-6"]]
           (rows c (str "SELECT g FROM sc WHERE city='A' GROUP BY g "
                        "ORDER BY COUNT(g) DESC LIMIT 1"))))
    (testing "the whole ordering, against the projected control"
      (is (= (mapv (comp vector first)
                   (rows c (str "SELECT g, COUNT(g) FROM sc WHERE city='A' "
                                "GROUP BY g ORDER BY COUNT(g) DESC")))
             (rows c (str "SELECT g FROM sc WHERE city='A' GROUP BY g "
                          "ORDER BY COUNT(g) DESC")))
          "projecting the aggregate must not change the order"))))

(deftest order-by-an-aggregate-does-not-bind-to-a-subquery-named-for-it
  ;; PostgreSQL names `(SELECT COUNT(x) FROM …)` `count`. The ORDER BY
  ;; aggregate was resolved against output columns BY NAME, so it bound
  ;; to that scalar subquery's column -- a constant -- and every group
  ;; sorted by the same value. The name now has to belong to a find
  ;; element that really is this aggregate.
  (with-open [c (jdbc)]
    (exec!! c "CREATE TABLE gs (id int, country text)")
    (exec!! c (str "INSERT INTO gs SELECT g, CASE WHEN g <= 466 THEN 'CZE' "
                   "ELSE 'SVK' END FROM generate_series(1,597) g"))
    (is (= [["CZE" "597"] ["SVK" "597"]]
           (rows c (str "SELECT country, (SELECT COUNT(id) FROM gs) FROM gs "
                        "GROUP BY country ORDER BY COUNT(id) DESC")))
        "CZE has 466 rows and SVK 131, so CZE sorts first")
    (testing "HAVING on a projected aggregate still resolves by name"
      (is (= [["CZE" "466"]]
             (rows c (str "SELECT country, COUNT(id) FROM gs GROUP BY country "
                          "HAVING COUNT(id) > 200")))))))

(deftest order-by-an-output-alias
  ;; PostgreSQL resolves an ORDER BY name against the OUTPUT columns
  ;; first. Two shapes failed with 42703:
  ;;
  ;;   ORDER BY <alias> LIMIT n   a secondary-index probe for
  ;;                              `ORDER BY <col> LIMIT n` resolved the
  ;;                              name as an INPUT column and let the
  ;;                              42703 escape, instead of declining the
  ;;                              fast path. Without the LIMIT the probe
  ;;                              never ran, so it looked like a LIMIT bug.
  ;;
  ;;   SUM(x)/12 AS m … ORDER BY m   an expression over aggregates is
  ;;                              spliced in after the query, and its
  ;;                              alias is not in `find-aliases` at all.
  (with-open [c (jdbc)]
    (exec!! c "CREATE TABLE ym (customerid int, consumption float)")
    (exec!! c "INSERT INTO ym VALUES (1,10),(1,20),(2,30),(3,5)")
    (testing "alias of a plain column, with a LIMIT"
      (is (= [["3"] ["2"]] (rows c "SELECT customerid AS c FROM ym ORDER BY c DESC LIMIT 2"))))
    (testing "alias of an expression over an aggregate"
      (is (= [["3"]]
             (rows c (str "SELECT customerid FROM ym GROUP BY customerid "
                          "ORDER BY SUM(consumption)/12 ASC LIMIT 1")))))
    (testing "and a genuinely unknown name is still 42703"
      (is (thrown-with-msg? Exception #"column \"nosuch\" does not exist"
                            (rows c "SELECT customerid FROM ym ORDER BY nosuch LIMIT 2"))))))
