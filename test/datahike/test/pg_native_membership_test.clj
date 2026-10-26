(ns datahike.test.pg-native-membership-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.schema :as pgs])
  (:import [java.sql Connection DriverManager]
           [org.postgresql PGStatement]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/native_membership?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- rows [^Connection c text]
  (with-open [s (.createStatement c) r (.executeQuery s text)]
    (let [width (.getColumnCount (.getMetaData r))]
      (loop [result []]
        (if (.next r)
          (recur (conj result (mapv #(.getString r (int %)) (range 1 (inc width)))))
          result)))))

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn
                    (vec (concat
                          (for [ident [:nl/a :nl/b :nr/a :nr/b :ne/a :nm/value
                                       :nh/visible :nh/hidden :np/a :np/b
                                       :nd/a :nd/b :ndh/visible :ndh/hidden :mr/value
                                       :owners/id :targets/value :targets/note :foreign/value]]
                            {:db/ident ident :db/valueType :db.type/long
                             :db/cardinality :db.cardinality/one})
                          [{:db/ident :nm/tag :db/valueType :db.type/long
                            :db/cardinality :db.cardinality/many}
                           {:db/ident :nsingle/tag :db/valueType :db.type/long
                            :db/cardinality :db.cardinality/many}
                           {:db/ident :owners/target :db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/one}
                           {:db/ident :mr/owner :db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/one}
                           {:db/ident (pgs/row-marker-attr "mr") :db/valueType :db.type/boolean
                            :db/cardinality :db.cardinality/one}])))
        (d/transact conn [{:nl/a 1} {:nl/b 2} {:nl/a 3 :nl/b 3}
                          {:nr/a 10} {:nr/b 20} {:nr/a 30 :nr/b 30}
                          {:nm/tag #{1 2 3}} {:nm/value 4} {:nm/tag #{4 5} :nm/value 5}
                          {:nsingle/tag #{1 2 3}} {:nsingle/tag #{4 5}}
                          {:nh/visible 1} {:nh/hidden 2} {:np/a 1}
                          {:nd/a 1} {:nd/b 2} {:nd/a 3 :nd/b 3}
                          {:ndh/visible 1} {:ndh/hidden 2}
                          {:db/id "target-a" :targets/value 7}
                          {:db/id "target-b" :targets/note 8}
                          {:db/id "other" :foreign/value 9}
                          {:db/id "owner-a" :owners/id 1 :owners/target "target-a"}
                          {:db/id "owner-b" :owners/id 2 :owners/target "target-b"}
                          {:db/id "owner-c" :owners/id 3 :owners/target "other"}
                          {:owners/id 4}
                          {:mr/value 7 :mr/owner "owner-a" (pgs/row-marker-attr "mr") true}
                          {:mr/owner "owner-b" (pgs/row-marker-attr "mr") true}
                          {:mr/value 99 :mr/owner "owner-c"}])
        (pgs/set-hint! conn :nh/hidden {:hidden true})
        (pgs/set-hint! conn :ndh/hidden {:hidden true})
        (let [{:keys [server]} (pg/start-server {"native_membership" conn} {:port 0})]
          (try
            (binding [*port* (.getPort server) *conn* conn]
              (with-open [c (jdbc) s (.createStatement c)]
                (.execute s "CREATE TABLE marker_control(a int,b int)")
                (.execute s "INSERT INTO marker_control VALUES(NULL,NULL),(1,NULL),(NULL,2)"))
              ;; Native datoms sharing the namespace are not SQL rows when
              ;; the table has an authoritative SQL row marker.
              (d/transact conn [{:marker_control/a 99}])
              (f))
            (finally (.stop server))))
        (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :once fixture)

(deftest native-rows-exist-through-any-attribute
  (with-open [c (jdbc)]
    (is (= [[nil "2"] ["1" nil] ["3" "3"]]
           (rows c "SELECT a,b FROM nl ORDER BY a NULLS FIRST")))
    (is (= [["3" "2"]] (rows c "SELECT count(*),count(a) FROM nl")))
    (is (= [["2"]] (rows c "SELECT b FROM nl WHERE a IS NULL")))
    (is (= [["0"]] (rows c "SELECT count(*) FROM ne")))))

(deftest cross-join-anchors-every-alias-and-preserves-bags
  (with-open [c (jdbc)]
    (is (= [["9"]] (rows c "SELECT count(*) FROM nl l,nr r")))
    (is (= 9 (count (rows c "SELECT 1 FROM nl l CROSS JOIN nr r"))))
    (is (= (frequencies (for [l [nil "1" "3"] r [nil "10" "30"]] [l r]))
           (frequencies (rows c "SELECT l.a,r.a FROM nl l,nr r"))))
    (is (= [["9"]]
           (rows c "SELECT count(*) FROM (SELECT l.a AS x,r.a AS y FROM nl l,nr r) d")))))

(deftest self-joins-use-distinct-entity-bindings
  (with-open [c (jdbc)]
    (is (= [["9"]] (rows c "SELECT count(*) FROM nl l,nl r")))
    (is (= [["1" "1"] ["3" "3"]]
           (rows c "SELECT l.a,r.a FROM nl l JOIN nl r ON l.a=r.a ORDER BY l.a")))))

(deftest prepared-cross-join-keeps-sparse-right-rows
  (with-open [c (jdbc)
              ps (.prepareStatement c "SELECT count(*) FROM nl l,nr r WHERE l.a>=?")]
    (.setPrepareThreshold ^PGStatement ps 1)
    (doseq [[minimum expected] [[1 6] [3 3] [9 0] [1 6]]]
      (.setLong ps 1 minimum)
      (with-open [r (.executeQuery ps)]
        (is (.next r))
        (is (= expected (.getLong r 1)))))))

(deftest membership-deduplicates-multivalued-attributes
  (with-open [c (jdbc)]
    (is (= [["3"]] (rows c "SELECT count(*) FROM nm")))
    (is (= [[nil] ["4"] ["5"]]
           (rows c "SELECT value FROM nm ORDER BY value NULLS FIRST")))
    (is (= [["9"]] (rows c "SELECT count(*) FROM nl l,nm m")))
    (is (= [["2"]] (rows c "SELECT count(*) FROM nsingle")))
    (is (= 2 (count (rows c "SELECT 1 FROM nsingle"))))))

(deftest sql-row-markers-retain-all-null-rows
  (with-open [c (jdbc)]
    (is (= [["3"]] (rows c "SELECT count(*) FROM marker_control")))
    (is (= [["9"]] (rows c "SELECT count(*) FROM nl l,marker_control m")))
    (is (= 3 (count (rows c "SELECT a,b FROM marker_control"))))
    (is (= [["0"]] (rows c "SELECT count(*) FROM marker_control WHERE a=99")))
    (is (= [["0"]]
           (rows c "SELECT count(*) FROM (VALUES(99)) l(v) JOIN marker_control r ON l.v=r.a")))
    (is (= [[nil]]
           (rows c "SELECT r.a FROM (VALUES(99)) l(v) LEFT JOIN marker_control r ON l.v=r.a")))))

(deftest nullable-outer-alias-is-not-anchored-in-the-outer-query
  (with-open [c (jdbc)]
    (is (= [["3"]] (rows c "SELECT count(*) FROM nl l LEFT JOIN ne r ON l.a=r.a")))
    (is (= [["9"]] (rows c "SELECT count(*) FROM nl l LEFT JOIN nr r ON true")))
    (is (= [[nil nil] ["1" nil] ["3" nil]]
           (rows c "SELECT l.a,r.a FROM nl l LEFT JOIN ne r ON true ORDER BY l.a NULLS FIRST")))))

(deftest hidden-columns-still-establish-native-row-membership
  (with-open [c (jdbc)]
    (is (= [["2"]] (rows c "SELECT count(*) FROM nh")))
    (is (= [[nil] ["1"]] (rows c "SELECT visible FROM nh ORDER BY visible NULLS FIRST")))))

(deftest left-owned-refs-require-membership-of-the-joined-table
  (with-open [c (jdbc)]
    (let [result (rows c (str "SELECT o.id,t.value,t.db_id FROM owners o "
                              "LEFT JOIN targets t ON o.target=t.db_id ORDER BY o.id"))]
      (is (= [["1" "7"] ["2" nil] ["3" nil] ["4" nil]]
             (mapv #(subvec % 0 2) result)))
      (is (= [false false true true] (mapv #(nil? (nth % 2)) result))))))

(deftest prepared-membership-does-not-cache-entities
  (with-open [c (jdbc) ps (.prepareStatement c "SELECT count(*) FROM np")]
    (.setPrepareThreshold ^PGStatement ps 1)
    (with-open [r (.executeQuery ps)] (is (.next r)) (is (= 1 (.getLong r 1))))
    (d/transact *conn* [{:np/b 2}])
    (with-open [r (.executeQuery ps)] (is (.next r)) (is (= 2 (.getLong r 1))))))

(deftest right-owned-refs-cannot-admit-unmarked-rows
  (with-open [c (jdbc)]
    (let [result (rows c (str "SELECT o.id,r.value,r.db_id FROM owners o "
                              "LEFT JOIN mr r ON r.owner=o.db_id ORDER BY o.id"))]
      (is (= [["1" "7"] ["2" nil] ["3" nil] ["4" nil]]
             (mapv #(subvec % 0 2) result)))
      (is (= [false false true true] (mapv #(nil? (nth % 2)) result))))
    (is (= [["1" "8"] ["2" nil] ["3" nil] ["4" nil]]
           (rows c (str "SELECT o.id,r.value+1 FROM owners o "
                        "LEFT JOIN mr r ON r.owner=o.db_id ORDER BY o.id"))))))

(deftest chained-nullable-left-ref-owner-stays-null-extended
  (with-open [c (jdbc)]
    (let [text (str "FROM marker_control m LEFT JOIN owners o ON m.a=o.id "
                    "LEFT JOIN mr r ON r.owner=o.db_id")]
      (is (= [["3"]] (rows c (str "SELECT count(*) " text))))
      (is (= (frequencies [[nil nil] [nil nil] ["1" "7"]])
             (frequencies (rows c (str "SELECT o.id,r.value " text))))))))

(deftest sparse-grouping-and-other-outer-directions
  (with-open [c (jdbc)]
    (is (= [[nil "1"] ["1" "1"] ["3" "1"]]
           (rows c "SELECT a,count(*) FROM nl GROUP BY a ORDER BY a NULLS FIRST")))
    (is (= [["3"]] (rows c "SELECT count(*) FROM ne l RIGHT JOIN nr r ON true")))
    (is (= (frequencies [["1" nil] [nil nil] ["3" nil]
                         [nil "10"] [nil nil] [nil "30"]])
           (frequencies (rows c "SELECT l.a,r.a FROM nl l FULL JOIN nr r ON l.a=r.a"))))
    (is (= [["1"]] (rows c "SELECT DISTINCT 1 FROM nl l,nr r")))))

(deftest native-dml-uses-the-same-membership-definition
  (with-open [c (jdbc) s (.createStatement c)]
    (is (= 1 (.executeUpdate s "UPDATE nd SET b=9 WHERE a IS NULL")))
    (is (= [["9"]] (rows c "SELECT b FROM nd WHERE a IS NULL")))
    (is (= 1 (.executeUpdate s "DELETE FROM nd WHERE a IS NULL")))
    (is (= [["2"]] (rows c "SELECT count(*) FROM nd")))
    (is (= 1 (.executeUpdate s "DELETE FROM ndh WHERE visible IS NULL")))
    (is (= [["1"]] (rows c "SELECT count(*) FROM ndh")))
    (is (= 0 (.executeUpdate s "DELETE FROM marker_control WHERE a=99")))
    (is (= 1 (count (d/q '{:find [?e] :where [[?e :marker_control/a 99]]}
                         (d/db *conn*)))))))
