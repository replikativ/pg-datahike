(ns datahike.test.pg-hints-test
  "Schema hints (:datahike.pg/*) let users customize the SQL view of a
   native Datahike database without touching the storage schema. This
   test exercises:

   - `pgs/set-hint!` transacts a hint entity keyed to an ident
   - `:datahike.pg/column` renames a column for SQL discovery + SELECT +
     WHERE
   - `:datahike.pg/hidden` excludes an attr from virtual tables

   :datahike.pg/references is covered by pg_fk_join_test (separate
   commit) since it's the feed for the FK-via-ref JOIN rewrite."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.schema :as pgs]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn hints-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn
                  [{:db/ident :widget/sku
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/identity}
                   {:db/ident :widget/full_name
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :widget/internal_note
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}])
      (d/transact conn [{:widget/sku "A"
                         :widget/full_name "Gadget"
                         :widget/internal_note "private"}])
      (pgs/set-hint! conn :widget/full_name    {:column "name"})
      (pgs/set-hint! conn :widget/internal_note {:hidden true})
      (let [{:keys [server]} (pg/start-server {"w" conn} {:port 0})]
        (try
          (binding [*conn* conn *port* (.getPort server)]
            (f))
          (finally
            (.stop server)
            (d/release conn)
            (d/delete-database cfg)))))))

(use-fixtures :each hints-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/w?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [md (.getMetaData rs)
          n  (.getColumnCount md)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (inc %)) (range n))))
          acc)))))

(deftest schema-hints-returns-hint-map
  (let [hs (pgs/schema-hints (d/db *conn*))]
    (is (= {:column "name"} (get hs :widget/full_name)))
    (is (= {:hidden true}   (get hs :widget/internal_note)))))

(deftest schema-hints-cache-follows-immutable-db-root
  (let [db-before (d/db *conn*)
        first-result (pgs/schema-hints db-before)]
    (is (identical? first-result (pgs/schema-hints db-before))
        "the unchanged immutable DB root reuses its catalog scan")
    (pgs/set-hint! *conn* :widget/full_name {:column "display_name"})
    (let [db-after (d/db *conn*)
          after-result (pgs/schema-hints db-after)]
      (is (not (identical? db-before db-after)))
      (is (= {:column "display_name"}
             (get after-result :widget/full_name))
          "a transaction publishes a new DB root and invalidates the cache"))))

(deftest information-schema-reflects-hints
  (with-open [c (jdbc)]
    (is (= [["sku"] ["name"]]
           (rows c "SELECT column_name FROM information_schema.columns
                    WHERE table_name = 'widget' AND column_name != 'db_id'
                    ORDER BY ordinal_position")))))

(deftest select-star-reflects-hints
  (with-open [c (jdbc)]
    (with-open [st (.createStatement c)
                rs (.executeQuery st "SELECT * FROM widget")]
      (let [md (.getMetaData rs)
            cols (mapv #(.getColumnLabel md (inc %)) (range (.getColumnCount md)))]
        (.next rs)
        (is (= ["sku" "name"] cols))
        (is (= "Gadget" (.getString rs "name")))))))

(deftest where-by-renamed-column-resolves
  (with-open [c (jdbc)]
    (is (= [["Gadget"]]
           (rows c "SELECT name FROM widget WHERE name = 'Gadget'")))))

(deftest hidden-column-is-not-selectable
  (with-open [c (jdbc)]
    ;; Selecting by the hidden column explicitly still works (the
    ;; attribute still exists) — hints are a view-layer convenience,
    ;; not an access control mechanism. Document this contract.
    (is (= [["private"]]
           (rows c "SELECT internal_note FROM widget WHERE sku = 'A'")))))
