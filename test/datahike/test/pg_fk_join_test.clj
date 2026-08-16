(ns datahike.test.pg-fk-join-test
  "FK-via-ref JOIN rewrite: on a native Datahike schema where
   :db.type/ref attrs store the target entity-id, SQL JOIN on
   `a.fk = b.pk` should resolve by entity identity even though
   `a.fk` surfaces as the entity-id and `b.pk` is the user's business
   key. Covers both the auto-detected case (RHS is :db.unique/identity)
   and the hint-driven case (:datahike.pg/references override)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.schema :as pgs]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn fk-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn
                  [{:db/ident :company/id
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/identity}
                   {:db/ident :company/name
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :person/id
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one
                    :db/unique :db.unique/identity}
                   {:db/ident :person/name
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :person/company
                    :db/valueType :db.type/ref
                    :db/cardinality :db.cardinality/one}])
      (d/transact conn
                  [{:company/id 1 :company/name "Acme"}
                   {:company/id 2 :company/name "Globex"}])
      (d/transact conn
                  [{:person/id 1 :person/name "Alice"   :person/company [:company/id 1]}
                   {:person/id 2 :person/name "Bob"     :person/company [:company/id 1]}
                   {:person/id 3 :person/name "Charlie" :person/company [:company/id 2]}])
      (let [{:keys [server]} (pg/start-server {"fk" conn} {:port 0})]
        (try
          (binding [*conn* conn *port* (.getPort server)]
            (f))
          (finally
            (.stop server)
            (d/release conn)
            (d/delete-database cfg)))))))

(use-fixtures :each fk-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/fk?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- pairs [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (loop [acc []]
      (if (.next rs)
        (recur (conj acc [(.getString rs 1) (.getString rs 2)]))
        acc))))

(deftest fk-via-ref-auto-inner-join
  (with-open [c (jdbc)]
    (is (= [["Alice"   "Acme"]
            ["Bob"     "Acme"]
            ["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.id ORDER BY p.name")))))

(deftest db-id-join-still-works
  ;; Regression: the explicit db_id JOIN path must remain functional —
  ;; it's used by DDL-created tables and by any code that reaches into
  ;; the synthetic db_id column directly.
  (with-open [c (jdbc)]
    (is (= [["Alice"   "Acme"]
            ["Bob"     "Acme"]
            ["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.db_id ORDER BY p.name")))))

(deftest fk-via-ref-hint-wins-over-auto
  ;; Even when the FK target table has multiple unique attrs, an explicit
  ;; :datahike.pg/references hint picks one. We prove this by adding a
  ;; second unique attr to company and verifying the hint-chosen one
  ;; still drives the JOIN.
  (d/transact *conn*
              [{:db/ident :company/tax_id
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/unique :db.unique/identity}])
  (pgs/set-hint! *conn* :person/company {:references :company/id})
  (with-open [c (jdbc)]
    (is (= [["Alice"   "Acme"]
            ["Bob"     "Acme"]
            ["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.id ORDER BY p.name")))))

(deftest where-on-joined-table
  (with-open [c (jdbc)]
    (is (= [["Charlie" "Globex"]]
           (pairs c "SELECT p.name, c.name FROM person p JOIN company c
                     ON p.company = c.id
                     WHERE c.name = 'Globex' ORDER BY p.name")))))
