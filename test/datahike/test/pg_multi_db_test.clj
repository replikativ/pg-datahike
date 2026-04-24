(ns datahike.test.pg-multi-db-test
  "End-to-end test of multi-DB routing: one PgWireServer serves multiple
   Datahike connections; pgjdbc clients pick a conn via the `database=`
   parameter in the JDBC URL (which lands in the StartupMessage)."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *server* nil)
(def ^:dynamic *port*   nil)
(def ^:dynamic *registry* nil)

(defn- fresh-cfg []
  {:store {:backend :memory :id (java.util.UUID/randomUUID)}
   :schema-flexibility :write
   :keep-history? false})

(defn- make-conn! [attr-ns]
  (let [cfg (fresh-cfg)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          ident-attr (keyword (name attr-ns) "id")
          name-attr  (keyword (name attr-ns) "name")]
      (d/transact conn
                  [{:db/ident ident-attr :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                   {:db/ident name-attr :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}])
      [cfg conn ident-attr name-attr])))

(defn multi-db-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [[prod-cfg prod-conn pid pname]  (make-conn! "person")
        [stag-cfg stag-conn wid wname]  (make-conn! "widget")]
    (d/transact prod-conn [{pid 1 pname "Alice"} {pid 2 pname "Bob"}])
    (d/transact stag-conn [{wid 10 wname "Gadget"} {wid 11 wname "Sprocket"}])
    (let [registry {"prod" prod-conn "staging" stag-conn}
          {:keys [server]} (pg/start-server registry {:port 0})]
      (try
        (binding [*server*   server
                  *port*     (.getPort server)
                  *registry* registry]
          (f))
        (finally
          (.stop server)
          (d/release prod-conn)
          (d/release stag-conn)
          (d/delete-database prod-cfg)
          (d/delete-database stag-cfg))))))

(use-fixtures :each multi-db-fixture)

(defn- ^Connection connect-to [db]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/" db
        "?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- scalar [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (.next rs)
    (.getString rs 1)))

(defn- scalar-long [^Connection c sql]
  (Long/parseLong (scalar c sql)))

(deftest current-database-reflects-startup-param
  (testing "each connection's current_database() returns its own name"
    (with-open [cp (connect-to "prod")
                cs (connect-to "staging")]
      (is (= "prod"    (scalar cp "SELECT current_database()")))
      (is (= "staging" (scalar cs "SELECT current_database()"))))))

(deftest routing-isolates-tables
  (testing "prod only sees its table, staging only sees its own"
    (with-open [cp (connect-to "prod")]
      (is (= 2 (scalar-long cp "SELECT count(*) FROM person")))
      (is (thrown? SQLException
                   (scalar cp "SELECT count(*) FROM widget"))))
    (with-open [cs (connect-to "staging")]
      (is (= 2 (scalar-long cs "SELECT count(*) FROM widget")))
      (is (thrown? SQLException
                   (scalar cs "SELECT count(*) FROM person"))))))

(deftest pg-database-enumerates-registry
  (testing "pg_database surfaces every registered db plus templates"
    (with-open [c (connect-to "prod")]
      (with-open [st (.createStatement c)
                  rs (.executeQuery st "SELECT datname FROM pg_database ORDER BY datname")]
        (let [names (loop [acc []]
                      (if (.next rs) (recur (conj acc (.getString rs 1))) acc))]
          (is (= ["prod" "staging" "template0" "template1"] names)))))))

(deftest unknown-database-is-rejected
  (testing "connecting with a bogus database name raises 3D000"
    (let [thrown
          (try
            (with-open [c (connect-to "nonsuch")]
              (scalar c "SELECT 1"))
            nil
            (catch SQLException e e))]
      (is (some? thrown))
      (when thrown
        ;; pgjdbc surfaces our ErrorResponse as a generic SQLException;
        ;; the important contract is SQLSTATE 3D000 (or
        ;; invalid_catalog_name wording as a fallback).
        (is (or (= "3D000" (.getSQLState thrown))
                (str/includes? (.getMessage thrown) "does not exist")))))))
