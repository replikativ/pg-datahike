(ns datahike.test.pg-jsonb-canonical-test
  "jsonb fidelity: stored jsonb must behave like PostgreSQL jsonb, not json.

   jsonb normalizes on input — object key order is dropped, insignificant
   whitespace removed, and only the LAST of duplicate keys is kept — so equality,
   DISTINCT and GROUP BY compare by STRUCTURE, not by the input text. pg-datahike
   used to store the raw input string, so `'{\"a\":1,\"b\":2}'::jsonb =
   '{\"b\":2,\"a\":1}'::jsonb` was FALSE (two different strings) where Postgres
   returns TRUE. Canonicalizing on ingest (recursive key-sort, whitespace-normalize,
   duplicate-collapse) fixes it. json (the text-faithful type) is NOT normalized."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn jsonb-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"jsonb" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each jsonb-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/jsonb?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs ^long %) (range 1 (inc n)))))
          acc)))))

(deftest jsonb-equality-ignores-key-order
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int, payload jsonb)")
    (exec! c "INSERT INTO t (id, payload) VALUES (1, '{\"b\":2,\"a\":1}')")
    (exec! c "INSERT INTO t (id, payload) VALUES (2, '{\"a\":1,\"b\":2}')")
    (is (= [["{\"a\":1,\"b\":2}"] ["{\"a\":1,\"b\":2}"]]
           (rows c "SELECT payload FROM t ORDER BY id"))
        "both round-trip to the SAME canonical text, whatever the input order")
    (is (= [["1"] ["2"]]
           (rows c "SELECT id FROM t WHERE payload = '{\"a\":1,\"b\":2}' ORDER BY id"))
        "row 1 (inserted as {b,a}) matches an {a,b} literal — equality is structural")
    (is (= [["{\"a\":1,\"b\":2}"]]
           (rows c "SELECT DISTINCT payload FROM t"))
        "DISTINCT collapses the two — they are one jsonb value")))

(deftest jsonb-drops-duplicate-keys-last-wins
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int, payload jsonb)")
    (exec! c "INSERT INTO t (id, payload) VALUES (1, '{\"a\":1,\"a\":2}')")
    (is (= [["{\"a\":2}"]]
           (rows c "SELECT payload FROM t"))
        "duplicate keys collapse, last wins (Postgres jsonb semantics)")))

(deftest jsonb-normalizes-whitespace-and-nesting
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int, payload jsonb)")
    (exec! c "INSERT INTO t (id, payload) VALUES (1, '{ \"z\" : { \"y\":1, \"x\":2 } , \"a\":3 }')")
    (is (= [["{\"a\":3,\"z\":{\"x\":2,\"y\":1}}"]]
           (rows c "SELECT payload FROM t"))
        "whitespace stripped and keys sorted recursively")))

(deftest jsonb-preserves-array-order
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE t (id int, payload jsonb)")
    (exec! c "INSERT INTO t (id, payload) VALUES (1, '[3,1,2]')")
    (is (= [["[3,1,2]"]]
           (rows c "SELECT payload FROM t"))
        "arrays are ordered — element order is preserved, only object keys sort")))
