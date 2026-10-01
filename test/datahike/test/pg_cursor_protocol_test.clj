(ns datahike.test.pg-cursor-protocol-test
  "DECLARE / FETCH / MOVE / CLOSE over the EXTENDED query protocol.

   Every existing cursor test drives the handler in process, which is
   the simple-query path. Over the extended protocol both of the
   statements that read a cursor were broken, and neither failure could
   be seen from the other side:

   `FETCH` reported NoData at Describe, so Execute then streamed
   DataRows the client had no field structure for -- pgjdbc raises
   `Received resultset tuples, but no field structure for them` and
   EVERY cursor read over JDBC failed. The identical bug, with the
   identical message, had already been found and fixed for `EXECUTE`.

   `MOVE ALL` called `.execute` on the handler from inside a statement,
   with the caller's `*cached-parsed*` still in scope, so it re-ran the
   OUTER plan -- itself -- until the stack ended and the connection
   died. The fourth caller to make that mistake.

   Expectations are a PostgreSQL 17 oracle's, driven through the same
   pgjdbc."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager ResultSet ResultSetMetaData
            SQLException Statement PreparedStatement]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0 :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"cur" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (doto (DriverManager/getConnection
         (str "jdbc:postgresql://127.0.0.1:" *port*
              "/cur?user=x&password=x&sslmode=disable"))
    (.setAutoCommit false)))

(defn- update! [^Connection c sql]
  (with-open [s (.createStatement c)] (.executeUpdate s sql)))

(defn- rows [^Connection c sql]
  (with-open [s (.createStatement c) r (.executeQuery s sql)]
    (loop [acc []] (if (.next r) (recur (conj acc (.getInt r 1))) acc))))

(deftest fetch-describes-the-cursors-columns
  ;; Over the extended protocol this raised IllegalStateException on the
  ;; FIRST fetch, so cursors were unusable from any JDBC client.
  (with-open [c (jdbc)]
    (update! c "DECLARE cur CURSOR FOR SELECT g, g * 2 FROM generate_series(1,6) g")
    (with-open [s (.createStatement c) r (.executeQuery s "FETCH 2 FROM cur")]
      (let [^ResultSetMetaData m (.getMetaData r)]
        (testing "the RowDescription arrives, and it is the cursor's shape"
          (is (= 2 (.getColumnCount m)))
          (is (= "g" (.getColumnName m 1)))))
      (is (= [1 2] (loop [acc []] (if (.next r) (recur (conj acc (.getInt r 1))) acc)))))
    (testing "and through a PreparedStatement, which Describes separately"
      (with-open [p (.prepareStatement c "FETCH 1 FROM cur") r (.executeQuery p)]
        (is (= [3] (loop [acc []] (if (.next r) (recur (conj acc (.getInt r 1))) acc))))))))

(deftest move-all-does-not-recurse-into-its-own-plan
  ;; `MOVE ALL` ran its COUNT(*) through `.execute` on the handler while
  ;; the caller's `*cached-parsed*` was still bound, so the nested call
  ;; re-ran the outer MOVE. A StackOverflowError that took the
  ;; connection with it.
  (with-open [c (jdbc)]
    (update! c "DECLARE cur CURSOR FOR SELECT generate_series(1,5)")
    (with-open [p (.prepareStatement c "MOVE ALL IN cur")]
      (.execute p)
      (is (= 5 (.getUpdateCount p))))
    (testing "the cursor is exhausted, and the connection still works"
      (is (= [] (rows c "FETCH 1 FROM cur")))
      (update! c "CLOSE cur")
      (.commit c)
      (is (= [1] (rows c "SELECT 1"))))))

(deftest the-whole-cursor-surface-over-jdbc
  (with-open [c (jdbc)]
    (update! c "DECLARE cur CURSOR FOR SELECT g FROM generate_series(1,6) g")
    (is (= [1 2] (rows c "FETCH 2 FROM cur")))
    (with-open [p (.prepareStatement c "MOVE 1 IN cur")]
      (.execute p)
      (is (= 1 (.getUpdateCount p))))
    (is (= [4 5 6] (rows c "FETCH ALL FROM cur")))
    (with-open [p (.prepareStatement c "MOVE ALL IN cur")]
      (.execute p)
      (is (= 0 (.getUpdateCount p))))
    (update! c "CLOSE cur")
    (.commit c)
    (is (= [1] (rows c "SELECT 1")))))
