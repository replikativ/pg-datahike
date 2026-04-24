(ns datahike.test.pg-unique-test
  "PRIMARY KEY + UNIQUE constraint enforcement through the pgwire
   translator. Datahike's `:db.unique/identity` is an UPSERT key (Datomic
   semantics) — Datalog callers going through d/transact get upsert; SQL
   callers going through the INSERT translator get PG-compatible 23505
   on collision.

   Coverage:
     - Single-col PRIMARY KEY (inline and table-level) → :db.unique/identity
     - Single-col UNIQUE                                → :db.unique/value
     - Multi-col  PRIMARY KEY                           → tuple + identity
     - Multi-col  UNIQUE                                → tuple + value
     - INSERT PK collision → 23505
     - INSERT UNIQUE collision → 23505
     - Batch self-collision → 23505
     - ON CONFLICT DO UPDATE still upserts
     - d/transact map-form still upserts (Datomic compat)"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          factory (reify PgWireServer$QueryHandlerFactory
                    (create [_] (pg/make-query-handler conn)))
          server (PgWireServer. 0 "127.0.0.1" factory)]
      (.start server)
      (try
        (binding [*conn* conn *port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- open ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/datahike?user=datahike&password=x&sslmode=disable&preferQueryMode=simple")))

(defn- sqlstate-of [^Exception e]
  (when (instance? SQLException e) (.getSQLState ^SQLException e)))

;; ---------------------------------------------------------------------------
;; Schema emission — inspect the Datahike schema after CREATE TABLE.

(deftest single-col-pk-emits-identity
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t1 (id INTEGER PRIMARY KEY, name TEXT)")))
  (let [s (:schema (d/db *conn*))]
    (is (= :db.unique/identity (:db/unique (get s :t1/id))))
    (is (nil? (:db/unique (get s :t1/name))))))

(deftest single-col-unique-emits-value
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t2 (id INTEGER PRIMARY KEY, email TEXT UNIQUE)")))
  (let [s (:schema (d/db *conn*))]
    (is (= :db.unique/identity (:db/unique (get s :t2/id))))
    (is (= :db.unique/value    (:db/unique (get s :t2/email))))))

(deftest table-level-pk-and-unique
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t3 (id INTEGER, name TEXT, PRIMARY KEY (id), UNIQUE (name))")))
  (let [s (:schema (d/db *conn*))]
    (is (= :db.unique/identity (:db/unique (get s :t3/id))))
    (is (= :db.unique/value    (:db/unique (get s :t3/name))))))

(deftest multi-col-pk-emits-tuple
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t4 (a INT, b INT, note TEXT, PRIMARY KEY (a, b))")))
  (let [s (:schema (d/db *conn*))
        tuple-attr (:t4/pg$pk_tuple s)]
    (is (some? tuple-attr) "tuple attr must exist")
    (is (= :db.type/tuple (:db/valueType tuple-attr)))
    (is (= [:t4/a :t4/b]  (:db/tupleAttrs tuple-attr)))
    (is (= :db.unique/identity (:db/unique tuple-attr)))))

;; ---------------------------------------------------------------------------
;; INSERT-time 23505 on collision.

(deftest insert-pk-collision-raises-23505
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT)")
      (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'A')")
      (let [raised (try
                     (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'B')")
                     nil
                     (catch SQLException e e))]
        (is (some? raised) "must raise on PK collision")
        (is (= "23505" (sqlstate-of raised)))))))

(deftest insert-unique-collision-raises-23505
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, email TEXT UNIQUE)")
      (.executeUpdate st "INSERT INTO t (id, email) VALUES (1, 'a@x')")
      (let [raised (try
                     (.executeUpdate st "INSERT INTO t (id, email) VALUES (2, 'a@x')")
                     nil
                     (catch SQLException e e))]
        (is (some? raised))
        (is (= "23505" (sqlstate-of raised)))))))

(deftest insert-batch-self-collision-raises-23505
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT)")
      (let [raised (try
                     (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'A'), (1, 'B')")
                     nil
                     (catch SQLException e e))]
        (is (some? raised))
        (is (= "23505" (sqlstate-of raised)))))))

(deftest insert-multi-col-pk-collision-raises-23505
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (a INT, b INT, note TEXT, PRIMARY KEY (a, b))")
      (.executeUpdate st "INSERT INTO t (a, b, note) VALUES (1, 2, 'first')")
      ;; Distinct composite key: allowed.
      (.executeUpdate st "INSERT INTO t (a, b, note) VALUES (1, 3, 'second')")
      ;; Same composite key: raises.
      (let [raised (try
                     (.executeUpdate st "INSERT INTO t (a, b, note) VALUES (1, 2, 'dup')")
                     nil
                     (catch SQLException e e))]
        (is (some? raised))
        (is (= "23505" (sqlstate-of raised)))))))

;; ---------------------------------------------------------------------------
;; ON CONFLICT still upserts — enforcement only applies to bare INSERT.

(deftest on-conflict-do-update-still-upserts
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT, age INT)")
      (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (1, 'A', 10)")
      ;; This collides on PK, but ON CONFLICT DO UPDATE should update in-place.
      (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (1, 'A2', 20) ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name, age = EXCLUDED.age")
      (with-open [rs (.executeQuery st "SELECT name, age FROM t WHERE id = 1")]
        (is (.next rs))
        (is (= "A2" (.getString rs 1)))
        (is (= 20   (.getInt    rs 2)))))))

(deftest on-conflict-do-nothing-still-skips
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT)")
      (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'A')")
      (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'B') ON CONFLICT (id) DO NOTHING")
      (with-open [rs (.executeQuery st "SELECT name FROM t WHERE id = 1")]
        (is (.next rs))
        (is (= "A" (.getString rs 1)) "DO NOTHING keeps original value")))))

;; ---------------------------------------------------------------------------
;; Datalog-side upsert via PK is preserved (Datomic compat).

;; ---------------------------------------------------------------------------
;; UPDATE SET on a PK column — must raise 23505 on collision.

(deftest update-pk-to-existing-raises-23505
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT)")
      (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'A')")
      (.executeUpdate st "INSERT INTO t (id, name) VALUES (2, 'B')")
      (let [raised (try
                     (.executeUpdate st "UPDATE t SET id = 1 WHERE id = 2")
                     nil
                     (catch SQLException e e))]
        (is (some? raised))
        (is (= "23505" (sqlstate-of raised)))))))

(deftest update-non-pk-column-still-works
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT)")
      (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'A')")
      (.executeUpdate st "UPDATE t SET name = 'A2' WHERE id = 1")
      (with-open [rs (.executeQuery st "SELECT name FROM t WHERE id = 1")]
        (is (.next rs))
        (is (= "A2" (.getString rs 1)))))))

(deftest update-pk-to-self-is-fine
  ;; Updating a row's PK to its current value is a no-op, not a collision.
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT)")
      (.executeUpdate st "INSERT INTO t (id, name) VALUES (1, 'A')")
      ;; This would trip a naive check (id=1 already exists), but that's
      ;; the SAME entity we're updating.
      (.executeUpdate st "UPDATE t SET id = 1, name = 'B' WHERE id = 1")
      (with-open [rs (.executeQuery st "SELECT name FROM t WHERE id = 1")]
        (is (.next rs))
        (is (= "B" (.getString rs 1)))))))

(deftest datalog-upsert-via-pk-still-works
  (with-open [c (open)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE t (id INT PRIMARY KEY, name TEXT, age INT)")))
  ;; Bypass SQL: transact a map entity twice with the same :t/id. The
  ;; SECOND one must upsert into the first (Datomic semantics), mutating
  ;; name + age — NOT raise.
  (d/transact *conn* [{:t/id 1 :t/name "A" :t/age 10 :t/db-row-exists true}])
  (d/transact *conn* [{:t/id 1 :t/name "A2" :t/age 20 :t/db-row-exists true}])
  (let [rows (d/q '[:find ?id ?n ?a
                    :where
                    [?e :t/id ?id]
                    [?e :t/name ?n]
                    [?e :t/age ?a]]
                  (d/db *conn*))]
    (is (= #{[1 "A2" 20]} rows)
        "upsert mutates one entity — no duplicate row")))
