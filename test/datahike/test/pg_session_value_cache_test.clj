(ns datahike.test.pg-session-value-cache-test
  "Session values read by a CACHED plan.

   Translated plans live in a server-wide cache, so the session that
   translates a statement first is not the session that runs it. A plan
   that captured its translating session's state answered that session's
   `current_schema` / `current_database()` to everyone else -- for any
   use that is not the whole projection, since only the sole-call form
   is answered without translation.

   Expectations are PostgreSQL's: a session value is the EXECUTING
   session's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn- fresh-cfg []
  {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
   :schema-flexibility :write :keep-history? false})

(defn- make-conn!
  "A connection whose schema is IDENTICAL to its sibling's: equal schemas
   and an equal catalog basis are what let two databases share a cache
   entry."
  []
  (let [cfg (fresh-cfg)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn [{:db/ident :seed/id :db/valueType :db.type/long
                         :db/cardinality :db.cardinality/one
                         :db/unique :db.unique/identity}])
      (d/transact conn [{:seed/id 1}])
      [cfg conn])))

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [[cfg-a conn-a] (make-conn!)
        [cfg-b conn-b] (make-conn!)
        {:keys [server]} (pg/start-server {"alpha" conn-a "beta" conn-b} {:port 0})]
    (try
      (binding [*port* (.getPort server)] (f))
      (finally
        (.stop server)
        (d/release conn-a) (d/release conn-b)
        (d/delete-database cfg-a) (d/delete-database cfg-b)))))

(use-fixtures :each fixture)

(defn- ^Connection connect-to [db]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port* "/" db
        "?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- scalar [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (.next rs)
    (.getString rs 1)))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(deftest current-schema-is-the-executing-sessions
  ;; Not the sole projection, so it is translated and cached.
  (let [sql "SELECT current_schema, id FROM t"]
    (with-open [a (connect-to "alpha")
                b (connect-to "alpha")]
      (exec! a "CREATE TABLE t (id int)")
      (exec! a "INSERT INTO t VALUES (1)")
      (testing "the plan is cached by the first session to run it"
        (is (= "public" (scalar a sql))))
      (exec! a "SET search_path TO pg_catalog, public")
      (is (= "pg_catalog" (scalar a sql)))
      (testing "another session reusing that plan reads its own search_path"
        (is (= "public" (scalar b sql))))
      (testing "and still sees its own after setting one"
        (exec! b "SET search_path TO pg_catalog, public")
        (is (= "pg_catalog" (scalar b sql)))
        (exec! a "SET search_path TO public")
        (is (= "public" (scalar a sql)))
        (is (= "pg_catalog" (scalar b sql)))))))

(deftest current-database-is-the-executing-sessions
  ;; Two databases with equal schemas and an equal catalog basis.
  (let [sql "SELECT current_database(), id FROM t"]
    (with-open [a (connect-to "alpha")
                b (connect-to "beta")]
      (doseq [c [a b]]
        (exec! c "CREATE TABLE t (id int)")
        (exec! c "INSERT INTO t VALUES (1)"))
      (is (= "alpha" (scalar a sql)))
      (is (= "beta" (scalar b sql)))
      (testing "the same holds inside a row projection (RETURNING)"
        (is (= "alpha" (scalar a "INSERT INTO t VALUES (2) RETURNING current_database()")))
        (is (= "beta" (scalar b "INSERT INTO t VALUES (2) RETURNING current_database()")))))))

(deftest the-session-functions-work-in-an-expression
  ;; Each was answerable only as a WHOLE statement -- classify matched
  ;; the sole projection and a handler answered it -- so
  ;; `SELECT pg_backend_pid(), 1` and `WHERE pid = pg_backend_pid()`
  ;; raised 42883 on a server where `SELECT pg_backend_pid()` works.
  ;;
  ;; They read the session-state atom, which is what a Datalog function
  ;; running off the connection's thread can still reach; the plan is
  ;; marked session-dependent so it never serves another session.
  (with-open [a (connect-to "alpha")
              b (connect-to "alpha")]
    (testing "pg_backend_pid is the same value however it is asked for"
      (let [alone (scalar a "SELECT pg_backend_pid()")
            in-expr (scalar a "SELECT pg_backend_pid(), 1")
            again (scalar a "SELECT pg_backend_pid()")]
        (is (= alone in-expr again))
        (is (pos? (Long/parseLong alone)))))
    (testing "and it is the EXECUTING session's, not the translating one's"
      (is (not= (scalar a "SELECT pg_backend_pid(), 1")
                (scalar b "SELECT pg_backend_pid(), 1"))))
    (testing "in a predicate"
      (is (= "1" (scalar a "SELECT 1 WHERE pg_backend_pid() > 0"))))
    (testing "txid_current reads the transaction it runs in"
      (is (= "t" (scalar a "SELECT txid_current() > 0")))
      (is (= (scalar a "SELECT txid_current()")
             (scalar a "SELECT txid_current(), 2"))))))
