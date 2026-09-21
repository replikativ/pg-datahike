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

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- row [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (.next rs)
    (let [n (.getColumnCount (.getMetaData rs))]
      (mapv #(.getString rs (int %)) (range 1 (inc n))))))

(defn- state [^Connection c sql]
  (try (exec! c sql) :no-error
       (catch java.sql.SQLException e (.getSQLState e))))

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

(deftest advisory-locks-sleep-and-notify-work-in-an-expression
  ;; The last three server shortcuts. `pg_sleep`, `pg_notify` and the six
  ;; advisory-lock functions were answerable only as a WHOLE statement,
  ;; so `SELECT pg_sleep(0), 2` -- and the `CASE WHEN
  ;; pg_try_advisory_lock(…)` a migration tool guards a step with --
  ;; raised 42883 on a server where the bare call works.
  ;;
  ;; The advisory ones read the session-state atom for the session id the
  ;; registry is keyed by; `pg_sleep` and `pg_notify` need no session at
  ;; all and stay cacheable.
  ;;
  ;; Expectations are a PostgreSQL 17 oracle's; `void` renders as the
  ;; EMPTY STRING there, not as NULL.
  (with-open [a (connect-to "alpha")
              b (connect-to "alpha")]
    (testing "void functions in a projection"
      (is (= "" (scalar a "SELECT pg_sleep(0)")))
      (is (= "" (scalar a "SELECT pg_sleep(0), 2")))
      (is (= "" (scalar a "SELECT pg_notify('c','p'), 3")))
      (is (= "2" (nth (row a "SELECT pg_sleep(0), 2") 1))))
    (testing "a lock is exclusive across sessions, and re-entrant within one"
      (is (= "t" (scalar a "SELECT pg_try_advisory_lock(900)")))
      (is (= "f" (scalar b "SELECT pg_try_advisory_lock(900)")))
      (is (= "t" (scalar a "SELECT pg_try_advisory_lock(900)")) "re-entrant")
      (is (= "t" (scalar a "SELECT pg_advisory_unlock(900)")))
      (is (= "f" (scalar b "SELECT pg_try_advisory_lock(900)"))
          "one unlock does not release a doubly-held lock")
      (is (= "t" (scalar a "SELECT pg_advisory_unlock(900)")))
      (is (= "t" (scalar b "SELECT pg_try_advisory_lock(900)")))
      (is (= "" (scalar b "SELECT pg_advisory_unlock_all()"))))
    (testing "inside an expression, which is how a migration tool asks"
      (is (= "got" (scalar a "SELECT CASE WHEN pg_try_advisory_lock(901) THEN 'got' ELSE 'busy' END")))
      (is (= "busy" (scalar b "SELECT CASE WHEN pg_try_advisory_lock(901) THEN 'got' ELSE 'busy' END")))
      (is (= "t" (scalar a "SELECT pg_advisory_unlock(901)"))))
    (testing "the two-key namespace is distinct from the one-key one"
      (is (= "t" (scalar a "SELECT pg_try_advisory_lock(1, 2)")))
      (is (= "t" (scalar b "SELECT pg_try_advisory_lock(1)"))
          "a single key 1 is a different lock from the pair (1,2)")
      (is (= "t" (scalar a "SELECT pg_advisory_unlock(1, 2)")))
      (is (= "t" (scalar b "SELECT pg_advisory_unlock(1)"))))
    (testing "a transaction-level lock outside a transaction is 25P01"
      (is (= "25P01" (state a "SELECT pg_advisory_xact_lock(902)"))))))

(deftest shared-advisory-locks-and-what-pg-locks-reports
  ;; PostgreSQL's advisory locks come in two modes, and its own
  ;; regression file takes both on the same key in one statement, asks
  ;; `pg_locks` what is held, and expects the SESSION-level locks to
  ;; survive a ROLLBACK that drops the transaction-level ones. Only the
  ;; exclusive half existed here, `pg_locks` was always empty, and
  ;; `pg_advisory_unlock_all()` released transaction locks too.
  ;;
  ;; Expectations are a PostgreSQL 17 oracle's; the file is
  ;; src/test/regress/sql/advisory_lock.sql.
  (with-open [a (connect-to "alpha")
              b (connect-to "alpha")]
    (testing "two shared holders coexist; an exclusive one excludes"
      (is (= "t" (scalar a "SELECT pg_try_advisory_lock_shared(501)")))
      (is (= "t" (scalar b "SELECT pg_try_advisory_lock_shared(501)")))
      (is (= "f" (scalar b "SELECT pg_try_advisory_lock(501)")))
      (is (= "t" (scalar a "SELECT pg_advisory_unlock_shared(501)")))
      (is (= "t" (scalar b "SELECT pg_advisory_unlock_shared(501)")))
      (is (= "t" (scalar b "SELECT pg_try_advisory_lock(501)")))
      (is (= "f" (scalar a "SELECT pg_try_advisory_lock_shared(501)")))
      (is (= "t" (scalar b "SELECT pg_advisory_unlock(501)"))))
    (testing "pg_locks reports them as PostgreSQL does"
      (is (= "" (scalar a "SELECT pg_advisory_lock(1)")))
      (is (= "" (scalar a "SELECT pg_advisory_lock_shared(2)")))
      (is (= "" (scalar a "SELECT pg_advisory_lock(1, 1)")))
      (is (= [["advisory" "0" "1" "1" "ExclusiveLock" "t"]
              ["advisory" "0" "2" "1" "ShareLock" "t"]
              ["advisory" "1" "1" "2" "ExclusiveLock" "t"]]
             (rows a "SELECT locktype, classid, objid, objsubid, mode, granted
                        FROM pg_locks WHERE locktype = 'advisory'
                       ORDER BY classid, objid, objsubid"))
          "a one-argument key is (0, key, 1); a two-argument key is (a, b, 2)"))
    (testing "unlock_all releases the session locks, not the transaction ones"
      (is (= "" (scalar a "SELECT pg_advisory_unlock_all()")))
      (is (= [] (rows a "SELECT 1 FROM pg_locks WHERE locktype = 'advisory'"))))))

(deftest a-database-has-an-oid
  ;; `SELECT oid AS datoid FROM pg_database WHERE datname = current_database()`
  ;; opens PostgreSQL's own regression files, which then interpolate
  ;; `:datoid` into every later query. With no oid column the variable
  ;; stays unset and psql sends the literal `:datoid` to the server for
  ;; the rest of the file -- one missing column, every later statement.
  (with-open [c (connect-to "alpha")]
    (let [oid (scalar c "SELECT oid FROM pg_database WHERE datname = current_database()")]
      (is (some? oid))
      (is (pos? (Long/parseLong oid)))
      (is (= oid (scalar c "SELECT oid FROM pg_database WHERE datname = current_database()"))
          "and it is stable"))
    (testing "the templates keep PostgreSQL's own oids"
      (is (= [["1" "template1"] ["4" "template0"]]
             (rows c "SELECT oid, datname FROM pg_database
                       WHERE datname IN ('template0','template1') ORDER BY oid"))))
    (testing "the columns a client reads alongside it"
      (is (= [["6" "t" "-1"]]
             (rows c "SELECT encoding, datallowconn, datconnlimit FROM pg_database
                       WHERE datname = current_database()"))))))

