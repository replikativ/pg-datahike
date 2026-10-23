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

(deftest a-bare-session-keyword-compares-as-a-value
  ;; `current_user`, `user`, `session_user`, `current_schema` and the
  ;; LOCALTIME family are PostgreSQL keywords that take no parentheses,
  ;; so JSqlParser surfaces them as unqualified Columns. The equality
  ;; path saw two Columns, read it as an implicit join, and unified
  ;; `rolname` with a `pg_roles.current_user` attribute that does not
  ;; exist -- so `WHERE rolname = current_user` bound nothing and
  ;; answered NO ROWS with the matching row sitting right there.
  ;;
  ;; `CAST(current_user AS text)` worked throughout, which is what
  ;; localised it: the value path always knew; only the join shortcut
  ;; did not.
  (with-open [a (connect-to "alpha")]
    (is (= "datahike" (scalar a "SELECT current_user"))
        "the premise")
    (is (= [["datahike"]] (rows a "SELECT rolname FROM pg_roles WHERE rolname = current_user")))
    (testing "either side, and the other spellings"
      (is (= [["datahike"]] (rows a "SELECT rolname FROM pg_roles WHERE current_user = rolname")))
      (is (= [["datahike"]] (rows a "SELECT rolname FROM pg_roles WHERE rolname = user")))
      (is (= [["datahike"]] (rows a "SELECT rolname FROM pg_roles WHERE rolname = session_user"))))
    (testing "and it still agrees with the cast form that always worked"
      (is (= (rows a "SELECT rolname FROM pg_roles WHERE rolname = current_user")
             (rows a "SELECT rolname FROM pg_roles WHERE rolname = CAST(current_user AS text)"))))
    (testing "a non-matching comparison is still empty, not accidentally true"
      ;; There is no schema named `datahike`, so PostgreSQL answers no
      ;; rows here too -- the fix must not turn the predicate into a
      ;; tautology.
      (is (= [] (rows a "SELECT n.nspname FROM pg_namespace n WHERE n.nspname = current_user")))
      (is (seq (rows a "SELECT n.nspname FROM pg_namespace n WHERE n.nspname = 'public'"))))
    (testing "a real implicit join between two columns still joins"
      (exec! a "CREATE TABLE ij_a (id int, nm text)")
      (exec! a "CREATE TABLE ij_b (id int, tag text)")
      (exec! a "INSERT INTO ij_a VALUES (1, 'x'), (2, 'y')")
      (exec! a "INSERT INTO ij_b VALUES (1, 'p'), (3, 'q')")
      (is (= [["x" "p"]]
             (rows a "SELECT a.nm, b.tag FROM ij_a a, ij_b b WHERE a.id = b.id"))))))

(deftest current-setting-reads-the-executing-session
  ;; `current_setting` read a compiled-in map, so it was a SECOND read
  ;; point for a setting `SHOW` already answered from the session --
  ;; and the two disagreed: after `SET search_path = 'myschema'`,
  ;; `SHOW` said myschema and `current_setting` said the default.
  ;; `set_config` had been writing the session state all along; only
  ;; this reader ignored it.
  (with-open [a (connect-to "alpha")]
    (testing "GUC names are case-insensitive in PostgreSQL"
      ;; This raised 42704, because the lookup was a plain `get` on a
      ;; map with mixed-case keys (TimeZone, DateStyle).
      (is (= (scalar a "SELECT current_setting('search_path')")
             (scalar a "SELECT current_setting('SEARCH_PATH')")))
      (is (= "UTC" (scalar a "SELECT current_setting('timezone')")))
      (is (= "UTC" (scalar a "SELECT current_setting('TimeZone')"))))
    (testing "SET is visible to current_setting, and agrees with SHOW"
      (exec! a "SET search_path = 'myschema'")
      (is (= "myschema" (scalar a "SHOW search_path")))
      (is (= "myschema" (scalar a "SELECT current_setting('search_path')"))
          "one setting, one value, whichever way it is read"))
    (testing "set_config too"
      (is (= "other" (scalar a "SELECT set_config('search_path', 'other', false)")))
      (is (= "other" (scalar a "SELECT current_setting('search_path')")))
      (is (= "other" (scalar a "SHOW search_path"))))
    (testing "an unknown parameter still raises, and missing_ok still nils"
      (is (= "42704" (state a "SELECT current_setting('nosuch.gucname')")))
      (is (nil? (scalar a "SELECT current_setting('nosuch.gucname', true)"))))))

(deftest current-setting-is-not-shared-between-sessions
  ;; The hazard this namespace exists for. Now that the value comes from
  ;; the session, a cached plan must not serve the translating session's
  ;; setting to another -- the statement is marked session-dependent.
  (with-open [a (connect-to "alpha")
              b (connect-to "beta")]
    (exec! a "SET search_path = 'from_alpha'")
    (exec! b "SET search_path = 'from_beta'")
    ;; `a` translates first; `b` must not inherit its plan's answer.
    (is (= "from_alpha" (scalar a "SELECT current_setting('search_path')")))
    (is (= "from_beta" (scalar b "SELECT current_setting('search_path')")))
    (testing "and in an expression, not just the sole projection"
      (is (= ["from_alpha" "x"]
             (row a "SELECT current_setting('search_path'), 'x'")))
      (is (= ["from_beta" "x"]
             (row b "SELECT current_setting('search_path'), 'x'"))))))

(defn- with-warnings
  "First row, plus the statement's SQLWarnings in order."
  [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (.next rs)
    (let [n (.getColumnCount (.getMetaData rs))
          vals (mapv #(.getString rs (int %)) (range 1 (inc n)))]
      [vals (loop [w (.getWarnings st) acc []]
              (if w (recur (.getNextWarning w) (conj acc (.getMessage w))) acc))])))

(deftest unlocking-a-lock-you-do-not-hold-warns
  ;; PostgreSQL returns false AND warns (lock.c's LockRelease:
  ;; `you don't own a lock of type %s`). We returned false silently,
  ;; which was advisory_lock.sql's entire remaining gap -- eight
  ;; WARNING lines, four per statement, one per unlock in the SELECT
  ;; list, with the mode named.
  ;;
  ;; Expectations are PostgreSQL 17's, from advisory_lock.out.
  (with-open [a (connect-to "alpha")]
    (testing "one warning per failed unlock, in SELECT-list order, naming the mode"
      (is (= [["f" "f" "f" "f"]
              ["you don't own a lock of type ExclusiveLock"
               "you don't own a lock of type ShareLock"
               "you don't own a lock of type ExclusiveLock"
               "you don't own a lock of type ShareLock"]]
             (with-warnings a (str "SELECT pg_advisory_unlock(1),"
                                   " pg_advisory_unlock_shared(2),"
                                   " pg_advisory_unlock(1, 1),"
                                   " pg_advisory_unlock_shared(2, 2)")))))
    (testing "a lock you DO hold releases quietly"
      (is (= [[""] []] (with-warnings a "SELECT pg_advisory_lock(1)")))
      (is (= [["t"] []] (with-warnings a "SELECT pg_advisory_unlock(1)")))
      (testing "and unlocking it twice warns the second time"
        (is (= [["f"] ["you don't own a lock of type ExclusiveLock"]]
               (with-warnings a "SELECT pg_advisory_unlock(1)")))))
    (testing "the shared half names ShareLock"
      (is (= [[""] []] (with-warnings a "SELECT pg_advisory_lock_shared(7)")))
      (is (= [["t"] []] (with-warnings a "SELECT pg_advisory_unlock_shared(7)")))
      (is (= [["f"] ["you don't own a lock of type ShareLock"]]
             (with-warnings a "SELECT pg_advisory_unlock_shared(7)"))))))

(deftest set-timezone-is-stored-and-read-back
  ;; `SET TimeZone` was accepted and discarded, so `SHOW timezone`
  ;; answered the compiled-in UTC whatever the session asked for -- a
  ;; client that set a zone and read it back was told its request had
  ;; not taken.
  ;;
  ;; Every answer here is PostgreSQL 17's. A colon offset is a POSIX
  ;; string -- positive means WEST -- and SHOW answers it as written; a
  ;; plain number is hours EAST and SHOW answers it in POSIX notation.
  ;; This test once expected `'+02:00'` to SHOW as `-02:00`, written
  ;; down from what the implementation did rather than from PostgreSQL.
  (with-open [a (connect-to "alpha")]
    (is (= "UTC" (scalar a "SHOW timezone")) "the default")
    (testing "a zone name round-trips, and SHOW agrees with current_setting"
      (exec! a "SET timezone = 'Europe/Berlin'")
      (is (= "Europe/Berlin" (scalar a "SHOW timezone")))
      (is (= "Europe/Berlin" (scalar a "SELECT current_setting('timezone')")))
      (is (= "Europe/Berlin" (scalar a "SHOW TimeZone")) "spelling is case-insensitive"))
    (testing "a colon offset is a POSIX string, and SHOW answers it as written"
      (doseq [[set-to shows rendered]
              [["'04:30'"  "04:30"  "2025-12-31 19:30:00-04:30"]
               ["'-04:30'" "-04:30" "2026-01-01 04:30:00+04:30"]
               ["'+02:00'" "+02:00" "2025-12-31 22:00:00-02"]
               ["'00:30'"  "00:30"  "2025-12-31 23:30:00-00:30"]]]
        (exec! a (str "SET TIME ZONE " set-to))
        (is (= shows (scalar a "SHOW timezone")) (str "SET TIME ZONE " set-to))
        (is (= rendered (scalar a "SELECT '2026-01-01 00:00+00'::timestamptz"))
            (str "positive is WEST: " set-to))))
    (testing "a plain number is hours EAST, shown in POSIX notation"
      (doseq [[set-to shows rendered]
              [["'+2'"  "<+02>-02"         "2026-01-01 02:00:00+02"]
               ["5"     "<+05>-05"         "2026-01-01 05:00:00+05"]
               ["-7"    "<-07>+07"         "2025-12-31 17:00:00-07"]
               ["-7.5"  "<-07:30>+07:30"   "2025-12-31 16:30:00-07:30"]
               ["INTERVAL '+05:30' HOUR TO MINUTE" "<+05:30>-05:30" "2026-01-01 05:30:00+05:30"]]]
        (exec! a (str "SET TIME ZONE " set-to))
        (is (= shows (scalar a "SHOW timezone")) (str "SET TIME ZONE " set-to))
        (is (= rendered (scalar a "SELECT '2026-01-01 00:00+00'::timestamptz"))
            (str "SET TIME ZONE " set-to))))
    (testing "a zone name is matched case-insensitively and answered canonically"
      (exec! a "SET timezone = 'europe/berlin'")
      (is (= "Europe/Berlin" (scalar a "SHOW timezone")))
      (exec! a "SET timezone = 'utc'")
      (is (= "UTC" (scalar a "SHOW timezone")))
      (exec! a "SET timezone = 'PST8PDT'")
      (is (= "PST8PDT" (scalar a "SHOW timezone"))))
    (testing "RESET and DEFAULT go back to UTC"
      (exec! a "RESET timezone")
      (is (= "UTC" (scalar a "SHOW timezone")))
      (exec! a "SET timezone = 'Europe/Berlin'")
      (exec! a "SET timezone = DEFAULT")
      (is (= "UTC" (scalar a "SHOW timezone"))))
    (testing "an unrecognised zone raises 22023 rather than being ignored"
      (is (= "22023" (state a "SET timezone = 'Mars/Phobos'")))
      (is (= "UTC" (scalar a "SHOW timezone")) "and the old value stands"))
    (testing "a POSIX TZ string carrying its own rules is refused, not guessed"
      ;; Java's reading of `UTC-2` is not POSIX's, and PostgreSQL's
      ;; expected output does not pin it. doc/backlog.md records it.
      (is (= "22023" (state a "SET timezone = 'CST7CDT'"))))))

(deftest timezone-is-per-session
  (with-open [a (connect-to "alpha")
              b (connect-to "beta")]
    (exec! a "SET timezone = 'Europe/Berlin'")
    (exec! b "SET timezone = 'America/New_York'")
    (is (= "Europe/Berlin" (scalar a "SHOW timezone")))
    (is (= "America/New_York" (scalar b "SHOW timezone")))
    (testing "and current_setting, which reads through a cached plan"
      (is (= "Europe/Berlin" (scalar a "SELECT current_setting('timezone')")))
      (is (= "America/New_York" (scalar b "SELECT current_setting('timezone')"))))))

(deftest set-config-stores-what-set-stores
  ;; `set_config(name, value, is_local)` is the function spelling of
  ;; `SET`, and PostgreSQL treats them as one act. We had three writers
  ;; for one setting: the `SET` handler stored everything, the
  ;; whole-statement `set_config` shortcut stored only `search_path`,
  ;; and the TRANSLATED `set_config` -- the one reached when the call is
  ;; not the sole projection -- stored nothing at all and merely echoed.
  ;;
  ;; So `SET timezone = 'Europe/Berlin'` moved timestamptz output and
  ;; `set_config('timezone', 'Europe/Berlin', false)` did not, while
  ;; `SHOW timezone` reported whichever had run last. All three now go
  ;; through `guc/set-setting!`.
  (with-open [a (connect-to "alpha")]
    (testing "the whole-statement form stores, and SHOW agrees"
      (is (= "Europe/Berlin"
             (scalar a "SELECT set_config('timezone', 'Europe/Berlin', false)"))
          "the new value comes back, as PostgreSQL returns it")
      (is (= "Europe/Berlin" (scalar a "SHOW timezone")))
      (is (= "Europe/Berlin" (scalar a "SELECT current_setting('timezone')"))))
    (testing "and so does the translated form, which is not the sole projection"
      ;; A second select item takes this past classify's `sole?` gate and
      ;; into datahike.pg.sql.expr -- the path that stored nothing.
      (is (= ["America/New_York" "1"]
             (row a "SELECT set_config('timezone', 'America/New_York', false), 1")))
      (is (= "America/New_York" (scalar a "SHOW timezone"))))
    (testing "it reaches timestamptz output, which is the point"
      (exec! a "SELECT set_config('timezone', 'Europe/Berlin', false)")
      (is (= "2026-06-15 14:00:00+02"
             (scalar a "SELECT '2026-06-15 12:00:00+00'::timestamptz")))
      (is (= "2026-06-15 12:00:00"
             (scalar a "SELECT '2026-06-15 12:00:00'::timestamp"))
          "and a plain timestamp still does not move"))
    (testing "DateStyle too, through the translated form"
      (is (= "Postgres, DMY"
             (scalar a "SELECT set_config('datestyle', 'Postgres, DMY', false)")))
      (is (= "Postgres, DMY" (scalar a "SHOW datestyle"))))
    (testing "search_path, which only the statement form used to honour"
      (exec! a "SELECT set_config('search_path', 'myschema', false), 1")
      (is (= "myschema" (scalar a "SHOW search_path"))))
    (testing "a setting we do not model is answered, not refused"
      ;; asyncpg turns `jit` off around its type introspection and reads
      ;; the answer back; a 42704 there breaks its codec pipeline.
      (is (= "off" (scalar a "SELECT set_config('jit', 'off', false)")))
      (is (= ["off" "1"] (row a "SELECT set_config('jit', 'off', false), 1")))
      ;; And it is STORED, as PostgreSQL stores it. This once answered
      ;; `off` whatever was asked, because a setting we did not model
      ;; was not kept -- which also made `application_name` answer the
      ;; role name.
      (is (= "on" (scalar a "SELECT set_config('jit', 'on', false)")))
      (is (= "on" (scalar a "SELECT current_setting('jit')")))
      (is (= "myapp" (scalar a "SELECT set_config('application_name', 'myapp', false)")))
      (is (= "myapp" (scalar a "SHOW application_name"))))
    (testing "a custom dotted name is kept and read back"
      ;; PostgREST hands a request's identity to row-level security this
      ;; way; the `current_setting` after it raised 42704.
      (is (= "7" (scalar a "SELECT set_config('some.custom_thing', '7', false)")))
      (is (= "7" (scalar a "SELECT current_setting('some.custom_thing')")))
      (exec! a "RESET some.custom_thing")
      (is (= "" (scalar a "SHOW some.custom_thing"))))
    (testing "a read-only setting is PostgreSQL's 55P02"
      (is (= "55P02" (state a "SET server_version = '1'"))))
    (testing "a recognised setting given a value it cannot take still raises"
      (is (= "22023"
             (state a "SELECT set_config('timezone', 'Mars/Phobos', false)"))))
    (testing "the answer is the setting as it now stands, not the echo"
      ;; `set_config_by_name` ends with `GetConfigOptionByName`, so
      ;; PostgreSQL answers with the value the setting now HAS. That is
      ;; only the same as the string passed in when the value was taken
      ;; verbatim -- here it is canonicalised, and `SHOW` agrees.
      (is (= "Europe/Berlin" (scalar a "SELECT set_config('timezone', 'europe/berlin', false)"))
          "canonicalised, and reported the way SHOW reports it")
      (is (= "Europe/Berlin" (scalar a "SHOW timezone")))
      (is (= ["<+02>-02" "1"]
             (row a "SELECT set_config('timezone', '2', false), 1"))
          "and the translated form answers from the same reader"))
    (testing "a NULL value RESETS the setting, and answers the default"
      ;; `set_config_by_name` passes a NULL straight to
      ;; `set_config_option`, where it means "reset to default" -- and
      ;; then reports what that default is.
      (exec! a "SET timezone = 'Europe/Berlin'")
      (is (= "UTC" (scalar a "SELECT set_config('timezone', NULL, false)")))
      (is (= "UTC" (scalar a "SHOW timezone")))
      (exec! a "SET timezone = 'Europe/Berlin'")
      (is (= ["UTC" "1"] (row a "SELECT set_config('timezone', NULL, false), 1"))
          "and through the translated form")
      (is (= "UTC" (scalar a "SHOW timezone"))))
    (testing "RESET still clears every setting it used to"
      (doseq [[set-stmt var shown] [["SET timezone = 'Europe/Berlin'" "timezone" "UTC"]
                                    ["SET datestyle = 'Postgres, DMY'" "datestyle" "ISO, MDY"]
                                    ["SET search_path TO myschema" "search_path" "\"$user\", public"]]]
        (exec! a set-stmt)
        (exec! a (str "RESET " var))
        (is (= shown (scalar a (str "SHOW " var)))
            (str "RESET " var))))
    (testing "SET search_path with a value LIST keeps every element"
      ;; classify fills :value with only the FIRST element and :values
      ;; with all of them, so reading :value here dropped the rest.
      (exec! a "SET search_path TO alpha, beta, gamma")
      (is (= "alpha, beta, gamma" (scalar a "SHOW search_path"))))))

(deftest set-config-is-not-shared-between-sessions
  ;; The translated `set_config` captures its session-state atom at
  ;; TRANSLATION time, because the dynamic var is nil on the thread the
  ;; datalog function runs on. That capture is exactly what makes a
  ;; cached plan dangerous: the plan cache is server-wide, so without
  ;; `params/session-dependent!` the second session would write into the
  ;; first session's state.
  (with-open [a (connect-to "alpha")
              b (connect-to "beta")]
    (is (= ["Europe/Berlin" "1"]
           (row a "SELECT set_config('timezone', 'Europe/Berlin', false), 1")))
    (is (= ["Asia/Tokyo" "1"]
           (row b "SELECT set_config('timezone', 'Asia/Tokyo', false), 1")))
    (is (= "Europe/Berlin" (scalar a "SHOW timezone")))
    (is (= "Asia/Tokyo" (scalar b "SHOW timezone"))
        "b wrote its own session, not a's")))

(deftest settings-are-transactional
  ;; A setting is transactional in PostgreSQL, and each answer below was
  ;; read off PostgreSQL 17 for the same statements. All of these
  ;; persisted for the session: ROLLBACK did not undo a SET, nor ROLLBACK
  ;; TO SAVEPOINT one made after the savepoint, and `SET LOCAL` /
  ;; `set_config(…, true)` outlived their transaction.
  (with-open [a (connect-to "alpha")]
    (let [tz #(scalar a "SHOW timezone")]
      (testing "ROLLBACK undoes a SET, COMMIT keeps it"
        (exec! a "BEGIN") (exec! a "SET timezone = 'Asia/Tokyo'") (exec! a "ROLLBACK")
        (is (= "UTC" (tz)))
        (exec! a "BEGIN") (exec! a "SET timezone = 'Asia/Tokyo'") (exec! a "COMMIT")
        (is (= "Asia/Tokyo" (tz)))
        (exec! a "RESET timezone"))
      (testing "SET LOCAL lasts to the end of the transaction, even one that commits"
        (exec! a "BEGIN")
        (exec! a "SET timezone = 'Asia/Tokyo'")
        (exec! a "SET LOCAL timezone = 'Europe/Berlin'")
        (is (= "Europe/Berlin" (tz)))
        (exec! a "COMMIT")
        (is (= "Asia/Tokyo" (tz)) "the session-level SET survives, the LOCAL one does not")
        (exec! a "RESET timezone"))
      (testing "SET LOCAL outside a transaction changes nothing"
        (exec! a "SET LOCAL timezone = 'Asia/Tokyo'")
        (is (= "UTC" (tz))))
      (testing "set_config's is_local is SET LOCAL, in both spellings"
        (exec! a "BEGIN")
        (is (= "Asia/Tokyo" (scalar a "SELECT set_config('timezone', 'Asia/Tokyo', true)")))
        (is (= "Asia/Tokyo" (tz)))
        (exec! a "COMMIT")
        (is (= "UTC" (tz)))
        (exec! a "BEGIN")
        (is (= ["Asia/Tokyo" "1"] (row a "SELECT set_config('timezone', 'Asia/Tokyo', true), 1")))
        (exec! a "COMMIT")
        (is (= "UTC" (tz)))
        (is (= "Asia/Tokyo" (scalar a "SELECT set_config('timezone', 'Asia/Tokyo', true)"))
            "outside a block the call still answers the value it set")
        (is (= "UTC" (tz)) "and it is gone once the statement is"))
      (testing "ROLLBACK TO SAVEPOINT undoes what came after the savepoint"
        (exec! a "BEGIN")
        (exec! a "SET timezone = 'Asia/Tokyo'")
        (exec! a "SAVEPOINT s")
        (exec! a "SET timezone = 'Europe/Berlin'")
        (exec! a "ROLLBACK TO SAVEPOINT s")
        (is (= "Asia/Tokyo" (tz)))
        (exec! a "COMMIT")
        (is (= "Asia/Tokyo" (tz)))
        (exec! a "RESET timezone"))
      (testing "a failed transaction's SET is undone with it"
        (exec! a "BEGIN")
        (exec! a "SET search_path = myschema")
        (is (= "22012" (state a "SELECT 1/0")))
        (exec! a "ROLLBACK")
        (is (= "\"$user\", public" (scalar a "SHOW search_path"))))
      (testing "RESET ALL inside a block is undone by ROLLBACK"
        (exec! a "SET timezone = 'Asia/Tokyo'")
        (exec! a "BEGIN")
        (exec! a "RESET ALL")
        (is (= "UTC" (tz)))
        (exec! a "ROLLBACK")
        (is (= "Asia/Tokyo" (tz)))
        (exec! a "RESET timezone")))))
