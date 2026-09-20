(ns datahike.test.pg-function-resolution-test
  "A SQL function name resolves against what this server implements, and
   nothing else.

   The scalar-function translator used to end in a fallback: a name
   `clojure.core/resolve` could resolve was emitted as a Datalog clause,
   \"so a caller reaches a Clojure fn we did not enumerate\". Every
   public of `clojure.core` was therefore a function of this server, for
   anyone who can connect:

     select slurp('/etc/passwd')     read the file
     select spit('/tmp/x', 'y')      wrote the file
     select inc(1)                   answered 2
     select deref(1)                 a ClassCastException, XX000

   The decorated call forms resolved no better. `f(x) OVER (…)` built a
   window spec for any name at all, so an unknown one failed at
   execution with 0A000 and a scalar one with datalog's \"Cannot parse
   :find\" (XX000); `f(x) FILTER (WHERE …)` fell through to a default
   aggregate, so `nosuchfn(a) FILTER (WHERE true)` ANSWERED -- a COUNT.

   PostgreSQL resolves every call in the catalog: 42883 when the name is
   unknown, 42809 when it is a function of the wrong kind
   (ParseFuncOrColumn / transformWindowFuncCall, parse_func.c).
   Expectations here -- messages and SQLSTATEs -- are a PostgreSQL 17
   oracle's."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn- fr-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"fr" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fr-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/fr?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- err
  "[sqlstate, first line of the message] for a statement that must fail."
  [^Connection c sql]
  (try
    (with-open [st (.createStatement c)] (.execute st sql))
    [:no-error sql]
    (catch SQLException e
      [(.getSQLState e) (first (str/split-lines (.getMessage e)))])))

(defn- state [^Connection c sql] (first (err c sql)))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- scalar [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (.next rs)
    (.getString rs 1)))

(deftest a-name-this-server-does-not-implement-is-42883
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE fr_t (a int)")
    (exec! c "INSERT INTO fr_t VALUES (1), (2)")
    (testing "a Clojure function is not a SQL function"
      ;; The two that matter: they reach the filesystem of the host the
      ;; server runs on.
      (is (= ["42883" "ERROR: function slurp(unknown) does not exist"]
             (err c "SELECT slurp('/etc/hostname')")))
      (is (= ["42883" "ERROR: function spit(unknown, unknown) does not exist"]
             (err c "SELECT spit('/tmp/pg-datahike-must-not-exist', 'y')")))
      (is (false? (.exists (java.io.File. "/tmp/pg-datahike-must-not-exist"))))
      ;; And the rest of clojure.core with them, including the names that
      ;; used to answer (inc) and the ones that used to leak an internal
      ;; error class (deref, eval).
      (doseq [sql ["SELECT inc(1)" "SELECT identity(5)" "SELECT deref(1)"
                   "SELECT eval(1)" "SELECT read_string('(+ 1 2)')"]]
        (is (= "42883" (state c sql)) sql)))
    (testing "the message names the argument types, as ParseFuncOrColumn does"
      (is (= ["42883" "ERROR: function nosuchfn(integer) does not exist"]
             (err c "SELECT nosuchfn(1)")))
      (is (= ["42883" "ERROR: function nosuchfn(unknown, integer) does not exist"]
             (err c "SELECT nosuchfn('a', 2)"))))
    (testing "wherever a call can stand"
      (doseq [sql ["SELECT sum(inc(a)) FROM fr_t"
                   "SELECT a FROM fr_t WHERE inc(a) = 2"
                   "SELECT max(a) FILTER (WHERE inc(a) = 2) FROM fr_t"
                   "SELECT a FROM fr_t ORDER BY inc(a)"
                   "SELECT a FROM fr_t GROUP BY inc(a)"
                   "SELECT * FROM fr_t t1 JOIN fr_t t2 ON inc(t1.a) = t2.a"
                   "INSERT INTO fr_t VALUES (inc(1))"
                   "UPDATE fr_t SET a = inc(a)"
                   "DELETE FROM fr_t WHERE inc(a) = 2"]]
        (is (= "42883" (state c sql)) sql)))
    (testing "a schema qualifier does not open another namespace"
      ;; pg_catalog is where the builtins are, so it resolves to the same
      ;; function; every other qualifier stays part of the name and so
      ;; resolves to nothing -- `public.upper('a')` is 42883 in
      ;; PostgreSQL too, because upper is in pg_catalog.
      (is (= "A" (scalar c "SELECT pg_catalog.upper('a')")))
      (is (= "42883" (state c "SELECT public.upper('a')")))
      (is (= "42883" (state c "SELECT pg_catalog.inc(1)")))
      (is (= "42883" (state c "SELECT public.inc(1)")))
      (testing "including the aggregate and window paths, which read the name
                for themselves"
        ;; `pg_catalog.count(*)` reported that `count` does not exist,
        ;; on a server where `count(*)` answers; `pg_catalog.max(a)`
        ;; was worse -- it fell through to a per-ROW max, a silently
        ;; wrong column.
        (is (= "2" (scalar c "SELECT pg_catalog.count(*) FROM fr_t")))
        (is (= "3" (scalar c "SELECT pg_catalog.sum(a) FROM fr_t")))
        (is (= "2" (scalar c "SELECT pg_catalog.max(a) FROM fr_t")))
        (is (= "1" (scalar c "SELECT pg_catalog.row_number() OVER () FROM fr_t")))))
    (testing "a FROM-clause function resolves under the same rule"
      (is (= "1" (scalar c "SELECT * FROM generate_series(1,2)")))
      (is (= "1" (scalar c "SELECT * FROM pg_catalog.generate_series(1,2)")))
      ;; Taking the last dot-separated segment as the name invented a
      ;; function: this returned rows. (PostgreSQL rejects it one step
      ;; earlier, with 3F000 `schema "nosuchschema" does not exist`; we
      ;; do not read the schema yet -- see doc/consolidation-plan.md.)
      (is (= "42883" (state c "SELECT * FROM nosuchschema.unnest(ARRAY[1,2])")))
      (is (= "42883" (state c "SELECT * FROM nosuchfn(1)"))))
    (testing "the functions this server does implement still answer"
      (is (= "A" (scalar c "SELECT upper('a')")))
      (is (= "3" (scalar c "SELECT length('abc')")))
      (is (= "2" (scalar c "SELECT abs(-2)")))
      (is (= "2" (scalar c "SELECT count(*) FROM fr_t"))))))

(deftest only-a-window-function-or-an-aggregate-takes-over
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE fw (a int)")
    (exec! c "INSERT INTO fw VALUES (1), (2)")
    (testing "an unknown name is 42883, not a window spec that fails later"
      (is (= ["42883" "ERROR: function inc(integer) does not exist"]
             (err c "SELECT inc(a) OVER () FROM fw")))
      (is (= "42883" (state c "SELECT nosuchfn(a) OVER (PARTITION BY a) FROM fw"))))
    (testing "a scalar function with OVER is 42809"
      (is (= ["42809" (str "ERROR: OVER specified, but upper is not a window "
                           "function nor an aggregate function")]
             (err c "SELECT upper('x') OVER () FROM fw")))
      (is (= "42809" (state c "SELECT abs(a) OVER () FROM fw"))))
    (testing "a function we implement but PostgreSQL does not window is 42809 too"
      ;; These names are handled by the translator directly and appear
      ;; in none of its tables, so asking the tables alone answered
      ;; \"function nextval does not exist\" on a server where
      ;; `nextval('s')` works. The catalog answers instead.
      (exec! c "CREATE SEQUENCE fs")
      (is (= "42809" (state c "SELECT nextval('fs') OVER () FROM fw")))
      (is (= "42809" (state c "SELECT date('2020-01-01') OVER () FROM fw"))))
    (testing "the window functions and aggregates still work"
      (doseq [sql ["SELECT row_number() OVER (ORDER BY a) FROM fw"
                   "SELECT rank() OVER (ORDER BY a) FROM fw"
                   "SELECT dense_rank() OVER (ORDER BY a) FROM fw"
                   "SELECT ntile(1) OVER (ORDER BY a) FROM fw"
                   "SELECT first_value(a) OVER (ORDER BY a) FROM fw"
                   "SELECT last_value(a) OVER (ORDER BY a) FROM fw"
                   "SELECT nth_value(a, 1) OVER (ORDER BY a) FROM fw"
                   "SELECT percent_rank() OVER (ORDER BY a) FROM fw"
                   "SELECT cume_dist() OVER (ORDER BY a) FROM fw"]]
        (is (some? (scalar c sql)) sql))
      (is (= "1" (scalar c "SELECT row_number() OVER (ORDER BY a) FROM fw")))
      (is (= "1" (scalar c "SELECT first_value(a) OVER (ORDER BY a) FROM fw")))
      (is (nil? (scalar c "SELECT lag(a) OVER (ORDER BY a) FROM fw")))
      (is (= "2" (scalar c "SELECT lead(a) OVER (ORDER BY a) FROM fw")))
      (is (= "3" (scalar c "SELECT sum(a) OVER () FROM fw")))
      (is (= "2" (scalar c "SELECT count(*) OVER () FROM fw"))))))

(deftest only-an-aggregate-takes-filter
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE ff (a int)")
    (exec! c "INSERT INTO ff VALUES (1), (2)")
    (testing "an unknown name answered a COUNT"
      (is (= ["42883" "ERROR: function nosuchfn(integer) does not exist"]
             (err c "SELECT nosuchfn(a) FILTER (WHERE true) FROM ff"))))
    (testing "a scalar function with FILTER is 42809"
      (is (= ["42809" "ERROR: FILTER specified, but upper is not an aggregate function"]
             (err c "SELECT upper('x') FILTER (WHERE true) FROM ff"))))
    (testing "an aggregate with FILTER still works"
      (is (= "2" (scalar c "SELECT sum(a) FILTER (WHERE a > 1) FROM ff")))
      (is (= "1" (scalar c "SELECT count(*) FILTER (WHERE a > 1) FROM ff"))))))

(deftest a-check-constraint-resolves-its-functions-too
  ;; PostgreSQL resolves the CHECK expression when the table is created
  ;; and raises 42883 there; this server accepts the DDL and resolves on
  ;; first use. What must not happen either way is the call SUCCEEDING.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE fc (a int CHECK (inc(a) > 0))")
    (is (= "42883" (state c "INSERT INTO fc VALUES (1)")))))
