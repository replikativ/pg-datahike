(ns datahike.test.pg-sequence-session-test
  "currval / setval session and flag semantics.

   Three silent wrong answers, found by mapping sequence.c rather than
   by a failing report:

     1. `currval` returned the sequence's STORED value — or 0 for a
        never-advanced one — instead of raising 55000. currval is
        session-scoped in PG precisely because the caller reads it as
        \"the id my insert just got\"; answering with whatever another
        connection did last is the one thing it must not do.

     2. `setval(s, n, false)` ignored the is_called flag, so the next
        `nextval` returned n+increment where PG returns n. Wrong data,
        not just a wrong error.

     3. `setval` accepted a value outside the sequence's
        MINVALUE/MAXVALUE instead of raising 22003.

   We store the last value HANDED OUT, so `is_called false` persists
   `n - increment` — the same state PG spells as last_value=n with
   is_called=false.

   Messages and SQLSTATEs are byte-for-byte from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- v [sql]
  (ffirst (mapv vec (.-rows ^PgWireServer$QueryResult (.execute *handler* sql)))))

(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (.execute *handler* sql))
       (catch Exception e (ex-message e))))

;; ---------------------------------------------------------------------------
;; currval is session-scoped
;; ---------------------------------------------------------------------------

(deftest currval-before-nextval-raises
  (.execute *handler* "CREATE SEQUENCE s")
  (is (= "currval of sequence \"s\" is not yet defined in this session"
         (err "SELECT currval('s')"))
      "used to answer 0"))

(deftest currval-after-nextval-in-this-session
  (.execute *handler* "CREATE SEQUENCE s")
  (.execute *handler* "SELECT nextval('s')")
  (is (= "1" (v "SELECT currval('s')")))
  (.execute *handler* "SELECT nextval('s')")
  (is (= "2" (v "SELECT currval('s')"))))

(deftest currval-is-per-sequence
  (testing "advancing one sequence does not define currval for another"
    (.execute *handler* "CREATE SEQUENCE a")
    (.execute *handler* "CREATE SEQUENCE b")
    (.execute *handler* "SELECT nextval('a')")
    (is (= "1" (v "SELECT currval('a')")))
    (is (= "currval of sequence \"b\" is not yet defined in this session"
           (err "SELECT currval('b')")))))

(deftest newly-created-sequence-advances-transaction-locally
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE fresh_s")
  (is (= "1" (v "SELECT nextval('fresh_s')")))
  (is (= "2" (v "SELECT nextval('fresh_s')")))
  (.execute *handler* "COMMIT")
  (is (= "3" (v "SELECT nextval('fresh_s')"))))

(deftest rollback-removes-a-new-sequence-and-its-advances
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE rolled_back_s")
  (is (= "1" (v "SELECT nextval('rolled_back_s')")))
  (.execute *handler* "ROLLBACK")
  (is (= "relation \"rolled_back_s\" does not exist"
         (err "SELECT nextval('rolled_back_s')"))))

(deftest dropping-a-serial-table-drops-and-resets-its-owned-sequence
  (.execute *handler* "CREATE TABLE serial_owner (id serial PRIMARY KEY)")
  (is (= "1" (v "INSERT INTO serial_owner DEFAULT VALUES RETURNING id")))
  (.execute *handler* "DROP TABLE serial_owner")
  (is (= "relation \"serial_owner_id_seq\" does not exist"
         (err "SELECT nextval('serial_owner_id_seq')")))
  (.execute *handler* "CREATE TABLE serial_owner (id serial PRIMARY KEY)")
  (is (= "1" (v "INSERT INTO serial_owner DEFAULT VALUES RETURNING id"))))

(deftest transactional-drop-and-recreate-uses-the-replacement-serial-sequence
  (.execute *handler* "CREATE TABLE replace_serial (id serial PRIMARY KEY)")
  (is (= "1" (v "INSERT INTO replace_serial DEFAULT VALUES RETURNING id")))
  (.execute *handler* "BEGIN")
  (.execute *handler* "DROP TABLE replace_serial")
  (.execute *handler* "CREATE TABLE replace_serial (id serial PRIMARY KEY)")
  (is (= "1" (v "INSERT INTO replace_serial DEFAULT VALUES RETURNING id")))
  (.execute *handler* "COMMIT")
  (is (= "2" (v "INSERT INTO replace_serial DEFAULT VALUES RETURNING id"))))

(deftest dropping-a-table-preserves-an-unowned-same-pattern-sequence
  (.execute *handler* "CREATE TABLE unowned_pattern (id integer)")
  (.execute *handler* "CREATE SEQUENCE unowned_pattern_id_seq")
  (.execute *handler* "DROP TABLE unowned_pattern")
  (is (= "1" (v "SELECT nextval('unowned_pattern_id_seq')"))))

(deftest serial-creation-does-not-adopt-an-existing-same-pattern-sequence
  (.execute *handler* "CREATE SEQUENCE collision_id_seq")
  (is (= "relation \"collision_id_seq\" already exists"
         (err "CREATE TABLE collision (id serial)")))
  (is (= "1" (v "SELECT nextval('collision_id_seq')")))
  (is (= "relation \"collision\" does not exist"
         (err "SELECT * FROM collision"))))

(deftest savepoint-rollback-preserves-new-sequence-reservations
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE before_savepoint")
  (.execute *handler* "SAVEPOINT s")
  (is (= "1" (v "SELECT nextval('before_savepoint')")))
  (.execute *handler* "ROLLBACK TO SAVEPOINT s")
  ;; The sequence row survives because it predates the savepoint, and the
  ;; value handed out survives because PostgreSQL sequence advances are not
  ;; subtransactional.
  (is (= "2" (v "SELECT nextval('before_savepoint')")))
  (.execute *handler* "COMMIT")
  (is (= "3" (v "SELECT nextval('before_savepoint')"))))

(deftest savepoint-rollback-persists-reservation-without-another-nextval
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE commit_after_rollback")
  (.execute *handler* "SAVEPOINT s")
  (is (= "1" (v "SELECT nextval('commit_after_rollback')")))
  (.execute *handler* "ROLLBACK TO SAVEPOINT s")
  (.execute *handler* "COMMIT")
  (is (= "2" (v "SELECT nextval('commit_after_rollback')"))))

(deftest savepoint-rollback-discards-a-later-sequence-generation
  (.execute *handler* "BEGIN")
  (.execute *handler* "SAVEPOINT s")
  (.execute *handler* "CREATE SEQUENCE after_savepoint")
  (is (= "1" (v "SELECT nextval('after_savepoint')")))
  (.execute *handler* "ROLLBACK TO SAVEPOINT s")
  (is (= "relation \"after_savepoint\" does not exist"
         (err "SELECT nextval('after_savepoint')")))
  (.execute *handler* "ROLLBACK"))

(deftest local-nextval-observes-setval
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE local_setval")
  (is (= "1" (v "SELECT nextval('local_setval')")))
  (is (= "42" (v "SELECT setval('local_setval', 42)")))
  (is (= "43" (v "SELECT nextval('local_setval')")))
  (.execute *handler* "ROLLBACK"))

(deftest savepoint-rollback-preserves-local-setval
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE local_setval_savepoint")
  (is (= "1" (v "SELECT nextval('local_setval_savepoint')")))
  (.execute *handler* "SAVEPOINT s")
  (is (= "42" (v "SELECT setval('local_setval_savepoint', 42)")))
  (.execute *handler* "ROLLBACK TO SAVEPOINT s")
  (is (= "43" (v "SELECT nextval('local_setval_savepoint')")))
  (.execute *handler* "ROLLBACK"))

(deftest savepoint-rollback-restores-pre-alter-sequence-generation
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE local_restart")
  (is (= "1" (v "SELECT nextval('local_restart')")))
  (.execute *handler* "SAVEPOINT s")
  (.execute *handler* "ALTER SEQUENCE local_restart RESTART WITH 42")
  (is (= "42" (v "SELECT nextval('local_restart')")))
  (.execute *handler* "ROLLBACK TO SAVEPOINT s")
  (is (= "2" (v "SELECT nextval('local_restart')")))
  (.execute *handler* "COMMIT")
  (is (= "3" (v "SELECT nextval('local_restart')"))))

(deftest nested-savepoint-rollback-retains-older-sequence-generation
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE nested_generation")
  (is (= "1" (v "SELECT nextval('nested_generation')")))
  (.execute *handler* "SAVEPOINT outer_sp")
  (is (= "2" (v "SELECT nextval('nested_generation')")))
  (.execute *handler* "ALTER SEQUENCE nested_generation RESTART WITH 42")
  (.execute *handler* "SAVEPOINT inner_sp")
  (is (= "42" (v "SELECT nextval('nested_generation')")))
  (.execute *handler* "ROLLBACK TO SAVEPOINT inner_sp")
  (.execute *handler* "ROLLBACK TO SAVEPOINT outer_sp")
  (is (= "3" (v "SELECT nextval('nested_generation')"))
      "the gen-0 reservation remains reachable through the outer savepoint")
  (.execute *handler* "ROLLBACK"))

(deftest savepoint-distinguishes-local-drop-recreate-incarnations
  (.execute *handler* "BEGIN")
  (.execute *handler* "CREATE SEQUENCE local_incarnation")
  (is (= "1" (v "SELECT nextval('local_incarnation')")))
  (is (= "2" (v "SELECT nextval('local_incarnation')")))
  (.execute *handler* "SAVEPOINT s")
  (.execute *handler* "DROP SEQUENCE local_incarnation")
  (.execute *handler* "CREATE SEQUENCE local_incarnation")
  (is (= "1" (v "SELECT nextval('local_incarnation')")))
  (.execute *handler* "ROLLBACK TO SAVEPOINT s")
  (is (= "3" (v "SELECT nextval('local_incarnation')")))
  (.execute *handler* "ROLLBACK"))

(deftest setval-with-is-called-true-defines-currval
  (.execute *handler* "CREATE SEQUENCE s")
  (.execute *handler* "SELECT setval('s', 42)")
  (is (= "42" (v "SELECT currval('s')"))))

(deftest setval-with-is-called-false-does-not-define-currval
  (.execute *handler* "CREATE SEQUENCE s")
  (.execute *handler* "SELECT setval('s', 42, false)")
  (is (= "currval of sequence \"s\" is not yet defined in this session"
         (err "SELECT currval('s')"))))

;; ---------------------------------------------------------------------------
;; setval's is_called flag
;; ---------------------------------------------------------------------------

(deftest setval-is-called-controls-the-next-value
  (.execute *handler* "CREATE SEQUENCE s")
  (testing "false — the next nextval returns n ITSELF"
    (is (= "10" (v "SELECT setval('s', 10, false)")) "setval returns n either way")
    (is (= "10" (v "SELECT nextval('s')")) "was 11"))
  (testing "true — the next nextval returns n + increment"
    (is (= "20" (v "SELECT setval('s', 20, true)")))
    (is (= "21" (v "SELECT nextval('s')"))))
  (testing "the 2-arg form means true"
    (is (= "30" (v "SELECT setval('s', 30)")))
    (is (= "31" (v "SELECT nextval('s')")))))

(deftest setval-is-called-respects-a-custom-increment
  (.execute *handler* "CREATE SEQUENCE s INCREMENT 5 START 100")
  (is (= "100" (v "SELECT setval('s', 100, false)")))
  (is (= "100" (v "SELECT nextval('s')")))
  (is (= "105" (v "SELECT nextval('s')"))))

;; ---------------------------------------------------------------------------
;; setval bounds
;; ---------------------------------------------------------------------------

(deftest setval-out-of-bounds-raises
  (.execute *handler* "CREATE SEQUENCE s MINVALUE 5 MAXVALUE 20 START 5")
  (is (= "setval: value 999 is out of bounds for sequence \"s\" (5..20)"
         (err "SELECT setval('s', 999)")))
  (is (= "setval: value 1 is out of bounds for sequence \"s\" (5..20)"
         (err "SELECT setval('s', 1)")))
  (testing "a value inside the range is fine"
    (is (= "12" (v "SELECT setval('s', 12)")))))

;; ---------------------------------------------------------------------------
;; lastval, which shares the session state
;; ---------------------------------------------------------------------------

(deftest lastval-is-not-defined-by-setval
  (testing "PG sets lastval only from nextval"
    (.execute *handler* "CREATE SEQUENCE s")
    (.execute *handler* "SELECT setval('s', 42)")
    (is (= "lastval is not yet defined in this session"
           (err "SELECT lastval()")))
    (.execute *handler* "SELECT nextval('s')")
    (is (= "43" (v "SELECT lastval()")))))
