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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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
