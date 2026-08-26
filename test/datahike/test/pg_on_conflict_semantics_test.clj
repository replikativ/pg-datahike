(ns datahike.test.pg-on-conflict-semantics-test
  "ON CONFLICT arbitration and row counts — the follow-ups reported on
   PR #30, which fixed parameter resolution in these statements but
   left the conflict-target semantics alone.

   Every case here previously LOST DATA rather than erroring, and in the
   same way: when the arbiter doesn't match, the row is inserted, and
   Datahike's :db.unique/identity upsert then overwrites the row the
   statement was asking to preserve. So `ON CONFLICT … DO NOTHING`
   replaced the value it was supposed to leave alone.

     1. `INSERT … SELECT … ON CONFLICT (id)` ignored the target and
        arbitrated on ALL inserted columns, and its DO UPDATE arm was
        unimplemented.
     2. `ON CONFLICT ON CONSTRAINT t_pkey` degraded to an empty arbiter
        list — \"never conflicts\".
     3. `DO UPDATE … WHERE cond` was parsed and dropped, so rows the
        condition excluded got updated anyway.
     4. `DO NOTHING` reported `INSERT 0 1` for a row it skipped.

   Expectations captured from PostgreSQL 17 by differential testing."
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
          (.execute *handler*
                    "CREATE TABLE t (id int PRIMARY KEY, title text, n int)")
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))

(defn- tag [sql]
  (.-commandTag ^PgWireServer$QueryResult (run sql)))

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))

(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (run sql))
       (catch Exception e (ex-message e))))

(defn- seed! []
  (run "INSERT INTO t (id, title, n) VALUES (1, 'keep', 10)"))

;; ---------------------------------------------------------------------------
;; 1. INSERT … SELECT … ON CONFLICT
;; ---------------------------------------------------------------------------

(deftest insert-select-do-nothing-honours-the-conflict-target
  (testing "the row it was told to leave alone survives"
    (seed!)
    (is (= "INSERT 0 0"
           (tag "INSERT INTO t (id, title) SELECT 1, 'discard' ON CONFLICT (id) DO NOTHING")))
    (is (= [["1" "keep" "10"]] (rows "SELECT id, title, n FROM t")))))

(deftest insert-select-do-update-is-implemented
  (seed!)
  (is (= "INSERT 0 1"
         (tag (str "INSERT INTO t (id, title) SELECT 1, 'new' "
                   "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title"))))
  (is (= [["1" "new" "10"]] (rows "SELECT id, title, n FROM t"))
      "the non-updated column keeps its value"))

(deftest insert-select-do-update-evaluates-expressions
  (seed!)
  (run (str "INSERT INTO t (id, n) SELECT 1, 5 "
            "ON CONFLICT (id) DO UPDATE SET n = t.n + EXCLUDED.n"))
  (is (= [["15"]] (rows "SELECT n FROM t WHERE id = 1"))))

(deftest insert-select-without-conflict-still-inserts
  (is (= "INSERT 0 1"
         (tag "INSERT INTO t (id, title) SELECT 2, 'fresh' ON CONFLICT (id) DO NOTHING")))
  (is (= [["2" "fresh"]] (rows "SELECT id, title FROM t"))))

;; ---------------------------------------------------------------------------
;; 2. ON CONFLICT ON CONSTRAINT
;; ---------------------------------------------------------------------------

(deftest on-constraint-pkey-arbitrates-on-the-primary-key
  (seed!)
  (is (= "INSERT 0 0"
         (tag (str "INSERT INTO t (id, title) VALUES (1, 'overwrite') "
                   "ON CONFLICT ON CONSTRAINT t_pkey DO NOTHING"))))
  (is (= [["keep"]] (rows "SELECT title FROM t WHERE id = 1"))))

(deftest on-constraint-pkey-with-do-update
  (seed!)
  (run (str "INSERT INTO t (id, title) VALUES (1, 'updated') "
            "ON CONFLICT ON CONSTRAINT t_pkey DO UPDATE SET title = EXCLUDED.title"))
  (is (= [["updated"]] (rows "SELECT title FROM t WHERE id = 1"))))

(deftest unknown-constraint-name-raises
  (testing "an unresolvable name must NOT degrade to an empty arbiter —
            that reads as 'never conflicts' and overwrites the row"
    (seed!)
    (is (re-find #"does not exist"
                 (or (err (str "INSERT INTO t (id, title) VALUES (1, 'x') "
                               "ON CONFLICT ON CONSTRAINT nope DO NOTHING")) "")))
    (is (= [["keep"]] (rows "SELECT title FROM t WHERE id = 1")))))

;; ---------------------------------------------------------------------------
;; 3. DO UPDATE … WHERE
;; ---------------------------------------------------------------------------

(deftest do-update-where-false-skips-the-update
  (seed!)
  (is (= "INSERT 0 0"
         (tag (str "INSERT INTO t (id, title) VALUES (1, 'new') "
                   "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title WHERE FALSE"))))
  (is (= [["keep"]] (rows "SELECT title FROM t WHERE id = 1"))))

(deftest do-update-where-reads-the-existing-row
  (seed!)
  (testing "condition false for this row — no update"
    (is (= "INSERT 0 0"
           (tag (str "INSERT INTO t (id, title) VALUES (1, 'new') "
                     "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title "
                     "WHERE t.n > 100"))))
    (is (= [["keep"]] (rows "SELECT title FROM t WHERE id = 1"))))
  (testing "condition true — update applies"
    (is (= "INSERT 0 1"
           (tag (str "INSERT INTO t (id, title) VALUES (1, 'new') "
                     "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title "
                     "WHERE t.n > 5"))))
    (is (= [["new"]] (rows "SELECT title FROM t WHERE id = 1")))))

(deftest do-update-where-can-read-excluded
  (seed!)
  (run (str "INSERT INTO t (id, title, n) VALUES (1, 'bigger', 50) "
            "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title "
            "WHERE EXCLUDED.n > t.n"))
  (is (= [["bigger"]] (rows "SELECT title FROM t WHERE id = 1")))
  (run (str "INSERT INTO t (id, title, n) VALUES (1, 'smaller', 1) "
            "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title "
            "WHERE EXCLUDED.n > t.n"))
  (is (= [["bigger"]] (rows "SELECT title FROM t WHERE id = 1"))
      "a smaller EXCLUDED.n must not win"))

;; ---------------------------------------------------------------------------
;; 4. Row counts
;; ---------------------------------------------------------------------------

(deftest do-nothing-on-conflict-reports-zero
  (seed!)
  (is (= "INSERT 0 0"
         (tag "INSERT INTO t (id, title) VALUES (1, 'x') ON CONFLICT (id) DO NOTHING"))))

(deftest do-nothing-without-conflict-reports-one
  (is (= "INSERT 0 1"
         (tag "INSERT INTO t (id, title) VALUES (9, 'x') ON CONFLICT (id) DO NOTHING"))))

(deftest do-update-on-conflict-reports-one
  (seed!)
  (is (= "INSERT 0 1"
         (tag (str "INSERT INTO t (id, title) VALUES (1, 'x') "
                   "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title")))))

(deftest multi-row-counts-only-what-it-wrote
  (seed!)
  (is (= "INSERT 0 2"
         (tag (str "INSERT INTO t (id, title) VALUES (7,'a'), (1,'b'), (8,'c') "
                   "ON CONFLICT (id) DO NOTHING")))
      "three rows offered, one conflicted")
  (is (= [["1" "keep"] ["7" "a"] ["8" "c"]]
         (rows "SELECT id, title FROM t ORDER BY id"))))

(deftest plain-insert-count-is-unchanged
  (is (= "INSERT 0 1" (tag "INSERT INTO t (id, title) VALUES (3, 'a')")))
  (is (= "INSERT 0 2" (tag "INSERT INTO t (id, title) VALUES (4, 'b'), (5, 'c')"))))

;; ---------------------------------------------------------------------------
;; Unsupported arbiter forms must raise, not silently mis-arbitrate
;; ---------------------------------------------------------------------------

(deftest index-predicate-arbiter-raises
  (testing "ON CONFLICT (col) WHERE pred names a PARTIAL index; ignoring
            the predicate changes which rows count as conflicting"
    (seed!)
    (is (re-find #"index predicate is not supported"
                 (or (err (str "INSERT INTO t (id, title) VALUES (1, 'x') "
                               "ON CONFLICT (id) WHERE n > 0 DO NOTHING")) "")))
    (is (= [["keep"]] (rows "SELECT title FROM t WHERE id = 1")))))

;; ---------------------------------------------------------------------------
;; Guard: the plain forms PR #30 fixed keep working
;; ---------------------------------------------------------------------------

(deftest targetless-on-conflict-still-works
  (seed!)
  (is (= "INSERT 0 0"
         (tag "INSERT INTO t (id, title, n) VALUES (1, 'keep', 10) ON CONFLICT DO NOTHING")))
  (is (= [["1" "keep" "10"]] (rows "SELECT id, title, n FROM t"))))

(deftest composite-conflict-target
  (run "CREATE TABLE c (a int, b int, v text, UNIQUE (a,b))")
  (run "INSERT INTO c (a, b, v) VALUES (1, 2, 'orig')")
  (run (str "INSERT INTO c (a, b, v) VALUES (1, 2, 'new') "
            "ON CONFLICT (a, b) DO UPDATE SET v = EXCLUDED.v"))
  (is (= [["1" "2" "new"]] (rows "SELECT a, b, v FROM c"))))
