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

(deftest ^{:compatibility-gap :sequence-row-evaluation-order
           :postgres-major 17 :postgres-nextval 2}
  known-gap-batch-sequence-reservation-precedes-first-row-failure
  ;; Differential witness, not a PostgreSQL conformance assertion. Executed
  ;; against PostgreSQL 17: both failed statements below leave nextval at 2.
  ;; Our current eager reservation consumes the second candidate too (3).
  ;; Replace this gap assertion with oracle equality when the shared statement
  ;; executor closes beta blocker :sequence-row-evaluation-order.
  (doseq [[table suffix] [["gap_plain" ""]
                          ["gap_conflict" " ON CONFLICT(v) DO NOTHING"]]]
    (run (str "CREATE TABLE " table " (id serial, v int UNIQUE CHECK(v > 0))"))
    (is (some? (err (str "INSERT INTO " table "(v) VALUES (-1),(1)" suffix))))
    (is (= [["3"]] (rows (str "SELECT nextval('" table "_id_seq')")))
        "Known gap: PostgreSQL 17 returns 2; pg-datahike currently reserves both candidates")))

(deftest generated-always-retains-default-provenance
  (run "CREATE TABLE generated_ids (id int GENERATED ALWAYS AS IDENTITY, v int)")
  (is (some? (err "INSERT INTO generated_ids VALUES (nextval('generated_ids_id_seq'), 1)")))
  (run "BEGIN")
  (is (= "INSERT 0 1" (tag "INSERT INTO generated_ids(v) VALUES (2)")))
  (run "ROLLBACK")
  (is (= [["2"]] (rows "INSERT INTO generated_ids(v) VALUES (3) RETURNING id")))
  (is (= [["3"]] (rows "INSERT INTO generated_ids(id,v) VALUES (DEFAULT,4) RETURNING id"))))

(deftest inheritance-copies-serial-defaults-but-not-identity-generation
  (run "CREATE TABLE identity_parent (id int GENERATED ALWAYS AS IDENTITY, v int)")
  (run "CREATE TABLE identity_child () INHERITS (identity_parent)")
  (is (= "INSERT 0 1" (tag "INSERT INTO identity_child(id,v) VALUES (99,1)")))
  (is (some? (err "INSERT INTO identity_child(v) VALUES (2)")))
  (is (= [["1"]] (rows "INSERT INTO identity_parent(v) VALUES (3) RETURNING id")))
  (run "CREATE TABLE serial_parent (id serial, v int)")
  (run "CREATE TABLE serial_child () INHERITS (serial_parent)")
  (is (= [["1"]] (rows "INSERT INTO serial_child(v) VALUES (1) RETURNING id")))
  (is (= [["2"]] (rows "INSERT INTO serial_parent(v) VALUES (2) RETURNING id")))
  (is (= [["NO" nil]]
         (rows "SELECT is_identity,identity_generation FROM information_schema.columns WHERE table_name='serial_parent' AND column_name='id'")))
  (is (= [[""]]
         (rows "SELECT attidentity FROM pg_attribute WHERE attrelid='serial_parent'::regclass AND attname='id'"))))

(deftest temp-identity-defaults-use-the-physical-sequence
  (run "CREATE TEMP TABLE temp_ids (id int GENERATED ALWAYS AS IDENTITY, v int)")
  (is (= [["1"]] (rows "INSERT INTO temp_ids(v) VALUES (9) RETURNING id")))
  (run "BEGIN")
  (is (= [["2"]] (rows "INSERT INTO temp_ids(v) VALUES (10) RETURNING id")))
  (run "ROLLBACK")
  (is (= [["3"]] (rows "INSERT INTO temp_ids(v) VALUES (11) RETURNING id"))))

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

(deftest do-update-unqualified-columns-belong-to-the-target-row
  (seed!)
  (testing "VALUES and INSERT SELECT both accept PostgreSQL's usual form"
    (run (str "INSERT INTO t (id, n) VALUES (1, 5) "
              "ON CONFLICT (id) DO UPDATE SET n = n + EXCLUDED.n"))
    (run (str "INSERT INTO t (id, n) SELECT 1, 7 "
              "ON CONFLICT (id) DO UPDATE SET n = n + EXCLUDED.n"))
    (is (= [["22"]] (rows "SELECT n FROM t WHERE id = 1")))))

(deftest do-update-rejects-duplicate-target-columns
  (seed!)
  (let [result (run (str "INSERT INTO t(id,title,n) VALUES (1,'new',5) "
                         "ON CONFLICT(id) DO UPDATE "
                         "SET title=excluded.title, title='again'"))]
    (is (= "42601" (.-sqlstate ^PgWireServer$QueryResult result)))
    (is (= [["1" "keep" "10"]]
           (rows "SELECT id,title,n FROM t"))
        "duplicate assignments fail atomically")))

(deftest default-values-runs-through-conflict-arbitration
  (run (str "CREATE TABLE default_conflict("
            "id int DEFAULT 1 UNIQUE, note text DEFAULT 'new')"))
  (run "INSERT INTO default_conflict VALUES (1,'old')")
  (is (= "INSERT 0 0"
         (tag (str "INSERT INTO default_conflict DEFAULT VALUES "
                   "ON CONFLICT(id) DO NOTHING"))))
  (is (= [["1" "old"]]
         (rows "SELECT id,note FROM default_conflict")))
  (is (= [["1" "new"]]
         (rows (str "INSERT INTO default_conflict DEFAULT VALUES "
                    "ON CONFLICT(id) DO UPDATE SET note=excluded.note "
                    "RETURNING id,note")))))

(deftest insert-select-without-conflict-still-inserts
  (is (= "INSERT 0 1"
         (tag "INSERT INTO t (id, title) SELECT 2, 'fresh' ON CONFLICT (id) DO NOTHING")))
  (is (= [["2" "fresh"]] (rows "SELECT id, title FROM t"))))

(deftest insert-select-self-conflicts-use-command-cardinality
  (run "CREATE TABLE src (ord int PRIMARY KEY, id int, title text)")
  (run "INSERT INTO src VALUES (1, 7, 'first'), (2, 7, 'second')")
  (testing "DO NOTHING retains only one proposed row"
    (is (= "INSERT 0 1"
           (tag (str "INSERT INTO t (id, title) "
                     "SELECT id, title FROM src ORDER BY ord "
                     "ON CONFLICT (id) DO NOTHING"))))
    (let [stored (rows "SELECT id, title FROM t WHERE id = 7")]
      (is (= 1 (count stored)))
      (is (contains? #{["7" "first"] ["7" "second"]} (first stored)))))
  (testing "DO UPDATE cannot affect one command row twice"
    (is (re-find #"cannot affect row a second time"
                 (or (err (str "INSERT INTO t (id, title) "
                               "SELECT id + 1, title FROM src ORDER BY ord "
                               "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title"))
                     "")))
    (is (= [] (rows "SELECT id, title FROM t WHERE id = 8"))
        "the cardinality failure is atomic")))

(deftest insert-select-skipped-rows-are-not-returned
  (seed!)
  (is (= []
         (rows (str "INSERT INTO t (id, title) SELECT 1, 'discard' "
                    "ON CONFLICT (id) DO NOTHING RETURNING id, title"))))
  (is (= []
         (rows (str "INSERT INTO t (id, title) SELECT 1, 'discard' "
                    "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title "
                    "WHERE FALSE RETURNING id, title")))))

(deftest post-update-arbiter-state-matches-postgres
  (run "CREATE TABLE moved (id int PRIMARY KEY, code text UNIQUE)")
  (run "INSERT INTO moved VALUES (1, 'a')")
  (testing "a key moved away by an earlier row is available later in the command"
    (is (= "INSERT 0 2"
           (tag (str "INSERT INTO moved VALUES (1, 'b'), (2, 'a') "
                     "ON CONFLICT (id) DO UPDATE SET code = EXCLUDED.code"))))
    (is (= [["1" "b"] ["2" "a"]]
           (rows "SELECT id, code FROM moved ORDER BY id"))))
  (run "CREATE TABLE moved_target (id int PRIMARY KEY, code text UNIQUE, replacement text)")
  (run "INSERT INTO moved_target VALUES (1, 'a', NULL)")
  (testing "a later row matching the moved-to arbiter targets the same row twice"
    (is (re-find #"cannot affect row a second time"
                 (or (err (str "INSERT INTO moved_target VALUES "
                               "(9, 'a', 'b'), (10, 'b', 'c') "
                               "ON CONFLICT (code) DO UPDATE "
                               "SET code = EXCLUDED.replacement"))
                     "")))
    (is (= [["1" "a"]]
           (rows "SELECT id, code FROM moved_target"))
        "the 21000 statement remains atomic"))
  (run "CREATE TABLE moved_other (id int PRIMARY KEY, code text UNIQUE)")
  (run "INSERT INTO moved_other VALUES (1, 'a')")
  (testing "a moved-to value used by a later insert still violates another unique index"
    (is (re-find #"unique"
                 (or (err (str "INSERT INTO moved_other VALUES (1, 'b'), (2, 'b') "
                               "ON CONFLICT (id) DO UPDATE SET code = EXCLUDED.code"))
                     "")))
    (is (= [["1" "a"]]
           (rows "SELECT id, code FROM moved_other"))
        "the 23505 statement remains atomic")))

(deftest generated-identities-and-defaults-precede-arbitration
  (run (str "CREATE TABLE generated_conflict ("
            "id int GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY, "
            "code text UNIQUE, n int DEFAULT 9 NOT NULL)"))
  (run "INSERT INTO generated_conflict(id, code, n) VALUES (1, 'old', 1)")
  (testing "a generated identity can itself select the arbiter row"
    (is (= [["1" "generated"]]
           (rows (str "INSERT INTO generated_conflict(code) VALUES ('generated') "
                      "ON CONFLICT(id) DO UPDATE SET code=excluded.code "
                      "RETURNING id, code")))))
  (testing "EXCLUDED contains defaults and a conflicting attempt consumes identity"
    (is (= [["1" "9"]]
           (rows (str "INSERT INTO generated_conflict(code) VALUES ('generated') "
                      "ON CONFLICT(code) DO UPDATE SET n=excluded.n "
                      "RETURNING id, n"))))
    (is (= [["3" "next"]]
           (rows (str "INSERT INTO generated_conflict(code) VALUES ('next') "
                      "RETURNING id, code"))))))

(deftest identity-sequences-advance-outside-the-statement-transaction
  (run (str "CREATE TABLE sequence_conflict("
            "id int GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY, "
            "code text UNIQUE, n int NOT NULL)"))
  (run "BEGIN")
  (run "INSERT INTO sequence_conflict(code,n) VALUES ('rolled',1)")
  (run "ROLLBACK")
  (is (= [["2" "kept"]]
         (rows (str "INSERT INTO sequence_conflict(code,n) VALUES ('kept',1) "
                    "RETURNING id,code"))))
  (run "BEGIN")
  (is (= "INSERT 0 0"
         (tag (str "INSERT INTO sequence_conflict(code,n) VALUES ('kept',2) "
                   "ON CONFLICT(code) DO NOTHING"))))
  (run "ROLLBACK")
  (is (= [["4"]]
         (rows (str "INSERT INTO sequence_conflict(code,n) VALUES ('after_skip',1) "
                    "RETURNING id"))))
  (run "BEGIN")
  (run (str "INSERT INTO sequence_conflict(code,n) VALUES ('kept',3) "
            "ON CONFLICT(code) DO UPDATE SET n=excluded.n"))
  (run "ROLLBACK")
  (is (= [["1"]]
         (rows "SELECT n FROM sequence_conflict WHERE code='kept'")))
  (is (= [["6"]]
         (rows (str "INSERT INTO sequence_conflict(code,n) VALUES ('after_update',1) "
                    "RETURNING id"))))
  (let [failed (run "INSERT INTO sequence_conflict(code,n) VALUES ('bad',NULL)")]
    (is (= "23502" (.-sqlstate ^PgWireServer$QueryResult failed))))
  (is (= [["8"]]
         (rows (str "INSERT INTO sequence_conflict(code,n) VALUES ('after_error',1) "
                    "RETURNING id")))))

(deftest row-constraints-have-postgresql-conflict-timing
  (run "CREATE TABLE checked_conflict(id int PRIMARY KEY, n int NOT NULL CHECK(n > 0))")
  (run "INSERT INTO checked_conflict VALUES (1, 1)")
  (testing "NOT NULL and CHECK reject the candidate before DO NOTHING arbitration"
    (is (re-find #"not-null" (or (err (str "INSERT INTO checked_conflict VALUES (1, NULL) "
                                           "ON CONFLICT(id) DO NOTHING")) "")))
    (is (re-find #"check constraint"
                 (or (err (str "INSERT INTO checked_conflict VALUES (1, -1) "
                               "ON CONFLICT(id) DO NOTHING")) ""))))
  (run "CREATE TABLE conflict_parent(id int PRIMARY KEY)")
  (run (str "CREATE TABLE conflict_child(id int PRIMARY KEY, parent_id int "
            "REFERENCES conflict_parent(id))"))
  (run "INSERT INTO conflict_parent VALUES (1)")
  (run "INSERT INTO conflict_child VALUES (1, 1)")
  (testing "FK checks only rows that are actually inserted or updated"
    (is (= "INSERT 0 0"
           (tag (str "INSERT INTO conflict_child VALUES (1, 999) "
                     "ON CONFLICT(id) DO NOTHING"))))
    (is (re-find #"foreign key"
                 (or (err (str "INSERT INTO conflict_child VALUES (2, 999) "
                               "ON CONFLICT(id) DO NOTHING")) "")))
    (is (re-find #"foreign key"
                 (or (err (str "INSERT INTO conflict_child VALUES (1, 999) "
                               "ON CONFLICT(id) DO UPDATE "
                               "SET parent_id=excluded.parent_id")) "")))))

(deftest insert-select-order-determines-the-conflict-winner
  (run "CREATE TABLE ordered_source(ord int PRIMARY KEY, id int, title text)")
  (run "INSERT INTO ordered_source VALUES (1, 40, 'first'), (2, 40, 'second')")
  (run (str "INSERT INTO t(id, title) "
            "SELECT id, title FROM ordered_source ORDER BY ord DESC "
            "ON CONFLICT(id) DO NOTHING"))
  (is (= [["40" "second"]]
         (rows "SELECT id, title FROM t WHERE id=40"))))

(deftest non-arbiter-uniqueness-is-immediate
  (run "CREATE TABLE immediate_unique(id int PRIMARY KEY, code text UNIQUE)")
  (run "INSERT INTO immediate_unique VALUES (1, 'a'), (2, 'b')")
  (testing "a two-row key swap fails on the first update"
    (is (re-find #"unique"
                 (or (err (str "INSERT INTO immediate_unique VALUES (1, 'b'), (2, 'a') "
                               "ON CONFLICT(id) DO UPDATE SET code=excluded.code")) "")))
    (is (= [["1" "a"] ["2" "b"]]
           (rows "SELECT id, code FROM immediate_unique ORDER BY id"))))
  (testing "a native identity PK remains enforced when another index arbitrates"
    (is (re-find #"unique"
                 (or (err (str "INSERT INTO immediate_unique VALUES (1, 'c') "
                               "ON CONFLICT(code) DO NOTHING")) "")))))

(deftest inherited-conflict-targets-use-the-parent-storage-attribute
  (run "CREATE TABLE conflict_base(p int)")
  (run "CREATE TABLE conflict_leaf(c int) INHERITS(conflict_base)")
  (run "CREATE UNIQUE INDEX conflict_leaf_p_key ON conflict_leaf(p)")
  (run "INSERT INTO conflict_leaf(p, c) VALUES (1, 10)")
  (is (= [["2" "20"]]
         (rows (str "INSERT INTO conflict_leaf(p, c) VALUES (1, 20) "
                    "ON CONFLICT(p) DO UPDATE "
                    "SET p=excluded.p + 1, c=excluded.c RETURNING p,c"))))
  (is (= [["2" "20"]]
         (rows "SELECT p,c FROM conflict_leaf"))))

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
