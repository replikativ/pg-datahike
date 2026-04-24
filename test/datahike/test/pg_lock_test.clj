(ns datahike.test.pg-lock-test
  "Tests for row-level locking (SELECT ... FOR UPDATE) in the pgwire server.

   Covers SKIP LOCKED, NOWAIT, blocking (acquire-or-error), and lock release
   on COMMIT/ROLLBACK. Each handler acts as an independent pgwire session —
   the server-wide lock registry arbitrates between them."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def lock-test-schema
  [{:db/ident       :t/id
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity}
   {:db/ident       :t/name
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}])

(def lock-test-data
  [{:t/id 1 :t/name "one"}
   {:t/id 2 :t/name "two"}
   {:t/id 3 :t/name "three"}])

(def ^:dynamic *conn* nil)

(defn lock-test-fixture [f]
  ;; The row-lock registry is server-wide (defonce) so entries from previous
  ;; tests can persist across fixture boundaries. Reset before each test.
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          _ (d/transact conn lock-test-schema)
          _ (d/transact conn lock-test-data)]
      (try
        (binding [*conn* conn] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each lock-test-fixture)

(defn- mk-session []
  (pg/make-query-handler *conn*))

(defn- ex [handler sql]
  (let [^PgWireServer$QueryResult r (.execute handler sql)]
    {:err (.error r)
     :rows (vec (map vec (.rows r)))
     :tag (.commandTag r)}))

(defn- ids [result]
  (set (map (fn [row] (Long/parseLong (first row))) (:rows result))))

;; ============================================================================
;; SKIP LOCKED
;; ============================================================================

(deftest skip-locked-single-session
  (testing "Single session: FOR UPDATE SKIP LOCKED returns all matching rows"
    (let [s (mk-session)]
      (ex s "BEGIN")
      (let [r (ex s "SELECT id FROM t ORDER BY id FOR UPDATE SKIP LOCKED")]
        (is (nil? (:err r)))
        (is (= #{1 2 3} (ids r))))
      (ex s "COMMIT"))))

(deftest skip-locked-cross-session
  (testing "SKIP LOCKED hides rows locked by another session"
    (let [s1 (mk-session)
          s2 (mk-session)]
      (ex s1 "BEGIN")
      (ex s1 "SELECT id FROM t WHERE id=1 FOR UPDATE SKIP LOCKED")
      (ex s2 "BEGIN")
      (let [r (ex s2 "SELECT id FROM t ORDER BY id FOR UPDATE SKIP LOCKED")]
        (is (nil? (:err r)))
        (is (= #{2 3} (ids r)) "s2 sees only rows not held by s1"))
      (ex s2 "ROLLBACK")
      (ex s1 "COMMIT"))))

(deftest skip-locked-released-on-commit
  (testing "Locks held by s1 are released on COMMIT, visible to s2"
    (let [s1 (mk-session)
          s2 (mk-session)]
      (ex s1 "BEGIN")
      (ex s1 "SELECT id FROM t WHERE id=1 FOR UPDATE SKIP LOCKED")
      (ex s1 "COMMIT")
      (ex s2 "BEGIN")
      (let [r (ex s2 "SELECT id FROM t ORDER BY id FOR UPDATE SKIP LOCKED")]
        (is (= #{1 2 3} (ids r)) "s2 sees all rows after s1 committed"))
      (ex s2 "ROLLBACK"))))

(deftest skip-locked-released-on-rollback
  (testing "Locks held by s1 are released on ROLLBACK, visible to s2"
    (let [s1 (mk-session)
          s2 (mk-session)]
      (ex s1 "BEGIN")
      (ex s1 "SELECT id FROM t WHERE id=1 FOR UPDATE SKIP LOCKED")
      (ex s1 "ROLLBACK")
      (ex s2 "BEGIN")
      (let [r (ex s2 "SELECT id FROM t ORDER BY id FOR UPDATE SKIP LOCKED")]
        (is (= #{1 2 3} (ids r))))
      (ex s2 "ROLLBACK"))))

(deftest skip-locked-same-session-reacquire
  (testing "Same session re-locking a row it already holds is a no-op"
    (let [s (mk-session)]
      (ex s "BEGIN")
      (let [r1 (ex s "SELECT id FROM t WHERE id=1 FOR UPDATE SKIP LOCKED")
            r2 (ex s "SELECT id FROM t WHERE id=1 FOR UPDATE SKIP LOCKED")]
        (is (= #{1} (ids r1)))
        (is (= #{1} (ids r2)) "re-acquire in same session still returns the row"))
      (ex s "COMMIT"))))

;; ============================================================================
;; NOWAIT
;; ============================================================================

(deftest nowait-no-conflict
  (testing "NOWAIT succeeds when no rows are locked by others"
    (let [s (mk-session)]
      (ex s "BEGIN")
      (let [r (ex s "SELECT id FROM t ORDER BY id FOR UPDATE NOWAIT")]
        (is (nil? (:err r)))
        (is (= #{1 2 3} (ids r))))
      (ex s "COMMIT"))))

(deftest nowait-errors-on-conflict
  (testing "NOWAIT raises error when any row held by another session"
    (let [s1 (mk-session)
          s2 (mk-session)]
      (ex s1 "BEGIN")
      (ex s1 "SELECT id FROM t WHERE id=1 FOR UPDATE NOWAIT")
      (ex s2 "BEGIN")
      (let [r (ex s2 "SELECT id FROM t ORDER BY id FOR UPDATE NOWAIT")]
        (is (some? (:err r)) "s2 should error because s1 holds row 1"))
      (ex s2 "ROLLBACK")
      (ex s1 "COMMIT"))))

(deftest nowait-succeeds-on-disjoint
  (testing "NOWAIT succeeds when holding different rows"
    (let [s1 (mk-session)
          s2 (mk-session)]
      (ex s1 "BEGIN")
      (ex s1 "SELECT id FROM t WHERE id=1 FOR UPDATE NOWAIT")
      (ex s2 "BEGIN")
      (let [r (ex s2 "SELECT id FROM t WHERE id=2 FOR UPDATE NOWAIT")]
        (is (nil? (:err r)) "s2 should succeed on a different row")
        (is (= #{2} (ids r))))
      (ex s2 "COMMIT")
      (ex s1 "COMMIT"))))

;; ============================================================================
;; FOR NO KEY UPDATE — treated same as FOR UPDATE in A2
;; ============================================================================

(deftest no-key-update-skip-locked
  (testing "FOR NO KEY UPDATE SKIP LOCKED behaves like FOR UPDATE SKIP LOCKED"
    (let [s1 (mk-session)
          s2 (mk-session)]
      (ex s1 "BEGIN")
      (ex s1 "SELECT id FROM t WHERE id=1 FOR NO KEY UPDATE SKIP LOCKED")
      (ex s2 "BEGIN")
      (let [r (ex s2 "SELECT id FROM t ORDER BY id FOR NO KEY UPDATE SKIP LOCKED")]
        (is (= #{2 3} (ids r))))
      (ex s2 "ROLLBACK")
      (ex s1 "COMMIT"))))

;; ============================================================================
;; Three-way interleavings
;; ============================================================================

(deftest three-sessions-disjoint
  (testing "Three sessions each lock a different row"
    (let [s1 (mk-session) s2 (mk-session) s3 (mk-session)]
      (ex s1 "BEGIN")
      (ex s2 "BEGIN")
      (ex s3 "BEGIN")
      (is (= #{1} (ids (ex s1 "SELECT id FROM t WHERE id=1 FOR UPDATE SKIP LOCKED"))))
      (is (= #{2} (ids (ex s2 "SELECT id FROM t WHERE id=2 FOR UPDATE SKIP LOCKED"))))
      (is (= #{3} (ids (ex s3 "SELECT id FROM t WHERE id=3 FOR UPDATE SKIP LOCKED"))))
      ;; Fourth attempt from any session should see nothing locked
      (is (= #{} (ids (ex s1 "SELECT id FROM t WHERE id IN (2,3) FOR UPDATE SKIP LOCKED"))))
      (ex s1 "COMMIT") (ex s2 "COMMIT") (ex s3 "COMMIT"))))

;; ============================================================================
;; Queries without FOR UPDATE don't touch the registry
;; ============================================================================

(deftest plain-select-does-not-lock
  (testing "SELECT without FOR UPDATE leaves the registry untouched"
    (let [s1 (mk-session)
          s2 (mk-session)]
      (ex s1 "BEGIN")
      (ex s1 "SELECT id FROM t WHERE id=1")   ;; no FOR UPDATE
      (ex s2 "BEGIN")
      (let [r (ex s2 "SELECT id FROM t WHERE id=1 FOR UPDATE NOWAIT")]
        (is (nil? (:err r)) "s2 should not see a lock")
        (is (= #{1} (ids r))))
      (ex s2 "COMMIT")
      (ex s1 "COMMIT"))))
