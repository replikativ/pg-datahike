(ns datahike.test.pg-constraints-test
  "Integration tests for NOT NULL / DEFAULT / CHECK / FOREIGN KEY enforcement
   and CT6 rejection of GRANT / REVOKE / RLS. Each test drives the
   in-process query handler (same path the wire server uses) and asserts
   on the PG SQLSTATE emitted when a constraint is violated:

     23502  NOT NULL
     23503  foreign key
     23514  CHECK
     0A000  feature_not_supported (GRANT/REVOKE/policies)"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *h* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*conn* conn *h* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute ^datahike.pg.PgWireServer$QueryHandler *h* sql))

(defn- err [^PgWireServer$QueryResult r] (.error r))

(defn- ok? [^PgWireServer$QueryResult r] (nil? (.error r)))

(defn- err-contains?
  "A constraint-violation error message will contain the PG SQLSTATE in
   its structured ex-data, but the user-facing string only has the
   message. Match on the substrings we care about."
  [r needle]
  (and (some? (err r))
       (str/includes? (err r) needle)))

(defn- rows [^PgWireServer$QueryResult r]
  (when (ok? r) (vec (map vec (.rows r)))))

;; ============================================================================
;; NOT NULL (23502)
;; ============================================================================

(deftest not-null-insert
  (is (ok? (run "CREATE TABLE nn (id INT PRIMARY KEY, name TEXT NOT NULL)")))
  (testing "INSERT with null value rejected"
    (is (ok? (run "INSERT INTO nn VALUES (1, 'a')")))
    (is (err-contains? (run "INSERT INTO nn (id) VALUES (2)") "not-null"))
    (is (err-contains? (run "INSERT INTO nn VALUES (3, NULL)") "not-null")))
  (testing "UPDATE SET col = NULL rejected"
    (is (err-contains? (run "UPDATE nn SET name = NULL WHERE id = 1") "not-null"))))

;; ============================================================================
;; DEFAULT (literal and function)
;; ============================================================================

(deftest default-literal
  (is (ok? (run "CREATE TABLE dt (id INT PRIMARY KEY, tag TEXT DEFAULT 'unset')")))
  (is (ok? (run "INSERT INTO dt (id) VALUES (1)")))
  (is (= [["1" "unset"]] (rows (run "SELECT id, tag FROM dt WHERE id = 1")))))

(deftest default-function-now
  (is (ok? (run "CREATE TABLE dtf (id INT PRIMARY KEY, created TIMESTAMP DEFAULT NOW())")))
  (is (ok? (run "INSERT INTO dtf (id) VALUES (1)")))
  (let [r (run "SELECT created FROM dtf WHERE id = 1")]
    (is (ok? r))
    (is (some? (first (first (rows r)))))))

;; ============================================================================
;; CHECK constraints (23514)
;; ============================================================================

(deftest check-column-level
  (is (ok? (run "CREATE TABLE ck (id INT PRIMARY KEY, qty INT CHECK (qty > 0))")))
  (is (ok? (run "INSERT INTO ck VALUES (1, 5)")))
  (is (err-contains? (run "INSERT INTO ck VALUES (2, 0)") "check"))
  (is (err-contains? (run "INSERT INTO ck VALUES (3, -1)") "check"))
  (testing "UPDATE that violates CHECK is rejected"
    (is (err-contains? (run "UPDATE ck SET qty = -1 WHERE id = 1") "check"))))

(deftest check-null-is-unknown
  (testing "CHECK treats NULL comparisons as UNKNOWN → allowed (PG 3VL)"
    (is (ok? (run "CREATE TABLE ckn (id INT PRIMARY KEY, qty INT CHECK (qty > 0))")))
    (is (ok? (run "INSERT INTO ckn (id) VALUES (1)")))))

;; ============================================================================
;; FOREIGN KEY — child-side (23503 on orphan INSERT)
;; ============================================================================

(deftest fk-child-side
  (is (ok? (run "CREATE TABLE p1 (id INT PRIMARY KEY, n TEXT)")))
  (is (ok? (run "CREATE TABLE c1 (id INT PRIMARY KEY, pid INT, FOREIGN KEY (pid) REFERENCES p1 (id))")))
  (is (ok? (run "INSERT INTO p1 VALUES (1,'a'),(2,'b')")))
  (is (ok? (run "INSERT INTO c1 VALUES (10, 1)")))
  (is (err-contains? (run "INSERT INTO c1 VALUES (11, 999)") "foreign key"))
  (testing "MATCH SIMPLE — null child col allowed"
    (is (ok? (run "INSERT INTO c1 (id) VALUES (12)")))))

;; ============================================================================
;; FOREIGN KEY — parent-side RESTRICT (23503 on DELETE and on key UPDATE)
;; ============================================================================

(deftest fk-parent-restrict-delete
  (is (ok? (run "CREATE TABLE p2 (id INT PRIMARY KEY)")))
  (is (ok? (run "CREATE TABLE c2 (id INT PRIMARY KEY, pid INT, FOREIGN KEY (pid) REFERENCES p2 (id))")))
  (is (ok? (run "INSERT INTO p2 VALUES (1),(2),(3)")))
  (is (ok? (run "INSERT INTO c2 VALUES (10, 1)")))
  (testing "delete unreferenced row"
    (is (ok? (run "DELETE FROM p2 WHERE id = 3"))))
  (testing "delete referenced row fails with 23503"
    (is (err-contains? (run "DELETE FROM p2 WHERE id = 1") "foreign key"))))

(deftest fk-parent-restrict-update
  (is (ok? (run "CREATE TABLE p3 (id INT PRIMARY KEY, name TEXT)")))
  (is (ok? (run "CREATE TABLE c3 (id INT PRIMARY KEY, pid INT, FOREIGN KEY (pid) REFERENCES p3 (id))")))
  (is (ok? (run "INSERT INTO p3 VALUES (1, 'a'),(2, 'b')")))
  (is (ok? (run "INSERT INTO c3 VALUES (10, 1)")))
  (testing "non-key column UPDATE on referenced row ok"
    (is (ok? (run "UPDATE p3 SET name = 'aa' WHERE id = 1"))))
  (testing "key UPDATE on referenced row fails 23503"
    (is (err-contains? (run "UPDATE p3 SET id = 99 WHERE id = 1") "foreign key")))
  (testing "key UPDATE on unreferenced row ok"
    (is (ok? (run "UPDATE p3 SET id = 20 WHERE id = 2")))))

(deftest fk-cascade-on-delete
  (testing "ON DELETE CASCADE — inline form is lifted to a table-level
            FK by the rewrite layer; DELETE on the parent retracts the
            child rows in the same transaction."
    (is (ok? (run "CREATE TABLE pcas (id INT PRIMARY KEY)")))
    (is (ok? (run "CREATE TABLE ccas (id INT PRIMARY KEY, pid INT REFERENCES pcas(id) ON DELETE CASCADE)")))
    (is (ok? (run "INSERT INTO pcas VALUES (1),(2),(3)")))
    (is (ok? (run "INSERT INTO ccas VALUES (10, 1),(11, 1),(12, 2)")))
    (testing "DELETE parent → cascade-delete dependent children"
      (is (ok? (run "DELETE FROM pcas WHERE id = 1")))
      ;; Children of pid=1 should be gone; pid=2's child remains.
      ;; COUNT(*) returns rows as strings via the wire format here.
      (is (= "1" (-> (rows (run "SELECT COUNT(*) FROM ccas")) ffirst str))))
    (testing "DELETE unreferenced parent unaffected"
      (is (ok? (run "DELETE FROM pcas WHERE id = 3"))))))

(deftest fk-set-null-rejected-at-ddl
  (testing "ON DELETE SET NULL — not yet implemented; DDL rejected"
    (is (err-contains?
         (run "CREATE TABLE csn (id INT PRIMARY KEY, pid INT REFERENCES p2(id) ON DELETE SET NULL)")
         "not supported"))))

;; ============================================================================
;; CT6 — GRANT / REVOKE / RLS rejected with 0A000
;; ============================================================================

(deftest reject-grant-revoke
  (is (err-contains? (run "GRANT SELECT ON t TO bob") "not supported"))
  (is (err-contains? (run "REVOKE ALL ON t FROM bob") "not supported")))

(deftest reject-row-level-security
  (is (err-contains? (run "ALTER TABLE t ENABLE ROW LEVEL SECURITY") "not supported"))
  (is (err-contains? (run "alter table t disable row level security") "not supported"))
  (is (err-contains? (run "CREATE POLICY p ON t USING (true)") "not supported"))
  (is (err-contains? (run "DROP POLICY p ON t") "not supported")))

(deftest reject-extension
  (is (err-contains? (run "CREATE EXTENSION pg_trgm") "not supported"))
  (is (err-contains? (run "DROP EXTENSION pg_trgm") "not supported")))

;; ============================================================================
;; Silent-accept config — :compat and :silently-accept
;; ============================================================================

(defn- fresh-handler [opts]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [c (d/connect cfg)
          h (pg/make-query-handler c opts)]
      [h c cfg])))

(defn- close-handler [[_h c cfg]]
  (d/release c)
  (d/delete-database cfg))

(defn- exec
  "Run SQL against a raw handler (fixture-free) and return {:err :tag}."
  [h sql]
  (let [r (.execute ^datahike.pg.PgWireServer$QueryHandler h sql)]
    {:err (.error r) :tag (.commandTag r)}))

(deftest compat-permissive-accepts-grant-revoke-policy-rls
  (let [[h & _ :as bundle] (fresh-handler {:compat :permissive})]
    (try
      (is (= {:err nil :tag "GRANT"}                           (exec h "GRANT SELECT ON t TO bob")))
      (is (= {:err nil :tag "REVOKE"}                          (exec h "REVOKE ALL ON t FROM bob")))
      (is (= {:err nil :tag "CREATE POLICY"}                   (exec h "CREATE POLICY p ON t USING (true)")))
      (is (= {:err nil :tag "ALTER TABLE ROW LEVEL SECURITY"}  (exec h "ALTER TABLE t ENABLE ROW LEVEL SECURITY")))
      (is (= {:err nil :tag "CREATE EXTENSION"}                (exec h "CREATE EXTENSION pg_trgm")))
      (finally (close-handler bundle)))))

(deftest silently-accept-per-feature
  (testing ":silently-accept #{:grant} accepts GRANT but still rejects RLS"
    (let [[h & _ :as bundle] (fresh-handler {:silently-accept #{:grant}})]
      (try
        (is (= "GRANT" (:tag (exec h "GRANT SELECT ON t TO bob"))))
        (is (str/includes? (:err (exec h "ALTER TABLE t ENABLE ROW LEVEL SECURITY"))
                           "not supported"))
        (finally (close-handler bundle))))))

(deftest default-is-strict
  (testing "no opts ⇒ :strict ⇒ everything rejects"
    (let [[h & _ :as bundle] (fresh-handler nil)]
      (try
        (is (str/includes? (:err (exec h "GRANT SELECT ON t TO bob")) "not supported"))
        (is (str/includes? (:err (exec h "CREATE EXTENSION pg_trgm")) "not supported"))
        (finally (close-handler bundle))))))

;; ============================================================================
;; SAVEPOINT — error paths (25P01 outside-tx, 3B001 nonexistent)
;; ============================================================================

(deftest savepoint-outside-tx-raises-25p01
  (is (str/includes? (:err (exec *h* "SAVEPOINT stray"))
                     "transaction"))
  (is (str/includes? (:err (exec *h* "ROLLBACK TO SAVEPOINT stray"))
                     "transaction"))
  (is (str/includes? (:err (exec *h* "RELEASE SAVEPOINT stray"))
                     "transaction")))

(deftest savepoint-nonexistent-inside-tx-raises-3b001
  (exec *h* "BEGIN")
  (is (str/includes? (:err (exec *h* "ROLLBACK TO SAVEPOINT nonexistent"))
                     "does not exist"))
  ;; After the failure the tx is aborted; next command must be a rollback.
  (exec *h* "ROLLBACK")
  (exec *h* "BEGIN")
  (is (str/includes? (:err (exec *h* "RELEASE SAVEPOINT nonexistent"))
                     "does not exist"))
  (exec *h* "ROLLBACK"))

(deftest savepoint-rollback-to-actually-rolls-back
  (run "CREATE TABLE svt (id INT PRIMARY KEY)")
  (exec *h* "BEGIN")
  (exec *h* "INSERT INTO svt VALUES (1)")
  (exec *h* "SAVEPOINT s1")
  (exec *h* "INSERT INTO svt VALUES (2)")
  (is (= [["2"]] (rows (run "SELECT count(*) FROM svt"))))
  (exec *h* "ROLLBACK TO SAVEPOINT s1")
  (is (= [["1"]] (rows (run "SELECT count(*) FROM svt"))))
  (exec *h* "COMMIT"))

;; ============================================================================
;; Advisory locks
;; ============================================================================

(defn- try-lock-result
  "Extract the boolean string ('t' / 'f') from a pg_try_advisory_lock result."
  [r]
  (first (first (rows r))))

(deftest advisory-lock-basic
  (pg/reset-advisory-locks!)
  (let [[a _ca-cfg :as ab] (fresh-handler nil)
        [b _cb-cfg :as bb] (fresh-handler nil)]
    (try
      (is (= "t" (try-lock-result (.execute a "SELECT pg_try_advisory_lock(7)"))))
      (is (= "f" (try-lock-result (.execute b "SELECT pg_try_advisory_lock(7)"))))
      (testing "same-session re-lock is refcounted"
        (is (= "t" (try-lock-result (.execute a "SELECT pg_try_advisory_lock(7)"))))
        (is (= "t" (first (first (rows (.execute a "SELECT pg_advisory_unlock(7)"))))))
        ;; still held (count dropped to 1)
        (is (= "f" (try-lock-result (.execute b "SELECT pg_try_advisory_lock(7)"))))
        (.execute a "SELECT pg_advisory_unlock(7)")
        (is (= "t" (try-lock-result (.execute b "SELECT pg_try_advisory_lock(7)")))))
      (.execute b "SELECT pg_advisory_unlock(7)")
      (finally (close-handler ab) (close-handler bb)))))

(deftest advisory-xact-lock-auto-releases-on-commit
  (pg/reset-advisory-locks!)
  (let [[a _ :as ab] (fresh-handler nil)
        [b _ :as bb] (fresh-handler nil)]
    (try
      (.execute a "BEGIN")
      (.execute a "SELECT pg_advisory_xact_lock(99)")
      (is (= "f" (try-lock-result (.execute b "SELECT pg_try_advisory_lock(99)"))))
      (.execute a "COMMIT")
      (is (= "t" (try-lock-result (.execute b "SELECT pg_try_advisory_lock(99)"))))
      (.execute b "SELECT pg_advisory_unlock(99)")
      (finally (close-handler ab) (close-handler bb)))))

(deftest advisory-xact-lock-requires-tx
  (pg/reset-advisory-locks!)
  (is (str/includes? (:err (exec *h* "SELECT pg_advisory_xact_lock(1)"))
                     "transaction")))

(deftest advisory-locks-release-on-close
  (pg/reset-advisory-locks!)
  (let [[a _ :as ab] (fresh-handler nil)
        [b _ :as bb] (fresh-handler nil)]
    (try
      (is (= "t" (try-lock-result (.execute a "SELECT pg_try_advisory_lock(123)"))))
      (is (= "f" (try-lock-result (.execute b "SELECT pg_try_advisory_lock(123)"))))
      (.close a)
      (is (= "t" (try-lock-result (.execute b "SELECT pg_try_advisory_lock(123)"))))
      (.execute b "SELECT pg_advisory_unlock(123)")
      (finally (close-handler bb)))))

;; ============================================================================
;; Session introspection — stable plausible values, not literal string echoes
;; ============================================================================

(deftest session-introspection-returns-typed-values
  (let [pid  (first (first (rows (run "SELECT pg_backend_pid()"))))
        txid (first (first (rows (run "SELECT txid_current()"))))
        sleep (first (first (rows (run "SELECT pg_sleep(0)"))))]
    (is (some? pid))
    ;; pid must parse as an integer (not the literal function-name string)
    (is (integer? (try (Long/parseLong pid) (catch Throwable _ nil)))
        (str "pg_backend_pid should return an integer, got: " pid))
    (is (integer? (try (Long/parseLong txid) (catch Throwable _ nil)))
        (str "txid_current should return an integer, got: " txid))
    ;; pg_sleep returns void (empty string)
    (is (= "" sleep))))

;; ============================================================================
;; Maintenance + schema no-ops (VACUUM / REINDEX / CREATE SCHEMA / …)
;; ============================================================================

(deftest maintenance-noops
  (doseq [[sql expected-tag]
          [["VACUUM"                            "VACUUM"]
           ["VACUUM ANALYZE"                    "VACUUM"]
           ["REINDEX TABLE foo"                 "REINDEX"]
           ["CLUSTER"                           "CLUSTER"]
           ["CREATE SCHEMA foo"                 "CREATE SCHEMA"]
           ["ALTER SCHEMA foo OWNER TO x"       "ALTER SCHEMA"]
           ["DROP SCHEMA foo"                   "DROP SCHEMA"]]]
    (testing sql
      (let [r (exec *h* sql)]
        (is (nil? (:err r)))
        (is (= expected-tag (:tag r)))))))

;; ============================================================================
;; pg_extension exists and is empty
;; ============================================================================

(deftest pg-extension-empty-but-queryable
  (let [r (run "SELECT count(*) FROM pg_extension")]
    (is (= [["0"]] (rows r))))
  (let [r (run "SELECT extname FROM pg_extension WHERE extname = 'pg_trgm'")]
    (is (= [] (rows r)))))

;; ============================================================================
;; COPY — clean error when the target table doesn't exist
;; (Tier 2 of pgdump-import added wire-protocol COPY-IN support; this
;; harness's ad-hoc `exec` helper doesn't drive the full COPY-IN
;; sub-protocol — so we just verify that pre-COPY-IN errors surface
;; cleanly. See pg-copy-parse-test for the parser, pg-copy-text-format
;; / pg-copy-csv-format for the decoders, and the pgdump-roundtrip
;; CI harness for the full wire-level round-trip.)
;; ============================================================================

(deftest copy-against-missing-table-errors-cleanly
  (let [r (exec *h* "COPY t FROM STDIN")]
    ;; Either fails the wire test harness (which doesn't speak COPY-IN
    ;; and routes the CopyInResponse path as an error), or returns the
    ;; pre-COPY-IN "no columns" error from exec-copy-from-stdin.
    ;; Both are acceptable here — what we assert is that the harness
    ;; doesn't hang or crash.
    (is (or (some? (:err r)) (some? (:rows r))))))
