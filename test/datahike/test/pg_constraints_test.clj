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
            [datahike.pg.constraints.row :as row-constraints]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *h* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
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

(deftest alter-column-nullability
  (let [attnotnull (str "SELECT attnotnull FROM pg_attribute "
                        "WHERE attrelid = 'nn'::regclass AND attname = 'Mixed Case'")
        constraint (str "SELECT conname FROM pg_constraint "
                        "WHERE conrelid = 'nn'::regclass AND contype = 'n' "
                        "AND conname <> 'nn_id_not_null'")
        set-not-null "ALTER TABLE nn ALTER COLUMN \"Mixed Case\" SET NOT NULL"
        drop-not-null "ALTER TABLE nn ALTER \"Mixed Case\" DROP NOT NULL"]
    (is (ok? (run "CREATE TABLE nn (id INT PRIMARY KEY, \"Mixed Case\" TEXT, b TEXT)")))
    (is (ok? (run "INSERT INTO nn (id) VALUES (1)")))
    (is (= "23502" (.sqlstate ^PgWireServer$QueryResult (run set-not-null))))
    (is (= [["f"]] (rows (run attnotnull))))
    (is (= [] (rows (run constraint))))
    (is (ok? (run "UPDATE nn SET \"Mixed Case\" = 'filled' WHERE id = 1")))
    (is (ok? (run "BEGIN")))
    (is (ok? (run set-not-null)))
    (is (= [["t"]] (rows (run attnotnull))))
    (is (ok? (run "ROLLBACK")))
    (is (= [["f"]] (rows (run attnotnull))))
    (is (= "23502" (.sqlstate ^PgWireServer$QueryResult
                    (run (str set-not-null ", ALTER COLUMN b SET NOT NULL")))))
    (is (= [["f"]] (rows (run attnotnull))))
    (is (ok? (run set-not-null)))
    (is (ok? (run set-not-null)))
    (is (= [["t"]] (rows (run attnotnull))))
    (is (= 1 (count (rows (run constraint)))))
    (is (= "23502" (.sqlstate ^PgWireServer$QueryResult (run "INSERT INTO nn (id) VALUES (2)"))))
    (is (= "23502" (.sqlstate ^PgWireServer$QueryResult
                    (run "UPDATE nn SET \"Mixed Case\" = NULL WHERE id = 1"))))
    (is (= "23502" (.sqlstate ^PgWireServer$QueryResult
                    (run (str drop-not-null ", ALTER COLUMN b SET NOT NULL")))))
    (is (= [["t"]] (rows (run attnotnull))))
    (is (ok? (run drop-not-null)))
    (is (ok? (run drop-not-null)))
    (is (= [["f"]] (rows (run attnotnull))))
    (is (= [] (rows (run constraint))))
    (is (ok? (run "INSERT INTO nn (id) VALUES (2)")))
    (is (= "42703" (.sqlstate ^PgWireServer$QueryResult
                    (run "ALTER TABLE nn ALTER missing SET NOT NULL"))))
    (is (= "42P16" (.sqlstate ^PgWireServer$QueryResult
                    (run "ALTER TABLE nn ALTER id DROP NOT NULL"))))
    (is (ok? (run "CREATE VIEW nn_view AS SELECT id FROM nn")))
    (is (= "42809" (.sqlstate ^PgWireServer$QueryResult
                    (run "ALTER TABLE nn_view ALTER id SET NOT NULL"))))
    (is (ok? (run "CREATE TABLE nn_key (a INT, b INT GENERATED ALWAYS AS IDENTITY, c INT, PRIMARY KEY (a,c))")))
    (is (= "42P16" (.sqlstate ^PgWireServer$QueryResult
                    (run "ALTER TABLE nn_key ALTER a DROP NOT NULL"))))
    (is (= "42601" (.sqlstate ^PgWireServer$QueryResult
                    (run "ALTER TABLE nn_key ALTER b DROP NOT NULL"))))))

(deftest primary-key-is-implicitly-not-null
  (is (ok? (run "CREATE TABLE pkn (id INT PRIMARY KEY, name TEXT)")))
  (let [r (run "INSERT INTO pkn (name) VALUES ('missing')")]
    (is (err-contains? r "not-null"))
    (is (= "23502" (.sqlstate ^PgWireServer$QueryResult r))))
  (is (= [] (rows (run "SELECT * FROM pkn")))))

(deftest composite-primary-key-index-is-not-a-sql-column
  (is (ok? (run "CREATE TABLE cpk (a INT, b TEXT, PRIMARY KEY (a,b))")))
  (is (ok? (run "INSERT INTO cpk VALUES (1,'one')")))
  (is (= [["1" "one"]] (rows (run "SELECT * FROM cpk"))))
  (let [r (run "INSERT INTO cpk VALUES (NULL,'two')")]
    (is (= "23502" (.sqlstate ^PgWireServer$QueryResult r)))))

;; ============================================================================
;; DEFAULT (literal and function)
;; ============================================================================

(deftest default-literal
  (is (ok? (run "CREATE TABLE dt (id INT PRIMARY KEY, tag TEXT DEFAULT 'unset')")))
  (is (ok? (run "INSERT INTO dt (id) VALUES (1)")))
  (is (= [["1" "unset"]] (rows (run "SELECT id, tag FROM dt WHERE id = 1")))))

(deftest explicit-null-suppresses-a-default
  (is (ok? (run "CREATE TABLE dtn (id INT, tag TEXT DEFAULT 'unset')")))
  (is (ok? (run "INSERT INTO dtn VALUES (1, NULL)")))
  (is (ok? (run "INSERT INTO dtn (id) VALUES (2)")))
  (is (= [["1" nil] ["2" "unset"]]
         (rows (run "SELECT * FROM dtn ORDER BY id")))))

(deftest default-function-now
  (is (ok? (run "CREATE TABLE dtf (id INT PRIMARY KEY, created TIMESTAMP DEFAULT NOW())")))
  (is (ok? (run "INSERT INTO dtf (id) VALUES (1)")))
  (let [r (run "SELECT created FROM dtf WHERE id = 1")]
    (is (ok? r))
    (is (some? (first (first (rows r)))))))

(deftest temporal-default-is-assignment-coerced-to-text
  (is (ok? (run "CREATE TABLE dtft (id INT PRIMARY KEY, created TEXT DEFAULT NOW())")))
  (is (ok? (run "INSERT INTO dtft (id) VALUES (1)")))
  (let [created (ffirst (rows (run "SELECT created FROM dtft WHERE id = 1")))]
    (is (string? created))
    (is (re-find #"^\d{4}-\d{2}-\d{2} " created))))

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

(deftest fk-parent-must-be-a-row-of-the-referenced-table
  (is (ok? (run "CREATE TABLE marker_parent (id INT PRIMARY KEY)")))
  (is (ok? (run (str "CREATE TABLE marker_child (id INT PRIMARY KEY, pid INT, "
                     "FOREIGN KEY (pid) REFERENCES marker_parent (id))"))))
  ;; Datahike permits a native entity to carry a table's namespaced column
  ;; attribute. It is not a SQL row until it also carries the table marker.
  (d/transact *conn* [{:marker_parent/id 42}])
  (let [r (run "INSERT INTO marker_child VALUES (1, 42)")]
    (is (= "23503" (.sqlstate ^PgWireServer$QueryResult r)))
    (is (err-contains? r "foreign key"))))

(deftest self-referencing-fk-sees-pending-parent-row
  (is (ok? (run (str "CREATE TABLE self_parent (id INT PRIMARY KEY, parent_id INT, "
                     "FOREIGN KEY (parent_id) REFERENCES self_parent (id))"))))
  (is (ok? (run "INSERT INTO self_parent VALUES (1, NULL), (2, 1)")))
  (is (= [["1" nil] ["2" "1"]]
         (rows (run "SELECT * FROM self_parent ORDER BY id")))))

(deftest postgres-rejects-invalid-foreign-key-definitions
  (is (ok? (run "CREATE TABLE pktable (ptest1 int PRIMARY KEY, someoid oid)")))
  (doseq [[name columns state]
          [["fktable_fail1" "ftest1 int, CONSTRAINT fkfail1 FOREIGN KEY (ftest2) REFERENCES pktable" "42703"]
           ["fktable_fail2" "ftest1 int, CONSTRAINT fkfail1 FOREIGN KEY (ftest1) REFERENCES pktable(ptest2)" "42703"]
           ["fktable_fail3" "ftest1 int, CONSTRAINT fkfail1 FOREIGN KEY (tableoid) REFERENCES pktable(someoid)" "0A000"]
           ["fktable_fail4" "ftest1 oid, CONSTRAINT fkfail1 FOREIGN KEY (ftest1) REFERENCES pktable(tableoid)" "0A000"]
           ["fktable_fail5" "ftest1 oid REFERENCES pktable(someoid)" "42830"]
           ["fktable_fail6" "a int, b int, FOREIGN KEY (a,b) REFERENCES pktable(ptest1,ptest1)" "42830"]]]
    (let [sql (str "CREATE TABLE " name " (" columns ")")
          result (run sql)]
      (is (= state (.sqlstate ^PgWireServer$QueryResult result)) (str sql " => " (err result)))
      (is (empty? (rows (run (str "SELECT oid FROM pg_class WHERE relname = '" name "'")))))))
  (is (= [["0"]] (rows (run "SELECT count(*) FROM pg_constraint WHERE contype = 'f'"))))
  (is (ok? (run "DROP TABLE pktable"))))

(deftest fk-resolves-new-self-and-existing-unique-keys
  (is (ok? (run "CREATE TABLE self_ref (id int PRIMARY KEY, parent int REFERENCES self_ref)")))
  (is (ok? (run "INSERT INTO self_ref VALUES (1, NULL), (2, 1)")))
  (is (ok? (run "CREATE TABLE fk_index_parent (a int, b int)")))
  (is (ok? (run "CREATE UNIQUE INDEX fk_index ON fk_index_parent (a, b)")))
  (is (ok? (run "CREATE TABLE fk_index_child (a int, b int, FOREIGN KEY (b, a) REFERENCES fk_index_parent (b, a))")))
  (let [result (run "ALTER TABLE fk_index_child ADD FOREIGN KEY (a) REFERENCES fk_index_parent (c)")]
    (is (= "42703" (.sqlstate ^PgWireServer$QueryResult result))))
  (let [result (run "ALTER TABLE fk_index_child ADD FOREIGN KEY (a) REFERENCES fk_index_parent (a)")]
    (is (= "42830" (.sqlstate ^PgWireServer$QueryResult result))))
  (is (= [["1"]] (rows (run "SELECT count(*) FROM pg_constraint WHERE contype = 'f' AND conrelid = 'fk_index_child'::regclass")))))

(deftest fk-referenced-keys-belong-to-the-referenced-table
  (is (ok? (run "CREATE TABLE fk_ancestor (code int UNIQUE)")))
  (is (ok? (run "CREATE TABLE fk_inherited (extra int) INHERITS (fk_ancestor)")))
  (let [result (run "CREATE TABLE fk_orphan (code int REFERENCES fk_inherited(code))")]
    (is (= "42830" (.sqlstate ^PgWireServer$QueryResult result)) (err result))))

(deftest ordinary-insert-compiles-and-caches-one-row-plan
  (is (ok? (run "CREATE TABLE plan_plain (id INT, note TEXT)")))
  (let [calls (atom 0)
        preparations (atom 0)
        original-columns row-constraints/column-specs
        original-prepare row-constraints/prepare-candidate]
    (with-redefs [row-constraints/column-specs
                  (fn [db table-name]
                    (swap! calls inc)
                    (original-columns db table-name))
                  row-constraints/prepare-candidate
                  (fn [& args]
                    (swap! preparations inc)
                    (apply original-prepare args))]
      (is (ok? (run "INSERT INTO plan_plain VALUES (1, 'a')")))
      (is (ok? (run "INSERT INTO plan_plain VALUES (2, 'b')")))
      (is (= 1 @calls)
          "the cached row plan does not query columns again in the tx-fn or next INSERT")
      (is (zero? @preparations)
          "an unconstrained ordinary INSERT bypasses row preparation entirely"))))

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

(deftest grant-is-accepted-and-revoke-is-not
  ;; Everything is already permitted here -- `has_*_privilege` answers
  ;; `true` unconditionally -- so GRANT permits nothing new and is
  ;; honestly a no-op.
  (is (ok? (run "GRANT SELECT ON t TO bob")))
  ;; REVOKE is different in kind: it claims to TAKE access away, and a
  ;; caller that believes it has been told something false about who
  ;; can read their data. Refusing is loud and safe.
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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
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
        ;; GRANT is accepted under every preset now; CREATE EXTENSION
        ;; still needs :permissive, which is what :strict means here.
        (is (str/includes? (:err (exec h "CREATE EXTENSION pg_trgm")) "not supported"))
        (finally (close-handler bundle))))))

(deftest partitioned-table-strict-refuses-pg-dump-loads-plain-parent
  ;; Stripping PARTITION BY used to create an unpartitioned table that
  ;; accepted rows PostgreSQL rejects for lacking a partition.
  (let [ddl "CREATE TABLE pay (id int, d date) PARTITION BY RANGE (d)"]
    (testing "strict (default): 0A000, nothing created"
      (let [[h & _ :as bundle] (fresh-handler nil)]
        (try
          (let [r (.execute ^datahike.pg.PgWireServer$QueryHandler h ddl)]
            (is (= "0A000" (.sqlstate r)))
            (is (str/includes? (.error r) "PARTITION BY RANGE")))
          (is (some? (:err (exec h "SELECT count(*) FROM pay"))))
          (finally (close-handler bundle)))))
    (testing "per feature: :silently-accept #{:partitioned-table}"
      (let [[h & _ :as bundle] (fresh-handler {:silently-accept #{:partitioned-table}})]
        (try
          (is (= {:err nil :tag "CREATE TABLE"} (exec h ddl)))
          (finally (close-handler bundle)))))
    (testing ":compat :pg-dump loads the parent as a plain, empty table"
      (let [[h & _ :as bundle] (fresh-handler {:compat :pg-dump})]
        (try
          (is (= {:err nil :tag "CREATE TABLE"} (exec h ddl)))
          (is (nil? (:err (exec h "SELECT count(*) FROM pay"))))
          (finally (close-handler bundle)))))))

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
;; pg_extension exposes built-in compatibility extensions
;; ============================================================================

(deftest pg-extension-queryable
  (let [r (run "SELECT count(*) FROM pg_extension")]
    (is (= [["1"]] (rows r))))
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
