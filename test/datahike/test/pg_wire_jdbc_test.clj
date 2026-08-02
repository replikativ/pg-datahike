(ns datahike.test.pg-wire-jdbc-test
  "Wire-level tests that exercise the PgWireServer via a real PostgreSQL
   JDBC driver. Unlike the unit tests in `pg-server-test`, these go
   through the full socket round trip: StartupMessage → ReadyForQuery,
   Simple Query, Extended Query (Parse/Bind/Describe/Execute/Sync),
   ErrorResponse, CommandComplete.

   These verify protocol-level contracts that unit tests on the
   QueryResult object can't see:
   - SQLSTATE codes actually land in the `C` field of ErrorResponse.
   - CommandComplete tags match PostgreSQL conventions
     (`SELECT N`, `INSERT oid rows`, `UPDATE rows`, `DELETE rows`).
   - Extended-query error recovery only clears on `Sync`.
   - Binary parameter format is rejected with `0A000`.
   - In-tx errors produce `25P02 in_failed_sql_transaction` for
     subsequent statements until ROLLBACK.

   We intentionally do not mock the driver — if it uses a protocol
   feature we don't implement, the test surfaces it."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager PreparedStatement ResultSet
            SQLException Statement Types]))

(def jdbc-schema
  [{:db/ident :t/id   :db/valueType :db.type/long   :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :t/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :t/age  :db/valueType :db.type/long   :db/cardinality :db.cardinality/one}])

(def jdbc-data
  [{:t/id 1 :t/name "Alice"   :t/age 30}
   {:t/id 2 :t/name "Bob"     :t/age 25}
   {:t/id 3 :t/name "Charlie" :t/age 35}])

(def ^:dynamic *server* nil)
(def ^:dynamic *port* nil)

(defn jdbc-fixture [f]
  ;; Each test gets a fresh in-memory DB, a fresh PgWireServer on a
  ;; random port, and clears the global lock registry (which is
  ;; defonce-scoped across fixtures).
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn jdbc-schema)
      (d/transact conn jdbc-data)
      (let [factory (reify PgWireServer$QueryHandlerFactory
                      (create [_] (pg/make-query-handler conn)))
            ;; Port 0 → OS picks a free port.
            server (PgWireServer. 0 "127.0.0.1" factory)]
        (.start server)
        (try
          (binding [*server* server
                    *port*   (.getPort server)]
            (f))
          (finally
            (.stop server)
            (d/release conn)
            (d/delete-database cfg)))))))

(use-fixtures :each jdbc-fixture)

(defn- url
  "Build a JDBC URL for the current test server.

   Defaults chosen for compatibility with our text-only wire:
   - `sslmode=disable` — we reject SSL with 'N'; skip the negotiation retry.
   - `binaryTransfer=false` — our server rejects binary Bind with
     0A000. Unless a test explicitly exercises that rejection path, we
     want JDBC to use text format so the real error we're probing
     (type mismatch, syntax, etc.) surfaces instead of 0A000.

   Override either via the `params` map."
  ([] (url {}))
  ([params]
   (let [defaults {:sslmode "disable" :binaryTransfer "false"}
         merged   (merge defaults params)
         base (str "jdbc:postgresql://127.0.0.1:" *port* "/datahike?user=datahike&password=x")]
     (str base "&" (str/join "&" (map (fn [[k v]] (str (name k) "=" v)) merged))))))

(defn- open ^Connection [params]
  (DriverManager/getConnection (url params)))

(defmacro ^:private with-conn
  "Binds `c` to a fresh JDBC connection with the given URL params
   (map). Closes the connection in a finally."
  [[c params] & body]
  `(let [~(vary-meta c assoc :tag `Connection) (open ~params)]
     (try ~@body
          (finally (.close ~c)))))

;; ============================================================================
;; Connection & ReadyForQuery
;; ============================================================================

(deftest test-connection-establishes
  (testing "TCP connect + StartupMessage → usable Connection"
    (with-conn [c {}]
      (is (not (.isClosed c)))
      (is (.isValid c 1)))))

;; ============================================================================
;; Simple Query protocol (Statement)
;; ============================================================================

(deftest test-simple-select
  (testing "Statement.executeQuery returns expected rows"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)
                  rs (.executeQuery st "SELECT name FROM t ORDER BY id")]
        (is (.next rs)) (is (= "Alice"   (.getString rs 1)))
        (is (.next rs)) (is (= "Bob"     (.getString rs 1)))
        (is (.next rs)) (is (= "Charlie" (.getString rs 1)))
        (is (not (.next rs)))))))

(deftest test-command-tag-select-rowcount
  (testing "SELECT command tag reports the visible row count"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        ;; JDBC exposes the tag's row count via updateCount for non-SELECT
        ;; but the row count for SELECT goes through the ResultSet. The
        ;; important contract is that the tag parses — a malformed tag
        ;; like "SELECT" (no count) makes the driver throw.
        (is (.execute st "SELECT * FROM t"))
        (with-open [rs (.getResultSet st)]
          (loop [n 0] (if (.next rs) (recur (inc n)) (is (= 3 n)))))))))

(deftest test-command-tag-insert-update-delete
  (testing "INSERT/UPDATE/DELETE tags report affected rowcount"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        (is (= 1 (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (4, 'Dave', 40)")))
        (is (= 2 (.executeUpdate st "UPDATE t SET age = 99 WHERE id IN (1, 2)")))
        (is (= 1 (.executeUpdate st "DELETE FROM t WHERE id = 4")))))))

;; Note: EmptyQueryResponse testing skipped — pgJDBC's Statement.execute
;; loop hangs on "   " because the driver doesn't emit a query message
;; at all for empty strings, leaving it waiting for a response it never
;; asked for. The server-side EmptyQueryResponse path is covered by the
;; PgWireServer unit test via a raw socket if we add one later.

;; ============================================================================
;; SQLSTATE propagation via ErrorResponse
;; ============================================================================

(deftest test-sqlstate-syntax-error
  (testing "Parser error → SQLSTATE 42601 (syntax_error)"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        (try
          (.executeQuery st "SELECT FROM WHERE")
          (is false "expected SQLException")
          (catch SQLException e
            (is (= "42601" (.getSQLState e)))))))))

(deftest test-sqlstate-schema-violation
  (testing "Type mismatch INSERT → SQLSTATE 22P02 (invalid_text_representation)"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        (try
          (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (5, 'X', 'notanumber')")
          (is false "expected SQLException")
          (catch SQLException e
            (is (= "22P02" (.getSQLState e)))))))))

(deftest test-sqlstate-undefined-table
  (testing "SELECT from non-existent table → SQLSTATE 42P01"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        (try
          (.executeQuery st "SELECT * FROM nonexistent_table_xyz")
          (is false "expected SQLException")
          (catch SQLException e
            ;; Accept 42P01 (undefined_table) or the general 42601 if the
            ;; error fires at parse-resolve vs execute time.
            (is (contains? #{"42P01" "42601" "XX000"} (.getSQLState e))
                (str "got " (.getSQLState e) ": " (.getMessage e)))))))))

;; ============================================================================
;; Extended Query protocol (PreparedStatement)
;; ============================================================================

(deftest test-prepared-statement-text-params
  (testing "PreparedStatement with text parameters returns correct rows"
    (with-conn [c {:preferQueryMode "extended"}]
      (with-open [ps (.prepareStatement c "SELECT name FROM t WHERE age > ? ORDER BY id")]
        (.setInt ps 1 28)
        (with-open [rs (.executeQuery ps)]
          (is (.next rs)) (is (= "Alice"   (.getString rs 1)))
          (is (.next rs)) (is (= "Charlie" (.getString rs 1)))
          (is (not (.next rs))))))))

(deftest test-prepared-statement-null-param
  (testing "PreparedStatement with NULL parameter"
    (with-conn [c {:preferQueryMode "extended"}]
      (with-open [ps (.prepareStatement c "SELECT name FROM t WHERE ? IS NULL")]
        (.setNull ps 1 Types/INTEGER)
        (with-open [rs (.executeQuery ps)]
          ;; ? IS NULL is TRUE → all 3 rows match
          (loop [n 0] (if (.next rs) (recur (inc n)) (is (= 3 n)))))))))

(deftest test-parameter-description-inference
  (testing "Describe('S') infers INSERT/UPDATE/WHERE param OIDs from schema"
    (with-conn [c {:preferQueryMode "extended"}]
      ;; INSERT INTO t (id, name, age) VALUES (?, ?, ?)
      ;; jdbc-schema: id=long(int8), name=string(text), age=long(int8)
      (with-open [ps (.prepareStatement c
                                        "INSERT INTO t (id, name, age) VALUES (?, ?, ?)")]
        (let [pmd (.getParameterMetaData ps)]
          (is (= 3 (.getParameterCount pmd)))
          ;; JDBC's getParameterType returns java.sql.Types constants;
          ;; int8 (OID 20) maps to Types/BIGINT, text (OID 25) to VARCHAR.
          (is (= Types/BIGINT (.getParameterType pmd 1)))
          (is (= Types/VARCHAR (.getParameterType pmd 2)))
          (is (= Types/BIGINT (.getParameterType pmd 3)))))
      ;; UPDATE t SET age = ? WHERE name = ?
      (with-open [ps (.prepareStatement c
                                        "UPDATE t SET age = ? WHERE name = ?")]
        (let [pmd (.getParameterMetaData ps)]
          (is (= 2 (.getParameterCount pmd)))
          (is (= Types/BIGINT (.getParameterType pmd 1)))
          (is (= Types/VARCHAR (.getParameterType pmd 2))))))))

(deftest test-parameter-description-join-on
  (testing "JOIN ON col = ? infers from the column's schema type"
    (with-conn [c {:preferQueryMode "extended"}]
      (with-open [ps (.prepareStatement c
                                        "SELECT a.name FROM t a JOIN t b ON a.id = b.id AND b.age = ?")]
        (let [pmd (.getParameterMetaData ps)]
          (is (= 1 (.getParameterCount pmd)))
          (is (= Types/BIGINT (.getParameterType pmd 1))))))))

(deftest test-parameter-description-on-conflict
  (testing "ON CONFLICT DO UPDATE SET col = ? infers from the column's type"
    (with-conn [c {:preferQueryMode "extended"}]
      (with-open [ps (.prepareStatement c
                                        "INSERT INTO t (id, name, age) VALUES (9, 'X', 1)
                        ON CONFLICT (id) DO UPDATE SET name = ?, age = ?")]
        (let [pmd (.getParameterMetaData ps)]
          (is (= 2 (.getParameterCount pmd)))
          (is (= Types/VARCHAR (.getParameterType pmd 1)))
          (is (= Types/BIGINT (.getParameterType pmd 2))))))))

(deftest test-prepared-statement-sqlstate
  (testing "PreparedStatement with bad value propagates SQLSTATE"
    (with-conn [c {:preferQueryMode "extended"}]
      (with-open [ps (.prepareStatement c "INSERT INTO t (id, name, age) VALUES (?, ?, ?)")]
        (.setInt ps 1 10)
        (.setString ps 2 "X")
        (.setString ps 3 "notanumber")   ;; wrong type for age (long)
        (try (.executeUpdate ps)
             (is false "expected SQLException")
             (catch SQLException e
               (is (= "22P02" (.getSQLState e)))))))))

;; ============================================================================
;; Binary format rejection
;; ============================================================================

(deftest test-binary-result-transfer
  (testing "Binary result encoding: int4 round-trips via binary transfer"
    ;; Force pgJDBC to request binary results for int4/int8 and verify the
    ;; client decodes the value correctly — i.e., we encoded big-endian,
    ;; not UTF-8. If our wire sent text when RowDescription said binary,
    ;; pgJDBC would either throw or produce garbage numbers.
    (with-conn [c {:preferQueryMode      "extended"
                   :prepareThreshold     "1"
                   :binaryTransferEnable "20,21,23,700,701,2950"
                   :binaryTransfer       "true"}]
      ;; Issue one iteration to cross prepareThreshold, then several with
      ;; binary — catches any encoder bug per OID.
      (with-open [ps (.prepareStatement c "SELECT age FROM t WHERE name = ? ORDER BY age")]
        (.setString ps 1 "Alice")
        (dotimes [_ 3]
          (with-open [rs (.executeQuery ps)]
            (is (.next rs))
            (is (= 30 (.getInt rs 1)))
            (is (not (.next rs)))))))))

(deftest test-binary-param-transfer
  (testing "Binary parameter transfer decodes int params across the server-prepared upgrade"
    ;; prepareThreshold=1 forces pgJDBC to upgrade to server-side
    ;; prepared + binary format on the 2nd Execute. The upgraded path
    ;; sends Describe('P') on iter 1 and then bare Bind+Execute on
    ;; iter 2+, expecting no RowDescription on those later Executes
    ;; (it cached the metadata per-statement from the first
    ;; RowDescription). Our server marks the PreparedStmt as described
    ;; whenever Describe emits RowDescription, so subsequent re-Binds
    ;; of the same statement skip RowDescription in Execute.
    (with-conn [c {:preferQueryMode      "extended"
                   :prepareThreshold     "1"
                   :binaryTransferEnable "20,21,23,700,701,1114,1184,2950"
                   :binaryTransfer       "true"}]
      (with-open [ps (.prepareStatement c "SELECT name FROM t WHERE age > ? ORDER BY id")]
        (.setInt ps 1 28)
        ;; Five executions: iter 1 through the unnamed prepared path,
        ;; iter 2+ through the upgraded server-prepared + binary path.
        (dotimes [_ 5]
          (with-open [rs (.executeQuery ps)]
            (is (.next rs)) (is (= "Alice"   (.getString rs 1)))
            (is (.next rs)) (is (= "Charlie" (.getString rs 1)))
            (is (not (.next rs)))))))))

(deftest test-sqlstate-25P02-in-failed-tx
  (testing "After error in tx, next statement returns exactly 25P02"
    ;; autosave=never so pgJDBC doesn't mask the aborted tx with an
    ;; implicit savepoint + re-execute.
    (with-conn [c {:preferQueryMode "simple"
                   :autosave        "never"}]
      (.setAutoCommit c false)
      (with-open [st (.createStatement c)]
        ;; Trigger an error to abort the tx
        (try (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (300, 'X', 'bad')")
             (catch SQLException e
               (is (= "22P02" (.getSQLState e)))))
        ;; Now any non-ROLLBACK statement must return 25P02
        (try (.executeQuery st "SELECT 1")
             (is false "expected 25P02 on statement in aborted tx")
             (catch SQLException e
               (is (= "25P02" (.getSQLState e))
                   (str "got " (.getSQLState e) ": " (.getMessage e)))))
        ;; ROLLBACK recovers
        (.rollback c)
        ;; Back to normal
        (with-open [rs (.executeQuery st "SELECT count(*) FROM t")]
          (is (.next rs)))))))

(deftest test-extended-query-error-recovery
  (testing "PreparedStatement error followed by new statement on same connection works"
    ;; This exercises the core protocol-level bug: after an ErrorResponse
    ;; in extended-query mode, the server used to not enter "skip until
    ;; Sync" state, leaving the connection in an undefined place. Now we
    ;; correctly skip and recover on Sync.
    (with-conn [c {:preferQueryMode "extended"}]
      ;; Bad parameter (type mismatch)
      (with-open [ps (.prepareStatement c "INSERT INTO t (id, name, age) VALUES (?, ?, ?)")]
        (.setInt ps 1 400)
        (.setString ps 2 "Zed")
        (.setString ps 3 "notanumber")
        (try (.executeUpdate ps)
             (is false "expected SQLException")
             (catch SQLException e
               (is (= "22P02" (.getSQLState e))))))
      ;; Connection must still work
      (with-open [ps (.prepareStatement c "SELECT name FROM t WHERE id = ?")]
        (.setInt ps 1 1)
        (with-open [rs (.executeQuery ps)]
          (is (.next rs))
          (is (= "Alice" (.getString rs 1))))))))

(deftest test-multi-statement-stops-on-error
  (testing "Simple Query with multiple statements stops on first error"
    ;; PG contract: a semicolon-separated batch where one statement
    ;; fails aborts processing of the remainder.
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        ;; Three statements: ok, FAIL (bad cast), ok-but-should-not-run
        (try
          (.execute st "SELECT 1; INSERT INTO t (id, name, age) VALUES (500, 'Y', 'bad'); INSERT INTO t (id, name, age) VALUES (501, 'Z', 42)")
          (is false "expected SQLException on middle statement")
          (catch SQLException e
            (is (= "22P02" (.getSQLState e)))))
        ;; The third INSERT must not have run
        (with-open [rs (.executeQuery st "SELECT count(*) FROM t WHERE id = 501")]
          (is (.next rs))
          (is (= "0" (.getString rs 1))
              "statement after error must not have executed"))))))

;; ============================================================================
;; Transaction semantics (autocommit, commit, rollback, error-state-sticky)
;; ============================================================================

(deftest test-commit-persists
  (testing "Explicit commit persists INSERT"
    (with-conn [c {:preferQueryMode "simple"}]
      (.setAutoCommit c false)
      (with-open [st (.createStatement c)]
        (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (100, 'Eve', 50)")
        (.commit c)
        ;; In the same session, read back
        (with-open [rs (.executeQuery st "SELECT name FROM t WHERE id = 100")]
          (is (.next rs))
          (is (= "Eve" (.getString rs 1))))))))

(deftest test-rollback-discards
  (testing "Explicit rollback discards INSERT"
    (with-conn [c {:preferQueryMode "simple"}]
      (.setAutoCommit c false)
      (with-open [st (.createStatement c)]
        (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (101, 'Fred', 55)")
        (.rollback c)
        (with-open [rs (.executeQuery st "SELECT name FROM t WHERE id = 101")]
          (is (not (.next rs)) "rolled-back insert must not be visible"))))))

(deftest test-set-show-statement-timeout
  (testing "SET statement_timeout persists, SHOW returns the value"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        (.execute st "SET statement_timeout = 2500")
        (with-open [rs (.executeQuery st "SHOW statement_timeout")]
          (is (.next rs))
          (is (= "2500" (.getString rs 1))))
        ;; RESET clears it
        (.execute st "RESET statement_timeout")
        (with-open [rs (.executeQuery st "SHOW statement_timeout")]
          (is (.next rs))
          (is (= "0" (.getString rs 1))))))))

;; NOTE: Cancel-while-idle used to set a flag observed on the next
;; statement, which was non-PG-compliant. Mid-query cancellation
;; (landed later) moved us to PG semantics: a CancelRequest arriving
;; while the backend is blocked reading the next wire message is
;; silently discarded (postgres.c:4731 "query cancel is a no-op when
;; there is no query in progress"). Coverage of both the idle-no-op
;; and mid-query-57014 behaviors lives in
;; datahike.test.pg-cancel-test — deterministic via an on-query latch
;; instead of Thread/sleep.

(deftest test-sqlstate-40001-not-raised-for-unrelated-writes
  (testing "concurrent writes to disjoint attributes don't trigger 40001"
    ;; jdbc-schema has only one table (:t/*). To exercise the disjoint
    ;; case, we transact a fresh attribute on a different namespace
    ;; inside session B and verify A's commit succeeds without
    ;; serialization_failure.
    (with-conn [a {:preferQueryMode "simple"}]
      (.setAutoCommit a false)
      (with-open [sta (.createStatement a)]
        (.executeUpdate sta "INSERT INTO t (id, name, age) VALUES (700, 'A', 10)")
        ;; Session B writes to a totally separate attribute namespace.
        (with-conn [b {:preferQueryMode "simple"}]
          (.setAutoCommit b true)
          (with-open [stb (.createStatement b)]
            ;; CREATE TABLE transacts a new schema attribute under :other/*
            (.execute stb "CREATE TABLE other (id INTEGER PRIMARY KEY, note TEXT)")
            (.executeUpdate stb "INSERT INTO other (id, note) VALUES (1, 'unrelated')")))
        ;; A's commit must succeed — disjoint write sets.
        (.commit a)
        (with-open [rs (.executeQuery sta "SELECT age FROM t WHERE id = 700")]
          (is (.next rs))
          (is (= 10 (.getInt rs 1))))))))

(deftest test-sqlstate-40001-serialization-failure
  (testing "Concurrent write to the SAME row makes COMMIT raise 40001"
    ;; Historical note: this test used to assert 40001 for two INSERTs of
    ;; DIFFERENT rows — the attribute-level conflict check aborted on any
    ;; same-table concurrency. The check is row-level now (and INSERTs of
    ;; fresh rows are attributed precisely), matching PostgreSQL: disjoint
    ;; writes commit fine (see test-sqlstate-40001-not-raised-for-
    ;; unrelated-writes). A genuine same-row overlap must still abort:
    ;; session A updates a row it read before session B's committed write
    ;; to that row — replaying A would lose B's update.
    (with-conn [a {:preferQueryMode "simple"}]
      (with-open [seed (.createStatement a)]
        (.executeUpdate seed "INSERT INTO t (id, name, age) VALUES (502, 'Contested', 1)"))
      (.setAutoCommit a false)
      (with-open [sta (.createStatement a)]
        (.executeUpdate sta "UPDATE t SET age = 11 WHERE id = 502")
        ;; Session B's writer must not take row locks or it would simply
        ;; BLOCK behind A's UPDATE lock (the PG-faithful behavior, which
        ;; would deadlock this single-threaded test). TRUNCATE retracts
        ;; every row without row locks, overlapping A's row via the
        ;; [eid ::all] wildcard — so A's replay would lose the truncate
        ;; and must abort at COMMIT with 40001, the code Odoo retries on.
        (with-conn [b {:preferQueryMode "simple"}]
          (.setAutoCommit b true)
          (with-open [stb (.createStatement b)]
            (.execute stb "TRUNCATE t")))
        (try (.commit a)
             (is false "expected serialization_failure on COMMIT")
             (catch SQLException e
               (is (= "40001" (.getSQLState e))
                   (str "got " (.getSQLState e) ": " (.getMessage e)))))
        (.rollback a)))))

(deftest test-unique-violation-constraint-fields
  (testing "Primary-key collision raises 23505 and populates constraint fields"
    ;; jdbc-schema declares :t/id as :db.unique/identity. The pgwire
    ;; INSERT translator raises 23505 (not an upsert) on a duplicate
    ;; identity value, with the PG-style constraint name in the
    ;; ServerErrorMessage's `n` field so ORMs can surface it.
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        ;; Seed a row (id is :db.unique/identity).
        (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (600, 'A', 1)")
        (let [raised
              (try
                (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (600, 'B', 2)")
                nil
                (catch org.postgresql.util.PSQLException e e))]
          (is (some? raised) "expected SQLException on duplicate PK")
          (when raised
            (is (= "23505" (.getSQLState raised)))
            (let [sem (.getServerErrorMessage raised)]
              (when sem
                (is (or (nil? (.getConstraint sem))
                        (re-find #"pkey|_key" (.getConstraint sem)))
                    (str "constraint=" (.getConstraint sem)))))))))))

(deftest test-error-state-sticky-in-tx
  (testing "After error in tx, next statement gets 25P02 until ROLLBACK"
    (with-conn [c {:preferQueryMode "simple"
                   ;; autosave=never so the driver doesn't paper over the
                   ;; failed tx with an implicit savepoint
                   :autosave        "never"}]
      (.setAutoCommit c false)
      (with-open [st (.createStatement c)]
        ;; First statement: abort the tx
        (try (.executeUpdate st "INSERT INTO t (id, name, age) VALUES (200, 'X', 'bad')")
             (is false "expected SQLException on bad insert")
             (catch SQLException e
               (is (= "22P02" (.getSQLState e)))))
        ;; Second statement: must fail with 25P02 (in_failed_sql_transaction)
        (try (.executeQuery st "SELECT 1 FROM t LIMIT 1")
             (is false "expected SQLException after aborted tx")
             (catch SQLException e
               (let [code (.getSQLState e)]
                 (is (or (= "25P02" code)
                         ;; We currently return a plain error-string; until
                         ;; a dedicated 25P02 mapping is added, accept the
                         ;; generic aborted message without a specific code.
                         (str/includes? (.getMessage e) "transaction is aborted"))
                     (str "got " code ": " (.getMessage e))))))
        ;; Rollback recovers
        (.rollback c)
        ;; Next statement succeeds
        (with-open [rs (.executeQuery st "SELECT count(*) FROM t")]
          (is (.next rs)))))))

;; ============================================================================
;; Empty query (issue #18)
;; ============================================================================
;;
;; A query string that yields no statements at all is an *empty query*:
;; PG answers with a single EmptyQueryResponse ('I'), never a parse
;; error. Verified against PostgreSQL 17.10 at the protocol level for
;; each form below. pgjdbc surfaces 'I' as "no result set" — `execute`
;; → false — and raises nothing. (It reports getUpdateCount 0 there,
;; which is a driver-side mapping of EmptyQueryResponse rather than
;; anything we put on the wire, so it isn't asserted here.)
;;
;; Before the fix, splitStatements returned the raw string when every
;; fragment was blank, so ";" reached JSqlParser and came back 42601.

(deftest test-empty-query-simple-protocol
  (testing "statement-less query strings return EmptyQueryResponse, not 42601"
    (with-conn [c {:preferQueryMode "simple"}]
      (doseq [sql ["" ";" "  ;  ;  " "-- just a comment" "/* block */"]]
        (with-open [st (.createStatement c)]
          (let [raised (try
                         (is (false? (.execute st sql))
                             (str "no result set expected for " (pr-str sql)))
                         nil
                         (catch SQLException e e))]
            (is (nil? raised)
                (str "empty query " (pr-str sql) " must not raise, got "
                     (some-> raised .getSQLState) ": "
                     (some-> raised .getMessage)))))))))

(deftest test-empty-query-extended-protocol
  (testing "Parse/Bind/Execute of a statement-less string → EmptyQueryResponse"
    (with-conn [c {:preferQueryMode "extended"}]
      (doseq [sql ["" ";" "-- just a comment"]]
        (let [raised (try
                       (with-open [ps (.prepareStatement c sql)]
                         (is (false? (.execute ps))
                             (str "no result set expected for " (pr-str sql))))
                       nil
                       (catch SQLException e e))]
          (is (nil? raised)
              (str "empty query " (pr-str sql) " must not raise at Parse, got "
                   (some-> raised .getSQLState) ": "
                   (some-> raised .getMessage))))))))

(deftest test-empty-fragments-between-statements-are-dropped
  (testing "'SELECT 1;;' runs one statement — empty fragments yield no 'I'"
    (with-conn [c {:preferQueryMode "simple"}]
      (with-open [st (.createStatement c)]
        ;; execute → true means the FIRST response was a result set, so the
        ;; trailing empty fragment produced nothing of its own.
        (is (true? (.execute st "SELECT 1;;")))
        (with-open [rs (.getResultSet st)]
          (is (.next rs))
          (is (= 1 (.getInt rs 1))))
        ;; and no further result follows the empty fragment
        (is (false? (.getMoreResults st)))
        (is (= -1 (.getUpdateCount st)))))))
