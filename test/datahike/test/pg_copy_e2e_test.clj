(ns datahike.test.pg-copy-e2e-test
  "End-to-end integration test for `COPY ... FROM STDIN` against
   an in-process pg-datahike server. Drives the full wire-protocol
   COPY-IN sub-protocol via pgjdbc's `CopyManager` API — the same
   API that `psql \\copy` uses under the hood, so this exercises
   the same bytes that pg_dump's default-format output produces."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.io StringReader]
           [java.sql Connection DriverManager]
           [org.postgresql.copy CopyIn]
           [org.postgresql PGConnection]
           [org.postgresql.copy CopyManager]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn- jdbc-url [port]
  (str "jdbc:postgresql://localhost:" port "/datahike"
       "?user=datahike&password=datahike"))

(defn- copy-fixture [f]
  (Class/forName "org.postgresql.Driver")
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          srv  (pg/start-server conn {:port 0})
          port (.getPort ^datahike.pg.PgWireServer (:server srv))]
      (try
        ;; Define a target table over JDBC so the schema layer
        ;; matches what a real psql / pg_dump round-trip would set up.
        (with-open [c (DriverManager/getConnection (jdbc-url port))]
          (with-open [stmt (.createStatement c)]
            (.execute stmt
                      (str "CREATE TABLE users ("
                           "id INTEGER PRIMARY KEY, "
                           "name TEXT, "
                           "email TEXT, "
                           "active BOOLEAN)"))))
        (binding [*conn* conn *port* port]
          (f))
        (finally
          (.stop ^datahike.pg.PgWireServer (:server srv))
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each copy-fixture)

(defn- copy-in-text
  "Drive a `COPY users FROM stdin` using pgjdbc's CopyManager and the
   given text-format payload. Returns the row count reported by
   the server in the CommandComplete tag."
  [^Connection c ^String sql ^String body]
  (let [pgconn (.unwrap c PGConnection)
        cm    (CopyManager. pgconn)
        in    (StringReader. body)]
    (.copyIn cm sql in)))

(defn- query-rows [^Connection c sql]
  (with-open [stmt (.createStatement c)
              rs   (.executeQuery stmt sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          out)))))

;; ============================================================================
;; Text format
;; ============================================================================

(deftest copy-sequence-default-survives-failed-row
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_ids (id int GENERATED ALWAYS AS IDENTITY, v int NOT NULL)"))
    (is (thrown? java.sql.SQLException
                 (copy-in-text c "COPY copy_ids(v) FROM STDIN" "\\N\n")))
    (is (thrown? java.sql.SQLException
                 (copy-in-text c "COPY copy_ids(id,v) FROM STDIN" "\\N\t6\n")))
    (is (= 1 (copy-in-text c "COPY copy_ids(v) FROM STDIN" "7\n")))
    (is (= [[2 7]] (query-rows c "SELECT id,v FROM copy_ids")))
    (is (= 1 (copy-in-text c "COPY copy_ids(id,v) FROM STDIN" "99\t8\n")))
    (is (= [[2 7] [99 8]] (query-rows c "SELECT id,v FROM copy_ids ORDER BY id")))))

(deftest copy-late-row-failure-rolls-back-the-whole-statement
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_atomic (id int, v int NOT NULL)"))
    ;; The former implementation committed every 1,000 rows.  A failure in
    ;; row 1,001 therefore leaked a complete prefix of a failed COPY.
    (let [body (str (apply str (map #(str % "\t" % "\n") (range 1 1001)))
                    "1001\t\\N\n")]
      (is (thrown? java.sql.SQLException
                   (copy-in-text c "COPY copy_atomic(id,v) FROM STDIN" body))))
    (is (= [[0]] (query-rows c "SELECT count(*) FROM copy_atomic")))))

(deftest copy-late-unique-failure-preserves-sequence-cutoff
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st (str "CREATE TABLE copy_cutoff ("
                        "id int GENERATED ALWAYS AS IDENTITY, v int UNIQUE)")))
    (let [body (str (apply str (map #(str % "\n") (range 1 1001)))
                    ;; Row 1,001 evaluates its identity default and then fails.
                    "1\n"
                    ;; This row must never be evaluated.
                    "1002\n")]
      (is (thrown? java.sql.SQLException
                   (copy-in-text c "COPY copy_cutoff(v) FROM STDIN" body))))
    (is (= [[0]] (query-rows c "SELECT count(*) FROM copy_cutoff")))
    (is (= 1 (copy-in-text c "COPY copy_cutoff(v) FROM STDIN" "2000\n")))
    (is (= [[1002 2000]] (query-rows c "SELECT id,v FROM copy_cutoff")))))

(deftest copy-prepares-each-row-before-reading-the-next-one
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st (str "CREATE TABLE copy_error_order ("
                        "id int GENERATED ALWAYS AS IDENTITY, "
                        "v int CHECK (v > 0))")))
    (let [error (try
                  (copy-in-text c "COPY copy_error_order(v) FROM STDIN"
                                "-1\nnot-an-int\n")
                  nil
                  (catch java.sql.SQLException e e))]
      (is (= "23514" (.getSQLState ^java.sql.SQLException error))))
    ;; The CHECK-failing first row consumed its default; the later coercion
    ;; error was never reached.
    (is (= 1 (copy-in-text c "COPY copy_error_order(v) FROM STDIN" "1\n")))
    (is (= [[2 1]] (query-rows c "SELECT id,v FROM copy_error_order")))))

(deftest copy-processes-valid-rows-before-a-later-decoder-error
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st (str "CREATE TABLE copy_decode_order ("
                        "id int GENERATED ALWAYS AS IDENTITY, v int)")))
    ;; The first NL-terminated row is valid.  The later bare CR conflicts with
    ;; that established EOL style and fails during decoder finalization.
    (is (thrown? java.sql.SQLException
                 (copy-in-text c "COPY copy_decode_order(v) FROM STDIN"
                               "10\n20\r")))
    (is (= 1 (copy-in-text c "COPY copy_decode_order(v) FROM STDIN" "30\n")))
    (is (= [[2 30]] (query-rows c "SELECT id,v FROM copy_decode_order")))))

(deftest copy-processes-a-bare-cr-row-before-a-later-decoder-error
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st (str "CREATE TABLE copy_cr_decode_order ("
                        "id int GENERATED ALWAYS AS IDENTITY, v int)")))
    ;; This is the opposite streaming boundary: the first row establishes CR,
    ;; then the following NL is inconsistent.  Resolving CR lookahead must
    ;; publish row 1 to candidate evaluation before parsing the later error.
    (is (thrown? java.sql.SQLException
                 (copy-in-text c "COPY copy_cr_decode_order(v) FROM STDIN"
                               "10\r20\n")))
    (is (= 1 (copy-in-text c "COPY copy_cr_decode_order(v) FROM STDIN" "30\r")))
    (is (= [[2 30]] (query-rows c "SELECT id,v FROM copy_cr_decode_order")))))

(deftest copy-fail-discards-every-received-row
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_cancelled (id int)"))
    (let [cm (CopyManager. (.unwrap c PGConnection))
          ^CopyIn copy (.copyIn cm "COPY copy_cancelled(id) FROM STDIN")
          bytes (.getBytes (apply str (map #(str % "\n") (range 1 1201)))
                           java.nio.charset.StandardCharsets/UTF_8)]
      (.writeToCopy copy bytes 0 (alength bytes))
      (.cancelCopy copy))
    (is (= [[0]] (query-rows c "SELECT count(*) FROM copy_cancelled")))))

(deftest copy-participates-in-an-explicit-transaction
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_tx (id int)")
      (.execute st "BEGIN"))
    (is (= 2 (copy-in-text c "COPY copy_tx(id) FROM STDIN" "1\n2\n")))
    ;; Rows are visible to later statements in the same transaction.
    (is (= [[2]] (query-rows c "SELECT count(*) FROM copy_tx")))
    (with-open [st (.createStatement c)]
      (.execute st "ROLLBACK"))
    (is (= [[0]] (query-rows c "SELECT count(*) FROM copy_tx")))))

(deftest copy-commits-with-an-explicit-transaction
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_commit (id int)")
      (.execute st "BEGIN"))
    (is (= 2 (copy-in-text c "COPY copy_commit(id) FROM STDIN" "1\n2\n")))
    (with-open [st (.createStatement c)]
      (.execute st "COMMIT"))
    (is (= [[1] [2]] (query-rows c "SELECT id FROM copy_commit ORDER BY id")))))

(deftest two-copy-statements-in-one-transaction-use-distinct-tempids
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_twice (id int)")
      (.execute st "BEGIN"))
    (is (= 2 (copy-in-text c "COPY copy_twice(id) FROM STDIN" "1\n2\n")))
    (is (= 2 (copy-in-text c "COPY copy_twice(id) FROM STDIN" "3\n4\n")))
    (with-open [st (.createStatement c)]
      (.execute st "COMMIT"))
    (is (= [[1] [2] [3] [4]]
           (query-rows c "SELECT id FROM copy_twice ORDER BY id")))))

(deftest copy-rolls-back-to-savepoint
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_savepoint (id int)")
      (.execute st "BEGIN")
      (.execute st "SAVEPOINT before_copy"))
    (is (= 2 (copy-in-text c "COPY copy_savepoint(id) FROM STDIN" "1\n2\n")))
    (with-open [st (.createStatement c)]
      (.execute st "ROLLBACK TO SAVEPOINT before_copy")
      (.execute st "COMMIT"))
    (is (= [[0]] (query-rows c "SELECT count(*) FROM copy_savepoint")))))

(deftest copy-uses-a-sequence-created-in-the-surrounding-transaction
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "BEGIN")
      (.execute st (str "CREATE TABLE copy_local_sequence ("
                        "id int GENERATED ALWAYS AS IDENTITY, v int)")))
    (is (= 2 (copy-in-text c "COPY copy_local_sequence(v) FROM STDIN" "10\n20\n")))
    (is (= [[1 10] [2 20]]
           (query-rows c "SELECT id,v FROM copy_local_sequence ORDER BY id")))
    (with-open [st (.createStatement c)]
      (.execute st "COMMIT"))
    (is (= [[1 10] [2 20]]
           (query-rows c "SELECT id,v FROM copy_local_sequence ORDER BY id")))))

(deftest large-copy-with-existing-sequence-commits-inside-begin
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st (str "CREATE TABLE copy_existing_sequence ("
                        "id int GENERATED ALWAYS AS IDENTITY, v int)"))
      (.execute st "BEGIN"))
    ;; More reservations than the 512-entry recent-commit conflict ring.
    (let [body (apply str (map #(str % "\n") (range 600)))]
      (is (= 600 (copy-in-text c
                               "COPY copy_existing_sequence(v) FROM STDIN"
                               body))))
    (with-open [st (.createStatement c)]
      (.execute st "COMMIT"))
    (is (= [[600]]
           (query-rows c "SELECT count(*) FROM copy_existing_sequence")))))

(deftest copy-native-identity-is-rechecked-at-publication
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_concurrent (id int PRIMARY KEY, v int)"))
    (let [cm (CopyManager. (.unwrap c PGConnection))
          ^CopyIn copy (.copyIn cm "COPY copy_concurrent(id,v) FROM STDIN")
          bytes (.getBytes "1\t10\n" java.nio.charset.StandardCharsets/UTF_8)]
      (.writeToCopy copy bytes 0 (alength bytes))
      ;; Land a conflicting native write after candidate preparation but before
      ;; CopyDone. Datahike identity tempids would otherwise upsert onto it.
      (d/transact *conn* [{:db/id "concurrent-row"
                           :copy_concurrent/id 1
                           :copy_concurrent/v 99
                           :copy_concurrent/db-row-exists true}])
      (let [error (try (.endCopy copy) nil
                       (catch java.sql.SQLException e e))]
        (is (= "23505" (.getSQLState ^java.sql.SQLException error)))))
    (is (= [[1 99]]
           (query-rows c "SELECT id,v FROM copy_concurrent")))))

(deftest copy-self-fk-can-reference-a-row-in-a-later-batch
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st (str "CREATE TABLE copy_self_fk (id int PRIMARY KEY, "
                        "parent_id int REFERENCES copy_self_fk(id))")))
    (let [body (str "1\t1001\n"
                    (apply str (map #(str % "\t\\N\n") (range 2 1001)))
                    "1001\t\\N\n")]
      (is (= 1001 (copy-in-text c
                                "COPY copy_self_fk(id,parent_id) FROM STDIN"
                                body))))
    (is (= [[1001]]
           (query-rows c "SELECT parent_id FROM copy_self_fk WHERE id = 1")))))

(deftest failed-copy-aborts-an-explicit-transaction
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE copy_failed_tx (id int NOT NULL)")
      (.execute st "BEGIN"))
    (is (thrown? java.sql.SQLException
                 (copy-in-text c "COPY copy_failed_tx(id) FROM STDIN" "\\N\n")))
    (is (thrown? java.sql.SQLException
                 (query-rows c "SELECT count(*) FROM copy_failed_tx")))
    (with-open [st (.createStatement c)]
      (.execute st "ROLLBACK"))
    (is (= [[0]] (query-rows c "SELECT count(*) FROM copy_failed_tx")))))

(deftest copy-text-format-basic
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin"
                          (str "1\talice\talice@example.com\tt\n"
                               "2\tbob\tbob@example.com\tf\n"
                               "3\tcarol\tcarol@example.com\tt\n"))]
      (is (= 3 n) "COPY should report 3 rows")
      (is (= 3 (count (query-rows c "SELECT id, name FROM users")))))))

(deftest copy-accepts-a-schema-qualified-table
  ;; `COPY public.users` built its attributes in the `public` namespace
  ;; — `:public/id` instead of `:users/id` — so the transaction failed
  ;; with "Bad entity attribute". The row-marker line right next to it
  ;; already used the table name, so the two disagreed.
  ;;
  ;; It matters because pg_dump ALWAYS emits the qualified form:
  ;; restoring a real PostgreSQL dump died on its first COPY block.
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n (copy-in-text c "COPY public.users (id, name, email, active) FROM stdin"
                          (str "1\talice\talice@example.com\tt\n"
                               "2\tbob\tbob@example.com\tf\n"))]
      (is (= 2 n) "a schema-qualified COPY should load its rows")
      (is (= [[1 "alice"] [2 "bob"]]
             (query-rows c "SELECT id, name FROM users ORDER BY id"))))))

(deftest copy-text-format-with-null
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin"
                          (str "1\talice\t\\N\tt\n"
                               "2\t\\N\tbob@example.com\t\\N\n"))]
      (is (= 2 n))
      (let [rows (query-rows c "SELECT id, name, email FROM users ORDER BY id")]
        (is (= 2 (count rows)))
        ;; Row 1: email = NULL
        ;; Row 2: name = NULL
        ;; pgjdbc returns nil / Java null for SQL NULL
        (is (or (nil? (nth (first rows) 2)) (= "" (nth (first rows) 2))))
        (is (or (nil? (nth (second rows) 1)) (= "" (nth (second rows) 1))))))))

(deftest copy-text-format-with-escapes
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    ;; pg_dump-style: tabs and newlines inside string fields are
    ;; backslash-escaped.
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin"
                          "1\twith\\ttab\tline\\nwith\\nnewlines\tt\n")]
      (is (= 1 n))
      (let [rows (query-rows c "SELECT name, email FROM users")
            row (first rows)]
        (is (= "with\ttab" (first row)))
        (is (= "line\nwith\nnewlines" (second row)))))))

(deftest copy-text-format-eod-marker
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    ;; Bytes after `\.` on its own line are discarded
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin"
                          (str "1\talice\ta@x\tt\n"
                               "2\tbob\tb@x\tf\n"
                               "\\.\n"
                               "3\tignored\ti@x\tt\n"))]
      (is (= 2 n))
      (is (= 2 (count (query-rows c "SELECT * FROM users")))))))

;; ============================================================================
;; CSV format
;; ============================================================================

(deftest copy-csv-format-basic
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin WITH (FORMAT 'csv')"
                          (str "1,alice,alice@example.com,t\n"
                               "2,bob,bob@example.com,f\n"))]
      (is (= 2 n))
      (is (= 2 (count (query-rows c "SELECT id FROM users")))))))

(deftest copy-csv-format-quoted-fields
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin WITH (FORMAT 'csv')"
                          (str "1,\"hello, world\",\"line1\nline2\",t\n"
                               "2,\"with \"\"embedded\"\" quote\",e@x,f\n"))]
      (is (= 2 n))
      (let [rows (query-rows c "SELECT name, email FROM users ORDER BY id")]
        (is (= "hello, world" (first (first rows))))
        (is (= "line1\nline2"  (second (first rows))))
        (is (= "with \"embedded\" quote" (first (second rows))))))))

(deftest copy-csv-format-header-skip
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin WITH (FORMAT 'csv', HEADER true)"
                          (str "id,name,email,active\n"
                               "1,alice,a@x,t\n"
                               "2,bob,b@x,f\n"))]
      (is (= 2 n) "Header row must not be counted in COPY <n>")
      (is (= 2 (count (query-rows c "SELECT id FROM users")))))))

(deftest copy-csv-format-null-marker
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    ;; Custom NULL — only unquoted NIL becomes null
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin WITH (FORMAT 'csv', NULL 'NIL')"
                          (str "1,alice,NIL,t\n"
                               "2,\"NIL\",b@x,f\n"))]
      (is (= 2 n))
      (let [rows (query-rows c "SELECT id, name, email FROM users ORDER BY id")]
        ;; Row 1: email = NULL (unquoted NIL matches null marker)
        (is (or (nil? (nth (first rows) 2)) (= "" (nth (first rows) 2))))
        ;; Row 2: name = "NIL" (quoted, so literal string)
        (is (= "NIL" (nth (second rows) 1)))))))

;; ============================================================================
;; Large fixture / batching
;; ============================================================================

(deftest copy-large-fixture
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n-rows 2500
          body (apply str
                      (for [i (range n-rows)]
                        (format "%d\tname-%d\temail%d@x\tt\n" i i i)))
          n (copy-in-text c "COPY users (id, name, email, active) FROM stdin" body)]
      (is (= n-rows n) "All rows reported")
      (is (= n-rows (count (query-rows c "SELECT id FROM users")))))))

;; ============================================================================
;; Error paths
;; ============================================================================

(deftest copy-rejects-short-and-wide-rows
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (is (thrown? java.sql.SQLException
                 (copy-in-text c "COPY users(id,name) FROM STDIN" "1\n")))
    (is (thrown? java.sql.SQLException
                 (copy-in-text c "COPY users(id,name) FROM STDIN" "1\ta\textra\n")))
    (is (= [[0]] (query-rows c "SELECT count(*) FROM users")))))

(deftest copy-unknown-column-fails-before-copy-mode
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [e (try
              (copy-in-text c "COPY users (missing) FROM stdin" "value\n")
              nil
              (catch java.sql.SQLException e e))]
      (is (instance? java.sql.SQLException e))
      (is (= "42703" (.getSQLState ^java.sql.SQLException e)))
      (is (re-find #"column.*missing.*does not exist" (.getMessage e))))))

(deftest copy-binary-rejected-cleanly
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (is (thrown-with-msg? java.sql.SQLException #"BINARY"
                          (with-open [stmt (.createStatement c)]
                            (.execute stmt "COPY users FROM stdin WITH BINARY"))))))
