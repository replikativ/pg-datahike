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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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

(deftest copy-text-format-basic
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (let [n (copy-in-text c "COPY users (id, name, email, active) FROM stdin"
                          (str "1\talice\talice@example.com\tt\n"
                               "2\tbob\tbob@example.com\tf\n"
                               "3\tcarol\tcarol@example.com\tt\n"))]
      (is (= 3 n) "COPY should report 3 rows")
      (is (= 3 (count (query-rows c "SELECT id, name FROM users")))))))

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

(deftest copy-binary-rejected-cleanly
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (is (thrown-with-msg? java.sql.SQLException #"BINARY"
                          (with-open [stmt (.createStatement c)]
                            (.execute stmt "COPY users FROM stdin WITH BINARY"))))))
