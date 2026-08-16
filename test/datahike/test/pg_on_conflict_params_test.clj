(ns datahike.test.pg-on-conflict-params-test
  "Parameterized `INSERT … ON CONFLICT` over the extended-query protocol.

   `translate-insert`'s ON CONFLICT branch hands its work to a
   `:db.fn/call` tx-fn. The row maps used to be captured by that fn's
   CLOSURE, but every Execute-time pass (`substitute-params`,
   `resolve-nextvals!`, the INSERT value re-coercion) walks `:tx-data`
   as DATA and cannot see inside a Clojure fn — so `$N` placeholders
   survived into the conflict lookup and the asserted values, and any
   parameterized upsert died with

     class datahike.pg.sql.params.ParamRef cannot be cast to
     class java.lang.Number

   while the same statement with literals worked. Since parameterized
   `INSERT … ON CONFLICT` is what essentially every ORM emits, these
   tests pin the whole shape: DO UPDATE / DO NOTHING, parameters in the
   SET expressions, multi-row VALUES, composite conflict targets,
   RETURNING across reuses of one server-side prepared statement, and
   untyped text parameters that only narrow to the column type after
   substitution."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager PreparedStatement ResultSet]))

(def ^:dynamic *port* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          factory (reify PgWireServer$QueryHandlerFactory
                    (create [_] (pg/make-query-handler conn)))
          server (PgWireServer. 0 "127.0.0.1" factory)]
      (.start server)
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- open
  "Extended-query connection. `binaryTransfer=false` keeps binds in text
   format (our wire rejects binary Bind); `extra` appends driver options
   — `prepareThreshold=1` in particular forces pgjdbc to name the
   statement on the FIRST execute, so subsequent executes reuse the
   server-side parsed result instead of re-Parsing."
  (^Connection [] (open ""))
  (^Connection [extra]
   (DriverManager/getConnection
    (str "jdbc:postgresql://127.0.0.1:" *port*
         "/datahike?user=datahike&password=x&sslmode=disable"
         "&binaryTransfer=false&preferQueryMode=extended" extra))))

(defn- ddl! [^Connection c & stmts]
  (with-open [st (.createStatement c)]
    (doseq [s stmts] (.execute st s))))

(defn- rows
  "Run `sql` and return a vector of row vectors of boxed column values."
  [^Connection c ^String sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- returned
  "Execute a RETURNING statement and collect its rows."
  [^PreparedStatement ps]
  (with-open [^ResultSet rs (.executeQuery ps)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          acc)))))

;; ---------------------------------------------------------------------------
;; DO UPDATE / DO NOTHING with parameters in VALUES

(deftest param-on-conflict-do-update
  (testing "$N in VALUES reaches the conflict lookup and the asserted values"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT, body TEXT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO note (id, title, body) VALUES (?, ?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title"))]
        ;; No existing row → plain insert.
        (.setLong ps 1 1) (.setString ps 2 "first") (.setString ps 3 "b1")
        (is (= 1 (.executeUpdate ps)))
        ;; Same key → the DO UPDATE arm fires.
        (.setLong ps 1 1) (.setString ps 2 "second") (.setString ps 3 "b2")
        (.executeUpdate ps))
      (is (= [[1 "second" "b1"]] (rows c "SELECT id, title, body FROM note"))
          "title updated from EXCLUDED, body untouched (not in the SET list)"))))

(deftest param-on-conflict-do-nothing
  (testing "$N in VALUES with DO NOTHING keeps the original row"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT)")
      (with-open [ps (.prepareStatement
                      c "INSERT INTO note (id, title) VALUES (?, ?) ON CONFLICT (id) DO NOTHING")]
        (.setLong ps 1 1) (.setString ps 2 "keep")
        (.executeUpdate ps)
        (.setLong ps 1 1) (.setString ps 2 "discard")
        (.executeUpdate ps))
      (is (= [[1 "keep"]] (rows c "SELECT id, title FROM note"))))))

(deftest param-on-conflict-without-target
  (testing "ON CONFLICT DO NOTHING with no conflict target (all-columns check)"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE tag (a BIGINT, b TEXT)")
      (with-open [ps (.prepareStatement
                      c "INSERT INTO tag (a, b) VALUES (?, ?) ON CONFLICT DO NOTHING")]
        (.setLong ps 1 1) (.setString ps 2 "x") (.executeUpdate ps)
        (.setLong ps 1 1) (.setString ps 2 "x") (.executeUpdate ps)
        (.setLong ps 1 2) (.setString ps 2 "y") (.executeUpdate ps))
      (is (= [[1 "x"] [2 "y"]] (rows c "SELECT a, b FROM tag ORDER BY a"))))))

;; ---------------------------------------------------------------------------
;; Parameters inside the DO UPDATE SET expressions

(deftest param-in-do-update-set
  (testing "SET col = $N — the placeholder lives in the SET expression, not VALUES"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO note (id, title) VALUES (?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET title = ?"))]
        (.setLong ps 1 1) (.setString ps 2 "inserted") (.setString ps 3 "from-set")
        (.executeUpdate ps)
        (is (= [[1 "inserted"]] (rows c "SELECT id, title FROM note"))
            "no conflict yet — VALUES wins")
        (.setLong ps 1 1) (.setString ps 2 "ignored") (.setString ps 3 "from-set")
        (.executeUpdate ps))
      (is (= [[1 "from-set"]] (rows c "SELECT id, title FROM note"))))))

(deftest param-in-do-update-set-arithmetic
  (testing "the upsert-counter idiom: SET n = t.n + EXCLUDED.n and SET n = t.n + $N"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE ctr (id BIGINT PRIMARY KEY, n BIGINT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO ctr (id, n) VALUES (?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET n = ctr.n + EXCLUDED.n"))]
        (.setLong ps 1 1) (.setLong ps 2 10) (.executeUpdate ps)
        (.setLong ps 1 1) (.setLong ps 2 5)  (.executeUpdate ps))
      (is (= [[1 15]] (rows c "SELECT id, n FROM ctr")))
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO ctr (id, n) VALUES (?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET n = ctr.n + ?"))]
        (.setLong ps 1 1) (.setLong ps 2 0) (.setLong ps 3 100)
        (.executeUpdate ps))
      (is (= [[1 115]] (rows c "SELECT id, n FROM ctr"))
          "$3 resolves inside the SET arithmetic, not just in VALUES"))))

;; ---------------------------------------------------------------------------
;; Shapes around the conflict target

(deftest param-on-conflict-multi-row-values
  (testing "multi-row VALUES with parameters in every row"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO note (id, title) VALUES (?, ?), (?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title"))]
        (.setLong ps 1 1) (.setString ps 2 "a")
        (.setLong ps 3 2) (.setString ps 4 "b")
        (.executeUpdate ps)
        (.setLong ps 1 2) (.setString ps 2 "b2")
        (.setLong ps 3 3) (.setString ps 4 "c")
        (.executeUpdate ps))
      (is (= [[1 "a"] [2 "b2"] [3 "c"]]
             (rows c "SELECT id, title FROM note ORDER BY id"))))))

(deftest param-on-conflict-composite-target
  (testing "composite conflict target ON CONFLICT (a, b) with parameters"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE m2m (a BIGINT, b BIGINT, tag TEXT, PRIMARY KEY (a, b))")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO m2m (a, b, tag) VALUES (?, ?, ?) "
                             "ON CONFLICT (a, b) DO UPDATE SET tag = EXCLUDED.tag"))]
        (.setLong ps 1 1) (.setLong ps 2 2) (.setString ps 3 "t1") (.executeUpdate ps)
        (.setLong ps 1 1) (.setLong ps 2 2) (.setString ps 3 "t2") (.executeUpdate ps)
        (.setLong ps 1 1) (.setLong ps 2 3) (.setString ps 3 "t3") (.executeUpdate ps))
      (is (= [[1 2 "t2"] [1 3 "t3"]]
             (rows c "SELECT a, b, tag FROM m2m ORDER BY a, b"))))))

(deftest param-insert-select-on-conflict
  (testing "INSERT … SELECT $1, $2 … ON CONFLICT (the FROM-less SELECT row)"
    ;; The INSERT … SELECT arm of translate-insert has its own ON CONFLICT
    ;; tx-fn, with the same closure problem. Replaying an identical row is
    ;; what this pins — that arm matches conflicts on ALL inserted columns
    ;; and ignores the conflict target, a separate pre-existing limitation
    ;; this change does not address.
    (with-open [c (open)]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT)")
      (with-open [ps (.prepareStatement
                      c "INSERT INTO note (id, title) SELECT ?, ? ON CONFLICT (id) DO NOTHING")]
        (.setLong ps 1 1) (.setString ps 2 "keep") (.executeUpdate ps)
        (.setLong ps 1 1) (.setString ps 2 "keep") (.executeUpdate ps)
        (.setLong ps 1 2) (.setString ps 2 "other") (.executeUpdate ps))
      (is (= [[1 "keep"] [2 "other"]]
             (rows c "SELECT id, title FROM note ORDER BY id"))))))

;; ---------------------------------------------------------------------------
;; Reuse of one server-side prepared statement

(deftest param-on-conflict-returning-across-reuses
  (testing "RETURNING reports only the current execute's rows"
    ;; prepareThreshold=1 makes pgjdbc name the statement immediately, so
    ;; every execute after the first reuses the SAME parsed result — and
    ;; with it the :row-refs atom the ON CONFLICT tx-fn writes into.
    (with-open [c (open "&prepareThreshold=1")]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO note (id, title) VALUES (?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title "
                             "RETURNING id, title"))]
        (doseq [[id title] [[1 "a"] [2 "b"] [3 "c"] [1 "a2"]]]
          (.setLong ps 1 (long id))
          (.setString ps 2 title)
          (is (= [[id title]] (returned ps))
              (str "RETURNING for id=" id " must not carry earlier executes' rows"))))
      (is (= [[1 "a2"] [2 "b"] [3 "c"]]
             (rows c "SELECT id, title FROM note ORDER BY id"))))))

(deftest param-on-conflict-returning-in-transaction
  (testing "RETURNING inside an explicit transaction (tx-buffer replays at COMMIT)"
    (with-open [c (open "&prepareThreshold=1")]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT)")
      (.setAutoCommit c false)
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO note (id, title) VALUES (?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title "
                             "RETURNING id"))]
        (.setLong ps 1 1) (.setString ps 2 "a")
        (is (= [[1]] (returned ps)))
        (.setLong ps 1 2) (.setString ps 2 "b")
        (is (= [[2]] (returned ps))))
      (.commit c)
      (.setAutoCommit c true)
      (is (= [[1 "a"] [2 "b"]] (rows c "SELECT id, title FROM note ORDER BY id"))))))

;; ---------------------------------------------------------------------------
;; Value coercion of the tx-fn payload

(deftest param-on-conflict-untyped-text-param
  (testing "a text-typed parameter bound to a BIGINT column narrows after substitution"
    ;; node-postgres (and pgjdbc's setString) describe `$1` as varchar even
    ;; for an integer column; the value only becomes a Long once the
    ;; placeholder is resolved, so the re-coercion pass has to reach the
    ;; rows the ON CONFLICT tx-fn receives.
    (with-open [c (open)]
      (ddl! c "CREATE TABLE ctr (id BIGINT PRIMARY KEY, n BIGINT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO ctr (id, n) VALUES (?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET n = EXCLUDED.n"))]
        (.setString ps 1 "7") (.setString ps 2 "1")  (.executeUpdate ps)
        (.setString ps 1 "7") (.setString ps 2 "42") (.executeUpdate ps))
      (is (= [[7 42]] (rows c "SELECT id, n FROM ctr"))
          "second bind conflicts on the coerced key and updates in place"))))

(deftest param-on-conflict-null-param
  (testing "a NULL parameter is simply not asserted (EAV null)"
    (with-open [c (open)]
      (ddl! c "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT, body TEXT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO note (id, title, body) VALUES (?, ?, ?) "
                             "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title"))]
        (.setLong ps 1 1) (.setString ps 2 "t") (.setNull ps 3 java.sql.Types/VARCHAR)
        (.executeUpdate ps))
      (is (= [[1 "t" nil]] (rows c "SELECT id, title, body FROM note"))))))

;; ---------------------------------------------------------------------------
;; nextval() markers in an ON CONFLICT INSERT
;;
;; `{:fn :nextval …}` markers are resolved by a sibling pass to the
;; ParamRef substitution and were equally unreachable inside the closure.

(deftest nextval-in-values-with-on-conflict
  (testing "nextval() in the VALUES of an ON CONFLICT INSERT advances the sequence"
    (with-open [c (open)]
      (ddl! c "CREATE SEQUENCE note_seq"
            "CREATE TABLE note (id BIGINT PRIMARY KEY, title TEXT)")
      (with-open [ps (.prepareStatement
                      c (str "INSERT INTO note (id, title) VALUES (nextval('note_seq'), ?) "
                             "ON CONFLICT (id) DO UPDATE SET title = EXCLUDED.title"))]
        (.setString ps 1 "a") (.executeUpdate ps)
        (.setString ps 1 "b") (.executeUpdate ps))
      (is (= [[1 "a"] [2 "b"]] (rows c "SELECT id, title FROM note ORDER BY id"))))))
