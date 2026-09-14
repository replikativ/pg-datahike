(ns datahike.test.pg-nextval-in-values-test
  "Coverage for `nextval('seq')` appearing inside INSERT VALUES (and
   for `DEFAULT nextval('seq')` in CREATE TABLE). Both rely on the
   sibling-pass design: ParamRefs resolve at Bind, nextval markers
   resolve at Execute via the same CAS-retry path SELECT nextval(...)
   uses — so concurrent INSERTs get distinct values and the advance
   sticks even on rollback (PG's non-transactional nextval semantics)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]
           [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn- jdbc-url [port] (str "jdbc:postgresql://localhost:" port "/datahike"
                            "?user=datahike&password=datahike"))

(defn- nextval-fixture [f]
  (Class/forName "org.postgresql.Driver")
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          srv  (pg/start-server conn {:port 0})
          port (.getPort ^datahike.pg.PgWireServer (:server srv))]
      (try
        (binding [*port* port *conn* conn] (f))
        (finally
          (.stop ^datahike.pg.PgWireServer (:server srv))
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each nextval-fixture)

(defn- exec! [^Connection c ^String sql]
  (with-open [stmt (.createStatement c)] (.execute stmt sql)))

(defn- query-rows [^Connection c ^String sql]
  (with-open [stmt (.createStatement c)
              rs (.executeQuery stmt sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getObject rs (int %)) (range 1 (inc n)))))
          out)))))

;; ============================================================================
;; Limitation #1: DEFAULT nextval(...) in CREATE TABLE
;; ============================================================================

(deftest default-nextval-on-create-table
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE myseq")
    ;; The bare form `DEFAULT nextval('myseq')` was a JSqlParser parse
    ;; error before the rewrite rule wrapped it as `DEFAULT (nextval(...))`.
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY DEFAULT nextval('myseq'), label TEXT)")
    (exec! c "INSERT INTO t (label) VALUES ('a')")
    (exec! c "INSERT INTO t (label) VALUES ('b')")
    (let [rows (query-rows c "SELECT id, label FROM t ORDER BY id")]
      (is (= 2 (count rows)))
      (is (= "a" (-> rows first second)))
      (is (= "b" (-> rows second second)))
      (is (< (-> rows first first) (-> rows second first))
          "ids monotonically increase from the sequence"))))

(deftest default-nextval-paren-form-still-works
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    ;; The pre-existing parenthesised form must keep working — our rewrite
    ;; only injects parens when they're missing.
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY DEFAULT (nextval('s')), label TEXT)")
    (exec! c "INSERT INTO t (label) VALUES ('first')")
    (is (= 1 (count (query-rows c "SELECT id FROM t"))))))

;; ============================================================================
;; Limitation #2: nextval() in INSERT VALUES — simple query path
;; ============================================================================

(deftest nextval-in-values-simple-query
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE myseq")
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY, label TEXT)")
    (exec! c "INSERT INTO t VALUES (nextval('myseq'), 'a')")
    (exec! c "INSERT INTO t VALUES (nextval('myseq'), 'b')")
    (exec! c "INSERT INTO t VALUES (nextval('myseq'), 'c')")
    (let [rows (query-rows c "SELECT id, label FROM t ORDER BY id")]
      (is (= 3 (count rows)))
      (is (= [1 2 3] (mapv first rows))
          "Three nextval calls give 1,2,3 (initial seq starts at 1)")
      (is (= ["a" "b" "c"] (mapv second rows))))))

;; ============================================================================
;; Limitation #2: nextval() in INSERT VALUES — extended (prepared) query path
;; mixed with `?` parameters. Exercises both substitution passes composing.
;; ============================================================================

(deftest nextval-in-prepared-insert
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE myseq")
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY, label TEXT)")
    (with-open [ps (.prepareStatement c "INSERT INTO t VALUES (nextval('myseq'), ?)")]
      (.setString ps 1 "alice") (.execute ps)
      (.setString ps 1 "bob")   (.execute ps)
      (.setString ps 1 "carol") (.execute ps))
    (let [rows (query-rows c "SELECT id, label FROM t ORDER BY id")]
      (is (= 3 (count rows)))
      (is (= [1 2 3] (mapv first rows))
          "Each prepared execution advances the sequence")
      (is (= ["alice" "bob" "carol"] (mapv second rows))))))

(deftest prepared-batch-returning-mixed-nulls-reserves-distinct-defaults
  (with-open [c (DriverManager/getConnection
                 (str (jdbc-url *port*) "&prepareThreshold=1"))]
    (.setAutoCommit c false)
    (exec! c "CREATE TABLE mixednulltest (key serial primary key, value text)")
    (with-open [ps (.prepareStatement
                    c "INSERT INTO mixednulltest(value) VALUES (?)"
                    (into-array String ["key"]))]
      (doseq [value [nil "test" nil nil nil]]
        (.setObject ps 1 value)
        (.addBatch ps))
      (.executeBatch ps)
      (with-open [rs (.getGeneratedKeys ps)]
        (is (= [1 2 3 4 5]
               (loop [keys []]
                 (if (.next rs)
                   (recur (conj keys (.getInt rs 1)))
                   keys))))))
    (.rollback c)))

(deftest multiple-nextval-in-one-row
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE seqa")
    (exec! c "CREATE SEQUENCE seqb")
    (exec! c "CREATE TABLE t (a BIGINT PRIMARY KEY, b BIGINT)")
    (exec! c "INSERT INTO t VALUES (nextval('seqa'), nextval('seqb'))")
    (exec! c "INSERT INTO t VALUES (nextval('seqa'), nextval('seqb'))")
    (let [rows (query-rows c "SELECT a, b FROM t ORDER BY a")]
      (is (= [[1 1] [2 2]] (mapv vec rows))
          "Distinct sequences advance independently per row"))))

(deftest nextval-follows-row-and-expression-order
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    (exec! c "CREATE TABLE t (a BIGINT PRIMARY KEY, b BIGINT)")
    (exec! c (str "INSERT INTO t VALUES "
                  "(nextval('s'), nextval('s')),"
                  "(nextval('s'), nextval('s'))"))
    (is (= [[1 2] [3 4]]
           (mapv vec (query-rows c "SELECT a, b FROM t ORDER BY a"))))))

(deftest nextval-order-does-not-depend-on-map-iteration
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    (exec! c (str "CREATE TABLE wide ("
                  "c1 BIGINT, c2 BIGINT, c3 BIGINT, c4 BIGINT, c5 BIGINT, "
                  "c6 BIGINT, c7 BIGINT, c8 BIGINT, c9 BIGINT, c10 BIGINT)"))
    (exec! c (str "INSERT INTO wide VALUES ("
                  "nextval('s'),nextval('s'),nextval('s'),nextval('s'),nextval('s'),"
                  "nextval('s'),nextval('s'),nextval('s'),nextval('s'),nextval('s'))"))
    (is (= [(vec (range 1 11))]
           (mapv vec (query-rows c "SELECT * FROM wide"))))))

(deftest returning-does-not-reserve-sequence-values-again
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY, label TEXT)")
    (is (= [[1 "a"] [2 "b"]]
           (mapv vec
                 (query-rows c (str "INSERT INTO t VALUES "
                                    "(nextval('s'), 'a'), (nextval('s'), 'b') "
                                    "RETURNING id, label")))))
    (is (= [[3]] (mapv vec (query-rows c "SELECT nextval('s')"))))))

(deftest explicit-null-does-not-run-a-default
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    (exec! c "CREATE TABLE t (id BIGINT DEFAULT nextval('s'), label TEXT)")
    (exec! c "INSERT INTO t (id, label) VALUES (NULL, 'simple')")
    (with-open [ps (.prepareStatement c "INSERT INTO t (id, label) VALUES (?, ?)")]
      (.setNull ps 1 java.sql.Types/BIGINT)
      (.setString ps 2 "prepared")
      (.execute ps))
    (is (= [[1]] (mapv vec (query-rows c "SELECT nextval('s')")))
        "neither explicit NULL form consumes the omitted-column default")))

(deftest nextval-undefined-sequence-error
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY)")
    ;; Reference a sequence that doesn't exist — should raise, not silently fail.
    (is (thrown? java.sql.SQLException
                 (exec! c "INSERT INTO t VALUES (nextval('nosuch'))")))))

(deftest later-expression-error-preserves-earlier-sequence-effect
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    (exec! c "CREATE TABLE t (id BIGINT, n BIGINT)")
    (is (thrown? java.sql.SQLException
                 (exec! c "INSERT INTO t VALUES (nextval('s'), 1 / 0)")))
    (is (= [[2]] (mapv vec (query-rows c "SELECT nextval('s')"))))))

(deftest catalog-unique-index-stops-later-defaults
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    (exec! c (str "CREATE TABLE t (id BIGINT DEFAULT nextval('s'), "
                  "external_key BIGINT)"))
    (exec! c "CREATE UNIQUE INDEX t_external_key_key ON t(external_key)")
    (exec! c "INSERT INTO t(external_key) VALUES (10)")
    (is (thrown? java.sql.SQLException
                 (exec! c (str "INSERT INTO t(external_key) VALUES "
                               "(20), (10), (30)"))))
    (is (= [[4]] (mapv vec (query-rows c "SELECT nextval('s')")))
        "the failing row consumes its default, but the later row does not")
    (is (= [[1 10]]
           (mapv vec (query-rows c "SELECT id, external_key FROM t")))
        "the failed multi-row statement remains atomic")))

;; ============================================================================
;; PG semantics: nextval is non-transactional. The advance survives even
;; if the surrounding transaction rolls back.
;; ============================================================================

(deftest nextval-in-insert-survives-rollback
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE myseq")
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY, label TEXT)")
    (.setAutoCommit c false)
    (try
      (exec! c "INSERT INTO t VALUES (nextval('myseq'), 'will-rollback')")
      (.rollback c)
      (finally (.setAutoCommit c true)))
    ;; The row is gone, but the sequence advanced.
    (is (= 0 (count (query-rows c "SELECT id FROM t")))
        "Rolled-back row is not visible")
    ;; The next nextval is 2, not 1 — the rolled-back INSERT consumed slot 1.
    (with-open [stmt (.createStatement c)
                rs (.executeQuery stmt "SELECT nextval('myseq')")]
      (.next rs)
      (is (= 2 (.getLong rs 1))
          "Sequence advanced past the rolled-back nextval call"))))

(deftest nextval-after-an-earlier-transaction-write-commits
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE SEQUENCE s")
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY, label TEXT)")
    (exec! c "CREATE TABLE audit (id BIGINT PRIMARY KEY)")
    (.setAutoCommit c false)
    (try
      (exec! c "INSERT INTO audit VALUES (1)")
      (exec! c (str "INSERT INTO t VALUES (nextval('s'), 'kept') "
                    "ON CONFLICT(id) DO UPDATE SET label=EXCLUDED.label"))
      (.commit c)
      (finally (.setAutoCommit c true)))
    (is (= [[1 "kept"]] (mapv vec (query-rows c "SELECT id, label FROM t"))))
    (is (= [[1]] (mapv vec (query-rows c "SELECT id FROM audit"))))))

(deftest autocommit-upsert-does-not-reuse-stale-arbitration
  (let [a (pg/make-query-handler *conn*)
        b (pg/make-query-handler *conn*)
        state (fn [handler sql]
                (.-sqlstate ^PgWireServer$QueryResult (.execute handler sql)))]
    (is (nil? (state a "CREATE SEQUENCE s")))
    (is (nil? (state a (str "CREATE TABLE t (id BIGINT PRIMARY KEY "
                            "DEFAULT nextval('s'), k BIGINT UNIQUE)"))))
    (let [nextval-for-tx! @#'pg/nextval-for-tx!
          injected? (atom false)]
      (with-redefs-fn
        {#'pg/nextval-for-tx!
         (fn [& args]
           (let [value (apply nextval-for-tx! args)]
             (when (compare-and-set! injected? false true)
               (is (nil? (state b "INSERT INTO t(id,k) VALUES (99,1)"))))
             value))}
        #(is (= "40001"
                (state a (str "INSERT INTO t(k) VALUES (1) "
                              "ON CONFLICT(k) DO UPDATE SET k=excluded.k"))))))
    (is (= [["99" "1"]]
           (mapv vec
                 (.-rows ^PgWireServer$QueryResult
                  (.execute a "SELECT id,k FROM t")))))))

(deftest explicit-upsert-does-not-reuse-stale-arbitration
  (with-open [a (DriverManager/getConnection (jdbc-url *port*))
              b (DriverManager/getConnection (jdbc-url *port*))]
    (exec! a "CREATE SEQUENCE s")
    (exec! a (str "CREATE TABLE t (id BIGINT PRIMARY KEY DEFAULT nextval('s'), "
                  "k BIGINT UNIQUE)"))
    (.setAutoCommit a false)
    (let [nextval-for-tx! @#'pg/nextval-for-tx!
          injected? (atom false)]
      (with-redefs-fn
        {#'pg/nextval-for-tx!
         (fn [& args]
           (let [value (apply nextval-for-tx! args)]
             (when (compare-and-set! injected? false true)
               (exec! b "INSERT INTO t(id,k) VALUES (99,1)"))
             value))}
        #(exec! a (str "INSERT INTO t(k) VALUES (1) "
                       "ON CONFLICT(k) DO UPDATE SET k=excluded.k"))))
    (try
      (.commit a)
      (is false "stale materialized arbitration must not commit")
      (catch SQLException e
        (is (= "40001" (.getSQLState e)))))
    (.rollback a)
    (is (= [[99 1]] (mapv vec (query-rows b "SELECT id,k FROM t"))))))
