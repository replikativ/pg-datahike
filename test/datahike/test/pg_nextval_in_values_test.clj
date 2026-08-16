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
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

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
        (binding [*port* port] (f))
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

(deftest nextval-undefined-sequence-error
  (with-open [c (DriverManager/getConnection (jdbc-url *port*))]
    (exec! c "CREATE TABLE t (id BIGINT PRIMARY KEY)")
    ;; Reference a sequence that doesn't exist — should raise, not silently fail.
    (is (thrown? java.sql.SQLException
                 (exec! c "INSERT INTO t VALUES (nextval('nosuch'))")))))

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
