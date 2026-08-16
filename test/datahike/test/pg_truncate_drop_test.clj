(ns datahike.test.pg-truncate-drop-test
  "TRUNCATE and multi-name DROP TABLE — the two statements stock
   `pgbench -i` sends that JSqlParser can't carry alone:

     drop table if exists pgbench_accounts, pgbench_branches, …
       JSqlParser 5.2's Drop grammar takes ONE name (ParseException at
       the comma), so classify.clj token-parses the list form into
       :drop-table-multi → parse-sql re-tags it :ddl-drop with :tables.

     truncate table pgbench_accounts, pgbench_branches, …
       JSqlParser parses a Truncate AST but parse-sql had no branch,
       and the grammar lacks RESTART/CONTINUE IDENTITY — classify.clj
       token-parses the whole statement into :truncate.

   Both interceptions are token-driven, so keywords inside string
   literals / comments / quoted identifiers must not trigger them."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.classify :as c])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

;; ============================================================================
;; Classifier — multi-name DROP TABLE
;; ============================================================================

(deftest classify-drop-table-multi
  (testing "the exact pgbench -i drop statement"
    (is (= {:kind :drop-table-multi
            :tables ["pgbench_accounts" "pgbench_branches"
                     "pgbench_history" "pgbench_tellers"]
            :if-exists? true}
           (c/classify "drop table if exists pgbench_accounts, pgbench_branches, pgbench_history, pgbench_tellers"))))
  (testing "without IF EXISTS"
    (is (= {:kind :drop-table-multi :tables ["a" "b"] :if-exists? false}
           (c/classify "DROP TABLE a, b"))))
  (testing "trailing CASCADE / RESTRICT accepted and ignored"
    (is (= :drop-table-multi (:kind (c/classify "DROP TABLE a, b CASCADE"))))
    (is (= :drop-table-multi (:kind (c/classify "DROP TABLE a, b RESTRICT")))))
  (testing "schema qualifier dropped, quoted identifiers unwrapped"
    (is (= ["a" "Weird Name"]
           (:tables (c/classify "DROP TABLE public.a, \"Weird Name\"")))))
  (testing "trailing semicolon tolerated"
    (is (= :drop-table-multi (:kind (c/classify "DROP TABLE a, b;"))))))

(deftest classify-drop-table-single-stays-generic
  (testing "single-name DROP TABLE keeps flowing through JSqlParser"
    (is (= {:kind :generic-sql} (c/classify "DROP TABLE a")))
    (is (= {:kind :generic-sql} (c/classify "DROP TABLE IF EXISTS a CASCADE")))
    (is (= {:kind :generic-sql} (c/classify "DROP SEQUENCE a, b")))))

(deftest classify-drop-table-string-and-comment-safety
  (testing "a comma-list inside a string literal is not a table list"
    (is (= {:kind :generic-sql} (c/classify "SELECT 'drop table a, b'")))
    ;; Malformed DROP whose 'names' are a string literal → generic-sql,
    ;; JSqlParser reports the real syntax error.
    (is (= {:kind :generic-sql} (c/classify "DROP TABLE 'a, b'"))))
  (testing "comments between names are invisible"
    (is (= {:kind :drop-table-multi :tables ["a" "b"] :if-exists? false}
           (c/classify "drop table -- x, y\n a, /* z, */ b")))))

(deftest classify-drop-table-long-list-not-truncated
  (testing "a name list far beyond the classifier's 64-token prefix"
    (let [names (mapv #(str "t" %) (range 60))
          sql (str "DROP TABLE " (str/join ", " names))]
      (is (= {:kind :drop-table-multi :tables names :if-exists? false}
             (c/classify sql))))))

;; ============================================================================
;; Classifier — TRUNCATE
;; ============================================================================

(deftest classify-truncate
  (testing "the exact pgbench -i truncate statement"
    (is (= {:kind :truncate
            :tables ["pgbench_accounts" "pgbench_branches"
                     "pgbench_history" "pgbench_tellers"]
            :restart-identity? false :cascade? false}
           (c/classify "truncate table pgbench_accounts, pgbench_branches, pgbench_history, pgbench_tellers"))))
  (testing "TABLE keyword optional; ONLY and * accepted and ignored"
    (is (= {:kind :truncate :tables ["a"] :restart-identity? false :cascade? false}
           (c/classify "TRUNCATE a")))
    (is (= ["a" "b"] (:tables (c/classify "TRUNCATE ONLY a *, ONLY b")))))
  (testing "RESTART / CONTINUE IDENTITY"
    (is (true?  (:restart-identity? (c/classify "TRUNCATE t RESTART IDENTITY"))))
    (is (false? (:restart-identity? (c/classify "TRUNCATE t CONTINUE IDENTITY")))))
  (testing "CASCADE / RESTRICT carried"
    (is (true?  (:cascade? (c/classify "TRUNCATE t CASCADE"))))
    (is (false? (:cascade? (c/classify "TRUNCATE t RESTRICT"))))
    (is (true?  (:cascade? (c/classify "TRUNCATE t RESTART IDENTITY CASCADE"))))))

(deftest classify-truncate-string-safety
  (testing "string literal is not a table name"
    (is (= {:kind :generic-sql} (c/classify "TRUNCATE 'a, b'"))))
  (testing "TRUNCATE mentioned inside a string elsewhere"
    (is (= :generic-sql (:kind (c/classify "SELECT 'truncate table x'"))))))

;; ============================================================================
;; Handler-level
;; ============================================================================

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec ^PgWireServer$QueryResult [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (mapv vec (.rows r))))

(defn- release! [{:keys [conn cfg]}]
  (d/release conn)
  (d/delete-database cfg))

(deftest truncate-single-and-multi-table
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE pgbench_accounts(aid int, bid int, abalance int)"))))
      (is (nil? (.error (exec h "CREATE TABLE pgbench_branches(bid int, bbalance int)"))))
      (is (nil? (.error (exec h "INSERT INTO pgbench_accounts(aid,bid,abalance) VALUES (1,1,0),(2,1,0)"))))
      (is (nil? (.error (exec h "INSERT INTO pgbench_branches(bid,bbalance) VALUES (1,0)"))))
      (testing "single table"
        (let [r (exec h "TRUNCATE pgbench_branches")]
          (is (nil? (.error r)))
          (is (= "TRUNCATE TABLE" (.commandTag r))))
        (is (= [["0"]] (rows (exec h "SELECT count(*) FROM pgbench_branches"))))
        (is (= [["2"]] (rows (exec h "SELECT count(*) FROM pgbench_accounts")))))
      (testing "exact pgbench multi-table statement — unknown tables treated as empty"
        (is (nil? (.error (exec h "INSERT INTO pgbench_branches(bid,bbalance) VALUES (1,0)"))))
        (let [r (exec h "truncate table pgbench_accounts, pgbench_branches, pgbench_history, pgbench_tellers")]
          (is (nil? (.error r)))
          (is (= "TRUNCATE TABLE" (.commandTag r))))
        (is (= [["0"]] (rows (exec h "SELECT count(*) FROM pgbench_accounts"))))
        (is (= [["0"]] (rows (exec h "SELECT count(*) FROM pgbench_branches")))))
      (testing "truncating an already-empty table is a no-op success"
        (is (nil? (.error (exec h "TRUNCATE pgbench_accounts")))))
      (testing "data reload after truncate"
        (is (nil? (.error (exec h "INSERT INTO pgbench_accounts(aid,bid,abalance) VALUES (9,1,0)"))))
        (is (= [["9"]] (rows (exec h "SELECT aid FROM pgbench_accounts")))))
      (finally (release! h)))))

(deftest truncate-inside-transaction
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE t(x int)"))))
      (is (nil? (.error (exec h "INSERT INTO t(x) VALUES (1),(2)"))))
      (testing "TRUNCATE then ROLLBACK restores the rows"
        (is (nil? (.error (exec h "BEGIN"))))
        (is (nil? (.error (exec h "TRUNCATE t"))))
        ;; speculative db sees the truncate…
        (is (= [["0"]] (rows (exec h "SELECT count(*) FROM t"))))
        (is (nil? (.error (exec h "ROLLBACK"))))
        ;; …the rollback discards it.
        (is (= [["2"]] (rows (exec h "SELECT count(*) FROM t")))))
      (testing "TRUNCATE then COMMIT applies it"
        (is (nil? (.error (exec h "BEGIN"))))
        (is (nil? (.error (exec h "TRUNCATE t"))))
        (is (nil? (.error (exec h "INSERT INTO t(x) VALUES (7)"))))
        (is (nil? (.error (exec h "COMMIT"))))
        (is (= [["7"]] (rows (exec h "SELECT x FROM t")))))
      (finally (release! h)))))

(deftest truncate-restart-identity
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE t(id INTEGER GENERATED ALWAYS AS IDENTITY, x INTEGER)"))))
      (is (nil? (.error (exec h "INSERT INTO t(x) VALUES (10),(11),(12)"))))
      (is (= [["1"] ["2"] ["3"]] (rows (exec h "SELECT id FROM t ORDER BY id"))))
      (testing "RESTART IDENTITY resets the backing sequence"
        (is (nil? (.error (exec h "TRUNCATE t RESTART IDENTITY"))))
        (is (nil? (.error (exec h "INSERT INTO t(x) VALUES (20)"))))
        (is (= [["1" "20"]] (rows (exec h "SELECT id, x FROM t")))))
      (testing "CONTINUE IDENTITY (the default) keeps counting"
        (is (nil? (.error (exec h "TRUNCATE t CONTINUE IDENTITY"))))
        (is (nil? (.error (exec h "INSERT INTO t(x) VALUES (30)"))))
        (is (= [["2" "30"]] (rows (exec h "SELECT id, x FROM t")))))
      (finally (release! h)))))

(deftest truncate-cascade-rejected
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE t(x int)"))))
      (let [r (exec h "TRUNCATE t CASCADE")]
        (is (some? (.error r)))
        (is (= "0A000" (.sqlstate r))))
      (finally (release! h)))))

(deftest drop-table-multi-handler
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE pgbench_accounts(aid int)"))))
      (is (nil? (.error (exec h "CREATE TABLE pgbench_branches(bid int)"))))
      (is (nil? (.error (exec h "INSERT INTO pgbench_accounts(aid) VALUES (1)"))))
      (testing "exact pgbench statement — existing tables dropped, missing are a no-op"
        (let [r (exec h "drop table if exists pgbench_accounts, pgbench_branches, pgbench_history, pgbench_tellers")]
          (is (nil? (.error r)))
          (is (= "DROP TABLE" (.commandTag r))))
        ;; both tables really gone → undefined-table on SELECT
        (is (some? (.error (exec h "SELECT * FROM pgbench_accounts"))))
        (is (some? (.error (exec h "SELECT * FROM pgbench_branches")))))
      (testing "IF EXISTS on an all-missing list is a plain no-op"
        (is (nil? (.error (exec h "drop table if exists pgbench_accounts, pgbench_branches")))))
      (testing "recreate after multi-drop works"
        (is (nil? (.error (exec h "CREATE TABLE pgbench_accounts(aid int)"))))
        (is (nil? (.error (exec h "INSERT INTO pgbench_accounts(aid) VALUES (2)"))))
        (is (= [["2"]] (rows (exec h "SELECT aid FROM pgbench_accounts")))))
      (finally (release! h)))))
