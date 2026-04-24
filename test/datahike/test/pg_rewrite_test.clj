(ns datahike.test.pg-rewrite-test
  "Tests for the token-driven SQL source rewriter. The core invariant
   is that each rule produces spans based on token kinds, so hostile
   inputs like `SELECT 'REFERENCES'` or `-- REFERENCES` do NOT trigger
   the REFERENCES stripper."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.rewrite :as rw]))

;; ============================================================================
;; inline-references-rule
;; ============================================================================

(defn- strip-refs [sql]
  (rw/rewrite sql [rw/inline-references-rule]))

(deftest inline-references-basic
  (testing "bare inline REFERENCES stripped"
    (is (= "CREATE TABLE c (id INT, pid INT  )"
           (strip-refs "CREATE TABLE c (id INT, pid INT REFERENCES p(id))"))))
  (testing "inline REFERENCES without cols"
    (is (= "CREATE TABLE c (id INT, pid INT  )"
           (strip-refs "CREATE TABLE c (id INT, pid INT REFERENCES p)")))))

(deftest inline-references-with-restrict-action-stripped
  (is (= "CREATE TABLE c (pid INT  )"
         (strip-refs "CREATE TABLE c (pid INT REFERENCES p(id) ON DELETE RESTRICT)")))
  (is (= "CREATE TABLE c (pid INT  )"
         (strip-refs "CREATE TABLE c (pid INT REFERENCES p(id) ON DELETE NO ACTION)"))))

(deftest inline-references-with-cascade-raises-0a000
  (doseq [action ["CASCADE" "SET NULL" "SET DEFAULT"]
          verb ["DELETE" "UPDATE"]]
    (testing (str "ON " verb " " action " rejected")
      (let [ex (try (strip-refs
                     (str "CREATE TABLE c (pid INT REFERENCES p(id) ON "
                          verb " " action ")"))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        (is (= "0A000" (:sqlstate (ex-data ex))))))))

(deftest inline-references-table-level-not-stripped
  (testing "FOREIGN KEY (col) REFERENCES … is preserved — preceded by )"
    (let [sql "CREATE TABLE c (pid INT, FOREIGN KEY (pid) REFERENCES p (id))"]
      (is (= sql (strip-refs sql))))))

(deftest inline-references-hostile-cases
  (testing "REFERENCES inside a string literal is NOT stripped"
    (let [sql "SELECT 'REFERENCES x' AS s"]
      (is (= sql (strip-refs sql)))))
  (testing "REFERENCES inside a block comment is NOT stripped"
    (let [sql "CREATE TABLE t (/* REFERENCES p(id) */ id INT)"]
      (is (= sql (strip-refs sql)))))
  (testing "REFERENCES inside a line comment is NOT stripped"
    (let [sql "-- REFERENCES p(id)\nSELECT 1"]
      (is (= sql (strip-refs sql))))))

;; ============================================================================
;; create-index-anonymous-rule
;; ============================================================================

(defn- anon-idx [sql]
  (rw/rewrite sql [rw/create-index-anonymous-rule]))

(deftest create-index-anonymous-injects-name
  (testing "CREATE INDEX ON t (col) gets a name"
    (let [out (anon-idx "CREATE INDEX ON t (col)")]
      (is (re-find #"^CREATE INDEX idx_auto_\d+ ON t \(col\)$" out))))
  (testing "CREATE UNIQUE INDEX ON t (col) also gets a name"
    (let [out (anon-idx "CREATE UNIQUE INDEX ON t (col)")]
      (is (re-find #"^CREATE UNIQUE INDEX idx_auto_\d+ ON t \(col\)$" out))))
  (testing "CREATE INDEX foo ON t (col) — name already present, no change"
    (let [sql "CREATE INDEX foo ON t (col)"]
      (is (= sql (anon-idx sql))))))

(deftest create-index-hostile-cases
  (testing "CREATE INDEX inside a string is NOT mutated"
    (let [sql "SELECT 'CREATE INDEX ON t (col)' AS s"]
      (is (= sql (anon-idx sql)))))
  (testing "CREATE INDEX inside a comment is NOT mutated"
    (let [sql "-- CREATE INDEX ON t (col)\nSELECT 1"]
      (is (= sql (anon-idx sql))))))

;; ============================================================================
;; select-from-rule
;; ============================================================================

(defn- sel-from [sql]
  (rw/rewrite sql [rw/select-from-rule]))

(deftest select-from-injects-one
  (is (= "SELECT 1 FROM t"
         (sel-from "SELECT FROM t")))
  (is (= "WHERE EXISTS (SELECT 1 FROM t WHERE x = 1)"
         (sel-from "WHERE EXISTS (SELECT FROM t WHERE x = 1)"))))

(deftest select-from-hostile-cases
  (testing "SELECT with projection is untouched"
    (let [sql "SELECT * FROM t"]
      (is (= sql (sel-from sql)))))
  (testing "'SELECT FROM' literal in a string — untouched"
    (let [sql "SELECT 'SELECT FROM t' AS q"]
      (is (= sql (sel-from sql)))))
  (testing "comment-wrapped — untouched"
    (let [sql "/* SELECT FROM hi */ SELECT 1"]
      (is (= sql (sel-from sql))))))

;; ============================================================================
;; Multi-rule composition
;; ============================================================================

(deftest multi-rule-composition
  (let [sql (str "CREATE TABLE c (pid INT REFERENCES p(id));\n"
                 "CREATE INDEX ON c (pid);\n"
                 "SELECT FROM c WHERE pid IS NOT NULL")]
    (let [out (rw/rewrite sql rw/default-rules)]
      (is (not (re-find #"\bREFERENCES\b" out)))
      (is (re-find #"idx_auto_\d+" out))
      (is (re-find #"SELECT 1 FROM" out)))))
