(ns datahike.test.pg-preprocess-test
  "Regression tests for `datahike.pg.sql/preprocess-sql`.

   Each rewrite must respect token kinds — pattern text appearing inside
   string literals or comments must be left intact. The legacy regex tail
   in preprocess-sql operated directly on the source string and could
   therefore mutate user data when a SQL keyword the regex matches
   happened to appear inside a literal value (Metabase saved questions,
   Odoo i18n strings, BI dashboard descriptions, etc.).

   Each `*-in-string-literal` test asserts that the rewrite leaves a
   literal value containing the keyword phrase unchanged. The
   matching `*-still-rewrites` test asserts the legitimate
   target-shape is still handled."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql :as sql]))

;; The fn under test is private; reach in via the var.
(def ^:private preprocess #'sql/preprocess-sql)

;; ============================================================================
;; COLLATE — qualified form (psql `\d` family emits `COLLATE pg_catalog.default`)
;; ============================================================================

(deftest collate-qualified-still-rewrites
  (testing "outside any literal, qualified COLLATE is stripped (collation tracking is unimplemented)"
    ;; Token rules leave a single space in the matched span — same convention
    ;; as the inline-references rule. Trailing whitespace is parser-irrelevant.
    (is (= "SELECT name FROM t WHERE name = 'x'  "
           (preprocess "SELECT name FROM t WHERE name = 'x' COLLATE pg_catalog.default")))))

(deftest collate-qualified-in-string-literal
  (testing "qualified COLLATE inside a string literal must be preserved"
    (let [sql "INSERT INTO t (note) VALUES ('see COLLATE pg_catalog.default for details')"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; COLLATE — bare form (`COLLATE "C"`)
;; ============================================================================

(deftest collate-bare-still-rewrites
  (testing "outside any literal, bare COLLATE is stripped"
    (is (= "SELECT * FROM t ORDER BY name  "
           (preprocess "SELECT * FROM t ORDER BY name COLLATE \"C\"")))))

(deftest collate-bare-in-string-literal
  (testing "bare COLLATE inside a string literal must be preserved"
    (let [sql "INSERT INTO t (note) VALUES ('use COLLATE C in PG')"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; OPERATOR(qual.op) — psql `\d` qualifier-stripped operator references
;; ============================================================================

(deftest operator-qual-still-rewrites
  (testing "outside any literal, OPERATOR(pg_catalog.~) collapses to ~"
    (is (= "SELECT * FROM t WHERE relname ~ '^x$'"
           (preprocess "SELECT * FROM t WHERE relname OPERATOR(pg_catalog.~) '^x$'")))))

(deftest operator-qual-in-string-literal
  (testing "OPERATOR(qual.op) inside a string literal must be preserved"
    (let [sql "INSERT INTO docs (body) VALUES ('use OPERATOR(pg_catalog.~) for regex')"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; ALTER COLUMN … DROP DEFAULT
;; ============================================================================

(deftest alter-column-drop-default-still-rewrites
  (testing "ALTER COLUMN x DROP DEFAULT clause is removed (no-op in our schema)"
    (is (= "ALTER TABLE t   ADD COLUMN y INT"
           (preprocess "ALTER TABLE t ALTER COLUMN x DROP DEFAULT, ADD COLUMN y INT")))))

(deftest alter-column-drop-default-in-string-literal
  (testing "ALTER COLUMN ... DROP DEFAULT inside a string literal must be preserved"
    (let [sql "INSERT INTO logs (msg) VALUES ('after ALTER COLUMN status DROP DEFAULT, restart')"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; (PRIMARY KEY(col)) — PG-only PK-only inherits body
;; ============================================================================

(deftest primary-key-paren-still-rewrites
  (testing "(PRIMARY KEY(col)) body is replaced with (id serial) for INHERITS"
    (is (= "CREATE TABLE child (id serial) INHERITS (parent)"
           (preprocess "CREATE TABLE child (PRIMARY KEY(child_id)) INHERITS (parent)")))))

(deftest primary-key-paren-in-string-literal
  (testing "(PRIMARY KEY (...)) inside a string literal must be preserved"
    (let [sql "INSERT INTO docs (body) VALUES ('declare with (PRIMARY KEY (id)) syntax')"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; ALTER TABLE … TYPE … USING
;; ============================================================================

(deftest type-using-still-rewrites
  (testing "ALTER COLUMN ... TYPE int USING expr — USING half stripped"
    (is (= "ALTER TABLE t ALTER COLUMN x TYPE int  "
           (preprocess "ALTER TABLE t ALTER COLUMN x TYPE int USING x::int")))))

(deftest type-using-in-string-literal
  (testing "the substring 'TYPE x USING …' inside a literal must not be truncated"
    (let [sql "INSERT INTO docs (body) VALUES ('avoid TYPE int USING expr in clauses');"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; statement_timeout — the parser lives in server.clj but the same
;; literal-not-in-string invariant applies. See pg-server-test for the
;; round-trip tests; here we only assert that the substring inside a
;; literal is not visible to the timeout extractor.
;; ============================================================================

(deftest statement-timeout-in-string-literal-not-extracted
  (testing "a SELECT whose literal mentions `set statement_timeout = 5s` must NOT trigger a timeout"
    (let [parse #'datahike.pg.server/parse-statement-timeout
          sql "SELECT * FROM t WHERE note = 'set statement_timeout to 5s'"]
      (is (nil? (parse sql))))))

(deftest statement-timeout-set-extracted
  (testing "real SET statement_timeout extracts the milliseconds value"
    (let [parse #'datahike.pg.server/parse-statement-timeout]
      (is (= 5000 (parse "SET statement_timeout = 5000")))
      (is (= 5000 (parse "SET statement_timeout = '5s'")))
      (is (= 0 (parse "SET statement_timeout TO DEFAULT")))
      (is (= 0 (parse "RESET statement_timeout"))))))

(deftest statement-timeout-startup-option-extracted
  (let [parse #'datahike.pg.server/parse-startup-statement-timeout]
    (is (= 5000 (parse "-c statement_timeout=5s")))
    (is (= 250 (parse "-c search_path=public -cstatement_timeout=250ms")))
    (is (nil? (parse "-c search_path=public")))))

;; ============================================================================
;; INDEX/KEY varchar — quote reserved-word column names
;; ============================================================================

(deftest index-varchar-still-rewrites
  (testing "reserved-word column name `index varchar` is quoted to `\"index\" varchar`"
    (is (= "CREATE TABLE t (\"index\" varchar)"
           (preprocess "CREATE TABLE t (index varchar)"))))
  (testing "same for KEY"
    (is (= "CREATE TABLE t (\"key\" varchar)"
           (preprocess "CREATE TABLE t (key varchar)")))))

(deftest index-varchar-in-string-literal
  (testing "the phrase `index varchar` inside a literal must not be quoted"
    (let [sql "INSERT INTO docs (body) VALUES ('the index varchar pattern')"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; statement_timeout regex in server.clj — separate harness target
;; (covered indirectly: the SET statement_timeout regex sits in server.clj,
;;  not preprocess-sql. Track-via-test once we move it into classify.)

;; ============================================================================
;; Sanity: the existing token-rule rewrites already handle string literals.
;; This locks in the token-aware path's hostile-input behaviour, in case a
;; future refactor moves it back to a regex.
;; ============================================================================

(deftest references-in-literal-already-safe
  (testing "REFERENCES in a string literal — handled by token-driven rule"
    (let [sql "INSERT INTO docs (body) VALUES ('REFERENCES are forward declarations')"]
      (is (= sql (preprocess sql))))))

(deftest collate-in-line-comment
  (testing "COLLATE in a -- line comment must be preserved"
    (let [sql "SELECT 1 -- note: COLLATE pg_catalog.default not used\nFROM t"]
      (is (= sql (preprocess sql))))))

(deftest collate-in-block-comment
  (testing "COLLATE in a /* block comment */ must be preserved"
    (let [sql "SELECT 1 /* COLLATE \"C\" was here */ FROM t"]
      (is (= sql (preprocess sql))))))

;; ============================================================================
;; CREATE SEQUENCE is NOT rewritten any more — it is token-classified in
;; full (classify/classify-create-sequence) and never reaches JSqlParser,
;; so IF NOT EXISTS and NO MINVALUE survive preprocessing untouched.
;; ============================================================================

(deftest create-sequence-is-not-preprocessed
  (testing "IF NOT EXISTS is left in place for the classifier to read"
    (let [sql "CREATE SEQUENCE IF NOT EXISTS foo START WITH 5"]
      (is (= sql (preprocess sql)))))
  (testing "NO MINVALUE / NO MAXVALUE / NO CYCLE survive too"
    (let [sql "CREATE SEQUENCE foo NO MINVALUE NO MAXVALUE NO CYCLE"]
      (is (= sql (preprocess sql))))))

(deftest create-sequence-in-string-literal
  (testing "the phrase inside a string literal must be preserved"
    (let [sql "INSERT INTO t (note) VALUES ('CREATE SEQUENCE IF NOT EXISTS foo')"]
      (is (= sql (preprocess sql))))))
