(ns datahike.test.pg-rewrite-test
  "Tests for the token-driven SQL source rewriter. The core invariant
   is that each rule produces spans based on token kinds, so hostile
   inputs like `SELECT 'REFERENCES'` or `-- REFERENCES` do NOT trigger
   the REFERENCES lifter."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.rewrite :as rw]))

;; ============================================================================
;; inline-references-rule
;; ============================================================================

(defn- strip-refs [sql]
  (rw/rewrite sql [rw/inline-references-rule]))

(deftest inline-references-basic
  (testing "bare inline REFERENCES is lifted to a table constraint"
    (let [out (strip-refs "CREATE TABLE c (id INT, pid INT REFERENCES p(id))")]
      (is (re-find #"FOREIGN KEY\s*\(\s*pid\s*\)\s+REFERENCES\s+p\s*\(\s*id\s*\)" out))
      (is (not (re-find #"INT\s+REFERENCES" out)))))
  (testing "omitted parent columns survive parsing through an internal sentinel"
    (let [out (strip-refs "CREATE TABLE c (id INT, pid INT REFERENCES p)")]
      (is (re-find #"REFERENCES\s+p\s*\(\s*__pg_default_pk__\s*\)" out)))))

(deftest inline-references-with-restrict-action-lifted
  (doseq [action ["RESTRICT" "NO ACTION"]]
    (let [out (strip-refs
               (str "CREATE TABLE c (pid INT REFERENCES p(id) ON DELETE " action ")"))]
      (is (re-find #"FOREIGN KEY\s*\(\s*pid\s*\)" out))
      (is (str/includes? out (str "ON DELETE " action))))))

(deftest inline-references-on-delete-cascade-lifts-to-table-fk
  (testing "ON DELETE CASCADE — inline form is lifted to a table-level
            FOREIGN KEY clause appended before the closing `)`. The
            inline REFERENCES is stripped (so JSqlParser parses the
            column normally) and the runtime FK plumbing then tracks
            cascade behavior."
    (let [out (strip-refs
               "CREATE TABLE c (pid INT REFERENCES p(id) ON DELETE CASCADE)")]
      (is (re-find #"FOREIGN KEY\s*\(\s*pid\s*\)\s+REFERENCES" out))
      (is (re-find #"ON DELETE CASCADE" out))
      ;; The original inline `REFERENCES p(id) ON DELETE CASCADE` is gone
      ;; from the column position — only the appended table-level FK
      ;; carries it.
      (is (not (re-find #"INT\s+REFERENCES" out))))))

(deftest inline-references-with-unsupported-action-raises-0a000
  (doseq [[action verb] [["SET NULL" "DELETE"]
                         ["SET DEFAULT" "DELETE"]
                         ["CASCADE" "UPDATE"]
                         ["SET NULL" "UPDATE"]
                         ["SET DEFAULT" "UPDATE"]]]
    (testing (str "ON " verb " " action " rejected (not yet implemented)")
      (let [ex (try (strip-refs
                     (str "CREATE TABLE c (pid INT REFERENCES p(id) ON "
                          verb " " action ")"))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        ;; Resolve through the wire-boundary classifier — that's the
        ;; contract clients see, not the raw ex-data shape. Throw sites
        ;; describe errors structurally via :error keys; classifier
        ;; maps to SQLSTATE.
        (is (= "0A000" (first (datahike.pg.errors/classify-exception ex))))))))

(deftest inline-references-table-level-not-stripped
  (testing "FOREIGN KEY (col) REFERENCES … is preserved — preceded by )"
    (let [sql "CREATE TABLE c (pid INT, FOREIGN KEY (pid) REFERENCES p (id))"]
      (is (= sql (strip-refs sql))))))

;; ============================================================================
;; boolean-is-rule — wrap LHS of `IS [NOT] (TRUE|FALSE|UNKNOWN)`
;; ============================================================================

(defn- bool-is [sql] (rw/rewrite sql [rw/boolean-is-rule]))

(deftest boolean-is-wraps-in-expression
  (testing "x IN (...) IS NOT TRUE — IN's parens belong to IN, not to a boolean primary"
    (is (= "SELECT * FROM t WHERE (x IN (1)) IS NOT TRUE"
           (bool-is "SELECT * FROM t WHERE x IN (1) IS NOT TRUE"))))
  (testing "the canonical Odoo view-loading shape"
    (let [sql "WHERE md.module IN (SELECT name FROM ir_module_module) IS NOT TRUE"]
      (is (= "WHERE (md.module IN (SELECT name FROM ir_module_module)) IS NOT TRUE"
             (bool-is sql))))))

(deftest boolean-is-wraps-comparison
  (testing "x = 5 IS TRUE — comparison parses standalone but trips with IS"
    (is (= "SELECT * FROM t WHERE (x = 5) IS TRUE"
           (bool-is "SELECT * FROM t WHERE x = 5 IS TRUE")))))

(deftest boolean-is-wraps-exists
  (is (= "SELECT * FROM t WHERE (EXISTS (SELECT 1 FROM s)) IS TRUE"
         (bool-is "SELECT * FROM t WHERE EXISTS (SELECT 1 FROM s) IS TRUE"))))

(deftest boolean-is-already-parenthesised-not-rewrapped
  (testing "leave a single existing parenthesised group alone"
    (let [sql "SELECT * FROM t WHERE (x IN (1)) IS NOT TRUE"]
      (is (= sql (bool-is sql))))))

(deftest boolean-is-stops-at-AND-OR
  (testing "AND boundary: only the right-hand operand gets wrapped"
    (is (= "SELECT * FROM t WHERE x = 'foo' AND (y IN (1)) IS NOT TRUE"
           (bool-is "SELECT * FROM t WHERE x = 'foo' AND y IN (1) IS NOT TRUE")))))

(deftest boolean-is-hostile-cases
  (testing "IS NOT TRUE inside a string literal is NOT rewritten"
    (let [sql "SELECT 'IN (1) IS NOT TRUE' AS s FROM t"]
      (is (= sql (bool-is sql)))))
  (testing "IS NOT TRUE inside a block comment is NOT rewritten"
    (let [sql "SELECT 1 /* x IN (1) IS NOT TRUE */ FROM t"]
      (is (= sql (bool-is sql))))))

;; ============================================================================
;; inline-references — hostile cases (cross-cutting rule check)
;; ============================================================================

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

(deftest create-table-as-select-is-not-an-alias
  (testing "the statement-level AS introduces a query, not an alias named select"
    (let [sql "CREATE TABLE copied AS SELECT id FROM source"]
      (is (= sql (rw/rewrite sql [rw/quote-reserved-alias-rule])))))
  (testing "reserved projection aliases are still quoted"
    (is (= "SELECT 1 AS \"select\""
           (rw/rewrite "SELECT 1 AS select" [rw/quote-reserved-alias-rule])))))

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
      (is (re-find #"FOREIGN KEY\s*\(\s*pid\s*\)\s+REFERENCES" out))
      (is (re-find #"idx_auto_\d+" out))
      (is (re-find #"SELECT 1 FROM" out)))))

(deftest negative-numeric-scale-is-packed-for-jsqlparser
  (let [rewrite #(rw/rewrite % [rw/negative-numeric-scale-rule])]
    (is (= "CREATE TABLE t (n numeric(3,2042), d decimal(4,2045))"
           (rewrite "CREATE TABLE t (n numeric(3,-6), d decimal(4,-3))")))
    (testing "ordinary signs and opaque source regions are untouched"
      (doseq [sql ["SELECT -6::numeric"
                   "SELECT 'numeric(3,-6)'"
                   "-- numeric(3,-6)\nSELECT 1"
                   "CREATE TABLE t (n numeric(3,-1001))"
                   "CREATE TABLE t (n numeric(3,6))"]]
        (is (= sql (rewrite sql)))))))

(deftest wide-integer-literals-use-parser-safe-numeric-notation
  (let [rewrite #(rw/rewrite % [rw/wide-integer-literal-rule])]
    (is (= "SELECT 9223372036854775807, 9223372036854775808e0"
           (rewrite "SELECT 9223372036854775807, 9223372036854775808")))
    (is (= "SELECT -9999999999999999999999e0::numeric"
           (rewrite "SELECT -9999999999999999999999::numeric")))
    (is (= "SELECT '9999999999999999999999', 1.0e30"
           (rewrite "SELECT '9999999999999999999999', 1.0e30")))))

(deftest dollar-quoted-strings-become-parser-safe-literals
  (let [rewrite #(rw/rewrite % [rw/dollar-quoted-string-rule])]
    (testing "untagged and tagged literals preserve their bodies"
      (is (= "SELECT 'abc', 'it''s json'"
             (rewrite "SELECT $$abc$$, $json$it's json$json$")))
      (is (= "SELECT '', 'a\n\\b'"
             (rewrite "SELECT $$$$, $body$a\n\\b$body$"))))
    (testing "parameters and opaque source regions remain untouched"
      (is (= "SELECT $1, '$$inside$$', \"$identifier\" -- $$comment$$\n"
             (rewrite "SELECT $1, '$$inside$$', \"$identifier\" -- $$comment$$\n"))))
    (testing "an opener without its exact closer is never repaired into valid SQL"
      (doseq [sql ["SELECT $$abc" "SELECT $tag$abc$wrong$"]]
        (is (= sql (rewrite sql)))))))

;; ============================================================================
;; quote-reserved-alias-rule
;;
;; PG accepts `SELECT 1 AS select` (alias named after a reserved word).
;; JSqlParser 5 rejects it — the rule double-quotes the alias so the
;; SQL text round-trips through parse + equivalent semantics on PG.
;; ============================================================================

(defn- quote-alias [sql]
  (rw/rewrite sql [rw/quote-reserved-alias-rule]))

(deftest quote-reserved-alias-basic
  (testing "AS <hard-keyword> wrapped in double-quotes"
    (is (= "SELECT 1 AS \"select\"" (quote-alias "SELECT 1 AS select")))
    (is (= "SELECT 1 AS \"from\""   (quote-alias "SELECT 1 AS from")))
    (is (= "SELECT 1 AS \"where\""  (quote-alias "SELECT 1 AS where")))
    (is (= "SELECT 1 AS \"when\""   (quote-alias "SELECT 1 AS when")))
    (is (= "SELECT 1 AS \"join\""   (quote-alias "SELECT 1 AS join")))
    (is (= "SELECT 1 AS \"sample\"" (quote-alias "SELECT 1 AS sample")))))

(deftest quote-sample-alias-reference
  (is (= "SELECT \"sample\".x FROM t AS \"sample\""
         (quote-alias "SELECT sample.x FROM t AS sample"))))

(deftest quote-reserved-alias-folds-case
  ;; This assertion is REVERSED from what it used to be, and the old
  ;; expectation was wrong on its own terms: its inputs are UNQUOTED
  ;; aliases, which PostgreSQL folds to lower case. The rewrite adds
  ;; quotes purely to get a reserved word past the parser; treating those
  ;; synthetic quotes as the user's own made `SELECT 1 AS Select` come
  ;; back labelled `Select` where PostgreSQL says `select`. The old
  ;; docstring — "double-quoted aliases are case-sensitive" — described
  ;; an input the test never had, and contradicted
  ;; pg-column-naming-test/unquoted-alias-is-down-cased.
  (testing "an unquoted reserved-word alias folds, like any unquoted identifier"
    (is (= "SELECT 1 AS \"select\"" (quote-alias "SELECT 1 AS Select")))
    (is (= "SELECT 1 AS \"select\"" (quote-alias "SELECT 1 AS SELECT"))))
  (testing "a genuinely quoted alias keeps its case — it never reaches this rule"
    (is (= "SELECT 1 AS \"Select\"" (quote-alias "SELECT 1 AS \"Select\"")))))

(deftest create-view-as-select-is-not-an-alias
  (is (= "CREATE VIEW v AS SELECT 1 AS \"select\""
         (quote-alias "CREATE VIEW v AS SELECT 1 AS select"))))

(deftest quote-reserved-alias-skip-cast
  (testing "`CAST(x AS int)` — AS introduces type, not alias, leave it"
    (is (= "SELECT CAST(1 AS int) FROM t"
           (quote-alias "SELECT CAST(1 AS int) FROM t")))
    (is (= "SELECT CAST(name AS text) FROM t"
           (quote-alias "SELECT CAST(name AS text) FROM t"))))
  (testing "Nested CAST — still skip"
    (is (= "SELECT CAST(CAST(x AS int) AS bigint)"
           (quote-alias "SELECT CAST(CAST(x AS int) AS bigint)"))))
  (testing "CAST(... AS int) AS select — only the outer AS is an alias"
    (is (= "SELECT CAST(1 AS int) AS \"select\" FROM t"
           (quote-alias "SELECT CAST(1 AS int) AS select FROM t")))))

(deftest quote-reserved-alias-skip-non-reserved
  (testing "Non-reserved keyword aliases untouched"
    (is (= "SELECT 1 AS n"        (quote-alias "SELECT 1 AS n")))
    (is (= "SELECT 1 AS total"    (quote-alias "SELECT 1 AS total")))
    (is (= "SELECT 1 AS update"   (quote-alias "SELECT 1 AS update")))))

(deftest quote-reserved-alias-skip-inside-strings
  (testing "Keyword inside a string literal untouched"
    (let [sql "SELECT 'AS select' AS label FROM t"]
      (is (= sql (quote-alias sql))))))

(deftest quote-reserved-alias-skip-inside-comments
  (testing "AS <kw> inside a comment untouched"
    (let [sql "SELECT 1 /* AS select */ FROM t"]
      (is (= sql (quote-alias sql))))))

(deftest quote-reserved-alias-multiple-aliases
  (testing "Multi-column SELECT with several reserved aliases"
    (is (= "SELECT 1 AS \"select\", 2 AS \"from\", 3 AS ok"
           (quote-alias "SELECT 1 AS select, 2 AS from, 3 AS ok")))))

(deftest quote-reserved-alias-metabase-privilege-query
  (testing "Shape from Metabase's build_privilege_map"
    (let [sql (str "with table_privileges as ("
                   " select NULL as role, t.schemaname as schema, t.objectname as table,"
                   "   f(t.sn, 'update') as update,"
                   "   f(t.sn, 'select') as select"
                   " from tabs t)"
                   " select * from table_privileges")
          out (quote-alias sql)]
      ;; role, schema, table aren't in our reserved list → unchanged.
      ;; update, select ARE in our list (well update is not… let me
      ;; check: only the keywords JSqlParser 5.2 rejects are in the
      ;; set. Metabase's SELECT has both; this test captures the
      ;; behaviour on the SELECT alias specifically.
      (is (re-find #"as \"select\"" out))
      (is (not (re-find #"as \"role\"" out)))
      (is (not (re-find #"as \"schema\"" out))))))

(deftest quote-reserved-alias-with-already-quoted
  (testing "AS \"already-quoted\" untouched — tokenizer emits a distinct token kind"
    ;; The tokenizer emits quoted idents; our match requires :text in
    ;; the reserved set, which is the unquoted form only.
    (let [sql "SELECT 1 AS \"select\""]
      (is (= sql (quote-alias sql))))))

(deftest quote-reserved-alias-integrates-with-default-rules
  (testing "quote-reserved-alias-rule is part of default-rules"
    (let [sql "SELECT 1 AS select"]
      (is (= "SELECT 1 AS \"select\""
             (rw/rewrite sql rw/default-rules))))))

;; NOTE: the two CREATE SEQUENCE rewrite rules that used to be exercised
;; here (IF NOT EXISTS and NO MINVALUE/MAXVALUE/CYCLE stripping) are gone.
;; Sequence DDL is token-classified in full now and never reaches
;; JSqlParser, so there is nothing to rewrite — see pg-sequence-ddl-test.

;; ============================================================================
;; partition-by-clause / partition-by-rule — the clause is lifted out for
;; the parser; refusing it (or not, under :pg-dump) is the executor's call
;; ============================================================================

(deftest partition-by-clause-is-detected-and-lifted
  (doseq [[sql strategy] [["CREATE TABLE p (a int, b date) PARTITION BY RANGE (b)" "RANGE"]
                          ["CREATE TABLE p (a int) PARTITION BY LIST (a);" "LIST"]
                          ["create table p (a int) partition by hash ((a + 1))" "HASH"]]]
    (is (= strategy (:strategy (rw/partition-by-clause (cls/tokenize-all sql)))) sql)
    (let [out (rw/rewrite sql [rw/partition-by-rule])]
      (is (not (re-find #"(?i)partition" out)) sql)
      (is (re-find #"^(?i)create table p \(a int" out) sql))))

(deftest partition-by-clause-ignores-window-and-non-ddl-uses
  (doseq [sql ["SELECT g, sum(v) OVER (PARTITION BY g) FROM w"
               "SELECT rank() OVER (PARTITION BY range (x)) FROM w"
               "CREATE TABLE w2 AS SELECT count(*) OVER (PARTITION BY list (g)) FROM w"
               "CREATE TABLE t (note text DEFAULT 'PARTITION BY RANGE (x)')"
               "SELECT 'CREATE TABLE p (a int) PARTITION BY RANGE (a)'"]]
    (is (nil? (rw/partition-by-clause (cls/tokenize-all sql))) sql)
    (is (= sql (rw/rewrite sql [rw/partition-by-rule])) sql)))

;; ============================================================================
;; order-by-using-rule
;; ============================================================================

(defn- order-using [sql] (rw/rewrite sql [rw/order-by-using-rule]))

(deftest order-by-using-becomes-a-direction
  (testing "the btree ordering operators are exactly ASC and DESC"
    (is (= "SELECT x FROM t ORDER BY x ASC"  (order-using "SELECT x FROM t ORDER BY x USING <")))
    (is (= "SELECT x FROM t ORDER BY x DESC" (order-using "SELECT x FROM t ORDER BY x USING >")))
    (is (= "SELECT x FROM t ORDER BY x ASC"  (order-using "SELECT x FROM t ORDER BY x USING <=")))
    (is (= "SELECT x FROM t ORDER BY x DESC" (order-using "SELECT x FROM t ORDER BY x USING >="))))
  (testing "every key of a multi-column ORDER BY"
    (is (= "SELECT x FROM t ORDER BY a DESC, b ASC"
           (order-using "SELECT x FROM t ORDER BY a USING >, b USING <"))))
  (testing "any other operator is left alone -- ordering by it is not a direction"
    (is (= "SELECT x FROM t ORDER BY x USING ~<~"
           (order-using "SELECT x FROM t ORDER BY x USING ~<~"))))
  (testing "USING outside an ORDER BY is untouched"
    (is (= "DELETE FROM a USING b WHERE a.i = b.i"
           (order-using "DELETE FROM a USING b WHERE a.i = b.i")))
    ;; the ORDER BY has ended by the time this USING appears
    (is (= "SELECT x FROM t ORDER BY x LIMIT 1"
           (order-using "SELECT x FROM t ORDER BY x LIMIT 1")))))

;; ============================================================================
;; select-into-table-rule
;; ============================================================================

(deftest select-into-lifts-to-create-table-as
  (let [pre #(rw/rewrite % [rw/select-into-table-rule])]
    (is (= "CREATE TABLE t AS SELECT x  FROM s" (pre "SELECT x INTO t FROM s")))
    (testing "an INTO after FROM is not this clause"
      (is (= "SELECT x FROM s ORDER BY x" (pre "SELECT x FROM s ORDER BY x"))))
    (testing "a nested SELECT's INTO is not the statement's"
      (is (= "SELECT (SELECT 1) AS x FROM s" (pre "SELECT (SELECT 1) AS x FROM s"))))
    (testing "only a SELECT is rewritten"
      (is (= "INSERT INTO t SELECT x FROM s" (pre "INSERT INTO t SELECT x FROM s"))))))

;; ============================================================================
;; adjacent-string-literal-rule
;; ============================================================================

(deftest adjacent-string-literals-fold-into-one
  (let [pre #(rw/rewrite % [rw/adjacent-string-literal-rule])]
    (is (= "SELECT 'ab'" (pre "SELECT 'a'\n'b'")))
    (testing "a run of any length"
      (is (= "SELECT 'abc'" (pre "SELECT 'a'\n'b'\n'c'"))))
    (testing "an empty literal in the run"
      (is (= "SELECT 'ab'" (pre "SELECT 'a'\n''\n'b'"))))
    (testing "literals that are not adjacent are left alone"
      (is (= "SELECT 'a', 'b'" (pre "SELECT 'a', 'b'")))
      (is (= "SELECT 'a' || 'b'" (pre "SELECT 'a' || 'b'"))))))

;; ============================================================================
;; quote-reserved-alias-rule — words JSqlParser reserves and PostgreSQL does not
;; ============================================================================

(deftest reserved-words-usable-as-column-labels
  (doseq [w ["constraint" "distinct" "minus" "semi" "unbounded" "unique"]]
    (let [out (rw/rewrite (str "SELECT 1 AS " w) rw/default-rules)]
      (is (str/includes? out (str "\"" w "\""))
          (str w " should be quoted so it survives the parser")))))
