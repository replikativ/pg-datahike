(ns datahike.test.pg-shape-test
  "Tests for the structural SELECT-shape matcher. Exercises the
   catalog-probe predicates and, critically, the hostile cases
   substring-match was vulnerable to (keywords inside string
   literals, qualified identifiers inside comments)."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.shape :as shape]
            [datahike.pg.sql.classify :as cls]))

;; ============================================================================
;; summarize — structural extraction
;; ============================================================================

(defn- sum [sql] (shape/summarize (cls/tokenize sql)))

(deftest summarize-select-flag
  (is (true? (:select? (sum "SELECT 1"))))
  (is (true? (:select? (sum "  select  1"))))
  (is (true? (:select? (sum "/*c*/ SELECT 1"))))
  (is (false? (:select? (sum "INSERT INTO t VALUES (1)"))))
  (is (false? (:select? (sum "")))))

(deftest summarize-qrefs
  (testing "dotted names collected"
    (let [s (sum "SELECT pg_catalog.pg_class.oid FROM pg_catalog.pg_class")]
      (is (contains? (:qrefs s) "pg_catalog.pg_class.oid"))
      (is (contains? (:qrefs s) "pg_catalog.pg_class"))))
  (testing "bare idents stay in :idents not :qrefs"
    (let [s (sum "SELECT x FROM pg_constraint")]
      (is (contains? (:idents s) "pg_constraint"))
      (is (not (contains? (:qrefs s) "pg_constraint"))))))

(deftest summarize-as-aliases
  (is (contains? (:as-aliases (sum "SELECT fk.conname AS name FROM pg_constraint fk"))
                 ["fk.conname" "name"]))
  (testing "bare-ident AS alias"
    (is (contains? (:as-aliases (sum "SELECT count AS n FROM t"))
                   ["count" "n"])))
  (testing "implicit alias without AS is NOT captured (we only
            capture explicit AS)"
    (is (empty? (:as-aliases (sum "SELECT fk.conname name FROM t"))))))

(deftest summarize-function-calls
  (let [s (sum "SELECT format_type(atttypid, atttypmod), pg_get_constraintdef(oid) FROM x")]
    (is (contains? (:fn-names s) "format_type"))
    (is (contains? (:fn-names s) "pg_get_constraintdef"))
    (is (not (contains? (:fn-names s) "atttypid")))))

;; ============================================================================
;; catalog-probe — kind dispatch
;; ============================================================================

(deftest a-constraint-name-query-is-not-a-probe
  ;; It was: the handler answered `fk_<hash of the SQL>` -- a name no
  ;; catalog has. pg_constraint carries the real `conname`, so the query
  ;; runs like any other and answers `child_pid_fkey`.
  (is (nil? (shape/catalog-probe
             "SELECT fk.conname AS name FROM pg_constraint fk
               WHERE fk.conrelid = 'x'::regclass AND fk.contype = 'f'"))))

(deftest probe-primary-keys
  (is (= :get-primary-keys
         (shape/catalog-probe
          "SELECT NULL AS TABLE_CAT, n.nspname AS TABLE_SCHEM,
                  ct.relname AS TABLE_NAME, a.attname AS COLUMN_NAME,
                  (information_schema._pg_expandarray(i.indkey)).n AS KEY_SEQ,
                  ci.relname AS PK_NAME,
                  information_schema._pg_expandarray(i.indkey) AS KEYS,
                  a.attnum AS A_ATTNUM, result.key_seq AS KS,
                  result.pk_name AS PKN
             FROM pg_catalog.pg_class ct
             JOIN pg_catalog.pg_attribute a ON (ct.oid = a.attrelid)
            WHERE true"))))

(deftest a-column-metadata-query-is-not-a-probe
  ;; It was: a handler regexed the (oid, attnum) pairs out of pgjdbc's
  ;; inline UNION ALL and answered from the Datahike schema. The catalog
  ;; tables carry all of it -- pg_attrdef and pg_get_expr included, so
  ;; `… LIKE '%nextval(%'` finds a serial column's default -- and the
  ;; query answers exactly as PostgreSQL does through the ordinary SQL
  ;; path, once its LEFT JOIN works. That join is what the probe hid.
  (is (nil? (shape/catalog-probe
             "SELECT c.oid, a.attnum, a.attname, c.relname, n.nspname,
                     a.attnotnull OR (t.typtype = 'd' AND t.typnotnull),
                     a.attidentity != '' OR pg_catalog.pg_get_expr(def.adbin, def.adrelid) LIKE '%nextval(%'
                FROM pg_catalog.pg_class c
                JOIN pg_catalog.pg_namespace n ON (c.relnamespace = n.oid)
                JOIN pg_catalog.pg_attribute a ON (c.oid = a.attrelid)
                JOIN pg_catalog.pg_type t ON (a.atttypid = t.oid)
                LEFT JOIN pg_catalog.pg_attrdef def
                       ON (a.attrelid = def.adrelid AND a.attnum = def.adnum)
               WHERE (c.oid, a.attnum) IN ((16384, 1), (16384, 2))"))))

(deftest probe-catalogs-have-no-shape-shortcut
  ;; Every catalog a client reads is a real relation now, so no probe
  ;; answers from the SELECT list's shape: that shortcut made
  ;; `count(*) FROM pg_trigger` return zero rows labelled `?column?`.
  (testing "pg_trigger, schema-qualified or not"
    (is (nil? (shape/catalog-probe "SELECT * FROM pg_catalog.pg_trigger")))
    (is (nil? (shape/catalog-probe "SELECT count(*) FROM pg_trigger"))))
  (testing "pg_settings"
    (is (nil? (shape/catalog-probe "SELECT * FROM pg_settings"))))
  (testing "pg_constraint is now materialized — no shape-level shortcut"
    (is (nil? (shape/catalog-probe
               "SELECT n.nspname, c.conname FROM pg_constraint c
                 JOIN pg_namespace n ON c.connamespace = n.oid"))))
  (testing "pg_get_indexdef / pg_get_constraintdef now lower relationally — no shortcut"
    (is (nil? (shape/catalog-probe
               "SELECT pg_get_indexdef(oid) FROM pg_index WHERE indrelid = 16384")))
    (is (nil? (shape/catalog-probe
               "SELECT pg_get_constraintdef(oid) FROM pg_constraint")))))

(deftest probe-implemented-system-fns-do-not-intercept
  (testing "format_type is implemented in expr.clj — must reach the real SELECT path"
    (is (nil? (shape/catalog-probe
               "SELECT format_type(atttypid, atttypmod) AS t
                  FROM pg_attribute WHERE attrelid = 16384"))))
  (testing "obj_description / col_description are stubbed in expr.clj — same"
    (is (nil? (shape/catalog-probe
               "SELECT obj_description(oid, 'pg_class') FROM pg_class")))
    (is (nil? (shape/catalog-probe
               "SELECT col_description(attrelid, attnum) FROM pg_attribute")))))

(deftest probe-non-catalog-select-returns-nil
  (is (nil? (shape/catalog-probe "SELECT 1")))
  (is (nil? (shape/catalog-probe "SELECT * FROM users WHERE id = 1")))
  (is (nil? (shape/catalog-probe "SELECT count(*) FROM orders")))
  (is (nil? (shape/catalog-probe "INSERT INTO t VALUES (1)")))
  (is (nil? (shape/catalog-probe ""))))

;; ============================================================================
;; Hostile cases — strings and comments must NOT trigger probe matching
;; ============================================================================

(deftest probe-keyword-in-string-not-matched
  (testing "'pg_constraint' literal in projection — not matched"
    (is (nil? (shape/catalog-probe
               "SELECT 'pg_constraint' AS name FROM users"))))
  (testing "'format_type' in a string — not matched"
    (is (nil? (shape/catalog-probe
               "SELECT 'format_type is cool' FROM users")))))

(deftest probe-keyword-in-comment-not-matched
  (testing "pg_constraint inside block comment — not matched"
    (is (nil? (shape/catalog-probe
               "SELECT id /* from pg_constraint */ FROM users"))))
  (testing "pg_catalog.pg_class inside line comment — not field-metadata"
    (is (nil? (shape/catalog-probe
               "-- pg_catalog.pg_class c\nSELECT 1 FROM users")))))

(deftest probe-dollar-quoted-string
  (testing "pg_constraint inside $$...$$ literal — not matched"
    (is (nil? (shape/catalog-probe
               "SELECT $$SELECT FROM pg_constraint$$ AS q")))))

;; ============================================================================
;; Specificity ordering — the most specific named probe wins
;; ============================================================================

(deftest probe-ordering
  (testing "the primary-key probe still matches its own shape"
    (is (= :get-primary-keys
           (shape/catalog-probe
            "SELECT NULL AS TABLE_CAT,
                    (information_schema._pg_expandarray(i.indkey)).n AS KEY_SEQ,
                    result.key_seq AS KS, result.pk_name AS PKN
               FROM pg_catalog.pg_class ct
              WHERE true"))))
  (testing "a plain pg_class/pg_attribute join is nobody's probe"
    (is (nil? (shape/catalog-probe
               "SELECT c.oid, a.attnum, a.attname
                  FROM pg_catalog.pg_class c
                  JOIN pg_catalog.pg_attribute a ON c.oid = a.attrelid")))))
