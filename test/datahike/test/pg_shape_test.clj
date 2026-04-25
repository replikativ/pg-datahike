(ns datahike.test.pg-shape-test
  "Tests for the structural SELECT-shape matcher. Exercises the
   catalog-probe predicates and, critically, the hostile cases
   substring-match was vulnerable to (keywords inside string
   literals, qualified identifiers inside comments)."
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.shape :as shape]
            [datahike.pg.classify :as cls]))

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

(deftest probe-fk-conname
  (is (= :get-fk-conname
         (shape/catalog-probe
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

(deftest probe-field-metadata
  (is (= :get-field-metadata
         (shape/catalog-probe
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

(deftest probe-empty-catalog
  (testing "schema-qualified catalog reference (pg_trigger — still unmaterialized)"
    (is (= :empty-catalog
           (shape/catalog-probe
            "SELECT * FROM pg_catalog.pg_trigger"))))
  (testing "pg_settings / pg_trigger still unmaterialized"
    (is (= :empty-catalog (shape/catalog-probe "SELECT * FROM pg_settings")))
    (is (= :empty-catalog (shape/catalog-probe "SELECT * FROM pg_trigger"))))
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
  (testing "'pg_constraint' literal in projection — not empty-catalog"
    (is (nil? (shape/catalog-probe
               "SELECT 'pg_constraint' AS name FROM users"))))
  (testing "'fk.conname' in a string — not fk-conname"
    (is (nil? (shape/catalog-probe
               "SELECT 'fk.conname AS name' AS q FROM users"))))
  (testing "'format_type' in a string — not empty-catalog"
    (is (nil? (shape/catalog-probe
               "SELECT 'format_type is cool' FROM users")))))

(deftest probe-keyword-in-comment-not-matched
  (testing "pg_constraint inside block comment — not empty-catalog"
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
;; Specificity ordering — named probes beat :empty-catalog
;; ============================================================================

(deftest probe-ordering
  (testing "fk-conname SELECT also references pg_constraint — fk-conname wins"
    (is (= :get-fk-conname
           (shape/catalog-probe
            "SELECT fk.conname AS name FROM pg_constraint fk"))))
  (testing "field-metadata SELECT references pg_class — field-metadata wins"
    (is (= :get-field-metadata
           (shape/catalog-probe
            "SELECT c.oid, a.attnum, a.attname
               FROM pg_catalog.pg_class c
               JOIN pg_catalog.pg_attribute a ON c.oid = a.attrelid")))))
