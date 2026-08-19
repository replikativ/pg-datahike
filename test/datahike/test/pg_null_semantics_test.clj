(ns datahike.test.pg-null-semantics-test
  "PostgreSQL three-valued logic tests for the pg-server SQL engine.

   Datahike's EAV model represents NULL as the absence of an attribute
   assertion. col-var! emits `[(get-else $ ?e :ns/col :__null__) ?v]`
   and predicate emitters prepend a `(not= ?v :__null__)` guard where
   needed so that `col op V` on a NULL col yields UNKNOWN → FALSE in
   WHERE, matching SQL semantics."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

;; ============================================================================
;; Fixture: `t` table with (id long, val long, tag string, flag boolean),
;; rows where some columns are absent (NULL in SQL terms).
;; ============================================================================

(def nsem-schema
  [{:db/ident       :t/id
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity}
   {:db/ident       :t/val
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident       :t/tag
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :t/flag
    :db/valueType   :db.type/boolean
    :db/cardinality :db.cardinality/one}])

;; id=1 has all attrs set; id=2 has val/tag/flag missing; id=3 has val=50, tag="aa", flag=false
(def nsem-data
  [{:t/id 1 :t/val 10  :t/tag "hello"  :t/flag true}
   {:t/id 2}
   {:t/id 3 :t/val 50  :t/tag "aa"     :t/flag false}])

(def ^:dynamic *h* nil)

(defn nsem-fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn nsem-schema)
      (d/transact conn nsem-data)
      (try
        (binding [*h* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each nsem-fixture)

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (vec (map vec (.rows r)))))

(defn- err [^PgWireServer$QueryResult r] (.error r))

(defn- ids [r]
  (vec (sort (map #(Long/parseLong (first %)) (rows r)))))

;; ============================================================================
;; Comparison predicates: SQL says `col op V` is UNKNOWN when col IS NULL,
;; and WHERE treats UNKNOWN as FALSE. The null-guard in translate-comparison
;; must exclude id=2 (val/tag/flag missing) from all of the below.
;; ============================================================================

(deftest test-comparison-null-excluded
  (testing "= excludes NULL rows"
    (let [r (.execute *h* "SELECT id FROM t WHERE val = 10 ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1] (ids r)))))

  (testing "<> excludes NULL rows (not 2 — NULL != 10 is UNKNOWN, not TRUE)"
    (let [r (.execute *h* "SELECT id FROM t WHERE val <> 10 ORDER BY id")]
      (is (nil? (err r)))
      (is (= [3] (ids r)))))

  (testing "< excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE val < 20 ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1] (ids r)))))

  (testing "> excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE val > 5 ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1 3] (ids r)))))

  (testing ">= excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE val >= 10 ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1 3] (ids r)))))

  (testing "<= excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE val <= 10 ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1] (ids r))))))

;; ============================================================================
;; IS NULL / IS NOT NULL: the only SQL constructs that evaluate NULL to a
;; known TRUE/FALSE. id=2 is the only NULL row for val.
;; ============================================================================

(deftest test-is-null
  (testing "IS NULL matches the missing-attr row"
    (let [r (.execute *h* "SELECT id FROM t WHERE val IS NULL ORDER BY id")]
      (is (nil? (err r)))
      (is (= [2] (ids r)))))

  (testing "IS NOT NULL matches the other rows"
    (let [r (.execute *h* "SELECT id FROM t WHERE val IS NOT NULL ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1 3] (ids r))))))

;; ============================================================================
;; BETWEEN: the null-guard-clauses branch in the Between handler must exclude
;; NULL cols.
;; ============================================================================

(deftest test-between-null-excluded
  (testing "BETWEEN excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE val BETWEEN 0 AND 100 ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1 3] (ids r)))))

  (testing "NOT BETWEEN also excludes NULL (SQL: NULL NOT BETWEEN … → UNKNOWN → FALSE)"
    (let [r (.execute *h* "SELECT id FROM t WHERE val NOT BETWEEN 0 AND 5 ORDER BY id")]
      (is (nil? (err r)))
      ;; val=10 (not between 0..5 → true), val=50 (true), val=NULL (UNKNOWN → false).
      (is (= [1 3] (ids r))))))

;; ============================================================================
;; IN / NOT IN: both must exclude NULL cols.
;; ============================================================================

(deftest test-in-null-excluded
  (testing "IN excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE val IN (10, 50) ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1 3] (ids r)))))

  (testing "NOT IN excludes NULL (SQL: NULL NOT IN (…) → UNKNOWN → FALSE)"
    (let [r (.execute *h* "SELECT id FROM t WHERE val NOT IN (10) ORDER BY id")]
      (is (nil? (err r)))
      ;; val=10 → NOT IN false; val=50 → true; val=NULL → UNKNOWN → false.
      (is (= [3] (ids r))))))

;; ============================================================================
;; OR col IS NULL — the original Odoo test_access_filtered_records shape:
;; `(col IN (V) OR col IS NULL)`. The NULL row should now show up.
;; ============================================================================

(deftest test-or-col-is-null
  (testing "col IN (…) OR col IS NULL returns both IN-matches and NULLs"
    (let [r (.execute *h* "SELECT id FROM t WHERE val IN (10) OR val IS NULL ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1 2] (ids r)))))

  (testing "col = V OR col IS NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE val = 50 OR val IS NULL ORDER BY id")]
      (is (nil? (err r)))
      (is (= [2 3] (ids r))))))

;; ============================================================================
;; LIKE / ILIKE / NOT LIKE: must exclude NULL. Before this fix, re-find on
;; the :__null__ sentinel (a keyword) would throw ClassCastException.
;; ============================================================================

(deftest test-like-null-excluded
  (testing "LIKE excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE tag LIKE 'h%' ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1] (ids r)))))

  (testing "ILIKE excludes NULL"
    (let [r (.execute *h* "SELECT id FROM t WHERE tag ILIKE 'H%' ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1] (ids r)))))

  (testing "NOT LIKE excludes NULL (SQL: NULL NOT LIKE … → UNKNOWN → FALSE)"
    (let [r (.execute *h* "SELECT id FROM t WHERE tag NOT LIKE 'h%' ORDER BY id")]
      (is (nil? (err r)))
      ;; id=1 ("hello" NOT LIKE 'h%' → false), id=3 ("aa" NOT LIKE 'h%' → true),
      ;; id=2 (NULL → UNKNOWN → false).
      (is (= [3] (ids r))))))

;; ============================================================================
;; IS TRUE / IS FALSE / IS NOT TRUE / IS NOT FALSE: per SQL, these always
;; resolve to known TRUE or FALSE even for NULL (never UNKNOWN).
;; NULL IS TRUE → FALSE.    NULL IS FALSE → FALSE.
;; NULL IS NOT TRUE → TRUE. NULL IS NOT FALSE → TRUE.
;; ============================================================================

;; ============================================================================
;; ORDER BY on nullable columns must sort NULLs last (ASC) or first (DESC),
;; matching PostgreSQL's default. Clojure's `compare` on a Keyword vs Long
;; would throw ClassCastException without the server-side null-aware sort.
;; ============================================================================

(deftest test-order-by-with-nulls
  (testing "ORDER BY nullable ASC puts NULLs last (PG default)"
    (let [r (.execute *h* "SELECT id FROM t ORDER BY val ASC")]
      (is (nil? (err r)))
      ;; val: 1→10, 3→50, 2→NULL → expected order [1, 3, 2]
      (is (= [1 3 2] (vec (map #(Long/parseLong (first %)) (rows r)))))))

  (testing "ORDER BY nullable DESC puts NULLs first (PG default)"
    (let [r (.execute *h* "SELECT id FROM t ORDER BY val DESC")]
      (is (nil? (err r)))
      ;; val: 3→50, 1→10, 2→NULL → DESC: 2 (NULLs first), 3, 1
      (is (= [2 3 1] (vec (map #(Long/parseLong (first %)) (rows r)))))))

  (testing "ORDER BY on string column with NULLs"
    (let [r (.execute *h* "SELECT id FROM t ORDER BY tag ASC")]
      (is (nil? (err r)))
      ;; tag: 3→"aa", 1→"hello", 2→NULL → ASC: 3, 1, 2
      (is (= [3 1 2] (vec (map #(Long/parseLong (first %)) (rows r))))))))

;; ============================================================================
;; Scalar functions must propagate NULL: UPPER(NULL)=NULL, LENGTH(NULL)=NULL,
;; 1 + NULL = NULL, etc. Without the null-safe wrapper, these either throw
;; (ClassCastException on :__null__ keyword) or return garbage.
;; ============================================================================

(defn- cell [r]
  (first (first (rows r))))

(deftest test-scalar-fn-null-propagation
  (testing "UPPER(NULL) → NULL (reported as nil at wire boundary)"
    (let [r (.execute *h* "SELECT UPPER(tag) FROM t WHERE id = 2")]
      (is (nil? (err r)))
      (is (nil? (cell r)))))

  (testing "LOWER(NULL) → NULL"
    (let [r (.execute *h* "SELECT LOWER(tag) FROM t WHERE id = 2")]
      (is (nil? (err r)))
      (is (nil? (cell r)))))

  (testing "LENGTH(NULL) → NULL"
    (let [r (.execute *h* "SELECT LENGTH(tag) FROM t WHERE id = 2")]
      (is (nil? (err r)))
      (is (nil? (cell r)))))

  (testing "ABS(NULL) → NULL"
    (let [r (.execute *h* "SELECT ABS(val) FROM t WHERE id = 2")]
      (is (nil? (err r)))
      (is (nil? (cell r))))))

(deftest test-scalar-fn-non-null-still-works
  (testing "UPPER on actual string"
    (let [r (.execute *h* "SELECT UPPER(tag) FROM t WHERE id = 1")]
      (is (nil? (err r)))
      (is (= "HELLO" (cell r)))))

  (testing "LENGTH on actual string"
    (let [r (.execute *h* "SELECT LENGTH(tag) FROM t WHERE id = 1")]
      (is (nil? (err r)))
      (is (= "5" (cell r)))))

  (testing "ABS on actual number"
    (let [r (.execute *h* "SELECT ABS(val) FROM t WHERE id = 1")]
      (is (nil? (err r)))
      (is (= "10" (cell r))))))

(deftest test-arithmetic-null-propagation
  (testing "NULL + 1 → NULL"
    (let [r (.execute *h* "SELECT val + 1 FROM t WHERE id = 2")]
      (is (nil? (err r)))
      (is (nil? (cell r)))))

  (testing "NULL - 5 → NULL"
    (let [r (.execute *h* "SELECT val - 5 FROM t WHERE id = 2")]
      (is (nil? (err r)))
      (is (nil? (cell r)))))

  (testing "NULL * 2 → NULL"
    (let [r (.execute *h* "SELECT val * 2 FROM t WHERE id = 2")]
      (is (nil? (err r)))
      (is (nil? (cell r)))))

  (testing "non-null arithmetic still works"
    (let [r (.execute *h* "SELECT val + 1 FROM t WHERE id = 1")]
      (is (nil? (err r)))
      (is (= "11" (cell r))))))

(deftest test-is-boolean-with-null
  (testing "IS TRUE excludes NULL and FALSE"
    (let [r (.execute *h* "SELECT id FROM t WHERE flag IS TRUE ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1] (ids r)))))

  (testing "IS FALSE excludes NULL and TRUE"
    (let [r (.execute *h* "SELECT id FROM t WHERE flag IS FALSE ORDER BY id")]
      (is (nil? (err r)))
      (is (= [3] (ids r)))))

  (testing "IS NOT TRUE includes NULL (per SQL) and FALSE"
    (let [r (.execute *h* "SELECT id FROM t WHERE flag IS NOT TRUE ORDER BY id")]
      (is (nil? (err r)))
      (is (= [2 3] (ids r)))))

  (testing "IS NOT FALSE includes NULL (per SQL) and TRUE"
    (let [r (.execute *h* "SELECT id FROM t WHERE flag IS NOT FALSE ORDER BY id")]
      (is (nil? (err r)))
      (is (= [1 2] (ids r))))))

;; ============================================================================
;; Casting NULL
;; ============================================================================

(deftest test-cast-of-null-is-null
  ;; The compile-time constant fold in translate-cast-expr had no NULL
  ;; guard, so it stringified nil: `(str nil)` is "", and NULL::text
  ;; became the EMPTY STRING. Everything NULL-aware downstream then took
  ;; the wrong branch, and ::bool raised `invalid input syntax for type
  ;; boolean: ""` — the empty string being parsed back out.
  (testing "IS NULL survives a cast to every scalar type"
    (doseq [t ["text" "varchar" "char" "int" "bigint" "numeric" "bool"
               "date" "timestamp" "uuid" "jsonb" "json"]]
      (let [r (.execute *h* (str "SELECT NULL::" t " IS NULL"))]
        (is (nil? (err r)) (str "NULL::" t " must not error"))
        (is (= "t" (cell r)) (str "NULL::" t " IS NULL should be true")))))

  (testing "a cast NULL stays absent rather than becoming a value"
    (is (= "FELLBACK" (cell (.execute *h* "SELECT coalesce(NULL::text, 'FELLBACK')"))))
    (is (nil? (cell (.execute *h* "SELECT length(NULL::text)"))))
    (is (= "a" (cell (.execute *h*
                               "SELECT CASE WHEN NULL::text IS NULL THEN 'a' ELSE 'b' END")))))

  (testing "IS NOT NULL is correspondingly false"
    (is (= "f" (cell (.execute *h* "SELECT NULL::text IS NOT NULL")))))

  (testing "a non-NULL cast is unaffected"
    (is (= "7"   (cell (.execute *h* "SELECT 7::text"))))
    (is (= "f"   (cell (.execute *h* "SELECT 7::text IS NULL"))))
    (is (= "abc" (cell (.execute *h* "SELECT 'abc'::text"))))))

;; ============================================================================
;; NOT and three-valued logic
;; ============================================================================

(deftest test-not-excludes-null-rows
  ;; `NOT x` is TRUE only when x is FALSE, and a comparison with a NULL
  ;; operand is UNKNOWN — not FALSE. The null-guards were emitted INSIDE
  ;; the datalog `not`, so for a NULL row the guarded conjunction was
  ;; false and `not` made it true: `WHERE NOT (val = 10)` returned the
  ;; val-IS-NULL row (id=2), which PostgreSQL excludes.
  ;;
  ;; Fixture: id=1 val=10, id=2 all NULL, id=3 val=50.
  ;; Every expectation here was taken from PostgreSQL 17 on the same rows.
  (testing "a bare comparison under NOT"
    (is (= [1 3] (ids (.execute *h* "SELECT id FROM t WHERE NOT (val = 99)")))
        "the val-IS-NULL row must not survive negation")
    (is (= [3] (ids (.execute *h* "SELECT id FROM t WHERE NOT (val = 10)")))))

  (testing "under a disjunction — an OR is FALSE only if every disjunct is"
    (is (= [3] (ids (.execute *h* "SELECT id FROM t WHERE NOT (val = 10 OR val = 20)")))))

  (testing "a CONJUNCTION must NOT be tightened the same way"
    ;; `NOT (a AND b)` is TRUE as soon as one conjunct is FALSE, whatever
    ;; the other is — so the NULL row DOES belong here. Hoisting val's
    ;; guard would wrongly drop it, which is why AND-trees are excluded.
    (is (= [1 2 3] (ids (.execute *h* "SELECT id FROM t
                                        WHERE NOT (val = 10 AND id = 999)")))))

  (testing "IS NOT NULL under NOT still works"
    ;; It translates to exactly one guard-shaped clause; stripping guards
    ;; would leave a bare `(not)`, which datalog rejects outright.
    (is (= [2] (ids (.execute *h* "SELECT id FROM t WHERE NOT (val IS NOT NULL)")))))

  (testing "double negation"
    (is (= [1] (ids (.execute *h* "SELECT id FROM t WHERE NOT (NOT (val = 10))")))))

  (testing "a non-nullable column is unaffected"
    (is (= [2 3] (ids (.execute *h* "SELECT id FROM t WHERE NOT (id = 1)"))))))
