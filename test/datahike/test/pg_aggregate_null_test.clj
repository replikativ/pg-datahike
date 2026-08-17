(ns datahike.test.pg-aggregate-null-test
  "SQL-standard aggregate behavior in the presence of NULLs.

   Per PG / SQL:92:
   - COUNT(*)            counts all rows, including NULL-valued.
   - COUNT(col)          counts non-NULL values only.
   - COUNT(DISTINCT col) counts distinct non-NULL values.
   - SUM / AVG / MIN / MAX  skip NULL inputs and return NULL for
                             empty / all-NULL inputs (SUM returns NULL,
                             not 0).
   - GROUP BY col        treats NULL as a single group key (so one row
                             per distinct value, including one for NULL).
   - HAVING pred          treats NULL-valued predicates as UNKNOWN → FALSE
                             (the row's group is filtered).

   Our engine uses the `:__null__` keyword sentinel for missing
   attribute values. The aggregate-emission path in sql.clj routes
   SUM/AVG/MIN/MAX through null-filtering variants so they skip the
   sentinel and return NULL on empty/all-NULL collections."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def schema
  [{:db/ident :t/id   :db/valueType :db.type/long   :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :t/val  :db/valueType :db.type/long   :db/cardinality :db.cardinality/one}
   {:db/ident :t/grp  :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])

;; Rows:
;;  id=1 val=10  grp="A"
;;  id=2 val=20  grp="A"
;;  id=3  (val missing)  (grp missing)
;;  id=4 val=30  grp="B"
;;  id=5  (val missing) grp="B"
(def data
  [{:t/id 1 :t/val 10 :t/grp "A"}
   {:t/id 2 :t/val 20 :t/grp "A"}
   {:t/id 3}
   {:t/id 4 :t/val 30 :t/grp "B"}
   {:t/id 5 :t/grp "B"}])

(def ^:dynamic *h* nil)

(defn fx [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema)
      (d/transact conn data)
      (try (binding [*h* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fx)

(defn- ex [sql]
  (let [^PgWireServer$QueryResult r (.execute *h* sql)]
    {:err (.error r)
     :rows (mapv vec (.rows r))}))

(defn- cell [sql]
  (-> (ex sql) :rows first first))

;; ============================================================================
;; COUNT variants
;; ============================================================================

(deftest test-count-star-counts-all-rows
  (is (= "5" (cell "SELECT COUNT(*) FROM t"))))

(deftest test-count-col-skips-nulls
  (is (= "3" (cell "SELECT COUNT(val) FROM t"))
      "COUNT(val) must skip rows where val IS NULL (ids 3, 5)"))

(deftest test-count-distinct-skips-nulls
  ;; Distinct non-null values: 10, 20, 30 → 3
  (is (= "3" (cell "SELECT COUNT(DISTINCT val) FROM t"))))

;; ============================================================================
;; SUM / AVG / MIN / MAX
;; ============================================================================

(deftest test-sum-skips-nulls
  ;; 10 + 20 + 30 = 60
  (is (= "60" (cell "SELECT SUM(val) FROM t"))))

(deftest test-avg-skips-nulls
  ;; avg of [10 20 30] = 20. PG promotes AVG(int*) → numeric, so the
  ;; rendered value is `20.0000000000000000` (BigDecimal at the scale
  ;; our filter-avg-numeric uses). Accept any representation that
  ;; parses to 20 to stay tolerant if we tune the scale later.
  (let [r (cell "SELECT AVG(val) FROM t")]
    (is (and (string? r)
             (= 20.0 (Double/parseDouble r)))
        (str "got: " r))))

(deftest test-min-max-skip-nulls
  (is (= "10" (cell "SELECT MIN(val) FROM t")))
  (is (= "30" (cell "SELECT MAX(val) FROM t"))))

(deftest test-sum-on-all-null-returns-null
  ;; Where val IS NULL: ids 3, 5. SUM of 0 rows → NULL.
  (let [r (ex "SELECT SUM(val) FROM t WHERE val IS NULL")]
    (is (nil? (:err r)))
    (is (= [[nil]] (:rows r))
        "SUM over empty (all-NULL) input is NULL, not 0")))

(deftest test-avg-on-all-null-returns-null
  (let [r (ex "SELECT AVG(val) FROM t WHERE val IS NULL")]
    (is (nil? (:err r)))
    (is (= [[nil]] (:rows r)))))

(deftest test-min-max-on-all-null-returns-null
  (is (= nil (cell "SELECT MIN(val) FROM t WHERE val IS NULL")))
  (is (= nil (cell "SELECT MAX(val) FROM t WHERE val IS NULL"))))

(deftest test-count-on-all-null-returns-zero
  ;; COUNT is the exception: 0 rows gives 0, not NULL.
  (is (= "0" (cell "SELECT COUNT(val) FROM t WHERE val IS NULL"))))

(deftest test-count-star-on-empty-returns-zero
  (is (= "0" (cell "SELECT COUNT(*) FROM t WHERE id = 99999"))))

;; ============================================================================
;; GROUP BY with NULL
;; ============================================================================

(deftest test-group-by-null-is-one-group
  ;; grp values: "A" (2 rows), "B" (2 rows), NULL (1 row: id=3).
  ;; Expected: 3 groups.
  (let [r (ex "SELECT grp, COUNT(*) FROM t GROUP BY grp")]
    (is (nil? (:err r)))
    (is (= 3 (count (:rows r)))
        (str "expected 3 groups (A, B, NULL), got rows: " (:rows r)))))

(deftest test-group-by-null-group-has-correct-count
  (let [r (ex "SELECT grp, COUNT(*) FROM t GROUP BY grp ORDER BY grp")]
    (is (nil? (:err r)))
    ;; NULL-group row has grp=nil, count=1 (just id=3).
    (let [null-row (first (filter #(nil? (first %)) (:rows r)))]
      (is (some? null-row) "expected a group for NULL grp")
      (is (= "1" (second null-row))
          (str "NULL-grp count should be 1, got row: " null-row)))))

;; ============================================================================
;; HAVING with NULL
;; ============================================================================

(deftest test-having-excludes-null-predicate
  ;; HAVING SUM(val) > 15 — the NULL-grp group has SUM=NULL → UNKNOWN → excluded
  (let [r (ex "SELECT grp, SUM(val) FROM t GROUP BY grp HAVING SUM(val) > 15 ORDER BY grp")]
    (is (nil? (:err r)))
    ;; A: SUM=30 (30>15 → include), B: SUM=30 (include), NULL: SUM=NULL (exclude)
    (is (= 2 (count (:rows r))))
    (is (every? #(not (nil? (first %))) (:rows r))
        "HAVING with NULL predicate value must exclude that group")))

;; ============================================================================
;; MIN/MAX over a non-numeric column
;; ============================================================================

(deftest test-min-max-over-text
  ;; filter-min/filter-max were clojure.core/min|max, which cast to
  ;; Number, so MIN/MAX over any non-numeric column died with
  ;; `class java.lang.String cannot be cast to class java.lang.Number`.
  ;; It only fired when two or more values were actually compared, which
  ;; is why single-row groups and WHERE-narrowed aggregates looked fine.
  (testing "MIN/MAX over a text column"
    (is (= "A" (cell "SELECT MIN(grp) FROM t")))
    (is (= "B" (cell "SELECT MAX(grp) FROM t"))))

  (testing "NULLs are skipped, not ordered"
    ;; id=3 has no grp at all; PG ignores NULL input to MIN/MAX.
    (is (= "A" (cell "SELECT MIN(grp) FROM t WHERE id IN (1,2,3)")))
    (is (= "A" (cell "SELECT MAX(grp) FROM t WHERE id IN (1,2,3)"))))

  (testing "still correct when a group holds exactly one value"
    (is (= "B" (cell "SELECT MAX(grp) FROM t WHERE id = 4")))))

;; ============================================================================
;; string_agg over a real group
;; ============================================================================

(deftest test-string-agg-is-an-aggregate
  ;; Was a per-row function that stringified its value and DISCARDED the
  ;; delimiter, so this returned one row per input row instead of one
  ;; joined string. Every assertion uses ORDER BY: without it the order
  ;; of concatenation is unspecified in SQL, and our set-based engine
  ;; does not reproduce PostgreSQL's incidental insertion order.
  (testing "one row for the whole group"
    (is (= "A,A,B,B" (cell "SELECT string_agg(grp, ',' ORDER BY grp) FROM t"))))

  (testing "the delimiter is honoured"
    (is (= "A-A-B-B" (cell "SELECT string_agg(grp, '-' ORDER BY grp) FROM t"))))

  (testing "descending"
    (is (= "B,B,A,A" (cell "SELECT string_agg(grp, ',' ORDER BY grp DESC) FROM t"))))

  (testing "NULLs are skipped — id=3 has no grp"
    (is (= "A,A" (cell "SELECT string_agg(grp, ',' ORDER BY grp) FROM t WHERE id IN (1,2,3)"))))

  (testing "an all-NULL group is SQL NULL"
    (is (nil? (cell "SELECT string_agg(grp, ',') FROM t WHERE id = 3"))))

  (testing "per group under GROUP BY"
    (is (= [["A" "A,A"] ["B" "B,B"]]
           (:rows (ex "SELECT grp, string_agg(grp, ',' ORDER BY grp) FROM t
                       WHERE grp IS NOT NULL GROUP BY grp ORDER BY grp"))))))
