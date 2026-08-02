(ns datahike.test.pg-topk-test
  "Bounded top-k ORDER BY selection under LIMIT.

   When a server-side (null-safe) ORDER BY carries a LIMIT, exec-select
   selects the first n+o rows with a size-(n+o) heap instead of sorting
   the whole result. The selection must be indistinguishable from the
   full-sort path: same null-safe comparator, same stable tie order
   (original row order for equal keys), same OFFSET semantics (drop o
   after selecting n+o)."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

;; Private fns under test — top-k must match the exact comparator the
;; server uses, so pull both through their vars.
(def ^:private top-k-sort #'pg/top-k-sort)
(def ^:private null-safe-order-cmp #'pg/null-safe-order-cmp)

;; ============================================================================
;; Unit: top-k-sort vs the naive stable sort + drop + take reference.
;; ============================================================================

(defn- naive-sorted
  "The full-sort path exec-select falls back to: stable sort, then
   OFFSET drop and LIMIT take."
  [cmp rows lim off]
  (vec (cond->> (sort cmp rows)
         off (drop off)
         lim (take lim))))

(defn- top-k-path
  "The bounded path: select n+o survivors, then drop the offset."
  [cmp rows lim off]
  (vec (cond->> (top-k-sort (+ lim (or off 0)) cmp rows)
         off (drop off))))

(defn- gen-rows
  "n random 3-column rows from a seeded RNG. Values are small longs (to
   force ties) with nil and the :__null__ sentinel mixed in — both mean
   SQL NULL to the comparator."
  [^java.util.Random rng n]
  (vec (repeatedly n
                   (fn []
                     (vec (repeatedly 3
                                      (fn []
                                        (case (.nextInt rng 8)
                                          0 nil
                                          1 :__null__
                                          (long (.nextInt rng 5))))))))))

(deftest test-top-k-matches-full-sort-randomized
  (let [rng (java.util.Random. 42)
        rows (gen-rows rng 300)]
    (doseq [order-by [[0 :asc]
                      [0 :desc]
                      [0 :asc 1 :desc]
                      [2 :desc 0 :asc 1 :asc]]
            lim [0 1 5 50 299 500]
            off [nil 0 3 100 400]]
      (testing (str "order-by " order-by " limit " lim " offset " off)
        (let [cmp (null-safe-order-cmp order-by)]
          (is (= (naive-sorted cmp rows lim off)
                 (top-k-path cmp rows lim off))))))))

(deftest test-top-k-stable-tie-order
  ;; All keys equal → survivors must be the FIRST k rows in input
  ;; order, exactly as the stable full sort would keep them.
  (let [rows (mapv (fn [i] [1 (str "row-" i)]) (range 100))
        cmp (null-safe-order-cmp [0 :asc])]
    (is (= (vec (take 10 rows)) (vec (top-k-sort 10 cmp rows))))
    (is (= (naive-sorted cmp rows 7 20) (top-k-path cmp rows 7 20)))))

(deftest test-top-k-edges
  (let [cmp (null-safe-order-cmp [0 :asc])
        rows [[3] [1] [2]]]
    (testing "k = 0 → empty"
      (is (= [] (vec (top-k-sort 0 cmp rows)))))
    (testing "k > row count → full sorted result"
      (is (= [[1] [2] [3]] (vec (top-k-sort 10 cmp rows)))))
    (testing "empty input"
      (is (= [] (vec (top-k-sort 5 cmp [])))))
    (testing "NULLs last for ASC, first for DESC"
      (let [nrows [[nil] [2] [:__null__] [1]]]
        (is (= [[1] [2] [nil] [:__null__]]
               (vec (top-k-sort 4 cmp nrows))))
        (is (= [[nil] [:__null__] [2] [1]]
               (vec (top-k-sort 4 (null-safe-order-cmp [0 :desc]) nrows))))))))

;; ============================================================================
;; End to end: nullable ORDER BY column → server-side sort path.
;; LIMIT/OFFSET results must be the corresponding slice of the
;; unlimited (full-sort) query.
;; ============================================================================

(def topk-schema
  [{:db/ident       :t/id
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity}
   {:db/ident       :t/val
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}])

;; 200 rows; every third row has val missing (NULL); values repeat
;; (mod 17) so ORDER BY val alone has plenty of ties.
(def topk-data
  (mapv (fn [i]
          (cond-> {:t/id i}
            (pos? (mod i 3)) (assoc :t/val (mod (* 7 i) 17))))
        (range 200)))

(def ^:dynamic *h* nil)

(defn topk-fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn topk-schema)
      (d/transact conn topk-data)
      (try
        (binding [*h* (pg/make-query-handler conn)]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each topk-fixture)

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (vec (map vec (.rows r)))))

(defn- q [sql]
  (let [r (.execute *h* sql)]
    (is (nil? (.error r)) (str sql " → " (.error r)))
    (rows r)))

(deftest test-e2e-limit-slices-match-full-sort
  ;; `val` is nullable → server-side sort; `id` tiebreak makes the
  ;; order total so slice comparison is exact.
  (let [full (q "SELECT id, val FROM t ORDER BY val, id")]
    (is (= 200 (count full)))
    (testing "LIMIT engages top-k and matches the full-sort prefix"
      (is (= (vec (take 10 full))
             (q "SELECT id, val FROM t ORDER BY val, id LIMIT 10"))))
    (testing "LIMIT with OFFSET"
      (is (= (vec (take 10 (drop 25 full)))
             (q "SELECT id, val FROM t ORDER BY val, id LIMIT 10 OFFSET 25"))))
    (testing "LIMIT 0"
      (is (= [] (q "SELECT id, val FROM t ORDER BY val, id LIMIT 0"))))
    (testing "LIMIT larger than the result (full-sort fallback)"
      (is (= full (q "SELECT id, val FROM t ORDER BY val, id LIMIT 1000"))))
    (testing "OFFSET past the end"
      (is (= [] (q "SELECT id, val FROM t ORDER BY val, id LIMIT 5 OFFSET 500"))))))

(deftest test-e2e-desc-and-multi-key
  (let [full (q "SELECT id, val FROM t ORDER BY val DESC, id DESC")]
    (testing "DESC keys: NULLs first, prefix matches"
      (is (= (vec (take 12 full))
             (q "SELECT id, val FROM t ORDER BY val DESC, id DESC LIMIT 12"))))
    (testing "mixed directions"
      (let [mixed (q "SELECT id, val FROM t ORDER BY val DESC, id ASC")]
        (is (= (vec (take 8 (drop 4 mixed)))
               (q "SELECT id, val FROM t ORDER BY val DESC, id ASC LIMIT 8 OFFSET 4")))))))
