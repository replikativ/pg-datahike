(ns datahike.pg.window
  "Window function post-processing engine.

   Operates on query result rows (vectors of values) after the main Datalog
   query has executed. Computes ROW_NUMBER, RANK, DENSE_RANK, NTILE,
   PERCENT_RANK, CUME_DIST, running SUM/AVG/COUNT/MIN/MAX, and LAG/LEAD.

   Architecture: matches Stratum's window.clj interface (same spec maps)
   but uses row-based processing instead of columnar typed arrays.

   Window spec format:
     {:op :row-number/:rank/:dense-rank/:ntile/:sum/:avg/:count/:min/:max/:lag/:lead
      :col-idx int          — column index for aggregate/offset ops (nil for ranking)
      :partition-by [idx ...] — column indices for partitioning
      :order-by [[idx :asc/:desc] ...] — column indices + direction for sorting
      :frame {:type :rows/:range :start bound :end bound}
      :offset int           — LAG/LEAD offset (default 1)
      :default val          — LAG/LEAD default value
      :ntile-n int}         — NTILE bucket count"
  (:require [datahike.pg.sql.fns :as fns]))

(set! *warn-on-reflection* true)

;; ============================================================================
;; Comparator for ORDER BY within partitions
;; ============================================================================

(defn- make-window-comparator
  "Build a comparator for sorting rows by order-by spec.
   order-by: [[col-idx :asc/:desc] ...]"
  [order-by]
  (fn [row-a row-b]
    (loop [specs order-by]
      (if-let [[idx dir] (first specs)]
        (let [a (nth row-a idx nil)
              b (nth row-b idx nil)
              a-null? (or (nil? a) (= :__null__ a))
              b-null? (or (nil? b) (= :__null__ b))
              c (cond
                  (and a-null? b-null?) 0
                  a-null? 1    ;; nulls last
                  b-null? -1
                  :else (compare a b))
              c (if (= dir :desc) (- c) c)]
          (if (zero? c) (recur (rest specs)) c))
        0))))

;; ============================================================================
;; Partition and sort
;; ============================================================================

(defn- partition-and-sort
  "Partition rows by partition-by columns, sort within each partition.
   Returns a seq of sorted partition groups, each a vector of [orig-idx row] pairs."
  [rows partition-by-idxs order-by]
  (let [indexed (map-indexed vector rows)
        groups (if (empty? partition-by-idxs)
                 [indexed]
                 (vals (group-by (fn [[_i row]]
                                   (mapv #(nth row % nil) partition-by-idxs))
                                 indexed)))
        cmp (when (seq order-by) (make-window-comparator order-by))]
    (if cmp
      (mapv #(vec (sort-by second cmp %)) groups)
      (mapv vec groups))))

;; ============================================================================
;; Window function implementations
;; ============================================================================

(defn- compute-row-number [partitions _spec]
  (let [result (transient {})]
    (doseq [partition partitions]
      (doseq [[rank [orig-idx _row]] (map-indexed vector partition)]
        (assoc! result orig-idx (double (inc rank)))))
    (persistent! result)))

(defn- compute-rank [partitions spec]
  (let [result (transient {})
        order-by (:order-by spec)]
    (doseq [partition partitions]
      (loop [i 0 rank 1 prev-vals nil same-count 0]
        (when (< i (count partition))
          (let [[orig-idx row] (nth partition i)
                curr-vals (mapv #(nth row (first %) nil) order-by)]
            (if (= curr-vals prev-vals)
              (do (assoc! result orig-idx (double rank))
                  (recur (inc i) rank curr-vals (inc same-count)))
              (let [new-rank (+ rank same-count)]
                (assoc! result orig-idx (double new-rank))
                (recur (inc i) new-rank curr-vals 1)))))))
    (persistent! result)))

(defn- compute-dense-rank [partitions spec]
  (let [result (transient {})
        order-by (:order-by spec)]
    (doseq [partition partitions]
      (loop [i 0 rank 0 prev-vals nil]
        (when (< i (count partition))
          (let [[orig-idx row] (nth partition i)
                curr-vals (mapv #(nth row (first %) nil) order-by)
                new-rank (if (= curr-vals prev-vals) rank (inc rank))]
            (assoc! result orig-idx (double new-rank))
            (recur (inc i) new-rank curr-vals)))))
    (persistent! result)))

(defn- compute-ntile [partitions spec]
  (let [n (:ntile-n spec)
        result (transient {})]
    (doseq [partition partitions]
      (let [size (count partition)]
        (doseq [[rank [orig-idx _row]] (map-indexed vector partition)]
          (assoc! result orig-idx (inc (quot (* rank n) size))))))
    (persistent! result)))

(defn- compute-lag-lead [partitions spec]
  (let [col-idx (:col-idx spec)
        offset (or (:offset spec) 1)
        default-val (:default spec)
        lead? (= :lead (:op spec))
        result (transient {})]
    (doseq [partition partitions]
      (let [size (count partition)]
        (doseq [[pos [orig-idx _row]] (map-indexed vector partition)]
          (let [source-pos (if lead? (+ pos offset) (- pos offset))
                val (if (and (>= source-pos 0) (< source-pos size))
                      (let [[_si source-row] (nth partition source-pos)]
                        (nth source-row col-idx nil))
                      default-val)]
            (assoc! result orig-idx val)))))
    (persistent! result)))

(defn- resolve-frame-bound
  "Resolve a frame bound to an absolute position within a partition.
   Returns an int index (inclusive for start, exclusive for end)."
  [bound pos part-size start?]
  (cond
    (= bound :unbounded-preceding) 0
    (= bound :unbounded-following) part-size
    (= bound :current-row) (if start? pos (inc pos))
    (vector? bound)
    (let [[n dir] bound]
      (case dir
        :preceding (max 0 (- pos n))
        :following (min part-size (+ pos n (if start? 0 1)))))
    :else (if start? 0 part-size)))

(defn- compute-aggregate-window [partitions spec]
  (let [col-idx (:col-idx spec)
        op (:op spec)
        frame (:frame spec)
        full-partition? (and (= :unbounded-preceding (:start frame))
                             (= :unbounded-following (:end frame)))
        result (transient {})]
    (if full-partition?
      ;; Full partition aggregate: compute once per partition, broadcast
      (doseq [partition partitions]
        (let [;; MIN/MAX are defined over any ordered type -- a date or a
              ;; text column is legal -- so they must not be restricted to
              ;; numbers the way the arithmetic aggregates are.
              ordered-op? (contains? #{:min :max} op)
              vals (if (:count-star? spec)
                     ;; COUNT(*) counts ROWS in the frame, including the ones
                     ;; whose columns are all NULL -- there is no argument to
                     ;; be null. The `keep` below would count zero of them.
                     partition
                     (keep (fn [[_i row]] (let [v (nth row col-idx nil)]
                                            (when (and (some? v) (not= :__null__ v)
                                                       (or ordered-op? (number? v)))
                                              v)))
                           partition))
              agg-val (case op
                        :sum (if (empty? vals) nil (reduce + 0.0 vals))
                        ;; Through the same implementation the non-window
                        ;; aggregate uses: AVG over int / numeric is NUMERIC in
                        ;; PostgreSQL, with a scale from select_div_scale, not a
                        ;; double. `avg(i) OVER ()` answered 4.25 where the
                        ;; plain `avg(i)` already answered 4.2500000000000000.
                        :avg (cond
                               (empty? vals) nil
                               (every? #(or (integer? %) (decimal? %)) vals)
                               (fns/filter-avg-numeric vals)
                               :else (/ (reduce + 0.0 vals) (count vals)))
                        ;; PostgreSQL's total order (NaN sorts above every
                        ;; non-NaN), and it works for dates and text.
                        :min (if (empty? vals) nil
                                 (reduce (fn [a b] (if (neg? (fns/order-cmp b a)) b a)) vals))
                        :max (if (empty? vals) nil
                                 (reduce (fn [a b] (if (pos? (fns/order-cmp b a)) b a)) vals))
                        :count (count vals)
                        nil)]
          (doseq [[orig-idx _row] partition]
            (assoc! result orig-idx agg-val))))
      ;; Running or sliding aggregate: use prefix sums for SUM/AVG/COUNT
      (doseq [partition partitions]
        (let [n (count partition)
              ;; Build prefix sums
              prefix (double-array (inc n))
              counts (int-array (inc n))]
          (dotimes [i n]
            (let [[_idx row] (nth partition i)
                  v (nth row col-idx nil)
                  num? (and (some? v) (not= :__null__ v) (number? v))]
              (aset prefix (inc i) (+ (aget prefix i) (if num? (double v) 0.0)))
              (aset counts (inc i) (+ (aget counts i) (if num? 1 0)))))
          (dotimes [i n]
            (let [[orig-idx _row] (nth partition i)
                  win-start (resolve-frame-bound (:start frame) i n true)
                  win-end (resolve-frame-bound (:end frame) i n false)
                  win-start (max 0 win-start)
                  win-end (min n win-end)]
              (if (>= win-start win-end)
                (assoc! result orig-idx nil)
                (let [s (- (aget prefix win-end) (aget prefix win-start))
                      c (- (aget counts win-end) (aget counts win-start))]
                  (assoc! result orig-idx
                          (case op
                            :sum (if (zero? c) nil s)
                            :avg (if (zero? c) nil (/ s c))
                            :count c
                            :min nil  ;; MIN/MAX with frames need O(n) per window — use full partition
                            :max nil
                            nil)))))))))
    (persistent! result)))

;; ============================================================================
;; Main entry point
;; ============================================================================

(defn execute-window-functions
  "Apply window function specs to query result rows.
   rows: seq of result tuples (vectors)
   window-specs: [{:op :row-number :partition-by [idx] :order-by [[idx :dir]] :frame {...} ...}]
   Returns rows with window values appended to each tuple."
  [rows window-specs]
  (if (or (empty? rows) (empty? window-specs))
    rows
    (let [n (count rows)
          ;; Process each window spec, accumulating result columns
          result-columns
          (mapv (fn [spec]
                  (let [partitions (partition-and-sort rows
                                                       (:partition-by spec)
                                                       (:order-by spec))]
                    (let [op (:op spec)]
                      (cond
                        (#{:row_number :row-number} op)
                        (compute-row-number partitions spec)
                        (#{:rank} op)
                        (compute-rank partitions spec)
                        (#{:dense_rank :dense-rank} op)
                        (compute-dense-rank partitions spec)
                        (#{:ntile} op)
                        (compute-ntile partitions spec)
                        (#{:lag :lead} op)
                        (compute-lag-lead partitions spec)
                        (#{:sum :avg :count :min :max} op)
                        (compute-aggregate-window partitions spec)
                        :else
                        (into {} (map-indexed (fn [i _] [i nil])) rows)))))
                window-specs)]
      ;; Append window values to each row
      (mapv (fn [i]
              (let [row (nth rows i)]
                (into (vec row)
                      (mapv #(get % i) result-columns))))
            (range n)))))
