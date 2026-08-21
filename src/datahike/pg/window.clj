(ns datahike.pg.window
  "Window function post-processing engine.

   Operates on query result rows (vectors of values) after the main Datalog
   query has executed: partition, sort, resolve each row's FRAME, and compute
   the window value over it.

   Two rules govern the whole file.

   The frame is the unit of computation. PostgreSQL evaluates a window
   aggregate over the frame -- the slice of the sorted partition the frame
   clause names -- not over the partition. The default frame when a window
   has an ORDER BY is `RANGE UNBOUNDED PRECEDING AND CURRENT ROW`, which
   ends at the last PEER of the current row (every row with an equal sort
   key), so `sum(v) OVER (ORDER BY k)` on tied keys gives tied rows the same
   total. Reading that default as ROWS -- as this engine did -- makes every
   tie wrong, and dropping the frame entirely makes every running aggregate
   wrong.

   The aggregate is the one in `datahike.pg.sql.fns`. A window aggregate is
   the SAME function as the plain one, applied to the frame's values instead
   of the group's: the translator resolves it through the same
   `sql-aggregate->datalog` map and the same precision-variant rule, and
   hands the symbol down in the spec. This file therefore has no arithmetic
   of its own. It used to, and every one of those private implementations
   had drifted from its counterpart -- `sum` accumulated at double so
   `sum(numeric)` lost its scale and `sum(real)` gained float8 noise, `avg`
   over a running frame answered an integer where PostgreSQL answers
   NUMERIC, `min`/`max` over any frame but the whole partition answered
   NULL, and every aggregate the private `case` did not name -- array_agg,
   string_agg, stddev, the bool aggregates -- answered NULL for every row.

   Window spec format:
     {:op :row_number/:rank/:dense_rank/:ntile/:sum/:avg/…/:lag/:lead
      :col-idx int          — result-tuple index of the first argument
      :arg2-idx int         — index of the second (string_agg delimiter, …)
      :count-star? bool     — COUNT(*): counts frame ROWS, no argument
      :agg-sym sym          — fully-qualified aggregate fn (aggregates only)
      :partition-by [idx …] — column indices for partitioning
      :order-by [[idx :asc/:desc] …]
      :frame {:type :rows/:range :start bound :end bound}
      :offset-n int         — LAG/LEAD/NTH_VALUE offset (default 1)
      :default-val val      — LAG/LEAD default
      :ntile-n int}"
  (:require [datahike.pg.errors :as errors]
            [datahike.pg.sql.fns :as fns]))

(set! *warn-on-reflection* true)

;; ============================================================================
;; Partition and sort
;; ============================================================================

(defn- make-window-comparator
  "Comparator for the within-partition sort. `order-by` is [[col-idx dir] …].

   `fns/order-key-cmp` -- the same null/NaN-correct key comparison the
   server's ORDER BY uses -- rather than a private copy."
  [order-by]
  (fn [row-a row-b]
    (loop [specs order-by]
      (if-let [[idx dir nulls] (first specs)]
        (let [c (fns/order-key-cmp (nth row-a idx nil) (nth row-b idx nil) dir nulls)]
          (if (zero? c) (recur (rest specs)) c))
        0))))

(defn- partition-and-sort
  "Partition rows by partition-by columns, sort within each partition.
   Returns a vector of sorted partitions, each a vector of [orig-idx row]."
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
;; Peers and frames
;; ============================================================================

(defn- peer-bounds
  "For each row of a sorted `partition`, the half-open index range of its
   PEER GROUP: the rows whose ORDER BY key is equal to its own. Returns
   [starts ends] as int arrays.

   Peers are what makes a RANGE frame differ from a ROWS frame, and what
   `rank` counts. With no ORDER BY every row is a peer of every other, so
   the peer group is the whole partition -- which is why `sum(v) OVER ()`
   and `sum(v) OVER (ORDER BY k RANGE …)` agree on an unordered window."
  [partition order-by]
  (let [n (count partition)
        starts (int-array n)
        ends (int-array n)]
    (if (empty? order-by)
      (do (java.util.Arrays/fill starts (int 0))
          (java.util.Arrays/fill ends (int n))
          [starts ends])
      (let [key-of (fn [i] (let [[_ row] (nth partition i)]
                             (mapv (fn [[idx _]] (nth row idx nil)) order-by)))
            peers? (fn [a b] (every? (fn [[va vb]]
                                       (zero? (fns/order-key-cmp va vb :asc nil)))
                                     (map vector a b)))]
        (loop [i 0]
          (when (< i n)
            (let [k (key-of i)
                  j (loop [j (inc i)]
                      (if (and (< j n) (peers? k (key-of j))) (recur (inc j)) j))]
              (dotimes [p (- j i)]
                (aset starts (+ i p) (int i))
                (aset ends (+ i p) (int j)))
              (recur (long j)))))
        [starts ends]))))

(defn- range-offset-bound
  "Resolve a RANGE frame bound given as a value offset -- `RANGE BETWEEN 5
   PRECEDING AND CURRENT ROW`. The frame holds every row whose sort key is
   within `n` of the current row's, so the boundary is found by VALUE, not
   by row count.

   PostgreSQL requires exactly one ORDER BY column for these, and a NULL
   sort key frames only its own peer group (all NULLs are peers)."
  [partition order-by i k dir start? peer-lo peer-hi]
  (when-not (= 1 (count order-by))
    (throw (errors/pg-error
            :invalid-argument
            {:message (str "RANGE with offset PRECEDING/FOLLOWING requires "
                           "exactly one ORDER BY column")})))
  (let [[idx odir] (first order-by)
        n (count partition)
        val-at (fn [j] (let [[_ row] (nth partition j)] (nth row idx nil)))
        cur (val-at i)]
    (if (or (nil? cur) (= :__null__ cur))
      (if start? peer-lo peer-hi)
      (if-not (number? cur)
        (throw (errors/pg-error
                :feature-not-supported
                {:feature "RANGE with offset PRECEDING/FOLLOWING over a non-numeric ORDER BY column"}))
        ;; Ascending order: a PRECEDING bound is cur - k, a FOLLOWING bound
        ;; is cur + k. Descending order reverses which side of the current
        ;; value the earlier rows sit on, so the sign flips with it.
        (let [delta (if (= dir :preceding) (- k) k)
              delta (if (= odir :desc) (- delta) delta)
              limit (+ (double cur) (double delta))
              ;; in-frame? for a row's value under the sort direction
              in? (fn [v]
                    (and (some? v) (not= :__null__ v) (number? v)
                         (if (= odir :desc)
                           (if (= dir :preceding) (<= (double v) limit) (>= (double v) limit))
                           (if (= dir :preceding) (>= (double v) limit) (<= (double v) limit)))))]
          (if start?
            ;; first index at or after which every row is within the bound
            (loop [j 0] (if (and (< j n) (not (in? (val-at j)))) (recur (inc j)) j))
            ;; one past the last such index
            (loop [j (dec n)] (if (and (>= j 0) (not (in? (val-at j)))) (recur (dec j)) (inc j)))))))))

(defn- frame-bounds
  "The half-open [start end) index range of row `i`'s frame within its
   sorted partition."
  [partition order-by frame i peer-lo peer-hi]
  (let [n (count partition)
        rows? (not= :range (:type frame))
        resolve-b
        (fn [bound start?]
          (cond
            (= bound :unbounded-preceding) 0
            (= bound :unbounded-following) n
            (= bound :current-row) (if rows?
                                     (if start? i (inc i))
                                     (if start? peer-lo peer-hi))
            (vector? bound)
            (let [[k dir] bound]
              (if rows?
                (case dir
                  ;; end bounds are exclusive, hence the +1
                  :preceding (if start? (- i k) (- (inc i) k))
                  :following (if start? (+ i k) (+ i k 1)))
                (range-offset-bound partition order-by i k dir start? peer-lo peer-hi)))
            :else (if start? 0 n)))
        s (max 0 (min n (resolve-b (:start frame) true)))
        e (max 0 (min n (resolve-b (:end frame) false)))]
    [s (max s e)]))

;; ============================================================================
;; Aggregates over a frame
;; ============================================================================

(def ^:private resolve-agg
  "The aggregate the translator named, resolved once per symbol. The
   symbols are the same ones the plain (non-window) aggregate path emits
   into the Datalog :find clause."
  (memoize (fn [sym]
             (or (requiring-resolve sym)
                 (throw (errors/pg-error
                         :feature-not-supported
                         {:feature (str "window aggregate " sym)}))))))

(defn- frame-args
  "The aggregate's input over [start end): one value per row, or a [value
   delimiter] pair for the two-argument aggregates (string_agg, corr,
   the object aggregates) -- the same pair shape the plain aggregate path
   feeds them."
  [partition start end col-idx arg2-idx arg2-const]
  (let [slice (subvec partition start end)]
    (cond
      arg2-idx (mapv (fn [[_ row]] [(nth row col-idx nil) (nth row arg2-idx nil)]) slice)
      (some? arg2-const) (mapv (fn [[_ row]] [(nth row col-idx nil) arg2-const]) slice)
      :else (mapv (fn [[_ row]] (nth row col-idx nil)) slice))))

(defn- expanding-frame?
  "True when every row's frame starts at the partition's first row and ends
   no earlier than the previous row's -- the shape of the default frame and
   of `ROWS UNBOUNDED PRECEDING`. Lets a running aggregate accumulate in one
   pass instead of being recomputed from scratch at every row."
  [frame]
  (and (= :unbounded-preceding (:start frame))
       (contains? #{:current-row :unbounded-following} (:end frame))))

(defn- running-aggregate
  "Accumulate an expanding-frame aggregate in a single pass.

   Every step re-applies the SAME aggregate function the whole-frame path
   would use -- `(sum-fn [acc v])`, `(min-fn [acc v])` -- so the running
   answer is the aggregate's own answer, not a second implementation of it.
   Returns nil when `op` has no such step, and the caller recomputes."
  [op agg-sym partition ends col-idx]
  (let [n (count partition)
        v-at (fn [i] (let [[_ row] (nth partition i)] (nth row col-idx nil)))
        f (when agg-sym (resolve-agg agg-sym))
        numeric-avg? (= 'datahike.pg.sql/filter-avg-numeric agg-sym)
        out (object-array n)]
    (case op
      (:sum :min :max)
      (loop [i 0, filled 0, acc :__null__]
        (if (= i n)
          out
          (let [e (aget ^ints ends i)
                acc (loop [j filled, a acc]
                      (if (< j e) (recur (inc j) (f [a (v-at j)])) a))]
            (aset out i acc)
            (recur (inc i) (max filled e) acc))))

      :count
      (loop [i 0, filled 0, acc 0]
        (if (= i n)
          out
          (let [e (aget ^ints ends i)
                acc (loop [j filled, a (long acc)]
                      (if (< j e)
                        (recur (inc j) (+ a (long (fns/filter-count [(v-at j)]))))
                        a))]
            (aset out i acc)
            (recur (inc i) (max filled e) (long acc)))))

      :avg
      ;; AVG is a function of the running SUM and the running COUNT and
      ;; nothing else, which is what `fns/avg-numeric-of` exposes -- so the
      ;; running answer is bit-identical to the whole-frame one, scale rule
      ;; included.
      (let [acc (object-array 2)]
        (aset acc 0 (if numeric-avg? java.math.BigDecimal/ZERO (Double/valueOf 0.0)))
        (aset acc 1 (Long/valueOf 0))
        (loop [i 0, filled 0]
          (if (= i n)
            out
            (let [e (aget ^ints ends i)]
              (loop [j filled]
                (when (< j e)
                  (let [v (v-at j)]
                    (when-not (fns/sql-null? v)
                      (aset acc 0 (if numeric-avg?
                                    (.add ^java.math.BigDecimal (aget acc 0) (fns/->bigdec v))
                                    (Double/valueOf (+ (double (aget acc 0)) (double v)))))
                      (aset acc 1 (Long/valueOf (inc (long (aget acc 1)))))))
                  (recur (inc j))))
              (let [c (long (aget acc 1))]
                (aset out i (cond
                              (zero? c) :__null__
                              numeric-avg? (fns/avg-numeric-of (aget acc 0) c)
                              :else (/ (double (aget acc 0)) c))))
              (recur (inc i) (max filled e))))))
      nil)))

(defn- compute-aggregate-window
  "Window aggregate for every row, over that row's frame."
  [partitions spec]
  (let [{:keys [op frame col-idx arg2-idx arg2-const agg-sym count-star? order-by]} spec
        result (transient {})]
    (doseq [partition partitions]
      (let [n (count partition)
            [peer-lo peer-hi] (peer-bounds partition order-by)
            bounds (mapv #(frame-bounds partition order-by frame %
                                        (aget ^ints peer-lo %) (aget ^ints peer-hi %))
                         (range n))
            ;; A whole-partition frame is one aggregate broadcast to every
            ;; row; an expanding frame accumulates in one pass; anything
            ;; else is computed per row.
            whole? (and (= :unbounded-preceding (:start frame))
                        (= :unbounded-following (:end frame)))
            running (when (and (not whole?) (not count-star?)
                               (nil? arg2-idx) (nil? arg2-const)
                               (expanding-frame? frame))
                      (running-aggregate op agg-sym partition
                                         (int-array (map second bounds)) col-idx))
            agg-1 (fn [[s e]]
                    (cond
                      count-star? (- e s)
                      (nil? col-idx) :__null__
                      :else ((resolve-agg agg-sym)
                             (frame-args partition s e col-idx arg2-idx arg2-const))))]
        (cond
          whole?
          (let [v (agg-1 [0 n])]
            (doseq [[orig-idx _] partition] (assoc! result orig-idx v)))
          running
          (dotimes [i n]
            (assoc! result (first (nth partition i)) (aget ^objects running i)))
          :else
          (dotimes [i n]
            (assoc! result (first (nth partition i)) (agg-1 (nth bounds i)))))))
    (persistent! result)))

;; ============================================================================
;; Ranking functions
;; ============================================================================

(defn- rank-values
  "row_number / rank / dense_rank / percent_rank / cume_dist, all of which
   are functions of the peer groups alone. bigint for the three counters,
   float8 for the two fractions -- the types PostgreSQL declares."
  [partitions op]
  (let [result (transient {})]
    (doseq [partition partitions]
      (let [n (count partition)
            [peer-lo peer-hi] (peer-bounds partition (:order-by (meta partition)))
            dense (int-array n)]
        ;; dense_rank counts DISTINCT peer groups seen so far
        (loop [i 0, d 0, prev -1]
          (when (< i n)
            (let [lo (aget ^ints peer-lo i)
                  d (if (= lo prev) d (inc d))]
              (aset dense i (int d))
              (recur (inc i) d lo))))
        (dotimes [i n]
          (let [orig-idx (first (nth partition i))
                lo (aget ^ints peer-lo i)
                hi (aget ^ints peer-hi i)]
            (assoc! result orig-idx
                    (case op
                      :row_number (long (inc i))
                      :rank (long (inc lo))
                      :dense_rank (long (aget dense i))
                      ;; (rank - 1) / (rows - 1), and 0 for a single row
                      :percent_rank (if (= n 1) 0.0 (/ (double lo) (double (dec n))))
                      ;; fraction of rows at or before the current PEER group
                      :cume_dist (/ (double hi) (double n))))))))
    (persistent! result)))

(defn- compute-ntile
  "NTILE(n): PostgreSQL fills the LARGER buckets FIRST -- with 5 rows in 2
   buckets the split is 3/2, not 2/3, which is what the previous
   `(inc (quot (* pos n) size))` produced."
  [partitions spec]
  (let [b (:ntile-n spec)
        result (transient {})]
    (when (or (nil? b) (not (pos? (long b))))
      (throw (errors/pg-error :invalid-argument
                              {:message "argument of ntile must be greater than zero"})))
    (doseq [partition partitions]
      (let [size (count partition)
            b (long b)
            base (quot size b)                  ;; rows in a small bucket
            extra (rem size b)                  ;; buckets that get one more
            big-rows (* extra (inc base))]
        (dotimes [pos size]
          (let [orig-idx (first (nth partition pos))]
            (assoc! result orig-idx
                    (if (< pos big-rows)
                      (inc (quot pos (inc base)))
                      (+ extra 1 (quot (- pos big-rows) (max 1 base)))))))))
    (persistent! result)))

;; ============================================================================
;; Navigation functions
;; ============================================================================

(defn- compute-lag-lead
  "LAG/LEAD look at a row OFFSET positions away in the partition -- they
   ignore the frame, as PostgreSQL does. The offset and the default are the
   function's own second and third arguments; both were parsed and then
   dropped, so `lead(v, 2, -1)` behaved as `lead(v)`."
  [partitions spec]
  (let [col-idx (:col-idx spec)
        offset (long (or (:offset-n spec) 1))
        default-val (:default-val spec)
        lead? (= :lead (:op spec))
        result (transient {})]
    (doseq [partition partitions]
      (let [size (count partition)]
        (dotimes [pos size]
          (let [orig-idx (first (nth partition pos))
                source-pos (if lead? (+ pos offset) (- pos offset))
                v (if (and (>= source-pos 0) (< source-pos size))
                    (let [[_ source-row] (nth partition source-pos)]
                      (nth source-row col-idx nil))
                    default-val)]
            (assoc! result orig-idx v)))))
    (persistent! result)))

(defn- compute-value-fn
  "FIRST_VALUE / LAST_VALUE / NTH_VALUE -- the value at a position within
   the FRAME, which is why `last_value(v) OVER (ORDER BY k)` is the current
   row's peer value and not the partition's last: the default frame ends at
   the current row."
  [partitions spec]
  (let [{:keys [op col-idx frame order-by]} spec
        nth-n (long (or (:offset-n spec) 1))
        result (transient {})]
    (doseq [partition partitions]
      (let [n (count partition)
            [peer-lo peer-hi] (peer-bounds partition order-by)]
        (dotimes [i n]
          (let [orig-idx (first (nth partition i))
                [s e] (frame-bounds partition order-by frame i
                                    (aget ^ints peer-lo i) (aget ^ints peer-hi i))
                pos (case op
                      :first_value s
                      :last_value (dec e)
                      :nth_value (+ s (dec nth-n)))]
            (assoc! result orig-idx
                    (if (and (>= pos s) (< pos e) (>= pos 0) (< pos n))
                      (let [[_ row] (nth partition pos)] (nth row col-idx nil))
                      :__null__))))))
    (persistent! result)))

;; ============================================================================
;; Main entry point
;; ============================================================================

(def ^:private aggregate-ops
  "Window ops computed over the frame by an aggregate function."
  #{:sum :avg :count :min :max :stddev :stddev_samp :stddev_pop :variance
    :var_samp :var_pop :string_agg :array_agg :jsonb_agg :json_agg
    :jsonb_object_agg :json_object_agg :corr :median :count_distinct})

(defn- compute-spec
  [rows spec]
  (let [partitions (mapv #(with-meta % {:order-by (:order-by spec)})
                         (partition-and-sort rows (:partition-by spec) (:order-by spec)))
        op (:op spec)]
    (cond
      (#{:row_number :rank :dense_rank :percent_rank :cume_dist} op)
      (rank-values partitions op)

      (= :ntile op) (compute-ntile partitions spec)
      (#{:lag :lead} op) (compute-lag-lead partitions spec)
      (#{:first_value :last_value :nth_value} op) (compute-value-fn partitions spec)

      (or (contains? aggregate-ops op) (:agg-sym spec))
      (compute-aggregate-window partitions spec)

      :else
      ;; Not silently NULL. An unknown window function used to produce a
      ;; NULL column for every row, which reads as data.
      (throw (errors/pg-error :feature-not-supported
                              {:feature (str (name op) " as a window function")})))))

(defn execute-window-functions
  "Apply window function specs to query result rows.
   rows: seq of result tuples (vectors)
   Returns rows with one window value appended per spec."
  [rows window-specs]
  (if (or (empty? rows) (empty? window-specs))
    rows
    (let [n (count rows)
          cols (mapv #(compute-spec rows %) window-specs)]
      (mapv (fn [i]
              (into (vec (nth rows i))
                    ;; `:__null__` is how the rest of the pipeline carries SQL
                    ;; NULL, but the window columns have always handed nil to
                    ;; the row formatter -- keep that.
                    (mapv #(let [v (get % i)] (if (= :__null__ v) nil v)) cols)))
            (range n)))))
