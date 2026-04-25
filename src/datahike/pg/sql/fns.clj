(ns datahike.pg.sql.fns
  "SQL → Clojure function wrappers.

   Every named function the translator can emit at query time lives
   here, behind a stable (fully-qualified) symbol that Datahike's
   `resolve-fn` looks up at execute time:

   - Aggregate wrappers (`filter-sum` / `filter-avg` / …) — variants of
     the core Datahike aggregates that skip the `:__null__` sentinel and
     treat nil as SQL NULL. Needed because core's `sum`/`avg` throw on
     keyword arithmetic and `min`/`max` mis-order keyword vs number.

   - `null-safe` — wraps a scalar fn so either-operand-is-NULL returns
     NULL. Used to adapt Clojure string/math fns (upper, length,
     arithmetic) to SQL 3-valued logic.

   - SQL string fns (`sql-lpad`, `sql-position`, …) — thin wrappers
     with standard SQL semantics (1-based positions, padding behavior).

   - PG-catalog-function stubs (`pg-table-is-visible`, `pg-format-type`,
     `pg-get-expr`) — minimum viable to make JDBC drivers and ORMs
     that probe the catalog not trip over missing functions.

   - Lookup tables:
       * `sql-aggregate->datalog` — SQL agg name → fully-qualified
         symbol. Emitted symbol resolves via
         `datahike.pg.sql/filter-*` which re-exports from this ns.
       * `sql-fn->clj-fn` — SQL fn name → actual IFn value. Used by
         `translate-function-call` at emit time to wrap in `null-safe`.

   Runtime symbols re-exported from `datahike.pg.sql` so the
   translator's emitted forms keep the old qualified path
   (`'datahike.pg.sql/filter-count`, `'datahike.pg.sql/sql-+`, …)
   for backward compat with already-prepared statements cached on
   clients."
  (:require [clojure.string :as str]
            [datahike.pg.types :as types]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; FILTER-aware aggregate functions
;; Non-matching rows produce the :__null__ sentinel; these filter it out.
;; Keep the fn defs BEFORE the sql-aggregate->datalog map so the
;; `'datahike.pg.sql/filter-*` symbols in the map resolve once re-exported.

(defn filter-sum
  "SUM that ignores :__null__ sentinel values. Returns :__null__ if all filtered."
  [coll]
  (let [vs (remove #(= :__null__ %) coll)]
    (if (empty? vs) :__null__ (reduce + 0 vs))))

(defn filter-avg
  "AVG that ignores :__null__ sentinel values. Returns :__null__ if all filtered."
  [coll]
  (let [vs (remove #(= :__null__ %) coll)]
    (if (empty? vs) :__null__ (/ (double (reduce + 0 vs)) (count vs)))))

(defn filter-min
  "MIN that ignores :__null__ sentinel values. Returns :__null__ if all filtered."
  [coll]
  (let [vs (remove #(= :__null__ %) coll)]
    (if (empty? vs) :__null__ (apply min vs))))

(defn filter-max
  "MAX that ignores :__null__ sentinel values. Returns :__null__ if all filtered."
  [coll]
  (let [vs (remove #(= :__null__ %) coll)]
    (if (empty? vs) :__null__ (apply max vs))))

(defn filter-count
  "SQL COUNT(col) — counts non-NULL values. Unlike COUNT(*), which counts
   all rows, COUNT(col) skips rows where col IS NULL. Returns a long
   (never NULL — COUNT of empty is 0)."
  [coll]
  (count (remove #(or (nil? %) (= :__null__ %)) coll)))

(defn filter-array-agg
  "SQL array_agg(col) — collect all non-NULL values into a PgArray.
   Element-type inferred from the first non-nil element; falls back
   to :text when the input is empty (PG would return NULL; we follow
   that by returning `:__null__`).

   Requires datahike.pg.arrays which lives below this ns on the load
   order — use requiring-resolve at call time to avoid a cycle."
  [coll]
  (let [arr-ns (some-> 'datahike.pg.arrays find-ns)
        _ (when-not arr-ns (require 'datahike.pg.arrays))
        vs (into []
                 (map #(if (= :__null__ %) nil %))
                 coll)
        arr-fn (resolve 'datahike.pg.arrays/array)
        pick-type (fn [v]
                    (cond
                      (instance? Long v)    :int8
                      (instance? Integer v) :int4
                      (integer? v)          :int8
                      (instance? Double v)  :float8
                      (float? v)            :float8
                      (boolean? v)          :bool
                      (inst? v)             :timestamptz
                      :else                 :text))
        first-v (some identity vs)
        elem-type (if (some? first-v) (pick-type first-v) :text)]
    (arr-fn elem-type vs)))

(defn filter-count-distinct
  "SQL COUNT(DISTINCT col) — counts distinct non-NULL values."
  [coll]
  (count (distinct (remove #(or (nil? %) (= :__null__ %)) coll))))

(defn- drop-nulls [coll]
  (remove #(or (nil? %) (= :__null__ %)) coll))

(defn- drop-pair-nulls
  "For pair-aggregate inputs `[[p v] …]`, drop pairs whose value is
   nil/sentinel. Used by percentile / mode aggregates that receive the
   ordered-set parameter alongside the per-row value."
  [coll]
  (remove (fn [[_ v]] (or (nil? v) (= :__null__ v))) coll))

(defn filter-percentile-cont
  "PG `PERCENTILE_CONT(p) WITHIN GROUP (ORDER BY x)` — linearly-
   interpolated continuous percentile. Receives a coll of `[p x]` pairs
   (every `p` is the same constant, threaded per-row by the translator
   so the aggregate fn can stay single-arg per Datahike's aggregate
   contract). Returns the interpolated p-th percentile of the x values,
   `:__null__` for an empty input."
  [coll]
  (let [pairs (drop-pair-nulls coll)
        vs    (sort (map second pairs))
        n     (count vs)]
    (cond
      (zero? n) :__null__
      (== 1 n)  (first vs)
      :else
      (let [p (double (first (first pairs)))
            idx (* p (dec n))
            lo (long (Math/floor idx))
            hi (long (Math/ceil idx))
            frac (- idx lo)]
        (+ (* (nth vs lo) (- 1.0 frac))
           (* (nth vs hi) frac))))))

(defn filter-percentile-disc
  "PG `PERCENTILE_DISC(p) WITHIN GROUP (ORDER BY x)` — discrete
   percentile, no interpolation. Returns the value at position
   `ceil(p * n)` (1-based) in the sorted non-null x values."
  [coll]
  (let [pairs (drop-pair-nulls coll)
        vs    (sort (map second pairs))
        n     (count vs)]
    (if (zero? n)
      :__null__
      (let [p (double (first (first pairs)))
            idx (max 0 (min (dec n) (dec (long (Math/ceil (* p n))))))]
        (nth vs idx)))))

(defn filter-mode
  "PG `MODE() WITHIN GROUP (ORDER BY x)` — most frequent value.
   Receives raw x values (no percentile parameter). Tie-broken by
   ORDER BY ascending, matching PG's stable-sort tiebreak."
  [coll]
  (let [vs (drop-nulls coll)]
    (if (empty? vs)
      :__null__
      (let [freq (frequencies vs)
            max-cnt (apply max (vals freq))
            winners (filter #(= max-cnt (val %)) freq)]
        (first (sort (map key winners)))))))

(defn filter-variance-samp
  "SQL VAR_SAMP(x) — sample variance, ignores :__null__/nil. Returns
   :__null__ when fewer than 2 non-null values remain (matches PG)."
  [coll]
  (let [vs (drop-nulls coll)
        n  (count vs)]
    (if (< n 2)
      :__null__
      (let [mean (/ (double (reduce + 0 vs)) n)
            ss   (reduce + 0.0 (map #(let [d (- (double %) mean)] (* d d)) vs))]
        (/ ss (dec n))))))

(defn filter-stddev-samp
  "SQL STDDEV_SAMP(x) — sample standard deviation. See filter-variance-samp."
  [coll]
  (let [v (filter-variance-samp coll)]
    (if (= :__null__ v) :__null__ (Math/sqrt (double v)))))

(defn filter-corr
  "SQL CORR(y, x) — Pearson correlation. Input is a collection of [x y]
   pairs (as produced by translate-select for two-arg CORR). Pairs where
   either element is :__null__/nil are dropped. Returns :__null__ when
   fewer than 2 pairs remain or the denominator is zero (one side is a
   constant)."
  [pairs]
  (let [valid (remove (fn [p]
                        (let [a (first p) b (second p)]
                          (or (nil? a) (= :__null__ a)
                              (nil? b) (= :__null__ b))))
                      pairs)
        n (count valid)]
    (if (< n 2)
      :__null__
      (let [xs  (map #(double (first %))  valid)
            ys  (map #(double (second %)) valid)
            sx  (reduce + 0.0 xs)
            sy  (reduce + 0.0 ys)
            sxx (reduce + 0.0 (map #(* % %) xs))
            syy (reduce + 0.0 (map #(* % %) ys))
            sxy (reduce + 0.0 (map * xs ys))
            denom (* (Math/sqrt (- (* n sxx) (* sx sx)))
                     (Math/sqrt (- (* n syy) (* sy sy))))]
        (if (zero? denom)
          :__null__
          (/ (- (* n sxy) (* sx sy)) denom))))))

;; ---------------------------------------------------------------------------
;; null-safe wrapper + SQL null-safe arithmetic
;; Resolved by Datahike's `resolve-fn` at query time from fully-qualified
;; symbols emitted by translate-binary-arith.

(defn null-safe
  "Wrap a scalar function `f` to propagate SQL NULL: if any argument is the
   `:__null__` sentinel (or Clojure nil), return `:__null__` without calling
   `f`. Otherwise apply `f` to the arguments.

   Used to make scalar SQL functions (upper, lower, substring, length, abs,
   arithmetic, ...) match SQL semantics: `UPPER(NULL) = NULL`, `LENGTH(NULL)
   = NULL`, `1 + NULL = NULL`. Without this guard, Clojure string/numeric
   functions throw on `:__null__` (a Keyword, not a String/Number)."
  [f]
  (fn null-safe-call
    ([]         (f))
    ([a]        (if (or (nil? a) (= :__null__ a)) :__null__ (f a)))
    ([a b]      (if (or (nil? a) (= :__null__ a) (nil? b) (= :__null__ b)) :__null__ (f a b)))
    ([a b c]    (if (or (nil? a) (= :__null__ a) (nil? b) (= :__null__ b) (nil? c) (= :__null__ c))
                  :__null__ (f a b c)))
    ([a b c & more]
     (if (or (nil? a) (= :__null__ a) (nil? b) (= :__null__ b) (nil? c) (= :__null__ c)
             (some #(or (nil? %) (= :__null__ %)) more))
       :__null__
       (apply f a b c more)))))

(def sql-+ (null-safe +))
(def sql-- (null-safe -))
(def sql-* (null-safe *))
(def sql-div (null-safe /))
(def sql-mod (null-safe rem))

;; ---------------------------------------------------------------------------
;; SQL string function implementations

(defn sql-lpad
  "Left-pad string s to length n with fill character/string."
  ([s n] (sql-lpad s n " "))
  ([s n fill]
   (let [s (str s) fill (str fill) n (long n)]
     (if (>= (count s) n) (subs s 0 n)
         (let [pad-len (- n (count s))
               full-fill (apply str (repeat (inc (quot pad-len (count fill))) fill))]
           (str (subs full-fill 0 pad-len) s))))))

(defn sql-rpad
  "Right-pad string s to length n with fill character/string."
  ([s n] (sql-rpad s n " "))
  ([s n fill]
   (let [s (str s) fill (str fill) n (long n)]
     (if (>= (count s) n) (subs s 0 n)
         (let [pad-len (- n (count s))
               full-fill (apply str (repeat (inc (quot pad-len (count fill))) fill))]
           (str s (subs full-fill 0 pad-len)))))))

(defn sql-repeat
  "Repeat string s n times."
  [s n] (apply str (repeat (long n) (str s))))

(defn sql-initcap
  "Capitalize first letter of each word."
  [s] (str/replace (str s) #"\b\w" str/upper-case))

(defn sql-left
  "Return first n characters of string."
  [s n] (let [s (str s) n (long n)] (subs s 0 (min n (count s)))))

(defn sql-right
  "Return last n characters of string."
  [s n] (let [s (str s) n (long n)] (subs s (max 0 (- (count s) n)))))

(defn sql-position
  "Return 1-based position of substring in string, 0 if not found."
  [substring string]
  (let [idx (.indexOf (str string) (str substring))]
    (if (neg? idx) 0 (inc idx))))

;; ---------------------------------------------------------------------------
;; PostgreSQL catalog function stubs
;; Minimal implementations for ORM/driver/tool compatibility.

(defn pg-table-is-visible
  "Stub: always returns true (all tables are visible in the default schema)."
  [_oid] true)

(defn pg-format-type
  "Convert a type OID + type modifier to a type name string.
   Delegates to the centralized type registry."
  [type-oid _typmod]
  (types/format-type type-oid _typmod))

(defn pg-get-expr
  "Return the expression text as-is (CockroachDB's approach)."
  [expr-text _relation-oid] (str expr-text))

;; ---------------------------------------------------------------------------
;; Lookup tables used by translate-*.
;; Aggregate symbols resolve via `datahike.pg.sql/filter-*` — those names
;; are `def`-aliased from this ns in sql.clj to preserve the historical
;; emitted path (matters for clients that cache prepared-statement
;; parse results across deploys).

(def sql-aggregate->datalog
  "Map SQL aggregate function names (lowercased) to Datalog aggregate
   symbols. SUM/AVG/MIN/MAX route to our `filter-*` variants so they
   skip the `:__null__` sentinel that col-var! emits for missing
   attribute values — Datahike's raw `sum`/`avg` would throw trying
   `(+ :__null__ 0)`, and raw `min`/`max` would mis-order a keyword
   against a number.

   COUNT(*) is handled specially in translate-select (counts entities,
   always produces a long). COUNT(col) / COUNT(DISTINCT col) route to
   `filter-count` / `filter-count-distinct` which skip NULLs per SQL
   spec (PG: 'count(expression) returns the number of non-null input
   rows')."
  {"count"          'datahike.pg.sql/filter-count
   "sum"            'datahike.pg.sql/filter-sum
   "avg"            'datahike.pg.sql/filter-avg
   "min"            'datahike.pg.sql/filter-min
   "max"            'datahike.pg.sql/filter-max
   "count_distinct" 'datahike.pg.sql/filter-count-distinct
   "stddev"         'datahike.pg.sql/filter-stddev-samp
   "stddev_samp"    'datahike.pg.sql/filter-stddev-samp
   "stddev_pop"     'stddev
   "variance"       'datahike.pg.sql/filter-variance-samp
   "var_samp"       'datahike.pg.sql/filter-variance-samp
   "var_pop"        'variance
   "median"         'median
   "corr"           'datahike.pg.sql/filter-corr
   "array_agg"      'datahike.pg.sql/filter-array-agg
   ;; Ordered-set aggregates — `WITHIN GROUP (ORDER BY x)` syntax.
   ;; Translator routes them through the pair-aggregate path (like
   ;; corr) since the percentile fraction is a constant alongside
   ;; the per-row value, and Datalog aggregates take a single coll.
   "percentile_cont" 'datahike.pg.sql/filter-percentile-cont
   "percentile_disc" 'datahike.pg.sql/filter-percentile-disc
   "mode"            'datahike.pg.sql/filter-mode})

(defn aggregate-function? [^String fname]
  (contains? sql-aggregate->datalog (str/lower-case fname)))

(def sql-fn->clj-fn
  "Map of SQL function names (lowercased) to Clojure fn values.

   Values are actual `IFn` objects (not symbols) so we can wrap them in
   `null-safe` at emit time. Java static methods are wrapped in thin fns
   so they're callable through the same path."
  {"upper"    str/upper-case
   "lower"    str/lower-case
   "length"   count
   "abs"      clojure.core/abs
   "round"    #(Math/round (double %))
   "sqrt"     #(Math/sqrt (double %))
   "floor"    #(Math/floor (double %))
   "ceil"     #(Math/ceil (double %))
   "ceiling"  #(Math/ceil (double %))
   "trim"     str/trim
   "ltrim"    str/triml
   "rtrim"    str/trimr
   "replace"  str/replace
   "greatest" clojure.core/max
   "least"    clojure.core/min
   "sign"     #(Math/signum (double %))
   "exp"      #(Math/exp (double %))
   "log"      #(Math/log (double %))
   "power"    #(Math/pow (double %1) (double %2))
   "mod"      rem
   "lpad"     sql-lpad
   "rpad"     sql-rpad
   "repeat"   sql-repeat
   "initcap"  sql-initcap
   "reverse"  str/reverse
   "left"     sql-left
   "right"    sql-right
   "char_length"  count
   "octet_length" count
   "position"     sql-position
   "strpos"       sql-position
   "pg_table_is_visible"  pg-table-is-visible
   "format_type"          pg-format-type
   "pg_get_expr"          pg-get-expr})
