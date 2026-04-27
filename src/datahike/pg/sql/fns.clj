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
  "SUM that ignores :__null__ sentinel values. Returns :__null__ if all filtered.
   Uses Clojure's auto-promoting `+` so int8 inputs that overflow
   silently become BigInt — `filter-sum-numeric` is the variant the
   translator picks for SUM(int8) / SUM(numeric) where the result OID
   is NUMERIC; this one stays for SUM(int4) / SUM(float*)."
  [coll]
  (let [vs (remove #(= :__null__ %) coll)]
    (if (empty? vs) :__null__ (reduce + 0 vs))))

(defn filter-sum-numeric
  "SUM with explicit BigDecimal accumulation. PG returns NUMERIC for
   SUM(int8) and SUM(numeric) to avoid overflow; this variant matches
   that. Coerces inputs to BigDecimal once at the boundary so
   `+` stays primitive across the reduce. Skips nil and `:__null__`
   sentinels."
  [coll]
  (let [vs (remove #(or (nil? %) (= :__null__ %)) coll)]
    (if (empty? vs)
      :__null__
      (reduce (fn [^java.math.BigDecimal acc v]
                (.add acc (cond
                            (instance? java.math.BigDecimal v) v
                            (instance? java.math.BigInteger v) (java.math.BigDecimal. ^java.math.BigInteger v)
                            (integer? v) (java.math.BigDecimal/valueOf (long v))
                            (float? v)   (java.math.BigDecimal/valueOf (double v))
                            :else        (java.math.BigDecimal. (str v)))))
              java.math.BigDecimal/ZERO
              vs))))

(defn filter-avg
  "AVG that ignores :__null__ sentinel values. Returns :__null__ if all filtered.
   Returns Double — used for AVG(float4) / AVG(float8). For AVG(int*)
   / AVG(numeric) the translator picks `filter-avg-numeric` which
   preserves precision via BigDecimal."
  [coll]
  (let [vs (remove #(= :__null__ %) coll)]
    (if (empty? vs) :__null__ (/ (double (reduce + 0 vs)) (count vs)))))

(def ^:private ^:const numeric-min-sig-digits
  "Mirrors PG's `NUMERIC_MIN_SIG_DIGITS` (16). AVG / division aim for
   at least this many significant digits so numeric is no less
   accurate than float8."
  16)

(def ^:private ^:const numeric-max-display-scale
  "PG's NUMERIC_MAX_DISPLAY_SCALE upper bound. PG itself defaults to a
   high cap; 1000 is enough headroom for any realistic AVG and well
   below BigDecimal's intrinsic limits."
  1000)

(defn- decimal-weight
  "Approximate PG `NumericVar.weight` for a BigDecimal: index in
   DEC_DIGITS=4 base of the leading non-zero digit. Zero for
   `|value| < 10000`, 1 for 10000..99999999, etc. Negative for pure
   fractions (`0.0001..0.9999` is weight -1).

   Matches the integer-part-weight half of PG's weight formula. For
   AVG, where we only care about the magnitude relationship, this is
   sufficient."
  [^java.math.BigDecimal v]
  (if (zero? (.signum v))
    0
    (let [int-digits (- (.precision v) (.scale v))]
      ;; int-digits 1..4 → weight 0
      ;; int-digits 5..8 → weight 1
      ;; int-digits ≤ 0  → weight ≈ -ceil((-int-digits + 1) / 4) (fractional)
      (if (pos? int-digits)
        (long (Math/floor (/ (double (dec int-digits)) 4.0)))
        (long (Math/floor (/ (double (dec int-digits)) 4.0)))))))

(defn- leading-dec-digit-chunk
  "PG's NumericDigit base-10000 leading chunk: the value of the first
   DEC_DIGITS=4 group of decimal digits, or 0 for zero. Used by PG's
   select_div_scale to decide whether to subtract 1 from qweight when
   the dividend's leading digit ≤ the divisor's."
  [^java.math.BigDecimal v]
  (if (zero? (.signum v))
    0
    (let [unscaled (.unscaledValue (.abs v))
          s (.toString unscaled)
          ;; Take leading 1..4 digits, padded to 4 with trailing zeros
          ;; so 550 → 5500 (one DEC_DIGITS chunk), comparable across
          ;; magnitudes the way PG's first-digit comparison is.
          digits-needed 4
          chunk (if (>= (count s) digits-needed)
                  (subs s 0 digits-needed)
                  (str s (apply str (repeat (- digits-needed (count s)) \0))))]
      (Long/parseLong chunk))))

(defn- select-div-scale
  "PG-faithful scale selection for division (matches `select_div_scale`
   in src/backend/utils/adt/numeric.c).

   `rscale = NUMERIC_MIN_SIG_DIGITS - qweight * DEC_DIGITS`
     where qweight ≈ weight(sum) - weight(count) (- 1 when the
     leading DEC_DIGITS chunk of the sum is ≤ the count's chunk —
     the quotient might land one digit below the naive estimate).
   Then floor by max of input dscales, by 0, ceiling by
   NUMERIC_MAX_DISPLAY_SCALE."
  [^java.math.BigDecimal sum-bd n-count]
  (let [sum-scale (.scale sum-bd)
        sum-w (decimal-weight sum-bd)
        n-bd (java.math.BigDecimal/valueOf (long n-count))
        n-w (decimal-weight n-bd)
        sum-chunk (leading-dec-digit-chunk sum-bd)
        n-chunk (leading-dec-digit-chunk n-bd)
        qweight (cond-> (- sum-w n-w)
                  (<= sum-chunk n-chunk) dec)
        rscale (max (- numeric-min-sig-digits (* qweight 4))
                    sum-scale
                    0)]
    (min rscale numeric-max-display-scale)))

(defn filter-avg-numeric
  "AVG with BigDecimal precision. Matches PG's AVG(int*)→numeric and
   AVG(numeric)→numeric. Scale tracks PG's `select_div_scale`: at
   least 16 significant digits, no less than the input column's
   scale, and adjusted down for sums whose magnitude already takes
   most of the available digits.

   Concretely: AVG(int) on small values → 16 fractional digits (same
   as PG); AVG(NUMERIC(p, 2)) → max(16, 2) = 16 digits; AVG of a sum
   with weight 4+ digits in the integer part → fewer fractional
   digits, mirroring PG's precision-economy rule.

   Skips nil and `:__null__` sentinels."
  [coll]
  (let [vs (remove #(or (nil? %) (= :__null__ %)) coll)]
    (if (empty? vs)
      :__null__
      (let [sum (reduce (fn [^java.math.BigDecimal acc v]
                          (.add acc (cond
                                      (instance? java.math.BigDecimal v) v
                                      (instance? java.math.BigInteger v) (java.math.BigDecimal. ^java.math.BigInteger v)
                                      (integer? v) (java.math.BigDecimal/valueOf (long v))
                                      (float? v)   (java.math.BigDecimal/valueOf (double v))
                                      :else        (java.math.BigDecimal. (str v)))))
                        java.math.BigDecimal/ZERO
                        vs)
            n-count (count vs)
            n-bd (java.math.BigDecimal/valueOf (long n-count))
            rscale (int (select-div-scale sum n-count))]
        (.divide ^java.math.BigDecimal sum
                 ^java.math.BigDecimal n-bd
                 rscale
                 java.math.RoundingMode/HALF_UP)))))

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

(defn pg-many-ref-array
  "Per-row Datalog fn for `:db.cardinality/many :db.type/ref` SQL
   projection: given the source entity-id, fetch all values of
   `ref-attr` (each is a target entity-id), look up `target-pk-attr`
   on each, and return a PgArray of the resulting PK values.

   Empty for source entities with no ref values — matches what a
   real PG `int[]` column would render for a row that has no
   elements. Avoids the Datalog `array_agg` + or-join dance because
   it runs per-row in pure Clojure: the source entity is already
   bound, so we just do two lookups and box.

   Used as `[(?pg-many-ref-array $ ?source-eid :order/tags :tag/id) ?out]`
   in the translator-emitted Datalog query."
  [db source-eid ref-attr target-pk-attr]
  (let [arr-ns (some-> 'datahike.pg.arrays find-ns)
        _ (when-not arr-ns (require 'datahike.pg.arrays))
        arr-fn (resolve 'datahike.pg.arrays/array)
        ;; eavt scan: all `[source-eid ref-attr v]` datoms.
        datoms-fn (requiring-resolve 'datahike.api/datoms)
        target-eids (mapv :v (datoms-fn db {:index :eavt
                                            :components [source-eid ref-attr]}))
        ;; Per-target lookup of the PK value. Nil targets (deleted
        ;; or schema-attr eids) drop out; real-target-with-no-PK
        ;; passes through nil, matching PG's NULL element.
        pks (vec
             (for [eid target-eids
                   :let [pk-datoms (datoms-fn db {:index :eavt
                                                  :components [eid target-pk-attr]})
                         v (some-> ^datahike.datom.Datom (first pk-datoms) .-v)]
                   :when (some? v)]
               v))
        ;; Element type from the target PK attr's :db/valueType, NOT
        ;; from the first sample value (which would mistype an empty
        ;; array as :text). Defaults to :int8 — most FK PKs are bigint.
        schema (:schema db)
        target-vtype (get-in schema [target-pk-attr :db/valueType])
        elem-type (case target-vtype
                    :db.type/long    :int8
                    :db.type/string  :text
                    :db.type/uuid    :uuid
                    :db.type/bigdec  :numeric
                    :db.type/instant :timestamptz
                    :int8)]
    (arr-fn elem-type pks)))

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

;; The other `pg_*_is_visible` predicates: psql's `\df`, `\da`, `\dT`,
;; `\dF`, `\dD` all gate their list queries on these. We don't model
;; per-namespace visibility (everything lives in `public`), so they
;; mirror pg_table_is_visible's stub.
(def pg-function-is-visible (constantly true))
(def pg-type-is-visible (constantly true))
(def pg-namespace-is-visible (constantly true))
(def pg-ts-config-is-visible (constantly true))
(def pg-operator-is-visible (constantly true))
(def pg-conversion-is-visible (constantly true))

;; Function-introspection stubs. psql `\df` / `\sf` / `\df+` issue
;; these against pg_proc. We don't surface user-defined fns yet, so
;; they return empty strings — psql renders an empty cell rather than
;; failing.
(def pg-get-function-arguments (constantly ""))
(def pg-get-function-result    (constantly ""))
(def pg-get-functiondef        (constantly ""))
(def pg-get-function-identity-arguments (constantly ""))

;; Size functions. psql `\d+` / `\dt+` show "Size" via
;; `pg_size_pretty(pg_table_size(c.oid))`. We don't track on-disk
;; size for Datahike-backed tables; report 0 with a human-formatted
;; pretty-print so the cell isn't blank.
(def pg-table-size      (constantly 0))
(def pg-relation-size   (constantly 0))
(def pg-indexes-size    (constantly 0))
(def pg-database-size   (constantly 0))
(def pg-total-relation-size (constantly 0))

(defn pg-size-pretty
  "Format a byte count as PG's pretty string ('1024 bytes', '12 kB',
   '5 MB'). Mirrors `pg_size_pretty` in src/backend/utils/adt/dbsize.c."
  [bytes]
  (let [n (long (or bytes 0))]
    (cond
      (< n 10240)              (str n " bytes")
      (< n (* 10240 1024))     (str (long (/ (+ n 512) 1024)) " kB")
      (< n (* 10240 1024 1024)) (str (long (/ (+ n (* 512 1024)) (* 1024 1024))) " MB")
      :else                    (str (long (/ (+ n (* 512 1024 1024)) (* 1024 1024 1024))) " GB"))))

;; Encoding/locale stubs. psql `\l` shows server encoding via
;; `pg_encoding_to_char(d.encoding)`. We're always UTF8 — no other
;; encoding is meaningful for in-memory Datahike.
(def pg-encoding-to-char (constantly "UTF8"))

;; Object-definition reconstructors. psql `\d <table>`, `\d+`, `\dY`
;; query these against pg_statistic_ext, pg_partitioned_table,
;; pg_view, pg_trigger, pg_rewrite. We don't track any of these
;; entities, so reconstruct returns nil — psql renders no row /
;; "(none)" / blank.
(def pg-get-statisticsobjdef        (constantly :__null__))
(def pg-get-statisticsobjdef-columns (constantly :__null__))
(def pg-get-statisticsobjdef-expressions (constantly :__null__))
(def pg-get-partkeydef              (constantly :__null__))
(def pg-get-viewdef                 (constantly :__null__))
(def pg-get-triggerdef              (constantly :__null__))
(def pg-get-ruledef                 (constantly :__null__))
(def pg-get-publication-tables      (constantly :__null__))
(def pg-tablespace-location         (constantly :__null__))

;; Publication / replication stubs. psql `\d <table>` (12+) probes
;; pg_relation_is_publishable to render the "Publications" footer.
;; We don't model logical replication; everything is publishable but
;; nothing is published.
(def pg-relation-is-publishable     (constantly true))
(def pg-get-replica-identity-index  (constantly :__null__))
(def pg-get-replica-identity        (constantly "d"))

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
   "pg_table_is_visible"      pg-table-is-visible
   "pg_function_is_visible"   pg-function-is-visible
   "pg_type_is_visible"       pg-type-is-visible
   "pg_namespace_is_visible"  pg-namespace-is-visible
   "pg_ts_config_is_visible"  pg-ts-config-is-visible
   "pg_operator_is_visible"   pg-operator-is-visible
   "pg_conversion_is_visible" pg-conversion-is-visible
   "pg_get_function_arguments" pg-get-function-arguments
   "pg_get_function_result"    pg-get-function-result
   "pg_get_functiondef"        pg-get-functiondef
   "pg_get_function_identity_arguments" pg-get-function-identity-arguments
   "pg_table_size"          pg-table-size
   "pg_relation_size"       pg-relation-size
   "pg_indexes_size"        pg-indexes-size
   "pg_database_size"       pg-database-size
   "pg_total_relation_size" pg-total-relation-size
   "pg_size_pretty"         pg-size-pretty
   "pg_encoding_to_char"    pg-encoding-to-char
   "pg_get_statisticsobjdef"            pg-get-statisticsobjdef
   "pg_get_statisticsobjdef_columns"    pg-get-statisticsobjdef-columns
   "pg_get_statisticsobjdef_expressions" pg-get-statisticsobjdef-expressions
   "pg_get_partkeydef"      pg-get-partkeydef
   "pg_get_viewdef"         pg-get-viewdef
   "pg_get_triggerdef"      pg-get-triggerdef
   "pg_get_ruledef"         pg-get-ruledef
   "pg_get_publication_tables" pg-get-publication-tables
   "pg_tablespace_location" pg-tablespace-location
   "pg_relation_is_publishable"   pg-relation-is-publishable
   "pg_get_replica_identity_index" pg-get-replica-identity-index
   "pg_get_replica_identity"       pg-get-replica-identity
   "format_type"          pg-format-type
   "pg_get_expr"          pg-get-expr})
