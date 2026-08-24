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
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.bits :as pg-bits]
            [datahike.pg.errors :as errors]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.types :as types]
            [datahike.pg.prng]))

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

(defn filter-sum-float4
  "SUM over a `real` column, accumulated AT float4 precision.

   PostgreSQL's sum(float4) uses float4pl, so the running total is a
   float4 throughout and the result is a float4. Reducing with Clojure's
   `+` promotes to double on the first step, which is why `sum(r)` on a
   single 1.1::real answered 1.100000023841858 -- the widened float --
   rather than 1.1."
  [coll]
  (let [vs (remove #(or (nil? %) (= :__null__ %)) coll)]
    (if (empty? vs)
      :__null__
      ;; Clojure has no float primitive, so the narrowing is explicit at
      ;; each step -- which is exactly what float4pl does.
      (reduce (fn [acc v] (float (+ (float acc) (float v)))) (float 0) vs))))

(defn ->bigdec
  "Coerce one aggregate input to BigDecimal. The numeric SUM/AVG
   runtimes share it so they widen identically."
  ^java.math.BigDecimal [v]
  (cond
    (instance? java.math.BigDecimal v) v
    (instance? java.math.BigInteger v) (java.math.BigDecimal. ^java.math.BigInteger v)
    (integer? v) (java.math.BigDecimal/valueOf (long v))
    (float? v)   (java.math.BigDecimal/valueOf (double v))
    :else        (java.math.BigDecimal. (str v))))

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
      (reduce (fn [^java.math.BigDecimal acc v] (.add acc (->bigdec v)))
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

(def ^:private ^:const dec-digits 4)                ;; DEC_DIGITS, NBASE = 10000

(defn- nbase-weight+first
  "PostgreSQL stores a numeric as base-10000 digit groups. Return
   `[weight firstdigit]` -- the exponent of the leading group and its
   value -- which is the form select_div_scale is defined in, so the
   division result scale cannot be derived without reconstructing them."
  [^java.math.BigDecimal v]
  (if (zero? (.signum v))
    [0 0]
    (let [digits (.toString (.abs (.unscaledValue v)))
          p (count digits)
          ;; decimal exponent of the leading significant digit
          e (- p (.scale v) 1)
          weight (Math/floorDiv (long e) (long dec-digits))
          ;; how many of the leading decimal digits fall in that group
          k (int (inc (- e (* dec-digits weight))))
          grp (if (<= k p)
                (subs digits 0 k)
                (apply str digits (repeat (- k p) \0)))]
      [weight (Long/parseLong grp)])))

(defn- div-result-scale
  "PostgreSQL's select_div_scale (numeric.c): pick a result scale giving
   at least NUMERIC_MIN_SIG_DIGITS significant digits -- so numeric
   division is no less accurate than float8 -- but never fewer decimals
   than either input already displays.

   This is why `10.0 / 3` is 3.3333333333333333 and `1.0 / 3.0` is
   0.33333333333333333333: the quotient's estimated weight, not the
   inputs' scales, sets the digit count."
  ^long [^java.math.BigDecimal a ^java.math.BigDecimal b]
  (let [[w1 f1] (nbase-weight+first a)
        [w2 f2] (nbase-weight+first b)
        ;; Estimated weight of the quotient. When the leading groups are
        ;; equal PostgreSQL cannot tell and assumes a < b.
        qweight (cond-> (- (long w1) (long w2)) (<= (long f1) (long f2)) dec)]
    (-> (- numeric-min-sig-digits (* qweight dec-digits))
        (max (.scale a))
        (max (.scale b))
        (max 0)
        (min numeric-max-display-scale)
        long)))

(defn avg-numeric-of
  "AVG over a group already reduced to its SUM and its non-null COUNT.

   The numeric AVG depends on nothing else, which is what lets the
   window engine keep a RUNNING sum over an expanding frame and still
   get the byte-identical answer `filter-avg-numeric` gives for the
   whole frame -- rather than recomputing the aggregate from scratch at
   every row, or (as it did) inventing a second, double-precision AVG
   of its own."
  [^java.math.BigDecimal sum ^long n]
  (if (zero? n)
    :__null__
    (let [n-bd (java.math.BigDecimal/valueOf n)
          ;; The same select_div_scale port the `/` operator uses.
          ;; This site used to carry its own AVG-specialised copy,
          ;; with its own weight/leading-digit helpers and its own
          ;; duplicate of the two PG constants -- two ports of one
          ;; rule, free to drift.
          rscale (int (div-result-scale sum n-bd))]
      (.divide sum n-bd rscale java.math.RoundingMode/HALF_UP))))

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
      (avg-numeric-of (reduce (fn [^java.math.BigDecimal acc v]
                                (.add acc (->bigdec v)))
                              java.math.BigDecimal/ZERO
                              vs)
                      (count vs)))))

(defn sql-null?
  "SQL NULL, however it is carried. NULL travels as the `:__null__`
   sentinel rather than nil, because a datalog function binding that
   yields nil FILTERS THE ROW — so every NULL-producing fn returns the
   sentinel instead. Both spellings must count.

   A single predicate rather than `(not (contains? …))`: datahike reads
   a nested `(not <seq>)` inside a function-binding clause as
   negation-as-failure, which filters instead of binding."
  [v]
  (or (nil? v) (= :__null__ v)))

(defn sql-not-null?
  "Complement of `sql-null?`, as one predicate — see the note there."
  [v]
  (not (or (nil? v) (= :__null__ v))))

(defn- no-order-agg!
  "PostgreSQL has no min/max aggregate for these types — not an oversight
   on our side, and still absent on master (pg_aggregate.dat lists text,
   bpchar, the numerics, date/time/timestamp(tz), interval, inet, money,
   oid, record, anyarray and anyenum, but never bool, uuid or jsonb).
   Raise 42883 as PostgreSQL does rather than a ClassCastException."
  [fname tname]
  (throw (errors/pg-error
          :undefined-function
          {:function (str fname "(" tname ")")
           :hint (str "No function matches the given name and argument types. "
                      "You might need to add explicit type casts.")})))

(defn- order-agg-type-name
  "The PG type name to report when a value has no min/max aggregate, or
   nil when the value is orderable. Only `bool` and `uuid` qualify: they
   have no min/max aggregate in pg_aggregate.dat on any release,
   including master. `bytea` DOES have one upstream — we track upstream
   rather than pinning to the version we advertise, since PostgreSQL
   stays backwards compatible with clients, so implementing the newer
   behaviour costs nothing while a client gating on server_version never
   asks for it.

   A jsonb value is a String at this point and so is indistinguishable
   from text; `max(jsonb)` therefore answers instead of raising. Closing
   that needs the declared column type down here, which this layer does
   not carry."
  [v]
  (cond
    (instance? Boolean v)      "boolean"
    (instance? java.util.UUID v) "uuid"
    :else nil))

(defn- numspecial? [x] (types/numeric-special? x))

(defn- ->num-double
  "A numeric operand as a double, for the special-value paths."
  ^double [x]
  (if (numspecial? x) (types/numeric-special->double x) (double x)))

(defn- special-arith
  "Arithmetic where at least one operand is a numeric NaN or +-Infinity.

   Every such result is either another special or a value the double
   computation gets exactly right -- `Inf - Inf` is NaN, `Inf * 0` is
   NaN, `x / Inf` is 0 -- so routing through double loses nothing, and
   PostgreSQL's numeric special rules are IEEE's."
  [f a b]
  (let [r (f (->num-double a) (->num-double b))]
    (or (types/double->numeric-special r)
        (java.math.BigDecimal/valueOf r))))

(defn- numeric-special-cmp
  "PostgreSQL's numeric ordering, which is total: NaN above everything
   and equal to itself, then +Infinity, then the finite values, then
   -Infinity (numeric.c cmp_numerics)."
  ^long [a b]
  (let [rank (fn [x] (case (:kind x) :nan 2 :inf 1 :-inf -1))]
    (cond
      (and (numspecial? a) (numspecial? b))
      (let [ra (rank a) rb (rank b)] (long (compare ra rb)))
      (numspecial? a) (long (if (= :-inf (:kind a)) -1 1))
      (numspecial? b) (long (if (= :-inf (:kind b)) 1 -1))
      :else 0)))

(defn nan-num? [x]
  (and (number? x) (Double/isNaN (double x))))

(defn order-cmp
  "`compare`, with two corrections.

   A `byte[]` is not Comparable — PostgreSQL orders bytea by unsigned
   byte value (`byteacmp`, varlena.c), which is what compareUnsigned
   does.

   And NaN. PostgreSQL sorts NaN GREATER than every non-NaN and equal to
   itself (float.c float8_cmp_internal), giving a total order. Clojure's
   `compare` routes numbers through an `lt`-based comparison, so NaN
   compares EQUAL to everything:

     (compare ##NaN 1.0)          => 0
     (compare 1.0 ##NaN)          => 0
     (sort [1.0 ##NaN 0.5 2.0])   => (1.0 ##NaN 0.5 2.0)

   -- not merely mis-ordered but silently unsorted, and a comparator
   that is not transitive can also make a TimSort raise \"Comparison
   method violates its general contract\". So this has to be right
   BEFORE a NaN can reach a sort, which is why it lands with the change
   that lets NaN into the system at all."
  ^long [a b]
  (cond
    (and (bytes? a) (bytes? b))
    (java.util.Arrays/compareUnsigned ^bytes a ^bytes b)
    (or (types/numeric-special? a) (types/numeric-special? b))
    (numeric-special-cmp a b)
    (nan-num? a) (if (nan-num? b) 0 1)
    (nan-num? b) -1
    :else (compare a b)))

(defn order-key-cmp
  "Compare two ORDER BY key values. `dir` is :asc or :desc; `nulls` is
   :first, :last, or nil for PostgreSQL's default -- NULLS LAST for ASC
   and NULLS FIRST for DESC, i.e. NULL sorts as the largest value.

   The null half of an ORDER BY comparator, in one place. Both the
   server's ORDER BY fallback and the window engine's within-partition
   sort need it, and the window copy had drifted: it pinned nulls LAST
   in both directions and compared with `compare` rather than
   `order-cmp`, so a DESC window ordered its NULLs the wrong end and a
   NaN sort key left the partition silently unsorted."
  ^long [va vb dir nulls]
  (let [a-null? (or (nil? va) (= :__null__ va))
        b-null? (or (nil? vb) (= :__null__ vb))
        nulls-first? (if nulls (= nulls :first) (= dir :desc))]
    (cond
      (and a-null? b-null?) 0
      a-null? (if nulls-first? -1 1)
      b-null? (if nulls-first? 1 -1)
      (= dir :desc) (order-cmp vb va)
      :else (order-cmp va vb))))

(defn- order-agg
  "Reduce with `order-cmp` rather than `clojure.core/min`/`max`, which are
   NUMERIC-ONLY — they cast to Number, so MIN/MAX over any other type
   died with a raw `class java.lang.String cannot be cast to class
   java.lang.Number`. SQL orders every scalar type, and `max(name)` is
   ordinary SQL.

   This only ever fired when two or more values were actually compared:
   `apply max` on a one-element seq returns it untouched, so single-row
   groups and `WHERE`-narrowed aggregates looked fine.

   String comparison is Java's UTF-16 code-unit order, i.e. C collation
   — the same choice already made and documented for jsonb ordering."
  [fname pick coll]
  (let [vs (remove #(= :__null__ %) coll)]
    (if (empty? vs)
      :__null__
      (do
        (when-let [tname (order-agg-type-name (first vs))]
          (no-order-agg! fname tname))
        (reduce (fn [a b] (if (pick (order-cmp b a)) b a)) vs)))))

(defn filter-min
  "MIN that ignores :__null__ sentinel values. Returns :__null__ if all filtered."
  [coll]
  (order-agg "min" neg? coll))

(defn filter-max
  "MAX that ignores :__null__ sentinel values. Returns :__null__ if all filtered."
  [coll]
  (order-agg "max" pos? coll))

(defn filter-count
  "SQL COUNT(col) — counts non-NULL values. Unlike COUNT(*), which counts
   all rows, COUNT(col) skips rows where col IS NULL. Returns a long
   (never NULL — COUNT of empty is 0)."
  [coll]
  (count (remove #(or (nil? %) (= :__null__ %)) coll)))

(def filtered-out
  "Marker for a row an aggregate FILTER excluded.

   Distinct from `:__null__` because the two mean different things to the
   aggregates that PRESERVE nulls: `array_agg(x) FILTER (WHERE p)` keeps a
   NULL x on a row that passes p, and drops the row entirely on one that
   does not. Every other aggregate skips nulls anyway, so it only has to
   be told apart here."
  ::filtered-out)

(defn- box-array-agg
  "Box an ordered seq of array_agg element values (`:__null__` → nil) into a
   PgArray, inferring the element type from the first non-nil value."
  [ordered-vals]
  (let [vs (into [] (comp (remove #(= filtered-out %))
                          (map #(if (= :__null__ %) nil %)))
                 ordered-vals)
        arr-fn pg-arr/array
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
    ;; PostgreSQL's array_agg over NO rows is NULL, not an empty array --
    ;; which the docstring already claimed and the code did not do. Only
    ;; visible once an aggregate could see an empty group at all: a window
    ;; frame that excludes every row (`ROWS BETWEEN 2 PRECEDING AND 1
    ;; PRECEDING` on the first row) or a FILTER that admits none.
    (if (empty? vs) :__null__ (arr-fn elem-type vs))))

(defn filter-array-agg
  "SQL array_agg(col) — collect all non-NULL values into a PgArray.
   Element-type inferred from the first non-nil element; falls back
   to :text when the input is empty (PG would return NULL; we follow
   that by returning `:__null__`)."
  [coll]
  (box-array-agg coll))

(defn filter-jsonb-agg
  "SQL `jsonb_agg(expr)` — ONE jsonb array over the whole group.

   Was a per-row fn wrapping each value in its own array, so
   `SELECT jsonb_agg(id) FROM t` returned one row per input row instead
   of one row holding them all. Registering it in
   `sql-aggregate->datalog` is what makes datalog fold it over the
   group; `array_agg` next door is the same shape.

   NULLs are kept — PostgreSQL's jsonb_agg emits JSON null for them,
   unlike array_agg — and an empty group is SQL NULL."
  [coll]
  (let [vs (into [] (comp (remove #(= filtered-out %))
                          (map (fn [v] (if (= :__null__ v) :datahike.pg.jsonb/json-null v))))
                 coll)]
    (if (empty? vs) :__null__ vs)))

(defn- akey-compare
  "Null-safe comparator for in-aggregate `ORDER BY` keys. A key is a
   scalar or a vector (multiple ORDER BY columns, compared
   lexicographically). NULLs sort last (PG's default NULLS LAST for
   ASC).

   `:__null__` counts as NULL, not just `nil`. SQL NULL is carried as
   that sentinel everywhere else in this namespace, and it is what a
   nullable ORDER BY column actually binds to — so ordering by one threw
   `class clojure.lang.Keyword cannot be cast to class java.lang.String`
   rather than sorting it last."
  [a b]
  (let [null? (fn [x] (or (nil? x) (= :__null__ x)))]
    (cond
      (and (vector? a) (vector? b))
      (loop [a a b b]
        (cond (and (empty? a) (empty? b)) 0
              (empty? a) -1
              (empty? b) 1
              :else (let [c (akey-compare (first a) (first b))]
                      (if (zero? c) (recur (subvec a 1) (subvec b 1)) c))))
      (= a b) 0
      (null? a) 1
      (null? b) -1
      :else (compare a b))))

(defn filter-array-agg-ordered
  "SQL array_agg(expr ORDER BY … ASC) — `coll` is a collection of
   [sort-key value] pairs; sort ascending by sort-key, then box the values."
  [coll]
  (box-array-agg (map second (sort-by first akey-compare coll))))

(defn filter-array-agg-ordered-desc
  "SQL array_agg(expr ORDER BY … DESC) — descending counterpart of
   filter-array-agg-ordered."
  [coll]
  (box-array-agg (map second (sort-by first (fn [a b] (akey-compare b a)) coll))))

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
        datoms-fn d/datoms
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
        schema (dbi/-schema db)
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

(defn- bd-sqrt
  "Square root of a non-negative BigDecimal, rounded HALF_UP to `rscale`
   decimals -- PostgreSQL's `sqrt_var`, which computes to at least one
   guard digit and then `round_var`s to the requested scale."
  ^java.math.BigDecimal [^java.math.BigDecimal x ^long rscale]
  (if (zero? (.signum x))
    (.setScale java.math.BigDecimal/ZERO (int rscale))
    (let [mc (java.math.MathContext. (int (+ (.precision x) rscale 4))
                                     java.math.RoundingMode/HALF_UP)]
      (.setScale (.sqrt x mc) (int rscale) java.math.RoundingMode/HALF_UP))))

(defn- numeric-stddev-internal
  "PostgreSQL's `numeric_stddev_internal` (numeric.c), exactly.

   VAR_SAMP / VAR_POP / STDDEV_SAMP / STDDEV_POP over int2, int4, int8 or
   numeric return NUMERIC in PostgreSQL -- only the float4 / float8
   overloads return float8. Computing them in double instead was wrong
   twice over: the last digits differed (PG 26754.55172957, ours
   26754.55172956557, because the numeric result carries
   select_div_scale's digit count rather than a double's), and
   `stddev(bigint)` OVERFLOWED -- `(reduce + 0 …)` over int8 values near
   Long/MAX_VALUE threw where PostgreSQL simply widens.

   The formula avoids a mean entirely, which is what keeps it exact:

     numerator = N * sum(x^2) - sum(x)^2
     denom     = N * (N-1)   (sample)   or   N * N   (population)
     variance  = numerator / denom       at select_div_scale
     stddev    = sqrt(variance)          at the same scale

   A negative numerator can only be roundoff, and PostgreSQL answers 0."
  [coll sample? variance?]
  (let [vs (drop-nulls coll)
        n  (count vs)]
    (cond
      (zero? n) :__null__
      (and sample? (<= n 1)) :__null__
      ;; "By analogy to the behavior of the float8 functions, any infinity
      ;; input produces NaN output" -- numeric.c.
      (some types/numeric-special? vs) Double/NaN
      :else
      (let [bds   (mapv ->bigdec vs)
            sum-x (reduce (fn [^java.math.BigDecimal a ^java.math.BigDecimal b] (.add a b))
                          java.math.BigDecimal/ZERO bds)
            sum-x2 (reduce (fn [^java.math.BigDecimal a ^java.math.BigDecimal b]
                             (.add a (.multiply b b)))
                           java.math.BigDecimal/ZERO bds)
            n-bd  (java.math.BigDecimal/valueOf (long n))
            ;; rscale for the two products, from sumX's scale BEFORE
            ;; squaring -- both products are exact at it.
            rs0   (int (* 2 (.scale sum-x)))
            vx    (.setScale (.multiply sum-x sum-x) rs0 java.math.RoundingMode/HALF_UP)
            vx2   (.setScale (.multiply n-bd sum-x2) rs0 java.math.RoundingMode/HALF_UP)
            numer (.subtract vx2 vx)]
        (if (<= (.signum numer) 0)
          java.math.BigDecimal/ZERO
          (let [denom (if sample?
                        (.multiply n-bd (java.math.BigDecimal/valueOf (long (dec n))))
                        (.multiply n-bd n-bd))
                rscale (int (div-result-scale numer denom))
                var-v (.divide numer denom rscale java.math.RoundingMode/HALF_UP)]
            (if variance? var-v (bd-sqrt var-v rscale))))))))

(defn filter-variance-samp-numeric
  "VAR_SAMP over int2/int4/int8/numeric -- NUMERIC, per PostgreSQL."
  [coll] (numeric-stddev-internal coll true true))

(defn filter-variance-pop-numeric
  "VAR_POP over int2/int4/int8/numeric -- NUMERIC, per PostgreSQL."
  [coll] (numeric-stddev-internal coll false true))

(defn filter-stddev-samp-numeric
  "STDDEV_SAMP over int2/int4/int8/numeric -- NUMERIC, per PostgreSQL."
  [coll] (numeric-stddev-internal coll true false))

(defn filter-stddev-pop-numeric
  "STDDEV_POP over int2/int4/int8/numeric -- NUMERIC, per PostgreSQL."
  [coll] (numeric-stddev-internal coll false false))

(defn- youngs-cramer
  "PostgreSQL's float8 variance accumulator (`float8_accum`, float.c):
   the Youngs-Cramer one-pass update, returning [N Sx Sxx].

     N += 1;  Sx += x
     Sxx += (x*N - Sx)^2 / (N * N_prev)      -- after the first value

   Not the textbook two-pass mean formula, which is what this file used
   and which rounds differently in the last place: PostgreSQL's answer is
   this sequence of double operations, so reproducing the answer means
   reproducing the sequence."
  [vs]
  (reduce (fn [[^long n ^double sx ^double sxx] v]
            (let [x  (double v)
                  n' (inc n)
                  sx' (+ sx x)]
              (if (pos? n)
                (let [tmp (- (* x n') sx')]
                  [n' sx' (+ sxx (/ (* tmp tmp) (* (double n') (double n))))])
                [n' sx' (if (or (Double/isNaN x) (Double/isInfinite x))
                          Double/NaN
                          sxx)])))
          [0 0.0 0.0] vs))

(defn filter-variance-samp
  "SQL VAR_SAMP(x) over float4/float8 — float8, ignores :__null__/nil.
   :__null__ when fewer than 2 non-null values remain (matches PG).
   int2/int4/int8/numeric take `filter-variance-samp-numeric`."
  [coll]
  (let [[n _ sxx] (youngs-cramer (drop-nulls coll))]
    (if (< (long n) 2) :__null__ (/ (double sxx) (dec (long n))))))

(defn filter-variance-pop
  "SQL VAR_POP(x) over float4/float8 — float8. :__null__ for an empty
   group; 0 for a single value, as PostgreSQL gives."
  [coll]
  (let [[n _ sxx] (youngs-cramer (drop-nulls coll))]
    (if (< (long n) 1) :__null__ (/ (double sxx) (double (long n))))))

(defn filter-stddev-samp
  "SQL STDDEV_SAMP(x) over float4/float8. See filter-variance-samp."
  [coll]
  (let [v (filter-variance-samp coll)]
    (if (= :__null__ v) :__null__ (Math/sqrt (double v)))))

(defn filter-stddev-pop
  "SQL STDDEV_POP(x) over float4/float8. See filter-variance-pop."
  [coll]
  (let [v (filter-variance-pop coll)]
    (if (= :__null__ v) :__null__ (Math/sqrt (double v)))))

(defn filter-jsonb-object-agg
  "SQL `jsonb_object_agg(k, v)` — one object over the whole group.
   Input is a collection of `[k v]` pairs, the shape the two-argument
   aggregate path already produces for CORR.

   jsonb semantics: keys are canonicalised and a duplicate key takes the
   LAST value, which `into {}` gives directly. A NULL key is an error in
   PostgreSQL; we drop the pair rather than produce a null key."
  [pairs]
  (let [ps (remove (fn [p] (let [k (first p)] (or (nil? k) (= :__null__ k)))) pairs)]
    (if (empty? ps)
      :__null__
      (into {}
            (map (fn [p]
                   [(str (first p))
                    (let [v (second p)]
                      (if (or (nil? v) (= :__null__ v))
                        :datahike.pg.jsonb/json-null
                        v))]))
            ps))))

(defn filter-string-agg
  "SQL `string_agg(expr, delimiter)` — ONE string over the whole group.

   Was a per-row function that stringified its value and DISCARDED the
   delimiter, so `string_agg(nm, ',')` returned one row per input row
   instead of the single joined string. Registering it in
   `sql-aggregate->datalog` is what makes datalog fold it over the
   group; `jsonb_agg` next door had the same defect and the same fix.

   Input is a collection of `[value delimiter]` pairs — the shape the
   two-argument aggregate path already produces for CORR and the object
   aggregates. The delimiter is per-row only because that is how it
   reaches the aggregate; every row carries the same one, so the first
   is taken.

   PostgreSQL SKIPS null inputs and answers NULL for an all-null or
   empty group."
  [pairs]
  (let [ps (remove (fn [p] (let [v (first p)] (or (nil? v) (= :__null__ v)))) pairs)]
    (if (empty? ps)
      :__null__
      (let [d (second (first ps))]
        (str/join (if (or (nil? d) (= :__null__ d)) "" (str d))
                  (map (comp str first) ps))))))

(defn filter-string-agg-ordered
  "SQL `string_agg(expr, delim ORDER BY … ASC)` — `coll` is a collection
   of `[sort-key value delimiter]` triples; sort ascending by sort-key,
   then join. Separate from the array_agg pair shape because the
   delimiter has to ride along too."
  [coll]
  (filter-string-agg (map (fn [t] [(nth t 1) (nth t 2)])
                          (sort-by first akey-compare coll))))

(defn filter-string-agg-ordered-desc
  "Descending counterpart of `filter-string-agg-ordered`."
  [coll]
  (filter-string-agg (map (fn [t] [(nth t 1) (nth t 2)])
                          (sort-by first (fn [a b] (akey-compare b a)) coll))))

(defn filter-json-object-agg
  "SQL `json_object_agg(k, v)` — the text-faithful sibling.

   `json` keeps insertion order and DUPLICATE keys, so this builds text
   directly rather than a map. PostgreSQL pads this one:
   `{ \"b\" : 1, \"a\" : 2 }` — braces spaced, colons spaced."
  [pairs]
  (let [ps (remove (fn [p] (let [k (first p)] (or (nil? k) (= :__null__ k)))) pairs)]
    (if (empty? ps)
      :__null__
      (str "{ "
           (clojure.string/join
            ", " (map (fn [p]
                        (str ((requiring-resolve 'datahike.pg.jsonb/serialize-jsonb)
                              (str (first p)))
                             " : "
                             ((requiring-resolve 'datahike.pg.jsonb/serialize-jsonb)
                              (second p))))
                      ps))
           " }"))))

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

(def seek-no-match
  "Sentinel for a comparison constant that cannot equal ANY value of the
   attribute's stored type -- `intcol = 2.5`. Used as the seek key so the
   index lookup correctly finds nothing."
  ::seek-no-match)

(defn seek-key
  "Narrow a comparison constant to an attribute's stored type so that
   `col = <const>` can stay an INDEX SEEK rather than a scan.

   A datom pattern matches by value equality, which is type-sensitive,
   so the seek key has to be the type the column actually stores.
   PostgreSQL gets there by resolving the operator and casting the
   constant; this is the same step. It matters because a decimal literal
   is `numeric`: `WHERE f = 1.5` on a float8 column now arrives as a
   BigDecimal and would match nothing.

   Returns `seek-no-match` when the constant is not exactly
   representable in the stored type -- `intcol = 2.5` is false for every
   row, and a seek on a key nothing equals says precisely that.
   Non-numeric types pass through untouched."
  [v vtype]
  (cond
    (or (nil? v) (= :__null__ v)) v
    (not (number? v)) v
    :else
    (case vtype
      :db.type/long   (cond
                        (integer? v) v
                        ;; exactly integral -> the integer it equals
                        (== v (Math/rint (double v))) (long v)
                        :else seek-no-match)
      :db.type/double (double v)
      :db.type/float  (float v)
      :db.type/bigdec (bigdec v)
      v)))

(defn sql-eq?
  "SQL `=`. Numbers compare BY VALUE across types.

   PostgreSQL resolves a cross-type numeric comparison by promoting to
   the wider type, so `1.5::numeric = 1.5::float8` and `2 = 2.0` are both
   true. Clojure's `=` is type-sensitive for numbers -- `(= 1.5M 1.5)` is
   FALSE -- so every such comparison answered no rows:

     WHERE n = 1.5   (n numeric)  -> no rows
     WHERE i = 2.0   (i integer)  -> no rows
     WHERE n = f     (numeric vs float8) -> no rows

   `==` is the value comparison, and it promotes the same way PostgreSQL
   does (`(== 0.1M 0.1)` is true, matching `0.1::numeric = 0.1::float8`).
   Anything that is not a pair of numbers keeps `=`, including the
   `:__null__` sentinel -- three-valued logic is decided by the null
   guards around the predicate, not here.

   The ordering comparisons never had this problem: Clojure's `<` `>`
   `<=` `>=` are already cross-type."
  [a b]
  (cond
    ;; An ARRAY column comes back as canonical PG text ("{1,2,3}") while an
    ;; ARRAY[...] literal is a PgArray record, so `arr = ARRAY[1,2,3]`
    ;; compared a String to a record and answered false for every row.
    ;; Parse the text side with the record's element type and compare.
    (and (pg-arr/array? a) (string? b)) (sql-eq? a (pg-arr/from-pg-text b (:elem-type a)))
    (and (pg-arr/array? b) (string? a)) (sql-eq? (pg-arr/from-pg-text a (:elem-type b)) b)
    ;; A numeric special compares by PostgreSQL's total order, and equals
    ;; only its own kind.
    (or (numspecial? a) (numspecial? b))
    (and (numspecial? a) (numspecial? b) (= (:kind a) (:kind b)))
    ;; PostgreSQL's float and numeric comparisons treat NaN as EQUAL to
    ;; itself (float.c float8_cmp_internal, numeric.c cmp_numerics) --
    ;; unlike IEEE-754, and unlike Clojure's `==`, which answers false.
    (and (number? a) (number? b)
         (Double/isNaN (double a)) (Double/isNaN (double b)))
    true
    (and (number? a) (number? b)) (== a b)
    :else (= a b)))

(defn- nan-cmp-op
  "PostgreSQL orders NaN ABOVE every non-NaN for float and numeric
   comparison (float.c float8_cmp_internal), so `'NaN' > 'Infinity'` is
   TRUE. IEEE-754 -- and so Clojure's `<` `>` `<=` `>=` -- answers false
   for every comparison involving NaN, which made all four wrong the
   moment a NaN could exist."
  [pred]
  (fn [a b]
    (cond
      ;; NOT null-safe: these are PREDICATES, and `null-safe` yields the
      ;; :__null__ sentinel, which is TRUTHY in a datalog predicate
      ;; position -- so a NULL operand let the row through instead of
      ;; filtering it. SQL says UNKNOWN, and WHERE treats UNKNOWN as
      ;; FALSE (PostgreSQL collapses it at the qual boundary, EEOP_QUAL).
      ;; sql-eq? already answers false the same way, via `=`.
      (or (nil? a) (= :__null__ a) (nil? b) (= :__null__ b)) false
      (or (nan-num? a) (nan-num? b)
          (types/numeric-special? a) (types/numeric-special? b))
      (pred (order-cmp a b) 0)
      :else (pred (compare a b) 0))))

(def sql-lt? (nan-cmp-op <))
(def sql-gt? (nan-cmp-op >))
(def sql-le? (nan-cmp-op <=))
(def sql-ge? (nan-cmp-op >=))

(defn sql-ne?
  "SQL `<>`. The complement of `sql-eq?` on non-NULL operands.

   NOT simply `(not (sql-eq? a b))`: `sql-eq?` answers false for a NULL
   operand (correct in predicate position, where UNKNOWN collapses to
   FALSE), and negating that turns UNKNOWN into TRUE. `a <> 109` then
   kept the rows where a IS NULL, which PostgreSQL excludes. The other
   ordering comparisons reject NULL explicitly for the same reason -- see
   `nan-cmp-op`."
  [a b]
  (if (or (nil? a) (= :__null__ a) (nil? b) (= :__null__ b))
    false
    (not (sql-eq? a b))))

(def ^:private may-ops
  {:eq sql-eq? :ne sql-ne? :lt sql-lt? :gt sql-gt? :le sql-le? :ge sql-ge?})

(defn sql-may?
  "\"`a op b` is TRUE **or UNKNOWN**\" -- the complement of \"is FALSE\".

   Used to translate `NOT (x AND y)` with a SINGLE datalog negation:
   a conjunction is FALSE exactly when it is not (true-or-unknown), so
   `NOT (x AND y)` becomes `(not (and (sql-may? …x) (sql-may? …y)))`
   instead of a disjunction of two separate negations. Measured ~2x
   cheaper on a 20k-row scan, and it keeps the plan shape that the
   pre-3VL translation had.

   `op` is a keyword rather than the predicate itself because a datalog
   clause can only name a var or a literal, not a function value."
  [op a b]
  (or (sql-null? a) (sql-null? b) (boolean ((may-ops op) a b))))

(defn- three-valued
  "Lift a 2-valued comparison to SQL's three-valued VALUE semantics.

   The predicate-position comparisons (`sql-eq?`, `sql-lt?`, …) answer
   FALSE for a NULL operand, which is right in a WHERE clause -- that is
   where PostgreSQL collapses UNKNOWN to \"reject\" (execExprInterp.c,
   EEOP_QUAL). But in VALUE position the same comparison must yield NULL:

     SELECT a = 10 FROM t   -- a IS NULL  ->  NULL, not false

   These are deliberately SEPARATE symbols rather than a change to the
   existing ones. The `:__null__` sentinel is TRUTHY in a datalog
   predicate position, so making the WHERE comparisons return it would
   let NULL rows through -- a bug this codebase has already shipped once."
  [pred]
  (fn [a b]
    (if (or (sql-null? a) (sql-null? b)) :__null__ (boolean (pred a b)))))

(def sql-eq3? (three-valued sql-eq?))
(def sql-ne3? (three-valued sql-ne?))
(def sql-lt3? (three-valued sql-lt?))
(def sql-gt3? (three-valued sql-gt?))
(def sql-le3? (three-valued sql-le?))
(def sql-ge3? (three-valued sql-ge?))

(defn sql-and3
  "Kleene AND. FALSE dominates: `false AND NULL` is FALSE, because the
   conjunction is false whatever the unknown operand turns out to be.
   Only when nothing is FALSE and something is UNKNOWN is the result
   UNKNOWN."
  [a b]
  (cond
    (or (false? a) (false? b))     false
    (or (sql-null? a) (sql-null? b)) :__null__
    :else                          (and (boolean a) (boolean b))))

(defn sql-or3
  "Kleene OR. TRUE dominates: `true OR NULL` is TRUE."
  [a b]
  (cond
    (or (true? a) (true? b))       true
    (or (sql-null? a) (sql-null? b)) :__null__
    :else                          (or (boolean a) (boolean b))))

(defn sql-not3
  "Kleene NOT. `NOT NULL` is NULL -- negation cannot resolve an unknown."
  [a]
  (if (sql-null? a) :__null__ (not a)))

(defn sql-distinct?
  "`a IS DISTINCT FROM b`. Never UNKNOWN -- that is the whole point of it:
   it is the NULL-aware `<>`, where two NULLs are NOT distinct and a NULL
   IS distinct from any value."
  [a b]
  (let [an (sql-null? a) bn (sql-null? b)]
    (cond
      (and an bn) false
      (or an bn)  true
      :else       (not (sql-eq? a b)))))

(def non-strict-fns
  "Functions in `sql-fn->clj-fn` that must NOT be wrapped in `null-safe`.

   Almost every SQL function is strict -- NULL in, NULL out -- so the
   caller wraps the whole table. GREATEST and LEAST are the exceptions:
   PostgreSQL compiles them to a MinMaxExpr, which SKIPS null inputs and
   is NULL only when every input is. Wrapping them short-circuited to
   NULL before their own implementation ever ran, so `greatest(NULL, 5)`
   answered NULL instead of 5."
  #{"greatest" "least"
    ;; quote_nullable's whole purpose is to render a NULL as the text
    ;; "NULL"; wrapping it in null-safe short-circuited it to NULL.
    "quote_nullable"})

(defn- min-max-skipping-nulls
  "GREATEST / LEAST. See the note at their entries in the function table:
   PostgreSQL's MinMaxExpr ignores NULL inputs rather than propagating
   them, and answers NULL only when every input is NULL."
  [args better?]
  (let [vals (remove sql-null? args)]
    (if (empty? vals)
      :__null__
      (reduce (fn [a b] (if (better? b a) b a)) vals))))

(defn sql-between?
  "`v BETWEEN lo AND hi` in PREDICATE position: `lo <= v AND v <= hi`,
   Through the Kleene AND, so a NULL bound does NOT simply make it false:
   `1 BETWEEN 3 AND NULL` is FALSE (the first conjunct settles it), while
   `1 BETWEEN 0 AND NULL` is UNKNOWN. WHERE keeps only TRUE either way.

   A single predicate rather than a pair of clauses so that NOT BETWEEN
   can be its complement without a disjunction -- see `sql-not-between?`
   -- and through sql-le? rather than clojure.core's `<=`, which is not
   NaN-aware."
  [v lo hi]
  (true? (sql-and3 (sql-le3? lo v) (sql-le3? v hi))))

(defn sql-not-between?
  "`v NOT BETWEEN lo AND hi`, i.e. the Kleene NOT of `sql-between?`.

   Rejecting any NULL operand outright would be wrong: `1 NOT BETWEEN 3
   AND NULL` is TRUE, because `1 >= 3` is already FALSE and FALSE AND
   UNKNOWN is FALSE, whose negation is TRUE.

   Emitted as one predicate because the obvious `(or [(< v lo)]
   [(> v hi)])` is invalid datalog whenever lo and hi are different
   variables -- the two branches then bind different var sets, and
   `id NOT BETWEEN 0 AND i` raised \"Join variable not declared inside
   clauses\"."
  [v lo hi]
  (true? (sql-not3 (sql-and3 (sql-le3? lo v) (sql-le3? v hi)))))

(defn sql-not-distinct?
  "`a IS NOT DISTINCT FROM b`. A named function rather than
   `(not (sql-distinct? …))`, because datahike rejects a nested form as a
   clause argument -- it would be passed through as a literal list."
  [a b]
  (not (sql-distinct? a b)))

(defn sql-like3?
  "LIKE / regex match in VALUE position: NULL input yields NULL, not false.
   `re-find` cannot be used directly there -- it throws on the `:__null__`
   sentinel and returns nil (which drops the row) on no-match."
  [v re]
  (if (sql-null? v) :__null__ (boolean (re-find re v))))

(defn sql-in3?
  "`x IN (…)` in VALUE position, three-valued.

   NULL if x is NULL. Otherwise TRUE on a hit; on a miss the answer is
   UNKNOWN when the list contains a NULL (x might have equalled it) and
   FALSE only when every element is known and none matched."
  [vals v]
  (cond
    (sql-null? v)               :__null__
    (some #(sql-eq? % v) vals)  true
    (some sql-null? vals)       :__null__
    :else                       false))

(defn sql-between3?
  "`x BETWEEN lo AND hi` in VALUE position: `lo <= x AND x <= hi` under
   Kleene AND, so any NULL operand makes it UNKNOWN rather than false."
  [v lo hi]
  (sql-and3 (sql-le3? lo v) (sql-le3? v hi)))

(defn sql-in?
  "SQL `IN` over a literal list. `contains?` on a set is `=`-based and so
   inherits the cross-type numeric blindness described in `sql-eq?`;
   `WHERE n IN (1.5)` missed a numeric 1.5 for the same reason
   `WHERE n = 1.5` did. Falls back to a linear scan with `sql-eq?` only
   when the O(1) hit misses, so the common same-type case stays O(1)."
  [vals v]
  (or (contains? vals v)
      (boolean (some #(sql-eq? % v) vals))))

(defn- int-width-error [tname]
  (throw (errors/pg-error :numeric-value-out-of-range
                          {:message (str tname " out of range")})))

(defn- long-overflow->pg
  "Clojure's `+`/`-`/`*` raise a bare ArithmeticException \"long
   overflow\" at the int8 boundary; PostgreSQL raises 22003 \"bigint out
   of range\". A client trapping SQLSTATE 22003 never saw ours."
  [f]
  (fn [& args]
    (try (apply f args)
         (catch ArithmeticException e
           (if (= "long overflow" (.getMessage e))
             (int-width-error "bigint")
             (throw e))))))

(defn- float-inf? [x]
  (and (number? x) (Double/isInfinite (double x))))

(defn- checked-float
  "PostgreSQL's float overflow / underflow checks (float.h float8_pl,
   float8_mul, ...): a result that came out infinite from finite inputs
   is 22003 `value out of range: overflow`, and for multiply and divide
   a result that came out zero from non-zero inputs is 22003
   `value out of range: underflow`.

   IEEE-754 -- and so Java -- returns the infinity or the zero instead,
   which then propagated through the rest of the query as a value.
   PostgreSQL aborts the statement.

   The `!isinf(operand)` exemption matters: `Infinity * 2` is Infinity in
   PostgreSQL too, not an error. It is unreachable until infinities can
   be constructed, and correct in advance."
  [r a b underflow?]
  (cond
    (not (and (number? r) (Double/isInfinite (double r)) (not (float-inf? a)) (not (float-inf? b))))
    (if (and underflow?
             (number? r) (zero? (double r))
             (number? a) (not (zero? (double a)))
             (number? b) (not (zero? (double b)))
             ;; only a FLOAT result can underflow; exact types cannot
             (or (instance? Double r) (instance? Float r)))
      (throw (errors/pg-error :numeric-value-out-of-range
                              {:message "value out of range: underflow"}))
      r)
    :else
    (throw (errors/pg-error :numeric-value-out-of-range
                            {:message "value out of range: overflow"}))))

(defn- float-checked
  "Wrap a binary arithmetic op with PostgreSQL's float range checks."
  [f underflow?]
  (fn [a b] (checked-float (f a b) a b underflow?)))

(defn- special-aware
  "Route an operation through the special-value path when either operand
   is a numeric NaN or +-Infinity, which no BigDecimal operator accepts."
  [f g]
  (fn [a b]
    (if (or (types/numeric-special? a) (types/numeric-special? b))
      (special-arith g a b)
      (f a b))))

(def sql-+ (null-safe (special-aware (long-overflow->pg (float-checked + false)) +)))
(def sql-- (null-safe (special-aware (long-overflow->pg (float-checked - false)) -)))
(def sql-* (null-safe (special-aware (long-overflow->pg (float-checked * true)) *)))

(declare throw-division-by-zero)

;; PostgreSQL money is an int64 count of cents. Datahike carries the SQL
;; value as a scale-2 BigDecimal, so arithmetic must explicitly return to
;; cents or the carrier silently becomes unbounded numeric.
(defn- money-out-of-range []
  (throw (errors/pg-error :numeric-value-out-of-range
                          {:message "money out of range"})))

(defn- money->cents ^long [v]
  (try
    (.longValueExact (.movePointRight ^java.math.BigDecimal (bigdec v) 2))
    (catch ArithmeticException _ (money-out-of-range))))

(defn- cents->money ^java.math.BigDecimal [^long cents]
  (java.math.BigDecimal/valueOf cents 2))

(def sql-money+
  (null-safe
   (fn [a b]
     (try (cents->money (Math/addExact (money->cents a) (money->cents b)))
          (catch ArithmeticException _ (money-out-of-range))))))

(def sql-money-
  (null-safe
   (fn [a b]
     (try (cents->money (Math/subtractExact (money->cents a) (money->cents b)))
          (catch ArithmeticException _ (money-out-of-range))))))

(defn- checked-money-float-cents ^long [^double result]
  ;; FLOAT8_FITS_IN_INT64 uses an exclusive 2^63 upper bound. Long/MAX_VALUE
  ;; itself rounds to 2^63 as a double, so comparing against (double MAX)
  ;; would incorrectly accept it and Java's narrowing conversion would clamp.
  (let [two63 (Math/scalb (double 1.0) (int 63))]
    (if (or (Double/isNaN result) (Double/isInfinite result)
            (>= result two63) (< result (- two63)))
      (money-out-of-range)
      (long result))))

(def sql-money*
  (null-safe
   (fn [a b]
     (let [[money factor] (if (decimal? a) [a b] [b a])
           cents (money->cents money)]
       (if (integer? factor)
         (try (cents->money (Math/multiplyExact cents (long factor)))
              (catch ArithmeticException _ (money-out-of-range)))
         (cents->money
          (checked-money-float-cents
           (Math/rint (* (double cents) (double factor))))))))))

(def sql-money-div
  (null-safe
   (fn [money divisor]
     (let [cents (money->cents money)]
       (cond
         (and (number? divisor) (zero? divisor)) (throw-division-by-zero)
         (integer? divisor) (cents->money (quot cents (long divisor)))
         :else (cents->money
                (checked-money-float-cents
                 (Math/rint (/ (double cents) (double divisor))))))))))

(def sql-money-div-money
  (null-safe
   (fn [a b]
     (let [divisor (money->cents b)]
       (when (zero? divisor) (throw-division-by-zero))
       (/ (double (money->cents a)) (double divisor))))))

;; ---------------------------------------------------------------------------
;; date arithmetic
;;
;; `date + integer`, `date - integer` and `date - date` are their own
;; operators in PostgreSQL (date_pli / date_mii / date_mi), not the
;; numeric ones. Routing them through sql-+ / sql-- threw a raw
;; ClassCastException ("java.util.Date cannot be cast to
;; java.lang.Number"), so `d + 1` was an error rather than a date.
;;
;; Which operand is a date is decided by the TRANSLATOR from the
;; declared column type, not here: Datahike stores a `date` and a
;; `timestamp` column alike as java.util.Date, so a runtime type test
;; cannot tell them apart, and PostgreSQL gives the two different
;; answers (`timestamp - timestamp` is an interval, `date - date` is a
;; count of days). Only expressions the translator has already typed as
;; date reach these.

(defn- ->local-date
  "Narrow a date-typed value to a LocalDate. A `date` COLUMN arrives as
   a java.util.Date at UTC midnight; a `::date` CAST already produces a
   LocalDate."
  ^java.time.LocalDate [v]
  (cond
    (instance? java.time.LocalDate v) v
    (instance? java.time.LocalDateTime v) (.toLocalDate ^java.time.LocalDateTime v)
    (instance? java.util.Date v) (-> ^java.util.Date v .toInstant
                                     (.atZone java.time.ZoneOffset/UTC)
                                     .toLocalDate)
    (instance? java.time.Instant v) (-> ^java.time.Instant v
                                        (.atZone java.time.ZoneOffset/UTC)
                                        .toLocalDate)
    (string? v) (java.time.LocalDate/parse ^String v)
    :else (throw (errors/pg-error :invalid-text-representation
                                  {:type "date" :value (str v)}))))

;; Returning a LocalDate rather than a java.util.Date is deliberate: the
;; wire renderer emits a LocalDate as `2020-01-02` on its own, without
;; needing the declared OID to tell it this column is a date.
(def sql-date+
  (null-safe
   (fn date-plus [a b]
     (cond
       (number? b) (.plusDays (->local-date a) (long b))
       (number? a) (.plusDays (->local-date b) (long a))
       :else (throw (errors/pg-error :undefined-function
                                     {:name "date + date"}))))))

(def sql-date-
  (null-safe
   (fn date-minus [a b]
     (if (number? b)
       (.plusDays (->local-date a) (- (long b)))
       ;; date - date is a plain integer count of days, not an interval.
       (- (.toEpochDay (->local-date a)) (.toEpochDay (->local-date b)))))))

(defn- throw-division-by-zero []
  (throw (errors/pg-error :division-by-zero {})))

(defn- numeric-div
  "PostgreSQL numeric division. BigDecimal's own `divide` raises
   \"Non-terminating decimal expansion\" without an explicit scale, so
   this is not optional once decimal literals are numeric -- `10.0 / 3`
   would throw. Rounds half-up, as round_var does."
  [^java.math.BigDecimal a ^java.math.BigDecimal b]
  (.divide a b (int (div-result-scale a b)) java.math.RoundingMode/HALF_UP))

(defn- checked-int-arith
  "Apply `f` at PostgreSQL's declared integer width, raising 22003 when
   the result does not fit.

   int2 and int4 do not exist at runtime -- Datahike stores every
   integer as a Java long -- so the width has to come from the
   TRANSLATOR, which knows the declared column type. Without it
   `int4col + 1` on 2147483647 answered 2147483648: a value the column's
   own type cannot hold and a binary client cannot read.

   When the operands are NOT both integral the declared width no longer
   governs -- PostgreSQL has promoted to numeric or float8 by then -- so
   this falls through to the generic operator."
  [width f generic a b]
  (if (and (integer? a) (integer? b))
    (let [[lo hi tname] (get types/integer-width-limits width)
          r (try (f (long a) (long b))
                 ;; long arithmetic itself overflowed: that is the int8
                 ;; boundary, and Clojure raises a bare "long overflow"
                 ;; where PostgreSQL raises 22003.
                 (catch ArithmeticException _ (int-width-error tname)))]
      (if (or (< r (long lo)) (> r (long hi)))
        (int-width-error tname)
        r))
    (generic a b)))

(def sql-int+
  (null-safe (fn [width a b] (checked-int-arith width #(Math/addExact ^long %1 ^long %2) + a b))))
(def sql-int-
  (null-safe (fn [width a b] (checked-int-arith width #(Math/subtractExact ^long %1 ^long %2) - a b))))
(def sql-int*
  (null-safe (fn [width a b] (checked-int-arith width #(Math/multiplyExact ^long %1 ^long %2) * a b))))

(def sql-int-neg
  "Unary minus at a declared integer width. `-(-2147483648)` is out of
   range for int4 in PostgreSQL; Java's `-` wraps back to itself."
  (null-safe
   (fn [width a]
     (if (integer? a)
       (let [[lo hi tname] (get types/integer-width-limits width)
             r (try (Math/negateExact (long a))
                    (catch ArithmeticException _ (int-width-error tname)))]
         (if (or (< r (long lo)) (> r (long hi))) (int-width-error tname) r))
       (- a)))))

(def sql-int-abs
  "abs at a declared integer width. `abs(-9223372036854775808)` returned
   ITSELF -- a negative absolute value -- because Java's abs wraps."
  (null-safe
   (fn [width a]
     (if (integer? a)
       (let [[lo hi tname] (get types/integer-width-limits width)
             r (try (Math/absExact (long a))
                    (catch ArithmeticException _ (int-width-error tname)))]
         (if (or (< r (long lo)) (> r (long hi))) (int-width-error tname) r))
       (abs a)))))

(defn- int-div
  "PostgreSQL's int2div / int4div / int8div: integer over integer is
   integer, TRUNCATED TOWARD ZERO (-7 / 2 is -3, not -4). Clojure's `/`
   instead produces an exact Ratio, so `7 / 2` answered 3.5 and
   `2147483647 / 2` answered \"1.0737418235E9\" — a Ratio rendered
   through double, which is neither the right value nor a syntax any
   client can read back.

   `quot` is the truncating one; `//` and `Math/floorDiv` round toward
   negative infinity and would disagree with PostgreSQL on every
   negative operand."
  [a b]
  (quot a b))

(defn- checked-div
  "Division that raises SQLSTATE 22012 (\"division by zero\") on a zero
   divisor. PG errors for integer, float, AND numeric division alike
   (int4div / float8div / numeric_div in postgres utils/adt), whereas
   Clojure `(/ 1.0 0.0)` silently returns ##Inf — so the zero check must
   run BEFORE dividing. NULL propagation is handled by the `null-safe`
   wrapper around this fn, so NULL / 0 stays NULL as in PG.

   The operand types pick the operator, exactly as PostgreSQL's function
   resolution does: two integers divide as integers, anything else
   divides as it did."
  ([a b]
   (cond
     ;; numeric NaN absorbs even a zero divisor; PostgreSQL returns NaN
     ;; for NaN/0 while every non-NaN numeric divided by zero errors.
     (or (and (numspecial? a) (= :nan (:kind a)))
         (and (numspecial? b) (= :nan (:kind b))))
     types/nan-numeric
     (and (number? b) (zero? b)) (throw-division-by-zero)
     (or (numspecial? a) (numspecial? b)) (special-arith / a b)
     (and (integer? a) (integer? b)) (int-div a b)
     ;; numeric / numeric -- and numeric / integer, which PostgreSQL
     ;; also resolves as numeric. A float on either side outranks
     ;; numeric and falls through to Clojure's `/`, which yields a
     ;; double, exactly as float8 division should.
     (and (decimal? a) (or (decimal? b) (integer? b)))
     (numeric-div a (bigdec b))
     (and (decimal? b) (integer? a)) (numeric-div (bigdec a) b)
     :else (/ a b)))
  ([a b & more]
   (when (some #(and (number? %) (zero? %)) (cons b more))
     (throw-division-by-zero))
   (reduce (fn [acc x] (if (and (integer? acc) (integer? x)) (int-div acc x) (/ acc x)))
           a (cons b more))))

(defn- checked-mod
  "Modulo that raises SQLSTATE 22012 on a zero modulus — PG's int4mod /
   float8mod / numeric_mod all raise \"division by zero\" there.

   A numeric operand makes the result numeric, at `max(s1,s2)` like
   PostgreSQL's mod_var. Clojure's `rem` does not promote the other
   operand, so `mod(1, 3.0)` answered `1` where PostgreSQL answers
   `1.0`."
  [a b]
  (cond
    (or (and (numspecial? a) (= :nan (:kind a)))
        (and (numspecial? b) (= :nan (:kind b))))
    types/nan-numeric
    (and (number? b) (zero? b)) (throw-division-by-zero)
    (or (numspecial? a) (numspecial? b)) (special-arith rem a b)
    (and (or (decimal? a) (decimal? b)) (number? a) (number? b))
    (let [^java.math.BigDecimal x (bigdec a)
          ^java.math.BigDecimal y (bigdec b)]
      (.setScale (.remainder x y) (max (.scale x) (.scale y))
                 java.math.RoundingMode/UNNECESSARY))
    :else (rem a b)))

(def sql-div (null-safe checked-div))

(defn- f4
  "Narrow to float4 after computing. PostgreSQL's float4 operators
   compute AT float4 precision (float.h float4_pl et al), so `r + r` on
   1.1::real is 2.2 and not the 2.200000047683716 you get by widening
   both operands to double first."
  [x]
  (let [v (float x)]
    (if (and (Float/isInfinite v) (not (Double/isInfinite (double x))))
      (throw (errors/pg-error :numeric-value-out-of-range
                              {:message "value out of range: overflow"}))
      v)))

(def sql-f4+ (null-safe (fn [a b] (f4 (+ (float a) (float b))))))
(def sql-f4- (null-safe (fn [a b] (f4 (- (float a) (float b))))))
(def sql-f4* (null-safe (fn [a b] (f4 (* (float a) (float b))))))
(def sql-f4div
  (null-safe (fn [a b]
               (when (and (number? b) (zero? b)) (throw-division-by-zero))
               (f4 (/ (float a) (float b))))))

(def sql-int-div
  "Integer division at a declared width. Only one case can overflow --
   `INT_MIN / -1`, whose quotient is one past the maximum -- and Java
   wraps it back to INT_MIN rather than raising."
  (null-safe
   (fn [width a b]
     (when (and (number? b) (zero? b)) (throw-division-by-zero))
     (if (and (integer? a) (integer? b))
       (let [[lo hi tname] (get types/integer-width-limits width)
             ;; divideExact, not quot: the ONE overflow case is
             ;; INT_MIN / -1, whose quotient is one past the maximum,
             ;; and Java's quot wraps it back to INT_MIN. At int8 the
             ;; range check below cannot catch that -- INT_MIN is inside
             ;; the int8 range.
             r (try (Math/divideExact (long a) (long b))
                    (catch ArithmeticException _ (int-width-error tname)))]
         (if (or (< r (long lo)) (> r (long hi))) (int-width-error tname) r))
       (checked-div a b)))))

(def sql-mod (null-safe checked-mod))

;; ---------------------------------------------------------------------------
;; Math function implementations — PostgreSQL semantics, not IEEE-754
;;
;; The temptation is to map a SQL math function straight onto its
;; `java.lang.Math` namesake. That is wrong in three separate ways, and
;; every one of them used to be live here:
;;
;;   1. DOMAIN ERRORS. Java returns NaN/Infinity where PG raises. PG's
;;      float.c and numeric.c check the argument first:
;;        sqrt(-x)            → 2201F cannot take square root of a negative number
;;        ln(0) / log(0)      → 2201E cannot take logarithm of zero
;;        ln(-x)              → 2201E cannot take logarithm of a negative number
;;        power(0, -x)        → 2201F zero raised to a negative power is undefined
;;        power(-x, 0.5)      → 2201F a negative number raised to a
;;                                    non-integer power yields a complex result
;;        asin/acos(|x| > 1)  → 22003 input is out of range
;;        exp / power overflow→ 22003 value out of range: overflow
;;      Returning NaN instead was issue #22.
;;
;;   2. DIFFERENT FUNCTIONS UNDER THE SAME NAME. SQL `log(x)` is base-10;
;;      `Math/log` is natural log. That mapping silently returned
;;      4.605… for `log(100)` where PG returns 2 — a wrong answer, not
;;      an error. Natural log is `ln`.
;;
;;   3. DIFFERENT ROUNDING. `Math/round` breaks ties toward positive
;;      infinity (round(-2.5) = -2); PG rounds half away from zero
;;      (round(-2.5) = -3).
;;
;; NaN inputs are exempt from the domain checks: PG propagates NaN
;; through sqrt/ln/asin/power without raising (verified against 17.10).
;; Every check below therefore tests the input, and lets NaN through.

(defn- nan?* [x]
  (and (number? x) (Double/isNaN (double x))))

(defn- throw-power-domain [detail]
  (throw (errors/pg-error :invalid-argument-for-power-function {:message detail})))

(defn- throw-log-domain [detail]
  (throw (errors/pg-error :invalid-argument-for-logarithm {:message detail})))

(defn- throw-out-of-range [detail]
  (throw (errors/pg-error :numeric-value-out-of-range {:message detail})))

(defn- finite-range
  "PG's float.c overflow/underflow guard idiom, applied verbatim:

     if (isinf(result) && !isinf(arg1)) float_overflow_error();
     if (result == 0.0 && isfinite(arg1) && arg1 != 0.0)
       float_underflow_error();

   An infinite *input* may legitimately produce either an infinite or a
   zero result, and a zero input a zero result — only the finite→infinite
   and finite-nonzero→zero transitions are errors. The UNDERFLOW half is the one
   that gets forgotten: `exp(-1000.0::float8)` is an error in PG."
  ^double [^double result ^double input]
  (cond
    (and (Double/isInfinite result) (not (Double/isInfinite input)))
    (throw-out-of-range "value out of range: overflow")
    (and (zero? result) (not (Double/isInfinite input)) (not (zero? input)))
    (throw-out-of-range "value out of range: underflow")
    :else result))

(defn- finite-input
  "sin/cos/tan/cot reject an infinite argument with 22003 'input is out
   of range' (float.c:1895) instead of returning NaN as Java does. NaN
   passes through — only Infinity is rejected."
  ^double [x]
  (let [d (double x)]
    (if (Double/isInfinite d)
      (throw-out-of-range "input is out of range")
      d)))

(defn- sql-sqrt [x]
  (let [d (double x)]
    (cond
      (Double/isNaN d) d
      (neg? d) (throw-power-domain "cannot take square root of a negative number")
      :else (Math/sqrt d))))

(defn- sql-ln [x]
  (let [d (double x)]
    (cond
      (Double/isNaN d) d
      (zero? d) (throw-log-domain "cannot take logarithm of zero")
      (neg? d)  (throw-log-domain "cannot take logarithm of a negative number")
      :else (Math/log d))))

(defn- sql-log10 [x]
  (let [d (double x)]
    (cond
      (Double/isNaN d) d
      (zero? d) (throw-log-domain "cannot take logarithm of zero")
      (neg? d)  (throw-log-domain "cannot take logarithm of a negative number")
      :else (Math/log10 d))))

(defn- sql-log
  "SQL LOG. One argument → base 10 (NOT natural log — that is `ln`).
   Two arguments → LOG(base, x) = ln(x) / ln(base).

   `log(1, x)` divides by ln(1) = 0, which PG reports as 22012 division
   by zero (numeric.c reaches div_var_fast with a zero divisor) rather
   than as a logarithm-domain error — so route it through checked-div."
  ([x] (sql-log10 x))
  ([base x]
   (let [lb (sql-ln base)
         lx (sql-ln x)]
     (if (or (nan?* lb) (nan?* lx))
       Double/NaN
       (checked-div lx lb)))))

(defn- sql-exp [x]
  (let [d (double x)]
    (if (Double/isNaN d) d (finite-range (Math/exp d) d))))

(defn sql-power
  "PG's `dpow` — the implementation behind both `power(a,b)`/`pow(a,b)`
   and the `^` OPERATOR (which is exponentiation in PG, not xor; PG
   spells xor `#`). Public because the translator emits it for `^`.

   Carries PG's domain errors rather than IEEE results: 0 to a negative
   power and a negative base to a fractional power are 2201F, and an
   overflow to infinity from finite inputs is 22003."
  [base exponent]
  (let [b (double base)
        e (double exponent)]
    (cond
      (or (Double/isNaN b) (Double/isNaN e)) Double/NaN
      (and (zero? b) (neg? e))
      (throw-power-domain "zero raised to a negative power is undefined")
      (and (neg? b) (not (Double/isInfinite e)) (not= e (Math/rint e)))
      (throw-power-domain
       "a negative number raised to a non-integer power yields a complex result")
      :else
      (let [r (Math/pow b e)]
        (if (and (Double/isInfinite r)
                 (not (Double/isInfinite b))
                 (not (Double/isInfinite e)))
          (throw-out-of-range "value out of range: overflow")
          r)))))

(defn- unit-domain
  "asin/acos are defined on [-1, 1]; outside it PG raises 22003 'input
   is out of range' (float.c dasin/dacos) rather than returning NaN."
  ^double [x]
  (let [d (double x)]
    (cond
      (Double/isNaN d) d
      (> (Math/abs d) 1.0) (throw-out-of-range "input is out of range")
      :else d)))

(defn- sql-asin [x] (let [d (unit-domain x)] (if (Double/isNaN d) d (Math/asin d))))
(defn- sql-acos [x] (let [d (unit-domain x)] (if (Double/isNaN d) d (Math/acos d))))

(defn- sql-acosh
  "acosh is defined on [1, ∞). NaN passes the `< 1.0` guard (every
   NaN comparison is false) and comes back NaN, as in PG."
  [x]
  (let [d (double x)]
    (if (< d 1.0)
      (throw-out-of-range "input is out of range")
      (Math/log (+ d (Math/sqrt (- (* d d) 1.0)))))))

(defn- sql-atanh
  "atanh is defined on [-1, 1]; atanh(±1) is ±Infinity and NOT an error
   in PG, but an infinite argument is (float.c:2724)."
  [x]
  (let [d (double x)]
    (if (or (< d -1.0) (> d 1.0))
      (throw-out-of-range "input is out of range")
      (* 0.5 (Math/log (/ (+ 1.0 d) (- 1.0 d)))))))

(defn- sql-asinh [x]
  ;; PG's dasinh has no guards at all — mirror that exactly.
  (let [d (double x)]
    (Math/log (+ d (Math/sqrt (+ (* d d) 1.0))))))

(defn- sql-cbrt [x] (Math/cbrt (double x)))

(defn- sql-sign
  "float8 sign(NaN) is 0 in PG — dsign has no NaN guard, so both
   `arg1 > 0` and `arg1 < 0` are false and the result stays 0
   (float.c:1410). Java's Math/signum returns NaN instead. (The
   *numeric* sign(NaN) is NaN, but we have one representation.)"
  [x]
  (let [d (double x)]
    (cond (Double/isNaN d) 0.0
          (pos? d) 1.0
          (neg? d) -1.0
          :else 0.0)))

(defn- sql-round
  "SQL ROUND — half away from zero, i.e. PG's NUMERIC rounding
   (numeric.c:11657 round_var). `Math/round` breaks ties toward
   positive infinity and returns a long, so round(-2.5) came back -2
   instead of -3.

   Deliberately NOT float8 semantics. PG's round(float8) is `rint()`,
   which is half-to-EVEN — round(2.5::float8) is 2, not 3. We apply the
   numeric rule to every input because a decimal literal like `2.5` is
   numeric in PG but arrives here as a Double: dispatching on the
   runtime type would make `SELECT round(2.5)` answer 2 where PG
   answers 3, which is the more visible divergence. The right fix is
   upstream — type decimal literals as numeric — and this should
   become type-dispatched once that lands.

   Two-arg ROUND(x, n) rounds to n decimal places; PG defines it for
   numeric only and returns numeric, so compute in BigDecimal rather
   than reintroducing binary-float error."
  ([x]
   (if (integer? x)
     x
     (-> (bigdec x) (.setScale 0 java.math.RoundingMode/HALF_UP) (.longValueExact))))
  ([x n]
   (-> (bigdec x)
       (.setScale (int n) java.math.RoundingMode/HALF_UP)
       ;; strip to the plain numeric the wire encoder expects
       (.stripTrailingZeros)
       (.setScale (int n) java.math.RoundingMode/UNNECESSARY))))

(defn- sql-trunc
  "SQL TRUNC — round toward zero. TRUNC(x, n) truncates to n decimals."
  ([x]
   (if (integer? x)
     x
     (-> (bigdec x) (.setScale 0 java.math.RoundingMode/DOWN) (.longValueExact))))
  ([x n]
   (-> (bigdec x) (.setScale (int n) java.math.RoundingMode/DOWN))))

(defn- sql-gcd [a b]
  (.gcd (biginteger a) (biginteger b)))

(defn- sql-lcm
  "SQL LCM. lcm(0, x) = 0 (PG defines it so, no error)."
  [a b]
  (let [x (biginteger a) y (biginteger b)]
    (if (or (zero? a) (zero? b))
      (biginteger 0)
      (.abs (.divide (.multiply x y) (.gcd x y))))))

(defn- throw-width-bucket [detail]
  (throw (errors/pg-error :invalid-argument-for-width-bucket {:message detail})))

(defn- sql-width-bucket
  "SQL WIDTH_BUCKET(operand, low, high, count) — 1-based histogram
   bucket, 0 below `low`, count+1 at or above `high`.

   `low > high` is LEGAL and mirror-reverses the histogram; only
   `low == high` is an error. A NaN operand is allowed (PG treats NaN
   as larger than everything); NaN or infinite BOUNDS are not. All four
   argument failures are 2201G, not the 22003 used elsewhere in
  float.c — see float.c:4190-4203."
  [operand low high cnt]
  (let [o (->num-double operand)
        l (->num-double low)
        h (->num-double high)
        n (long cnt)]
    (when (<= n 0) (throw-width-bucket "count must be greater than zero"))
    (when (or (Double/isNaN l) (Double/isNaN h))
      (throw-width-bucket "lower and upper bounds cannot be NaN"))
    (when (or (Double/isInfinite l) (Double/isInfinite h))
      (throw-width-bucket "lower and upper bounds must be finite"))
    (when (= l h) (throw-width-bucket "lower bound cannot equal upper bound"))
    (let [asc? (< l h)
          below? (if asc? (< o l) (> o l))
          above? (if asc? (>= o h) (<= o h))]
      (cond
        (Double/isNaN o) (inc n)
        below? 0
        above? (inc n)
        :else (inc (long (Math/floor (* n (/ (- o l) (- h l))))))))))

;; ---------------------------------------------------------------------------
;; SQL string function implementations

(defn sql-length
  "SQL LENGTH / CHAR_LENGTH. Bit strings measure in bits, not in the
   record's map entries."
  [v]
  (if (pg-bits/pg-bit? v) (pg-bits/width v) (count v)))

(defn sql-octet-length
  "SQL OCTET_LENGTH. For a bit string PG reports ceil(bits / 8)."
  [v]
  (if (pg-bits/pg-bit? v) (pg-bits/octet-length v) (count v)))

;; ---------------------------------------------------------------------------
;; Bitwise operators — `&` `|` `~` `<<` `>>`, over integers and bit strings.
;;
;; PG has no cross-type integer forms (`int8 << int8` is actually an error
;; there, because the shift RHS is always int4 and int8→int4 is
;; assignment-only); we store every integer as int8 and compute on longs,
;; which is the int8 row of PG's operator table.
;;
;; Shift semantics follow PG's, which are just C's: `>>` is arithmetic
;; (sign-propagating), and the shift distance is masked to the word width
;; rather than saturating. Clojure's bit-shift-* on longs already do both.
;; ---------------------------------------------------------------------------

(defn- bit-dispatch
  "Apply `bitf` for bit-string operands, `intf` for integers."
  [bitf intf a b]
  (cond
    (or (sql-null? a) (sql-null? b)) :__null__
    (and (pg-bits/pg-bit? a) (pg-bits/pg-bit? b)) (bitf a b)
    :else (intf (long a) (long b))))

(defn sql-bit-and [a b] (bit-dispatch pg-bits/and-bits bit-and a b))
(defn sql-bit-or  [a b] (bit-dispatch pg-bits/or-bits  bit-or  a b))
(defn sql-bit-xor [a b] (bit-dispatch pg-bits/xor-bits bit-xor a b))

(defn sql-bit-not
  "`~` — bitwise NOT."
  [a]
  (cond
    (sql-null? a) :__null__
    (pg-bits/pg-bit? a) (pg-bits/not-bits a)
    :else (bit-not (long a))))

(defn sql-bit-shift-left
  "`<<`. On a bit string the width is preserved and vacated positions are
   zero-filled; on an integer it is a plain shift."
  [a n]
  (cond
    (or (sql-null? a) (sql-null? n)) :__null__
    (pg-bits/pg-bit? a) (pg-bits/shift-bits a (long n))
    :else (bit-shift-left (long a) (long n))))

(defn sql-bit-shift-right
  "`>>`. Arithmetic (sign-propagating) on integers, matching PG's bare C
   `>>` on a signed operand; width-preserving and zero-filled on a bit
   string."
  [a n]
  (cond
    (or (sql-null? a) (sql-null? n)) :__null__
    (pg-bits/pg-bit? a) (pg-bits/shift-bits a (- (long n)))
    :else (bit-shift-right (long a) (long n))))

(defn sql-bit-length
  "SQL BIT_LENGTH — the length in BITS.

   For a bit string that is its width; for text it is 8 × the byte
   length, which is why this can't just delegate to sql-length."
  [v]
  (if (pg-bits/pg-bit? v)
    (pg-bits/width v)
    (* 8 (count (.getBytes (str v) java.nio.charset.StandardCharsets/UTF_8)))))

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

(defn- ->s ^String [v] (str v))

(defn- nullable-str
  "Lift a 1-string-argument function to SQL strictness.

   The table-dispatched functions get this from `null-safe` at the call
   site, but the ones reached from a dedicated AST node -- TRIM, EXTRACT,
   SUBSTRING -- are emitted as a direct datalog clause and bypass it, so
   the `:__null__` sentinel reached `str` and `trim(s)` returned the
   literal text \":__null__\"."
  [f]
  (fn [& args] (if (some sql-null? args) :__null__ (apply f args))))

(defn sql-ascii
  "First character's code point, 0 for the empty string (varlena.c ascii)."
  [s]
  (let [^String t (->s s)] (if (zero? (.length t)) 0 (long (.codePointAt t 0)))))

(defn sql-chr
  "Character with the given code point. PostgreSQL rejects 0 outright
   (chr(0) is not a valid text character)."
  [n]
  (let [n (long n)]
    (when (zero? n)
      (throw (ex-info "null character not permitted"
                      {:error :program-limit-exceeded
                       :message "null character not permitted"})))
    (String. (Character/toChars (int n)))))

(defn- trim-set
  "Trim any leading/trailing character that appears in `chars`. PostgreSQL's
   btrim/ltrim/rtrim take a SET of characters, not a prefix to strip."
  [^String t ^String chars left? right?]
  (let [cs (set chars)
        n (.length t)
        lo (if left?
             (loop [i 0] (if (and (< i n) (cs (.charAt t i))) (recur (inc i)) i))
             0)
        hi (if right?
             (loop [i n] (if (and (> i lo) (cs (.charAt t (dec i)))) (recur (dec i)) i))
             n)]
    (subs t lo hi)))

(def sql-btrim
  (nullable-str (fn ([s] (str/trim (->s s)))
                  ([s chars] (trim-set (->s s) (->s chars) true true)))))

(def sql-ltrim
  (nullable-str (fn ([s] (str/triml (->s s)))
                  ([s chars] (trim-set (->s s) (->s chars) true false)))))

(def sql-rtrim
  (nullable-str (fn ([s] (str/trimr (->s s)))
                  ([s chars] (trim-set (->s s) (->s chars) false true)))))

(defn sql-md5
  "MD5 as a lowercase hex string, as PostgreSQL's md5() returns."
  [s]
  (let [d (.digest (java.security.MessageDigest/getInstance "MD5")
                   (.getBytes (->s s) java.nio.charset.StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" %) d))))

(defn sql-starts-with [s prefix] (.startsWith (->s s) (->s prefix)))

(defn sql-split-part
  "`split_part(string, delimiter, n)`. n is 1-based; a NEGATIVE n counts
   from the END. Out of range yields the empty string, not NULL."
  [s delim n]
  (let [parts (str/split (->s s) (java.util.regex.Pattern/compile
                                  (java.util.regex.Pattern/quote (->s delim)))
                         -1)
        cnt (count parts)
        n (long n)]
    (when (zero? n)
      (throw (ex-info "field position must not be zero"
                      {:error :invalid-parameter-value
                       :message "field position must not be zero"})))
    (let [idx (if (pos? n) (dec n) (+ cnt n))]
      (if (or (neg? idx) (>= idx cnt)) "" (nth parts idx)))))

(defn sql-translate
  "Replace each character of `from` with the character at the same
   position in `to`; characters of `from` with no counterpart are DELETED."
  [s from to]
  (let [^String t (->s s) ^String f (->s from) ^String r (->s to)
        sb (StringBuilder.)]
    (dotimes [i (.length t)]
      (let [c (.charAt t i)
            j (.indexOf f (int c))]
        (cond
          (neg? j)          (.append sb c)
          (< j (.length r)) (.append sb (.charAt r j))
          :else             nil)))          ; no counterpart -> dropped
    (.toString sb)))

(defn sql-overlay
  "`overlay(string placing new from start [for count])`. `count` defaults
   to the LENGTH OF THE REPLACEMENT, not to the rest of the string."
  ([s new start]
   (if (pg-bits/pg-bit? s)
     (let [replacement (if (pg-bits/pg-bit? new)
                         new
                         (pg-bits/parse-bit-literal (str new)))]
       (pg-bits/overlay-bits s replacement start))
     (sql-overlay s new start (count (->s new)))))
  ([s new start cnt]
   (if (pg-bits/pg-bit? s)
     (let [replacement (if (pg-bits/pg-bit? new)
                         new
                         (pg-bits/parse-bit-literal (str new)))]
       (pg-bits/overlay-bits s replacement start cnt))
     (let [^String t (->s s)
           n (.length t)
           start (long start)
           cnt (long cnt)
           lo (max 0 (dec start))
           hi (min n (max lo (+ lo (max 0 cnt))))]
       (str (subs t 0 (min lo n)) (->s new) (subs t hi))))))

(defn sql-quote-ident
  "Double-quote an identifier only when it needs it -- PostgreSQL leaves a
   plain lowercase identifier bare (quote_ident in ruleutils.c)."
  [s]
  (let [^String t (->s s)]
    (if (and (seq t)
             (re-matches #"[a-z_][a-z_0-9$]*" t)
             (not (contains? #{"select" "from" "where" "table" "user" "order" "group"
                               "and" "or" "not" "null" "true" "false"} t)))
      t
      (str \" (str/replace t "\"" "\"\"") \"))))

(defn sql-quote-literal [v]
  (if (sql-null? v)
    :__null__
    (str \' (str/replace (->s v) "'" "''") \')))

(defn sql-quote-nullable
  "Like quote_literal, but a NULL becomes the unquoted string NULL."
  [v]
  (if (sql-null? v) "NULL" (sql-quote-literal v)))

(defn sql-to-hex [n] (Long/toHexString (long n)))

(defn- re-flags->int
  "PostgreSQL's single-letter regex flags -> java.util.regex bit flags.
   `g` (global) is not a compile flag; the callers that honour it read it
   from the string themselves."
  [flags]
  (let [f (str (when (sql-null? flags) "") (when-not (sql-null? flags) flags))]
    (cond-> 0
      (str/includes? f "i") (bit-or java.util.regex.Pattern/CASE_INSENSITIVE)
      (str/includes? f "m") (bit-or java.util.regex.Pattern/MULTILINE)
      (str/includes? f "n") (bit-or java.util.regex.Pattern/MULTILINE)
      (str/includes? f "x") (bit-or java.util.regex.Pattern/COMMENTS))))

(defn- re-compile ^java.util.regex.Pattern [pattern flags]
  (java.util.regex.Pattern/compile (str pattern) (re-flags->int flags)))

(defn- pg-replacement->java
  "PostgreSQL writes capture references as `\\1`..`\\9` and the whole match
   as `\\&`; java.util.regex wants `$1` and `$0`. A literal `\\\\` stays one
   backslash, and `$` in the replacement must be escaped so Java does not
   read it as a group reference."
  [^String r]
  (let [sb (StringBuilder.)
        n (.length r)]
    (loop [i 0]
      (if (>= i n)
        (.toString sb)
        (let [c (.charAt r i)]
          (cond
            (and (= c \\) (< (inc i) n))
            (let [d (.charAt r (inc i))]
              (cond
                (Character/isDigit d) (do (.append sb "$") (.append sb d) (recur (+ i 2)))
                (= d \&)              (do (.append sb "$0") (recur (+ i 2)))
                (= d \\)              (do (.append sb "\\\\") (recur (+ i 2)))
                :else                 (do (.append sb d) (recur (+ i 2)))))
            (= c \$) (do (.append sb "\\$") (recur (inc i)))
            :else    (do (.append sb c) (recur (inc i)))))))))

(defn sql-regexp-replace
  "`regexp_replace(string, pattern, replacement [, flags])`. Replaces only
   the FIRST match unless the `g` flag is given -- the opposite of most
   languages' default, and the usual source of surprise."
  ([s pattern repl] (sql-regexp-replace s pattern repl nil))
  ([s pattern repl flags]
   (if (or (sql-null? s) (sql-null? pattern) (sql-null? repl))
     :__null__
     (let [m (.matcher (re-compile pattern flags) (str s))
           r (pg-replacement->java (str repl))]
       (if (str/includes? (str (when-not (sql-null? flags) flags)) "g")
         (.replaceAll m r)
         (.replaceFirst m r))))))

(defn sql-regexp-like
  "`regexp_like(string, pattern [, flags])` -- a partial match, as `~` is."
  ([s pattern] (sql-regexp-like s pattern nil))
  ([s pattern flags]
   (if (or (sql-null? s) (sql-null? pattern))
     :__null__
     (.find (.matcher (re-compile pattern flags) (str s))))))

(defn sql-regexp-count
  "`regexp_count(string, pattern [, start [, flags]])` -- non-overlapping
   matches at or after the 1-based `start`."
  ([s pattern] (sql-regexp-count s pattern 1 nil))
  ([s pattern start] (sql-regexp-count s pattern start nil))
  ([s pattern start flags]
   (if (or (sql-null? s) (sql-null? pattern))
     :__null__
     (let [^String t (str s)
           from (max 0 (dec (long (if (sql-null? start) 1 start))))]
       (if (> from (.length t))
         0
         (let [m (.matcher (re-compile pattern flags) t)]
           (.region m from (.length t))
           (loop [c 0] (if (.find m) (recur (inc c)) c))))))))

(defn- nth-match
  "The java Matcher positioned on the Nth match at or after `start`, or nil."
  [s pattern start n flags]
  (let [^String t (str s)
        from (max 0 (dec (long (if (sql-null? start) 1 start))))
        n (long (if (sql-null? n) 1 n))]
    (when (<= from (.length t))
      (let [m (.matcher (re-compile pattern flags) t)]
        (.region m from (.length t))
        (loop [i 1]
          (when (.find m)
            (if (= i n) m (recur (inc i)))))))))

(defn sql-regexp-substr
  "`regexp_substr(string, pattern [, start [, N [, flags]]])` -- the Nth
   match's text, NULL when there is no Nth match."
  ([s pattern] (sql-regexp-substr s pattern 1 1 nil))
  ([s pattern start] (sql-regexp-substr s pattern start 1 nil))
  ([s pattern start n] (sql-regexp-substr s pattern start n nil))
  ([s pattern start n flags]
   (if (or (sql-null? s) (sql-null? pattern))
     :__null__
     (if-let [m (nth-match s pattern start n flags)] (.group m) :__null__))))

(defn sql-regexp-instr
  "`regexp_instr(string, pattern [, start [, N [, endoption [, flags]]]])`
   -- the 1-based position of the Nth match, 0 when there is none.
   `endoption` 1 asks for the position AFTER the match."
  ([s pattern] (sql-regexp-instr s pattern 1 1 0 nil))
  ([s pattern start] (sql-regexp-instr s pattern start 1 0 nil))
  ([s pattern start n] (sql-regexp-instr s pattern start n 0 nil))
  ([s pattern start n endoption] (sql-regexp-instr s pattern start n endoption nil))
  ([s pattern start n endoption flags]
   (if (or (sql-null? s) (sql-null? pattern))
     :__null__
     (if-let [m (nth-match s pattern start n flags)]
       (if (and (not (sql-null? endoption)) (= 1 (long endoption)))
         (inc (.end m))
         (inc (.start m)))
       0))))

(defn- ->zdt ^java.time.ZonedDateTime [v]
  (cond
    (instance? java.util.Date v)          (.atZone (.toInstant ^java.util.Date v) java.time.ZoneOffset/UTC)
    (instance? java.time.Instant v)       (.atZone ^java.time.Instant v java.time.ZoneOffset/UTC)
    (instance? java.time.LocalDate v)     (.atStartOfDay ^java.time.LocalDate v java.time.ZoneOffset/UTC)
    (instance? java.time.LocalDateTime v) (.atZone ^java.time.LocalDateTime v java.time.ZoneOffset/UTC)
    :else nil))

(defn sql-extract
  "`EXTRACT(field FROM value)` / `date_part(field, value)`. PostgreSQL
   returns NUMERIC, so the result is a BigDecimal -- `extract(epoch …)`
   carries sub-second digits, and an integer-typed result would drop them."
  [field v]
  (if (or (sql-null? field) (sql-null? v))
    :__null__
    (let [f (str/lower-case (str/replace (str field) #"^'|'$" ""))
          zdt (->zdt v)]
      (if (nil? zdt)
        :__null__
        (let [n (case f
                  ("year" "years" "y")      (.getYear zdt)
                  ("month" "months" "mon")  (.getMonthValue zdt)
                  ("day" "days" "d")        (.getDayOfMonth zdt)
                  ("hour" "hours" "h")      (.getHour zdt)
                  ("minute" "minutes" "min") (.getMinute zdt)
                  "second"                  (+ (.getSecond zdt)
                                               (/ (double (.getNano zdt)) 1e9))
                  "milliseconds"            (+ (* 1000.0 (.getSecond zdt))
                                               (/ (double (.getNano zdt)) 1e6))
                  "microseconds"            (+ (* 1000000.0 (.getSecond zdt))
                                               (/ (double (.getNano zdt)) 1e3))
                  "quarter"                 (inc (quot (dec (.getMonthValue zdt)) 3))
                  ;; PostgreSQL's dow is 0=Sunday; java.time is 1=Monday..7=Sunday.
                  "dow"                     (mod (.getValue (.getDayOfWeek zdt)) 7)
                  "isodow"                  (.getValue (.getDayOfWeek zdt))
                  "doy"                     (.getDayOfYear zdt)
                  "epoch"                   (+ (double (.toEpochSecond zdt))
                                               (/ (double (.getNano zdt)) 1e9))
                  "week"                    (.get zdt (.weekOfWeekBasedYear
                                                       java.time.temporal.WeekFields/ISO))
                  "isoyear"                 (.get zdt (.weekBasedYear
                                                       java.time.temporal.WeekFields/ISO))
                  "decade"                  (quot (.getYear zdt) 10)
                  "century"                 (quot (+ (.getYear zdt) 99) 100)
                  "millennium"              (quot (+ (.getYear zdt) 999) 1000)
                  (throw (ex-info (str "unit \"" f "\" not recognized")
                                  {:error :invalid-parameter-value
                                   :message (str "unit \"" f "\" not recognized")})))]
          (if (integer? n)
            (java.math.BigDecimal/valueOf (long n))
            (java.math.BigDecimal/valueOf (double n))))))))

(defn sql-substring
  "PostgreSQL's `substring(str, start [, len])` (text_substring in
   varlena.c). Positions are 1-based, and a range reaching outside the
   string is CLAMPED rather than an error: `substring('abc', 0, 2)` is
   'a', because the window covers positions 0 and 1 and only position 1
   exists. A negative length IS an error.

   NULL in, NULL out -- the `:__null__` sentinel would otherwise reach
   `subs` and raise a raw ClassCastException on the Keyword."
  ([s start] (sql-substring s start nil))
  ([s start len]
   (if (or (sql-null? s) (sql-null? start) (and (some? len) (sql-null? len)))
     :__null__
     (if (pg-bits/pg-bit? s)
       (if (some? len)
         (pg-bits/substring-bits s start len)
         (pg-bits/substring-bits s start))
       (let [^String st (str s)
             n (.length st)
             start (long start)
             _ (when (and len (neg? (long len)))
                 (throw (ex-info "negative substring length not allowed"
                                 {:error :invalid-parameter-value
                                  :message "negative substring length not allowed"})))
             ;; Half-open window [start, start+len) in 1-based positions.
             to (if len (+ start (long len)) (inc n))
             lo (max 1 start)
             hi (min (inc n) to)]
         (if (<= hi lo) "" (subs st (dec lo) (dec hi))))))))

(defn sql-position
  "1-based position of `substring` in `string`, 0 if not found.

   Argument order is PostgreSQL's textpos(str, search_str) -- the STRING
   first. The `position(sub IN str)` syntax reads the other way round, but
   gram.y swaps the operands before they reach the function, so the SQL
   form is handled at the call site (see translate-function-call)."
  [string substring]
  (if (and (pg-bits/pg-bit? string) (pg-bits/pg-bit? substring))
    (pg-bits/position-bits string substring)
    (let [idx (.indexOf (str string) (str substring))]
      (if (neg? idx) 0 (inc idx)))))

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
(defn pg-is-in-recovery
  "PostgreSQL `pg_is_in_recovery()` — true on a standby still replaying
   WAL. We are never a standby, so this is always false.

   Worth having for one specific reason: it is the FIRST query real
   `pg_dump` sends, so without it pg_dump aborts before doing anything
   at all. Our own dump command is unaffected — this is about the
   standard client tool being able to talk to us."
  [] false)

(defn acldefault
  "PostgreSQL `acldefault(type, ownerId)` — the built-in default access
   privileges for an object kind. We have no privilege system, so this
   answers SQL NULL, which pg_dump reads as \"ACLs are default\" and so
   emits no GRANT/REVOKE statements.

   Returning NULL rather than a synthesised aclitem[] is deliberate: a
   fabricated ACL would have to be kept in step with a privilege model
   we do not have, and pg_dump compares it against the object's own
   (also absent) acl column."
  [& _] :__null__)

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
   ;; NOT Datalog's raw `stddev`/`variance`: those cast every element to
   ;; Number, so a group containing a NULL -- which arrives as the
   ;; `:__null__` sentinel -- died with "class clojure.lang.Keyword cannot
   ;; be cast to class java.lang.Number".
   "stddev_pop"     'datahike.pg.sql/filter-stddev-pop
   "variance"       'datahike.pg.sql/filter-variance-samp
   "var_samp"       'datahike.pg.sql/filter-variance-samp
   "var_pop"        'datahike.pg.sql/filter-variance-pop
   "median"         'median
   "corr"           'datahike.pg.sql/filter-corr
   "string_agg"     'datahike.pg.sql/filter-string-agg
   "jsonb_object_agg" 'datahike.pg.sql/filter-jsonb-object-agg
   "json_object_agg"  'datahike.pg.sql/filter-json-object-agg
   "array_agg"      'datahike.pg.sql/filter-array-agg
   "jsonb_agg"      'datahike.pg.sql/filter-jsonb-agg
   ;; json_agg and jsonb_agg render identically for arrays — the
   ;; families differ on OBJECT punctuation, not array punctuation.
   "json_agg"       'datahike.pg.sql/filter-jsonb-agg
   ;; Ordered-set aggregates — `WITHIN GROUP (ORDER BY x)` syntax.
   ;; Translator routes them through the pair-aggregate path (like
   ;; corr) since the percentile fraction is a constant alongside
   ;; the per-row value, and Datalog aggregates take a single coll.
   "percentile_cont" 'datahike.pg.sql/filter-percentile-cont
   "percentile_disc" 'datahike.pg.sql/filter-percentile-disc
   "mode"            'datahike.pg.sql/filter-mode})

(defn aggregate-function? [^String fname]
  (contains? sql-aggregate->datalog (str/lower-case fname)))

;; ---------------------------------------------------------------------------
;; numeric transcendentals
;;
;; PostgreSQL has BOTH a float8 and a numeric overload of sqrt / exp / ln
;; / log / log10 / power, and resolution picks the numeric one whenever
;; an argument is numeric. We answered float8 for all six, wrong in two
;; ways at once: the reported type, and the precision -- `2.0 ^ 10` is
;; 1024.0000000000000 in PostgreSQL, a scale that says how many digits
;; are meaningful, and we answered 1024.
;;
;; The scale is not the operands'. Each function has its own rule in
;; numeric.c, all of the shape "enough digits for NUMERIC_MIN_SIG_DIGITS
;; significant figures, but never fewer than the input already shows".
;; Those rules are ported. The VALUES are computed with BigDecimal at a
;; guard margin and rounded to the rule's scale, rather than porting
;; PostgreSQL's own exp_var/ln_var: agreement is then exact wherever the
;; fuzz reached, and any drift would be in the last digit.

(def ^:private ^:const numeric-max-result-scale 2000)

(defn- clamp-rscale ^long [^long rscale ^long dscale]
  (long (min (max rscale dscale 0) numeric-max-display-scale)))

(defn- sqrt-rscale
  "numeric_sqrt: sqrt roughly halves the weight, so half the digits of
   the integer part are already known."
  ^long [^java.math.BigDecimal a]
  (let [w (long (first (nbase-weight+first a)))
        sweight (if (>= w 0)
                  (+ (quot (* w dec-digits) 2) 1)
                  (- 1 (quot (- 1 (* w dec-digits)) 2)))]
    (clamp-rscale (- numeric-min-sig-digits sweight) (.scale a))))

(defn- exp-rscale
  "numeric_exp: exp(x) has about x*log10(e) integer digits."
  ^long [^java.math.BigDecimal a]
  (let [val (-> (* (.doubleValue a) 0.434294481903252)
                (max (double (- numeric-max-result-scale)))
                (min (double numeric-max-result-scale)))]
    (clamp-rscale (- numeric-min-sig-digits (long val)) (.scale a))))

(defn- estimate-ln-dweight
  "numeric.c estimate_ln_dweight — the decimal weight of ln(x), which is
   what decides how many digits ln has to produce. Near 1 it uses
   ln(1+x) ~= x, because ln there is small and its weight very negative."
  ^long [^java.math.BigDecimal v]
  (cond
    (not (pos? (.signum v))) 0
    (and (>= (.compareTo v (java.math.BigDecimal. "0.9")) 0)
         (<= (.compareTo v (java.math.BigDecimal. "1.1")) 0))
    (let [x (.subtract v java.math.BigDecimal/ONE)]
      (if (zero? (.signum x))
        0
        (let [[w f] (nbase-weight+first x)]
          (+ (* (long w) dec-digits) (long (Math/log10 (double f)))))))
    :else
    (let [ln-var (Math/log (Math/abs (.doubleValue v)))]
      (if (zero? ln-var) 0 (long (Math/log10 (Math/abs ln-var)))))))

(defn- ln-rscale ^long [^java.math.BigDecimal a]
  (clamp-rscale (- numeric-min-sig-digits (estimate-ln-dweight a)) (.scale a)))

(def ^:private ln10-str
  "2.30258509299404568401799145468436420760110148862877297603333")

(defn- bd-ln
  "ln to `prec` significant digits. Reduce x = m*10^k with 1 <= m < 10,
   then ln(m) by the atanh series 2*atanh((m-1)/(m+1)) -- which converges
   quickly because the argument is bounded well away from the series'
   singularity."
  ^java.math.BigDecimal [^java.math.BigDecimal x ^long prec]
  (let [mc (java.math.MathContext. (int (+ prec 15)))
        k (long (- (.precision x) (.scale x) 1))
        m (.movePointLeft x (int k))
        y (.divide (.subtract m java.math.BigDecimal/ONE)
                   (.add m java.math.BigDecimal/ONE) mc)
        y2 (.multiply y y mc)
        eps (.movePointLeft java.math.BigDecimal/ONE (int (+ prec 12)))
        series (loop [n 1 pow (.multiply y y2 mc) acc y]
                 (if (or (> n 2000) (< (.compareTo (.abs pow) eps) 0))
                   acc
                   (recur (inc n)
                          (.multiply pow y2 mc)
                          (.add acc (.divide pow (java.math.BigDecimal/valueOf (inc (* 2 n))) mc) mc))))]
    (.add (.multiply (java.math.BigDecimal/valueOf 2) series mc)
          (.multiply (java.math.BigDecimal/valueOf k) (java.math.BigDecimal. ln10-str) mc)
          mc)))

(defn- bd-exp
  "exp to `prec` significant digits. Halve the argument until the
   Maclaurin series converges quickly, then square back."
  ^java.math.BigDecimal [^java.math.BigDecimal x ^long prec]
  (let [mc (java.math.MathContext. (int (+ prec 15)))
        mag (Math/abs (.doubleValue x))
        halvings (long (max 0 (+ 4 (if (< mag 1.0) 0 (Math/ceil (/ (Math/log mag) (Math/log 2.0)))))))
        xr (.divide x (.pow (java.math.BigDecimal/valueOf 2) (int halvings)) mc)
        eps (.movePointLeft java.math.BigDecimal/ONE (int (+ prec 12)))
        s (loop [n 1 term java.math.BigDecimal/ONE acc java.math.BigDecimal/ONE]
            (if (or (> n 2000) (< (.compareTo (.abs term) eps) 0))
              acc
              (let [t (.divide (.multiply term xr mc) (java.math.BigDecimal/valueOf n) mc)]
                (recur (inc n) t (.add acc t mc)))))]
    (loop [i 0 v s] (if (>= i halvings) v (recur (inc i) (.multiply v v mc))))))

;; The numeric overloads. Dispatch is on the ARGUMENT's runtime class,
;; which mirrors PostgreSQL's function resolution: a numeric argument
;; selects the numeric candidate, anything else stays float8. Decimal
;; literals already arrive as BigDecimal, so `sqrt(2.0)` reaches here as
;; one and `sqrt(2)` does not.

(def ^:private session-prng
  "Per-process PRNG cell. PostgreSQL keeps one per SESSION; a single
   server process here shares one, which is the same thing for the
   single-connection case the differential exercises and is noted as a
   limitation for concurrent sessions."
  (delay (datahike.pg.prng/make-session-state)))

(defn sql-random
  "random() -- pg_prng_double over the session PRNG."
  []
  (datahike.pg.prng/draw-double! @session-prng))

(defn sql-setseed
  "setseed(x). PostgreSQL restricts the argument to [-1,1] and rejects
   NaN, then seeds via pg_prng_fseed."
  [x]
  (let [d (double x)]
    (when (or (Double/isNaN d) (< d -1.0) (> d 1.0))
      (throw (errors/pg-error
              :invalid-parameter-value
              {:message (str "setseed parameter " (types/float->pg-text d false)
                             " is out of allowed range [-1,1]")})))
    (datahike.pg.prng/set-seed! @session-prng d)
    ;; "" and not nil: a datalog function binding that yields nil FILTERS
    ;; the row, so `SELECT setseed(0.5)` came back with no rows at all.
    ;; PostgreSQL's void renders as one empty row.
    ""))

(defn sql-random-normal
  "random_normal(mean, stddev) -- Box-Muller over the same stream, as
   pg_prng_double_normal does."
  ([] (sql-random-normal 0.0 1.0))
  ([mean] (sql-random-normal mean 1.0))
  ([mean stddev]
   (let [u1 (max 1.0e-300 (sql-random))
         u2 (sql-random)
         z (* (Math/sqrt (* -2.0 (Math/log u1))) (Math/cos (* 2.0 Math/PI u2)))]
     (+ (double mean) (* (double stddev) z)))))

(defn sql-random-range
  "random(lo, hi) -- uniform over the CLOSED integer range."
  [lo hi]
  (let [l (long lo) h (long hi)]
    (when (> l h)
      (throw (errors/pg-error :invalid-parameter-value
                              {:message "lower bound must be less than or equal to upper bound"})))
    (+ l (long (Math/floor (* (sql-random) (double (inc (- h l)))))))))

(defn- throw-logarithm-error [msg]
  (throw (errors/pg-error :invalid-argument-for-logarithm {:message msg})))

(defn- num-arg? [x]
  (or (decimal? x) (numspecial? x)))

(defn- numeric-double-result
  "Carry a double computation back through NUMERIC's runtime type."
  [^double d]
  (or (types/double->numeric-special d)
      (.stripTrailingZeros (java.math.BigDecimal/valueOf d))))

(defn- numeric-special-unary [f x]
  (numeric-double-result (double (f (->num-double x)))))

(declare numeric-power numeric-log)

(defn sql-power-op
  "The `^` operator. PostgreSQL resolves it to numeric_power whenever an
   operand is numeric, exactly as the power() function does -- so
   `2.0 ^ 10` is 1024.0000000000000, not 1024."
  [b e]
  (cond
    (or (numspecial? b) (numspecial? e))
    (numeric-double-result (sql-power (->num-double b) (->num-double e)))
    (or (decimal? b) (decimal? e)) (numeric-power (bigdec b) (bigdec e))
    :else (sql-power b e)))

(defn sql-log2
  "log(base, x). PostgreSQL has only a NUMERIC two-argument log -- there
   is no float8 overload -- so both arguments coerce and the result is
   numeric even for `log(2,64)`."
  [b x]
  (if (or (numspecial? b) (numspecial? x))
    (numeric-double-result (sql-log (->num-double b) (->num-double x)))
    (numeric-log (bigdec b) (bigdec x))))

(defn- numeric-sqrt ^java.math.BigDecimal [^java.math.BigDecimal a]
  (when (neg? (.signum a))
    (throw (errors/pg-error :invalid-argument-for-power-function
                            {:message "cannot take square root of a negative number"})))
  (let [rs (sqrt-rscale a)]
    (.setScale (.sqrt a (java.math.MathContext. (int (+ rs 20))))
               (int rs) java.math.RoundingMode/HALF_UP)))

(defn- numeric-exp ^java.math.BigDecimal [^java.math.BigDecimal a]
  (let [rs (exp-rscale a)]
    (.setScale (bd-exp a (+ rs 20)) (int rs) java.math.RoundingMode/HALF_UP)))

(defn- numeric-ln ^java.math.BigDecimal [^java.math.BigDecimal a]
  (cond
    (zero? (.signum a)) (throw-logarithm-error "cannot take logarithm of zero")
    (neg? (.signum a)) (throw-logarithm-error "cannot take logarithm of a negative number"))
  (let [rs (ln-rscale a)]
    (.setScale (bd-ln a (+ rs 20)) (int rs) java.math.RoundingMode/HALF_UP)))

(defn- numeric-log
  "log(base, x) — numeric_log. Computed as ln(x)/ln(base) at a guard
   margin; the result scale follows the same MIN_SIG_DIGITS rule."
  ^java.math.BigDecimal [^java.math.BigDecimal base ^java.math.BigDecimal x]
  ;; 2201E, not the generic 22003 -- and the message names whichever
  ;; operand is at fault, which is the BASE for log(0, 10).
  (let [bad (cond (not (pos? (.signum base))) base
                  (not (pos? (.signum x)))    x
                  :else nil)]
    (when bad
      (throw-logarithm-error (if (zero? (.signum bad))
                               "cannot take logarithm of zero"
                               "cannot take logarithm of a negative number"))))
  (let [rs (ln-rscale x)
        p (+ rs 25)
        mc (java.math.MathContext. (int p))
        lb (bd-ln base p)]
    (when (zero? (.signum lb)) (throw-division-by-zero))
    (.setScale (.divide (bd-ln x p) lb mc) (int rs) java.math.RoundingMode/HALF_UP)))

(defn- power-rscale
  "numeric.c power_var_int: the result's decimal weight is about
   exp * log10(base), and MIN_SIG_DIGITS beyond that is what matters."
  ^long [^java.math.BigDecimal base ^java.math.BigDecimal e]
  (let [bd (Math/abs (.doubleValue base))
        f (if (zero? bd) 0.0 (* (.doubleValue e) (Math/log10 bd)))
        f (-> f (max (double (- numeric-max-result-scale)))
              (min (double numeric-max-result-scale)))]
    (clamp-rscale (- numeric-min-sig-digits (long f))
                  (max (.scale base) (.scale e)))))

(defn- numeric-power ^java.math.BigDecimal [^java.math.BigDecimal base ^java.math.BigDecimal e]
  (cond
    (and (zero? (.signum base)) (neg? (.signum e)))
    (throw (errors/pg-error :invalid-argument-for-power-function
                            {:message "zero raised to a negative power is undefined"}))
    (and (neg? (.signum base))
         (not (zero? (.compareTo (.stripTrailingZeros e)
                                 (.setScale (.stripTrailingZeros e) 0 java.math.RoundingMode/DOWN)))))
    (throw (errors/pg-error
            :invalid-argument-for-power-function
            {:message "a negative number raised to a non-integer power yields a complex result"})))
  (let [rs (power-rscale base e)
        p (+ rs 25)
        mc (java.math.MathContext. (int p))]
    (cond
      (zero? (.signum base))
      (.setScale (if (zero? (.signum e)) java.math.BigDecimal/ONE java.math.BigDecimal/ZERO)
                 (int rs) java.math.RoundingMode/HALF_UP)
      ;; integer exponent within int range: exact repeated multiplication,
      ;; which also handles a negative base
      (and (zero? (.scale (.stripTrailingZeros e)))
           (< (Math/abs (.doubleValue e)) 1.0e9))
      (let [n (.intValueExact (.toBigIntegerExact (.stripTrailingZeros e)))
            v (if (neg? n)
                (.divide java.math.BigDecimal/ONE (.pow base (- n) mc) mc)
                (.pow base n mc))]
        (.setScale v (int rs) java.math.RoundingMode/HALF_UP))
      :else
      (.setScale (bd-exp (.multiply e (bd-ln base p) mc) p)
                 (int rs) java.math.RoundingMode/HALF_UP))))

;; ---------------------------------------------------------------------------
;; Degree trigonometry — ported from PostgreSQL's float.c, NOT derived
;;
;; These are NOT `sin(x * pi/180)`. PostgreSQL divides through by the
;; function's own value at a reference angle, and that division CANCELS
;; the libm error at the endpoints: `sind(30)` is exactly 0.5, `tand(45)`
;; exactly 1, `asind(0.5)` exactly 30 — on any platform, whatever libm
;; returns. A naive implementation misses every one of those, and they
;; are precisely the values anyone checks.

(def ^:private ^:const radians-per-degree 0.0174532925199432957692)

(def ^:private sin-30 (Math/sin (* 30.0 radians-per-degree)))
(def ^:private one-minus-cos-60 (- 1.0 (Math/cos (* 60.0 radians-per-degree))))
(def ^:private asin-0-5 (Math/asin 0.5))
(def ^:private acos-0-5 (Math/acos 0.5))
(def ^:private atan-1-0 (Math/atan 1.0))

(defn- sind-0-to-30 ^double [^double x]
  (/ (/ (Math/sin (* x radians-per-degree)) sin-30) 2.0))

(defn- cosd-0-to-60 ^double [^double x]
  (- 1.0 (/ (/ (- 1.0 (Math/cos (* x radians-per-degree))) one-minus-cos-60) 2.0)))

(defn- sind-q1 ^double [^double x]
  (if (<= x 30.0) (sind-0-to-30 x) (cosd-0-to-60 (- 90.0 x))))

(defn- cosd-q1 ^double [^double x]
  (if (<= x 60.0) (cosd-0-to-60 x) (sind-0-to-30 (- 90.0 x))))

(def ^:private tan-45 (/ (sind-q1 45.0) (cosd-q1 45.0)))
(def ^:private cot-45 (/ (cosd-q1 45.0) (sind-q1 45.0)))

(defn- asind-q1 ^double [^double x]
  (if (<= x 0.5)
    (* (/ (Math/asin x) asin-0-5) 30.0)
    (- 90.0 (* (/ (Math/acos x) acos-0-5) 60.0))))

(defn- acosd-q1 ^double [^double x]
  (if (<= x 0.5)
    (- 90.0 (* (/ (Math/asin x) asin-0-5) 30.0))
    (* (/ (Math/acos x) acos-0-5) 60.0)))

(defn- degree-reduce
  "PostgreSQL's range reduction to [0,90]: fmod by 360, then reflect.
   `sin-sign?` and `flip-at-90?` select the parity each function needs."
  [^double x sin-sign? flip-at-90?]
  (let [x (rem x 360.0)
        [x sign] (if (< x 0.0) [(- x) (if sin-sign? -1.0 1.0)] [x 1.0])
        [x sign] (if (> x 180.0) [(- 360.0 x) (if sin-sign? (- sign) sign)] [x sign])
        [x sign] (if (> x 90.0)
                   [(- 180.0 x) (if flip-at-90? (- sign) sign)]
                   [x sign])]
    [x sign]))

(defn- deg-guard
  "NaN in, NaN out; an infinite input is 22003 (POSIX, per float.c)."
  [^double x]
  (when (Double/isInfinite x) (throw-out-of-range "input is out of range"))
  x)

(def sql-sind
  (null-safe (fn [x] (let [d (double x)]
                       (if (Double/isNaN d) d
                           (let [[a sign] (degree-reduce (deg-guard d) true false)]
                             (* sign (sind-q1 a))))))))

(def sql-cosd
  (null-safe (fn [x] (let [d (double x)]
                       (if (Double/isNaN d) d
                           (let [[a sign] (degree-reduce (deg-guard d) false true)]
                             (* sign (cosd-q1 a))))))))

(def sql-tand
  (null-safe (fn [x] (let [d (double x)]
                       (if (Double/isNaN d) d
                           (let [[a sign] (degree-reduce (deg-guard d) true true)
                                 r (* sign (/ (/ (sind-q1 a) (cosd-q1 a)) tan-45))]
                             ;; force -0.0 to 0.0, as dtand does
                             (if (zero? r) 0.0 r)))))))

(def sql-cotd
  (null-safe (fn [x] (let [d (double x)]
                       (if (Double/isNaN d) d
                           (let [[a sign] (degree-reduce (deg-guard d) true true)
                                 r (* sign (/ (/ (cosd-q1 a) (sind-q1 a)) cot-45))]
                             (if (zero? r) 0.0 r)))))))

(def sql-asind
  (null-safe (fn [x] (let [d (double x)]
                       (cond (Double/isNaN d) d
                             (or (< d -1.0) (> d 1.0))
                             (throw-out-of-range "input is out of range")
                             (>= d 0.0) (asind-q1 d)
                             :else (- (asind-q1 (- d))))))))

(def sql-acosd
  (null-safe (fn [x] (let [d (double x)]
                       (cond (Double/isNaN d) d
                             (or (< d -1.0) (> d 1.0))
                             (throw-out-of-range "input is out of range")
                             (>= d 0.0) (acosd-q1 d)
                             :else (+ 90.0 (asind-q1 (- d))))))))

(def sql-atand
  (null-safe (fn [x] (let [d (double x)]
                       (if (Double/isNaN d) d
                           (* (/ (Math/atan d) atan-1-0) 45.0))))))

(def sql-atan2d
  (null-safe (fn [y x] (let [dy (double y) dx (double x)]
                         (if (or (Double/isNaN dy) (Double/isNaN dx)) Double/NaN
                             (* (/ (Math/atan2 dy dx) atan-1-0) 45.0))))))

;; ---------------------------------------------------------------------------
;; erf / erfc — PostgreSQL calls libm; the JDK has no equivalent.
;;
;; Abramowitz & Stegun 7.1.26 is only 1e-7; this is the higher-precision
;; incomplete-gamma style expansion, good to ~1e-15 relative, which is
;; what the differential asserts (15 significant digits, not bit
;; equality — we are matching glibc, not a specification).

(def ^:private ^:const log-sqrt-pi 0.5723649429247001)   ;; ln(Gamma(1/2))

(defn- gamma-p-series
  "Regularized lower incomplete gamma P(a,x) by its series expansion
   (Numerical Recipes gser), iterated to double precision."
  ^double [^double a ^double x]
  (loop [n 1 ap a del (/ 1.0 a) sum (/ 1.0 a)]
    (if (or (> n 300) (< (Math/abs del) (* (Math/abs sum) 1.0e-17)))
      (* sum (Math/exp (+ (- x) (* a (Math/log x)) (- log-sqrt-pi))))
      (let [ap' (+ ap 1.0)
            del' (* del (/ x ap'))]
        (recur (inc n) ap' del' (+ sum del'))))))

(defn- gamma-q-cf
  "Regularized upper incomplete gamma Q(a,x) by continued fraction
   (Numerical Recipes gcf, modified Lentz)."
  ^double [^double a ^double x]
  (let [fpmin 1.0e-300]
    (loop [i 1
           b (+ x 1.0 (- a))
           c (/ 1.0 fpmin)
           d (/ 1.0 (+ x 1.0 (- a)))
           h (/ 1.0 (+ x 1.0 (- a)))]
      (if (> i 300)
        (* h (Math/exp (+ (- x) (* a (Math/log x)) (- log-sqrt-pi))))
        (let [an (* (- i) (- i a))
              b' (+ b 2.0)
              d' (let [v (+ (* an d) b')] (if (< (Math/abs v) fpmin) fpmin v))
              c' (let [v (+ b' (/ an c))] (if (< (Math/abs v) fpmin) fpmin v))
              d'' (/ 1.0 d')
              del (* d'' c')
              h' (* h del)]
          (if (< (Math/abs (- del 1.0)) 1.0e-17)
            (* h' (Math/exp (+ (- x) (* a (Math/log x)) (- log-sqrt-pi))))
            (recur (inc i) b' c' d'' h')))))))

(defn- erf-series
  "erf(x) via the regularized incomplete gamma: erf(x) = P(1/2, x^2) for
   x >= 0. PostgreSQL just calls libm's erf, so this is matching glibc
   rather than a specification -- the differential asserts 15 significant
   digits, not bit equality."
  ^double [^double x]
  (cond
    (zero? x) 0.0
    :else
    (let [ax (Math/abs x)
          x2 (* ax ax)
          e (if (< x2 1.5)
              (gamma-p-series 0.5 x2)
              (- 1.0 (gamma-q-cf 0.5 x2)))]
      (if (neg? x) (- e) e))))

(def sql-erf
  (null-safe (fn [x] (let [d (double x)]
                       (cond (Double/isNaN d) d
                             (Double/isInfinite d) (if (pos? d) 1.0 -1.0)
                             :else (erf-series d))))))

(defn- erfc-series
  "erfc(x) = Q(1/2, x^2) for x >= 0, taken from the upper incomplete
   gamma directly rather than as `1 - erf(x)` -- the subtraction loses
   most of the significant digits once erf(x) approaches 1."
  ^double [^double x]
  (let [ax (Math/abs x)
        x2 (* ax ax)
        q (if (< x2 1.5)
            (- 1.0 (gamma-p-series 0.5 x2))
            (gamma-q-cf 0.5 x2))]
    (if (neg? x) (- 2.0 q) q)))

(def sql-erfc
  (null-safe (fn [x] (let [d (double x)]
                       (cond (Double/isNaN d) d
                             (Double/isInfinite d) (if (pos? d) 0.0 2.0)
                             :else (erfc-series d))))))

;; ---------------------------------------------------------------------------
;; numeric helpers

(def sql-div-trunc
  "div(y, x) — numeric_div_trunc: the quotient truncated toward zero, at
   scale 0. Distinct from `/` (sql-div), which carries
   select_div_scale's scale -- and named apart from it deliberately: the
   two differ by exactly the thing the name would hide."
  (null-safe
   (fn [a b]
     (if (or (numspecial? a) (numspecial? b))
       (let [q (checked-div a b)]
         (if (numspecial? q) q (.setScale ^java.math.BigDecimal q 0)))
       (do
         (when (and (number? b) (zero? b)) (throw-division-by-zero))
         (let [x (bigdec a) y (bigdec b)]
           (.setScale (.divideToIntegralValue ^java.math.BigDecimal x ^java.math.BigDecimal y)
                      0 java.math.RoundingMode/DOWN)))))))

(def sql-factorial
  (null-safe
   (fn [n]
     (let [v (long n)]
       (cond
         (neg? v) (throw-out-of-range "factorial of a negative number is undefined")
         (> v 100000) (throw-out-of-range "value overflows numeric format")
         :else (loop [i 2 acc java.math.BigInteger/ONE]
                 (if (> i v)
                   (java.math.BigDecimal. acc)
                   (recur (inc i) (.multiply acc (java.math.BigInteger/valueOf i))))))))))

(def sql-scale
  "scale(numeric) — the declared display scale."
  (null-safe (fn [v] (long (.scale (bigdec v))))))

(def sql-min-scale
  "min_scale(numeric) — the scale still needed after dropping trailing
   zeros, floored at 0."
  (null-safe (fn [v] (max 0 (long (.scale (.stripTrailingZeros (bigdec v))))))))

(def sql-trim-scale
  "trim_scale(numeric) — the value with trailing zeros removed."
  (null-safe (fn [v] (let [b (.stripTrailingZeros (bigdec v))]
                       (if (neg? (.scale b)) (.setScale b 0) b)))))

(defn- special-passthrough
  "Wrap a one-argument numeric function so a NaN / +-Infinity operand
   passes through unchanged, which is what PostgreSQL does for rounding
   and absolute value: round(Infinity) is Infinity, abs(NaN) is NaN.
   BigDecimal has no representation for any of them, so without this
   they reached the JVM as a record and raised."
  [f]
  ;; variadic: round and trunc take an optional scale, and a 1-arity
  ;; wrapper silently broke `round(1.005, 2)`.
  (fn [x & more]
    (if (types/numeric-special? x) x (apply f x more))))

(defn- special-abs [f]
  (fn [x & more]
    (if (types/numeric-special? x)
      (if (= :-inf (:kind x)) types/inf-numeric x)
      (apply f x more))))

(defn- special-sign [f]
  (fn [x & more]
    (if (types/numeric-special? x)
      (case (:kind x) :nan x :inf 1 :-inf -1)
      (apply f x more))))

(defn- special-null
  "scale() and min_scale() answer NULL for a special -- there is no
   scale to report."
  [f]
  (fn [x & more]
    (if (types/numeric-special? x) :__null__ (apply f x more))))

(defn pg-input-valid?
  "Pure subset of pg_input_is_valid(text, regtype) for application-facing
   scalar types. The function deliberately invokes the same input helpers as
   casts; validation must not become a second, more permissive parser."
  [value type-name]
  (try
    (let [s (str value)
          [_ base modifier] (re-matches #"(?is)^\s*(.+?)(?:\(([^)]*)\))?\s*$"
                                        (str type-name))
          base (some-> base str/lower-case str/trim)
          modifier (some-> modifier str/trim)
          int-value (fn [lo hi]
                      (let [n (Long/parseLong (str/trim s))]
                        (<= lo n hi)))
          float-value (fn []
                        (or (some? (coerce/special-float s))
                            (do (Double/parseDouble (str/trim s)) true)))
          char-value (fn []
                       (if modifier
                         (let [limit (Long/parseLong modifier)
                               unpadded (str/replace s #"\s+$" "")]
                           (<= (count unpadded) limit))
                         true))]
      (boolean
       (case base
         ("bool" "boolean") (some? (coerce/parse-bool-token s))
         ("int2" "smallint") (int-value -32768 32767)
         ("int4" "int" "integer") (int-value -2147483648 2147483647)
         ("int8" "bigint") (do (Long/parseLong (str/trim s)) true)
         ("oid") (let [n (Long/parseLong (str/trim s))]
                   (<= 0 n 4294967295))
         ("float4" "real" "float8" "double precision") (float-value)
         ("numeric" "decimal") (do (coerce/coerce-numeric s :bigdec) true)
         ("uuid") (do (java.util.UUID/fromString (str/trim s)) true)
         ("bit" "bit varying" "varbit")
         (do (pg-bits/parse-bit-literal (str/trim s)
                                        (contains? #{"bit varying" "varbit"} base))
             (if modifier
               (let [width (Long/parseLong modifier)]
                 (if (= base "bit") (= (count (str/trim s)) width)
                     (<= (count (str/trim s)) width)))
               true))
         ("char" "character" "varchar" "character varying" "text" "name")
         (char-value)
         false)))
    (catch Throwable _ false)))

(defn pg-input-error-info
  "The four-column record returned by pg_input_error_info. This is kept
   beside pg-input-valid? so both APIs share the same accepted scalar input
   surface. SQL NULL is represented by the query engine's sentinel."
  [value type-name]
  (let [[_ raw-base modifier] (re-matches #"(?is)^\s*(.+?)(?:\(([^)]*)\))?\s*$"
                                          (str type-name))
        base (some-> raw-base str/lower-case str/trim)
        display-type (case base
                       ("bool" "boolean") "boolean"
                       ("int2" "smallint") "smallint"
                       ("int4" "int" "integer") "integer"
                       ("int8" "bigint") "bigint"
                       ("varchar" "character varying") "character varying"
                       ("char" "character") "character"
                       base)
        display-type (if modifier
                       (str display-type "(" (str/trim modifier) ")")
                       display-type)
        too-long? (and modifier
                       (contains? #{"char" "character" "varchar" "character varying"} base))
        message (if too-long?
                  (str "value too long for type " display-type)
                  (str "invalid input syntax for type " display-type ": " (pr-str (str value))))
        sqlstate (if too-long? "22001" "22P02")]
    (if (pg-input-valid? value type-name)
      [nil nil nil nil]
      [message nil nil sqlstate])))

(def sql-function-specs
  "Function metadata shared by lowering, UPDATE evaluation, arity checking,
   and result-OID inference.

   New functions should start here. The older execution/arity/OID tables are
   incrementally derived from this registry as their entries migrate, avoiding
   another bespoke lowering branch for each PostgreSQL function family."
  {;; PostgreSQL resolves an unknown literal in these homogeneous numeric
   ;; calls from another, already-typed argument.  Lowering consumes this
   ;; metadata before translating the argument expressions.
   "div"              {:unknown-args :homogeneous}
   "log"              {:unknown-args :homogeneous}
   "mod"              {:unknown-args :homogeneous}
   "power"            {:unknown-args :homogeneous}
   "pow"              {:unknown-args :homogeneous}
   "gcd"              {:unknown-args :homogeneous}
   "lcm"              {:unknown-args :homogeneous}
   ;; The first three arguments share a numeric overload; count is int4.
   "width_bucket"     {:unknown-args {:homogeneous-prefix 3}}
   "booleq"           {:impl = :arities #{2}
                       :strict? true :return-oid types/oid-bool}
   "boolne"           {:impl not= :arities #{2}
                       :strict? true :return-oid types/oid-bool}
   "pg_input_is_valid" {:impl pg-input-valid? :arities #{2}
                        :strict? true :return-oid types/oid-bool}
   "get_bit"          {:impl pg-bits/get-bit :arities #{2}
                       :strict? true :return-oid types/oid-int4}
   "set_bit"          {:impl pg-bits/set-bit :arities #{3}
                       :strict? true :return-oid types/oid-bit}
   "bit_count"        {:impl pg-bits/bit-count :arities #{1}
                       :strict? true :return-oid types/oid-int8}
   "jsonb_contains"   {:impl jb/jsonb-contains?   :arities #{2}
                       :strict? true :return-oid types/oid-bool}
   "jsonb_contained"  {:impl jb/jsonb-contained?  :arities #{2}
                       :strict? true :return-oid types/oid-bool}
   "jsonb_exists"     {:impl jb/jsonb-exists?     :arities #{2}
                       :strict? true :return-oid types/oid-bool}
   "jsonb_exists_any" {:impl jb/jsonb-exists-any? :arities #{2}
                       :strict? true :return-oid types/oid-bool}
   "jsonb_exists_all" {:impl jb/jsonb-exists-all? :arities #{2}
                       :strict? true :return-oid types/oid-bool}})

(def ^:private legacy-sql-fn->clj-fn
  {"upper"    str/upper-case
   "lower"    str/lower-case
   ;; NOT bare `count`: a PgBit is a defrecord, so `count` returns its
   ;; number of MAP ENTRIES (2), not its bit width. PG's length() on a
   ;; bit string is the bit count; octet_length is ceil(bits/8).
   "length"   sql-length
   "abs"      (special-abs clojure.core/abs)
   "floor"    (special-passthrough #(Math/floor (double %)))
   "ceil"     (special-passthrough #(Math/ceil (double %)))
   "ceiling"  #(Math/ceil (double %))
   ;; The 2-argument forms take a SET OF CHARACTERS to strip, not a
   ;; prefix -- and str/triml takes only one argument, so `ltrim(s, 'ab')`
   ;; raised a raw ArityException.
   "trim"     sql-btrim
   "ltrim"    sql-ltrim
   "rtrim"    sql-rtrim
   "replace"  str/replace
   ;; Not clojure.core/max|min: those are numeric-only and threw a raw
   ;; ClassCastException on `greatest('a','b')` or two dates. Unlike
   ;; MIN/MAX these are not aggregates and PostgreSQL defines them over
   ;; any type with an ordering, so there is nothing to reject here.
   ;; NOT strict, unlike almost every other SQL function: PostgreSQL's
   ;; MinMaxExpr SKIPS null inputs, so `greatest(NULL, 5)` is 5, and the
   ;; result is NULL only when EVERY input is. Folding the sentinel
   ;; through order-cmp instead made one NULL argument poison the answer.
   "greatest" (fn [& args] (min-max-skipping-nulls args #(pos? (order-cmp %1 %2))))
   "least"    (fn [& args] (min-max-skipping-nulls args #(neg? (order-cmp %1 %2))))
   ;; sql-mod, not bare `rem`: `rem` neither raises PostgreSQL's
   ;; "division by zero" on a zero modulus (it threw a raw Java
   ;; "Divide by zero") nor promotes an integer operand against a
   ;; numeric one, so `mod(1, 3.0)` answered 1 rather than 1.0.
   "mod"      sql-mod

   ;; --- Math: PG semantics, see the "Math function implementations"
   ;; section above. Do NOT swap these back for bare Math/* methods —
   ;; each one differs from its Java namesake in domain checking,
   ;; base, or tie-breaking.
   ;; Each of these has a numeric overload in PostgreSQL, selected
   ;; whenever an argument is numeric -- see the numeric-* fns above.
   "sqrt"     (fn [x] (cond (numspecial? x) (numeric-special-unary sql-sqrt x)
                            (num-arg? x) (numeric-sqrt x)
                            :else (sql-sqrt x)))
   "cbrt"     sql-cbrt
   "exp"      (fn [x] (cond (numspecial? x) (numeric-special-unary sql-exp x)
                            (num-arg? x) (numeric-exp x)
                            :else (sql-exp x)))
   "ln"       (fn [x] (cond (numspecial? x) (numeric-special-unary sql-ln x)
                            (num-arg? x) (numeric-ln x)
                            :else (sql-ln x)))
   "log"      (fn ([x] (cond (numspecial? x) (numeric-special-unary sql-log x)
                             (num-arg? x) (numeric-log (java.math.BigDecimal. "10") x)
                             :else (sql-log x)))
                ([b x] (sql-log2 b x)))
   "log10"    (fn [x] (cond (numspecial? x) (numeric-special-unary sql-log10 x)
                            (num-arg? x) (numeric-log (java.math.BigDecimal. "10") x)
                            :else (sql-log10 x)))
   "power"    sql-power-op
   "pow"      sql-power-op
   "round"    (special-passthrough sql-round)
   "trunc"    (special-passthrough sql-trunc)
   "sign"     (special-sign sql-sign)
   "gcd"      sql-gcd
   ;; Degree trigonometry, div/factorial and the scale inspectors --
   ;; ported rather than derived; see their definitions.
   "sind"     sql-sind
   "cosd"     sql-cosd
   "tand"     sql-tand
   "cotd"     sql-cotd
   "asind"    sql-asind
   "acosd"    sql-acosd
   "atand"    sql-atand
   "atan2d"   sql-atan2d
   "erf"      sql-erf
   "erfc"     sql-erfc
   "random"   (fn [] (sql-random))
   "setseed"  sql-setseed
   "random_normal" (fn ([] (sql-random-normal))
                     ([m] (sql-random-normal m))
                     ([m sd] (sql-random-normal m sd)))
   "div"      sql-div-trunc
   "factorial" sql-factorial
   "scale"    (special-null sql-scale)
   "min_scale" (special-null sql-min-scale)
   "trim_scale" (special-passthrough sql-trim-scale)
   "lcm"      sql-lcm
   "width_bucket" sql-width-bucket
   "pi"       (fn [] Math/PI)
   ;; degrees/radians route through PG's checked multiply/divide, so
   ;; they can raise 22003 where Math/toDegrees silently returns Inf.
   "degrees"  #(let [d (double %)] (finite-range (Math/toDegrees d) d))
   "radians"  #(let [d (double %)] (finite-range (Math/toRadians d) d))
   ;; sin/cos/tan/cot reject an infinite argument (22003); tan and cot
   ;; deliberately do NOT check their result — PG documents cot(0) as
   ;; Infinity rather than an error.
   "sin"      #(let [d (finite-input %)] (finite-range (Math/sin d) d))
   "cos"      #(let [d (finite-input %)] (finite-range (Math/cos d) d))
   "tan"      #(Math/tan (finite-input %))
   "cot"      #(/ 1.0 (Math/tan (finite-input %)))
   "asin"     sql-asin
   "acos"     sql-acos
   "atan"     #(Math/atan (double %))
   "atan2"    #(Math/atan2 (double %1) (double %2))
   ;; sinh/cosh overflow to ±Infinity WITHOUT an error in PG (float.c
   ;; :2611) — unlike exp. cosh does raise on underflow. asinh has no
   ;; guards at all. Mirrored exactly, inconsistency included.
   "sinh"     #(Math/sinh (double %))
   "cosh"     #(let [d (double %) r (Math/cosh d)]
                 (if (and (zero? r) (not (zero? d)))
                   (throw-out-of-range "value out of range: underflow")
                   r))
   "tanh"     #(Math/tanh (double %))
   "asinh"    sql-asinh
   "acosh"    sql-acosh
   "atanh"    sql-atanh
   "lpad"     sql-lpad
   "rpad"     sql-rpad
   "repeat"   sql-repeat
   "initcap"  sql-initcap
   "reverse"  str/reverse
   "left"     sql-left
   "right"    sql-right
   "char_length"  sql-length
   "octet_length" sql-octet-length
   "bit_length"   sql-bit-length
   ;; `position(sub IN str)` swaps its operands at the PARSER (gram.y), so
   ;; the FUNCTION takes (string, substring) -- same as strpos, which shares
   ;; textpos with it in varlena.c. We had both pointing at a
   ;; (substring, string) implementation, so `strpos('abc','b')` answered 0.
   "position"     sql-position
   "strpos"       sql-position
   "ascii"        sql-ascii
   "date_part"    sql-extract
   "regexp_replace" sql-regexp-replace
   "regexp_like"    sql-regexp-like
   "regexp_count"   sql-regexp-count
   "regexp_substr"  sql-regexp-substr
   "regexp_instr"   sql-regexp-instr
   "chr"          sql-chr
   "btrim"        sql-btrim
   "md5"          sql-md5
   "starts_with"  sql-starts-with
   "split_part"   sql-split-part
   "translate"    sql-translate
   "overlay"      sql-overlay
   "quote_ident"  sql-quote-ident
   "quote_literal" sql-quote-literal
   "quote_nullable" sql-quote-nullable
   "to_hex"       sql-to-hex
   "pg_is_in_recovery"        pg-is-in-recovery
   "acldefault"               acldefault
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
   "pg_get_expr"          pg-get-expr

   ;; --- SQL:2011 Allen interval predicates ------------------------------
   ;;
   ;; 4-arg pure functions on half-open intervals `[from, to)`. Two
   ;; intervals are compared by their endpoints (longs or longs-encoded
   ;; instants). The 10 verbs cover the full Allen relation set plus
   ;; the SQL:2011-style boundary distinctions (`STRICTLY_*` excludes
   ;; touching boundaries; `IMMEDIATELY_*` requires exact touch).
   ;;
   ;; Signature: `(fn [a-from a-to b-from b-to] boolean)`
   ;; All endpoints must be the same orderable type; the planner does
   ;; not coerce. Usage:
   ;;   SELECT * FROM events
   ;;     WHERE OVERLAPS(a_start, a_end, b_start, b_end);
   "overlaps"               (fn [af at bf bt] (and (< af bt) (< bf at)))
   "equals_period"          (fn [af at bf bt] (and (= af bf) (= at bt)))
   "contains_period"        (fn [af at bf bt] (and (<= af bf) (>= at bt)))
   "strictly_contains_period" (fn [af at bf bt] (and (< af bf) (> at bt)))
   "precedes"               (fn [af at bf _bt] (<= at bf))
   "strictly_precedes"      (fn [af at bf _bt] (< at bf))
   "immediately_precedes"   (fn [_af at bf _bt] (= at bf))
   "succeeds"               (fn [af _at _bf bt] (>= af bt))
   "strictly_succeeds"      (fn [af _at _bf bt] (> af bt))
   "immediately_succeeds"   (fn [af _at _bf bt] (= af bt))
   ;; MEETS is the standard alias for IMMEDIATELY_PRECEDES (A.end == B.start)
   "meets"                  (fn [_af at bf _bt] (= at bf))})

(def sql-fn->clj-fn
  "Map of SQL function names (lowercased) to Clojure fn values.

   Values are actual `IFn` objects (not symbols) so we can wrap them in
   `null-safe` at emit time. Java static methods are wrapped in thin fns
   so they're callable through the same path."
  (merge legacy-sql-fn->clj-fn
         (into {} (keep (fn [[fname spec]]
                          (when (contains? spec :impl)
                            [fname (:impl spec)])))
               sql-function-specs)))

;; ---------------------------------------------------------------------------
;; Declared arities
;;
;; PG resolves a function call against pg_proc at PARSE time, so a wrong
;; argument count is a 42883 ("function sqrt(integer, integer) does not
;; exist") before anything runs. Our functions are plain IFns, so a bad
;; count used to surface at execute time as Clojure's own ArityException,
;; which reached the client as XX000 ("Wrong number of args (2) passed
;; to: fns/fn--72143") — an internal error string for what is really a
;; user-facing name-resolution failure.
;;
;; Entries map a lowercased SQL function name to the SET of argument
;; counts it accepts. A name ABSENT from this map is unchecked (the
;; catalog stubs and the variadic string helpers), so this can be filled
;; in incrementally without breaking anything.

(def ^:private legacy-sql-fn-arities
  {"pi"       #{0}
   "abs"      #{1} "sign"    #{1} "sqrt"  #{1} "cbrt"  #{1}
   "exp"      #{1} "ln"      #{1} "log10" #{1}
   "floor"    #{1} "ceil"    #{1} "ceiling" #{1}
   "degrees"  #{1} "radians" #{1}
   "sin"      #{1} "cos"     #{1} "tan"   #{1} "cot"  #{1}
   "asin"     #{1} "acos"    #{1} "atan"  #{1}
   "sinh"     #{1} "cosh"    #{1} "tanh"  #{1}
   "asinh"    #{1} "acosh"   #{1} "atanh" #{1}
   "log"      #{1 2}     ; log(x) = base 10; log(base, x)
   "round"    #{1 2}     ; round(x); round(x, decimals)
   "trunc"    #{1 2}
   "power"    #{2} "pow" #{2} "atan2" #{2} "mod" #{2} "gcd" #{2} "lcm" #{2}
   "sind" #{1} "cosd" #{1} "tand" #{1} "cotd" #{1}
   "asind" #{1} "acosd" #{1} "atand" #{1} "atan2d" #{2}
   "erf" #{1} "erfc" #{1}
   "random" #{0} "setseed" #{1} "random_normal" #{0 1 2}
   "div" #{2} "factorial" #{1}
   "scale" #{1} "min_scale" #{1} "trim_scale" #{1}
   "width_bucket" #{4}
   "upper"    #{1} "lower" #{1} "initcap" #{1} "reverse" #{1}
   "length"   #{1} "char_length" #{1} "octet_length" #{1} "bit_length" #{1}
   "left"     #{2} "right" #{2} "position" #{2} "strpos" #{2}
   "ascii"    #{1} "chr" #{1} "md5" #{1} "to_hex" #{1}
   "btrim"    #{1 2} "starts_with" #{2} "split_part" #{3} "translate" #{3}
   "ltrim"    #{1 2} "rtrim" #{1 2} "trim" #{1 2}
   "date_part" #{2}
   "regexp_replace" #{3 4} "regexp_like" #{2 3} "regexp_count" #{2 3 4}
   "regexp_substr"  #{2 3 4 5} "regexp_instr" #{2 3 4 5 6}
   "overlay"  #{3 4}
   "quote_ident" #{1} "quote_literal" #{1} "quote_nullable" #{1}
   "repeat"   #{2}
   "lpad"     #{2 3} "rpad" #{2 3}})

(def sql-fn-arities
  "SQL function name → set of accepted argument counts. Absent = unchecked."
  (merge legacy-sql-fn-arities
         (into {} (keep (fn [[fname spec]]
                          (when (contains? spec :arities)
                            [fname (:arities spec)])))
               sql-function-specs)))

(defn check-arity!
  "Raise 42883 when `argc` is not an accepted arity for `fname`. No-op
   for unchecked names. Called at translate time so the error lands
   where PG's does — during parse/analyze, not execution."
  [^String fname argc]
  (let [lname (str/lower-case fname)]
    (when-let [ok (get sql-fn-arities lname)]
      (when-not (contains? ok argc)
        ;; PG renders the signature with the argument types it resolved,
        ;; and "unknown" for untyped literals — `sqrt('a','b')` reports
        ;; `function sqrt(unknown, unknown) does not exist`. We don't
        ;; carry inferred arg types this far down, so report the arity
        ;; with `unknown` placeholders, which is what PG itself prints
        ;; for the untyped-literal case. The HINT is PG's verbatim.
        (throw (errors/pg-error
                :undefined-function
                {:function (str lname "(" (str/join ", " (repeat argc "unknown")) ")")
                 :hint (str "No function matches the given name and argument types. "
                            "You might need to add explicit type casts.")}))))))
