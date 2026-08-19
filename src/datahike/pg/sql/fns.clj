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
            ;; The same select_div_scale port the `/` operator uses.
            ;; This site used to carry its own AVG-specialised copy,
            ;; with its own weight/leading-digit helpers and its own
            ;; duplicate of the two PG constants -- two ports of one
            ;; rule, free to drift.
            rscale (int (div-result-scale sum n-bd))]
        (.divide ^java.math.BigDecimal sum
                 ^java.math.BigDecimal n-bd
                 rscale
                 java.math.RoundingMode/HALF_UP)))))

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

(defn- order-cmp
  "`compare`, except that a `byte[]` is not Comparable — PostgreSQL
   orders bytea by unsigned byte value (`byteacmp`, varlena.c), which is
   what compareUnsigned does."
  ^long [a b]
  (if (and (bytes? a) (bytes? b))
    (java.util.Arrays/compareUnsigned ^bytes a ^bytes b)
    (compare a b)))

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

(defn- box-array-agg
  "Box an ordered seq of array_agg element values (`:__null__` → nil) into a
   PgArray, inferring the element type from the first non-nil value."
  [ordered-vals]
  (let [vs (into [] (map #(if (= :__null__ %) nil %)) ordered-vals)
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
    (arr-fn elem-type vs)))

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
  (let [vs (mapv (fn [v] (if (= :__null__ v) :datahike.pg.jsonb/json-null v)) coll)]
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
      (into {} (map (fn [p] [(str (first p)) (second p)])) ps))))

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
  (if (and (number? a) (number? b)) (== a b) (= a b)))

(defn sql-ne?
  "SQL `<>`. The complement of `sql-eq?`; see there."
  [a b]
  (not (sql-eq? a b)))

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

(def sql-+ (null-safe (long-overflow->pg (float-checked + false))))
(def sql-- (null-safe (long-overflow->pg (float-checked - false))))
(def sql-* (null-safe (long-overflow->pg (float-checked * true))))

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
   (when (and (number? b) (zero? b)) (throw-division-by-zero))
   (cond
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
  (when (and (number? b) (zero? b)) (throw-division-by-zero))
  (if (and (or (decimal? a) (decimal? b)) (number? a) (number? b))
    (let [^java.math.BigDecimal x (bigdec a)
          ^java.math.BigDecimal y (bigdec b)]
      (.setScale (.remainder x y) (max (.scale x) (.scale y))
                 java.math.RoundingMode/UNNECESSARY))
    (rem a b)))

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
     if (result == 0.0 && arg1 != 0.0) float_underflow_error();

   An infinite *input* may legitimately produce an infinite result, and
   a zero input a zero result — only the finite→infinite and
   nonzero→zero transitions are errors. The UNDERFLOW half is the one
   that gets forgotten: `exp(-1000.0::float8)` is an error in PG."
  ^double [^double result ^double input]
  (cond
    (and (Double/isInfinite result) (not (Double/isInfinite input)))
    (throw-out-of-range "value out of range: overflow")
    (and (zero? result) (not (zero? input)))
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
  (let [o (double operand) l (double low) h (double high) n (long cnt)]
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
  (if (and (pg-bits/pg-bit? a) (pg-bits/pg-bit? b))
    (bitf a b)
    (intf (long a) (long b))))

(defn sql-bit-and [a b] (bit-dispatch pg-bits/and-bits bit-and a b))
(defn sql-bit-or  [a b] (bit-dispatch pg-bits/or-bits  bit-or  a b))
(defn sql-bit-xor [a b] (bit-dispatch pg-bits/xor-bits bit-xor a b))

(defn sql-bit-not
  "`~` — bitwise NOT."
  [a]
  (if (pg-bits/pg-bit? a) (pg-bits/not-bits a) (bit-not (long a))))

(defn sql-bit-shift-left
  "`<<`. On a bit string the width is preserved and vacated positions are
   zero-filled; on an integer it is a plain shift."
  [a n]
  (if (pg-bits/pg-bit? a)
    (pg-bits/shift-bits a (long n))
    (bit-shift-left (long a) (long n))))

(defn sql-bit-shift-right
  "`>>`. Arithmetic (sign-propagating) on integers, matching PG's bare C
   `>>` on a signed operand; width-preserving and zero-filled on a bit
   string."
  [a n]
  (if (pg-bits/pg-bit? a)
    (pg-bits/shift-bits a (- (long n)))
    (bit-shift-right (long a) (long n))))

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
   "stddev_pop"     'stddev
   "variance"       'datahike.pg.sql/filter-variance-samp
   "var_samp"       'datahike.pg.sql/filter-variance-samp
   "var_pop"        'variance
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

(def sql-fn->clj-fn
  "Map of SQL function names (lowercased) to Clojure fn values.

   Values are actual `IFn` objects (not symbols) so we can wrap them in
   `null-safe` at emit time. Java static methods are wrapped in thin fns
   so they're callable through the same path."
  {"upper"    str/upper-case
   "lower"    str/lower-case
   ;; NOT bare `count`: a PgBit is a defrecord, so `count` returns its
   ;; number of MAP ENTRIES (2), not its bit width. PG's length() on a
   ;; bit string is the bit count; octet_length is ceil(bits/8).
   "length"   sql-length
   "abs"      clojure.core/abs
   "floor"    #(Math/floor (double %))
   "ceil"     #(Math/ceil (double %))
   "ceiling"  #(Math/ceil (double %))
   "trim"     str/trim
   "ltrim"    str/triml
   "rtrim"    str/trimr
   "replace"  str/replace
   ;; Not clojure.core/max|min: those are numeric-only and threw a raw
   ;; ClassCastException on `greatest('a','b')` or two dates. Unlike
   ;; MIN/MAX these are not aggregates and PostgreSQL defines them over
   ;; any type with an ordering, so there is nothing to reject here.
   "greatest" (fn [& args] (reduce (fn [a b] (if (pos? (order-cmp b a)) b a)) args))
   "least"    (fn [& args] (reduce (fn [a b] (if (neg? (order-cmp b a)) b a)) args))
   ;; sql-mod, not bare `rem`: `rem` neither raises PostgreSQL's
   ;; "division by zero" on a zero modulus (it threw a raw Java
   ;; "Divide by zero") nor promotes an integer operand against a
   ;; numeric one, so `mod(1, 3.0)` answered 1 rather than 1.0.
   "mod"      sql-mod

   ;; --- Math: PG semantics, see the "Math function implementations"
   ;; section above. Do NOT swap these back for bare Math/* methods —
   ;; each one differs from its Java namesake in domain checking,
   ;; base, or tie-breaking.
   "sqrt"     sql-sqrt
   "cbrt"     sql-cbrt
   "exp"      sql-exp
   "ln"       sql-ln
   "log"      sql-log            ; base 10 (1-arg) / log(base, x) (2-arg)
   "log10"    sql-log10
   "power"    sql-power
   "pow"      sql-power
   "round"    sql-round
   "trunc"    sql-trunc
   "sign"     sql-sign
   "gcd"      sql-gcd
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
   "position"     sql-position
   "strpos"       sql-position
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

(def sql-fn-arities
  "SQL function name → set of accepted argument counts. Absent = unchecked."
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
   "width_bucket" #{4}
   "upper"    #{1} "lower" #{1} "initcap" #{1} "reverse" #{1}
   "length"   #{1} "char_length" #{1} "octet_length" #{1} "bit_length" #{1}
   "left"     #{2} "right" #{2} "position" #{2} "strpos" #{2}
   "repeat"   #{2}
   "lpad"     #{2 3} "rpad" #{2 3}})

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
