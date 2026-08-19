(ns datahike.pg.sql.cast
  "One implementation of `CAST(<value> AS <type>)`.

   The same cast semantics used to be written out four times — in
   `sql.clj`'s table-free literal fast path, in `expr.clj`'s
   `translate-cast`, in `stmt.clj`'s `apply-sql-cast`, and in
   `coerce.clj`'s INSERT path — each a `case`/`cond` over
   `types/cast-category` that had drifted from the others. Which copy ran
   depended on the SHAPE of the expression, not on its meaning, so the
   same cast could behave three ways:

     29::bit(4)          → literal fast path      → correct
     (-44)::bit(12)      → translate-cast         → passed through as -44
     '101'::bit(3)::int  → nested, another path   → read the digits as
                                                    decimal 101, not 5

   Issue #12 hit exactly this for `'1'::boolean`, and issue #19 hit it
   again for bit. Adding a branch to one copy fixes one shape.

   This namespace holds the value-level semantics — `(value, type) →
   value`. Callers keep their own surrounding logic (when to fold at
   translate time, how to bind a runtime var, how to read a JSqlParser
   AST node); they just stop reimplementing what a cast MEANS.

   `parse-timestamp` is injected rather than required because the parser
   lives in `expr.clj`, which requires this namespace's dependencies —
   taking it as an argument keeps this namespace a leaf and avoids a
   load cycle."
  (:require [clojure.string :as str]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.bits :as pg-bits]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.types :as types]))

(set! *warn-on-reflection* true)

(defn- bit-width
  "Width from a cast target. `cast-category` strips the `(…)`, so re-read
   it from the original type string. Bare `bit` means bit(1); bare `bit
   varying` means unlimited (nil)."
  [type-str varying?]
  (or (some-> (re-find #"\((\d+)\)" type-str) second Integer/parseInt)
      (when-not varying? 1)))

(defn- base-type-name
  "The cast target with its `(…)` modifier stripped, lower-cased."
  [type-str]
  (-> (str type-str) (clojure.string/replace #"\s*\([^)]*\)" "")
      clojure.string/trim clojure.string/lower-case))

(defn- out-of-range! [tname]
  (throw (errors/pg-error :numeric-value-out-of-range
                          {:message (str tname " out of range")})))

(defn numeric-typmod
  "`[precision scale]` from a `numeric(p[,s])` target, or nil for bare
   `numeric`. A modifier with no scale means scale 0 -- `numeric(10)`
   truncates to an integer, which is easy to miss."
  [type-str]
  (when-let [m (re-find #"\(\s*(\d+)\s*(?:,\s*(-?\d+)\s*)?\)" (str type-str))]
    [(Integer/parseInt (nth m 1))
     (if (nth m 2) (Integer/parseInt (nth m 2)) 0)]))

(defn apply-numeric-typmod
  "PostgreSQL's apply_typmod (numeric.c): round to the declared scale,
   then reject anything whose integer part no longer fits the declared
   precision.

   Both halves were missing. The scale is why `123.456::numeric(10,1)`
   answered 123.456 instead of 123.5, and the precision is why
   `123456::numeric(5,2)` was accepted at all -- 22003 numeric field
   overflow was never raised on any path."
  [^java.math.BigDecimal v p s]
  (let [scaled (.setScale v (int s) java.math.RoundingMode/HALF_UP)
        ;; PG's own test: |value| must be < 10^(p-s).
        limit (.pow (java.math.BigDecimal. "10") (int (- p s)))]
    (if (>= (.compareTo (.abs scaled) limit) 0)
      (throw (errors/pg-error
              :numeric-value-out-of-range
              ;; PostgreSQL puts the arithmetic in DETAIL, not the message.
              {:message "numeric field overflow"
               :detail (str "A field with precision " p ", scale " s
                            " must round to an absolute value less than 10^"
                            (- p s) ".")}))
      scaled)))

(defn cast-to-integer
  "Cast to one of PostgreSQL's three integer widths.

   Two things this has to do that a plain `coerce-numeric … :long` does
   not. It ROUNDS rather than truncates -- and the two source families
   round DIFFERENTLY, which is not a detail we get to smooth over:

     float  -> int   rint, half to EVEN          (float.c dtoi4)
     numeric-> int   half AWAY FROM ZERO         (numeric.c round_var)

   so `2.5::float8::int` is 2 while `2.5::numeric::int` is 3. And it
   RANGE-CHECKS against the target width: every integer target used to
   collapse to Java long, so `100000::int2` and `99999999999::int4`
   passed through unchanged where PostgreSQL raises 22003."
  [v type-str]
  (let [w (get types/integer-type-width (base-type-name type-str) :int8)
        [lo hi tname] (get types/integer-width-limits w)
        rounded (cond
                  (integer? v)      v
                  (decimal? v)      (.setScale ^java.math.BigDecimal v 0
                                               java.math.RoundingMode/HALF_UP)
                  (number? v)       (Math/rint (double v))
                  :else             (coerce/coerce-numeric v :long))]
    ;; Compare before narrowing: `(long 1e30)` saturates silently, so a
    ;; range test on the narrowed value would pass.
    (if (number? rounded)
      (let [cmp (bigdec rounded)]
        (if (or (neg? (compare cmp (bigdec lo)))
                (pos? (compare cmp (bigdec hi))))
          (out-of-range! tname)
          (long rounded)))
      rounded)))

(defn cast-to-float
  "float4 and float8. `real` is a DISTINCT type, not a spelling of
   double precision: `1.1::real` is 1.100000023841858, and a value that
   does not fit is an error rather than an Infinity."
  [v type-str]
  (let [d (coerce/coerce-numeric v :double)]
    (if (= :float4 (get {"float4" :float4 "real" :float4} (base-type-name type-str)))
      (let [f (float d)]
        (if (and (Double/isFinite (double d)) (Float/isInfinite f))
          (out-of-range! "real")
          f))
      d)))

(defn cast-to-bit
  "int / text / bit → bit(n) or bit varying(n).

   An integer source keeps the RIGHTMOST n bits and sign-extends on the
   left (varbit.c:1550), which is why `(-44)::bit(12)` is
   `111111010100` and not the digits of -44."
  [v type-str explicit?]
  (let [varying? (= :varbit (types/cast-category type-str))
        w (bit-width type-str varying?)]
    (cond
      (pg-bits/pg-bit? v)
      (-> (assoc v :varying? varying?) (pg-bits/coerce-width w explicit?))

      (number? v)
      (cond-> (pg-bits/from-integer (long v) (or w 1))
        varying? (assoc :varying? true))

      :else
      (-> (pg-bits/parse-bit-literal (str v) varying?)
          (pg-bits/coerce-width w explicit?)))))

(defn cast-scalar
  "Apply a SQL cast of `v` to the target named by `type-str`.

   Options:
     :explicit?       — an explicit CAST reshapes silently; an assignment
                        raises instead (matters for bit width coercion).
     :parse-timestamp — fn String → java.util.Date, from expr.clj.
     :resolve-regclass— fn String → oid, for `::regclass`.
     :prefer-local-datetime? — return a LocalDateTime (microsecond
                        precision) rather than a Date for a timestamp
                        cast. See the :timestamp branch.
     :src-oid         — the OID of the value being cast, when the caller
                        knows it. Only `::text` uses it, to tell a date
                        from a timestamp: both are java.util.Date here.

   Returns `v` unchanged for a target this doesn't classify, which is
   what every call site did before and keeps unknown types passing
   through rather than erroring."
  [v type-str {:keys [explicit? parse-timestamp resolve-regclass
                      prefer-local-datetime? src-oid]
               :or {explicit? true}}]
  (if (or (nil? v) (= :__null__ v))
    v
    (let [cat (types/cast-category type-str)]
      (case cat
        ;; Both json types VALIDATE on input — `json_in` does a full
        ;; RFC-8259 parse and only then keeps the original bytes — and
        ;; only jsonb normalises afterwards. Handled here rather than at
        ;; either call site because a BARE literal cast is constant-folded
        ;; in sql.clj while any other cast reaches translate-cast-expr,
        ;; and both delegate here.
        (:json :jsonb)
        (if (string? v)
          (do ((requiring-resolve 'datahike.pg.jsonb/validate-json!) v)
              (if (= :jsonb cat)
                ((requiring-resolve 'datahike.pg.jsonb/serialize-jsonb) v)
                v))
          (if (= :jsonb cat)
            ((requiring-resolve 'datahike.pg.jsonb/serialize-jsonb) v)
            v))
        ;; A bit value cast to a number is a REINTERPRETATION of its bits
        ;; (varbit.c:1598), not a decimal read of its digits, so this has
        ;; to come before the generic numeric branches — and a PgBit
        ;; reaching `str` would stringify as a defrecord.
        (:integer :float :numeric)
        (if (pg-bits/pg-bit? v)
          (let [n (pg-bits/to-long v)]
            (case cat :float (double n) n))
          (case cat
            :integer (cast-to-integer v type-str)
            :float   (cast-to-float v type-str)
            :numeric (let [bd (coerce/coerce-numeric v :bigdec)]
                       (if-let [[p sc] (numeric-typmod type-str)]
                         (apply-numeric-typmod bd p sc)
                         bd))))

        ;; `str` on a temporal value is java.util.Date.toString, which is
        ;; both the wrong format and rendered in the JVM's default time
        ;; zone — see types/temporal->pg-text.
        :text (cond
                (pg-bits/pg-bit? v) (pg-bits/to-pg-text v)
                (pg-arr/array? v)   (pg-arr/to-pg-text v)
                (string? v)         v
                :else               (types/->pg-text v src-oid))

        :boolean (if (boolean? v)
                   v
                   (let [b (coerce/parse-bool-token (str v))]
                     (when (nil? b)
                       (throw (errors/pg-error :invalid-text-representation
                                               {:type "boolean" :value (str v)})))
                     b))

        (:bit :varbit) (cast-to-bit v type-str explicit?)

        :uuid (if (instance? java.util.UUID v)
                v
                (java.util.UUID/fromString (str v)))

        :bytes (cond
                 (bytes? v)  v
                 (string? v) (or (coerce/parse-bytea-hex v)
                                 (.getBytes ^String v "UTF-8"))
                 :else v)

        :timestamp
        (cond
          (instance? java.util.Date v) v
          (instance? java.time.LocalDateTime v) v
          :else
          (let [norm (-> (str v) str/trim
                         (str/replace #"(\d{4}-\d{2}-\d{2})\s+(\d)" "$1T$2"))
                ;; LocalDateTime keeps microseconds; parse-timestamp routes
                ;; through java.util.Date, which is millisecond-only, and
                ;; pgjdbc asserts the full '…130861' in its error strings.
                ;; Only the literal-fold path takes the precise branch
                ;; today — the others feed values into stores expecting a
                ;; Date. Unifying that is a separate change.
                ldt (when prefer-local-datetime?
                      (try (java.time.LocalDateTime/parse norm)
                           (catch Exception _ nil)))]
            (or ldt
                (if parse-timestamp (parse-timestamp (str v)) v))))

        :date (cond
                (instance? java.time.LocalDate v) v
                (instance? java.util.Date v)
                (-> ^java.util.Date v .toInstant
                    (.atZone java.time.ZoneOffset/UTC) .toLocalDate)
                (instance? java.time.LocalDateTime v)
                (.toLocalDate ^java.time.LocalDateTime v)
                :else
                (let [s (str/trim (str v))]
                  (or (try (java.time.LocalDate/parse
                            s (java.time.format.DateTimeFormatter/ofPattern "yyyy-M-d"))
                           (catch Exception _ nil))
                      (try (java.time.LocalDate/parse (first (str/split s #"[ T]")))
                           (catch Exception _ nil))
                      (when parse-timestamp
                        (let [d (parse-timestamp s)]
                          (when (instance? java.util.Date d)
                            (-> ^java.util.Date d .toInstant
                                (.atZone java.time.ZoneOffset/UTC) .toLocalDate))))
                      v)))

        :time (cond
                (instance? java.time.LocalTime v) v
                (instance? java.time.LocalDateTime v)
                (.toLocalTime ^java.time.LocalDateTime v)
                :else (let [s (str/trim (str v))
                            time-only (or (second (re-find #"^\d{4}-\d{1,2}-\d{1,2}[ T](.+)$" s)) s)]
                        (try (java.time.LocalTime/parse time-only)
                             (catch Exception _ v))))

        ;; Not a width-classified category — the OID-name types.
        (cond
          (= type-str "regnamespace") 2200
          (= type-str "regclass") (if resolve-regclass (resolve-regclass (str v)) v)
          :else v)))))
