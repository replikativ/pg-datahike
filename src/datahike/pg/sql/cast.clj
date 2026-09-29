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
            [datahike.pg.input :as input]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.tsearch :as tsearch]
            [datahike.pg.types :as types]
            [datahike.pg.vector :as pg-vector]))

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
     (if (nth m 2)
       (types/decode-numeric-scale (Integer/parseInt (nth m 2)))
       0)]))

(defn apply-numeric-typmod
  "PostgreSQL's apply_typmod (numeric.c): round to the declared scale,
   then reject anything whose integer part no longer fits the declared
   precision.

   Both halves were missing. The scale is why `123.456::numeric(10,1)`
   answered 123.456 instead of 123.5, and the precision is why
   `123456::numeric(5,2)` was accepted at all -- 22003 numeric field
  overflow was never raised on any path."
  [v p s]
  (if (types/numeric-special? v)
    (if (= :nan (:kind v))
      v
      (throw (errors/pg-error
              :numeric-value-out-of-range
              {:message "numeric field overflow"
               :detail (str "A field with precision " p ", scale " s
                            " cannot hold an infinite value.")})))
    (let [^java.math.BigDecimal decimal v
          scaled (.setScale decimal (int s) java.math.RoundingMode/HALF_UP)
          result (if (neg? s) (.setScale scaled 0) scaled)
          ;; PG's own test: |value| must be < 10^(p-s).
          ;; scaleByPowerOfTen also covers s > p, where the exponent is
          ;; negative (for example numeric(3,6) has a limit of 10^-3).
          limit (.scaleByPowerOfTen java.math.BigDecimal/ONE (int (- p s)))]
      (if (>= (.compareTo (.abs scaled) limit) 0)
        (throw (errors/pg-error
                :numeric-value-out-of-range
                ;; PostgreSQL puts the arithmetic in DETAIL, not the message.
                {:message "numeric field overflow"
                 :detail (let [exponent (- p s)]
                           (str "A field with precision " p ", scale " s
                                " must round to an absolute value less than "
                                (if (zero? exponent) "1" (str "10^" exponent))
                                "."))}))
        result))))

(def ^:private money-min-cents (biginteger Long/MIN_VALUE))
(def ^:private money-max-cents (biginteger Long/MAX_VALUE))

(defn parse-money
  "Parse PostgreSQL's `money` input in the C locale.

   The upstream regression suite fixes `lc_monetary` to C. Its input
   accepts a dollar sign, comma separators, and either a minus sign or
   parentheses for a negative amount. PostgreSQL stores an int64 count
   of cents; we retain the equivalent two-scale BigDecimal carrier."
  [v]
  (if-not (string? v)
    (let [^java.math.BigDecimal bd (coerce/coerce-numeric v :bigdec)]
      (.setScale bd 2 java.math.RoundingMode/HALF_UP))
    (let [input v
          ;; cash.c cash_in, positionally: [ ( ] [sign] [$] [sign] digits
          ;; (with , separators) [.digits] [sign] [$] [ ) ]. It used to strip
          ;; every '-', space and paren wherever they were, so
          ;; '2020-01-01'::money answered -$20,200,101.00.
          [_ lp s1 s2 digits s3 rp]
          (re-matches #"\s*(\()?\s*([+-])?\s*\$?\s*([+-])?\s*(\d[\d,]*(?:\.\d*)?|\.\d+)\s*([+-])?\s*\$?\s*(\))?\s*" v)
          signs (remove nil? [s1 s2 s3])]
      (when (or (nil? digits) (not= (some? lp) (some? rp)) (> (count signs) 1)
                (and lp (seq signs)))
        (throw (errors/pg-error :invalid-text-representation
                                {:type "money" :value input})))
      (let [negative? (or (some? lp) (= ["-"] signs))
            numeric-text (str/replace digits "," "")
            unsigned (java.math.BigDecimal. numeric-text)
            signed (if negative? (.negate unsigned) unsigned)
            scaled (.setScale signed 2 java.math.RoundingMode/HALF_UP)
            cents (.toBigIntegerExact (.movePointRight scaled 2))]
        (when (or (neg? (.compareTo cents money-min-cents))
                  (pos? (.compareTo cents money-max-cents)))
          (throw (errors/pg-error
                  :numeric-value-out-of-range
                  {:message (str "value " (pr-str input)
                                 " is out of range for type money")})))
        scaled))))

(def ^:private char-type-limit
  "The text types that carry a length modifier, and whether an over-long
   value is truncated or refused. PostgreSQL truncates on an EXPLICIT
   cast and raises 22001 on an assignment -- `text` has no limit at all."
  #{"varchar" "character varying" "char" "character" "bpchar"})

(defn- text-length-limit
  "The `n` of `varchar(n)` / `char(n)`, or nil when the target carries no
   length."
  [type-str]
  (when (contains? char-type-limit (base-type-name type-str))
    (some-> (re-find #"\(\s*(\d+)\s*\)" (str type-str)) second Integer/parseInt)))

(defn- apply-text-length
  "Truncate to the declared length. `cast-scalar`'s explicit? flag is the
   same distinction PostgreSQL draws: an explicit cast truncates
   silently, an assignment refuses."
  [^String v type-str explicit?]
  (if-let [n (text-length-limit type-str)]
    (if (<= (count v) n)
      v
      (if explicit?
        (subs v 0 n)
        (throw (errors/pg-error
                :string-data-right-truncation
                {:message (str "value too long for type "
                               (if (contains? #{"char" "character" "bpchar"}
                                              (base-type-name type-str))
                                 "character(" "character varying(")
                               n ")")}))))
    v))

(declare cast-number-to-integer integer-to-oid)

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
  (if (string? v)
    ;; Text is READ, not converted: int4in, not a numeric cast, so
    ;; '1.5'::int is 22P02 where 1.5::int rounds.
    (let [base (base-type-name type-str)]
      (input/parse (if (= "oid" base)
                     types/oid-oid
                     (get {:int2 types/oid-int2 :int4 types/oid-int4 :int8 types/oid-int8}
                          (get types/integer-type-width base :int8)))
                   v))
    (if (and (integer? v) (= "oid" (base-type-name type-str)))
      (integer-to-oid v)
      (cast-number-to-integer v type-str))))

(defn- integer-to-oid
  "int4 -> oid reinterprets the bits, so a negative wraps into uint32;
   int8 -> oid (oid.c int8_oid) accepts 0 .. 4294967295. Both arrive as a
   Long here, so a value in either domain is accepted."
  [v]
  (let [n (long v)]
    (cond
      (<= 0 n 4294967295) n
      (<= -2147483648 n -1) (bit-and n 0xFFFFFFFF)
      :else (throw (errors/pg-error :numeric-value-out-of-range
                                    {:message "OID out of range"})))))

(defn- cast-number-to-integer
  "cast-to-integer for a non-text source: round and range-check."
  [v type-str]
  (let [w (get types/integer-type-width (base-type-name type-str) :int8)
        [lo hi tname] (get types/integer-width-limits w)
        _ (when (types/numeric-special? v)
            ;; PostgreSQL names the value rather than the range here.
            (throw (errors/pg-error
                    :numeric-value-out-of-range
                    {:message (str "cannot convert "
                                   (if (= :nan (:kind v)) "NaN" "infinity")
                                   " to " tname)})))
        _ (when (and (number? v)
                     (or (Double/isNaN (double v)) (Double/isInfinite (double v))))
            ;; PostgreSQL reports these as a plain range failure for the
            ;; target width; without the guard the JVM raised a raw
            ;; "Infinite or NaN" out of BigDecimal.
            (out-of-range! (nth (get types/integer-width-limits
                                     (get types/integer-type-width
                                          (base-type-name type-str) :int8))
                                2)))
        rounded (cond
                  (integer? v)      v
                  ;; `true::int` is 1 and `false::int` is 0. Not a number
                  ;; to coerce-numeric, which raised "cannot coerce class
                  ;; java.lang.Boolean to bigint" -- so a projected
                  ;; comparison could not be cast, as in `(a = 10)::int`.
                  (boolean? v)      (if v 1 0)
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
  (let [float4? (contains? #{"float4" "real"} (base-type-name type-str))
        tname (if float4? "real" "double precision")
        narrow (fn [^double d]
                 ;; .floatValue, not Clojure's `float`, which range-checks
                 ;; and raises on an infinity -- and an infinity narrows to
                 ;; a float infinity perfectly well.
                 (if float4?
                   (let [f (.floatValue (Double/valueOf d))]
                     (if (and (Double/isFinite d) (Float/isInfinite f))
                       (out-of-range! "real")
                       f))
                   d))]
    (cond
      ;; Text is read by float4in / float8in.
      (string? v)
      (input/parse (if float4? types/oid-float4 types/oid-float8) v)

      (types/numeric-special? v)
      ;; A numeric NaN / +-Infinity maps straight onto the float one.
      (narrow (types/numeric-special->double v))

      :else
      (let [d (double (coerce/coerce-numeric v :double))
            ;; A source that is ALREADY special is not an overflow --
            ;; `'Infinity'::numeric::float8` is Infinity, not an error.
            ;; Only a Double or Float can BE infinite; a BigDecimal is
            ;; exact however large, so testing it
            ;; via `(double v)` would trigger the very overflow we are
            ;; checking for on a finite source.
            src-finite? (and (nil? (coerce/special-float v))
                             (not (and (or (instance? Double v) (instance? Float v))
                                       (Double/isInfinite (double v)))))
            too-big (fn []
                      (throw (errors/pg-error
                              :numeric-value-out-of-range
                              {:message (str "\""
                                             (if (decimal? v)
                                               (.toPlainString ^java.math.BigDecimal v) v)
                                             "\" is out of range for type " tname)})))]
        ;; A literal too large or too small for the target is an ERROR in
        ;; PostgreSQL (float8in_internal checks ERANGE), not an Infinity or
        ;; a silent zero.
        (when (and src-finite? (Double/isInfinite d)) (too-big))
        (when (and src-finite? (zero? d) (number? v)
                   ;; compare exactly, not through the double that just
                   ;; underflowed to zero
                   (not (zero? (.signum (bigdec v)))))
          (too-big))
        (narrow d)))))

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

(def ^:private time-input
  ;; H:MM[:SS[.frac]] [AM|PM] [zone], or compact HHMMSS -- the forms of
  ;; datetime.c DecodeTimeOnly that applications use. Zone: Z, UTC, or
  ;; +-HH[[:]MM[[:]SS]].
  #"(?i)^(?:(\d{1,2}):(\d{1,2})(?::(\d{1,2})(?:\.(\d+))?)?|(\d{2})(\d{2})(\d{2}))\s*(am|pm)?\s*(z|utc|[+-]\d{1,2}(?::?\d{2}(?::?\d{2})?)?)?$")

(defn- parse-time-input
  "PostgreSQL time / timetz input. A plain `time` ignores a zone, as
   time_in does; `timetz` keeps it and defaults to the session zone (UTC).
   A leading date (`2020-01-01 10:00`) is allowed and dropped. Raises 22007
   for text that is not a time and 22008 for an out-of-range field -- it
   used to hand the unparsed STRING back, typed as a time."
  [^String input timetz?]
  (let [s (str/trim input)
        time-part (or (second (re-find #"^\d{4}-\d{1,2}-\d{1,2}[ T](.+)$" s)) s)
        type-name (if timetz? "time with time zone" "time")
        [_ h m sec frac ch cm cs ampm zone] (re-matches time-input time-part)]
    (when-not (or h ch)
      (throw (errors/pg-error :invalid-datetime-format {:type type-name :value input})))
    (let [raw-hour (Long/parseLong (or h ch))
          minute (Long/parseLong (or m cm))
          second (Long/parseLong (or sec cs "0"))
          ;; Rounded to microseconds, as PostgreSQL stores them.
          micros (if frac
                   (.longValue (.setScale (.movePointRight (java.math.BigDecimal. (str "0." frac)) 6)
                                          0 java.math.RoundingMode/HALF_UP))
                   0)
          hour (case (some-> ampm str/lower-case)
                 "am" (if (= 12 raw-hour) 0 raw-hour)
                 "pm" (if (< raw-hour 12) (+ raw-hour 12) raw-hour)
                 raw-hour)
          ;; 24:00:00 exactly is a valid PostgreSQL time; nothing later is.
          end-of-day? (and (= 24 hour) (zero? minute) (zero? second) (nil? frac))]
      (when (or (and (> hour 23) (not end-of-day?)) (> minute 59) (> second 59)
                (and ampm (or (zero? raw-hour) (> raw-hour 12))))
        (throw (errors/pg-error :datetime-field-overflow {:value input})))
      (let [nanos-of-day (+ (* (+ (* (+ (* hour 60) minute) 60) second) 1000000000)
                            (* micros 1000))
            end-of-day? (= nanos-of-day 86400000000000)
            _ (when (> nanos-of-day 86400000000000)
                (throw (errors/pg-error :datetime-field-overflow {:value input})))
            t (when-not end-of-day? (java.time.LocalTime/ofNanoOfDay nanos-of-day))
            offset (when timetz?
                     (if (or (nil? zone) (#{"z" "utc"} (str/lower-case zone)))
                       java.time.ZoneOffset/UTC
                       (let [[_ sign zh zm zs] (re-matches #"([+-])(\d{1,2}):?(\d{2})?:?(\d{2})?" zone)
                             secs (+ (* 3600 (Long/parseLong zh))
                                     (* 60 (Long/parseLong (or zm "0")))
                                     (Long/parseLong (or zs "0")))]
                         (java.time.ZoneOffset/ofTotalSeconds
                          (int (if (= "-" sign) (- secs) secs))))))]
        (cond
          ;; 24:00:00 is a valid PostgreSQL time (pgjdbc sends LocalTime.MAX
          ;; as it) but java.time has no end-of-day value. Time values are
          ;; stored as their canonical text, so carry this one AS that text:
          ;; it stores, renders and orders correctly (zero-padded).
          end-of-day?
          (str "24:00:00" (when timetz?
                            ((requiring-resolve 'datahike.pg.types/offset-text) offset)))
          (not timetz?) t
          :else (java.time.OffsetTime/of t offset))))))

(defn- internal-char-in
  "PostgreSQL's charin followed by charout (utils/adt/char.c), since a
   `\"char\"` is held as its output text. Input is a `\\ooo` octal escape or
   else the FIRST BYTE of the UTF-8 text (\\0 when empty). Output is empty
   for \\0, the character itself when ASCII, and `\\ooo` for a high-bit
   byte -- so `'é'::\"char\"` is `\\303`, not `é`."
  [^String t]
  (let [b (cond
            (empty? t) 0
            (re-matches #"\\[0-3][0-7][0-7]" t) (Long/parseLong (subs t 1) 8)
            :else (bit-and 0xff (aget (.getBytes t java.nio.charset.StandardCharsets/UTF_8) 0)))]
    (cond
      (zero? b) ""
      (< b 0x80) (str (char b))
      :else (format "\\%03o" b))))

(def month-names
  ;; PostgreSQL's own table (`datetktbl` in datetime.c) takes the
  ;; three-letter abbreviation and the full name, case-insensitively.
  ;; It lives here, not beside the parser that reads it, because
  ;; classifying a FAILURE needs it too: `Feb 30, 2024` names a real
  ;; month, so it is a field out of range (22008), where `Foo 30,
  ;; 2024` is not a date at all (22007).
  (into {}
        (mapcat (fn [[i full]] [[(subs full 0 3) i] [full i]]))
        (map-indexed (fn [i m] [(inc i) m])
                     ["january" "february" "march" "april" "may" "june" "july"
                      "august" "september" "october" "november" "december"])))

(defn month-name->number [^String t]
  (get month-names (str/lower-case t)))

(defn- month-name-date-shape?
  "A date spelled with a month NAME and otherwise only digits and
   separators -- the month-name counterpart of `ymd-shape`."
  [^String s]
  (let [body (str/trim (str/replace s #"\s+\d{1,2}:\d{2}(:\d{2}(\.\d+)?)?$" ""))
        toks (remove str/blank? (str/split body #"[,\-/ ]+"))]
    (and (seq toks)
         (every? #(or (re-matches #"\d{1,6}" %) (month-name->number %)) toks)
         (some month-name->number toks)
         (some #(re-matches #"\d{4}" %) toks))))

(defn- date-ish-shape?
  "Text written as date FIELDS -- digits and separators, or a month
   name -- whatever the field order.

   Only for telling 22008 (fields out of range) from 22007 (not a date
   at all). It must never decide which parser runs: `13/10/2017` and
   `2/30/2017` are out of RANGE in PostgreSQL, and so is `2024-02-30`,
   but only the dash spelling is read by the strict y-m-d parser."
  [^String s]
  (let [t (str/trim s)]
    (or (re-matches #"(?i)^\d{1,6}[-/]\d{1,2}[-/]\d{1,4}([ T].*|\s+(ad|bc))?$" t)
        (month-name-date-shape? t))))

(defn bad-timestamp!
  "The timestamp counterpart of `bad-date!`: PostgreSQL tells a value
   whose date FIELDS are impossible (22008) from text that is not a
   timestamp at all (22007), and says so with the target's own name.

   Public because the INSERT coercion needs the same verdict -- writing
   a timestamp has to refuse exactly what casting one refuses, and by
   the same rule."
  [^String s tz?]
  (if (date-ish-shape? s)
    (throw (ex-info (str "date/time field value out of range: \"" s "\"")
                    {:error :datetime-field-overflow :sqlstate "22008"}))
    (throw (ex-info (str "invalid input syntax for type timestamp"
                         (when tz? " with time zone") ": \"" s "\"")
                    {:error :invalid-datetime-format :sqlstate "22007"}))))

(def ^:private ymd-shape
  "A date written as digits and separators -- the shape PostgreSQL calls
   out of RANGE rather than bad SYNTAX when its fields do not make a
   date."
  ;; Routes to `parse-date-strict`, which now reads either separator by
  ;; DateStyle order -- so slashes belong here. They did NOT while that
  ;; parser was `uuuu-M-d`: widening this regex then sent `8/10/2017`
  ;; to a parser that could not read slashes and turned a date
  ;; PostgreSQL accepts into an error. Classification stays separate
  ;; (`date-ish-shape?`), because the two jobs diverge: `Feb 30, 2024`
  ;; is a date SHAPE that this parser must not be handed.
  #"(?i)^\d{1,6}[-/]\d{1,6}[-/]\d{1,6}(\s+(ad|bc))?$")

(defn- two-digit-year
  "PostgreSQL maps a two-digit year 70-99 to the 1900s and 00-69 to the
   2000s (`AdjustYearToCurrentCentury`, datetime.c)."
  [^long y]
  (cond (>= y 100) y (>= y 70) (+ 1900 y) :else (+ 2000 y)))

(defn decode-numeric-date
  "Three numeric date fields to a LocalDate, per `DateStyle`'s field
   ORDER -- the half of DateStyle that says whether `8/10/2017` is
   August 10th or the 8th of October.

   PostgreSQL's rules (DecodeDateTime):

     - a leading field of FOUR or more digits is the year, whatever the
       order, so `2017-08-10` reads the same under MDY, DMY and YMD;
     - otherwise the order assigns the fields, and a two-digit year is
       mapped to a century;
     - the fields are then CHECKED, not rolled: `13/10/2017` is a month
       13 under MDY and an error, while under DMY it is the 13th of
       October.

   Returns nil when the fields do not make a date; the caller decides
   whether that is 22008 or 22007. Separators do not matter here --
   `8/10/2017` and `10-08-2017` differ only in punctuation, and
   PostgreSQL reads both by order."
  [^String f1 ^String f2 ^String f3 order]
  (let [n1 (parse-long f1) n2 (parse-long f2) n3 (parse-long f3)]
    (when (and n1 n2 n3)
      (let [[y m d] (cond
                      ;; A four-digit leading field is unambiguous and
                      ;; wins over the order, which is why `2017-08-10`
                      ;; is the same date under every DateStyle.
                      (>= (count f1) 4) [n1 n2 n3]
                      (= order :ymd)    [(two-digit-year n1) n2 n3]
                      (= order :dmy)    [(if (>= (count f3) 4) n3 (two-digit-year n3)) n2 n1]
                      :else             [(if (>= (count f3) 4) n3 (two-digit-year n3)) n1 n2])]
        (try (java.time.LocalDate/of (int y) (int m) (int d))
             (catch Exception _ nil))))))

(defn- parse-date-strict
  "A numeric date, with the fields checked rather than rolled and the
   session's `DateStyle` order applied.

   It used to be `uuuu-M-d` and nothing else, so the ORDER half of
   DateStyle was ignored on input: `SET datestyle TO ymd` then
   `date '1/8/1999'` answered 1999-01-08 where PostgreSQL reads day
   1999 and raises. The rolling resolver was the other half of the
   problem -- SMART quietly moves 1997-04-31 to the 30th, and
   PostgreSQL rejects it."
  [^String s]
  (let [bc? (re-find #"(?i)\s+bc$" s)
        body (str/trim (str/replace s #"(?i)\s+(ad|bc)$" ""))]
    (when-let [[_ f1 f2 f3] (re-matches #"(\d{1,6})[-/](\d{1,6})[-/](\d{1,6})" body)]
      (when-let [d (decode-numeric-date f1 f2 f3 (second types/*date-style*))]
        ;; PostgreSQL's 1 BC is the proleptic year 0, 2 BC is -1, and so
        ;; on -- `2040-04-10 BC` is proleptic -2039.
        (if bc? (.withYear d (- 1 (.getYear d))) d)))))

(def ^:private trailing-zone-re
  "A numeric zone at the end of a datetime literal.

   Two spellings, and the distinction is load-bearing. Attached to a
   TIME (`12:00:00+05`) it needs no space; standing alone
   (`2000-09-07 -07`) it must have one, because `2000-01-01` ends in
   `-01` and an end-anchored pattern with no such guard eats the day and
   leaves `2000-01` -- which then parses as nothing at all."
  #"(?i)^(?:(.*\d{1,2}:\d{2}(?::\d{2})?(?:\.\d+)?)\s*|(.*\d)\s+)([+-]\d{2}(?::?\d{2})?|Z)$")

(defn split-trailing-zone
  "`[head zone-offset]` for a datetime literal that carries a numeric
   zone, else `[s nil]`.

   Splitting it out is the whole point: `timestamp without time zone`
   keeps the fields and DROPS the zone (datetime.c decodes tzp and then
   ignores it), while `timestamptz` converts by it. Reading the literal
   once and letting the target decide is what keeps the two answers from
   being parsed by different code."
  [^String s]
  (let [t (str/trim (str s))]
    (if-let [[_ after-time after-date z] (re-matches trailing-zone-re t)]
      [(str/trim (or after-time after-date))
       (if (contains? #{"Z" "z"} z)
         java.time.ZoneOffset/UTC
         (let [[_ sign hh mm] (re-matches #"([+-])(\d{2}):?(\d{2})?" z)]
           (java.time.ZoneOffset/ofHoursMinutes
            (* (if (= "-" sign) -1 1) (parse-long hh))
            (* (if (= "-" sign) -1 1) (parse-long (or mm "0"))))))]
      [t nil])))

(defn- bad-date!
  "PostgreSQL tells a date whose FIELDS are impossible (22008) from text
   that is not a date at all (22007)."
  [^String s]
  (if (date-ish-shape? s)
    (throw (ex-info (str "date/time field value out of range: \"" s "\"")
                    {:error :datetime-field-overflow :sqlstate "22008"}))
    (throw (ex-info (str "invalid input syntax for type date: \"" s "\"")
                    {:error :invalid-datetime-format :sqlstate "22007"}))))

(defn cast-scalar
  "Apply a SQL cast of `v` to the target named by `type-str`.

   Options:
     :explicit?       — an explicit CAST reshapes silently; an assignment
                        raises instead (matters for bit width coercion).
     :parse-timestamp — fn String → java.util.Date, from expr.clj.
     :resolve-regclass— fn String → oid, for `::regclass`.
     :resolve-regtype — fn String → oid, for `::regtype`.
     :prefer-local-datetime? — return a LocalDateTime (microsecond
                        precision) rather than a Date for a timestamp
                        cast. See the :timestamp branch.
     :src-oid         — the OID of the value being cast, when the caller
                        knows it. Only `::text` uses it, to tell a date
                        from a timestamp: both are java.util.Date here.

   Returns `v` unchanged for a target this doesn't classify, which is
   what every call site did before and keeps unknown types passing
   through rather than erroring."
  [v type-str {:keys [explicit? parse-timestamp resolve-regclass resolve-regtype
                      prefer-local-datetime? src-oid]
               :or {explicit? true}}]
  (if (or (nil? v) (= :__null__ v))
    v
    (let [cat (types/cast-category type-str)]
      (case cat
        :tsvector (tsearch/canonical-tsvector v)
        :tsquery (tsearch/canonical-tsquery v)

        :vector
        (let [typmod (some-> (re-find #"\(\s*(\d+)\s*\)" (str type-str))
                             second Long/parseLong)]
          (pg-vector/coerce v typmod))

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
            ;; A FLOAT source goes through PostgreSQL's own %g
            ;; conversion, not through its shortest-round-trip text --
            ;; see coerce/float->numeric.
            :numeric (let [bd (if (or (instance? Float v) (instance? Double v))
                                (or (types/double->numeric-special (double v))
                                    (coerce/float->numeric v))
                                (coerce/coerce-numeric v :bigdec))]
                       (if-let [[p sc] (numeric-typmod type-str)]
                         (apply-numeric-typmod bd p sc)
                         bd))))

        ;; money is stored through Datahike's BigDecimal carrier. Real
        ;; PostgreSQL stores an int64 count of the locale's fractional
        ;; units; with the default two fractional digits this is the same
        ;; value model at the SQL boundary. Locale-specific symbols remain
        ;; a presentation concern, while scale and OID stay faithful.
        :money
        (parse-money v)

        ;; `str` on a temporal value is java.util.Date.toString, which is
        ;; both the wrong format and rendered in the JVM's default time
        ;; zone — see types/temporal->pg-text.
        ;; `str` on a temporal value is java.util.Date.toString, which is
        ;; both the wrong format and rendered in the JVM's default time
        ;; zone — see types/temporal->pg-text. The length modifier is
        ;; applied after, since it applies to the RENDERED text.
        :internal-char (internal-char-in
                        (if (string? v) v (types/->pg-text v src-oid)))

        :text (apply-text-length
               (cond
                 (pg-bits/pg-bit? v) (pg-bits/to-pg-text v)
                 (pg-arr/array? v)   (pg-arr/to-pg-text v)
                 (string? v)         v
                 :else               (types/->pg-text v src-oid))
               type-str explicit?)

        :boolean (cond
                   (boolean? v) v
                   ;; PostgreSQL has an int -> bool cast (bool.c int4_bool):
                   ;; zero is false, anything else true. We stringified the
                   ;; number and handed it to the TEXT parser, which accepts
                   ;; only the exact tokens '1' and '0' -- so `20::bool`
                   ;; raised "invalid input syntax for type boolean: 20".
                   (integer? v) (not (zero? v))
                   :else (input/parse-bool (str v)))

        (:bit :varbit) (cast-to-bit v type-str explicit?)

        :uuid (if (instance? java.util.UUID v)
                v
                (input/parse-uuid (str v)))

        :bytes (cond
                 (bytes? v)  v
                 (string? v) (or (coerce/parse-bytea-hex v)
                                 (.getBytes ^String v "UTF-8"))
                 :else v)

        :timestamp
        (cond
          (instance? java.util.Date v) v
          (instance? java.time.LocalDateTime v) v
          ;; A NAMED zone inside the literal -- `… America/New_York`,
          ;; `… PST`. A timestamptz applies it; a plain timestamp
          ;; ignores it (datetime.c keeps the fields and drops the
          ;; zone). Neither happened: the zone was parsed away and the
          ;; wall clock kept, so `'… 17:32:01 America/New_York'` read
          ;; back as 17:32:01+00 -- five hours out, reported as
          ;; success -- and the extended spelling did not parse at all
          ;; and passed its own text through as a timestamptz.
          (and (string? v)
               (re-find #"(?i)\s[A-Za-z][A-Za-z_]*(?:/[A-Za-z_+-]+)*\s*$" (str/trim v))
               (not (re-find #"(?i)\s(AM|PM|BC|AD)\s*$" (str/trim v))))
          (let [t (str/trim (str v))
                idx (.lastIndexOf t " ")
                head (str/trim (subs t 0 idx))
                zone (str/trim (subs t (inc idx)))
                tz? (contains? #{"timestamptz" "timestamp with time zone"}
                               (types/base-type-name-of type-str))
                inner (cast-scalar head "timestamp"
                                   {:explicit? true
                                    :parse-timestamp parse-timestamp})]
            (if (or (nil? inner) (string? inner))
              ;; The head is not a timestamp either, so this is not
              ;; the shape we took it for -- hand the WHOLE value back
              ;; to the ordinary path so it reports on what was written.
              v
              (if-not tz?
                inner
                (let [^java.time.LocalDateTime ldt
                      (cond
                        (instance? java.time.LocalDateTime inner) inner
                        (instance? java.util.Date inner)
                        (java.time.LocalDateTime/ofInstant
                         (.toInstant ^java.util.Date inner)
                         java.time.ZoneOffset/UTC)
                        :else nil)]
                  (if (nil? ldt)
                    inner
                    (java.util.Date/from
                     (.toInstant
                      (.atZone ldt
                               ;; The CAST lowercases the zone in its
                               ;; message -- the datetime decoder folds
                               ;; the token -- while AT TIME ZONE keeps
                               ;; what was written. PostgreSQL really
                               ;; does differ between the two.
                               (try (types/resolve-time-zone zone)
                                    (catch Exception _
                                      (throw (errors/pg-error
                                              :invalid-parameter-value
                                              {:message
                                               (str "time zone \""
                                                    (str/lower-case zone)
                                                    "\" not recognized")}))))))))))))
          :else
          (let [tz? (contains? #{"timestamptz" "timestamp with time zone"}
                               (types/base-type-name-of type-str))
                ;; A NUMERIC offset, the sibling of the named-zone
                ;; branch above: `timestamp without time zone` keeps the
                ;; fields it was given and DROPS the zone (datetime.c
                ;; decodes tzp and then ignores it for DTK_DATE without
                ;; a zone), while timestamptz converts by it. Only the
                ;; named spelling was handled, so
                ;; `'2000-01-01 12:00:00+05'::timestamp` answered
                ;; 07:00:00 -- five hours out, and reported as success.
                ;; The zone is read ONCE and applied by the target, so
                ;; the two types cannot be decided by different code.
                [head zone] (split-trailing-zone (str v))
                ;; The head carries no zone, so parsing it as an instant
                ;; at UTC yields exactly its wall clock. That reuses the
                ;; whole accumulated spelling table rather than growing
                ;; a second one beside it.
                head-local (let [p (when parse-timestamp (parse-timestamp head))]
                             (when (instance? java.util.Date p)
                               (java.time.LocalDateTime/ofInstant
                                (.toInstant ^java.util.Date p)
                                java.time.ZoneOffset/UTC)))
                v (if zone head v)
                norm (-> (str v) str/trim
                         ;; ISO 8601 BASIC -- `19970210 173201`,
                         ;; `19970210T173201`, `19970210`. PostgreSQL
                         ;; reads the separator-less spelling; rewriting
                         ;; it to the extended one here means both this
                         ;; cast and the expression parser get it,
                         ;; rather than one of them.
                         (str/replace #"^(\d{4})(\d{2})(\d{2})([ T])(\d{2})(\d{2})(\d{2})"
                                      "$1-$2-$3T$5:$6:$7")
                         (str/replace #"^(\d{4})(\d{2})(\d{2})$" "$1-$2-$3")
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
            (or
                ;; A literal WITH a zone is finished here: timestamptz
                ;; converts by it, timestamp drops it. Only these two
                ;; answers are possible and both are decided from the
                ;; one reading above -- `'… 12:00:00 +05'` (space) and
                ;; `'… 12:00+02'` (no seconds) used to reach neither
                ;; branch and raise, and `'2000-09-07 -07'::timestamptz`
                ;; answered midnight UTC instead of 07:00.
             (when (and zone head-local)
               (if tz?
                 (java.util.Date/from (.toInstant ^java.time.LocalDateTime head-local
                                                  ^java.time.ZoneOffset zone))
                 (if prefer-local-datetime?
                   head-local
                   (java.util.Date/from
                    (.toInstant ^java.time.LocalDateTime head-local
                                java.time.ZoneOffset/UTC)))))
             ldt
                ;; `norm` was computed and then only consulted on the
                ;; branch above, so a spelling this normalises but the
                ;; injected parser does not know -- ISO basic -- fell
                ;; through to the raw input and a timestamp column got
                ;; a string that is not a timestamp.
             (let [p (when parse-timestamp (parse-timestamp (str v)))]
               (when-not (or (nil? p) (string? p)) p))
             (try (java.time.LocalDateTime/parse norm)
                  (catch Exception _ nil))
             (let [p (when parse-timestamp (parse-timestamp (str v)))]
               (when-not (or (nil? p) (string? p)) p))
                ;; Nothing parsed. Returning `v` here is what made
                ;; `'2024-02-30'::timestamp` answer with its own TEXT
                ;; and `'nonsense'::timestamp` likewise -- a value that
                ;; is not a timestamp, indistinguishable from one that
                ;; is. PostgreSQL raises, and distinguishes impossible
                ;; FIELDS (22008) from text that is not a timestamp at
                ;; all (22007).
             (bad-timestamp! (str v) tz?))))

        :date (cond
                (instance? java.time.LocalDate v) v
                (instance? java.util.Date v)
                (-> ^java.util.Date v .toInstant
                    (.atZone java.time.ZoneOffset/UTC) .toLocalDate)
                (instance? java.time.LocalDateTime v)
                (.toLocalDate ^java.time.LocalDateTime v)
                :else
                (let [s (str/trim (str v))]
                  (if (re-matches ymd-shape s)
                    ;; A bare `y-m-d` is decided HERE and nowhere else.
                    ;; Falling through to the timestamp parser is what
                    ;; rolled 1997-04-31 to the 30th and 1997-02-29 to
                    ;; the 28th: it is lenient, and PostgreSQL is not.
                    (or (parse-date-strict s) (bad-date! s))
                    (or (try (java.time.LocalDate/parse (first (str/split s #"[ T]")))
                             (catch Exception _ nil))
                        (when parse-timestamp
                          (let [d (parse-timestamp s)]
                            (when (instance? java.util.Date d)
                              (-> ^java.util.Date d .toInstant
                                  (.atZone java.time.ZoneOffset/UTC) .toLocalDate))))
                        ;; Passing the text through was a silent wrong
                        ;; answer: `'1997-13-01'::date` answered the
                        ;; string `1997-13-01`, a thirteenth month.
                        (bad-date! s)))))

        :time (let [timetz? (contains? #{"timetz" "time with time zone"}
                                       (types/base-type-name-of type-str))
                    local (cond
                            (instance? java.time.OffsetTime v)
                            (if timetz? v (.toLocalTime ^java.time.OffsetTime v))
                            (instance? java.time.LocalTime v) v
                            (instance? java.time.LocalDateTime v)
                            (.toLocalTime ^java.time.LocalDateTime v)
                            (instance? java.util.Date v)
                            (-> ^java.util.Date v .toInstant
                                (.atZone java.time.ZoneOffset/UTC) .toLocalTime)
                            :else (parse-time-input (str v) timetz?))]
                (if (and timetz? (instance? java.time.LocalTime local))
                  (java.time.OffsetTime/of ^java.time.LocalTime local java.time.ZoneOffset/UTC)
                  local))

        ;; Not a width-classified category — the OID-name types.
        (cond
          (= type-str "regnamespace") 2200
          (= type-str "regclass") (if resolve-regclass (resolve-regclass (str v)) v)
          (= type-str "regtype") (if resolve-regtype (resolve-regtype (str v)) v)
          :else v)))))
