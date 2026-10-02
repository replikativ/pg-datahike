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
            [datahike.pg.datetime.carrier :as carrier]
            [datahike.pg.datetime.in :as in]
            [datahike.pg.datetime.zone :as zone]
            [datahike.pg.bits :as pg-bits]
            [datahike.pg.errors :as errors]
            [datahike.pg.geo :as geo]
            [datahike.pg.mac :as mac]
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

(defn datetime-ctx
  "What the ported parser reads from session state.

   `:now` is deliberately ABSENT, so the four clock-reading tokens --
   `now`, `today`, `tomorrow`, `yesterday` -- fail as unparseable.
   That is what this path needs and what the server already did: their
   value is the STATEMENT's, and a cast is constant-folded into a
   cached plan, so folding one would freeze it for that plan's life.
   Supporting them needs the deferred treatment `now()` gets, and the
   parser is ready for it -- pass a `:now` and they work.

   `:session-zone` is UTC because the server's TimeZone setting is
   (expr.clj:1613). When that becomes settable, this is where it
   reads from."
  []
  {:date-order (second types/*date-style*)
   :session-zone zone/utc})

(defn- parse-time-input
  "PostgreSQL `time_in` / `timetz_in`, via the ported parser.

   Everything this used to spell as a regex -- the optional leading
   date, AM/PM, the zone, `allballs`, 24:00:00 -- is now
   `DecodeTimeOnly`'s business, and so are the forms the regex never
   had: `040506`, `0405`, `now`, a text month refused, a weekday
   refused."
  [^String input timetz?]
  (let [ctx (datetime-ctx)]
    (if timetz?
      (carrier/->timetz (in/timetz-in input ctx))
      (carrier/->time (in/time-in input ctx)))))

(defn- parse-date-strict
  "PostgreSQL `date_in`, via the ported parser. Returns a `LocalDate`
   or an infinity sentinel, and RAISES rather than returning nil --
   there is no longer a fallback for it to fall through to.

   It used to be a three-field digit match plus a separate tail
   validator, which between them could not read a text
   month, a Julian day, a day-of-year, a run-together number, or the
   `Postgres` output style that PostgreSQL's own regression files are
   written in."
  [^String s]
  (carrier/->date (in/date-in s (datetime-ctx))))

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

(defn cast-scalar
  "Apply a SQL cast of `v` to the target named by `type-str`.

   Options:
     :explicit?       — an explicit CAST reshapes silently; an assignment
                        raises instead (matters for bit width coercion).
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
  ;; `:parse-timestamp` is still accepted and IGNORED. Every caller
  ;; passes it, and the temporal arms now use the ported parser, so
  ;; destructuring it would only invite someone to wire it back in.
  ;; Dropping it from the ~20 call sites is cleanup, not behaviour.
  [v type-str {:keys [explicit? resolve-regclass resolve-regtype
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

        ;; A geometric value is held as its canonical text. Without
        ;; this the cast fell through to the default and handed back
        ;; whatever was written: `'garbage'::point` answered `garbage`.
        :geometric
        (geo/geometric-in (types/base-type-name-of type-str) v)

        ;; A MAC address is held as its canonical text, so the cast is
        ;; the input function. macaddr8 <-> macaddr is a real conversion
        ;; rather than a re-parse: widening inserts ff:fe and narrowing
        ;; is only defined when they are there.
        :mac
        (let [tname (types/base-type-name-of type-str)
              from (when (string? v) v)]
          (cond
            (nil? v) nil
            (and (= "macaddr" tname) (mac/macaddr8-bytes from)
                 (not (mac/macaddr-bytes from)))
            (mac/mac8->mac from)
            :else (mac/mac-in tname v)))

        ;; As for `:date`: the non-string branches are carrier
        ;; conversions and every string spelling goes to the ported
        ;; `timestamp_in` / `timestamptz_in`.
        ;;
        ;; What this replaces was four parsers consulted in turn --
        ;; a trailing-zone regex, a named-zone peeler, `LocalDateTime/parse`
        ;; and an injected `parse-timestamp` -- each of which could
        ;; return nil and hand on to the next, with the LAST one
        ;; passing the input text through unparsed. A literal that none
        ;; of them read came back as a string typed as a timestamp,
        ;; which is the passthrough-on-failure shape: "did not parse"
        ;; and "parsed" were the same value.
        :timestamp
        (let [tz? (contains? #{"timestamptz" "timestamp with time zone"}
                             (types/base-type-name-of type-str))]
          (cond
            (instance? java.util.Date v) v
            (instance? java.time.LocalDateTime v) v
            (nil? v) nil
            :else
            (let [r ((if tz? in/timestamptz-in in/timestamp-in)
                     (str v) (datetime-ctx))]
              ;; A timestamptz may not be a bare `LocalDateTime`: the
              ;; renderer needs to know to print the `+00`. It used to
              ;; become a `java.util.Date` for that, at the cost of
              ;; microseconds -- `OffsetDateTime` carries both, and
              ;; `types/->pg-text` already renders one.
              ;;
              ;; Gated on `prefer-local-datetime?` like the plain
              ;; timestamp beside it, because the other callers feed
              ;; values into stores that expect a Date.
              (cond
                (not prefer-local-datetime?) (carrier/->date-value r)
                tz? (carrier/->offset-datetime r)
                :else (carrier/->local-datetime r)))))

        ;; Every string spelling goes to `date_in`. The branches above
        ;; it are carrier conversions, not parsing -- a value that is
        ;; already a date needs no decoder.
        :date (cond
                ;; `infinity` survives a cast between temporal types: it
                ;; is a VALUE of date, timestamp and timestamptz alike,
                ;; not a particular instant. It is carried as the
                ;; extreme java.util.Date, so every arm below that reads
                ;; `.toInstant` turned it into year 292278994 --
                ;; `'infinity'::timestamp::date` answered
                ;; `292278994-08-17`, a plausible date and a wrong one.
                (types/infinite-datetime v) v
                (instance? java.time.LocalDate v) v
                (instance? java.util.Date v)
                (-> ^java.util.Date v .toInstant
                    (.atZone java.time.ZoneOffset/UTC) .toLocalDate)
                (instance? java.time.LocalDateTime v)
                (.toLocalDate ^java.time.LocalDateTime v)
                :else (parse-date-strict (str/trim (str v))))

        :time (let [timetz? (contains? #{"timetz" "time with time zone"}
                                       (types/base-type-name-of type-str))
                    local (cond
                            ;; `'infinity'::timestamp::time` is NULL in
                            ;; PostgreSQL -- a time has no infinity, so
                            ;; there is nothing to carry. Reading the
                            ;; sentinel's instant gave `07:12:55.807`,
                            ;; the time-of-day of Long/MAX_VALUE.
                            (types/infinite-datetime v) ::infinite
                            (instance? java.time.OffsetTime v)
                            (if timetz? v (.toLocalTime ^java.time.OffsetTime v))
                            (instance? java.time.LocalTime v) v
                            (instance? java.time.LocalDateTime v)
                            (.toLocalTime ^java.time.LocalDateTime v)
                            (instance? java.util.Date v)
                            (-> ^java.util.Date v .toInstant
                                (.atZone java.time.ZoneOffset/UTC) .toLocalTime)
                            :else (parse-time-input (str v) timetz?))]
                (cond
                  (= ::infinite local) :__null__
                  (and timetz? (instance? java.time.LocalTime local))
                  (java.time.OffsetTime/of ^java.time.LocalTime local java.time.ZoneOffset/UTC)
                  :else local))

        ;; Not a width-classified category — the OID-name types.
        (cond
          (= type-str "regnamespace") 2200
          (= type-str "regclass") (if resolve-regclass (resolve-regclass (str v)) v)
          (= type-str "regtype") (if resolve-regtype (resolve-regtype (str v)) v)
          :else v)))))
