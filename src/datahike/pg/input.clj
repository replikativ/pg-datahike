(ns datahike.pg.input
  "PostgreSQL type input functions (pg_type.typinput) for the scalar types
   whose text form has one meaning: bool, the integers, oid, float4/float8,
   numeric and uuid. Ported from boolin (bool.c), pg_strtoint16/32/64_safe
   and uint32in_subr (numutils.c), float4in/float8in (float.c), numeric_in
   and uuid_in.

   Every path that turns text into one of these types -- an untyped
   literal meeting a typed operand, a cast from text or unknown, a text
   parameter (PgParamCodec calls in), an array element, COPY -- goes
   through `parse`. Invalid
   input raises 22P02 and out-of-range input 22003, with PostgreSQL's
   messages; nothing returns the text it could not parse.

   Carriers: bool -> Boolean; int2/int4/int8/oid -> Long; float4 -> Float;
   float8 -> Double; numeric -> BigDecimal (or the NaN/Infinity carrier);
   uuid -> java.util.UUID."
  (:refer-clojure :exclude [parse-uuid])
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.types :as types])
  (:import [datahike.pg PgParamCodec PgWireServer$PgProtocolException]))

(defn- invalid [type-name ^String s]
  (throw (errors/pg-error :invalid-text-representation
                          {:type type-name :value s})))

(defn- out-of-range [type-name ^String s]
  (throw (errors/pg-error :numeric-value-out-of-range
                          {:message (str "value \"" s "\" is out of range for type " type-name)})))

(defn- space? [c]
  ;; C isspace in the "C" locale
  (contains? #{\space \tab \newline \return \u000B \formfeed} c))

(defn- c-trim
  "`s` without leading and trailing C-locale whitespace."
  ^String [^String s]
  (let [n (.length s)
        b (loop [i 0] (if (and (< i n) (space? (.charAt s i))) (recur (inc i)) i))
        e (loop [j n] (if (and (> j b) (space? (.charAt s (dec j)))) (recur (dec j)) j))]
    (subs s b e)))

;; ---------------------------------------------------------------------------
;; bool

(defn parse-bool
  "boolin: surrounding whitespace ignored; any prefix of true/false/yes/no,
   on/off (at least two characters: 'o' alone is ambiguous), 1 or 0, case
   insensitive."
  [^String s]
  (let [v (.toLowerCase (c-trim s) java.util.Locale/ROOT)
        n (.length v)
        prefix-of? (fn [^String word min-len]
                     (and (>= n min-len) (<= n (.length word)) (.startsWith word v)))]
    (cond
      (or (prefix-of? "true" 1) (prefix-of? "yes" 1) (prefix-of? "on" 2)) true
      (or (prefix-of? "false" 1) (prefix-of? "no" 1) (prefix-of? "off" 2)) false
      (= v "1") true
      (= v "0") false
      :else (invalid "boolean" s))))

;; ---------------------------------------------------------------------------
;; integers

(defn- ascii-digit
  "The value of `c` as a digit in `radix`, ASCII only (C's isdigit and
   isxdigit), or -1."
  ^long [c ^long radix]
  (let [d (cond (<= (int \0) (int c) (int \9)) (- (int c) (int \0))
                (<= (int \a) (int c) (int \f)) (+ 10 (- (int c) (int \a)))
                (<= (int \A) (int c) (int \F)) (+ 10 (- (int c) (int \A)))
                :else -1)]
    (if (< d radix) d -1)))

(defn- parse-integer
  "pg_strtoint{16,32,64}_safe: surrounding whitespace, a sign, then decimal
   digits or a 0x/0o/0b prefix; `_` may separate digits. Range-checked
   against [lo, hi]."
  [^String s type-name lo hi]
  (let [n (.length s)
        skip-space (fn [i] (loop [i i] (if (and (< i n) (space? (.charAt s i))) (recur (inc i)) i)))
        i (skip-space 0)
        [minus? i] (cond (and (< i n) (= \- (.charAt s i))) [true (inc i)]
                         (and (< i n) (= \+ (.charAt s i))) [false (inc i)]
                         :else [false i])
        prefix (when (and (< (inc i) n) (= \0 (.charAt s i)))
                 (case (.charAt s (inc i)) (\x \X) 16 (\o \O) 8 (\b \B) 2 nil))
        radix (or prefix 10)
        start (if prefix (+ i 2) i)
        limit (+ (biginteger hi) 1)
        [acc end] (loop [j start, acc 0N]
                    (if (>= j n)
                      [acc j]
                      (let [c (.charAt s j)
                            d (ascii-digit c radix)]
                        (cond
                          (>= d 0)
                          (let [acc (+ (* acc radix) d)]
                            (if (> acc limit) (out-of-range type-name s) (recur (inc j) acc)))
                          (= \_ c)
                          (let [nxt (inc j)]
                            (if (or (and (= radix 10) (= j start))
                                    (>= nxt n)
                                    (neg? (ascii-digit (.charAt s nxt) radix)))
                              (invalid type-name s)
                              (recur nxt acc)))
                          :else [acc j]))))]
    (when (= end start) (invalid type-name s))
    (when-not (= n (skip-space end)) (invalid type-name s))
    (let [v (if minus? (- acc) acc)]
      (if (or (< v lo) (> v hi))
        (out-of-range type-name s)
        (long v)))))

(defn parse-int2 [s] (parse-integer s "smallint" Short/MIN_VALUE Short/MAX_VALUE))
(defn parse-int4 [s] (parse-integer s "integer" Integer/MIN_VALUE Integer/MAX_VALUE))
(defn parse-int8 [s] (parse-integer s "bigint" Long/MIN_VALUE Long/MAX_VALUE))

(defn parse-oid
  "oidin (uint32in_subr): strtoul with base 0 -- 0x hex, a leading 0 for
   octal -- and surrounding whitespace. A minus sign is accepted for
   backward compatibility when the negated value fits int32, and wraps
   into uint32: `-1` is 4294967295."
  [^String s]
  (let [t (c-trim s)
        [minus? body] (case (first t) \- [true (subs t 1)] \+ [false (subs t 1)] [false t])
        [radix digits] (cond (re-matches #"0[xX][0-9a-fA-F]+" body) [16 (subs body 2)]
                             (re-matches #"0[0-7]*" body) [8 body]
                             (re-matches #"[1-9][0-9]*" body) [10 body]
                             :else (invalid "oid" s))
        v (BigInteger. ^String digits (int radix))]
    (cond
      (and minus? (<= v 2147483648)) (bit-and (- (long v)) 0xFFFFFFFF)
      (and (not minus?) (<= v 4294967295)) (long v)
      :else (out-of-range "oid" s))))

;; ---------------------------------------------------------------------------
;; floats

(def ^:private decimal-float
  #"[+-]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?")

(def ^:private hex-float
  #"([+-]?)0[xX]((?:[0-9a-fA-F]+\.?[0-9a-fA-F]*|\.[0-9a-fA-F]+))(?:[pP]([+-]?[0-9]+))?")

(def ^:private special-float
  #"(?i)([+-]?)(inf|infinity|nan)")

(defn- strtod
  "glibc strtod over the whole (already trimmed) string: decimal, hex with
   an optional binary exponent, inf/infinity/nan; nil when not a number."
  [^String t]
  (cond
    (re-matches decimal-float t) (Double/parseDouble t)
    (re-matches hex-float t)
    (let [[_ sign mant exp] (re-matches hex-float t)]
      (Double/parseDouble (str sign "0x" mant "p" (or exp "0"))))
    (re-matches special-float t)
    (let [[_ sign word] (re-matches special-float t)]
      (if (= "nan" (.toLowerCase ^String word))
        Double/NaN
        (if (= "-" sign) Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY)))))

(defn- underflow?
  "Did a nonzero mantissa round to zero? strtod's ERANGE on underflow."
  [^String t v]
  (and (zero? v)
       (let [hex? (re-find #"^[+-]?0[xX]" t)
             mantissa (if hex?
                        (str/replace-first (first (str/split t #"[pP]")) #"^[+-]?0[xX]" "")
                        (first (str/split t #"[eE]")))]
         (boolean (re-find (if hex? #"[1-9a-fA-F]" #"[1-9]") mantissa)))))

(defn parse-float8
  "float8in: strtod over the trimmed input; out of range (overflow to
   infinity, or underflow to zero) raises 22003."
  [^String s]
  (let [t (c-trim s)
        v (when (seq t) (strtod t))]
    (cond
      (nil? v) (invalid "double precision" s)
      (or (and (Double/isInfinite v) (not (re-find #"(?i)inf" t)))
          (underflow? t v))
      (throw (errors/pg-error :numeric-value-out-of-range
                              {:message (str "\"" t "\" is out of range for type double precision")}))
      :else v)))

(defn parse-float4
  "float4in: parsed as a double (strtod), then range-checked for real."
  [^String s]
  (let [t (c-trim s)
        v (when (seq t) (strtod t))]
    (cond
      (nil? v) (invalid "real" s)
      (or (Double/isNaN v) (Double/isInfinite v)) (unchecked-float v)
      :else
      (let [f (unchecked-float v)]
        (if (or (Float/isInfinite f) (and (zero? f) (not (zero? v))) (underflow? t v))
          (throw (errors/pg-error :numeric-value-out-of-range
                                  {:message (str "\"" t "\" is out of range for type real")}))
          f)))))

;; ---------------------------------------------------------------------------
;; uuid

(defn parse-uuid
  "uuid_in: 32 hex digits, optionally in braces, with an optional hyphen
   after any group of four; no surrounding whitespace."
  [^String s]
  (let [m (re-matches #"(\{)?((?:[0-9a-fA-F]{4}-?){7}[0-9a-fA-F]{4})(\})?" s)]
    (if (and m (= (boolean (nth m 1)) (boolean (nth m 3))))
      (let [hex (.replace ^String (nth m 2) "-" "")]
        (java.util.UUID. (.longValue (BigInteger. (subs hex 0 16) 16))
                         (.longValue (BigInteger. (subs hex 16) 16))))
      (invalid "uuid" s))))

;; ---------------------------------------------------------------------------

(def ^:private parsers
  {types/oid-bool parse-bool
   types/oid-int2 parse-int2
   types/oid-int4 parse-int4
   types/oid-int8 parse-int8
   types/oid-oid parse-oid
   types/oid-float4 parse-float4
   types/oid-float8 parse-float8
   ;; numeric_in: coerce-numeric already matches it, NaN/Infinity,
   ;; underscores and 0x/0o/0b integers included.
   types/oid-numeric #(coerce/coerce-numeric % :bigdec)
   types/oid-uuid parse-uuid})

(defn parser
  "The input function for `oid`, or nil when that type's input is not
   handled here."
  [oid]
  (get parsers oid))

(defn parse
  "Read `s` as a value of type `oid` with PostgreSQL's input function,
   raising its error on invalid input. nil for nil."
  [oid ^String s]
  (when (some? s)
    ((or (parser oid)
         (throw (ex-info "no input function" {:oid oid})))
     s)))

;; Text-format Bind parameters are read by the same functions. The wire
;; layer needs the SQLSTATE on a PgProtocolException; anything else it
;; reports as XX000.
(PgParamCodec/setTextInput
 (reify java.util.function.BiFunction
   (apply [_ oid s]
     (try (parse (long oid) s)
          (catch clojure.lang.ExceptionInfo e
            (let [[sqlstate message] (errors/classify-exception e)]
              (throw (PgWireServer$PgProtocolException. sqlstate message))))))))
