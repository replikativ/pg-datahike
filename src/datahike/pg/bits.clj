(ns datahike.pg.bits
  "PostgreSQL BIT / BIT VARYING values.

   Before this namespace a bit value was a bare `java.lang.String` of
   '0'/'1' characters. The digits were right, but nothing else was: the
   type reported as `text` (OID 25) rather than `bit` (1560), so
   `pg_typeof(0::bit)` answered `text` and clients that consult the
   RowDescription OID mapped the column wrong (issue #19).

   A wrapper record — the same shape `PgArray` and `PgRecord` already
   use — carries the two things a string cannot:

   1. **The WIDTH, which is part of the value.** PG compares bit strings
      with a memcmp of the left-aligned bytes and then breaks ties on
      length (varbit.c:817), so `B'101'` and `B'10100000'` are NOT equal
      and `B'0' < B'00' < B'000'`. Any representation that trims or
      normalises the digits silently changes equality, ordering and
      index lookups.

   2. **bit vs bit varying**, which are distinct types (1560 / 1562) with
      different coercion rules: `bit(n)` zero-pads on the RIGHT to
      exactly n, `bit varying(n)` truncates but never pads.

   The digit string stays the storage form because it is exactly PG's
   text output format (varbit.c:586 always emits `bitlen` '0'/'1'
   characters — never hex, never a `B` prefix), and because `bits`
   already encodes the width in its length.

   Ordering note: comparing the digit strings lexicographically and then
   by length gives the same answer as PG's memcmp-then-length, because
   the zero pad PG compares against behaves like trailing '0' characters
   and the tie-break covers the proper-prefix case."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]))

(set! *warn-on-reflection* true)

(defrecord PgBit [bits varying?])

(defn pg-bit?
  "True iff v is a PgBit."
  [v]
  (instance? PgBit v))

(defn make-bit
  "Construct a bit-string value from a '0'/'1' digit string."
  ([bits] (make-bit bits false))
  ([bits varying?] (->PgBit bits (boolean varying?))))

(defn width
  "Number of bits — PG's `length()` / `bit_length()`."
  [^PgBit b]
  (count (:bits b)))

(defn octet-length
  "PG's `octet_length` on a bit string: ceil(width / 8)."
  [^PgBit b]
  (quot (+ (width b) 7) 8))

(defn to-pg-text
  "PG text output: the digit run itself, exactly `width` characters."
  [^PgBit b]
  (:bits b))

;; ---------------------------------------------------------------------------
;; Parsing

(defn- invalid-digit [s kind]
  (throw (errors/pg-error :invalid-text-representation
                          {:detail (str "\"" s "\" is not a valid " kind " digit")})))

(defn parse-bit-literal
  "Parse the text form of a bit value.

   Accepts what `bit_in` accepts (varbit.c:165): a `b`/`B` prefix, an
   `x`/`X` prefix for hex, or a bare digit run. Hex expands to exactly
   FOUR bits per digit including leading zeros — `X'1F'` is the 8-bit
   value `00011111`, not 5 bits — which is the detail a text-backed
   implementation gets wrong (it changes `length()`, `octet_length()`
   and the sort position).

   Raises 22P02 with PG's wording on a bad character."
  ([s] (parse-bit-literal s false))
  ([^String s varying?]
   (let [c0 (when (pos? (count s)) (.charAt s 0))
         hex? (or (= c0 \x) (= c0 \X))
         body (if (or hex? (= c0 \b) (= c0 \B)) (subs s 1) s)]
     (make-bit
      (if hex?
        (str/join
         (map (fn [ch]
                (let [d (Character/digit ^char ch 16)]
                  (when (neg? d) (invalid-digit (str ch) "hexadecimal"))
                  ;; 4 bits per hex digit, leading zeros preserved.
                  (str/replace (format "%4s" (Integer/toBinaryString d)) \space \0)))
              body))
        (do (doseq [ch body]
              (when-not (or (= ch \0) (= ch \1))
                (invalid-digit (str ch) "binary")))
            body))
      varying?))))

;; ---------------------------------------------------------------------------
;; Width coercion

(defn coerce-width
  "Apply a `bit(n)` / `bit varying(n)` width to a value.

   `bit` zero-pads or truncates on the RIGHT to exactly n (varbit.c:390);
   `bit varying` truncates on the right but NEVER pads, so a shorter
   value passes through unchanged (varbit.c:751). Padding on the left,
   or padding a varbit, silently corrupts the value.

   `explicit?` distinguishes a cast from an assignment: PG only reshapes
   silently for an explicit cast and otherwise raises 22026 / 22001."
  [^PgBit b n explicit?]
  (if (or (nil? n) (<= (long n) 0) (= (long n) (width b)))
    b
    (let [n (long n)
          w (width b)
          varying? (:varying? b)]
      (cond
        (and varying? (< w n)) b            ; varbit never pads

        (not explicit?)
        (throw (if varying?
                 (errors/pg-error
                  :string-data-right-truncation
                  {:detail (str "bit string too long for type bit varying(" n ")")})
                 (errors/pg-error
                  :string-data-length-mismatch
                  {:detail (str "bit string length " w
                                " does not match type bit(" n ")")})))

        (> w n) (assoc b :bits (subs (:bits b) 0 n))
        :else   (assoc b :bits (str (:bits b)
                                    (str/join (repeat (- n w) \0))))))))

;; ---------------------------------------------------------------------------
;; Integer conversions

(defn from-integer
  "PG's `int -> bit(n)`: keep the RIGHTMOST n bits, sign-extending on the
   left when n exceeds the source width (varbit.c:1550).

   `29::bit(4)` is `1101` — the low nibble — and `(-44)::bit(12)` is
   `111111010100`. Rendering the integer's binary string and padding on
   the wrong end gives wrong bits for every negative value. This never
   errors; high bits are silently discarded."
  [^long v ^long n]
  (let [n (if (<= n 0) 1 n)
        two-c (if (>= n 64)
                ;; Wider than the source: sign-extend.
                (let [s (Long/toBinaryString v)          ; already 64-bit two's complement for v<0
                      s (if (neg? v) s (str/replace (format (str "%" 64 "s") s) \space \0))]
                  (str (str/join (repeat (- n 64) (if (neg? v) \1 \0))) s))
                (let [mask (if (= n 64) -1 (dec (bit-shift-left 1 n)))
                      low (bit-and v mask)
                      s (Long/toBinaryString low)]
                  (if (< (count s) n)
                    (str (str/join (repeat (- n (count s)) \0)) s)
                    (subs s (- (count s) n)))))]
    (make-bit two-c)))

(defn to-long
  "PG's `bit -> int8`: a REINTERPRETATION of the bits, not a range check.
   32 one-bits cast to int4 give -1, not 4294967295 (varbit.c:1598).
   Only the width is validated."
  [^PgBit b]
  (let [w (width b)]
    (when (> w 64)
      (throw (errors/pg-error :numeric-value-out-of-range
                              {:detail "bigint out of range"})))
    (if (zero? w)
      0
      (let [u (BigInteger. ^String (:bits b) 2)]
        ;; Two's-complement reinterpretation at the value's own width.
        (if (and (= w 64) (= \1 (.charAt ^String (:bits b) 0)))
          (.longValue (.subtract u (.shiftLeft BigInteger/ONE 64)))
          (.longValue u))))))

(defn compare-bits
  "PG's bit_cmp ordering: content first, then width."
  [^PgBit a ^PgBit b]
  (let [c (compare (:bits a) (:bits b))]
    (if (zero? c) (compare (width a) (width b)) c)))
