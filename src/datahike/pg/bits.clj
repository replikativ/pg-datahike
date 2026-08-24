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

(defrecord PgBit [bits varying?]
  ;; PG orders bit strings by a memcmp of the left-aligned bytes with a
  ;; zero pad, then breaks ties on length (varbit.c:817). Comparing the
  ;; digit strings lexicographically gives the same answer — the zero
  ;; pad behaves exactly like trailing '0' characters — and the length
  ;; tie-break covers the proper-prefix case, so `B'0' < B'00'`.
  ;;
  ;; Implemented on the record itself because every ordering path
  ;; (ORDER BY, <, >, BETWEEN, min/max) reaches values through
  ;; java.lang.Comparable; without it a bit value in a comparison threw
  ;; "PgBit cannot be cast to java.lang.Comparable".
  Comparable
  (compareTo [_ other]
    (let [^PgBit o other
          c (compare bits (:bits o))]
      (if (zero? c)
        (compare (count bits) (count (:bits o)))
        c))))

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

(defn- same-size!
  "PG refuses bitwise operations on bit strings of different widths
   (varbit.c:1230) rather than padding — the width is part of the value,
   so padding would silently change one operand."
  [op ^PgBit a ^PgBit b]
  (when-not (= (count (:bits a)) (count (:bits b)))
    ;; 22026, not the 22001 the width-coercion path uses — PG separates
    ;; "these two operands don't line up" from "this value is too long
    ;; for its declared width".
    (throw (errors/pg-error
            :string-data-length-mismatch
            {:detail (str "cannot " op " bit strings of different sizes")}))))

(defn- zipwith-bits
  "Combine two equal-width bit strings position by position.

   The result is always `bit`, never `bit varying`: PG declares
   `&`/`|`/`#`/`~`/`<<`/`>>` only on bit, with a bit result, and a varbit
   operand reaches them through the implicit binary cast — so
   `pg_typeof(varbit & varbit)` is `bit`. (`||` is the mirror image: it
   is declared only on varbit and always returns varbit.)"
  [f ^PgBit a ^PgBit b]
  (make-bit (apply str (map (fn [x y] (if (f (= x \1) (= y \1)) \1 \0))
                            (:bits a) (:bits b)))
            false))

(defn and-bits
  "`&` — bitwise AND of two equal-width bit strings."
  [a b] (same-size! "AND" a b) (zipwith-bits #(and %1 %2) a b))

(defn or-bits
  "`|` — bitwise OR of two equal-width bit strings."
  [a b] (same-size! "OR" a b) (zipwith-bits #(or %1 %2) a b))

(defn xor-bits
  "`#` — bitwise XOR of two equal-width bit strings."
  [a b] (same-size! "XOR" a b) (zipwith-bits not= a b))

(defn not-bits
  "`~` — bitwise NOT. Width is preserved; the result is `bit` (see
   zipwith-bits on why never varbit)."
  [^PgBit b]
  (make-bit (apply str (map {\0 \1, \1 \0} (:bits b))) false))

(defn shift-bits
  "`<<` / `>>` on a bit string — shift WITHIN the existing width, zero
   filling (varbit.c:1310). The result is the same width as the input,
   so `B'1100' << 1` is `1000`, not `11000`. A shift at least as wide as
   the value yields all zeros. A negative distance shifts the other way,
   as PG's does."
  [^PgBit b n]
  (let [w (count (:bits b))
        n (long n)]
    (make-bit
     (cond
       (>= (Math/abs n) w) (apply str (repeat w \0))
       (pos? n) (str (subs (:bits b) n) (apply str (repeat n \0)))
       (neg? n) (let [n (- n)]
                  (str (apply str (repeat n \0)) (subs (:bits b) 0 (- w n))))
       :else (:bits b))
     false)))

(defn concat-bits
  "`||` on two bit strings — PG's `bitcat` (varbit.c:1180).

   The result is always `bit varying`, whatever the inputs were: the
   widths add up, so no fixed-width type could describe it."
  [^PgBit a ^PgBit b]
  (make-bit (str (:bits a) (:bits b)) true))

(defn substring-bits
  "PostgreSQL `substring(bit, start [, length])`.

   The requested window is 1-based and may begin before or end after the
   value; only its overlap is returned.  A negative length is rejected.
   PostgreSQL declares the result as `bit`, even when the input reached the
   function through the implicit varbit-to-bit cast."
  ([b start] (substring-bits b start nil))
  ([^PgBit b start length]
   (let [digits (:bits b)
         n (count digits)
         start (long start)
         _ (when (and (some? length) (neg? (long length)))
             (throw (ex-info "negative substring length not allowed"
                             {:error :invalid-parameter-value
                              :message "negative substring length not allowed"})))
         to (if (some? length) (+ start (long length)) (inc n))
         lo (max 1 start)
         hi (min (inc n) to)]
     (make-bit (if (<= hi lo) "" (subs digits (dec lo) (dec hi))) false))))

(defn position-bits
  "One-based position of `needle` in `haystack`, or zero when absent.

   PostgreSQL's bitposition has one deliberate difference from Java/text
   search: an empty bit string contains no match, even for an empty needle."
  [^PgBit haystack ^PgBit needle]
  (if (zero? (width haystack))
    0
    (let [idx (.indexOf ^String (:bits haystack) ^String (:bits needle))]
      (if (neg? idx) 0 (inc idx)))))

(defn overlay-bits
  "PostgreSQL `overlay(bit placing bit from start [for length])`.

   This is the SQL-standard definition: prefix before `start`, replacement,
   then the suffix after the replaced window.  Starting beyond the end
   appends immediately (there is no zero fill)."
  ([target replacement start]
   (overlay-bits target replacement start (width replacement)))
  ([^PgBit target ^PgBit replacement start length]
   (let [start (long start)
         length (long length)
         _ (when (or (not (pos? start)) (neg? length))
             (throw (ex-info "negative substring length not allowed"
                             {:error :invalid-parameter-value
                              :message "negative substring length not allowed"})))
         digits (:bits target)
         n (count digits)
         prefix-end (min n (dec start))
         suffix-start (min n (+ (dec start) length))]
     (make-bit (str (subs digits 0 prefix-end)
                    (:bits replacement)
                    (subs digits suffix-start))
               false))))

;; ---------------------------------------------------------------------------
;; SQL literals
;;
;; `B'1001000'` and `X'4A'` are the SQL standard's bit-string literals.
;; PG types both as `bit` (1560) — `SELECT B'1001000'` describes as bit
;; and `pg_typeof` answers `bit`. We used to hand the digits back as a
;; bare String, i.e. text (25), which is issue #28 and also silently
;; dropped the width that makes bit comparison and ordering correct.
;;
;; These live here rather than in the expression translator because
;; oid-infer needs the same predicate to type the column at Describe
;; time, and it cannot depend on the translator (the translator already
;; depends on it).

(def ^:private quoted-hex-bit-literal-re
  ;; The quoted form only. JSqlParser also produces HexValue for the
  ;; `0x4A` spelling, which PostgreSQL does not accept at all, so
  ;; matching it here would invent syntax rather than mirror PG. Match
  ;; malformed quoted bodies too so they reach bit_in-style validation
  ;; and report the offending digit instead of "HexValue unsupported".
  #"(?is)^x'.*'$")

(defn bit-string-literal?
  "True for a SQL bit-string literal — `B'1001000'` or `X'4A'`.

   JSqlParser spells the two differently: `B'…'` is a StringValue
   carrying prefix \"B\", `X'…'` is a HexValue."
  [expr]
  (or (and (instance? net.sf.jsqlparser.expression.StringValue expr)
           (let [p (.getPrefix ^net.sf.jsqlparser.expression.StringValue expr)]
             (and p (.equalsIgnoreCase ^String p "B"))))
      (and (instance? net.sf.jsqlparser.expression.HexValue expr)
           (some? (re-matches
                   quoted-hex-bit-literal-re
                   (str (.getValue ^net.sf.jsqlparser.expression.HexValue expr)))))))

(defn bit-string-literal-value
  "The PgBit for a literal accepted by `bit-string-literal?`.

   Both spellings produce type `bit`, not `bit varying`: PG's grammar
   builds a BitString constant that `bit_in` types as bit, so the width
   is exactly what was written. Hex expands to four bits per digit,
   leading zeros included — `X'4A'` is the 8-bit `01001010`."
  [expr]
  (if (instance? net.sf.jsqlparser.expression.StringValue expr)
    (parse-bit-literal
     (.getNotExcapedValue ^net.sf.jsqlparser.expression.StringValue expr) false)
    ;; HexValue's getValue keeps the `x'…'` wrapper; parse-bit-literal
    ;; wants the prefix but not the quotes.
    (let [s (str (.getValue ^net.sf.jsqlparser.expression.HexValue expr))]
      (parse-bit-literal (str "x" (subs s 2 (dec (count s)))) false))))

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
                              {:message "bigint out of range"})))
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

(defn get-bit
  "Return bit `n`, indexed left-to-right from zero like PostgreSQL get_bit."
  [^PgBit b n]
  (let [n (long n)
        w (width b)]
    (when (or (neg? n) (>= n w))
      (throw (errors/pg-error
              :array-element-error
              {:detail (str "bit index " n " out of valid range (0.." (dec w) ")")})))
    (if (= \1 (.charAt ^String (:bits b) (int n))) 1 0)))

(defn set-bit
  "Return a same-width bit value with zero-based, left-to-right bit `n` set."
  [^PgBit b n new-bit]
  (let [n (long n)
        new-bit (long new-bit)
        w (width b)]
    (when (or (neg? n) (>= n w))
      (throw (errors/pg-error
              :array-element-error
              {:detail (str "bit index " n " out of valid range (0.." (dec w) ")")})))
    (when-not (contains? #{0 1} new-bit)
      (throw (errors/pg-error :invalid-parameter-value
                              {:message "new bit must be 0 or 1"})))
    (assoc b :bits (str (subs (:bits b) 0 (int n))
                        new-bit
                        (subs (:bits b) (inc (int n)))))))

(defn bit-count
  "Number of set bits in a bit string."
  [^PgBit b]
  (long (count (filter #(= \1 %) (:bits b)))))
