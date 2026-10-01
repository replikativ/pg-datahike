(ns datahike.pg.sql.coerce
  "Numeric coercion helpers for SQL value paths (CAST + INSERT/UPDATE).

   Both `apply-sql-cast` (in stmt.clj) and `coerce-insert-value`
   (also stmt.clj) used to inline `Long/parseLong` /
   `Double/parseDouble` / `BigDecimal.` with subtly different
   null/blank/overflow rules:

     * apply-sql-cast threw `NumberFormatException` on parse failure
       and `.longValue` on overflow (silently truncating).
     * coerce-insert-value's BigInteger branch silently truncated
       to long via `Number.longValue` — `.longValue` of `2^63` returns
       `Long/MIN_VALUE`, which is a wrong-value bug, not a parse bug.
     * The bigdec / float / double string branches each had their
       own `(try … (catch NumberFormatException _ val))` returning
       the original string on failure, which Datahike then rejected
       downstream with a generic schema error instead of `22P02`.

   This namespace centralises those rules so every numeric write goes
   through helpers that raise the right SQLSTATE:

     * `22003 numeric_value_out_of_range` when a value can't fit
       the target's range.
     * `22P02 invalid_text_representation` when a string can't be
       parsed as the target type.

   Both errors are encoded as `ex-info` with `:sqlstate`; the wire
   layer's `handler.clj` already lifts those into ErrorResponse
   messages."
  (:require [datahike.pg.types :as types]
            [clojure.string :as str])
  (:import [java.math BigInteger BigDecimal]
           [java.util.concurrent.atomic AtomicLong]))

(set! *warn-on-reflection* true)

(def ^:private LONG_MIN_BI (BigInteger/valueOf Long/MIN_VALUE))
(def ^:private LONG_MAX_BI (BigInteger/valueOf Long/MAX_VALUE))

(defn pg-error
  "Build an ex-info that the wire layer renders as PG ErrorResponse.
   `sqlstate` is the 5-char SQLSTATE; `msg` is the human-readable text."
  ([sqlstate msg]      (pg-error sqlstate msg nil))
  ([sqlstate msg data] (ex-info msg (merge {:sqlstate sqlstate} data))))

(defonce ^:private ^AtomicLong uuid-v7-ticks
  ;; Milliseconds plus a 12-bit monotonic fraction. A process-local state is
  ;; sufficient: PostgreSQL only promises monotonicity within a backend.
  (AtomicLong. -1))

(defn generate-uuid-v7
  "Generate an RFC 9562 UUIDv7 using the current Unix millisecond and random
   payload bits. Shared by expression evaluation and deferred INSERT execution."
  []
  (let [wall-tick (bit-shift-left
                   (bit-and (System/currentTimeMillis) 0xFFFFFFFFFFFF) 12)
        tick (loop []
               (let [previous (.get uuid-v7-ticks)
                     candidate (max wall-tick (inc previous))]
                 (if (.compareAndSet uuid-v7-ticks previous candidate)
                   candidate
                   (recur))))
        millis (unsigned-bit-shift-right tick 12)
        fraction (bit-and tick 0x0FFF)
        random (java.util.UUID/randomUUID)
        msb (unchecked-long
             (bit-or (bit-shift-left millis 16)
                     0x7000
                     fraction))
        lsb (unchecked-long
             (bit-or Long/MIN_VALUE
                     (bit-and (.getLeastSignificantBits random)
                              0x3FFFFFFFFFFFFFFF)))]
    (java.util.UUID. msb lsb)))

(defn ^long bigint->long
  "BigInteger → primitive long, or raise `22003 numeric_value_out_of_range`."
  [^BigInteger bi]
  (if (and (>= (.compareTo bi LONG_MIN_BI) 0)
           (<= (.compareTo bi LONG_MAX_BI) 0))
    (.longValueExact bi)
    (throw (pg-error "22003"
                     (str "bigint out of range: " (.toString bi))
                     {:value bi}))))

(defn ^long coerce-bigint
  "Coerce a numeric value to a Java long with PG-style overflow checking.

   Raises `22003 numeric_value_out_of_range` when v exceeds Long range.
   Truncates fractional parts (matches `CAST(numeric AS int8)` — PG
   actually rounds, but Datahike has no exact-numeric int type; this
   mirrors the rest of the pipeline that uses `(long …)`).

   Strings: parsed strictly as integers (no decimal point, no
   exponent). Use `coerce-numeric :long` if you want decimal-string
   support."
  [v]
  (cond
    (instance? Long v) v
    (or (instance? Integer v) (instance? Short v) (instance? Byte v))
    (long v)
    (instance? BigInteger v)
    (bigint->long ^BigInteger v)
    (instance? clojure.lang.BigInt v)
    (bigint->long (.toBigInteger ^clojure.lang.BigInt v))
    (instance? BigDecimal v)
    (bigint->long (.toBigInteger ^BigDecimal v))
    (or (instance? Double v) (instance? Float v))
    (let [d (double v)]
      (if (or (Double/isNaN d) (Double/isInfinite d)
              (< d (double Long/MIN_VALUE)) (> d (double Long/MAX_VALUE)))
        (throw (pg-error "22003"
                         (str "bigint out of range: " d)
                         {:value v}))
        (long d)))
    (string? v)
    (let [s (.trim ^String v)]
      (cond
        (.isEmpty s)
        (throw (pg-error "22P02" "invalid input syntax for type bigint: \"\""))
        :else
        (try (bigint->long (BigInteger. s))
             (catch NumberFormatException _
               (throw (pg-error "22P02"
                                (str "invalid input syntax for type bigint: \"" s \")))))))
    :else
    (throw (pg-error "22P02"
                     (str "cannot coerce " (class v) " to bigint")
                     {:value v}))))

(defn special-float
  "The float value a PostgreSQL special-value spelling denotes, or nil.

   float8in accepts `NaN`, `Infinity`, `-Infinity`, `inf`, `-inf` and a
   leading `+`, case-insensitively, with surrounding whitespace only
   (float.c float8in_internal). numeric_in accepts the same set and says
   so in a comment. `NaN` takes no sign."
  [s]
  (when (string? s)
    (let [t (.toLowerCase (.trim ^String s))]
      (case t
        "nan"                                    Double/NaN
        ("inf" "+inf" "infinity" "+infinity")    Double/POSITIVE_INFINITY
        ("-inf" "-infinity")                     Double/NEGATIVE_INFINITY
        nil))))

(defn ^BigDecimal parse-decimal
  "Parse a string as BigDecimal — accepts scientific notation, trims
   whitespace. Raises `22P02` on unparseable input. Returns nil for
   nil input. Empty / whitespace-only strings raise 22P02."
  [s]
  (when (some? s)
    (let [raw (str s)
          t (.trim ^String raw)]
      (if (.isEmpty t)
        (throw (pg-error "22P02"
                         (str "invalid input syntax for type numeric: \"" raw "\"")))
        (try (BigDecimal. t)
             (catch NumberFormatException _
               (throw (pg-error "22P02"
                                (str "invalid input syntax for type numeric: \"" raw \")))))))))

(def ^:private ^:const numeric-max-integer-digits 131072)

(defn- check-numeric-input-range ^BigDecimal [^BigDecimal value]
  (when (and (not (zero? (.signum value)))
             (> (- (.precision value) (.scale value))
                numeric-max-integer-digits))
    (throw (pg-error "22003" "value overflows numeric format")))
  value)

(defn ^BigDecimal parse-numeric-text
  "PostgreSQL `numeric_in` for finite values.

   In addition to decimal/scientific notation, PostgreSQL 17 accepts a
   single underscore between digits and base-prefixed binary, octal and
   hexadecimal integers. An underscore directly after the base prefix is
   also accepted. BigDecimal itself supports none of those extensions, so
   validate their placement before removing separators or converting the
   integer radix."
  [s]
  (when (some? s)
    (let [raw (str s)
          t (.trim ^String raw)
          invalid! #(throw (pg-error "22P02"
                                     (format "invalid input syntax for type numeric: \"%s\"" raw)))]
      (check-numeric-input-range
       (if-let [[_ sign prefix digits]
                (re-matches (re-pattern "(?i)^([+-]?)(0[box])(_?[0-9a-f](?:_?[0-9a-f])*)$") t)]
         (let [radix (case (Character/toLowerCase (char (last prefix)))
                       \b 2
                       \o 8
                       \x 16)
               body  (str/replace-first digits "_" "")]
           (if (every? #(or (= \_ %) (<= 0 (Character/digit (char %) radix))) body)
             (let [unsigned (str/replace body "_" "")
                   signed   (str (when (= "-" sign) "-") unsigned)]
               (try
                 (BigDecimal. (BigInteger. signed radix))
                 (catch NumberFormatException _ (invalid!))))
             (invalid!)))
         (if (re-matches (re-pattern "^[+-]?(?:(?:[0-9](?:_?[0-9])*)(?:\\.(?:[0-9](?:_?[0-9])*)?)?|\\.(?:[0-9](?:_?[0-9])*))(?:[eE][+-]?[0-9](?:_?[0-9])*)?$") t)
           (parse-decimal (str/replace t "_" ""))
           (invalid!)))))))

(defn float->numeric
  "PostgreSQL's float -> numeric conversion, which is NOT
   shortest-round-trip.

   numeric.c float8_numeric / float4_numeric print the value with
   `snprintf(\"%.*g\", DBL_DIG /* 15 */ or FLT_DIG /* 6 */, val)` and
   feed that to the numeric parser -- so the cast deliberately drops the
   digits beyond the type's guaranteed precision. This is why
   `(0.1::float8 + 0.2::float8)::numeric` is 0.3 in PostgreSQL and not
   0.30000000000000004, and why `1.1::real::numeric` is 1.1.

   Java's `%g` keeps trailing zeros where C's strips them, hence the
   stripTrailingZeros; the scale is then clamped at zero because
   PostgreSQL's numeric never carries a negative display scale."
  ^java.math.BigDecimal [v]
  (let [digits (if (instance? Float v) 6 15)
        s (String/format java.util.Locale/ROOT (str "%." digits "g")
                         (object-array [(double v)]))
        bd (.stripTrailingZeros (java.math.BigDecimal. ^String s))]
    (if (neg? (.scale bd)) (.setScale bd 0) bd)))

(defn coerce-numeric
  "Coerce `v` (number or string) to the requested numeric `target`.

   `target` is one of:
     :long    — Java Long; raises 22003 on overflow. Strings may include
                a decimal/exponent (parsed via BigDecimal then narrowed).
     :double  — Java Double (±Infinity allowed, mirrors PG float8).
     :float   — Java Float  (±Infinity allowed, mirrors PG real).
     :bigdec  — Java BigDecimal (exact, scientific notation OK).

   Numbers pass through as the right type. Strings go through
   `parse-decimal` as the canonical intermediate. Unparseable strings
   raise `22P02`; out-of-range numbers (only for `:long`) raise
   `22003`. nil → nil."
  [v target]
  (when (some? v)
    (case target
      :long
      (cond
        (string? v) (let [bd (parse-decimal v)]
                      ;; truncate fractional part (.toBigInteger drops
                      ;; the scale), then range-check.
                      (bigint->long (.toBigInteger ^BigDecimal bd)))
        :else       (coerce-bigint v))

      :bigdec
      (cond
        (instance? BigDecimal v) v
        ;; NaN / +-Infinity, which BigDecimal cannot hold -- carried by
        ;; types/PgNumericSpecial instead. PostgreSQL's numeric_in
        ;; accepts the same spellings float8in does and says so.
        (types/numeric-special? v) v
        (special-float v) (types/double->numeric-special (special-float v))
        (instance? BigInteger v) (BigDecimal. ^BigInteger v)
        (instance? clojure.lang.BigInt v)
        (BigDecimal. (.toBigInteger ^clojure.lang.BigInt v))
        (number? v) (bigdec v)
        (string? v) (parse-numeric-text v)
        :else (throw (pg-error "22P02"
                               (str "cannot coerce " (class v) " to numeric")
                               {:value v})))

      :double
      (cond
        (instance? Double v) v
        (number? v) (double v)
        ;; NaN / +-Infinity before the decimal parser, which cannot
        ;; represent them -- so they used to fail as 22P02 and PostgreSQL
        ;; accepts every one.
        (special-float v) (special-float v)
        (string? v) (.doubleValue (parse-decimal v))
        :else (throw (pg-error "22P02"
                               (str "cannot coerce " (class v) " to double")
                               {:value v})))

      :float
      ;; `(float very-large)` raises IllegalArgumentException via
      ;; clojure.lang.RT — bypass with Java cast so PG's 'real'-style
      ;; ±Infinity-on-overflow behaviour is preserved.
      (cond
        (instance? Float v) v
        (number? v) (.floatValue ^Number v)
        (special-float v) (float (special-float v))
        (string? v) (.floatValue ^Number (parse-decimal v))
        :else (throw (pg-error "22P02"
                               (str "cannot coerce " (class v) " to float")
                               {:value v}))))))

;; ============================================================================
;; PG-style typinput dispatch for unknown string literals
;; ============================================================================
;;
;; PG's parser tags single-quoted literals as `unknown` until the
;; surrounding operator/function call resolves their target type, then
;; calls the type's `typinput` function (oidin, int4in, int8in,
;; float8in, numericin, boolin, …) to produce a typed Const. See
;; src/backend/parser/parse_coerce.c:233 (`if (inputTypeId == UNKNOWNOID
;; && IsA(node, Const)) ... apply target type's typinput`).
;;
;; Concrete user-visible effect: `WHERE c.oid = '16384'` works because
;; PG resolves `=(oid, unknown)` → `=(oid, oid)`, then runs
;; `oidin('16384')` to get the long. Without this, comparing an
;; oid-column to a quoted-digit literal returns 0 rows (long ≠ string).
;; psql's `\d <table>` family relies on it; pgjdbc's
;; `getColumns(oid='16384', ...)` etc. use the same idiom.
;;
;; We don't have a full operator-resolution pass. The translator
;; instead detects the shape `Column <op> StringValue` (and reverse,
;; and IN/BETWEEN) at translation time and dispatches the unknown
;; literal through this table when the column resolves to a Datahike
;; valueType we recognise.

(defn- hex-decode
  ^bytes [^String hex]
  (let [n   (.length hex)
        out (java.io.ByteArrayOutputStream.)
        hex-val (fn [c]
                  (let [ch (long (int ^Character c))]
                    (cond
                      (and (>= ch 48) (<= ch 57))  (- ch 48)
                      (and (>= ch 97) (<= ch 102)) (- ch 87)
                      (and (>= ch 65) (<= ch 70))  (- ch 55))))
        skip? (fn [c] (or (= \space c) (= \newline c) (= \tab c) (= \return c)))
        bad!  (fn [c] (throw (ex-info (str "invalid hexadecimal digit: \"" c "\"")
                                      {:error :invalid-parameter-value
                                       :sqlstate "22023"})))]
    (loop [i 0, pending nil]
      (if (>= i n)
        (if pending
          (throw (ex-info "invalid hexadecimal data: odd number of digits"
                          {:error :invalid-parameter-value
                           :sqlstate "22023"}))
          (.toByteArray out))
        (let [c (.charAt hex i)]
          (cond
            ;; Whitespace is skipped only BETWEEN pairs. `hex_decode_safe`
            ;; skips at the top of a loop that consumes two digits, so
            ;; `'\x6 162'` is an error, not `\x6162`.
            (and (skip? c) (nil? pending)) (recur (inc i) pending)
            :else
            (let [v (or (hex-val c) (bad! c))]
              (if pending
                (do (.write out (unchecked-int
                                 (bit-or (bit-shift-left (long pending) 4) (long v))))
                    (recur (inc i) nil))
                (recur (inc i) v)))))))))

(defn bytea-in
  "`byteain` (varlena.c). Two formats, chosen by the first two
   characters:

   - `\\x` followed by hex digits, whitespace permitted between them.
   - otherwise the traditional escaped style: `\\\\` is one backslash,
     `\\nnn` with n in [0-3][0-7][0-7] is that octal byte, EVERY other
     character is itself, and a lone backslash followed by neither is
     22P02.

   The escaped style was not implemented -- a string that was not hex
   had its UTF-8 bytes taken verbatim, so `'\\\\141'::bytea` kept six
   bytes where PostgreSQL stores four, and a lone backslash was accepted
   where PostgreSQL rejects it. `byteain` works on BYTES, not
   characters, so a multi-byte character contributes all of its bytes;
   this scans the UTF-8 encoding for the same reason."
  ^bytes [^String s]
  (if (and (>= (.length s) 2) (= \\ (.charAt s 0)) (= \x (.charAt s 1)))
    (hex-decode (subs s 2))
    (let [^bytes in (.getBytes s java.nio.charset.StandardCharsets/UTF_8)
          n   (alength in)
          out (java.io.ByteArrayOutputStream.)
          bs  (byte 0x5c)
          oct? (fn [^long b ^long lo ^long hi] (and (>= b lo) (<= b hi)))]
      (loop [i 0]
        (if (>= i n)
          (.toByteArray out)
          (let [b (long (aget in i))]
            (cond
              (not= b 0x5c)
              (do (.write out (unchecked-int b)) (recur (inc i)))

              (and (< (+ i 3) n)
                   (oct? (long (aget in (+ i 1))) 0x30 0x33)
                   (oct? (long (aget in (+ i 2))) 0x30 0x37)
                   (oct? (long (aget in (+ i 3))) 0x30 0x37))
              (do (.write out (unchecked-int
                               (+ (bit-shift-left (- (long (aget in (+ i 1))) 0x30) 6)
                                  (bit-shift-left (- (long (aget in (+ i 2))) 0x30) 3)
                                  (- (long (aget in (+ i 3))) 0x30))))
                  (recur (+ i 4)))

              (and (< (inc i) n) (= 0x5c (long (aget in (inc i)))))
              (do (.write out (unchecked-int bs)) (recur (+ i 2)))

              :else
              ;; one backslash, followed by neither another nor valid octal
              (throw (ex-info "invalid input syntax for type bytea"
                              {:error :invalid-text-representation
                               :sqlstate "22P02"})))))))))

(defn parse-bytea-hex
  "The bytea input function, under the name its callers already use.
   Returns nil for a nil or non-string input so the `(or … )` shapes
   around the call sites still read, but a STRING is now always read by
   `bytea-in` -- it never falls back to the literal's own bytes, which
   is what let `'\\xZZ'::bytea` answer `\\x5c785a5a` instead of raising."
  [s]
  (when (string? s)
    (bytea-in s)))

;; The input functions of the types with an unambiguous text form -- bool,
;; the integers, oid, float4/8, numeric, uuid -- live in datahike.pg.input.
;; What is left here is the lenient reading of the storage types that
;; have none yet: date/time (until the datetime decoder), keyword, symbol.

(def vtype->typinput
  "`{:db/valueType → (fn [^String s] typed-value-or-nil)}` for the storage
   types without a datahike.pg.input function. A nil return means the
   literal is unparseable; coerce-unknown then keeps the string."
  {:db.type/string  identity
   ;; SQL has no keyword literal; clients send the bare name as a
   ;; string. `(keyword "draft") → :draft`, `(keyword "foo/bar") →
   ;; :foo/bar`. Blank strings stay as nil so the surrounding
   ;; comparison falls through to text equality and matches nothing.
   :db.type/keyword (fn [^String s] (when-not (clojure.string/blank? s) (keyword s)))
   :db.type/symbol  (fn [^String s] (when-not (clojure.string/blank? s) (symbol s)))})

(defn coerce-unknown
  "Lenient reading of an unknown-type string literal for the storage
   types without a datahike.pg.input function (date/time, keyword,
   symbol). Returns the typed value on success and the original string
   on failure -- the passthrough the datetime decoder will retire
   (consolidation plan, Phase 1.3). Every other type's literal goes
   through datahike.pg.input, which raises instead.

   The `:db.type/instant` typinput needs a parse-timestamp helper
   that lives in expr.clj; instant coercion is wired separately via
   `coerce-comparison-operands` taking an explicit timestamp parser."
  ([^String s vtype] (coerce-unknown s vtype nil))
  ([^String s vtype timestamp-parser]
   (cond
     (nil? s) nil
     (= vtype :db.type/instant)
     (or (when timestamp-parser
           (try (timestamp-parser s) (catch Throwable _ nil)))
         s)
     :else
     (if-let [f (vtype->typinput vtype)]
       (or (f s) s)
       s))))
