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
  (:import [java.math BigInteger BigDecimal]))

(set! *warn-on-reflection* true)

(def ^:private LONG_MIN_BI (BigInteger/valueOf Long/MIN_VALUE))
(def ^:private LONG_MAX_BI (BigInteger/valueOf Long/MAX_VALUE))

(defn pg-error
  "Build an ex-info that the wire layer renders as PG ErrorResponse.
   `sqlstate` is the 5-char SQLSTATE; `msg` is the human-readable text."
  ([sqlstate msg]      (pg-error sqlstate msg nil))
  ([sqlstate msg data] (ex-info msg (merge {:sqlstate sqlstate} data))))

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

(defn ^BigDecimal parse-decimal
  "Parse a string as BigDecimal — accepts scientific notation, trims
   whitespace. Raises `22P02` on unparseable input. Returns nil for
   nil input. Empty / whitespace-only strings raise 22P02."
  [s]
  (when (some? s)
    (let [t (.trim ^String s)]
      (if (.isEmpty t)
        (throw (pg-error "22P02" "invalid input syntax for numeric: \"\""))
        (try (BigDecimal. t)
             (catch NumberFormatException _
               (throw (pg-error "22P02"
                                (str "invalid input syntax for numeric: \"" t \")))))))))

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
        (instance? BigInteger v) (BigDecimal. ^BigInteger v)
        (instance? clojure.lang.BigInt v)
        (BigDecimal. (.toBigInteger ^clojure.lang.BigInt v))
        (number? v) (bigdec v)
        (string? v) (parse-decimal v)
        :else (throw (pg-error "22P02"
                               (str "cannot coerce " (class v) " to numeric")
                               {:value v})))

      :double
      (cond
        (instance? Double v) v
        (number? v) (double v)
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
        (string? v) (.floatValue ^Number (parse-decimal v))
        :else (throw (pg-error "22P02"
                               (str "cannot coerce " (class v) " to float")
                               {:value v}))))))
