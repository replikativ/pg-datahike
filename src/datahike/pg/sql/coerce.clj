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
    (let [t (.trim ^String s)]
      (if (.isEmpty t)
        (throw (pg-error "22P02" "invalid input syntax for numeric: \"\""))
        (try (BigDecimal. t)
             (catch NumberFormatException _
               (throw (pg-error "22P02"
                                (str "invalid input syntax for numeric: \"" t \")))))))))

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
        (string? v) (parse-decimal v)
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

(defn parse-bytea-hex
  "Decode a PostgreSQL bytea hex-format literal (`\\xDEADBEEF`) to a byte array.
   Accepts both `\\x...` and `\\\\x...` prefixes (JDBC/psycopg2 escape variants).
   Returns nil for values that don't look like hex bytea literals."
  [s]
  (when (string? s)
    (let [trimmed (str/trim s)
          without-prefix (cond
                           (str/starts-with? trimmed "\\x") (subs trimmed 2)
                           (str/starts-with? trimmed "\\\\x") (subs trimmed 3)
                           :else nil)]
      (when (and without-prefix
                 (re-matches #"[0-9a-fA-F]*" without-prefix)
                 (even? (count without-prefix)))
        (let [n (/ (count without-prefix) 2)
              bs (byte-array n)]
          (dotimes [i n]
            (aset-byte bs i
                       (unchecked-byte
                        (Integer/parseInt
                         (subs without-prefix (* 2 i) (+ 2 (* 2 i)))
                         16))))
          bs)))))

(defn parse-bool-token
  "Mirror PG's `parse_bool_with_len` (bool.c): any prefix of true/yes
   and false/no ('t', 'tru', 'ye', …), 'on'/'off' needing ≥2 chars so
   a bare 'o' stays ambiguous ('on', 'of', 'off'), and exact '1'/'0'
   (case-insensitive, leading/trailing whitespace ignored). Returns
   nil for unrecognised input."
  [^String s]
  (let [v (clojure.string/lower-case (.trim s))
        n (.length v)
        prefix? (fn [^String word] (and (pos? n) (<= n (.length word))
                                        (.startsWith word v)))]
    (cond
      (or (prefix? "true") (prefix? "yes")) true
      (or (prefix? "false") (prefix? "no")) false
      (and (>= n 2) (prefix? "off")) false
      (= v "on") true
      (= v "1") true
      (= v "0") false)))

(defn- safe [f]
  (fn [s] (try (f s) (catch Throwable _ nil))))

(def vtype->typinput
  "`{:db/valueType → (fn [^String s] typed-value-or-nil)}`. Each fn is
   the Datahike-side analogue of PG's typinput for the corresponding
   target type — `oidin`/`int8in` → `Long/parseLong`, `numericin` →
   `BigDecimal.`, etc. A nil return means the literal is unparseable
   for that type; the caller decides how to handle (typically: keep
   the original string so the comparison falls through to text
   equality, never matches, returns 0 rows — exactly what PG would do
   if the surrounding operator-resolution failed).

   Restricted to pure / immutable conversions; mutable typinputs
   (e.g. timestamptz with the session's TimeZone GUC) need ctx
   threaded through and aren't covered."
  {:db.type/long    (safe #(Long/parseLong (.trim ^String %)))
   :db.type/double  (safe #(Double/parseDouble (.trim ^String %)))
   :db.type/float   (safe #(.floatValue ^Number (Double/parseDouble (.trim ^String %))))
   :db.type/bigdec  (safe #(BigDecimal. (.trim ^String %)))
   :db.type/boolean parse-bool-token
   :db.type/uuid    (safe #(java.util.UUID/fromString (.trim ^String %)))
   :db.type/string  identity
   ;; SQL has no keyword literal; clients send the bare name as a
   ;; string. `(keyword "draft") → :draft`, `(keyword "foo/bar") →
   ;; :foo/bar`. Blank strings stay as nil so the surrounding
   ;; comparison falls through to text equality and matches nothing.
   :db.type/keyword (fn [^String s] (when-not (clojure.string/blank? s) (keyword s)))
   :db.type/symbol  (fn [^String s] (when-not (clojure.string/blank? s) (symbol s)))})

(defn coerce-unknown
  "PG-style typinput dispatch: coerce an unknown-type string literal to
   `vtype` using the type's typinput equivalent. Returns the typed
   value on success, the original string on failure (so the
   surrounding comparison falls through to text equality and matches
   nothing — PG's outcome when an unknown literal can't resolve to
   the operator's expected type).

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
