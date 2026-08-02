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

   Returns `v` unchanged for a target this doesn't classify, which is
   what every call site did before and keeps unknown types passing
   through rather than erroring."
  [v type-str {:keys [explicit? parse-timestamp resolve-regclass
                      prefer-local-datetime?]
               :or {explicit? true}}]
  (if (or (nil? v) (= :__null__ v))
    v
    (let [cat (types/cast-category type-str)]
      (case cat
        ;; A bit value cast to a number is a REINTERPRETATION of its bits
        ;; (varbit.c:1598), not a decimal read of its digits, so this has
        ;; to come before the generic numeric branches — and a PgBit
        ;; reaching `str` would stringify as a defrecord.
        (:integer :float :numeric)
        (if (pg-bits/pg-bit? v)
          (let [n (pg-bits/to-long v)]
            (case cat :float (double n) n))
          (case cat
            :integer (coerce/coerce-numeric v :long)
            :float   (coerce/coerce-numeric v :double)
            :numeric (coerce/coerce-numeric v :bigdec)))

        :text (cond
                (pg-bits/pg-bit? v) (pg-bits/to-pg-text v)
                (pg-arr/array? v)   (pg-arr/to-pg-text v)
                (string? v)         v
                :else               (str v))

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
