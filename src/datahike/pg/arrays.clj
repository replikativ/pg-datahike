(ns datahike.pg.arrays
  "First-class array values for pgwire-datahike.

   Mirrors PostgreSQL's `ArrayType` (src/include/utils/array.h): a value
   carrying element-type + elements + lbound. We keep it simple — no
   multi-dim support at this layer (Phase 2 follow-up). Subscripting,
   slicing, membership, containment, concat, and PG text-format codec.

   Design notes:
   - `elem-type` is a keyword matching our types/oid-* naming
     (`:int8`, `:text`, `:name`, `:bool`, `:float8`, …) — not an OID.
     Resolution to OID happens at the oid-infer / describeResult layer.
   - `elements` is a plain Clojure vector; NULL elements are `nil`.
     (We deliberately do NOT use the `:__null__` sentinel here — arrays
     are values we own end-to-end, so we use Clojure's own NULL.)
   - Subscript semantics follow PG exactly: 1-indexed, out-of-range
     returns nil (SQL NULL), NOT an error. `arr[0]` and `arr[-1]` both
     return nil.
   - `defrecord` gives us structural equality/hash — important for
     datahike/datalog round-trip (arrays flow through query results,
     bind params, and DISTINCT/GROUP BY without special handling).

   Text codec follows PG (`src/backend/utils/adt/arrayfuncs.c`:
   `array_out` / `array_in`):
   - `{}` for empty
   - Elements joined by `,`
   - Quote `\"…\"` when element contains `,`, `\"`, `\\`, `{`, `}`, or
     whitespace, or is empty, or is the literal `NULL` (case-insensitive)
   - Inside quotes: escape `\\` and `\"` with backslash
   - NULL elements emit unquoted `NULL` token
   - Booleans as `t`/`f` (PG text format for BOOL)"
  (:require [clojure.string :as str]))

(defrecord PgArray [elem-type elements lbound])

(defn array
  "Construct a PgArray with elements and element-type keyword (:int8,
   :text, :name, :bool, :float8, etc.). Default lbound is 1 (PG default)."
  ([elem-type elements]
   (array elem-type elements 1))
  ([elem-type elements lbound]
   (->PgArray elem-type (vec elements) lbound)))

(defn array?
  "True iff v is a PgArray instance."
  [v]
  (instance? PgArray v))

(defn length
  "Number of elements. Differs from PG's `array_length` — this returns 0
   for an empty array; call-sites that need PG's NULL-for-empty can
   check explicitly."
  [^PgArray a]
  (count (:elements a)))

(defn element-type
  "Element-type keyword (:text, :int8, etc.)."
  [^PgArray a]
  (:elem-type a))

;; ---------------------------------------------------------------------------
;; Subscripting / slicing
;; ---------------------------------------------------------------------------

(defn subscript
  "PG array subscript: arr[n]. 1-indexed, returns nil (SQL NULL) for
   out-of-range or nil index. Matches PG exactly — never throws."
  [^PgArray a n]
  (when (and a n (pos-int? n) (<= n (length a)))
    (nth (:elements a) (dec n))))

(defn slice
  "PG array slice: arr[lo:hi]. 1-indexed, inclusive bounds, clamped to
   [1, length]. Returns a PgArray (possibly empty) preserving elem-type.
   nil bounds → PG would use the default (1 / length)."
  [^PgArray a lo hi]
  (let [n (length a)
        lo' (max 1 (or lo 1))
        hi' (min n (or hi n))]
    (if (or (> lo' hi') (zero? n))
      (array (:elem-type a) [])
      (array (:elem-type a)
             (subvec (:elements a) (dec lo') hi')))))

;; ---------------------------------------------------------------------------
;; Operators (predicates + value-producing)
;; ---------------------------------------------------------------------------

(defn member?
  "PG `x = ANY(arr)` semantics with 3-valued logic:
   - returns true if any element = x
   - returns false if no element = x AND no elements were NULL
   - returns nil (UNKNOWN) if x isn't matched but NULLs were present
     (since a NULL element could match any x)"
  [^PgArray a x]
  (let [elts (:elements a)]
    (cond
      (some #(= % x) elts) true
      (some nil? elts)     nil
      :else                false)))

(defn any-match?
  "Predicate form: returns true if (pred e) returns truthy for some e."
  [^PgArray a pred]
  (boolean (some pred (:elements a))))

(defn all-match?
  "Predicate form: returns true if (pred e) returns truthy for every e.
   Vacuously true for an empty array (PG semantics for x = ALL(<empty>))."
  [^PgArray a pred]
  (every? pred (:elements a)))

(defn contains-arr?
  "PG `a @> b`: every non-null element of b is present in a."
  [^PgArray a ^PgArray b]
  (let [as (set (:elements a))]
    (every? #(or (nil? %) (contains? as %)) (:elements b))))

(defn overlap?
  "PG `a && b`: any element of a is in b (ignoring NULLs)."
  [^PgArray a ^PgArray b]
  (let [bs (set (remove nil? (:elements b)))]
    (boolean (some #(contains? bs %) (:elements a)))))

(defn concat-arrs
  "PG `a || b`. Element-types must match; we leave compatibility checks
   to the call-site (PG coerces via least common supertype)."
  [^PgArray a ^PgArray b]
  (array (:elem-type a) (into (:elements a) (:elements b))))

;; ---------------------------------------------------------------------------
;; Text codec
;; ---------------------------------------------------------------------------

(def ^:private text-specials
  "Chars that force double-quoting on output per PG array_out."
  #{\, \" \\ \{ \} \space \tab \newline \return})

(defn- needs-quote?
  [^String s]
  (or (.isEmpty s)
      (.equalsIgnoreCase s "NULL")
      (some text-specials s)))

(defn- escape-for-array-text
  [^String s]
  (str/replace s #"[\\\"]" "\\\\$0"))

(declare to-pg-text)

(defn- element->text
  "Format a single element for array text output. Booleans → t/f,
   numbers → Java toString, strings → quoted+escaped as needed, nil →
   unquoted NULL. Nested PgArrays and sequential collections recurse
   into PG's `{…}` format so multi-dim ARRAY[[1,0],[0,1]] renders
   as `{{1,0},{0,1}}` (matching PG's `array_out`)."
  [v]
  (cond
    (nil? v)        "NULL"
    (boolean? v)    (if v "t" "f")
    (number? v)     (str v)
    (array? v)      (to-pg-text v)
    (sequential? v) (if (empty? v)
                      "{}"
                      (str "{" (str/join "," (map element->text v)) "}"))
    :else           (let [s (str v)]
                      (if (needs-quote? s)
                        (str "\"" (escape-for-array-text s) "\"")
                        s))))

(defn to-pg-text
  "Render a PgArray to PG's canonical text format: `{e1,e2,...}`.
   Used by the wire layer's value->string for any PgArray value."
  [^PgArray a]
  (let [elts (:elements a)]
    (if (empty? elts)
      "{}"
      (str "{" (str/join "," (map element->text elts)) "}"))))

;; --- parsing ----

(defn- coerce-token
  "Coerce a raw token to the target element-type. Unquoted NULL token
   → nil. Element-type drives the destination type."
  [raw quoted? elem-type]
  (cond
    (and (not quoted?) (some-> raw str/lower-case (= "null")))
    nil
    (nil? raw) nil
    :else
    (case elem-type
      :int2     (Long/parseLong raw)
      :int4     (Long/parseLong raw)
      :int8     (Long/parseLong raw)
      :float4   (Double/parseDouble raw)
      :float8   (Double/parseDouble raw)
      :bool     (case (str/lower-case raw)
                  ("t" "true" "1" "yes" "y" "on")  true
                  ("f" "false" "0" "no" "n" "off") false
                  (Boolean/parseBoolean raw))
      raw)))

(defn- parse-elements
  "Walk PG text array body (content between the outer {}) into a vector
   of raw tokens + per-token quoted? flags. Handles escape sequences
   inside quoted strings."
  [^String body]
  (let [n (.length body)]
    (loop [i 0
           acc []
           current (StringBuilder.)
           quoted? false
           in-quote? false
           escape? false]
      (if (>= i n)
        (if (or (pos? (.length current)) quoted?)
          (conj acc [(.toString current) quoted?])
          acc)
        (let [c (.charAt body i)]
          (cond
            escape?
            (do (.append current c)
                (recur (inc i) acc current quoted? in-quote? false))

            (and in-quote? (= c \\))
            (recur (inc i) acc current quoted? in-quote? true)

            (and in-quote? (= c \"))
            (recur (inc i) acc current quoted? false false)

            (and (not in-quote?) (= c \"))
            (recur (inc i) acc current true true false)

            (and (not in-quote?) (= c \,))
            (recur (inc i)
                   (conj acc [(.toString current) quoted?])
                   (StringBuilder.)
                   false false false)

            :else
            (do (.append current c)
                (recur (inc i) acc current quoted? in-quote? false))))))))

(defn from-pg-text
  "Parse PG array text format `{…}` into a PgArray of the given
   element-type. Inverse of `to-pg-text`. Raises on malformed input."
  [^String s elem-type]
  (let [s (str/trim s)]
    (when-not (and (str/starts-with? s "{") (str/ends-with? s "}"))
      (throw (ex-info "Invalid array text literal" {:input s})))
    (let [body (subs s 1 (dec (count s)))]
      (if (str/blank? body)
        (array elem-type [])
        (let [tokens (parse-elements body)
              elements (mapv (fn [[raw quoted?]]
                               (coerce-token raw quoted? elem-type))
                             tokens)]
          (array elem-type elements))))))
