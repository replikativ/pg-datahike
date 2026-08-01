(ns datahike.pg.arrays
  "First-class array values for pgwire-datahike.

   Mirrors PostgreSQL's `ArrayType` (src/include/utils/array.h): a
   value carrying element-type, the element vector (possibly nested
   for multi-dim), per-dimension sizes, and per-dimension lower
   bounds. Subscripting, slicing, membership, containment, concat,
   PG text-format codec.

   Design notes:
   - `elem-type` is a keyword matching our types/oid-* naming
     (`:int8`, `:text`, `:name`, `:bool`, `:float8`, …) — not an OID.
     Resolution to OID happens at the oid-infer / describeResult layer.
   - `elements` is a Clojure vector. For 1-D arrays it's a flat
     vector of element values (`nil` represents SQL NULL). For
     multi-dim it's a nested vector — outermost level is the first
     dimension, innermost is the last. Always a uniform shape; PG
     rejects ragged arrays and so do we (the parser raises on a
     dimension mismatch).
   - `dims` is the vector of per-dimension sizes. `nil` is shorthand
     for `[(count elements)]` — a 1-D array. For multi-dim,
     `(count dims) = ndim`, `(reduce * dims) = total leaf count`.
   - `lbounds` is the vector of per-dimension lower bounds. `nil` is
     shorthand for `[1, 1, …]`. PG defaults to 1; non-1 lbounds come
     from `ARRAY[lo:hi]=…` literals (rare). Preserved through
     to-pg-text/from-pg-text round-trip when non-default.
   - We deliberately do NOT use the `:__null__` sentinel inside
     arrays. Arrays are values we own end-to-end, so element NULLs
     are Clojure `nil`.
   - Subscript semantics follow PG exactly: 1-indexed (offset by
     lbound), out-of-range returns `nil` (SQL NULL), NOT an error.
     `arr[0]` and `arr[-1]` return nil for the default lbound=1.
   - `defrecord` gives us structural equality/hash — important for
     datahike/datalog round-trip (arrays flow through query results,
     bind params, and DISTINCT/GROUP BY without special handling).

   Text codec follows PG (`src/backend/utils/adt/arrayfuncs.c`:
   `array_out` / `array_in`):
   - `{}` for empty
   - Elements joined by `,`; nested dimensions emit nested `{…}`
   - Quote `\"…\"` when element contains `,`, `\"`, `\\`, `{`, `}`, or
     whitespace, or is empty, or is the literal `NULL` (case-insensitive)
   - Inside quotes: escape `\\` and `\"` with backslash
   - NULL elements emit unquoted `NULL` token
   - Booleans as `t`/`f` (PG text format for BOOL)
   - Non-default lbounds emit a `[lo1:hi1][lo2:hi2]…=` prefix"
  (:require [clojure.string :as str]
            [datahike.pg.sql.coerce :as coerce]))

(defrecord PgArray [elem-type elements dims lbounds])

(defn- inner-seq
  "Treat both Clojure sequentials and nested `PgArray` records as
   sub-arrays. PgArrays that arrive as elements of a parent vector
   (from expr.clj's ArrayConstructor build-fn at runtime, where each
   inner ArrayConstructor binds its result-var to a PgArray) need to
   be unwrapped so dim/leaf walking sees the underlying nested
   structure rather than treating the whole record as an opaque
   leaf."
  [v]
  (cond
    (instance? PgArray v) (:elements v)
    (sequential? v)       v
    :else                 nil))

(defn- compute-dims
  "Walk `elements` to derive per-dimension sizes. Returns nil for an
   empty top-level (PG treats `{}` as zero-element 1-D). Raises on
   ragged shape — PG rejects them and so do we."
  [elements]
  (letfn [(walk [v]
            (let [vs (inner-seq v)]
              (cond
                ;; All children are themselves sub-arrays — recurse.
                (and vs (seq vs) (every? #(some? (inner-seq %)) vs))
                (let [child-shapes (mapv walk vs)]
                  (when (seq (rest child-shapes))
                    (when-not (apply = child-shapes)
                      (throw (ex-info "ragged array — all sub-arrays must have same shape"
                                      {:error :array-element-error
                                       :detail "ragged array — all sub-arrays must have same shape"
                                       :input elements}))))
                  (into [(count vs)] (or (first child-shapes) [])))
                ;; Leaf-level vector / PgArray
                vs
                [(count vs)]
                :else
                [])))]
    (walk elements)))

(defn array
  "Construct a PgArray. Element-type is a keyword (`:int8`, `:text`,
   `:bool`, …). Optional dims (vector of per-dim sizes) and lbounds
   (vector of per-dim lower bounds, defaulting to 1 for each).

   For 1-D arrays you can omit dims/lbounds — they're derived from
   `(count elements)` and `1` respectively. For multi-dim, pass a
   nested-vector `elements` and dims is computed automatically; or
   pass dims explicitly when the elements form is something exotic."
  ([elem-type elements]
   (array elem-type elements nil nil))
  ([elem-type elements dims-or-lbound]
   ;; back-compat with the 3-arity (elem-type elements lbound)
   ;; that called sites still use. Vector → dims, scalar → lbound.
   (if (or (nil? dims-or-lbound) (vector? dims-or-lbound))
     (array elem-type elements dims-or-lbound nil)
     (array elem-type elements nil [dims-or-lbound])))
  ([elem-type elements dims lbounds]
   (let [elements (vec elements)
         dims     (or dims (compute-dims elements))
         ndim     (count dims)
         lbounds  (or lbounds (vec (repeat (max 1 ndim) 1)))]
     (->PgArray elem-type elements dims lbounds))))

(defn array?
  "True iff v is a PgArray instance."
  [v]
  (instance? PgArray v))

(defn ndim
  "Number of dimensions. 1 for the 1-D default; more for nested arrays."
  [^PgArray a]
  (max 1 (count (:dims a))))

(defn length
  "Element count along the outermost (first) dimension. For 1-D arrays
   that's the total leaf count. For multi-dim it's the size of dim 1.
   Differs from PG's `array_length` — this returns 0 for an empty
   array; call-sites that need PG's NULL-for-empty check explicitly."
  [^PgArray a]
  (if (seq (:dims a))
    (first (:dims a))
    (count (:elements a))))

(defn length-d
  "Length along dimension `d` (1-indexed). PG's `array_length(arr, d)`
   returns NULL when d is out of range — call-sites should check
   `(<= d (ndim a))` before relying on the result."
  [^PgArray a d]
  (let [d (long d)]
    (when (and (pos? d) (<= d (ndim a)))
      (nth (:dims a) (dec d) (count (:elements a))))))

(defn lbound
  "Lower bound for dimension `d` (1-indexed, default 1). PG: arrays
   default to 1; explicit `[lo:hi]=` literals can shift this."
  ([^PgArray a] (lbound a 1))
  ([^PgArray a d]
   (or (get (:lbounds a) (dec (long d))) 1)))

(defn ubound
  "Upper bound for dimension `d` (1-indexed). `(lbound a d) + (length-d
   a d) - 1`, or nil if d out of range."
  [^PgArray a d]
  (when-let [n (length-d a d)]
    (+ (lbound a d) (long n) -1)))

(defn element-type
  "Element-type keyword (:text, :int8, etc.)."
  [^PgArray a]
  (:elem-type a))

;; ---------------------------------------------------------------------------
;; Subscripting / slicing
;; ---------------------------------------------------------------------------

(defn subscript
  "PG array subscript: `arr[n]`. 1-indexed (offset by lbound),
   out-of-range or nil index returns nil (SQL NULL) — never throws.

   For multi-dim arrays, returns the nth sub-array (still a PgArray
   with rank reduced by 1) so caller can chain `(subscript (subscript
   m 1) 2)` for `m[1][2]`. Matches PG: `arr[i]` on a 2-D yields a
   1-D row, `arr[i][j]` yields a scalar."
  [^PgArray a n]
  (when (and a n (integer? n))
    (let [n (long n)
          lo (lbound a 1)
          off (- n lo)]
      (when (and (not (neg? off)) (< off (length a)))
        (let [child (nth (:elements a) off)]
          (if (and (sequential? child) (> (ndim a) 1))
            (let [child-elements (vec child)
                  child-dims     (vec (rest (:dims a)))
                  child-lbounds  (vec (rest (:lbounds a)))]
              (->PgArray (:elem-type a) child-elements child-dims child-lbounds))
            child))))))

(defn slice
  "PG array slice: `arr[lo:hi]`. 1-indexed, inclusive bounds, clamped
   to `[lbound, lbound+length-1]`. Returns a PgArray (possibly empty)
   preserving elem-type. nil bounds → PG default (lbound / lbound+len-1).

   Slicing operates on the outermost dimension only; inner shape and
   lbounds are preserved. Multi-dim aware."
  [^PgArray a lo hi]
  (let [n        (length a)
        lb       (lbound a 1)
        ub       (+ lb n -1)
        lo'      (max lb (or lo lb))
        hi'      (min ub (or hi ub))
        lo-off   (- lo' lb)
        hi-off   (- hi' lb)
        slice-len (max 0 (inc (- hi-off lo-off)))]
    (if (or (> lo' hi') (zero? n))
      (array (:elem-type a) [] (vec (cons 0 (rest (:dims a)))) (:lbounds a))
      (let [sliced (subvec (:elements a) lo-off (+ lo-off slice-len))
            new-dims (vec (cons slice-len (rest (:dims a))))]
        (->PgArray (:elem-type a) sliced new-dims (:lbounds a))))))

(defn flat-elements
  "Walk a (possibly nested) elements vector to a flat seq of leaves —
   used by member? / contains-arr? / overlap? and the binary codec
   which encode element-by-element regardless of shape. Preserves nil
   leaves so 3-valued logic works through them. Descends into nested
   PgArray records as well as Clojure sequentials so an outer
   PgArray whose `:elements` happen to hold inner PgArrays (the
   common shape produced by expr.clj's ArrayConstructor build-fn for
   `ARRAY[ARRAY[…],…]`) is walked correctly."
  [^PgArray a]
  (let [walk (fn walk [v]
               (if-let [vs (inner-seq v)]
                 (mapcat walk vs)
                 [v]))]
    (walk (:elements a))))

(defn multidim?
  "True if the array has more than one dimension. Several PG functions
   (array_append, array_prepend, array_position, array_remove)
   explicitly reject multi-dim arrays; callers raise the matching
   feature_not_supported error in that case."
  [^PgArray a]
  (> (ndim a) 1))

(defn replace-leaves
  "Walk every leaf of `a`'s nested elements, applying `f` to each
   leaf value. Returns a new PgArray with the same shape, dims, and
   lbounds. Used by `array_replace`, which PG applies to all leaves
   regardless of dimensionality (`arrayfuncs.c:6662`). Descends into
   nested PgArrays as well as Clojure sequentials."
  [^PgArray a f]
  (let [walk (fn walk [v]
               (if-let [vs (inner-seq v)]
                 (mapv walk vs)
                 (f v)))]
    (->PgArray (:elem-type a)
               (mapv walk (:elements a))
               (:dims a)
               (:lbounds a))))

(defn match-shape
  "Predicate: does `b` have a shape that's compatible with being a
   sub-element of `a`? PG accepts this when `(rest dims-a)` matches
   `dims-b` exactly. Used by array_cat for the asymmetric N-D || (N-1)-D
   and (N-1)-D || N-D cases (`array_userfuncs.c:471–479`)."
  [^PgArray a ^PgArray b]
  (= (vec (rest (:dims a))) (:dims b)))

(defn cat-rejecting-mismatch
  "PG `||` with full multi-dim semantics. Three cases:
     1. ndim-a == ndim-b: concat along outer dim. Inner dims must
        match (line 442–453).
     2. ndim-a == ndim-b + 1: append b as a new sub-element of a.
        b's dims must match a's `(rest dims)` (line 471–479).
     3. ndim-b == ndim-a + 1: prepend a as a new sub-element of b.
        a's dims must match b's `(rest dims)` (line 499–507).
   Result lbound comes from the first arg. NULL handling at the wire
   layer above this — pass non-NULL PgArrays."
  [^PgArray a ^PgArray b]
  (let [na (ndim a) nb (ndim b)
        aelts (:elements a) belts (:elements b)
        adims (:dims a)     bdims (:dims b)
        albs  (:lbounds a)]
    (cond
      ;; Case 1: same ndim, concat along outer
      (= na nb)
      (do
        (when (and (> na 1) (not= (vec (rest adims)) (vec (rest bdims))))
          (throw (ex-info "cannot concatenate incompatible arrays — inner dims must match"
                          {:error :array-element-error
                           :detail "cannot concatenate incompatible arrays — inner dims must match"
                           :a-dims adims :b-dims bdims})))
        (->PgArray (:elem-type a)
                   (into aelts belts)
                   (into [(+ (long (first adims)) (long (first bdims)))]
                         (rest adims))
                   albs))

      ;; Case 2: a is N-D, b is (N-1)-D — append b as outer-dim element
      (= na (inc nb))
      (do
        (when (not (match-shape a b))
          (throw (ex-info "cannot concatenate incompatible arrays — sub-array shape mismatch"
                          {:error :array-element-error
                           :detail "cannot concatenate incompatible arrays — sub-array shape mismatch"
                           :a-dims adims :b-dims bdims})))
        (->PgArray (:elem-type a)
                   (conj aelts belts)
                   (into [(inc (long (first adims)))] (rest adims))
                   albs))

      ;; Case 3: b is N-D, a is (N-1)-D — prepend a as outer-dim element
      (= nb (inc na))
      (do
        (when (not (match-shape b a))
          (throw (ex-info "cannot concatenate incompatible arrays — sub-array shape mismatch"
                          {:error :array-element-error
                           :detail "cannot concatenate incompatible arrays — sub-array shape mismatch"
                           :a-dims adims :b-dims bdims})))
        (->PgArray (:elem-type b)
                   (into [aelts] belts)
                   (into [(inc (long (first bdims)))] (rest bdims))
                   (:lbounds b)))

      :else
      (throw (ex-info "cannot concatenate arrays of different dimensionality"
                      {:error :array-element-error
                       :detail "cannot concatenate arrays of different dimensionality"
                       :a-ndim na :b-ndim nb})))))

;; ---------------------------------------------------------------------------
;; Operators (predicates + value-producing)
;; ---------------------------------------------------------------------------

(defn member?
  "PG `x = ANY(arr)` semantics with 3-valued logic. Operates on
   flattened leaves so multi-dim arrays match anywhere:
     ARRAY[[1,2],[3,4]] = ANY(…) is true if any leaf equals x.
   - returns true if any leaf = x
   - returns false if no leaf = x AND no NULL leaves
   - returns nil (UNKNOWN) if x isn't matched but NULL leaves exist
     (a NULL leaf could match any x)"
  [^PgArray a x]
  (let [leaves (flat-elements a)]
    (cond
      (some #(= % x) leaves) true
      (some nil? leaves)     nil
      :else                  false)))

(defn any-match?
  "Predicate form: returns true if (pred leaf) returns truthy for some
   leaf. Walks all dimensions."
  [^PgArray a pred]
  (boolean (some pred (flat-elements a))))

(defn all-match?
  "Predicate form: returns true if (pred leaf) returns truthy for
   every leaf. Vacuously true for an empty array (PG semantics for
   `x = ALL(<empty>)`)."
  [^PgArray a pred]
  (every? pred (flat-elements a)))

(defn contains-arr?
  "PG `a @> b`: every non-null leaf of b is present somewhere in a.
   Operates on flattened leaves — PG ignores shape for `@>`."
  [^PgArray a ^PgArray b]
  (let [as (set (flat-elements a))]
    (every? #(or (nil? %) (contains? as %)) (flat-elements b))))

(defn overlap?
  "PG `a && b`: any leaf of a is a leaf of b (NULLs ignored).
   Operates on flattened leaves."
  [^PgArray a ^PgArray b]
  (let [bs (set (remove nil? (flat-elements b)))]
    (boolean (some #(contains? bs %) (flat-elements a)))))

(declare cat-rejecting-mismatch)

(defn concat-arrs
  "PG `a || b` — full multi-dim semantics via `cat-rejecting-mismatch`.
   Element-types must match; we leave compatibility checks to the
   call-site (PG coerces via least common supertype)."
  [^PgArray a ^PgArray b]
  (cat-rejecting-mismatch a b))

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

(defn- lbound-prefix
  "PG emits a `[lo:hi]…=` prefix only when any lbound != 1. Format
   per dim: `[lbound:lbound+length-1]`. We emit the prefix only for
   non-default lbounds so 1-D arrays with the conventional
   `lbound=1` round-trip through `{…}` exactly."
  [^PgArray a]
  (let [lbs (:lbounds a)
        dims (:dims a)]
    (when (and (seq lbs) (some (fn [lb] (not= 1 lb)) lbs))
      (str/join ""
                (map-indexed
                 (fn [i lb]
                   (let [n (nth dims i (count (:elements a)))]
                     (str "[" lb ":" (+ (long lb) (long n) -1) "]")))
                 lbs)))))

(defn to-pg-text
  "Render a PgArray to PG's canonical text format: `{e1,e2,...}` for
   1-D, nested `{{…},{…}}` for multi-dim. Emits `[lo1:hi1][lo2:hi2]…=`
   prefix when any lower bound != 1.

   Used by the wire layer's value->string for any PgArray value."
  [^PgArray a]
  (let [elts (:elements a)
        body (if (empty? elts)
               "{}"
               (str "{" (str/join "," (map element->text elts)) "}"))]
    (if-let [pfx (lbound-prefix a)]
      (str pfx "=" body)
      body)))

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
      :bool     (let [b (coerce/parse-bool-token raw)]
                  (when (nil? b)
                    (throw (ex-info (str "invalid input syntax for type boolean: "
                                         (pr-str raw))
                                    {:error :invalid-text-representation
                                     :type "boolean" :value raw})))
                  b)
      raw)))

(defn- parse-lbound-prefix
  "If the input starts with `[lo:hi][lo:hi]…=`, consume it and return
   `[remaining-string [lo1 lo2 …]]`. Otherwise return `[s nil]`."
  [^String s]
  (if-not (str/starts-with? s "[")
    [s nil]
    (loop [s   s
           lbs []]
      (if (str/starts-with? s "[")
        ;; PG array bounds may be negative (e.g. `[-1:0]`), so allow a sign.
        (if-let [m (re-find #"^\[(-?\d+):(-?\d+)\](.*)$" s)]
          (let [lo (Long/parseLong (nth m 1))
                rest-s (nth m 3)]
            (recur rest-s (conj lbs lo)))
          (throw (ex-info "Invalid array lbound prefix"
                          {:error :invalid-text-representation :type "array"
                           :detail "invalid array lbound prefix"
                           :input s})))
        (do
          (when-not (str/starts-with? s "=")
            (throw (ex-info "Expected `=` after array lbound prefix"
                            {:error :invalid-text-representation :type "array"
                             :detail "expected `=` after array lbound prefix"
                             :input s})))
          [(subs s 1) lbs])))))

(defn- parse-tree
  "Walk a PG text array body, building the nested element tree.
   Recognises nested `{…}` as sub-arrays and emits per-token raw
   strings + quoted flags to feed `coerce-token`. Returns a map
   `{:tree <nested-vec> :consumed <int>}` for the parsed prefix,
   with `consumed` = number of chars eaten (including the outer
   `{` and `}`).

   `s` is the full string and `start` is the index of the leading
   `{`. The returned `:tree` is a vector. Each element is either
   another tree-vector (nested) or a `[raw quoted?]` token pair
   (leaf). Coercion to typed values happens later in `coerce-tree`,
   once we know the elem-type.

   `pending?` tracks whether the current builder represents an
   in-progress scalar token (so a comma or close-brace knows whether
   to emit it). It's `true` after any character (or quote) has been
   consumed for the current slot, and `false` at the very start of a
   slot (right after `{` or `,`) and after a nested `{…}` is
   absorbed."
  [^String s start]
  (let [n (.length s)]
    (when-not (and (< start n) (= \{ (.charAt s start)))
      (throw (ex-info "Expected `{`"
                      {:error :invalid-text-representation :type "array"
                       :detail "malformed array literal — expected `{`"
                       :input s :at start})))
    (loop [i        (inc start)
           items    []
           current  (StringBuilder.)
           quoted?  false
           in-quote? false
           escape?   false
           pending?  false]
      (when (>= i n)
        (throw (ex-info "Unterminated array literal"
                        {:error :invalid-text-representation :type "array"
                         :detail "unterminated array literal"
                         :input s})))
      (let [c (.charAt s i)]
        (cond
          escape?
          (do (.append current c)
              (recur (inc i) items current quoted? in-quote? false true))

          (and in-quote? (= c \\))
          (recur (inc i) items current quoted? in-quote? true true)

          (and in-quote? (= c \"))
          (recur (inc i) items current quoted? false false true)

          (and (not in-quote?) (= c \"))
          (recur (inc i) items current true true false true)

          ;; Nested array — recurse, then continue from after `}`. The
          ;; slot is closed by the sub-array, so reset pending? to
          ;; false: a following `,` must not emit a phantom empty
          ;; token, and a following `}` closes the outer array
          ;; cleanly. Only valid at the start of a slot; mid-token
          ;; `{` would only come from a malformed string.
          (and (not in-quote?) (not pending?) (= c \{))
          (let [sub (parse-tree s i)]
            (recur (+ i (long (:consumed sub)))
                   (conj items (:tree sub))
                   (StringBuilder.)
                   false false false false))

          (and (not in-quote?) pending? (= c \{))
          (throw (ex-info "Unexpected `{` mid-token"
                          {:error :invalid-text-representation :type "array"
                           :detail "unexpected `{` mid-token in array literal"
                           :input s :at i}))

          (and (not in-quote?) (= c \,))
          ;; Close the current slot. If pending? is false, the slot
          ;; was a nested array already added to items — nothing to
          ;; close. Otherwise emit the scalar token (quoted or not).
          (recur (inc i)
                 (if pending?
                   (conj items [(.toString current) quoted?])
                   items)
                 (StringBuilder.)
                 false false false false)

          (and (not in-quote?) (= c \}))
          {:tree (if pending?
                   (conj items [(.toString current) quoted?])
                   items)
           :consumed (- (inc i) start)}

          :else
          (do (.append current c)
              (recur (inc i) items current quoted? in-quote? false true)))))))

(defn- token?
  "A `[raw quoted?]` leaf pair from `parse-tree`."
  [node]
  (and (vector? node) (= 2 (count node))
       (string? (first node)) (boolean? (second node))))

(defn- coerce-tree
  "Walk the raw token tree from `parse-tree`, coercing leaf tokens to
   the target element-type. Returns `[coerced-elements dims]` —
   `coerced-elements` is the (possibly nested) vector of typed
   values, `dims` is the per-level size vector. Validates uniform
   shape (rejects ragged arrays the same way PG does)."
  [tree elem-type]
  (letfn [(walk [node]
            (cond
              ;; Leaf pair → scalar value, dims contributes 0 levels.
              (token? node)
              [(coerce-token (first node) (second node) elem-type) []]

              ;; Sub-array → recurse on each element, validate shape.
              (vector? node)
              (let [children   (mapv walk node)
                    child-dims (mapv second children)]
                (when (seq (rest child-dims))
                  (when-not (apply = child-dims)
                    (throw (ex-info "ragged array — sub-arrays must have same shape"
                                    {:error :array-element-error
                                     :detail "ragged array — sub-arrays must have same shape"
                                     :dims child-dims}))))
                [(mapv first children)
                 (into [(count node)] (or (first child-dims) []))])

              :else
              (throw (ex-info "Unexpected parse-tree shape"
                              {:error :internal-error
                               :node node}))))]
    (walk tree)))

(defn from-pg-text
  "Parse PG array text format into a PgArray of the given element-type.
   Inverse of `to-pg-text`. Handles:
   - `{}` — empty array
   - `{1,2,3}` — flat 1-D
   - `{{1,2},{3,4}}` — multi-dim (nested braces)
   - `[2:4]={a,b,c}` — non-default lbound
   - `[1:2][1:2]={{1,0},{0,1}}` — non-default multi-dim lbound
   - quoted strings with `,`, `\"`, `\\` escape sequences
   - unquoted `NULL` token → element nil
   Raises on malformed input or ragged shape."
  [^String s elem-type]
  (let [s (str/trim s)
        [body lbounds] (parse-lbound-prefix s)
        body (str/trim body)]
    (when-not (and (str/starts-with? body "{") (str/ends-with? body "}"))
      (throw (ex-info "Invalid array text literal"
                      {:error :invalid-text-representation :type "array"
                       :detail "malformed array literal — must start with `{` and end with `}`"
                       :input s})))
    (let [{:keys [tree]} (parse-tree body 0)]
      (if (empty? tree)
        (array elem-type [] [0] (or lbounds [1]))
        (let [[coerced dims] (coerce-tree tree elem-type)]
          (array elem-type coerced dims (or lbounds (vec (repeat (max 1 (count dims)) 1)))))))))

