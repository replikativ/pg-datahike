(ns datahike.pg.jsonb
  "PostgreSQL jsonb type support for the PgWire compatibility layer.

   Stores jsonb values as Clojure data structures (maps, vectors, strings,
   numbers, booleans, nil) serialized to/from JSON strings via jsonista.

   Implements PostgreSQL jsonb operators and functions as pure Clojure
   functions that can be used as Datalog predicates or post-processing."
  (:require [jsonista.core :as json]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; ============================================================================
;; JSON parsing / serialization
;; ============================================================================

(def ^:private mapper
  "Parse-side mapper. `USE_BIG_DECIMAL_FOR_FLOATS` is what makes number
   fidelity possible at all: PostgreSQL's jsonb numbers are `numeric`,
   and a Java double cannot represent `1.00` distinctly from `1`, nor
   9007199254740993 exactly. Once a value has been through a double the
   information is gone and no renderer can recover it."
  (doto ^com.fasterxml.jackson.databind.ObjectMapper
   (json/object-mapper {:decode-key-fn str})
    (.configure com.fasterxml.jackson.databind.DeserializationFeature/USE_BIG_DECIMAL_FOR_FLOATS
                true)))

(def json-null
  "PostgreSQL's JSON `null` is a VALUE, distinct from SQL NULL: `IS NULL`
   on it is false and `jsonb_typeof` answers \"null\". Representing it as
   Clojure `nil` conflated the two, and because a datalog function
   binding that yields nil FILTERS THE ROW, `SELECT p->'k'` on a JSON
   null returned no rows at all where PostgreSQL returns one row.

   Distinct from `:__null__`, which is this codebase's SQL-NULL
   sentinel — `->>` collapses JSON null TO SQL NULL, so both exist and
   they are not the same thing."
  ::json-null)

(def ^:private numeric-max-dscale
  "PostgreSQL NUMERIC_DSCALE_MAX. Beyond it, 22003." 16383)

(def ^:private numeric-max-int-digits
  "PostgreSQL NUMERIC_WEIGHT_MAX * DEC_DIGITS. Beyond it, 22003." 131072)

(defn- check-numeric-range!
  "BigDecimal is strictly more permissive than PostgreSQL's numeric, so
   without this we accept documents PostgreSQL rejects."
  [^java.math.BigDecimal d]
  (when (> (.scale d) numeric-max-dscale)
    (throw (ex-info "value overflows numeric format"
                    {:error :numeric-value-out-of-range :sqlstate "22003"})))
  (when (> (- (.precision d) (.scale d)) numeric-max-int-digits)
    (throw (ex-info "value overflows numeric format"
                    {:error :numeric-value-out-of-range :sqlstate "22003"})))
  d)

(defn- normalize-tree
  "Bring a freshly-parsed tree into the value model:

     - EVERY number becomes a BigDecimal. Clojure's `=`/`hash` on
       BigDecimal are scale-INsensitive, which reproduces PostgreSQL's
       `numeric_eq` — and therefore DISTINCT and GROUP BY — for free.
       That only holds if the representation is UNIFORM: `(= 1 1M)` is
       false, so a tree mixing Long and BigDecimal compares wrong.
     - JSON null becomes `json-null` rather than nil (see above).
     - Objects keep unique keys; Jackson already applied last-wins.

   Runs once per parse, alongside the parse we already pay for."
  [v]
  (cond
    (nil? v)     json-null
    (map? v)     (persistent! (reduce-kv (fn [m k x] (assoc! m (str k) (normalize-tree x)))
                                         (transient {}) v))
    (vector? v)  (mapv normalize-tree v)
    (sequential? v) (mapv normalize-tree v)
    (instance? java.math.BigDecimal v) (check-numeric-range! v)
    (integer? v) (java.math.BigDecimal. (str v))
    (float? v)   (check-numeric-range! (java.math.BigDecimal. (str v)))
    :else        v))

(defn parse-jsonb
  "Parse a JSON string to the jsonb value model.
   Returns nil for nil input, passes through non-strings."
  [v]
  (cond
    (nil? v) nil
    (string? v) (try (normalize-tree (json/read-value v mapper))
                     (catch clojure.lang.ExceptionInfo e (throw e))
                     (catch Exception _ v))
    :else v))

;; ---------------------------------------------------------------------------
;; The canonical writer — PostgreSQL's jsonb output, not Jackson's
;; ---------------------------------------------------------------------------

(defn- pg-key-cmp
  "PostgreSQL's jsonb object-key order: LENGTH FIRST, then bytewise over
   the UTF-8 encoding (`lengthCompareJsonbString`, jsonb_util.c).

   It is neither alphabetical nor collation-aware, which is why Jackson's
   ORDER_MAP_ENTRIES_BY_KEYS is the wrong tool: `{\"z\":1,\"aa\":2,\"b\":3}`
   is `{\"b\": 3, \"z\": 1, \"aa\": 2}` in PostgreSQL and
   `{\"aa\":2,\"b\":3,\"z\":1}` under alphabetical order. Length is in
   OCTETS, so a 2-byte `é` sorts with the 2-character keys."
  ^long [^String a ^String b]
  (let [ba (.getBytes a java.nio.charset.StandardCharsets/UTF_8)
        bb (.getBytes b java.nio.charset.StandardCharsets/UTF_8)]
    (if (not= (alength ba) (alength bb))
      (- (alength ba) (alength bb))
      (java.util.Arrays/compareUnsigned ba bb))))

(defn- append-json-string!
  "PostgreSQL's `escape_json`: escape exactly \b \f \n \r \t \" and
   backslash; any other byte below 0x20 becomes a \\u escape with four LOWERCASE hex
   digits; everything from 0x20 up is emitted raw — including `/`, DEL,
   and all multi-byte UTF-8. PostgreSQL never emits a \\u escape for non-ASCII."
  [^StringBuilder sb ^String s]
  (.append sb \")
  (dotimes [i (.length s)]
    (let [c (.charAt s i)]
      (case c
        \backspace (.append sb "\\b")
        \formfeed  (.append sb "\\f")
        \newline   (.append sb "\\n")
        \return    (.append sb "\\r")
        \tab       (.append sb "\\t")
        \"         (.append sb "\\\"")
        \\        (.append sb "\\\\")
        (if (< (int c) 0x20)
          (.append sb (format "\\u%04x" (int c)))
          (.append sb c)))))
  (.append sb \"))

(defn- append-number!
  "PostgreSQL renders a jsonb number with `numeric_out`: plain positional
   notation, never scientific, with exactly the literal's own scale.

   `dscale = max(0, fraction-digits - exponent)`, which is what
   BigDecimal's own `scale()` gives after parsing — `1e3` parses to scale
   -3, so clamping at 0 reproduces PostgreSQL exactly: `1e3` → `1000`,
   `1.00` → `1.00`, `1e-3` → `0.001`. `toPlainString` is required;
   `toString` would emit `1E+3`."
  [^StringBuilder sb v]
  (.append sb
           (cond
             (instance? java.math.BigDecimal v)
             (let [^java.math.BigDecimal d v]
               (.toPlainString (if (neg? (.scale d)) (.setScale d 0) d)))
             (instance? Double v)
             (.toPlainString (java.math.BigDecimal/valueOf ^double v))
             :else (str v))))

(defn- emit!
  [^StringBuilder sb v]
  (cond
    (nil? v)     (.append sb "null")
    (= json-null v) (.append sb "null")
    (map? v)     (do (.append sb \{)
                     (reduce (fn [first? k]
                               (when-not first? (.append sb ", "))
                               (append-json-string! sb (str k))
                               (.append sb ": ")
                               (emit! sb (get v k))
                               false)
                             true
                             (sort pg-key-cmp (map str (keys v))))
                     (.append sb \}))
    (sequential? v) (do (.append sb \[)
                        (reduce (fn [first? x]
                                  (when-not first? (.append sb ", "))
                                  (emit! sb x)
                                  false)
                                true v)
                        (.append sb \]))
    (string? v)  (append-json-string! sb v)
    (boolean? v) (.append sb (if v "true" "false"))
    (number? v)  (append-number! sb v)
    :else        (append-json-string! sb (str v))))

(defn serialize-jsonb
  "The canonical jsonb TEXT for a value, byte-for-byte as PostgreSQL
   renders it: keys length-first then bytewise, `\", \"` between pairs and
   `\": \"` after each key, numbers via numeric semantics, duplicate keys
   already collapsed last-wins by the parser.

   This is both the stored form and the form a client reads back, because
   PostgreSQL normalizes jsonb on input and has no memory of the original
   text. `json` is the text-faithful type and must never come through
   here.

   A string that is valid JSON is re-emitted canonically; a string that is
   not JSON becomes a JSON string scalar; a Clojure map/vector is written
   directly. nil in, nil out."
  [v]
  (when (some? v)
    (let [data (if (string? v)
                 (try (json/read-value v mapper)
                      (catch Exception _ v))
                 v)
          sb (StringBuilder.)]
      (emit! sb data)
      (.toString sb))))

(def canonicalize-jsonb
  "Deprecated alias for `serialize-jsonb`; they were always the same fn."
  serialize-jsonb)

;; ============================================================================
;; Operators: -> and ->> (field/element access)
;; ============================================================================

(defn jsonb-get
  "PostgreSQL -> operator: get jsonb field by key (text) or element by index (int).
   Returns jsonb (Clojure data structure).
   Returns :__null__ sentinel unchanged (NULL propagation for get-else)."
  [v key-or-idx]
  (if (= v :__null__) :__null__
      (let [parsed (parse-jsonb v)
            ;; MISSING is SQL NULL; a JSON null that is PRESENT is the
            ;; jsonb value `null` and must survive as one.
            ;;
            ;; Returning bare nil for "missing" was a row-loss bug, not
            ;; just a wrong type: this fn is invoked as a datalog
            ;; function binding, and a binding that yields nil FILTERS
            ;; THE ROW. `SELECT p->'nope' FROM t` returned zero rows
            ;; where PostgreSQL returns one row of NULL. `:__null__` is
            ;; the sentinel the rest of the pipeline renders as SQL NULL.
            missing :__null__]
        (cond
          (and (map? parsed) (string? key-or-idx))
          (get parsed key-or-idx missing)

          (and (map? parsed) (integer? key-or-idx))
          (get parsed (str key-or-idx) missing)

          (and (sequential? parsed) (integer? key-or-idx))
          (let [idx (if (neg? key-or-idx)
                      (+ (count parsed) key-or-idx)
                      key-or-idx)]
            (if (or (neg? idx) (>= idx (count parsed)))
              missing
              (nth parsed idx)))

          :else missing))))

(defn jsonb-get-text
  "PostgreSQL ->> operator: get field/element as text string.
   Returns a string or `:__null__` sentinel. Never returns nil — Datahike's
   function-binding clauses filter the row when the binding returns nil, but
   `foo->>missing_key` should produce SQL NULL while keeping the row."
  [v key-or-idx]
  (let [result (jsonb-get v key-or-idx)]
    (cond
      (= result :__null__) :__null__
      ;; `->` yields the jsonb value `null`; `->>` collapses it to SQL
      ;; NULL. That asymmetry is PostgreSQL's, and it is the reason both
      ;; sentinels have to exist.
      (= result json-null) :__null__
      (string? result)  result
      (boolean? result) (str result)
      ;; A number renders through the canonical writer so it keeps its
      ;; numeric scale — `->> ` on 1.00 is "1.00", not "1.0".
      (or (number? result) (coll? result)) (serialize-jsonb result)
      (some? result) (str result)
      :else :__null__)))

(defn jsonb-get-path
  "PostgreSQL #> operator: extract jsonb at path.
   Path is a sequence of text keys."
  [v path]
  (reduce jsonb-get (parse-jsonb v) path))

(defn jsonb-get-path-text
  "PostgreSQL #>> operator: extract text at path.
   Returns `:__null__` sentinel when the path doesn't exist — returning nil
   would make Datahike's function-binding clause filter the row."
  [v path]
  (let [result (jsonb-get-path v path)]
    (cond
      (nil? result) :__null__
      (string? result) result
      :else (json/write-value-as-string result))))

;; ============================================================================
;; Operators: containment and existence
;; ============================================================================

(defn jsonb-contains?
  "PostgreSQL @> operator: does left contain right?"
  [left right]
  (let [l (parse-jsonb left)
        r (parse-jsonb right)]
    (cond
      (and (map? l) (map? r))
      (every? (fn [[k v]]
                (let [lv (get l k ::missing)]
                  (if (and (map? lv) (map? v))
                    (jsonb-contains? lv v)
                    (= lv v))))
              r)

      (and (sequential? l) (sequential? r))
      ;; Array containment: every element of r must be in l
      (let [l-set (set l)]
        (every? l-set r))

      :else (= l r))))

(defn jsonb-contained?
  "PostgreSQL <@ operator: is left contained in right?"
  [left right]
  (jsonb-contains? right left))

(defn jsonb-exists?
  "PostgreSQL ? operator: does key exist in jsonb object?"
  [v key]
  (let [parsed (parse-jsonb v)]
    (cond
      (map? parsed) (contains? parsed key)
      (sequential? parsed) (some #{key} parsed)
      :else false)))

(defn jsonb-exists-any?
  "PostgreSQL ?| operator: does any of the keys exist?"
  [v keys]
  (let [parsed (parse-jsonb v)]
    (some #(jsonb-exists? parsed %) keys)))

(defn jsonb-exists-all?
  "PostgreSQL ?& operator: do all keys exist?"
  [v keys]
  (let [parsed (parse-jsonb v)]
    (every? #(jsonb-exists? parsed %) keys)))

;; ============================================================================
;; Operators: modification
;; ============================================================================

(defn jsonb-concat
  "PostgreSQL || operator: concatenate/merge two jsonb values."
  [left right]
  (let [l (parse-jsonb left)
        r (parse-jsonb right)]
    (cond
      (and (map? l) (map? r)) (merge l r)
      (and (sequential? l) (sequential? r)) (into (vec l) r)
      (sequential? l) (conj (vec l) r)
      (sequential? r) (into [l] r)
      :else r)))

(defn jsonb-delete-key
  "PostgreSQL - operator (text): remove key from jsonb object."
  [v key]
  (let [parsed (parse-jsonb v)]
    (if (map? parsed)
      (dissoc parsed key)
      (if (sequential? parsed)
        (vec (remove #{key} parsed))
        parsed))))

(defn jsonb-delete-keys
  "PostgreSQL - operator (text[]): remove multiple keys."
  [v keys]
  (reduce jsonb-delete-key v keys))

(defn jsonb-delete-idx
  "PostgreSQL - operator (int4): remove element by index from array."
  [v idx]
  (let [parsed (parse-jsonb v)]
    (when (sequential? parsed)
      (let [i (if (neg? idx) (+ (count parsed) idx) idx)]
        (vec (concat (take i parsed) (drop (inc i) parsed)))))))

(defn jsonb-delete-path
  "PostgreSQL #- operator: remove element at path."
  [v path]
  (let [parsed (parse-jsonb v)]
    (if (= (count path) 1)
      (jsonb-delete-key parsed (first path))
      (let [head (first path)
            child (jsonb-get parsed head)]
        (if child
          (let [updated (jsonb-delete-path child (rest path))]
            (if (map? parsed)
              (assoc parsed head updated)
              (assoc (vec parsed) (Long/parseLong head) updated)))
          parsed)))))

;; ============================================================================
;; Functions: builders
;; ============================================================================

;; ============================================================================
;; Operator registry
;; ============================================================================

(def op
  "SQL operator string → the runtime fn implementing it.

   THE registry. Every consumer — the SELECT emitter that lowers an
   operator into a datalog function-call clause, and the UPDATE SET
   interpreter that applies it eagerly to a materialised entity map —
   looks the fn up here rather than carrying its own `if`. Those two
   had already drifted: one wrapped the `->` result in
   `serialize-jsonb` and the other did not, which is precisely the
   divergence a shared table prevents. (That difference is preserved
   at the UPDATE call site for now and resolved deliberately when the
   operator semantics are fixed, not silently by this refactor.)

   Adding an operator is one entry here plus, for the ones the parser
   currently rejects, a narrowing of `sql/unsupported-op-chars`."
  {"->"  jsonb-get
   "->>" jsonb-get-text
   "#>"  jsonb-get-path
   "#>>" jsonb-get-path-text
   "@>"  jsonb-contains?
   "<@"  jsonb-contained?
   "?"   jsonb-exists?
   "?|"  jsonb-exists-any?
   "?&"  jsonb-exists-all?
   "||"  jsonb-concat
   "-"   jsonb-delete-key})

(defn jsonb-build-object
  "PostgreSQL jsonb_build_object(k1, v1, k2, v2, ...): build jsonb from pairs."
  [& args]
  (let [pairs (partition 2 args)]
    (into {} (map (fn [[k v]] [(str k) v]) pairs))))

(defn jsonb-build-array
  "PostgreSQL jsonb_build_array(v1, v2, ...): build jsonb array."
  [& args]
  (vec args))

;; ============================================================================
;; Functions: transformation
;; ============================================================================

(defn jsonb-strip-nulls
  "PostgreSQL jsonb_strip_nulls(jsonb): recursively remove null-valued keys."
  [v]
  (let [parsed (parse-jsonb v)]
    (cond
      (map? parsed)
      (into {} (keep (fn [[k v]]
                       (when (some? v)
                         [k (jsonb-strip-nulls v)])))
            parsed)

      (sequential? parsed)
      (mapv jsonb-strip-nulls parsed)

      :else parsed)))

(defn jsonb-set
  "PostgreSQL jsonb_set(target, path, new_value, create_missing?):
   Set value at path in jsonb."
  ([target path new-value] (jsonb-set target path new-value true))
  ([target path new-value create-missing?]
   (let [parsed (parse-jsonb target)
         nv (parse-jsonb new-value)
         path-vec (if (sequential? path) (vec path) [path])]
     (if (= (count path-vec) 1)
       (let [k (first path-vec)]
         (if (map? parsed)
           (if (or create-missing? (contains? parsed k))
             (assoc parsed k nv)
             parsed)
           parsed))
       (let [head (first path-vec)
             child (jsonb-get parsed head)
             updated (jsonb-set (or child {}) (rest path-vec) nv create-missing?)]
         (if (map? parsed)
           (assoc parsed head updated)
           (if (and (sequential? parsed) (integer? (parse-long head)))
             (assoc (vec parsed) (parse-long head) updated)
             parsed)))))))

(defn jsonb-insert
  "PostgreSQL jsonb_insert(target, path, new_value, insert_after?):
   Insert value at path position in jsonb array."
  ([target path new-value] (jsonb-insert target path new-value false))
  ([target path new-value insert-after?]
   ;; Simplified: delegates to jsonb-set for objects
   (jsonb-set target path new-value true)))

;; ============================================================================
;; Functions: introspection
;; ============================================================================

(defn jsonb-typeof
  "PostgreSQL jsonb_typeof(jsonb): return type name as string."
  [v]
  (let [parsed (parse-jsonb v)]
    (cond
      (nil? parsed)        "null"
      (= json-null parsed) "null"
      (map? parsed)        "object"
      (sequential? parsed) "array"
      (string? parsed)     "string"
      (number? parsed)     "number"
      (boolean? parsed)    "boolean"
      :else                "string")))

(defn jsonb-array-length
  "PostgreSQL jsonb_array_length(jsonb): return array length."
  [v]
  (let [parsed (parse-jsonb v)]
    (if (sequential? parsed) (count parsed) nil)))

(defn jsonb-object-keys
  "PostgreSQL jsonb_object_keys(jsonb): return keys of object.
   Returns a sequence (set-returning in SQL)."
  [v]
  (let [parsed (parse-jsonb v)]
    (if (map? parsed) (keys parsed) [])))

;; ============================================================================
;; Functions: set-returning (expand jsonb to rows)
;; ============================================================================

(defn jsonb-each
  "PostgreSQL jsonb_each(jsonb): expand object to (key, value) rows.
   Returns sequence of [key value] pairs where value is jsonb."
  [v]
  (let [parsed (parse-jsonb v)]
    (if (map? parsed)
      (map (fn [[k v]] [k v]) parsed)
      [])))

(defn jsonb-each-text
  "PostgreSQL jsonb_each_text(jsonb): expand object to (key, value) rows.
   Returns sequence of [key text-value] pairs."
  [v]
  (let [parsed (parse-jsonb v)]
    (if (map? parsed)
      (map (fn [[k v]] [k (if (string? v) v (json/write-value-as-string v))]) parsed)
      [])))

(defn jsonb-array-elements
  "PostgreSQL jsonb_array_elements(jsonb): expand array to element rows."
  [v]
  (let [parsed (parse-jsonb v)]
    (if (sequential? parsed) parsed [])))

(defn jsonb-array-elements-text
  "PostgreSQL jsonb_array_elements_text(jsonb): expand array to text rows."
  [v]
  (map #(if (string? %) % (json/write-value-as-string %))
       (jsonb-array-elements v)))

;; ============================================================================
;; Functions: aggregation helpers
;; These are used as reduce functions in the SQL translator's aggregate handling
;; ============================================================================

;; ============================================================================
;; Functions: misc
;; ============================================================================

(defn jsonb-pretty
  "PostgreSQL jsonb_pretty(jsonb): pretty-print jsonb."
  [v]
  (let [parsed (parse-jsonb v)]
    (json/write-value-as-string parsed (json/object-mapper {:pretty true}))))

(defn to-jsonb
  "PostgreSQL to_jsonb(any): convert any value to jsonb."
  [v]
  (cond
    (nil? v) nil
    (string? v) (try (json/read-value v mapper) (catch Exception _ v))
    :else v))

;; ============================================================================
;; Wire protocol: formatting jsonb for transport
;; ============================================================================

