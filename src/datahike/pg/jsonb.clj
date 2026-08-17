(ns datahike.pg.jsonb
  "PostgreSQL jsonb type support for the PgWire compatibility layer.

   Stores jsonb values as Clojure data structures (maps, vectors, strings,
   numbers, booleans, nil) serialized to/from JSON strings via jsonista.

   Implements PostgreSQL jsonb operators and functions as pure Clojure
   functions that can be used as Datalog predicates or post-processing."
  (:require [jsonista.core :as json]
            [clojure.string :as str]
            [datahike.pg.arrays :as pg-arr]
            [datahike.pg.records :as records]))

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

(def ^:private strict-mapper
  "Parse mapper for the INPUT path. `FAIL_ON_TRAILING_TOKENS` is the one
   strictness PostgreSQL has that Jackson does not by default: `json_in`
   parses ONE complete document and rejects anything after it, while
   Jackson stops at the first value — so `SELECT '1 2'::jsonb` and
   `'{} {}'` were accepted, returning `1` and `{}`."
  (doto ^com.fasterxml.jackson.databind.ObjectMapper
   (json/object-mapper {:decode-key-fn str})
    (.configure com.fasterxml.jackson.databind.DeserializationFeature/USE_BIG_DECIMAL_FOR_FLOATS
                true)
    (.configure com.fasterxml.jackson.databind.DeserializationFeature/FAIL_ON_TRAILING_TOKENS
                true)))

(defn validate-json!
  "Parse `s` the way PostgreSQL's `json_in` does: a full RFC-8259 parse
   that RAISES on anything malformed.

   Both `json` and `jsonb` validate on input — `json` then stores the
   original bytes, `jsonb` stores the parsed tree — so this gates both.

   We were not validating at all: `parse-jsonb` catches its own parse
   error and falls back to treating the text as a JSON string scalar,
   which is right for `to_jsonb('some text')` and wrong for
   `'...'::jsonb`. So `'\"abc'::jsonb` (unclosed quote) silently became
   the string `\"abc`, and 27 statements in PostgreSQL's own
   `jsonb.sql` returned a value where PostgreSQL raises.

   Jackson is already strict about the rest — invalid escapes, raw
   control bytes, leading zeros, `NaN`, unquoted keys, trailing commas,
   uppercase literals — so this is about not SWALLOWING its verdict."
  [^String s]
  (try
    (json/read-value s strict-mapper)
    (catch Exception e
      (throw (ex-info "invalid input syntax for type json"
                      {:error :invalid-text-representation
                       :sqlstate "22P02"
                       :detail (.getMessage e)})))))

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
    ;; PostgreSQL dispatches to_json/to_jsonb on the argument's DECLARED
    ;; TYPE (json_categorize_type, json.c) — an array becomes a JSON
    ;; array, a composite becomes an object, a timestamp becomes
    ;; ISO-8601. We dispatch on the runtime Clojure class, and PgArray /
    ;; PgRecord are defrecords, so `map?` is TRUE for them: they fell
    ;; into the object branch and leaked their internals as keys
    ;; (`{":dims": [3], ":elements": …}`). These must be tested BEFORE
    ;; the map branch.
    (pg-arr/array? v) (mapv normalize-tree (:elements v))
    (records/record? v)
    (let [fs (:fields v)]
      (persistent!
       (reduce (fn [m [i f]]
                 (assoc! m (or (:name f) (str "f" (inc i))) (normalize-tree (:value f))))
               (transient {}) (map-indexed vector fs))))
    ;; A temporal value is ISO-8601, not java.util.Date's .toString
    ;; ("Sun Aug 16 19:00:16 PDT 2026").
    ;; PostgreSQL's JsonEncodeDateTime renders the value AS STORED — a
    ;; `timestamp` carries no zone, so it must not be shifted. Formatting
    ;; in the default zone reproduces what PostgreSQL prints for the same
    ;; value; converting to UTC moved it by the offset.
    ;; The column output path renders an instant by stringifying it (UTC)
    ;; and stripping the trailing Z, so a stored timestamp reads back as
    ;; the wall clock it was written with. Use the SAME convention here,
    ;; or `to_jsonb(t)` and `SELECT t` disagree about the same value —
    ;; formatting in the system zone shifted it by the local offset.
    (inst? v)    (let [^java.time.Instant inst (if (instance? java.time.Instant v)
                                                 v
                                                 (.toInstant ^java.util.Date v))]
                   (str/replace (str inst) "Z" ""))
    (instance? java.time.LocalDateTime v) (str v)
    (instance? java.time.LocalDate v)     (str v)
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

(def ^:dynamic *json-style*
  "Object punctuation for the writer.

   PostgreSQL renders the two families differently, and the difference
   is only in objects: `jsonb` emits `\": \"` after a key and `\", \"`
   between pairs, while a `json` value that PostgreSQL BUILT (rather
   than echoed verbatim) is compact — `{\"a\":1}`. Arrays are `[1, 2]`
   in both."
  {:pair ": " :sep ", "})

(defn- emit!
  [^StringBuilder sb v]
  (cond
    (nil? v)     (.append sb "null")
    (= json-null v) (.append sb "null")
    (map? v)     (do (.append sb \{)
                     (reduce (fn [first? k]
                               (when-not first? (.append sb (:sep *json-style*)))
                               (append-json-string! sb (str k))
                               (.append sb (:pair *json-style*))
                               (emit! sb (get v k))
                               false)
                             true
                             (sort pg-key-cmp (map str (keys v))))
                     (.append sb \}))
    ;; `(:sep …)`, not a hardcoded ", ": the two families differ on ARRAY
    ;; punctuation as well as object punctuation. `to_json(ARRAY[1,2])`
    ;; is `[1,2]` in PostgreSQL and was `[1, 2]` here.
    (sequential? v) (do (.append sb \[)
                        (reduce (fn [first? x]
                                  (when-not first? (.append sb (:sep *json-style*)))
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
                 ;; A value that did NOT come from JSON text still has to
                 ;; go through the value model: PgRecord and PgArray are
                 ;; defrecords, so `map?` is true for them and `emit!`
                 ;; wrote their INTERNALS —
                 ;; `json_agg(t)` over a whole-row reference produced
                 ;; `[{":fields": null, ":type-oid": null}, …]`.
                 ;; `to_json`/`to_jsonb` normalise before calling here,
                 ;; which is why they looked correct and every other
                 ;; caller did not.
                 (normalize-tree v))
          sb (StringBuilder.)]
      (emit! sb data)
      (.toString sb))))

(defn serialize-json
  "Canonical text in the `json` family's punctuation — compact objects,
   as PostgreSQL renders a json value it constructed
   (`json_strip_nulls('{\"a\":1,\"z\":null}')` -> `{\"a\":1}`)."
  [v]
  (binding [*json-style* {:pair ":" :sep ","}]
    (serialize-jsonb v)))

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
            missing :__null__
            ;; An array index can arrive as the STRING "1" — the chain
            ;; emitter carries the ident as text, and PostgreSQL's own
            ;; `#>` path is text[] too. Coerce for sequential targets so
            ;; `d->'arr'->>1` reaches element 1 instead of falling
            ;; through to "missing".
            key-or-idx (if (and (sequential? parsed)
                                (string? key-or-idx)
                                (re-matches #"-?\d+" key-or-idx))
                         (parse-long key-or-idx)
                         key-or-idx)]
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

(defn- ->path-seq
  "The step list for `#>` / `#>>`.

   PostgreSQL types the right operand `text[]`, and it reaches us either
   as a PgArray record or — for the common literal form `'{a,b,1}'` — as
   the raw array TEXT. Reducing over that string walked its characters,
   so `d #> '{a,b,1}'` descended into `{`, `a`, `,` … and found nothing.

   Steps stay strings: `jsonb-get` already treats a string step as a key
   and PostgreSQL's own `#>` uses text elements, resolving `1` against
   an array by position through the same path."
  [path]
  (cond
    (record? path)     (mapv str (or (:elements path) []))
    (sequential? path) (mapv str path)
    (string? path)     (let [t (str/trim path)]
                         (if (and (str/starts-with? t "{") (str/ends-with? t "}"))
                           (let [inner (subs t 1 (dec (count t)))]
                             (if (str/blank? inner)
                               []
                               (mapv str/trim (str/split inner #","))))
                           [t]))
    (nil? path)        []
    :else              [path]))

(defn jsonb-get-path
  "PostgreSQL #> operator: extract jsonb at path."
  [v path]
  (let [steps (->path-seq path)]
    (reduce (fn [acc k]
              (if (= acc :__null__)
                :__null__
                ;; A numeric step indexes an array; jsonb-get dispatches
                ;; on the step's type, so hand it a long when it is one.
                (jsonb-get acc (if (re-matches #"-?\d+" (str k))
                                 (parse-long (str k))
                                 k))))
            (parse-jsonb v) steps)))

(defn jsonb-get-path-text
  "PostgreSQL #>> operator: extract text at path.
   Returns `:__null__` sentinel when the path doesn't exist — returning nil
   would make Datahike's function-binding clause filter the row."
  [v path]
  (let [result (jsonb-get-path v path)]
    (cond
      (nil? result)         :__null__
      (= :__null__ result)  :__null__
      (= json-null result)  :__null__
      (string? result)      result
      :else                 (serialize-jsonb result))))

;; ============================================================================
;; Operators: containment and existence
;; ============================================================================

(defn jsonb-eq?
  "PostgreSQL's jsonb `=`, which compares VALUES and is numeric-scale
   INSENSITIVE: `'1.00'::jsonb = '1'::jsonb` is true even though the two
   render differently. Comparing our canonical text alone is therefore
   too STRICT — it is a canonical form for structure, not for numbers,
   because PostgreSQL keeps display scale on purpose.

   Text equality is the fast path and is sound in one direction: equal
   canonical text implies equal values, so only differing text has to be
   parsed. That confines the cost to exactly the case that was wrong.

   Structural comparison is then just `=` on the parsed trees: numbers
   are uniformly BigDecimal in the value model, and Clojure's `=` on
   BigDecimal is scale-insensitive, which is `numeric_eq`."
  [a b]
  (or (= a b)
      (= (parse-jsonb a) (parse-jsonb b))))

(defn jsonb-ne?
  "PostgreSQL's jsonb `<>`. Not `not=` on the canonical text: that
   answers TRUE for `1.00` vs `1`, which are the same jsonb value."
  [a b]
  (not (jsonb-eq? a b)))

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

(defn- ->key-seq
  "The key list for `?|` / `?&`.

   PostgreSQL types their right operand `text[]`, so it arrives here as
   a PgArray RECORD. `some`/`every?` over a record iterate its MAP
   ENTRIES, so every key test compared a `[:elem-type …]` pair against a
   string and `doc ?| array['a','b']` answered false for everything."
  [ks]
  (cond
    (record? ks)     (or (:elements ks) [])
    (string? ks)     [ks]
    (sequential? ks) ks
    (nil? ks)        []
    :else            [ks]))

(defn jsonb-exists-any?
  "PostgreSQL ?| operator: does any of the keys exist?"
  [v keys]
  (let [parsed (parse-jsonb v)]
    (boolean (some #(jsonb-exists? parsed %) (->key-seq keys)))))

(defn jsonb-exists-all?
  "PostgreSQL ?& operator: do all keys exist?"
  [v keys]
  (let [parsed (parse-jsonb v)]
    (every? #(jsonb-exists? parsed %) (->key-seq keys))))

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
  {"="   jsonb-eq?
   "->"  jsonb-get
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

;; ---------------------------------------------------------------------------
;; The `json_*` family — text-faithful, unlike `jsonb_*`
;; ---------------------------------------------------------------------------

(defn- emit-json-value!
  "Render one value in `json` (not jsonb) form. Values reuse the jsonb
   writer — the difference between the families is in the OBJECT
   punctuation and in what is kept, not in how a scalar looks."
  [^StringBuilder sb v]
  (emit! sb (normalize-tree v)))

(defn json-build-object
  "PostgreSQL `json_build_object(k1, v1, …)`.

   NOT `jsonb_build_object` with a different name. `json` is the
   text-faithful type, so this preserves ARGUMENT ORDER and KEEPS
   DUPLICATE KEYS, where the jsonb form sorts and takes the last:

     json_build_object('b',1,'a',2,'a',3) -> {\"b\" : 1, \"a\" : 2, \"a\" : 3}
     jsonb_build_object(same)             -> {\"a\": 3, \"b\": 1}

   Which is why it builds TEXT straight from the arguments instead of
   going through a Clojure map — a map cannot hold either property.
   PostgreSQL's separator here is `\" : \"`, spaces on both sides
   (json.c's composite_to_json), not the jsonb writer's `\": \"`."
  [& args]
  (let [sb (StringBuilder.)]
    (.append sb \{)
    (doseq [[i [k v]] (map-indexed vector (partition 2 args))]
      (when (pos? i) (.append sb ", "))
      (append-json-string! sb (str k))
      (.append sb " : ")
      (emit-json-value! sb v))
    (.append sb \})
    (.toString sb)))

(defn json-build-array
  "PostgreSQL `json_build_array(v1, …)`. Arrays render identically in
   both families, so only the element order matters and it is preserved
   by construction."
  [& args]
  (let [sb (StringBuilder.)]
    (.append sb \[)
    (doseq [[i v] (map-indexed vector args)]
      (when (pos? i) (.append sb ", "))
      (emit-json-value! sb v))
    (.append sb \])
    (.toString sb)))

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
      ;; A JSON null is `json-null`, not nil — `some?` was true for it,
      ;; so nothing was stripped. (Arrays keep their nulls in
      ;; PostgreSQL; only object keys are removed.)
      (into {} (keep (fn [[k v]]
                       (when-not (or (nil? v) (= json-null v))
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
  "PostgreSQL `to_jsonb(anyelement)` / `to_json` — convert a SQL value
   INTO a json value.

   It does NOT parse its argument. `to_jsonb('{\"a\":1}'::text)` is the
   json STRING `\"{\\\"a\\\":1}\"`, not an object — the text is a text
   value being wrapped, not a document being read. We parsed it, so a
   text column holding JSON silently became a structure.

   Only an argument that is ALREADY json/jsonb passes through, and the
   caller decides that from the column type, since at runtime both are
   Clojure strings.

   Returns canonical TEXT, so a json string renders quoted (`\"x\"`) the
   way PostgreSQL prints it."
  ([v] (to-jsonb v false))
  ([v already-json?]
   (cond
     (nil? v)          nil
     (= :__null__ v)   :__null__
     already-json?     (serialize-jsonb v)
     :else             (let [sb (StringBuilder.)]
                         (emit! sb (normalize-tree v))
                         (.toString sb)))))

(defn to-json
  "PostgreSQL `to_json` / `row_to_json` — `to_jsonb`'s sibling in the
   `json` family, which differs ONLY in punctuation: compact objects and
   arrays where jsonb spaces them. `to_json(ARRAY[1,2])` is `[1,2]` and
   `to_jsonb(ARRAY[1,2])` is `[1, 2]`; both were rendering the jsonb
   way because the two names shared one implementation."
  ([v] (to-json v false))
  ([v already-json?]
   (binding [*json-style* {:pair ":" :sep ","}]
     (to-jsonb v already-json?))))

;; ============================================================================
;; Wire protocol: formatting jsonb for transport
;; ============================================================================

