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

(def ^:private mapper (json/object-mapper {:decode-key-fn str}))

;; Canonical mapper — writes object keys in sorted order at every level, so two
;; jsonb values that differ only in key order (or whitespace) serialize identically.
;; This is what makes stored jsonb behave like Postgres jsonb rather than json:
;; jsonb DROPS key order and whitespace and keeps only the LAST of duplicate keys
;; (Jackson does last-wins on parse), so `=`, DISTINCT and GROUP BY compare by
;; structure, not by input text.
(def ^:private canonical-mapper
  (doto ^com.fasterxml.jackson.databind.ObjectMapper
   (json/object-mapper {:decode-key-fn str})
    (.configure com.fasterxml.jackson.databind.SerializationFeature/ORDER_MAP_ENTRIES_BY_KEYS
                true)))

(defn parse-jsonb
  "Parse a JSON string to a Clojure data structure.
   Returns nil for nil input, passes through non-strings."
  [v]
  (cond
    (nil? v) nil
    (string? v) (try (json/read-value v mapper)
                     (catch Exception _ v))
    :else v))

(defn canonicalize-jsonb
  "The canonical jsonb TEXT for a value — recursively key-sorted, whitespace-
   normalized, duplicate keys collapsed to the last. This is the form stored and the
   form a client reads back (Postgres normalizes jsonb on input). A string that is
   valid JSON is re-emitted canonically; a string that is not JSON is a JSON string
   scalar; a Clojure map/vector is written canonically. Returns nil for nil.

   NOTE: numeric normalization is Jackson's, not Postgres's — e.g. `1.00`/`1e3` may
   not match PG's exact jsonb numeric canonical form. Structural (key/whitespace/
   duplicate) canonicalization is exact; numeric is best-effort."
  [v]
  (when (some? v)
    (let [data (if (string? v)
                 (try (json/read-value v mapper)   ;; valid JSON text → its data
                      (catch Exception _ v))       ;; not JSON → a string scalar
                 v)]
      (json/write-value-as-string data canonical-mapper))))

(defn serialize-jsonb
  "Serialize a value to canonical jsonb TEXT for storage. Alias of
   `canonicalize-jsonb` — every write path canonicalizes, so stored jsonb is
   structure-normalized regardless of how it arrived (map/vector from a Clojure
   literal, or a `'…'::jsonb` string literal)."
  [v]
  (canonicalize-jsonb v))

;; ============================================================================
;; Operators: -> and ->> (field/element access)
;; ============================================================================

(defn jsonb-get
  "PostgreSQL -> operator: get jsonb field by key (text) or element by index (int).
   Returns jsonb (Clojure data structure).
   Returns :__null__ sentinel unchanged (NULL propagation for get-else)."
  [v key-or-idx]
  (if (= v :__null__) :__null__
      (let [parsed (parse-jsonb v)]
        (cond
          (and (map? parsed) (string? key-or-idx))
          (get parsed key-or-idx)

          (and (map? parsed) (integer? key-or-idx))
          (get parsed (str key-or-idx))

          (and (sequential? parsed) (integer? key-or-idx))
          (let [idx (if (neg? key-or-idx)
                      (+ (count parsed) key-or-idx)
                      key-or-idx)]
            (nth parsed idx nil))

          :else nil))))

(defn jsonb-get-text
  "PostgreSQL ->> operator: get field/element as text string.
   Returns a string or `:__null__` sentinel. Never returns nil — Datahike's
   function-binding clauses filter the row when the binding returns nil, but
   `foo->>missing_key` should produce SQL NULL while keeping the row."
  [v key-or-idx]
  (let [result (jsonb-get v key-or-idx)]
    (cond
      (= result :__null__) :__null__
      (some? result)
      (if (or (string? result) (number? result) (boolean? result))
        (str result)
        (json/write-value-as-string result))
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

