(ns datahike.pg.records
  "Composite / record values (PG `record` 2249 and named composite types).

   A `PgRecord` carries its field values plus each field's OID and the
   record's own type OID (2249 for an anonymous `ROW(...)`, or a named
   composite type's OID). The OIDs are needed for the binary wire codec
   (PG's record binary format is [nfields][per-field oid,len,data]); the
   values render to PG's canonical `record_out` text for text-format
   clients. Mirrors the role `datahike.pg.arrays/PgArray` plays for arrays."
  (:refer-clojure :exclude [record?])
  (:require [clojure.string :as str]
            [datahike.pg.arrays :as arr]
            [datahike.pg.types :as types]))

;; type-oid : the record's own OID (2249 anonymous, else a composite OID)
;; fields   : vector of {:oid <field-oid> :value <scalar|nil|PgArray|PgRecord>}
(defrecord PgRecord [type-oid fields])

(defn record? [v] (instance? PgRecord v))

(defn make-record
  "Build a PgRecord from a seq of field values, inferring each field's OID
   via `oid-fn` (a value->oid function, e.g. types/infer-oid-from-value).
   `type-oid` defaults to 2249 (anonymous record)."
  ([oid-fn values] (make-record oid-fn values 2249))
  ([oid-fn values type-oid]
   (->PgRecord type-oid
               (mapv (fn [v] {:oid (oid-fn v) :value v}) values))))

(defn named-record
  "A PgRecord whose fields carry NAMES, for the composite values SQL can
   select a field out of: `(information_schema._pg_expandarray(a)).n`.
   An anonymous ROW has no names -- PostgreSQL calls those fields f1, f2,
   … and `field-value` falls back to that."
  [names+oids+values]
  (->PgRecord 2249
              (mapv (fn [[n oid v]] {:name n :oid oid :value v})
                    names+oids+values)))

(defn field-value
  "The value of the field `fname` of a record: by name when the record
   has names, else by PostgreSQL's positional `fN` spelling. Returns
   ::absent when the record has no such field, which the caller reports
   as 42703."
  [^PgRecord r ^String fname]
  (let [fields (:fields r)
        lower (str/lower-case fname)
        by-name (first (keep-indexed (fn [i f]
                                       (when (and (:name f)
                                                  (= lower (str/lower-case (:name f))))
                                         i))
                                     fields))
        idx (or by-name
                (when-let [[_ n] (re-matches #"(?i)f(\d+)" lower)]
                  (dec (Long/parseLong n))))]
    (if (and idx (< -1 idx (count fields)))
      (:value (nth fields idx))
      ::absent)))

(defn with-layout
  "Retag an anonymous record with a named composite type and its declared
   field OIDs. Composite casts need this metadata after expression lowering:
   the values alone cannot distinguish, for example, `text` from `point`,
   because both currently use a string JVM carrier."
  [^PgRecord record type-oid field-oids]
  (when-not (= (count (:fields record)) (count field-oids))
    (throw (ex-info "cannot cast record to a composite type with a different number of columns"
                    {:error :cannot-coerce
                     :sqlstate "42846"})))
  (assoc record
         :type-oid type-oid
         :fields (mapv (fn [field oid] (assoc field :oid oid))
                       (:fields record) field-oids)))

(declare to-pg-text)

(defn- field-needs-quote?
  "PG quotes a record field if it is the empty string or contains any of
   `( ) , \" \\` or whitespace (rowtypes.c record_out)."
  [^String s]
  (or (= "" s)
      (boolean (re-find #"[(),\"\\\s]" s))))

(defn- field-cell
  "Render one field value to its record_out cell. NULL → bare empty (no
   quotes); everything else renders then is quoted+escaped when needed."
  [v oid]
  (cond
    (nil? v)        ""               ; SQL NULL → nothing between the commas
    (= :__null__ v) ""
    :else
    ;; record_out calls each FIELD type's output function -- the field
    ;; OID is what tells money from numeric and date from timestamp.
    (let [raw (cond
                (boolean? v)   (if v "t" "f")
                (arr/array? v) (arr/to-pg-text v)
                (record? v)    (to-pg-text v)
                :else          (types/->pg-text v oid))]
      (if (field-needs-quote? raw)
        (str "\"" (str/replace raw #"[\"\\]" "\\\\$0") "\"")
        raw))))

(defn to-pg-text
  "Render a PgRecord to PG's canonical `record_out` text: `(f1,f2,...)`."
  [^PgRecord r]
  (str "(" (str/join "," (map #(field-cell (:value %) (:oid %)) (:fields r))) ")"))

(defn register-layouts!
  "Recursively register each PgRecord's field OIDs (via `reg-fn` — a 2-arg fn
   [record-text int-array-of-field-oids], i.e. PgParamCodec/registerRecordLayout)
   keyed by its canonical record_out text. Lets the anonymous-record binary
   encoder recover the per-field OIDs the text alone can't carry."
  [reg-fn ^PgRecord r]
  (reg-fn (to-pg-text r)
          (int-array (map (comp types/oid->wire-int :oid) (:fields r))))
  (doseq [f (:fields r) :when (record? (:value f))]
    (register-layouts! reg-fn (:value f))))

;; ── Canonical text back to a record ─────────────────────────────────────
;;
;; A record that goes through a MATERIALISED relation -- a derived table,
;; a CTE, a set operation -- is stored as its canonical text, the way an
;; array column is (`:pg/array-elem`), because a speculative db's columns
;; hold Datahike scalars. Rendering it is then right by construction;
;; reading a FIELD out of it again needs the text parsed, and the field
;; names and OIDs the text does not carry, which the column's
;; `:pg/record-fields` layout records.

(defn from-pg-text
  "The raw field cells of PG's canonical record text `(a,b,\"c,d\",)`, in
   order. A quoted cell is unescaped (`\"\"` → `\"`, `\\x` → `x`); an
   UNQUOTED empty cell is nil, which is how `record_in` reads SQL NULL;
   a quoted empty cell is the empty string. Returns nil when `s` is not
   record text, so a caller can fall through to its other cases."
  [^String s]
  (let [s (str/trim (or s ""))]
    (when (and (str/starts-with? s "(") (str/ends-with? s ")") (>= (count s) 2))
      (let [body (subs s 1 (dec (count s)))
            n (count body)
            ;; `quoted?` is the cell's, `in?` the position's: `\"\"` is one
            ;; escaped quote INSIDE a quoted cell and an empty quoted cell
            ;; when it is the whole cell, and only the state tells them
            ;; apart. An empty cell that was never quoted is NULL.
            cell-value (fn [^StringBuilder sb quoted?]
                         (when (or quoted? (pos? (.length sb))) (str sb)))]
        (if (zero? n)
          []
          (loop [i 0, sb (StringBuilder.), quoted? false, in? false, out []]
            (if (= i n)
              (conj out (cell-value sb quoted?))
              (let [c (.charAt body i)]
                (cond
                  in?
                  (cond
                    (and (= c \") (< (inc i) n) (= \" (.charAt body (inc i))))
                    (recur (+ i 2) (.append sb \") quoted? true out)

                    (= c \")
                    (recur (inc i) sb quoted? false out)

                    (and (= c \\) (< (inc i) n))
                    (recur (+ i 2) (.append sb (.charAt body (inc i))) quoted? true out)

                    :else
                    (recur (inc i) (.append sb c) quoted? true out))

                  (= c \")
                  (recur (inc i) sb true true out)

                  (= c \,)
                  (recur (inc i) (StringBuilder.) false false
                         (conj out (cell-value sb quoted?)))

                  (and (= c \\) (< (inc i) n))
                  (recur (+ i 2) (.append sb (.charAt body (inc i))) quoted? false out)

                  :else
                  (recur (inc i) (.append sb c) quoted? false out))))))))))

(defn layout
  "A record's [name oid] pairs -- what canonical text loses."
  [^PgRecord r]
  (mapv (fn [f] [(:name f) (:oid f)]) (:fields r)))

(defn layout->text
  "A layout as one string, for a schema entity: `name:oid` per field,
   comma separated, the name left empty for an anonymous ROW's field."
  [lay]
  (str/join "," (map (fn [[n oid]] (str (or n "") ":" (or oid 25))) lay)))

(defn text->layout
  "Inverse of `layout->text`."
  [^String s]
  (when (seq s)
    (mapv (fn [part]
            (let [i (str/last-index-of part ":")
                  nm (subs part 0 i)
                  oid (subs part (inc i))]
              [(when (seq nm) nm) (parse-long oid)]))
          (str/split s #","))))

(defn text->record
  "Rebuild a PgRecord from canonical text. Each field is read with its
   OID's input function when `lay` names one and the type has one;
   otherwise the cell stays text -- a value that renders identically and
   only loses the field's declared type. nil when `s` is not record text."
  [^String s lay parse-fn]
  (when-let [cells (from-pg-text s)]
    (->PgRecord 2249
                (vec (map-indexed
                      (fn [i cell]
                        (let [[nm oid] (nth lay i [nil types/oid-text])
                              oid (or oid types/oid-text)
                              v (when (some? cell)
                                  (or (try (parse-fn oid cell) (catch Exception _ nil))
                                      cell))]
                          (cond-> {:oid oid :value v}
                            nm (assoc :name nm))))
                      cells)))))
