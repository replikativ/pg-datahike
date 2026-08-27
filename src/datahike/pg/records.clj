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
            [datahike.pg.arrays :as arr]))

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
  [v]
  (cond
    (nil? v)        ""               ; SQL NULL → nothing between the commas
    (= :__null__ v) ""
    :else
    (let [raw (cond
                (boolean? v)   (if v "t" "f")
                (number? v)    (str v)
                (arr/array? v) (arr/to-pg-text v)
                (record? v)    (to-pg-text v)
                :else          (str v))]
      (if (field-needs-quote? raw)
        (str "\"" (str/replace raw #"[\"\\]" "\\\\$0") "\"")
        raw))))

(defn to-pg-text
  "Render a PgRecord to PG's canonical `record_out` text: `(f1,f2,...)`."
  [^PgRecord r]
  (str "(" (str/join "," (map (comp field-cell :value) (:fields r))) ")"))

(defn register-layouts!
  "Recursively register each PgRecord's field OIDs (via `reg-fn` — a 2-arg fn
   [record-text int-array-of-field-oids], i.e. PgParamCodec/registerRecordLayout)
   keyed by its canonical record_out text. Lets the anonymous-record binary
   encoder recover the per-field OIDs the text alone can't carry."
  [reg-fn ^PgRecord r]
  (reg-fn (to-pg-text r) (int-array (map :oid (:fields r))))
  (doseq [f (:fields r) :when (record? (:value f))]
    (register-layouts! reg-fn (:value f))))
