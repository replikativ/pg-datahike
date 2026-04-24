(ns datahike.pg.types
  "PostgreSQL type system registry for the PgWire compatibility layer.

   Centralizes all type mappings between PostgreSQL OIDs, SQL type names,
   Datahike value types, and wire protocol format codes.

   Authoritative source: PostgreSQL 19devel src/include/catalog/pg_type.dat

   Three directions of mapping:
   1. SQL name → Datahike type (for CREATE TABLE)
   2. Datahike type → PG OID (for wire protocol RowDescription)
   3. PG OID → SQL name (for format_type() and information_schema)
   4. SQL name → category (for CAST type classification)"
  (:import [datahike.pg PgWireServer]))

;; ============================================================================
;; PostgreSQL type OIDs (from pg_type.dat)
;; ============================================================================

(def oid-bool      16)
(def oid-bytea      17)
(def oid-int8       20)
(def oid-int2       21)
(def oid-int4       23)
(def oid-text       25)
(def oid-oid        26)
(def oid-json      114)
(def oid-float4    700)
(def oid-float8    701)
(def oid-varchar  1043)
(def oid-bpchar   1042)
(def oid-name       19)
(def oid-date     1082)
(def oid-time     1083)
(def oid-timestamp 1114)
(def oid-timestamptz 1184)
(def oid-interval 1186)
(def oid-numeric  1700)
(def oid-uuid     2950)
(def oid-jsonb    3802)

;; ============================================================================
;; SQL name → Datahike value type (for CREATE TABLE DDL)
;; ============================================================================

(def sql-name->dh-type
  "Map SQL type names (lowercased) to Datahike :db/valueType keywords.
   Covers all common SQL and PostgreSQL type name variants."
  {;; String types
   "text"              :db.type/string
   "varchar"           :db.type/string
   "character varying" :db.type/string
   "char"              :db.type/string
   "character"         :db.type/string
   "bpchar"            :db.type/string
   "name"              :db.type/string
   "citext"            :db.type/string
   ;; Integer types
   "integer"           :db.type/long
   "int"               :db.type/long
   "int2"              :db.type/long
   "int4"              :db.type/long
   "int8"              :db.type/long
   "bigint"            :db.type/long
   "smallint"          :db.type/long
   "serial"            :db.type/long
   "serial2"           :db.type/long
   "serial4"           :db.type/long
   "bigserial"         :db.type/long
   "serial8"           :db.type/long
   "smallserial"       :db.type/long
   ;; Float types
   "double precision"  :db.type/double
   "double"            :db.type/double
   "float"             :db.type/double
   "float4"            :db.type/float
   "float8"            :db.type/double
   "real"              :db.type/float
   "numeric"           :db.type/bigdec
   "decimal"           :db.type/bigdec
   ;; Boolean
   "boolean"           :db.type/boolean
   "bool"              :db.type/boolean
   ;; Date/time
   "timestamp"         :db.type/instant
   "timestamp without time zone" :db.type/instant
   "timestamp with time zone"    :db.type/instant
   "timestamptz"       :db.type/instant
   "date"              :db.type/instant
   "time"              :db.type/instant
   "time without time zone"      :db.type/instant
   "time with time zone"         :db.type/instant
   ;; Binary
   "bytea"             :db.type/bytes
   ;; UUID
   "uuid"              :db.type/uuid
   ;; JSON (stored as string in Datahike)
   "json"              :db.type/string
   "jsonb"             :db.type/string
   ;; OID / system (mapped to long)
   "oid"               :db.type/long
   "regclass"          :db.type/long
   "regtype"           :db.type/long})

;; ============================================================================
;; Datahike value type → PostgreSQL OID (for wire protocol)
;; ============================================================================

(def pg-name->oid
  "Map a PostgreSQL type name (the string stored on :pg/type when
   the column's original SQL type doesn't match the Datahike
   valueType 1:1 — e.g. `date` vs `timestamp` both collapse to
   :db.type/instant) back to its wire OID. Used by
   infer-param-oid-for-column so pgjdbc's ParameterDescription sees
   the SQL-declared type, not our internal reduction."
  {"bool"        oid-bool
   "int2"        oid-int2
   "int4"        oid-int4
   "int8"        oid-int8
   "text"        oid-text
   "varchar"     oid-varchar
   "float4"      oid-float4
   "float8"      oid-float8
   "numeric"     oid-numeric
   "date"        oid-date
   "time"        oid-time
   "timestamp"   oid-timestamp
   "timestamptz" oid-timestamptz
   "uuid"        oid-uuid
   "json"        oid-json
   "jsonb"       oid-jsonb
   "bytea"       oid-bytea})

(def dh-type->oid
  "Map Datahike :db/valueType to PostgreSQL type OID for wire protocol."
  {:db.type/string  oid-text
   :db.type/long    oid-int8
   :db.type/boolean oid-bool
   :db.type/double  oid-float8
   :db.type/float   oid-float4
   :db.type/bigint  oid-int8
   :db.type/bigdec  oid-numeric
   :db.type/instant oid-timestamp
   :db.type/uuid    oid-uuid
   :db.type/keyword oid-text
   :db.type/symbol  oid-text
   :db.type/ref     oid-int8
   :db.type/bytes   oid-text
   :db.type/number  oid-float8
   :db.type/tuple   oid-text})

;; ============================================================================
;; Datahike value type → PostgreSQL type name (for information_schema)
;; ============================================================================

(def dh-type->pg-name
  "Map Datahike :db/valueType to PostgreSQL type name string."
  {:db.type/string  "text"
   :db.type/long    "bigint"
   :db.type/boolean "boolean"
   :db.type/double  "double precision"
   :db.type/float   "real"
   :db.type/bigint  "bigint"
   :db.type/bigdec  "numeric"
   :db.type/instant "timestamp without time zone"
   :db.type/uuid    "uuid"
   :db.type/keyword "text"
   :db.type/symbol  "text"
   :db.type/ref     "bigint"
   :db.type/bytes   "bytea"
   :db.type/number  "double precision"
   :db.type/tuple   "text"})

;; ============================================================================
;; PostgreSQL OID → type name (for format_type() catalog function)
;; ============================================================================

(def oid->pg-name
  "Map PostgreSQL type OID to canonical type name string."
  {oid-bool       "boolean"
   oid-int2       "smallint"
   oid-int4       "integer"
   oid-int8       "bigint"
   oid-float4     "real"
   oid-float8     "double precision"
   oid-numeric    "numeric"
   oid-text       "text"
   oid-varchar    "character varying"
   oid-bpchar     "character"
   oid-name       "name"
   oid-bytea      "bytea"
   oid-date       "date"
   oid-time       "time without time zone"
   oid-timestamp  "timestamp without time zone"
   oid-timestamptz "timestamp with time zone"
   oid-interval   "interval"
   oid-uuid       "uuid"
   oid-json       "json"
   oid-jsonb      "jsonb"
   oid-oid        "oid"})

;; ============================================================================
;; SQL CAST type classification (for translate-cast-expr)
;; ============================================================================
;; These sets use exact type names from SQL grammar — no substring matching.

(def cast-integer-types
  "SQL type names that cast to integer (Clojure long)."
  #{"integer" "int" "int2" "int4" "int8" "bigint" "smallint"
    "serial" "serial2" "serial4" "serial8" "bigserial" "smallserial"})

(def cast-float-types
  "SQL type names that cast to floating point (Clojure double)."
  #{"double precision" "double" "float" "float4" "float8"
    "real" "numeric" "decimal" "dec"})

(def cast-text-types
  "SQL type names that cast to text (Clojure string)."
  #{"text" "varchar" "character varying" "character" "char" "bpchar" "name"})

(def cast-boolean-types
  "SQL type names that cast to boolean."
  #{"boolean" "bool"})

(def cast-timestamp-types
  "SQL type names that cast to timestamp/instant."
  #{"timestamp" "timestamp without time zone" "timestamp with time zone"
    "timestamptz" "date" "time" "time without time zone"
    "time with time zone" "interval"})

(def cast-date-types
  "SQL type names that cast to a DATE (no time component).
   Needed because PG's DATE/TIME/TIMESTAMP all serialize differently
   in text format — e.g. DATE is '2017-03-13', TIME is '14:25:48',
   TIMESTAMP is '2017-03-13 14:25:48'. Datahike stores them all as
   :db.type/instant, so the distinction is only preserved through
   CAST expressions (where we know the target display type)."
  #{"date"})

(def cast-time-types
  "SQL type names that cast to a TIME (no date component)."
  #{"time" "time without time zone" "time with time zone"})

(def cast-uuid-types
  "SQL type names that cast to UUID."
  #{"uuid"})

(def cast-bytes-types
  "SQL type names that cast to a byte array."
  #{"bytea"})

(def cast-bit-types
  "SQL type names that cast to a PG bit string — emitted as a string of
   ASCII '0'/'1' characters in PG's text format. pgjdbc's testgetBadBoolean
   asserts `29::bit(4)` serializes as \"1101\"."
  #{"bit"})

;; ============================================================================
;; Catalog data: pg_type rows for virtual table materialization
;; ============================================================================

(def pg-type-catalog
  "Common PostgreSQL types for the pg_type virtual table.
   Each entry: [oid typname typlen typtype]"
  [[oid-bool      "bool"       1  "b"]
   [oid-bytea     "bytea"     -1  "b"]
   [oid-int8      "int8"       8  "b"]
   [oid-int2      "int2"       2  "b"]
   [oid-int4      "int4"       4  "b"]
   [oid-text      "text"      -1  "b"]
   [oid-oid       "oid"        4  "b"]
   [oid-json      "json"      -1  "b"]
   [oid-float4    "float4"     4  "b"]
   [oid-float8    "float8"     8  "b"]
   [oid-varchar   "varchar"   -1  "b"]
   [oid-bpchar    "bpchar"    -1  "b"]
   [oid-name      "name"      64  "b"]
   [oid-date      "date"       4  "b"]
   [oid-time      "time"       8  "b"]
   [oid-timestamp "timestamp"  8  "b"]
   [oid-timestamptz "timestamptz" 8 "b"]
   [oid-interval  "interval"  16  "b"]
   [oid-numeric   "numeric"   -1  "b"]
   [oid-uuid      "uuid"      16  "b"]
   [oid-jsonb     "jsonb"     -1  "b"]])

;; ============================================================================
;; Type size for wire protocol RowDescription
;; ============================================================================

(def oid->wire-size
  "Map OID to type size for RowDescription's typlen field.
   Positive = fixed size in bytes, -1 = variable length."
  {oid-bool       1
   oid-int2       2
   oid-int4       4
   oid-int8       8
   oid-float4     4
   oid-float8     8
   oid-text      -1
   oid-varchar   -1
   oid-bpchar    -1
   oid-name      64
   oid-bytea     -1
   oid-date       4
   oid-time       8
   oid-timestamp  8
   oid-timestamptz 8
   oid-interval  16
   oid-numeric   -1
   oid-uuid      16
   oid-json      -1
   oid-jsonb     -1
   oid-oid        4})

;; ============================================================================
;; Convenience functions
;; ============================================================================

(defn oid-for-dh-type
  "Return the PostgreSQL type OID for a Datahike valueType keyword."
  [vtype]
  (get dh-type->oid vtype oid-text))

(defn pg-name-for-dh-type
  "Return the PostgreSQL type name string for a Datahike valueType keyword."
  [vtype]
  (get dh-type->pg-name vtype "text"))

(defn dh-type-for-sql-name
  "Return the Datahike valueType for a SQL type name (lowercased)."
  [sql-name]
  (get sql-name->dh-type sql-name :db.type/string))

(defn format-type
  "PostgreSQL format_type(oid, typmod) — return type name for an OID."
  [type-oid _typmod]
  (get oid->pg-name (if (number? type-oid) (long type-oid) 0) "text"))

(defn wire-size
  "Return the wire protocol type size for an OID."
  [oid]
  (get oid->wire-size oid (short -1)))

(defn cast-category
  "Classify a SQL type name for CAST handling.
   Returns :integer, :float, :text, :boolean, :date, :time,
   :timestamp, :uuid, :bytes, or nil. :date and :time are checked
   before :timestamp so callers can emit the display-appropriate Java
   type (LocalDate / LocalTime vs Instant)."
  [sql-type-name]
  (when sql-type-name
    (let [;; Strip type arguments: "timestamp(6)" → "timestamp"
          base (clojure.string/replace sql-type-name #"\s*\([^)]*\)" "")]
      (cond
        (contains? cast-integer-types base)   :integer
        (contains? cast-float-types base)     :float
        (contains? cast-text-types base)      :text
        (contains? cast-boolean-types base)   :boolean
        (contains? cast-date-types base)      :date
        (contains? cast-time-types base)      :time
        (contains? cast-timestamp-types base) :timestamp
        (contains? cast-uuid-types base)      :uuid
        (contains? cast-bytes-types base)     :bytes
        (contains? cast-bit-types base)       :bit
        :else nil))))

(defn infer-oid-from-value
  "Infer a PostgreSQL type OID from a Clojure runtime value."
  [v]
  (cond
    (instance? clojure.lang.Ratio v) oid-float8
    (instance? Long v)    oid-int8
    (instance? Integer v) oid-int4
    (integer? v)          oid-int8
    (instance? Double v)  oid-float8
    (instance? Float v)   oid-float4
    (float? v)            oid-float8
    (boolean? v)          oid-bool
    (inst? v)             oid-timestamp
    (uuid? v)             oid-uuid
    :else                 oid-text))
