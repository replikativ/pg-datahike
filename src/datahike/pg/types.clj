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

;; Array OIDs — every scalar type has a paired `T[]` OID. PG catalog
;; rows: `SELECT typname, oid, typelem FROM pg_type WHERE typelem <> 0`.
;; We only materialise the types we actually emit; others can follow.
(def oid-bool-array        1000)
(def oid-bytea-array       1001)
(def oid-name-array        1003)
(def oid-int2-array        1005)
(def oid-int4-array        1007)
(def oid-text-array        1009)
(def oid-int8-array        1016)
(def oid-float4-array      1021)
(def oid-float8-array      1022)
(def oid-oid-array         1028)
(def oid-varchar-array     1015)
(def oid-bpchar-array      1014)
(def oid-date-array        1182)
(def oid-time-array        1183)
(def oid-timestamp-array   1115)
(def oid-timestamptz-array 1185)
(def oid-numeric-array     1231)
(def oid-uuid-array        2951)
(def oid-json-array        199)
(def oid-jsonb-array       3807)

(def element-oid->array-oid
  "Scalar element OID → corresponding T[] OID."
  {oid-bool        oid-bool-array
   oid-bytea       oid-bytea-array
   oid-name        oid-name-array
   oid-int2        oid-int2-array
   oid-int4        oid-int4-array
   oid-text        oid-text-array
   oid-int8        oid-int8-array
   oid-float4      oid-float4-array
   oid-float8      oid-float8-array
   oid-oid         oid-oid-array
   oid-varchar     oid-varchar-array
   oid-bpchar      oid-bpchar-array
   oid-date        oid-date-array
   oid-time        oid-time-array
   oid-timestamp   oid-timestamp-array
   oid-timestamptz oid-timestamptz-array
   oid-numeric     oid-numeric-array
   oid-uuid        oid-uuid-array
   oid-json        oid-json-array
   oid-jsonb       oid-jsonb-array})

(def array-oid->element-oid
  "Inverse of element-oid->array-oid: T[] OID → T OID."
  (into {} (map (fn [[e a]] [a e])) element-oid->array-oid))

(def elem-kw->oid
  "Element-type keyword (as stored on PgArray :elem-type) → OID."
  {:bool        oid-bool
   :bytea       oid-bytea
   :name        oid-name
   :int2        oid-int2
   :int4        oid-int4
   :text        oid-text
   :int8        oid-int8
   :float4      oid-float4
   :float8      oid-float8
   :oid         oid-oid
   :varchar     oid-varchar
   :bpchar      oid-bpchar
   :date        oid-date
   :time        oid-time
   :timestamp   oid-timestamp
   :timestamptz oid-timestamptz
   :numeric     oid-numeric
   :uuid        oid-uuid
   :json        oid-json
   :jsonb       oid-jsonb})

(def oid->elem-kw
  "Inverse of elem-kw->oid."
  (into {} (map (fn [[k v]] [v k])) elem-kw->oid))

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
   [oid-jsonb     "jsonb"     -1  "b"]
   ;; Array types — one per scalar with a paired T[] OID. typtype="b"
   ;; like scalars; the typelem linkage is exposed via element-oid
   ;; lookups at query time (see datahike.pg.sql.catalog).
   [oid-bool-array        "_bool"        -1 "b"]
   [oid-bytea-array       "_bytea"       -1 "b"]
   [oid-name-array        "_name"        -1 "b"]
   [oid-int2-array        "_int2"        -1 "b"]
   [oid-int4-array        "_int4"        -1 "b"]
   [oid-text-array        "_text"        -1 "b"]
   [oid-int8-array        "_int8"        -1 "b"]
   [oid-float4-array      "_float4"      -1 "b"]
   [oid-float8-array      "_float8"      -1 "b"]
   [oid-oid-array         "_oid"         -1 "b"]
   [oid-varchar-array     "_varchar"     -1 "b"]
   [oid-bpchar-array      "_bpchar"      -1 "b"]
   [oid-date-array        "_date"        -1 "b"]
   [oid-time-array        "_time"        -1 "b"]
   [oid-timestamp-array   "_timestamp"   -1 "b"]
   [oid-timestamptz-array "_timestamptz" -1 "b"]
   [oid-numeric-array     "_numeric"     -1 "b"]
   [oid-uuid-array        "_uuid"        -1 "b"]
   [oid-json-array        "_json"        -1 "b"]
   [oid-jsonb-array       "_jsonb"       -1 "b"]])

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
   oid-oid        4
   ;; Array types are always variable-length on the wire.
   oid-bool-array        -1
   oid-bytea-array       -1
   oid-name-array        -1
   oid-int2-array        -1
   oid-int4-array        -1
   oid-text-array        -1
   oid-int8-array        -1
   oid-float4-array      -1
   oid-float8-array      -1
   oid-oid-array         -1
   oid-varchar-array     -1
   oid-bpchar-array      -1
   oid-date-array        -1
   oid-time-array        -1
   oid-timestamp-array   -1
   oid-timestamptz-array -1
   oid-numeric-array     -1
   oid-uuid-array        -1
   oid-json-array        -1
   oid-jsonb-array       -1})

;; ============================================================================
;; Convenience functions
;; ============================================================================

(defn oid-for-dh-type
  "Return the PostgreSQL type OID for a Datahike valueType keyword."
  [vtype]
  (get dh-type->oid vtype oid-text))

(def ^:private oid->dh-type-map
  "Inverse of dh-type->oid for the OIDs that materialize-set-op!'s
   schema inference cares about. Several Datahike types collapse to
   the same OID (string/keyword/symbol/bytes/number/tuple → text or
   float8), so this is biased toward the most useful inverse
   (text → :db.type/string, float8 → :db.type/double, int8 → long)."
  {oid-bool        :db.type/boolean
   oid-int2        :db.type/long
   oid-int4        :db.type/long
   oid-int8        :db.type/long
   oid-float4      :db.type/double
   oid-float8      :db.type/double
   oid-numeric     :db.type/bigdec
   oid-text        :db.type/string
   oid-varchar     :db.type/string
   oid-bpchar      :db.type/string
   oid-name        :db.type/string
   oid-date        :db.type/instant
   oid-time        :db.type/instant
   oid-timestamp   :db.type/instant
   oid-timestamptz :db.type/instant
   oid-uuid        :db.type/uuid
   oid-bytea       :db.type/bytes})

(defn dh-type-for-oid
  "Inverse lookup: PG OID → Datahike valueType. Returns nil for
   unknown OIDs (caller decides fallback)."
  [oid]
  (when oid (get oid->dh-type-map (long oid))))

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
   :timestamp, :uuid, :bytes, :array, or nil. :date and :time are
   checked before :timestamp so callers can emit the
   display-appropriate Java type (LocalDate / LocalTime vs Instant).
   Any type-name ending in `[]` classifies as :array; the element
   category can be resolved by recursing on the prefix."
  [sql-type-name]
  (when sql-type-name
    (let [;; Strip type arguments: "timestamp(6)" → "timestamp"
          base (clojure.string/replace sql-type-name #"\s*\([^)]*\)" "")]
      (cond
        (clojure.string/ends-with? base "[]") :array
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

(defn cast-array-elem-kw
  "For a SQL type name like `int[]`, return the element-type keyword
   used by PgArray :elem-type (:int8, :text, :bool, etc.). Returns
   nil for non-array target types."
  [sql-type-name]
  (when sql-type-name
    (let [base (clojure.string/replace sql-type-name #"\s*\([^)]*\)" "")]
      (when (clojure.string/ends-with? base "[]")
        (let [elem (clojure.string/trim (subs base 0 (- (count base) 2)))
              cat (cast-category elem)]
          (case cat
            :integer :int8
            :float   :float8
            :text    :text
            :boolean :bool
            :date    :date
            :time    :time
            :timestamp :timestamp
            :uuid    :uuid
            :text))))))

(defn infer-oid-from-value
  "Infer a PostgreSQL type OID from a Clojure runtime value."
  [v]
  (cond
    ;; PgArray → the T[] OID corresponding to its element type.
    ;; Kept as a record-instance check (via class name) to avoid
    ;; a require-loop; callers have the concrete record. A miss
    ;; falls back to text[] (1009).
    (and (some? v)
         (= "datahike.pg.arrays.PgArray" (.getName (class v))))
    (get element-oid->array-oid
         (get elem-kw->oid (:elem-type v))
         oid-text-array)
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
