(ns datahike.pg.types
  "PostgreSQL type system registry for the PgWire compatibility layer.

   Centralizes all type mappings between PostgreSQL OIDs, SQL type names,

   Centralizes all type mappings between PostgreSQL OIDs, SQL type names,
   Datahike value types, and wire protocol format codes.

   Authoritative source: PostgreSQL 19devel src/include/catalog/pg_type.dat

   Three directions of mapping:
   1. SQL name → Datahike type (for CREATE TABLE)
   2. Datahike type → PG OID (for wire protocol RowDescription)
   3. PG OID → SQL name (for format_type() and information_schema)
   4. SQL name → category (for CAST type classification)"
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import [datahike.pg PgWireServer]))

;; ============================================================================
;; PostgreSQL type OIDs (from pg_type.dat)
;; ============================================================================

(def oid-bool      16)
(def oid-bytea      17)
(def oid-char       18)   ;; PG internal "char" (1 byte) — pg_type.typtype etc.
(def oid-int8       20)
(def oid-int2       21)
(def oid-int4       23)
(def oid-text       25)
(def oid-oid        26)
(def oid-tid        27)
(def oid-json      114)
(def oid-money     790)
(def oid-float4    700)
(def oid-float8    701)
(def oid-varchar  1043)
(def oid-bpchar   1042)
(def oid-name       19)
(def oid-void     2278)
(def oid-date     1082)
(def oid-time     1083)
(def oid-timestamp 1114)
(def oid-timestamptz 1184)
(def oid-interval 1186)
(def oid-numeric  1700)
(def oid-uuid     2950)
(def oid-regclass  2205)
(def oid-regtype  2206)
(def oid-regnamespace 4089)
(def oid-bit      1560)
(def oid-varbit   1562)
(def oid-jsonb    3802)
(def oid-pg-lsn   3220)

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
(def oid-money-array       791)
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
   oid-money       oid-money-array
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
   :money       oid-money
   :jsonb       oid-jsonb})

(def oid->elem-kw
  "Inverse of elem-kw->oid."
  (into {} (map (fn [[k v]] [v k])) elem-kw->oid))

(def oid-preserving-pg-name
  "OIDs whose datahike valueType (string/long) would report a DIFFERENT OID on
   read-back. A materialised column with one of these inferred OIDs carries
   :pg/type = this name so oid-infer round-trips the original OID rather than the
   storage-type default. char(18) must stay char (not text) — asyncpg's typeinfo
   binary-decodes typtype to bytes b'c'; oid(26) must stay oid (not int8)."
  {18 "char", 26 "oid", oid-name "name", oid-money "money",
   oid-interval "interval", oid-bit "bit", oid-varbit "varbit"})

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
   "money"             :db.type/bigdec
   ;; Boolean
   "boolean"           :db.type/boolean
   "bool"              :db.type/boolean
   ;; Date/time
   "timestamp"         :db.type/instant
   "timestamp without time zone" :db.type/instant
   "timestamp with time zone"    :db.type/instant
   "timestamptz"       :db.type/instant
   "date"              :db.type/instant
   ;; time-of-day: store the normalized text (a bare time can't be a
   ;; java.util.Date/Instant, and parse-timestamp-string rejects it).
   ;; Consistent with `timetz`, which already falls through to string.
   ;; The :pg/type "time" hint still drives the wire OID (1083/1266).
   "time"              :db.type/string
   "timetz"            :db.type/string
   "time without time zone"      :db.type/string
   "time with time zone"         :db.type/string
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
;; SQL name → element keyword (for array column types)
;; ============================================================================

(def sql-name->elem-kw
  "SQL type name (lowercased, no `(p,s)` parens) → the array-element
   keyword used on `PgArray :elem-type`. More specific than
   `sql-name->dh-type` because integer width matters at the wire
   layer (`int4[]` → `_int4`, OID 1007; `int8[]` → `_int8`, OID 1016)
   even though both reduce to `:db.type/long` in Datahike storage."
  {"text"              :text
   "varchar"           :varchar
   "character varying" :varchar
   "char"              :bpchar
   "character"         :bpchar
   "bpchar"            :bpchar
   "name"              :name
   "citext"            :text
   "integer"           :int4
   "int"               :int4
   "int4"              :int4
   "int2"              :int2
   "smallint"          :int2
   "int8"              :int8
   "bigint"            :int8
   "double precision"  :float8
   "double"            :float8
   "float"             :float8
   "float8"            :float8
   "float4"            :float4
   "real"              :float4
   "numeric"           :numeric
   "money"             :money
   "decimal"           :numeric
   "boolean"           :bool
   "bool"              :bool
   "timestamp"                       :timestamp
   "timestamp without time zone"     :timestamp
   "timestamp with time zone"        :timestamptz
   "timestamptz"                     :timestamptz
   "date"              :date
   "time"              :time
   "time without time zone"          :time
   "time with time zone"             :time
   "uuid"              :uuid
   "json"              :json
   "jsonb"             :jsonb
   "bytea"             :bytea
   "oid"               :oid})

(defn parse-array-type-name
  "Parse a SQL type string for arrays. Returns
   `{:elem <kw> :pg-name \"_T\" :ndim N}` or `nil` if not an array.

   Strips `(p,s)` typmod, tolerates `int ARRAY` / `int ARRAY[3]`
   (PG's alternative array syntax — the size is informational,
   PG doesn't enforce it). Element-name is matched against
   `sql-name->elem-kw` so we cover every scalar in our registry.

   Multi-dim is reflected by `:ndim`; we accept arbitrary N at parse
   time (DDL can choose to reject N>1 if it isn't ready to handle
   them, but the parser doesn't lose information).

       \"int[]\"        → {:elem :int4, :pg-name \"_int4\", :ndim 1}
       \"text[][]\"     → {:elem :text, :pg-name \"_text\", :ndim 2}
       \"int ARRAY\"    → {:elem :int4, :pg-name \"_int4\", :ndim 1}
       \"int ARRAY[3]\" → {:elem :int4, :pg-name \"_int4\", :ndim 1}
       \"numeric(p,s)[]\" → {:elem :numeric, :pg-name \"_numeric\", :ndim 1}"
  [^String s]
  (let [norm (-> s
                 str/lower-case
                 str/trim
                 (str/replace #"\s*\([^)]*\)" "")  ;; strip typmod (p,s)
                 ;; "int ARRAY" / "int ARRAY[3]" → "int[]" (size ignored)
                 (str/replace #"\s+array\s*\[\s*\d*\s*\]" "[]")
                 (str/replace #"\s+array$" "[]")
                 str/trim)
        m (re-matches #"^([a-z][a-z0-9 ]*?)\s*((?:\[\s*\d*\s*\])+)\s*$" norm)]
    (when m
      (let [elem-name (str/trim (nth m 1))
            brackets  (nth m 2)
            ndim      (count (re-seq #"\[" brackets))
            elem-kw   (get sql-name->elem-kw elem-name)]
        (when elem-kw
          {:elem elem-kw
           :pg-name (str "_" (name elem-kw))
           :ndim ndim})))))

;; ============================================================================
;; Datahike value type → PostgreSQL OID (for wire protocol)
;; ============================================================================

(def pg-name->oid
  "Map a PostgreSQL type name (the string stored on :pg/type when
   the column's original SQL type doesn't match the Datahike
   valueType 1:1 — e.g. `date` vs `timestamp` both collapse to
   :db.type/instant) back to its wire OID. Used by
   infer-param-oid-for-column so pgjdbc's ParameterDescription sees
   the SQL-declared type, not our internal reduction.

   Includes paired `_T` entries for every scalar — `_int4` →
   oid-int4-array, etc. — so an array column's `:pg/type` (set
   to `_int4` etc. by translate-create-table for `int[]` columns)
   resolves directly to its array OID."
  (merge
   {"bool"        oid-bool
    "int2"        oid-int2
    "int4"        oid-int4
    "int8"        oid-int8
    "text"        oid-text
    "name"        oid-name
    "money"       oid-money
    "varchar"     oid-varchar
    "bpchar"      oid-bpchar
    "float4"      oid-float4
    "float8"      oid-float8
    "numeric"     oid-numeric
    "date"        oid-date
    "time"        oid-time
    "timestamp"   oid-timestamp
    "timestamptz" oid-timestamptz
    "interval"    oid-interval
    "uuid"        oid-uuid
    "json"        oid-json
    "jsonb"       oid-jsonb
    "pg_lsn"      oid-pg-lsn
    "oid"         oid-oid
    "regclass"    oid-regclass
    "regtype"     oid-regtype
    "regnamespace" oid-regnamespace
    "tid"         oid-tid
    "char"        oid-char
    "bytea"       oid-bytea
    ;; bit / bit varying. Datahike has no bit type, so these columns
    ;; store PG's text form (the digit run) as :db.type/string — the
    ;; :pg/type hint is the only thing that keeps them from advertising
    ;; text (25) instead of bit (1560) / varbit (1562).
    "bit"         oid-bit
    "varbit"      oid-varbit}
   ;; Array entries: "_T" → array OID. Generated from elem-kw->oid
   ;; so adding a new scalar type only needs three rows
   ;; (elem-kw->oid, element-oid->array-oid, sql-name->elem-kw).
   (into {}
         (keep (fn [[kw oid]]
                 (when-let [arr-oid (element-oid->array-oid oid)]
                   [(str "_" (name kw)) arr-oid])))
         elem-kw->oid)))

(def oid->pg-type-marker
  "Inverse of `pg-name->oid`: wire OID to the canonical string persisted in
   `:pg/type`.  This is deliberately distinct from `oid->pg-name`, whose
   values are PostgreSQL display names such as `integer` and `double
   precision`; persisted hints must be resolvable by `pg-name->oid` again."
  (into {} (map (fn [[pg-name oid]] [oid pg-name])) pg-name->oid))

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

(def ^:private oid->pg-name-scalar
  "Scalar type OID -> canonical name. See `oid->pg-name` for the full map."
  {oid-bool       "boolean"
   oid-int2       "smallint"
   oid-int4       "integer"
   oid-int8       "bigint"
   oid-float4     "real"
   oid-float8     "double precision"
   oid-numeric    "numeric"
   oid-regclass   "regclass"
   oid-regtype    "regtype"
   oid-regnamespace "regnamespace"
   oid-bit        "bit"
   oid-varbit     "bit varying"
   oid-text       "text"
   oid-varchar    "character varying"
   oid-bpchar     "character"
   oid-name       "name"
   oid-money      "money"
   oid-bytea      "bytea"
   oid-date       "date"
   oid-time       "time without time zone"
   oid-timestamp  "timestamp without time zone"
   oid-timestamptz "timestamp with time zone"
   oid-interval   "interval"
   oid-uuid       "uuid"
   oid-json       "json"
   oid-jsonb      "jsonb"
   oid-pg-lsn     "pg_lsn"
   oid-oid        "oid"
   oid-tid        "tid"}

  ;; Array OIDs, derived rather than listed so the two maps cannot drift.
  ;; Without them `format_type(1007, -1)` fell through to "text", so an
  ;; `int[]` column reported the right OID (1007) and the wrong NAME.
  ;; PG spells these `integer[]`.
  )

(def oid->pg-name
  "Map PostgreSQL type OID to canonical type name string, arrays included."
  (into oid->pg-name-scalar
        (keep (fn [[arr-oid elem-oid]]
                (when-let [n (get oid->pg-name-scalar elem-oid)]
                  [arr-oid (str n "[]")])))
        array-oid->element-oid))

;; ============================================================================
;; SQL CAST type classification (for translate-cast-expr)
;; ============================================================================
;; These sets use exact type names from SQL grammar — no substring matching.

(def cast-integer-types
  "SQL type names that cast to integer (Clojure long)."
  #{"integer" "int" "int2" "int4" "int8" "bigint" "smallint"
    "serial" "serial2" "serial4" "serial8" "bigserial" "smallserial"
    ;; `oid` is an unsigned-32 integer value-wise — `$1::oid` / `$1::oid[]`
    ;; must coerce to long(s), not text (asyncpg's introspection binds
    ;; `oid = any($1::oid[])`). The reg* alias types are oid-backed but
    ;; TEXT-displayed (regtype → 'int4'), so they are deliberately excluded.
    "oid"})

(def integer-type-width
  "Which of PostgreSQL's three integer widths a cast target names.
   `cast-category` folds them all to :integer, but the width decides
   both the range check and the name in the overflow message, so it has
   to be recovered from the type string. `oid` is unsigned-32 but PG
   reports its overflows against the same 32-bit boundary."
  {"int2" :int2 "smallint" :int2 "serial2" :int2 "smallserial" :int2
   "int8" :int8 "bigint" :int8 "serial8" :int8 "bigserial" :int8
   "integer" :int4 "int" :int4 "int4" :int4 "serial" :int4 "serial4" :int4
   "oid" :int4})

(def oid->integer-width
  "The integer width an OID denotes, or absent for a non-integer type.
   Absence is meaningful: it is how arithmetic decides an expression has
   left integer territory."
  {oid-int2 :int2 oid-int4 :int4 oid-int8 :int8})

(def integer-width-limits
  "`[min max type-name]` per width; the name is the one PostgreSQL uses
   in \"<name> out of range\"."
  {:int2 [-32768 32767 "smallint"]
   :int4 [-2147483648 2147483647 "integer"]
   :int8 [Long/MIN_VALUE Long/MAX_VALUE "bigint"]})

(def cast-float-types
  "SQL type names that cast to floating point (Clojure double)."
  #{"double precision" "double" "float" "float4" "float8"
    "real"})

(def cast-numeric-types
  "SQL type names that cast to arbitrary-precision decimal (Clojure
   bigdec / java.math.BigDecimal). Kept distinct from floats so a
   `::numeric` cast preserves scale and precision (e.g. 0.001000) and
   reports OID 1700 — asyncpg uses binary numeric, not float8."
  #{"numeric" "decimal" "dec"})

(def cast-money-types
  "SQL names for PostgreSQL's fixed-scale money type."
  #{"money"})

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
    "time with time zone"})

(def cast-interval-types
  "SQL type names that denote an interval. Intervals currently retain their
   text carrier, but they must remain a distinct SQL type for operator and
   RowDescription inference."
  #{"interval"})

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
  #{"time" "timetz" "time without time zone" "time with time zone"})

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

(def cast-varbit-types
  "SQL type names for BIT VARYING. A distinct type from `bit` (OID 1562
   vs 1560) with different width coercion: `bit(n)` zero-pads on the
   right, `bit varying(n)` truncates but never pads."
  #{"varbit" "bit varying"})

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
   [oid-tid       "tid"        6  "b"]
   [oid-json      "json"      -1  "b"]
   [oid-money     "money"      8  "b"]
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
   [oid-bit       "bit"       -1  "b"]
   [oid-varbit    "varbit"    -1  "b"]
   [oid-jsonb     "jsonb"     -1  "b"]
   [oid-pg-lsn    "pg_lsn"     8  "b"]
   [oid-regclass  "regclass"    4  "b"]
   [oid-regtype   "regtype"     4  "b"]
   [oid-regnamespace "regnamespace" 4 "b"]
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
   [oid-money-array       "_money"       -1 "b"]
   [oid-jsonb-array       "_jsonb"       -1 "b"]])

;; ============================================================================
;; Type size for wire protocol RowDescription
;; ============================================================================

(def oid->wire-size
  "Map OID to type size for RowDescription's typlen field.
   Positive = fixed size in bytes, -1 = variable length."
  {oid-bool       1
   oid-char       1
   oid-int2       2
   oid-int4       4
   oid-int8       8
   oid-float4     4
   oid-float8     8
   oid-text      -1
   oid-varchar   -1
   oid-bpchar    -1
   oid-name      64
   oid-money      8
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
   oid-pg-lsn     8
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
   oid-money-array       -1
   oid-jsonb-array       -1})

;; ============================================================================
;; Type resolution — PostgreSQL's category / preferred / implicit-cast
;; tables, and the algorithm that reads them
;; ============================================================================

(def oid->category
  "`typcategory` from pg_type.dat, for the types we carry.

   PostgreSQL resolves the type of CASE / COALESCE / GREATEST / LEAST /
   UNION, and of an operator's arguments, from three catalog facts and
   nothing else: a type's CATEGORY, whether it is its category's
   PREFERRED type, and which coercions are IMPLICIT. Guessing at the
   answer instead -- `coalesce(a, b)` takes a's type -- is wrong the
   moment the arguments differ: PostgreSQL answers float8 for
   `coalesce(numeric, float8)`, which prints 1.5 where numeric prints
   1.50."
  {oid-bool        :B
   oid-int2        :N  oid-int4    :N  oid-int8   :N
   oid-float4      :N  oid-float8  :N  oid-numeric :N  oid-money :N  oid-oid :N
   oid-text        :S  oid-varchar :S  oid-bpchar :S  oid-name :S  oid-char :S
   oid-date        :D  oid-time    :D  oid-timestamp :D  oid-timestamptz :D
   oid-interval    :T
   oid-bit         :V  oid-varbit  :V
   oid-uuid        :U  oid-bytea   :U  oid-json   :U  oid-jsonb :U  oid-tid :U
   oid-pg-lsn      :U})

(def preferred-oids
  "`typispreferred`. One per category among the types we carry: a
   preferred type is never given up during resolution."
  #{oid-bool oid-float8 oid-oid oid-text oid-timestamptz oid-interval oid-varbit})

(def implicit-casts
  "castsource -> #{casttarget} for the pg_cast.dat entries whose
   castcontext is 'i' (implicit). Only implicit casts count for type
   resolution -- `float8 -> numeric` exists but is ASSIGNMENT, which is
   exactly why numeric loses to float8 and not the other way round."
  {oid-int2      #{oid-int4 oid-int8 oid-float4 oid-float8 oid-numeric oid-oid}
   oid-int4      #{oid-int8 oid-float4 oid-float8 oid-numeric oid-oid}
   oid-int8      #{oid-float4 oid-float8 oid-numeric oid-oid}
   oid-float4    #{oid-float8}
   oid-numeric   #{oid-float4 oid-float8}
   oid-text      #{oid-bpchar oid-varchar oid-name}
   oid-varchar   #{oid-text oid-bpchar oid-name}
   oid-bpchar    #{oid-text oid-varchar oid-name}
   oid-name      #{oid-text}
   oid-char      #{oid-text}
   oid-date      #{oid-timestamp oid-timestamptz}
   oid-time      #{oid-interval}
   oid-timestamp #{oid-timestamptz}
   oid-bit       #{oid-varbit}
   oid-varbit    #{oid-bit}})

(defn implicit-coercible?
  "Can `from` be coerced to `to` implicitly? `can_coerce_type` with
   COERCION_IMPLICIT, for a single scalar argument."
  [from to]
  (or (= from to)
      (contains? (get implicit-casts from #{}) to)))

(defn select-common-type
  "PostgreSQL's `select_common_type` (parse_coerce.c), verbatim in
   structure:

     - all inputs the same type -> that type
     - otherwise walk the rest, holding a candidate:
         * a nil (unknown) input is skipped
         * a different CATEGORY is an error -- the constructs cannot be
           matched
         * the candidate gives way only if it is NOT its category's
           preferred type, the candidate coerces implicitly to the new
           type, and the new type does NOT coerce back
     - all-unknown resolves to text

   `oids` may contain nils for operands whose type we could not infer;
   they are the UNKNOWN of the algorithm. Returns nil when every input
   is unknown AND `unknown->text?` is false, so a caller that would
   rather keep its own answer can.

   `context` is the SQL construct's name for the error message; when it
   is nil a category mismatch returns nil instead of raising, which is
   what PostgreSQL's own callers do when they want to test rather than
   resolve."
  ([oids] (select-common-type oids nil true))
  ([oids context] (select-common-type oids context true))
  ([oids context unknown->text?]
   (let [known (remove nil? oids)]
     (cond
       (empty? known) (when unknown->text? oid-text)
       (apply = known) (first known)
       :else
       (loop [[n & more] (rest known)
              ptype (first known)]
         (if (nil? n)
           ptype
           (let [pcat (get oid->category ptype)
                 ncat (get oid->category n)
                 ppref (contains? preferred-oids ptype)]
             (cond
               (= n ptype) (recur more ptype)
               ;; A type we have no category for cannot be reasoned
               ;; about; keep the candidate rather than guess.
               (or (nil? pcat) (nil? ncat)) (recur more ptype)
               (not= ncat pcat)
               (if context
                 (throw (ex-info (str context " types "
                                      (get oid->pg-name ptype "?") " and "
                                      (get oid->pg-name n "?")
                                      " cannot be matched")
                                 {:error :datatype-mismatch
                                  :sqlstate "42804"}))
                 nil)
               (and (not ppref)
                    (implicit-coercible? ptype n)
                    (not (implicit-coercible? n ptype)))
               (recur more n)
               :else (recur more ptype)))))))))

(defn- coercion-targets
  "The types a value of `oid` can reach implicitly, itself included."
  [oid]
  (conj (get implicit-casts oid #{}) oid))

(def ^:private no-comparison-operator-oids
  "Types among the OIDs we expose that have no ordinary comparison
   operators in PostgreSQL.

   This is deliberately a small bridge to the generated operator catalog:
   implicit coercibility can tell us whether an operator candidate could
   accept two different input types, but it cannot tell us that a candidate
   exists in the first place. `json = json` is the load-bearing example --
   identical input types, but PostgreSQL defines equality only for jsonb.
   Arrays inherit their element type's comparison support, so json[] is
   excluded as well."
  #{oid-json oid-json-array})

(defn comparison-compatible?
  "Would PostgreSQL find an operator for a comparison between these two
   types?

   `oper_select_candidate` comes down to this for the cross-type
   comparison families: with no exact match, a candidate survives only
   if BOTH arguments coerce to its argument type implicitly. So the test
   is whether the two types have any implicit target in common.

   `boolean = integer` is the case this exists to catch -- PostgreSQL
   raises 42883 rather than answering false. `date = time` is the one
   that shows why a category test is not enough: both are category D and
   neither coerces to the other, so PostgreSQL has no candidate there
   either, while `date = timestamp` resolves through timestamp.

   Either side unknown means an untyped literal, which takes the other
   side's type; a type absent from the tables stays lenient.

   `op` is accepted now so this API has the shape of PostgreSQL's real
   operator lookup. The bridge table above currently excludes types with no
   comparison family at all; a generated pg_operator slice can make this
   lookup fully operator-specific."
  ([a b] (comparison-compatible? '= a b))
  ([_op a b]
   (and (not (contains? no-comparison-operator-oids a))
        (not (contains? no-comparison-operator-oids b))
        (or (nil? a) (nil? b) (= a b)
            (nil? (get oid->category a)) (nil? (get oid->category b))
            (boolean (seq (set/intersection (coercion-targets a)
                                            (coercion-targets b))))))))

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
   oid-money       :db.type/bigdec
   oid-text        :db.type/string
   oid-varchar     :db.type/string
   oid-bpchar      :db.type/string
   oid-name        :db.type/string
   oid-bit         :db.type/string
   oid-varbit      :db.type/string
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

;; ============================================================================
;; NUMERIC typmod — encodes precision and scale per PG's atttypmod scheme

;; PG encodes NUMERIC's typmod as:
;;   typmod = ((precision << 16) | (scale & 0x7ff)) + VARHDRSZ
;;   VARHDRSZ = 4
;; A typmod of -1 means "no precision specified" (PG default for plain
;; NUMERIC). Decoding: subtract 4, scale = signed low 11 bits, precision =
;; upper 16 bits. We mirror PG exactly so clients (pgjdbc, psycopg2,
;; Metabase) see the same value they'd see on a real PG.
(def ^:const var-hdr-sz 4)

(defn encode-numeric-typmod
  "Encode PG NUMERIC(precision, scale) → atttypmod integer.
   Returns -1 when both precision and scale are nil (unconstrained)."
  [precision scale]
  (if (and (nil? precision) (nil? scale))
    -1
    (let [p (or precision 0)
          s (or scale 0)]
      (+ (bit-or (bit-shift-left p 16) (bit-and s 0x7ff))
         var-hdr-sz))))

(defn decode-numeric-scale
  "Sign-extend PostgreSQL's packed 11-bit NUMERIC scale.

   This also decodes the parser-only 1048..2047 representation emitted by
   `negative-numeric-scale-rule`; ordinary positive scales 0..1000 are
   unchanged."
  [packed]
  (let [x (bit-and (long packed) 0x7ff)]
    (- (bit-xor x 1024) 1024)))

(defn base-type-name-of
  "The SQL type name with its `(…)` modifier stripped, lower-cased."
  [type-str]
  (-> (str type-str) (str/replace #"\s*\([^)]*\)" "") str/trim str/lower-case))

(defn parse-char-length
  "The `n` of `varchar(n)` / `char(n)`, or nil for an unmodified text
   type. Only the length-carrying names qualify -- `text` has no limit."
  [type-str]
  (when (contains? #{"varchar" "character varying" "char" "character" "bpchar"}
                   (base-type-name-of type-str))
    (some-> (re-find #"\(\s*(\d+)\s*\)" (str type-str)) second Integer/parseInt)))

(defn parse-bit-length
  "The `n` of `bit(n)` / `bit varying(n)`, or nil when no modifier was
   declared. PostgreSQL stores this directly as atttypmod (unlike character
   types, which add VARHDRSZ)."
  [type-str]
  (when (contains? #{"bit" "varbit" "bit varying"}
                   (base-type-name-of type-str))
    (some-> (re-find #"\(\s*(\d+)\s*\)" (str type-str)) second Integer/parseInt)))

(defn decode-numeric-typmod
  "Inverse of `encode-numeric-typmod`. Returns `[precision scale]` or
   `[nil nil]` for typmod -1 (unconstrained NUMERIC)."
  [typmod]
  (if (or (nil? typmod) (neg? typmod))
    [nil nil]
    (let [adjusted (- typmod var-hdr-sz)]
      [(bit-shift-right adjusted 16)
       (decode-numeric-scale adjusted)])))

(defn parse-numeric-args
  "Parse the JSqlParser ColDataType string `\"NUMERIC (10, 2)\"` (or
   `\"DECIMAL(10,2)\"` etc.) into `[precision scale]`. Returns
   `[nil nil]` when no parens present (plain `NUMERIC`)."
  [type-str]
  (when type-str
    (if-let [[_ args-str] (re-find #"(?i)^\s*(?:numeric|decimal)\s*\(([^)]*)\)" type-str)]
      (let [parts (mapv str/trim (str/split args-str #","))
            p (try (Long/parseLong (nth parts 0 "")) (catch Exception _ nil))
            s (try (Long/parseLong (nth parts 1 "0")) (catch Exception _ nil))]
        [p (if s (decode-numeric-scale s) 0)])
      [nil nil])))

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
  [type-oid typmod]
  (let [oid (cond
              (number? type-oid) (long type-oid)
              (string? type-oid) (try (Long/parseLong type-oid)
                                      (catch Exception _ 0))
              :else 0)
        tm (cond
             (number? typmod) (long typmod)
             (string? typmod) (try (Long/parseLong typmod)
                                   (catch Exception _ -1))
             :else -1)
        base (if (= oid oid-bpchar)
               "bpchar"
               (get oid->pg-name oid "text"))]
    (if (neg? tm)
      base
      (cond
        (= oid oid-numeric)
        (let [[precision scale] (decode-numeric-typmod tm)]
          (if precision (format "numeric(%d,%d)" precision scale) base))

        (= oid oid-varchar) (format "character varying(%d)" (max 0 (- tm 4)))
        (= oid oid-bpchar)  (format "character(%d)" (max 0 (- tm 4)))
        (= oid oid-bit)     (format "bit(%d)" tm)
        (= oid oid-varbit)  (format "bit varying(%d)" tm)
        (= oid oid-time)    (format "time(%d) without time zone" tm)
        (= oid 1266)        (format "time(%d) with time zone" tm)
        (= oid oid-timestamp)   (format "timestamp(%d) without time zone" tm)
        (= oid oid-timestamptz) (format "timestamp(%d) with time zone" tm)
        :else base))))

(defn wire-size
  "Return the wire protocol type size for an OID."
  [oid]
  (get oid->wire-size oid (short -1)))

(defn cast-category
  "Classify a SQL type name for CAST handling.
   Returns :integer, :float, :text, :boolean, :date, :time,
   :timestamp, :interval, :uuid, :bytes, :array, :json, :jsonb, or nil. :date and :time are
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
        ;; `json` / `jsonb`. Absent here, `::jsonb` fell through every
        ;; arm of cast-scalar and returned its input UNCHANGED — the
        ;; cast was a no-op that only set the wire OID, so a malformed
        ;; literal was accepted and a well-formed one was not
        ;; canonicalised.
        (= base "json")                       :json
        (= base "jsonb")                      :jsonb
        (contains? cast-integer-types base)   :integer
        (contains? cast-numeric-types base)   :numeric
        (contains? cast-money-types base)     :money
        (contains? cast-float-types base)     :float
        (contains? cast-text-types base)      :text
        (contains? cast-boolean-types base)   :boolean
        (contains? cast-date-types base)      :date
        (contains? cast-time-types base)      :time
        (contains? cast-timestamp-types base) :timestamp
        (contains? cast-interval-types base)  :interval
        (contains? cast-uuid-types base)      :uuid
        (contains? cast-bytes-types base)     :bytes
        (contains? cast-varbit-types base)    :varbit
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
            :numeric :numeric
            :text    :text
            :boolean :bool
            :date    :date
            :time    :time
            :timestamp :timestamp
            :uuid    :uuid
            :text))))))

(defrecord PgLsn [^java.math.BigInteger value]
  Object
  (toString [_]
    (let [mask (java.math.BigInteger. "FFFFFFFF" 16)
          hi (.shiftRight value 32)
          lo (.and value mask)]
      (format "%X/%08X" hi lo))))

(defn pg-lsn? [x] (instance? PgLsn x))
(defn pg-lsn [^java.math.BigInteger value] (->PgLsn value))

(defrecord PgNumericSpecial [kind])

(defn numeric-special
  "NaN / +-Infinity as a NUMERIC. BigDecimal cannot represent any of
   them, and PostgreSQL has had all three since 14, so they need a
   carrier of their own -- the same shape PgBit and PgArray already use
   for values Clojure has no native equivalent of.

   `kind` is :nan, :inf or :-inf."
  [kind]
  (->PgNumericSpecial kind))

(defn numeric-special? [x] (instance? PgNumericSpecial x))

(def nan-numeric (numeric-special :nan))
(def inf-numeric (numeric-special :inf))
(def -inf-numeric (numeric-special :-inf))

;; Datahike's :db.type/bigdec quite deliberately accepts BigDecimal only,
;; while PostgreSQL NUMERIC also has NaN and +/-Infinity. Keep those values
;; in the ordinary AVET/EAVT numeric index by assigning them three BigDecimal
;; representatives outside PostgreSQL's finite NUMERIC domain. PostgreSQL's
;; on-disk numeric weight is bounded (a finite value cannot reach 10^200000),
;; so this mapping is injective over values the SQL API is allowed to accept.
;; The representatives preserve PostgreSQL's total order:
;;
;;     NaN > Infinity > every finite value > -Infinity
;;
;; They are an SQL storage detail, not a new Datahike value type. Decode them
;; whenever a numeric datom crosses back into expression evaluation.
(def ^:private numeric-special-storage-scale -200000)

(def ^:private numeric-special->storage-map
  {:nan  (java.math.BigDecimal. (java.math.BigInteger/valueOf 3)
                                numeric-special-storage-scale)
   :inf  (java.math.BigDecimal. (java.math.BigInteger/valueOf 2)
                                numeric-special-storage-scale)
   :-inf (java.math.BigDecimal. (java.math.BigInteger/valueOf -2)
                                numeric-special-storage-scale)})

(def ^:private numeric-storage->special-map
  (into {} (map (fn [[kind value]] [value (numeric-special kind)]))
        numeric-special->storage-map))

(defn numeric-value->storage
  "Encode a PgNumericSpecial for a :db.type/bigdec attribute. Finite
   numeric values pass through unchanged."
  [v]
  (if (numeric-special? v)
    (get numeric-special->storage-map (:kind v))
    v))

(defn numeric-special-storage?
  "True when v is one of the three reserved at-rest NUMERIC values."
  [v]
  (and (instance? java.math.BigDecimal v)
       (contains? numeric-storage->special-map v)))

(defn numeric-storage->value
  "Decode a reserved numeric BigDecimal to its SQL value. Ordinary values
   (and the SQL NULL sentinel used inside translated queries) pass through."
  [v]
  (if (instance? java.math.BigDecimal v)
    (get numeric-storage->special-map v v)
    v))

(defn numeric-special->double ^double [x]
  (case (:kind x)
    :nan Double/NaN
    :inf Double/POSITIVE_INFINITY
    :-inf Double/NEGATIVE_INFINITY))

(defn double->numeric-special
  "The special a double denotes, or nil when it is finite."
  [^double d]
  (cond (Double/isNaN d) nan-numeric
        (Double/isInfinite d) (if (pos? d) inf-numeric -inf-numeric)
        :else nil))

(defn numeric-special-text [x]
  (case (:kind x) :nan "NaN" :inf "Infinity" :-inf "-Infinity"))

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
    ;; PgRecord → its own type-oid (2249 for an anonymous ROW, else the
    ;; named composite OID). Class-name check avoids a require-loop, as
    ;; for PgArray above.
    (and (some? v)
         (= "datahike.pg.records.PgRecord" (.getName (class v))))
    (or (:type-oid v) 2249)
    ;; PgBit → bit (1560) or bit varying (1562). Class-name check to
    ;; avoid a require-loop, as for PgArray above; without it a bit
    ;; value reported as text because its digits are a String (#19).
    (and (some? v)
         (= "datahike.pg.bits.PgBit" (.getName (class v))))
    (if (:varying? v) oid-varbit oid-bit)
    (instance? clojure.lang.Ratio v) oid-float8
    (instance? Long v)    oid-int8
    (instance? Integer v) oid-int4
    (integer? v)          oid-int8
    (instance? Double v)  oid-float8
    (instance? Float v)   oid-float4
    (float? v)            oid-float8
    ;; BigDecimal is `numeric`. Without this it fell to the :else text
    ;; branch, which only stayed invisible while decimal literals were
    ;; doubles -- once they became numeric, every value-inferred decimal
    ;; reported as text (25).
    (decimal? v)          oid-numeric
    (numeric-special? v)  oid-numeric
    (pg-lsn? v)           oid-pg-lsn
    (boolean? v)          oid-bool
    (inst? v)             oid-timestamp
    ;; ::date / ::time cast results are java.time locals (issue #13);
    ;; without these they reported as text.
    (instance? java.time.LocalDate v)     oid-date
    (instance? java.time.LocalTime v)     oid-time
    (instance? java.time.LocalDateTime v) oid-timestamp
    (uuid? v)             oid-uuid
    :else                 oid-text))

;; ============================================================================
;; Temporal text rendering
;; ============================================================================

(defn float->pg-text
  "PostgreSQL's text form of a float. `float4?` selects the narrower
   rules for `real`.

   PostgreSQL prints shortest-round-trip digits (float.c
   float8out_internal, via Ryu, whenever extra_float_digits > 0 -- and
   its default is 1). Java's `Double.toString` since JDK 19 also
   produces shortest-round-trip digits, so the DIGITS already agree; what
   differs is entirely the presentation, in three ways:

     - the fixed-vs-scientific threshold. PostgreSQL uses fixed point iff
       the scientific exponent is in [-4, 15) for float8 and [-4, 6) for
       float4 (d2s.c to_chars, f2s.c); Java switches at 1e7 / 1e-3. So
       `1e7` is 10000000 in PostgreSQL and \"1.0E7\" in Java.
     - the exponent spelling: lowercase `e`, an explicit sign, and at
       least two digits -- `1e+300`, `1e-05` -- against Java's `E300`.
     - Java always keeps at least two significant digits (\"1.0E7\");
       PostgreSQL's mantissa is minimal (`1e+15`).

   Every float above ~1e7 or below 1e-4 was therefore emitted in a
   syntax PostgreSQL never produces."
  [v float4?]
  (let [d (double v)]
    (cond
      (Double/isNaN d)      "NaN"
      (Double/isInfinite d) (if (pos? d) "Infinity" "-Infinity")
      ;; -0.0 is a distinct float value and PostgreSQL prints its sign:
      ;; `0.0 / -1` is `-0` there. `zero?` is true for both zeros, so the
      ;; sign has to come from the bit pattern.
      (zero? d)             (if (neg? (Double/doubleToRawLongBits d)) "-0" "0")
      ;; Java and PostgreSQL both use shortest-round-trip output, but their
      ;; boundary choice differs for the two least positive subnormals.
      ;; PostgreSQL's Ryu formatter emits the familiar one-digit forms.
      (and float4? (= (float v) Float/MIN_VALUE)) "1e-45"
      (and (not float4?) (= d Double/MIN_VALUE)) "5e-324"
      :else
      (let [s (if float4? (Float/toString (float v)) (Double/toString d))
            minus? (str/starts-with? s "-")
            bd (.stripTrailingZeros (java.math.BigDecimal. ^String (if minus? (subs s 1) s)))
            digits (.toString (.unscaledValue bd))
            ;; scientific exponent: the power of ten of the leading digit
            exp (- (.precision bd) (.scale bd) 1)
            fixed? (and (>= exp -4) (< exp (if float4? 6 15)))
            body (if fixed?
                   (.toPlainString bd)
                   (str (subs digits 0 1)
                        (when (> (count digits) 1) (str "." (subs digits 1)))
                        "e" (if (neg? exp) "-" "+")
                        (let [a (Math/abs exp)]
                          (if (< a 10) (str "0" a) (str a)))))]
        (if minus? (str "-" body) body)))))

(defn temporal->pg-text
  "PostgreSQL's text rendering of a temporal value, or nil if `v` is not
   temporal.

   The wire renderer had its own copy of these rules, so anything that
   converted a value to text by a route OTHER than the wire — `::text`,
   `CAST(… AS varchar)`, `||`, `concat()` — fell through to Clojure's
   `str` and emitted `java.util.Date.toString`:

     SELECT ts::text  →  Wed Jan 01 02:00:00 PST 2020
                         (want 2020-01-01 10:00:00)

   which is not merely misformatted: it is rendered in the JVM's default
   time zone and locale, so the same query answered differently on
   different machines.

   `src-oid` disambiguates `date` from `timestamp`. Datahike has only
   :db.type/instant, so a `date` COLUMN and a `timestamp` COLUMN both
   arrive here as java.util.Date at UTC and nothing about the value says
   which is which. A `::date` CAST produces a LocalDate and needs no
   hint. Absent a hint, an instant renders as a timestamp — the wider of
   the two, and the one that loses no information."
  ([v] (temporal->pg-text v nil))
  ([v src-oid]
   (cond
     (instance? java.time.LocalDate v)     (str v)
     (instance? java.time.LocalTime v)     (str v)
     (instance? java.time.LocalDateTime v) (str/replace (str v) "T" " ")

     (and (inst? v) (= src-oid oid-date))
     (-> ^java.util.Date v .toInstant (.atZone java.time.ZoneOffset/UTC) .toLocalDate str)

     (inst? v)
     (let [^java.time.Instant inst (if (instance? java.time.Instant v)
                                     v
                                     (.toInstant ^java.util.Date v))]
       (-> (str inst)
           (str/replace "T" " ")
           ;; timestamptz keeps a UTC offset; timestamp drops it.
           (str/replace "Z" (if (= src-oid oid-timestamptz) "+00" ""))))

     :else nil)))

(defn ->pg-text
  "`str`, except that temporal values render the PostgreSQL way. Every
   value→text conversion that is not the wire renderer should go through
   here; see `temporal->pg-text` for why."
  ([v] (->pg-text v nil))
  ([v src-oid]
   (cond
     ;; See the same branch in the wire renderer: `.toString` on a
     ;; negative-scale BigDecimal produces an exponent form PostgreSQL
     ;; never emits.
     (numeric-special? v) (numeric-special-text v)
     (instance? java.math.BigDecimal v) (.toPlainString ^java.math.BigDecimal v)
     ;; Same PostgreSQL float form the wire renderer uses.
     (or (instance? Float v) (instance? Double v))
     (float->pg-text v (instance? Float v))
     :else (or (temporal->pg-text v src-oid) (str v)))))

(defn decimal-literal
  "The value of an unadorned SQL decimal literal, built from its ORIGINAL
   TOKEN rather than from JSqlParser's `.getValue`.

   PostgreSQL types such a literal as `numeric`, not float8: `0.1 + 0.2`
   is 0.3 and `1.10` keeps its trailing zero. `.getValue` has already
   gone through a double by the time we see it, losing both the
   exactness and the scale, so neither is recoverable there -- but the
   token itself survives on the node.

   Handles the exponent forms too (`1.0e3`, `1e-3`), which PostgreSQL
   also types as numeric. `fallback` is returned if the token will not
   parse, so a caller can keep its previous behaviour."
  ([token] (decimal-literal token nil))
  ([token fallback]
   (try
     (let [bd (java.math.BigDecimal. (str/trim (str token)))]
       ;; PostgreSQL's numeric never carries a NEGATIVE display scale --
       ;; set_var_from_str normalises it away. BigDecimal does not:
       ;; `1e2` parses to unscaled 1 at scale -2. Multiply is the one
       ;; operator that propagates it (its result scale is s1+s2, where
       ;; add/subtract take a max and divide clamps at 0), so `1e2 *
       ;; 1.25` answered 125 where PostgreSQL answers 125.00.
       (if (neg? (.scale bd)) (.setScale bd 0) bd))
     (catch Exception _ fallback))))
