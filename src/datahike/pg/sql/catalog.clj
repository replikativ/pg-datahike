(ns datahike.pg.sql.catalog
  "Virtual PostgreSQL catalog materialization + system-query routing.

   Datahike has no real `pg_catalog.*` or `information_schema.*` tables —
   pgwire synthesizes them on demand from the user schema. Every
   catalog table the pg_* ecosystem expects is a `{:schema [...]
   :data-fn (fn [user-schema cte-db] ...)}` entry that builds its row
   set lazily when a query references it.

   The registry is extensible at runtime: libraries built on top of
   pg-datahike can add their own virtual catalog tables via
   `register-catalog-table!` (e.g. Odoo's internal metadata tables,
   a `pg_stat_activity`-style probe, etc.) without modifying this
   namespace.

   Two public entry points:
   - `register-catalog-table!` / `unregister-catalog-table!`
     — the extension seam
   - `extract-empty-catalog-shape` / `system-query?`
     — called by the wire handler to short-circuit common boot probes
       (pgjdbc's field-metadata, Hibernate's feature detection) into
       fast paths before JSqlParser even runs."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.shape :as shape]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.statement.select PlainSelect SelectItem AllColumns Join ParenthesedSelect SetOperationList]
           [net.sf.jsqlparser.expression.operators.relational EqualsTo]
           [net.sf.jsqlparser.expression StringValue Alias]))

(set! *warn-on-reflection* true)

;; Alias so the extracted body (which still calls bare `unquote-ident`)
;; resolves without rewriting each call site.
(def ^:private unquote-ident params/unquote-ident)

(def ^:const pg-role-oid
  "OID of the single role we expose. 10 is what a stock PostgreSQL gives
   its bootstrap superuser, so anything keying off it sees a familiar
   value."
  10)

(def ^:const pg-role-name
  "The role every connection authenticates as — see server.clj, which
   answers the same name for current_user / session_user."
  "datahike")

(def ^:private built-in-catalog-tables
  "Virtual catalog tables this layer materializes on demand. Extension
   tables are added via register-catalog-table! and don't appear here."
  #{"pg_type" "pg_class" "pg_tables" "pg_views" "pg_matviews" "pg_attribute"
    "pg_namespace" "pg_database" "pg_proc" "pg_roles"
    "pg_indexes"
    ;; pg_sequences — the user-facing view over every sequence's
    ;; parameters and current position (issue #26). Distinct from
    ;; information_schema.sequences, which omits last_value and is the
    ;; SQL-standard spelling; both are populated from the same
    ;; :__seq__/* entities.
    "pg_sequences"
    ;; pg_index is PG's internal index catalog (distinct from the
    ;; user-facing pg_indexes view). pgjdbc's PK probe joins it by oid.
    ;; pg_attrdef tracks per-column default expressions — we never emit
    ;; any, so rows are always empty, but the LEFT JOIN in pgjdbc's
    ;; field-metadata query still needs the table to exist.
    "pg_index" "pg_attrdef"
    ;; pg_constraint backs `pg_get_constraintdef(oid)` — we synthesize
    ;; one row per CHECK / FK / PK / UNIQUE constraint with a pre-baked
    ;; condef text column.
    "pg_constraint"
    ;; pg_extension is probed by framework feature-detection (Rails,
    ;; Hibernate probe for `pg_trgm`, `uuid-ossp`, `citext`, …). We
    ;; never install any extensions, so the table is always empty.
    "pg_extension"
    ;; The next four are always-empty virtual tables — modelling them
    ;; as a real (empty) catalog rather than going through shape.clj's
    ;; empty-catalog short-circuit lets LEFT JOINs against them produce
    ;; NULL-filled rows instead of dropping the entire result set.
    ;; Metabase's get-tables query LEFT JOINs both pg_description and
    ;; pg_stat_user_tables; without rows for the table itself, the
    ;; LEFT JOIN must still match — same shape pgjdbc field-metadata
    ;; uses for pg_attrdef.
    "pg_description"
    "pg_stat_user_tables"
    "pg_depend"
    "pg_inherits"
    "pg_enum"
    "information_schema_columns" "information_schema_tables"
    "information_schema_sequences"
    "information_schema_table_constraints"
    "information_schema_key_column_usage"})

(declare extra-catalog-tables)

(defn catalog-tables
  "Set of every recognized catalog table — built-ins + runtime-
   registered extensions."
  []
  (into built-in-catalog-tables (keys @extra-catalog-tables)))

(defn catalog-table-name
  "Normalize a JSqlParser Table node to the internal catalog key used
   by `catalog-tables` / `catalog-schema-for` / `catalog-data-for`.
   Returns nil if the node isn't a recognized catalog.

   Handles:
     pg_type                         → \"pg_type\"
     pg_catalog.pg_type              → \"pg_type\"
     information_schema.columns      → \"information_schema_columns\""
  [^Table t]
  (when t
    (let [raw (str/lower-case (unquote-ident (.getName t)))
          ;; JSqlParser returns getSchemaName verbatim — including any
          ;; surrounding double-quotes — so a `"information_schema"."x"`
          ;; reference would compare as the literal string
          ;; `"information_schema"` (quotes included) and miss every
          ;; cond branch below. Strip them the same way we do for the
          ;; table name itself.
          schema (when-let [s (.getSchemaName t)]
                   (str/lower-case (unquote-ident s)))
          normalized (cond
                       (= schema "information_schema") (str "information_schema_" raw)
                       (= schema "pg_catalog")          raw
                       :else                            raw)]
      (when (contains? (catalog-tables) normalized) normalized))))

;; ============================================================================
;; Catalog registry
;; ----------------------------------------------------------------------------
;; Two layers:
;;   * Built-in tables — pg_type, pg_class, pg_attribute, … — defined by the
;;     per-table case expressions in catalog-schema-for* / catalog-data-for*.
;;   * Extension tables — registered at runtime via register-catalog-table!.
;;
;; Library consumers of datahike-pg can expose their own virtual catalog
;; surfaces (e.g. app-level metrics, temporal views) without forking
;; this namespace. Built-ins take precedence on name collision.
;; ============================================================================

(def ^:private extra-catalog-tables
  "Runtime-registered catalog tables. Map of table-name →
   {:schema [{:db/ident … :db/valueType … :db/cardinality …} …]
    :data-fn (fn [user-schema cte-db] [row-map …])}."
  (atom {}))

(defn register-catalog-table!
  "Register an additional virtual catalog table. `entry` must supply
   :schema (a seq of Datahike schema entry maps — include a row-marker
   attr) and :data-fn (a fn of [user-schema cte-db] returning a seq of
   row entity-maps).

   **Init-time only.** The registry is a process-local atom — it is
   not persisted to Datahike and does not survive a server restart.
   Call this from your application's startup code, before
   `start-server`, not lazily on first query. If your host process
   restarts (deploy, crash), each registration must be re-applied
   before the first connection lands; otherwise clients hitting an
   un-re-registered table see \"relation does not exist\".

   Built-in catalogs (pg_class, pg_attribute, …) are unaffected by
   restart — they're derived from the user schema in Datahike, which
   is durable on file/kv backends."
  [table-name entry]
  (assert (string? table-name))
  (assert (:schema entry) "register-catalog-table! entry needs :schema")
  (assert (fn? (:data-fn entry)) "register-catalog-table! entry needs :data-fn")
  (swap! extra-catalog-tables assoc table-name entry)
  nil)

(defn unregister-catalog-table!
  "Remove a previously-registered catalog table. No-op if unregistered."
  [table-name]
  (swap! extra-catalog-tables dissoc table-name)
  nil)

(declare catalog-schema-for* catalog-data-for* built-in-catalog-tables)

(def ^:dynamic *registered-databases*
  "When bound (by the server's handler factory), a seq of strings — the
   names of databases registered at server-start time. Surfaced as rows
   in the virtual `pg_database` catalog so tools that enumerate databases
   (psql \\l, pg_dump --list, pgjdbc's DatabaseMetaData) see them.

   Unbound (nil) falls back to the legacy single-database shape —
   [\"template0\" \"template1\" \"datahike\"] — so tests that start a bare
   handler without a registry still discover the expected row set."
  nil)

(defn catalog-schema-for
  "Resolve a catalog table's Datahike schema. Checks extensions first
   (allowing userland overrides in theory, though we don't rely on
   that), then falls back to the built-ins."
  [table-name]
  (or (:schema (get @extra-catalog-tables table-name))
      (catalog-schema-for* table-name)))

(defn catalog-data-for
  "Build one catalog table's row data. Same precedence as
   catalog-schema-for."
  [table-name user-schema cte-db]
  (if-let [entry (get @extra-catalog-tables table-name)]
    ((:data-fn entry) user-schema cte-db)
    (catalog-data-for* table-name user-schema cte-db)))

(defn catalog-schema-for*
  "Built-in catalog schema — every table-name is a key in a case
   expression. Library consumers register additional tables via
   register-catalog-table! (above); the consolidated lookup goes
   through catalog-schema-for."
  [table-name]
  (case table-name
    "pg_type"
    [{:db/ident :pg_type/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_type/typname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_type/typlen :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     ;; typtype is PG's "char" (OID 18): clients (asyncpg's is_scalar_type)
     ;; binary-decode it to a single byte, so advertise OID 18 not text.
     {:db/ident :pg_type/typtype :db/valueType :db.type/string :db/cardinality :db.cardinality/one :pg/type "char"}
     ;; typelem: element type OID for array types (0 for scalars). Clients
     ;; (asyncpg's TYPE_BY_OID, libpq) read it to detect/decode arrays.
     {:db/ident :pg_type/typelem :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     ;; typdelim: element delimiter (',' for all but a few types). asyncpg's
     ;; typeinfo reads elem_t.typdelim and does `elemdelim[0]` while BUILDING an
     ;; array codec — a null there throws and the whole composite fails to
     ;; resolve. PG "char" (OID 18), like typtype.
     {:db/ident :pg_type/typdelim :db/valueType :db.type/string :db/cardinality :db.cardinality/one :pg/type "char"}
     ;; typnamespace: asyncpg's type-introspection INNER JOINs pg_namespace
     ;; on it, so every type must carry one (all in `public` = 2200).
     {:db/ident :pg_type/typnamespace :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_type") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_attribute"
    [{:db/ident :pg_attribute/attname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     ;; atttypid is PG's `oid` type — declare it so `array_agg(atttypid)`
     ;; (asyncpg's typeinfo attrtypoids) infers oid[] (1028), the array type
     ;; asyncpg core-registers. Reporting int8[] (1016) made asyncpg loop
     ;; forever re-introspecting the unregistered array codec.
     {:db/ident :pg_attribute/atttypid :db/valueType :db.type/long :db/cardinality :db.cardinality/one :pg/type "oid"}
     {:db/ident :pg_attribute/attnum :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     ;; Integer OID matching pg_class.oid so pgjdbc's
     ;; `pg_class c JOIN pg_attribute a ON c.oid = a.attrelid` works.
     {:db/ident :pg_attribute/attrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     ;; NOT NULL marker — true for PK columns, false otherwise. pgjdbc's
     ;; field-metadata projection reads it as `a.attnotnull OR (...)`.
     {:db/ident :pg_attribute/attnotnull :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     ;; PG identity-column kind: '' = not identity, 'a' = always,
     ;; 'd' = by default. We never emit identity columns.
     {:db/ident :pg_attribute/attidentity :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     ;; PostgreSQL type storage strategy: p=plain, m=main, x=extended.
     ;; psql's \d+ renders this as Plain/Main/Extended.
     {:db/ident :pg_attribute/attstorage :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     ;; atttypmod: encodes NUMERIC(p, s) precision/scale (and varchar(n)
     ;; length when we add it). -1 = unconstrained — what real PG
     ;; reports for plain `NUMERIC` or `TEXT` columns. Drives
     ;; information_schema.columns.numeric_precision / numeric_scale.
     {:db/ident :pg_attribute/atttypmod :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     ;; attisdropped: asyncpg's composite-field introspection filters
     ;; `NOT ia.attisdropped`, so the column must exist (always false here).
     {:db/ident :pg_attribute/attisdropped :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_attribute") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_namespace"
    ;; nspowner/nspacl exist for pg_dump, which selects them and then
    ;; looks the owner up in its role map. A NULL owner resolved to OID
    ;; 0 and it aborted with "role with OID 0 does not exist".
    [{:db/ident :pg_namespace/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_namespace/nspname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_namespace/nspowner :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_namespace") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    ;; A single role, the one every connection authenticates as. We have
    ;; no privilege system; this exists so ownership resolves.
    "pg_roles"
    [{:db/ident :pg_roles/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolsuper :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolinherit :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolcreaterole :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolcreatedb :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolcanlogin :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolreplication :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolbypassrls :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_roles/rolconnlimit :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_roles") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_database"
    [{:db/ident :pg_database/datname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_database/datdba :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_database") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_proc"
    [{:db/ident :pg_proc/proname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_proc/provolatile :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_proc/pronamespace :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_proc/pronargs :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_proc/prorettype :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_proc/proargtypes :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_proc") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_class"
    [;; Stable per-table OID — the primary key of pg_class. Populated
     ;; from the table's :pg/table-oid. Lets pgjdbc's metadata joins
     ;; (pg_class.oid = pg_attribute.attrelid, etc.) resolve rows.
     {:db/ident :pg_class/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_class/relname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_class/relnamespace :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_class/relkind :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     ;; reltype: the pg_type OID of this relation's composite row-type.
     ;; asyncpg joins composite pg_type → pg_class on `c.reltype = t.oid`.
     {:db/ident :pg_class/reltype :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_class") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]

    "pg_index"
    [{:db/ident :pg_index/indrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_index/indexrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     ;; PG stores indkey as an int2vector — a sequence of column
     ;; positions. We hold a JSON-ish string "[1]" or "[1,2]"; pgjdbc
     ;; calls information_schema._pg_expandarray on it, which we have
     ;; stubbed to pass through via SQL translation.
     {:db/ident :pg_index/indkey :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_index/indisprimary :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_index/indisunique :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_index/indisvalid :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     ;; indpred / indexprs are bytea in real PG but tested only for IS
     ;; NULL — a string column with "" (treated as non-null) works.
     ;; pgjdbc's query requires these to be NULL for a valid PK match,
     ;; so we emit actual NULL via empty value + get-else pathway.
     {:db/ident :pg_index/indpred :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_index/indexprs :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     ;; Pre-baked CREATE INDEX text — `pg_get_indexdef(oid)` lowers to
     ;; a join that reads this column directly. PG generates the
     ;; string at runtime from indkey + indrelid; we synthesize once
     ;; at catalog load.
     {:db/ident :pg_index/indexdef :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_index") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]

    "pg_attrdef"
    ;; Always empty for us (no column defaults), but must exist as a
    ;; catalog table so pgjdbc's LEFT JOIN doesn't fail to resolve.
    [{:db/ident :pg_attrdef/adrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_attrdef/adnum :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_attrdef/adbin :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_attrdef") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_description"
    ;; Always empty — we don't track COMMENT ON anything. Metabase's
    ;; get-tables query LEFT JOINs against this; an empty real table
    ;; produces NULL-filled left-side rows.
    [{:db/ident :pg_description/objoid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_description/classoid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_description/objsubid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_description/description :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_description") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_stat_user_tables"
    ;; Always empty — we don't track row-count statistics. Metabase
    ;; LEFT JOINs against this for an `estimated_row_count` field.
    [{:db/ident :pg_stat_user_tables/relid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_stat_user_tables/schemaname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_stat_user_tables/relname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_stat_user_tables/n_live_tup :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_stat_user_tables/n_dead_tup :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_stat_user_tables") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_depend"
    ;; Always empty — no dependency tracking. Some pgjdbc / pg_dump
    ;; queries reference it; an empty table is the right shape.
    [{:db/ident :pg_depend/classid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_depend/objid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_depend/objsubid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_depend/refclassid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_depend/refobjid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_depend/refobjsubid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_depend/deptype :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_depend") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_inherits"
    ;; Always empty — we don't model table inheritance.
    [{:db/ident :pg_inherits/inhrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_inherits/inhparent :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_inherits/inhseqno :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_inherits") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_enum"
    ;; Always empty — we don't model PG enum types. Metabase's
    ;; describe-database probes `SELECT enumtypid FROM pg_enum`
    ;; before sync-fields; without this catalog, the query errors
    ;; out and sync-fields aborts.
    [{:db/ident :pg_enum/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_enum/enumtypid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_enum/enumsortorder :db/valueType :db.type/double :db/cardinality :db.cardinality/one}
     {:db/ident :pg_enum/enumlabel :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_enum") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_constraint"
    ;; One row per CHECK / FK / PK / UNIQUE constraint. `condef` is
    ;; the synthesized text — `pg_get_constraintdef(oid)` lowers to
    ;; `[?c :pg_constraint/oid ?arg] [?c :pg_constraint/condef ?out]`.
    ;;
    ;; conkey / confkey are int2[] in PG. We store them as the PG
    ;; array text form ("{1,2,3}") so the wire layer renders them
    ;; correctly and the runtime ANY/ALL path can parse them back
    ;; into a pg-arr on demand. Metabase's FK introspection joins
    ;; pg_attribute to pg_constraint via `attnum = ANY(c.conkey)`,
    ;; so populating these isn't optional.
    [{:db/ident :pg_constraint/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/conname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/contype :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/conrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/connamespace :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/confrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/condeferrable :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/condeferred :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/convalidated :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/conkey :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/confkey :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/condef :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_constraint") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_extension"
    ;; Always empty — we never install extensions. Clients probe
    ;; `SELECT * FROM pg_extension WHERE extname='pg_trgm'` to feature-
    ;; detect; returning zero rows is the right answer.
    [{:db/ident :pg_extension/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_extension/extname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_extension/extowner :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_extension/extnamespace :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_extension/extrelocatable :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_extension/extversion :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_extension") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_tables"
    [{:db/ident :pg_tables/schemaname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_tables/tablename :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_tables/tableowner :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_tables/tablespace :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_tables/hasindexes :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_tables/hasrules :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_tables/hastriggers :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_tables/rowsecurity :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_tables") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    ;; pg_sequences — one row per sequence. Column names and types
    ;; follow PG's own view (see src/backend/catalog/system_views.sql):
    ;; the identifier columns are `name`, the numeric ones int8, and
    ;; `data_type` is a regtype. `last_value` is the one nullable
    ;; column — NULL until the sequence has actually been advanced.
    "pg_sequences"
    [{:db/ident :pg_sequences/schemaname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/sequencename :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/sequenceowner :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/data_type :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/start_value :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/min_value :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/max_value :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/increment_by :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/cycle :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/cache_size :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_sequences/last_value :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_sequences") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    ;; pg_views — populated from transactional :datahike.pg/view-* metadata.
    "pg_views"
    [{:db/ident :pg_views/schemaname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_views/viewname   :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_views/viewowner  :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_views/definition :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_views") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_matviews"
    [{:db/ident :pg_matviews/schemaname  :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_matviews/matviewname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_matviews/matviewowner :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_matviews/definition  :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_matviews/ispopulated :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_matviews") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    ;; information_schema.columns — mirror PG's information_schema.sql
    ;; columns view. Domain-to-Datahike-type mapping:
    ;;   cardinal_number  → :db.type/long  (ordinal_position, *_precision, *_scale)
    ;;   sql_identifier   → :db.type/string (table/schema/column names, udt_*)
    ;;   character_data   → :db.type/string (data_type, defaults, identity_generation)
    ;;   yes_or_no        → :db.type/string ("YES" / "NO" — 3-char varchar in PG)
    "information_schema_columns"
    [{:db/ident :information_schema_columns/table_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/table_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/table_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/column_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/ordinal_position :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/column_default :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/is_nullable :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/data_type :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/character_maximum_length :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/character_octet_length :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/numeric_precision :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/numeric_precision_radix :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/numeric_scale :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/datetime_precision :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/udt_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/udt_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/udt_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/dtd_identifier :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/is_self_referencing :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/is_identity :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/identity_generation :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/is_generated :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/is_updatable :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "information_schema_columns") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "information_schema_tables"
    [{:db/ident :information_schema_tables/table_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_tables/table_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_tables/table_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_tables/table_type :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "information_schema_tables") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "information_schema_sequences"
    [{:db/ident :information_schema_sequences/sequence_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_sequences/sequence_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_sequences/sequence_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_sequences/start_value :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_sequences/minimum_value :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_sequences/maximum_value :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_sequences/increment :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "information_schema_sequences") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_indexes"
    [{:db/ident :pg_indexes/schemaname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_indexes/tablename :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_indexes/indexname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_indexes/tablespace :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_indexes/indexdef :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_indexes") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "information_schema_table_constraints"
    [{:db/ident :information_schema_table_constraints/constraint_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/constraint_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/constraint_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/table_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/table_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/table_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/constraint_type :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/is_deferrable :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/initially_deferred :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_table_constraints/enforced :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "information_schema_table_constraints") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "information_schema_key_column_usage"
    [{:db/ident :information_schema_key_column_usage/constraint_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/constraint_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/constraint_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/table_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/table_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/table_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/column_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/ordinal_position :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/position_in_unique_constraint :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "information_schema_key_column_usage") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    nil))

(def ^:private sequence-defaults
  "Fallbacks for sequences transacted before an attribute existed —
   notably the implicit sequences translate-create-table installs for
   IDENTITY / serial columns, which only ever set name/value/increment.
   These are PG's own defaults for a bigint sequence."
  {:__seq__/increment 1
   :__seq__/minvalue 1
   :__seq__/maxvalue Long/MAX_VALUE
   :__seq__/cache 1
   :__seq__/cycle false
   :__seq__/start 1
   :__seq__/type "bigint"})

(defn sequence-entities
  "Every sequence in `db` as a map of its :__seq__/* attributes, merged
   over `sequence-defaults`, sorted by name.

   Shared by pg_sequences, information_schema.sequences and the
   relkind='S' rows in pg_class so the three cannot disagree."
  [db]
  (->> (d/q '{:find [(pull ?e [*])]
              :where [[?e :__seq__/name _]]}
            db)
       (map first)
       (map #(merge sequence-defaults %))
       (sort-by :__seq__/name)
       vec))

(def sequence-backed-catalogs
  "Catalog tables whose rows come from :__seq__/* entities, and whose
   content can therefore change without the user schema changing. The
   catalog cache keys on the schema hash, so these need an extra
   component — see `sequence-state`."
  #{"pg_sequences" "information_schema_sequences" "pg_class"})

(defn sequence-state
  "A cheap value that changes whenever any sequence is created, dropped
   or advanced. Deliberately just the name/value pairs: the other
   parameters are fixed at CREATE time, so a sequence whose name and
   position are unchanged has unchanged catalog rows."
  [db]
  (sort (d/q '{:find [?n ?v]
               :where [[?e :__seq__/name ?n]
                       [?e :__seq__/value ?v]]}
             db)))

(defn sequence-last-value
  "PG's `last_value`: the value most recently handed out, or nil when
   the sequence has never been advanced.

   We store the last value handed out, so a never-called sequence holds
   `start - increment` (see ddl/sequence-entity). PG models the same
   state as last_value + is_called=false and reports last_value as NULL
   in pg_sequences until the first nextval."
  [{:__seq__/keys [value start increment]}]
  (when-not (= value (- start increment))
    value))

(defn- view-entities [db]
  (if db
    (mapv (fn [[eid name definition]]
            (let [columns-str (:datahike.pg/view-columns
                               (d/pull db '[:datahike.pg/view-columns] eid))]
              {:name name
               :definition definition
               :columns (when columns-str
                          (try (edn/read-string columns-str)
                               (catch Exception _ nil)))}))
          (d/q '{:find [?e ?name ?definition]
                 :where [[?e :datahike.pg/view-name ?name]
                         [?e :datahike.pg/view-definition ?definition]]}
               db))
    []))

(defn- attribute-storage [oid]
  (cond
    (= oid types/oid-numeric) "m"
    (or (contains? types/array-oid->element-oid oid)
        (contains? #{types/oid-bytea types/oid-text types/oid-varchar
                     types/oid-bpchar types/oid-json types/oid-jsonb}
                   oid)) "x"
    :else "p"))

(defn catalog-data-for*
  "Built-in catalog data — see catalog-schema-for*. Dispatches a
   per-table body; library consumers add more via the registry."
  [table-name user-schema cte-db]
  (case table-name
    "pg_type"
    ;; Full base + array type set from the central registry (types/pg-type-
    ;; catalog) — including oid 26 (oid), name, numeric, json/jsonb, time/
    ;; interval and the _T[] array types. The previous hardcoded 10-row list
    ;; omitted most, so client type-introspection (e.g. asyncpg's
    ;; set_type_codec → TYPE_BY_OID) failed with "unknown type pg_catalog.X".
    ;; typelem is the element OID for array types (typname "_int4" → int4's
    ;; oid), 0 for scalars.
    (let [name->oid (into {} (map (fn [[o n _ _]] [n o])) types/pg-type-catalog)
          base (mapv (fn [[oid tname tlen ttype]]
                       (let [elem (when (str/starts-with? tname "_")
                                    (name->oid (subs tname 1)))]
                         {:pg_type/oid (long oid) :pg_type/typname tname
                          :pg_type/typlen (long tlen) :pg_type/typtype ttype
                          :pg_type/typelem (long (or elem 0))
                          :pg_type/typdelim ","
                          :pg_type/typnamespace 2200
                          (pgs/row-marker-attr "pg_type") true}))
                     types/pg-type-catalog)
          ;; User composite types (CREATE TYPE … AS (..)) — typtype 'c',
          ;; variable length, namespace public.
          composites (mapv (fn [{:keys [name oid]}]
                             {:pg_type/oid oid :pg_type/typname name
                              :pg_type/typlen -1 :pg_type/typtype "c"
                              :pg_type/typelem 0 :pg_type/typdelim ","
                              :pg_type/typnamespace 2200
                              (pgs/row-marker-attr "pg_type") true})
                           (pgs/composite-types cte-db))]
      (into base composites))
    "pg_attribute"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))
          ;; Bulk-fetch :pg/typmod from the db so we don't N+1 per
          ;; column. Returns {attr-ident → typmod-int}.
          q-fn d/q
          typmods (when cte-db
                    (into {}
                          (q-fn '{:find [?ident ?typmod]
                                  :where [[?e :db/ident ?ident]
                                          [?e :pg/typmod ?typmod]]}
                                cte-db)))]
      (into
       ;; composite-type fields: attrelid = the composite's pg_class oid
       ;; (= its type oid here); atttypid = each field's PG type OID.
       (vec (for [{:keys [oid fields]} (pgs/composite-types cte-db)
                  [idx f] (map-indexed vector fields)]
              {:pg_attribute/attname (:field-name f)
               :pg_attribute/atttypid (long (:oid f))
               :pg_attribute/attnum (long (inc idx))
               :pg_attribute/attrelid (long oid)
               :pg_attribute/attnotnull false
               :pg_attribute/attidentity ""
               :pg_attribute/attstorage (attribute-storage (:oid f))
               :pg_attribute/atttypmod -1
               :pg_attribute/attisdropped false
               (pgs/row-marker-attr "pg_attribute") true}))
       (concat
        (for [[tname {:keys [columns]}] (sort-by key tables)
              [idx col] (map-indexed vector columns)
              :let [tbl-oid (or (pgs/table-oid cte-db tname)
                                   ;; Pre-existing tables from before we
                                   ;; started tracking :pg/table-oid — fall
                                   ;; back to the attnum-derived composite
                                   ;; key convention (name → hash) so stale
                                   ;; data still has a stable attrelid.
                                (Math/abs (.hashCode ^String tname)))
                    pk? (= :db.unique/identity (:unique col))
                       ;; -1 = unconstrained (real PG's default for
                       ;; plain NUMERIC / TEXT). Defined NUMERIC(p, s)
                       ;; columns get a positive value via DDL.
                    typmod (long (or (get typmods (:attr col)) -1))]]
          {:pg_attribute/attname (:name col)
              ;; Cardinality-many columns project as PG arrays, so
              ;; their atttypid must be the array OID — pgjdbc reads
              ;; this for ResultSetMetaData and the field-metadata
              ;; cache key. Mirrors the OID inference in
              ;; oid-infer/column-oid.
           :pg_attribute/atttypid
          ;; `(:oid col)`, NOT a fresh derivation from :valuetype. The
          ;; column map already carries the authoritative OID from
          ;; `declared-col-oid`, which honours the `:pg/type` recorded at
          ;; CREATE TABLE. Recomputing from storage type collapsed every
          ;; declared type back onto its Datahike carrier: a `date`
          ;; column reported `timestamp without time zone` (1114) and an
          ;; `int` column reported `bigint` (20). Drivers read this to
          ;; pick a codec, so it is not cosmetic — and a date column's
          ;; binary encode then failed and silently shipped text bytes
          ;; labelled as binary.
           (long (let [base (:oid col)]
                   (if (and (= :db.cardinality/many (:cardinality col))
                           ;; `_int4` already resolved to 1007 via
                           ;; :pg/type; promoting again would give int[][].
                            (not (contains? types/array-oid->element-oid base)))
                     (get types/element-oid->array-oid base types/oid-text-array)
                     base)))
           :pg_attribute/attnum (long (inc idx))
           :pg_attribute/attrelid (long tbl-oid)
           :pg_attribute/attnotnull pk?
           :pg_attribute/attidentity ""
           :pg_attribute/attstorage (attribute-storage (:oid col))
           :pg_attribute/atttypmod typmod
           :pg_attribute/attisdropped false
           (pgs/row-marker-attr "pg_attribute") true})
        (for [{:keys [name columns]} (view-entities cte-db)
              [idx col] (map-indexed vector columns)]
          {:pg_attribute/attname (:name col)
           :pg_attribute/atttypid (long (:oid col))
           :pg_attribute/attnum (long (inc idx))
           :pg_attribute/attrelid (long (Math/abs (.hashCode ^String name)))
           :pg_attribute/attnotnull false
           :pg_attribute/attidentity ""
           :pg_attribute/attstorage (attribute-storage (:oid col))
           :pg_attribute/atttypmod (long (or (:typmod col) -1))
           :pg_attribute/attisdropped false
           (pgs/row-marker-attr "pg_attribute") true}))))
    "pg_namespace"
    [{:pg_namespace/oid 2200 :pg_namespace/nspname "public"
      :pg_namespace/nspowner pg-role-oid
      (pgs/row-marker-attr "pg_namespace") true}]
    "pg_roles"
    [{:pg_roles/oid pg-role-oid :pg_roles/rolname pg-role-name
      :pg_roles/rolsuper true :pg_roles/rolinherit true
      :pg_roles/rolcreaterole true :pg_roles/rolcreatedb true
      :pg_roles/rolcanlogin true :pg_roles/rolreplication false
      :pg_roles/rolbypassrls true :pg_roles/rolconnlimit -1
      (pgs/row-marker-attr "pg_roles") true}]
    "pg_database"
    ;; PG always ships with template0, template1, plus each real db.
    ;; Tools (pgjdbc's DatabaseMetaData tests, Odoo's boot, pg_dump) look
    ;; these up. When the server's handler factory bound
    ;; `*registered-databases*`, each name in it becomes a row alongside
    ;; the templates. Unbound → legacy "datahike" placeholder so bare-
    ;; handler tests still see the expected shape.
    (let [real (or (seq *registered-databases*) ["datahike"])]
      (into [{:pg_database/datname "template0" :pg_database/datdba 10
              (pgs/row-marker-attr "pg_database") true}
             {:pg_database/datname "template1" :pg_database/datdba 10
              (pgs/row-marker-attr "pg_database") true}]
            (map (fn [name]
                   {:pg_database/datname name :pg_database/datdba 10
                    (pgs/row-marker-attr "pg_database") true}))
            real))
    "pg_proc"
    (mapv (fn [[pname vol nargs ret args]]
            {:pg_proc/proname pname
             :pg_proc/provolatile vol
             :pg_proc/pronamespace 11
             :pg_proc/pronargs (long nargs)
             :pg_proc/prorettype (long ret)
             :pg_proc/proargtypes (or args "")
             (pgs/row-marker-attr "pg_proc") true})
          [["now" "s" 0 1184 ""] ["current_timestamp" "s" 0 1184 ""]
           ["current_date" "s" 0 1082 ""] ["current_time" "s" 0 1083 ""]
           ["timeofday" "v" 0 25 ""] ["clock_timestamp" "v" 0 1184 ""]
           ["statement_timestamp" "s" 0 1184 ""]
           ["transaction_timestamp" "s" 0 1184 ""]
           ["age" "i" 2 25 "1114 1114"]
           ["date_part" "i" 2 701 "25 1114"]
           ["date_trunc" "i" 2 1114 "25 1114"]
           ["extract" "i" 2 701 "25 1114"]
           ["to_char" "s" 2 25 "1114 25"]
           ["to_date" "s" 2 1082 "25 25"]
           ["to_timestamp" "s" 2 1184 "25 25"]
           ["upper" "i" 1 25 "25"] ["lower" "i" 1 25 "25"]
           ["initcap" "i" 1 25 "25"] ["length" "i" 1 23 "25"]
           ["char_length" "i" 1 23 "25"] ["character_length" "i" 1 23 "25"]
           ["octet_length" "i" 1 23 "25"]
           ["trim" "i" 1 25 "25"] ["ltrim" "i" 1 25 "25"]
           ["rtrim" "i" 1 25 "25"] ["btrim" "i" 1 25 "25"]
           ["substr" "i" 3 25 "25 23 23"]
           ["substring" "i" 3 25 "25 23 23"]
           ["replace" "i" 3 25 "25 25 25"]
           ["regexp_replace" "i" 3 25 "25 25 25"]
           ["regexp_match" "i" 2 25 "25 25"]
           ["split_part" "i" 3 25 "25 25 23"]
           ["string_agg" "i" 2 25 "25 25"]
           ["concat" "i" 2 25 "25 25"]
           ["concat_ws" "i" 3 25 "25 25 25"]
           ["position" "i" 2 23 "25 25"]
           ["lpad" "i" 3 25 "25 23 25"] ["rpad" "i" 3 25 "25 23 25"]
           ["reverse" "i" 1 25 "25"] ["repeat" "i" 2 25 "25 23"]
           ["left" "i" 2 25 "25 23"] ["right" "i" 2 25 "25 23"]
           ["abs" "i" 1 701 "701"] ["round" "i" 1 701 "701"]
           ["floor" "i" 1 701 "701"] ["ceil" "i" 1 701 "701"]
           ["ceiling" "i" 1 701 "701"]
           ["trunc" "i" 1 701 "701"] ["sign" "i" 1 701 "701"]
           ["mod" "i" 2 701 "701 701"]
           ["power" "i" 2 701 "701 701"]
           ["sqrt" "i" 1 701 "701"]
           ["exp" "i" 1 701 "701"] ["ln" "i" 1 701 "701"]
           ["log" "i" 2 701 "701 701"]
           ["greatest" "i" 2 701 "701 701"]
           ["least" "i" 2 701 "701 701"]
           ["count" "i" 1 20 "2276"] ["sum" "i" 1 701 "701"]
           ["avg" "i" 1 701 "701"] ["min" "i" 1 701 "701"]
           ["max" "i" 1 701 "701"]
           ["cast" "i" 2 2276 "2276 25"]
           ["coalesce" "i" 2 2276 "2276 2276"]
           ["nullif" "i" 2 2276 "2276 2276"]
           ["jsonb_build_object" "i" 2 3802 "2276 2276"]
           ["row_to_json" "i" 1 114 "2249"] ["to_json" "i" 1 114 "2276"]
           ["jsonb_build_array" "i" 1 3802 "2276"]
           ["jsonb_set" "i" 3 3802 "3802 1009 3802"]
           ["jsonb_insert" "i" 3 3802 "3802 1009 3802"]
           ["jsonb_strip_nulls" "i" 1 3802 "3802"]
           ["jsonb_array_elements" "i" 1 3802 "3802"]
           ["jsonb_object_keys" "i" 1 25 "3802"]
           ["jsonb_typeof" "i" 1 25 "3802"]
           ["jsonb_pretty" "i" 1 25 "3802"]
           ["version" "s" 0 25 ""]
           ["current_database" "s" 0 19 ""]
           ["current_schema" "s" 0 19 ""]
           ["current_user" "s" 0 19 ""]
           ["session_user" "s" 0 19 ""]
           ["current_setting" "s" 1 25 "25"]
           ["set_config" "s" 3 25 "25 25 16"]
           ["pg_backend_pid" "s" 0 23 ""] ["pg_is_in_recovery" "s" 0 16 ""]
           ["pg_typeof" "s" 1 2206 "2276"]
           ["format_type" "s" 2 25 "26 23"]
           ["pg_get_userbyid" "s" 1 19 "26"]
           ["pg_get_expr" "s" 2 25 "194 26"]
           ["pg_get_constraintdef" "s" 1 25 "26"]
           ["pg_get_indexdef" "s" 1 25 "26"]
           ["has_schema_privilege" "s" 2 16 "25 25"]
           ["has_table_privilege" "s" 2 16 "25 25"]
           ["has_column_privilege" "s" 3 16 "25 25 25"]
           ["obj_description" "s" 2 25 "26 25"]
           ["col_description" "s" 2 25 "26 23"]
           ["pg_relation_size" "v" 1 20 "26"]
           ["pg_total_relation_size" "v" 1 20 "26"]
           ["pg_database_size" "v" 1 20 "19"]
           ["pg_table_size" "v" 1 20 "26"]])
    "pg_class"
    (into
     (mapv (fn [t]
             (let [tbl-oid (or (pgs/table-oid cte-db t)
                               (Math/abs (.hashCode ^String t)))]
               {:pg_class/oid (long tbl-oid)
                :pg_class/relname t
                :pg_class/relnamespace 2200
                :pg_class/relkind "r"
                (pgs/row-marker-attr "pg_class") true}))
           (pgs/table-names user-schema))
     ;; composite types get a pg_class row (relkind 'c'); asyncpg joins
     ;; pg_type → pg_class on `reltype = type-oid`, and pg_attribute on
     ;; `attrelid = pg_class.oid`. We use the type OID for both.
     (into
      (mapv (fn [{:keys [name oid]}]
              {:pg_class/oid oid
               :pg_class/relname name
               :pg_class/relnamespace 2200
               :pg_class/relkind "c"
               :pg_class/reltype oid
               (pgs/row-marker-attr "pg_class") true})
            (pgs/composite-types cte-db))
      ;; Sequences are relations in PG (relkind 'S'), which is how
      ;; pg_dump, psql's \ds and ORM introspection find them at all —
      ;; without a row here a sequence is invisible to anything that
      ;; walks pg_class (issue #26).
      (into
       (mapv (fn [s]
               (let [nm (:__seq__/name s)]
                 {:pg_class/oid (long (Math/abs (.hashCode ^String nm)))
                  :pg_class/relname nm
                  :pg_class/relnamespace 2200
                  :pg_class/relkind "S"
                  (pgs/row-marker-attr "pg_class") true}))
             (sequence-entities cte-db))
       (mapv (fn [{:keys [name]}]
               {:pg_class/oid (long (Math/abs (.hashCode ^String name)))
                :pg_class/relname name
                :pg_class/relnamespace 2200
                :pg_class/relkind "v"
                (pgs/row-marker-attr "pg_class") true})
             (view-entities cte-db)))))
    "pg_tables"
    (mapv (fn [t]
            {:pg_tables/schemaname "public"
             :pg_tables/tablename t
             :pg_tables/tableowner "datahike"
             :pg_tables/tablespace "pg_default"
             :pg_tables/hasindexes true
             :pg_tables/hasrules false
             :pg_tables/hastriggers false
             :pg_tables/rowsecurity false
             (pgs/row-marker-attr "pg_tables") true})
          (pgs/table-names user-schema))
    "pg_views"
    (mapv (fn [{view-name :name definition :definition}]
            {:pg_views/schemaname "public"
             :pg_views/viewname view-name
             :pg_views/viewowner "datahike"
             :pg_views/definition definition
             (pgs/row-marker-attr "pg_views") true})
          (view-entities cte-db))
    "information_schema_columns"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))
          ;; udt_name in PG follows the underlying base-type convention from
          ;; pg_type — `int4` / `int8` / `varchar` / `timestamp`, NOT the
          ;; SQL-spelled `data_type` ("integer" / "bigint" / …). Metabase
          ;; routes its column-type inference off `udt_name`, so getting
          ;; this right matters more than data_type spelling.
          udt-name (fn [vtype]
                     (case vtype
                       :db.type/string  "text"
                       :db.type/long    "int8"
                       :db.type/boolean "bool"
                       :db.type/double  "float8"
                       :db.type/float   "float4"
                       :db.type/instant "timestamp"
                       :db.type/uuid    "uuid"
                       :db.type/ref     "int8"
                       :db.type/keyword "text"
                       :db.type/bigdec  "numeric"
                       :db.type/bytes   "bytea"
                       "text"))
          ;; PG's information_schema fills numeric_precision / _scale only
          ;; for numeric-categoried types and leaves the rest NULL. We do
          ;; the same — Metabase reads these to infer fixed-point vs
          ;; floating-point columns. For NUMERIC(p, s) columns we
          ;; decode the per-attr typmod (set at DDL time) so users see
          ;; their declared (10, 2) etc., not unconstrained NULL.
          q-fn d/q
          typmods (when cte-db
                    (into {}
                          (q-fn '{:find [?ident ?tm]
                                  :where [[?e :db/ident ?ident]
                                          [?e :pg/typmod ?tm]]}
                                cte-db)))
          numeric-precision (fn [vtype attr]
                              (let [[p _] (when-let [tm (get typmods attr)]
                                            (types/decode-numeric-typmod tm))]
                                (case vtype
                                  :db.type/long    64
                                  :db.type/ref     64
                                  :db.type/float   24
                                  :db.type/double  53
                                  :db.type/bigdec  p
                                  :db.type/bigint  64
                                  nil)))
          numeric-radix     (fn [vtype]
                              (case vtype
                                :db.type/long    2
                                :db.type/ref     2
                                :db.type/float   2
                                :db.type/double  2
                                :db.type/bigdec  10
                                :db.type/bigint  2
                                nil))
          numeric-scale     (fn [vtype attr]
                              (let [[_ s] (when-let [tm (get typmods attr)]
                                            (types/decode-numeric-typmod tm))]
                                (case vtype
                                  :db.type/long    0
                                  :db.type/ref     0
                                  :db.type/bigdec  s
                                  :db.type/bigint  0
                                  nil)))
          datetime-precision (fn [vtype]
                               (case vtype
                                 :db.type/instant 6
                                 nil))
          ;; Drop nil-valued keys per PG's information_schema convention —
          ;; columns that don't apply to the type are simply absent (the
          ;; wire layer surfaces them as SQL NULL).
          drop-nils (fn [m] (into {} (remove (comp nil? val)) m))]
      (vec (for [[tname {:keys [columns]}] (sort-by key tables)
                 [idx col] (map-indexed vector
                                        (cons {:name "db_id" :valuetype :db.type/long :unique :db.unique/identity} columns))
                 :let [vtype     (:valuetype col)
                       pos       (long (inc idx))
                       identity? (and (= "db_id" (:name col)) (zero? idx))]]
             (drop-nils
              {:information_schema_columns/table_catalog          "datahike"
               :information_schema_columns/table_schema           "public"
               :information_schema_columns/table_name             tname
               :information_schema_columns/column_name            (:name col)
               :information_schema_columns/ordinal_position       pos
               :information_schema_columns/column_default         nil
               :information_schema_columns/is_nullable            (if identity? "NO" "YES")
               ;; From the column's DECLARED OID, not its storage
               ;; valueType — the same correction pg_attribute.atttypid
               ;; needed. :db.type/instant carries date, time and
               ;; timestamp alike, so a `date` column reported
               ;; `timestamp without time zone` here.
               :information_schema_columns/data_type
               (or (get types/oid->pg-name (:oid col)) (pgs/pg-type-name vtype))
               :information_schema_columns/character_maximum_length nil
               :information_schema_columns/character_octet_length nil
               :information_schema_columns/numeric_precision      (numeric-precision vtype (:attr col))
               :information_schema_columns/numeric_precision_radix (numeric-radix vtype)
               :information_schema_columns/numeric_scale          (numeric-scale vtype (:attr col))
               :information_schema_columns/datetime_precision     (datetime-precision vtype)
               :information_schema_columns/udt_catalog            "datahike"
               :information_schema_columns/udt_schema             "pg_catalog"
               :information_schema_columns/udt_name               (udt-name vtype)
               :information_schema_columns/dtd_identifier         (str pos)
               :information_schema_columns/is_self_referencing    "NO"
               :information_schema_columns/is_identity            (if identity? "YES" "NO")
               :information_schema_columns/identity_generation    (if identity? "BY DEFAULT" nil)
               :information_schema_columns/is_generated           "NEVER"
               :information_schema_columns/is_updatable           "YES"
               (pgs/row-marker-attr "information_schema_columns") true}))))
    "information_schema_tables"
    (mapv (fn [t]
            {:information_schema_tables/table_catalog "datahike"
             :information_schema_tables/table_schema "public"
             :information_schema_tables/table_name t
             :information_schema_tables/table_type "BASE TABLE"
             (pgs/row-marker-attr "information_schema_tables") true})
          (pgs/table-names user-schema))
    "pg_sequences"
    (mapv (fn [s]
            (cond-> {:pg_sequences/schemaname "public"
                     :pg_sequences/sequencename (:__seq__/name s)
                     :pg_sequences/sequenceowner "datahike"
                     :pg_sequences/data_type (:__seq__/type s)
                     :pg_sequences/start_value (:__seq__/start s)
                     :pg_sequences/min_value (:__seq__/minvalue s)
                     :pg_sequences/max_value (:__seq__/maxvalue s)
                     :pg_sequences/increment_by (:__seq__/increment s)
                     :pg_sequences/cycle (:__seq__/cycle s)
                     :pg_sequences/cache_size (:__seq__/cache s)
                     (pgs/row-marker-attr "pg_sequences") true}
              ;; Omit rather than store nil — an absent attribute is how
              ;; this layer spells SQL NULL, and last_value is NULL until
              ;; the sequence has been advanced.
              (sequence-last-value s)
              (assoc :pg_sequences/last_value (sequence-last-value s))))
          (sequence-entities cte-db))
    "information_schema_sequences"
    ;; Same source as pg_sequences. This used to hardcode
    ;; start/minimum/maximum to the bigint defaults, which stopped being
    ;; true once CREATE SEQUENCE started storing the real parameters
    ;; (issue #21) — `CREATE SEQUENCE s START 5 MAXVALUE 100` reported
    ;; start 1, max 2^63-1.
    (mapv (fn [s]
            {:information_schema_sequences/sequence_catalog "datahike"
             :information_schema_sequences/sequence_schema "public"
             :information_schema_sequences/sequence_name (:__seq__/name s)
             :information_schema_sequences/start_value (str (:__seq__/start s))
             :information_schema_sequences/minimum_value (str (:__seq__/minvalue s))
             :information_schema_sequences/maximum_value (str (:__seq__/maxvalue s))
             :information_schema_sequences/increment (str (:__seq__/increment s))
             (pgs/row-marker-attr "information_schema_sequences") true})
          (sequence-entities cte-db))
    "pg_indexes"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))]
      (vec
       (for [[tname {:keys [columns]}] (sort-by key tables)
             col columns
             :when (or (:unique col) (:indexed? col))
             :let [unique? (some? (:unique col))
                   idxname (str tname "_" (:name col) (if unique? "_key" "_idx"))]]
         {:pg_indexes/schemaname "public"
          :pg_indexes/tablename tname
          :pg_indexes/indexname idxname
          :pg_indexes/tablespace "pg_default"
          :pg_indexes/indexdef (str "CREATE "
                                    (when unique? "UNIQUE ")
                                    "INDEX " idxname
                                    " ON public." tname
                                    " (" (:name col) ")")
          (pgs/row-marker-attr "pg_indexes") true})))

    ;; pg_index — internal index catalog pgjdbc's PK probe joins on.
    ;; One row per PK (indisprimary=true) or single-col UNIQUE
    ;; (indisunique=true). indkey encodes the indexed column
    ;; position(s); for single-col PK/UNIQUE that's just [attnum].
    "pg_index"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))]
      (vec
       (for [[tname {:keys [columns]}] (sort-by key tables)
             [idx col] (map-indexed vector columns)
             :when (:unique col)
             :let [tbl-oid (or (pgs/table-oid cte-db tname)
                               (Math/abs (.hashCode ^String tname)))
                   ;; Synthesize an index oid deterministic from
                   ;; (tbl-oid, attname). Doesn't need to match PG's
                   ;; counter — just unique within pg_index.
                   idx-oid (bit-or 0x40000000 (bit-xor tbl-oid
                                                       (Math/abs (.hashCode
                                                                  ^String (:name col)))))
                   primary? (= :db.unique/identity (:unique col))
                   attnum (inc idx)
                   idx-name (str tname "_" (:name col)
                                 (if primary? "_pkey" "_key"))
                   ;; pg_get_indexdef format: "CREATE [UNIQUE] INDEX
                   ;; <name> ON <schema>.<table> USING btree (<col>)".
                   ;; Always UNIQUE here since we only synthesize rows
                   ;; for unique columns; btree is PG's default access
                   ;; method.
                   idxdef (str "CREATE UNIQUE INDEX " idx-name
                               " ON public." tname
                               " USING btree (" (:name col) ")")]]
         {:pg_index/indrelid (long tbl-oid)
          :pg_index/indexrelid (long idx-oid)
          :pg_index/indkey (str attnum)
          :pg_index/indisprimary primary?
          :pg_index/indisunique true
          :pg_index/indisvalid true
          :pg_index/indpred ""
          :pg_index/indexprs ""
          :pg_index/indexdef idxdef
          (pgs/row-marker-attr "pg_index") true})))

    ;; pg_attrdef — empty. LEFT JOIN in pgjdbc's field-metadata query
    ;; means missing rows are fine; the table just needs to exist so
    ;; JSqlParser and our catalog lookup don't fail to resolve it.
    "pg_attrdef"
    []
    ;; pg_extension — always empty; we never install extensions.
    "pg_extension"
    []
    ;; Always-empty tables. Modelling them as real (empty) catalog
    ;; tables so LEFT JOINs against them produce NULL fills rather
    ;; than dropping the entire row set (the empty-catalog short-
    ;; circuit doesn't differentiate INNER from LEFT).
    "pg_description"
    []
    "pg_stat_user_tables"
    []
    "pg_depend"
    []
    "pg_inherits"
    []
    "pg_enum"
    []
    ;; pg_constraint — one row per UNIQUE/PK column + per CHECK + per FK.
    ;; `condef` is the rendered text that pg_get_constraintdef returns.
    ;; Synthesised OIDs are stable hashes of (kind, name, table) so two
    ;; runs of the same DB produce the same oids — matching how
    ;; PG-side oids stay stable for the life of a constraint.
    "pg_constraint"
    (let [tables  (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))
          q-fn    d/q
          ;; CHECK constraints persisted via :pg/check-* attrs.
          checks  (try
                    (q-fn '{:find  [?n ?t ?x]
                            :keys  [name table expr]
                            :where [[?e :pg/check-name ?n]
                                    [?e :pg/check-table ?t]
                                    [?e :pg/check-expr ?x]]}
                          cte-db)
                    (catch Throwable _ []))
          ;; FK constraints persisted via :pg/fk-* attrs.
          fks (try
                (q-fn '{:find  [?n ?ct ?cc ?pt ?pc]
                        :keys  [name child-table child-cols parent-table parent-cols]
                        :where [[?e :pg/fk-name ?n]
                                [?e :pg/fk-child-table ?ct]
                                [?e :pg/fk-child-cols ?cc]
                                [?e :pg/fk-parent-table ?pt]
                                [?e :pg/fk-parent-cols ?pc]]}
                      cte-db)
                (catch Throwable _ []))
          ->oid (fn [kind nm tbl]
                  ;; Tag the high bit to avoid collisions with table OIDs.
                  (bit-or 0x50000000
                          (Math/abs (.hashCode ^String (str kind ":" nm ":" tbl)))))
          ;; pg_attribute attnums are 1-based positions in the column
          ;; list as derive-virtual-tables emits it. Mirror that order
          ;; here so conkey values reference the same slots.
          attnum-for (fn [tname col-name]
                       (some (fn [[idx col]]
                               (when (= col-name (:name col))
                                 (long (inc idx))))
                             (map-indexed vector
                                          (get-in tables [tname :columns]))))
          ->conkey   (fn [attnums]
                       ;; PG int2[] text form: "{1,2}" — already PG's
                       ;; canonical wire encoding for an int2[] column.
                       (str "{" (str/join "," attnums) "}"))]
      (vec
       (concat
        ;; UNIQUE / PRIMARY KEY rows — one per unique column.
        (for [[tname {:keys [columns]}] (sort-by key tables)
              col columns
              :when (:unique col)
              :let [tbl-oid  (or (pgs/table-oid cte-db tname)
                                 (Math/abs (.hashCode ^String tname)))
                    primary? (= :db.unique/identity (:unique col))
                    contype  (if primary? "p" "u")
                    cname    (str tname "_" (:name col)
                                  (if primary? "_pkey" "_key"))
                    condef   (str (if primary? "PRIMARY KEY (" "UNIQUE (")
                                  (:name col) ")")
                    attnum   (attnum-for tname (:name col))]]
          {:pg_constraint/oid           (long (->oid contype cname tname))
           :pg_constraint/conname       cname
           :pg_constraint/contype       contype
           :pg_constraint/conrelid      (long tbl-oid)
           :pg_constraint/connamespace  2200
           :pg_constraint/confrelid     0
           :pg_constraint/condeferrable false
           :pg_constraint/condeferred   false
           :pg_constraint/convalidated  true
           :pg_constraint/conkey        (->conkey (if attnum [attnum] []))
           :pg_constraint/confkey       "{}"
           :pg_constraint/condef        condef
           (pgs/row-marker-attr "pg_constraint") true})
        ;; CHECK constraints — one row per persisted :pg/check-*.
        (for [{cname :name tname :table cexpr :expr} checks
              :let [tbl-oid (or (pgs/table-oid cte-db tname)
                                (Math/abs (.hashCode ^String tname)))]]
          {:pg_constraint/oid           (long (->oid "c" cname tname))
           :pg_constraint/conname       cname
           :pg_constraint/contype       "c"
           :pg_constraint/conrelid      (long tbl-oid)
           :pg_constraint/connamespace  2200
           :pg_constraint/confrelid     0
           :pg_constraint/condeferrable false
           :pg_constraint/condeferred   false
           :pg_constraint/convalidated  true
           ;; CHECK constraints don't have a column-attnum vector at
           ;; the catalog level — PG synthesises conkey only when the
           ;; CHECK references a single column. We don't parse the
           ;; expression that deeply, so leave both keys empty.
           :pg_constraint/conkey        "{}"
           :pg_constraint/confkey       "{}"
           :pg_constraint/condef        (str "CHECK (" cexpr ")")
           (pgs/row-marker-attr "pg_constraint") true})
        ;; FK constraints — child-cols / parent-cols are stored as
        ;; JSON-serialized strings; render them as comma-joined
        ;; identifiers in the condef (matches PG's pg_get_constraintdef).
        (for [{cname :name child :child-table child-cols :child-cols
               parent :parent-table parent-cols :parent-cols} fks
              :let [tbl-oid (or (pgs/table-oid cte-db child)
                                (Math/abs (.hashCode ^String child)))
                    parent-oid (or (pgs/table-oid cte-db parent)
                                   (Math/abs (.hashCode ^String parent)))
                    parse-cols (fn [s]
                                 (try
                                   (let [v (jb/parse-jsonb s)]
                                     (cond (vector? v)     v
                                           (sequential? v) (vec v)
                                           :else           [v]))
                                   (catch Throwable _ [s])))
                    cs (parse-cols child-cols)
                    ps (parse-cols parent-cols)
                    child-attnums  (vec (keep #(attnum-for child %) cs))
                    parent-attnums (vec (keep #(attnum-for parent %) ps))]]
          {:pg_constraint/oid           (long (->oid "f" cname child))
           :pg_constraint/conname       cname
           :pg_constraint/contype       "f"
           :pg_constraint/conrelid      (long tbl-oid)
           :pg_constraint/connamespace  2200
           :pg_constraint/confrelid     (long parent-oid)
           :pg_constraint/condeferrable false
           :pg_constraint/condeferred   false
           :pg_constraint/convalidated  true
           :pg_constraint/conkey        (->conkey child-attnums)
           :pg_constraint/confkey       (->conkey parent-attnums)
           :pg_constraint/condef        (str "FOREIGN KEY (" (str/join ", " cs)
                                             ") REFERENCES " parent
                                             " (" (str/join ", " ps) ")")
           (pgs/row-marker-attr "pg_constraint") true}))))
    "information_schema_table_constraints"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))]
      (vec
       (concat
        (for [[tname _] (sort-by key tables)]
          {:information_schema_table_constraints/constraint_catalog "datahike"
           :information_schema_table_constraints/constraint_schema "public"
           :information_schema_table_constraints/constraint_name (str tname "_pkey")
           :information_schema_table_constraints/table_catalog "datahike"
           :information_schema_table_constraints/table_schema "public"
           :information_schema_table_constraints/table_name tname
           :information_schema_table_constraints/constraint_type "PRIMARY KEY"
           :information_schema_table_constraints/is_deferrable "NO"
           :information_schema_table_constraints/initially_deferred "NO"
           :information_schema_table_constraints/enforced "YES"
           (pgs/row-marker-attr "information_schema_table_constraints") true})
        (for [[tname {:keys [columns]}] (sort-by key tables)
              col columns
              :when (:unique col)]
          {:information_schema_table_constraints/constraint_catalog "datahike"
           :information_schema_table_constraints/constraint_schema "public"
           :information_schema_table_constraints/constraint_name (str tname "_" (:name col) "_key")
           :information_schema_table_constraints/table_catalog "datahike"
           :information_schema_table_constraints/table_schema "public"
           :information_schema_table_constraints/table_name tname
           :information_schema_table_constraints/constraint_type "UNIQUE"
           :information_schema_table_constraints/is_deferrable "NO"
           :information_schema_table_constraints/initially_deferred "NO"
           :information_schema_table_constraints/enforced "YES"
           (pgs/row-marker-attr "information_schema_table_constraints") true}))))
    "information_schema_key_column_usage"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))]
      (vec
       (concat
        (for [[tname _] (sort-by key tables)]
          {:information_schema_key_column_usage/constraint_catalog "datahike"
           :information_schema_key_column_usage/constraint_schema "public"
           :information_schema_key_column_usage/constraint_name (str tname "_pkey")
           :information_schema_key_column_usage/table_catalog "datahike"
           :information_schema_key_column_usage/table_schema "public"
           :information_schema_key_column_usage/table_name tname
           :information_schema_key_column_usage/column_name "db_id"
           :information_schema_key_column_usage/ordinal_position 1
           (pgs/row-marker-attr "information_schema_key_column_usage") true})
        (for [[tname {:keys [columns]}] (sort-by key tables)
              col columns
              :when (:unique col)]
          {:information_schema_key_column_usage/constraint_catalog "datahike"
           :information_schema_key_column_usage/constraint_schema "public"
           :information_schema_key_column_usage/constraint_name (str tname "_" (:name col) "_key")
           :information_schema_key_column_usage/table_catalog "datahike"
           :information_schema_key_column_usage/table_schema "public"
           :information_schema_key_column_usage/table_name tname
           :information_schema_key_column_usage/column_name (:name col)
           :information_schema_key_column_usage/ordinal_position 1
           :information_schema_key_column_usage/position_in_unique_constraint 1
           (pgs/row-marker-attr "information_schema_key_column_usage") true}))))
    nil))

(declare collect-in-stmt! collect-in-expr!)

(defn- collect-in-plain!
  "Recurse into a PlainSelect's FROM-item and JOIN right-items. For
   each Table that maps to a catalog, mutate it to the internal name
   and record it in the accumulator. For ParenthesedSelect items
   (derived tables), recurse into the inner select. Also scan WHERE
   for embedded subqueries (EXISTS / scalar subquery references).
   Returns the updated accumulator set.

   Arg order is `[acc stmt]` consistently across the helpers so that
   `(reduce collect-in-plain! #{} stmts)` works as a natural
   accumulator pattern."
  [acc ^PlainSelect stmt]
  (let [items (cons (.getFromItem stmt)
                    (map (fn [^Join j] (.getRightItem j))
                         (or (.getJoins stmt) [])))
        acc (reduce
             (fn [acc item]
               (cond
                 (instance? Table item)
                 (if-let [tn (catalog-table-name ^Table item)]
                   (do (.setName ^Table item tn)
                       (.setSchemaName ^Table item nil)
                       (conj acc tn))
                   acc)
                 (instance? ParenthesedSelect item)
                 (collect-in-stmt! acc (.getSelect ^ParenthesedSelect item))
                 :else acc))
             acc
             items)
        where-expr (.getWhere stmt)]
    (if where-expr
      (collect-in-expr! acc where-expr)
      acc)))

(defn- collect-in-expr!
  "Scan an Expression tree for embedded ParenthesedSelects (subqueries
   in WHERE / projection / EXISTS) and recurse into them."
  [acc expr]
  (cond
    (nil? expr) acc
    (instance? ParenthesedSelect expr)
    (collect-in-stmt! acc (.getSelect ^ParenthesedSelect expr))
    (instance? net.sf.jsqlparser.expression.operators.relational.ExistsExpression expr)
    (collect-in-expr!
     acc
     (.getRightExpression ^net.sf.jsqlparser.expression.operators.relational.ExistsExpression expr))
    (instance? net.sf.jsqlparser.expression.BinaryExpression expr)
    (let [^net.sf.jsqlparser.expression.BinaryExpression be expr]
      (-> acc
          (collect-in-expr! (.getLeftExpression be))
          (collect-in-expr! (.getRightExpression be))))
    (instance? net.sf.jsqlparser.expression.Parenthesis expr)
    (collect-in-expr! acc
                      (.getExpression ^net.sf.jsqlparser.expression.Parenthesis expr))
    :else acc))

(defn collect-in-stmt!
  "Walk a SELECT / ParenthesedSelect / SetOperationList statement and
   collect the set of catalog table names it references (anywhere —
   top-level FROM/JOINs, derived tables, UNION branches, WHERE
   subqueries, CTE bodies). Mutates matching Table nodes to the
   normalized internal catalog name. Returns the accumulated set."
  ([stmt] (collect-in-stmt! #{} stmt))
  ([acc stmt]
   (cond
     (nil? stmt) acc
     (instance? PlainSelect stmt)
     (let [^PlainSelect ps stmt
           acc (reduce (fn [a ^net.sf.jsqlparser.statement.select.WithItem wi]
                         ;; .getSelect unsafely casts the underlying
                         ;; ParenthesedStatement to ParenthesedSelect —
                         ;; it bombs on DML-bodied CTEs (INSERT/UPDATE/
                         ;; DELETE inside WITH …). Drop through
                         ;; .getParenthesedStatement instead and only
                         ;; recurse when the body is actually a
                         ;; select-shaped node.
                         (let [body (try (.getParenthesedStatement wi)
                                         (catch Throwable _ nil))
                               inner (cond
                                       (instance? PlainSelect body) body
                                       (instance? SetOperationList body) body
                                       (instance? ParenthesedSelect body)
                                       (.getSelect ^ParenthesedSelect body)
                                       :else nil)]
                           (if inner (collect-in-stmt! a inner) a)))
                       acc
                       (or (.getWithItemsList ps) []))]
       (collect-in-plain! acc ps))
     (instance? ParenthesedSelect stmt)
     (collect-in-stmt! acc (.getSelect ^ParenthesedSelect stmt))
     (instance? SetOperationList stmt)
     (reduce collect-in-stmt! acc (.getSelects ^SetOperationList stmt))
     :else acc)))

(defn catalog-tables-used
  "Walk a PlainSelect's FROM + JOIN items (plus any nested derived
   tables, UNION branches, and WHERE subqueries) and return the set of
   catalog table names referenced. Mutates matching Table nodes to the
   normalized internal name so the SQL translator sees `pg_attribute`
   instead of `pg_catalog.pg_attribute`."
  [^PlainSelect stmt]
  (collect-in-stmt! stmt))

(defn catalog-tables-in-stmt
  "Same as catalog-tables-used but accepts any Statement (PlainSelect,
   ParenthesedSelect, SetOperationList). Used by sql/parse-sql's
   top-level dispatch to pick up catalog refs under UNIONs and
   top-level ParenthesedSelects."
  [stmt]
  (collect-in-stmt! stmt))

;; ============================================================================
;; System query detection
;; ============================================================================

(defn extract-empty-catalog-shape
  "Parse a SELECT that was classified as `:empty-catalog` and return
   `{:names [String…] :oids [int…]}` matching the projection shape the
   client expects in RowDescription. Used when we respond to a known-
   empty catalog query with zero rows — clients like pgJDBC's
   `DatabaseMetaData.getTables` issue 12-column SELECTs and will raise
   `column index out of range` if the RowDescription doesn't match.

   For `SELECT *` and anything we can't parse, returns nil so callers
   can fall back to a minimal 1-column shape (which is wrong, but
   harmless for psycopg2 / asyncpg that always go by column name).

   Types are all OID_TEXT. That's the honest answer (we don't know),
   and it matches PG's `unknown`-to-`text` coercion at the wire
   boundary for untyped columns."
  [^String sql]
  (try
    (let [stmt (CCJSqlParserUtil/parse ^String sql)]
      (when-let [^PlainSelect ps (cond
                                   (instance? PlainSelect stmt) stmt
                                   (instance? ParenthesedSelect stmt)
                                   (.getSelect ^ParenthesedSelect stmt)
                                   :else nil)]
        (let [items (.getSelectItems ps)]
          ;; Bail on SELECT * — we have no schema for catalog views,
          ;; so the column shape is unknowable without materialising.
          (when-not (some (fn [^SelectItem it]
                            (instance? AllColumns (.getExpression it)))
                          items)
            (let [names (mapv
                         (fn [^SelectItem it]
                           (or
                            ;; Explicit AS alias takes precedence.
                            (when-let [a (.getAlias it)]
                              (unquote-ident (.getName ^Alias a)))
                            ;; Bare column reference: use the column name.
                            (let [e (.getExpression it)]
                              (when (instance? Column e)
                                (unquote-ident (.getColumnName ^Column e))))
                            ;; Anything else (function call, expression
                            ;; without alias): PG would name it
                            ;; "?column?" — mirror that.
                            "?column?"))
                         items)]
              {:names names
               :oids  (vec (repeat (count names) types/oid-text))})))))
    (catch Exception _
      nil)))

(def classify-system-kinds
  "Classifier :kind values that route to the system-type dispatch in
   server.clj. Kinds NOT in this set fall through to JSqlParser or the
   complex-pattern catalog probes below."
  #{:set :show
    :prepare :execute-prepared :deallocate
    :declare-cursor :fetch-cursor :close-cursor :move-cursor
    :begin :commit :savepoint :release-savepoint :rollback-to-savepoint
    :discard-all :discard-scoped
    ;; RESET ALL / RESET <var> (datahike.* + statement_timeout RESETs are
    ;; intercepted earlier in the simple-query path; everything else
    ;; lands on the :reset handler). LISTEN/UNLISTEN/NOTIFY are no-ops
    ;; (no notification delivery) — UNLISTEN especially must bypass
    ;; JSqlParser, which can't parse it. All hit asyncpg's pool reset.
    :reset :listen-noop :unlisten-noop :notify-noop
    ;; Transaction isolation level: SET SESSION CHARACTERISTICS … and
    ;; SET [LOCAL] TRANSACTION ISOLATION LEVEL … track the session/tx
    ;; isolation that SHOW transaction_isolation must report back.
    :set-session-isolation :set-transaction-isolation
    :version :now :current-schema :current-database
    :pg-keywords :nextval :currval :lastval :setval
    :try-advisory-xact-lock :try-advisory-lock
    :advisory-xact-lock :advisory-unlock-all :advisory-unlock :advisory-lock
    :pg-backend-pid :txid-current :pg-sleep :pg-notify
    :comment-on :lock-table :create-index
    :maintenance-noop :schema-noop
    :create-database :drop-database
    ;; CREATE TYPE … AS ENUM and CREATE DOMAIN both bypass JSqlParser
    ;; (which can't / won't parse them) and run our own parsers.
    :create-type-enum :create-type-composite :create-domain
    ;; CREATE / ALTER SEQUENCE — JSqlParser's grammar covers only a
    ;; subset of PG's option list (INCREMENT BY but not INCREMENT,
    ;; no AS / IF NOT EXISTS / NO MINVALUE / signed values) and has no
    ;; AlterSequence branch downstream, so both are parsed here.
    :create-sequence :alter-sequence
    ;; TRUNCATE (whole statement — JSqlParser's Truncate grammar lacks
    ;; RESTART/CONTINUE IDENTITY) and multi-name DROP TABLE (JSqlParser
    ;; 5.2 parses a single name only). Both re-tagged to non-:system
    ;; types in parse-sql's sys-type case, like :create-domain.
    :truncate :drop-table-multi
    ;; pg_dump-emitted utility statements we silently accept
    :owner-noop :psql-meta :set-config
    ;; COPY-IN routes through the wire-protocol sub-protocol; the
    ;; exec-system handler is just a thin trampoline that returns a
    ;; QueryResult with copyInMode set, so the Java wire layer
    ;; emits CopyInResponse and transitions to COPY-IN.
    :copy-from-stdin
    ;; datahike.* branching / versioning functions
    :dh-branches :dh-current-branch :dh-commit-id :dh-parent-commits
    :dh-create-branch :dh-delete-branch})

(defn system-query?*
  "Inner implementation — takes an already-computed classify result so
   callers that have one (parse-sql) can avoid classifying twice per
   statement.

   Two-stage routing: kinds from the classifier's whitelist go
   straight through; everything else is a candidate SELECT body that
   shape/catalog-probe inspects structurally (see shape.clj for the
   probe catalogue)."
  [^String sql cls-info]
  (let [kind (:kind cls-info)]
    (cond
      (contains? classify-system-kinds kind) kind
      (= :generic-sql kind) (shape/catalog-probe sql)
      :else nil)))

(defn system-query?
  "Check if a SQL string is a system/catalog query and return the
   handler keyword for the dispatch in server.clj; nil otherwise.

   Delegates the leading-keyword routing to datahike.pg.sql.classify for
   structural correctness (keyword-inside-string, keyword-inside-
   comment, case mix). A handful of complex pgjdbc / Odoo catalog
   probes still use substring matching on deep SELECT bodies — those
   stay here until we grow an AST-shape matcher."
  [^String sql]
  (system-query?* sql (cls/classify sql)))

;; ============================================================================
;; Main entry point
;; ============================================================================
