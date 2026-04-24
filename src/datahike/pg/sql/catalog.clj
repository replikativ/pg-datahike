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
  (:require [clojure.string :as str]
            [datahike.pg.classify :as cls]
            [datahike.pg.schema :as pgs]
            [datahike.pg.shape :as shape]
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

(def ^:private built-in-catalog-tables
  "Virtual catalog tables this layer materializes on demand. Extension
   tables are added via register-catalog-table! and don't appear here."
  #{"pg_type" "pg_class" "pg_tables" "pg_views" "pg_matviews" "pg_attribute"
    "pg_namespace" "pg_database" "pg_proc"
    "pg_indexes"
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
          schema (when-let [s (.getSchemaName t)] (str/lower-case s))
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
   row entity-maps)."
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
     {:db/ident :pg_type/typtype :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_type") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_attribute"
    [{:db/ident :pg_attribute/attname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_attribute/atttypid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
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
     {:db/ident (pgs/row-marker-attr "pg_attribute") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    "pg_namespace"
    [{:db/ident :pg_namespace/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_namespace/nspname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "pg_namespace") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
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
    "pg_constraint"
    ;; One row per CHECK / FK / PK / UNIQUE constraint. `condef` is
    ;; the synthesized text — `pg_get_constraintdef(oid)` lowers to
    ;; `[?c :pg_constraint/oid ?arg] [?c :pg_constraint/condef ?out]`.
    ;; We deliberately do NOT model the int2[]/oid[] vector columns
    ;; (conkey, confkey, conpfeqop) — clients that read them via
    ;; `pg_get_constraintdef` get the rendered text instead.
    [{:db/ident :pg_constraint/oid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/conname :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/contype :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/conrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/connamespace :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/confrelid :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/condeferrable :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/condeferred :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :pg_constraint/convalidated :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
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
    ;; pg_views — list of all user-defined views. We don't store views
    ;; so this is always empty, but ORMs (Metabase, pgAdmin) union it
    ;; with pg_tables during table discovery; not having it raises.
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
    "information_schema_columns"
    [{:db/ident :information_schema_columns/table_catalog :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/table_schema :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/table_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/column_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/ordinal_position :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/data_type :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/udt_name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/character_maximum_length :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_columns/is_nullable :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
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
     {:db/ident :information_schema_key_column_usage/ordinal_position :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident :information_schema_key_column_usage/position_in_unique_constraint :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
     {:db/ident (pgs/row-marker-attr "information_schema_key_column_usage") :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
    nil))

(defn catalog-data-for*
  "Built-in catalog data — see catalog-schema-for*. Dispatches a
   per-table body; library consumers add more via the registry."
  [table-name user-schema cte-db]
  (case table-name
    "pg_type"
    (mapv (fn [[oid tname tlen ttype]]
            {:pg_type/oid (Long/parseLong oid) :pg_type/typname tname
             :pg_type/typlen (Long/parseLong tlen) :pg_type/typtype ttype
             (pgs/row-marker-attr "pg_type") true})
          [["16" "bool" "1" "b"] ["20" "int8" "8" "b"] ["23" "int4" "4" "b"]
           ["25" "text" "-1" "b"] ["700" "float4" "4" "b"] ["701" "float8" "8" "b"]
           ["1043" "varchar" "-1" "b"] ["1082" "date" "4" "b"]
           ["1114" "timestamp" "8" "b"] ["2950" "uuid" "16" "b"]])
    "pg_attribute"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))]
      (vec (for [[tname {:keys [columns]}] (sort-by key tables)
                 [idx col] (map-indexed vector columns)
                 :let [tbl-oid (or (pgs/table-oid cte-db tname)
                                   ;; Pre-existing tables from before we
                                   ;; started tracking :pg/table-oid — fall
                                   ;; back to the attnum-derived composite
                                   ;; key convention (name → hash) so stale
                                   ;; data still has a stable attrelid.
                                   (Math/abs (.hashCode ^String tname)))
                       pk? (= :db.unique/identity (:unique col))]]
             {:pg_attribute/attname (:name col)
              :pg_attribute/atttypid (long (pgs/oid-for-valuetype (:valuetype col)))
              :pg_attribute/attnum (long (inc idx))
              :pg_attribute/attrelid (long tbl-oid)
              :pg_attribute/attnotnull pk?
              :pg_attribute/attidentity ""
              (pgs/row-marker-attr "pg_attribute") true})))
    "pg_namespace"
    [{:pg_namespace/oid 2200 :pg_namespace/nspname "public"
      (pgs/row-marker-attr "pg_namespace") true}]
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
           ["pg_backend_pid" "s" 0 23 ""]
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
    (mapv (fn [t]
            (let [tbl-oid (or (pgs/table-oid cte-db t)
                              (Math/abs (.hashCode ^String t)))]
              {:pg_class/oid (long tbl-oid)
               :pg_class/relname t
               :pg_class/relnamespace 2200
               :pg_class/relkind "r"
               (pgs/row-marker-attr "pg_class") true}))
          (pgs/table-names user-schema))
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
    "information_schema_columns"
    (let [tables (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))
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
                       "text"))]
      (vec (for [[tname {:keys [columns]}] (sort-by key tables)
                 [idx col] (map-indexed vector
                                        (cons {:name "db_id" :valuetype :db.type/long} columns))]
             {:information_schema_columns/table_catalog "datahike"
              :information_schema_columns/table_schema "public"
              :information_schema_columns/table_name tname
              :information_schema_columns/column_name (:name col)
              :information_schema_columns/ordinal_position (str (inc idx))
              :information_schema_columns/data_type (pgs/pg-type-name (:valuetype col))
              :information_schema_columns/udt_name (udt-name (:valuetype col))
              :information_schema_columns/is_nullable "YES"
              (pgs/row-marker-attr "information_schema_columns") true})))
    "information_schema_tables"
    (mapv (fn [t]
            {:information_schema_tables/table_catalog "datahike"
             :information_schema_tables/table_schema "public"
             :information_schema_tables/table_name t
             :information_schema_tables/table_type "BASE TABLE"
             (pgs/row-marker-attr "information_schema_tables") true})
          (pgs/table-names user-schema))
    "information_schema_sequences"
    (let [q-fn (requiring-resolve 'datahike.api/q)
          seq-results (q-fn '{:find [?n ?v ?i]
                              :where [[?e :__seq__/name ?n]
                                      [?e :__seq__/value ?v]
                                      [?e :__seq__/increment ?i]]}
                            cte-db)]
      (mapv (fn [[sname _sval sincr]]
              {:information_schema_sequences/sequence_catalog "datahike"
               :information_schema_sequences/sequence_schema "public"
               :information_schema_sequences/sequence_name sname
               :information_schema_sequences/start_value "1"
               :information_schema_sequences/minimum_value "1"
               :information_schema_sequences/maximum_value "9223372036854775807"
               :information_schema_sequences/increment (str sincr)
               (pgs/row-marker-attr "information_schema_sequences") true})
            seq-results))
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
    ;; pg_constraint — one row per UNIQUE/PK column + per CHECK + per FK.
    ;; `condef` is the rendered text that pg_get_constraintdef returns.
    ;; Synthesised OIDs are stable hashes of (kind, name, table) so two
    ;; runs of the same DB produce the same oids — matching how
    ;; PG-side oids stay stable for the life of a constraint.
    "pg_constraint"
    (let [tables  (pgs/derive-virtual-tables user-schema (pgs/schema-hints cte-db))
          q-fn    (requiring-resolve 'datahike.api/q)
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
                          (Math/abs (.hashCode ^String (str kind ":" nm ":" tbl)))))]
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
                                  (:name col) ")")]]
          {:pg_constraint/oid           (long (->oid contype cname tname))
           :pg_constraint/conname       cname
           :pg_constraint/contype       contype
           :pg_constraint/conrelid      (long tbl-oid)
           :pg_constraint/connamespace  2200
           :pg_constraint/confrelid     0
           :pg_constraint/condeferrable false
           :pg_constraint/condeferred   false
           :pg_constraint/convalidated  true
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
                                   (let [v ((requiring-resolve 'datahike.pg.jsonb/parse-jsonb) s)]
                                     (cond (vector? v)     v
                                           (sequential? v) (vec v)
                                           :else           [v]))
                                   (catch Throwable _ [s])))
                    cs (parse-cols child-cols)
                    ps (parse-cols parent-cols)]]
          {:pg_constraint/oid           (long (->oid "f" cname child))
           :pg_constraint/conname       cname
           :pg_constraint/contype       "f"
           :pg_constraint/conrelid      (long tbl-oid)
           :pg_constraint/connamespace  2200
           :pg_constraint/confrelid     (long parent-oid)
           :pg_constraint/condeferrable false
           :pg_constraint/condeferred   false
           :pg_constraint/convalidated  true
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
           :information_schema_key_column_usage/ordinal_position "1"
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
           :information_schema_key_column_usage/ordinal_position "1"
           :information_schema_key_column_usage/position_in_unique_constraint "1"
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
                         (collect-in-stmt! a (.getSelect wi)))
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
    :version :now :current-schema :current-database
    :pg-keywords :nextval :currval :setval
    :try-advisory-xact-lock :try-advisory-lock
    :advisory-xact-lock :advisory-unlock-all :advisory-unlock :advisory-lock
    :pg-backend-pid :txid-current :pg-sleep
    :comment-on :lock-table :create-view :create-index
    :maintenance-noop :schema-noop
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

   Delegates the leading-keyword routing to datahike.pg.classify for
   structural correctness (keyword-inside-string, keyword-inside-
   comment, case mix). A handful of complex pgjdbc / Odoo catalog
   probes still use substring matching on deep SELECT bodies — those
   stay here until we grow an AST-shape matcher."
  [^String sql]
  (system-query?* sql (cls/classify sql)))

;; ============================================================================
;; Main entry point
;; ============================================================================

