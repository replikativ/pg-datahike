(ns datahike.pg.schema
  "Derives virtual PostgreSQL table definitions from Datahike schemas.

   Maps attribute namespace prefixes to table names and attribute local names
   to column names. For example:
     :person/name  → table 'person', column 'name'
     :person/age   → table 'person', column 'age'

   Every virtual table gets an implicit 'db_id' column (the entity ID)."
  (:require [clojure.string :as str]
            [datahike.pg.types :as types]))

(set! *warn-on-reflection* true)

;; Delegate to centralized type registry
(defn oid-for-valuetype
  "Return the PostgreSQL type OID for a Datahike valueType keyword."
  [vtype]
  (types/oid-for-dh-type vtype))

(defn pg-type-name
  "Return the PostgreSQL type name string for a Datahike valueType keyword."
  [vtype]
  (types/pg-name-for-dh-type vtype))

;; ============================================================================
;; Internal namespace filter
;; ============================================================================

(def ^:private internal-ns-prefixes
  #{"db" "db.type" "db.cardinality" "db.unique" "db.install"
    "db.entity" "db.part" "db.secondary" "db.sys"
    ;; Pgwire's own meta-attrs for schema hints (see hint-schema below)
    ;; and for DDL-emitted constraints / catalog integrity. These live
    ;; on ident entities to tune the SQL-side view; they must not
    ;; themselves appear as virtual tables.
    "datahike.pg" "pg"})

;; ============================================================================
;; User-facing schema hints
;; ============================================================================
;;
;; Opt-in meta-attributes that a user transacts onto existing `:db/ident`
;; entities to customize the SQL view of a Datahike-native database. Inert
;; from Datahike's perspective — schema.clj and the translator consult them
;; when deriving virtual tables.
;;
;; Example:
;;   (d/transact conn
;;     [{:db/ident :person/full_name  :datahike.pg/column     "name"}
;;      {:db/ident :person/ssn        :datahike.pg/hidden     true}
;;      {:db/ident :person/company    :datahike.pg/references :company/id}])
;;
;; The server's ensure-pg-schema! installs the meta-attr definitions
;; automatically so users can transact hints without pre-declaring under
;; `:schema-flexibility :write`; `:read` callers can eagerly call
;; (d/transact conn pgs/hint-schema) first.

(def hint-schema
  "Datahike schema for the :datahike.pg/* hint entities. A hint is a
   dedicated entity (NOT attributes attached to the ident) that points at
   its target via :datahike.pg/for-ident. This sidesteps Datahike's
   schema-update guard, which forbids adding arbitrary attrs to an
   already-transacted schema entity — a necessary limit on real schema
   migration but one that makes ident-attached annotations impractical.

   `ensure-pg-schema!` in server.clj installs this for every handler-
   backed conn. Users can also transact it eagerly under
   `:schema-flexibility :read`."
  [{:db/ident :datahike.pg/for-ident
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Points the hint entity at its target attribute's ident (e.g. :widget/full_name). One hint entity per ident."}
   {:db/ident :datahike.pg/column
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Override the SQL column name for the target attribute. Defaults to the attribute's local name."}
   {:db/ident :datahike.pg/hidden
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "When true, the target attribute is excluded from virtual table derivation (pg_tables, information_schema.columns, SELECT *)."}
   {:db/ident :datahike.pg/references
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "On a :db.type/ref target, the attribute ident whose value JOINs resolve against (e.g. :company/id). Makes `JOIN … ON a.fk = b.pk` work on native Datahike schemas where refs store entity-ids."}
   {:db/ident :datahike.pg/table
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Override the SQL table name for the target attribute's namespace. Rare — namespace/table symmetry is usually what you want."}])

(defn ensure-hint-schema!
  "Idempotently install the :datahike.pg/* hint schema attrs on `conn`.
   Safe to call multiple times. Called automatically by `set-hint!` and
   by the server's `ensure-pg-schema!`. Exposed as public so callers
   running under `:schema-flexibility :read` can prime the schema
   before their first `set-hint!` or eager hint transaction."
  [conn]
  (let [transact-fn (requiring-resolve 'datahike.api/transact)
        db-fn       (requiring-resolve 'datahike.api/db)
        schema      (:schema (db-fn conn))
        missing     (remove (fn [{:keys [db/ident]}] (get schema ident))
                            hint-schema)]
    (when (seq missing)
      (transact-fn conn (vec missing)))))

(defn set-hint!
  "Convenience: transact or upsert one hint entity. `target` is the
   attribute ident the hint applies to (e.g. `:widget/full_name`).
   `hint` is a map of `:column` / `:hidden` / `:references` / `:table`.
   Idempotent — `:datahike.pg/for-ident` is :db.unique/identity, so
   repeated calls upsert the same hint entity. Ensures the hint schema
   is installed first so callers needn't pre-install."
  [conn target hint]
  (ensure-hint-schema! conn)
  (let [transact-fn (requiring-resolve 'datahike.api/transact)
        entity (-> {:datahike.pg/for-ident target}
                   (into (keep (fn [[k v]]
                                 (when (some? v)
                                   [(keyword "datahike.pg" (name k)) v])))
                         hint))]
    (transact-fn conn [entity])))

(defn derive-ref-targets
  "For each `:db.type/ref` attribute, determine the *target PK attribute*
   that SQL projection should dereference to. `SELECT order.customer FROM
   order` should return the customer's PK value (1, 2, 3) — what a real
   PostgreSQL FK column stores — not the Datahike entity-id (10, 11, 12)
   that the ref attribute physically holds.

   Returns `{ref-attr-ident → target-pk-attr-ident}` (a subset of the
   ref attrs in the schema; refs with no resolvable target are omitted
   and fall back to projecting the raw entity-id).

   Resolution order:
   1. Explicit `:datahike.pg/references K` hint (authoritative).
   2. Convention: `(name ref-attr)` names a namespace, and the
      `:db.unique/identity` attr in that namespace is the target.
      Example: `:order/customer` (ref) → namespace `customer` →
      `:customer/id` (the only `:db.unique/identity` in `customer`).
      This matches the seed style most projects use, where ref attrs
      are named after the table they point to.

   Only `:db.cardinality/one` refs are derefed — many-cardinality refs
   would yield multiple PK rows per source row, which has no clean SQL
   projection equivalent."
  [schema hints]
  (let [;; Build {namespace → unique-identity-attr} index once.
        ns->unique-id (reduce-kv
                       (fn [m k v]
                         (if (and (keyword? k)
                                  (= :db.unique/identity (:db/unique v))
                                  (namespace k))
                           (assoc m (namespace k) k)
                           m))
                       {}
                       schema)]
    (reduce-kv
     (fn [m attr-ident props]
       (if (and (keyword? attr-ident)
                (= :db.type/ref (:db/valueType props))
                (= :db.cardinality/one (:db/cardinality props)))
         (let [hint (get hints attr-ident)
               target (or (:references hint)
                          (get ns->unique-id (name attr-ident)))]
           (if target (assoc m attr-ident target) m))
         m))
     {}
     schema)))

(defn schema-hints
  "Return `{attr-ident → {:column str? :hidden bool? :references kw? :table str?}}`
   by scanning the db for :datahike.pg/for-ident-rooted hint entities.
   Nil-safe: returns an empty map when `db` is nil (pure-schema call sites)."
  [db]
  (if db
    (let [q-fn (requiring-resolve 'datahike.api/q)
          ;; The :datahike.pg/* attrs may not yet be in the schema (bare
          ;; conn without ensure-pg-schema!) — keep the query resilient
          ;; to each attr being absent by using per-attr lookups instead
          ;; of a single join.
          rows (q-fn '{:find [?for-ident ?e]
                       :where [[?e :datahike.pg/for-ident ?for-ident]]}
                     db)
          pull-fn (requiring-resolve 'datahike.api/pull)]
      (into {}
            (keep (fn [[for-ident e]]
                    (let [p (pull-fn db
                                     '[:datahike.pg/column
                                       :datahike.pg/hidden
                                       :datahike.pg/references
                                       :datahike.pg/table]
                                     e)
                          h (cond-> {}
                              (:datahike.pg/column p)     (assoc :column (:datahike.pg/column p))
                              (:datahike.pg/hidden p)     (assoc :hidden true)
                              (:datahike.pg/references p) (assoc :references (:datahike.pg/references p))
                              (:datahike.pg/table p)      (assoc :table (:datahike.pg/table p)))]
                      (when (seq h) [for-ident h]))))
            rows))
    {}))

(def ^:const row-marker-col "db-row-exists")

(defn row-marker-attr
  "Return the row-existence marker attribute for a table.
   E.g., :person/db-row-exists for table 'person'."
  [table-name]
  (keyword table-name row-marker-col))

;; PG OID allocation starts here. Values below this are PG's
;; pre-assigned system OIDs (pg_type built-ins, etc.), which clients may
;; assume are stable. User tables get 16384+ to avoid any collision.
(def ^:const first-user-oid 16384)

(defn table-oid
  "The :pg/table-oid attached to this table's row-marker entity, or nil
   if the table doesn't exist yet or was created before we started
   allocating OIDs. Datahike's `(:schema db)` only surfaces schema-level
   attrs (:db/valueType etc.); custom attrs on the ident entity require
   a Datalog lookup against the db."
  [db table-name]
  (let [q-fn (requiring-resolve 'datahike.api/q)]
    (ffirst (q-fn '{:find [?oid]
                    :in [$ ?ident]
                    :where [[?e :db/ident ?ident]
                            [?e :pg/table-oid ?oid]]}
                  db (row-marker-attr table-name)))))

(defn next-table-oid
  "Pick the next unused :pg/table-oid. We query via Datalog because the
   attribute is attached to the row-marker entity, not part of the
   :schema map."
  [db]
  (let [q-fn (requiring-resolve 'datahike.api/q)
        used (into #{}
                   (map first)
                   (q-fn '{:find [?oid]
                           :where [[?e :pg/table-oid ?oid]]}
                         db))
        mx (if (seq used) (apply max used) (dec first-user-oid))]
    (inc mx)))

(declare derive-virtual-tables)

(defn column-attnum
  "Return the 1-based PG attnum for a column, given the derived
   virtual-table map. The ordering MUST match pg_attribute row
   emission and RowDescription emission or pgjdbc's field-metadata
   JOIN (keyed on (tableOid, attnum)) won't resolve columns — so both
   sites call this helper.

   Returns nil when the table or column isn't known."
  [schema table-name col-name]
  (let [cols (get-in (derive-virtual-tables schema) [table-name :columns])]
    (when (seq cols)
      (some (fn [[i c]] (when (= col-name (:name c)) (inc i)))
            (map-indexed vector cols)))))

(defn- internal-attr?
  "Return true if an attribute ident belongs to the internal Datahike schema
   or is a SQL row-existence marker."
  [ident]
  (when-let [ns (namespace ident)]
    (or (contains? internal-ns-prefixes ns)
        (str/starts-with? ns "db.")
        (= (name ident) row-marker-col))))

;; ============================================================================
;; Virtual table derivation
;; ============================================================================

(defn- attr->table-col
  "Split an attribute ident into [table-name column-name].
   :person/name → [\"person\" \"name\"]"
  [ident]
  (when-let [ns (namespace ident)]
    [ns (name ident)]))

(defn derive-virtual-tables
  "Derive virtual table definitions from a Datahike schema map.

   Returns a map of table-name → {:columns [...] :attrs {...}}
   where each column is {:name str :attr keyword :oid int :valuetype keyword
                         :cardinality keyword :unique keyword-or-nil :ref? bool
                         :references keyword-or-nil}
   and :attrs maps column-name → attribute-ident.

   Filters out internal (db.*) attributes by default.

   When `hints` is supplied (a map `{attr-ident → hint-map}` typically
   built by `schema-hints`), applies per-attribute customizations:
   - `:datahike.pg/hidden true`  → attribute excluded entirely
   - `:datahike.pg/column \"x\"` → column renamed to \"x\"
   - `:datahike.pg/references K`→ carried through on the col as `:references`
     for the translator's FK-via-ref JOIN rewrite."
  ([schema] (derive-virtual-tables schema {}))
  ([schema hints]
   (let [user-attrs (remove (fn [[k _]] (or (not (keyword? k))
                                            (internal-attr? k)
                                            (nil? (namespace k))
                                            (:hidden (get hints k))))
                            schema)]
     (reduce
      (fn [tables [ident props]]
        (let [[table-name local-col] (attr->table-col ident)
              h (get hints ident)
              col-name (or (:column h) local-col)
              vtype (:db/valueType props)
              col {:name        col-name
                   :attr        ident
                   :oid         (oid-for-valuetype vtype)
                   :valuetype   vtype
                   :cardinality (:db/cardinality props)
                   :unique      (:db/unique props)
                   :ref?        (= vtype :db.type/ref)
                   :references  (:references h)
                   :indexed?    (or (:db/index props) (some? (:db/unique props)))}]
          (-> tables
              (update-in [table-name :columns] (fnil conj []) col)
              (assoc-in [table-name :attrs col-name] ident))))
      {}
      user-attrs))))

(defn table-names
  "Return sorted list of virtual table names for a schema."
  [schema]
  (sort (keys (derive-virtual-tables schema))))

;; ============================================================================
;; Catalog query helpers (pg_tables, information_schema, etc.)
;; ============================================================================

(defn information-schema-columns-rows
  "Return rows for information_schema.columns query.
   Each row: [table_catalog table_schema table_name column_name ordinal_position
              data_type is_nullable column_default].

   Accepts an optional `db` (or `hints` map) so :datahike.pg/column /
   :datahike.pg/hidden are applied to the emitted row set. Callers that
   don't have a db in scope can keep the 1-arity form."
  ([schema] (information-schema-columns-rows schema {}))
  ([schema db-or-hints]
   (let [hints  (if (map? db-or-hints) db-or-hints (schema-hints db-or-hints))
         tables (derive-virtual-tables schema hints)]
     (for [[tname {:keys [columns]}] (sort-by key tables)
           [idx col] (map-indexed vector (cons {:name "db_id" :valuetype :db.type/long} columns))]
       ["datahike" "public" tname (:name col) (str (inc idx))
        (pg-type-name (:valuetype col)) "YES" nil]))))

(defn column-order-from-db
  "Derive column creation order for a table from schema entity IDs.
   Returns [col-name ...] in the order attributes were transacted (CREATE TABLE order).
   Requires a Datahike db value. Falls back to alphabetical if db is nil."
  [db table-name]
  (if db
    (let [q-fn (requiring-resolve 'datahike.api/q)
          ns-prefix (str table-name "/")
          results (q-fn '{:find [?e ?ident]
                          :where [[?e :db/ident ?ident]]
                          :order-by [?e :asc]}
                        db)]
      (mapv (fn [[_ ident]] (name ident))
            (filter (fn [[_ ident]]
                      (and (keyword? ident)
                           (= table-name (namespace ident))
                           (not= (name ident) row-marker-col)))
                    results)))
    nil))

(defn column-info
  "Return ordered column info for a specific table, prepending db_id.
   When db is provided, columns are ordered by schema entity ID
   (CREATE TABLE order) and :datahike.pg/* hints (column rename, hide,
   FK target) are applied to the derivation.
   Returns [{:name str :attr keyword :oid int ...} ...]"
  ([schema table-name] (column-info schema table-name nil))
  ([schema table-name db]
   (let [hints  (schema-hints db)
         tables (derive-virtual-tables schema hints)]
     (when-let [table (get tables table-name)]
       (let [columns (:columns table)
             ;; Order by entity ID if db is available. Keyed by the
             ;; attr's local name (always the original, independent of
             ;; any :datahike.pg/column rename) so column-order-from-db's
             ;; original-name-based output still reconciles.
             ordered (if-let [col-order (when db (column-order-from-db db table-name))]
                       (let [col-map (into {} (map (fn [c] [(name (:attr c)) c]) columns))]
                         (vec (keep col-map col-order)))
                       columns)]
         (into [{:name "db_id" :attr :db/id :oid types/oid-int8
                 :valuetype :db.type/long :cardinality :db.cardinality/one
                 :ref? false :indexed? true}]
               ordered))))))
