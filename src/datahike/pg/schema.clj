(ns datahike.pg.schema
  "Derives virtual PostgreSQL table definitions from Datahike schemas.

   Maps attribute namespace prefixes to table names and attribute local names
   to column names. For example:
     :person/name  → table 'person', column 'name'
     :person/age   → table 'person', column 'age'

   Every virtual table gets an implicit 'db_id' column (the entity ID)."
  (:require [clojure.string :as str]
            [datahike.api :as d]
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
    :db/doc "Override the SQL table name for the target attribute's namespace. Rare — namespace/table symmetry is usually what you want."}
   {:db/ident :datahike.pg/enum-of
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Column's SQL type is a registered ENUM (`:datahike.pg.enum/name`). Storage lowers to text; the dump re-emits with the enum type name."}
   {:db/ident :datahike.pg/domain-of
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Column's SQL type is a registered DOMAIN (`:datahike.pg.domain/name`). Storage lowers to the domain's base type; the dump re-emits with the domain name."}])

(defn ensure-hint-schema!
  "Idempotently install the :datahike.pg/* hint schema attrs on `conn`.
   Safe to call multiple times. Called automatically by `set-hint!` and
   by the server's `ensure-pg-schema!`. Exposed as public so callers
   running under `:schema-flexibility :read` can prime the schema
   before their first `set-hint!` or eager hint transaction."
  [conn]
  (let [transact-fn d/transact
        db-fn       d/db
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
  (let [transact-fn d/transact
        entity (-> {:datahike.pg/for-ident target}
                   (into (keep (fn [[k v]]
                                 (when (some? v)
                                   [(keyword "datahike.pg" (name k)) v])))
                         hint))]
    (transact-fn conn [entity])))

(defn ref-attrs-from-schema
  "Return the seq of `:db.type/ref :db.cardinality/one` attribute idents
   in `schema`. Excludes internal namespaces."
  [schema]
  (keep (fn [[k v]]
          (when (and (keyword? k)
                     (namespace k)
                     (not (contains? internal-ns-prefixes (namespace k)))
                     (= :db.type/ref (:db/valueType v))
                     (= :db.cardinality/one (:db/cardinality v)))
            k))
        schema))

;; Per-(schema, hints) warn dedup: the warning set is keyed on the
;; schema map's identity so two different schemas (e.g. across tests)
;; warn independently, and the same schema doesn't re-warn every
;; translate. Schema is the natural scope because the warnings here
;; are about schema-level facts (ref-target convention misses).
;;
;; WeakHashMap means GC reclaims warning state when a schema goes
;; away (test conn closed, server shut down) — a long-running server
;; that re-creates schemas during its lifetime gets fresh state per
;; schema instance without manual cleanup.
(def ^:private schema-warn-sets
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

(defn- schema-warn-set
  "Get-or-create the warned-key set for a given schema map."
  [schema]
  (or (.get ^java.util.Map schema-warn-sets schema)
      (let [a (atom #{})]
        (.put ^java.util.Map schema-warn-sets schema a)
        a)))

(defn- warn-once!
  "Print a one-shot warning to *err*, keyed so repeated calls for the
   same `(schema, k)` are no-ops. The dedup is scoped to the schema
   value — distinct schemas (e.g. across test runs that build fresh
   conns) warn independently, and a long-running server that
   re-creates a schema after a fix gets a fresh warning emit. Plain
   println; pgwire-datahike doesn't depend on a logging library yet."
  [schema k msg]
  (let [warned (schema-warn-set schema)]
    (when-not (contains? @warned k)
      (swap! warned conj k)
      (binding [*out* *err*]
        (println (str "[pgwire-datahike] WARN: " msg))))))

(def ^:private ref-targets-cache
  "Memoization for `derive-ref-targets`. Outer key is the schema map
   itself (WeakHashMap — releases when the conn's schema is GC'd);
   inner value is a HashMap of `hints → result` so a single schema
   shared by multiple conn-with-different-hints scenarios doesn't
   thrash. Schemas are value-typed and typically stable across queries,
   so the cache hit rate is near-100%."
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

;; Per-query memoisation for `schema-hints` and `derive-virtual-tables`.
;;
;; In a single parse-sql call, `catalog-data-for*` (catalog.clj) is
;; invoked once per catalog table referenced — pg_class, pg_attribute,
;; pg_index, … — and each invocation calls
;; `(derive-virtual-tables user-schema (schema-hints cte-db))`. Both
;; inputs are stable across the whole query, so we walk the user
;; schema and re-run the hint Datalog query N times where 1 would
;; suffice. On a 600-table Odoo schema with 5 catalog tables in one
;; query, that's ~5 schema walks instead of 1.
;;
;; Why IdentityHashMap rather than WeakHashMap:
;;   - WeakHashMap uses `.equals` for hash-collision resolution.
;;   - `datahike.db.DB.equals` (db.cljc:302) throws ClassCastException
;;     on two distinct DB records (it iterates an internal map but
;;     treats Datoms as Map.Entry). Any second `.get` for a different
;;     DB on the same hash bucket would crash, surfacing as
;;     "SQL parse error: class datahike.datom.Datom cannot be cast to
;;      class java.util.Map$Entry".
;;   - IdentityHashMap uses `==` (System.identityHashCode), never
;;     touches `.equals`, so the broken DB equality doesn't matter.
;;
;; Lifetime: bound to a fresh map per parse-sql call (sql.clj
;; catalog-enrichment block), so the map is GC'd with the query — no
;; leak even though IdentityHashMap doesn't have weak references.

(def ^:dynamic *catalog-tx-cache*
  "When non-nil, a map `{:hints ihm :tables ihm}` of two
   IdentityHashMaps used to memoise `schema-hints` (keyed by db) and
   `derive-virtual-tables` (keyed by schema, with inner keyed by
   hints). Bound at the top of parse-sql's catalog enrichment block
   so all `catalog-data-for*` calls in one query share the cache."
  nil)

(defn make-catalog-tx-cache
  "Construct a fresh per-query cache. Caller `binding`s
   *catalog-tx-cache* to the result around their unit of work."
  []
  {:hints  (java.util.IdentityHashMap.)
   :tables (java.util.IdentityHashMap.)})

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

   Both `:db.cardinality/one` AND `:db.cardinality/many` refs are
   included. The result map's value is a `target-pk-attr` for the
   one-cardinality case, OR a vector `[target-pk-attr :many]` for
   many-cardinality refs — the translator branches on the shape:
     - one  → emit a chained `get-else` that derefs to a single
              target-PK value (matching real PG INT FK columns)
     - many → emit a per-row Clojure fn (`fns/pg-many-ref-array`)
              that boxes all target PKs into a PgArray (matching
              `int8[]` columns)

   Pure schema-side derivation; for runtime data validation against the
   actual entities a ref points to (polymorphism detection,
   namespace-mismatch detection), see `validate-ref-targets!`.

   Memoized — schema is value-typed and typically stable per conn, so
   recomputing per query is wasted work."
  [schema hints]
  (let [hint-key (or hints {})
        ;; Two-level cache: outer is WeakHashMap by schema (releases
        ;; with conn); inner is HashMap by hints. Prevents the
        ;; cache-key vector from being GC'd-before-use that would
        ;; happen with a single-level WeakHashMap[[schema hints]].
        inner (or (.get ^java.util.Map ref-targets-cache schema)
                  (let [m (java.util.Collections/synchronizedMap
                           (java.util.HashMap.))]
                    (.put ^java.util.Map ref-targets-cache schema m)
                    m))]
    (or (.get ^java.util.Map inner hint-key)
        (let [ns->unique-id (reduce-kv
                             (fn [m k v]
                               (if (and (keyword? k)
                                        (= :db.unique/identity (:db/unique v))
                                        (namespace k))
                                 (assoc m (namespace k) k)
                                 m))
                             {}
                             schema)
              result (reduce-kv
                      (fn [m attr-ident props]
                        (if (and (keyword? attr-ident)
                                 (= :db.type/ref (:db/valueType props)))
                          (let [hint (get hints attr-ident)
                                target (or (:references hint)
                                           (get ns->unique-id (name attr-ident)))
                                cardinality (:db/cardinality props)]
                            (cond
                              (and target (= :db.cardinality/one cardinality))
                              (assoc m attr-ident target)
                              (and target (= :db.cardinality/many cardinality))
                              (assoc m attr-ident [target :many])
                              :else m))
                          m))
                      {}
                      schema)]
          (.put ^java.util.Map inner hint-key result)
          result))))

(defn- bulk-ref-target-namespaces
  "Single Datalog query that returns `{ref-attr → #{target-namespace}}`
   for every ref attr in `ref-attrs` at once. Avoids N+1 round-trips
   when `validate-ref-targets!` cross-checks the schema against live
   data — which it does on every translate-select.

   Empty namespace sets for refs with no current entities pass through
   unchanged so the caller can trust the convention until data
   appears."
  [db ref-attrs]
  (when (and db (seq ref-attrs))
    (let [q-fn d/q
          ;; One query: walk every datom whose attr is in the
          ;; ref-attrs set, follow the ref, then take any attr on the
          ;; target as a namespace witness. Filtering internal attrs
          ;; here keeps the post-processing simple.
          rows (q-fn '{:find  [?ref ?attr]
                       :in    [$ [?ref ...]]
                       :where [[_ ?ref ?target]
                               [?target ?attr _]]}
                     db (vec ref-attrs))]
      (reduce (fn [acc [ref-attr attr]]
                (if (and (keyword? attr)
                         (some? (namespace attr))
                         (not (contains? internal-ns-prefixes (namespace attr))))
                  (update acc ref-attr (fnil conj #{}) (namespace attr))
                  acc))
              ;; Initialise every ref to empty set so the caller can
              ;; tell "no data yet" from "missing entry".
              (zipmap ref-attrs (repeat #{}))
              rows))))

(defn validate-ref-targets!
  "Cross-check `ref-targets` (from `derive-ref-targets`) against the
   actual data in `db`. Drops:
   - polymorphic refs — entities span multiple namespaces; convention
     can't pick a single target safely. User must add an explicit
     `:datahike.pg/references` hint per polymorphic ref.
   - namespace-mismatched refs — convention picked target X but the
     data points to namespace Y. Often means the user intended `:X` but
     named the ref attr after a different concept (`:order/buyer` ref
     to `customer`). Hint should override.

   Each drop emits a one-shot stderr warning. Returns the validated
   subset of `ref-targets`. Refs with no current data (empty set)
   pass through unchanged — the convention is the best guess until
   data appears.

   Also warns about ref attrs that have NO ref-targets entry at all
   (no hint, no namespace match) so the user knows to set a hint
   if they want SQL-FK projection on that column."
  [db schema ref-targets]
  (when (and db schema)
    (doseq [ref-attr (ref-attrs-from-schema schema)
            :when (not (contains? ref-targets ref-attr))]
      (warn-once! schema [::no-target ref-attr]
                  (str "ref attr " ref-attr " has no SQL FK target — "
                       "set :datahike.pg/references hint to enable "
                       "FK-style projection (otherwise it projects as "
                       "the raw entity-id)."))))
  (if-not (and db (seq ref-targets))
    ref-targets
    (let [;; One bulk query for every ref attr in ref-targets. Replaces
          ;; the old per-attr Datalog round-trip — for a schema with
          ;; N ref attrs, drops translate-time cost from O(N) round-
          ;; trips to 1.
          ns-by-ref (bulk-ref-target-namespaces db (keys ref-targets))]
      (reduce-kv
       (fn [acc ref-attr target-entry]
         (let [target-pk (if (vector? target-entry) (first target-entry) target-entry)
               actual-ns (get ns-by-ref ref-attr #{})
               expected-ns (namespace target-pk)]
           (cond
             ;; No data yet — trust the convention/hint.
             (zero? (count actual-ns)) (assoc acc ref-attr target-entry)

             (= 1 (count actual-ns))
             (if (= (first actual-ns) expected-ns)
               (assoc acc ref-attr target-entry)
               (do (warn-once! schema [::namespace-mismatch ref-attr]
                               (str "ref attr " ref-attr " resolves to target "
                                    target-pk " (namespace " expected-ns ")"
                                    " but data points to namespace "
                                    (first actual-ns)
                                    ". SQL projection will fall back to the "
                                    "raw entity-id; set :datahike.pg/references "
                                    "to override."))
                   acc))

             :else
             (do (warn-once! schema [::polymorphic-ref ref-attr]
                             (str "ref attr " ref-attr " is polymorphic — "
                                  "entities span namespaces " (sort actual-ns)
                                  ". SQL projection falls back to the raw "
                                  "entity-id; set :datahike.pg/references to "
                                  "force a single target."))
                 acc))))
       {}
       ref-targets))))

(defn- schema-hints*
  [db]
  (let [q-fn d/q
        ;; The :datahike.pg/* attrs may not yet be in the schema (bare
        ;; conn without ensure-pg-schema!) — keep the query resilient
        ;; to each attr being absent by using per-attr lookups instead
        ;; of a single join.
        rows (q-fn '{:find [?for-ident ?e]
                     :where [[?e :datahike.pg/for-ident ?for-ident]]}
                   db)
        pull-fn d/pull]
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
          rows)))

(defn schema-hints
  "Return `{attr-ident → {:column str? :hidden bool? :references kw? :table str?}}`
   by scanning the db for :datahike.pg/for-ident-rooted hint entities.
   Nil-safe: returns an empty map when `db` is nil (pure-schema call sites).
   When `*catalog-tx-cache*` is bound (within parse-sql's catalog
   enrichment), reuses the result across all `catalog-data-for*` calls
   in the current query."
  [db]
  (cond
    (nil? db) {}
    (some? *catalog-tx-cache*)
    (let [^java.util.IdentityHashMap m (:hints *catalog-tx-cache*)]
      (or (.get m db)
          (let [v (schema-hints* db)]
            (.put m db v)
            v)))
    :else (schema-hints* db)))

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
  (let [q-fn d/q]
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
  (let [q-fn d/q
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

(defn- derive-virtual-tables*
  [schema hints]
  (let [user-attrs (remove (fn [[k _]] (or (not (keyword? k))
                                           (internal-attr? k)
                                           (nil? (namespace k))
                                           (:hidden (get hints k))))
                           schema)
        tables-from-attrs (reduce
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
                           user-attrs)
         ;; Also surface tables that exist in the schema but have NO own
         ;; user columns — typically INHERITS children whose columns all
         ;; live in the parent's namespace. Without this, pg_class doesn't
         ;; list them, Odoo's `table_exists()` returns false, and Odoo
         ;; issues a redundant CREATE TABLE that we then reject as 42P07.
         ;; Detect via the row-marker attr (`<table>/db-row-exists`) which
         ;; every pgwire CREATE TABLE installs.
        marker-tables (into #{}
                            (keep (fn [[k _]]
                                    (when (and (keyword? k)
                                               (= (name k) row-marker-col)
                                               (namespace k))
                                      (namespace k))))
                            schema)]
    (reduce (fn [tables tname]
              (cond-> tables
                (not (contains? tables tname))
                (assoc tname {:columns [] :attrs {}})))
            tables-from-attrs
            marker-tables)))

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
     for the translator's FK-via-ref JOIN rewrite.

   Each catalog table referenced in a query calls this with the same
   schema/hints. When `*catalog-tx-cache*` is bound, the result is
   memoised across all calls in the current parse-sql so the schema
   walk happens once per query (not once per catalog table)."
  ([schema] (derive-virtual-tables schema {}))
  ([schema hints]
   (if-let [c *catalog-tx-cache*]
     (let [^java.util.IdentityHashMap outer (:tables c)
           inner (or (.get outer schema)
                     (let [m (java.util.IdentityHashMap.)]
                       (.put outer schema m)
                       m))]
       (or (.get ^java.util.IdentityHashMap inner hints)
           (let [v (derive-virtual-tables* schema hints)]
             (.put ^java.util.IdentityHashMap inner hints v)
             v)))
     (derive-virtual-tables* schema hints))))

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
    (let [q-fn d/q
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
