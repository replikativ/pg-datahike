(ns datahike.pg.catalog.objects
  "Persistent PostgreSQL catalog identities for user-defined objects.

   PostgreSQL identifies an object by (catalog relation OID, object OID),
   with an optional sub-object number for columns.  Keeping that shape here
   lets routines, triggers, dependencies, and future catalog work share one
   transactional identity model instead of inventing per-feature IDs."
  (:require [datahike.api :as d]
            [datahike.db]
            [datahike.db.interface :as dbi]))

(set! *warn-on-reflection* true)

(def ^:const first-user-oid 16384)
(def ^:const max-oid 4294967295)

(defn valid-user-oid?
  "True for an allocatable PostgreSQL user OID. OID is uint32, while values
   below FirstNormalObjectId belong to PostgreSQL's bootstrap catalogs."
  [oid]
  (and (integer? oid)
       (<= first-user-oid (long oid) max-oid)))

(defn- validate-allocation-cursor [cursor]
  (let [cursor (long cursor)]
    (when-not (<= first-user-oid cursor (inc max-oid))
      (throw (ex-info "invalid PostgreSQL OID allocation cursor"
                      {:error :invalid-catalog-state :cursor cursor})))
    cursor))

(defn- ensure-allocatable-oid [oid]
  (when (> (long oid) max-oid)
    (throw (ex-info "PostgreSQL OID space is exhausted"
                    {:error :program-limit-exceeded :sqlstate "54000"})))
  (long oid))

(def ^:const pg-class-oid 1259)
(def ^:const pg-type-oid 1247)
(def ^:const pg-proc-oid 1255)
(def ^:const pg-namespace-oid 2615)
(def ^:const pg-trigger-oid 2620)

(def catalog-relation-oids
  "pg_class OIDs of the system catalogs we materialise, from each header's
   `CATALOG(name, oid, ...)` declaration (src/include/catalog/*.h). What a
   catalog row's `tableoid` answers; pg_dump keys every object it reads by
   (tableoid, oid). Views have no tableoid and are deliberately absent."
  {"pg_attrdef" 2604 "pg_attribute" 1249 "pg_class" 1259 "pg_collation" 3456
   "pg_constraint" 2606 "pg_database" 1262 "pg_depend" 2608
   "pg_description" 2609 "pg_enum" 3501 "pg_extension" 3079 "pg_index" 2610
   "pg_inherits" 2611 "pg_namespace" 2615 "pg_policy" 3256 "pg_proc" 1255
   "pg_publication" 6104 "pg_publication_namespace" 6237
   "pg_publication_rel" 6106 "pg_rewrite" 2618 "pg_statistic_ext" 3381
   "pg_trigger" 2620 "pg_type" 1247})

(def ^:const pg-catalog-namespace-oid 11)
(def ^:const public-namespace-oid 2200)

(def ^:const catalog-version 3)
(def catalog-key :user-catalog)

(def schema
  "Datahike schema for the shared user-object catalog.  Address and identity
   keys use strict unique/value semantics: an attempted duplicate must fail,
   never upsert or merge two independently-created objects."
  [{:db/ident :datahike.pg.catalog/key
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :datahike.pg.catalog/version
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.catalog/next-oid
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.object/address-key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/value}
   {:db/ident :datahike.pg.object/identity-key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/value}
   {:db/ident :datahike.pg.object/class-oid
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :datahike.pg.object/oid
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :datahike.pg.object/kind
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.object/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :datahike.pg.object/namespace
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :datahike.pg.object/owner-oid
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.object/revision
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.object/legacy-oid?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.column/address-key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/value}
   {:db/ident :datahike.pg.column/name-key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/value}
   {:db/ident :datahike.pg.column/relation
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :datahike.pg.column/attnum
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :datahike.pg.column/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.column/storage-ident
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :datahike.pg.column/type-oid
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.column/typmod
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.column/dropped?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.column/local?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.column/inherit-count
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :datahike.pg.column/inherited-from-address
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(defn address
  "A PostgreSQL object address.  `sub-id` is zero for the object itself and
   positive for a sub-object such as a table column."
  ([class-oid object-oid] (address class-oid object-oid 0))
  ([class-oid object-oid sub-id]
   [(long class-oid) (long object-oid) (long sub-id)]))

(defn address-key
  "Stable structural serialization of a top-level catalog address."
  [class-oid object-oid]
  (pr-str [(long class-oid) (long object-oid)]))

(defn identity-key
  "Stable structural serialization of a namespace-scoped object identity.
   `tail` carries kind-specific identity, notably a routine's argument types."
  ([class-oid namespace-oid name]
   (identity-key class-oid namespace-oid name nil))
  ([class-oid namespace-oid name tail]
   (pr-str [(long class-oid)
            (when (some? namespace-oid) (long namespace-oid))
            (str name)
            tail])))

(defn column-address-key [relation-oid attnum]
  (pr-str [pg-class-oid (long relation-oid) (long attnum)]))

(defn column-name-key [relation-oid name]
  (pr-str [(long relation-oid) (str name)]))

(defn- object-map [entity]
  (when entity
    (select-keys entity
                 [:db/id
                  :datahike.pg.object/address-key
                  :datahike.pg.object/identity-key
                  :datahike.pg.object/class-oid
                  :datahike.pg.object/oid
                  :datahike.pg.object/kind
                  :datahike.pg.object/name
                  :datahike.pg.object/namespace
                  :datahike.pg.object/owner-oid
                  :datahike.pg.object/revision
                  :datahike.pg.object/legacy-oid?])))

(defn- entity-by-unique [db attr value]
  ;; FilteredDB (valid-at/as-of) supports Datalog and numeric entity lookup,
  ;; but deliberately has no lookup-ref thunk. Resolve the unique key through
  ;; Datalog first so temporal catalog enrichment remains usable.
  (let [db (loop [db db]
             (if (instance? datahike.db.FilteredDB db)
               (recur (.-unfiltered-db ^datahike.db.FilteredDB db))
               db))]
    (when-let [eid (ffirst
                    (d/q {:find '[?entity]
                          :in '[$ ?value]
                          :where [['?entity attr '?value]]}
                         db value))]
      (d/pull db '[*] eid))))

(defn- catalog-metadata-db [db]
  (loop [db db]
    (if (instance? datahike.db.FilteredDB db)
      (recur (.-unfiltered-db ^datahike.db.FilteredDB db))
      db)))

(defn object-by-address [db class-oid object-oid]
  (when (get (dbi/-schema db) :datahike.pg.object/address-key)
    (some-> (entity-by-unique db :datahike.pg.object/address-key
                              (address-key class-oid object-oid))
            object-map)))

(defn object-by-identity
  ([db class-oid namespace-oid name]
   (object-by-identity db class-oid namespace-oid name nil))
  ([db class-oid namespace-oid name tail]
   (when (get (dbi/-schema db) :datahike.pg.object/identity-key)
     (some-> (entity-by-unique db :datahike.pg.object/identity-key
                               (identity-key class-oid namespace-oid name tail))
             object-map))))

(defn namespace-by-name [db name]
  (object-by-identity db pg-namespace-oid nil name))

(defn namespace-by-oid [db oid]
  (object-by-address db pg-namespace-oid oid))

(defn objects-in-namespace [db namespace-oid]
  (let [db (catalog-metadata-db db)]
    (when-let [namespace-eid (:db/id (namespace-by-oid db namespace-oid))]
      (mapv (comp object-map #(d/pull db '[*] %))
            (map first
                 (d/q '{:find [?object]
                        :in [$ ?namespace]
                        :where [[?object :datahike.pg.object/namespace ?namespace]]}
                      db namespace-eid))))))

(defn objects-by-kind [db kind]
  (let [db (catalog-metadata-db db)]
    (when (get (dbi/-schema db) :datahike.pg.object/kind)
      (mapv (comp object-map #(d/pull db '[*] %))
            (map first
                 (d/q '{:find [?object]
                        :in [$ ?kind]
                        :where [[?object :datahike.pg.object/kind ?kind]]}
                      db kind))))))

(def ^:private column-keys
  [:db/id
   :datahike.pg.column/address-key
   :datahike.pg.column/name-key
   :datahike.pg.column/relation
   :datahike.pg.column/attnum
   :datahike.pg.column/name
   :datahike.pg.column/storage-ident
   :datahike.pg.column/type-oid
   :datahike.pg.column/typmod
   :datahike.pg.column/dropped?
   :datahike.pg.column/local?
   :datahike.pg.column/inherit-count
   :datahike.pg.column/inherited-from-address])

(defn- column-map [entity]
  (when entity (select-keys entity column-keys)))

(defn column-by-attnum [db relation-oid attnum]
  (when (get (dbi/-schema db) :datahike.pg.column/address-key)
    (some-> (entity-by-unique db :datahike.pg.column/address-key
                              (column-address-key relation-oid attnum))
            column-map)))

(defn column-by-name [db relation-oid name]
  (when (get (dbi/-schema db) :datahike.pg.column/name-key)
    (some-> (entity-by-unique db :datahike.pg.column/name-key
                              (column-name-key relation-oid name))
            column-map)))

(defn columns-by-relation
  "All persisted column subobjects for a relation in attnum order. Dropped
   tombstones are included unless `live-only?` is true."
  ([db relation-oid] (columns-by-relation db relation-oid false))
  ([db relation-oid live-only?]
   (let [db (catalog-metadata-db db)]
     (when-let [relation-eid (:db/id (object-by-address db pg-class-oid relation-oid))]
       (->> (d/q '{:find [?column]
                   :in [$ ?relation]
                   :where [[?column :datahike.pg.column/relation ?relation]]}
                 db relation-eid)
            (map (comp column-map #(d/pull db column-keys %) first))
            (remove #(and live-only? (:datahike.pg.column/dropped? %)))
            (sort-by :datahike.pg.column/attnum)
            vec)))))

(defn next-attnum [db relation-oid]
  (let [maximum (reduce max 0
                        (map :datahike.pg.column/attnum
                             (columns-by-relation db relation-oid)))]
    (when (>= maximum 1600)
      (throw (ex-info "tables can have at most 1600 columns"
                      {:error :too-many-columns :sqlstate "54011"})))
    (inc maximum)))

(defn create-columns-tx
  "Create durable column subobject rows. The relation may be created in the
   same transaction, so its lookup ref need not resolve in `db` yet."
  [relation-oid columns]
  (mapv
   (fn [{:keys [attnum name storage-ident type-oid typmod local? inherit-count
                inherited-from-address]
         :or {local? true inherit-count 0}}]
     (cond-> {:datahike.pg.column/address-key
              (column-address-key relation-oid attnum)
              :datahike.pg.column/name-key (column-name-key relation-oid name)
              :datahike.pg.column/relation
              [:datahike.pg.object/address-key
               (address-key pg-class-oid relation-oid)]
              :datahike.pg.column/attnum (long attnum)
              :datahike.pg.column/name (str name)
              :datahike.pg.column/storage-ident storage-ident
              :datahike.pg.column/dropped? false
              :datahike.pg.column/local? (boolean local?)
              :datahike.pg.column/inherit-count (long inherit-count)}
       inherited-from-address
       (assoc :datahike.pg.column/inherited-from-address
              inherited-from-address)
       (some? type-oid) (assoc :datahike.pg.column/type-oid (long type-oid))
       (some? typmod) (assoc :datahike.pg.column/typmod (long typmod))))
   columns))

(defn drop-relation-columns-tx [db relation-oid]
  (mapv (fn [column]
          [:db/retractEntity
           [:datahike.pg.column/address-key
            (:datahike.pg.column/address-key column)]])
        (columns-by-relation db relation-oid)))

(defn tombstone-column-tx
  "Mark a column address dropped while retaining its attnum. The live name
   key is released so a later column with the same SQL name receives a fresh
   address. Revision guarding belongs to the surrounding relation mutation."
  [db relation-oid attnum]
  (when-let [column (column-by-attnum db relation-oid attnum)]
    (let [column-ref [:datahike.pg.column/address-key
                      (:datahike.pg.column/address-key column)]]
      (when-not (:datahike.pg.column/dropped? column)
        [[:db/retract column-ref :datahike.pg.column/name-key
          (:datahike.pg.column/name-key column)]
         [:db/add column-ref :datahike.pg.column/dropped? true]]))))

(defn rename-column-tx
  "Rename a live column without changing its relation-local address or
   physical Datahike storage ident."
  [db relation-oid attnum new-name new-storage-ident]
  (when-let [column (column-by-attnum db relation-oid attnum)]
    (when-not (:datahike.pg.column/dropped? column)
      (let [column-ref [:datahike.pg.column/address-key
                        (:datahike.pg.column/address-key column)]]
        [[:db/retract column-ref :datahike.pg.column/name-key
          (:datahike.pg.column/name-key column)]
         [:db/add column-ref :datahike.pg.column/name-key
          (column-name-key relation-oid new-name)]
         [:db/add column-ref :datahike.pg.column/name (str new-name)]
         [:db/add column-ref :datahike.pg.column/storage-ident
          new-storage-ident]]))))

(defn resolve-search-path
  "Resolve PostgreSQL search_path entries to existing namespace OIDs.
   pg_catalog is implicitly searched first unless explicitly positioned.
   `$user` contributes a schema only when it exists."
  [db entries user-name]
  (let [expanded (map #(if (= "$user" %) user-name %) entries)
        explicit (keep #(some-> (namespace-by-name db %) :datahike.pg.object/oid)
                       expanded)
        explicit (vec (distinct explicit))]
    (if (some #{pg-catalog-namespace-oid} explicit)
      explicit
      (into [pg-catalog-namespace-oid] explicit))))

(defn creation-namespace
  "The first existing explicit search_path entry.  The implicit pg_catalog
   entry used for lookup is deliberately not a creation target."
  [db entries user-name]
  (some #(let [name (if (= "$user" %) user-name %)]
           (namespace-by-name db name))
        entries))

(defn catalog-entity [db]
  (when (get (dbi/-schema db) :datahike.pg.catalog/key)
    (entity-by-unique db :datahike.pg.catalog/key catalog-key)))

(defn- oid-in-use? [db oid]
  (boolean
   (ffirst
    (d/q '{:find [?object]
           :in [$ ?oid]
           :where [[?object :datahike.pg.object/oid ?oid]]}
         db oid))))

(defn reserve-user-oid-tx
  "Reserve one currently-unused user OID with a CAS operation that the caller
   must transact together with the object creation.  Allocation is therefore
   rolled back with its DDL and naturally chains against a speculative DB."
  [db]
  (let [catalog (catalog-entity db)
        catalog-eid (:db/id catalog)]
    (when-not catalog-eid
      (throw (ex-info "user-object catalog is not initialized"
                      {:error :catalog-not-initialized})))
    (loop [candidate (validate-allocation-cursor
                      (or (:datahike.pg.catalog/next-oid catalog)
                          first-user-oid))]
      (ensure-allocatable-oid candidate)
      (if (oid-in-use? db candidate)
        (recur (inc candidate))
        {:oid candidate
         :tx-data [[:db/cas catalog-eid :datahike.pg.catalog/next-oid
                    (:datahike.pg.catalog/next-oid catalog)
                    (inc candidate)]]}))))

(defn reserve-user-oids-tx
  "Reserve `n` noncolliding OIDs with one allocator CAS. Used when PostgreSQL
   creates coupled catalog objects, such as a relation and its row type."
  [db n]
  (when-not (pos-int? n)
    (throw (ex-info "OID reservation count must be positive"
                    {:error :invalid-oid-reservation :count n})))
  (let [catalog (catalog-entity db)
        catalog-eid (:db/id catalog)
        start (validate-allocation-cursor
               (or (:datahike.pg.catalog/next-oid catalog) first-user-oid))]
    (when-not catalog-eid
      (throw (ex-info "user-object catalog is not initialized"
                      {:error :catalog-not-initialized})))
    (loop [candidate (long (or start first-user-oid))
           oids []]
      (if (= n (count oids))
        {:oids oids
         :tx-data [[:db/cas catalog-eid :datahike.pg.catalog/next-oid
                    start candidate]]}
        (do
          (ensure-allocatable-oid candidate)
          (if (oid-in-use? db candidate)
            (recur (inc candidate) oids)
            (recur (inc candidate) (conj oids candidate))))))))

(defn create-object-tx
  "Build an object-row transaction.  Namespace may be nil for cluster-global
   objects; otherwise it must already exist.  The two :db.unique/value keys
   are the atomic stale-reader guard: Datahike rejects duplicates and cannot
   upsert/merge them as it would :db.unique/identity."
  [db {:keys [class-oid oid kind name namespace-oid owner-oid revision
              legacy-oid? identity-tail]
       :or {owner-oid 10 revision 1}}]
  (let [namespace (when (some? namespace-oid)
                    (namespace-by-oid db namespace-oid))]
    (when (and (some? namespace-oid) (nil? namespace))
      (throw (ex-info (str "namespace OID " namespace-oid " does not exist")
                      {:error :undefined-schema :sqlstate "3F000"})))
    [(cond-> {:datahike.pg.object/address-key (address-key class-oid oid)
              :datahike.pg.object/identity-key
              (identity-key class-oid namespace-oid name identity-tail)
              :datahike.pg.object/class-oid (long class-oid)
              :datahike.pg.object/oid (long oid)
              :datahike.pg.object/kind kind
              :datahike.pg.object/name (str name)
              :datahike.pg.object/owner-oid (long owner-oid)
              :datahike.pg.object/revision (long revision)}
       namespace (assoc :datahike.pg.object/namespace
                        [:datahike.pg.object/address-key
                         (address-key pg-namespace-oid namespace-oid)])
       (some? legacy-oid?) (assoc :datahike.pg.object/legacy-oid?
                                  (boolean legacy-oid?)))]))

(defn drop-object-tx [db class-oid object-oid]
  (when-let [object (object-by-address db class-oid object-oid)]
    (let [object-ref [:datahike.pg.object/address-key
                      (address-key class-oid object-oid)]
          revision (long (or (:datahike.pg.object/revision object) 0))]
      [[:db/cas object-ref :datahike.pg.object/revision revision (inc revision)]
       [:db/retractEntity object-ref]])))

(defn bump-revision-tx [db class-oid object-oid]
  (when-let [object (object-by-address db class-oid object-oid)]
    (let [object-ref [:datahike.pg.object/address-key
                      (address-key class-oid object-oid)]
          revision (long (or (:datahike.pg.object/revision object) 0))]
      [[:db/cas object-ref :datahike.pg.object/revision
        revision (inc revision)]])))

(defn rename-object-tx
  [db class-oid object-oid new-name identity-tail]
  (when-let [object (object-by-address db class-oid object-oid)]
    (let [object-ref [:datahike.pg.object/address-key
                      (address-key class-oid object-oid)]
          namespace-oid (some-> (:datahike.pg.object/namespace object)
                                :datahike.pg.object/oid)
          old-key (:datahike.pg.object/identity-key object)
          new-key (identity-key class-oid namespace-oid new-name identity-tail)
          revision (long (or (:datahike.pg.object/revision object) 0))]
      [[:db/cas object-ref :datahike.pg.object/revision revision (inc revision)]
       [:db/retract object-ref :datahike.pg.object/identity-key old-key]
       [:db/add object-ref :datahike.pg.object/identity-key new-key]
       [:db/add object-ref :datahike.pg.object/name (str new-name)]])))

(defn- migration-object-map
  [{:keys [tempid class-oid oid kind name namespace-tempid owner-oid
           legacy-oid? identity-tail]}]
  (cond-> {:db/id tempid
           :datahike.pg.object/address-key (address-key class-oid oid)
           :datahike.pg.object/identity-key
           (identity-key class-oid
                         (case namespace-tempid
                           "catalog-namespace-pg-catalog" pg-catalog-namespace-oid
                           "catalog-namespace-public" public-namespace-oid
                           nil)
                         name identity-tail)
           :datahike.pg.object/class-oid (long class-oid)
           :datahike.pg.object/oid (long oid)
           :datahike.pg.object/kind kind
           :datahike.pg.object/name (str name)
           :datahike.pg.object/owner-oid (long (or owner-oid 10))
           :datahike.pg.object/revision 1}
    namespace-tempid (assoc :datahike.pg.object/namespace namespace-tempid)
    (some? legacy-oid?) (assoc :datahike.pg.object/legacy-oid?
                               (boolean legacy-oid?))))

(defn initialize-catalog
  "Transaction function for an atomic, idempotent initial migration.
   `legacy-objects` must already have deterministic, collision-free OIDs."
  [txdb legacy-objects legacy-columns next-oid]
  (let [existing (catalog-entity txdb)
        version (:datahike.pg.catalog/version existing)]
    (cond
      (= catalog-version version) []
      (and version (> (long version) catalog-version))
      (throw (ex-info "object catalog was written by a newer pg-datahike"
                      {:error :catalog-version-too-new
                       :supported catalog-version :found version}))
      existing
      (throw (ex-info "unsupported partial or older object catalog"
                      {:error :catalog-migration-required
                       :supported catalog-version :found version}))
      :else
      (let [namespaces [{:tempid "catalog-namespace-pg-catalog"
                         :class-oid pg-namespace-oid
                         :oid pg-catalog-namespace-oid
                         :kind :namespace :name "pg_catalog"}
                        {:tempid "catalog-namespace-public"
                         :class-oid pg-namespace-oid
                         :oid public-namespace-oid
                         :kind :namespace :name "public"}]]
        (into [{:datahike.pg.catalog/key catalog-key
                :datahike.pg.catalog/version catalog-version
                :datahike.pg.catalog/next-oid (long (max first-user-oid next-oid))}]
              (concat (map migration-object-map namespaces)
                      (map migration-object-map legacy-objects)
                      (mapcat (fn [{:keys [relation-oid columns]}]
                                (create-columns-tx relation-oid columns))
                              legacy-columns)
                      (mapcat :legacy-tx-data legacy-objects)))))))

(defn initialization-tx
  ([legacy-objects next-oid]
   (initialization-tx legacy-objects [] next-oid))
  ([legacy-objects legacy-columns next-oid]
   [[:db.fn/call initialize-catalog
     (vec legacy-objects) (vec legacy-columns) (long next-oid)]]))

(defn migrate-v1-to-v2
  "Atomic catalog-v1 upgrade that installs durable column subaddresses."
  [txdb legacy-columns]
  (let [catalog (catalog-entity txdb)
        version (:datahike.pg.catalog/version catalog)]
    (cond
      (and version (>= (long version) 2)) []
      (= 1 version)
      (into [[:db/cas (:db/id catalog) :datahike.pg.catalog/version
              1 2]]
            (mapcat (fn [{:keys [relation-oid columns]}]
                      (create-columns-tx relation-oid columns))
                    legacy-columns))
      (and version (> (long version) catalog-version))
      (throw (ex-info "object catalog was written by a newer pg-datahike"
                      {:error :catalog-version-too-new
                       :supported catalog-version :found version}))
      :else
      (throw (ex-info "unsupported partial or older object catalog"
                      {:error :catalog-migration-required
                       :supported catalog-version :found version})))))

(defn v1-to-v2-tx [legacy-columns]
  [[:db.fn/call migrate-v1-to-v2 (vec legacy-columns)]])

(defn migrate-v2-to-v3
  "Atomic address-completeness migration. `tx-data` contains preplanned
   generic object and column rows; allocator/version CAS operations make a
   stale or concurrent plan fail instead of partially merging."
  [txdb tx-data expected-next next-oid]
  (let [catalog (catalog-entity txdb)
        version (:datahike.pg.catalog/version catalog)]
    (cond
      (= catalog-version version) []
      (= 2 version)
      (into [[:db/cas (:db/id catalog) :datahike.pg.catalog/version
              2 catalog-version]
             [:db/cas (:db/id catalog) :datahike.pg.catalog/next-oid
              expected-next next-oid]]
            tx-data)
      (and version (> (long version) catalog-version))
      (throw (ex-info "object catalog was written by a newer pg-datahike"
                      {:error :catalog-version-too-new
                       :supported catalog-version :found version}))
      :else
      (throw (ex-info "catalog v2-to-v3 migration requires version 2"
                      {:error :catalog-migration-required
                       :supported catalog-version :found version})))))

(defn v2-to-v3-tx [tx-data expected-next next-oid]
  [[:db.fn/call migrate-v2-to-v3 (vec tx-data)
    (long expected-next) (long next-oid)]])
