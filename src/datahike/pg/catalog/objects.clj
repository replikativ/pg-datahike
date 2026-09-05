(ns datahike.pg.catalog.objects
  "Persistent PostgreSQL catalog identities for user-defined objects.

   PostgreSQL identifies an object by (catalog relation OID, object OID),
   with an optional sub-object number for columns.  Keeping that shape here
   lets routines, triggers, dependencies, and future catalog work share one
   transactional identity model instead of inventing per-feature IDs."
  (:require [datahike.api :as d]
            [datahike.db.interface :as dbi]))

(set! *warn-on-reflection* true)

(def ^:const first-user-oid 16384)

(def ^:const pg-class-oid 1259)
(def ^:const pg-type-oid 1247)
(def ^:const pg-proc-oid 1255)
(def ^:const pg-namespace-oid 2615)
(def ^:const pg-trigger-oid 2620)

(def ^:const pg-catalog-namespace-oid 11)
(def ^:const public-namespace-oid 2200)

(def ^:const catalog-version 1)
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
  (when-let [eid (ffirst
                  (d/q {:find '[?entity]
                        :in '[$ ?value]
                        :where [['?entity attr '?value]]}
                       db value))]
    (d/entity db eid)))

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
  (when-let [namespace-eid (:db/id (namespace-by-oid db namespace-oid))]
    (mapv (comp object-map #(d/entity db %))
          (map first
               (d/q '{:find [?object]
                      :in [$ ?namespace]
                      :where [[?object :datahike.pg.object/namespace ?namespace]]}
                    db namespace-eid)))))

(defn objects-by-kind [db kind]
  (when (get (dbi/-schema db) :datahike.pg.object/kind)
    (mapv (comp object-map #(d/entity db %))
          (map first
               (d/q '{:find [?object]
                      :in [$ ?kind]
                      :where [[?object :datahike.pg.object/kind ?kind]]}
                    db kind)))))

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
    (loop [candidate (long (or (:datahike.pg.catalog/next-oid catalog)
                               first-user-oid))]
      (if (oid-in-use? db candidate)
        (recur (inc candidate))
        {:oid candidate
         :tx-data [[:db/cas catalog-eid :datahike.pg.catalog/next-oid
                    (:datahike.pg.catalog/next-oid catalog)
                    (inc candidate)]]}))))

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
  "Transaction function for an atomic, idempotent version-1 migration.
   `legacy-objects` must already have deterministic, collision-free OIDs."
  [txdb legacy-objects next-oid]
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
                      (mapcat :legacy-tx-data legacy-objects)))))))

(defn initialization-tx [legacy-objects next-oid]
  [[:db.fn/call initialize-catalog (vec legacy-objects) (long next-oid)]])
