(ns datahike.pg.constraints.unique
  "Removable PostgreSQL UNIQUE INDEX enforcement over durable descriptors."
  (:require [clojure.edn :as edn]
            [datahike.api :as d]
            [datahike.array :as array]
            [datahike.datom]
            [datahike.db.interface :as dbi]
            [datahike.pg.cache :as cache]
            [datahike.pg.catalog.basis :as basis]
            [datahike.pg.catalog.objects :as objects])
  (:import [datahike.java IEntity]))

(def predicate-id ::indexes)

(def ^:private scalar-nan-key ::scalar-nan)

(defn canonical-key-value
  "Return a hashable value whose equality matches PostgreSQL UNIQUE-key
   equality for the value carriers used by pg-datahike.

   Datahike's array wrapper supplies content equality for primitive/object
   arrays. Scalar float NaNs need an additional sentinel: PostgreSQL's float4
   and float8 btree operators consider all NaNs equal, while Clojure map/set
   equality considers a JVM NaN unequal even to itself."
  [value]
  (cond
    (and (or (instance? Double value) (instance? Float value))
         (Double/isNaN (double value)))
    scalar-nan-key

    ;; PostgreSQL considers both signed zeros equal. Clojure's numeric
    ;; equality currently does too, but normalize them here so callers do not
    ;; depend on the boxed Float/Double representation.
    (and (or (instance? Double value) (instance? Float value))
         (zero? value))
    0.0

    :else
    (array/wrap-comparable value)))

(defn- index-probe-safe? [value]
  (= (canonical-key-value value) (array/wrap-comparable value)))

(def ^:private descriptor-attrs
  #{:datahike.pg.index/table
    :datahike.pg.index/relation
    :datahike.pg.index/method
    :datahike.pg.index/keys
    :datahike.pg.index/unique?
    :datahike.pg.index/primary?
    :datahike.pg.index/constraint?
    :datahike.pg.index/predicate
    :datahike.pg.index/legacy-incomplete?
    :datahike.pg.object/address-key
    :datahike.pg.object/identity-key
    :datahike.pg.object/class-oid
    :datahike.pg.object/name
    :datahike.pg.object/namespace
    :datahike.pg.object/oid
    :datahike.pg.object/kind
    :datahike.pg.column/address-key
    :datahike.pg.column/name-key
    :datahike.pg.column/relation
    :datahike.pg.column/attnum
    :datahike.pg.column/name
    :datahike.pg.column/storage-ident
    :datahike.pg.column/dropped?
    :__inherit__/parent
    :__inherit__/child})

(def ^:private index-attrs
  [:datahike.pg.index/table
   :datahike.pg.index/relation
   :datahike.pg.index/method
   :datahike.pg.index/keys
   :datahike.pg.index/unique?
   :datahike.pg.index/primary?
   :datahike.pg.index/constraint?
   :datahike.pg.index/legacy-incomplete?])

(defn- invalid-descriptor! [name message]
  (throw (ex-info (str "invalid unique index descriptor " name ": " message)
                  {:error :invalid-catalog-state
                   :sqlstate "XX000"
                   :index name})))

(defn- descendant-tables [db table]
  (if-not (get (dbi/-schema db) :__inherit__/child)
    #{}
    (loop [pending [table] found #{}]
      (if-let [parent (peek pending)]
        (let [children
              (into #{}
                    (map first)
                    (d/q '{:find [?child]
                           :in [$ ?parent]
                           :where [[?inheritance :__inherit__/parent ?parent]
                                   [?inheritance :__inherit__/child ?child]]}
                         db parent))
              fresh (remove found children)]
          (recur (into (pop pending) fresh) (into found fresh)))
        (disj found table)))))

(defn- read-index-descriptors [db]
  (if-not (get (dbi/-schema db) :datahike.pg.object/kind)
    []
    (let [object-eids (into #{} (map :db/id) (objects/objects-by-kind db :index))
          candidate-eids
          (reduce
           (fn [eids attr]
             (if (get (dbi/-schema db) attr)
               (into eids
                     (map first)
                     (d/q {:find '[?entity]
                           :in '[$]
                           :where [['?entity attr '_]]}
                          db))
               eids))
           object-eids index-attrs)]
      (->> candidate-eids
           (mapv
            (fn [entity]
              (let [object (d/pull db
                                   '[* {:datahike.pg.index/relation [*]}]
                                   entity)
                    oid (:datahike.pg.object/oid object)
                    name (:datahike.pg.object/name object)
                    table (:datahike.pg.index/table object)
                    relation (:datahike.pg.index/relation object)
                    keys-edn (:datahike.pg.index/keys object)
                    unique? (:datahike.pg.index/unique? object)
                    primary? (:datahike.pg.index/primary? object)
                    constraint? (:datahike.pg.index/constraint? object)
                    legacy-incomplete? (:datahike.pg.index/legacy-incomplete? object)
                    method (:datahike.pg.index/method object)
                    namespace-ref (:datahike.pg.object/namespace object)
                    namespace-eid (if (map? namespace-ref)
                                    (:db/id namespace-ref)
                                    namespace-ref)
                    relation-namespace-ref
                    (:datahike.pg.object/namespace relation)
                    relation-namespace-eid
                    (if (map? relation-namespace-ref)
                      (:db/id relation-namespace-ref)
                      relation-namespace-ref)
                    namespace-oid (objects/object-namespace-oid db object)
                    relation-oid (:datahike.pg.object/oid relation)
                    _ (when-not (and (= :index (:datahike.pg.object/kind object))
                                     (= objects/pg-class-oid
                                        (:datahike.pg.object/class-oid object))
                                     (integer? oid)
                                     (string? name)
                                     (= (objects/address-key objects/pg-class-oid oid)
                                        (:datahike.pg.object/address-key object))
                                     (= (objects/identity-key
                                         objects/pg-class-oid
                                         namespace-oid name)
                                        (:datahike.pg.object/identity-key object))
                                     (integer? namespace-oid)
                                     (= namespace-eid relation-namespace-eid)
                                     (contains? #{(keyword "datahike.pg.index" name)
                                                  (objects/scoped-index-ident oid)}
                                                (:db/ident object))
                                     (string? table)
                                     relation
                                     (= :table (:datahike.pg.object/kind relation))
                                     (= objects/pg-class-oid
                                        (:datahike.pg.object/class-oid relation))
                                     (integer? relation-oid)
                                     (= (objects/address-key objects/pg-class-oid
                                                             relation-oid)
                                        (:datahike.pg.object/address-key relation))
                                     (= (objects/identity-key
                                         objects/pg-class-oid
                                         namespace-oid
                                         (:datahike.pg.object/name relation))
                                        (:datahike.pg.object/identity-key relation))
                                     (= table (or (:datahike.pg.object/storage-name relation)
                                                  (:datahike.pg.object/name relation)))
                                     (string? method)
                                     (string? keys-edn)
                                     (boolean? unique?))
                        (invalid-descriptor! name "incomplete or incoherent index metadata"))
                    _ (when (and (:datahike.pg.index/predicate object)
                                 (or unique? (not= "btree" method)))
                        (invalid-descriptor! name "partial declarations require a non-unique btree index"))
                    _ (when (and unique? (not= "btree" method))
                        (invalid-descriptor! name "unique enforcement requires btree"))
                    _ (when (and (or primary? constraint?) (not unique?))
                        (invalid-descriptor! name "a constraint index must enforce uniqueness"))
                    keys (try
                           (edn/read-string keys-edn)
                           (catch Exception _
                             (invalid-descriptor! name "malformed key metadata")))
                    attrs (mapv :storage-ident keys)]
                (when-not (and (vector? keys)
                               (if (true? legacy-incomplete?)
                                 (and (false? unique?) (empty? keys))
                                 (and (seq keys) (every? map? keys))))
                  (invalid-descriptor! name "missing key metadata"))
                (doseq [[key attr] (map vector keys attrs)]
                  (if-let [{:keys [sql columns type-oid]} (:expression key)]
                    (when-not (and (not unique?) (= "btree" method) (= 0 (:attnum key))
                                   (nil? attr) (string? sql) (pos-int? type-oid) (set? columns)
                                   (every? (fn [attnum]
                                             (when-let [column (objects/column-by-attnum db relation-oid attnum)]
                                               (not (:datahike.pg.column/dropped? column))))
                                           columns))
                      (invalid-descriptor! name "invalid expression key metadata"))
                    (let [schema-entry (get (dbi/-schema db) attr)
                          registered (objects/column-by-attnum db relation-oid (:attnum key))]
                      (when-not (and (keyword? attr) schema-entry)
                        (invalid-descriptor! name (str "unknown key attribute " attr)))
                      (when-not (and registered
                                     (not (:datahike.pg.column/dropped? registered))
                                     (= (:name key) (:datahike.pg.column/name registered))
                                     (= attr (:datahike.pg.column/storage-ident registered)))
                        (invalid-descriptor! name "key does not name a live table column"))
                      (when (= :db.cardinality/many (:db/cardinality schema-entry))
                        (invalid-descriptor! name "cardinality-many keys are unsupported")))))
                (when-not (get (dbi/-schema db) (keyword table "db-row-exists"))
                  (invalid-descriptor! name "table row marker is missing"))
                {:entity entity
                 :oid (long oid)
                 :name name
                 :constraint? (true? constraint?)
                 :table table
                 :keys keys
                 :attrs attrs
                 :marker (keyword table "db-row-exists")
                 :descendant-markers
                 (into #{} (map #(keyword % "db-row-exists"))
                       (descendant-tables db table))
                 :unique? unique?})))
           (filterv :unique?)
           (sort-by :oid)
           vec))))

(defonce ^:private descriptor-cache (cache/bounded-cache 8))

(defn index-descriptors [db]
  ;; Descriptor validation reads schema, registered objects/columns and
  ;; inheritance edges. All belong to the catalog selector. Ordinary row
  ;; writes preserve that token; catalog edits, including speculative ones,
  ;; invalidate it. The cached projection contains no DB or entity handles.
  (if-let [token (basis/checked-token db)]
    (or (.get ^java.util.Map descriptor-cache token)
        (let [descriptors (read-index-descriptors db)]
          (.put ^java.util.Map descriptor-cache token descriptors)
          descriptors))
    (read-index-descriptors db)))

(defn- physical-row? [descriptor entity]
  (and (true? (get entity (:marker descriptor)))
       (not-any? #(true? (get entity %)) (:descendant-markers descriptor))))

(defn own-row? [db descriptor eid]
  (physical-row? descriptor (d/entity db eid)))

(defn- reference-id [db value]
  (cond
    (nil? value) nil
    (instance? IEntity value) (:db/id value)
    (number? value) value
    :else (:db/id (d/entity db value))))

(defn- row-key-values [db descriptor eid]
  (let [entity (d/entity db eid)]
    (when (physical-row? descriptor entity)
      (let [values (mapv (fn [attr]
                           (let [value (get entity attr)]
                             (if (instance? IEntity value) (reference-id db value) value)))
                         (:attrs descriptor))]
        ;; PostgreSQL UNIQUE indexes use NULLS DISTINCT unless explicitly
        ;; declared otherwise (that syntax is rejected by the DDL parser).
        (when (every? some? values) values)))))

(defn row-key [db descriptor eid]
  (when-let [values (row-key-values db descriptor eid)]
    (mapv canonical-key-value values)))

(defn- unique-violation! [descriptor key]
  (throw (ex-info "unique violation"
                  {:error :unique-violation
                   :constraint (:name descriptor)
                   :table (:table descriptor)
                   :columns (mapv :name (:keys descriptor))
                   :value key})))

(defn- table-eids [db descriptor]
  (map first
       (d/q '{:find [?entity]
              :in [$ ?marker]
              :where [[?entity ?marker true]]}
            db (:marker descriptor))))

(defn validate-descriptor! [db descriptor]
  (reduce
   (fn [seen eid]
     (if-let [key (row-key db descriptor eid)]
       (if (contains? seen key)
         (unique-violation! descriptor key)
         (conj seen key))
       seen))
   #{}
   (table-eids db descriptor))
  nil)

(defn validate-db! [db]
  (doseq [descriptor (index-descriptors db)]
    (validate-descriptor! db descriptor))
  nil)

(defn unique-arbiters [db table]
  (into [] (comp (filter #(= table (:table %))) (map :attrs))
        (index-descriptors db)))

(defn- index-row-keys [db descriptor eids]
  (reduce
   (fn [by-key eid]
     (if-let [k (row-key db descriptor eid)]
       (let [prior (get by-key k)]
         ;; One other row suffices when a writer checks uniqueness on the
         ;; proposed db-after and excludes the candidate itself.
         (cond
           (nil? prior) (assoc by-key k eid)
           (vector? prior) by-key
           :else (assoc by-key k [prior eid])))
       by-key))
   {} eids))

(defn conflict-lookup
  "Return a key lookup for one physical table on this immutable database.
   Prefer a point lookup when any key component has zero or one AVET hit.
   For shared or unindexed values, build a composite-key map over the
   matching rows once. A new execution or writer basis gets a fresh lookup."
  ([db table] (conflict-lookup db table nil))
  ([db table descriptor]
   (let [schema (dbi/-schema db)
         reference-attrs (or (dbi/-attrs-by db :db.type/ref) #{})
         table-descriptor (delay (or descriptor
                                     {:table table :marker (keyword table "db-row-exists")
                                      :descendant-markers
                                      (into #{} (map #(keyword % "db-row-exists"))
                                            (descendant-tables db table))}))
         groups (volatile! {})]
     (fn [attrs values & [except-eid]]
       (let [values (if (some reference-attrs attrs)
                      (mapv (fn [attr value]
                              (if (contains? reference-attrs attr)
                                (reference-id db value)
                                value))
                            attrs values)
                      values)]
         (when (and (seq attrs) (every? some? values))
           (let [descriptor (assoc @table-descriptor :attrs attrs)
                 key (mapv canonical-key-value values)
                 probes (keep-indexed
                         (fn [i attr]
                           (let [entry (get schema attr)
                                 value (nth values i)]
                             (when (and (or (:db/index entry) (:db/unique entry))
                                        (index-probe-safe? value))
                               [i (d/datoms db :avet attr value)])))
                         attrs)]
             (if-let [[_ hits] (some (fn [[_ hits :as probe]]
                                       (when-not (next hits) probe))
                                     probes)]
               (when-let [hit (first hits)]
                 (let [eid (.-e ^datahike.datom.Datom hit)]
                   (when (and (not= except-eid eid) (= key (row-key db descriptor eid))) eid)))
               (let [[i hits] (first probes)
                     group (if i [attrs i (nth key i)] [attrs])
                     by-key
                     (or (get @groups group)
                         (let [eids (if i
                                      (map #(.-e ^datahike.datom.Datom %) hits)
                                      (table-eids db descriptor))
                               m (index-row-keys db descriptor eids)]
                           (vswap! groups assoc group m)
                           m))
                     found (get by-key key)]
                 (if (vector? found)
                   (if (= except-eid (first found)) (second found) (first found))
                   (when (not= except-eid found) found)))))))))))

(defn- report-datoms [report]
  (filter #(instance? datahike.datom.Datom %)
          (:tx-data report)))

(defn- validate-eids! [db descriptor eids]
  ;; SQL arbitration reads from the pre-write snapshot. Here the same lookup
  ;; reads from the writer's post-write snapshot and excludes the changed
  ;; entity itself: uniqueness fails only if another physical row matches it.
  (let [lookup (conflict-lookup db (:table descriptor) descriptor)]
    (doseq [eid eids
            :let [values (row-key-values db descriptor eid)]
            :when (and values (lookup (:attrs descriptor) values eid))]
      (unique-violation! descriptor (mapv canonical-key-value values)))))

(defn validate-report!
  "Writer-side invariant gate over a fully resolved Datahike tx report."
  [{:keys [db-before db-after] :as report}]
  (let [datoms (vec (report-datoms report))
        touched-attrs (into #{}
                            (map (fn [^datahike.datom.Datom datom]
                                   (let [attr (.-a datom)]
                                     ;; Reports contain numeric attribute refs
                                     ;; when enabled; descriptors use idents.
                                     ;; Retractions may need the old mapping.
                                     (or (dbi/-ident-for db-after attr)
                                         (dbi/-ident-for db-before attr)
                                         attr))))
                            datoms)
        touched-eids (into #{} (map #(.-e ^datahike.datom.Datom %)) datoms)
        catalog-change? (some descriptor-attrs touched-attrs)
        wholesale-change? (and (empty? datoms) (not (identical? db-before db-after)))]
    (doseq [descriptor (index-descriptors db-after)]
      (if (or catalog-change? wholesale-change?)
        (validate-descriptor! db-after descriptor)
        (when (or (some touched-attrs (:attrs descriptor))
                  (contains? touched-attrs (:marker descriptor))
                  (some touched-attrs (:descendant-markers descriptor)))
          (validate-eids! db-after descriptor touched-eids))))
    nil))
