(ns datahike.pg.constraints.unique
  "Removable PostgreSQL UNIQUE INDEX enforcement over durable descriptors."
  (:require [clojure.edn :as edn]
            [datahike.api :as d]
            [datahike.array :as array]
            [datahike.datom]
            [datahike.db.interface :as dbi]
            [datahike.pg.catalog.objects :as objects]))

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

(defn index-descriptors [db]
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
           object-eids index-attrs)
          public-namespace (objects/object-by-address
                            db objects/pg-namespace-oid
                            objects/public-namespace-oid)]
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
                                         objects/public-namespace-oid name)
                                        (:datahike.pg.object/identity-key object))
                                     (= (:db/id public-namespace) namespace-eid)
                                     (= (keyword "datahike.pg.index" name)
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
                                         objects/public-namespace-oid table)
                                        (:datahike.pg.object/identity-key relation))
                                     (= (:db/id public-namespace)
                                        relation-namespace-eid)
                                     (= table (:datahike.pg.object/name relation))
                                     (string? method)
                                     (string? keys-edn)
                                     (boolean? unique?))
                        (invalid-descriptor! name "incomplete or incoherent index metadata"))
                    _ (when (and unique? (not= "btree" method))
                        (invalid-descriptor! name "unique enforcement requires btree"))
                    keys (try
                           (edn/read-string keys-edn)
                           (catch Exception _
                             (invalid-descriptor! name "malformed key metadata")))
                    attrs (mapv :storage-ident keys)]
                (when-not (and (vector? keys)
                               (if (true? legacy-incomplete?)
                                 (and (false? unique?) (empty? keys))
                                 (and (seq keys)
                                      (every? map? keys)
                                      (every? keyword? attrs))))
                  (invalid-descriptor! name "missing key storage identity"))
                (doseq [[key attr] (map vector keys attrs)
                        :let [schema-entry (get (dbi/-schema db) attr)
                              registered (objects/column-by-attnum
                                          db relation-oid (:attnum key))]]
                  (when-not schema-entry
                    (invalid-descriptor! name (str "unknown key attribute " attr)))
                  (when-not (and registered
                                 (not (:datahike.pg.column/dropped? registered))
                                 (= (:name key) (:datahike.pg.column/name registered))
                                 (= attr (:datahike.pg.column/storage-ident registered)))
                    (invalid-descriptor! name "key does not name a live table column"))
                  (when (= :db.cardinality/many (:db/cardinality schema-entry))
                    (invalid-descriptor! name "cardinality-many keys are unsupported")))
                (when-not (get (dbi/-schema db) (keyword table "db-row-exists"))
                  (invalid-descriptor! name "table row marker is missing"))
                {:entity entity
                 :oid (long oid)
                 :name name
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

(defn own-row? [db descriptor eid]
  (let [entity (d/entity db eid)]
    (and (true? (get entity (:marker descriptor)))
         (not-any? #(true? (get entity %))
                   (:descendant-markers descriptor)))))

(defn row-key [db descriptor eid]
  (when (own-row? db descriptor eid)
    (let [entity (d/entity db eid)
          values (mapv #(get entity %) (:attrs descriptor))]
      ;; PostgreSQL UNIQUE indexes use NULLS DISTINCT unless explicitly
      ;; declared otherwise (that syntax is rejected by the DDL parser).
      (when (every? some? values)
        (mapv canonical-key-value values)))))

(defn- unique-violation! [descriptor key]
  (throw (ex-info "unique violation"
                  {:error :unique-violation
                   :sqlstate "23505"
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

(defn conflicting-eid
  "Find an existing row of `table` with all `attrs` equal to `values`.
   Descendant rows are excluded so inheritance does not merge physical index
   domains that PostgreSQL keeps separate."
  [db table attrs values]
  (when (and (seq attrs) (every? some? values))
    (let [descriptor {:table table
                      :attrs (vec attrs)
                      :marker (keyword table "db-row-exists")
                      :descendant-markers
                      (into #{} (map #(keyword % "db-row-exists"))
                            (descendant-tables db table))}
          expected (mapv canonical-key-value values)]
      (some (fn [eid]
              (when (and (own-row? db descriptor eid)
                         (= expected (row-key db descriptor eid)))
                eid))
            (if-let [probe-attr
                     (some (fn [attr]
                             (let [entry (get (dbi/-schema db) attr)
                                   value (nth values
                                              (.indexOf ^java.util.List
                                               (vec attrs) attr))]
                               (when (and (or (:db/index entry)
                                              (:db/unique entry))
                                          (index-probe-safe? value))
                                 attr)))
                           attrs)]
              (map first
                   (d/q '{:find [?entity]
                          :in [$ ?attr ?value]
                          :where [[?entity ?attr ?value]]}
                        db probe-attr
                        (nth values (.indexOf ^java.util.List (vec attrs)
                                              probe-attr))))
              (table-eids db descriptor))))))

(defn- report-datoms [report]
  (filter #(instance? datahike.datom.Datom %)
          (:tx-data report)))

(defn- candidate-eids [db descriptor eid]
  (let [probe-attr (some (fn [attr]
                           (let [entry (get (dbi/-schema db) attr)]
                             (when (or (:db/index entry) (:db/unique entry))
                               attr)))
                         (:attrs descriptor))
        first-value (when probe-attr (get (d/entity db eid) probe-attr))
        probe-attr (when (and probe-attr (index-probe-safe? first-value))
                     probe-attr)]
    ;; Query using the unwrapped stored value. The first key attribute is
    ;; monotonically AVET-indexed by CREATE INDEX; Datalog retains Datahike's
    ;; value equality for arrays while row-key supplies hashable comparison.
    (if probe-attr
      (map first
           (d/q '{:find [?entity]
                  :in [$ ?attr ?value]
                  :where [[?entity ?attr ?value]]}
                db probe-attr first-value))
      (table-eids db descriptor))))

(defn- validate-eids! [db descriptor eids]
  (doseq [eid eids
          :let [key (row-key db descriptor eid)]
          :when key
          other (candidate-eids db descriptor eid)
          :when (and (not= eid other)
                     (own-row? db descriptor other)
                     (= key (row-key db descriptor other)))]
    (unique-violation! descriptor key)))

(defn validate-report!
  "Writer-side invariant gate over a fully resolved Datahike tx report."
  [{:keys [db-before db-after] :as report}]
  (let [datoms (vec (report-datoms report))
        touched-attrs (into #{} (map #(.-a ^datahike.datom.Datom %)) datoms)
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
