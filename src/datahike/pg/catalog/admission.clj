(ns datahike.pg.catalog.admission
  "Closed metadata observations for an explicitly attested INSERT subset."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db]
            [datahike.db.interface :as dbi]
            [datahike.db.utils :as dbu]
            [datahike.query :as query]
            [datahike.pg.catalog.basis :as basis]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.constraints.row :as row]
            [datahike.pg.schema :as pgs]))

(def ^:private scalar-types
  #{:db.type/string :db.type/long :db.type/boolean :db.type/double
    :db.type/float :db.type/bigdec :db.type/bigint :db.type/uuid :db.type/instant})
(def ^:private builtin-types
  #{"int2" "int4" "int8" "text" "varchar" "bpchar" "bool"
    "float4" "float8" "numeric" "uuid" "date" "timestamp" "timestamptz"})

(defn- unsupported! [] (throw (ex-info "Outside literal INSERT observation bounds" {::unsupported true})))

(defn- bounded-vector [xs limit]
  (let [result (vec (take (inc limit) xs))]
    (when (> (count result) limit) (unsupported!))
    result))

(defn- eids-for [db attr value]
  (if (contains? (dbi/-schema db) attr)
    (set (d/q '[:find [?e ...] :in $ ?a ?v :where [?e ?a ?v]] db attr value))
    #{}))

(defn- stable-reference [db eid]
  (let [entity (d/entity db eid)]
    (or (some-> (:db/ident entity) (vector :ident))
        (some-> (:datahike.pg.object/address-key entity) (vector :object-address))
        (some-> (:datahike.pg.column/address-key entity) (vector :column-address))
        [:entity eid])))

(defn- raw-entity [db eid]
  (when eid
    (set (map (fn [datom]
                (let [attr (dbi/-ident-for db (:a datom))
                      value (:v datom)]
                  [attr (if (and (integer? value) (dbu/ref? db attr))
                          (stable-reference db value)
                          value)]))
              (bounded-vector (d/datoms db :eavt eid) 4096)))))

(defn- observations [db table target-name dependency-shape raw-constraint-metadata]
  (let [schema (dbi/-schema db)
        hints (binding [pgs/*catalog-tx-cache* nil] (pgs/schema-hints db))
        ci (pgs/ci-index schema hints)
        canonical (pgs/canonical-table ci target-name)
        attrs (bounded-vector (sort (filter #(and (keyword? %) (= table (namespace %)))
                                            (keys schema))) 256)
        columns (bounded-vector (pgs/column-info schema table db) 257)
        entities (into {} (map (fn [attr]
                                 [attr (d/q '[:find ?e . :in $ ?a :where [?e :db/ident ?a]] db attr)])) attrs)
        raw-columns (into {} (map (fn [[attr eid]] [attr (raw-entity db eid)])) entities)
        relation (objects/object-by-identity db objects/pg-class-oid
                                             objects/public-namespace-oid table)
        registered-columns (when-let [oid (:datahike.pg.object/oid relation)]
                             (bounded-vector (objects/columns-by-relation db oid) 256))
        constraints (or raw-constraint-metadata (row/constraint-metadata db table))
        parents (eids-for db :__inherit__/child table)
        children (eids-for db :__inherit__/parent table)
        incoming-fks (eids-for db :pg/fk-parent-table table)
        views (eids-for db :datahike.pg/view-name table)
        target-hints (into {} (map (fn [attr]
                                     [attr (->> (eids-for db :datahike.pg/for-ident attr)
                                                (map #(raw-entity db %))
                                                (sort-by pr-str)
                                                vec)])) attrs)
        reverse-tuples (into {} (filter (fn [[_ spec]] (some (set attrs) (:db/tupleAttrs spec)))) schema)
        sequences (if (contains? schema :__seq__/name)
                    (set (filter (fn [[_ name]]
                                   (and (string? name) (str/starts-with? name (str table "_"))
                                        (str/ends-with? name "_seq")
                                        (< (inc (count table)) (- (count name) 4))
                                        (contains? (set attrs)
                                                   (keyword table (subs name (inc (count table))
                                                                        (- (count name) 4))))))
                                 (d/q '[:find ?e ?name :where [?e :__seq__/name ?name]] db)))
                    #{})
        feature? (fn [attr]
                   (let [entity (d/entity db attr)]
                     (or (:datahike.pg/domain-of entity) (:datahike.pg/enum-of entity)
                         (:pg/array-elem entity) (:pg/default-kind entity)
                         (when-let [type (:pg/type entity)] (not (builtin-types type))))))
        eligible? (and (contains? #{:literal-insert-v1 :target-delete-v1}
                                  dependency-shape)
                       (= canonical table) (seq columns)
                       (every? #(and (scalar-types (:db/valueType (get schema %)))
                                     (= :db.cardinality/one (:db/cardinality (get schema %)))
                                     (not (feature? %))) attrs)
                       (or (nil? relation) (= :table (:datahike.pg.object/kind relation)))
                       (every? #(and (contains? #{16 20 21 23 25 700 701 1042 1043
                                                  1082 1114 1184 1700 2950}
                                                (:datahike.pg.column/type-oid %))
                                     (not (pos? (or (:datahike.pg.column/inherit-count %) 0))))
                               registered-columns)
                       (empty? parents) (empty? views) (empty? reverse-tuples)
                       (if (= :literal-insert-v1 dependency-shape)
                         (and (empty? sequences)
                              (empty? (:checks constraints)) (empty? (:fks constraints))
                              (empty? (:domain-enum constraints))
                              (not-any? :default (:columns constraints)))
                         (and (empty? children) (empty? incoming-fks))))]
    (when eligible?
      {:canonical canonical :columns columns :schema (select-keys schema attrs)
       ;; Canonical column resolution includes registered relations outside the
       ;; public namespace. Preserve absence separately from an empty entry.
       :column-resolution (select-keys (:columns ci) [table])
       :registered-column-resolution (select-keys (:registered-columns ci) [table])
       :ident-entities (update-vals entities boolean)
       :column-metadata raw-columns :hints target-hints
       :relation (raw-entity db (:db/id relation))
       :registered-columns (mapv (fn [column]
                                   [(:datahike.pg.column/attnum column)
                                    (raw-entity db (:db/id column))])
                                 registered-columns)
       :table-oid (pgs/table-oid db table)
       :constraints constraints :inheritance parents :children children
       :incoming-fks incoming-fks :views views
       :reverse-tuples reverse-tuples :sequences sequences})))

(defn- observed [db table target-name dependency-shape raw-constraint-metadata]
  (try
    ;; An admission proof must observe this exact candidate, including inside
    ;; a transaction. Do not inherit the query result cache's snapshot keys.
    (when-let [value (binding [query/*query-result-cache?* false
                               pgs/*catalog-tx-cache* nil]
                       (observations db table target-name dependency-shape
                                     raw-constraint-metadata))]
      (let [frozen (basis/freeze-projection value)]
        (when (:token-eligible? frozen) (:value frozen))))
    (catch clojure.lang.ExceptionInfo error
      (if (or (::unsupported (ex-data error))
              (contains? #{:catalog-basis-limit :catalog-basis-unsupported-value}
                         (:error (ex-data error))))
        nil
        (throw error)))))

(defn capture
  "Return a detached certificate for the translator's attested literal INSERT
   subset, or nil. No SQL/default evaluation occurs. Reader errors propagate.
   Optional raw constraint metadata must come from this same DB and table;
   it is reused only during capture, never during writer-time validation."
  ([db parsed] (capture db parsed nil))
  ([db parsed raw-constraint-metadata]
   (when (and (instance? datahike.db.DB db)
              (= :write (:schema-flexibility (dbi/-config db)))
              (contains? #{[:insert :literal-insert-v1]
                           [:delete :target-delete-v1]}
                         [(:type parsed) (:catalog-dependency-shape parsed)])
              (string? (:table parsed)) (<= 1 (count (:table parsed)) 512))
     (when-let [value (observed db (:table parsed)
                                (or (:catalog-target-name parsed) (:table parsed))
                                (:catalog-dependency-shape parsed)
                                raw-constraint-metadata)]
       {::certificate true :table (:table parsed)
        :dependency-shape (:catalog-dependency-shape parsed)
        :target-name (or (:catalog-target-name parsed) (:table parsed))
        :config (select-keys (dbi/-config db) [:attribute-refs? :schema-flexibility])
        :observations value}))))

(defn valid?
  "Recheck the closed metadata observations on the writer's current DB."
  [certificate db]
  (boolean
   (and (::certificate certificate) (instance? datahike.db.DB db)
        (= (:config certificate)
           (select-keys (dbi/-config db) [:attribute-refs? :schema-flexibility]))
        (= (:observations certificate)
           (observed db (:table certificate) (:target-name certificate)
                     (:dependency-shape certificate) nil)))))
