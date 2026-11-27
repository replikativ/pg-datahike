(ns datahike.pg.sql.inheritance
  "Physical storage and scan branches for SQL table inheritance."
  (:require [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.schema :as pgs]))

(def layout-attr
  {:db/ident :datahike.pg.object/relation-layout
   :db/valueType :db.type/long
   :db/cardinality :db.cardinality/one})

(defn table-object [db table]
  (or (objects/object-by-storage-name db table)
      (objects/object-by-identity db objects/pg-class-oid objects/public-namespace-oid table)))

(defn physical? [db table]
  (when (and db (get (dbi/-schema db) (:db/ident layout-attr)))
    (when-let [eid (:db/id (table-object db table))]
      (= 2 (:datahike.pg.object/relation-layout (d/entity db eid))))))

(defn descendants [db table]
  (when (and db (get (dbi/-schema db) :__inherit__/parent))
    (loop [frontier #{table}, seen #{table}]
      (let [children (into #{}
                           (comp (map first) (remove seen))
                           (d/q '[:find ?child :in $ [?parent ...]
                                  :where [?e :__inherit__/parent ?parent]
                                  [?e :__inherit__/child ?child]]
                                db (vec frontier)))]
        (if (seq children)
          (recur children (into seen children))
          (disj seen table))))))

(defn- column-map [db oid]
  (into {}
        (map (juxt :datahike.pg.column/name :datahike.pg.column/storage-ident))
        (objects/columns-by-relation db oid true)))

(defn scan-branches
  "A branch for the target and for each descendant with separate storage.
   Legacy descendants still enter through the ancestor's stored marker."
  [db table]
  (when (and db (get (dbi/-schema db) (:db/ident layout-attr)))
    (when-let [children (seq (filter #(physical? db %) (sort (descendants db table))))]
      (let [root-oid (:datahike.pg.object/oid (table-object db table))
            root-columns (column-map db root-oid)]
        (mapv (fn [storage]
                (let [oid (:datahike.pg.object/oid (table-object db storage))
                      local (column-map db oid)]
                  {:table storage
                   :marker (pgs/row-marker-attr storage)
                   :oid oid
                   :attrs (into {}
                                (map (fn [[name attr]]
                                       [attr (or (get local name)
                                                 (throw (ex-info "Missing inherited storage column"
                                                                 {:table storage :column name})))]))
                                root-columns)}))
              (cons table children))))))

(defn row-branch [branches entity]
  (some #(when (true? (get entity (:marker %))) %) branches))

(defn branch-clause
  "Bind an entity and optional projected value inside its physical branch."
  [branches eid output projection]
  (list* 'or-join (cond-> [eid] output (conj output))
         (map (fn [{:keys [marker] :as branch}]
                (let [member [eid marker true]]
                  (if output
                    (list 'and member [(projection branch) output])
                    member)))
              branches)))

(defn key-clause [branches eid attr value]
  (list* 'or-join (cond-> [eid] (symbol? value) (conj value))
         (map (fn [{:keys [marker attrs]}]
                (list 'and [eid marker true] [eid (get attrs attr attr) value]))
              branches)))

(defn project-row [branch row]
  (reduce-kv (fn [attrs target source] (assoc attrs target (get row source)))
             row (:attrs branch)))

(defn catalog-edges [db]
  (when (and db (get (dbi/-schema db) :__inherit__/parent))
    (into []
          (keep (fn [[child parent ordinal]]
                  (let [child-oid (:datahike.pg.object/oid (table-object db child))
                        parent-oid (:datahike.pg.object/oid (table-object db parent))]
                    (when (and child-oid parent-oid)
                      [child-oid parent-oid (inc ordinal)]))))
          (d/q '[:find ?child ?parent ?ordinal
                 :where [?edge :__inherit__/child ?child]
                 [?edge :__inherit__/parent ?parent]
                 [(get-else $ ?edge :__inherit__/ordinal 0) ?ordinal]]
               db))))
