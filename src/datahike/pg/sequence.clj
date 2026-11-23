(ns datahike.pg.sequence
  "Stored sequence position shared by execution, catalog reads, and dumps."
  (:require [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.db.utils :as dbu]))

(def called-attribute
  {:db/ident :__seq__/is-called
   :db/valueType :db.type/boolean
   :db/cardinality :db.cardinality/one})

(def ownership-attribute
  {:db/ident :__seq__/owned-by-column
   :db/valueType :db.type/string
   :db/cardinality :db.cardinality/one
   :db/index true})

(def position-attributes #{:__seq__/is-called :__seq__/value})

(defn id-by-name [db name]
  (when (and name db (contains? (dbi/-schema db) :__seq__/name))
    (dbu/entid db [:__seq__/name name])))

(defn entity-by-name [db name]
  (when-let [eid (id-by-name db name)]
    (d/pull db '[*] eid)))

(defn owned-by-column
  "Sequence names owned by a stable catalog column. Older SERIAL and
   IDENTITY declarations store the physical table and conventional name."
  [db table column]
  (let [schema (dbi/-schema db)
        address (:datahike.pg.column/address-key column)
        storage (:datahike.pg.column/storage-ident column)
        explicit (when (and address (contains? schema :__seq__/owned-by-column))
                   (mapv
                    (fn [datom] (:__seq__/name (d/entity db (:e datom))))
                    (d/datoms db :avet :__seq__/owned-by-column address)))
        legacy-name (when storage (str table "_" (name storage) "_seq"))
        legacy (entity-by-name db legacy-name)]
    (cond-> explicit
      (and (= table (:__seq__/owned-by-table legacy))
           (nil? (:__seq__/owned-by-column legacy)))
      (conj legacy-name))))

(defn addressed-owned-columns
  "Sequence bindings for columns with a persistent catalog address."
  [db table]
  (when (and (contains? (dbi/-schema db) :__seq__/owned-by-column)
             (contains? (dbi/-schema db) :__seq__/owned-by-table)
             (contains? (dbi/-schema db) :__seq__/identity-generation))
    (mapv (fn [[attr seq-name generation]]
            {:col (name attr) :ns (namespace attr) :seq-name seq-name :generation generation})
          (d/q '[:find ?attr ?seq-name ?generation
                 :in $ ?table
                 :where [?seq :__seq__/owned-by-table ?table]
                 [?seq :__seq__/owned-by-column ?address]
                 [?seq :__seq__/name ?seq-name]
                 [?column :datahike.pg.column/address-key ?address]
                 [?column :datahike.pg.column/storage-ident ?attr]
                 [(get-else $ ?seq :__seq__/identity-generation "") ?generation]]
               db table))))

(defn position
  "Return [last_value is_called]. Older stores distinguish the initial
   position from an advanced one with the start-minus-increment sentinel."
  [{:__seq__/keys [value is-called start increment]
    :or {value 0 start 1 increment 1}}]
  (if (some? is-called)
    [value is-called]
    (if (= value (-' start increment))
      [start false]
      [value true])))

(defn install-called-attribute [db]
  (if (contains? (dbi/-schema db) :__seq__/is-called)
    []
    [called-attribute]))

(defn position-tx [db sequence-ref [value called?]]
  (conj (install-called-attribute db)
        [:db/add sequence-ref :__seq__/value value]
        [:db/add sequence-ref :__seq__/is-called called?]))
