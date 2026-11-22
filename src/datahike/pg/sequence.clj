(ns datahike.pg.sequence
  "Stored sequence position shared by execution, catalog reads, and dumps."
  (:require [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.db.utils :as dbu]))

(def called-attribute
  {:db/ident :__seq__/is-called
   :db/valueType :db.type/boolean
   :db/cardinality :db.cardinality/one})

(def position-attributes #{:__seq__/is-called :__seq__/value})

(defn id-by-name [db name]
  (when (and name db (contains? (dbi/-schema db) :__seq__/name))
    (dbu/entid db [:__seq__/name name])))

(defn entity-by-name [db name]
  (when-let [eid (id-by-name db name)]
    (d/pull db '[*] eid)))

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
