(ns datahike.pg.sequence
  "Stored sequence position shared by execution, catalog reads, and dumps."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.db.utils :as dbu]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.catalog.regoid :as regoid]))

(def called-attribute
  {:db/ident :__seq__/is-called
   :db/valueType :db.type/boolean
   :db/cardinality :db.cardinality/one})

(def identity-generation-attribute
  {:db/ident :__seq__/identity-generation
   :db/valueType :db.type/string
   :db/cardinality :db.cardinality/one})

(def persistence-attribute
  {:db/ident :__seq__/persistence
   :db/valueType :db.type/string
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

(defn object-by-storage-name [db storage]
  (let [registered (objects/object-by-storage-name db storage)
        object (if (= :sequence (:datahike.pg.object/kind registered))
                 registered
                 (objects/object-by-identity db objects/pg-class-oid objects/public-namespace-oid storage))]
    (when (= :sequence (:datahike.pg.object/kind object)) object)))

(defn object-storage-name [object]
  (or (:datahike.pg.object/storage-name object) (:datahike.pg.object/name object)))

(defn counter-reference
  "Return a supported default reference to a known counter."
  [db storage]
  (let [sequence (object-by-storage-name db storage)]
    (if sequence (str (:datahike.pg.object/oid sequence)) storage)))

(defn catalog-name [db storage]
  (let [object (object-by-storage-name db storage)
        namespace-oid (or (objects/object-namespace-oid db object) objects/public-namespace-oid)]
    {:name (or (:datahike.pg.object/name object) storage)
     :schema (or (:datahike.pg.object/name (objects/namespace-by-oid db namespace-oid)) "public")
     :namespace-oid namespace-oid}))

(defn stored-counter-reference
  "Bind a legacy default that stored the public counter name. Numeric and
   qualified references already carry their own identity."
  [db reference]
  (if (and (string? reference)
           (re-matches #"[a-z_][a-z0-9_$]*" reference)
           (id-by-name db reference))
    (if-let [object (object-by-storage-name db reference)]
      (str (:datahike.pg.object/oid object))
      (str "public." reference))
    reference))

(defn reference-storage-name
  "Resolve a SQL sequence name or numeric regclass value to the counter's name."
  [db spelling]
  (when (and db (string? spelling))
    (if (re-matches #"[0-9]+" spelling)
      (when-let [oid (parse-long spelling)]
        (when (objects/valid-user-oid? oid)
          (let [object (objects/object-by-address db objects/pg-class-oid oid)]
            (when (= :sequence (:datahike.pg.object/kind object)) (object-storage-name object)))))
      (let [parts (regoid/parse-qualified-name spelling)
            schema (when (> (count parts) 1) (nth parts (- (count parts) 2)))
            legacy (if (= 1 (count parts)) spelling (last (str/split spelling #"\." 2)))
            oid (try
                  (binding [regoid/*db* db regoid/*relation-db* db
                            regoid/*relation-provider* regoid/registry-relation]
                    (regoid/relation-name-oid spelling))
                  (catch clojure.lang.ExceptionInfo error
                    (let [state (:sqlstate (ex-data error))]
                      (when-not (or (#{"42P01" "3F000"} state)
                                    (and (= "0A000" state) (= 1 (count parts))
                                         (str/starts-with? (peek parts) "__dh_pg_temp_")))
                        (throw error)))))
            object (when oid (objects/object-by-address db objects/pg-class-oid oid))]
        (if object
          (when (= :sequence (:datahike.pg.object/kind object)) (object-storage-name object))
          ;; Old stores could use the literal spelling and have no catalog
          ;; object. Such counters belong to public.
          (when (or (= "public" schema) (and (nil? schema) (some #{"public"} regoid/*search-path*)))
            (some (fn [candidate]
                    (when (and (id-by-name db candidate)
                               (nil? (:datahike.pg.object/storage-name (object-by-storage-name db candidate))))
                      candidate))
                  [(peek parts) legacy])))))))

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
