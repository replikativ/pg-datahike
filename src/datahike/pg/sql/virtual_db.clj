(ns datahike.pg.sql.virtual-db
  "Query-local catalog and relation materialization over Datahike views."
  (:require [datahike.api :as d]
            [datahike.datom :as datom]
            [datahike.db.interface :as dbi])
  (:import [datahike.db FilteredDB]))

(deftype VirtualDB [backing source cutoff context]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [_ k fallback]
    (case k
      (:schema :config :store :max-tx :max-eid) (get backing k fallback)
      fallback))

  dbi/IDB
  (-schema [_] (dbi/-schema backing))
  (-rschema [_] (dbi/-rschema backing))
  (-system-entities [_] (dbi/-system-entities backing))
  (-attrs-by [_ property] (dbi/-attrs-by backing property))
  (-max-tx [_] (dbi/-max-tx backing))
  (-max-eid [_] (dbi/-max-eid backing))
  (-temporal-index? [_] (dbi/-temporal-index? backing))
  (-keep-history? [_] (dbi/-keep-history? backing))
  (-config [_] (dbi/-config backing))
  (-ref-for [_ ident] (dbi/-ref-for backing ident))
  (-ident-for [_ ref] (dbi/-ident-for backing ref))

  dbi/ISecondaryView
  (-secondary-view [_] (dbi/-secondary-view source))

  dbi/ISearch
  (-search-context [_] context)
  (-search [_ pattern ctx] (dbi/-search backing pattern ctx))
  (-batch-search [_ pattern-mask batch-fn ctx]
    (dbi/-batch-search backing pattern-mask batch-fn ctx))

  dbi/IIndexAccess
  (-datoms [_ index components ctx] (dbi/-datoms backing index components ctx))
  (-seek-datoms [_ index components ctx] (dbi/-seek-datoms backing index components ctx))
  (-rseek-datoms [_ index components ctx] (dbi/-rseek-datoms backing index components ctx))
  (-index-range [this attr start end ctx]
    (dbi/-index-range backing attr start end (dbi/context-set-current-db-if-not-set ctx this))))

(defn- origin [db]
  (loop [db db]
    (cond
      (instance? FilteredDB db) (recur (.-unfiltered-db ^FilteredDB db))
      (satisfies? dbi/IHistory db) (recur (dbi/-origin db))
      :else db)))

(defn- include-virtual-datoms [xf virtual?]
  (fn [rf]
    (let [filtered-rf (xf rf)]
      (fn
        ([] (filtered-rf))
        ([acc] (filtered-rf acc))
        ([acc datom]
         (if (virtual? datom)
           (rf acc datom)
           (filtered-rf acc datom)))))))

(defn- view-context [source cutoff]
  (let [context (dbi/-search-context source)
        time-pred (dbi/context-time-pred context)
        xform (dbi/context-xform context)
        ;; The backing DB is immutable. Every transaction above this
        ;; cutoff was created for a SQL virtual relation in this query.
        virtual? #(> (datom/datom-tx %) cutoff)]
    (cond-> context
      time-pred (assoc :timepred #(or (virtual? %) (time-pred %)))
      xform (assoc :xform (include-virtual-datoms xform virtual?)))))

(defn with-data
  "Materialize a virtual relation in the query's DB value. Derive its data
   from the supplied view before calling this function. On historical or
   filtered views, preserve the view's user datoms and admit the local
   materialization transactions. The backing snapshot shares its indexes
   with the original immutable DB."
  [db tx-data]
  (cond
    (instance? VirtualDB db)
    (let [^VirtualDB view db]
      (VirtualDB. (d/db-with (.-backing view) tx-data)
                  (.-source view) (.-cutoff view) (.-context view)))

    :else
    (let [base (origin db)]
      (if (identical? db base)
        (d/db-with db tx-data)
        (let [cutoff (dbi/-max-tx base)]
          (VirtualDB. (d/db-with base tx-data) db cutoff (view-context db cutoff)))))))
