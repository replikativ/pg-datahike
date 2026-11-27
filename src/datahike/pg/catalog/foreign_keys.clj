(ns datahike.pg.catalog.foreign-keys
  "Stored foreign-key definitions shared by catalog projection and row enforcement."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.jsonb :as jb]))

(def ^:private definitions-query
  '{:find [?name ?child ?child-cols ?parent ?parent-cols ?on-update ?on-delete]
    :keys [name child-table child-cols parent-table parent-cols on-update on-delete]
    :where [[?fk :pg/fk-child-table ?child]
            [?fk :pg/fk-parent-table ?parent]
            [?fk :pg/fk-child-cols ?child-cols]
            [?fk :pg/fk-parent-cols ?parent-cols]
            [(get-else $ ?fk :pg/fk-on-update :no-action) ?on-update]
            [(get-else $ ?fk :pg/fk-on-delete :no-action) ?on-delete]
            (or-join [?fk ?name]
                     [?fk :pg/fk-conname ?name]
                     [?fk :pg/fk-name ?name])]})

(defn definitions
  "Return raw column definitions in their stored order. A bound endpoint
   uses the physical table name, including for relations outside public."
  ([db] (d/q definitions-query db))
  ([db side table]
   (let [binding (case side :child '?child :parent '?parent)]
     (d/q (assoc definitions-query :in ['$ binding]) db table))))

(defn action-code [action]
  (case action
    :no-action "a"
    :restrict "r"
    :cascade "c"
    :set-null "n"
    :set-default "d"
    " "))

(defn for-children [db children]
  (d/q (assoc definitions-query :in '[$ [?child ...]]) db children))

(defn column-names
  "Decode the stored ordered column list, including legacy scalar spellings."
  [value]
  (try
    (let [v (jb/parse-jsonb value)]
      (if (sequential? v) (vec v) [v]))
    (catch Exception _ [value])))

(defn catalog-fields
  "Project the same relation addresses, key vectors and actions for a scan or lookup."
  [{:keys [name on-delete on-update]} child parent child-attnums parent-attnums]
  {:pg_constraint/db-row-exists true
   :pg_constraint/contype "f"
   :pg_constraint/conname name
   :pg_constraint/conrelid (long child)
   :pg_constraint/confrelid (long parent)
   :pg_constraint/conkey (str "{" (str/join "," child-attnums) "}")
   :pg_constraint/confkey (str "{" (str/join "," parent-attnums) "}")
   :pg_constraint/confdeltype (action-code on-delete)
   :pg_constraint/confupdtype (action-code on-update)})
