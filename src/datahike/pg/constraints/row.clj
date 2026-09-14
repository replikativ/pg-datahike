(ns datahike.pg.constraints.row
  "Shared PostgreSQL row preparation and immediate constraint phases.

   `prepare-candidate` materializes INSERT defaults and enforces NOT NULL.
   `validate-pre-arbiter!` adds CHECK/domain validation before ON CONFLICT
   arbitration. `validate-mutation!` validates an actual INSERT/UPDATE result
   and therefore also enforces foreign keys.  Keeping those phases distinct
   mirrors PostgreSQL's ExecInsert/ExecOnConflictUpdate ordering."
  (:require [datahike.api :as d]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.schema :as pgs])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]))

(defn column-specs [db table-name]
  (let [schema (:schema db)
        ancestors (loop [child table-name, seen #{table-name}, out []]
                    (let [parent (when (get schema :__inherit__/child)
                                   (ffirst
                                    (d/q '{:find [?parent]
                                           :in [$ ?child]
                                           :where [[?edge :__inherit__/child ?child]
                                                   [?edge :__inherit__/parent ?parent]]}
                                         db child)))]
                      (if (and parent (not (seen parent)))
                        (recur parent (conj seen parent) (conj out parent))
                        out)))]
    (let [ordered-columns
          (:columns
           (reduce (fn [{:keys [seen] :as state} column]
                     (if (contains? seen (:name column))
                       state
                       (-> state
                           (update :seen conj (:name column))
                           (update :columns conj column))))
                   {:seen #{} :columns []}
                   (mapcat #(pgs/column-info schema % db)
                           (cons table-name ancestors))))]
      (into []
            (keep (fn [{:keys [name attr]}]
                    (when (and attr (not= :db/id attr))
                      (let [attr (if (get schema attr)
                                   attr
                                   (or (some #(let [candidate (keyword % name)]
                                                (when (get schema candidate) candidate))
                                             ancestors)
                                       attr))
                            entity (d/entity db attr)
                            kind (:pg/default-kind entity)]
                        {:name name :attr attr
                         :not-null? (true? (:pg/not-null entity))
                         :default (when kind
                                    [kind (:pg/default-value entity)
                                     (:pg/default-arg entity)])}))))
            ordered-columns))))

(defn constraint-metadata
  "Read per-table row-constraint inputs without parsing SQL expressions or
   evaluating defaults, predicates, or queries written in SQL. FK column JSON
   is decoded only to resolve the same physical attribute mappings as a plan.
   Retains raw definitions so changes can be compared before using a plan."
  [db table-name]
  (let [schema (:schema db)
        columns (column-specs db table-name)
        checks (when (get schema :pg/check-name)
                 (mapv (fn [[constraint expression]]
                         {:constraint constraint
                          :expression expression})
                       (d/q '{:find [?name ?expression]
                              :in [$ ?table]
                              :where [[?check :pg/check-name ?name]
                                      [?check :pg/check-table ?table]
                                      [?check :pg/check-expr ?expression]]}
                            db table-name)))
        child-by-name (into {} (map (juxt :name :attr))
                            (pgs/column-info schema table-name db))
        fks (when (get schema :pg/fk-name)
              (mapv (fn [[constraint child-json parent-table parent-json]]
                      (let [parent-cols (vec (jb/parse-jsonb parent-json))
                            parent-by-name (into {} (map (juxt :name :attr))
                                                 (pgs/column-info schema parent-table db))]
                        {:constraint constraint
                         :child-definition child-json
                         :parent-definition parent-json
                         :child-attrs (mapv child-by-name
                                            (vec (jb/parse-jsonb child-json)))
                         :parent-table parent-table
                         :parent-attrs (mapv parent-by-name parent-cols)}))
                    (d/q '{:find [?name ?child-cols ?parent-table ?parent-cols]
                           :in [$ ?table]
                           :where [[?fk :pg/fk-name ?name]
                                   [?fk :pg/fk-child-table ?table]
                                   [?fk :pg/fk-child-cols ?child-cols]
                                   [?fk :pg/fk-parent-table ?parent-table]
                                   [?fk :pg/fk-parent-cols ?parent-cols]]}
                         db table-name)))
        domain-enum
        (into {}
              (keep
               (fn [{:keys [name attr]}]
                 (let [column (d/entity db attr)
                       domain-name (:datahike.pg/domain-of column)
                       enum-name (:datahike.pg/enum-of column)]
                   (cond
                     domain-name
                     (when-let [domain-eid
                                (ffirst
                                 (d/q '{:find [?domain]
                                        :in [$ ?name]
                                        :where [[?domain :datahike.pg.domain/name ?name]]}
                                      db domain-name))]
                       (let [domain (d/entity db domain-eid)
                             expression (:datahike.pg.domain/check-expr domain)]
                         [name {:kind :domain :attr attr :domain-name domain-name
                                :check-name (:datahike.pg.domain/check-name domain)
                                :not-null? (true? (:datahike.pg.domain/not-null domain))
                                :check-expression expression}]))

                     enum-name
                     [name {:kind :enum :attr attr :enum-name enum-name
                            :unsafe-values
                            (if (get (:schema db) :datahike.pg.enum/unsafe-values)
                              (into #{} (map (comp str first))
                                    (d/q '{:find [?value]
                                           :in [$ ?name]
                                           :where [[?enum :datahike.pg.enum/name ?name]
                                                   [?enum :datahike.pg.enum/unsafe-values ?value]]}
                                         db enum-name))
                              #{})
                            :values (into #{} (map (comp str first))
                                          (d/q '{:find [?value]
                                                 :in [$ ?name]
                                                 :where [[?enum :datahike.pg.enum/name ?name]
                                                         [?enum :datahike.pg.enum/values ?value]]}
                                               db enum-name))}]
                     :else nil)))
               columns))]
    {:table table-name :columns columns
     :checks (vec checks) :fks (vec fks) :domain-enum domain-enum}))

(defn- parse-constraint-expression [expression]
  (try
    (CCJSqlParserUtil/parseCondExpression expression)
    (catch Exception _
      (CCJSqlParserUtil/parseExpression expression))))

(defn compile-constraint-metadata
  "Compile a previously read metadata snapshot. Does not reread its database
   or evaluate defaults/predicates. Check ordering follows the input snapshot."
  [{:keys [checks fks domain-enum] :as metadata}]
  (assoc metadata
         :checks (when checks
                   (mapv (fn [{:keys [constraint expression]}]
                           {:constraint constraint :ast (parse-constraint-expression expression)})
                         checks))
         :fks (when fks (mapv #(dissoc % :child-definition :parent-definition) fks))
         :domain-enum
         (into {}
               (map (fn [[name spec]]
                      [name (if (= :domain (:kind spec))
                              (-> spec
                                  (dissoc :check-expression)
                                  (assoc :check-ast
                                         (when-let [expression (:check-expression spec)]
                                           (parse-constraint-expression expression))))
                              spec)]))
               domain-enum)))

(defn constraint-plan
  "Compile per-table metadata once for a statement/tx function."
  [db table-name]
  (compile-constraint-metadata (constraint-metadata db table-name)))

(defn plan-required?
  "True when an INSERT needs the row-constraint transaction function.

   A table's ordinary column list alone is not a reason to wrap every row.
   The wrapper is needed only for defaults/NOT NULL, row constraints, or to
   erase an explicit SQL NULL before handing the entity map to Datahike."
  [plan explicit-nulls?]
  (or explicit-nulls?
      (some (fn [{:keys [not-null? default]}]
              (or not-null? default))
            (:columns plan))
      (seq (:checks plan))
      (seq (:fks plan))
      (seq (:domain-enum plan))))

(defn eval-default [kind value]
  (case kind
    :literal (try (cond
                    (nil? value) nil
                    (= "true" value) true
                    (= "false" value) false
                    (re-matches #"-?\d+" value) (Long/parseLong value)
                    (re-matches #"-?\d+\.\d+" value) (Double/parseDouble value)
                    :else value)
                  (catch Exception _ value))
    (:bit :bit-coerced) value
    :fn (case value
          "now" (java.util.Date.)
          "current_date" (java.time.LocalDate/now java.time.ZoneOffset/UTC)
          "current_time" (java.time.LocalTime/now java.time.ZoneOffset/UTC)
          "current_user" "datahike"
          nil)
    nil))

(defn prepare-candidate
  "Materialize omitted defaults and reject NULL in NOT NULL columns.
   `coerce-fn` receives [value attr]. Sequence defaults must already have
   been reserved through the live nextval service before row validation."
  [attrs plan coerce-fn]
  (reduce
   (fn [{:keys [attrs] :as result}
        {:keys [name attr not-null? default]}]
     (let [present? (contains? attrs attr)
           current (get attrs attr)
           [kind value arg] default
           resolved
           (cond
             present? current
             (= :nextval kind)
             (throw (ex-info "sequence default was not reserved before row validation"
                             {:error :unresolved-sequence-default
                              :sequence arg :column name}))
             default (eval-default kind value)
             :else nil)
           coerced (when (some? resolved) (coerce-fn resolved attr))]
       (when (and not-null? (nil? resolved))
         (throw (ex-info "not-null violation"
                         {:error :not-null-violation :sqlstate "23502"
                          :table (namespace attr) :column name})))
       (cond-> result
         (or present? default) (assoc-in [:attrs attr] coerced))))
   {:attrs attrs}
   (:columns plan)))

(defn logical-row [table-name attrs columns]
  (reduce (fn [row {:keys [name attr]}]
            (if (contains? attrs attr)
              (assoc row (keyword table-name name) (get attrs attr))
              row))
          attrs columns))

(defn validate-pre-arbiter!
  "Enforce constraints PostgreSQL checks before unique-index arbitration.
   `eval-check-fn` receives [AST logical-row table-name schema].
   `validate-domain-fn`, when supplied, receives the storage row."
  [db table-name attrs plan eval-check-fn validate-domain-fn]
  (doseq [{:keys [name attr not-null?]} (:columns plan)
          :when (and not-null? (nil? (get attrs attr)))]
    (throw (ex-info "not-null violation"
                    {:error :not-null-violation :sqlstate "23502"
                     :table table-name :column name})))
  (when (seq (:checks plan))
    (let [logical (logical-row table-name attrs (:columns plan))]
      (doseq [{:keys [constraint ast]} (:checks plan)
              :when (false? (eval-check-fn ast logical table-name (:schema db)))]
        (throw (ex-info "check constraint violation"
                        {:error :check-violation :sqlstate "23514"
                         :table table-name :constraint constraint})))))
  (doseq [[column-name spec] (:domain-enum plan)
          :let [value (get attrs (:attr spec))]]
    (cond
      (and (= :domain (:kind spec)) (:not-null? spec) (nil? value))
      (throw (ex-info "domain not-null violation"
                      {:error :not-null-violation :sqlstate "23502"
                       :table table-name :column column-name
                       :domain (:domain-name spec)}))

      (and (= :domain (:kind spec)) (:check-ast spec)
           (false? (eval-check-fn (:check-ast spec)
                                  {(keyword "" "value") value}
                                  "" (:schema db))))
      (throw (ex-info "domain check constraint violation"
                      {:error :check-violation :sqlstate "23514"
                       :table table-name :column column-name
                       :constraint (or (:check-name spec)
                                       (str (:domain-name spec) "_check"))}))

      (and (= :enum (:kind spec)) (some? value)
           (not (contains? (:values spec) (str value))))
      (throw (ex-info "invalid input value for enum"
                      {:error :invalid-text-representation :sqlstate "22P02"
                       :type (:enum-name spec) :value value
                       :table table-name :column column-name}))

      (and (= :enum (:kind spec)) (some? value)
           (contains? (:unsafe-values spec) (str value)))
      (throw (ex-info (str "unsafe use of new value " (pr-str (str value))
                           " of enum type " (:enum-name spec))
                      {:sqlstate "55P04"
                       :hint "New enum values must be committed before they can be used."
                       :type (:enum-name spec) :value value
                       :table table-name :column column-name}))))
  (when validate-domain-fn (validate-domain-fn attrs))
  nil)

(defn validate-mutation!
  "Validate an actual INSERT/UPDATE result. Unlike a speculative candidate,
   this phase includes FK enforcement and never applies INSERT defaults."
  [db table-name attrs plan effective-rows eval-check-fn validate-domain-fn]
  (validate-pre-arbiter!
   db table-name attrs plan eval-check-fn validate-domain-fn)
  (when (seq (:fks plan))
    (doseq [{:keys [constraint child-attrs parent-table parent-attrs]} (:fks plan)
            :let [child-values (mapv #(get attrs %) child-attrs)]
            :when (every? some? child-values)
            :let [parent-marker (pgs/row-marker-attr parent-table)
                  patterns (into [['?parent parent-marker true]]
                                 (map (fn [attr value] ['?parent attr value])
                                      parent-attrs child-values))
                  db-hits (mapv first
                                (d/q {:find '[?parent] :where patterns} db))
                  ;; An effective row supersedes its db value. Check every
                  ;; matching db entity: the first one may have moved away
                  ;; while another valid parent still carries the key.
                  db-hit? (some (fn [eid]
                                  (if-let [effective (get effective-rows eid)]
                                    (and (true? (get effective parent-marker))
                                         (= child-values
                                            (mapv effective parent-attrs)))
                                    true))
                                db-hits)
                  ;; Pending/effective entities count only when they are
                  ;; rows of the referenced table. An arbitrary entity can
                  ;; legally carry the same namespaced attributes but is not
                  ;; a SQL parent row without the marker.
                  pending-hit (some #(and (true? (get % parent-marker))
                                          (= child-values
                                             (mapv % parent-attrs)))
                                    (conj (vec (vals effective-rows)) attrs))]
            :when (not (or db-hit? pending-hit))]
      (throw (ex-info "foreign key violation"
                      {:error :foreign-key-violation :sqlstate "23503"
                       :table table-name :constraint constraint}))))
  nil)
