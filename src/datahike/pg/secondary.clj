(ns datahike.pg.secondary
  "Bridge between PostgreSQL access-path planning and Datahike secondaries.

   The SQL predicate remains authoritative. This namespace only constructs
   conservative candidate queries and exposes a generic external-engine clause
   so Datahike can pass an EntityBitSet through its planner without first
   materializing a large vector of entity ids in pg-datahike."
  (:require [datahike.pg.tsearch :as tsearch]))

(defn candidates
  "Generic secondary candidate set used from generated Datalog clauses.

   The planner recognizes the metadata and calls ISecondaryIndex/-search on the
   named immutable index generation. The function body is merely the legacy
   fallback shape; pg-datahike only emits this clause when the external-engine
   planner is available."
  {:datahike/external-engine
   {:index-key 0
    :binding-columns [:entity-id]
    :accepts-entity-filter? true
    :query-spec-fn first
    :input-vars :all-bound
    :cost-model (fn [_db _idx-ident _args _n-cols]
                  {:estimated-card 100 :cost-per-result 0.02})}}
  [_idx-ident query-spec]
  query-spec)

(defn filtered-candidates
  "Secondary candidate set that must consume an upstream entity relation.

   This is a separate marker from `candidates` because accepting a bitmap and
   requiring one are different planner contracts.  Filtered ANN needs the
   latter: running before the SQL predicate would recreate PostgreSQL's
   post-filter under-fill cliff and would prevent Proximum from using its
   native filtered search."
  {:datahike/external-engine
   {:index-key 0
    :binding-columns [:entity-id]
    :accepts-entity-filter? true
    :requires-entity-filter? true
    :query-spec-fn first
    :input-vars :all-bound
    :cost-model (fn [_db _idx-ident _args _n-cols]
                  {:estimated-card 100 :cost-per-result 0.02})}}
  [_idx-ident query-spec]
  query-spec)

(defn- analyzer-safe-plan
  "Keep only PostgreSQL lexemes that StandardAnalyzer indexes byte-for-byte.

   Scriptum's generic Datahike adapter indexes string values as analyzed text.
   A lower-case ASCII alphanumeric PostgreSQL lexeme is one exact Lucene term.
   Any other term becomes :all; boolean simplification retains safe conjuncts
   where possible. This can reduce selectivity, but never introduce a false
   negative before PostgreSQL's authoritative @@ recheck."
  [plan]
  (cond
    (#{:all :none} plan) plan

    (contains? #{:term :prefix} (:op plan))
    (if (re-matches #"[a-z0-9]+" (:lexeme plan)) plan :all)

    (contains? #{:and :or} (:op plan))
    (let [[left right] (mapv analyzer-safe-plan (:args plan))]
      (case (:op plan)
        :and (cond
               (= :none left) :none
               (= :none right) :none
               (= :all left) right
               (= :all right) left
               :else {:op :and :args [left right]})
        :or (cond
              (= :all left) :all
              (= :all right) :all
              (= :none left) right
              (= :none right) left
              :else {:op :or :args [left right]})))

    :else :all))

(defn- scriptum-builders []
  (try
    (require 'scriptum.core)
    {:term (requiring-resolve 'scriptum.core/term-query)
     :prefix (requiring-resolve 'scriptum.core/prefix-query)
     :bool (requiring-resolve 'scriptum.core/bool-query)}
    (catch Throwable _ nil)))

(defn- compile-scriptum-query [builders plan]
  (cond
    (= :all plan) :all
    (= :none plan) ::none
    (= :term (:op plan)) ((:term builders) :value (:lexeme plan))
    (= :prefix (:op plan)) ((:prefix builders) :value (:lexeme plan))
    (contains? #{:and :or} (:op plan))
    (let [occur (if (= :and (:op plan)) :must :should)]
      ((:bool builders)
       (mapv (fn [arg] [(compile-scriptum-query builders arg) occur])
             (:args plan))))
    :else ::none))

(defn scriptum-query-spec
  "Compile a PostgreSQL tsquery into a complete Scriptum candidate query.

   Returns nil when Scriptum's analyzer-free builders are unavailable. The
   caller must then keep the exact primary scan. The returned declarations are
   deliberately PostgreSQL-facing: Lucene candidates always require @@
   recheck, have complete recall, and carry no SQL-visible ordering."
  [query]
  (when-let [builders (scriptum-builders)]
    (let [{:keys [query query-id]} (tsearch/tsquery-candidate-plan query)
          plan (analyzer-safe-plan query)
          lucene-query (compile-scriptum-query builders plan)]
      (when-not (= ::none lucene-query)
        {:query lucene-query
         :query-id query-id
         :precision :recheck
         :recall :complete
         :ordering :none}))))
