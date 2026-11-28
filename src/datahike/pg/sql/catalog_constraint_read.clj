(ns datahike.pg.sql.catalog-constraint-read
  "Relation-scoped inputs for introspection of a named table constraint."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.schema :as schema]
            [datahike.pg.sql.catalog :as catalog]
            [datahike.pg.sql.catalog-relation-read :as relations]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.params :as params])
  (:import [net.sf.jsqlparser.expression CastExpression Function StringValue]
           [net.sf.jsqlparser.expression.operators.conditional AndExpression]
           [net.sf.jsqlparser.expression.operators.relational EqualsTo]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.statement.select Join PlainSelect SelectItem]))

(set! *warn-on-reflection* true)

(def ^:private tables #{"pg_constraint" "pg_class" "pg_description"})

(def ^:private declarations
  (delay
    (d/db-with
     (dc/empty-db {:pg/type {:db/valueType :db.type/string :db/cardinality :db.cardinality/one}}
                  {:schema-flexibility :write :keep-history? false})
     (vec (mapcat catalog/catalog-schema-for* (sort tables))))))

(defn analysis-db [] @declarations)

(defn- field [aliases expr]
  (when (instance? Column expr)
    (let [^Column c expr
          owner (some-> (.getTable c) .getName params/unquote-ident aliases)
          name (params/unquote-ident (.getColumnName c))]
      (when (nil? (.getArrayConstructor c))
        (if owner
          (keyword owner name)
          (when (and (nil? (.getTable c)) (= "conname" name)) :pg_constraint/conname))))))

(defn- conjunction [x]
  (if (instance? AndExpression x)
    (into (conjunction (.getLeftExpression ^AndExpression x))
          (conjunction (.getRightExpression ^AndExpression x)))
    [x]))

(defn- filter-term [aliases x]
  (when (instance? EqualsTo x)
    (let [attr (field aliases (.getLeftExpression ^EqualsTo x))
          value (.getRightExpression ^EqualsTo x)]
      (cond
        (and (#{:pg_constraint/conname :pg_class/relname} attr) (instance? StringValue value)
             (nil? (.getPrefix ^StringValue value)))
        [attr (.getNotExcapedValue ^StringValue value)]

        (and (= :pg_class/relnamespace attr) (instance? CastExpression value)
             (= "regnamespace" (str/lower-case (str (.getColDataType ^CastExpression value))))
             (= "current_schema" (str/lower-case (str (.getLeftExpression ^CastExpression value)))))
        [attr true]))))

(defn- eq-join? [aliases ^Join join expected]
  (let [[on :as terms] (vec (.getOnExpressions join))]
    (and (= 1 (count terms)) (instance? EqualsTo on)
         (= expected (set (map #(field aliases %)
                               [(.getLeftExpression ^EqualsTo on) (.getRightExpression ^EqualsTo on)])))
         (empty? (.getUsingColumns join)) (not (.isNatural join)))))

(defn- call-args [expr name]
  (when (and (instance? Function expr)
             (= name (str/lower-case (.getName ^Function expr)))
             ;; Reject DISTINCT, named arguments, filter/order modifiers and
             ;; other expressions hidden outside the ordinary argument list.
             (= (str expr)
                (str (.getName ^Function expr) "(" (.getParameters ^Function expr) ")")))
    (vec (.getParameters ^Function expr))))

(defn plan [^PlainSelect stmt _db names]
  (when (= tables (set names))
    (let [[^Join inner ^Join outer :as joins] (.getJoins stmt)
          sources (cons (.getFromItem stmt) (map #(.getRightItem ^Join %) joins))]
      (when (and (= 2 (count joins)) (.isInnerJoin inner) (.isLeft outer)
                 (every? #(and (instance? Table %) (= (catalog/catalog-table-name %)
                                                      (ctx/inference-table-name %))) sources)
                 (= ["pg_constraint" "pg_class" "pg_description"] (map catalog/catalog-table-name sources)))
        (let [aliases (into {} (map (fn [^Table t]
                                      [(params/unquote-ident (or (some-> t .getAlias .getName) (.getName t)))
                                       (catalog/catalog-table-name t)])) sources)
              terms (conjunction (.getWhere stmt))
              preds (into {} (keep #(filter-term aliases %)) terms)
              items (.getSelectItems stmt)
              [descr definition :as args] (call-args (.getExpression ^SelectItem (first items)) "coalesce")
              oid (call-args definition "pg_get_constraintdef")]
          (when (and (= 3 (count aliases)) (= 1 (count items))
                     (= 2 (count args)) (= :pg_description/description (field aliases descr))
                     (= 1 (count oid)) (= :pg_constraint/oid (field aliases (first oid)))
                     (eq-join? aliases inner #{:pg_class/oid :pg_constraint/conrelid})
                     (eq-join? aliases outer #{:pg_constraint/oid :pg_description/objoid})
                     (= (count terms) (count preds)) (:pg_class/relname preds) (:pg_constraint/conname preds))
            {:relation-name (:pg_class/relname preds) :constraint-name (:pg_constraint/conname preds)}))))))

(defn query-db [db {:keys [relation-name constraint-name]}]
  (let [classes (relations/rows db {:relation-names #{relation-name}})
        storage (mapv (fn [r]
                        (let [o (objects/object-by-address db objects/pg-class-oid (:pg_class/oid r))]
                          (when (and (= :table (:datahike.pg.object/kind o))
                                     (not (:datahike.pg.object/legacy-oid? o)))
                            (or (:datahike.pg.object/storage-name o) (:datahike.pg.object/name o))))) classes)]
    ;; Tables without a durable identity use the general catalog scan.
    (when (and (seq storage) (every? string? storage))
      (binding [schema/*catalog-tx-cache* (schema/make-catalog-tx-cache)]
        (let [rows (catalog/constraint-rows db (set storage))
              matches (filter #(= constraint-name (:pg_constraint/conname %)) rows)
              oids (into #{} (map :pg_constraint/oid) matches)
              descriptions (filter #(oids (:pg_description/objoid %))
                                   (catalog/catalog-data-for "pg_description" (:schema db) db))]
          (d/db-with (analysis-db) (into classes (concat matches descriptions))))))))
