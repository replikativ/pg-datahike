(ns datahike.pg.sql.catalog-read
  "Read-only inputs for simple joins over PostgreSQL relation metadata."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.pg.catalog.foreign-keys :as foreign-keys]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.schema :as schema]
            [datahike.pg.sql.catalog :as catalog]
            [datahike.pg.sql.derived :as derived]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.params :as params])
  (:import [datahike.db DB]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.statement.select PlainSelect SelectItem Join]
           [net.sf.jsqlparser.expression ArrayConstructor CastExpression JdbcParameter JdbcNamedParameter LongValue StringValue]
           [net.sf.jsqlparser.expression.operators.conditional AndExpression]
           [net.sf.jsqlparser.expression.operators.relational EqualsTo InExpression ExpressionList]))

(set! *warn-on-reflection* true)

(def ^:dynamic *execution-inputs?*
  "Bound for a wire plan whose executor resolves catalog rows at execution."
  false)

(def ^:private tables #{"pg_class" "pg_attribute" "pg_constraint"})

(def ^:private declarations
  ;; Only built-in declarations live here. Execution always reads metadata
  ;; from its own statement snapshot.
  (delay
    (d/db-with
     (dc/empty-db {:pg/type {:db/valueType :db.type/string :db/cardinality :db.cardinality/one}}
                  {:schema-flexibility :write :keep-history? false})
     (vec (mapcat catalog/catalog-schema-for* (sort tables))))))

(def ^:private projected-fields
  #{:pg_class/db-row-exists :pg_class/oid :pg_class/relname :pg_class/relnamespace
    :pg_attribute/db-row-exists :pg_attribute/attrelid :pg_attribute/attnum :pg_attribute/attname
    :pg_constraint/db-row-exists :pg_constraint/conrelid :pg_constraint/confrelid
    :pg_constraint/contype :pg_constraint/conname :pg_constraint/confdeltype
    :pg_constraint/confupdtype :pg_constraint/conkey :pg_constraint/confkey})

(defn- simple-expression? [aliases x]
  (cond
    (instance? Column x)
    (let [^Column col x
          ac (.getArrayConstructor col)
          relation (some-> (.getTable col) .getName params/unquote-ident aliases)]
      (and relation
           (projected-fields (keyword relation (params/unquote-ident (.getColumnName col))))
           (or (nil? ac)
               (every? #(or (instance? LongValue %)
                            (instance? JdbcParameter %)
                            (instance? JdbcNamedParameter %))
                       (.getExpressions ^ArrayConstructor ac)))))
    (instance? StringValue x) true
    (instance? LongValue x) true
    (instance? CastExpression x)
    (and (= "regnamespace" (str/lower-case (str (.getColDataType ^CastExpression x))))
         (= "current_schema" (str/lower-case (str (.getLeftExpression ^CastExpression x)))))
    (instance? EqualsTo x)
    (and (simple-expression? aliases (.getLeftExpression ^EqualsTo x))
         (simple-expression? aliases (.getRightExpression ^EqualsTo x)))
    (instance? AndExpression x)
    (and (simple-expression? aliases (.getLeftExpression ^AndExpression x))
         (simple-expression? aliases (.getRightExpression ^AndExpression x)))
    (instance? InExpression x)
    (let [values (.getRightExpression ^InExpression x)]
      (and (simple-expression? aliases (.getLeftExpression ^InExpression x))
           (instance? ExpressionList values)
           (every? #(instance? StringValue %) values)))
    :else false))

(defn analysis-db
  "A bounded compiler schema for direct, read-only catalog SELECTs.
   Nested scopes, temporal views and extension callbacks use ordinary enrichment."
  [stmt db names]
  (when (and *execution-inputs?* (instance? DB db) (instance? PlainSelect stmt)
             (every? (set names) ["pg_class" "pg_constraint"]) (every? tables names)
             (catalog/statement-stable-catalogs? names)
             (not params/*nested-parse?*) (empty? params/*temp-table-map*)
             (= :write (get-in db [:config :schema-flexibility]))
             (not-any? #(contains? (:schema db) %) projected-fields)
             (derived/plain-shape? stmt))
    (let [^PlainSelect select stmt
          joins (.getJoins select)
          sources (cons (.getFromItem select) (map #(.getRightItem ^Join %) joins))
          aliases (into {}
                        (keep (fn [item]
                                (when (instance? Table item)
                                  (let [^Table t item]
                                    [(params/unquote-ident (or (some-> t .getAlias .getName) (.getName t)))
                                     (catalog/catalog-table-name t)]))))
                        sources)]
      (when (and (every? (fn [^Join j]
                           (and (derived/unmodified-join? j)
                                (or (.isInnerJoin j) (.isSimple j) (.isCross j)))) joins)
                 (every? #(and (instance? Table %) (tables (catalog/catalog-table-name %))
                               (= (catalog/catalog-table-name %) (ctx/inference-table-name %))) sources)
                 (every? #(instance? Column (.getExpression ^SelectItem %)) (.getSelectItems select))
                 (every? #(simple-expression? aliases %)
                         (concat [(.getWhere select)]
                                 (map #(.getExpression ^SelectItem %) (.getSelectItems select))
                                 (mapcat #(.getOnExpressions ^Join %) joins))))
        @declarations))))

(defn- field-use [clause]
  (let [[head attr value] clause]
    (cond
      (symbol? head) [head attr value]
      (and (seq? head) (= 'get-else (first head)))
      [(nth head 2) (nth head 3) attr])))

(defn- variable-for [uses attr]
  (some (fn [[_ a v]] (when (and (= a attr) (symbol? v)) v)) uses))

(defn- names-for [uses where]
  (or (some (fn [[_ a v]] (when (and (= :pg_class/relname a) (string? v)) #{v})) uses)
      (when-let [v (variable-for uses :pg_class/relname)]
        (some (fn [[f]]
                (when (and (seq? f) (= 'datahike.pg.query-fns/sql-in? (first f))
                           (= v (last f)) (set? (second f)) (every? string? (second f)))
                  (second f))) where))))

(defn lookup-plan
  "Follow positive inner-join equalities from one FK source to each class
   and attribute alias. Other predicates remain in the original query."
  [{:keys [where]}]
  (let [uses (keep field-use where)
        aliases (group-by first uses)
        constraints (filter #(some (fn [[_ a _]] (= a :pg_constraint/contype)) %) (vals aliases))
        fk (first constraints)
        child (variable-for fk :pg_constraint/conrelid)
        parent (variable-for fk :pg_constraint/confrelid)
        endpoints (set (remove nil? [child parent]))]
    (when (and (= 1 (count constraints)) child
               (every? (comp projected-fields second) uses)
               (some (fn [[_ a v]] (and (= a :pg_constraint/contype) (= v "f"))) fk)
               (every? (fn [fields]
                         (or (= fk fields)
                             (contains? endpoints (variable-for fields :pg_class/oid))
                             (contains? endpoints (variable-for fields :pg_attribute/attrelid))))
                       (vals aliases)))
      (some (fn [fields]
              (when (= child (variable-for fields :pg_class/oid))
                (names-for fields where)))
            (vals aliases)))))

(defn- child-storage-names [db names]
  ;; Include the physical spelling for native or pre-registry tables. The
  ;; SQL predicates will recheck logical name and namespace on every match.
  (into (set names)
        (mapcat (fn [n]
                  (keep #(get (d/entity db (:e %)) :datahike.pg.object/storage-name)
                        (d/datoms db :avet :datahike.pg.object/name n))))
        names))

(defn- visible-column? [db retired c]
  (let [ident (:datahike.pg.column/storage-ident c)]
    (or (:datahike.pg.column/dropped? c)
        (and (schema/user-column-ident? ident)
             (not (contains? retired (namespace ident)))
             (get (:schema db) ident)
             (not (:datahike.pg/hidden (d/entity db [:datahike.pg/for-ident ident])))))))

(defn- endpoint [db retired storage]
  (when-let [oid (schema/storage-table-oid db storage)]
    (let [o (objects/object-by-address db objects/pg-class-oid oid)
          namespace-oid (objects/object-namespace-oid db o)
          cols (objects/columns-by-relation db oid)]
      (when (and (not (contains? retired storage))
                 namespace-oid (string? (:datahike.pg.object/name o))
                 (= :table (:datahike.pg.object/kind o))
                 (not (:datahike.pg.object/legacy-oid? o))
                 (seq cols)
                 (every? #(visible-column? db retired %) cols))
        {:class {:pg_class/oid oid :pg_class/relname (:datahike.pg.object/name o)
                 :pg_class/relnamespace namespace-oid
                 :pg_class/db-row-exists true}
         ;; A dropped attnum has a display name in pg_attribute. Key
         ;; resolution uses live names, even if a live quoted identifier
         ;; has the exact spelling of another column's tombstone.
         :attnums (into {} (comp (remove :datahike.pg.column/dropped?)
                                 (map (juxt :datahike.pg.column/name :datahike.pg.column/attnum))) cols)
         :columns (mapv (fn [c]
                          (let [num (:datahike.pg.column/attnum c)]
                            {:pg_attribute/attrelid oid :pg_attribute/attnum num
                             :pg_attribute/attname (objects/catalog-column-name c)
                             :pg_attribute/db-row-exists true})) cols)}))))

(defn query-db
  "Build the joined relation inputs from this execution snapshot. Return nil
   for unsupported metadata so the caller can use the canonical full scan."
  [db names]
  (when (and (instance? DB db) (catalog/statement-stable-catalogs? tables))
    (let [children (child-storage-names db names)
          fks (foreign-keys/for-children db children)
          storage (into #{} (mapcat (juxt :child-table :parent-table)) fks)
          retired (schema/retired-table-names db)
          ends (into {} (map (fn [s] [s (endpoint db retired s)])) storage)]
      (when (every? some? (vals ends))
        (let [cols (mapcat :columns (vals ends))
              keys-for (fn [storage names]
                         (let [numbers (mapv (:attnums (ends storage)) (foreign-keys/column-names names))]
                           (when (every? some? numbers) numbers)))
              rows (mapv (fn [{:keys [child-table parent-table child-cols parent-cols] :as fk}]
                           (let [child (get-in ends [child-table :class :pg_class/oid])
                                 parent (get-in ends [parent-table :class :pg_class/oid])
                                 ck (keys-for child-table child-cols) pk (keys-for parent-table parent-cols)]
                             (when (and ck pk)
                               (foreign-keys/catalog-fields fk child parent ck pk)))) fks)]
          (when (every? some? rows)
            (d/db-with @declarations (into rows (concat (map :class (vals ends)) cols)))))))))
