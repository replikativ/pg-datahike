(ns datahike.pg.sql.catalog-relation-read
  "Bounded pg_class reads for positive relation-name and relation-kind probes."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.schema :as schema]
            [datahike.pg.sql.catalog :as catalog]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.derived :as derived]
            [datahike.pg.sql.params :as params])
  (:import [net.sf.jsqlparser.expression CastExpression StringValue]
           [net.sf.jsqlparser.expression.operators.conditional AndExpression]
           [net.sf.jsqlparser.expression.operators.relational EqualsTo ExpressionList InExpression]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.statement.select PlainSelect SelectItem]))

(set! *warn-on-reflection* true)

(def ^:private fields #{"relname" "relnamespace" "relkind" "oid"})

(defn- field [alias x]
  (when (instance? Column x)
    (let [^Column c x
          owner (some-> (.getTable c) .getName params/unquote-ident)]
      (when (and (or (nil? owner) (= alias owner)) (nil? (.getArrayConstructor c)))
        (fields (params/unquote-ident (.getColumnName c)))))))

(defn- terms [x]
  (if (instance? AndExpression x)
    (into (terms (.getLeftExpression ^AndExpression x))
          (terms (.getRightExpression ^AndExpression x)))
    [x]))

(defn- predicate [alias x]
  (cond
    (instance? InExpression x)
    (let [^InExpression x x
          f (field alias (.getLeftExpression x))
          values (.getRightExpression x)]
      (when (and (#{"relname" "relkind"} f) (not (.isNot x))
                 (instance? ExpressionList values) (seq values)
                 (every? #(and (instance? StringValue %) (nil? (.getPrefix ^StringValue %))) values))
        [f (set (map #(.getNotExcapedValue ^StringValue %) values))]))

    (instance? EqualsTo x)
    (let [^EqualsTo x x
          rhs (.getRightExpression x)]
      (when (and (= "relnamespace" (field alias (.getLeftExpression x)))
                 (instance? CastExpression rhs)
                 (= "regnamespace" (str/lower-case (str (.getColDataType ^CastExpression rhs))))
                 (= "current_schema" (str/lower-case (str (.getLeftExpression ^CastExpression rhs)))))
        ["relnamespace" true]))))

(defn plan [stmt]
  (when (and (instance? PlainSelect stmt) (derived/plain-shape? stmt))
    (let [^PlainSelect s stmt
          source (.getFromItem s)]
      (when (and (empty? (.getJoins s)) (instance? Table source)
                 (= "pg_class" (catalog/catalog-table-name source) (ctx/inference-table-name source)))
        (let [^Table t source
              alias (params/unquote-ident (or (some-> t .getAlias .getName) (.getName t)))
              terms (terms (.getWhere s))
              filters (into {} (keep #(predicate alias %)) terms)]
          (when (and (= (count terms) (count filters))
                     (contains? filters "relname") (contains? filters "relkind")
                     (every? #{"r" "v" "m" "p"} (filters "relkind"))
                     (every? #(<= (alength (.getBytes ^String % java.nio.charset.StandardCharsets/UTF_8))
                                  params/max-identifier-bytes)
                             (filters "relname"))
                     (every? #(field alias (.getExpression ^SelectItem %)) (.getSelectItems s)))
            {:relation-names (filters "relname")}))))))

(defn- named-entities [db attr value]
  (when (contains? (:schema db) attr)
    (map #(d/entity db (:e %)) (d/datoms db :avet attr value))))

(defn storage-candidates
  "Physical namespaces which can publish the requested logical names."
  [db names]
  (into (set names)
        (mapcat (fn [n]
                  (keep :datahike.pg.object/storage-name
                        (named-entities db :datahike.pg.object/name n))))
        names))

(defn rows
  "Produce the pg_class fields admitted by plan from the execution snapshot.
   The original query rechecks the name, kind and namespace predicates."
  [db {:keys [relation-names]}]
  (let [candidates (storage-candidates db relation-names)
        retired (schema/retired-table-names db)
        ;; Native tables have no row marker; their schema namespace still
        ;; establishes membership, as it does in schema/table-names.
        native (into #{} (comp (filter schema/user-column-ident?) (map namespace)
                               (remove #(contains? (:schema db) (schema/row-marker-attr %))))
                     (keys (:schema db)))
        tables (for [t (into candidates native)
                     :when (and (not (contains? retired t))
                                (or (native t) (contains? (:schema db) (schema/row-marker-attr t))))
                     :let [row (catalog/table-class-identity db t)]
                     :when (relation-names (:pg_class/relname row))
                     :let [object (objects/object-by-address db objects/pg-class-oid (:pg_class/oid row))
                           parent? (when object
                                     (:datahike.pg.partition/definition (d/entity db (:db/id object))))
                           matview? (seq (named-entities db :datahike.pg.matview/name t))]]
                 (assoc row :pg_class/relkind (catalog/table-relkind parent? matview?)))
        views (for [n relation-names
                    e (named-entities db :datahike.pg/view-name n)
                    :when (:datahike.pg/view-definition e)
                    :let [o (objects/object-by-identity db objects/pg-class-oid
                                                        objects/public-namespace-oid n)]]
                {:pg_class/relname n :pg_class/relnamespace objects/public-namespace-oid
                 :pg_class/oid (long (or (:datahike.pg.object/oid o) (Math/abs (.hashCode ^String n))))
                 :pg_class/relkind "v" :pg_class/db-row-exists true})
        catalogs (for [n relation-names :let [oid (objects/catalog-relation-oids n)] :when oid]
                   {:pg_class/relname n :pg_class/relnamespace 11 :pg_class/oid oid
                    :pg_class/relkind "r" :pg_class/db-row-exists true})]
    (into [] cat [tables views catalogs])))
