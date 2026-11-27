(ns datahike.pg.sql.index-predicate
  "Bind ordinary B-tree index predicates to the indexed table's columns."
  (:require [clojure.string :as str]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.catalog.regoid :as regoid]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
  (:import [datahike.pg ColumnSubstitutingDeParser]
           [java.util.function Function]
           [net.sf.jsqlparser.expression BinaryExpression BooleanValue DoubleValue
            LongValue NotExpression NullValue SignedExpression StringValue]
           [net.sf.jsqlparser.expression.operators.conditional AndExpression OrExpression]
           [net.sf.jsqlparser.expression.operators.relational
            ComparisonOperator IsBooleanExpression IsNullExpression ParenthesedExpressionList]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.schema Column]))

(set! *warn-on-reflection* true)

(defn- keyword-token? [token word]
  (and (= :ident (:type token)) (= word (str/lower-case (:text token)))))

(defn split-predicate
  "Parse the index declaration and its WHERE expression independently.
   JSqlParser's CREATE INDEX grammar cannot parse PostgreSQL predicates."
  [^String sql]
  (let [[_ second-token & tail] (drop-while #(= ";" (:text %)) (cls/tokenize sql))
        tokens (if (keyword-token? second-token "unique") (next tail) tail)]
    (loop [remaining tokens depth 0]
      (when-let [{:keys [text pos end] :as token} (first remaining)]
        (if (and (zero? depth) (keyword-token? token "where"))
          (let [last-token (last (remove #(= ";" (:text %)) (next remaining)))]
            {:head (subs sql 0 pos)
             :concurrently? (keyword-token? (first tokens) "concurrently")
             :predicate (if last-token (str/trim (subs sql end (:end last-token))) "")})
          (recur (next remaining) (case text "(" (inc depth) ")" (dec depth) depth)))))))

(defn- syntax [message]
  (ex-info message {:sqlstate "42601"}))

(defn- unsupported []
  (errors/pg-error :feature-not-supported
                   {:message "partial index predicates support simple column tests and comparisons"}))

(defn- parse-predicate [sql]
  (when (str/blank? sql)
    (throw (syntax "missing index predicate after WHERE")))
  (try
    (CCJSqlParserUtil/parseCondExpression sql false)
    (catch net.sf.jsqlparser.JSQLParserException _
      (throw (syntax "invalid index predicate")))))

(def ^:private numeric-types #{:integer :float :numeric})
(def ^:private comparison-types #{:text :boolean :integer :float :numeric})
(def ^:private comparison-operators #{"=" "<>" "!=" "<" "<=" ">" ">="})

(defn- require-boolean! [type]
  (when-not (#{:boolean :null} type)
    (throw (ex-info "argument of WHERE must be type boolean" {:sqlstate "42804"}))))

(declare expression-type)

(defn- binary-type [lookup ^BinaryExpression expression]
  (let [left (expression-type lookup (.getLeftExpression expression))
        right (expression-type lookup (.getRightExpression expression))]
    (if (or (instance? AndExpression expression) (instance? OrExpression expression))
      (do (require-boolean! left) (require-boolean! right))
      (when-not (or (and (= left right) (comparison-types left))
                    (and (numeric-types left) (numeric-types right))
                    (and (#{:null} left) (or (comparison-types right) (= :null right)))
                    (and (= :null right) (comparison-types left)))
        (throw (unsupported))))
    :boolean))

(defn- expression-type [lookup expression]
  (cond
    (instance? Column expression)
    (let [column (lookup expression)
          oid (:datahike.pg.column/type-oid column)]
      (types/cast-category (types/format-type oid -1)))

    (instance? ParenthesedExpressionList expression)
    (if (= 1 (count expression))
      (expression-type lookup (first expression))
      (throw (unsupported)))

    (instance? IsNullExpression expression)
    (do (expression-type lookup (.getLeftExpression ^IsNullExpression expression)) :boolean)

    (instance? IsBooleanExpression expression)
    (do (require-boolean! (expression-type lookup (.getLeftExpression ^IsBooleanExpression expression)))
        :boolean)

    (instance? NotExpression expression)
    (do (require-boolean! (expression-type lookup (.getExpression ^NotExpression expression)))
        :boolean)

    (or (instance? AndExpression expression)
        (instance? OrExpression expression)
        (and (instance? ComparisonOperator expression)
             (comparison-operators (.getStringExpression ^ComparisonOperator expression))))
    (binary-type lookup expression)

    (instance? SignedExpression expression)
    (let [operand (.getExpression ^SignedExpression expression)]
      (if (or (instance? LongValue operand) (instance? DoubleValue operand))
        (expression-type lookup operand)
        (throw (unsupported))))

    (instance? LongValue expression) :integer
    (instance? DoubleValue expression) :numeric
    (instance? BooleanValue expression) :boolean
    (instance? StringValue expression) :text
    (instance? NullValue expression) :null
    :else (throw (unsupported))))

(defn bind-predicate [db relation-oid sql]
  (let [ast (parse-predicate sql)
        relation (objects/object-by-address db objects/pg-class-oid relation-oid)
        table (:datahike.pg.object/name relation)
        namespace (objects/namespace-by-oid db (objects/object-namespace-oid db relation))
        schema (:datahike.pg.object/name namespace)
        dependencies (volatile! #{})
        lookup (fn [^Column column]
                 (when (.getArrayConstructor column)
                   (throw (unsupported)))
                 (let [name (params/unquote-ident (.getColumnName column))
                       qualifier (some-> (.getTable column) .getFullyQualifiedName not-empty)
                       found (when (or (nil? qualifier)
                                       (contains? #{[table] [schema table]}
                                                  (regoid/parse-qualified-name qualifier)))
                               (objects/column-by-name db relation-oid name))]
                   (when-not found
                     (throw (errors/pg-error :undefined-column {:table table :column (str column)})))
                   (vswap! dependencies conj (:datahike.pg.column/attnum found))
                   found))]
    (require-boolean! (expression-type lookup ast))
    (let [rendered (ColumnSubstitutingDeParser/deparse
                    ast (reify Function
                          (apply [_ column]
                            (fns/quote-identifier (:datahike.pg.column/name (lookup column))))))]
      {:sql (if (or (instance? Column ast) (instance? BooleanValue ast)
                    (instance? ParenthesedExpressionList ast))
              rendered
              (str "(" rendered ")"))
       :columns @dependencies})))

(defn rename-column [{:keys [sql] :as predicate} old-name new-name]
  (assoc predicate :sql (ColumnSubstitutingDeParser/deparse
                         (parse-predicate sql)
                         (reify Function
                           (apply [_ column]
                             (let [name (params/unquote-ident (.getColumnName ^Column column))]
                               (fns/quote-identifier (if (= name old-name) new-name name))))))))
