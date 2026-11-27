(ns datahike.pg.sql.index-predicate
  "Bind ordinary B-tree key expressions and predicates to the indexed table."
  (:require [clojure.string :as str]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.catalog.regoid :as regoid]
            [datahike.pg.errors :as errors]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.fns :as fns]
            [datahike.pg.sql.oid-infer :as oid-infer]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
  (:import [datahike.pg ColumnSubstitutingDeParser]
           [java.util.function Function]
           [net.sf.jsqlparser.expression BinaryExpression BooleanValue DoubleValue
            LongValue NotExpression NullValue SignedExpression StringValue]
           [net.sf.jsqlparser.expression.operators.arithmetic Addition Concat Multiplication Subtraction]
           [net.sf.jsqlparser.expression.operators.conditional AndExpression OrExpression]
           [net.sf.jsqlparser.expression.operators.relational
            ComparisonOperator IsBooleanExpression IsNullExpression ParenthesedExpressionList]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.schema Column]))

(set! *warn-on-reflection* true)

(defn- keyword-token? [token word]
  (and (= :ident (:type token)) (= word (str/lower-case (:text token)))))

(defn- index-options [tokens]
  (let [[_ second-token & tail] (drop-while #(= ";" (:text %)) tokens)]
    (if (keyword-token? second-token "unique") (next tail) tail)))

(defn- split-predicate [^String sql tokens]
  (loop [remaining tokens depth 0]
    (when-let [{:keys [text pos end] :as token} (first remaining)]
      (if (and (zero? depth) (keyword-token? token "where"))
        (let [last-token (last (remove #(= ";" (:text %)) (next remaining)))]
          {:head (subs sql 0 pos)
           :predicate (if last-token (str/trim (subs sql end (:end last-token))) "")})
        (recur (next remaining) (case text "(" (inc depth) ")" (dec depth) depth))))))

(defn- expression-key [sql position tokens]
  (when-let [[_ tail] (cls/read-balanced tokens)]
    (when (seq tail)
      (throw (errors/pg-error :feature-not-supported
                              {:message "expression-key index options are not supported"})))
    {:position position
     :start (:pos (first tokens))
     :end (:end (peek tokens))
     :sql (subs sql (:end (first tokens)) (:pos (peek tokens)))}))

(defn split-declaration
  "Let JSqlParser parse the index header with placeholder column keys.
   Retain each parenthesized expression by its position in the key list."
  [^String sql]
  (let [tokens (index-options (cls/tokenize sql))
        predicate (split-predicate sql tokens)
        head (or (:head predicate) sql)
        key-list (->> tokens
                      (take-while #(< (:pos %) (count head)))
                      (drop-while #(not= "(" (:text %))))
        [inside] (cls/read-balanced key-list)
        expressions (into [] (keep-indexed (partial expression-key head))
                          (cls/split-top-level-commas inside))]
    (cond
      (seq expressions)
      (assoc predicate
             :head (reduce (fn [head {:keys [position start end]}]
                             (str (subs head 0 start)
                                  "__pg_index_expr_" position " " (subs head end)))
                           head (rseq expressions))
             :expression-keys (into {} (map (juxt :position :sql)) expressions)
             :concurrently? (keyword-token? (first tokens) "concurrently"))
      predicate (assoc predicate :concurrently? (keyword-token? (first tokens) "concurrently")))))

(defn- syntax [message]
  (ex-info message {:sqlstate "42601"}))

(defn- unsupported []
  (errors/pg-error :feature-not-supported
                   {:message "index expressions support simple scalar tests, comparisons and arithmetic"}))

(defn- parse-index-expression [sql]
  (when (str/blank? sql)
    (throw (syntax "missing index expression")))
  (try
    (CCJSqlParserUtil/parseCondExpression sql false)
    (catch net.sf.jsqlparser.JSQLParserException _
      (throw (syntax "invalid index expression")))))

(def ^:private numeric-types #{:integer :float :numeric})
(def ^:private comparison-types #{:text :boolean :integer :float :numeric})
(def ^:private comparison-operators #{"=" "<>" "!=" "<" "<=" ">" ">="})

(defn- require-boolean! [type]
  (when-not (#{:boolean :null} type)
    (throw (ex-info "argument of WHERE must be type boolean" {:sqlstate "42804"}))))

(declare expression-type)

(defn- arithmetic? [expression]
  (or (instance? Addition expression)
      (instance? Subtraction expression)
      (instance? Multiplication expression)))

(defn- binary-type [lookup ^BinaryExpression expression]
  (let [left (expression-type lookup (.getLeftExpression expression))
        right (expression-type lookup (.getRightExpression expression))]
    (cond
      (or (instance? AndExpression expression) (instance? OrExpression expression))
      (do (require-boolean! left) (require-boolean! right) :boolean)

      (arithmetic? expression)
      (do
        (when-not (and (numeric-types left) (numeric-types right))
          (throw (unsupported)))
        (cond
          (or (= left :float) (= right :float)) :float
          (or (= left :numeric) (= right :numeric)) :numeric
          :else :integer))

      (instance? Concat expression)
      (if (= left right :text) :text (throw (unsupported)))

      :else
      (if (or (and (= left right) (comparison-types left))
              (and (numeric-types left) (numeric-types right))
              (and (= :null left) (or (comparison-types right) (= :null right)))
              (and (= :null right) (comparison-types left)))
        :boolean
        (throw (unsupported))))))

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
        (arithmetic? expression)
        (instance? Concat expression)
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

(defn- unwrap-key [ast]
  (if (and (instance? ParenthesedExpressionList ast) (= 1 (count ast)))
    (recur (first ast))
    ast))

(defn- bind-expression [db relation-oid sql predicate?]
  (let [ast (cond-> (parse-index-expression sql) (not predicate?) unwrap-key)
        relation (objects/object-by-address db objects/pg-class-oid relation-oid)
        table (:datahike.pg.object/name relation)
        storage-table (or (:datahike.pg.object/storage-name relation) table)
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
                   found))
        type (expression-type lookup ast)
        _ (when predicate? (require-boolean! type))
        rendered (ColumnSubstitutingDeParser/deparse
                  ast (reify Function
                        (apply [_ column]
                          (fns/quote-identifier (:datahike.pg.column/name (lookup column))))))]
    (if (and (not predicate?) (instance? Column ast))
      {:column (:datahike.pg.column/name (lookup ast))}
      (cond-> {:sql (if (or (instance? Column ast) (instance? BooleanValue ast)
                            (instance? ParenthesedExpressionList ast)
                            (instance? LongValue ast) (instance? DoubleValue ast)
                            (instance? SignedExpression ast) (instance? StringValue ast))
                      rendered
                      (str "(" rendered ")"))
               :columns @dependencies}
        (not predicate?)
        (assoc :type-oid
               (or (and (not= :null type)
                        (oid-infer/expr-oid
                         ast {:db db :schema (:schema db)
                              :hints (pgs/schema-hints db)
                              :default-table storage-table :table-aliases {table storage-table}}))
                   (throw (unsupported))))))))

(defn bind-predicate [db relation-oid sql]
  (bind-expression db relation-oid sql true))

(defn bind-key [db relation-oid sql]
  (bind-expression db relation-oid sql false))

(defn rename-column [{:keys [sql] :as predicate} old-name new-name]
  (assoc predicate :sql (ColumnSubstitutingDeParser/deparse
                         (parse-index-expression sql)
                         (reify Function
                           (apply [_ column]
                             (let [name (params/unquote-ident (.getColumnName ^Column column))]
                               (fns/quote-identifier (if (= name old-name) new-name name))))))))
