(ns datahike.pg.sql.analysis
  "Metadata for virtual relations whose rows must not run during EXPLAIN."
  (:require [clojure.string :as str]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.cast :as sql-cast]
            [datahike.pg.sql.oid-infer :as oid]
            [datahike.pg.types :as types])
  (:import [net.sf.jsqlparser.expression Parenthesis CastExpression]
           [net.sf.jsqlparser.schema Column]
           [net.sf.jsqlparser.expression.operators.relational ExpressionList ParenthesedExpressionList]
           [net.sf.jsqlparser.statement.select Values TableFunction]))

(defn- unsupported! [feature]
  (binding [params/*analysis-only?* true]
    (params/require-execution! feature)))

(defn- unwrap [expression]
  (cond
    (instance? Parenthesis expression) (recur (.getExpression ^Parenthesis expression))
    (and (instance? ExpressionList expression) (= 1 (count expression)))
    (recur (first expression))
    :else expression))

(defn expression-typmod
  [expression output-oid schema db context]
  (let [expression (unwrap expression)]
    (cond
      (instance? Column expression)
      (when context
        (let [resolved (ctx/resolve-column expression (:table-aliases context)
                                           (:default-table context) (:col-overrides context)
                                           (:derived-aliases context) (:ci-index context))
              attr (if (keyword? resolved) resolved
                       (when (= :aliased (first resolved)) (nth resolved 2)))]
          (when attr (or (get-in schema [attr :pg/typmod])
                         (params/pg-typmod-of-attr db attr)))))

      (instance? CastExpression expression)
      (let [datatype (.getColDataType ^CastExpression expression)
            arguments (vec (or (.getArgumentsStringList datatype) []))
            base-oid (get types/array-oid->element-oid output-oid output-oid)
          ;; JSQLParser sometimes embeds the modifier in getDataType and
          ;; leaves getArgumentsStringList empty (notably PostgreSQL :: casts).
            length (some-> (or (first arguments)
                               (second (re-find #"\(\s*(\d+)" (str datatype))))
                           str parse-long)
            type-str (str/replace (str datatype) #"\[\]" "")
            type-name (types/base-type-name-of type-str)]
        (cond
          (= types/oid-numeric base-oid)
          (when-let [[precision scale] (sql-cast/numeric-typmod (str datatype))]
            (types/encode-numeric-typmod precision scale))
          (contains? #{types/oid-varchar types/oid-bpchar} base-oid)
          (when-let [length (or (types/parse-char-length type-str)
                                (when (contains? #{"char" "character"} type-name) 1))]
            (+ 4 length))
          (contains? #{types/oid-bit types/oid-varbit} base-oid)
          (or (types/parse-bit-length type-str) (when (= "bit" type-name) 1))
          (contains? #{types/oid-time types/oid-timetz types/oid-timestamp types/oid-timestamptz} base-oid)
          length))

      :else nil)))

(defn normalize-expression
  "Remove syntactic parentheses without changing an expression's identity."
  [expression]
  (unwrap expression))

(defn expression-output
  "Analyze an expression through the ordinary SQL translator, never its runner.
   UNKNOWN is a syntactic property: constant plans can omit resolution OIDs."
  [expression schema db]
  (when-not params/*parse-sql* (unsupported! "expression metadata without a SQL analyzer"))
  (let [plan (binding [params/*analysis-only?* true
                       params/*bound-params* nil]
               (params/*parse-sql* (str "SELECT (" expression ")") schema db))
        _ (when (= :error (:type plan))
            (throw (ex-info (:message plan)
                            {:sqlstate (:sqlstate plan) :error-fields (:error-fields plan)})))
        column (first (:output-columns plan))
        output-oid (or (:oid column)
                       (first (get-in plan [:visible-select-items :oids]))
                       (first (:select-item-oids plan))
                       (first (get-in plan [:metadata :oids])))
        unknown? (oid/untyped-literal? (unwrap expression))]
    (when (and (not unknown?) (not (and (integer? output-oid) (pos? output-oid))))
      (unsupported! "an unresolved expression output type"))
    (cond-> (assoc (or column {}) :oid (when-not unknown? output-oid) :unknown? unknown?
                   :typmod (or (:typmod column)
                               (expression-typmod (unwrap expression) output-oid schema db nil)))
      (nil? (:name column)) (assoc :name "?column?"))))

(defn values-output
  "Resolve VALUES columns across every expression row without evaluating cells."
  [^Values values schema db]
  (let [raw (vec (.getExpressions values))
        rows (if (and (seq raw) (instance? ParenthesedExpressionList (first raw)))
               (mapv vec raw) [raw])
        width (count (first rows))]
    (when (some #(not= width (count %)) rows)
      (throw (ex-info "VALUES lists must all be the same length" {:sqlstate "42601"})))
    (let [outputs (mapv (fn [row] (mapv #(expression-output % schema db) row)) rows)]
      (mapv (fn [i]
              (let [columns (mapv #(nth % i) outputs)
                    common (types/select-common-type (mapv :oid columns) "VALUES")
                    modifier (:typmod (first columns))]
                (cond-> {:name (str "column" (inc i)) :oid common}
                  (and (some? modifier)
                       (every? #(and (= common (:oid %)) (= modifier (:typmod %))) columns))
                  (assoc :typmod modifier))))
            (range width)))))

(def ^:private fixed-table-functions
  {"pg_input_error_info" {:names ["message" "detail" "hint" "sql_error_code"] :oids [25 25 25 25] :arity #{2}}
   "pg_get_keywords" {:names ["word" "catcode" "barelabel" "catdesc" "baredesc"] :oids [25 18 16 25 25] :arity #{0}}
   "jsonb_array_elements" {:names ["value"] :oids [3802] :arity #{1}}
   "json_array_elements" {:names ["value"] :oids [114] :arity #{1}}
   "jsonb_array_elements_text" {:names ["value"] :oids [25] :arity #{1}}
   "json_array_elements_text" {:names ["value"] :oids [25] :arity #{1}}
   "jsonb_each" {:names ["key" "value"] :oids [25 3802] :arity #{1}}
   "json_each" {:names ["key" "value"] :oids [25 114] :arity #{1}}
   "jsonb_each_text" {:names ["key" "value"] :oids [25 25] :arity #{1}}
   "json_each_text" {:names ["key" "value"] :oids [25 25] :arity #{1}}
   "jsonb_object_keys" {:names ["jsonb_object_keys"] :oids [25] :arity #{1}}
   "json_object_keys" {:names ["json_object_keys"] :oids [25] :arity #{1}}
   "regexp_split_to_table" {:names ["regexp_split_to_table"] :oids [25] :arity #{2 3}}
   "string_to_table" {:names ["string_to_table"] :oids [25] :arity #{2 3}}
   "now" {:names ["now"] :oids [1184] :arity #{0}}
   "current_timestamp" {:names ["current_timestamp"] :oids [1184] :arity #{0}}
   "transaction_timestamp" {:names ["transaction_timestamp"] :oids [1184] :arity #{0}}
   "statement_timestamp" {:names ["statement_timestamp"] :oids [1184] :arity #{0}}
   "clock_timestamp" {:names ["clock_timestamp"] :oids [1184] :arity #{0}}})

(defn- no-overload! [name]
  (throw (ex-info (str "function " name " does not exist for these argument types")
                  {:sqlstate "42883" :error :undefined-function})))

(defn- series-oid [oids]
  (let [n (count oids)
        time? (or (some #{1082 1114 1184} (take 2 oids)) (= 1186 (nth oids 2 nil)))]
    (cond
      (not (contains? #{2 3} n)) (no-overload! "generate_series")
      (every? nil? oids)
      (throw (ex-info "function generate_series is not unique" {:sqlstate "42725"}))
      time?
      (if (and (= n 3) (contains? #{1186 nil} (nth oids 2))
               (every? #(contains? #{1082 1114 1184 nil} %) (take 2 oids)))
        (if (and (some #{1114} (take 2 oids))
                 (every? #(contains? #{1114 nil} %) (take 2 oids))) 1114 1184)
        (no-overload! "generate_series"))
      :else
      (let [common (try (types/select-common-type oids "generate_series" false)
                        (catch clojure.lang.ExceptionInfo _ nil))]
        (cond
          (= 21 common)
          (throw (ex-info "function generate_series is not unique" {:sqlstate "42725"}))
          (contains? #{23 20 1700} common) common
          :else (no-overload! "generate_series"))))))

(defn table-function-shape
  "Declared FROM-function columns without invoking its argument evaluator.
   Functions whose row layout cannot be established here fail before execution."
  [^TableFunction table-function function-name coldefs schema db]
  (let [function (.getFunction table-function)
        arguments (vec (or (.getParameters function) []))
        outputs (mapv #(expression-output % schema db) arguments)
        argument-oids (mapv :oid outputs)
        fixed (get fixed-table-functions function-name)
        _ (when (or fixed (str/starts-with? function-name "json"))
            (let [expected (cond
                             (str/starts-with? function-name "jsonb_") [3802]
                             (str/starts-with? function-name "json_") [114]
                             (contains? #{"regexp_split_to_table" "string_to_table"
                                          "pg_input_error_info"} function-name)
                             (repeat (count arguments) 25)
                             :else [])]
              (when (some false? (map (fn [actual wanted]
                                        (or (nil? actual) (types/implicit-coercible? actual wanted)))
                                      argument-oids expected))
                (no-overload! function-name))))
        columns
        (cond
          fixed
          (do
            (when-not (contains? (:arity fixed) (count arguments)) (no-overload! function-name))
            (mapv (fn [name oid] {:name name :oid oid}) (:names fixed) (:oids fixed)))
          (= function-name "generate_series")
          [{:name function-name :oid (series-oid argument-oids)}]
          (contains? #{"unnest" "information_schema._pg_expandarray"} function-name)
          (let [element (when (= 1 (count arguments))
                          (types/array-oid->element-oid (first argument-oids)))]
            (when-not element
              (cond
                (not= 1 (count arguments))
                (unsupported! "a FROM array function with multiple arguments")
                (nil? (first argument-oids))
                (throw (ex-info "could not resolve the polymorphic FROM function"
                                {:sqlstate (if (= function-name "unnest") "42725" "42804")}))
                (= "m" (:typtype (types/catalog-type (first argument-oids))))
                (unsupported! "FROM expansion of multirange values")
                (pos? (or (:typelem (types/catalog-type (first argument-oids))) 0))
                (unsupported! "FROM expansion of this array carrier")
                :else (no-overload! function-name)))
            (cond-> [{:name (if (= function-name "unnest") "unnest" "x") :oid element}]
              (= function-name "information_schema._pg_expandarray") (conj {:name "n" :oid 23})))
          (contains? #{"json_to_record" "jsonb_to_record" "json_to_recordset" "jsonb_to_recordset"} function-name)
          (do
            (when-not (seq coldefs)
              (throw (ex-info "a column definition list is required for functions returning record"
                              {:sqlstate "42601"})))
            (mapv (fn [[name type]]
                    (let [base (-> type (str/replace #"\s*\([^)]*\)" "") str/lower-case)
                          oid (or (get types/pg-name->oid base)
                                  (some-> (get types/sql-name->elem-kw base) types/elem-kw->oid)
                                  (some-> (types/parse-array-type-name base) :pg-name types/pg-name->oid))]
                      (when-not oid (unsupported! "this record-function column type"))
                      {:name name :oid oid})) coldefs))
          :else (unsupported! (str "metadata for FROM function " function-name)))
        columns (cond-> (vec columns)
                  (= "ORDINALITY" (some-> (.getWithClause table-function) str))
                  (conj {:name "ordinality" :oid 20}))]
    {:aliases (mapv :name columns)
     :oids (mapv :oid columns)
     :columns columns
     :rows []
     :vtypes (mapv #(or (types/dh-type-for-oid (:oid %)) :db.type/string) columns)
     :pg-types (mapv #(get types/oid->pg-type-marker (:oid %)) columns)}))
