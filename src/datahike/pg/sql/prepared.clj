(ns datahike.pg.sql.prepared
  "Detached analysis inputs for prepared statements containing OID literals.

   The source and observations distinguish re-lowering an analyzed statement
   from analyzing its original text after a dependency changes. No artifact
   contains a database, parser node, or executable closure."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.arrays :as arrays]
            [datahike.pg.bits :as bits]
            [datahike.pg.catalog.basis :as basis]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.catalog.regoid :as regoid]
            [datahike.pg.input :as input]
            [datahike.pg.schema :as schema]
            [datahike.pg.sql.catalog :as catalog]
            [datahike.pg.sql.classify :as classify]
            [datahike.pg.sql.expr :as expr]
            [datahike.pg.sql.oid-infer :as oid-infer]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types])
  (:import [java.lang.reflect InvocationTargetException Method Modifier]
           [java.util IdentityHashMap]
           [net.sf.jsqlparser.expression BinaryExpression CastExpression Function
            Parenthesis StringValue CaseExpression]
           [net.sf.jsqlparser.expression.operators.relational Between InExpression ParenthesedExpressionList]
           [net.sf.jsqlparser.parser ASTNodeAccess]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.statement.delete Delete]
           [net.sf.jsqlparser.statement.insert Insert]
           [net.sf.jsqlparser.statement.select PlainSelect Select Values]
           [net.sf.jsqlparser.statement.update Update UpdateSet]))

(def ^:private max-source-chars 65536)
(def ^:private max-nodes 16384)
(def ^:private max-sites 4096)
(def ^:private max-rows 100000)
(def ^:dynamic *capture-literals?* true)

(defn- unsupported! []
  (throw (ex-info "Prepared analysis is outside the detached observation bounds"
                  {::unsupported true})))

(defn- bounded-vector [xs limit]
  (let [v (vec (take (inc limit) xs))]
    (when (> (count v) limit) (unsupported!))
    v))

(defn- frozen [value]
  (let [result (basis/freeze-projection value)]
    (when-not (:token-eligible? result) (unsupported!))
    (:value result)))

(defn- ident [value]
  (params/unquote-ident (str value)))

(defn- null-or-literal [node]
  (cond
    (instance? Parenthesis node) (recur (.getExpression ^Parenthesis node))
    (and (instance? ParenthesedExpressionList node) (= 1 (count node)))
    (recur (first node))
    :else node))

(defn- literal? [node]
  (let [node (null-or-literal node)]
    (and (instance? StringValue node) (not (bits/bit-string-literal? node)))))

(defn- type-spelling [oid]
  (or (get types/oid->pg-name oid)
      (some-> (types/array-oid->element-oid oid) types/oid->pg-name (str "[]"))))

(defn- alias-element [oid]
  (if (contains? regoid/alias-oids oid) oid
      (let [elem (types/array-oid->element-oid oid)]
        (when (contains? regoid/alias-oids elem) elem))))

(defn- source-span
  "Validate a parser node's token coordinates against the lexical source.
   The literal must be unambiguous within that node; never search by value."
  [node tokens]
  (let [^ASTNodeAccess node (null-or-literal node)
        ast (.getASTNode node)]
    (when-not ast (unsupported!))
    (let [first-token (.jjtGetFirstToken ast)
          last-token (.jjtGetLastToken ast)]
      (when-not (and first-token last-token) (unsupported!))
      ;; JSqlParser 5.2's absolute offsets are one-based, with an
      ;; exclusive end; classify's string coordinates are zero-based.
      (let [start (dec (.-absoluteBegin first-token))
            end (dec (.-absoluteEnd last-token))
            candidates (filter #(and (= :string (:type %))
                                     (<= start (:pos %)) (< (:pos %) end)) tokens)]
        (when-not (= 1 (count candidates)) (unsupported!))
        (let [{:keys [pos end]} (first candidates)]
          [pos end])))))

(defn- sql-string [s]
  (str "'" (str/replace s "'" "''") "'"))

(defn- bind-literal!
  ([state tokens node target-oid] (bind-literal! state tokens node target-oid true))
  ([state tokens node target-oid typed?]
   (when (and (literal? node) (or (alias-element target-oid) (= target-oid types/oid-oid)))
     (when-not *capture-literals?* (unsupported!))
     (let [node (null-or-literal node)
           text (expr/string-value-text ^StringValue node)
           elem (types/array-oid->element-oid target-oid)
           alias (alias-element target-oid)
           value (if elem
                   (arrays/from-pg-text text (types/oid->elem-kw elem))
                   (if alias
                     (regoid/input (types/oid->pg-name target-oid) text)
                     (input/parse types/oid-oid text)))
          ;; A plain oid reads digits, not names. It still contributes a
          ;; relation invalidation address, as ISREGCLASSCONST does.
           [start end] (source-span node tokens)
           storage (if elem (arrays/to-storage-text value) (str value))
           replacement (if typed?
                         (str "CAST(" (sql-string storage) " AS " (type-spelling target-oid) ")")
                         (sql-string storage))
           site {:start start :end end :oid target-oid :replacement replacement}
           existing (get-in @state [:sites start])]
       (when (and existing (not= site existing)) (unsupported!))
       (when (>= (count (:sites @state)) max-sites) (unsupported!))
       (swap! state assoc-in [:sites start] site)
       (swap! state update :types conj target-oid)
      ;; Array Consts do not contain scalar regclass Const nodes.
       (when (and (not elem) (contains? #{types/oid-regclass types/oid-oid} target-oid))
         (swap! state update :relation-oids conj (long value)))))))

(defn- metadata-row [db datom]
  (let [attr (dbi/-ident-for db (:a datom))]
    ;; A catalog entity can also carry runtime state, notably a sequence's
    ;; current value. Retain definitions and schema properties only.
    (when (or (= "db" (namespace attr)) (basis/catalog-attribute? attr))
      [attr (:e datom) (:v datom) (:tx datom) (:added datom)])))

(def ^:private relation-name-attributes
  #{:datahike.pg/table :datahike.pg/view-name :datahike.pg.matview/name
    :datahike.pg.trigger/table :datahike.pg.index/table
    :__inherit__/parent :__inherit__/child :__seq__/name
    :pg/table :pg/check-table :pg/fk-child-table :pg/fk-parent-table})

(def ^:private type-name-attributes
  #{:datahike.pg.enum/name :datahike.pg.domain/name :datahike.pg.composite/name})

(defn- function-entities [db [name arity]]
  (if (get (dbi/-schema db) :datahike.pg.function/name)
    (bounded-vector
     (d/q '[:find ?e :in $ ?name ?arity
            :where [?e :datahike.pg.function/name ?name]
            [?e :datahike.pg.function/arity ?arity]] db name arity) max-nodes)
    []))

(defn- observations [db {:keys [relation-oids relation-names functions types bindings]}]
  (let [db-schema (dbi/-schema db)
        relation-objects (mapv #(objects/object-by-address db objects/pg-class-oid %)
                               (sort relation-oids))
        relation-identities (mapv #(catalog/relation-identity db {:oid %}) (sort relation-oids))
        _ (doseq [[object identity] (map vector relation-objects relation-identities)]
            (when (and (nil? object) identity
                       (not= objects/pg-catalog-namespace-oid (:namespace-oid identity)))
              ;; Inferred native relations lack the persistent revision that
              ;; records storage resets; builtin catalog descriptors are static.
              (unsupported!)))
        type-objects (mapv #(regoid/type-identity db {:oid %}) (sort types))
        type-names (into #{} (comp (filter :user?) (map :typname)) type-objects)
        names (into (into relation-names (keep :datahike.pg.object/name) relation-objects)
                    (keep :name) relation-identities)
        columns (mapcat #(objects/columns-by-relation db %) (sort relation-oids))
        attrs (into (into #{} (keep :datahike.pg.column/storage-ident) columns)
                    (filter #(and (keyword? %) (contains? names (namespace %))))
                    (keys db-schema))
        catalog-attrs (filter basis/catalog-attribute? (conj (set (keys db-schema)) :db/ident))
        rows (bounded-vector (mapcat #(d/datoms db :aevt %) catalog-attrs) max-rows)
        function-eids functions
        function-objects (keep #(when-let [oid (:datahike.pg.function/oid (d/entity db %))]
                                  (objects/object-by-address db objects/pg-proc-oid oid))
                               function-eids)
        roots (into (into #{} (keep :db/id) relation-objects)
                    (concat (keep :db/id columns)
                            (keep #(some-> (objects/object-by-address db objects/pg-type-oid %) :db/id)
                                  types)
                            function-eids (keep :db/id function-objects)))
        roots (reduce (fn [roots datom]
                        (let [a (dbi/-ident-for db (:a datom)) v (:v datom)]
                          (if (or (and (= a :db/ident) (contains? attrs v))
                                  (and (= a :datahike.pg/for-ident) (contains? attrs v))
                                  (and (contains? type-name-attributes a)
                                       (contains? type-names v))
                                  (and (contains? relation-name-attributes a)
                                       (contains? names v)))
                            (conj roots (:e datom)) roots))) roots rows)
        ;; Capture dependent metadata rows through actual ref attributes,
        ;; not through coincidentally equal integer/OID values.
        roots (loop [roots roots depth 0]
                (when (> depth 32) (unsupported!))
                (let [next (reduce (fn [out datom]
                                     (let [a (dbi/-ident-for db (:a datom))]
                                       (if (and (= :db.type/ref (get-in db-schema [a :db/valueType]))
                                                (contains? roots (:v datom)))
                                         (conj out (:e datom)) out))) roots rows)]
                  (when (> (count next) max-nodes) (unsupported!))
                  (if (= roots next) roots (recur next (inc depth)))))
        entity-rows (bounded-vector
                     (mapcat #(keep (partial metadata-row db) (d/datoms db :eavt %))
                             (sort roots)) max-rows)
        ;; Entity tx IDs make committed metadata ABA visible even when the
        ;; current definition equals its earlier value.
        schema-value (into {} (filter (fn [[k v]]
                                        (or (contains? attrs k)
                                            (and (number? k) (contains? attrs v))))) db-schema)
        resolved-bindings (mapv (fn [[spelling _]]
                                  [spelling (regoid/input "regclass" spelling)]) bindings)]
    (frozen {:relations (mapv #(select-keys % [:datahike.pg.object/oid :datahike.pg.object/name
                                               :datahike.pg.object/kind]) relation-objects)
             :relation-identities relation-identities
             :schema schema-value :metadata entity-rows :bindings resolved-bindings
             :types type-objects
             :config (select-keys (dbi/-config db) [:attribute-refs? :schema-flexibility])})))

(defn- table-name [^Table table]
  (let [ns (.getSchemaName table)]
    (str (when ns (str ns "."))
         ;; Preserve quoting: dotted quoted names are one component.
         (.getName table))))

(defn- require-builtin-type! [db oid]
  (when oid
    (let [identity (regoid/type-identity db {:oid oid})]
      ;; Type spellings remain in lowered relation/column metadata. Until
      ;; lowering pins those independently of names, user types cannot
      ;; participate in a complete detached analysis proof.
      (when (or (nil? identity) (:user? identity)) (unsupported!)))))

(defn- require-explicit-write-dependencies!
  "Defaults, generated expressions and identity generation can introduce
   analyzed dependencies absent from the statement's raw argument tree."
  [db ^Table table]
  (let [name (ident (.getName table))
        oid (regoid/input "regclass" (table-name table))
        attrs (into #{} (keep :datahike.pg.column/storage-ident)
                    (bounded-vector (objects/columns-by-relation db oid) max-nodes))
        db-schema (dbi/-schema db)
        datoms (fn [attribute]
                 (when (contains? db-schema attribute)
                   (bounded-vector (d/datoms db :aevt attribute) max-rows)))
        column-expression? (fn [datom]
                             (let [entity (d/entity db (:e datom))]
                               (or (contains? attrs (:db/ident entity))
                                   (contains? attrs (:datahike.pg/for-ident entity)))))]
    (when (or (some #(= name (:v %)) (datoms :__seq__/owned-by-table))
              (some #(some column-expression? (datoms %))
                    [:pg/default-kind :pg/default-value :pg/default-arg
                     :pg/generated :pg/generated-expr]))
      (unsupported!))))

(defn- relation! [state db ^Table table ctes]
  (let [name (ident (.getName table))]
    (when-not (and (str/blank? (.getSchemaName table)) (contains? ctes name))
      (let [spelling (table-name table)
            oid (regoid/input "regclass" spelling)
            object (objects/object-by-address db objects/pg-class-oid oid)]
        (when (str/starts-with? name "__") (unsupported!))
        (when (contains? #{:view :materialized-view :composite-type} (:datahike.pg.object/kind object))
          ;; Their rewrite dependencies require the stored analyzed body.
          (unsupported!))
        (let [columns (objects/columns-by-relation db oid)
              type-oids (if (seq columns)
                          (map :datahike.pg.column/type-oid columns)
                          (map :oid (schema/column-info (dbi/-schema db) name db)))]
          ;; Even an unselected column contributes to the relation's
          ;; metadata projection, so check the complete row definition.
          (doseq [type-oid type-oids] (require-builtin-type! db type-oid)))
        (swap! state update :relation-oids conj oid)
        (swap! state update :bindings conj [spelling oid])
        (when-not object (swap! state update :relation-names conj name))))))

(defn- target-column-oid [db parsed ^Table table ^Column column]
  (let [name (ident (.getName table))
        column-name (ident (.getColumnName column))
        oid (regoid/input "regclass" (table-name table))
        object (objects/column-by-name db oid column-name)
        source-db (or (:enriched-db parsed) db)]
    (or (:datahike.pg.column/type-oid object)
        (:oid (some #(when (= column-name (:name %)) %)
                    (schema/column-info (dbi/-schema source-db) name source-db))))))

(defn- read-property [object ^Method method]
  (try (.invoke method object (object-array 0))
       (catch InvocationTargetException error
         ;; Convenience typed getters cast a SELECT to PlainSelect/Values;
         ;; the general getSelect getter still exposes the actual child.
         (if (instance? ClassCastException (.getCause error)) nil
             (throw error)))))

(defn- node? [value]
  (and value (str/starts-with? (.getName (class value)) "net.sf.jsqlparser.")))

(defn- children [node]
  (keep (fn [^Method method]
          (let [name (.getName method)
                result-type (.getReturnType method)
                child-type? (or (str/starts-with? (.getName result-type) "net.sf.jsqlparser.")
                                (.isAssignableFrom java.lang.Iterable result-type)
                                (.isAssignableFrom java.util.Map result-type)
                                (.isArray result-type))]
            (when (and (zero? (.getParameterCount method))
                       child-type?
                       (not (Modifier/isStatic (.getModifiers method)))
                       (str/starts-with? name "get")
                       (not (contains? #{"getClass" "getASTNode" "getParent" "getParenthesedStatement"} name)))
              [name (read-property node method)])))
        (sort-by #(.getName ^Method %) (.getMethods (class node)))))

(defn- source-types! [state db spelling]
  (let [oid (regoid/type-oid db (types/normalize-sql-type-name spelling))]
    ;; Re-lowering still resolves explicit cast target names. A user type's
    ;; rename must not silently turn that into fresh analysis of a sibling
    ;; alias literal; pinning arbitrary type syntax is a separate concern.
    (when (:user? (regoid/type-identity db {:oid oid})) (unsupported!))
    (swap! state update :types conj oid)
    oid))

(defn- capture-source [source db parsed]
  (let [tokens (vec (classify/tokenize-all source))
        ast (CCJSqlParserUtil/parse source)
        state (atom {:sites (sorted-map) :relation-oids #{} :relation-names #{}
                     :functions #{} :types #{} :bindings #{} :nodes 0})
        seen (IdentityHashMap.)
        source-db (or (:enriched-db parsed) db)
        column-oids (atom {})
        initial-env {:db source-db :schema (dbi/-schema source-db)
                     :table-aliases (:table-aliases parsed) :default-table (:table parsed)
                     :hints (schema/schema-hints source-db)}]
    (letfn [(column-oid [table column]
              (let [key [(table-name table) (ident (.getColumnName ^Column column))]]
                (if (contains? @column-oids key) (get @column-oids key)
                    (let [oid (target-column-oid db parsed table column)]
                      (swap! column-oids assoc key oid)
                      oid))))
            (bind-against [literal typed env]
              (when (literal? literal)
                (let [oid (oid-infer/expr-oid typed env)]
                  (when-not oid (unsupported!))
                  (bind-literal! state tokens literal oid))))
            (walk [node env ctes parent property depth]
              (when (> depth 64) (unsupported!))
              (when (and (instance? Table node)
                         (or (contains? #{"getFromItem" "getRightItem"} property)
                             (and (= property "getTable")
                                  (or (instance? Insert parent) (instance? Update parent)
                                      (instance? Delete parent)))))
                (relation! state db node ctes))
              (cond
                (instance? java.util.Map node)
                (doseq [v (.values ^java.util.Map node)] (walk v env ctes parent property (inc depth)))

                (instance? java.lang.Iterable node)
                (doseq [v node] (walk v env ctes parent property (inc depth)))

                (and node (.isArray (class node)))
                (doseq [v (seq node)] (walk v env ctes parent property (inc depth)))

                (node? node)
                (when-not (.containsKey seen node)
                  (.put seen node true)
                  (when (> (:nodes (swap! state update :nodes inc)) max-nodes) (unsupported!))
                  (let [ctes (if (instance? Select node)
                               (into ctes (keep #(some-> % .getAlias .getName ident))
                                     (or (.getWithItemsList ^Select node) [])) ctes)
                        env (if (instance? PlainSelect node)
                              (let [from (.getFromItem ^PlainSelect node)
                                    tables (filter #(instance? Table %)
                                                   (cons from (map #(.getRightItem %)
                                                                   (.getJoins ^PlainSelect node))))
                                    aliases (into {} (map (fn [^Table t]
                                                            [(or (some-> t .getAlias .getName ident)
                                                                 (ident (.getName t)))
                                                             (ident (.getName t))])) tables)]
                                (assoc env :table-aliases (merge (:table-aliases env) aliases)
                                       :default-table (when (instance? Table from)
                                                        (ident (.getName ^Table from))))) env)]
                    (cond
                      (instance? Column node)
                      (when-let [oid (oid-infer/expr-oid node env)]
                        (require-builtin-type! db oid)
                        (swap! state update :types conj oid))

                      (instance? CastExpression node)
                      (let [^CastExpression cast node
                            oid (source-types! state db (str (.getColDataType cast)))]
                        (bind-literal! state tokens (.getLeftExpression cast) oid false))

                      (instance? Function node)
                      (let [^Function function node
                            fname (expr/resolution-name (.getName function))
                            args (vec (or (.getParameters function) []))
                            alias-type (types/normalize-sql-type-name fname)
                            base-name (last (str/split fname #"\."))
                            key [base-name (count args)]
                            user-entities (function-entities db key)]
                        (when (> (count user-entities) 1) (unsupported!))
                        ;; Lowering dispatches builtins before stored functions.
                        ;; Do not attest an unused same-named stored candidate.
                        (when (and (seq user-entities) (expr/known-function-name? base-name))
                          (unsupported!))
                        (cond
                          (and (contains? regoid/alias-types alias-type) (= 1 (count args)))
                          (bind-literal! state tokens (first args) (source-types! state db alias-type))

                          ;; This implementation's nextval still consumes a
                          ;; textual sequence name. Preserve that spelling,
                          ;; but include the sequence as an analysis input.
                          (and (= base-name "nextval") (= 1 (count args)) (literal? (first args)))
                          (let [name (expr/string-value-text ^StringValue (null-or-literal (first args)))
                                oid (regoid/input "regclass" name)]
                            (swap! state update :relation-oids conj oid))

                          (seq user-entities)
                          ;; Observe the analyzed callee, not future siblings
                          ;; of the same name and arity. Ambiguous initial
                          ;; overloads are outside this detached proof.
                          (doseq [[eid] user-entities]
                            (swap! state update :functions conj eid)
                            (let [definition (d/entity db eid)
                                  ;; Lowering currently selects overloads by
                                  ;; cast category rather than exact signature.
                                  ;; A later sibling could change a nonzero
                                  ;; call even if this definition is unchanged.
                                  _ (when (seq args) (unsupported!))
                                  body (:datahike.pg.function/body definition)
                                  language (:datahike.pg.function/language definition)]
                              (when-not (and (string? body)
                                             (or (nil? language) (= "sql" language)))
                                (unsupported!))
                              (let [body-ast (CCJSqlParserUtil/parse body)]
                                (when-not (instance? Select body-ast) (unsupported!))
                                (binding [*capture-literals?* false]
                                  (walk body-ast initial-env #{} nil nil (inc depth))))))

                          ;; A builtin's polymorphic or common-type arguments
                          ;; may coerce an unknown literal to a neighboring
                          ;; alias type. Without its analyzed argument types,
                          ;; declining the whole artifact prevents partial
                          ;; capture (COALESCE, NULLIF, array functions, etc.).
                          (and (some literal? args)
                               (not (contains? #{"to_regproc" "to_regprocedure" "to_regclass"
                                                 "to_regtype" "to_regnamespace"} base-name)))
                          (unsupported!)))

                      ;; CASE and array constructors can resolve unknown
                      ;; leaves from distant result arms/elements. Their
                      ;; complete coercion tree is not exposed by lowering.
                      (or (instance? CaseExpression node)
                          (contains? #{"RowConstructor" "SetOperationList"}
                                     (.getSimpleName (class node))))
                      (unsupported!)

                      (= "ArrayConstructor" (.getSimpleName (class node)))
                      (when (some (fn [[_ child]]
                                    (or (literal? child)
                                        (and (instance? java.lang.Iterable child)
                                             (some literal? child))))
                                  (children node))
                        (unsupported!))

                      (instance? InExpression node)
                      (let [^InExpression in node
                            left (.getLeftExpression in)
                            right (.getRightExpression in)]
                        (when (literal? left) (unsupported!))
                        (if (instance? ParenthesedExpressionList right)
                          (doseq [value right] (bind-against value left env))
                          ;; Subqueries can coerce either output at the
                          ;; membership boundary; do not preserve half.
                          (unsupported!)))

                      (instance? Between node)
                      (let [^Between between node
                            left (.getLeftExpression between)
                            start (.getBetweenExpressionStart between)
                            end (.getBetweenExpressionEnd between)]
                        (bind-against start left env)
                        (bind-against end left env)
                        (when (literal? left) (unsupported!)))

                      (and (instance? BinaryExpression node)
                           (contains? #{"EqualsTo" "NotEqualsTo" "GreaterThan" "GreaterThanEquals"
                                        "MinorThan" "MinorThanEquals" "IsDistinctExpression"} (.getSimpleName (class node))))
                      (let [^BinaryExpression binary node
                            left (.getLeftExpression binary) right (.getRightExpression binary)]
                        (when (literal? left)
                          (let [oid (oid-infer/expr-oid right env)]
                            (when-not oid (unsupported!))
                            (bind-literal! state tokens left oid)))
                        (when (literal? right)
                          (let [oid (oid-infer/expr-oid left env)]
                            (when-not oid (unsupported!))
                            (bind-literal! state tokens right oid))))

                      (instance? BinaryExpression node)
                      (let [^BinaryExpression binary node]
                        (when (or (literal? (.getLeftExpression binary))
                                  (literal? (.getRightExpression binary)))
                          (unsupported!)))

                      (instance? Insert node)
                      (let [^Insert insert node
                            table (.getTable insert)
                            values (.getValues insert)]
                        (require-explicit-write-dependencies! db table)
                        (when values
                          (let [columns (or (seq (.getColumns insert))
                                            (map #(Column. (:name %))
                                                 (remove #(= "db_id" (:name %))
                                                         (schema/column-info (dbi/-schema db)
                                                                             (ident (.getName table)) db))))
                                expressions (vec (.getExpressions ^Values values))
                                rows (if (and (seq expressions)
                                              (instance? ParenthesedExpressionList (first expressions)))
                                       expressions [expressions])]
                            (doseq [row rows [column value] (map vector columns row)]
                              (bind-literal! state tokens value (column-oid table column))))))

                      (instance? Update node)
                      (let [^Update update node]
                        (require-explicit-write-dependencies! db (.getTable update))
                        (doseq [^UpdateSet assignment (.getUpdateSets update)
                                [column value] (map vector (.getColumns assignment) (.getValues assignment))]
                          (bind-literal! state tokens value
                                         (column-oid (.getTable update) column)))))
                    (doseq [[property child] (children node)]
                      (walk child env ctes node property (inc depth)))))))]
      (when-not (or (instance? Select ast) (instance? Insert ast)
                    (instance? Update ast) (instance? Delete ast)) (unsupported!))
      (walk ast initial-env #{} nil nil 0)
      (let [{:keys [sites] :as spec} @state
            sites (vec (vals sites))]
        (when (seq sites)
          (when (some (fn [[a b]] (> (:end a) (:start b))) (partition 2 1 sites))
            (unsupported!))
          {:source (reduce (fn [s {:keys [start end replacement]}]
                             (str (subs s 0 start) replacement (subs s end)))
                           source (reverse sites))
           :spec (dissoc spec :sites :nodes)})))))

(defn- eligible-source?
  "Reject ordinary text-only SQL before constructing a second parser AST or
   querying catalog projections. Metadata scans use indexed attribute slices."
  [source db]
  (let [tokens (classify/tokenize-all source)
        type-name? (fn [value]
                     (when value
                       (let [name (-> (str value) str/lower-case
                                      (str/replace #"^:" "")
                                      (str/replace #"^_" "")
                                      (str/replace #"\[\]$" ""))]
                         (or (= "oid" name) (contains? regoid/alias-types name)))))
        schema (dbi/-schema db)
        attribute-values (fn [attribute]
                           (when (contains? schema attribute)
                             (map :v (take (inc max-rows) (d/datoms db :aevt attribute)))))]
    (and (some #(= :string (:type %)) tokens)
         (or (some #(and (contains? #{:ident :quoted} (:type %))
                         (type-name? (ident (:text %)))) tokens)
             (some type-name? (attribute-values :pg/type))
             (some type-name? (attribute-values :pg/array-elem))
             (some (fn [value]
                     (some type-name? (edn/read-string value)))
                   (attribute-values :datahike.pg.function/arg-types))))))

(defn analyze
  "Capture supported OID literal coercions and exact analysis inputs.
   `parser-source` must be the parser-normalized SQL whose coordinates the
   pristine AST uses. Unsupported or oversized shapes return nil."
  [parser-source db parsed context]
  (try
    (when (and db (string? parser-source) (<= (count parser-source) max-source-chars)
               (eligible-source? parser-source db))
      (binding [regoid/*db* db regoid/*relation-db* db
                regoid/*relation-provider* catalog/relation-identity
                regoid/*relation-cache* (atom nil)
                regoid/*search-path* (or (:search-path context) regoid/*search-path*)
                regoid/*database-name* (or (:database-name context) regoid/*database-name*)]
        (when-let [artifact (capture-source parser-source db parsed)]
          (assoc artifact ::artifact true :context (frozen context)
                 :observations (observations db (:spec artifact))))))
    (catch Exception _ nil)))

(defn valid?
  "Recheck detached dependency definitions on the supplied statement DB.
   No broad catalog change alone invalidates an otherwise equal projection."
  [artifact db context]
  (boolean
   (try
     (and (::artifact artifact) db (= (:context artifact) (frozen context))
          (binding [regoid/*db* db regoid/*relation-db* db
                    regoid/*relation-provider* catalog/relation-identity
                    regoid/*relation-cache* (atom nil)
                    regoid/*search-path* (or (:search-path context) regoid/*search-path*)
                    regoid/*database-name* (or (:database-name context) regoid/*database-name*)]
            (= (:observations artifact) (observations db (:spec artifact)))))
     (catch Exception _ false))))
