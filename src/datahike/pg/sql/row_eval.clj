(ns datahike.pg.sql.row-eval
  "Row-level SQL expressions -- CHECK constraints, domain checks,
   RETURNING, ON CONFLICT ... WHERE -- evaluated by the SELECT translator,
   not by a second interpreter.

   PostgreSQL evaluates these by projecting over a tuple (ExecCheck,
   ExecQual, ExecProcessReturning): the expression's column references are
   Vars bound to the tuple's attributes. Here the tuple is a ROW SCOPE: a
   table's columns bound (params/*from-bindings*) to placeholders typed by
   their declared OIDs, the expressions translated once as
   `SELECT <exprs>`, and run per row with the row's values bound. Names
   resolve by the translator's own scoping: a subquery that references the
   row correlates through params/*row-scope-aliases* exactly as it would
   against an outer FROM item, and the scope's table supplies column
   metadata (params/*row-scope-tables*).

   Errors propagate with their SQLSTATE, as they do in PostgreSQL."
  (:require [clojure.string :as str]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql :as sql]
            [datahike.pg.constraints.row :as row-constraints]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.types :as types])
  (:import [datahike.pg ColumnSubstitutingDeParser ColumnSubstitutingDeParser$Unsupported]
           [net.sf.jsqlparser.schema Column]))

(def ^:private row-base
  "Row placeholders are numbered from here: above any `$n` a statement
   can carry (the protocol's parameter count is 16-bit), so they never
   collide with the statement's own parameters and a plan does not
   depend on how many there are."
  100000)

(def ^:private value-scope
  "The alias of a scope without a table (a domain's VALUE)."
  "__row")

(defn table-columns
  "A table's columns, [{:name :attr :oid}] in order, inherited ones
   included, each stored under the table that declares it and typed by
   that table."
  [db schema table]
  (sql/cached-result
   [::columns table schema]
   (fn []
     (let [declared-oid (fn [attr]
                          (some #(when (= attr (:attr %)) (:oid %))
                                (pgs/column-info schema (namespace attr) db)))]
       (mapv (fn [{:keys [name attr]}]
               {:name name :attr attr :oid (declared-oid attr)})
             (row-constraints/column-specs db table))))))

(defn- type-oid
  "The OID a VALUE of `t` takes: a type name, or the attribute of the
   column whose declared type it has."
  [db schema t]
  (if (string? t)
    (types/pg-name->oid (types/normalize-sql-type-name t))
    (some #(when (= t (:attr %)) (:oid %))
          (table-columns db schema (namespace t)))))

(defn- stored-value
  "A column's value as SELECT reads it from storage."
  [schema attr v]
  (if (= :db.type/bigdec (get-in schema [attr :db/valueType]))
    (types/numeric-storage->value v)
    v))

(defn- scopes
  "The relations the expressions see, each {:alias :table :columns}, a
   column {:name :oid :read (fn [row])}:
   - table `table` as `alias`, its values the row's storage attributes;
   - with :excluded?, ON CONFLICT's `excluded` row (the row's
     :excluded/<column> entries);
   - with `column-types` ({name attr-or-type}), a table-less scope for
     names such as a domain's VALUE, read from the row's bare keys."
  [db schema table {:keys [alias excluded? column-types]}]
  (let [cols (when (seq table) (table-columns db schema table))]
    (cond-> []
      (seq table)
      (conj {:alias (or alias table) :table table
             :columns (mapv (fn [{:keys [name attr oid]}]
                              {:name name :oid oid
                               :read #(stored-value schema attr (get % attr))})
                            cols)})
      excluded?
      (conj {:alias "excluded" :table table
             :columns (mapv (fn [{:keys [name attr oid]}]
                              {:name name :oid oid
                               :read #(stored-value schema attr
                                                    (get % (keyword "excluded" name)))})
                            cols)})
      (seq column-types)
      (conj {:alias value-scope
             :columns (vec (for [[n t] column-types]
                             {:name n :oid (type-oid db schema t)
                              :read #(get % (keyword "" n))}))}))))

(defn- expression-text
  "The expression's SQL. Names of a table-less scope are quoted: a
   domain's VALUE is a keyword to the parser once it is text again."
  [ast quoted]
  (if (seq quoted)
    (try
      (ColumnSubstitutingDeParser/deparse
       ast (reify java.util.function.Function
             (apply [_ c]
               (let [n (params/unquote-ident (.getColumnName ^Column c))]
                 (if (and (nil? (.getTable ^Column c)) (contains? quoted n))
                   (str "\"" n "\"")
                   (str c))))))
      (catch ColumnSubstitutingDeParser$Unsupported e
        (throw (ex-info (str (.getMessage e) " in a domain check is not supported")
                        {:sqlstate "0A000"}))))
    (str ast)))

(defn- plan-error!
  "Raise a plan's translation error with its SQLSTATE."
  [plan]
  (throw (ex-info (str (:message plan))
                  {:sqlstate (:sqlstate plan) :error-fields (:error-fields plan)})))

(defn- compiled-plan
  "{:plan translated-plan :run (fn [in-args]) or nil} for the expressions
   `asts` over `scs`, placeholders numbered across them from row-base.

   Kept in parse-sql's result cache: the key holds the expressions' text,
   the scopes with their column types, the schema and the translation
   context, and DDL clears it. A plan that failed to translate is not
   kept; one the row projection cannot run raises 0A000."
  [asts scs schema db]
  (let [quoted (into #{} (comp (filter #(nil? (:table %))) (mapcat :columns) (map :name)) scs)
        texts (mapv #(expression-text % quoted) asts)
        k (into [::plan texts
                 (mapv (fn [{:keys [alias table columns]}]
                         [alias table (mapv (juxt :name :oid) columns)])
                       scs)
                 schema]
                (sql/translation-context))
        c (sql/cached-result
           k
           (fn []
             (let [slots (reductions + 0 (map (comp count :columns) scs))
                   bindings (into {} (map (fn [{:keys [alias columns]} start]
                                            [alias (into {} (map-indexed
                                                             (fn [i {:keys [name]}]
                                                               [name (params/->ParamRef (+ row-base start i 1))]))
                                                         columns)])
                                          scs slots))
                   ;; The row scope is the only outer context: nothing the
                   ;; caller is translating reaches into the plan.
                   p (binding [params/*bound-params* nil
                               params/*from-bindings* bindings
                               params/*from-binding-oids*
                               (into {} (map (fn [{:keys [alias columns]}]
                                               [alias (into {} (map (juxt :name :oid)) columns)]))
                                     scs)
                               params/*from-source-aliases* nil
                               params/*outer-scope-aliases* nil
                               params/*lateral-outer-aliases* nil
                               params/*row-scope-aliases* (set (map :alias scs))
                               params/*row-scope-tables*
                               (into {} (keep (fn [{:keys [alias table]}]
                                                (when (and table (not= alias "excluded")) [alias table])))
                                     scs)]
                       (sql/parse-sql (str "SELECT " (str/join ", " (map #(str "(" % ")") texts)))
                                      schema db))]
               (cond
                 (= :error (:type p)) p
                 (not= :select (:type p))
                 {:type :error :sqlstate "0A000"
                  :message (str "row expression planned as " (name (:type p)) " is not supported")}
                 :else {:plan p :run (stmt/const-select-fn p)}))))]
    (when (= :error (:type c)) (plan-error! c))
    c))

(defn row-values
  "The values of the SQL expressions `asts` over one row of `table`, and
   their OIDs: {:values [...] :oids [...]}, NULL as nil. `row` holds the
   row's storage attributes.

   Options:
     :alias        the name the row is visible under (default `table`)
     :excluded?    ON CONFLICT: `excluded` is in scope too, its values the
                   row's :excluded/<column> entries
     :column-types {name attr-or-type-name} for names the table lacks --
                   a domain's VALUE, typed as the domain's base type,
                   read from the row's bare key
     :exec-db      the database subqueries read (default `db`):
                   RETURNING's command snapshot, which does not see the
                   statement's own writes

   Parameters in the expressions (`$1`) take the values of
   params/*bound-params*."
  [asts row table schema db & [{:keys [exec-db] :as opts}]]
  (let [b params/*bound-params*
        bound (if (sequential? b) (vec b) [])
        scs (scopes db schema table opts)
        {:keys [plan run]} (compiled-plan asts scs schema db)
        readers (into [] (mapcat :columns) scs)
        ;; Values are read only for the placeholders the plan uses.
        fetch (fn [idx]
                (let [idx (long idx)]
                  (if (> idx row-base)
                    ((:read (nth readers (- idx row-base 1))) row)
                    (nth bound (dec idx) nil))))
        bound-plan (params/substitute-select-plan plan fetch)
        exec-db (or exec-db db)
        n (count asts)
        values (binding [params/*runtime-db* exec-db]
                 (if run
                   (vec (take n (or (run (:in-args bound-plan)) (repeat nil))))
                   (stmt/run-const-select-row bound-plan exec-db n)))]
    {:values values
     :oids (vec (take n (:select-item-oids plan)))}))

(defn row-oids
  "The OIDs `row-values` returns for `asts` over `table`, without a row:
   what Describe reports for RETURNING."
  [asts table schema db & [opts]]
  (let [{:keys [plan]} (compiled-plan asts (scopes db schema table opts) schema db)]
    (vec (:select-item-oids plan))))

(defn check-result
  "true, false or nil (unknown) for the CHECK expression `ast` over `row`
   (see row-values for `opts`). Only false is a violation: CHECK passes on
   NULL, unlike WHERE."
  ([ast row ns schema db] (check-result ast row ns schema db nil))
  ([ast row ns schema db opts]
   (let [v (first (:values (row-values [ast] row ns schema db opts)))]
     (when (some? v) (boolean v)))))

(defn check-fn
  "The CHECK evaluator constraint validation takes: (fn [ast row ns schema
   & [column-types]]) over `db`."
  [db]
  (fn [ast row ns schema & [column-types]]
    (check-result ast row ns schema db {:column-types column-types})))
