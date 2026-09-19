(ns datahike.pg.sql.row-eval
  "Row-level SQL expressions -- CHECK constraints, domain checks, ON
   CONFLICT ... WHERE -- evaluated by the SELECT translator, not by a
   second interpreter.

   The expression is deparsed with every column reference replaced by a
   typed parameter, `CAST($n AS <declared type>)`, and run as the one-row
   `SELECT (<expr>)` with the row's values bound. Every operator,
   function, LIKE, regex, IS DISTINCT FROM and type rule is therefore the
   one SELECT uses. The interpreter this replaces answered 'satisfied'
   for any shape it did not know and compared only numbers with < and >,
   so `CHECK (name > 'm')` or `CHECK (s LIKE 'a%')` accepted any row.

   Errors propagate with their SQLSTATE, as they do in PostgreSQL."
  (:require [datahike.api :as d]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.types :as types])
  (:import [datahike.pg ColumnSubstitutingDeParser ColumnSubstitutingDeParser$Unsupported]
           [net.sf.jsqlparser.schema Column]))

(defn- attr-sql-type
  "The declared SQL type of the column stored as `attr`, as CAST spells
   it: modifiers included (`bit(3)`, `character(3)`, `numeric(10,2)`),
   an enum by its name. nil when the attribute has no column type."
  [db schema attr]
  (let [pg-type (params/pg-type-of-attr db attr)
        enum (when-not pg-type (:datahike.pg/enum-of (d/entity db attr)))]
    (cond
      enum enum
      pg-type (if-let [oid (types/pg-name->oid pg-type)]
                (types/format-type oid (or (params/pg-typmod-of-attr db attr) -1))
                pg-type)
      :else (some-> (get-in schema [attr :db/valueType]) types/dh-type->pg-name))))

(def ^:private attr-types
  "[schema {attr type}]: declared types, read once per schema value. The
   schema map is the same object across the rows of a statement, and a
   declared type changes only with it. Concurrent writers on different
   schemas only cost each other the lookups."
  (atom [nil {}]))

(defn- declared-type
  "The SQL type a column takes: `t` itself when it is a type name, else
   the declared type of the column stored as attribute `t`."
  [db schema t]
  (if (string? t)
    t
    (let [[s cached] @attr-types
          cached (if (identical? s schema) cached {})]
      (if (contains? cached t)
        (get cached t)
        (let [ty (attr-sql-type db schema t)]
          (reset! attr-types [schema (assoc cached t ty)])
          ty)))))

(def ^:private plans
  "Translated `SELECT (<expr>)` plans by SQL text. A row expression is
   evaluated once per written row, so it is translated once and each row
   only binds its values, as a prepared statement's Execute does. The
   text carries every column's declared type in its CASTs, and a row
   expression reads no table (subqueries are refused), so the text is the
   whole key. A plan that failed to translate is not kept."
  (java.util.concurrent.ConcurrentHashMap.))

(defn- compiled-plan
  "{:plan translated-plan :run (fn [in-args]) or nil} for `text`."
  [text schema db]
  (or (.get ^java.util.Map plans text)
      (let [p (binding [params/*bound-params* nil]
                (sql/parse-sql (str "SELECT (" text ")") schema db))
            c {:plan p :run (when-not (= :error (:type p)) (stmt/const-select-fn p))}]
        (when-not (= :error (:type p))
          (when (> (.size ^java.util.Map plans) 512) (.clear ^java.util.Map plans))
          (.put ^java.util.Map plans text c))
        c)))

(defn- substitute
  "`ast` as SQL with each column a typed parameter numbered after
   `offset`, and the values those parameters take."
  [ast row ns schema db column-types offset]
  (let [values (java.util.ArrayList.)
        text (try
               (ColumnSubstitutingDeParser/deparse
                ast
                (reify java.util.function.Function
                  (apply [_ c]
                    (let [^Column c c
                          n (params/unquote-ident (.getColumnName c))
                          t (or (declared-type db schema (or (get column-types n) (keyword ns n)))
                                (throw (errors/pg-error :undefined-column {:column n})))]
                      ;; The column's value, without the subscript `x[1]`
                      ;; carries: the deparser keeps that outside the cast.
                      (.add values (stmt/eval-update-expr
                                    (Column. (.getTable c) (.getColumnName c))
                                    row ns schema))
                      (str "CAST($" (+ offset (.size values)) " AS " t ")")))))
               (catch ColumnSubstitutingDeParser$Unsupported e
                 (throw (errors/pg-error :feature-not-supported
                                         {:feature (str (ex-message e) " in a row-level expression")}))))]
    [text (vec values)]))

(defn eval-expression
  "Value of the SQL expression `ast` over `row`, NULL as nil.

   `row` is looked up as UPDATE's SET evaluation does (stmt/eval-update-
   expr): storage attributes of table `ns`, or bare keys. A column is
   typed as the attribute `ns`/column declares, unless `column-types`
   ({column-name attr-or-type-name}) says otherwise -- a domain's VALUE
   takes the domain's base type. Parameters already in the expression
   (`$1` in an ON CONFLICT ... WHERE) keep their numbers and the bound
   values of `params/*bound-params*`; column parameters follow them."
  ([ast row ns schema db] (eval-expression ast row ns schema db nil))
  ([ast row ns schema db column-types]
   (let [b params/*bound-params*
         bound (if (sequential? b) (vec b) [])
         [text values] (substitute ast row ns schema db column-types (count bound))
         {:keys [plan run]} (compiled-plan text schema db)
         plan (params/substitute-select-plan plan (into bound values))]
     (if run
       (run (:in-args plan))
       (stmt/run-const-select plan db)))))

(defn check-result
  "true, false or nil (unknown) for the CHECK expression `ast` over `row`.
   Only false is a violation: CHECK passes on NULL, unlike WHERE."
  ([ast row ns schema db] (check-result ast row ns schema db nil))
  ([ast row ns schema db column-types]
   (let [v (eval-expression ast row ns schema db column-types)]
     (when (some? v) (boolean v)))))

(defn check-fn
  "The CHECK evaluator constraint validation takes: (fn [ast row ns schema
   & [column-types]]) over `db`."
  [db]
  (fn [ast row ns schema & [column-types]]
    (check-result ast row ns schema db column-types)))
