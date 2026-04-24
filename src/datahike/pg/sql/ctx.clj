(ns datahike.pg.sql.ctx
  "Translation context for the SQL → Datalog translator.

   The `ctx` is an immutable map of per-translation atoms that every
   `translate-*` fn threads through. Atoms capture the side-effecting
   state (fresh-var counter, collected where clauses, entity-var
   bindings, prepared-statement placeholders, …) while the outer map
   stays shareable with nested sub-translations.

   Fn inventory:

   - `resolve-column` / `resolve-inherited-attr` — map a JSqlParser
     `Column` reference to a Datahike attribute keyword, following the
     table-alias map and PostgreSQL INHERITS semantics.
   - `make-ctx` — build a fresh context.
   - `fresh-var!`, `entity-var!`, `add-clause!`, `col-var!` —
     primitives the translators call to allocate logic variables,
     assign entity bindings, append where clauses, and lazily
     produce `get-else`-backed column bindings.
   - `materialize-arg!` — bind a composite expression to a fresh var.
   - `null-guard-clauses` / `make-columns-optional!` — SQL
     3-valued-logic helpers.
   - `collect-vars` — recursively gather `?v`-style symbols out of
     a translated form.

   Fns originally marked `^:private` in sql.clj are promoted to
   public here so the extracted translate-* namespaces can reach
   them without re-exporting through the top-level sql ns."
  (:require [clojure.string :as str]
            [datahike.pg.sql.params :as params])
  (:import [net.sf.jsqlparser.expression Alias]
           [net.sf.jsqlparser.schema Column Table]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Table alias resolution

(defn resolve-column
  "Resolve a column reference to a Datahike attribute keyword.
   Uses table-aliases map {alias → table-name} and schema-tables for lookup.
   Handles table inheritance: if a column doesn't exist in the child table's
   namespace but does exist in an inherited parent's namespace, resolves to
   the parent namespace (PostgreSQL INHERITS semantics).

   Optional 4th arg `col-overrides` is `{table-name → {hinted-col-name → attr-ident}}`
   from `make-ctx`; when a column name appears there, the override wins
   over the default `(keyword table-name col-name)` construction. Makes
   `:datahike.pg/column` renames resolve on the read side too.

   Returns either:
     [:db-id alias-key]       — for db_id references
     [:aliased alias-key kw]  — for aliased column references (self-joins)
     :ns/col                  — for regular column references"
  ([^Column col table-aliases default-table]
   (resolve-column col table-aliases default-table {}))
  ([^Column col table-aliases default-table col-overrides]
   (let [table-ref (.getTable col)
         table-alias (when table-ref (params/unquote-ident (.getName ^Table table-ref)))
         alias-key (or table-alias default-table)
         table-name (get table-aliases alias-key alias-key)
         col-name (params/unquote-ident (.getColumnName col))]
     (if (= col-name "db_id")
       [:db-id alias-key]
       (let [kw (or (get-in col-overrides [table-name col-name])
                    (keyword table-name col-name))]
         (if (not= alias-key table-name)
           [:aliased alias-key kw]
           kw))))))

(defn resolve-inherited-attr
  "For INHERITS support: check if an attribute exists in the table's schema.
   If not, walk up the inheritance chain to find it in a parent.
   Returns the resolved keyword (possibly in parent namespace) or the original."
  [attr schema db]
  (if (get schema attr)
    attr  ;; attribute exists in the table's own namespace
    ;; Check for inheritance: is there a parent table?
    (let [table-name (namespace attr)
          col-name (name attr)
          q-fn (requiring-resolve 'datahike.api/q)
          parent (ffirst (q-fn '{:find [?p]
                                 :where [[?e :__inherit__/child ?c]
                                         [?e :__inherit__/parent ?p]]
                                 :in [$ ?c]}
                               db table-name))]
      (if parent
        (let [parent-attr (keyword parent col-name)]
          (if (get schema parent-attr)
            parent-attr  ;; found in parent namespace
            attr))       ;; not found in parent either, return original
        attr))))

;; ---------------------------------------------------------------------------
;; Context constructor + primitives

(defn extract-table-info
  "Extract table name and alias from a FROM clause Table."
  [^Table table]
  (let [name (params/unquote-ident (.getName table))
        alias (.getAlias table)
        alias-name (when alias (params/unquote-ident (.getName ^Alias alias)))]
    {:name  name
     :alias (or alias-name name)}))

(defn make-ctx
  "Create a fresh translation context. The options map may carry:
   - :db        — live Datahike db snapshot (for schema inherits lookup,
                  virtual catalog resolution, subquery execution)
   - :parse-sql — recursion hook: a fn of [sql schema db] that re-enters
                  the parser to translate inner SQL strings (IN /
                  EXISTS subqueries). Passed by `datahike.pg.sql/parse-sql`
                  at top-level ctx construction so expression translators
                  can recurse without a cyclic namespace load. Callers
                  that don't need subquery support can omit it.
   - :hints     — `{attr-ident → hint-map}` from `datahike.pg.schema/schema-hints`.
                  Drives the `:col-overrides` lookup used by `resolve-column`
                  so `WHERE <renamed-col>` and `JOIN … ON …` resolve hint-
                  mapped columns to their real attribute keywords."
  [schema table-aliases default-table & [{:keys [db parse-sql hints]}]]
  {:schema        schema
   :table-aliases table-aliases
   :default-table default-table
   :db            db
   :parse-sql     parse-sql
   :hints         (or hints {})
   ;; Precomputed {table-name → {hinted-col-name → attr-ident}} so
   ;; resolve-column maps user-facing column names back to the storage-
   ;; level attribute in O(1). Only entries for columns with a
   ;; :datahike.pg/column rename — `resolve-column` falls back to the
   ;; default name-from-ident path for unhinted columns.
   :col-overrides (reduce-kv (fn [acc ident h]
                               (if-let [col (:column h)]
                                 (let [ns (namespace ident)]
                                   (assoc-in acc [ns col] ident))
                                 acc))
                             {}
                             (or hints {}))
   :var-counter   (atom 0)
   :col->var      (atom {})       ;; [alias-key attr-keyword] or attr-keyword → ?var-symbol
   :entity-vars   (atom {})       ;; alias-key → ?entity-var
   :where-clauses (atom [])
   :find-elements (atom [])
   :with-vars     (atom #{})
   :in-params     (atom [])       ;; extra :in parameter symbols for CASE fns etc.
   :in-args       (atom [])
   :left-join-evars (atom #{})    ;; entity vars from LEFT JOIN right side (don't get-else on these)
   ;; Vars emitted by col-var! that are bound via get-else and thus may
   ;; carry the `:__null__` sentinel. Predicates involving these need
   ;; null guards (per SQL's three-valued logic: `col op V` when col is
   ;; NULL → UNKNOWN → false in WHERE).
   :nullable-vars (atom #{})
   ;; Prepared-statement parameter placeholders. Map {index → ?var},
   ;; populated when translate-expr encounters a JSqlParser JdbcParameter
   ;; (`?` or `$N`). The handler looks at `:param-placeholders` on the
   ;; parse-sql result to know how many slots to fill and in what var
   ;; order at Bind-time.
   :param-placeholders (atom (sorted-map))})

(defn fresh-var!
  "Generate a fresh logic variable ?v1, ?v2, etc."
  [ctx]
  (symbol (str "?v" (swap! (:var-counter ctx) inc))))

(defn entity-var!
  "Get or create the entity variable for a table alias."
  [ctx alias-key]
  (let [evars (:entity-vars ctx)]
    (or (get @evars alias-key)
        (let [v (symbol (str "?" alias-key "_eid"))]
          (swap! evars assoc alias-key v)
          v))))

(defn add-clause!
  "Append a Datalog clause to the context's where-clauses."
  [ctx clause]
  (swap! (:where-clauses ctx) conj clause))

(defn col-var!
  "Get or create the logic variable for an attribute.

   For ordinary columns, binds the var via `get-else` so rows without the
   column still flow through (bound to the `:__null__` sentinel). This
   models SQL NULL semantics — a missing attribute is NULL, not a reason
   to drop the row — and the var is recorded in `:nullable-vars` so
   comparison predicates can wrap it in null-guards (three-valued logic).

   Exception: when the entity is a LEFT JOIN right-side var (already
   registered in `:left-join-evars`), emit a plain data pattern instead.
   The LEFT JOIN or-join construction (run later in translate-select)
   identifies those patterns and relocates them into the matched branch,
   synthesising `:__null__` via `ground` in the unmatched branch. A
   get-else with the LEFT JOIN sentinel entity-id would otherwise throw.
   These vars are still added to `:nullable-vars` so null-guards apply.

   Handles three forms of attr:
     [:db-id alias-key]        → return entity var for the alias
     [:aliased alias-key kw]   → aliased column (self-joins)
     :ns/col                   → regular column"
  [ctx attr]
  (cond
    ;; [:db-id alias-key] → entity var (not nullable)
    (and (vector? attr) (= :db-id (first attr)))
    (entity-var! ctx (second attr))

    ;; [:aliased alias-key :ns/col] → aliased column for self-joins
    (and (vector? attr) (= :aliased (first attr)))
    (let [alias-key (nth attr 1)
          kw (nth attr 2)
          cache-key [alias-key kw]
          cvars (:col->var ctx)]
      (or (get @cvars cache-key)
          (let [v (symbol (str "?" alias-key "_" (name kw)))
                evar (entity-var! ctx alias-key)
                lj? (contains? @(:left-join-evars ctx) evar)]
            (swap! cvars assoc cache-key v)
            (if lj?
              (add-clause! ctx [evar kw v])
              (add-clause! ctx [(list 'get-else '$ evar kw :__null__) v]))
            (swap! (:nullable-vars ctx) conj v)
            v)))

    ;; Regular keyword :ns/col
    :else
    (let [alias-key (namespace attr)
          ;; Resolve inherited attributes: if :child/col doesn't exist in schema
          ;; but :parent/col does (via __inherit__), use the parent namespace
          resolved-attr (if-let [db (:db ctx)]
                          (resolve-inherited-attr attr (:schema ctx) db)
                          attr)
          cache-key [alias-key resolved-attr]
          cvars (:col->var ctx)]
      (or (get @cvars cache-key)
          (let [v (symbol (str "?" alias-key "_" (name resolved-attr)))
                evar (entity-var! ctx alias-key)
                lj? (contains? @(:left-join-evars ctx) evar)]
            (swap! cvars assoc cache-key v)
            (if lj?
              (add-clause! ctx [evar resolved-attr v])
              (add-clause! ctx [(list 'get-else '$ evar resolved-attr :__null__) v]))
            (swap! (:nullable-vars ctx) conj v)
            v)))))

;; ---------------------------------------------------------------------------
;; Expression helpers

(defn materialize-arg!
  "If arg is a compound form (seq), bind it to a fresh var via a
   function-binding clause and return the var. Otherwise return arg."
  [ctx arg]
  (if (seq? arg)
    (let [v (fresh-var! ctx)]
      (add-clause! ctx [arg v])
      v)
    arg))

(defn null-guard-clauses
  "Return a vector of predicate clauses that assert none of the supplied
   vars is the `:__null__` sentinel. Used to make comparison predicates
   null-safe per SQL's three-valued logic: `col op V` when col IS NULL
   yields UNKNOWN, and WHERE treats UNKNOWN as FALSE (row filtered).

   Only vars that were emitted via col-var! (and therefore recorded in
   `:nullable-vars`) need guarding — other vars cannot be `:__null__`."
  [ctx vars]
  (let [nullable @(:nullable-vars ctx)]
    (vec (keep (fn [v]
                 (when (and (symbol? v) (contains? nullable v))
                   [(list 'not= v :__null__)]))
               vars))))

(defn make-columns-optional!
  "Convert plain data patterns for the given variable symbols to get-else.
   Used by COALESCE, NULLIF, CASE — functions that handle NULLs explicitly.
   Skips patterns whose entity var is a LEFT JOIN right-side entity var,
   because those patterns will be moved inside the or-join where NULL
   synthesis is handled by the matched/unmatched branches."
  [ctx vars]
  (let [lj-evars @(:left-join-evars ctx)]
    (doseq [v vars :when (symbol? v)]
      (let [clauses @(:where-clauses ctx)
            match (first (filter (fn [c]
                                   (and (vector? c) (= 3 (count c))
                                        (= v (nth c 2))
                                        (keyword? (second c))
                                        (symbol? (first c))))
                                 clauses))]
        (when (and match
                   ;; Don't convert patterns on LEFT JOIN right-side entity vars.
                   ;; The LEFT JOIN or-join handles NULL synthesis for these.
                   (not (contains? lj-evars (first match))))
          (let [[evar attr _] match
                ;; Replace in-place to preserve clause ordering (critical for binding resolution)
                replacement [(list 'get-else '$ evar attr :__null__) v]]
            (reset! (:where-clauses ctx)
                    (mapv (fn [c] (if (= c match) replacement c))
                          @(:where-clauses ctx)))))))))

(defn collect-vars
  "Collect all logic variables (symbols starting with ?) from a form."
  [form]
  (cond
    (symbol? form) (when (str/starts-with? (str form) "?") #{form})
    (sequential? form) (into #{} (mapcat collect-vars) form)
    (map? form) (into #{} (mapcat collect-vars) (vals form))
    :else #{}))
