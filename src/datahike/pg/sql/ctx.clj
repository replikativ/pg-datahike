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
            [datahike.api :as d]
            [datahike.pg.sql.fns :as fns]
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
   (resolve-column col table-aliases default-table {} nil))
  ([^Column col table-aliases default-table col-overrides]
   (resolve-column col table-aliases default-table col-overrides nil))
  ([^Column col table-aliases default-table col-overrides derived-aliases]
   (let [table-ref (.getTable col)
         table-alias (when table-ref (params/unquote-ident (.getName ^Table table-ref)))
         alias-key (or table-alias default-table)
         table-name (get table-aliases alias-key alias-key)
         col-name (params/unquote-ident (.getColumnName col))
         derived? (contains? (or derived-aliases #{}) alias-key)]
     (cond
       ;; db_id on a real-table alias is the entity-id (special-cased
       ;; everywhere as the [:db-id alias] vector). On a derived alias,
       ;; db_id is just a projected value column on the speculative
       ;; entity — resolve to the regular `:<alias>/db_id` keyword.
       (and (= col-name "db_id") (not derived?))
       [:db-id alias-key]

       :else
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
          q-fn d/q
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

(defn attr-of
  "The Datahike attribute a `resolve-column` result denotes, with
   INHERITS resolution applied — or nil for the `[:db-id …]` form,
   which denotes an entity rather than an attribute.

   `resolve-column` returns two attribute-bearing shapes, `:ns/col` and
   `[:aliased alias :ns/col]`, and inheritance has to be resolved for
   BOTH: an INHERITS child stores its parent's columns under the PARENT
   namespace on the same entity (`:par/pname`, not `:chi/pname`), so a
   reference that keeps the child namespace binds nothing and reads as
   NULL.

   Every consumer used to do this itself and every one of them handled
   only the keyword shape, so `SELECT c.pname FROM child c` — and any
   other aliased reference to an inherited column — silently returned
   NULL where PostgreSQL returns the value. Routing all of them through
   here is what keeps the two shapes from drifting apart again."
  [ctx resolved]
  (let [kw (cond
             (keyword? resolved) resolved
             (and (vector? resolved) (= :aliased (first resolved))) (nth resolved 2)
             :else nil)]
    (when kw
      (if-let [db (:db ctx)]
        (resolve-inherited-attr kw (:schema ctx) db)
        kw))))

(defn with-resolved-attr
  "`resolved` with its attribute replaced by `attr-of`, preserving the
   shape. Use when the caller needs to pass the whole resolve-column
   result onward rather than just the attribute."
  [ctx resolved]
  (if-let [a (attr-of ctx resolved)]
    (if (vector? resolved)
      [:aliased (nth resolved 1) a]
      a)
    resolved))

;; ---------------------------------------------------------------------------
;; Context constructor + primitives

(def ^:dynamic *relation-namespaces*
  "`{relation-name -> storage-namespace}` for speculative relations —
   currently the CTEs in scope. Bound by the translator; consulted here
   so no FROM-clause site can forget it.

   A CTE is stored as ordinary attributes in a speculative db, and its
   namespace used to be its own name, so a CTE named after a real table
   wrote into that table's namespace and the two merged instead of the
   CTE shadowing the table. Redirecting the NAME while leaving the ALIAS
   as the user wrote it routes the reference through the same machinery
   as `FROM emp e`, which already resolves an alias to a differently-named
   relation correctly."
  {})

(defn extract-table-info
  "Extract table name and alias from a FROM clause Table.

   `:name` is the relation's STORAGE name and `:alias` the name the
   query refers to it by; they differ for an aliased table and for any
   relation in `*relation-namespaces*`."
  [^Table table]
  (let [name (params/unquote-ident (.getName table))
        alias (.getAlias table)
        alias-name (when alias (params/unquote-ident (.getName ^Alias alias)))]
    {:name  (get *relation-namespaces* name name)
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
  [schema table-aliases default-table & [{:keys [db parse-sql hints derived-aliases ref-targets]}]]
  {:schema        schema
   :table-aliases table-aliases
   :default-table default-table
   :db            db
   :parse-sql     parse-sql
   :hints         (or hints {})
   ;; {ref-attr-ident → target-pk-attr-ident} — drives SQL FK
   ;; semantics: projecting a `:db.type/ref` column yields the
   ;; target's PK value (matching a real-PG INT FK column), not the
   ;; raw Datahike entity-id. Computed once at the handler entry
   ;; point (datahike.pg.schema/derive-ref-targets) and threaded
   ;; through every translation. Empty-map default keeps
   ;; refs-as-eids behavior for callers that don't supply this.
   :ref-targets   (or ref-targets {})
   ;; Aliases of FROM/JOIN-position derived tables — `(SELECT … FROM …) AS x`
   ;; whose rows have been materialised into the speculative db with their
   ;; own entity-id space. translate-join uses this to suppress the ref/db_id
   ;; unification it would otherwise apply, because a derived alias's `db_id`
   ;; column is just the projected source-entity-id value, not a real
   ;; entity-id in the speculative db. Without this, JOIN ON outer.ref =
   ;; derived.db_id rebinds the derived alias's entity-var to the source
   ;; entity-id, then `derived.col` resolves through the wrong attribute
   ;; namespace (source's :customer/name instead of the speculative
   ;; :c/name) and matches nothing.
   :derived-aliases (or derived-aliases #{})
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
   ;; Data-pattern clauses emitted by the equi-join unification /
   ;; value-bound fast paths (unify-inner-equijoin!, bind-col-value!).
   ;; These patterns ARE the join/filter — the projection-nullability
   ;; pass in translate-select must never rewrite them to get-else, or
   ;; the `:__null__` sentinel becomes joinable and NULL = NULL rows
   ;; reappear.
   :required-join-patterns (atom #{})
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

(defn- emit-ref-deref!
  "Emit a Datalog or-join that binds `pk-var` to the target-PK value of
   the ref entity-id `ref-eid-var`, handling the null-ref case.

   Pattern:
     (or-join [?ref-eid ?pk]
       (and [(= ?ref-eid :__null__)] [(ground :__null__) ?pk])
       (and [(not= ?ref-eid :__null__)]
            [(get-else $ ?ref-eid target-pk :__null__) ?pk]))

   The wrapper is necessary because get-else's entity arg must be a
   number / nil / lookup-ref — passing the `:__null__` sentinel keyword
   (which `col-var!` emits when the source row lacks the ref attr)
   would throw at search time."
  [ctx ref-eid-var pk-var target-pk-attr]
  (add-clause! ctx
               (list 'or-join [ref-eid-var pk-var]
                     (list 'and
                           [(list '= ref-eid-var :__null__)]
                           [(list 'ground :__null__) pk-var])
                     (list 'and
                           [(list 'not= ref-eid-var :__null__)]
                           [(list 'get-else '$ ref-eid-var target-pk-attr :__null__) pk-var]))))

(defn- emit-many-ref-array!
  "For `:db.cardinality/many :db.type/ref` columns: emit a function-
   binding that calls `fns/pg-many-ref-array` with the source entity
   (already-bound `evar`), the ref attr, and the target PK attr.
   Returns the var that's bound to the resulting PgArray.

   The fn does the array construction in pure Clojure (one
   `d/datoms` for the ref values, one per target for the PK lookup),
   bypassing Datalog's grouping mechanics. Empty PgArray for source
   entities with no ref values — matches PG's `int8[]` projection
   shape for an empty array column."
  [ctx evar ref-attr target-pk-attr out-var]
  (let [;; Bind the runtime fn once per ctx and reuse — multiple
        ;; many-ref columns in the same query share the symbol.
        param-key ::pg-many-ref-array-param
        fn-param (or (get @(:col->var ctx) param-key)
                     (let [p (symbol (str "?pg-many-ref-array"
                                          (swap! (:var-counter ctx) inc)))
                           f #'fns/pg-many-ref-array]
                       (swap! (:in-params ctx) conj p)
                       (swap! (:in-args ctx)   conj f)
                       (swap! (:col->var ctx) assoc param-key p)
                       p))]
    (add-clause! ctx [(list fn-param '$ evar ref-attr target-pk-attr) out-var])))

(defn ref-eid-var!
  "Get/create the logic variable bound to the *raw entity-id* a
   `:db.type/ref` attribute holds, bypassing the SQL-projection
   dereference that `col-var!` applies for ref columns.

   Used only by the JOIN-condition rewriter in `translate-join`, which
   unifies the right alias's entity-var with the ref's value (the
   target entity-id) — an optimization that turns `JOIN c ON p.fk =
   c.pk` into a single direct entity binding instead of two passes
   (deref + value equality).

   Forms accepted: same as `col-var!`. For non-ref attrs this returns
   the same var that `col-var!` does — there's nothing to dereference.

   Cached separately from `col-var!` (key `[alias-key attr :__eid__]`)
   so projection sites and JOIN sites can both fetch their respective
   binding without invalidating each other."
  [ctx attr]
  (cond
    (and (vector? attr) (= :db-id (first attr)))
    (entity-var! ctx (second attr))

    :else
    (let [alias-key (if (vector? attr) (nth attr 1) (namespace attr))
          resolved-attr (attr-of ctx attr)
          cache-key [alias-key resolved-attr :__eid__]
          cvars (:col->var ctx)]
      (or (get @cvars cache-key)
          (let [v (symbol (str "?" alias-key "_" (name resolved-attr) "_eid"))
                evar (entity-var! ctx alias-key)
                lj? (contains? @(:left-join-evars ctx) evar)]
            (swap! cvars assoc cache-key v)
            (if lj?
              (add-clause! ctx [evar resolved-attr v])
              (add-clause! ctx [(list 'get-else '$ evar resolved-attr :__null__) v]))
            (swap! (:nullable-vars ctx) conj v)
            v)))))

(defn- check-agg-alias-collision!
  "Targeted PG-compat sniff: real PG raises 42703 on any unresolved
   column reference. pgwire-datahike treats missing attrs as NULL by
   design (EAV semantics — see datahike.test.pg-server-test/test-
   semantic-errors), but that silence is harmful when the column name
   matches a SELECT-list aggregation alias: the user almost certainly
   meant HAVING (or a nested sub-query) and would otherwise see a
   silent zero-rows result.

   Triggered only for that exact pattern. Genuinely-missing columns
   continue to bind to the :__null__ sentinel."
  [ctx attr]
  (let [agg-set (:agg-aliases-warning-set ctx)
        col-name (cond
                   (and (vector? attr) (= :aliased (first attr))) (name (nth attr 2))
                   (keyword? attr) (name attr)
                   :else nil)]
    (when (and col-name agg-set
               (contains? agg-set (str/lower-case col-name))
               (not (get (:schema ctx) (if (vector? attr) (nth attr 2) attr))))
      (throw (ex-info "undefined column referenced in WHERE"
                      {:error  :undefined-column
                       :column col-name
                       :hint   (str "\"" col-name "\" is a SELECT-list "
                                    "aggregation alias and cannot be "
                                    "referenced in WHERE; use HAVING or "
                                    "wrap the query in a subquery")})))))

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

   `:db.type/ref` columns: when `ctx`'s `:ref-targets` map has an entry
   for the resolved attr, the returned var is the *target-PK value*
   (e.g. for `:order/customer` → the referenced customer's
   `:customer/id`), matching how a real PG FK column projects. Callers
   that need the raw entity-id (only the JOIN-condition rewriter) use
   `ref-eid-var!` instead.

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
    ;; Mirrors the regular keyword branch below: must unpack the
    ;; ref-targets entry's `[target-attr :many]` shape into a (target,
    ;; many?) pair, otherwise an aliased projection of an M2M ref
    ;; column passes the raw vector as a datalog attr and crashes
    ;; with "Bad format for attribute in pattern".
    (and (vector? attr) (= :aliased (first attr)))
    (let [alias-key (nth attr 1)
          kw (attr-of ctx attr)
          cache-key [alias-key kw]
          cvars (:col->var ctx)
          ref-target-entry (get (:ref-targets ctx) kw)
          [ref-target many?] (cond
                               (vector? ref-target-entry) [(first ref-target-entry) true]
                               (some? ref-target-entry)   [ref-target-entry false]
                               :else                       [nil false])]
      (or (get @cvars cache-key)
          (do
            (check-agg-alias-collision! ctx attr)
            (cond
              ;; M2M ref → emit array-projection fn (same as the
              ;; non-aliased branch).
              many?
              (let [arr-v (symbol (str "?" alias-key "_" (name kw) "_arr"))
                    evar (entity-var! ctx alias-key)]
                (emit-many-ref-array! ctx evar kw ref-target arr-v)
                (swap! cvars assoc cache-key arr-v)
                arr-v)

              :else
              (let [v (symbol (str "?" alias-key "_" (name kw)))
                    evar (entity-var! ctx alias-key)
                    lj? (contains? @(:left-join-evars ctx) evar)]
                (if lj?
                  (add-clause! ctx [evar kw v])
                  (add-clause! ctx [(list 'get-else '$ evar kw :__null__) v]))
                (swap! (:nullable-vars ctx) conj v)
                (if ref-target
                  (let [pk-v (symbol (str "?" alias-key "_" (name kw) "_pk"))]
                    (emit-ref-deref! ctx v pk-v ref-target)
                    (swap! (:nullable-vars ctx) conj pk-v)
                    (swap! cvars assoc cache-key pk-v)
                    pk-v)
                  (do (swap! cvars assoc cache-key v) v)))))))

    ;; Regular keyword :ns/col
    :else
    (let [alias-key (namespace attr)
          resolved-attr (attr-of ctx attr)
          cache-key [alias-key resolved-attr]
          cvars (:col->var ctx)
          ref-target-entry (get (:ref-targets ctx) resolved-attr)
          ;; ref-targets value can be either:
          ;;   target-attr           — :db.cardinality/one (deref to single PK)
          ;;   [target-attr :many]   — :db.cardinality/many (PgArray of PKs)
          [ref-target many?] (cond
                               (vector? ref-target-entry) [(first ref-target-entry) true]
                               (some? ref-target-entry)   [ref-target-entry false]
                               :else                       [nil false])]
      (or (get @cvars cache-key)
          (do
            (check-agg-alias-collision! ctx resolved-attr)
            (cond
              ;; :db.cardinality/many ref → PgArray of target PKs.
              ;; Bypasses get-else (which only returns one value for
              ;; multi-cardinality attrs) and the standard col-var!
              ;; flow entirely. The aggregation happens per-row in
              ;; the bound fn, so the surrounding query's row count
              ;; is unchanged.
              many?
              (let [arr-v (symbol (str "?" alias-key "_" (name resolved-attr) "_arr"))
                    evar (entity-var! ctx alias-key)]
                (emit-many-ref-array! ctx evar resolved-attr ref-target arr-v)
                (swap! cvars assoc cache-key arr-v)
                arr-v)

              :else
              (let [v (symbol (str "?" alias-key "_" (name resolved-attr)))
                    evar (entity-var! ctx alias-key)
                    lj? (contains? @(:left-join-evars ctx) evar)]
                (if lj?
                  (add-clause! ctx [evar resolved-attr v])
                  (add-clause! ctx [(list 'get-else '$ evar resolved-attr :__null__) v]))
                (swap! (:nullable-vars ctx) conj v)
                (if ref-target
                  (let [pk-v (symbol (str "?" alias-key "_" (name resolved-attr) "_pk"))]
                    (emit-ref-deref! ctx v pk-v ref-target)
                    (swap! (:nullable-vars ctx) conj pk-v)
                    (swap! cvars assoc cache-key pk-v)
                    pk-v)
                  (do (swap! cvars assoc cache-key v) v)))))))))

;; ---------------------------------------------------------------------------
;; Inner-equijoin unification
;;
;; `a.x = b.y` used to translate as two independent `get-else` bindings
;; plus an `(= ?ax ?by)` predicate. The engine cannot index a predicate,
;; so it materialises the full cross product of the two relations before
;; filtering — O(n²) time AND heap (an 8k-row self-join OOMed a 2 GB
;; heap; the unified form below runs it in ~18 ms). Emitting two plain
;; data patterns that share ONE logic var
;;
;;     [?a_eid :a/x ?j] [?b_eid :b/y ?j]
;;
;; lets the engine hash-join on ?j. SQL NULL semantics survive intact:
;; NULL is stored as an *absent attribute*, so a row with a NULL join
;; key produces no datom and can never match — exactly SQL's
;; `NULL = anything → UNKNOWN → filtered` for INNER joins. (OUTER joins
;; never reach this path; they go through the or-join machinery.)

(defn- plain-join-col
  "When `resolved` (a resolve-column result) denotes a plain data-pattern
   column — a keyword or [:aliased …] attr with no ref-target deref, not
   a derived-table alias (their materialised rows can carry the
   `:__null__` sentinel as a stored value, which would wrongly unify),
   and not an OUTER-JOIN right side — return `{:cache-key k :attr kw
   :evar ?e}`. Else nil."
  [ctx resolved]
  (let [[alias-key attr]
        (cond
          (keyword? resolved)                                   [(namespace resolved) (attr-of ctx resolved)]
          (and (vector? resolved) (= :aliased (first resolved))) [(nth resolved 1) (attr-of ctx resolved)]
          :else nil)]
    (when (and alias-key (keyword? attr)
               (nil? (get (:ref-targets ctx) attr))
               (not (contains? (:derived-aliases ctx) alias-key)))
      (let [evar (entity-var! ctx alias-key)]
        (when-not (contains? @(:left-join-evars ctx) evar)
          {:cache-key [alias-key attr] :attr attr :evar evar})))))

(defn unify-inner-equijoin!
  "Try to translate an INNER-JOIN equality between two plain columns as
   shared-variable data patterns (see comment above). Returns true when
   handled; nil when the caller must fall back to the predicate path
   (ref-deref columns, derived tables, expressions, both vars already
   bound)."
  [ctx l-resolved r-resolved]
  (when-let [l (plain-join-col ctx l-resolved)]
    (when-let [r (plain-join-col ctx r-resolved)]
      (let [cvars (:col->var ctx)
            l-cached (get @cvars (:cache-key l))
            r-cached (get @cvars (:cache-key r))]
        ;; Both columns already bound to distinct vars elsewhere — the
        ;; plain patterns couldn't share a var without rebinding one of
        ;; them; leave it to the predicate path.
        (when-not (and l-cached r-cached)
          (let [jv (or l-cached r-cached
                       (symbol (str "?join" (swap! (:var-counter ctx) inc))))]
            ;; If a side was already get-else-bound, the plain pattern
            ;; simply narrows it to rows where the datom exists and
            ;; matches — the sentinel value never appears in storage
            ;; for base tables, so `:__null__` rows drop out (correct
            ;; for INNER joins).
            (add-clause! ctx [(:evar l) (:attr l) jv])
            (add-clause! ctx [(:evar r) (:attr r) jv])
            (swap! (:required-join-patterns ctx) conj
                   [(:evar l) (:attr l) jv] [(:evar r) (:attr r) jv])
            (when-not l-cached (swap! cvars assoc (:cache-key l) jv))
            (when-not r-cached (swap! cvars assoc (:cache-key r) jv))
            true))))))

(defn bind-col-value!
  "Translate `col = <constant>` as a value-bound data pattern
   `[?eid :attr v]` — an indexable clause — instead of a get-else
   binding plus `(= ?col v)` predicate over every row. Only sound in a
   top-level conjunctive context (the caller guards that) and only
   emitted when the constant's runtime class matches the attribute's
   declared valueType, so pattern-equality can't diverge from
   predicate-equality on cross-type comparisons. Returns true when
   handled."
  [ctx resolved v]
  (when (and (some? v) (not (symbol? v)) (not (seq? v)) (not= :__null__ v))
    (when-let [c (plain-join-col ctx resolved)]
      (let [vtype (get-in (:schema ctx) [(:attr c) :db/valueType])
            match? (case vtype
                     :db.type/long    (instance? Long v)
                     :db.type/string  (instance? String v)
                     :db.type/boolean (instance? Boolean v)
                     :db.type/uuid    (instance? java.util.UUID v)
                     :db.type/keyword (keyword? v)
                     false)]
        (when match?
          (add-clause! ctx [(:evar c) (:attr c) v])
          (swap! (:required-join-patterns ctx) conj [(:evar c) (:attr c) v])
          true)))))

(defn bind-col-param!
  "Translate `col = $N` (extended-protocol parameter) as a data pattern
   `[?e :attr ?pN]` whose value var is `:in`-bound at Execute — the
   engine seeks the index with the bound value instead of scanning a
   get-else binding + equality predicate (this was why `pgbench -M
   prepared` point lookups were 10x slower than interpolated literals).
   SQL `col = NULL` must yield zero rows: the `(some? ?pN)` guard
   enforces that — a nil `:in` binding degrades the pattern itself to a
   scan, but the guard then rejects every row. Same soundness rules as
   bind-col-value! (top-level conjunct, plain column). Returns true
   when handled."
  [ctx resolved pvar]
  (when (symbol? pvar)
    (when-let [c (plain-join-col ctx resolved)]
      (add-clause! ctx [(:evar c) (:attr c) pvar])
      (swap! (:required-join-patterns ctx) conj [(:evar c) (:attr c) pvar])
      (add-clause! ctx [(list 'some? pvar)])
      true)))

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
  (let [lj-evars @(:left-join-evars ctx)
        required @(:required-join-patterns ctx)]
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
                   (not (contains? lj-evars (first match)))
                   ;; Nor equi-join unification patterns — they ARE the join.
                   (not (contains? required match)))
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
