(ns datahike.pg.shape
  "Structural shape matching for long-form SELECT statements.

   A companion to datahike.pg.classify: classify routes by the first
   few tokens; shape answers 'what does the rest of this SELECT look
   like?'. Used by sql/system-query?* to identify pgjdbc + Odoo
   catalog probes that can't be recognized by a leading keyword —
   their identifying signal lives deep in the projection list, the
   FROM clause, or buried qualified identifiers.

   This replaces the substring-match branches the classifier
   previously fell back to (str/includes? on lowercased SQL).
   Substring matching is safe when the needle is a distinctive
   SQL-only string like 'fk.conname as name', but it's a different
   model than the rest of the pipeline: a string literal containing
   that phrase would false-match. Tokenizing and matching on
   structural features (qualified references, AS aliases, function
   names) is immune to keyword-inside-string / keyword-inside-
   comment hostile inputs.

   API:
     (catalog-probe sql) → :get-fk-conname | :get-primary-keys
                         | :get-field-metadata | :empty-catalog | nil"
  (:require [clojure.string :as str]
            [datahike.pg.classify :as cls]))

;; ============================================================================
;; Token helpers
;; ============================================================================

(defn- bare-ident? [tok]
  (= :ident (:type tok)))

(defn- punct? [tok ^String text]
  (and tok (= :punct (:type tok)) (= text (:text tok))))

(defn- kw-text
  "Lowercase text of a bare ident; nil for other token types (keeps
   matching case-insensitive for SQL keywords while leaving quoted
   identifiers case-sensitive as PG specifies)."
  [tok]
  (when (bare-ident? tok) (str/lower-case (:text tok))))

(defn- kw=?
  [tok ^String s]
  (= s (kw-text tok)))

;; ============================================================================
;; Dotted-name reader — consumes ident [. ident [. ident]] and returns
;; the lowercase composite plus the index one past the last token. Only
;; bare idents participate; quoted identifiers stop the run (PG rule —
;; "Foo"."Bar" is a distinct identifier from foo.bar).
;; ============================================================================

(defn- dotted-name
  [toks ^long idx]
  (let [t0 (nth toks idx nil)]
    (if-not (bare-ident? t0)
      [nil idx]
      (loop [parts [(str/lower-case (:text t0))]
             i (inc idx)]
        (let [dot (nth toks i nil)
              nxt (nth toks (inc i) nil)]
          (if (and (punct? dot ".") (bare-ident? nxt))
            (recur (conj parts (str/lower-case (:text nxt)))
                   (+ i 2))
            [(str/join "." parts) i]))))))

;; ============================================================================
;; Single-pass shape summary
;; ============================================================================

(defn summarize
  "Walk a token stream once and extract the structural features our
   catalog probes key off. Comments are ignored — they carry no
   structural signal and already come through as :comment tokens.

   Returns:
     {:select?      boolean — the stream starts with SELECT
      :qrefs        #{\"fk.conname\" \"pg_catalog.pg_class\" …}
                    — every dotted name referenced anywhere.
      :idents       #{\"pg_constraint\" \"pg_class\" …}
                    — every bare ident, lowercased.
      :fn-names     #{\"format_type\" …}
                    — every ident (bare or dotted) directly followed
                    by `(`.
      :as-aliases   #{[\"fk.conname\" \"name\"] …}
                    — every `<qname> AS <ident>` pair.}"
  [toks]
  (let [toks (vec toks)
        n (count toks)]
    (loop [i 0
           select? false
           qrefs #{}
           idents #{}
           fn-names #{}
           as-aliases #{}]
      (if (>= i n)
        {:select? select?
         :qrefs qrefs
         :idents idents
         :fn-names fn-names
         :as-aliases as-aliases}
        (let [t (nth toks i)]
          (cond
            (not (bare-ident? t))
            (recur (inc i) select? qrefs idents fn-names as-aliases)

            :else
            (let [[qname nxt] (dotted-name toks i)
                  dotted? (str/includes? qname ".")
                  next-tok (nth toks nxt nil)
                  alias-tok (nth toks (inc nxt) nil)
                  select?' (or select?
                               (and (not dotted?) (= qname "select")))
                  qrefs' (cond-> qrefs
                           dotted? (conj qname))
                  idents' (cond-> idents
                            (not dotted?) (conj qname))
                  fn-names' (cond-> fn-names
                              (punct? next-tok "(") (conj qname))
                  as-aliases' (cond-> as-aliases
                                (and (kw=? next-tok "as")
                                     (bare-ident? alias-tok))
                                (conj [qname (str/lower-case (:text alias-tok))]))]
              (recur nxt select?' qrefs' idents' fn-names' as-aliases'))))))))

;; ============================================================================
;; Probe predicates
;; ============================================================================

(def ^:private empty-catalog-tables
  "Catalog / introspection tables we don't materialize. A SELECT that
   references any of these gets an empty-row result matching the
   outer SELECT's projection shape. pg_index, pg_attrdef and
   pg_constraint are deliberately NOT here — they flow through the
   real catalog path since we synthesize rows for them."
  #{"pg_depend" "pg_description"
    "pg_inherits" "pg_rewrite" "pg_trigger"
    "pg_stat_user_tables" "pg_stat_activity"
    "pg_locks" "pg_settings"})

(def ^:private empty-catalog-fns
  "PG system functions that route to the empty-catalog handler. The set
   is empty now that pg_get_indexdef / pg_get_constraintdef lower to
   real catalog joins (see datahike.pg.sql.expr) and the comment-lookup
   pair stub to NULL. Kept as a hook for any future fns that need the
   shape-level shortcut."
  #{})

(defn- fk-conname?
  "Odoo's post-add-foreign-key lookup:
     SELECT fk.conname AS name FROM pg_constraint fk WHERE …"
  [{:keys [as-aliases idents]}]
  (and (contains? as-aliases ["fk.conname" "name"])
       (contains? idents "pg_constraint")))

(defn- primary-keys?
  "pgjdbc DatabaseMetaData.getPrimaryKeys — a wide join with the
   distinctive `information_schema._pg_expandarray` helper and two
   named result columns (result.key_seq, result.pk_name)."
  [{:keys [qrefs]}]
  (and (contains? qrefs "result.key_seq")
       (contains? qrefs "result.pk_name")
       (contains? qrefs "information_schema._pg_expandarray")))

(defn- field-metadata?
  "pgjdbc PgResultSetMetaData.fetchFieldMetaData — a 5-way JOIN whose
   projection list starts with `c.oid, a.attnum, a.attname` against
   `pg_catalog.pg_class c` and `pg_catalog.pg_attribute a`."
  [{:keys [qrefs]}]
  (and (contains? qrefs "c.oid")
       (contains? qrefs "a.attnum")
       (contains? qrefs "a.attname")
       (contains? qrefs "pg_catalog.pg_class")
       (contains? qrefs "pg_catalog.pg_attribute")))

(defn- empty-catalog?
  "Any SELECT that touches a catalog table or function we don't
   implement. Checked LAST — the specific probes above would also
   satisfy this predicate (they reference pg_constraint, pg_class,
   etc.) and must win."
  [{:keys [idents qrefs fn-names]}]
  (or (boolean (some empty-catalog-tables idents))
      (boolean (some empty-catalog-tables
                     (map #(last (str/split % #"\.")) qrefs)))
      (boolean (some empty-catalog-fns fn-names))))

;; ============================================================================
;; Public entry
;; ============================================================================

(defn catalog-probe
  "If sql is a SELECT that matches a known catalog-probe shape,
   return the :kind keyword the dispatch in server.clj expects; else
   nil.

   Order of checks (most-specific first) matters: every named probe
   would also trigger :empty-catalog since they all reference
   catalog tables. We want the richer classification to win."
  [^String sql]
  (let [shape (summarize (cls/tokenize sql))]
    (when (:select? shape)
      (cond
        (field-metadata? shape) :get-field-metadata
        (primary-keys?   shape) :get-primary-keys
        (fk-conname?     shape) :get-fk-conname
        (empty-catalog?  shape) :empty-catalog))))
