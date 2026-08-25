(ns datahike.pg.dump
  "Dump a pg-datahike database to portable PostgreSQL SQL.

   This is the inverse of our `pg_dump --inserts | psql` import path.
   It walks a Datahike connection's schema, reverse-engineers the SQL
   DDL we originally translated from, and emits a sequence of SQL
   strings that can be replayed against either pg-datahike OR a real
   PostgreSQL server.

   Output layout (mirrors `pg_dump`'s):

     -- Header
     SET client_encoding = 'UTF8';

     -- DDL: CREATE TABLE for every table + CREATE SEQUENCE for
     -- every named sequence. PRIMARY KEY / UNIQUE are inline; FKs
     -- are deferred to the bottom so data load order doesn't matter.
     CREATE TABLE \"customer\" (...);
     CREATE TABLE \"order\" (...);
     CREATE SEQUENCE \"customer_id_seq\";

     -- Data
     COPY \"customer\" (...) FROM stdin;
     ...
     \\.

     -- FKs (or any other ALTER TABLE ADD CONSTRAINT) — last, so
     -- forward / cyclic references load cleanly.
     ALTER TABLE \"order\" ADD CONSTRAINT order_customer_id_fkey
       FOREIGN KEY (customer_id) REFERENCES customer (id);

     -- Sequence current values
     SELECT pg_catalog.setval('customer_id_seq', 42, true);

   Two output shapes via :format:
     :inserts (default) — INSERT INTO ... VALUES (...); per row.
                          More universally re-loadable; works against
                          tier-1 of pg-datahike's own import path.
     :copy              — COPY ... FROM stdin (text format) per row.
                          More compact; requires the loader to drive
                          the COPY-IN sub-protocol (psql does this
                          automatically via \\copy / pg_dump's stdin
                          path).

   What we capture today:
     - column type (driven by :db/valueType + :pg/type override)
     - PRIMARY KEY (from :db.unique/identity)
     - UNIQUE (from :db.unique/value)
     - NOT NULL (from :pg/not-null annotation entity)
     - DEFAULT (from :pg/default-kind + :pg/default-value)
     - FOREIGN KEY (from :datahike.pg/references hint)
     - CHECK (from :pg/check-* entities)
     - sequences (from :__seq__/* entities)
     - all data rows (filtered by the :<table>/db-row-exists marker)

   What we DON'T capture (yet):
     - VIEW / MATERIALIZED VIEW definitions
     - INDEXes (we don't materialize them; PG would create them
       fresh on the load side)
     - TRIGGERs / functions (we don't have these)
     - COMMENT ON
     - GRANT / REVOKE / OWNER (no role system)

   Public API:
     (dump conn)                            ; lazy seq of SQL strings
     (dump conn {:format :copy})
     (dump conn {:sections #{:schema}})     ; or #{:data}, default :all
     (dump conn {:exclude-tables #{\"x\"}})  ; skip these table namespaces"
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.schema :as pgs]))

;; ----------------------------------------------------------------------------
;; Schema introspection
;; ----------------------------------------------------------------------------

(def ^:private internal-namespace-prefixes
  "Namespace prefixes (matched as `(or (= ns prefix) (starts-with? ns
   prefix-with-dot))`) that we hide from dumps. These are Datahike
   and pg-datahike internal — schema attribute entities, validation
   metadata, sequences, branching state, etc. — not user tables."
  #{"db"           ;; :db/ident, :db/valueType, db.entity/*, db.valid/*, db.secondary/*, …
    "datahike.pg"  ;; pg-datahike schema hints
    "pg"           ;; :pg/type, :pg/not-null, :pg/check-*, :pg/default-*
    "datahike"     ;; :datahike/* (versioning, branching, …)
    "dh.ref"       ;; :dh.ref/* — datahike cross-database references (system schema)
    "__seq__"})    ;; sequences — handled separately

(defn- internal-namespace? [^String ns]
  (boolean
   (some (fn [prefix]
           (or (= ns prefix)
               (str/starts-with? ns (str prefix "."))))
         internal-namespace-prefixes)))

(defn- attr-entity
  "Pull the full entity-map for a schema attribute (the reverse-side
   data: :pg/type, :pg/not-null, :datahike.pg/references, etc.).
   Returns a map of all keys present on the attribute's entity."
  [db ident-kw]
  (let [eid (ffirst (d/q '{:find [?e]
                           :in [$ ?ident]
                           :where [[?e :db/ident ?ident]]}
                         db ident-kw))]
    (when eid
      (into {} (d/entity db eid)))))

(defn- collect-tables
  "Walk the schema: group attribute keywords by namespace, drop the
   internal ones, return [{:table str :columns [{:name str :ident kw
   :spec map :ent map} ...]}].

   Column ordering follows `pgs/column-order-from-db` (which orders
   by entity-id == CREATE TABLE order), falling back to alphabetical
   if no per-table order is recoverable from the db. Preserving
   declaration order matters for `SELECT *` callers and for
   bidirectional roundtrip equality.

   The `:<table>/db-row-exists` row-marker is dropped — internal
   bookkeeping pg-datahike re-adds on CREATE TABLE.

   `:db.type/tuple` attrs are dropped — those represent composite
   primary keys, which Datahike auto-populates from their component
   attributes. They aren't actual SQL columns; emitting them would
   produce a phantom `<table>_pkey` column on the target."
  [db]
  (let [schema (dbi/-schema db)
        by-ns (->> schema
                   keys
                   (filter keyword?)
                   (remove #(internal-namespace? (namespace %)))
                   (group-by namespace))
        non-tuple? (fn [ident]
                     (not= :db.type/tuple (:db/valueType (get schema ident))))]
    (for [[table-ns idents] (sort by-ns)
          :let [filtered (->> idents
                              (remove #(= "db-row-exists" (name %)))
                              (filter non-tuple?))
                ident-by-name (into {} (map (juxt name identity)) filtered)
                ordered-names (or (some-> (pgs/column-order-from-db db table-ns) seq)
                                  (sort (map name filtered)))
                cols (mapv (fn [n]
                             (let [ident (get ident-by-name n)]
                               {:name (name ident)
                                :ident ident
                                :spec (get schema ident)
                                :ent (attr-entity db ident)}))
                           (filter ident-by-name ordered-names))]
          :when (seq cols)]
      {:table table-ns
       :columns cols})))

;; ----------------------------------------------------------------------------
;; Type reverse-mapping
;; ----------------------------------------------------------------------------

(defn- scalar-vtype->sql-type
  "Reverse-map a single :db/valueType to a PG scalar type. Used
   internally; callers above this should use vtype->sql-type which
   also handles enum/domain/array/cardinality-many wrappers."
  [vtype]
  (case vtype
    :db.type/long      "bigint"
    :db.type/bigint    "numeric"
    :db.type/bigdec    "numeric"
    :db.type/float     "real"
    :db.type/double    "double precision"
    :db.type/string    "text"
    :db.type/boolean   "boolean"
    :db.type/instant   "timestamp"
    :db.type/uuid      "uuid"
    :db.type/keyword   "text"
    :db.type/symbol    "text"
    :db.type/ref       "bigint"
    "text"))

(defn- vtype->sql-type
  "Reverse-map a Datahike :db/valueType to a PostgreSQL type name.
   :datahike.pg/enum-of and :datahike.pg/domain-of win first — when
   present, the column was declared with an ENUM or DOMAIN and we
   must emit the type name (not the lowered base) so the dump
   replays into a target that knows the type. :pg/type override
   (set for JSONB and array-of-anything) wins next; :pg/array-elem
   turns the result into `T[]`. Finally, `:db.cardinality/many` (a
   native Datahike feature with no SQL equivalent for scalars) is
   lowered to `T[]` so the array-aware INSERT branch can render the
   value as a PG array literal."
  [{:keys [spec ent]}]
  (let [vtype (:db/valueType spec)
        cardinality-many? (= :db.cardinality/many (:db/cardinality spec))
        enum-of (:datahike.pg/enum-of ent)
        domain-of (:datahike.pg/domain-of ent)
        pg-type (:pg/type ent)
        array-elem (:pg/array-elem ent)
        scalar (cond
                 enum-of    enum-of
                 domain-of  domain-of
                 ;; :pg/type carries the OID-registry name (e.g. "int4",
                 ;; "int2" for narrow integer columns). Emit the canonical
                 ;; SQL spelling pg_dump uses so the dump replays cleanly
                 ;; and diffs byte-for-byte against a real pg_dump.
                 pg-type    (case pg-type
                              "int4" "integer"
                              "int2" "smallint"
                              "int8" "bigint"
                              pg-type)
                 array-elem (str (scalar-vtype->sql-type array-elem) "[]")
                 :else      (scalar-vtype->sql-type vtype))]
    (if cardinality-many?
      (str scalar "[]")
      scalar)))

;; ----------------------------------------------------------------------------
;; Identifier quoting
;; ----------------------------------------------------------------------------

(def ^:private pg-reserved-words
  "Subset of PG reserved words we always quote. Not exhaustive — when
   in doubt the dump output gets a quoted ident, which is always safe
   except for the small gotcha that `\"col\"` and `\"COL\"` are
   distinct identifiers in PG (lowercase-ident-folding only happens
   for unquoted names). We emit the name as STORED: for a database
   created by a current pg-datahike that is already folded, but a
   database predating the fold keeps `MixedCase` and a Datalog-native
   one keeps `firstName`. Quoting verbatim is what makes those restore
   unchanged — at the cost that dump/restore does NOT normalise case."
  #{"order" "user" "table" "select" "from" "where" "group"
    "by" "and" "or" "not" "in" "is" "null" "true" "false"
    "primary" "key" "foreign" "references" "unique" "check"
    "default" "create" "alter" "drop" "as" "on"})

(defn- quote-ident [^String s]
  ;; Always quote: simpler + always correct. PG's lower-case folding
  ;; for unquoted idents would otherwise mangle the case for clients
  ;; that round-trip through pg_dump.
  (str "\"" (str/replace s #"\"" "\"\"") "\""))

;; ----------------------------------------------------------------------------
;; CREATE TABLE
;; ----------------------------------------------------------------------------

(defn- column-clause
  "Render a single column definition for CREATE TABLE."
  [{:keys [name spec ent] :as col}]
  (let [type-str (vtype->sql-type col)
        unique-id? (= :db.unique/identity (:db/unique spec))
        unique-val? (= :db.unique/value (:db/unique spec))
        not-null? (or unique-id? (:pg/not-null ent))
        default (when-let [k (:pg/default-kind ent)]
                  (case k
                    :literal (let [v (:pg/default-value ent)]
                               (cond
                                 (string? v) (str "'" (str/replace v #"'" "''") "'")
                                 (nil? v) "NULL"
                                 :else (str v)))
                    :bit (str "B'" (:pg/default-value ent) "'")
                    :bit-coerced (str "'" (:pg/default-value ent) "'")
                    :now "now()"
                    :nextval (str "nextval('" (:pg/default-arg ent) "')")
                    nil))]
    (str (quote-ident name)
         " " type-str
         (when not-null? " NOT NULL")
         (when default (str " DEFAULT " default))
         (when unique-id? " PRIMARY KEY")
         (when unique-val? " UNIQUE"))))

(defn- emit-create-table
  "Emit a single CREATE TABLE statement (string) without trailing
   constraints — those go in the post-data ALTER TABLE section."
  [{:keys [table columns]}]
  (str "CREATE TABLE " (quote-ident table) " (\n"
       (str/join ",\n"
                 (for [col columns]
                   (str "    " (column-clause col))))
       "\n);"))

;; ----------------------------------------------------------------------------
;; Sequences
;; ----------------------------------------------------------------------------

(defn- collect-sequences
  "Find every named sequence and its current value. pg-datahike
   stores each sequence as an entity with :__seq__/name +
   :__seq__/value + :__seq__/increment."
  [db]
  (->> (d/q '{:find [?n ?v ?inc]
              :where [[?e :__seq__/name ?n]
                      [?e :__seq__/value ?v]
                      [?e :__seq__/increment ?inc]]}
            db)
       sort
       (mapv (fn [[n v inc]]
               {:name n :value v :increment inc}))))

(defn- emit-create-sequence [{:keys [name increment]}]
  (str "CREATE SEQUENCE " (quote-ident name)
       (when (and increment (not= 1 increment))
         (str " INCREMENT BY " increment))
       ";"))

(defn- emit-setval [{:keys [name value]}]
  ;; `is_called = true` → next nextval returns value+increment.
  (str "SELECT pg_catalog.setval('" name "', " value ", true);"))

;; ----------------------------------------------------------------------------
;; ENUMs and DOMAINs (registry entities under :datahike.pg.enum/* and
;; :datahike.pg.domain/*). Emitted ahead of CREATE TABLE so column-type
;; references resolve at replay time.
;; ----------------------------------------------------------------------------

(defn- collect-enums
  "Walk the enum registry. Returns [{:name str :values [str ...]}]
   sorted by name for stable dump output."
  [db]
  (->> (d/q '{:find [?n ?vs-ord]
              :where [[?e :datahike.pg.enum/name ?n]
                      [?e :datahike.pg.enum/values-ordered ?vs-ord]]}
            db)
       (sort-by first)
       (mapv (fn [[n vs-ord]]
               {:name n
                :values (str/split (or vs-ord "") #"\n")}))))

(defn- emit-create-enum
  [{:keys [name values]}]
  (str "CREATE TYPE " (quote-ident name) " AS ENUM ("
       (str/join ", " (map #(str "'" (str/replace % #"'" "''") "'") values))
       ");"))

(defn- collect-domains
  "Walk the domain registry. Returns
   [{:name str :base-type str :base-args [...] :check-name str|nil
     :check-expr str|nil :not-null bool :default-raw str|nil}]."
  [db]
  (let [results (d/q '{:find [?e ?n]
                       :where [[?e :datahike.pg.domain/name ?n]]}
                     db)]
    (->> results
         (sort-by second)
         (mapv (fn [[eid n]]
                 (let [ent (into {} (d/entity db eid))]
                   {:name n
                    :base-type (:datahike.pg.domain/base-type ent)
                    :base-args (some-> (:datahike.pg.domain/base-args ent) sort vec)
                    :check-name (:datahike.pg.domain/check-name ent)
                    :check-expr (:datahike.pg.domain/check-expr ent)
                    :not-null (:datahike.pg.domain/not-null ent)
                    :default-raw (:datahike.pg.domain/default-raw ent)}))))))

(defn- emit-create-domain
  [{:keys [name base-type base-args check-name check-expr not-null default-raw]}]
  (let [base (str base-type
                  (when (seq base-args)
                    (str "(" (str/join "," base-args) ")")))]
    (str "CREATE DOMAIN " (quote-ident name) " AS " base
         (when not-null " NOT NULL")
         (when default-raw (str " DEFAULT " default-raw))
         (when check-expr
           (str (when check-name (str " CONSTRAINT " (quote-ident check-name)))
                " CHECK (" check-expr ")"))
         ";")))

;; ----------------------------------------------------------------------------
;; Foreign keys (post-data)
;; ----------------------------------------------------------------------------

(defn- collect-fks
  "For every column with `:datahike.pg/references`, collect a
   FK descriptor."
  [tables]
  (for [{:keys [table columns]} tables
        {:keys [name ident ent]} columns
        :let [target-ns (:datahike.pg/references ent)]
        :when target-ns]
    {:table table
     :col name
     :target-table (clojure.core/name target-ns)
     :target-col "id"  ;; pg-datahike convention — see schema/derive-fk-target
     :constraint (str table "_" name "_fkey")}))

(defn- emit-fk-constraint [{:keys [table col target-table target-col constraint]}]
  (str "ALTER TABLE " (quote-ident table)
       " ADD CONSTRAINT " (quote-ident constraint)
       " FOREIGN KEY (" (quote-ident col) ")"
       " REFERENCES " (quote-ident target-table)
       " (" (quote-ident target-col) ");"))

;; ----------------------------------------------------------------------------
;; Data emission
;; ----------------------------------------------------------------------------

(defn- table-rows
  "Pull all rows for `table`.

   Row discovery: SQL-created tables carry a `:<table>/db-row-exists`
   marker attribute that pg-datahike sets at INSERT time. Native
   Datahike databases (no SQL layer) don't have it. So we use the
   marker if present; otherwise we discover entity-ids by 'has any
   attribute in this table's namespace' — broader but works for any
   schema."
  [db table cols]
  (let [marker-attr (keyword table "db-row-exists")
        marker-present? (contains? (dbi/-schema db) marker-attr)
        col-idents (mapv :ident cols)
        eids (if marker-present?
               (mapv first
                     (sort (d/q '{:find [?e]
                                  :in [$ ?marker]
                                  :where [[?e ?marker true]]}
                                db marker-attr)))
               ;; Native fallback: any entity that has at least one
               ;; column attr from this table. Schema-attribute
               ;; entities (which also live in the db) are filtered
               ;; out by the column query — they have :db/ident and
               ;; :db/valueType, never the user's column attrs.
               (sort
                (into #{}
                      (mapcat (fn [attr]
                                (mapv first
                                      (d/q '{:find [?e]
                                             :in [$ ?a]
                                             :where [[?e ?a]]}
                                           db attr))))
                      col-idents)))]
    (mapv (fn [eid]
            (mapv (fn [{:keys [ident spec]}]
                    (let [many? (= :db.cardinality/many (:db/cardinality spec))
                          values (mapv first
                                       (d/q '{:find [?v]
                                              :in [$ ?e ?a]
                                              :where [[?e ?a ?v]]}
                                            db eid ident))]
                      (cond
                        ;; cardinality-many: collect ALL values into a vec
                        ;; so the array-aware formatter can render `'{a,b}'`.
                        many? (when (seq values) (vec values))
                        ;; cardinality-one: single value (or nil).
                        :else (first values))))
                  cols))
          eids)))

(defn- escape-array-element
  "Render one element of a PG array literal `'{...}'`. Strings are
   double-quoted with backslash-escapes; numbers/bools are bare;
   nil is the literal NULL token."
  [v]
  (cond
    (nil? v) "NULL"
    (boolean? v) (if v "t" "f")
    (number? v) (str v)
    (string? v)
    (str "\""
         (-> v
             (str/replace "\\" "\\\\")
             (str/replace "\"" "\\\""))
         "\"")
    (instance? java.util.Date v)
    (str "\""
         (.format (java.time.format.DateTimeFormatter/ofPattern
                   "yyyy-MM-dd HH:mm:ss.SSSXXX"
                   java.util.Locale/ROOT)
                  (.atZone (.toInstant ^java.util.Date v)
                           (java.time.ZoneId/of "UTC")))
         "\"")
    (instance? java.util.UUID v) (str v)
    (keyword? v) (subs (str v) 1)
    :else (str v)))

(defn- format-value-for-insert
  "Render a Clojure value as a SQL literal for an INSERT clause."
  [v col]
  (cond
    (nil? v) "NULL"
    ;; cardinality-many — render as PG array literal `'{a,b,c}'`.
    ;; The column's value is a vector (or nil if the entity has no
    ;; values for this attr).
    (and (vector? v)
         (= :db.cardinality/many (-> col :spec :db/cardinality)))
    (str "'{" (str/join "," (map escape-array-element v)) "}'")
    (boolean? v) (if v "true" "false")
    (number? v) (str v)
    (string? v) (str "'" (str/replace v #"'" "''") "'")
    (instance? java.util.Date v)
    (str "'" (.format (java.time.format.DateTimeFormatter/ofPattern
                       "yyyy-MM-dd HH:mm:ss.SSSXXX"
                       java.util.Locale/ROOT)
                      (.atZone (.toInstant ^java.util.Date v)
                               (java.time.ZoneId/of "UTC"))) "'")
    (instance? java.util.UUID v)
    (str "'" v "'::uuid")
    (keyword? v) (str "'" (subs (str v) 1) "'")
    :else (str "'" (str/replace (str v) #"'" "''") "'")))

(defn- emit-insert
  "Render one INSERT INTO ... VALUES ...; statement."
  [table cols row]
  (str "INSERT INTO " (quote-ident table)
       " (" (str/join ", " (map (comp quote-ident :name) cols)) ")"
       " VALUES (" (str/join ", " (map (fn [v c] (format-value-for-insert v c))
                                       row cols)) ");"))

(defn- escape-text-field
  "Apply PG COPY text-format escaping: backslash, tab, newline, CR
   are backslash-escaped. NULL → \\N."
  [v col]
  (cond
    (nil? v) "\\N"
    (boolean? v) (if v "t" "f")
    (instance? java.util.Date v)
    (.format (java.time.format.DateTimeFormatter/ofPattern
              "yyyy-MM-dd HH:mm:ss.SSSXXX"
              java.util.Locale/ROOT)
             (.atZone (.toInstant ^java.util.Date v)
                      (java.time.ZoneId/of "UTC")))
    :else
    (-> (str v)
        (str/replace "\\" "\\\\")
        (str/replace "\t" "\\t")
        (str/replace "\n" "\\n")
        (str/replace "\r" "\\r"))))

(defn- emit-copy-block
  "Render a `COPY t (cols) FROM stdin;` + body + `\\.\n` block as one
   string. Mimics pg_dump's default emission shape."
  [table cols rows]
  (let [header (str "COPY " (quote-ident table)
                    " (" (str/join ", " (map (comp quote-ident :name) cols)) ")"
                    " FROM stdin;")
        body (str/join
              "\n"
              (for [row rows]
                (str/join "\t" (map (fn [v c] (escape-text-field v c)) row cols))))]
    (if (seq rows)
      (str header "\n" body "\n\\.")
      (str header "\n\\."))))

;; ----------------------------------------------------------------------------
;; Top-level
;; ----------------------------------------------------------------------------

(defn dump
  "Dump a pg-datahike database to a lazy seq of SQL strings. Each
   element is one SQL statement (or one COPY-block, terminating in
   `\\.`); writers can `(str/join \"\\n\" (dump conn))` to get the
   full file contents."
  ([conn]
   (dump conn {}))
  ([conn {:keys [format sections exclude-tables]
          :or {format :inserts
               sections #{:schema :data}}}]
   (let [db (d/db conn)
         tables (cond->> (collect-tables db)
                  (seq exclude-tables)
                  (remove #(contains? exclude-tables (:table %))))
         seqs (collect-sequences db)
         fks (collect-fks tables)
         schema? (contains? sections :schema)
         data?   (contains? sections :data)]
     (concat
      ;; Header
      ["-- pg-datahike dump"
       "SET client_encoding = 'UTF8';"
       "SET standard_conforming_strings = on;"]

      ;; ENUMs and DOMAINs first — CREATE TABLE may reference them.
      (when schema?
        (let [enums (collect-enums db)
              domains (collect-domains db)]
          (concat
           (when (seq enums)
             (concat [""] (map emit-create-enum enums)))
           (when (seq domains)
             (concat [""] (map emit-create-domain domains))))))

      ;; CREATE TABLE
      (when schema?
        (concat
         (mapcat (fn [t]
                   ["" (str "-- Table: " (quote-ident (:table t)))
                    (emit-create-table t)])
                 tables)
         (when (seq seqs)
           (concat [""] (map emit-create-sequence seqs)))))

      ;; Data
      (when data?
        (mapcat
         (fn [{:keys [table columns]}]
           (let [rows (table-rows db table columns)]
             (concat
              ["" (str "-- Data: " (quote-ident table)
                       " (" (count rows) " rows)")]
              (case format
                :copy
                [(emit-copy-block table columns rows)]
                :inserts
                (mapv #(emit-insert table columns %) rows)))))
         tables))

      ;; Constraints (post-data so cyclic FKs are fine)
      (when (and schema? (seq fks))
        (concat
         ["" "-- Constraints"]
         (map emit-fk-constraint fks)))

      ;; Sequence values (post-data so setval reflects what we just
      ;; loaded — matches pg_dump's behaviour)
      (when (and schema? (seq seqs))
        (concat
         ["" "-- Sequence values"]
         (map emit-setval seqs)))))))

(defn dump-to-string
  "Convenience: realise the lazy seq into a single SQL string."
  ([conn] (dump-to-string conn {}))
  ([conn opts]
   (str/join "\n" (dump conn opts))))
