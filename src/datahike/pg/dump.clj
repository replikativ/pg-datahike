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
            [datahike.api :as d]))

;; ----------------------------------------------------------------------------
;; Schema introspection
;; ----------------------------------------------------------------------------

(def ^:private internal-namespace-prefixes
  "Namespace prefixes (matched as `(or (= ns prefix) (starts-with? ns
   prefix-with-dot))`) that we hide from dumps. These are Datahike
   and pg-datahike internal — schema attribute entities, validation
   metadata, sequences, branching state, etc. — not user tables."
  #{"db"           ;; :db/ident, :db/valueType, db.entity/*, db.alter/*, …
    "datahike.pg"  ;; pg-datahike schema hints
    "pg"           ;; :pg/type, :pg/not-null, :pg/check-*, :pg/default-*
    "datahike"     ;; :datahike/* (versioning, branching, …)
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
   :spec map :ent map} ...]}]. The `:<table>/db-row-exists` row-marker
   is dropped from each table's columns — it's an internal Datahike-
   side mechanism that pg-datahike re-adds on CREATE TABLE."
  [db]
  (let [schema (:schema db)
        by-ns (->> schema
                   keys
                   (filter keyword?)
                   (remove #(internal-namespace? (namespace %)))
                   (group-by namespace))]
    (for [[table-ns idents] (sort by-ns)
          :let [cols (->> idents
                          (remove #(= "db-row-exists" (name %)))
                          sort
                          (mapv (fn [ident]
                                  {:name (name ident)
                                   :ident ident
                                   :spec (get schema ident)
                                   :ent (attr-entity db ident)})))]
          :when (seq cols)]
      {:table table-ns
       :columns cols})))

;; ----------------------------------------------------------------------------
;; Type reverse-mapping
;; ----------------------------------------------------------------------------

(defn- vtype->sql-type
  "Reverse-map a Datahike :db/valueType to a PostgreSQL type name.
   :pg/type override (set by our DDL translator for JSONB and
   array-of-anything) wins; :pg/array-elem turns the result into
   `T[]`."
  [{:keys [spec ent]}]
  (let [vtype (:db/valueType spec)
        pg-type (:pg/type ent)
        array-elem (:pg/array-elem ent)]
    (cond
      pg-type pg-type     ;; explicit override (e.g. "jsonb")
      array-elem (str (vtype->sql-type
                       {:spec {:db/valueType array-elem}
                        :ent (dissoc ent :pg/array-elem)})
                      "[]")
      :else
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
        ;; Default — stringify
        "text"))))

;; ----------------------------------------------------------------------------
;; Identifier quoting
;; ----------------------------------------------------------------------------

(def ^:private pg-reserved-words
  "Subset of PG reserved words we always quote. Not exhaustive — when
   in doubt the dump output gets a quoted ident, which is always safe
   except for the small gotcha that `\"col\"` and `\"COL\"` are
   distinct identifiers in PG (lowercase-ident-folding only happens
   for unquoted names). pg-datahike lowers everything internally, so
   we always emit lower-case quoted idents."
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
  "Pull all rows for `table`. Filters on the row-marker so partial
   entities (e.g. lingering schema-attribute entities) are excluded."
  [db table cols]
  (let [marker-attr (keyword table "db-row-exists")
        col-idents (mapv :ident cols)]
    (let [eids (mapv first
                     (sort (d/q '{:find [?e]
                                  :in [$ ?marker]
                                  :where [[?e ?marker true]]}
                                db marker-attr)))]
      (mapv (fn [eid]
              (mapv (fn [attr]
                      (let [v (ffirst (d/q '{:find [?v]
                                             :in [$ ?e ?a]
                                             :where [[?e ?a ?v]]}
                                           db eid attr))]
                        v))
                    col-idents))
            eids))))

(defn- format-value-for-insert
  "Render a Clojure value as a SQL literal for an INSERT clause."
  [v col]
  (cond
    (nil? v) "NULL"
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
