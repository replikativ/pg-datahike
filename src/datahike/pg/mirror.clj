(ns datahike.pg.mirror
  "Read-only shadow databases fed by a PostgreSQL logical publication."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.catalog.objects :as catalog-objects]
            [datahike.pg.constraints.unique :as unique]
            [datahike.pg.jsonb :as jsonb]
            [datahike.pg.schema :as pgs]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.ctx :as ctx]
            [datahike.pg.sql.rewrite :as rewrite]
            [datahike.pg.types :as types])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandler PgWireServer$QueryResult]
           [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel OverlappingFileLockException]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardOpenOption]
           [java.sql Connection DriverManager PreparedStatement ResultSet SQLException Types]
           [java.util Properties UUID]
           [java.util.concurrent TimeUnit]
           [org.postgresql PGConnection]
           [org.postgresql.copy CopyDual]
           [org.postgresql.core BaseConnection]
           [org.postgresql.replication LogSequenceNumber]
           [org.postgresql.util PSQLException]))

(def ^:private sql-types
  {16 "boolean" 17 "bytea" 20 "bigint" 21 "smallint" 23 "integer" 25 "text"
   114 "json" 700 "real" 701 "double precision" 1009 "text[]"
   1042 "bpchar" 1043 "varchar" 1082 "date" 1083 "time" 1114 "timestamp"
   1184 "timestamptz" 1266 "timetz" 1700 "numeric" 2950 "uuid"
   3614 "tsvector" 3802 "jsonb"})

(defn- require! [condition message data]
  (when-not condition
    (throw (ex-info message data))))

(defn- ident [name]
  (str "\"" (str/replace name "\"" "\"\"") "\""))

(defn- literal [value]
  (str "'" (str/replace value "'" "''") "'"))

(defn- relation-name [{:keys [schema name]}]
  (str (ident schema) "." (ident name)))

(defn- storage-table [{:keys [storage-name] :as table}]
  (cond-> table storage-name (assoc :name storage-name)))

(defn- comma-list [values] (str/join ", " values))

(defn- command! [^Connection conn sql]
  (with-open [stmt (.createStatement conn)]
    (.execute stmt sql)))

(defn- bind! [^PreparedStatement stmt args]
  (doseq [[index value] (map-indexed vector args)]
    (if (nil? value)
      (.setNull stmt (inc index) Types/VARCHAR)
      (.setString stmt (inc index) (str value))))
  stmt)

(defn- query [^Connection conn sql args row]
  (with-open [stmt (bind! (.prepareStatement conn sql) args)
              result (.executeQuery stmt)]
    (loop [rows []]
      (if (.next result)
        (recur (conj rows (row result)))
        rows))))

(defn- connect [url replication?]
  (let [properties (doto (Properties.)
                     (.setProperty "connectTimeout" "10")
                     (.setProperty "socketTimeout" "30"))]
    (when replication?
      (.setProperty properties "replication" "database")
      (.setProperty properties "assumeMinServerVersion" "10")
      (.setProperty properties "preferQueryMode" "simple"))
    (let [connection (DriverManager/getConnection url properties)]
      (try
        (when-not replication?
          ;; A default expression can contain timestamp literals or objects
          ;; resolved through search_path. Keep catalog comparison stable.
          (command! connection "SET DateStyle = 'ISO, YMD'")
          (command! connection "SET TIME ZONE 'UTC'")
          (command! connection "SET search_path = pg_catalog"))
        connection
        (catch Throwable error
          (.close connection)
          (throw error))))))

(defn- source-identity [^Connection replication source]
  (with-open [stmt (.createStatement replication)
              result (.executeQuery stmt "IDENTIFY_SYSTEM")]
    (require! (.next result) "IDENTIFY_SYSTEM returned no row" {})
    (let [database (.getString result "dbname")
          [sql-database database-oid version]
          (first (query source
                        (str "SELECT datname, oid, current_setting('server_version_num') "
                             "FROM pg_database WHERE datname = current_database()")
                        [] (fn [^ResultSet r] [(.getString r 1) (.getLong r 2) (.getInt r 3)])))]
      (require! (and (= database sql-database) version (<= 180000 version 189999))
                "Expected a PostgreSQL 18 publication database" {:version version :database database})
      {:system-id (.getString result "systemid")
       :timeline (.getInt result "timeline") :database database :database-oid database-oid})))

(defn- builtin-ddl-type [{:keys [oid typmod]}]
  (let [base (sql-types oid)]
    (cond
      (= -1 typmod) base
      (and (#{1042 1043} oid) (> typmod types/var-hdr-sz))
      (str base "(" (- typmod types/var-hdr-sz) ")")
      (and (#{1083 1114 1184 1266} oid) (<= 0 typmod 6)) (str base "(" typmod ")")
      (= 1700 oid)
      (let [[precision scale] (types/decode-numeric-typmod typmod)]
        (when (and precision (<= 1 precision 1000) (<= -1000 scale 1000))
          (str base "(" precision "," scale ")"))))))

(defn- column-ddl-type [{:keys [user-type] :as column}]
  (if user-type (ident (:name user-type)) (builtin-ddl-type column)))

(defn- source-user-type [^ResultSet row]
  (when-let [kind (case (.getString row "type_kind") "e" :enum "d" :domain nil)]
    (let [name (.getString row "type_name")
          schema (.getString row "type_schema")
          base-type {:oid (.getLong row "type_base_oid")
                     :typmod (.getInt row "type_base_typmod")}
          labels (when-let [values (.getArray row "enum_labels")]
                   (vec (.getArray values)))
          problem (cond
                    (not= schema "public") :type-schema
                    (or (not= name (types/normalize-sql-type-name (ident name)))
                        (re-find #"[.\"()\[\]]" name)
                        (contains? types/pg-name->oid name)
                        (not= types/oid-text (pgs/field-type->oid name))) :type-name
                    (and (= kind :enum)
                         (or (empty? labels) (some #(or (empty? %) (re-find #"[\r\n]" %)) labels))) :enum-labels
                    (and (= kind :domain)
                         (or (contains? types/array-oid->element-oid (:oid base-type))
                             (nil? (builtin-ddl-type base-type)))) :domain-base-type
                    (and (= kind :domain) (not= -1 (:typmod base-type))) :domain-base-modifier
                    (and (= kind :domain)
                         (> (.getInt row "domain_check_count") 1)) :domain-checks
                    (and (= kind :domain)
                         (not (.getBoolean row "domain_validated"))) :unvalidated-domain-check)]
      (if problem
        {:problem problem}
        (cond-> {:kind kind :schema schema :name name :oid (.getLong row "atttypid")}
          (= kind :enum) (assoc :labels labels
                                :label-oids (mapv long (.getArray (.getArray row "enum_label_oids")))
                                :label-order (mapv double (.getArray (.getArray row "enum_label_order"))))
          (= kind :domain) (assoc :base-type base-type
                                  :not-null? (.getBoolean row "domain_not_null")
                                  :default (.getString row "domain_default")
                                  :check-name (.getString row "domain_check_name")
                                  :check-expr (.getString row "domain_check_expr")))))))

(def ^:private source-type-fields
  (str "t.typtype AS type_kind, t.typname AS type_name, tn.nspname AS type_schema, "
       "t.typbasetype AS type_base_oid, t.typtypmod AS type_base_typmod, "
       "t.typnotnull AS domain_not_null, pg_catalog.pg_get_expr(t.typdefaultbin, 0) AS domain_default, "
       "e.labels AS enum_labels, e.label_oids AS enum_label_oids, e.label_order AS enum_label_order, "
       "dc.count AS domain_check_count, "
       "coalesce(dc.validated, true) AS domain_validated, "
       "dc.name AS domain_check_name, dc.expression AS domain_check_expr "))

(def ^:private source-type-joins
  (str "LEFT JOIN pg_catalog.pg_namespace tn ON tn.oid = t.typnamespace "
       "LEFT JOIN LATERAL (SELECT array_agg(e.enumlabel::text ORDER BY e.enumsortorder) AS labels, "
       " array_agg(e.oid ORDER BY e.enumsortorder) AS label_oids, "
       " array_agg(e.enumsortorder ORDER BY e.enumsortorder) AS label_order "
       " FROM pg_catalog.pg_enum e WHERE e.enumtypid = t.oid) e ON t.typtype = 'e' "
       "LEFT JOIN LATERAL (SELECT count(*) AS count, bool_and(d.convalidated) AS validated, "
       " min(d.conname::text) AS name, min(pg_catalog.pg_get_expr(d.conbin, 0)) AS expression "
       " FROM pg_catalog.pg_constraint d WHERE d.contypid = t.oid AND d.contype = 'c') dc ON t.typtype = 'd' "))

(defn- read-source-types [source expected]
  (into {}
        (query source (str "SELECT t.oid AS atttypid, " source-type-fields
                           "FROM pg_catalog.pg_type t " source-type-joins
                           "WHERE t.oid = ANY(CAST(? AS oid[]))")
               [(str "{" (comma-list (keys expected)) "}")]
               (fn [^ResultSet row] [(.getLong row "atttypid") (source-user-type row)]))))

(defn- read-publication-row [^ResultSet row]
  (let [schema (.getString row "schemaname")
        parent-id (.getLong row "partition_parent")
        leaves (when-let [ids (.getArray row "partition_leaves")]
                 (mapv long (.getArray ids)))
        partition-bound (.getString row "partition_bound")
        partition-key (when leaves
                        {:strategy (.getString row "partition_strategy")
                         :attnums (mapv parse-long (str/split (.getString row "partition_attributes") #" "))
                         :opclasses (.getString row "partition_opclasses")
                         :collations (.getString row "partition_collations")
                         :expressions (.getString row "partition_expressions")
                         :default-id (.getLong row "partition_default")
                         :definition (.getString row "partition_key_definition")})
        relation (when schema
                   (cond-> {:schema schema :name (.getString row "tablename")
                            :relation-id (.getLong row "relation_id")}
                     (pos? parent-id) (assoc :partition-parent parent-id)
                     partition-bound (assoc :partition-bound partition-bound)
                     leaves (assoc :partition-leaves leaves :partition-key partition-key)))
        column-name (.getString row "attname")
        key-position (.getInt row "key_position")
        user-type (when column-name (source-user-type row))
        column (when column-name
                 (cond-> {:name column-name :oid (.getLong row "atttypid")
                          :typmod (.getInt row "atttypmod") :key? (pos? key-position)
                          :attnum (.getInt row "attnum") :not-null? (.getBoolean row "attnotnull")
                          :default (.getString row "default_expr")}
                   (pos? key-position) (assoc :key-position key-position)
                   (and user-type (not (:problem user-type))) (assoc :user-type user-type)))]
    {:publication-oid (.getLong row "publication_oid")
     :operations {:insert (.getBoolean row "pubinsert") :update (.getBoolean row "pubupdate")
                  :delete (.getBoolean row "pubdelete") :truncate (.getBoolean row "pubtruncate")}
     :relation relation
     :primary-key-name (.getString row "primary_key_name")
     :table-issues
     (when relation
       (cond-> []
         (= "pgdh_mirror" schema) (conj {:reason :reserved-schema})
         (.getBoolean row "has_filter") (conj {:reason :row-filter})
         (and (= "p" (.getString row "relkind")) (nil? leaves))
         (conj {:reason :root-partition-routing})
         (and (or leaves (.getBoolean row "relispartition")) (not= "public" schema))
         (conj {:reason :partition-schema})
         (.getBoolean row "partition_detach_pending") (conj {:reason :partition-detach-pending})
         (not (#{"r" "p"} (.getString row "relkind"))) (conj {:reason :relation-kind})
         (not= "p" (.getString row "relpersistence")) (conj {:reason :nonpermanent-table})
         (not= "d" (.getString row "relreplident"))
         (conj {:reason :replica-identity :mode (.getString row "relreplident")})
         (and (= "r" (.getString row "relkind"))
              (not (.getBoolean row "relispartition"))
              (.getBoolean row "has_inheritance")) (conj {:reason :inheritance})
         (.getBoolean row "has_row_security") (conj {:reason :row-security})))
     :column column
     :column-issues
     (when column
       (let [source-type (.getString row "source_type")]
         (mapv #(assoc % :column column-name :source-type source-type)
               (cond-> []
                 (not= "" (.getString row "attgenerated")) (conj {:reason :generated-column})
                 (not (.getBoolean row "is_published")) (conj {:reason :unpublished-column})
                 (not (.getBoolean row "can_select")) (conj {:reason :missing-select})
                 (:problem user-type)
                 (conj {:reason :type-definition :detail (:problem user-type) :oid (:oid column)})
                 (and (nil? user-type) (not (contains? sql-types (:oid column))))
                 (conj {:reason :type :oid (:oid column)})
                 (and (contains? sql-types (:oid column)) (nil? (column-ddl-type column)))
                 (conj {:reason :type-modifier :typmod (:typmod column)})))))}))

(defn- scan-publication [source publication]
  ;; Read leaves, parent layouts and columns from one publisher snapshot.
  ;; A parent read projection requires coverage of every descendant leaf.
  (let [rows (query source
                    (str "WITH publication AS (SELECT * FROM pg_catalog.pg_publication WHERE pubname = ?), "
                         "published AS (SELECT c.oid AS relation_id, p.schemaname, p.tablename, p.attnames, p.rowfilter "
                         "  FROM publication pub JOIN pg_catalog.pg_publication_tables p ON p.pubname = pub.pubname "
                         "  JOIN pg_catalog.pg_namespace n ON n.nspname = p.schemaname "
                         "  JOIN pg_catalog.pg_class c ON c.relnamespace = n.oid AND c.relname = p.tablename), "
                         "parent_roots AS ("
                         "  SELECT c.oid FROM publication pub JOIN pg_catalog.pg_class c ON c.relkind = 'p' "
                         "  WHERE NOT pub.pubviaroot AND c.relpersistence = 'p' AND c.oid >= 16384 AND (pub.puballtables "
                         "    OR EXISTS (SELECT 1 FROM pg_catalog.pg_publication_rel pr "
                         "               WHERE pr.prpubid = pub.oid AND pr.prrelid = c.oid) "
                         "    OR EXISTS (SELECT 1 FROM pg_catalog.pg_publication_namespace pn "
                         "               WHERE pn.pnpubid = pub.oid AND pn.pnnspid = c.relnamespace)) "
                         "  UNION SELECT c.oid FROM publication pub, published p "
                         "  JOIN pg_catalog.pg_class leaf ON leaf.oid = p.relation_id AND leaf.relispartition "
                         "  CROSS JOIN LATERAL pg_catalog.pg_partition_ancestors(p.relation_id) a(ancestor) "
                         "  JOIN pg_catalog.pg_class c ON c.oid = a.ancestor AND c.relkind = 'p' WHERE NOT pub.pubviaroot), "
                         "covered_parents AS ("
                         "  SELECT c.oid FROM parent_roots c "
                         "  WHERE NOT EXISTS (SELECT 1 FROM pg_catalog.pg_partition_tree(c.oid) t "
                         "                    WHERE t.isleaf AND NOT EXISTS "
                         "                      (SELECT 1 FROM published p WHERE p.relation_id = t.relid))), "
                         "parent_candidates AS ("
                         "  SELECT DISTINCT t.relid::oid AS oid FROM covered_parents c "
                         "  CROSS JOIN LATERAL pg_catalog.pg_partition_tree(c.oid) t WHERE NOT t.isleaf), "
                         "parents AS ("
                         "  SELECT c.oid, ARRAY(SELECT t.relid::oid FROM pg_catalog.pg_partition_tree(c.oid) t "
                         "                     WHERE t.isleaf ORDER BY t.relid::oid) AS leaves FROM parent_candidates c), "
                         "relations AS ("
                         "  SELECT p.*, NULL::oid[] AS partition_leaves FROM published p "
                         "  UNION ALL SELECT c.oid, n.nspname, c.relname, NULL, NULL, p.leaves FROM parents p "
                         "  JOIN pg_catalog.pg_class c ON c.oid = p.oid "
                         "  JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace) "
                         "SELECT pub.oid AS publication_oid, pub.pubinsert, pub.pubupdate, pub.pubdelete, pub.pubtruncate, "
                         "       p.schemaname, p.tablename, c.oid AS relation_id, p.rowfilter IS NOT NULL AS has_filter, "
                         "       c.relkind, c.relpersistence, c.relreplident, c.relispartition, "
                         "       parent.inhparent AS partition_parent, p.partition_leaves, "
                         "       (coalesce(parent.inhdetachpending, false) OR EXISTS "
                         "         (SELECT 1 FROM pg_catalog.pg_inherits ip WHERE ip.inhparent = c.oid AND ip.inhdetachpending)) "
                         "       AS partition_detach_pending, "
                         "       pg_catalog.pg_get_expr(c.relpartbound, c.oid) AS partition_bound, "
                         "       pt.partstrat AS partition_strategy, pt.partattrs::text AS partition_attributes, "
                         "       pt.partclass::text AS partition_opclasses, pt.partcollation::text AS partition_collations, "
                         "       pg_catalog.pg_get_expr(pt.partexprs, c.oid) AS partition_expressions, "
                         "       pt.partdefid AS partition_default, pg_catalog.pg_get_partkeydef(pt.partrelid) AS partition_key_definition, "
                         "       pg_catalog.row_security_active(c.oid) AS has_row_security, "
                         "       EXISTS (SELECT 1 FROM pg_catalog.pg_inherits "
                         "               WHERE inhrelid = c.oid OR inhparent = c.oid) AS has_inheritance, "
                         "       a.attname, a.atttypid, a.atttypmod, a.attgenerated, a.attnum, a.attnotnull, "
                         "       pg_catalog.pg_get_expr(def.adbin, def.adrelid) AS default_expr, "
                         "       pg_catalog.format_type(a.atttypid, a.atttypmod) AS source_type, "
                         source-type-fields ", "
                         "       (p.partition_leaves IS NOT NULL OR a.attname = ANY(p.attnames)) AS is_published, "
                         "       pg_catalog.has_column_privilege(c.oid, a.attnum, 'SELECT') AS can_select, "
                         "       pk.key_position, idx.relname AS primary_key_name "
                         "FROM publication pub "
                         "LEFT JOIN relations p ON true "
                         "LEFT JOIN pg_catalog.pg_class c ON c.oid = p.relation_id "
                         "LEFT JOIN pg_catalog.pg_inherits parent ON c.relispartition AND parent.inhrelid = c.oid "
                         "LEFT JOIN pg_catalog.pg_partitioned_table pt ON pt.partrelid = c.oid "
                         "LEFT JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped "
                         "LEFT JOIN pg_catalog.pg_type t ON t.oid = a.atttypid AND t.typtype IN ('d', 'e') "
                         source-type-joins
                         "LEFT JOIN pg_catalog.pg_attrdef def ON def.adrelid = a.attrelid AND def.adnum = a.attnum "
                         "LEFT JOIN pg_catalog.pg_index i ON i.indrelid = c.oid AND i.indisprimary "
                         " AND i.indimmediate AND i.indisvalid "
                         "LEFT JOIN pg_catalog.pg_class idx ON idx.oid = i.indexrelid "
                         "LEFT JOIN LATERAL (SELECT k.pos AS key_position "
                         " FROM unnest(i.indkey) WITH ORDINALITY AS k(attnum, pos) "
                         " WHERE k.pos <= i.indnkeyatts AND k.attnum = a.attnum) pk ON true "
                         "ORDER BY p.schemaname, p.tablename, a.attnum")
                    [publication] read-publication-row)
        {:keys [publication-oid operations]} (first rows)
        tables (mapv (fn [table-rows]
                       (let [{:keys [relation table-issues primary-key-name]} (first table-rows)
                             columns (into [] (keep :column) table-rows)
                             issues (into table-issues (mapcat :column-issues) table-rows)]
                         (assoc relation :columns columns :primary-key-name primary-key-name
                                :issues (cond-> issues
                                          (empty? columns) (conj {:reason :no-columns})
                                          (and (nil? (:partition-leaves relation)) (not-any? :key? columns))
                                          (conj {:reason :missing-immediate-primary-key})))))
                     (partition-by :relation (filter :relation rows)))
        disabled (into [] (remove #(get operations %)) [:insert :update :delete :truncate])
        issues (cond
                 (nil? publication-oid) [{:reason :publication-not-found}]
                 :else (cond-> []
                         (empty? tables) (conj {:reason :empty-publication})
                         (seq disabled) (conj {:reason :disabled-operations :operations disabled})))]
    (cond-> {:publication publication :publication-oid publication-oid :issues issues
             :tables (into [] (remove :partition-leaves) tables)
             :compatible? (and (empty? issues) (every? (comp empty? :issues) tables))}
      (some :partition-leaves tables) (assoc :partition-parents (filterv :partition-leaves tables)))))

(defn- publisher-types [tables]
  (into (sorted-map)
        (keep (fn [{:keys [user-type]}]
                (when user-type [(:name user-type) user-type])))
        (mapcat :columns tables)))

(defn- all-relations [manifest]
  (into (:tables manifest) (:partition-parents manifest)))

(defn- publication-manifest [source publication]
  (let [{:keys [publication-oid tables partition-parents compatible?] :as report}
        (scan-publication source publication)]
    (require! compatible? "Publication cannot be mirrored" {:report report})
    (let [user-types (publisher-types (all-relations report))]
      (cond-> {:publication-oid publication-oid :catalog-version 3
               :tables (mapv #(dissoc % :issues) tables)}
        (seq partition-parents) (assoc :partition-parents (mapv #(dissoc % :issues) partition-parents))
        (seq user-types) (assoc :types user-types)))))

(defn- validate-source-options! [source-url publication]
  (require! (and (string? source-url) (str/starts-with? source-url "jdbc:postgresql://")
                 (string? publication) (re-matches #"[a-z_][a-z0-9_]{0,62}" publication))
            "Expected a PostgreSQL source URL and publication name" {}))

(defn publication-report
  "Inspect a PostgreSQL 18 publication using only the source catalogs.
   Returns publication-wide issues and the issues for every table."
  [{:keys [source-url publication]}]
  (validate-source-options! source-url publication)
  (with-open [source (connect source-url false)]
    (let [version (first (query source "SHOW server_version_num" [] #(.getInt ^ResultSet % 1)))]
      (require! (and version (<= 180000 version 189999))
                "Expected a PostgreSQL 18 publication database" {:version version})
      (scan-publication source publication))))

(defn- checked-result ^PgWireServer$QueryResult [^PgWireServer$QueryResult result]
  (when-let [error (.-error result)]
    (throw (ex-info error {:sqlstate (.-sqlstate result)})))
  result)

(defn- sink-execute! ^PgWireServer$QueryResult [^PgWireServer$QueryHandler destination sql]
  (binding [ctx/*physical-table-names?* true]
    (checked-result (.execute destination sql))))

(defn- number-bind-markers [tokens]
  ;; The executor may reorder an UPDATE's assignments before reparsing them
  ;; as a projection. Like pgjdbc, retain each parameter's protocol number.
  (map-indexed (fn [index {:keys [pos end]}] [pos end (str "$" (inc index))])
               (filter #(and (= :param (:type %)) (= "?" (:text %))) tokens)))

(defn- sink-prepared! ^PgWireServer$QueryResult [^PgWireServer$QueryHandler destination sql args]
  (let [oids (int-array (repeat (count args) PgWireServer/OID_VARCHAR))
        statement (rewrite/rewrite sql [number-bind-markers])]
    (binding [ctx/*physical-table-names?* true]
      (checked-result (.executePrepared destination (.parse destination statement oids)
                                        (object-array (cons nil args)))))))

(defn- affected-count [^PgWireServer$QueryResult result]
  (parse-long (last (str/split (.-commandTag result) #" "))))

(defn- transaction! [destination f]
  (sink-execute! destination "BEGIN")
  (try
    (let [result (f)]
      (sink-execute! destination "COMMIT")
      result)
    (catch Throwable error
      (try (sink-execute! destination "ROLLBACK")
           (catch Throwable rollback-error (.addSuppressed error rollback-error)))
      (throw error))))

(defn- copy-relation [table]
  (str (relation-name table) " ("
       (comma-list (map (comp ident :name) (:columns table))) ")"))

(defn- load-copy! [^PgWireServer$QueryHandler destination table feed!]
  (let [reply (sink-execute! destination (str "COPY " (copy-relation table) " FROM STDIN"))]
    (require! (.-copyInMode reply) "Expected COPY IN response" {:table (relation-name table)}))
  (try
    (feed! #(.copyChunk destination ^bytes %))
    (affected-count (checked-result (.copyComplete destination)))
    (catch Throwable error
      (.copyAbort destination "Mirror COPY failed")
      (throw error))))

(defn- copy-table! [^Connection source destination table on-bytes]
  (let [source-api (.getCopyAPI (.unwrap source PGConnection))
        out (.copyOut source-api (str "COPY " (copy-relation table) " TO STDOUT"))]
    (try
      (let [written (load-copy! destination table
                                (fn [write!]
                                  (loop []
                                    (when-let [^bytes chunk (.readFromCopy out)]
                                      (on-bytes (alength chunk))
                                      (write! chunk)
                                      (recur)))))]
        (require! (= written (.getHandledRowCount out)) "COPY row counts differ"
                  {:table (relation-name table) :source (.getHandledRowCount out) :destination written})
        written)
      (finally (when (.isActive out) (.cancelCopy out))))))

(defn- transfer-staged-table! [stage destination table live-table on-bytes]
  (let [export (sink-execute! stage (str "COPY " (copy-relation table) " TO STDOUT"))]
    (require! (.-copyOutMode export) "Expected COPY OUT response" {:table (relation-name table)})
    (let [lines (.-copyOutLines export)
          written (load-copy! destination live-table
                              (fn [write!]
                                (doseq [line lines
                                        :let [chunk (.getBytes (str line "\n") StandardCharsets/UTF_8)]]
                                  (on-bytes (alength chunk))
                                  (write! chunk))))]
      (require! (= (count lines) written) "Staged COPY row counts differ"
                {:table (relation-name table) :source (count lines) :destination written}))))

(def ^:private checkpoint-table "pgdh_mirror.checkpoint")

(defn- checkpoint [^PgWireServer$QueryHandler destination]
  (let [reply (.execute destination (str "SELECT origin, slot_name, publication, manifest, end_lsn "
                                         "FROM " checkpoint-table " WHERE id = 1"))]
    (when-not (= "42P01" (.-sqlstate reply))
      (when-let [row (first (.-rows (checked-result reply)))]
        (let [[origin slot publication manifest end-lsn] row]
          {:origin (edn/read-string origin) :slot slot :publication publication
           :manifest (some-> manifest edn/read-string) :end-lsn end-lsn})))))

(defn- init-checkpoint! [destination origin slot publication]
  (transaction!
   destination
   (fn []
     (sink-execute! destination
                    (str "CREATE TABLE " checkpoint-table " (id integer PRIMARY KEY, "
                         "origin text NOT NULL, slot_name text NOT NULL, publication text NOT NULL, "
                         "manifest text, end_lsn text, source_xid text, source_commit_micros text)"))
     (sink-prepared! destination (str "INSERT INTO " checkpoint-table
                                      " (id, origin, slot_name, publication) VALUES (1, ?, ?, ?)")
                     [(pr-str origin) slot publication]))))

(defn- local-column-attrs [db {:keys [name columns] :as table}]
  (let [relation (pgs/storage-table-oid db name)]
    (require! relation "Missing local mirror relation" {:table (relation-name table)})
    (mapv (fn [column]
            (or (:datahike.pg.column/storage-ident
                 (catalog-objects/column-by-name db relation (:name column)))
                (throw (ex-info "Missing local mirror column"
                                {:table (relation-name table) :column (:name column)}))))
          columns)))

(defn- stage-data! [destination f]
  (checked-result (pg/stage-tx! destination [[:db.fn/call f]])))

(defn- column-metadata-tx [db table]
  (let [attrs (local-column-attrs db table)
        tx (into []
                 (mapcat (fn [attr {:keys [not-null? default]}]
                           (let [entity (d/entity db attr)
                                 eid (:db/id entity)
                                 was-not-null? (true? (:pg/not-null entity))
                                 kind (:pg/default-kind entity)
                                 previous (:pg/default-value entity)]
                             (cond-> []
                               (and not-null? (not was-not-null?)) (conj [:db/add eid :pg/not-null true])
                               (and (not not-null?) was-not-null?) (conj [:db/retract eid :pg/not-null true])
                               (and (some? default) (not= kind :publisher)) (conj [:db/add eid :pg/default-kind :publisher])
                               (and (some? default) (not= default previous)) (conj [:db/add eid :pg/default-value default])
                               (and (nil? default) kind) (conj [:db/retract eid :pg/default-kind kind])
                               (and (nil? default) previous) (conj [:db/retract eid :pg/default-value previous]))))
                         attrs (:columns table)))]
    (if (seq tx)
      (into tx (catalog-objects/bump-revision-tx
                db catalog-objects/pg-class-oid (pgs/storage-table-oid db (:name table))))
      tx)))

(defn- set-column-metadata! [destination table]
  (stage-data! destination #(column-metadata-tx % table)))

(defn- key-columns [table]
  (sort-by #(or (:key-position %) (:attnum %)) (filter :key? (:columns table))))

(defn- install-primary-key! [destination table]
  (let [name (:primary-key-name table)]
    (sink-execute! destination
                   (str "CREATE UNIQUE INDEX " (ident name) " ON " (relation-name table)
                        " (" (comma-list (map (comp ident :name) (key-columns table))) ")"))
    (stage-data! destination
                 (fn [db]
                   (let [object (catalog-objects/object-by-identity
                                 db catalog-objects/pg-class-oid catalog-objects/public-namespace-oid name)]
                     (require! (= :index (:datahike.pg.object/kind object))
                               "Missing replacement primary index" {:table (relation-name table)})
                     [[:db/add (:db/id object) :datahike.pg.index/primary? true]])))))

(defn- enum-member-tx [db name {:keys [label-oids label-order]}]
  (let [attr :datahike.pg.enum/publisher-members]
    (cond-> []
      (not (contains? (dbi/-schema db) attr))
      (conj {:db/ident attr :db/valueType :db.type/string :db/cardinality :db.cardinality/one})
      true
      (conj [:db/add [:datahike.pg.enum/name name] attr (pr-str (mapv vector label-oids label-order))]))))

(defn- refresh-type-tx [db [old {:keys [kind name labels not-null? default check-name check-expr] :as definition}]]
  (let [object (catalog-objects/object-by-identity db catalog-objects/pg-type-oid
                                                   catalog-objects/public-namespace-oid name)
        registry-name (case kind :enum :datahike.pg.enum/name :domain :datahike.pg.domain/name)
        eid (d/q '[:find ?e . :in $ ?attr ?name :where [?e ?attr ?name]] db registry-name name)
        _ (require! (and eid (= kind (:datahike.pg.object/kind object)))
                    "Missing local user type during schema refresh" {:type name})
        type-tx (case kind
                  :enum
                  (let [old-labels (set (:labels old))
                        new-labels (set labels)]
                    (cond-> (enum-member-tx db name definition)
                      (not= (:labels old) labels)
                      (into (concat (for [label (:labels old) :when (not (contains? new-labels label))]
                                      [:db/retract eid :datahike.pg.enum/values label])
                                    (for [label labels :when (not (contains? old-labels label))]
                                      [:db/add eid :datahike.pg.enum/values label])
                                    [[:db/add eid :datahike.pg.enum/values-ordered (str/join "\n" labels)]]))))
                  :domain
                  (let [entity (d/entity db eid)]
                    (keep (fn [[attr value]]
                            (let [previous (get entity attr)]
                              (when (not= previous value)
                                (if (nil? value) [:db/retract eid attr previous] [:db/add eid attr value]))))
                          [[:datahike.pg.domain/not-null not-null?]
                           [:datahike.pg.domain/default-raw default]
                           [:datahike.pg.domain/check-name check-name]
                           [:datahike.pg.domain/check-expr check-expr]])))]
    (when (seq type-tx)
      (into (vec type-tx) (catalog-objects/bump-revision-tx
                           db catalog-objects/pg-type-oid (:datahike.pg.object/oid object))))))

(defn- install-types! [destination user-types]
  (doseq [{:keys [kind name labels base-type not-null?] :as definition} (vals user-types)]
    (sink-execute! destination
                   (case kind
                     :enum (str "CREATE TYPE " (ident name) " AS ENUM (" (comma-list (map literal labels)) ")")
                     :domain (str "CREATE DOMAIN " (ident name) " AS "
                                  (if (= types/oid-float8 (:oid base-type))
                                    "float8"
                                    (builtin-ddl-type base-type))
                                  (when not-null? " NOT NULL"))))
    ;; Source defaults and checks are already deparsed SQL. Store those
    ;; expressions verbatim instead of reconstructing them from DDL tokens.
    (stage-data! destination #(refresh-type-tx % [definition definition]))))

(defn- create-table! [destination {:keys [columns] :as table}]
  (let [keys (key-columns table)
        native-key? (and (= 1 (count keys))
                         (= (:primary-key-name table) (str (:name table) "_pkey")))]
    (sink-execute!
     destination
     (str "CREATE TABLE " (relation-name table) " ("
          (comma-list (map (fn [{:keys [name] :as column}] (str (ident name) " " (column-ddl-type column))) columns))
          (when native-key? (str ", PRIMARY KEY (" (ident (:name (first keys))) ")"))
          ")"))
    (set-column-metadata! destination table)
    (when (and (seq keys) (not native-key?))
      (install-primary-key! destination table))))

(defn- partition-query [parent tables]
  (let [columns (comma-list (map (comp ident :name) (:columns parent)))]
    (str/join " UNION ALL "
              (map (fn [id] (str "SELECT " columns " FROM " (relation-name (tables id))))
                   (:partition-leaves parent)))))

(defn- local-partition-key [db table local]
  (when-let [{:keys [attnums default-id] :as key} (:partition-key table)]
    (let [oid (:datahike.pg.object/oid (get local (:relation-id table)))
          names (into {} (map (juxt :attnum :name)) (:columns table))]
      (-> key
          (dissoc :default-id :definition)
          (assoc :default-oid (get-in local [default-id :datahike.pg.object/oid] 0)
                 :attnums (mapv (fn [attnum]
                                  (if (zero? attnum)
                                    0
                                    (let [column (catalog-objects/column-by-name db oid (get names attnum))
                                          local-attnum (:datahike.pg.column/attnum column)]
                                      (require! local-attnum "Missing local partition key column"
                                                {:table (relation-name table) :attnum attnum})
                                      local-attnum)))
                                attnums))))))

(defn- partition-topology-tx [db manifest]
  (let [relations (all-relations manifest)
        by-id (into {} (map (juxt :relation-id identity)) relations)
        local (into {} (map (fn [[id table]]
                              [id (catalog-objects/object-by-address
                                   db catalog-objects/pg-class-oid
                                   (pgs/storage-table-oid db (:name (storage-table table))))])) by-id)
        schema (dbi/-schema db)
        attributes (when (seq (:partition-parents manifest))
                     (for [[attr vtype] [[:datahike.pg.partition/definition :db.type/string]
                                         [:datahike.pg.partition/parent-oid :db.type/long]
                                         [:datahike.pg.partition/key :db.type/string]
                                         [:datahike.pg.partition/key-definition :db.type/string]
                                         [:datahike.pg.partition/bound :db.type/string]]
                           :when (not (contains? schema attr))]
                       {:db/ident attr :db/valueType vtype :db/cardinality :db.cardinality/one}))]
    (into (vec attributes)
          (mapcat
           (fn [{:keys [relation-id partition-parent partition-leaves] :as table}]
             (let [{eid :db/id oid :datahike.pg.object/oid} (get local relation-id)
                   _ (require! eid "Missing local partition relation" {:table (relation-name table)})
                   old (d/entity db eid)
                   definition (when partition-leaves (partition-query table by-id))
                   parent-oid (get-in local [partition-parent :datahike.pg.object/oid])
                   partition-key (some-> (local-partition-key db table local) pr-str)
                   changes (into []
                                 (keep (fn [[attr value]]
                                         (let [before (get old attr)]
                                           (cond
                                             (= before value) nil
                                             (nil? value) [:db/retract eid attr before]
                                             :else [:db/add eid attr value]))))
                                 [[:datahike.pg.partition/definition definition]
                                  [:datahike.pg.partition/parent-oid parent-oid]
                                  [:datahike.pg.partition/key partition-key]
                                  [:datahike.pg.partition/key-definition (get-in table [:partition-key :definition])]
                                  [:datahike.pg.partition/bound (when parent-oid (:partition-bound table))]])]
               (when (seq changes)
                 (into changes (catalog-objects/bump-revision-tx db catalog-objects/pg-class-oid oid))))))
          relations)))

(defn- install-partition-topology! [destination manifest]
  (stage-data! destination #(partition-topology-tx % manifest)))

(defn- slot-info [source slot]
  (first (query source
                (str "SELECT confirmed_flush_lsn::text, restart_lsn::text, active, invalidation_reason, "
                     "       plugin = 'pgoutput' AND database = current_database() "
                     "       AND NOT temporary AND NOT two_phase AND NOT failover, "
                     "       pg_wal_lsn_diff(pg_current_wal_lsn(), restart_lsn)::text, "
                     "       pg_wal_lsn_diff(pg_current_wal_lsn(), confirmed_flush_lsn)::text "
                     "FROM pg_replication_slots WHERE slot_name = ?")
                [slot] (fn [^ResultSet r]
                         {:confirmed-lsn (.getString r 1) :restart-lsn (.getString r 2)
                          :active? (.getBoolean r 3) :invalidation (.getString r 4)
                          :matches? (.getBoolean r 5)
                          :retained-wal-bytes (some-> (.getString r 6) parse-long)
                          :unconfirmed-wal-bytes (some-> (.getString r 7) parse-long)}))))

(defn- snapshot-budget [check-running! limit]
  (let [copied (volatile! 0)]
    (fn [n]
      (check-running!)
      (require! (<= (vswap! copied + n) limit) "Snapshot exceeds the configured byte limit" {:limit limit}))))

(defn- with-slot-snapshot! [^Connection replication source slot temporary? f]
  (let [api (.getReplicationAPI (.unwrap replication PGConnection))
        builder (-> api .createReplicationSlot .logical (.withSlotName slot) (.withOutputPlugin "pgoutput"))
        created (.make (if temporary? (.withTemporaryOption builder) builder))
        snapshot (.getSnapshotName created)]
    (require! (seq snapshot) "The slot did not export a snapshot" {:slot slot})
    (command! source "BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY")
    (try
      (command! source (str "SET TRANSACTION SNAPSHOT " (literal snapshot)))
      (command! source "SET LOCAL row_security = off")
      (f (.asString (.getConsistentPoint created)))
      (finally (command! source "ROLLBACK")))))

(defn- bootstrap! [replication source destination {:keys [slot publication]} check-running! max-snapshot-bytes]
  (when-let [existing (slot-info source slot)]
    ;; The unique slot name was persisted locally before its first creation.
    (require! (and (:matches? existing) (not (:active? existing)))
              "Cannot restart an incomplete snapshot while its slot is in use or has changed" {:slot slot})
    (query source "SELECT pg_drop_replication_slot(?)" [slot] (constantly nil)))
  (let [on-bytes (snapshot-budget check-running! max-snapshot-bytes)]
    (with-slot-snapshot!
      replication source slot false
      (fn [end-lsn]
        (let [manifest (publication-manifest source publication)]
          (transaction!
           destination
           (fn []
             (install-types! destination (:types manifest))
             (doseq [table (:tables manifest)]
               (check-running!)
               (create-table! destination table)
               (copy-table! source destination table on-bytes))
             (doseq [parent (:partition-parents manifest)]
               (create-table! destination parent))
             (when (seq (:partition-parents manifest))
               (install-partition-topology! destination manifest))
             (sink-prepared! destination
                             (str "UPDATE " checkpoint-table " SET manifest = ?, end_lsn = ? WHERE id = 1")
                             [(pr-str manifest) end-lsn]))))))))

(defn- lsn [value] (LogSequenceNumber/valueOf (str value)))

(defn- compare-lsn [a b]
  (Long/compareUnsigned (.asLong (lsn a)) (.asLong (lsn b))))

(defn- byte-char [^ByteBuffer buffer] (char (bit-and 0xff (.get buffer))))
(defn- uint32 [^ByteBuffer buffer] (Integer/toUnsignedLong (.getInt buffer)))
(defn- uint16 [^ByteBuffer buffer] (Short/toUnsignedInt (.getShort buffer)))
(defn- read-lsn [^ByteBuffer buffer] (.asString (LogSequenceNumber/valueOf (.getLong buffer))))

(defn- read-cstr [^ByteBuffer buffer]
  (let [out (ByteArrayOutputStream.)]
    (loop []
      (let [x (bit-and 0xff (.get buffer))]
        (if (zero? x)
          (.toString out StandardCharsets/UTF_8)
          (do (.write out x) (recur)))))))

(defn- read-tuple [^ByteBuffer buffer]
  (mapv (fn [_]
          (case (byte-char buffer)
            \n nil
            \u ::unchanged
            \t (let [size (.getInt buffer)]
                 (require! (<= 0 size (.remaining buffer)) "Invalid pgoutput field length" {:size size})
                 (let [bytes (byte-array size)]
                   (.get buffer bytes)
                   (str (.decode (.newDecoder StandardCharsets/UTF_8) (ByteBuffer/wrap bytes)))))
            (throw (ex-info "Unexpected pgoutput field format" {}))))
        (range (uint16 buffer))))

(defn- tuple-after [^ByteBuffer buffer actual permitted]
  (require! (contains? permitted actual) "Unexpected pgoutput tuple kind" {:actual actual})
  (read-tuple buffer))

(defn- decode
  "Decode one unstreamed pgoutput v1 message. Text fields retain their SQL
   input spelling. An unchanged TOAST field carries a distinct marker."
  [^ByteBuffer buffer]
  (let [tag (byte-char buffer)
        message
        (case tag
          \B {:op :begin :commit-lsn (read-lsn buffer)
              :commit-micros (.getLong buffer) :xid (uint32 buffer)}
          \C (let [flags (.get buffer)]
               (require! (zero? flags) "Unsupported commit flags" {:flags flags})
               {:op :commit :commit-lsn (read-lsn buffer) :end-lsn (read-lsn buffer)
                :commit-micros (.getLong buffer)})
          \R (let [relation-id (uint32 buffer)
                   schema (read-cstr buffer)
                   name (read-cstr buffer)
                   identity (byte-char buffer)]
               {:op :relation :relation-id relation-id :schema schema :name name :identity identity
                :columns
                (mapv (fn [_]
                        (let [flags (.get buffer)
                              column {:key? (pos? (bit-and flags 1))
                                      :name (read-cstr buffer) :oid (uint32 buffer)}]
                          (assoc column :typmod (.getInt buffer))))
                      (range (uint16 buffer)))})
          \I {:op :insert :relation-id (uint32 buffer)
              :new (tuple-after buffer (byte-char buffer) #{\N})}
          \U (let [relation-id (uint32 buffer)
                   kind (byte-char buffer)
                   old (when (#{\K \O} kind) (read-tuple buffer))
                   new-kind (if old (byte-char buffer) kind)]
               {:op :update :relation-id relation-id :old old
                :new (tuple-after buffer new-kind #{\N})})
          \D {:op :delete :relation-id (uint32 buffer)
              :old (tuple-after buffer (byte-char buffer) #{\K \O})}
          \T (let [count (.getInt buffer)
                   flags (.get buffer)]
               (require! (<= 1 count (quot (.remaining buffer) 4)) "Invalid truncate relation count" {:count count})
               {:op :truncate :relation-ids (mapv (fn [_] (uint32 buffer)) (range count))
                :flags flags})
          \M (let [flags (byte-char buffer)
                   position (read-lsn buffer)
                   prefix (read-cstr buffer)
                   size (.getInt buffer)]
               (require! (and (#{(char 0) (char 1)} flags) (= size (.remaining buffer)))
                         "Invalid pgoutput logical message" {})
               (let [payload (when (and (= "pg_datahike_mirror" prefix) (= 36 size))
                               (let [bytes (byte-array size)]
                                 (.get buffer bytes)
                                 (String. bytes StandardCharsets/US_ASCII)))]
                 (.position buffer (.limit buffer))
                 {:op :message :transactional? (= (char 1) flags) :lsn position :payload payload}))
          \O (do (read-lsn buffer) (read-cstr buffer) {:op :origin})
          ;; pgoutput supplies a base type name for domains. The manifest
          ;; uses the source relation OID and the full catalog definition.
          \Y {:op :type :oid (uint32 buffer) :schema (read-cstr buffer) :name (read-cstr buffer)}
          (throw (ex-info "Unsupported pgoutput message" {:tag tag})))]
    (require! (zero? (.remaining buffer)) "Extra bytes in a pgoutput message" {:tag tag})
    message))

(defn- relation! [relations relation-id]
  (or (get relations relation-id)
      (throw (ex-info "pgoutput change has no validated relation" {:relation-id relation-id}))))

(defn- tuple-pairs [table values]
  (let [columns (:columns table)]
    (require! (= (count columns) (count values)) "pgoutput tuple width differs from its relation"
              {:table (:name table) :columns (count columns) :values (count values)})
    (mapv vector columns values)))

(defn- cast-param [{:keys [oid user-type]}]
  (str "CAST(? AS " (if user-type (ident (:name user-type)) (sql-types oid)) ")"))

(defn- key-pairs [table values]
  (let [pairs (filterv (comp :key? first) (tuple-pairs table values))]
    (require! (every? #(and (some? (second %)) (not= ::unchanged (second %))) pairs)
              "pgoutput omitted a replica identity column" {:table (:name table)})
    pairs))

(defn- key-filter [keys]
  (str " WHERE "
       (str/join " AND " (map (fn [[column _]]
                                (str (ident (:name column)) " = " (cast-param column)))
                              keys))))

(defn- apply-change! [destination relations {:keys [op relation-id relation-ids old new]}]
  (if (= op :truncate)
    (sink-execute! destination
                   (str "TRUNCATE "
                        (comma-list (map #(relation-name (relation! relations %)) relation-ids))))
    (let [table (relation! relations relation-id)
          table-name (relation-name table)
          [sql values read-check?]
          (case op
            :insert
            (let [pairs (tuple-pairs table new)
                  columns (map first pairs)]
              (require! (not-any? #{::unchanged} new) "INSERT has an unchanged TOAST marker"
                        {:relation-id relation-id})
              [(str "INSERT INTO " table-name " (" (comma-list (map (comp ident :name) columns))
                    ") VALUES (" (comma-list (map cast-param columns)) ")")
               new])

            :update
            (let [keys (key-pairs table (or old new))
                  old-key-values (into {} keys)
                  changed (filterv (fn [[column value]]
                                     (and (not= ::unchanged value)
                                          (or (not (:key? column))
                                              (not= value (get old-key-values column)))))
                                   (tuple-pairs table new))]
              (if (empty? changed)
                [(str "SELECT 1 FROM " table-name (key-filter keys)) (mapv second keys) true]
                [(str "UPDATE " table-name " SET "
                      (comma-list (map (fn [[column _]]
                                         (str (ident (:name column)) " = " (cast-param column)))
                                       changed))
                      (key-filter keys))
                 (into (mapv second changed) (map second) keys)]))

            :delete
            (let [keys (key-pairs table old)]
              [(str "DELETE FROM " table-name (key-filter keys)) (mapv second keys)]))
          result (sink-prepared! destination sql values)
          count (if read-check? (count (.-rows result)) (affected-count result))]
      (require! (= 1 count) "A mirrored change did not affect exactly one row"
                {:relation-id relation-id :operation op :affected count}))))

(defn- apply-commit! [destination relations previous {:keys [end-lsn xid commit-micros changes install! manifest]}]
  (transaction!
   destination
   (fn []
     (doseq [change changes]
       (apply-change! destination relations change))
     (when install! (install! destination))
     (let [result (sink-prepared! destination
                                  (str "UPDATE " checkpoint-table
                                       " SET end_lsn = ?, source_xid = ?, source_commit_micros = ?"
                                       (when manifest ", manifest = ?")
                                       " WHERE id = 1 AND end_lsn = ?")
                                  (conj (cond-> [end-lsn (str xid) (str commit-micros)]
                                          manifest (conj (pr-str manifest)))
                                        previous))]
       (require! (= 1 (affected-count result)) "Mirror checkpoint changed while applying a transaction"
                 {:expected previous :next end-lsn})))))

(defn- replication-copy ^CopyDual [^Connection replication slot publication confirmed-lsn]
  (let [^BaseConnection base (.unwrap replication BaseConnection)
        command (str "START_REPLICATION SLOT " (ident slot) " LOGICAL " (.asString (lsn confirmed-lsn))
                     " (proto_version '1', publication_names " (literal publication) ", messages 'true')")
        copy (.startCopy (.getQueryExecutor base) command true)]
    (require! (instance? CopyDual copy) "Expected replication COPY BOTH response" {})
    copy))

(defn- feedback! [^CopyDual copy position request-reply?]
  ;; pgJDBC's PGReplicationStream can raise its flush LSN on keepalives.
  ;; Write the COPY BOTH feedback directly using only a durable position.
  (let [buffer (ByteBuffer/allocate 34)
        durable (.asLong (lsn position))]
    (.put buffer (byte (int \r)))
    (.putLong buffer durable)
    (.putLong buffer durable)
    (.putLong buffer durable)
    (.putLong buffer (* 1000 (- (System/currentTimeMillis) 946684800000)))
    (.put buffer (byte (if request-reply? 1 0)))
    (.writeToCopy copy (.array buffer) 0 (.capacity buffer))
    (.flushCopy copy)))

(defn- read-packet [^CopyDual copy]
  (when-not (.isActive copy)
    (throw (SQLException. "Publisher ended the replication stream" "08006")))
  (when-let [bytes (.readFromCopy copy false)]
    (let [buffer (ByteBuffer/wrap bytes)]
      (case (byte-char buffer)
        \w (do
             (require! (< 24 (.remaining buffer)) "Invalid XLogData packet" {})
             (.position buffer (+ (.position buffer) 24))
             {:data (.slice buffer)})
        \k (do
             (require! (= 17 (.remaining buffer)) "Invalid replication keepalive" {})
             (.position buffer (+ (.position buffer) 16))
             {:reply? (= 1 (.get buffer))})
        (throw (ex-info "Unexpected PostgreSQL replication packet" {}))))))

(defn- validate-resume! [source {:keys [slot end-lsn]}]
  (let [{:keys [matches? active? invalidation confirmed-lsn] :as info} (slot-info source slot)]
    (when active?
      (throw (SQLException. "Replication slot is still active" "55006")))
    (require! (and matches? (nil? invalidation) confirmed-lsn
                   (not (pos? (compare-lsn confirmed-lsn end-lsn))))
              "Replication slot cannot resume from the durable checkpoint"
              {:slot slot :checkpoint end-lsn :slot-status info :resnapshot-required? true})
    info))

(defn- emit-barrier!
  ([source] (emit-barrier! source (str (UUID/randomUUID))))
  ([source barrier]
   (query source "SELECT pg_logical_emit_message(true, 'pg_datahike_mirror', CAST(? AS text))"
          [barrier] (constantly nil))
   barrier))

(defn- publish-status! [state info]
  (swap! state merge (select-keys info [:confirmed-lsn :restart-lsn :retained-wal-bytes :unconfirmed-wal-bytes])))

(defn- checked-running-slot! [source slot state]
  (let [{:keys [matches? invalidation] :as info} (slot-info source slot)]
    (require! (and matches? (nil? invalidation))
              "Replication slot is unavailable" {:slot slot :slot-status info :resnapshot-required? true})
    (publish-status! state info)))

(defn- new-slot-name [prefix]
  (str prefix "_" (str/replace (str (UUID/randomUUID)) "-" "")))

(defn- tables-by-id [manifest]
  (into {} (map (juxt :relation-id identity)) (:tables manifest)))

(defn- relations-by-id [manifest]
  (into {} (map (juxt :relation-id identity)) (all-relations manifest)))

(defn- partition-layout [manifest]
  (into {} (keep (fn [{:keys [relation-id partition-parent partition-leaves partition-key partition-bound]}]
                   (when (or partition-parent partition-leaves)
                     [relation-id [partition-parent partition-leaves partition-key partition-bound]])))
        (all-relations manifest)))

(defn- partition-affected [previous current]
  ;; Resnapshot the affected tree as a unit so cross-leaf changes share
  ;; the same cutover as the new parent projection.
  (let [old-layout (partition-layout previous)
        new-layout (partition-layout current)]
    (into #{}
          (mapcat (fn [id]
                    (let [old (get old-layout id)
                          new (get new-layout id)]
                      (when (not= new old)
                        (into [id] (concat (second old) (second new)))))))
          (into (set (keys old-layout)) (keys new-layout)))))

(defn- removed-relations [previous current]
  (let [remaining (relations-by-id current)]
    (into [] (remove #(contains? remaining (:relation-id %))) (all-relations previous))))

(defn- wire-table [table]
  (update (select-keys table [:schema :name :relation-id :columns])
          :columns #(mapv (fn [column] (select-keys column [:name :oid :typmod :key?])) %)))

(defn- same-type-definition? [old current]
  (= old (if (:label-oids old) current (dissoc current :label-oids :label-order))))

(defn- type-changes! [previous current allow-schema-refresh?]
  (let [before (:types previous)
        types-by-oid (into {} (map (juxt :oid identity)) (vals before))
        identity-keys [:schema :name :oid :kind :base-type]]
    (into {}
          (for [[name definition] (:types current)
                :let [installed (or (get before name) (get types-by-oid (:oid definition)))]
                :when (and installed (not= installed definition)
                           (or allow-schema-refresh? (not (same-type-definition? installed definition))))
                :let [context {:reason :changed-type-definition :type name}]]
            (do
              (require! allow-schema-refresh? "Publisher changed a mirrored user type" context)
              (require! (= (select-keys installed identity-keys) (select-keys definition identity-keys))
                        "Schema refresh requires the same PostgreSQL type identity" context)
              (when (= :enum (:kind installed))
                (let [[old new] (if-let [oids (:label-oids installed)]
                                  [oids (:label-oids definition)]
                                  [(:labels installed) (:labels definition)])]
                  (require! (= old (into [] (filter (set old)) new))
                            "Schema refresh requires an ordered baseline for the installed enum labels"
                            (assoc context :reason :unknown-enum-label-identity))))
              [name [installed definition]])))))

(defn- same-table? [previous current]
  (if (every? :attnum (:columns previous))
    (let [shape #(dissoc % :storage-name :primary-key-name :partition-leaves :partition-parent
                         :partition-key :partition-bound :columns)
          columns (fn [table] (mapv #(dissoc % :key-position :user-type) (:columns table)))]
      (and current (= (shape previous) (shape current))
           (= (columns previous) (columns current))
           (every? true? (map #(same-type-definition? (:user-type %1) (:user-type %2))
                              (:columns previous) (:columns current)))
           (or (nil? (:primary-key-name previous))
               (and (= (:primary-key-name previous) (:primary-key-name current))
                    (= (mapv :attnum (key-columns previous))
                       (mapv :attnum (key-columns current)))))))
    (and current (= (wire-table previous) (wire-table current)))))

(defn- changed-tables! [previous current affected removed type-changes allow-schema-refresh?]
  (let [before (relations-by-id previous)
        occupied (into (or (:retired-storage previous) #{})
                       (map (comp :name storage-table)) (all-relations previous))
        missing (remove #(or (:partition-parent %) (:partition-leaves %)) removed)]
    (require! (and (= (:publication-oid previous) (:publication-oid current)) (empty? missing))
              "Publication schema or membership changed"
              {:removed (mapv relation-name missing)})
    (let [changes (filterv (fn [table]
                             (let [prior (get before (:relation-id table))]
                               (or (nil? prior)
                                   (contains? affected (:relation-id table))
                                   (contains? (:pending-schema previous) (:relation-id table))
                                   (not (same-table? prior table))
                                   (some #(contains? type-changes (get-in % [:user-type :name])) (:columns table))
                                   (and allow-schema-refresh?
                                        (or (not-every? :attnum (:columns prior))
                                            (and (nil? (:primary-key-name prior))
                                                 (not= (mapv :attnum (key-columns prior))
                                                       (mapv :attnum (key-columns table)))))))))
                           (all-relations current))]
      (mapv (fn [table]
              (let [prior (get before (:relation-id table))
                    storage (if prior
                              (:name (storage-table prior))
                              (if (contains? occupied (:name table))
                                (new-slot-name "pgdh_relation")
                                (:name table)))]
                (when (and prior (not (same-table? prior table)))
                  (require! allow-schema-refresh?
                            "Publication schema or membership changed" {:table (relation-name prior)})
                  (require! (and (or (every? :attnum (:columns prior)) (same-table? prior table))
                                 (= (:schema prior) (:schema table)))
                            "Schema refresh requires a known column layout and the same schema"
                            {:table (relation-name table)}))
                ;; A legacy checkpoint knows the wire order. Identical wire
                ;; layout lets us assign the current attnums before staging.
                {:table table
                 :storage-name storage
                 :previous (if (and prior (not-every? :attnum (:columns prior)))
                             (update prior :columns
                                     (fn [columns]
                                       (mapv (fn [old new] (assoc old :attnum (:attnum new))) columns (:columns table))))
                             prior)}))
            changes))))

(defn- require-stable-manifest! [target source]
  (let [after (relations-by-id source)]
    (require! (and (= (:publication-oid target) (:publication-oid source))
                   (= (partition-layout target) (partition-layout source))
                   (every? (fn [table] (same-table? table (get after (:relation-id table))))
                           (all-relations target)))
              "Publication changed during refresh" {})))

(defn- typed-column [column]
  (select-keys column [:oid :typmod]))

(defn- primary-key-changed? [previous table]
  (or (not= (mapv :attnum (key-columns previous))
            (mapv :attnum (key-columns table)))
      (and (:primary-key-name previous)
           (not= (:primary-key-name previous) (:primary-key-name table)))))

(defn- matching-key-indexes! [previous table]
  (let [by-attnum (into {} (map-indexed (fn [i col] [(:attnum col) [i col]])) (:columns table))]
    (mapv (fn [before]
            (let [[i after] (get by-attnum (:attnum before))]
              (require! (and after (= (typed-column before) (typed-column after)))
                        "Schema refresh cannot match rows without the previous primary key columns and types"
                        {:table (relation-name table) :reason :missing-previous-key :column (:name before)})
              i))
          (key-columns previous))))

(defn- drop-primary-key! [destination previous]
  (stage-data!
   destination
   (fn [db]
     (let [relation-oid (pgs/storage-table-oid db (:name previous))]
       (if-let [oid (catalog-objects/primary-index-oid db relation-oid)]
         (catalog-objects/drop-object-tx db catalog-objects/pg-class-oid oid)
         (let [keys (key-columns previous)
               attr (if (= 1 (count keys))
                      (first (local-column-attrs db (assoc previous :columns keys)))
                      (keyword (:name previous) "pg$pk_tuple"))
               entity (d/entity db attr)]
           (require! (= :db.unique/identity (:db/unique entity))
                     "Missing local primary key during schema refresh" {:table (relation-name previous)})
           [[:db/retract (:db/id entity) :db/unique :db.unique/identity]]))))))

(defn- adapt-layout! [destination stage-db previous table staged-table]
  (let [before (into {} (map (juxt :attnum identity)) (:columns previous))
        after (into {} (map (juxt :attnum identity)) (:columns table))
        alter (str "ALTER TABLE " (relation-name table) " ")
        renames (into []
                      (keep (fn [{:keys [attnum name]}]
                              (when-let [old (get before attnum)]
                                (when (not= (:name old) name)
                                  [(:name old) (new-slot-name "pgdh_col") name]))))
                      (:columns table))
        retypes (filterv (fn [{:keys [attnum] :as column}]
                           (when-let [prior (get before attnum)]
                             (not= (typed-column column) (typed-column prior))))
                         (:columns table))]
    (doseq [{:keys [attnum name]} (:columns previous) :when (not (contains? after attnum))]
      (sink-execute! destination (str alter "DROP COLUMN " (ident name))))
    ;; Use temporary names first so a source column-name swap is unambiguous.
    (doseq [[old temporary _] renames]
      (sink-execute! destination (str alter "RENAME COLUMN " (ident old) " TO " (ident temporary))))
    (doseq [[_ temporary assigned] renames]
      (sink-execute! destination (str alter "RENAME COLUMN " (ident temporary) " TO " (ident assigned))))
    (doseq [{:keys [name oid typmod user-type] :as column} retypes]
      (let [[staged-attr] (local-column-attrs stage-db (assoc staged-table :columns [column]))
            definition (d/entity stage-db staged-attr)]
        (stage-data! destination
                     (fn [db]
                       (pg/mirror-column-rewrite-tx db (:name table) name
                                                    (if user-type (pgs/resolve-type-oid db (column-ddl-type column)) oid)
                                                    (when (not= -1 typmod) typmod) definition)))))
    (doseq [{:keys [attnum name] :as column} (:columns table) :when (not (contains? before attnum))]
      (sink-execute! destination
                     (str alter "ADD COLUMN " (ident name) " " (column-ddl-type column))))))

(defn- stored-rows [db table attrs]
  (let [marker (pgs/row-marker-attr (:name table))]
    (map (fn [eid]
           (let [row (d/entity db eid)]
             [eid (mapv #(get row %) attrs)]))
         (d/q '[:find [?row ...] :in $ ?marker :where [?row ?marker true]] db marker))))

(defn- replace-row-tx [eid attrs previous values]
  (keep (fn [[attr old new]]
          (cond
            (= old new) nil
            (nil? new) [:db/retract eid attr old]
            :else [:db/add eid attr new]))
        (map vector attrs previous values)))

(defn- previous-key [columns values]
  (mapv (fn [{:keys [oid user-type]} value]
          (condp = (or (get-in user-type [:base-type :oid]) oid)
            types/oid-jsonb (jsonb/parse-jsonb value)
            types/oid-bpchar (some-> value (str/replace #" +\z" ""))
            (unique/canonical-key-value value)))
        columns values))

(defn- enum-renames [type-changes]
  (into {}
        (keep (fn [[name [old new]]]
                (when-let [oids (:label-oids old)]
                  (let [new-labels (zipmap (:label-oids new) (:labels new))
                        renames (into {} (keep (fn [[oid label]]
                                                 (let [current (new-labels oid)]
                                                   (when (not= label current) [label current]))))
                                      (map vector oids (:labels old)))]
                    (when (seq renames) [name renames])))))
        type-changes))

(defn- rows-by-previous-key! [table rows key-indexes check-running!]
  (let [seen (volatile! #{})
        columns (mapv (:columns table) key-indexes)]
    (mapv (fn [[_ values]]
            (check-running!)
            (let [key (previous-key columns (mapv values key-indexes))]
              (require! (and (every? some? key) (not (contains? @seen key)))
                        "Schema refresh found a missing or duplicate value in the previous primary key"
                        {:table (relation-name table) :reason :ambiguous-previous-key})
              (vswap! seen conj key)
              [key values]))
          rows)))

(defn- reconcile-table-tx [db table key-indexes staged-schema staged-rows renamed-labels check-running!]
  (let [attrs (local-column-attrs db table)
        value-types (mapv #(get-in (dbi/-schema db) [% :db/valueType]) attrs)
        _ (require! (= staged-schema value-types) "Staged storage types differ from live storage types"
                    {:table (relation-name table)})
        key-attrs (mapv #(nth attrs %) key-indexes)
        key-columns (mapv (:columns table) key-indexes)
        key-renames (mapv #(get renamed-labels (get-in % [:user-type :name]) {}) key-columns)
        live-rows (stored-rows db table key-attrs)
        live (into {} (map (fn [[eid key]]
                             [(previous-key key-columns (mapv #(get %1 %2 %2) key-renames key)) eid]))
                   live-rows)
        ;; Retract every old spelling before adding any new one. A label
        ;; swap or reuse can otherwise collide with a native unique key.
        renamed-key-tx (when (some seq key-renames)
                         (into []
                               (mapcat (fn [[eid values]]
                                         (keep (fn [[attr renames value]]
                                                 (when (contains? renames value) [:db/retract eid attr value]))
                                               (map vector key-attrs key-renames values))))
                               live-rows))
        outstanding (volatile! live)
        marker (pgs/row-marker-attr (:name table))
        result (reduce
                (fn [tx [key values]]
                  (check-running!)
                  (doseq [[column value] (map vector (:columns table) values)]
                    (require! (or (some? value) (not (:not-null? column)))
                              "Staged row violates publisher nullability"
                              {:table (relation-name table) :column (:name column)}))
                  (let [eid (get @outstanding key)]
                    (vswap! outstanding dissoc key)
                    (if eid
                      (let [row (d/entity db eid)
                            previous (mapv #(get row %) attrs)]
                        (into tx (replace-row-tx eid attrs previous values)))
                      (conj tx (into {marker true}
                                     (keep (fn [[attr value]] (when (some? value) [attr value])))
                                     (map vector attrs values))))))
                [] staged-rows)]
    (into (into (vec renamed-key-tx)
                (map (fn [eid] (check-running!) [:db/retractEntity eid]))
                (vals @outstanding))
          result)))

(defn- reconcile-table! [destination stage-db table live-table key-indexes renamed-labels check-running!]
  (let [attrs (local-column-attrs stage-db table)
        staged-schema (mapv #(get-in (dbi/-schema stage-db) [% :db/valueType]) attrs)
        staged-rows (rows-by-previous-key! table (stored-rows stage-db table attrs) key-indexes check-running!)]
    (stage-data! destination #(reconcile-table-tx % live-table key-indexes staged-schema staged-rows
                                                  renamed-labels check-running!))))

(defn- matches-relation? [table message]
  (and table (= \d (:identity message)) (= (wire-table table) (wire-table message))))

(defn- select-change [included? {:keys [op relation-id relation-ids] :as message}]
  (if (= :truncate op)
    (when-let [ids (seq (filterv included? relation-ids))]
      (assoc message :relation-ids (vec ids)))
    (when (included? relation-id) message)))

(defn- stream!
  "Read ordered protocol v1 transactions. A commit is acknowledged after its
   callback succeeds. Returning true from commit! ends this stream."
  [copy end-lsn confirmed-lsn max-transaction-bytes
   {:keys [tick! relation-frame! select-change! barrier? commit!]}]
  (let [last-packet (volatile! (System/nanoTime))
        next-feedback (volatile! 0)
        feedback-position (volatile! confirmed-lsn)
        reply! (fn [request-reply?]
                 (feedback! copy @feedback-position request-reply?)
                 (vreset! next-feedback (+ (System/nanoTime) (.toNanos TimeUnit/SECONDS 1))))]
    (loop [relations {} pending nil durable end-lsn]
      (tick! (nil? pending))
      (when (>= (System/nanoTime) @next-feedback)
        (reply! true))
      (let [{:keys [^ByteBuffer data reply?] :as packet} (read-packet copy)]
        (when packet (vreset! last-packet (System/nanoTime)))
        (when reply? (reply! false))
        (if data
          (let [size (.remaining data)
                {:keys [op relation-id end-lsn transactional? payload] :as message} (decode data)]
            (case op
              :relation
              (do
                (relation-frame! message)
                (recur (assoc relations relation-id message) pending durable))

              :begin
              (do
                (require! (nil? pending) "Received BEGIN before the previous COMMIT" {})
                (recur relations (assoc message :changes [] :bytes 0) durable))

              (:origin :type)
              (recur relations pending durable)

              :message
              (recur relations (if (and transactional? payload (barrier? payload))
                                 (do
                                   (require! pending "Received a transactional message outside a transaction" {})
                                   (update pending :barriers (fnil conj #{}) payload))
                                 pending)
                     durable)

              :commit
              (do
                (require! (and pending (= (:commit-lsn pending) (:commit-lsn message))
                               (= (:commit-micros pending) (:commit-micros message))
                               (not (neg? (compare-lsn end-lsn (:commit-lsn message)))))
                          "pgoutput COMMIT did not match BEGIN" {:commit message})
                (let [new? (pos? (compare-lsn end-lsn durable))
                      stop? (when new? (commit! relations durable (assoc pending :end-lsn end-lsn)))]
                  (when (pos? (compare-lsn end-lsn @feedback-position))
                    (vreset! feedback-position end-lsn))
                  (reply! false)
                  (if stop? end-lsn (recur relations nil (if new? end-lsn durable)))))

              (let [bytes (+ (or (:bytes pending) 0) size)]
                (require! pending "Received a change outside a pgoutput transaction" {:operation op})
                (require! (<= bytes max-transaction-bytes) "Transaction exceeds the configured byte limit"
                          {:xid (:xid pending) :limit max-transaction-bytes})
                (recur relations
                       (let [change (select-change! message)]
                         (cond-> (assoc pending :bytes bytes)
                           change (update :changes conj change)))
                       durable))))
          (do
            (when-not packet
              (when (> (- (System/nanoTime) @last-packet) (.toNanos TimeUnit/SECONDS 30))
                (throw (SQLException. "Timed out waiting for publisher replication traffic" "08006")))
              (Thread/sleep 20))
            (recur relations pending durable)))))))

(defn- set-copy-format! [destination]
  (sink-execute! destination "SET DateStyle = 'ISO, YMD'")
  (sink-execute! destination "SET TIME ZONE 'UTC'"))

(defn- stage-tables! [{:keys [source-url publication max-snapshot-bytes max-transaction-bytes]}
                      origin manifest {:keys [conn changes tables barrier phase]} check-running!]
  (with-open [replication (connect source-url true)
              source (connect source-url false)
              destination (pg/make-query-handler conn)]
    (require! (= origin (source-identity replication source)) "Publisher changed during table refresh" {})
    (set-copy-format! destination)
    (let [slot (new-slot-name "pgdh_sync")
          on-bytes (snapshot-budget check-running! max-snapshot-bytes)
          end-lsn (with-slot-snapshot!
                    replication source slot true
                    (fn [position]
                      (require-stable-manifest! manifest (publication-manifest source publication))
                      (transaction!
                       destination
                       (fn []
                         (let [relations (mapv :table changes)]
                           (install-types! destination (publisher-types relations))
                           (doseq [table relations]
                             (check-running!)
                             (create-table! destination table)))
                         (doseq [table tables]
                           (check-running!)
                           (copy-table! source destination table on-bytes))))
                      position))
          expected (into {} (map (juxt :relation-id identity)) tables)
          included? (set (keys expected))]
      (reset! phase :catching-up)
      (stream! (replication-copy replication slot publication end-lsn)
               end-lsn end-lsn max-transaction-bytes
               {:tick! (fn [_] (check-running!))
                :barrier? #(= barrier %)
                :relation-frame! (fn [{:keys [relation-id] :as message}]
                                   (when-let [table (get expected relation-id)]
                                     (require! (matches-relation? table message)
                                               "Staged relation changed" {:relation relation-id})))
                :select-change! #(select-change included? %)
                :commit! (fn [_ _ {:keys [changes bytes barriers]}]
                           (when (seq changes)
                             (on-bytes bytes)
                             (transaction! destination
                                           #(doseq [change changes] (apply-change! destination expected change))))
                           (contains? barriers barrier))}))))

(defn- start-refresh! [config origin manifest changes check-running!]
  (let [tables (into [] (comp (map :table) (remove :partition-leaves)) changes)
        store {:store {:backend :memory :id (random-uuid)}
               :schema-flexibility :write :keep-history? false :max-string-length 0}
        _ (d/create-database store)
        conn (d/connect store)
        stopped? (atom false)
        done (promise)
        stage {:store store :conn conn :tables tables :changes changes :manifest manifest
               :phase (atom :copying) :barrier (str (UUID/randomUUID))
               :stopped? stopped? :done done}
        running! #(do (check-running!)
                      (require! (not @stopped?) "Table refresh stopped" {}))
        worker (Thread. (fn []
                          (deliver done
                                   (try {:position (stage-tables! config origin manifest stage running!)}
                                        (catch Throwable error {:error error}))))
                        "pg-datahike-table-refresh")]
    (.start worker)
    (assoc stage :worker worker)))

(defn- stop-refresh! [{:keys [worker stopped? conn store]}]
  (reset! stopped? true)
  (.join ^Thread worker)
  (try (d/release conn) (finally (d/delete-database store))))

(defn- refresh-result! [stage check-running!]
  (let [deadline (+ (System/nanoTime) (.toNanos TimeUnit/MINUTES 10))]
    (loop []
      (check-running!)
      (require! (< (System/nanoTime) deadline) "Timed out waiting for table refresh cutover" {})
      (if-let [{:keys [position error]} (deref (:done stage) 50 nil)]
        (if error (throw error) position)
        (recur)))))

(defn- set-table-name! [destination storage-name logical-name]
  (stage-data! destination
               (fn [db]
                 (catalog-objects/rename-table-tx db (pgs/storage-table-oid db storage-name)
                                                  logical-name))))

(defn- retire-partition! [destination table]
  ;; Keep the old attributes and entities for AS OF. The catalog advances
  ;; to a fresh storage name if the publisher later reuses the SQL name.
  (let [storage (:name (storage-table table))
        retired-name (new-slot-name "pgdh_retired")]
    (stage-data!
     destination
     (fn [db]
       (let [oid (pgs/storage-table-oid db storage)
             object (catalog-objects/object-by-address db catalog-objects/pg-class-oid oid)
             eid (:db/id object)
             row-type (catalog-objects/object-by-identity
                       db catalog-objects/pg-type-oid catalog-objects/public-namespace-oid (:name table))
             primary (catalog-objects/primary-index-oid db oid)
             entity (d/entity db eid)
             attr :datahike.pg.partition/retired-storage
             schema-tx (when-not (contains? (dbi/-schema db) attr)
                         [{:db/ident attr :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])]
         (into (cond-> (conj (vec schema-tx) [:db/add eid attr storage])
                 row-type (conj [:db/add (:db/id row-type) :datahike.pg.object/kind :retired-row-type]))
               (concat (catalog-objects/rename-table-tx db oid retired-name)
                       (when primary (catalog-objects/drop-object-tx db catalog-objects/pg-class-oid primary))
                       (keep (fn [attr]
                               (when-let [value (get entity attr)]
                                 [:db/retract eid attr value]))
                             [:datahike.pg.partition/definition :datahike.pg.partition/parent-oid
                              :datahike.pg.partition/key :datahike.pg.partition/key-definition
                              :datahike.pg.partition/bound]))))))))

(defn- install-staged-tables! [stage destination check-running! max-snapshot-bytes]
  (let [on-bytes (snapshot-budget check-running! max-snapshot-bytes)
        stage-db (d/db (:conn stage))
        {:keys [changes removed installed-manifest refresh-topology? type-changes]} stage
        renamed-labels (enum-renames type-changes)
        rekeys (filterv (fn [{:keys [table previous]}]
                          (and previous (primary-key-changed? previous table)))
                        changes)
        renames (into []
                      (keep (fn [{:keys [table previous storage-name]}]
                              (when (and previous (not= (:name previous) (:name table)))
                                [storage-name (:name table)])))
                      changes)]
    (with-open [staged (pg/make-query-handler (:conn stage))]
      (set-copy-format! staged)
      (install-types! destination (:new-types stage))
      (doseq [change (vals type-changes)]
        (stage-data! destination #(refresh-type-tx % change)))
      (doseq [table removed]
        (check-running!)
        (retire-partition! destination table))
      (doseq [{:keys [previous storage-name]} rekeys :when (seq (key-columns previous))]
        (check-running!)
        (drop-primary-key! destination (assoc previous :name storage-name)))
      (doseq [{:keys [table previous storage-name]} changes :when previous]
        (check-running!)
        (let [live-table (assoc table :name storage-name)]
          (adapt-layout! destination stage-db previous live-table table)
          (when-not (:partition-leaves table)
            (reconcile-table! destination stage-db table live-table
                              (matching-key-indexes! previous table) renamed-labels check-running!))
          (set-column-metadata! destination live-table)))
      ;; Vacate every old SQL name before assigning the new names. This
      ;; handles name swaps and tables added under a just-vacated name.
      (doseq [[storage] renames]
        (set-table-name! destination storage (new-slot-name "pgdh_rename")))
      (doseq [[storage logical] renames]
        (set-table-name! destination storage logical))
      (doseq [{:keys [table previous storage-name]} changes :when (nil? previous)]
        (check-running!)
        (let [live-table (assoc table :name storage-name)]
          (create-table! destination live-table)
          (when-not (:partition-leaves table)
            (transfer-staged-table! staged destination table live-table on-bytes))
          (when (not= storage-name (:name table))
            (set-table-name! destination storage-name (:name table)))))
      ;; Relation and index names share a PostgreSQL namespace. Install
      ;; the replacement keys after retiring old names across the batch.
      (doseq [{:keys [table storage-name]} rekeys]
        (check-running!)
        (when (seq (key-columns table))
          (install-primary-key! destination (assoc table :name storage-name))))
      (when refresh-topology?
        (install-partition-topology! destination installed-manifest)))))

(defn- changed-relation-ids [changes]
  (into #{} (mapcat #(if (= :truncate (:op %)) (:relation-ids %) [(:relation-id %)])) changes))

(defn- installed-manifest [current previous changes removed type-changes]
  ;; Legacy tables retain their stored definitions until they participate
  ;; in a refresh. Observing their source metadata does not install it.
  (let [installed (into {}
                        (map (fn [{:keys [table storage-name]}]
                               [(:relation-id table)
                                (cond-> table
                                  (not= (:name table) storage-name) (assoc :storage-name storage-name))]))
                        changes)
        prior (relations-by-id previous)
        types (into (or (:types previous) (sorted-map))
                    (filter (fn [[name _]]
                              (or (not (contains? (:types previous) name))
                                  (contains? type-changes name))))
                    (:types current))
        retired (into (or (:retired-storage previous) #{})
                      (map (comp :name storage-table)) removed)
        layouts (fn [tables] (mapv #(or (get installed (:relation-id %)) (get prior (:relation-id %))) tables))]
    (cond-> (update current :tables layouts)
      (seq (:partition-parents current)) (update :partition-parents layouts)
      (seq retired) (assoc :retired-storage retired)
      (seq types) (assoc :types types)
      (:last-schema-refresh previous) (assoc :last-schema-refresh (:last-schema-refresh previous)))))

(defn- consume! [copy source destination saved state confirmed-lsn check-running! ready! config]
  (let [{:keys [origin manifest end-lsn publication slot]} saved
        {:keys [max-transaction-bytes max-snapshot-bytes schema-refresh?]} config
        applied (volatile! manifest)
        durable (volatile! end-lsn)
        pending-schema (volatile! (or (:pending-schema manifest) {}))
        live (volatile! (tables-by-id manifest))
        observed (volatile! @live)
        stage (volatile! nil)
        stage-signaled? (volatile! false)
        following? (volatile! false)
        _ (command! source "SET synchronous_commit = local")
        _ (set-copy-format! destination)
        initial (emit-barrier! source)
        initial-deadline (+ (System/nanoTime) (.toNanos TimeUnit/MINUTES 10))
        next-watermark (volatile! (+ (System/nanoTime) (.toNanos TimeUnit/SECONDS 30)))
        next-check (volatile! 0)
        refresh! (fn []
                   (let [current (publication-manifest source publication)]
                     (vreset! observed (tables-by-id current))
                     (if-let [active @stage]
                       (require-stable-manifest! (:source-manifest active) current)
                       (let [affected (partition-affected @applied current)
                             removed (removed-relations @applied current)
                             type-changes (type-changes! @applied current schema-refresh?)
                             changes (changed-tables! @applied current affected removed type-changes schema-refresh?)]
                         (when (or (seq changes) (seq removed))
                           (doseq [{:keys [table previous]} changes
                                   :when (and previous (nil? (:partition-leaves table)))]
                             (matching-key-indexes! previous table))
                           (let [pending-tables (into removed (keep :previous) changes)
                                 new-pending (into {}
                                                   (comp (remove :partition-leaves)
                                                         (map (fn [table] [(:relation-id table) @durable])))
                                                   pending-tables)
                                 manifest (installed-manifest current @applied changes removed type-changes)]
                             (vswap! pending-schema #(merge new-pending %))
                             (vreset! stage (assoc (start-refresh! config origin current changes check-running!)
                                                   :installed-manifest manifest
                                                   :source-manifest current
                                                   :removed removed
                                                   :type-changes type-changes
                                                   :refresh-topology? (or (seq (:partition-parents @applied))
                                                                          (seq (:partition-parents current)))
                                                   :new-types (apply dissoc (:types current) (keys (:types @applied)))))
                             (vreset! stage-signaled? false)))))))
        live? (fn [id] (and (contains? @live id) (not (contains? @pending-schema id))))
        validate-transaction! (fn [relations changes]
                                (let [ids (changed-relation-ids changes)
                                      stale? (fn [id]
                                               (or (and (not (contains? @observed id)) (not (contains? @live id)))
                                                   (and (live? id)
                                                        (not (matches-relation? (get @live id)
                                                                                (get relations id))))))
                                      used-types (into {} (comp (filter live?)
                                                                (mapcat #(:columns (get @live %)))
                                                                (keep (fn [{:keys [oid user-type]}]
                                                                        (when user-type [oid user-type]))))
                                                       ids)]
                                  ;; A changed enum spelling may identify a
                                  ;; different key without any wire change.
                                  (when (or (some stale? ids)
                                            (when (seq used-types)
                                              (let [current (read-source-types source used-types)]
                                                (some (fn [[oid before]]
                                                        (not (same-type-definition? before (get current oid))))
                                                      used-types))))
                                    (refresh!))
                                  (when-let [id (some #(when (stale? %) %) ids)]
                                    (throw (ex-info "Publication relation changed"
                                                    {:relation (get relations id)})))))
        apply-live! (fn [previous transaction cutover? active]
                      (let [candidate (cond
                                        cutover?
                                        (cond-> (:installed-manifest active)
                                          (seq @pending-schema)
                                          (assoc :last-schema-refresh
                                                 {:from @pending-schema :cutover (:end-lsn transaction)}))

                                        (not= @pending-schema (or (:pending-schema @applied) {}))
                                        (assoc @applied :pending-schema @pending-schema))
                            changes (into [] (keep #(select-change live? %)) (:changes transaction))
                            relations (into {} (map (fn [id] [id (storage-table (get @live id))]))
                                            (changed-relation-ids changes))]
                        (apply-commit! destination relations previous
                                       (cond-> (assoc transaction :changes changes)
                                         candidate (assoc :manifest candidate)
                                         cutover?
                                         (assoc :install! #(install-staged-tables! active % check-running! max-snapshot-bytes))))
                        (when candidate (vreset! applied candidate))))]
    (swap! state assoc :last-schema-refresh (:last-schema-refresh manifest))
    (try
      (stream!
       copy end-lsn confirmed-lsn max-transaction-bytes
       {:tick! (fn [between-transactions?]
                 (check-running!)
                 (let [now (System/nanoTime)]
                   (when-not @following?
                     (require! (< now initial-deadline) "Timed out waiting for the initial catch-up barrier" {}))
                   (when (>= now @next-check)
                     (checked-running-slot! source slot state)
                     (refresh!)
                     (vreset! next-check (+ now (.toNanos TimeUnit/SECONDS 5)))
                     (when (and @following? between-transactions? (>= now @next-watermark)
                                (> (or (:unconfirmed-wal-bytes @state) 0) (* 16 1024 1024)))
                       (emit-barrier! source)
                       (vreset! next-watermark (+ now (.toNanos TimeUnit/SECONDS 30)))))
                   (when-let [active @stage]
                     (when-let [error (:error (deref (:done active) 0 nil))] (throw error))
                     (let [phase @(:phase active)
                           status {:state phase :tables (mapv relation-name (:tables active))}]
                       (when-not (= status (:refresh @state))
                         (swap! state assoc :refresh status))
                       (when (and between-transactions? (= :catching-up phase) (not @stage-signaled?))
                         (emit-barrier! source (:barrier active))
                         (vreset! stage-signaled? true))))))
        :barrier? (fn [payload] (or (= initial payload) (= (:barrier @stage) payload)))
        :relation-frame! (constantly nil)
        :select-change! identity
        :commit! (fn [relations previous {:keys [end-lsn xid commit-micros barriers changes] :as transaction}]
                   ;; A reconnect can decode old relation layouts below the
                   ;; durable checkpoint. Validate only changes we will apply.
                   (validate-transaction! relations changes)
                   (let [active @stage
                         cutover? (contains? barriers (:barrier active))
                         initial? (contains? barriers initial)]
                     (when (or cutover? initial?) (refresh!))
                     (when cutover?
                       (require! (= end-lsn (refresh-result! active check-running!))
                                 "Table refresh reached a different cutover position" {:position end-lsn})
                       (refresh!))
                     (try
                       (apply-live! previous transaction cutover? active)
                       (catch clojure.lang.ExceptionInfo error
                         ;; DROP NOT NULL preserves the wire layout. A NULL may
                         ;; beat the periodic catalog scan to the mirror.
                         (let [prior @pending-schema]
                           (when-not (and schema-refresh? (not cutover?) (= "23502" (:sqlstate (ex-data error))))
                             (throw error))
                           (refresh!)
                           (when (= prior @pending-schema) (throw error))
                           (apply-live! previous transaction false nil))))
                     (vreset! durable end-lsn)
                     (swap! state assoc :applied-lsn end-lsn :source-xid xid :source-commit-micros commit-micros)
                     (when cutover?
                       (vreset! live (tables-by-id @applied))
                       (vreset! pending-schema {})
                       (vreset! stage nil)
                       (stop-refresh! active)
                       (swap! state #(-> % (dissoc :refresh)
                                         (assoc :last-schema-refresh (:last-schema-refresh @applied)))))
                     (when initial?
                       (ready!)
                       (vreset! following? true))
                     false))})
      (finally
        (when-let [active @stage] (stop-refresh! active))
        (swap! state dissoc :refresh)))))

(defn- replicate! [{:keys [source-url publication slot-prefix max-snapshot-bytes] :as config}
                   destination state check-running! ready!]
  (with-open [replication (connect source-url true)
              source (connect source-url false)]
    (let [origin (source-identity replication source)
          saved (or (checkpoint destination)
                    (let [slot (new-slot-name slot-prefix)]
                      ;; Validate before creating a slot that pins WAL.
                      (publication-manifest source publication)
                      (init-checkpoint! destination origin slot publication)
                      (checkpoint destination)))]
      (require! (= [origin publication] ((juxt :origin :publication) saved))
                "The mirror belongs to a different publisher, timeline, or publication"
                {:saved-origin (:origin saved) :origin origin :saved-publication (:publication saved)})
      (swap! state assoc :slot (:slot saved) :applied-lsn (:end-lsn saved))
      (when-not (:end-lsn saved)
        (swap! state assoc :state :snapshot)
        (bootstrap! replication source destination saved check-running! max-snapshot-bytes)))
    (let [{:keys [slot end-lsn] :as saved} (checkpoint destination)
          {:keys [confirmed-lsn] :as info} (validate-resume! source saved)]
      (publish-status! state info)
      (swap! state assoc :state :catching-up :applied-lsn end-lsn)
      (consume! (replication-copy replication slot publication confirmed-lsn)
                source destination saved state confirmed-lsn check-running! ready! config))))

(defn- retryable? [error]
  (and (instance? SQLException error)
       (let [sqlstate (.getSQLState ^SQLException error)]
         (or (some-> sqlstate (str/starts-with? "08"))
             (#{"55006" "57P01" "57P02" "57P03"} sqlstate)))))

(defn- mirror-error [error]
  (merge {:message (if (instance? SQLException error)
                     (or (when (instance? PSQLException error)
                           (some-> ^PSQLException error .getServerErrorMessage .getMessage))
                         "Publisher connection or replication command failed")
                     (ex-message error))}
         (when (instance? SQLException error)
           {:sqlstate (.getSQLState ^SQLException error)})
         (ex-data error)))

(defn- open-store [directory]
  (let [path (.getCanonicalPath (io/file directory))
        config {:store {:backend :file :path path
                        :id (UUID/nameUUIDFromBytes (.getBytes path StandardCharsets/UTF_8))
                        :config {:sync-blob? true :sync-store? true}}
                :schema-flexibility :write :keep-history? true :max-string-length 0}]
    (when-not (d/database-exists? config)
      (d/create-database config))
    (d/connect config)))

(defn- serve! [config conn state stopping?]
  (let [listener (atom nil)
        check-running! (fn []
                         (when @stopping?
                           (throw (ex-info "Mirror stopped" {}))))
        ready! (fn []
                 (check-running!)
                 (when-not @listener
                   (reset! listener (pg/start-server conn (assoc (:server config) :database-read-only? true))))
                 (let [^PgWireServer server (:server @listener)]
                   (swap! state assoc :state :following :port (.getPort server) :last-error nil)))]
    (try
      (with-open [destination (pg/make-query-handler conn)]
        (loop [retry-ms 250]
          (when-not @stopping?
            (let [error (try
                          (replicate! config destination state check-running! ready!)
                          (catch Exception e e))]
              (when-not @stopping?
                (let [retry? (retryable? error)
                      delay (if (= :following (:state @state)) 250 retry-ms)]
                  (swap! state assoc :state (if retry? :reconnecting :paused)
                         :last-error (mirror-error error))
                  (if retry?
                    (do (Thread/sleep (long delay))
                        (recur (min 5000 (* 2 delay))))
                    (while (not @stopping?)
                      (Thread/sleep 100)))))))))
      (finally
        (when @listener
          (pg/stop-server @listener))))))

(defn- acquire-store-lock [directory]
  (let [file (io/file (str directory ".mirror-lock"))
        channel (FileChannel/open (.toPath file)
                                  (into-array StandardOpenOption
                                              [StandardOpenOption/CREATE StandardOpenOption/WRITE]))]
    (try
      (let [lock (try (.tryLock channel) (catch OverlappingFileLockException _ nil))
            marker "pg-datahike logical mirror v1\n"
            bytes (.getBytes marker StandardCharsets/UTF_8)]
        (require! lock "Another mirror is using this store" {:store-path directory})
        (if (zero? (.size channel))
          (let [store (io/file directory)]
            (require! (or (not (.exists store))
                          (and (.isDirectory store) (some-> (.list store) alength zero?)))
                      "The mirror requires a new or previously initialized store path"
                      {:store-path directory})
            (let [buffer (ByteBuffer/wrap bytes)]
              (while (.hasRemaining buffer) (.write channel buffer)))
            (.force channel true))
          (require! (and (= (alength bytes) (.size channel))
                         (= marker (Files/readString (.toPath file))))
                    "The mirror store marker is invalid" {:store-path directory}))
        channel)
      (catch Throwable e
        (.close channel)
        (throw e)))))

(defn start!
  "Start a PostgreSQL 18 subscriber. Bootstrap and catch-up run asynchronously.
   The :server options are passed to the read-only pgwire listener after catch-up.
   Monitor with status and release resources with stop!. Use a dedicated :store-path."
  [{:keys [source-url publication store-path slot-prefix server max-snapshot-bytes max-transaction-bytes schema-refresh?]
    :or {slot-prefix "pgdh_mirror"
         max-snapshot-bytes (* 128 1024 1024)
         max-transaction-bytes (* 64 1024 1024)}
    :as config}]
  (validate-source-options! source-url publication)
  (require! (and (string? slot-prefix) (re-matches #"[a-z0-9_]{1,30}" slot-prefix)
                 (string? store-path) (seq store-path)
                 (pos-int? max-snapshot-bytes) (pos-int? max-transaction-bytes))
            "Expected a store path, slot prefix and positive byte limits" {})
  (require! (or (nil? schema-refresh?) (boolean? schema-refresh?))
            "Expected a boolean schema-refresh? option" {})
  (require! (and (or (nil? server) (map? server))
                 (not-any? #(get server %) [:database-template :on-create-database :on-delete-database]))
            "Mirror listeners do not support database provisioning" {})
  (let [directory (.getCanonicalPath (io/file store-path))
        config (assoc config :slot-prefix slot-prefix :max-snapshot-bytes max-snapshot-bytes
                      :max-transaction-bytes max-transaction-bytes)
        state (atom {:state :starting :publication publication})
        stopping? (atom false)]
    (io/make-parents (str directory ".mirror-lock"))
    (let [lock (acquire-store-lock directory)
          worker (Thread.
                  (fn []
                    (try
                      (with-open [_ lock]
                        (let [conn (open-store directory)]
                          (try (serve! config conn state stopping?)
                               (finally (d/release conn)))))
                      (when @stopping? (swap! state assoc :state :stopped :port nil))
                      (catch Exception e
                        (swap! state assoc :state :failed :port nil :last-error (mirror-error e)))))
                  "pg-datahike-logical-subscriber")]
      (.start worker)
      {:state state :stopping? stopping? :worker worker})))

(defn status
  "Return the mirror's current state and progress. The source timestamp is
   measured in microseconds since PostgreSQL's 2000-01-01 UTC epoch."
  [{:keys [state]}]
  @state)

(defn stop!
  "Stop following and close the read-only listener. The durable checkpoint
   and publisher slot are kept for the next start."
  [{:keys [stopping? ^Thread worker] :as mirror}]
  (reset! stopping? true)
  (.join worker)
  (status mirror))

(defn- run-mirror! [config]
  (let [mirror (start! config)]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable #(stop! mirror)))
    (loop [previous nil]
      (let [current (status mirror)]
        (when (not= previous current) (prn current) (flush))
        (if (or (= :failed (:state current))
                (and (= :paused (:state current)) (nil? (:port current))))
          (do (stop! mirror) (System/exit 1))
          (do (Thread/sleep 1000) (recur current)))))))

(defn -main [& args]
  (let [check? (= "--check" (first args))
        paths (if check? (rest args) args)]
    (require! (= 1 (count paths))
              "Usage: clojure -M -m datahike.pg.mirror [--check] path-to-mirror.edn" {})
    (let [source-url (System/getenv "PG_MIRROR_SOURCE_URL")
          config (cond-> (edn/read-string (slurp (first paths)))
                   source-url (assoc :source-url source-url))]
      (if check?
        (let [report (publication-report config)]
          (prn report)
          (flush)
          (when-not (:compatible? report) (System/exit 1)))
        (run-mirror! config)))))
