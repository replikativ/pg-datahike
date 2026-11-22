(ns datahike.pg.mirror
  "Read-only shadow databases fed by a PostgreSQL logical publication."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
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
  {16 "boolean" 20 "bigint" 21 "smallint" 23 "integer" 25 "text"
   700 "real" 701 "double precision" 1043 "varchar" 1082 "date"
   1114 "timestamp" 1184 "timestamptz" 1700 "numeric" 2950 "uuid"})

(defn- require! [condition message data]
  (when-not condition
    (throw (ex-info message data))))

(defn- ident [name]
  (str "\"" (str/replace name "\"" "\"\"") "\""))

(defn- literal [value]
  (str "'" (str/replace value "'" "''") "'"))

(defn- relation-name [{:keys [schema name]}]
  (str (ident schema) "." (ident name)))

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
    (DriverManager/getConnection url properties)))

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

(defn- column-ddl-type [{:keys [oid typmod]}]
  (let [base (sql-types oid)]
    (cond
      (= -1 typmod) base
      (and (= 1043 oid) (> typmod types/var-hdr-sz))
      (str base "(" (- typmod types/var-hdr-sz) ")")
      (and (#{1114 1184} oid) (<= 0 typmod 6)) (str base "(" typmod ")")
      (= 1700 oid)
      (let [[precision scale] (types/decode-numeric-typmod typmod)]
        (when (and precision (<= 1 precision 1000) (<= -1000 scale 1000))
          (str base "(" precision "," scale ")"))))))

(defn- read-publication-row [^ResultSet row]
  (let [schema (.getString row "schemaname")
        relation (when schema
                   {:schema schema :name (.getString row "tablename")
                    :relation-id (.getLong row "relation_id")})
        column-name (.getString row "attname")
        column (when column-name
                 {:name column-name :oid (.getLong row "atttypid")
                  :typmod (.getInt row "atttypmod") :key? (.getBoolean row "is_key")})]
    {:publication-oid (.getLong row "publication_oid")
     :operations {:insert (.getBoolean row "pubinsert") :update (.getBoolean row "pubupdate")
                  :delete (.getBoolean row "pubdelete") :truncate (.getBoolean row "pubtruncate")}
     :relation relation
     :table-issues
     (when relation
       (cond-> []
         (= "pgdh_mirror" schema) (conj {:reason :reserved-schema})
         (.getBoolean row "has_filter") (conj {:reason :row-filter})
         (or (.getBoolean row "relispartition") (= "p" (.getString row "relkind")))
         (conj {:reason :partitioning})
         (not (#{"r" "p"} (.getString row "relkind"))) (conj {:reason :relation-kind})
         (not= "p" (.getString row "relpersistence")) (conj {:reason :nonpermanent-table})
         (not= "d" (.getString row "relreplident"))
         (conj {:reason :replica-identity :mode (.getString row "relreplident")})
         (.getBoolean row "has_inheritance") (conj {:reason :inheritance})
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
                 (not (contains? sql-types (:oid column))) (conj {:reason :type :oid (:oid column)})
                 (and (contains? sql-types (:oid column)) (nil? (column-ddl-type column)))
                 (conj {:reason :type-modifier :typmod (:typmod column)})))))}))

(defn- scan-publication [source publication]
  (let [rows (query source
                    (str "SELECT pub.oid AS publication_oid, pub.pubinsert, pub.pubupdate, pub.pubdelete, pub.pubtruncate, "
                         "       p.schemaname, p.tablename, c.oid AS relation_id, p.rowfilter IS NOT NULL AS has_filter, "
                         "       c.relkind, c.relpersistence, c.relreplident, c.relispartition, "
                         "       pg_catalog.row_security_active(c.oid) AS has_row_security, "
                         "       EXISTS (SELECT 1 FROM pg_catalog.pg_inherits "
                         "               WHERE inhrelid = c.oid OR inhparent = c.oid) AS has_inheritance, "
                         "       a.attname, a.atttypid, a.atttypmod, a.attgenerated, "
                         "       pg_catalog.format_type(a.atttypid, a.atttypmod) AS source_type, "
                         "       a.attname = ANY(p.attnames) AS is_published, "
                         "       pg_catalog.has_column_privilege(c.oid, a.attnum, 'SELECT') AS can_select, "
                         "       EXISTS (SELECT 1 FROM pg_catalog.pg_index i "
                         "               CROSS JOIN LATERAL unnest(i.indkey) WITH ORDINALITY AS k(attnum, pos) "
                         "               WHERE i.indrelid = c.oid AND i.indisprimary AND i.indimmediate "
                         "               AND i.indisvalid AND k.pos <= i.indnkeyatts AND k.attnum = a.attnum) AS is_key "
                         "FROM pg_catalog.pg_publication pub "
                         "LEFT JOIN pg_catalog.pg_publication_tables p ON p.pubname = pub.pubname "
                         "LEFT JOIN pg_catalog.pg_namespace n ON n.nspname = p.schemaname "
                         "LEFT JOIN pg_catalog.pg_class c ON c.relnamespace = n.oid AND c.relname = p.tablename "
                         "LEFT JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped "
                         "WHERE pub.pubname = ? ORDER BY p.schemaname, p.tablename, a.attnum")
                    [publication] read-publication-row)
        {:keys [publication-oid operations]} (first rows)
        tables (mapv (fn [table-rows]
                       (let [{:keys [relation table-issues]} (first table-rows)
                             columns (into [] (keep :column) table-rows)
                             issues (into table-issues (mapcat :column-issues) table-rows)]
                         (assoc relation :columns columns
                                :issues (cond-> issues
                                          (empty? columns) (conj {:reason :no-columns})
                                          (not-any? :key? columns) (conj {:reason :missing-immediate-primary-key})))))
                     (partition-by :relation (filter :relation rows)))
        disabled (into [] (remove #(get operations %)) [:insert :update :delete :truncate])
        issues (cond
                 (nil? publication-oid) [{:reason :publication-not-found}]
                 :else (cond-> []
                         (empty? tables) (conj {:reason :empty-publication})
                         (seq disabled) (conj {:reason :disabled-operations :operations disabled})))]
    {:publication publication :publication-oid publication-oid :issues issues :tables tables
     :compatible? (and (empty? issues) (every? (comp empty? :issues) tables))}))

(defn- publication-manifest [source publication]
  (let [{:keys [publication-oid tables compatible?] :as report} (scan-publication source publication)]
    (require! compatible? "Publication cannot be mirrored" {:report report})
    {:publication-oid publication-oid :tables (mapv #(dissoc % :issues) tables)}))

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
  (checked-result (.execute destination sql)))

(defn- number-bind-markers [tokens]
  ;; The executor may reorder an UPDATE's assignments before reparsing them
  ;; as a projection. Like pgjdbc, retain each parameter's protocol number.
  (map-indexed (fn [index {:keys [pos end]}] [pos end (str "$" (inc index))])
               (filter #(and (= :param (:type %)) (= "?" (:text %))) tokens)))

(defn- sink-prepared! ^PgWireServer$QueryResult [^PgWireServer$QueryHandler destination sql args]
  (let [oids (int-array (repeat (count args) PgWireServer/OID_VARCHAR))
        statement (rewrite/rewrite sql [number-bind-markers])]
    (checked-result (.executePrepared destination (.parse destination statement oids)
                                      (object-array (cons nil args))))))

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

(defn- transfer-staged-table! [stage destination table on-bytes]
  (let [export (sink-execute! stage (str "COPY " (copy-relation table) " TO STDOUT"))]
    (require! (.-copyOutMode export) "Expected COPY OUT response" {:table (relation-name table)})
    (let [lines (.-copyOutLines export)
          written (load-copy! destination table
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

(defn- create-table! [destination {:keys [columns] :as table}]
  (sink-execute!
   destination
   (str "CREATE TABLE " (relation-name table) " ("
        (comma-list (map (fn [{:keys [name] :as column}] (str (ident name) " " (column-ddl-type column))) columns))
        ", PRIMARY KEY (" (comma-list (keep #(when (:key? %) (ident (:name %))) columns)) "))")))

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
      (command! source "SET LOCAL DateStyle = 'ISO, YMD'")
      (command! source "SET LOCAL TIME ZONE 'UTC'")
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
             (doseq [table (:tables manifest)]
               (check-running!)
               (create-table! destination table)
               (copy-table! source destination table on-bytes))
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

(defn- cast-param [{:keys [oid]}]
  (str "CAST(? AS " (sql-types oid) ")"))

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

(defn- added-tables!
  "Require the publisher to retain each admitted relation and its wire layout."
  [previous current]
  (let [before (tables-by-id previous)
        after (tables-by-id current)
        changed (into [] (keep (fn [[id table]] (when-not (= table (get after id)) table))) before)]
    (require! (and (= (:publication-oid previous) (:publication-oid current)) (empty? changed))
              "Publication schema or membership changed"
              {:changed (mapv #(select-keys % [:schema :name :relation-id]) changed)})
    (into [] (remove #(contains? before (:relation-id %))) (:tables current))))

(defn- matches-relation? [table message]
  (and (= \d (:identity message))
       (= table (select-keys message [:schema :name :relation-id :columns]))))

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

              :origin
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
                      origin manifest {:keys [conn tables barrier phase]} check-running!]
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
                      (added-tables! manifest (publication-manifest source publication))
                      (transaction!
                       destination
                       (fn []
                         (doseq [table tables]
                           (check-running!)
                           (create-table! destination table)
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
                :commit! (fn [relations _ {:keys [changes bytes barriers]}]
                           (when (seq changes)
                             (on-bytes bytes)
                             (transaction! destination
                                           #(doseq [change changes] (apply-change! destination relations change))))
                           (contains? barriers barrier))}))))

(defn- start-refresh! [config origin manifest tables check-running!]
  (let [store {:store {:backend :memory :id (random-uuid)}
               :schema-flexibility :write :keep-history? false :max-string-length 0}
        _ (d/create-database store)
        conn (d/connect store)
        stopped? (atom false)
        done (promise)
        stage {:store store :conn conn :tables tables :manifest manifest
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

(defn- install-staged-tables! [stage destination check-running! max-snapshot-bytes]
  (let [on-bytes (snapshot-budget check-running! max-snapshot-bytes)]
    (with-open [staged (pg/make-query-handler (:conn stage))]
      (set-copy-format! staged)
      (doseq [table (:tables stage)]
        (check-running!)
        (create-table! destination table)
        (transfer-staged-table! staged destination table on-bytes)))))

(defn- consume! [copy source destination saved state confirmed-lsn check-running! ready! config]
  (let [{:keys [origin manifest end-lsn publication slot]} saved
        {:keys [max-transaction-bytes max-snapshot-bytes]} config
        applied (volatile! manifest)
        live-ids (volatile! (set (keys (tables-by-id manifest))))
        observed (volatile! (tables-by-id manifest))
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
                   (let [current (publication-manifest source publication)
                         added (added-tables! @applied current)]
                     (vreset! observed (tables-by-id current))
                     (if-let [active @stage]
                       (added-tables! (:manifest active) current)
                       (when (seq added)
                         (vreset! stage (start-refresh! config origin current added check-running!))
                         (vreset! stage-signaled? false)))))]
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
        :relation-frame! (fn [{:keys [relation-id] :as message}]
                           (when-not (matches-relation? (get @observed relation-id) message)
                             (refresh!))
                           (require! (matches-relation? (get @observed relation-id) message)
                                     "Publication relation changed" {:relation (dissoc message :op)}))
        :select-change! #(select-change @live-ids %)
        :commit! (fn [relations previous {:keys [end-lsn xid commit-micros barriers] :as transaction}]
                   (let [active @stage
                         cutover? (contains? barriers (:barrier active))
                         initial? (contains? barriers initial)]
                     (when (or cutover? initial?) (refresh!))
                     (when cutover?
                       (require! (= end-lsn (refresh-result! active check-running!))
                                 "Table refresh reached a different cutover position" {:position end-lsn})
                       (refresh!))
                     (apply-commit! destination relations previous
                                    (cond-> transaction
                                      cutover?
                                      (assoc :manifest (:manifest active)
                                             :install! #(install-staged-tables! active % check-running! max-snapshot-bytes))))
                     (swap! state assoc :applied-lsn end-lsn :source-xid xid :source-commit-micros commit-micros)
                     (when cutover?
                       (vreset! applied (:manifest active))
                       (vreset! live-ids (set (keys (tables-by-id @applied))))
                       (vreset! stage nil)
                       (stop-refresh! active)
                       (swap! state dissoc :refresh))
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
  [{:keys [source-url publication store-path slot-prefix server max-snapshot-bytes max-transaction-bytes]
    :or {slot-prefix "pgdh_mirror"
         max-snapshot-bytes (* 128 1024 1024)
         max-transaction-bytes (* 64 1024 1024)}
    :as config}]
  (validate-source-options! source-url publication)
  (require! (and (string? slot-prefix) (re-matches #"[a-z0-9_]{1,30}" slot-prefix)
                 (string? store-path) (seq store-path)
                 (pos-int? max-snapshot-bytes) (pos-int? max-transaction-bytes))
            "Expected a store path, slot prefix and positive byte limits" {})
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
