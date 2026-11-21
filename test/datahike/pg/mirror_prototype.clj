(ns datahike.pg.mirror-prototype
  "A local prototype for a whole-table PostgreSQL publication. Each source
   commit and its replay checkpoint share one pg-datahike SQL transaction."
  (:require [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.sql Connection DriverManager PreparedStatement ResultSet Types]
           [java.util Properties]
           [java.util.concurrent TimeUnit]
           [org.postgresql PGConnection]
           [org.postgresql.replication LogSequenceNumber PGReplicationStream]))

(def ^:private sql-types
  {16 "boolean" 20 "bigint" 21 "smallint" 23 "integer" 25 "text"})

(defn- require! [condition message data]
  (when-not condition
    (throw (ex-info message data))))

(defn- validate-names! [slot publication]
  (require! (and (string? slot) (re-matches #"[a-z0-9_]{1,63}" slot)
                 (string? publication) (re-matches #"[a-z_][a-z0-9_]{0,62}" publication))
            "Expected ordinary PostgreSQL slot and publication names"
            {:slot slot :publication publication}))

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

(defn- update! [^Connection conn sql args]
  (with-open [stmt (bind! (.prepareStatement conn sql) args)]
    (.executeUpdate stmt)))

(defn- transaction! [^Connection conn f]
  (require! (.getAutoCommit conn) "Expected a connection with no open transaction" {})
  (.setAutoCommit conn false)
  (try
    (let [result (f)]
      (.commit conn)
      result)
    (catch Throwable error
      (try (.rollback conn)
           (catch Throwable rollback-error (.addSuppressed error rollback-error)))
      (throw error))
    (finally (.setAutoCommit conn true))))

(defn- connect [url] (DriverManager/getConnection url))

(defn- connect-replication [url]
  (DriverManager/getConnection
   url
   (doto (Properties.)
     (.setProperty "replication" "database")
     (.setProperty "assumeMinServerVersion" "10")
     (.setProperty "preferQueryMode" "simple"))))

(defn- source-identity [^Connection conn]
  (with-open [stmt (.createStatement conn)
              result (.executeQuery stmt "IDENTIFY_SYSTEM")]
    (require! (.next result) "IDENTIFY_SYSTEM returned no row" {})
    {:source-system (.getString result "systemid")
     :source-database (.getString result "dbname")}))

(defn- validate-publication! [source {:keys [publication tables]}]
  (let [operations (first (query source
                                 (str "SELECT pubinsert, pubupdate, pubdelete, pubtruncate "
                                      "FROM pg_publication WHERE pubname = ?")
                                 [publication]
                                 (fn [^ResultSet r] (mapv #(.getBoolean r (int %)) (range 1 5)))))
        rows (query source
                    (str "SELECT p.schemaname, p.tablename, p.rowfilter, "
                         "       c.relkind, c.relpersistence, c.relreplident, "
                         "       a.attname, a.atttypid, a.attgenerated, "
                         "       a.attname = ANY(p.attnames), "
                         "       EXISTS (SELECT 1 FROM pg_index i WHERE i.indrelid = c.oid "
                         "               AND i.indisprimary AND a.attnum = ANY(i.indkey)) "
                         "FROM pg_publication_tables p "
                         "JOIN pg_namespace n ON n.nspname = p.schemaname "
                         "JOIN pg_class c ON c.relnamespace = n.oid AND c.relname = p.tablename "
                         "JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped "
                         "WHERE p.pubname = ? ORDER BY p.schemaname, p.tablename, a.attnum")
                    [publication]
                    (fn [^ResultSet r]
                      {:relation [(.getString r 1) (.getString r 2)]
                       :ordinary? (and (nil? (.getString r 3))
                                       (= "r" (.getString r 4))
                                       (= "p" (.getString r 5))
                                       (= "d" (.getString r 6))
                                       (= "" (.getString r 9))
                                       (.getBoolean r 10))
                       :column {:name (.getString r 7) :oid (.getLong r 8) :key? (.getBoolean r 11)}}))
        actual (group-by :relation rows)
        expected (into {} (map (juxt (juxt :schema :name) :columns)) tables)]
    (require! (= [true true true true] operations) "Expected all four publication operations" {:publication publication})
    (require! (= (set (keys expected)) (set (keys actual))) "Publication tables differ from the configured tables"
              {:expected (set (keys expected)) :actual (set (keys actual))})
    (doseq [[relation columns] expected
            :let [published (get actual relation)]]
      (require! (and (some :key? columns)
                     (every? #(contains? sql-types (:oid %)) columns)
                     (every? :ordinary? published)
                     (= columns (mapv :column published)))
                "Expected an unfiltered keyed table with the configured column types"
                {:relation relation :expected columns :actual published}))))

(defn- copy-table! [^Connection source ^Connection destination table during-copy]
  (let [columns (comma-list (map (comp ident :name) (:columns table)))
        table-name (relation-name table)
        source-api (.getCopyAPI (.unwrap source PGConnection))
        destination-api (.getCopyAPI (.unwrap destination PGConnection))
        out (.copyOut source-api (str "COPY " table-name " (" columns ") TO STDOUT"))]
    (try
      (let [in (.copyIn destination-api (str "COPY " table-name " (" columns ") FROM STDIN"))]
        (try
          (loop [first-chunk? true]
            (when-let [^bytes chunk (.readFromCopy out)]
              (when (and first-chunk? during-copy) (during-copy table))
              (.writeToCopy in chunk 0 (alength chunk))
              (recur false)))
          (let [written (.endCopy in)]
            (require! (= written (.getHandledRowCount out)) "COPY row counts differ"
                      {:table table-name :source (.getHandledRowCount out) :destination written})
            written)
          (finally (when (.isActive in) (.cancelCopy in)))))
      (finally (when (.isActive out) (.cancelCopy out))))))

(def ^:private checkpoint-table "pgdh_mirror_checkpoint")

(defn bootstrap!
  "Load an exported slot snapshot into empty destination tables and commit
   its starting LSN with the rows. The caller supplies matching schemas.
   during-copy runs on the first chunk of each copied table for tests."
  [{:keys [source-url destination-url slot publication tables during-copy] :as config}]
  (validate-names! slot publication)
  (with-open [replication (connect-replication source-url)
              source (connect source-url)
              destination (connect destination-url)]
    (let [{:keys [source-system source-database]} (source-identity replication)
          api (.getReplicationAPI (.unwrap replication PGConnection))
          slot-info (-> api .createReplicationSlot .logical (.withSlotName slot)
                        (.withOutputPlugin "pgoutput") .make)
          snapshot (.getSnapshotName slot-info)
          initial-lsn (.asString (.getConsistentPoint slot-info))]
      (require! (seq snapshot) "The slot did not export a snapshot" {:slot slot})
      (command! source "BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY")
      (try
        (command! source (str "SET TRANSACTION SNAPSHOT " (literal snapshot)))
        (validate-publication! source config)
        (transaction!
         destination
         (fn []
           (command! destination
                     (str "CREATE TABLE " checkpoint-table " (id integer PRIMARY KEY, "
                          "source_system text NOT NULL, source_database text NOT NULL, "
                          "slot_name text NOT NULL, publication text NOT NULL, "
                          "end_lsn text NOT NULL, source_xid text, source_commit_micros text)"))
           (let [counts (mapv #(copy-table! source destination % during-copy) tables)]
             (update! destination (str "INSERT INTO " checkpoint-table
                                       " (id, source_system, source_database, slot_name, publication, end_lsn)"
                                       " VALUES (1, ?, ?, ?, ?, ?)")
                      [source-system source-database slot publication initial-lsn])
             {:end-lsn initial-lsn :table-counts counts})))
        (finally (command! source "ROLLBACK"))))))

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

(defn decode
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
                          (require! (= -1 (.getInt buffer)) "Unexpected column type modifier"
                                    {:relation [schema name] :column column})
                          column))
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
    (command! destination
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
          count (if read-check?
                  (count (query destination sql values (constantly true)))
                  (update! destination sql values))]
      (require! (= 1 count) "A mirrored change did not affect exactly one row"
                {:relation-id relation-id :operation op :affected count}))))

(defn- checkpoint [destination]
  (first (query destination
                (str "SELECT source_system, source_database, slot_name, publication, end_lsn"
                     " FROM " checkpoint-table " WHERE id = 1")
                []
                (fn [^ResultSet r]
                  {:source-system (.getString r 1) :source-database (.getString r 2)
                   :slot (.getString r 3) :publication (.getString r 4) :end-lsn (.getString r 5)}))))

(defn- apply-commit! [destination relations previous {:keys [end-lsn xid commit-micros changes]}]
  (transaction!
   destination
   (fn []
     (doseq [change changes]
       (apply-change! destination relations change))
     (let [count (update! destination
                          (str "UPDATE " checkpoint-table
                               " SET end_lsn = ?, source_xid = ?, source_commit_micros = ? "
                               "WHERE id = 1 AND end_lsn = ?")
                          [end-lsn xid commit-micros previous])]
       (require! (= 1 count) "Mirror checkpoint changed while applying a transaction"
                 {:expected previous :next end-lsn})))))

(defn- confirm! [^PGReplicationStream stream end-lsn]
  (let [position (lsn end-lsn)]
    (.setAppliedLSN stream position)
    (.setFlushedLSN stream position)
    (.forceUpdateStatus stream)))

(defn- read-message [^PGReplicationStream stream idle-ms]
  (let [deadline (+ (System/nanoTime) (* 1000000 idle-ms))]
    (loop []
      (if-let [buffer (.readPending stream)]
        (decode buffer)
        (do
          (require! (< (System/nanoTime) deadline) "No pgoutput message before the idle deadline"
                    {:idle-ms idle-ms})
          (Thread/sleep 10)
          (recur))))))

(defn follow!
  "Apply up to :commits committed transactions. Resume at the slot's last
   confirmation and discard any transaction already in the atomic local
   checkpoint. before-ack runs after a local commit is durable."
  [{:keys [source-url destination-url slot publication tables commits idle-ms before-ack]
    :or {commits 1 idle-ms 15000} :as config}]
  (validate-names! slot publication)
  (require! (and (pos-int? commits) (pos-int? idle-ms)) "Expected positive commit and idle limits"
            {:commits commits :idle-ms idle-ms})
  (with-open [replication (connect-replication source-url)
              source (connect source-url)
              destination (connect destination-url)]
    (validate-publication! source config)
    (let [origin (assoc (source-identity replication) :slot slot :publication publication)
          saved (checkpoint destination)
          previous (:end-lsn saved)
          confirmed (first (query source
                                  (str "SELECT confirmed_flush_lsn::text FROM pg_replication_slots "
                                       "WHERE slot_name = ? AND plugin = 'pgoutput' "
                                       "AND database = current_database() "
                                       "AND NOT temporary AND NOT active AND invalidation_reason IS NULL")
                                  [slot] (fn [^ResultSet r] (.getString r 1))))
          expected (into {} (map (juxt (juxt :schema :name) :columns)) tables)
          api (.getReplicationAPI (.unwrap replication PGConnection))]
      (require! (= origin (dissoc saved :end-lsn)) "Destination checkpoint belongs to a different source"
                {:expected origin :saved saved})
      (require! (and previous confirmed (not (pos? (compare-lsn confirmed previous))))
                "The slot has no resumable position for this checkpoint"
                {:confirmed confirmed :checkpoint previous})
      (with-open [stream (-> api .replicationStream .logical
                             (.withSlotName slot)
                             (.withStartPosition (lsn confirmed))
                             (.withSlotOption "proto_version" 1)
                             (.withSlotOption "publication_names" publication)
                             (.withStatusInterval 1 TimeUnit/SECONDS) .start)]
        (loop [relations {} pending nil durable previous results []]
          (if (= (count results) commits)
            results
            (let [{:keys [op relation-id schema name columns identity end-lsn] :as message}
                  (read-message stream idle-ms)]
              (case op
                :relation
                (do
                  (require! (and (= identity \d) (= columns (get expected [schema name])))
                            "Publication relation changed" {:relation [schema name] :columns columns})
                  (recur (assoc relations relation-id message) pending durable results))

                :begin
                (do
                  (require! (nil? pending) "Received BEGIN before the previous COMMIT" {})
                  (recur relations (assoc message :changes []) durable results))

                :origin
                (recur relations pending durable results)

                :commit
                (do
                  (require! (and pending (= (:commit-lsn pending) (:commit-lsn message))
                                 (= (:commit-micros pending) (:commit-micros message)))
                            "pgoutput COMMIT did not match BEGIN" {:commit message})
                  (let [replayed? (not (pos? (compare-lsn end-lsn durable)))
                        result {:state (if replayed? :replayed :applied)
                                :end-lsn end-lsn :xid (:xid pending)
                                :unchanged-toast-fields
                                (count (filter #{::unchanged} (mapcat :new (:changes pending))))}]
                    (when-not replayed?
                      (apply-commit! destination relations durable (assoc pending :end-lsn end-lsn))
                      (when before-ack (before-ack result)))
                    (confirm! stream end-lsn)
                    (recur relations nil (if replayed? durable end-lsn) (conj results result))))

                (do
                  (require! pending "Received a change outside a pgoutput transaction" {:operation op})
                  (recur relations (update pending :changes conj message) durable results))))))))))
