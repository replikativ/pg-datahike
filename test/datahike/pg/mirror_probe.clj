(ns datahike.pg.mirror-probe
  "End-to-end mirror probe using two disposable native PostgreSQL 18 databases."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.mirror :as mirror]
            [datahike.pg.server :as pg])
  (:import [java.io PushbackReader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.sql Connection DriverManager SQLException]
           [java.util.concurrent TimeUnit]
           [org.postgresql PGConnection]))

(def ^:private schema-ddl
  [(str "CREATE TABLE probe_parent (tenant integer, id bigint, label varchar(40), payload text, "
        "occurred timestamptz DEFAULT '2024-02-29 19:25:19.123456+05:30', "
        "PRIMARY KEY (tenant, id) INCLUDE (label))")
   (str "CREATE TABLE probe_child (id integer PRIMARY KEY, tenant integer, parent_id bigint, note text, "
        "amount numeric(12,3) DEFAULT 3.14159, bucket numeric(8,-2), "
        "wall_time timestamp(6) DEFAULT '2024-02-29 13:55:19.123457', "
        "rounded timestamp(3) DEFAULT '1999-12-31 23:59:59.999500', "
        "FOREIGN KEY (tenant, parent_id) REFERENCES probe_parent(tenant, id) ON DELETE CASCADE)")])

(defn- check! [condition message data]
  (when-not condition (throw (ex-info message data))))

(defn- sql! [^Connection conn & statements]
  (with-open [stmt (.createStatement conn)]
    (doseq [sql statements] (.execute stmt sql))))

(defn- rows [^Connection conn sql]
  (with-open [stmt (.createStatement conn)
              result (.executeQuery stmt sql)]
    (let [n (.getColumnCount (.getMetaData result))]
      (loop [out []]
        (if (.next result)
          (recur (conj out (mapv #(.getString result (int %)) (range 1 (inc n)))))
          out)))))

(defn- fixture-rows [conn]
  [(rows conn "SELECT tenant, id, label, payload, occurred::text FROM probe_parent ORDER BY tenant, id")
   (rows conn "SELECT id, tenant, parent_id, note, amount, bucket, wall_time::text, rounded::text FROM probe_child ORDER BY id")])

(defn- literal [value]
  (str "'" (str/replace value "'" "''") "'"))

(defn- slot-query [source slot cols]
  (first (rows source (str "SELECT " cols " FROM pg_replication_slots WHERE slot_name = " (literal slot)))))

(defn- await! [message f]
  (let [deadline (+ (System/nanoTime) (.toNanos TimeUnit/SECONDS 40))]
    (loop []
      (if-let [result (f)]
        result
        (do
          (check! (< (System/nanoTime) deadline) message {})
          (Thread/sleep 25)
          (recur))))))

(defn- await-state! [subscriber state]
  (await! (str "Mirror did not reach " state)
          (fn []
            (let [seen (mirror/status subscriber)]
              (if (= state (:state seen))
                seen
                (do (check! (not (#{:failed :paused} (:state seen))) "Mirror paused or failed" seen)
                    nil))))))

(defn- sink-url [subscriber]
  (str "jdbc:postgresql://127.0.0.1:" (:port (await-state! subscriber :following))
       "/datahike?user=reader&sslmode=disable&prepareThreshold=1"))

(defn- check-rows! [source reference sink subscriber]
  (sql! sink "SET TIME ZONE 'UTC'")
  (let [expected (fixture-rows source)]
    (await! "Native subscriber failed to converge" #(= expected (fixture-rows reference)))
    (try
      (await! "pg-datahike subscriber failed to converge"
              #(let [status (mirror/status subscriber)]
                 (check! (not (#{:failed :paused} (:state status)))
                         "Mirror paused before matching source rows" status)
                 (= expected (fixture-rows sink))))
      (catch clojure.lang.ExceptionInfo e
        (throw (ex-info (ex-message e) (mirror/status subscriber) e))))))

(defn- expect-read-only! [f]
  (let [state (try (f) nil (catch SQLException e (.getSQLState e)))]
    (check! (= "25006" state) "Expected the mirror write to be rejected" {:sqlstate state})))

(defn- check-read-only! [^Connection sink]
  (doseq [statement ["BEGIN READ WRITE"
                     "SET SESSION CHARACTERISTICS AS TRANSACTION READ WRITE"
                     "CREATE SCHEMA probe_write"
                     "CREATE TABLE probe_write (id integer)"
                     "SELECT nextval('some_sequence')"
                     "TRUNCATE probe_parent"]]
    (expect-read-only! #(sql! sink statement)))
  (with-open [statement (.prepareStatement sink "UPDATE probe_parent SET label = ? WHERE tenant = 1 AND id = 1")]
    (.setString statement 1 "blocked")
    (expect-read-only! #(.executeUpdate statement)))
  (expect-read-only! #(.copyIn (.getCopyAPI (.unwrap sink PGConnection)) "COPY probe_parent FROM STDIN"))
  (check! (= [["on"]] (rows sink "SHOW transaction_read_only")) "Mirror should advertise read-only access" {}))

(defn- child-crash! [config log]
  (let [java (str (System/getProperty "java.home") "/bin/java")
        command [java "-Duser.timezone=UTC" "-cp" (System/getProperty "java.class.path")
                 "clojure.main" "-m" "datahike.pg.mirror-probe" "--crash-worker"]
        process (-> (ProcessBuilder. ^java.util.List command)
                    (.redirectErrorStream true)
                    (.redirectOutput (io/file log))
                    (.start))]
    (with-open [input (io/writer (.getOutputStream process))]
      (.write input (pr-str config)))
    (when-not (.waitFor process 110 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Mirror crash worker did not finish" {:log log})))
    (check! (= 86 (.exitValue process)) "Mirror worker did not crash before feedback"
            {:exit (.exitValue process) :log log})))

(defn- run-crash-worker! [config]
  (let [apply-commit #'mirror/apply-commit!
        original @apply-commit
        refresh? (= :refresh (:probe-crash-mode config))]
    (with-redefs-fn {apply-commit (fn [destination relations previous transaction]
                                    (original destination relations previous transaction)
                                    (when (if refresh?
                                            (:install! transaction)
                                            (seq (:changes transaction)))
                                      (.halt (Runtime/getRuntime) 86)))}
      (fn []
        (let [subscriber (mirror/start! config)]
          (try
            (if refresh?
              (await! "Crash worker did not reach the refresh commit"
                      #(let [seen (mirror/status subscriber)]
                         (check! (not (#{:paused :failed} (:state seen))) "Refresh crash worker stopped" seen)
                         false))
              (check! (not (await-state! subscriber :following)) "Crash worker reached cutover" {}))
            (finally (mirror/stop! subscriber))))))))

(defn- row-history [db after]
  (set (d/q '[:find [?tx ...]
              :in $ ?after
              :where [?e :probe_parent/label _ ?tx] [(> ?tx ?after)]]
            (d/history db) after)))

(defn- inspect-db [path f]
  (let [conn (#'mirror/open-store path)]
    (try (f (d/db conn)) (finally (d/release conn)))))

(defn- durable-position [path]
  (let [conn (#'mirror/open-store path)]
    (try
      (with-open [handler (pg/make-query-handler conn)]
        (:end-lsn (#'mirror/checkpoint handler)))
      (finally (d/release conn)))))

(defn- check-admission! [source config]
  (sql! source
        "CREATE TABLE probe_unhandled (id integer PRIMARY KEY, observed timetz, payload jsonb)"
        "CREATE TABLE probe_no_key (note text)"
        "ALTER TABLE probe_no_key REPLICA IDENTITY FULL"
        "CREATE TABLE probe_no_columns ()"
        "CREATE PUBLICATION probe_empty"
        (str "CREATE PUBLICATION probe_incompatible FOR TABLE "
             "probe_unhandled (id, observed) WHERE (id > 0), probe_no_key, probe_no_columns "
             "WITH (publish = 'insert')"))
  (let [report #(mirror/publication-report (assoc config :publication %))
        slots-before (rows source "SELECT slot_name FROM pg_replication_slots WHERE database = current_database()")
        incompatible (report "probe_incompatible")
        issues (into {} (map (fn [table] [(:name table) (set (map (juxt :reason :column) (:issues table)))]))
                     (:tables incompatible))]
    (check! (= [{:reason :publication-not-found}] (:issues (report "probe_absent")))
            "Missing publication was not reported" {})
    (check! (= [{:reason :empty-publication}] (:issues (report "probe_empty")))
            "Empty publication was not reported" {})
    (check! (= [{:reason :disabled-operations :operations [:update :delete :truncate]}] (:issues incompatible))
            "Incomplete publication operations were not reported" incompatible)
    (check! (= {"probe_unhandled" #{[:row-filter nil] [:type "observed"]
                                    [:unpublished-column "payload"] [:type "payload"]}
                "probe_no_key" #{[:replica-identity nil] [:missing-immediate-primary-key nil]}
                "probe_no_columns" #{[:no-columns nil] [:missing-immediate-primary-key nil]}}
               issues)
            "The catalog check omitted an incompatible table or column" incompatible)
    (check! (not (:compatible? incompatible)) "Incompatible publication was admitted" incompatible)
    (check! (:compatible? (report "mirror_pub")) "Publication with common type modifiers was rejected" {})
    (check! (= slots-before (rows source "SELECT slot_name FROM pg_replication_slots WHERE database = current_database()"))
            "The catalog check created a slot" {})))

(defn- column-modifiers [conn]
  (rows conn (str "SELECT c.relname, a.attname, a.atttypmod "
                  "FROM pg_class c JOIN pg_attribute a ON a.attrelid = c.oid "
                  "WHERE c.relname IN ('probe_parent', 'probe_child') "
                  "AND a.attname IN ('label', 'amount', 'bucket', 'occurred', 'wall_time', 'rounded') "
                  "ORDER BY c.relname, a.attname")))

(defn- timestamp-catalog [conn]
  (rows conn (str "SELECT column_name, udt_name, datetime_precision FROM information_schema.columns "
                  "WHERE table_name IN ('probe_parent', 'probe_child') "
                  "AND column_name IN ('occurred', 'wall_time', 'rounded') ORDER BY column_name")))

(defn run-probe!
  "Exercise initial snapshot, PostgreSQL row parity, SQL read-only enforcement,
   reconnect, crash replay, history, schema drift and lost slot detection."
  [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-oracle-" (make-array FileAttribute 0)))
        path (str directory "/store")
        log (str directory "/crash.log")
        native-slot (str "mirror_native_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "mirror_pub" :store-path path :server {:port 0}}
        subscriber (atom nil)]
    (prn {:artifacts directory})
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (try
        (sql! source "SET TIME ZONE 'UTC'")
        (sql! reference "SET TIME ZONE 'UTC'")
        (apply sql! source schema-ddl)
        (apply sql! reference schema-ddl)
        (sql! source
              "INSERT INTO probe_parent SELECT 1, 1, 'before', string_agg(md5(i::text), '' ORDER BY i) FROM generate_series(1, 1000) i"
              "INSERT INTO probe_parent VALUES (1, 2, 'delete me', NULL)"
              "INSERT INTO probe_child VALUES (10, 1, 1, 'keep'), (20, 1, 2, 'cascade')"
              "CREATE PUBLICATION mirror_pub FOR TABLE probe_parent, probe_child")
        (check-admission! source config)
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION mirror_native CONNECTION " (literal source-conninfo)
                             " PUBLICATION mirror_pub WITH (create_slot = false, slot_name = "
                             (literal native-slot) ")"))

        ;; The first snapshot exceeds its byte budget. The next attempt must
        ;; discard the failed snapshot and start a fresh copy with the owned slot.
        (reset! subscriber (mirror/start! (assoc config :max-snapshot-bytes 32)))
        (let [failed (await-state! @subscriber :paused)]
          (check! (and (nil? (:port failed)) (nil? (:applied-lsn failed)))
                  "A partial snapshot became visible" failed))
        (mirror/stop! @subscriber)

        (let [copy-table #'mirror/copy-table!
              original @copy-table
              copy-writes? (atom false)]
          (with-redefs-fn
            {copy-table
             (fn [publisher destination table on-bytes]
               (when (compare-and-set! copy-writes? false true)
                 (check! (nil? (:port (mirror/status @subscriber))) "Mirror exposed an unfinished snapshot" {})
                 (sql! source "BEGIN"
                       "INSERT INTO probe_parent VALUES (1, 90, 'rolled back', NULL)"
                       "ROLLBACK" "BEGIN"
                       "UPDATE probe_parent SET label = 'during copy' WHERE tenant = 1 AND id = 1"
                       (str "UPDATE probe_child SET id = 11, amount = 6.626571, bucket = 1234, "
                            "wall_time = '2024-02-29 13:55:19.123458' WHERE id = 10")
                       "INSERT INTO probe_child VALUES (12, 1, 1, NULL)"
                       "DELETE FROM probe_parent WHERE tenant = 1 AND id = 2"
                       "COMMIT"))
               (original publisher destination table on-bytes))}
            (fn []
              (reset! subscriber (mirror/start! config))
              (await-state! @subscriber :following)))
          (check! @copy-writes? "Expected a concurrent write during bootstrap" {}))

        (let [{:keys [slot]} (mirror/status @subscriber)]
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (check-rows! source reference sink @subscriber)
            (check! (= (column-modifiers source) (column-modifiers sink))
                    "Mirror metadata changed the source type modifiers"
                    {:source (column-modifiers source) :mirror (column-modifiers sink)})
            (check! (= (timestamp-catalog source) (timestamp-catalog sink))
                    "Mirror metadata changed the source timestamp types"
                    {:source (timestamp-catalog source) :mirror (timestamp-catalog sink)})
            (check-read-only! sink)
            (let [[pid] (slot-query source slot "active_pid")]
              (sql! source (str "SELECT pg_terminate_backend(" pid ")"))
              (sql! source "UPDATE probe_parent SET label = 'after reconnect' WHERE tenant = 1 AND id = 1")
              (check-rows! source reference sink @subscriber)
              (check! (not= [pid] (slot-query source slot "active_pid"))
                      "Expected a replacement replication connection" {}))
            ;; A keepalive can advertise a WAL position ahead of the last
            ;; committed output. It must not advance the durable feedback.
            (sql! source "SELECT pg_logical_emit_message(false, 'unrelated', 'ignore me')")
            (Thread/sleep 1500)
            (let [durable (get (mirror/status @subscriber) :applied-lsn)]
              (check! (= [durable] (slot-query source slot "confirmed_flush_lsn"))
                      "Keepalive changed the confirmed position beyond the checkpoint" {})))

          (mirror/stop! @subscriber)
          (let [[before before-instant] (inspect-db path
                                                    #(let [tx (:max-tx %)]
                                                       [tx (:db/txInstant (d/entity % tx))]))
                checkpoint (durable-position path)
                [confirmed] (slot-query source slot "confirmed_flush_lsn")]
            (sql! source (str "UPDATE probe_parent SET label = 'after crash', payload = NULL, "
                              "occurred = '2024-02-29 13:55:19.123457+00' WHERE tenant = 1 AND id = 1"))
            (let [apply-change #'mirror/apply-change!
                  original @apply-change]
              (with-redefs-fn {apply-change (fn [destination relations change]
                                              (original destination relations change)
                                              (throw (ex-info "Interrupted before commit" {})))}
                (fn []
                  (reset! subscriber (mirror/start! config))
                  (check! (= "Interrupted before commit"
                             (get-in (await-state! @subscriber :paused) [:last-error :message]))
                          "The apply interruption was not reached" {})
                  (mirror/stop! @subscriber))))
            (check! (empty? (inspect-db path #(row-history % before))) "A staged row escaped rollback" {})
            (check! (= checkpoint (durable-position path)) "A staged row changed the durable cursor" {})
            (check! (= [confirmed] (slot-query source slot "confirmed_flush_lsn"))
                    "A staged row advanced replication feedback" {})
            (child-crash! config log)
            (await! "The crashed slot remained active" #(= [confirmed "false"] (slot-query source slot "confirmed_flush_lsn, active::text")))
            (let [crash-history (inspect-db path #(row-history % before))]
              (check! (= 1 (count crash-history))
                      "Crash left anything other than one history transaction" {:history crash-history})
              (check! (not= checkpoint (durable-position path))
                      "The durable cursor did not commit with the source row" {})
              (reset! subscriber (mirror/start! config))
              (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
                (check-rows! source reference sink @subscriber)
                (sql! sink (str "SET datahike.as_of = "
                                (literal (str (.toInstant ^java.util.Date before-instant)))))
                (check! (= [["2024-02-29 13:55:19.123456+00"]]
                           (rows sink "SELECT occurred::text FROM probe_parent WHERE tenant = 1 AND id = 1"))
                        "The pre-crash timestamp lost precision in a historical SQL read" {}))
              (mirror/stop! @subscriber)
              (check! (= crash-history (inspect-db path #(row-history % before)))
                      "Replayed commit rewrote Datahike history" {})
              (check! (= #{"after reconnect"}
                         (inspect-db path
                                     #(set (d/q '[:find [?label ...]
                                                  :where [_ :probe_parent/label ?label]]
                                                (d/as-of % before)))))
                      "The pre-crash row is missing from historical reads" {})))

          (reset! subscriber (mirror/start! config))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (sql! source "TRUNCATE probe_parent, probe_child")
            (check-rows! source reference sink @subscriber)
            (check! (= [[] []] (fixture-rows sink)) "Multi-table truncate left rows" {})
            (await! "Publisher did not confirm the truncate"
                    #(= [(:applied-lsn (mirror/status @subscriber))] (slot-query source slot "confirmed_flush_lsn")))
            (let [[confirmed] (slot-query source slot "confirmed_flush_lsn")]
              (sql! source "ALTER TABLE probe_parent ADD COLUMN unexpected integer"
                    "INSERT INTO probe_parent (tenant, id, unexpected) VALUES (1, 4, 10)")
              (await-state! @subscriber :paused)
              (check! (= [confirmed] (slot-query source slot "confirmed_flush_lsn"))
                      "Schema drift advanced the checkpoint" {})
              (check! (= [[] []] (fixture-rows sink)) "Schema drift made partial data visible" {})))

          (mirror/stop! @subscriber)
          (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot) ")"))
          (reset! subscriber (mirror/start! config))
          (check! (get-in (await-state! @subscriber :paused) [:last-error :resnapshot-required?])
                  "Missing slot should require a new snapshot" {})
          {:status :passed :slot slot :artifacts directory})
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (sql! reference "DROP SUBSCRIPTION IF EXISTS mirror_native")
          (when (= ["false"] (slot-query source native-slot "active::text"))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal native-slot) ")"))))))))

(defn- refresh-rows [conn]
  [(rows conn "SELECT id, label FROM refresh_account ORDER BY id")
   (rows conn "SELECT id, note, happened::text FROM refresh_event ORDER BY id")
   (rows conn "SELECT id, note FROM refresh_note ORDER BY id")])

(defn- table-absent? [conn]
  (try
    (rows conn "SELECT id FROM refresh_event")
    false
    (catch SQLException error
      (= "42P01" (.getSQLState error)))))

(defn- sync-slots [source]
  (rows source (str "SELECT slot_name FROM pg_replication_slots"
                    " WHERE database = current_database() AND temporary"
                    " AND left(slot_name, 10) = 'pgdh_sync_'")))

(defn run-refresh-probe!
  "Exercise an online publication addition, a failed handoff, and a crash
   after the durable handoff but before feedback to PostgreSQL."
  [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-refresh-" (make-array FileAttribute 0)))
        path (str directory "/store")
        log (str directory "/refresh-crash.log")
        native-slot (str "mirror_refresh_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "mirror_refresh_pub" :store-path path :server {:port 0}}
        subscriber (atom nil)
        schema ["CREATE TABLE refresh_account (id integer PRIMARY KEY, label text)"
                (str "CREATE TABLE refresh_event (id integer PRIMARY KEY, note varchar(40), "
                     "happened timestamptz(6))")
                "CREATE TABLE refresh_note (id integer PRIMARY KEY, note text)"]]
    (prn {:artifacts directory})
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (try
        (doseq [c [source reference]]
          (sql! c "SET TIME ZONE 'UTC'")
          (apply sql! c schema))
        (sql! source "INSERT INTO refresh_account VALUES (1, 'initial')"
              "INSERT INTO refresh_event VALUES (1,'snapshot','2024-06-01 12:00:00.123456+00')"
              "INSERT INTO refresh_note VALUES (1, 'before truncate')"
              "CREATE PUBLICATION mirror_refresh_pub FOR TABLE refresh_account")
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION mirror_refresh_native CONNECTION "
                             (literal source-conninfo) " PUBLICATION mirror_refresh_pub"
                             " WITH (create_slot = false, slot_name = " (literal native-slot) ")"))
        (reset! subscriber (mirror/start! config))
        (let [{:keys [slot]} (await-state! @subscriber :following)
              started (promise)
              release-copy (promise)
              copy-table #'mirror/copy-table!
              transfer #'mirror/transfer-staged-table!
              copy-original @copy-table
              transfer-original @transfer]
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (check! (table-absent? sink) "Unpublished table was visible" {})
            (with-redefs-fn
              {copy-table
               (fn [publisher destination table on-bytes]
                 (when (= "refresh_event" (:name table))
                   (deliver started true)
                   (check! (deref release-copy 30000 false) "Probe did not release the staging snapshot" {}))
                 (copy-original publisher destination table on-bytes))
               transfer
               (fn [staged destination table on-bytes]
                 (transfer-original staged destination table on-bytes)
                 (throw (ex-info "Interrupted refresh before durable cutover" {})))}
              (fn []
                (try
                  (sql! source "ALTER PUBLICATION mirror_refresh_pub ADD TABLE refresh_event, refresh_note")
                  (sql! reference "ALTER SUBSCRIPTION mirror_refresh_native REFRESH PUBLICATION")
                  (check! (deref started 30000 false) "Online refresh did not start copying"
                          (mirror/status @subscriber))
                  (check! (= 1 (count (sync-slots source))) "Refresh did not use an owned temporary slot" {})
                  ;; The two table versions must advance to one boundary
                  ;; even when the source changes both in one transaction.
                  (sql! source "BEGIN"
                        "TRUNCATE refresh_account, refresh_note"
                        "INSERT INTO refresh_account VALUES (1, 'during copy')"
                        (str "UPDATE refresh_event SET note = 'during copy', "
                             "happened = '2024-06-01 12:00:00.123457+00' WHERE id = 1")
                        "INSERT INTO refresh_note VALUES (2, 'after truncate')"
                        "COMMIT")
                  (await! "Existing table stopped following during the staging copy"
                          #(= [["during copy"]] (rows sink "SELECT label FROM refresh_account WHERE id = 1")))
                  (check! (table-absent? sink) "A staged table escaped before its commit" {})
                  (finally (deliver release-copy true)))
                (let [paused (await-state! @subscriber :paused)]
                  (check! (= "Interrupted refresh before durable cutover" (get-in paused [:last-error :message]))
                          "Did not reach the interruption after staging rows" paused))
                (check! (table-absent? sink) "A rolled-back staging copy became visible" {})
                (check! (= [["during copy"]] (rows sink "SELECT label FROM refresh_account WHERE id = 1"))
                        "An interruption lost already-applied rows" {}))))
          (mirror/stop! @subscriber)
          (await! "A stopped stage retained publisher WAL" #(empty? (sync-slots source)))
          ;; The child starts with only the old manifest and no staging data.
          ;; It must resnapshot, then crash after its new tables and manifest
          ;; commit but before the publisher is sent that commit position.
          (child-crash! (assoc config :probe-crash-mode :refresh) log)
          (await! "The crashed main slot remained active" #(= ["false"] (slot-query source slot "active::text")))
          (await! "A crashed stage retained publisher WAL" #(empty? (sync-slots source)))
          (let [cutover (durable-position path)
                history (inspect-db path #(d/q '[:find ?event ?value ?tx
                                                 :where [?event :refresh_event/note ?value ?tx]]
                                               (d/history %)))
                [confirmed] (slot-query source slot "confirmed_flush_lsn")]
            (check! (= [["t"]]
                       (rows source (str "SELECT " (literal confirmed) "::pg_lsn < " (literal cutover) "::pg_lsn")))
                    "Refresh was acknowledged before the crash" {:confirmed confirmed :durable cutover})
            (check! (= ["during copy"] (mapv second history)) "The handoff installed extra history" {:history history})
            (reset! subscriber (mirror/start! config))
            (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
              (sql! sink "SET TIME ZONE 'UTC'")
              (let [expected (refresh-rows source)]
                (await! "Native refresh subscriber failed to converge" #(= expected (refresh-rows reference)))
                (check! (= expected (refresh-rows sink)) "Restart did not recover the durable refresh" {}))
              (check! (= history (inspect-db path #(d/q '[:find ?event ?value ?tx
                                                          :where [?event :refresh_event/note ?value ?tx]]
                                                        (d/history %))))
                      "Replayed handoff rewrote Datahike history" {})
              (sql! source "BEGIN"
                    "UPDATE refresh_account SET label = 'after cutover' WHERE id = 1"
                    "UPDATE refresh_event SET note = 'after cutover' WHERE id = 1"
                    "COMMIT")
              (let [expected (refresh-rows source)]
                (await! "Native subscriber did not follow the multi-table commit" #(= expected (refresh-rows reference)))
                (await! "Mirror did not follow the multi-table commit" #(= expected (refresh-rows sink))))
              (check! (= 1 (inspect-db path #(count
                                              (d/q '[:find ?tx
                                                     :where [_ :refresh_account/label "after cutover" ?tx]
                                                     [_ :refresh_event/note "after cutover" ?tx]]
                                                   (d/history %)))))
                      "The post-cutover transaction was split locally" {})))
          (mirror/stop! @subscriber)
          (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot) ")"))
          {:status :passed :artifacts directory})
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (sql! reference "DROP SUBSCRIPTION IF EXISTS mirror_refresh_native")
          (when (= ["false"] (slot-query source native-slot "active::text"))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal native-slot) ")"))))))))

(def ^:private schema-refresh-fixture
  ["CREATE TABLE schema_account (id integer PRIMARY KEY, label text)"
   (str "CREATE TABLE schema_items (id integer PRIMARY KEY, label varchar(32) NOT NULL DEFAULT 'original', "
        "obsolete text, stamp timestamp(6) DEFAULT '2024-06-01 12:00:00.123456')")
   "CREATE TABLE schema_meta (id integer PRIMARY KEY, tag text DEFAULT 'original')"])

(def ^:private source-schema-changes
  ["BEGIN"
   "ALTER TABLE schema_items RENAME COLUMN label TO summary"
   "ALTER TABLE schema_items DROP COLUMN obsolete"
   (str "ALTER TABLE schema_items ALTER COLUMN stamp TYPE timestamptz(3) "
        "USING stamp AT TIME ZONE 'America/Los_Angeles'")
   "ALTER TABLE schema_items ALTER COLUMN summary SET DEFAULT concat('publ','isher')"
   "ALTER TABLE schema_items ALTER COLUMN summary DROP NOT NULL"
   "ALTER TABLE schema_items ADD COLUMN flag text DEFAULT 'fast' NOT NULL"
   "ALTER TABLE schema_meta ALTER COLUMN tag SET DEFAULT 'changed'"
   "COMMIT"])

(defn- evolved-rows [conn]
  [(rows conn "SELECT id, label FROM schema_account ORDER BY id")
   (rows conn "SELECT id, summary, stamp::text, flag FROM schema_items ORDER BY id")
   (rows conn "SELECT id, tag FROM schema_meta ORDER BY id")])

(defn- schema-item-column-metadata [conn]
  (rows conn (str "SELECT column_name, data_type, datetime_precision, is_nullable, column_default "
                  "FROM information_schema.columns "
                  "WHERE table_name = 'schema_items' AND column_name <> 'db_id' "
                  "ORDER BY ordinal_position")))

(defn- item-eid [path]
  (inspect-db path #(d/q '[:find ?eid . :where [?eid :schema_items/id 1]] %)))

(defn- verify-table-rename! [source reference sink subscriber path]
  (let [[[oid]] (rows sink "SELECT 'schema_items'::regclass::oid")
        at (inspect-db path :max-tx)
        eid (item-eid path)
        previous (rows sink "SELECT id, summary FROM schema_items ORDER BY id")
        previous-columns (schema-item-column-metadata sink)
        ddl ["BEGIN"
             "ALTER TABLE schema_items RENAME TO schema_archive"
             "CREATE TABLE schema_items (id integer PRIMARY KEY, body text NOT NULL)"
             "COMMIT"]
        live-rows (fn [conn]
                    [(rows conn "SELECT id, summary FROM schema_archive ORDER BY id")
                     (rows conn "SELECT id, body FROM schema_items ORDER BY id")])]
    (apply sql! reference ddl)
    (apply sql! source
           (into (pop ddl)
                 ["ALTER PUBLICATION mirror_schema_pub ADD TABLE schema_items"
                  "INSERT INTO schema_items VALUES (1, 'new relation')"
                  "UPDATE schema_archive SET summary = 'during rename' WHERE id = 1"
                  "COMMIT"]))
    (sql! reference "ALTER SUBSCRIPTION mirror_schema_native REFRESH PUBLICATION")
    (await! "The table rename and its replacement did not become visible"
            #(let [status (mirror/status subscriber)]
               (check! (not (#{:failed :paused} (:state status))) "Rename paused the mirror" status)
               (and (= [["schema_archive"]] (rows sink "SELECT relname FROM pg_class WHERE relname = 'schema_archive'"))
                    (= (live-rows source) (live-rows sink)))))
    (await! "Native subscriber did not converge after rename" #(= (live-rows source) (live-rows reference)))
    (check! (= [[oid oid (str eid)]]
               (rows sink "SELECT 'schema_archive'::regclass::oid, tableoid, db_id FROM schema_archive WHERE id = 1"))
            "The rename changed the relation or row identity" {})
    (check! (= [[oid "schema_archive" "schema_archive"]]
               (rows sink (str "SELECT c.oid, c.relname, t.typname FROM pg_class c JOIN pg_type t ON t.oid = c.reltype "
                               "WHERE c.oid = 'schema_archive'::regclass")))
            "The renamed relation and its row type disagree" {})
    (sql! sink (str "SET datahike.as_of = " at))
    (check! (= previous (rows sink "SELECT id, summary FROM schema_items ORDER BY id"))
            "Name reuse changed pre-rename rows" {})
    (check! (= previous-columns (schema-item-column-metadata sink))
            "Name reuse changed the pre-rename catalog" {})
    (sql! sink "RESET datahike.as_of")
    ;; The old physical namespace now has a different logical owner. Both
    ;; a column refresh and ordinary WAL apply must still target the archive.
    (let [add-column "ALTER TABLE schema_archive ADD COLUMN tag text NOT NULL DEFAULT 'archived'"]
      (sql! reference add-column)
      (sql! source add-column
            "BEGIN"
            "UPDATE schema_archive SET summary = 'after rename', tag = 'changed' WHERE id = 1"
            "UPDATE schema_items SET body = 'replacement updated' WHERE id = 1"
            "COMMIT"))
    (await! "Mirror stopped following the two relations after the rename"
            #(let [status (mirror/status subscriber)]
               (check! (not (#{:failed :paused} (:state status))) "Post-rename schema change paused the mirror" status)
               (and (= (live-rows source) (live-rows sink))
                    (= [["tag"]] (rows sink (str "SELECT column_name FROM information_schema.columns "
                                                 "WHERE table_name = 'schema_archive' AND column_name = 'tag'")))
                    (= [["changed"]] (rows sink "SELECT tag FROM schema_archive WHERE id = 1")))))))

(defn run-schema-refresh-probe!
  "Compare publisher schema refresh with a native subscriber given the same
   DDL. Interrupt the handoff and revert a pending catalog change, then crash
   again after the durable install and verify the recovered history."
  [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-schema-" (make-array FileAttribute 0)))
        path (str directory "/store")
        log (str directory "/schema-crash.log")
        native-slot (str "mirror_schema_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "mirror_schema_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        old-query "SELECT id, label, obsolete, stamp::text FROM schema_items ORDER BY id"
        temporal-query (fn [at]
                         (str "SELECT id, label, obsolete, stamp::text FROM schema_items "
                              "FOR SYSTEM_TIME AS OF " (literal at) " ORDER BY id"))
        old-rows [["1" "original" "keep history" "2024-06-01 12:00:00.123456"]]]
    (prn {:artifacts directory})
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (try
        (doseq [c [source reference]]
          (sql! c "SET TIME ZONE 'UTC'")
          (apply sql! c schema-refresh-fixture))
        (sql! source "INSERT INTO schema_account VALUES (1, 'original')"
              "INSERT INTO schema_items (id, obsolete) VALUES (1, 'keep history')"
              "INSERT INTO schema_meta (id) VALUES (1)"
              "CREATE PUBLICATION mirror_schema_pub FOR TABLE schema_account, schema_items, schema_meta")
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION mirror_schema_native CONNECTION "
                             (literal source-conninfo) " PUBLICATION mirror_schema_pub"
                             " WITH (create_slot=false, slot_name=" (literal native-slot) ")"))
        (await! "Native subscriber did not load the initial layout" #(= old-rows (rows reference old-query)))
        (reset! subscriber (mirror/start! config))
        (let [{:keys [slot]} (await-state! @subscriber :following)
              old-columns (schema-item-column-metadata source)
              old-id (item-eid path)
              old-at (inspect-db path #(str (.toInstant ^java.util.Date
                                             (:db/txInstant (d/entity % (:max-tx %))))))
              ids (set (map parse-long
                            (first (rows source (str "SELECT 'schema_items'::regclass::oid, "
                                                     "'schema_meta'::regclass::oid")))))
              started (promise)
              release-copy (promise)
              copy-var #'mirror/copy-table!
              install-var #'mirror/install-staged-tables!
              copy-original @copy-var
              install-original @install-var]
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (check! (= old-rows (rows sink old-query)) "Mirror did not load the initial layout" {})
            (with-redefs-fn
              {copy-var (fn [publisher destination table on-bytes]
                          (when (= "schema_items" (:name table))
                            (deliver started true)
                            (check! (deref release-copy 30000 false) "Probe did not release the schema snapshot" {}))
                          (copy-original publisher destination table on-bytes))
               install-var (fn [stage destination check-running! max-bytes]
                             (install-original stage destination check-running! max-bytes)
                             (throw (ex-info "Interrupted schema install" {})))}
              (fn []
                (try
                  ;; PostgreSQL's native subscriber needs the same DDL on its
                  ;; destination before it can consume rows with the new layout.
                  (apply sql! reference source-schema-changes)
                  (apply sql! source source-schema-changes)
                  (check! (deref started 30000 false) "Schema refresh did not begin" (mirror/status @subscriber))
                  (sql! source "BEGIN"
                        "UPDATE schema_account SET label = 'during schema copy' WHERE id = 1"
                        "UPDATE schema_items SET summary = NULL WHERE id = 1"
                        "INSERT INTO schema_items (id) VALUES (2)"
                        "INSERT INTO schema_meta (id) VALUES (2)"
                        "COMMIT")
                  (await! "Unchanged relation stopped applying during schema copy"
                          #(= [["during schema copy"]] (rows sink "SELECT label FROM schema_account WHERE id = 1")))
                  (let [[[manifest]] (rows sink "SELECT manifest FROM pgdh_mirror.checkpoint WHERE id = 1")]
                    (check! (= ids (set (keys (:pending-schema (edn/read-string manifest)))))
                            "Skipped relations were not recorded durably" {}))
                  (check! (= old-rows (rows sink old-query)) "New layout escaped the staging store" {})
                  (finally (deliver release-copy true)))
                (let [paused (await-state! @subscriber :paused)]
                  (check! (= "Interrupted schema install" (get-in paused [:last-error :message]))
                          "The schema rollback test did not reach the end of installation" paused))
                (check! (= old-rows (rows sink old-query)) "Rollback exposed the new schema" {}))))
          (mirror/stop! @subscriber)
          ;; This table has returned to its original catalog shape. The row
          ;; inserted with its temporary default still needs to be resynced.
          (doseq [c [source reference]]
            (sql! c "ALTER TABLE schema_meta ALTER COLUMN tag SET DEFAULT 'original'"))
          (child-crash! (assoc config :probe-crash-mode :refresh) log)
          (await! "The crashed slot remained active" #(= ["false"] (slot-query source slot "active::text")))
          (await! "Schema refresh left a temporary slot" #(empty? (sync-slots source)))
          (let [cutover (durable-position path)
                [confirmed] (slot-query source slot "confirmed_flush_lsn")]
            (check! (= [["t"]]
                       (rows source (str "SELECT " (literal confirmed) "::pg_lsn < " (literal cutover) "::pg_lsn")))
                    "Schema cutover was acknowledged before the crash" {:confirmed confirmed :durable cutover})
            (check! (= old-id (item-eid path)) "Existing primary key lost its Datahike entity" {})
            (reset! subscriber (mirror/start! config))
            (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
              (sql! sink "SET TIME ZONE 'UTC'")
              (let [expected (evolved-rows source)]
                (await! "Native subscriber did not converge on the new layout" #(= expected (evolved-rows reference)))
                (check! (= expected (evolved-rows sink)) "Mirror rows differ after schema refresh"
                        {:source expected :mirror (evolved-rows sink)}))
              (check! (= (schema-item-column-metadata source) (schema-item-column-metadata sink))
                      "Publisher defaults, types, or nullability changed"
                      {:source (schema-item-column-metadata source) :mirror (schema-item-column-metadata sink)})
              (check! (= ids (set (keys (get-in (mirror/status @subscriber) [:last-schema-refresh :from]))))
                      "Restart lost the resnapshot positions" {})
              (sql! sink (str "SET datahike.as_of = " (literal old-at)))
              (check! (= old-rows (rows sink old-query)) "Historical schema or row was lost" {})
              (check! (= old-columns (schema-item-column-metadata sink))
                      "Historical catalog differs from the publisher's original columns"
                      {:expected old-columns :mirror (schema-item-column-metadata sink)})
              (sql! sink "RESET datahike.as_of")
              (check! (= old-rows (rows sink (temporal-query old-at)))
                      "Extended query with inline system time lost the historical row" {})
              (sql! source "INSERT INTO schema_items (id) VALUES (3)")
              (await! "Mirror stopped following after schema refresh" #(= (evolved-rows source) (evolved-rows sink)))
              ;; Dropping nullability and a default leaves the wire descriptor
              ;; intact. A NULL can reach apply before the next catalog poll.
              (let [ddl (str "ALTER TABLE schema_items ALTER COLUMN flag DROP NOT NULL, "
                             "ALTER COLUMN flag DROP DEFAULT")]
                (sql! reference ddl)
                (sql! source ddl
                      "UPDATE schema_items SET flag = NULL WHERE id = 2"
                      "INSERT INTO schema_items (id) VALUES (4)"))
              (await! "Mirror did not refresh a metadata-only change"
                      #(and (= (evolved-rows source) (evolved-rows sink))
                            (= (schema-item-column-metadata source) (schema-item-column-metadata sink))))
              (verify-table-rename! source reference sink @subscriber path)))
          (mirror/stop! @subscriber)
          (reset! subscriber (mirror/start! config))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (sql! source "BEGIN"
                  "UPDATE schema_archive SET tag = 'after restart' WHERE id = 1"
                  "UPDATE schema_items SET body = 'after restart' WHERE id = 1"
                  "COMMIT")
            (await! "Restart lost the persisted routes for the renamed and replacement tables"
                    #(and (= [["after restart"]] (rows sink "SELECT tag FROM schema_archive WHERE id = 1"))
                          (= [["after restart"]] (rows sink "SELECT body FROM schema_items WHERE id = 1")))))
          (mirror/stop! @subscriber)
          (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot) ")"))
          {:status :passed :artifacts directory})
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (sql! reference "DROP SUBSCRIPTION IF EXISTS mirror_schema_native")
          (when (= ["false"] (slot-query source native-slot "active::text"))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal native-slot) ")"))))))))

(defn -main [& [argument]]
  (if (= "--crash-worker" argument)
    (run-crash-worker! (edn/read (PushbackReader. *in*)))
    (let [source (System/getenv "PG_MIRROR_SOURCE_URL")
          reference (System/getenv "PG_MIRROR_REFERENCE_URL")
          conninfo (System/getenv "PG_MIRROR_SOURCE_CONNINFO")]
      (check! (every? seq [source reference conninfo])
              "Set PG_MIRROR_SOURCE_URL, PG_MIRROR_REFERENCE_URL and PG_MIRROR_SOURCE_CONNINFO" {})
      (prn ((case argument
              "--schema-refresh" run-schema-refresh-probe!
              "--refresh" run-refresh-probe!
              run-probe!)
            source reference conninfo)))))
