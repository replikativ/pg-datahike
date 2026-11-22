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
  [(str "CREATE TABLE probe_parent (tenant integer, id bigint, label text, payload text, "
        "PRIMARY KEY (tenant, id) INCLUDE (label))")
   (str "CREATE TABLE probe_child (id integer PRIMARY KEY, tenant integer, parent_id bigint, note text, "
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
  [(rows conn "SELECT tenant, id, label, payload FROM probe_parent ORDER BY tenant, id")
   (rows conn "SELECT id, tenant, parent_id, note FROM probe_child ORDER BY id")])

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

(defn- check-rows! [source reference sink]
  (let [expected (fixture-rows source)]
    (await! "Native subscriber failed to converge" #(= expected (fixture-rows reference)))
    (await! "pg-datahike subscriber failed to converge" #(= expected (fixture-rows sink)))))

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
        original @apply-commit]
    (with-redefs-fn {apply-commit (fn [destination relations previous transaction]
                                    (original destination relations previous transaction)
                                    (when (seq (:changes transaction))
                                      (.halt (Runtime/getRuntime) 86)))}
      (fn []
        (let [subscriber (mirror/start! config)]
          (try
            (check! (not (await-state! subscriber :following)) "Crash worker reached cutover" {})
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
        (apply sql! source schema-ddl)
        (apply sql! reference schema-ddl)
        (sql! source
              "INSERT INTO probe_parent SELECT 1, 1, 'before', string_agg(md5(i::text), '' ORDER BY i) FROM generate_series(1, 1000) i"
              "INSERT INTO probe_parent VALUES (1, 2, 'delete me', NULL)"
              "INSERT INTO probe_child VALUES (10, 1, 1, 'keep'), (20, 1, 2, 'cascade')"
              "CREATE PUBLICATION mirror_pub FOR TABLE probe_parent, probe_child"
              (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
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
                       "UPDATE probe_child SET id = 11 WHERE id = 10"
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
            (check-rows! source reference sink)
            (check-read-only! sink)
            (let [[pid] (slot-query source slot "active_pid")]
              (sql! source (str "SELECT pg_terminate_backend(" pid ")"))
              (sql! source "UPDATE probe_parent SET label = 'after reconnect' WHERE tenant = 1 AND id = 1")
              (check-rows! source reference sink)
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
          (let [before (inspect-db path :max-tx)
                checkpoint (durable-position path)
                [confirmed] (slot-query source slot "confirmed_flush_lsn")]
            (sql! source "UPDATE probe_parent SET label = 'after crash', payload = NULL WHERE tenant = 1 AND id = 1")
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
                (check-rows! source reference sink))
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
            (check-rows! source reference sink)
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

(defn -main [& [argument]]
  (if (= "--crash-worker" argument)
    (run-crash-worker! (edn/read (PushbackReader. *in*)))
    (let [source (System/getenv "PG_MIRROR_SOURCE_URL")
          reference (System/getenv "PG_MIRROR_REFERENCE_URL")
          conninfo (System/getenv "PG_MIRROR_SOURCE_CONNINFO")]
      (check! (every? seq [source reference conninfo])
              "Set PG_MIRROR_SOURCE_URL, PG_MIRROR_REFERENCE_URL and PG_MIRROR_SOURCE_CONNINFO" {})
      (prn (run-probe! source reference conninfo)))))
