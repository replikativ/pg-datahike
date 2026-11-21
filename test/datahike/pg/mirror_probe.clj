(ns datahike.pg.mirror-probe
  "Native PostgreSQL 18 integration probe for the live mirror prototype.
   Use a fresh disposable source database on a server with wal_level=logical."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datahike.api :as d]
            [datahike.pg.mirror-prototype :as mirror]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer]
           [java.io PushbackReader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.sql Connection DriverManager]
           [java.util UUID]
           [java.util.concurrent TimeUnit]))

(def ^:private relations
  [{:schema "public" :name "probe_parent"
    :columns [{:name "id" :oid 20 :key? true}
              {:name "label" :oid 25 :key? false}
              {:name "payload" :oid 25 :key? false}]}
   {:schema "public" :name "probe_child"
    :columns [{:name "id" :oid 23 :key? true}
              {:name "parent_id" :oid 20 :key? false}
              {:name "note" :oid 25 :key? false}]}])

(def ^:private schema-ddl
  ["CREATE TABLE probe_parent (id bigint PRIMARY KEY, label text, payload text)"
   (str "CREATE TABLE probe_child (id integer PRIMARY KEY, "
        "parent_id bigint REFERENCES probe_parent(id), note text)")])

(def ^:private watched
  [:probe_parent/id :probe_parent/label :probe_parent/payload
   :probe_child/id :probe_child/parent_id :probe_child/note :pgdh_mirror_checkpoint/end_lsn])

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
  [(rows conn "SELECT id, label, payload FROM probe_parent ORDER BY id")
   (rows conn "SELECT id, parent_id, note FROM probe_child ORDER BY id")])

(defn- slot-status [source slot]
  (first (rows source
               (str "SELECT confirmed_flush_lsn::text, active::text FROM pg_replication_slots "
                    "WHERE slot_name = '" slot "'"))))

(defn- await-slot! [source slot position]
  (let [deadline (+ (System/nanoTime) 10000000000)]
    (loop []
      (let [[confirmed active] (slot-status source slot)]
        (when-not (and (= position confirmed) (= "false" active))
          (check! (< (System/nanoTime) deadline) "Source slot did not reach the expected position"
                  {:expected position :confirmed confirmed :active active})
          (Thread/sleep 25)
          (recur))))))

(defn- child-crash! [config log]
  (let [java (str (System/getProperty "java.home") "/bin/java")
        command [java "-cp" (System/getProperty "java.class.path")
                 "clojure.main" "-m" "datahike.pg.mirror-probe" "--crash-worker"]
        process (-> (ProcessBuilder. ^java.util.List command)
                    (.redirectErrorStream true)
                    (.redirectOutput (io/file log))
                    (.start))]
    (with-open [input (io/writer (.getOutputStream process))]
      (.write input (pr-str config)))
    (when-not (.waitFor process 90 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Mirror worker did not finish before the test deadline" {:log log})))
    (check! (= 86 (.exitValue process)) "Mirror worker did not crash at the post-commit point"
            {:exit (.exitValue process) :log log})))

(defn- with-sink [config f]
  (let [conn (d/connect config)
        srv (pg/start-server conn {:port 0 :host "127.0.0.1"})
        port (.getPort ^PgWireServer (:server srv))
        url (str "jdbc:postgresql://127.0.0.1:" port "/datahike"
                 "?user=datahike&password=datahike&sslmode=disable&prepareThreshold=1")]
    (try
      (f conn url)
      (finally
        (pg/stop-server srv)
        (d/release conn)))))

(defn- history-transactions [db after]
  (set (d/q '[:find [?tx ...]
              :in $ ?after [?attr ...]
              :where [_ ?attr _ ?tx] [(> ?tx ?after)]]
            (d/history db) after watched)))

(defn run-probe!
  "Create and inspect a two-table mirror in an isolated durable store."
  [source-url]
  (let [directory (str (Files/createTempDirectory "pgdh-live-mirror-" (make-array FileAttribute 0)))
        log (str directory "/worker.log")
        store-config {:store {:backend :file :path (str directory "/sink") :id (UUID/randomUUID)}
                      :schema-flexibility :write :keep-history? true :max-string-length 0}
        slot (str "pgdh_prototype_" (subs (str (UUID/randomUUID)) 0 8))
        config {:source-url source-url :slot slot :publication "pgdh_prototype"
                :tables relations :commits 1 :idle-ms 30000}]
    (prn {:mirror-artifacts directory})
    (d/create-database store-config)
    (with-open [source (DriverManager/getConnection source-url)]
      (apply sql! source schema-ddl)
      (sql! source
            (str "INSERT INTO probe_parent SELECT 1, 'before', "
                 "string_agg(md5(i::text), '' ORDER BY i) FROM generate_series(1, 1000) AS i")
            "INSERT INTO probe_parent VALUES (2, 'delete me', NULL)"
            "INSERT INTO probe_child VALUES (10, 1, 'keep')"
            "CREATE PUBLICATION pgdh_prototype FOR TABLE probe_parent, probe_child")
      (let [original (fixture-rows source)
            first-commit!
            (fn [table]
              (when (= "probe_parent" (:name table))
                (sql! source
                      "BEGIN"
                      "INSERT INTO probe_parent VALUES (90, 'rolled back', NULL)"
                      "ROLLBACK"
                      "BEGIN"
                      "UPDATE probe_parent SET label = 'after' WHERE id = 1"
                      "UPDATE probe_child SET id = 11 WHERE id = 10"
                      "INSERT INTO probe_child VALUES (12, 1, NULL)"
                      "DELETE FROM probe_parent WHERE id = 2"
                      "COMMIT")))
            {:keys [start-lsn snapshot-tx apply-tx first-end]}
            (with-sink
              store-config
              (fn [conn destination-url]
                (with-open [destination (DriverManager/getConnection destination-url)]
                  (apply sql! destination schema-ddl)
                  (let [config (assoc config :destination-url destination-url)
                        snapshot (mirror/bootstrap! (assoc config :during-copy first-commit!))
                        snapshot-tx (:max-tx (d/db conn))]
                    (check! (= [2 1] (:table-counts snapshot)) "Unexpected snapshot counts" snapshot)
                    (check! (= original (fixture-rows destination)) "COPY did not retain the exported snapshot" {})
                    (check! (= "after" (ffirst (rows source "SELECT label FROM probe_parent WHERE id = 1")))
                            "The publisher did not commit during COPY" {})
                    (child-crash! config log)
                    (let [db (d/db conn)
                          apply-tx (:max-tx db)
                          end (ffirst (rows destination "SELECT end_lsn FROM pgdh_mirror_checkpoint"))]
                      (await-slot! source slot (:end-lsn snapshot))
                      (check! (not= end (:end-lsn snapshot)) "Local checkpoint did not advance" {:end end})
                      (check! (= (fixture-rows source) (fixture-rows destination))
                              "The crashed worker did not leave the committed source rows" {})
                      (check! (= #{apply-tx} (history-transactions db snapshot-tx))
                              "Rows and checkpoint did not share one local commit"
                              {:history (history-transactions db snapshot-tx) :apply-tx apply-tx})
                      {:start-lsn (:end-lsn snapshot) :snapshot-tx snapshot-tx
                       :apply-tx apply-tx :first-end end})))))]
        (with-sink
          store-config
          (fn [conn destination-url]
            (with-open [destination (DriverManager/getConnection destination-url)]
              (let [config (assoc config :destination-url destination-url)
                    replay (first (mirror/follow! config))]
                (check! (and (= :replayed (:state replay)) (= first-end (:end-lsn replay))
                             (pos? (:unchanged-toast-fields replay)))
                        "Restart did not receive the duplicate transaction and unchanged TOAST field" replay)
                (await-slot! source slot first-end)
                (check! (= #{apply-tx} (history-transactions (d/db conn) snapshot-tx))
                        "Redelivery rewrote mirror history" {})
                (check! (= (fixture-rows source) (fixture-rows destination))
                        "Resumed sink rows differ from the source" {})
                (check! (= #{[1 "before"] [2 "delete me"]}
                           (d/q '[:find ?id ?label :where [?e :probe_parent/id ?id]
                                  [?e :probe_parent/label ?label]]
                                (d/as-of (d/db conn) snapshot-tx)))
                        "The original snapshot is absent from reopened store history" {})
                (sql! source "UPDATE probe_parent SET label = 'after restart', payload = NULL WHERE id = 1")
                (let [next-commit (first (mirror/follow! config))
                      db (d/db conn)
                      next-tx (:max-tx db)]
                  (check! (= :applied (:state next-commit)) "Restart did not follow a new commit" next-commit)
                  (await-slot! source slot (:end-lsn next-commit))
                  (check! (= (fixture-rows source) (fixture-rows destination))
                          "Rows differ after applying a new commit" {})
                  (check! (= #{apply-tx next-tx} (history-transactions db snapshot-tx))
                          "Expected one local history transaction per source commit" {})
                  {:start-lsn start-lsn :first-commit replay :next-commit next-commit
                   :history-transactions [apply-tx next-tx] :artifacts directory})))))))))

(defn -main [& [argument]]
  (if (= "--crash-worker" argument)
    (let [config (edn/read (PushbackReader. *in*))]
      (mirror/follow! (assoc config :before-ack
                             (fn [result]
                               (prn result)
                               (flush)
                               (.halt (Runtime/getRuntime) 86))))
      (throw (ex-info "Worker returned without reaching the crash hook" {})))
    (let [source-url (or argument (System/getenv "PG_MIRROR_SOURCE_URL"))]
      (check! source-url "Set PG_MIRROR_SOURCE_URL to an empty native PostgreSQL database" {})
      (prn (run-probe! source-url)))))
