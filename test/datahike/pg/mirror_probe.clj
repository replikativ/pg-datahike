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
  ["CREATE TYPE probe_rating AS ENUM ('Unrated', 'G', 'PG-13', 'R', 'contains''quote')"
   (str "CREATE DOMAIN probe_year AS integer DEFAULT 2001 NOT NULL "
        "CONSTRAINT probe_year_range CHECK (VALUE >= 1901 AND VALUE <= 2155)")
   (str "CREATE TABLE probe_parent (tenant integer, id bigint, label varchar(40), payload text, "
        "occurred timestamptz DEFAULT '2024-02-29 19:25:19.123456+05:30', "
        "PRIMARY KEY (tenant, id) INCLUDE (label))")
   (str "CREATE TABLE probe_child (id integer PRIMARY KEY, tenant integer, parent_id bigint, note text, "
        "amount numeric(12,3) DEFAULT 3.14159, bucket numeric(8,-2), "
        "wall_time timestamp(6) DEFAULT '2024-02-29 13:55:19.123457', "
        "rounded timestamp(3) DEFAULT '1999-12-31 23:59:59.999500', "
        "FOREIGN KEY (tenant, parent_id) REFERENCES probe_parent(tenant, id) ON DELETE CASCADE)")
   (str "CREATE TABLE probe_types (id integer PRIMARY KEY, doc json, attrs jsonb, "
        "observed timetz(6), wall time(3), fixed character(4), payload bytea, tags text[], lexemes tsvector)")
   (str "CREATE TABLE probe_type_keys (observed timetz(6), payload bytea, tags text[], "
        "attrs jsonb, fixed character(4), lexemes tsvector, label text, "
        "rating probe_rating, released probe_year, "
        "PRIMARY KEY (observed, payload, tags, attrs, fixed, lexemes, rating, released))")])

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

(defn- type-rows [conn]
  (rows conn (str "SELECT id, doc::text, attrs::text, observed::text, wall::text, fixed, "
                  "payload::text, tags::text, lexemes::text FROM probe_types ORDER BY id")))

(defn- type-key-rows [conn]
  (rows conn (str "SELECT observed::text, payload::text, tags::text, attrs::text, fixed::text, "
                  "lexemes::text, label, rating::text, released::text FROM probe_type_keys ORDER BY label")))

(defn- fixture-rows [conn]
  [(rows conn "SELECT tenant, id, label, payload, occurred::text FROM probe_parent ORDER BY tenant, id")
   (rows conn "SELECT id, tenant, parent_id, note, amount, bucket, wall_time::text, rounded::text FROM probe_child ORDER BY id")
   (type-rows conn)
   (type-key-rows conn)])

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
        "CREATE TABLE probe_unhandled (id integer PRIMARY KEY, observed inet, payload xml)"
        "CREATE DOMAIN probe_multi_check AS integer CHECK (VALUE > 0) CHECK (VALUE < 42)"
        "CREATE TABLE probe_unhandled_domain (id integer PRIMARY KEY, n probe_multi_check)"
        "CREATE TABLE probe_no_key (note text)"
        "ALTER TABLE probe_no_key REPLICA IDENTITY FULL"
        "CREATE TABLE probe_no_columns ()"
        "CREATE PUBLICATION probe_empty"
        (str "CREATE PUBLICATION probe_incompatible FOR TABLE "
             "probe_unhandled (id, observed) WHERE (id > 0), probe_no_key, probe_no_columns, probe_unhandled_domain "
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
                "probe_unhandled_domain" #{[:type-definition "n"]}
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

(defn- type-catalog [conn]
  (rows conn (str "SELECT column_name, data_type, udt_name, datetime_precision, character_maximum_length, "
                  "character_octet_length FROM information_schema.columns WHERE table_name = 'probe_types' "
                  "AND column_name NOT IN ('id', 'db_id') ORDER BY column_name")))

(defn- user-type-catalog [conn]
  [(rows conn (str "SELECT column_name, data_type, udt_schema, udt_name, domain_schema, domain_name, is_nullable "
                   "FROM information_schema.columns WHERE table_name = 'probe_type_keys' "
                   "AND column_name IN ('rating', 'released') ORDER BY column_name"))
   (rows conn (str "SELECT typname, typtype, COALESCE(typnotnull, false) FROM pg_type "
                   "WHERE typname IN ('probe_year', 'probe_rating') ORDER BY typname"))
   (rows conn "SELECT enumlabel FROM pg_enum WHERE enumtypid = 'probe_rating'::regtype ORDER BY enumsortorder")
   (rows conn (str "SELECT conname FROM pg_constraint WHERE contypid = 'probe_year'::regtype "
                   "AND contype = 'c' ORDER BY conname"))
   (rows conn (str "SELECT pg_typeof(rating)::text, pg_typeof(released)::text "
                   "FROM probe_type_keys ORDER BY label"))])

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
              (str "INSERT INTO probe_types SELECT 1, '{ \"dup\": 1, \"dup\": 2 }'::json, "
                   "jsonb_build_object('toast', string_agg(md5(i::text), '' ORDER BY i), "
                   "'decimal', 1.200, 'null', NULL), '23:59:59.123456-03:30', '11:12:13.111500', "
                   "'é', decode('00ffa10a','hex'), '[-2:0]={\"NULL\",NULL,\"a,b\"}'::text[], "
                   "'fox:3A cat:2 cat:1'::tsvector FROM generate_series(1, 1000) i")
              (str "INSERT INTO probe_types (id, attrs, observed, wall, fixed, payload, tags, lexemes) VALUES "
                   "(2, 'null', '24:00:00+01:30:45', '23:59:59.999500', '', decode('','hex'), '{}', '')")
              (str "INSERT INTO probe_type_keys VALUES "
                   "('14:00:00.123456+04', decode('00ff','hex'), '[0:1]={a,NULL}', '{\"num\":1.200}', "
                   "'a', 'cat:2 fox:1D', 'one', 'Unrated', DEFAULT), "
                   "('24:00:00+01:30:45', decode('','hex'), '{}', 'null', '雪', '', 'two', 'G', 1998)")
              "CREATE PUBLICATION mirror_pub FOR TABLE probe_parent, probe_child, probe_types, probe_type_keys")
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
                       (str "UPDATE probe_types SET observed = '23:59:59.123457-03:30', "
                            "wall = '22:00:59.999500', fixed = '雪', payload = decode('ff00','hex'), "
                            "tags = '[0:2]={\"a,b\",\"NULL\",NULL}', lexemes = 'cat:1D fox:2B' WHERE id = 1")
                       "UPDATE probe_type_keys SET label = 'during copy' WHERE label = 'one'"
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
            (check! (= (type-catalog source) (type-catalog sink))
                    "The mirror catalog changed application type metadata"
                    {:source (type-catalog source) :mirror (type-catalog sink)})
            (check! (= (user-type-catalog source) (user-type-catalog sink))
                    "The mirror catalog changed enum or domain metadata"
                    {:source (user-type-catalog source) :mirror (user-type-catalog sink)})
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
                previous-types (type-rows source)
                previous-type-keys (type-key-rows source)
                [confirmed] (slot-query source slot "confirmed_flush_lsn")]
            (sql! source "BEGIN"
                  (str "UPDATE probe_parent SET label = 'after crash', payload = NULL, "
                       "occurred = '2024-02-29 13:55:19.123457+00' WHERE tenant = 1 AND id = 1")
                  (str "UPDATE probe_types SET doc = ' [1, 2] ', attrs = '{\"changed\": [true, null, 2.00]}', "
                       "observed = '24:00:00+02', tags = ARRAY['', 'a'] WHERE id = 1")
                  (str "UPDATE probe_type_keys SET observed = '14:00:00.123457+04', rating = 'contains''quote', "
                       "released = 1997 WHERE label = 'during copy'")
                  "DELETE FROM probe_type_keys WHERE label = 'two'"
                  "COMMIT")
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
                        "The pre-crash timestamp lost precision in a historical SQL read" {})
                (check! (= previous-types (type-rows sink))
                        "The pre-crash application types changed in a historical read" {})
                (check! (= previous-type-keys (type-key-rows sink))
                        "The pre-crash replica identity changed in a historical read" {}))
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
            (sql! source "TRUNCATE probe_parent, probe_child, probe_types, probe_type_keys")
            (check-rows! source reference sink @subscriber)
            (check! (every? empty? (fixture-rows sink)) "Multi-table truncate left rows" {})
            (await! "Publisher did not confirm the truncate"
                    #(= [(:applied-lsn (mirror/status @subscriber))] (slot-query source slot "confirmed_flush_lsn")))
            (let [[confirmed] (slot-query source slot "confirmed_flush_lsn")]
              (sql! source "ALTER TABLE probe_parent ADD COLUMN unexpected integer"
                    "INSERT INTO probe_parent (tenant, id, unexpected) VALUES (1, 4, 10)")
              (await-state! @subscriber :paused)
              (check! (= [confirmed] (slot-query source slot "confirmed_flush_lsn"))
                      "Schema drift advanced the checkpoint" {})
              (check! (every? empty? (fixture-rows sink)) "Schema drift made partial data visible" {})))

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
               (fn [staged destination table live-table on-bytes]
                 (transfer-original staged destination table live-table on-bytes)
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
        "obsolete text, stamp timestamp(6) DEFAULT '2024-06-01 12:00:00.123456', "
        "tokens text DEFAULT '{first,NULL}')")
   (str "CREATE TABLE schema_meta (id integer, key_data jsonb DEFAULT '{\"num\":1.20}', "
        "tag text DEFAULT 'original', PRIMARY KEY (id, key_data))")])

(def ^:private source-schema-changes
  ["BEGIN"
   "ALTER TABLE schema_items RENAME COLUMN label TO summary"
   "ALTER TABLE schema_items DROP COLUMN obsolete"
   (str "ALTER TABLE schema_items ALTER COLUMN stamp TYPE timestamptz(3) "
        "USING stamp AT TIME ZONE 'America/Los_Angeles'")
   "ALTER TABLE schema_items ALTER COLUMN summary SET DEFAULT concat('publ','isher')"
   "ALTER TABLE schema_items ALTER COLUMN summary DROP NOT NULL"
   "ALTER TABLE schema_items ADD COLUMN flag text DEFAULT 'fast' NOT NULL"
   "ALTER TABLE schema_items ALTER COLUMN tokens DROP DEFAULT"
   "ALTER TABLE schema_items ALTER COLUMN tokens TYPE text[] USING tokens::text[]"
   "ALTER TABLE schema_items ADD COLUMN aliases text[] DEFAULT ARRAY['initial']"
   "ALTER TABLE schema_items ADD COLUMN observed timetz(6) DEFAULT '14:30:00.123456+04'"
   "ALTER TABLE schema_items ADD COLUMN metadata jsonb DEFAULT '{\"defined\": true}'"
   "CREATE TYPE schema_quality AS ENUM ('new', 'active', 'archived')"
   "CREATE DOMAIN schema_year AS integer CHECK (VALUE >= 1901 AND VALUE <= 2155)"
   "ALTER TABLE schema_items ADD COLUMN quality schema_quality DEFAULT 'new'"
   "ALTER TABLE schema_items ADD COLUMN released schema_year DEFAULT 2020"
   "ALTER TABLE schema_meta ALTER COLUMN tag SET DEFAULT 'changed'"
   "COMMIT"])

(defn- evolved-rows [conn]
  [(rows conn "SELECT id, label FROM schema_account ORDER BY id")
   (rows conn (str "SELECT id, summary, stamp::text, flag, tokens::text, aliases::text, observed::text, "
                   "metadata::text, quality::text, released::text FROM schema_items ORDER BY id"))
   (rows conn "SELECT id, tag, key_data::text FROM schema_meta ORDER BY id")])

(defn- schema-item-column-metadata [conn]
  ;; Match the mirror's publisher connection when deparsing defaults.
  (sql! conn "SET search_path = pg_catalog")
  (try
    (rows conn (str "SELECT column_name, data_type, datetime_precision, is_nullable, column_default "
                    "FROM information_schema.columns "
                    "WHERE table_name = 'schema_items' AND column_name <> 'db_id' "
                    "ORDER BY ordinal_position"))
    (finally (sql! conn "RESET search_path"))))

(defn- item-eid [path]
  (inspect-db path #(d/q '[:find ?eid . :where [?eid :schema_items/id 1]] %)))

(defn- metadata-eid [path]
  (inspect-db path #(d/q '[:find ?eid . :where [?eid :schema_meta/id 1]] %)))

(defn- verify-table-rename! [source reference sink subscriber path]
  (let [[[oid]] (rows sink "SELECT 'schema_items'::regclass::oid")
        at (inspect-db path :max-tx)
        eid (item-eid path)
        previous (rows sink "SELECT id, summary FROM schema_items ORDER BY id")
        previous-columns (schema-item-column-metadata sink)
        key-query (str "SELECT conname FROM pg_constraint "
                       "WHERE conrelid = 'schema_archive'::regclass AND contype = 'p'")
        ddl ["BEGIN"
             "ALTER TABLE schema_items RENAME TO schema_archive"
             "ALTER TABLE schema_archive RENAME CONSTRAINT schema_items_pkey TO schema_archive_pkey"
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
    (check! (= (rows source key-query) (rows sink key-query))
            "The renamed relation's primary key catalog differs" {})
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
        old-query "SELECT id, label, obsolete, stamp::text, tokens FROM schema_items ORDER BY id"
        temporal-query (fn [at]
                         (str "SELECT id, label, obsolete, stamp::text, tokens FROM schema_items "
                              "FOR SYSTEM_TIME AS OF " (literal at) " ORDER BY id"))
        old-rows [["1" "original" "keep history" "2024-06-01 12:00:00.123456" "{first,NULL}"]]]
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
              old-meta-id (metadata-eid path)
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
                        (str "UPDATE schema_items SET summary = NULL, tokens = '[0:1]={x,y}', "
                             "observed = '14:30:00.123457+04', quality = 'active', released = 2024 WHERE id = 1")
                        "INSERT INTO schema_items (id) VALUES (2)"
                        "UPDATE schema_meta SET key_data = '{\"num\":1.200}' WHERE id = 1"
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
            (check! (= old-meta-id (metadata-eid path))
                    "Equivalent JSONB key lost its Datahike entity during refresh" {})
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

(defn- primary-key-layout [conn]
  (rows conn (str "SELECT c.conname, c.conkey::text, i.indkey::text FROM pg_constraint c "
                  "JOIN pg_index i ON c.conindid = i.indexrelid "
                  "WHERE c.conrelid = 'key_items'::regclass AND c.contype = 'p' AND i.indisprimary")))

(defn run-primary-key-probe! [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-key-" (make-array FileAttribute 0)))
        path (str directory "/store")
        native-slot (str "mirror_key_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "mirror_key_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        slot (atom nil)
        table-ddl (str "CREATE TABLE key_items (legacy integer PRIMARY KEY, region integer NOT NULL, "
                       "item_no bigint NOT NULL, label text)")
        row-query "SELECT legacy, region, item_no, label FROM key_items ORDER BY item_no, label"
        key-column-query (str "SELECT column_name, ordinal_position FROM information_schema.key_column_usage "
                              "WHERE table_name = 'key_items' AND constraint_name = 'key_items_v2' ORDER BY ordinal_position")
        change-key (fn [key-name columns]
                     (str "ALTER TABLE key_items DROP CONSTRAINT " key-name ", ADD CONSTRAINT "
                          "key_items_v2 PRIMARY KEY (" columns ")"))]
    (prn {:artifacts directory})
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (try
        (doseq [c [source reference]] (sql! c table-ddl))
        (sql! source "INSERT INTO key_items VALUES (1, 7, 101, 'first'), (2, 7, 102, 'second')"
              "CREATE PUBLICATION mirror_key_pub FOR TABLE key_items")
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION mirror_key_native CONNECTION "
                             (literal source-conninfo) " PUBLICATION mirror_key_pub"
                             " WITH (create_slot=false, slot_name=" (literal native-slot) ")"))
        (await! "Native subscriber did not load the first key layout" #(= (rows source row-query) (rows reference row-query)))
        (reset! subscriber (mirror/start! config))
        (reset! slot (:slot (await-state! @subscriber :following)))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (let [old-layout (primary-key-layout source)
                old-at (inspect-db path :max-tx)
                eid-query "SELECT db_id FROM key_items WHERE item_no = 101"
                old-eid (rows sink eid-query)
                ddl (change-key "key_items_pkey" "item_no, region")]
            (check! (= old-layout (primary-key-layout sink)) "Initial primary key catalog differs" {})
            (sql! reference ddl)
            (sql! source "BEGIN" ddl
                  "UPDATE key_items SET label = 'during cutover' WHERE legacy = 1"
                  "INSERT INTO key_items VALUES (3, 8, 103, 'during cutover')"
                  "COMMIT")
            (await! "Mirror did not install the replacement primary key"
                    #(let [state (mirror/status @subscriber)]
                       (check! (not (#{:paused :failed} (:state state))) "The replacement key paused the mirror" state)
                       (and (= (primary-key-layout source) (primary-key-layout sink))
                            (= (rows source row-query) (rows sink row-query)))))
            (check! (= old-eid (rows sink eid-query)) "Replacing the key changed an existing entity" {})
            (check! (= (rows source key-column-query) (rows sink key-column-query))
                    "Information schema disagrees on the replacement key columns" {})
            (sql! sink (str "SET datahike.as_of = " old-at))
            (check! (= old-layout (primary-key-layout sink)) "Old primary key catalog was lost" {})
            (sql! sink "RESET datahike.as_of")
            ;; These rows share the old key. WAL must use the new key
            ;; when one of them is changed or deleted.
            (sql! source "INSERT INTO key_items VALUES (1, 7, 104, 'duplicate old key')"
                  "UPDATE key_items SET label = 'new key update' WHERE item_no = 101 AND region = 7"
                  "DELETE FROM key_items WHERE item_no = 102 AND region = 7")
            (await! "Mirror did not use the replacement key for live changes"
                    #(= (rows source row-query) (rows sink row-query)))
            (check! (= old-eid (rows sink eid-query)) "Live update changed an existing entity" {})
            (let [ddl (change-key "key_items_v2" "region, item_no")]
              (sql! reference ddl)
              (sql! source ddl))
            (await! "Mirror did not update the key column order"
                    #(= (primary-key-layout source) (primary-key-layout sink)))))
        (mirror/stop! @subscriber)
        (reset! subscriber (mirror/start! config))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (sql! source "UPDATE key_items SET label = 'after restart' WHERE item_no = 101 AND region = 7")
          (await! "Key replacement did not survive restart"
                  #(= (rows source row-query) (rows sink row-query) (rows reference row-query)))
          (let [before (rows sink row-query)
                layout (primary-key-layout sink)
                ddl (str "ALTER TABLE key_items DROP CONSTRAINT key_items_v2, "
                         "ADD CONSTRAINT key_items_label_pkey PRIMARY KEY (label)")]
            (sql! reference ddl)
            (sql! source "BEGIN" ddl
                  "UPDATE key_items SET item_no = 101 WHERE label = 'duplicate old key'"
                  "COMMIT")
            (let [{:keys [last-error]} (await-state! @subscriber :paused)]
              (check! (= :ambiguous-previous-key (:reason last-error))
                      "Duplicate old key did not report an ambiguous row match" last-error))
            (check! (= [before layout] [(rows sink row-query) (primary-key-layout sink)])
                    "An ambiguous key changed the published mirror" {})
            (sql! source "BEGIN"
                  "UPDATE key_items SET item_no = 104 WHERE label = 'duplicate old key'"
                  (change-key "key_items_label_pkey" "region, item_no")
                  "ALTER TABLE key_items ALTER COLUMN label DROP NOT NULL"
                  "COMMIT")))
        (mirror/stop! @subscriber)
        (reset! subscriber (mirror/start! config))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (check! (= [(rows source row-query) (primary-key-layout source)]
                     [(rows sink row-query) (primary-key-layout sink)])
                  "Restoring the old key did not recover the pending refresh" {})
          (let [before (rows sink row-query)
                layout (primary-key-layout sink)]
            (sql! source "ALTER TABLE key_items DROP CONSTRAINT key_items_v2, DROP COLUMN region, ADD PRIMARY KEY (item_no)")
            (let [{:keys [last-error]} (await-state! @subscriber :paused)]
              (check! (= :missing-previous-key (:reason last-error))
                      "Unmatchable old key did not report the specific admission failure" last-error))
            (check! (= [before layout] [(rows sink row-query) (primary-key-layout sink)])
                    "An unmatchable key changed the published mirror" {})))
        {:status :passed :artifacts directory}
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (when (and @slot (= ["false"] (slot-query source @slot "active::text")))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal @slot) ")")))
          (sql! reference "DROP SUBSCRIPTION IF EXISTS mirror_key_native")
          (when (= ["false"] (slot-query source native-slot "active::text"))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal native-slot) ")"))))))))

(defn- partition-catalog [conn]
  [(rows conn (str "SELECT c.relname, p.partstrat, p.partnatts, d.relname, p.partclass::text, "
                   "p.partcollation::text, pg_get_partkeydef(c.oid) FROM pg_partitioned_table p "
                   "JOIN pg_class c ON c.oid = p.partrelid LEFT JOIN pg_class d ON d.oid = p.partdefid "
                   "ORDER BY c.relname"))
   (rows conn (str "SELECT c.relname, a.attname FROM pg_partitioned_table p JOIN pg_class c ON c.oid = p.partrelid "
                   "JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(p.partattrs) "
                   "ORDER BY c.relname, a.attname"))
   (rows conn (str "SELECT c.relname, pg_get_expr(p.partexprs, c.oid) FROM pg_partitioned_table p "
                   "JOIN pg_class c ON c.oid = p.partrelid WHERE p.partexprs IS NOT NULL ORDER BY c.relname"))
   (rows conn (str "SELECT c.relname, pg_get_expr(c.relpartbound, c.oid) FROM pg_class c "
                   "WHERE c.relispartition AND c.relkind IN ('r','p') ORDER BY c.relname"))
   (rows conn "SELECT pg_get_partkeydef('pay_audit'::regclass)")])

(defn run-partition-probe!
  "Compare leaf-routed partitions with PostgreSQL, including a populated
   attachment, writes during staging, crash recovery, and detachment."
  [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-partitions-" (make-array FileAttribute 0)))
        path (str directory "/store")
        slot-name (str "mirror_partition_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "partition_pub" :store-path path :server {:port 0}}
        subscriber (atom nil)
        ddl [(str "CREATE TABLE pay (obsolete integer, bucket integer, id integer, note varchar(40), "
                  "occurred timestamptz(6) DEFAULT '2024-02-29 12:00:00.123456+00', "
                  "PRIMARY KEY (bucket,id)) PARTITION BY RANGE (bucket)")
             "CREATE TABLE pay_jan PARTITION OF pay FOR VALUES FROM (0) TO (10)"
             "CREATE TABLE pay_second PARTITION OF pay FOR VALUES FROM (10) TO (20) PARTITION BY RANGE (bucket)"
             "CREATE TABLE pay_feb PARTITION OF pay_second FOR VALUES FROM (10) TO (20)"
             "CREATE TABLE pay_empty PARTITION OF pay FOR VALUES FROM (30) TO (40) PARTITION BY RANGE (bucket)"
             "CREATE TABLE pay_rest PARTITION OF pay DEFAULT"
             "ALTER TABLE pay DROP COLUMN obsolete"
             "CREATE TABLE pay_expr (bucket integer, id integer) PARTITION BY LIST ((bucket + 1))"
             "CREATE TABLE pay_expr_a PARTITION OF pay_expr FOR VALUES IN (2)"
             "ALTER TABLE pay_expr_a ADD PRIMARY KEY (id)"
             "CREATE TABLE pay_audit (id integer PRIMARY KEY, note text)"]
        row-query (str "SELECT bucket, id, note, occurred::text, tableoid::regclass::text "
                       "FROM pay ORDER BY bucket, id")
        parent-oid-query "SELECT oid FROM pg_class WHERE relname = 'pay' AND relkind = 'p'"
        edges-query (str "SELECT p.relname, c.relname FROM pg_inherits i "
                         "JOIN pg_class p ON i.inhparent = p.oid JOIN pg_class c ON i.inhrelid = c.oid "
                         "WHERE c.relkind IN ('r','p') ORDER BY p.relname, c.relname")
        snapshot-at (fn [] (inspect-db path #(str (.toInstant ^java.util.Date
                                                   (:db/txInstant (d/entity % (:max-tx %)))))))
        probe-rows (fn [conn]
                     [(rows conn row-query)
                      (rows conn "SELECT bucket, id, note FROM pay_second ORDER BY bucket,id")
                      (rows conn "SELECT * FROM pay_empty")
                      (rows conn "SELECT * FROM pay_expr ORDER BY id")
                      (rows conn "SELECT count(*) FROM ONLY pay")
                      (rows conn "SELECT * FROM pay ORDER BY bucket,id")
                      (rows conn "SELECT count(*) FROM pay WHERE tableoid = 'pay_feb'::regclass")
                      (rows conn "SELECT tableoid::regclass::text, count(*) FROM pay GROUP BY tableoid ORDER BY 1")
                      (rows conn (str "SELECT a.id, p.tableoid::regclass::text FROM pay_audit a "
                                      "LEFT JOIN pay p ON p.id = a.id ORDER BY a.id, p.bucket"))
                      (rows conn "SELECT * FROM pay_audit ORDER BY id")])
        check-rows (fn [source reference sink]
                     (let [expected (probe-rows source)]
                       (await! "Native partition subscriber did not converge" #(= expected (probe-rows reference)))
                       (await! "Mirror partition rows did not converge"
                               #(let [state (mirror/status @subscriber)]
                                  (check! (not (#{:failed :paused} (:state state)))
                                          "Mirror paused during partition refresh" state)
                                  (= expected (probe-rows sink))))
                       (check! (= (rows source edges-query) (rows sink edges-query))
                               "Partition catalog edges differ" {:source (rows source edges-query)
                                                                 :mirror (rows sink edges-query)})
                       (check! (= (partition-catalog source) (partition-catalog sink))
                               "Partition keys or bounds differ" {:source (partition-catalog source)
                                                                  :mirror (partition-catalog sink)})))]
    (prn {:artifacts directory})
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (try
        (doseq [c [source reference]]
          (sql! c "SET TIME ZONE 'UTC'")
          (apply sql! c ddl))
        (sql! source
              "INSERT INTO pay VALUES (1,1,'moves'), (2,2,'truncate'), (11,1,'existing')"
              "INSERT INTO pay_expr VALUES (1,1)"
              "INSERT INTO pay_audit VALUES (1,'initial'), (3,'unmatched')"
              "CREATE PUBLICATION partition_pub FOR TABLE pay, pay_audit, pay_expr"
              "CREATE PUBLICATION partition_subset_pub FOR TABLE pay_jan"
              "CREATE PUBLICATION partition_root_pub FOR TABLE pay WITH (publish_via_partition_root = true)")
        (let [report (mirror/publication-report (assoc config :publication "partition_subset_pub"))]
          (check! (and (:compatible? report) (= ["pay_jan"] (mapv :name (:tables report)))
                       (empty? (:partition-parents report)))
                  "A partial publication exposed its parent or an unpublished sibling" report))
        (let [report (mirror/publication-report (assoc config :publication "partition_root_pub"))]
          (check! (some #(= :root-partition-routing (:reason %)) (mapcat :issues (:tables report)))
                  "Root-routed publication lacked a routing blocker" report))
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal slot-name) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION partition_native CONNECTION " (literal source-conninfo)
                             " PUBLICATION partition_pub WITH (create_slot=false, slot_name=" (literal slot-name) ")"))
        (reset! subscriber (mirror/start! config))
        (let [{:keys [slot]} (await-state! @subscriber :following)]
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (check-rows source reference sink)
            (with-open [st (.prepareStatement sink "SELECT p.tableoid FROM pay p WHERE p.id = ? ORDER BY p.bucket")]
              (.setInt st 1 1)
              (with-open [rs (.executeQuery st)]
                (check! (= "oid" (.getColumnTypeName (.getMetaData rs) 1)) "tableoid lost its OID type" {})
                (check! (and (.next rs) (pos? (.getLong rs 1))) "Prepared tableoid read lost its leaf" {})))
            (sql! source "UPDATE pay SET bucket=12 WHERE bucket=1 AND id=1" "TRUNCATE pay_jan")
            (check-rows source reference sink)
            (let [pre-attach (rows sink row-query)
                  pre-attach-at (snapshot-at)
                  parent-oid (rows sink parent-oid-query)
                  ddl [(str "CREATE TABLE pay_mar (id integer, note varchar(40), bucket integer, "
                            "occurred timestamptz(6) DEFAULT '2024-02-29 12:00:00.123457+00', "
                            "PRIMARY KEY (bucket,id))")
                       "ALTER TABLE pay ATTACH PARTITION pay_mar FOR VALUES FROM (20) TO (30)"]
                  started (promise)
                  continue-copy (promise)
                  copy-original @#'mirror/copy-table!
                  install-original @#'mirror/install-staged-tables!]
              (apply sql! reference ddl)
              (with-redefs-fn
                {#'mirror/copy-table!
                 (fn [publisher destination table on-bytes]
                   (when (= "pay_mar" (:name table))
                     (deliver started true)
                     (check! (deref continue-copy 30000 false) "Partition stage was not released" {}))
                   (copy-original publisher destination table on-bytes))
                 #'mirror/install-staged-tables!
                 (fn [stage destination check-running! max-bytes]
                   (install-original stage destination check-running! max-bytes)
                   (throw (ex-info "Interrupted partition cutover" {})))}
                (fn []
                  (try
                    (sql! source "BEGIN" (first ddl)
                          "INSERT INTO pay_mar VALUES (2,'attached rows',21)" (second ddl) "COMMIT")
                    (sql! reference "ALTER SUBSCRIPTION partition_native REFRESH PUBLICATION")
                    (check! (deref started 30000 false) "Attached partition was not staged"
                            (mirror/status @subscriber))
                    (sql! source "BEGIN"
                          "UPDATE pay SET bucket=22 WHERE bucket=12 AND id=1"
                          "UPDATE pay_audit SET note='during attach' WHERE id=1" "COMMIT")
                    (await! "Unrelated rows stopped following during partition refresh"
                            #(= [["1" "during attach"]] (rows sink "SELECT * FROM pay_audit WHERE id=1")))
                    (check! (= pre-attach (rows sink row-query)) "Partition changes escaped before cutover" {})
                    (finally (deliver continue-copy true)))
                  (check! (= "Interrupted partition cutover"
                             (get-in (await-state! @subscriber :paused) [:last-error :message]))
                          "The interrupted partition cutover did not reach installation" {})
                  (check! (= pre-attach (rows sink row-query)) "Partition cutover rollback changed parent rows" {})))
              (mirror/stop! @subscriber)
              (child-crash! (assoc config :probe-crash-mode :refresh) (str directory "/crash.log"))
              (await! "The crashed partition slot remained active"
                      #(= ["false"] (slot-query source slot "active::text")))
              (let [[confirmed] (slot-query source slot "confirmed_flush_lsn")
                    cutover (durable-position path)]
                (check! (= [["t"]] (rows source (str "SELECT " (literal confirmed)
                                                     "::pg_lsn < " (literal cutover) "::pg_lsn")))
                        "The partition cutover was acknowledged before the crash" {}))
              (reset! subscriber (mirror/start! config))
              (with-open [restarted (DriverManager/getConnection (sink-url @subscriber))]
                (check-rows source reference restarted)
                (check! (= parent-oid (rows restarted parent-oid-query)) "Partition attachment changed the parent OID" {})
                (sql! restarted (str "SET datahike.as_of = " (literal pre-attach-at)))
                (check! (= pre-attach (rows restarted row-query)) "Historical parent lost the pre-attach rows" {})
                (sql! restarted "RESET datahike.as_of")
                (let [before (rows restarted row-query)
                      old-catalog (partition-catalog restarted)
                      before-at (snapshot-at)]
                  (sql! reference "ALTER TABLE pay DETACH PARTITION pay_mar")
                  (sql! source "ALTER TABLE pay DETACH PARTITION pay_mar")
                  (sql! reference "ALTER SUBSCRIPTION partition_native REFRESH PUBLICATION")
                  (check-rows source reference restarted)
                  (check! (= "42P01" (try (rows restarted "SELECT * FROM pay_mar") nil
                                          (catch SQLException error (.getSQLState error))))
                          "The detached publication leaf stayed queryable" {})
                  (sql! restarted (str "SET datahike.as_of = " (literal before-at)))
                  (check! (= before (rows restarted row-query)) "Historical parent lost detached partition rows" {})
                  (check! (= old-catalog (partition-catalog restarted)) "Historical partition metadata changed" {})
                  (sql! restarted "RESET datahike.as_of")
                  (sql! reference "TRUNCATE pay_mar" (second ddl))
                  (sql! source "UPDATE pay_mar SET note='changed while detached'" (second ddl))
                  (sql! reference "ALTER SUBSCRIPTION partition_native REFRESH PUBLICATION")
                  (check-rows source reference restarted)
                  (sql! source "TRUNCATE pay")
                  (check-rows source reference restarted)))))
          (mirror/stop! @subscriber)
          (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot) ")"))
          {:status :passed :artifacts directory})
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (sql! reference "DROP SUBSCRIPTION IF EXISTS partition_native")
          (when (= ["false"] (slot-query source slot-name "active::text"))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot-name) ")"))))))))

(defn- type-refresh-catalog [conn]
  [(rows conn (str "SELECT oid, enumlabel, enumsortorder FROM pg_enum "
                   "WHERE enumtypid = 'type_mood'::regtype ORDER BY enumsortorder"))
   (rows conn (str "SELECT typnotnull, pg_get_expr(typdefaultbin, 0) FROM pg_type "
                   "WHERE oid = 'type_amount'::regtype"))
   (rows conn (str "SELECT conname FROM pg_constraint WHERE contypid = 'type_amount'::regtype "
                   "AND contype = 'c' ORDER BY conname"))
   (rows conn (str "SELECT column_name, udt_name, domain_name FROM information_schema.columns "
                   "WHERE table_name = 'type_items' AND column_name <> 'db_id' ORDER BY ordinal_position"))])

(defn run-type-refresh-probe! [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-type-refresh-" (make-array FileAttribute 0)))
        path (str directory "/store")
        native-slot (str "mirror_type_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "type_refresh_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        main-slot (atom nil)
        query-items "SELECT id::text, label, promoted::text, qty::text, legacy::text FROM type_items ORDER BY label"
        query-details "SELECT id, state::text, amount::text FROM type_details ORDER BY id"
        current-rows (fn [conn] [(rows conn query-items) (rows conn query-details)])
        row-ids #(inspect-db path (fn [db]
                                    (into {} (d/q '[:find ?label ?e
                                                    :where [?e :type_items/label ?label]] db))))
        type-ids #(rows % "SELECT typname, oid FROM pg_type WHERE typname IN ('type_mood', 'type_amount') ORDER BY typname")
        ddl ["BEGIN"
             "ALTER TYPE type_mood RENAME VALUE 'first' TO 'temporary'"
             "ALTER TYPE type_mood RENAME VALUE 'second' TO 'first'"
             "ALTER TYPE type_mood RENAME VALUE 'temporary' TO 'second'"
             "ALTER TYPE type_mood ADD VALUE 'middle' BEFORE 'first'"
             "ALTER DOMAIN type_amount SET DEFAULT 7"
             "ALTER DOMAIN type_amount DROP NOT NULL"
             "ALTER DOMAIN type_amount DROP CONSTRAINT type_nonnegative"
             "ALTER DOMAIN type_amount ADD CONSTRAINT type_new_range CHECK (VALUE >= -2)"
             (str "ALTER TABLE type_items ALTER COLUMN promoted TYPE type_mood USING promoted::type_mood, "
                  "ALTER COLUMN qty TYPE type_amount USING qty::type_amount, "
                  "ALTER COLUMN legacy TYPE text USING legacy::text")
             "COMMIT"]]
    (prn {:artifacts directory})
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (try
        (doseq [conn [source reference]]
          (sql! conn "CREATE TYPE type_mood AS ENUM ('first', 'second')"
                (str "CREATE DOMAIN type_amount AS integer DEFAULT 2 NOT NULL "
                     "CONSTRAINT type_nonnegative CHECK (VALUE >= 0)")
                (str "CREATE TABLE type_items (id type_mood PRIMARY KEY, label text, promoted text, "
                     "qty integer, legacy type_amount)")
                "CREATE TABLE type_details (id integer PRIMARY KEY, state type_mood, amount type_amount)"))
        (sql! source "INSERT INTO type_items VALUES ('first','a','first',8,2), ('second','b','second',9,3)"
              "INSERT INTO type_details VALUES (1,'first',2)"
              "CREATE PUBLICATION type_refresh_pub FOR TABLE type_items, type_details")
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION type_refresh_native CONNECTION "
                             (literal source-conninfo) " PUBLICATION type_refresh_pub "
                             "WITH (create_slot=false, slot_name=" (literal native-slot) ")"))
        (let [before (current-rows source)]
          (await! "Native subscriber did not load the type refresh fixture" #(= before (current-rows reference)))
          (reset! subscriber (mirror/start! config))
          (reset! main-slot (:slot (await-state! @subscriber :following)))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (check! (= before (current-rows sink)) "Mirror type fixture rows differ" {})
            (let [old-catalog (type-refresh-catalog source)
                  old-type-ids (type-ids sink)
                  old-row-ids (row-ids)
                  before-at (inspect-db path #(str (.toInstant ^java.util.Date
                                                    (:db/txInstant (d/entity % (:max-tx %))))))
                  install-var #'mirror/install-staged-tables!
                  install-original @install-var]
              (check! (= old-catalog (type-refresh-catalog sink)) "Initial type metadata differs"
                      {:source old-catalog :mirror (type-refresh-catalog sink)})
              (apply sql! reference ddl)
              (with-redefs-fn
                {install-var (fn [stage destination check-running! max-bytes]
                               (install-original stage destination check-running! max-bytes)
                               (throw (ex-info "Interrupted type cutover" {})))}
                (fn []
                  (apply sql! source ddl)
                  ;; A WAL update uses a label that identified a different
                  ;; entity before DDL. It must wait for the new definition.
                  (sql! source "UPDATE type_items SET qty=-1 WHERE id='second'"
                        "INSERT INTO type_details VALUES (2,'middle',NULL)")
                  (check! (= "Interrupted type cutover"
                             (get-in (await-state! @subscriber :paused) [:last-error :message]))
                          "Type refresh did not reach installation" (mirror/status @subscriber))
                  (check! (= before (current-rows sink)) "Interrupted type installation changed rows" {})
                  (check! (= old-catalog (type-refresh-catalog sink)) "Interrupted type installation changed metadata" {})))
              (mirror/stop! @subscriber)
              (child-crash! (assoc config :probe-crash-mode :refresh) (str directory "/crash.log"))
              (await! "Crashed type refresh slot remained active"
                      #(= ["false"] (slot-query source @main-slot "active::text")))
              (let [[confirmed] (slot-query source @main-slot "confirmed_flush_lsn")
                    cutover (durable-position path)]
                (check! (= [["t"]] (rows source (str "SELECT " (literal confirmed)
                                                     "::pg_lsn < " (literal cutover) "::pg_lsn")))
                        "Type cutover was acknowledged before the crash" {}))
              (reset! subscriber (mirror/start! config))
              (with-open [restarted (DriverManager/getConnection (sink-url @subscriber))]
                (let [expected (current-rows source)]
                  (await! "Native subscriber did not follow the type migration" #(= expected (current-rows reference)))
                  (check! (= expected (current-rows restarted)) "Recovered rows differ after the type migration"
                          {:source expected :mirror (current-rows restarted)}))
                (check! (= old-type-ids (type-ids restarted)) "Changing a type altered its local OID" {})
                (check! (= old-row-ids (row-ids)) "Renamed enum primary keys changed entity identity"
                        {:old old-row-ids :new (row-ids)})
                (check! (= (type-refresh-catalog source) (type-refresh-catalog restarted)) "Refreshed type metadata differs"
                        {:source (type-refresh-catalog source) :mirror (type-refresh-catalog restarted)})
                (sql! restarted (str "SET datahike.as_of = " (literal before-at)))
                (check! (= before (current-rows restarted)) "Historical rows lost old type values" {})
                (check! (= old-catalog (type-refresh-catalog restarted)) "Historical type catalog changed"
                        {:source old-catalog :mirror (type-refresh-catalog restarted)})
                (sql! restarted "RESET datahike.as_of")
                (sql! source "DELETE FROM type_items WHERE id='first'"
                      "UPDATE type_items SET promoted='middle', qty=12 WHERE id='second'"
                      "INSERT INTO type_details (id,state) VALUES (3,'first')")
                (let [expected (current-rows source)]
                  (await! "Native subscriber did not apply writes after type cutover" #(= expected (current-rows reference)))
                  (await! "Mirror did not apply writes after type cutover" #(= expected (current-rows restarted))))))))
        {:status :passed :artifacts directory}
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (sql! reference "DROP SUBSCRIPTION IF EXISTS type_refresh_native")
          (when (= ["false"] (slot-query source @main-slot "active::text"))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal @main-slot) ")"))))))))

(defn -main [& [argument]]
  (if (= "--crash-worker" argument)
    (run-crash-worker! (edn/read (PushbackReader. *in*)))
    (let [source (System/getenv "PG_MIRROR_SOURCE_URL")
          reference (System/getenv "PG_MIRROR_REFERENCE_URL")
          conninfo (System/getenv "PG_MIRROR_SOURCE_CONNINFO")]
      (check! (every? seq [source reference conninfo])
              "Set PG_MIRROR_SOURCE_URL, PG_MIRROR_REFERENCE_URL and PG_MIRROR_SOURCE_CONNINFO" {})
      (prn ((case argument
              "--partitions" run-partition-probe!
              "--type-refresh" run-type-refresh-probe!
              "--primary-key-refresh" run-primary-key-probe!
              "--schema-refresh" run-schema-refresh-probe!
              "--refresh" run-refresh-probe!
              run-probe!)
            source reference conninfo)))))
