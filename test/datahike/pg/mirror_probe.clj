(ns datahike.pg.mirror-probe
  "End-to-end mirror probe using two disposable native PostgreSQL 18 databases."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.mirror :as mirror]
            [datahike.pg.mirror.spool :as spool]
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
        "observed timetz(6), wall time(3), fixed character(4), payload bytea, tags text[], lexemes tsvector, "
        "member_ids uuid[], amounts numeric(8,-2)[], snapshots timestamptz(6)[], samples integer[])")
   (str "CREATE TABLE probe_type_keys (observed timetz(6), payload bytea, tags text[], "
        "attrs jsonb, fixed character(4), lexemes tsvector, label text, "
        "rating probe_rating, released probe_year, ratios real[], entries jsonb[], "
        "PRIMARY KEY (observed, payload, tags, attrs, fixed, lexemes, rating, released, ratios, entries))")])

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
                  "payload::text, tags::text, lexemes::text, member_ids::text, amounts::text, "
                  "snapshots::text, samples::text, array_dims(samples), array_ndims(samples) "
                  "FROM probe_types ORDER BY id")))

(defn- type-key-rows [conn]
  (rows conn (str "SELECT observed::text, payload::text, tags::text, attrs::text, fixed::text, "
                  "lexemes::text, label, rating::text, released::text, ratios::text, entries::text "
                  "FROM probe_type_keys ORDER BY label")))

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
                                            (seq (:relation-ids transaction)))
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

(defn- durable-checkpoint [path]
  (inspect-db path #'mirror/checkpoint))

(defn- durable-position [path]
  (:end-lsn (durable-checkpoint path)))

(defn- check-admission! [source config]
  (sql! source
        "CREATE TABLE probe_unhandled (id integer PRIMARY KEY, observed inet, payload xml)"
        "CREATE DOMAIN probe_multi_check AS integer CHECK (VALUE > 0) CHECK (VALUE < 42)"
        "CREATE TABLE probe_unhandled_domain (id integer PRIMARY KEY, n probe_multi_check)"
        "CREATE FUNCTION probe_positive(n integer) RETURNS boolean LANGUAGE sql IMMUTABLE RETURN n > 0"
        "CREATE DOMAIN probe_custom_check AS integer CHECK (public.probe_positive(VALUE))"
        "CREATE TABLE probe_custom_domain (id integer PRIMARY KEY, n probe_custom_check)"
        "CREATE FUNCTION probe_lt(a integer, b integer) RETURNS boolean LANGUAGE sql IMMUTABLE RETURN a < b"
        "CREATE OPERATOR public.## (FUNCTION = probe_lt, LEFTARG = integer, RIGHTARG = integer)"
        "CREATE DOMAIN probe_operator_check AS integer CHECK (VALUE OPERATOR(public.##) 10)"
        "CREATE TABLE probe_operator_domain (id integer PRIMARY KEY, n probe_operator_check)"
        "CREATE PUBLICATION probe_custom_domain_pub FOR TABLE probe_custom_domain, probe_operator_domain"
        "CREATE SCHEMA probe_other"
        "CREATE TABLE probe_other.accounts (id integer NOT NULL)"
        "CREATE UNIQUE INDEX accounts_identity ON probe_other.accounts (id)"
        "ALTER TABLE probe_other.accounts REPLICA IDENTITY USING INDEX accounts_identity"
        "CREATE PUBLICATION probe_other_pub FOR TABLE probe_other.accounts"
        "CREATE TABLE probe_no_key (note text)"
        "ALTER TABLE probe_no_key REPLICA IDENTITY NOTHING"
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
    (let [custom (report "probe_custom_domain_pub")
          other (report "probe_other_pub")]
      (check! (and (not (:compatible? custom))
                   (= #{"probe_custom_domain" "probe_operator_domain"}
                      (into #{} (comp (filter #(some (fn [issue] (= :domain-publisher-dependency (:detail issue)))
                                                     (:issues %)))
                                      (map :name))
                            (:tables custom))))
              "Domain check depending on a publisher function or operator was admitted" custom)
      (check! (:compatible? other)
              "Alternate identity table in a named schema was rejected" other))
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
  [(rows conn (str "SELECT column_name, data_type, udt_name, datetime_precision, character_maximum_length, "
                   "character_octet_length FROM information_schema.columns WHERE table_name = 'probe_types' "
                   "AND column_name NOT IN ('id', 'db_id') ORDER BY column_name"))
   (rows conn (str "SELECT attname, atttypid, atttypmod, format_type(atttypid, atttypmod) FROM pg_attribute "
                   "WHERE attrelid = 'probe_types'::regclass "
                   "AND attname IN ('member_ids', 'amounts', 'snapshots', 'samples') ORDER BY attname"))])

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
  [source-url reference-url source-conninfo & [full?]]
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
        (when full?
          (sql! source "ALTER TABLE probe_parent REPLICA IDENTITY FULL"
                "ALTER TABLE probe_child REPLICA IDENTITY FULL"
                "ALTER TABLE probe_type_keys REPLICA IDENTITY FULL"))
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
              (str "UPDATE probe_types SET "
                   "member_ids = '[0:1]={82d1ad1a-a1ea-4660-9f33-4dcbdaa8ad76,NULL}', "
                   "amounts = '{1234,NULL,-3456}', snapshots = '{\"2024-02-29 13:55:19.123457+05:30\",NULL}', "
                   "samples = '[-1:0][3:4]={{1,NULL},{2,3}}' WHERE id = 1")
              "UPDATE probe_types SET member_ids = '{}', samples = '{}' WHERE id = 2"
              (str "INSERT INTO probe_type_keys VALUES "
                   "('14:00:00.123456+04', decode('00ff','hex'), '[0:1]={a,NULL}', '{\"num\":1.200}', "
                   "'a', 'cat:2 fox:1D', 'one', 'Unrated', DEFAULT, '{-0,NaN,NULL}', "
                   "'{\"{\\\"num\\\": 1.200}\",\"null\",NULL}'), "
                   "('24:00:00+01:30:45', decode('','hex'), '{}', 'null', '雪', '', 'two', 'G', 1998, '{}', '{}')")
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
                            "tags = '[0:2]={\"a,b\",\"NULL\",NULL}', lexemes = 'cat:1D fox:2B', "
                            "snapshots = '{\"2024-02-29 13:55:19.123458+05:30\",NULL}' WHERE id = 1")
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
            (let [query "SELECT snapshots, snapshots::text FROM probe_types ORDER BY id"]
              (try
                (doseq [conn [source sink]] (sql! conn "SET TIME ZONE 'America/Vancouver'"))
                (check! (= (rows source query) (rows sink query))
                        "The mirror lost the reader's time zone when rendering an array" {})
                (finally
                  (doseq [conn [source sink]] (sql! conn "SET TIME ZONE 'UTC'")))))
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
                       "observed = '24:00:00+02', tags = ARRAY['', 'a'], "
                       "member_ids = '{104846d1-b048-4f78-8835-f4b5b98f9f18}', amounts = NULL, "
                       "samples = '[1:2]={8,13}' WHERE id = 1")
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

(defn- check-unpublication! [source reference subscriber config slot directory]
  (let [path (:store-path config)
        frozen (fn [] (vals (get-in (durable-checkpoint path) [:manifest :unpublished-tables])))
        event-rows #(rows % "SELECT id, note FROM refresh_event ORDER BY id")
        event-identity #(rows % "SELECT 'refresh_event'::regclass::oid, db_id FROM refresh_event WHERE id = 1")]
    (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
      (let [identity (event-identity sink)]
        (mirror/stop! @subscriber)
        (sql! source "UPDATE refresh_event SET note = 'last published value' WHERE id = 1")
        (await! "Native subscriber missed the last published update"
                #(= [["1" "last published value"]] (event-rows reference)))
        (sql! source "ALTER PUBLICATION mirror_refresh_pub DROP TABLE refresh_event"
              "UPDATE refresh_event SET note = 'while unpublished' WHERE id = 1"
              "UPDATE refresh_account SET label = 'still following' WHERE id = 1")
        (sql! reference "ALTER SUBSCRIPTION mirror_refresh_native REFRESH PUBLICATION")
        (child-crash! (assoc config :probe-crash-mode :refresh) (str directory "/removal-crash.log"))
        (await! "The crashed removal slot remained active" #(= ["false"] (slot-query source slot "active::text")))
        (check! (= ["refresh_event"] (mapv :name (frozen))) "Removal did not reach the durable checkpoint" {})
        (reset! subscriber (mirror/start! config))
        (with-open [restarted (DriverManager/getConnection (sink-url @subscriber))]
          (check! (= (event-rows reference) (event-rows restarted) [["1" "last published value"]])
                  "Publisher removal changed the last mirrored rows" {})
          (check! (= identity (event-identity restarted)) "Unpublication changed relation or row identity" {})
          (check! (= [["still following"]] (rows restarted "SELECT label FROM refresh_account"))
                  "A retained table stopped following after removal" {})
          (sql! source "ALTER PUBLICATION mirror_refresh_pub ADD TABLE refresh_event"
                "INSERT INTO refresh_event (id, note) VALUES (2, 'republished')")
          (sql! reference "TRUNCATE refresh_event"
                "ALTER SUBSCRIPTION mirror_refresh_native REFRESH PUBLICATION")
          (let [expected (event-rows source)]
            (await! "Native subscriber did not recopy the republished table" #(= expected (event-rows reference)))
            (await! "Mirror did not resync the republished table" #(= expected (event-rows restarted))))
          (check! (= identity (event-identity restarted)) "Re-adding the same publisher table changed identity" {}))))
    (sql! source "BEGIN"
          "DROP TABLE refresh_note"
          "DROP TYPE refresh_status"
          "ALTER PUBLICATION mirror_refresh_pub DROP TABLE refresh_account, refresh_event"
          "COMMIT")
    (sql! reference "ALTER SUBSCRIPTION mirror_refresh_native REFRESH PUBLICATION")
    (await! "An empty publication did not retain its last rows"
            #(= #{"refresh_account" "refresh_event" "refresh_note"} (set (map :name (frozen)))))
    (mirror/stop! @subscriber)
    (reset! subscriber (mirror/start! config))
    (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
      (check! (= (refresh-rows reference) (refresh-rows sink)) "Restart forgot the unpublished tables" {})
      (check! (= [["active"]] (rows sink "SELECT status::text FROM refresh_note"))
              "Dropping the source table and its type lost its retained values" {})
      (let [query "SELECT id, note, status::text FROM refresh_note"
            before (rows sink query)
            as-of (inspect-db path #(str (.toInstant ^java.util.Date (:db/txInstant (d/entity % (:max-tx %))))))]
        (sql! source "CREATE TABLE refresh_note (id integer PRIMARY KEY, note text)"
              "INSERT INTO refresh_note VALUES (3, 'replacement')"
              "ALTER PUBLICATION mirror_refresh_pub ADD TABLE refresh_note")
        (await! "A replacement with the same name did not take over the live name"
                #(= [["3" "replacement"]] (rows sink "SELECT id, note FROM refresh_note")))
        (sql! sink (str "SET datahike.as_of = " (literal as-of)))
        (check! (= before (rows sink query)) "Name reuse lost the historical rows or type" {}))
      (check! (= #{"refresh_account" "refresh_event"} (set (map :name (frozen))))
              "Name reuse lost an unrelated retained table" {}))))

(defn run-refresh-probe!
  "Exercise online publication changes, an interrupted handoff, and recovery
   after committing a handoff but before feedback to PostgreSQL."
  [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-refresh-" (make-array FileAttribute 0)))
        path (str directory "/store")
        log (str directory "/refresh-crash.log")
        native-slot (str "mirror_refresh_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "mirror_refresh_pub" :store-path path :server {:port 0}}
        subscriber (atom nil)
        schema ["CREATE TYPE refresh_status AS ENUM ('active', 'closed')"
                "CREATE TABLE refresh_account (id integer PRIMARY KEY, label text)"
                (str "CREATE TABLE refresh_event (id integer PRIMARY KEY, note varchar(40), "
                     "happened timestamptz(6))")
                (str "CREATE TABLE refresh_note (id integer PRIMARY KEY, note text, "
                     "status refresh_status DEFAULT 'active')")]]
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
          (check-unpublication! source reference subscriber config slot directory)
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
                  (let [manifest (:manifest (durable-checkpoint path))]
                    (check! (= ids (set (keys (:pending-schema manifest))))
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
          (await! "Publisher did not release the schema-refresh slot"
                  #(= ["false"] (slot-query source slot "active::text")))
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

(defn run-primary-key-probe! [source-url reference-url source-conninfo & [full?]]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-key-" (make-array FileAttribute 0)))
        path (str directory "/store")
        native-slot (str "mirror_key_ref_" (str/replace (str (random-uuid)) "-" ""))
        config {:source-url source-url :publication "mirror_key_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        slot (atom nil)
        table-ddl (str "CREATE TABLE key_items (legacy integer PRIMARY KEY, region integer NOT NULL, "
                       "item_no bigint NOT NULL, label text" (when full? ", payload text") ")")
        row-query (str "SELECT legacy, region, item_no, label"
                       (when full? ", md5(payload)") " FROM key_items ORDER BY item_no, label")
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
        (when full?
          (sql! source "ALTER TABLE key_items REPLICA IDENTITY FULL"))
        (sql! source "INSERT INTO key_items VALUES (1, 7, 101, 'first'), (2, 7, 102, 'second')"
              "CREATE PUBLICATION mirror_key_pub FOR TABLE key_items")
        (when full?
          (sql! source (str "UPDATE key_items SET payload = "
                            "(SELECT string_agg(md5(i::text),'') FROM generate_series(1,4000) i) "
                            "WHERE legacy = 1"))
          (check! (:compatible? (mirror/publication-report config))
                  "Keyed FULL is not admitted" {}))
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION mirror_key_native CONNECTION "
                             (literal source-conninfo) " PUBLICATION mirror_key_pub"
                             " WITH (create_slot=false, slot_name=" (literal native-slot) ")"))
        (await! "Native subscriber did not load the first key layout" #(= (rows source row-query) (rows reference row-query)))
        (reset! subscriber (mirror/start! config))
        (reset! slot (:slot (await-state! @subscriber :following)))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (when full?
            (let [entity (rows sink "SELECT db_id FROM key_items WHERE legacy = 1")
                  identity-mode #(get-in (durable-checkpoint path) [:manifest :tables 0 :replica-identity])]
              (check! (= \f (identity-mode)) "FULL mode was not recorded durably" {})
              (sql! source "UPDATE key_items SET legacy = 9 WHERE legacy = 1")
              (await! "FULL update did not use the old primary key"
                      #(= (rows source row-query) (rows sink row-query) (rows reference row-query)))
              (check! (= entity (rows sink "SELECT db_id FROM key_items WHERE legacy = 9"))
                      "FULL key update changed the entity" {})
              (sql! source "UPDATE key_items SET legacy = 1 WHERE legacy = 9")
              (await! "FULL update did not restore the row"
                      #(= (rows source row-query) (rows sink row-query)))
              (doseq [[mode expected] [["DEFAULT" nil] ["FULL" \f]]]
                (sql! source "BEGIN" (str "ALTER TABLE key_items REPLICA IDENTITY " mode)
                      "UPDATE key_items SET label = label || ' refreshed' WHERE legacy = 1"
                      "COMMIT")
                (await! "Replica identity change did not reach a durable cutover"
                        #(and (= expected (identity-mode))
                              (= (rows source row-query) (rows sink row-query) (rows reference row-query))))
                (check! (= entity (rows sink "SELECT db_id FROM key_items WHERE legacy = 1"))
                        "Replica identity refresh changed the entity" {}))))
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
          (await! "Restoring the old key did not recover the pending refresh"
                  #(and (nil? (:refresh (mirror/status @subscriber)))
                        (empty? (get-in (durable-checkpoint path) [:manifest :pending-schema]))
                        (= [(rows source row-query) (primary-key-layout source)]
                           [(rows sink row-query) (primary-key-layout sink)])))
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

(defn- check-companion-report! [source config]
  (let [report #(mirror/publication-report config)
        issues #(set (map :reason (:issues (report))))]
    (check! (:compatible? (report)) "Matched root and leaf publications were rejected" (report))
    ;; An explicit list of today's leaves would omit tomorrow's attachment.
    (let [result (mirror/publication-report (assoc config :leaf-publication "partition_subset_pub"))]
      (check! (some #(= :different-publication-membership (:reason %)) (:issues result))
              "Different publication selectors were accepted" result))
    (sql! source "ALTER PUBLICATION partition_root_pub SET TABLE pay (bucket,id,note,occurred), pay_audit, pay_expr")
    (check! (contains? (issues) :publication-projection) "An explicit column list was accepted" (report))
    (sql! source "ALTER PUBLICATION partition_root_pub SET TABLE pay WHERE (bucket >= 0), pay_audit, pay_expr")
    (check! (contains? (issues) :publication-projection) "A root row filter was accepted" (report))
    (sql! source "ALTER PUBLICATION partition_root_pub SET TABLE pay, pay_audit, pay_expr")
    (check! (:compatible? (report)) "Restoring the root definition did not restore admission" (report))))

(defn- check-companion-resume! [source config subscriber slot]
  (sql! source "ALTER PUBLICATION partition_root_pub SET (publish = 'insert')")
  (let [error (:last-error (await-state! @subscriber :paused))
        saved (durable-checkpoint (:store-path config))
        confirmed (slot-query source slot "confirmed_flush_lsn")]
    (check! (some #(= :different-publication-options (:reason %)) (get-in error [:report :issues]))
            "The running mirror accepted a mismatched publication" error)
    (mirror/stop! @subscriber)
    (sql! source "ALTER PUBLICATION partition_root_pub SET (publish = 'insert, update, delete, truncate')")
    (reset! subscriber (mirror/start! (dissoc config :leaf-publication)))
    (check! (= :different-leaf-publication (:reason (:last-error (await-state! @subscriber :paused))))
            "Restart changed the durable stream selection" (mirror/status @subscriber))
    (mirror/stop! @subscriber)
    (sql! source "DROP PUBLICATION partition_root_pub"
          (str "CREATE PUBLICATION partition_root_pub FOR TABLE pay, pay_audit, pay_expr "
               "WITH (publish_via_partition_root = true)"))
    (reset! subscriber (mirror/start! config))
    (check! (= :publication-identity-changed (:reason (:last-error (await-state! @subscriber :paused))))
            "Restart accepted a replaced publication" (mirror/status @subscriber))
    (check! (= saved (durable-checkpoint (:store-path config))) "Rejected restarts moved the checkpoint" {})
    (check! (= confirmed (slot-query source slot "confirmed_flush_lsn"))
            "Rejected restarts changed publisher feedback" {})))

(defn run-partition-probe!
  "Compare publisher partitions with a native subscriber, including a
   populated attachment, writes during staging, crash recovery, and detachment."
  [source-url reference-url source-conninfo & [companion?]]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-partitions-" (make-array FileAttribute 0)))
        path (str directory "/store")
        slot-name (str "mirror_partition_ref_" (str/replace (str (random-uuid)) "-" ""))
        config (cond-> {:source-url source-url :publication "partition_pub" :store-path path :server {:port 0}}
                 companion? (assoc :publication "partition_root_pub" :leaf-publication "partition_pub"))
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
                      (rows conn "SELECT count(*) FROM ONLY public.pay")
                      (rows conn (str "SELECT p.id FROM pay p LEFT JOIN ONLY pay q "
                                      "ON p.bucket = q.bucket AND p.id = q.id ORDER BY p.bucket, p.id"))
                      (rows conn "SELECT * FROM pay ORDER BY bucket,id")
                      (rows conn "SELECT count(*) FROM pay WHERE tableoid = 'pay_feb'::regclass")
                      (rows conn "SELECT tableoid::regclass::text, count(*) FROM pay GROUP BY tableoid ORDER BY 1")
                      (rows conn (str "SELECT a.id, p.tableoid::regclass::text FROM pay_audit a "
                                      "LEFT JOIN pay p ON p.id = a.id ORDER BY a.id, p.bucket"))
                      (rows conn "SELECT * FROM pay_audit ORDER BY id")])
        check-rows (fn [source reference sink]
                     (sql! sink "SET TIME ZONE 'UTC'")
                     (let [expected (probe-rows source)]
                       (await! "Native partition subscriber did not converge" #(= expected (probe-rows reference)))
                       (try
                         (await! "Mirror partition rows did not converge"
                                 #(let [state (mirror/status @subscriber)]
                                    (check! (not (#{:failed :paused} (:state state)))
                                            "Mirror paused during partition refresh" state)
                                    (= expected (probe-rows sink))))
                         (catch clojure.lang.ExceptionInfo e
                           (throw (ex-info (ex-message e)
                                           {:source expected :mirror (probe-rows sink) :status (mirror/status @subscriber)}
                                           e))))
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
              (str "CREATE PUBLICATION partition_root_pub FOR TABLE pay, pay_audit, pay_expr "
                   "WITH (publish_via_partition_root = true)"))
        (let [report (mirror/publication-report (assoc (dissoc config :leaf-publication) :publication "partition_subset_pub"))]
          (check! (and (:compatible? report) (= ["pay_jan"] (mapv :name (:tables report)))
                       (empty? (:partition-parents report)))
                  "A partial publication exposed its parent or an unpublished sibling" report))
        (let [report (mirror/publication-report (assoc (dissoc config :leaf-publication) :publication "partition_root_pub"))]
          (check! (some #(= :root-partition-routing (:reason %)) (mapcat :issues (:tables report)))
                  "Root-routed publication lacked a routing blocker" report))
        (when companion? (check-companion-report! source config))
        (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal slot-name) ", 'pgoutput')"))
        (sql! reference (str "CREATE SUBSCRIPTION partition_native CONNECTION " (literal source-conninfo)
                             " PUBLICATION partition_pub WITH (create_slot=false, slot_name=" (literal slot-name) ")"))
        (reset! subscriber (mirror/start! config))
        (let [{:keys [slot]} (await-state! @subscriber :following)]
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (check-rows source reference sink)
            (when companion?
              (let [saved (durable-checkpoint path)]
                (check! (= ["partition_root_pub" "partition_pub"]
                           [(:publication saved) (get-in saved [:manifest :leaf-publication :name])])
                        "Checkpoint lost the root or leaf publication" saved)))
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
          (when companion? (check-companion-resume! source config subscriber slot))
          (mirror/stop! @subscriber)
          (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot) ")"))
          {:status :passed :artifacts directory})
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (sql! reference "DROP SUBSCRIPTION IF EXISTS partition_native")
          (when (= ["false"] (slot-query source slot-name "active::text"))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot-name) ")"))))))))

(defn run-root-publication-probe! [source-url reference-url source-conninfo]
  (run-partition-probe! source-url reference-url source-conninfo true))

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

(defn- pg-control! [pg-bin directory & args]
  (let [command (into [(str pg-bin "/pg_ctl") "-D" directory "-w" "-t" "30"] args)
        process (-> (ProcessBuilder. ^java.util.List command) (.redirectErrorStream true) .start)]
    (when-not (.waitFor process 35 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Timed out waiting for pg_ctl" {:command command})))
    (check! (zero? (.exitValue process)) "pg_ctl failed"
            {:command command :output (slurp (.getInputStream process))})))

(defn- caught-up-standby! [standby position]
  (await! "Standby has not replayed the test checkpoint"
          #(= [["t"]] (rows standby (str "SELECT pg_last_wal_replay_lsn() >= " (literal position) "::pg_lsn")))))

(defn- synchronize-slot! [{:keys [pg-bin standby-dir]} source standby slot]
  (let [synced-position #(first (slot-query standby slot
                                            (str "CASE WHEN synced AND failover AND NOT temporary "
                                                 "AND invalidation_reason IS NULL THEN confirmed_flush_lsn END")))]
    (loop [attempt 0]
      (when-not (synced-position)
        (check! (< attempt 50) "Standby did not synchronize the mirror slot"
                {:slot (slot-query standby slot "restart_lsn, confirmed_flush_lsn, temporary::text, synced::text")})
        ;; Let the decoder serialize a snapshot as the standby's temporary
        ;; slot waits for the primary to reach its retention horizon.
        (sql! source "SELECT pg_log_standby_snapshot()" "SELECT pg_log_standby_snapshot()")
        (#'mirror/emit-barrier! source)
        (Thread/sleep 400)
        (recur (inc attempt))))
    ;; Freeze slot synchronization while the physical standby keeps replaying.
    ;; This makes the promoted slot replay WAL already committed in the mirror.
    (spit (str standby-dir "/postgresql.auto.conf") "\nsync_replication_slots = off\n" :append true)
    (pg-control! pg-bin standby-dir "reload")
    (await! "Standby slot sync worker did not stop"
            #(and (= [["off"]] (rows standby "SHOW sync_replication_slots"))
                  (= [["t"]] (rows standby (str "SELECT NOT EXISTS (SELECT 1 FROM pg_stat_activity "
                                                "WHERE backend_type = 'slot sync worker')")))))
    (synced-position)))

(defn- failover-checkpoint! [path slot expected-timeline]
  (let [saved (durable-checkpoint path)]
    (check! (= [slot expected-timeline] [(:slot saved) (get-in saved [:origin :timeline])])
            "Slot or timeline in the durable checkpoint is incorrect" saved)
    saved))

(defn- reject-confirmation-ahead! [config promoted subscriber slot]
  (mirror/stop! @subscriber)
  (let [path (:store-path config)
        before (failover-checkpoint! path slot 2)]
    (sql! promoted "INSERT INTO ha_items VALUES (4,'never acknowledged by the mirror',41)"
          (str "SELECT pg_replication_slot_advance(" (literal slot) ", pg_current_wal_lsn())"))
    (let [[confirmed] (slot-query promoted slot "confirmed_flush_lsn")]
      (check! (pos? (#'mirror/compare-lsn confirmed (:end-lsn before)))
              "Probe did not move the slot past the checkpoint" {})
      (reset! subscriber (mirror/start! config))
      (let [{:keys [last-error port]} (await-state! @subscriber :paused)]
        (check! (and (nil? port) (:resnapshot-required? last-error)
                     (= confirmed (get-in last-error [:slot-status :confirmed-lsn])))
                "Mirror did not reject the confirmation ahead of its checkpoint" last-error))
      (check! (= before (durable-checkpoint path)) "Ahead slot changed the durable checkpoint" {})
      (check! (= [confirmed] (slot-query promoted slot "confirmed_flush_lsn")) "Rejected resume advanced the slot" {}))))

(defn- promote-during-refresh! [fixture config source standby subscriber sink slot]
  (let [{:keys [pg-bin primary-dir standby-dir standby-url]} fixture
        path (:store-path config)
        original-checkpoint (failover-checkpoint! path slot 1)
        synced-before (synchronize-slot! fixture source standby slot)
        first-row-id #(inspect-db path (fn [db] (d/q '[:find ?e . :where [?e :ha_items/id 1]] db)))
        old-row-id (first-row-id)
        old-rows (rows sink "SELECT id,note FROM ha_items ORDER BY id")
        old-at (inspect-db path #(str (.toInstant ^java.util.Date
                                       (:db/txInstant (d/entity % (:max-tx %))))))
        copy-started (promise)
        release-copy (promise)
        first-copy? (atom true)
        copy-var #'mirror/copy-table!
        copy-original @copy-var]
    (with-redefs-fn
      {copy-var (fn [publisher destination table on-bytes]
                  (when (and (= "ha_items" (:name table)) (compare-and-set! first-copy? true false))
                    (deliver copy-started true)
                    (check! (deref release-copy 35000 false) "Probe did not release the staged COPY" {}))
                  (copy-original publisher destination table on-bytes))}
      (fn []
        (let [before (try
                       (sql! source "ALTER TABLE ha_items ADD COLUMN stamp integer DEFAULT 12")
                       (check! (deref copy-started 25000 false) "Mirror did not start a schema refresh"
                               (mirror/status @subscriber))
                       (sql! source "BEGIN" "INSERT INTO ha_items (id,note) VALUES (2,'staged')"
                             "UPDATE ha_audit SET note='durable before promotion' WHERE id=1" "COMMIT")
                       (await! "The unrelated table did not follow during staging"
                               #(= [["durable before promotion"]] (rows sink "SELECT note FROM ha_audit")))
                       (let [{:keys [manifest end-lsn] :as saved} (durable-checkpoint path)]
                         (check! (seq (:pending-schema manifest)) "Pending refresh was not durable" saved)
                         (check! (pos? (#'mirror/compare-lsn end-lsn synced-before))
                                 "Probe needs a standby slot behind the checkpoint" {:checkpoint end-lsn :synced synced-before})
                         (caught-up-standby! standby end-lsn)
                         (pg-control! pg-bin primary-dir "stop" "-m" "fast")
                         (pg-control! pg-bin standby-dir "promote")
                         saved)
                       (finally (deliver release-copy true)))]
          (with-open [promoted (DriverManager/getConnection standby-url)]
            (sql! promoted "INSERT INTO ha_items VALUES (3,'after promotion',31)")
            (await! "Mirror did not restart and finish the refresh on timeline 2"
                    #(let [status (mirror/status @subscriber)]
                       (check! (not (#{:paused :failed} (:state status))) "Mirror stopped after promotion" status)
                       (and (= :following (:state status)) (= 2 (:source-timeline status))
                            (:last-schema-refresh status) (nil? (:refresh status)))))
            (let [query "SELECT id,note,stamp FROM ha_items ORDER BY id"
                  expected (rows promoted query)
                  after (failover-checkpoint! path slot 2)]
              (check! (= expected (rows sink query)) "Rows differ after the promoted primary's refresh" {})
              (check! (and old-row-id (= old-row-id (first-row-id))) "Failover refresh changed an existing entity ID" {})
              (check! (and (pos? (#'mirror/compare-lsn (:end-lsn after) (:end-lsn before)))
                           (nil? (get-in after [:manifest :pending-schema])))
                      "Failover did not finish and checkpoint the schema refresh" after)
              (check! (= 1 (inspect-db path #(count (d/q '[:find ?tx
                                                           :where [_ :ha_audit/note "durable before promotion" ?tx]]
                                                         (d/history %)))))
                      "Replaying the lagging synchronized slot rewrote durable history" {})
              (sql! sink (str "SET datahike.as_of = " (literal old-at)))
              (check! (= old-rows (rows sink "SELECT id,note FROM ha_items ORDER BY id")) "Failover changed historical rows" {})
              (check! (= (get-in original-checkpoint [:origin :timeline])
                         (inspect-db path #(get-in (#'mirror/checkpoint
                                                    (d/as-of % (java.util.Date/from (java.time.Instant/parse old-at))))
                                                   [:origin :timeline])))
                      "Historical checkpoint moved to the new timeline" {})
              (sql! sink "RESET datahike.as_of")
              (mirror/stop! @subscriber)
              (reset! subscriber (mirror/start! config))
              (with-open [restarted (DriverManager/getConnection (sink-url @subscriber))]
                (check! (= expected (rows restarted query)) "Restart on the promoted primary changed rows" {}))
              (reject-confirmation-ahead! config promoted subscriber slot))))))))

(defn- reject-divergent-promotion! [fixture config source standby subscriber sink slot]
  (let [{:keys [pg-bin primary-dir standby-dir fixture-dir standby-url]} fixture
        synced-before (synchronize-slot! fixture source standby slot)]
    (sql! source "ALTER SYSTEM SET synchronized_standby_slots = ''" "SELECT pg_reload_conf()")
    (await! "Publisher did not release its logical decoder from standby feedback"
            #(= [[""]] (rows source "SHOW synchronized_standby_slots")))
    (pg-control! pg-bin standby-dir "stop" "-m" "fast")
    (sql! source "INSERT INTO ha_items VALUES (99,'durable on the old timeline only')")
    (await! "Mirror did not apply the divergent row"
            #(= [["99"]] (rows sink "SELECT id FROM ha_items WHERE id=99")))
    (let [before (failover-checkpoint! (:store-path config) slot 1)]
      (pg-control! pg-bin primary-dir "stop" "-m" "fast")
      (pg-control! pg-bin standby-dir "start" "-l" (str fixture-dir "/standby.log"))
      (pg-control! pg-bin standby-dir "promote")
      (with-open [promoted (DriverManager/getConnection standby-url)]
        (check! (empty? (rows promoted "SELECT id FROM ha_items WHERE id=99"))
                "Probe did not create a divergent primary" {})
        (let [{:keys [last-error]} (await-state! @subscriber :paused)]
          (check! (= :timeline-fork-before-checkpoint (:reason last-error))
                  "Mirror paused for the wrong promotion condition" last-error))
        (check! (= before (durable-checkpoint (:store-path config))) "Rejected failover altered the checkpoint" {})
        (check! (= [["99"]] (rows sink "SELECT id FROM ha_items WHERE id=99")) "Rejected failover changed served rows" {})
        (check! (= [synced-before] (slot-query promoted slot "confirmed_flush_lsn"))
                "Rejected failover acknowledged divergent WAL" {})))))

(defn run-failover-probe! [{:keys [primary-url standby-url source-url fixture-dir mode] :as fixture}]
  (let [config {:source-url source-url :publication "failover_pub" :store-path (str fixture-dir "/store")
                :server {:port 0} :schema-refresh? true :failover? true}
        subscriber (atom nil)]
    (with-open [source (DriverManager/getConnection primary-url)
                standby (DriverManager/getConnection standby-url)]
      (try
        (sql! source "CREATE TABLE ha_items (id integer PRIMARY KEY, note text)"
              "CREATE TABLE ha_audit (id integer PRIMARY KEY, note text)"
              "INSERT INTO ha_items VALUES (1,'initial')" "INSERT INTO ha_audit VALUES (1,'initial')"
              "CREATE PUBLICATION failover_pub FOR TABLE ha_items, ha_audit"
              "CREATE ROLE mirror_follower LOGIN REPLICATION"
              "GRANT SELECT ON ha_items, ha_audit TO mirror_follower"
              "GRANT EXECUTE ON FUNCTION pg_logical_emit_message(boolean, text, text, boolean) TO mirror_follower")
        (reset! subscriber (mirror/start! config))
        (let [{:keys [slot source-timeline]} (await-state! @subscriber :following)]
          (check! (= 1 source-timeline) "Probe requires a fresh primary on timeline 1" {})
          (check! (= ["true" "false"] (slot-query source slot "failover::text, synced::text"))
                  "The initial mirror slot did not advertise failover" {:slot slot})
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (case mode
              "promote" (promote-during-refresh! fixture config source standby subscriber sink slot)
              "diverge" (reject-divergent-promotion! fixture config source standby subscriber sink slot))))
        {:status :passed :mode mode}
        (finally (when @subscriber (mirror/stop! @subscriber)))))))

(defn- pause-on-slot-loss! [config subscriber expected]
  (reset! subscriber (mirror/start! (assoc config :resnapshot-on-slot-loss? false)))
  (let [{:keys [last-error]} (await-state! @subscriber :paused)]
    (check! (= expected (:reason last-error)) "Mirror paused for a different slot condition" last-error))
  (mirror/stop! @subscriber))

(defn- interrupted-slot-recovery! [config subscriber]
  (let [install-var #'mirror/install-staged-tables!
        original @install-var]
    (with-redefs-fn
      {install-var (fn [& args]
                     (apply original args)
                     (throw (ex-info "Interrupted replacement snapshot" {})))}
      (fn []
        (reset! subscriber (mirror/start! config))
        (try
          (let [{:keys [last-error]} (await-state! @subscriber :paused)]
            (check! (= "Interrupted replacement snapshot" (:message last-error))
                    "Recovery paused before reaching its install boundary" last-error))
          (finally (mirror/stop! @subscriber)))))))

(defn- invalidate-idle-slot! [source slot]
  (try
    (sql! source "ALTER SYSTEM SET idle_replication_slot_timeout = '1s'" "SELECT pg_reload_conf()")
    (await! "PostgreSQL did not enable idle slot expiry"
            #(= [["1s"]] (rows source "SHOW idle_replication_slot_timeout")))
    (await! "Slot did not become eligible for idle expiry"
            #(= ["t"] (slot-query source slot "NOT active AND clock_timestamp() - inactive_since >= interval '1 second'")))
    (sql! source "CHECKPOINT")
    (check! (= ["idle_timeout"] (slot-query source slot "invalidation_reason")) "PostgreSQL did not invalidate the idle slot" {})
    (finally
      (sql! source "ALTER SYSTEM RESET idle_replication_slot_timeout" "SELECT pg_reload_conf()")
      (await! "PostgreSQL did not disable idle slot expiry"
              #(= [["0"]] (rows source "SHOW idle_replication_slot_timeout"))))))

(defn- history-preserving-slot-recovery! [config source subscriber original-slot path]
  (let [copy-started (promise)
        release-copy (promise)
        copy-var #'mirror/copy-table!
        original @copy-var]
    (with-redefs-fn
      {copy-var (fn [publisher destination table on-bytes]
                  (when (= "recovery_items" (:name table))
                    (deliver copy-started true)
                    (check! (deref release-copy 25000 false) "Probe did not release the recovery copy" {}))
                  (original publisher destination table on-bytes))}
      (fn []
        (reset! subscriber (mirror/start! config))
        (try
          (check! (deref copy-started 25000 false) "Recovery did not start copying the replacement snapshot"
                  (mirror/status @subscriber))
          (check! (= ["idle_timeout"] (slot-query source original-slot "invalidation_reason"))
                  "Original slot was retired before installation" {})
          (check! (= original-slot (:slot (durable-checkpoint path))) "Copy advanced the durable slot before install" {})
          (sql! source "INSERT INTO recovery_items VALUES (5,'during snapshot',21)")
          (finally (deliver release-copy true)))
        (let [status (await-state! @subscriber :following)]
          (check! (= :slot-invalidated (get-in status [:last-recovery :reason])) "Idle timeout was not recorded" status)
          (check! (nil? (slot-query source original-slot "slot_name")) "Retired slot remains on the publisher" {})
          (check! (not= original-slot (:slot status)) "Recovery reused the invalid slot" status)
          status)))))

(defn- recover-while-following! [source subscriber sink query]
  (let [old-slot (:slot (mirror/status @subscriber))
        before (rows sink query)
        reconnecting (promise)
        allow-reconnect (promise)
        replicate-var #'mirror/replicate!
        original @replicate-var]
    (with-redefs-fn
      {replicate-var (fn [& args]
                       (deliver reconnecting true)
                       (check! (deref allow-reconnect 20000 false) "Probe did not release the reconnect" {})
                       (apply original args))}
      (fn []
        (try
          (check! (= ["t"] (slot-query source old-slot "pg_terminate_backend(active_pid, 3000)"))
                  "Could not terminate the publisher's active decoder" {})
          (check! (deref reconnecting 20000 false) "Mirror did not attempt to reconnect" (mirror/status @subscriber))
          (sql! source (str "SELECT pg_drop_replication_slot(" (literal old-slot) ")")
                "INSERT INTO recovery_items (id,label,stamp) VALUES (6,'lost while following',22)")
          (check! (= before (rows sink query)) "The existing listener stopped serving the durable snapshot" {})
          (finally (deliver allow-reconnect true)))
        (await! "The running mirror did not recover its lost slot"
                #(let [{:keys [state slot port]} (mirror/status @subscriber)]
                   (and (= :following state) (not= old-slot slot) port (= (rows source query) (rows sink query)))))))))

(defn- recover-from-ahead-slot! [config source subscriber query]
  (let [old-slot (:slot (mirror/status @subscriber))
        replication-copy #'mirror/replication-copy
        original @replication-copy
        advanced? (atom false)]
    (mirror/stop! @subscriber)
    (sql! source "INSERT INTO recovery_items (id,label,stamp) VALUES (7,'ahead of the mirror',23)")
    (with-redefs-fn
      {replication-copy
       (fn [replication slot publication position]
         (when (and (= slot old-slot) (compare-and-set! advanced? false true))
           (sql! source (str "SELECT pg_replication_slot_advance(" (literal old-slot) ", pg_current_wal_lsn())")))
         (original replication slot publication position))}
      (fn []
        (reset! subscriber (mirror/start! config))
        (let [{:keys [last-recovery]} (await-state! @subscriber :following)]
          (check! @advanced? "Probe did not advance the slot between validation and acquisition" {})
          (check! (= [:slot-confirmation-ahead old-slot] ((juxt :reason :old-slot) last-recovery))
                  "The mirror did not recover its ahead slot" last-recovery)
          (check! (nil? (slot-query source old-slot "slot_name")) "The ahead slot was not retired" {}))))
    (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
      (check! (= (rows source query) (rows sink query)) "Snapshot recovery lost rows from the skipped WAL interval" {}))))

(defn run-slot-recovery-probe! [source-url follower-url directory]
  (let [path (str directory "/store")
        config {:source-url follower-url :publication "recovery_pub" :store-path path
                :schema-refresh? true :resnapshot-on-slot-loss? true :server {:port 0}}
        subscriber (atom nil)
        first-id #(inspect-db path (fn [db] (d/q '[:find ?e . :where [?e :recovery_items/id 1]] db)))
        original-query "SELECT id,note FROM recovery_items ORDER BY id"
        updated-query "SELECT id,label,stamp FROM recovery_items ORDER BY id"]
    (with-open [source (DriverManager/getConnection source-url)]
      (try
        (sql! source "CREATE TABLE recovery_items (id int PRIMARY KEY, note text)"
              "INSERT INTO recovery_items VALUES (1,'original'),(2,'deleted during gap')"
              "CREATE PUBLICATION recovery_pub FOR TABLE recovery_items"
              "CREATE ROLE mirror_recovery_reader LOGIN REPLICATION"
              "GRANT SELECT ON recovery_items TO mirror_recovery_reader"
              "GRANT EXECUTE ON FUNCTION pg_logical_emit_message(boolean, text, text, boolean) TO mirror_recovery_reader")
        (reset! subscriber (mirror/start! config))
        (let [{old-slot :slot} (await-state! @subscriber :following)
              old-id (first-id)
              before (with-open [sink (DriverManager/getConnection (sink-url @subscriber))] (rows sink original-query))
              before-at (inspect-db path #(str (.toInstant ^java.util.Date
                                                (:db/txInstant (d/entity % (:max-tx %))))))]
          (mirror/stop! @subscriber)
          (let [old (durable-checkpoint path)]
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal old-slot) ")")
                  "BEGIN"
                  "ALTER TABLE recovery_items RENAME COLUMN note TO label"
                  "ALTER TABLE recovery_items ADD COLUMN stamp integer DEFAULT 7"
                  "UPDATE recovery_items SET label='changed during gap' WHERE id=1"
                  "DELETE FROM recovery_items WHERE id=2"
                  "INSERT INTO recovery_items VALUES (3,'inserted during gap',12)"
                  "CREATE TABLE recovery_new (id int PRIMARY KEY, note text)"
                  "INSERT INTO recovery_new VALUES (1,'new published table')"
                  "GRANT SELECT ON recovery_new TO mirror_recovery_reader"
                  "ALTER PUBLICATION recovery_pub ADD TABLE recovery_new"
                  "COMMIT")
            (pause-on-slot-loss! config subscriber :slot-missing)
            (check! (= old (durable-checkpoint path)) "Default slot loss changed the durable checkpoint" {})
            (interrupted-slot-recovery! config subscriber)
            (let [{:keys [slot end-lsn manifest]} (durable-checkpoint path)
                  reserved (get-in manifest [:pending-recovery :slot])]
              (check! (and (= slot old-slot) (= end-lsn (:end-lsn old)) (seq reserved)
                           (= (:manifest old) (dissoc manifest :pending-recovery)))
                      "Interrupted recovery altered the visible publication or cursor" manifest)
              (check! (= [[reserved]] (rows source "SELECT slot_name FROM pg_replication_slots"))
                      "Recovery did not persist its only replacement slot before copying" {})
              (check! (= old-id (first-id)) "Interrupted recovery changed existing identity" {})
              (pause-on-slot-loss! config subscriber :pending-slot-recovery)
              (child-crash! (assoc config :probe-crash-mode :refresh) (str directory "/recovery-crash.log"))
              (let [cutover (durable-checkpoint path)
                    gap (peek (get-in cutover [:manifest :recovery-gaps]))]
                (check! (= [reserved reserved old-slot (:end-lsn old) (:end-lsn cutover) :slot-missing]
                           [(:slot cutover) (:new-slot gap) (:old-slot gap) (:from-lsn gap) (:snapshot-lsn gap) (:reason gap)])
                        "Durable gap or replacement slot differs" {:expected-slot reserved :gap gap})
                (check! (nil? (get-in cutover [:manifest :pending-recovery])) "Installed recovery stayed pending" {})
                (await! "Replacement slot was still active after crash"
                        #(= [(:snapshot-lsn gap) "false"] (slot-query source reserved "confirmed_flush_lsn,active::text")))
                (check! (= [[reserved]] (rows source "SELECT slot_name FROM pg_replication_slots")) "Recovery leaked a slot after restart" {})
                (sql! source "INSERT INTO recovery_items VALUES (4,'after recovery crash',18)")
                (reset! subscriber (mirror/start! config))
                (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
                  (check! (= (rows source updated-query) (rows sink updated-query)) "Post-crash WAL did not catch up" {})
                  (check! (= (rows source "SELECT * FROM recovery_new ORDER BY id")
                             (rows sink "SELECT id,note FROM recovery_new ORDER BY id")) "Recovery lost a new table" {})
                  (check! (= old-id (first-id)) "Recovery changed a matched entity's identity" {})
                  (sql! sink (str "SET datahike.as_of = " (literal before-at)))
                  (check! (= before (rows sink original-query)) "Recovery changed pre-gap history" {}))
                (mirror/stop! @subscriber)
                (check! (= [gap] (get-in (durable-checkpoint path) [:manifest :recovery-gaps]))
                        "Post-crash startup repeated the gap" {})
                (invalidate-idle-slot! source reserved)
                (sql! source "UPDATE recovery_items SET stamp=14 WHERE id=1")
                (history-preserving-slot-recovery! config source subscriber reserved path)
                (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
                  (check! (= (rows source updated-query) (rows sink updated-query))
                          "Recovery lost a change committed during its replacement snapshot" {})
                  (sql! source "ALTER TABLE recovery_items ADD COLUMN after_recovery text DEFAULT 'included'")
                  (await! "Schema refresh did not finish after slot recovery"
                          #(get-in (mirror/status @subscriber) [:last-schema-refresh :cutover]))
                  (check! (= [["included"]] (rows sink "SELECT after_recovery FROM recovery_items WHERE id=1"))
                          "Post-recovery schema refresh failed" {})
                  (check! (= [:slot-missing :slot-invalidated]
                             (mapv :reason (get-in (durable-checkpoint path) [:manifest :recovery-gaps])))
                          "Schema refresh lost the durable gap records" {})
                  (recover-while-following! source subscriber sink updated-query))
                (recover-from-ahead-slot! config source subscriber updated-query)
                (check! (= [:slot-missing :slot-invalidated :slot-missing :slot-confirmation-ahead]
                           (mapv :reason (get-in (durable-checkpoint path) [:manifest :recovery-gaps])))
                        "Recovery lost or repeated a gap record" {})))))
        {:status :passed :probe :slot-recovery}
        (finally (when @subscriber (mirror/stop! @subscriber)))))))

(defn- spool-files [path]
  (filterv #(str/ends-with? (.getName ^java.io.File %) ".pgoutput")
           (some-> (io/file (str path ".mirror-spool")) .listFiles)))

(defn- stream-insert! [conn start]
  (sql! conn (str "INSERT INTO stream_items SELECT id, payload FROM generate_series(" start "," (+ start 7)
                  ") AS ids(id) CROSS JOIN (SELECT string_agg(md5(n::text),'') AS payload "
                  "FROM generate_series(1,384) AS seed(n)) AS data")))

(defn- flush-stream! [conn]
  (sql! conn "SELECT pg_logical_emit_message(false, 'stream_probe', 'flush', true)"))

(defn- stream-rows [conn]
  (rows conn "SELECT id, length(payload), md5(payload) FROM stream_items ORDER BY id"))

(defn- await-stream-rows! [source sink subscriber]
  (let [expected (stream-rows source)]
    (await! "Streamed rows did not converge"
            #(let [status (mirror/status @subscriber)]
               (check! (not (#{:paused :failed} (:state status))) "Mirror stopped applying streamed changes" status)
               (and (= expected (stream-rows sink)) (zero? (:spooled-bytes status)))))))

(defn- interleave-streams! [source tx-a tx-b sink subscriber path]
  (sql! tx-a "BEGIN")
  (stream-insert! tx-a 100)
  (flush-stream! tx-a)
  (await! "PostgreSQL did not send a streamed segment before commit"
          #(pos? (:spooled-bytes (mirror/status @subscriber))))
  (let [initial-bytes (:spooled-bytes (mirror/status @subscriber))]
    (sql! tx-a "SAVEPOINT discard")
    (stream-insert! tx-a 200)
    (sql! tx-a "SAVEPOINT nested")
    (stream-insert! tx-a 300)
    (flush-stream! tx-a)
    (await! "The streamed savepoint did not reach the spool"
            #(> (:spooled-bytes (mirror/status @subscriber)) (+ initial-bytes 65536))))
  (sql! tx-b "BEGIN")
  (stream-insert! tx-b 400)
  (flush-stream! tx-b)
  (await! "Two streamed transactions were not buffered concurrently" #(= 2 (count (spool-files path))))
  (sql! source "INSERT INTO stream_items VALUES (50,'ordinary commit')")
  (await! "Ordinary commit was blocked behind streamed transactions"
          #(= [["50"]] (rows sink "SELECT id FROM stream_items ORDER BY id")))
  (sql! tx-b "COMMIT")
  (await! "First streamed commit did not publish while the other was open"
          #(and (= [["8"]] (rows sink "SELECT count(*) FROM stream_items WHERE id BETWEEN 400 AND 407"))
                (= 1 (count (spool-files path)))))
  (check! (= [["0"]] (rows sink "SELECT count(*) FROM stream_items WHERE id BETWEEN 100 AND 399"))
          "An uncommitted streamed row was exposed" {})
  (let [before-abort (:spooled-bytes (mirror/status @subscriber))]
    (sql! tx-a "ROLLBACK TO SAVEPOINT discard")
    (flush-stream! tx-a)
    (await! "The spool did not truncate after rolling back a streamed savepoint"
            #(< (:spooled-bytes (mirror/status @subscriber)) before-abort)))
  (stream-insert! tx-a 500)
  (sql! tx-a "COMMIT")
  (await-stream-rows! source sink subscriber)
  (check! (= [["0"]] (rows sink "SELECT count(*) FROM stream_items WHERE id BETWEEN 200 AND 399"))
          "Nested savepoint rows survived their rollback" {})
  (sql! tx-b "BEGIN")
  (stream-insert! tx-b 600)
  (flush-stream! tx-b)
  (await! "Expected a spool for the transaction that will abort" #(seq (spool-files path)))
  (sql! tx-b "ROLLBACK")
  (await! "The top-level abort did not release its spool"
          #(and (empty? (spool-files path)) (zero? (:spooled-bytes (mirror/status @subscriber)))))
  (check! (= (stream-rows source) (stream-rows sink)) "Top-level abort changed the mirror rows" {}))

(defn- streamed-history [path id]
  (inspect-db path #(into #{} (map first)
                          (d/q '[:find ?tx :in $ ?id :where [_ :stream_items/id ?id ?tx]] (d/history %) id))))

(defn- refresh-with-streamed-catch-up! [source sink subscriber]
  (let [copy-started (promise)
        release-copy (promise)
        stage-spooled (atom false)
        copy-var #'mirror/copy-table!
        copy-original @copy-var
        append-var #'spool/append!
        append-original @append-var]
    (with-redefs-fn
      {copy-var (fn [publisher destination table on-bytes]
                  (when (= "stream_items" (:name table))
                    (deliver copy-started true)
                    (check! (deref release-copy 25000 false) "Probe did not release the refresh copy" {}))
                  (copy-original publisher destination table on-bytes))
       append-var (fn [& args]
                    (when (= "pg-datahike-table-refresh" (.getName (Thread/currentThread)))
                      (reset! stage-spooled true))
                    (apply append-original args))}
      (fn []
        (try
          (sql! source "ALTER TABLE stream_items ADD COLUMN refreshed stream_status DEFAULT 'new layout'")
          (check! (deref copy-started 25000 false) "Schema refresh did not export its snapshot" (mirror/status @subscriber))
          (stream-insert! source 900)
          (check! (= [["0"]] (rows sink "SELECT count(*) FROM stream_items WHERE id >= 900"))
                  "Streamed rows escaped the staged schema" {})
          (finally (deliver release-copy true)))
        (await! "Schema refresh did not apply the streamed catch-up transaction"
                #(get-in (mirror/status @subscriber) [:last-schema-refresh :cutover]))
        (check! @stage-spooled "The temporary slot did not exercise streamed catch-up" {})
        (check! (= [["8"]] (rows sink "SELECT count(*) FROM stream_items WHERE id >= 900 AND refreshed='new layout'"))
                "Cutover lost streamed changes or fast defaults" {})
        (await-stream-rows! source sink subscriber)))))

(defn- retry-stream-after-quota! [config source subscriber slot directory]
  (let [path (:store-path config)]
    (mirror/stop! @subscriber)
    (reset! subscriber (mirror/start! (assoc config :max-stream-spool-bytes 32768)))
    (let [{:keys [applied-lsn]} (await-state! @subscriber :following)]
      (await! "The slot did not confirm the pre-limit checkpoint"
              #(= [applied-lsn] (slot-query source slot "confirmed_flush_lsn")))
      (stream-insert! source 800)
      (let [{:keys [last-error spooled-bytes]} (await-state! @subscriber :paused)]
        (check! (= :stream-spool-limit (:reason last-error)) "Expected a pause at the spool limit" last-error)
        (check! (and (zero? spooled-bytes) (empty? (spool-files path))) "A spool remained after the limit error" {}))
      (check! (= applied-lsn (durable-position path)) "Quota error advanced the durable checkpoint" {})
      (check! (= [applied-lsn] (slot-query source slot "confirmed_flush_lsn")) "Quota error acknowledged the transaction" {})
      (mirror/stop! @subscriber)
      (child-crash! config (str directory "/stream-crash.log"))
      (await! "Stream crash did not leave the upstream slot behind the new checkpoint"
              #(= [applied-lsn "false"] (slot-query source slot "confirmed_flush_lsn,active::text")))
      (check! (seq (spool-files path)) "Child applied the large transaction without using its spool" {})
      (let [history (streamed-history path 800)]
        (check! (= 1 (count history)) "Streamed commit was not recorded atomically" {:history history})
        (check! (= history (streamed-history path 807)) "Rows from one streamed commit used different local transactions" {})
        (reset! subscriber (mirror/start! config))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (await-stream-rows! source sink subscriber))
        (check! (empty? (spool-files path)) "Restart retained a stale spool" {})
        (check! (= history (streamed-history path 800)) "Crash replay duplicated a streamed commit" {})))))

(defn run-streaming-probe! [source-url follower-url directory]
  (let [path (str directory "/stream-store")
        config {:source-url follower-url :publication "stream_pub" :store-path path :server {:port 0}
                :schema-refresh? true}
        subscriber (atom nil)]
    (with-open [source (DriverManager/getConnection source-url)
                tx-a (DriverManager/getConnection source-url)
                tx-b (DriverManager/getConnection source-url)]
      (try
        (sql! source "CREATE TYPE stream_status AS ENUM ('new layout', 'other')"
              "CREATE TABLE stream_items (id int PRIMARY KEY, payload text NOT NULL)"
              "CREATE ROLE mirror_stream_reader LOGIN REPLICATION"
              "ALTER ROLE mirror_stream_reader SET logical_decoding_work_mem='64kB'"
              "GRANT SELECT ON stream_items TO mirror_stream_reader"
              "GRANT EXECUTE ON FUNCTION pg_logical_emit_message(boolean, text, text, boolean) TO mirror_stream_reader"
              "CREATE PUBLICATION stream_pub FOR TABLE stream_items")
        (reset! subscriber (mirror/start! config))
        (let [{:keys [slot]} (await-state! @subscriber :following)]
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (interleave-streams! source tx-a tx-b sink subscriber path))
          (sql! tx-a "BEGIN")
          (stream-insert! tx-a 700)
          (flush-stream! tx-a)
          (await! "Uncommitted stream did not reach disk" #(seq (spool-files path)))
          (let [before (:applied-lsn (mirror/status @subscriber))]
            (sql! source "INSERT INTO stream_items VALUES (51,'acknowledged while a stream is open')")
            (await! "Publisher did not confirm the newer ordinary commit"
                    #(let [position (:applied-lsn (mirror/status @subscriber))]
                       (and (pos? (#'mirror/compare-lsn position before))
                            (= [position] (slot-query source slot "confirmed_flush_lsn")))))
            (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
              (check! (= [["51"]] (rows sink "SELECT id FROM stream_items WHERE id=51"))
                      "The acknowledged ordinary commit was missing" {})
              (check! (= [["0"]] (rows sink "SELECT count(*) FROM stream_items WHERE id BETWEEN 700 AND 707"))
                      "An open stream became visible before disconnect" {})))
          (mirror/stop! @subscriber)
          (check! (empty? (spool-files path)) "Stopping left uncommitted files behind" {})
          (sql! tx-a "COMMIT")
          (reset! subscriber (mirror/start! config))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (await-stream-rows! source sink subscriber)
            (refresh-with-streamed-catch-up! source sink subscriber))
          (retry-stream-after-quota! config source subscriber slot directory))
        {:status :passed :probe :streaming}
        (finally (when @subscriber (mirror/stop! @subscriber)))))))

(defn- lifecycle-rows [conn]
  (rows conn (str "SELECT id::text, value::text, score::text, note, pg_typeof(id)::text, "
                  "pg_typeof(value)::text, pg_typeof(score)::text FROM lifecycle_items ORDER BY note")))

(defn- lifecycle-catalog [conn]
  [(rows conn (str "SELECT typname, typtype, COALESCE(typbasetype, 0), pg_get_expr(typdefaultbin, 0) "
                   "FROM pg_type WHERE typname LIKE 'lifecycle_%' AND typtype IN ('e','d') ORDER BY typname"))
   (rows conn (str "SELECT t.typname, e.oid, e.enumlabel FROM pg_enum e JOIN pg_type t ON e.enumtypid=t.oid "
                   "WHERE t.typname LIKE 'lifecycle_%' ORDER BY t.typname, e.enumsortorder"))])

(defn- lifecycle-oids [conn]
  (into {} (rows conn "SELECT typname, oid FROM pg_type WHERE typname LIKE 'lifecycle_%' AND typtype IN ('e','d','c')")))

(defn- lifecycle-snapshot [conn]
  [(lifecycle-rows conn) (lifecycle-catalog conn) (lifecycle-oids conn)])

(defn- lifecycle-compare! [source sink subscriber]
  (let [expected [(lifecycle-rows source) (lifecycle-catalog source)]]
    (try
      (await! "Registered type rows or catalogs did not converge"
              #(let [status (mirror/status @subscriber)]
                 (check! (not (#{:paused :failed} (:state status))) "Mirror stopped during a registered type change" status)
                 (= expected [(lifecycle-rows sink) (lifecycle-catalog sink)])))
      (catch clojure.lang.ExceptionInfo error
        (throw (ex-info (ex-message error)
                        {:source expected :mirror [(lifecycle-rows sink) (lifecycle-catalog sink)]
                         :status (mirror/status @subscriber)} error))))))

(defn run-type-lifecycle-probe! [source-url follower-url directory]
  (let [path (str directory "/type-lifecycle-store")
        config {:source-url follower-url :publication "lifecycle_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        first-id #(inspect-db path (fn [db] (d/q '[:find ?e . :where [?e :lifecycle_items/note "first"]] db)))
        snapshot-time #(inspect-db path (fn [db] (str (.toInstant ^java.util.Date
                                                       (:db/txInstant (d/entity db (:max-tx db)))))))
        replacement ["BEGIN"
                     "ALTER TYPE lifecycle_state RENAME TO lifecycle_old_state"
                     "CREATE TYPE lifecycle_state AS ENUM ('alpha','off','new','on')"
                     (str "ALTER TABLE lifecycle_items ALTER COLUMN value TYPE lifecycle_state USING "
                          "(CASE WHEN value::text='new' THEN 'alpha' ELSE value::text END)::lifecycle_state")
                     "DROP TYPE lifecycle_old_state"
                     "ALTER DOMAIN lifecycle_credit RENAME TO lifecycle_old_credit"
                     "CREATE DOMAIN lifecycle_credit AS integer DEFAULT 9 CONSTRAINT lifecycle_positive CHECK (VALUE >= 0)"
                     "ALTER TABLE lifecycle_items ALTER COLUMN score TYPE lifecycle_credit USING score::int::lifecycle_credit"
                     "ALTER TABLE lifecycle_aux RENAME TO lifecycle_aux_archive"
                     "CREATE DOMAIN lifecycle_aux AS integer DEFAULT 11"
                     "ALTER TABLE lifecycle_items ADD COLUMN secondary lifecycle_aux DEFAULT 12"
                     "COMMIT"]]
    (with-open [source (DriverManager/getConnection source-url)]
      (try
        (sql! source "CREATE TYPE lifecycle_mood AS ENUM ('low','high')"
              "CREATE TYPE lifecycle_flag AS ENUM ('off','on')"
              "CREATE DOMAIN lifecycle_score AS smallint DEFAULT 3 CHECK (VALUE > 0)"
              (str "CREATE TABLE lifecycle_items (id lifecycle_mood PRIMARY KEY, value lifecycle_flag, "
                   "score lifecycle_score, note text)")
              "CREATE TABLE lifecycle_aux (id integer PRIMARY KEY)"
              "INSERT INTO lifecycle_items VALUES ('low','off',4,'first'),('high','on',6,'second')"
              "INSERT INTO lifecycle_aux VALUES (1)"
              "CREATE ROLE mirror_lifecycle_reader LOGIN REPLICATION"
              "GRANT SELECT ON lifecycle_items,lifecycle_aux TO mirror_lifecycle_reader"
              "GRANT EXECUTE ON FUNCTION pg_logical_emit_message(boolean,text,text,boolean) TO mirror_lifecycle_reader"
              "CREATE PUBLICATION lifecycle_pub FOR TABLE lifecycle_items,lifecycle_aux")
        (reset! subscriber (mirror/start! config))
        (let [{:keys [slot]} (await-state! @subscriber :following)
              row-id (first-id)]
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (let [before (lifecycle-snapshot sink)
                  before-at (snapshot-time)
                  old-oids (lifecycle-oids sink)]
              (lifecycle-compare! source sink subscriber)
              (sql! source "BEGIN"
                    "ALTER TYPE lifecycle_mood RENAME TO lifecycle_state"
                    "ALTER TYPE lifecycle_state RENAME VALUE 'low' TO 'idle'"
                    "CREATE TYPE lifecycle_mood AS ENUM ('off','on','new')"
                    "ALTER TABLE lifecycle_items ALTER COLUMN value TYPE lifecycle_mood USING value::text::lifecycle_mood"
                    "DROP TYPE lifecycle_flag"
                    "ALTER DOMAIN lifecycle_score RENAME TO lifecycle_credit"
                    "COMMIT"
                    "UPDATE lifecycle_items SET value='new' WHERE id='idle'")
              (lifecycle-compare! source sink subscriber)
              (let [renamed (lifecycle-oids sink)]
                (check! (= (old-oids "lifecycle_mood") (renamed "lifecycle_state")) "Enum rename allocated a new local OID" {})
                (check! (= (old-oids "lifecycle_score") (renamed "lifecycle_credit")) "Domain rename allocated a new local OID" {})
                (check! (and (not= (old-oids "lifecycle_mood") (renamed "lifecycle_mood"))
                             (not (contains? renamed "lifecycle_flag")))
                        "Reused and dropped type names have the wrong identities" renamed)
                (check! (and row-id (= row-id (first-id))) "Rename changed the matching row's identity" {})
                (sql! source "BEGIN" "ALTER TYPE lifecycle_mood RENAME TO lifecycle_swap"
                      "ALTER TYPE lifecycle_state RENAME TO lifecycle_mood"
                      "ALTER TYPE lifecycle_swap RENAME TO lifecycle_state" "COMMIT")
                (lifecycle-compare! source sink subscriber)
                (let [swapped (lifecycle-oids sink)
                      before-replacement (lifecycle-snapshot sink)
                      before-replacement-at (snapshot-time)
                      install-original @#'mirror/install-staged-tables!]
                  (check! (= [(renamed "lifecycle_mood") (renamed "lifecycle_state")]
                             [(swapped "lifecycle_state") (swapped "lifecycle_mood")])
                          "Type name swap reassigned or lost an OID" swapped)
                  (with-redefs-fn
                    {#'mirror/install-staged-tables! (fn [& args]
                                                       (apply install-original args)
                                                       (throw (ex-info "Interrupted registered type replacement" {})))}
                    (fn []
                      (apply sql! source replacement)
                      (check! (= "Interrupted registered type replacement"
                                 (get-in (await-state! @subscriber :paused) [:last-error :message]))
                              "Probe did not reach the replacement install" (mirror/status @subscriber))
                      (check! (= before-replacement (lifecycle-snapshot sink)) "Replacement rollback altered visible types" {})))
                  (mirror/stop! @subscriber)
                  (child-crash! (assoc config :probe-crash-mode :refresh) (str directory "/lifecycle-crash.log"))
                  (await! "Crashed type replacement kept the publisher slot active"
                          #(= ["false"] (slot-query source slot "active::text")))
                  (check! (pos? (#'mirror/compare-lsn (durable-position path)
                                                      (first (slot-query source slot "confirmed_flush_lsn"))))
                          "Publisher acknowledged type replacement before the crash" {})
                  (reset! subscriber (mirror/start! config))
                  (with-open [restarted (DriverManager/getConnection (sink-url @subscriber))]
                    (lifecycle-compare! source restarted subscriber)
                    (let [current (lifecycle-oids restarted)]
                      (check! (and (not= (swapped "lifecycle_state") (current "lifecycle_state"))
                                   (not= (swapped "lifecycle_credit") (current "lifecycle_credit"))
                                   (= (swapped "lifecycle_credit") (current "lifecycle_old_credit"))
                                   (= (swapped "lifecycle_aux") (current "lifecycle_aux_archive"))
                                   (not (contains? current "lifecycle_old_state")))
                              "Replacement types or renamed row type have the wrong OIDs" current))
                    (check! (= [["12" "lifecycle_aux"] ["12" "lifecycle_aux"]]
                               (rows restarted "SELECT secondary, pg_typeof(secondary)::text FROM lifecycle_items ORDER BY note"))
                            "Reusing a table row type's old name produced the wrong column" {})
                    (doseq [[at expected] [[before-at before] [before-replacement-at before-replacement]]]
                      (sql! restarted (str "SET datahike.as_of = " (literal at)))
                      (check! (= expected (lifecycle-snapshot restarted)) "Historical type identities or rows changed"
                              {:expected expected :actual (lifecycle-snapshot restarted)}))
                    (sql! restarted "RESET datahike.as_of")
                    (sql! source "ALTER DOMAIN lifecycle_old_credit RENAME TO lifecycle_unused")
                    (lifecycle-compare! source restarted subscriber)
                    (check! (= (swapped "lifecycle_credit") ((lifecycle-oids restarted) "lifecycle_unused"))
                            "An unreferenced rename did not preserve the tracked OID" {})
                    (sql! source "DROP DOMAIN lifecycle_unused"
                          "UPDATE lifecycle_items SET score=32768,value='off' WHERE id='idle'")
                    (lifecycle-compare! source restarted subscriber)
                    (check! (= row-id (first-id)) "Replacement changed the matching row's identity" {})
                    (check! (not (contains? (lifecycle-oids restarted) "lifecycle_unused")) "Dropped tracked type remained visible" {})))))))
        {:status :passed :probe :type-lifecycle}
        (finally (when @subscriber (mirror/stop! @subscriber)))))))

(defn run-index-identity-probe! [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-index-identity-" (make-array FileAttribute 0)))
        path (str directory "/store")
        config {:source-url source-url :publication "mirror_identity_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        slot (atom nil)
        native-slot "mirror_identity_native"
        account-query "SELECT id, external_id, account_code, label FROM indexed_accounts ORDER BY id, external_id"
        member-query "SELECT project_id, member_no, note FROM indexed_members ORDER BY project_id, member_no"
        catalog-query (str "SELECT ic.relname, i.indisprimary, i.indisunique FROM pg_index i "
                           "JOIN pg_class ic ON ic.oid = i.indexrelid "
                           "WHERE i.indrelid = '%s'::regclass ORDER BY ic.relname")
        manifest-index (fn [name]
                         (some #(when (= name (:name %)) (:identity-index %))
                               (get-in (durable-checkpoint path) [:manifest :tables])))]
    (prn {:artifacts directory})
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (let [agrees? (fn [sink]
                      (every? #(= (rows source %) (rows reference %) (rows sink %))
                              [account-query member-query]))]
        (try
          (doseq [conn [source reference]]
            (sql! conn
                  (str "CREATE TABLE indexed_accounts (id integer PRIMARY KEY, external_id text NOT NULL, "
                       "account_code integer NOT NULL, label text)")
                  "CREATE UNIQUE INDEX accounts_external ON indexed_accounts (external_id) INCLUDE (label)"
                  "ALTER TABLE indexed_accounts REPLICA IDENTITY USING INDEX accounts_external"
                  "CREATE TABLE indexed_members (project_id integer NOT NULL, member_no bigint NOT NULL, note text)"
                  "CREATE UNIQUE INDEX members_identity ON indexed_members (member_no, project_id) INCLUDE (note)"
                  "ALTER TABLE indexed_members REPLICA IDENTITY USING INDEX members_identity"))
          (sql! source
                "INSERT INTO indexed_accounts VALUES (1, 'first', 101, 'one'), (2, 'second', 102, 'two')"
                "INSERT INTO indexed_members VALUES (7, 101, NULL), (8, 101, 'other project'), (7, 102, 'obsolete')"
                "CREATE PUBLICATION mirror_identity_pub FOR TABLE indexed_accounts, indexed_members"
                "CREATE TABLE indexed_pattern (name text NOT NULL)"
                "CREATE UNIQUE INDEX indexed_pattern_key ON indexed_pattern (name text_pattern_ops)"
                "ALTER TABLE indexed_pattern REPLICA IDENTITY USING INDEX indexed_pattern_key"
                "CREATE PUBLICATION mirror_pattern_pub FOR TABLE indexed_pattern")
          (let [report (mirror/publication-report config)
                unsupported (mirror/publication-report (assoc config :publication "mirror_pattern_pub"))]
            (check! (:compatible? report) "The source indexes were not admitted" report)
            (check! (some #(= :unsupported-index (:detail %))
                          (get-in unsupported [:tables 0 :issues]))
                    "A non-default identity operator class was admitted" unsupported)
            (sql! source "DROP INDEX indexed_pattern_key")
            (let [missing (mirror/publication-report (assoc config :publication "mirror_pattern_pub"))]
              (check! (some #(= :missing-index (:detail %)) (get-in missing [:tables 0 :issues]))
                      "A table without its selected index was admitted" missing)))
          (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
          (sql! reference (str "CREATE SUBSCRIPTION mirror_identity_native CONNECTION "
                               (literal source-conninfo) " PUBLICATION mirror_identity_pub "
                               "WITH (create_slot=false, slot_name=" (literal native-slot) ")"))
          (reset! subscriber (mirror/start! config))
          (reset! slot (:slot (await-state! @subscriber :following)))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (await! "Snapshot disagrees with native replication" #(agrees? sink))
            (check! (= [["accounts_external" "f" "t"] ["indexed_accounts_pkey" "t" "t"]]
                       (rows sink (format catalog-query "indexed_accounts")))
                    "The selected identity index replaced the real primary key" {})
            (check! (= [["members_identity" "f" "t"]] (rows sink (format catalog-query "indexed_members")))
                    "The mirror fabricated a primary key for a keyless join table" {})
            (let [account-eid (rows sink "SELECT db_id FROM indexed_accounts WHERE id = 1")
                  member-eid (rows sink "SELECT db_id FROM indexed_members WHERE project_id = 7 AND member_no = 101")
                  prior-index (manifest-index "indexed_members")]
              (check! (= [2 1] (:attnums prior-index)) "Index INCLUDE column became part of the row key" prior-index)
              (sql! source "BEGIN"
                    "UPDATE indexed_accounts SET label = 'same locator' WHERE id = 1"
                    "UPDATE indexed_accounts SET id = 3, external_id = 'third' WHERE id = 1"
                    "DELETE FROM indexed_accounts WHERE id = 2"
                    "UPDATE indexed_members SET member_no = 103, note = 'changed' WHERE project_id = 7 AND member_no = 101"
                    "DELETE FROM indexed_members WHERE project_id = 7 AND member_no = 102"
                    "COMMIT")
              (await! "Indexed updates/deletes did not match the native subscriber" #(agrees? sink))
              (check! (= account-eid (rows sink "SELECT db_id FROM indexed_accounts WHERE id = 3"))
                      "Indexed update lost the entity behind the primary key" {})
              (doseq [conn [reference source]]
                (sql! conn "BEGIN"
                      "ALTER TABLE indexed_members REPLICA IDENTITY NOTHING"
                      "DROP INDEX members_identity"
                      "CREATE UNIQUE INDEX members_identity ON indexed_members (member_no, project_id)"
                      "ALTER TABLE indexed_members REPLICA IDENTITY USING INDEX members_identity"
                      "COMMIT"))
              (sql! source "UPDATE indexed_members SET note = 'replacement index' WHERE project_id = 7")
              (await! "Replacement index with identical wire flags did not refresh the manifest"
                      #(and (not= (:oid prior-index) (:oid (manifest-index "indexed_members"))) (agrees? sink)))
              (check! (= member-eid (rows sink "SELECT db_id FROM indexed_members WHERE project_id = 7 AND member_no = 103"))
                      "Keyless schema refresh changed the entity" {})
              (sql! reference "ALTER TABLE indexed_accounts DROP CONSTRAINT indexed_accounts_pkey")
              (sql! source "BEGIN"
                    "ALTER TABLE indexed_accounts DROP CONSTRAINT indexed_accounts_pkey"
                    "INSERT INTO indexed_accounts VALUES (3, 'fourth', 104, 'duplicate former key')"
                    "COMMIT")
              (await! "Primary key removal did not retain the indexed identity"
                      #(and (= [["accounts_external" "f" "t"]] (rows sink (format catalog-query "indexed_accounts")))
                            (agrees? sink)))
              (check! (= account-eid (rows sink "SELECT db_id FROM indexed_accounts WHERE external_id = 'third'"))
                      "Primary key removal changed the entity" {})
              (sql! source "UPDATE indexed_accounts SET id = 5 WHERE external_id = 'fourth'")
              (await! "The former primary key was not updated through the selected index" #(agrees? sink))
              (doseq [conn [reference source]]
                (sql! conn "BEGIN"
                      "ALTER TABLE indexed_accounts ADD PRIMARY KEY (id)"
                      "ALTER TABLE indexed_accounts REPLICA IDENTITY USING INDEX indexed_accounts_pkey"
                      "DROP INDEX accounts_external"
                      "COMMIT"))
              (sql! source "UPDATE indexed_accounts SET label = 'using the primary index' WHERE id = 3")
              (await! "Selecting the primary index did not retire the old identity index"
                      #(and (= "indexed_accounts_pkey" (:name (manifest-index "indexed_accounts")))
                            (= [["indexed_accounts_pkey" "t" "t"]] (rows sink (format catalog-query "indexed_accounts")))
                            (agrees? sink)))))
          (mirror/stop! @subscriber)
          (reset! subscriber (mirror/start! config))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (sql! source "BEGIN"
                  "UPDATE indexed_accounts SET id = 4 WHERE id = 3"
                  "DELETE FROM indexed_members WHERE project_id = 8 AND member_no = 101"
                  "COMMIT")
            (await! "Durable restart lost an indexed lookup" #(agrees? sink)))
          {:status :passed :artifacts directory}
          (finally
            (when @subscriber (mirror/stop! @subscriber))
            (when (= ["false"] (slot-query source @slot "active::text"))
              (sql! source (str "SELECT pg_drop_replication_slot(" (literal @slot) ")")))
            (sql! reference "DROP SUBSCRIPTION IF EXISTS mirror_identity_native")
            (when (= ["false"] (slot-query source native-slot "active::text"))
              (sql! source (str "SELECT pg_drop_replication_slot(" (literal native-slot) ")")))))))))

(defn run-array-refresh-probe! [source-url _reference-url _source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-array-refresh-" (make-array FileAttribute 0)))
        config {:source-url source-url :publication "array_refresh_pub" :store-path (str directory "/store")
                :schema-refresh? true :server {:port 0}}
        query "SELECT id, locator::text, amounts::text, member_ids::text FROM array_refresh"
        eid-query "SELECT db_id FROM array_refresh WHERE id = 1"]
    (with-open [source (DriverManager/getConnection source-url)]
      (sql! source
            (str "CREATE TABLE array_refresh (id int NOT NULL, locator numeric[] PRIMARY KEY, "
                 "obsolete text, amounts numeric(8,2)[])")
            "INSERT INTO array_refresh VALUES (1, '[-1:0]={1.00,NULL}', 'old', '{1234.56}')"
            "CREATE PUBLICATION array_refresh_pub FOR TABLE array_refresh")
      (let [subscriber (mirror/start! config)]
        (try
          (with-open [sink (DriverManager/getConnection (sink-url subscriber))]
            (let [eid (rows sink eid-query)]
              (sql! source "BEGIN"
                    (str "ALTER TABLE array_refresh DROP COLUMN obsolete, ADD COLUMN member_ids uuid[], "
                         "ALTER COLUMN amounts TYPE numeric(8,-2)[]")
                    (str "UPDATE array_refresh SET locator = '[-1:0]={1.0,NULL}', "
                         "member_ids = '{82d1ad1a-a1ea-4660-9f33-4dcbdaa8ad76}' WHERE id = 1")
                    "COMMIT")
              (let [expected (rows source query)]
                (await! "Array schema refresh did not match the publisher"
                        #(when (:last-schema-refresh (mirror/status subscriber))
                           (= expected (rows sink query)))))
              (check! (= eid (rows sink eid-query)) "An equivalent array key changed the entity" {})
              (sql! source "UPDATE array_refresh SET amounts = '{9399.5}' WHERE id = 1")
              (await! "Live apply lost the array key after schema refresh"
                      #(= (rows source query) (rows sink query)))))
          {:status :passed :artifacts directory}
          (finally
            (let [slot (:slot (mirror/status subscriber))]
              (mirror/stop! subscriber)
              (when (= ["false"] (slot-query source slot "active::text"))
                (sql! source (str "SELECT pg_drop_replication_slot(" (literal slot) ")"))))))))))

(defn- generated-column-catalog [conn]
  [(rows conn (str "SELECT a.attname, a.attgenerated, a.atthasdef, pg_get_expr(d.adbin, d.adrelid) "
                   "FROM pg_attribute a LEFT JOIN pg_attrdef d ON d.adrelid = a.attrelid AND d.adnum = a.attnum "
                   "WHERE a.attrelid = 'public.mirror_generated'::regclass "
                   "AND a.attname IN ('code', 'bonus') ORDER BY a.attname"))
   (rows conn (str "SELECT column_name, is_generated, generation_expression, column_default FROM information_schema.columns "
                   "WHERE table_schema = 'public' AND table_name = 'mirror_generated' "
                   "AND column_name IN ('code', 'bonus') ORDER BY column_name"))])

(defn- copy-text [^Connection conn command]
  (let [writer (java.io.StringWriter.)]
    (.copyOut (.getCopyAPI (.unwrap conn PGConnection)) ^String command writer)
    (str writer)))

(defn run-generated-probe! [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-generated-" (make-array FileAttribute 0)))
        path (str directory "/store")
        native-slot "mirror_generated_native"
        config {:source-url source-url :publication "mirror_generated_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        slot (atom nil)
        base-query "SELECT id, raw, code FROM public.mirror_generated ORDER BY id"
        expanded-query "SELECT id, raw, code, bonus FROM public.mirror_generated ORDER BY id"
        entity-query "SELECT db_id FROM public.mirror_generated WHERE id = 1"]
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (let [agrees? (fn [sink query] (= (rows source query) (rows reference query) (rows sink query)))
            metadata-matches? (fn [sink] (= (generated-column-catalog source) (generated-column-catalog sink)))
            cutover #(get-in (mirror/status @subscriber) [:last-schema-refresh :cutover])]
        (try
          (sql! source
                (str "CREATE FUNCTION public.derived_input(n int) RETURNS int IMMUTABLE LANGUAGE SQL "
                     "AS 'SELECT CASE WHEN n < 0 THEN NULL ELSE n * 17 END'")
                (str "CREATE TABLE public.mirror_generated (id integer PRIMARY KEY, raw integer NOT NULL, "
                     "code integer GENERATED ALWAYS AS (public.derived_input(raw)) STORED)")
                "INSERT INTO public.mirror_generated (id, raw) VALUES (1, 4), (2, -1)"
                (str "CREATE PUBLICATION mirror_generated_pub FOR TABLE public.mirror_generated "
                     "WITH (publish_generated_columns = stored)")
                "CREATE PUBLICATION mirror_omits_generated FOR TABLE public.mirror_generated"
                (str "CREATE TABLE public.mirror_virtual (id integer PRIMARY KEY, raw integer, "
                     "code integer GENERATED ALWAYS AS (raw * 2) VIRTUAL)")
                (str "CREATE PUBLICATION mirror_virtual_pub FOR TABLE public.mirror_virtual "
                     "WITH (publish_generated_columns = stored)")
                "SET search_path = pg_catalog")
          (check! (:compatible? (mirror/publication-report config))
                  "Stored generated column was not admitted" {})
          (doseq [[publication kind] [["mirror_omits_generated" "s"] ["mirror_virtual_pub" "v"]]]
            (let [report (mirror/publication-report (assoc config :publication publication))]
              (check! (and (not (:compatible? report))
                           (some #(and (= :generated-column (:reason %)) (= kind (:kind %)))
                                 (get-in report [:tables 0 :issues])))
                      "Unsupported generated column was admitted" report)))
          ;; PostgreSQL checks the replica identity against every publication
          ;; that publishes changes for the table.
          (sql! source "DROP PUBLICATION mirror_omits_generated")
          (sql! reference "CREATE TABLE public.mirror_generated (id integer PRIMARY KEY, raw integer NOT NULL, code integer)")
          (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
          (sql! reference (str "CREATE SUBSCRIPTION mirror_generated_native CONNECTION "
                               (literal source-conninfo) " PUBLICATION mirror_generated_pub "
                               "WITH (create_slot=false, slot_name=" (literal native-slot) ")"))
          (reset! subscriber (mirror/start! config))
          (reset! slot (:slot (await-state! @subscriber :following)))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (await! "Snapshot values differed from the native subscriber" #(agrees? sink base-query))
            (check! (metadata-matches? sink) "The generated expression metadata differed"
                    {:source (generated-column-catalog source) :mirror (generated-column-catalog sink)})
            (doseq [command ["COPY public.mirror_generated TO STDOUT"
                             "COPY (SELECT id, raw, code FROM public.mirror_generated ORDER BY id) TO STDOUT"]]
              (check! (= (sort (str/split-lines (copy-text source command)))
                         (sort (str/split-lines (copy-text sink command))))
                      "COPY treated a generated column differently from PostgreSQL" {:command command}))
            (let [entity (rows sink entity-query)]
              (sql! source "BEGIN"
                    "UPDATE public.mirror_generated SET raw = 6 WHERE id = 1"
                    "INSERT INTO public.mirror_generated (id, raw) VALUES (3, 10)"
                    "DELETE FROM public.mirror_generated WHERE id = 2"
                    "COMMIT")
              (await! "Live apply missed a generated value" #(agrees? sink base-query))
              (let [before (inspect-db path :max-tx)
                    old-catalog (generated-column-catalog sink)
                    old-rows (rows sink base-query)
                    prior-cutover (cutover)]
                (sql! source "BEGIN"
                      (str "ALTER TABLE public.mirror_generated ALTER COLUMN code "
                           "SET EXPRESSION AS (public.derived_input(raw) + 100)")
                      "UPDATE public.mirror_generated SET raw = raw + 1"
                      "COMMIT")
                (await! "The expression change did not reach a durable cutover"
                        #(and (not= prior-cutover (cutover))
                              (metadata-matches? sink) (agrees? sink base-query)))
                (sql! sink (str "SET datahike.as_of = " before))
                (check! (= [old-catalog old-rows] [(generated-column-catalog sink) (rows sink base-query)])
                        "History lost the old generated expression or values" {})
                (sql! sink "RESET datahike.as_of"))
              (let [prior-cutover (cutover)]
                (sql! reference "ALTER TABLE public.mirror_generated ADD COLUMN bonus integer")
                (sql! source "BEGIN"
                      "ALTER TABLE public.mirror_generated ALTER COLUMN code DROP EXPRESSION"
                      "ALTER TABLE public.mirror_generated ALTER COLUMN code SET DEFAULT 404"
                      (str "ALTER TABLE public.mirror_generated ADD COLUMN bonus integer "
                           "GENERATED ALWAYS AS (public.derived_input(raw + 2)) STORED NOT NULL")
                      "UPDATE public.mirror_generated SET code = 51"
                      "COMMIT")
                (await! "Column generation changes did not reach a durable cutover"
                        #(and (not= prior-cutover (cutover))
                              (metadata-matches? sink) (agrees? sink expanded-query))))
              (check! (= entity (rows sink entity-query)) "Generation changes replaced the entity" {})))
          (mirror/stop! @subscriber)
          (reset! subscriber (mirror/start! config))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (sql! source "INSERT INTO public.mirror_generated (id, raw) VALUES (5, 8)")
            (await! "Restart lost the new generated values or column default" #(agrees? sink expanded-query))
            (let [prior-cutover (cutover)
                  entity (rows sink entity-query)]
              (sql! reference "ALTER TABLE public.mirror_generated ALTER COLUMN bonus SET NOT NULL")
              (doseq [conn [reference source]]
                (sql! conn "BEGIN"
                      "CREATE UNIQUE INDEX mirror_generated_bonus ON public.mirror_generated (bonus)"
                      "ALTER TABLE public.mirror_generated REPLICA IDENTITY USING INDEX mirror_generated_bonus"
                      "COMMIT"))
              (await! "Generated replica identity did not reach a durable cutover"
                      #(and (not= prior-cutover (cutover))
                            (= "mirror_generated_bonus"
                               (get-in (durable-checkpoint path) [:manifest :tables 0 :identity-index :name]))))
              (sql! source "UPDATE public.mirror_generated SET raw = 14 WHERE id = 1"
                    "DELETE FROM public.mirror_generated WHERE id = 3")
              (await! "Apply missed the changing generated replica identity" #(agrees? sink expanded-query))
              (check! (= entity (rows sink entity-query)) "Generated key update replaced the entity" {}))
            (let [query "SELECT id, raw, doubled FROM public.mirror_generated_later ORDER BY id"]
              (sql! reference "CREATE TABLE public.mirror_generated_later (id integer PRIMARY KEY, raw integer, doubled integer)")
              (sql! source "BEGIN"
                    (str "CREATE TABLE public.mirror_generated_later (id integer PRIMARY KEY, raw integer, "
                         "doubled integer GENERATED ALWAYS AS (raw * 2) STORED)")
                    "INSERT INTO public.mirror_generated_later (id, raw) VALUES (1, 7), (2, 11)"
                    "ALTER PUBLICATION mirror_generated_pub ADD TABLE public.mirror_generated_later"
                    "COMMIT")
              (sql! reference "ALTER SUBSCRIPTION mirror_generated_native REFRESH PUBLICATION")
              (await! "An online table addition lost its generated columns"
                      #(and (some (fn [table] (= "mirror_generated_later" (:name table)))
                                  (get-in (durable-checkpoint path) [:manifest :tables]))
                            (agrees? sink query)))
              (sql! source "UPDATE public.mirror_generated_later SET raw = 12 WHERE id = 2")
              (await! "Live apply lost the new table's generated columns" #(agrees? sink query))
              (let [prior-cutover (cutover)
                    query "SELECT id, raw FROM public.mirror_generated_later ORDER BY id"]
                (doseq [conn [reference source]]
                  (sql! conn "ALTER TABLE public.mirror_generated_later DROP COLUMN doubled"))
                (sql! source "UPDATE public.mirror_generated_later SET raw = 13 WHERE id = 2")
                (await! "Dropping a generated column did not reach a durable cutover"
                        #(and (not= prior-cutover (cutover)) (agrees? sink query))))))
          {:status :passed :artifacts directory}
          (finally
            (when @subscriber (mirror/stop! @subscriber))
            (when (and @slot (= ["false"] (slot-query source @slot "active::text")))
              (sql! source (str "SELECT pg_drop_replication_slot(" (literal @slot) ")")))
            (sql! reference "DROP SUBSCRIPTION IF EXISTS mirror_generated_native")
            (when (= ["false"] (slot-query source native-slot "active::text"))
              (sql! source (str "SELECT pg_drop_replication_slot(" (literal native-slot) ")")))))))))

(defn run-keyless-full-probe! [source-url reference-url source-conninfo]
  (let [directory (str (Files/createTempDirectory "pgdh-mirror-keyless-full-" (make-array FileAttribute 0)))
        path (str directory "/store")
        config {:source-url source-url :publication "mirror_keyless_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        slot (atom nil)
        native-slot "mirror_keyless_native"
        fixture (str "CREATE TABLE loose_rows (tag text, amount numeric, doc jsonb, pad bpchar, "
                     "scores real[], observed timetz(6), payload text)")
        select-rows (fn [projection]
                      (str "SELECT tag, amount::text, doc::text, scores::text, observed::text, md5(payload), "
                           projection " FROM loose_rows ORDER BY tag NULLS FIRST, amount"))
        query (select-rows "pad::text")
        entity-query "SELECT db_id FROM loose_rows ORDER BY db_id"
        rewrite-query "SELECT payload::text FROM loose_rewrite ORDER BY payload"]
    (with-open [source (DriverManager/getConnection source-url)
                reference (DriverManager/getConnection reference-url)]
      (let [agrees? (fn [sink sql] (= (rows source sql) (rows reference sql) (rows sink sql)))
            cutover #(get-in (mirror/status @subscriber) [:last-schema-refresh :cutover])]
        (try
          (doseq [conn [source reference]]
            (sql! conn fixture
                  "CREATE TABLE loose_rewrite (payload integer)"
                  "CREATE TABLE loose_mode (tag text)"))
          (sql! source
                "ALTER TABLE loose_rows REPLICA IDENTITY FULL"
                "ALTER TABLE loose_rewrite REPLICA IDENTITY FULL"
                "ALTER TABLE loose_mode REPLICA IDENTITY FULL"
                (str "INSERT INTO loose_rows SELECT 'dup', 7.00, '{\"scale\":1.00}', 'p  '::bpchar, "
                     "'{-0,NaN,NULL}'::real[], '01:00:00.123456+05:45', j.payload FROM generate_series(1,2), "
                     "(SELECT string_agg(md5(i::text), '' ORDER BY i) AS payload FROM generate_series(1,4000) i) j")
                "INSERT INTO loose_rows (amount) VALUES (8), (8)"
                "INSERT INTO loose_rows (amount) VALUES (6), (6)"
                "INSERT INTO loose_rewrite VALUES (1), (1)"
                "INSERT INTO loose_mode VALUES ('one'), ('two')"
                "CREATE PUBLICATION mirror_keyless_pub FOR TABLE loose_rows, loose_rewrite, loose_mode"
                "CREATE TABLE loose_json (payload json)"
                "ALTER TABLE loose_json REPLICA IDENTITY FULL"
                "CREATE PUBLICATION mirror_keyless_json_pub FOR TABLE loose_json")
          (let [report (mirror/publication-report config)
                invalid (mirror/publication-report (assoc config :publication "mirror_keyless_json_pub"))]
            (check! (:compatible? report) "Keyless FULL publication was not admitted" report)
            (check! (some #(= :missing-equality-operator (:detail %)) (get-in invalid [:tables 0 :issues]))
                    "A keyless FULL column without equality was admitted" invalid))
          (when (= [["1"]] (rows source "SELECT 1 FROM pg_catalog.pg_collation WHERE collprovider = 'i' LIMIT 1"))
            (sql! source "CREATE COLLATION loose_icu (provider = icu, locale = 'und-u-ks-level2', deterministic = false)"
                  "CREATE TABLE loose_nondeterministic (payload text COLLATE loose_icu)"
                  "ALTER TABLE loose_nondeterministic REPLICA IDENTITY FULL"
                  "CREATE PUBLICATION mirror_keyless_icu_pub FOR TABLE loose_nondeterministic")
            (let [report (mirror/publication-report (assoc config :publication "mirror_keyless_icu_pub"))]
              (check! (some #(= :nondeterministic-collation (:detail %)) (get-in report [:tables 0 :issues]))
                      "A keyless FULL column with nondeterministic collation was admitted" report)))
          (sql! source (str "SELECT pg_create_logical_replication_slot(" (literal native-slot) ", 'pgoutput')"))
          (sql! reference (str "CREATE SUBSCRIPTION mirror_keyless_native CONNECTION "
                               (literal source-conninfo) " PUBLICATION mirror_keyless_pub "
                               "WITH (create_slot=false, slot_name=" (literal native-slot) ")"))
          (reset! subscriber (mirror/start! config))
          (reset! slot (:slot (await-state! @subscriber :following)))
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (await! "Keyless FULL snapshot lost duplicate rows" #(agrees? sink query))
            (let [[chosen other] (rows sink "SELECT db_id FROM loose_rows WHERE tag = 'dup' ORDER BY db_id")]
              (sql! source "BEGIN"
                    "UPDATE loose_rows SET amount = 9 WHERE ctid = (SELECT ctid FROM loose_rows WHERE tag = 'dup' LIMIT 1)"
                    "UPDATE loose_rows SET tag = 'changed' WHERE tag = 'dup' AND amount = 9"
                    "DELETE FROM loose_rows WHERE ctid = (SELECT ctid FROM loose_rows WHERE amount = 8 LIMIT 1)"
                    "COMMIT")
              (await! "Old tuples did not match one row at a time within the transaction" #(agrees? sink query))
              (check! (= [chosen other]
                         [(first (rows sink "SELECT db_id FROM loose_rows WHERE tag = 'changed'"))
                          (first (rows sink "SELECT db_id FROM loose_rows WHERE tag = 'dup'"))])
                      "Keyless apply did not choose the first matching entity" {}))
            (let [entities (rows sink entity-query)
                  before (inspect-db path :max-tx)
                  previous (rows sink query)
                  prior-cutover (cutover)
                  updated-query (select-rows "note")]
              (doseq [conn [reference source]]
                (sql! conn (str "ALTER TABLE loose_rows DROP COLUMN pad, ADD COLUMN note integer DEFAULT 42, "
                                "ALTER COLUMN amount TYPE numeric(12,3)")))
              (await! "Keyless refresh did not preserve row multiplicities"
                      #(and (not= prior-cutover (cutover)) (agrees? sink updated-query)))
              (check! (= entities (rows sink entity-query)) "Keyless refresh replaced matching entities" {})
              (sql! sink (str "SET datahike.as_of = " before))
              (check! (= previous (rows sink query)) "Historical keyless rows were lost at refresh" {})
              (sql! sink "RESET datahike.as_of"))
            (let [old-ids (set (rows sink "SELECT db_id FROM loose_rewrite ORDER BY db_id"))
                  prior-cutover (cutover)]
              (doseq [conn [reference source]]
                (sql! conn "ALTER TABLE loose_rewrite ALTER COLUMN payload TYPE text"))
              (await! "Rewriting the last comparable column lost duplicates"
                      #(and (not= prior-cutover (cutover)) (agrees? sink rewrite-query)))
              (check! (not-any? old-ids (rows sink "SELECT db_id FROM loose_rewrite"))
                      "A rewrite with no comparable columns reused old entity IDs" {}))
            (let [query "SELECT tag FROM loose_mode ORDER BY tag"
                  chosen (rows sink "SELECT db_id FROM loose_mode WHERE tag = 'one'")
                  prior-cutover (cutover)]
              (doseq [conn [reference source]]
                (sql! conn "BEGIN"
                      "ALTER TABLE loose_mode ADD PRIMARY KEY (tag)"
                      "ALTER TABLE loose_mode REPLICA IDENTITY DEFAULT"
                      "COMMIT"))
              (await! "Adding a primary key did not cut over"
                      #(and (not= prior-cutover (cutover)) (agrees? sink query)))
              (sql! source "UPDATE loose_mode SET tag = 'changed' WHERE tag = 'one'")
              (await! "Publisher key did not take over after refresh" #(agrees? sink query))
              (check! (= chosen (rows sink "SELECT db_id FROM loose_mode WHERE tag = 'changed'"))
                      "Adopting a key changed the entity" {})
              (let [prior-cutover (cutover)]
                (sql! reference "ALTER TABLE loose_mode DROP CONSTRAINT loose_mode_pkey")
                (sql! source "BEGIN"
                      "ALTER TABLE loose_mode DROP CONSTRAINT loose_mode_pkey"
                      "ALTER TABLE loose_mode REPLICA IDENTITY FULL"
                      "INSERT INTO loose_mode VALUES ('changed')"
                      "COMMIT")
                (await! "Dropping a primary key lost a row that now has a duplicate"
                        #(and (not= prior-cutover (cutover)) (agrees? sink query))))
              (check! (= chosen (take 1 (rows sink "SELECT db_id FROM loose_mode WHERE tag = 'changed' ORDER BY db_id")))
                      "Dropping the previous key changed a matched entity" {})))
          (mirror/stop! @subscriber)
          (let [confirmed (slot-query source @slot "confirmed_flush_lsn")
                checkpoint (durable-position path)]
            (sql! source "DELETE FROM loose_rewrite WHERE ctid = (SELECT ctid FROM loose_rewrite LIMIT 1)")
            (child-crash! config (str directory "/keyless-crash.log"))
            (await! "The crashed keyless apply left its slot active"
                    #(= (conj confirmed "false") (slot-query source @slot "confirmed_flush_lsn, active::text")))
            (check! (not= checkpoint (durable-position path))
                    "Keyless apply lost its checkpoint before feedback" {}))
          (reset! subscriber (mirror/start! config))
          (await-state! @subscriber :following)
          (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
            (sql! source "INSERT INTO loose_rewrite VALUES ('after restart')")
            (await! "Restart applied a duplicate-row deletion twice" #(agrees? sink rewrite-query)))
          {:status :passed :artifacts directory}
          (finally
            (when @subscriber (mirror/stop! @subscriber))
            (when (and @slot (= ["false"] (slot-query source @slot "active::text")))
              (sql! source (str "SELECT pg_drop_replication_slot(" (literal @slot) ")")))
            (sql! reference "DROP SUBSCRIPTION IF EXISTS mirror_keyless_native")
            (when (= ["false"] (slot-query source native-slot "active::text"))
              (sql! source (str "SELECT pg_drop_replication_slot(" (literal native-slot) ")")))))))))

(defn- run-checkpoint-upgrade-probe! [url directory]
  (let [path (str directory "/checkpoint-store")
        config {:source-url url :publication "checkpoint_pub" :store-path path :server {:port 0}}
        store #(let [conn (#'mirror/open-store path)]
                 (try (with-open [handler (pg/make-query-handler conn)] (% conn handler))
                      (finally (d/release conn))))]
    (with-open [source (DriverManager/getConnection url)]
      (sql! source "CREATE TABLE anchor (id integer PRIMARY KEY, note text)"
            "INSERT INTO anchor VALUES (1, 'original')"
            "CREATE TABLE public.checkpoint (id integer PRIMARY KEY, note text)"
            "INSERT INTO public.checkpoint VALUES (1, 'published')"
            "CREATE PUBLICATION checkpoint_pub FOR TABLE anchor")
      (let [subscriber (mirror/start! config)]
        (try (await-state! subscriber :following)
             (finally (mirror/stop! subscriber))))
      (let [{:keys [origin slot publication manifest end-lsn] :as saved} (durable-checkpoint path)]
        ;; Install the schema and row emitted by the earlier SQL checkpoint.
        (store (fn [_ handler]
                 (#'mirror/transaction!
                  handler
                  (fn []
                    (#'mirror/sink-execute!
                     handler
                     (str "CREATE TABLE pgdh_mirror.checkpoint (id integer PRIMARY KEY, "
                          "origin text NOT NULL, slot_name text NOT NULL, publication text NOT NULL, "
                          "manifest text, end_lsn text, source_xid text, source_commit_micros text)"))
                    (#'mirror/sink-prepared!
                     handler
                     (str "INSERT INTO pgdh_mirror.checkpoint "
                          "(id, origin, slot_name, publication, manifest, end_lsn) VALUES (1, ?, ?, ?, ?, ?)")
                     [(pr-str origin) slot publication (pr-str manifest) end-lsn])
                    (#'mirror/stage-data! handler
                                          (constantly [[:db/retractEntity [:pgdh-mirror-key :checkpoint]]]))))))
        (let [legacy-at (inspect-db path #(str (.toInstant ^java.util.Date
                                                (:db/txInstant (d/entity % (:max-tx %))))))]
          (with-open [_ (#'mirror/acquire-store-lock path)]
            (store #'mirror/upgrade-checkpoint!))
          (let [after (durable-checkpoint path)]
            (check! (= (select-keys saved [:slot :origin :end-lsn])
                       (select-keys after [:slot :origin :end-lsn]))
                    "Upgrading the legacy record changed the stored position" {}))
          (let [subscriber (mirror/start! config)]
            (try
              (check! (= slot (:slot (await-state! subscriber :following))) "Upgrade replaced the source slot" {})
              (with-open [sink (DriverManager/getConnection (sink-url subscriber))]
                (check! (= [["original"]] (rows sink "SELECT note FROM anchor")) "Upgrade changed table contents" {})
                (sql! sink (str "SET datahike.as_of = " (literal legacy-at)))
                (doseq [query ["SELECT slot_name FROM public.checkpoint"
                               "SELECT slot_name FROM pgdh_mirror.checkpoint"
                               "COPY public.checkpoint (slot_name) TO STDOUT"]]
                  (let [state (try (sql! sink query) nil (catch SQLException error (.getSQLState error)))]
                    (check! (= "42P01" state) "The legacy checkpoint is visible through SQL history" {:query query})))
                (sql! sink "RESET datahike.as_of")
                (sql! source "ALTER PUBLICATION checkpoint_pub ADD TABLE public.checkpoint")
                (await! "Published checkpoint table did not replace the legacy SQL name"
                        #(try (= [["published"]] (rows sink "SELECT note FROM public.checkpoint"))
                              (catch SQLException _ false)))
                (sql! source "UPDATE public.checkpoint SET note = 'after update'")
                (await! "WAL for the published checkpoint table did not converge"
                        #(= [["after update"]] (rows sink "SELECT note FROM public.checkpoint"))))
              {:status :passed :store path}
              (finally (mirror/stop! subscriber)))))))))

(defn run-namespace-probe! [source-url]
  (let [directory (.toFile (Files/createTempDirectory "pgdh-mirror-scoped-store-" (make-array FileAttribute 0)))
        path (str (io/file directory "store"))
        config {:source-url source-url :publication "scoped_pub" :store-path path
                :schema-refresh? true :server {:port 0}}
        subscriber (atom nil)
        slot (atom nil)
        frozen (atom nil)
        left "billing.events"
        right "\"Archive Data\".events"
        moved "shipping.arrivals"
        public-table "public.external_claim"
        table-sql (fn [table]
                    (str "CREATE TABLE " table
                         " (id bigint, external_id int NOT NULL, label text, CONSTRAINT event_pk PRIMARY KEY (id))"))
        ids (fn [conn table]
              (first (rows conn (str "SELECT '"
                                     table "'::regclass::oid, c.reltype FROM pg_class c WHERE c.oid = '"
                                     table "'::regclass::oid"))))
        contents (fn [conn table]
                   (rows conn (str "SELECT id, external_id, label FROM " table " ORDER BY id")))
        compare-tables! (fn [source sink tables]
                          (await! "Scoped publication rows diverged"
                                  #(let [status (mirror/status @subscriber)]
                                     (check! (not (#{:failed :paused} (:state status))) "Mirror paused" status)
                                     (every? (fn [table] (= (contents source table) (contents sink table))) tables))))
        index-rows (fn [conn]
                     (rows conn (str "SELECT n.nspname, i.relname, x.indisprimary, x.indkey::text FROM pg_index x "
                                     "JOIN pg_class i ON i.oid = x.indexrelid JOIN pg_namespace n ON n.oid = i.relnamespace "
                                     "WHERE n.nspname IN ('billing', 'Archive Data', 'shipping') ORDER BY 1, 2")))
        locators (fn [conn table]
                   (rows conn (str "SELECT db_id FROM " table " ORDER BY id")))]
    (with-open [source (DriverManager/getConnection source-url)]
      (try
        (sql! source "CREATE SCHEMA billing" "CREATE SCHEMA \"Archive Data\""
              (table-sql left) (table-sql right)
              "CREATE TABLE public.events_external (id int PRIMARY KEY, external_id int, label text)"
              "CREATE TABLE public.invoices (id int PRIMARY KEY, external_id int, label text)"
              "INSERT INTO public.events_external VALUES (1, 1, 'public alias')"
              "INSERT INTO public.invoices VALUES (2, 2, 'prior storage name reused by an index')"
              "CREATE UNIQUE INDEX events_external ON billing.events (external_id)"
              "CREATE UNIQUE INDEX events_external ON \"Archive Data\".events (external_id)"
              "ALTER TABLE billing.events REPLICA IDENTITY USING INDEX events_external"
              "ALTER TABLE \"Archive Data\".events REPLICA IDENTITY USING INDEX events_external"
              "INSERT INTO billing.events VALUES (1, 11, 'billing')"
              "INSERT INTO \"Archive Data\".events VALUES (1, 11, 'archive'), (2, 22, 'to delete')"
              (str "CREATE PUBLICATION scoped_pub FOR TABLE billing.events, "
                   "\"Archive Data\".events, public.events_external, public.invoices"))
        (let [admission (mirror/publication-report config)]
          (check! (:compatible? admission) "Scoped publication was rejected" admission))
        (reset! subscriber (mirror/start! config))
        (reset! slot (:slot (await-state! @subscriber :following)))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (compare-tables! source sink [left right])
          (check! (= (index-rows source) (index-rows sink)) "Scoped indexes differ after snapshot" {})
          (sql! source "BEGIN"
                "UPDATE billing.events SET id = 3, label = 'billing WAL' WHERE external_id = 11"
                "UPDATE \"Archive Data\".events SET label = 'archive WAL' WHERE external_id = 11"
                "DELETE FROM \"Archive Data\".events WHERE external_id = 22"
                "COMMIT")
          (compare-tables! source sink [left right])
          (let [local-ids (ids sink left)
                billing-rows (contents sink left)
                billing-locators (locators sink left)
                before (inspect-db path (fn [db] (.toInstant ^java.util.Date
                                                  (:db/txInstant (d/entity db (:max-tx db))))))]
            (check! (= (parse-long (first (ids source left)))
                       (inspect-db path (fn [db]
                                          (d/q '[:find ?source .
                                                 :in $ ?local
                                                 :where [?table :datahike.pg.object/oid ?local]
                                                 [?table :datahike.pg.mirror/publisher-relation-oid ?source]]
                                               db (parse-long (first local-ids))))))
                    "Publisher relation OID was not retained" {})
            (sql! source "BEGIN"
                  "CREATE SCHEMA shipping"
                  "ALTER TABLE billing.events SET SCHEMA shipping"
                  "ALTER TABLE shipping.events RENAME TO arrivals"
                  "ALTER TABLE public.events_external RENAME TO external_claim"
                  "ALTER TABLE public.invoices RENAME CONSTRAINT invoices_pkey TO events_external"
                  "ALTER TABLE shipping.arrivals ADD COLUMN received text DEFAULT 'pending'"
                  "UPDATE shipping.arrivals SET label = 'arrived' WHERE id = 3"
                  "COMMIT")
            (await! "Schema move did not reach the mirror"
                    #(let [status (mirror/status @subscriber)]
                       (check! (not (#{:failed :paused} (:state status))) "Schema move paused the mirror" status)
                       (= [["t"]] (rows sink "SELECT to_regclass('shipping.arrivals') IS NOT NULL"))))
            (compare-tables! source sink [moved right public-table "public.invoices"])
            (check! (= [["i"]] (rows sink "SELECT relkind FROM pg_class WHERE oid = 'public.events_external'::regclass"))
                    "An archived storage name hid a new index" {})
            (check! (= local-ids (ids sink moved)) "Schema move changed the table or row type OID" {})
            (check! (= billing-locators (locators sink moved)) "Schema move changed record identity" {})
            (check! (= [["pending"]] (rows sink "SELECT received FROM shipping.arrivals")) "Column addition lost old rows" {})
            (with-open [historical (DriverManager/getConnection (sink-url @subscriber))]
              (sql! historical (str "SET datahike.as_of = " (literal before)))
              (check! (= billing-rows (contents historical left)) "Historical lookup lost the earlier schema" {})))
          (sql! source "BEGIN"
                "ALTER TABLE \"Archive Data\".events REPLICA IDENTITY DEFAULT"
                "DROP INDEX \"Archive Data\".events_external"
                "CREATE UNIQUE INDEX events_external ON \"Archive Data\".events (external_id, id)"
                "ALTER TABLE \"Archive Data\".events REPLICA IDENTITY USING INDEX events_external"
                "UPDATE \"Archive Data\".events SET label = 'replacement index' WHERE external_id = 11"
                (table-sql left)
                "INSERT INTO billing.events VALUES (4, 44, 'reused name')"
                "ALTER PUBLICATION scoped_pub ADD TABLE billing.events"
                "COMMIT")
          (await! "Publisher table addition did not reach the mirror"
                  #(let [status (mirror/status @subscriber)]
                     (check! (not (#{:failed :paused} (:state status))) "Table addition paused the mirror" status)
                     (= [["t"]] (rows sink "SELECT to_regclass('billing.events') IS NOT NULL"))))
          (compare-tables! source sink [left moved right])
          (check! (= (index-rows source) (index-rows sink)) "Identity index replacement affected a sibling" {})
          (let [left-ids (ids sink left)
                right-ids (ids sink right)]
            (sql! source "BEGIN" "CREATE SCHEMA transit"
                  "ALTER TABLE billing.events SET SCHEMA transit"
                  "ALTER TABLE \"Archive Data\".events SET SCHEMA billing"
                  "ALTER TABLE transit.events SET SCHEMA \"Archive Data\""
                  "DROP SCHEMA transit" "COMMIT")
            (compare-tables! source sink [left right])
            (check! (= [right-ids left-ids] [(ids sink left) (ids sink right)])
                    "Swapping same-named tables changed local identities" {})
            (check! (= (index-rows source) (index-rows sink)) "Swapping tables changed scoped indexes" {})))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (sql! source "BEGIN"
                (str "CREATE TABLE shipping.parcels (id int PRIMARY KEY, external_id int, label text) "
                     "PARTITION BY RANGE (id)")
                "CREATE TABLE billing.parcel_range PARTITION OF shipping.parcels FOR VALUES FROM (0) TO (100)"
                (str "CREATE TABLE \"Archive Data\".parcel_range PARTITION OF shipping.parcels "
                     "FOR VALUES FROM (100) TO (200)")
                "INSERT INTO shipping.parcels VALUES (1, 11, 'partition left'), (101, 22, 'partition right')"
                "ALTER PUBLICATION scoped_pub ADD TABLE shipping.parcels"
                "COMMIT")
          (await! "The parent partition did not reach the mirror"
                  #(let [status (mirror/status @subscriber)]
                     (check! (not (#{:failed :paused} (:state status))) "Named partitions paused the mirror" status)
                     (= [["t"]] (rows sink "SELECT to_regclass('shipping.parcels') IS NOT NULL"))))
          (compare-tables! source sink ["shipping.parcels" "billing.parcel_range" "\"Archive Data\".parcel_range"])
          (let [partition-catalog
                (str "SELECT n.nspname, c.relname FROM pg_inherits i JOIN pg_class c ON c.oid = i.inhrelid "
                     "JOIN pg_namespace n ON n.oid = c.relnamespace "
                     "WHERE i.inhparent = 'shipping.parcels'::regclass ORDER BY 1,2")]
            (check! (= (rows source partition-catalog) (rows sink partition-catalog))
                    "Cross-schema partition attachments differ" {}))
          (check! (= [] (rows sink "SELECT id FROM ONLY shipping.parcels")) "ONLY expanded the named parent" {})
          (reset! frozen (contents sink right))
          (let [removed-id (parse-long (first (ids source right)))]
            (sql! source "ALTER PUBLICATION scoped_pub DROP TABLE \"Archive Data\".events")
            (await! "Removed table was not recorded as unpublished"
                    #(contains? (get-in (durable-checkpoint path) [:manifest :unpublished-tables]) removed-id)))
          (check! (= @frozen (contents sink right)) "An active namesake hid the unpublished table" {}))
        (mirror/stop! @subscriber)
        (reset! subscriber (mirror/start! config))
        (with-open [sink (DriverManager/getConnection (sink-url @subscriber))]
          (check! (= @slot (:slot (mirror/status @subscriber))) "Restart replaced the replication slot" {})
          (sql! source "BEGIN"
                "TRUNCATE billing.events"
                "UPDATE \"Archive Data\".events SET label = 'unpublished after restart' WHERE id = 4"
                "UPDATE public.external_claim SET label = 'after restart' WHERE id = 1"
                "UPDATE shipping.arrivals SET label = 'also after restart' WHERE id = 3"
                "UPDATE shipping.parcels SET id = 102, label = 'moved after restart' WHERE id = 1"
                "COMMIT")
          (compare-tables! source sink
                           [left moved public-table "shipping.parcels" "billing.parcel_range" "\"Archive Data\".parcel_range"])
          (check! (= @frozen (contents sink right)) "Restart lost the frozen unpublished relation" {}))
        {:status :passed :schemas 4}
        (finally
          (when @subscriber (mirror/stop! @subscriber))
          (when (and @slot (= ["false"] (slot-query source @slot "active::text")))
            (sql! source (str "SELECT pg_drop_replication_slot(" (literal @slot) ")")))
          (doseq [file (reverse (file-seq directory))]
            (io/delete-file file)))))))

(defn -main [& [argument mode]]
  (cond
    (= "--failover" argument)
    (prn (run-failover-probe! {:pg-bin (System/getenv "PG_MIRROR_PG_BIN")
                               :fixture-dir (System/getenv "PG_MIRROR_FAILOVER_DIR")
                               :primary-dir (System/getenv "PG_MIRROR_PRIMARY_DIR")
                               :standby-dir (System/getenv "PG_MIRROR_STANDBY_DIR")
                               :primary-url (System/getenv "PG_MIRROR_PRIMARY_URL")
                               :standby-url (System/getenv "PG_MIRROR_STANDBY_URL")
                               :source-url (System/getenv "PG_MIRROR_SOURCE_URL") :mode mode}))
    (= "--checkpoint-upgrade" argument)
    (prn (run-checkpoint-upgrade-probe! (System/getenv "PG_MIRROR_RECOVERY_ADMIN_URL")
                                        (System/getenv "PG_MIRROR_RECOVERY_DIR")))
    (= "--slot-recovery" argument)
    (prn (run-slot-recovery-probe! (System/getenv "PG_MIRROR_RECOVERY_ADMIN_URL")
                                   (System/getenv "PG_MIRROR_SOURCE_URL")
                                   (System/getenv "PG_MIRROR_RECOVERY_DIR")))
    (= "--streaming" argument)
    (prn (run-streaming-probe! (System/getenv "PG_MIRROR_RECOVERY_ADMIN_URL")
                               (System/getenv "PG_MIRROR_SOURCE_URL")
                               (System/getenv "PG_MIRROR_RECOVERY_DIR")))
    (= "--type-lifecycle" argument)
    (prn (run-type-lifecycle-probe! (System/getenv "PG_MIRROR_RECOVERY_ADMIN_URL")
                                    (System/getenv "PG_MIRROR_SOURCE_URL")
                                    (System/getenv "PG_MIRROR_RECOVERY_DIR")))
    (= "--namespaces" argument)
    (let [source (System/getenv "PG_MIRROR_SOURCE_URL")]
      (check! (seq source) "Set PG_MIRROR_SOURCE_URL to a disposable publisher database" {})
      (prn (run-namespace-probe! source)))
    (= "--crash-worker" argument)
    (run-crash-worker! (edn/read (PushbackReader. *in*)))
    :else
    (let [source (System/getenv "PG_MIRROR_SOURCE_URL")
          reference (System/getenv "PG_MIRROR_REFERENCE_URL")
          conninfo (System/getenv "PG_MIRROR_SOURCE_CONNINFO")]
      (check! (every? seq [source reference conninfo])
              "Set PG_MIRROR_SOURCE_URL, PG_MIRROR_REFERENCE_URL and PG_MIRROR_SOURCE_CONNINFO" {})
      (prn ((case argument
              "--partitions" run-partition-probe!
              "--root-publication" run-root-publication-probe!
              "--type-refresh" run-type-refresh-probe!
              "--primary-key-refresh" run-primary-key-probe!
              "--index-identity" run-index-identity-probe!
              "--array-refresh" run-array-refresh-probe!
              "--generated-columns" run-generated-probe!
              "--full-identity" #(run-primary-key-probe! %1 %2 %3 true)
              "--keyless-full" run-keyless-full-probe!
              "--full-identity-recovery" #(run-probe! %1 %2 %3 true)
              "--schema-refresh" run-schema-refresh-probe!
              "--refresh" run-refresh-probe!
              run-probe!)
            source reference conninfo)))))
