(ns datahike.pg.main
  "Standalone CLI entrypoint for pg-datahike. Two subcommands:

     serve  (default) — boot a pgwire server. See `serve --help`.
     dump   — dump a pg-datahike database as portable PostgreSQL
              SQL. Output replays into either pg-datahike or real
              PostgreSQL via psql. See `dump --help`.

   Usage:
     java -jar pg-datahike-VERSION-standalone.jar [serve] [SERVE-OPTS]
     java -jar pg-datahike-VERSION-standalone.jar dump [DUMP-OPTS]

   The default subcommand is `serve` (so the existing
   `java -jar pg-datahike.jar --port 5432` invocation keeps working
   without explicitly typing `serve`)."
  (:require [clojure.core.async :refer [<!!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.dump :as dump]
            [datahike.pg.sql.database :as database]
            [konserve.core :as k]
            [konserve.store :as ks])
  (:import [java.io File])
  (:gen-class))

(set! *warn-on-reflection* true)

;; ----------------------------------------------------------------------------
;; Argument parsing — handcrafted to keep the dep-tree minimal in the uberjar
;; ----------------------------------------------------------------------------

(def ^:private default-data-dir
  (or (System/getenv "PG_DATAHIKE_DATA_DIR")
      (str (System/getProperty "user.home") "/.local/share/pg-datahike")))

(defn- parse-serve-args [args]
  (loop [args args
         out  {:port 5432
               :host "127.0.0.1"
               :data-dir default-data-dir
               :memory? false
               :create-database? true
               :history? false
               :dbs []}]
    (if (empty? args)
      (cond-> out
        (empty? (:dbs out)) (assoc :dbs ["datahike"]))
      (let [[a & rest-args] args]
        (case a
          "--port"
          (recur (rest rest-args) (assoc out :port (Integer/parseInt (first rest-args))))
          "--host"
          (recur (rest rest-args) (assoc out :host (first rest-args)))
          "--data-dir"
          (recur (rest rest-args) (assoc out :data-dir (first rest-args)))
          "--memory"
          (recur rest-args (assoc out :memory? true))
          "--db"
          (recur (rest rest-args) (update out :dbs conj (first rest-args)))
          "--no-create-database"
          (recur rest-args (assoc out :create-database? false))
          "--history"
          (recur rest-args (assoc out :history? true))
          "--help"
          (recur rest-args (assoc out :help? true))
          "--version"
          (recur rest-args (assoc out :version? true))
          (do (println (str "Unknown option: " a))
              (System/exit 2)))))))

(defn- parse-dump-args [args]
  (loop [args args
         out  {:data-dir default-data-dir
               :db "datahike"
               :config-file nil
               :format :inserts
               :sections #{:schema :data}
               :out-file nil
               :exclude-tables #{}}]
    (if (empty? args)
      out
      (let [[a & rest-args] args]
        (case a
          "--data-dir"
          (recur (rest rest-args) (assoc out :data-dir (first rest-args)))
          "--db"
          (recur (rest rest-args) (assoc out :db (first rest-args)))
          "--config"
          (recur (rest rest-args) (assoc out :config-file (first rest-args)))
          "--copy"
          (recur rest-args (assoc out :format :copy))
          "--inserts"
          (recur rest-args (assoc out :format :inserts))
          "--schema-only"
          (recur rest-args (assoc out :sections #{:schema}))
          "--data-only"
          (recur rest-args (assoc out :sections #{:data}))
          "--out"
          (recur (rest rest-args) (assoc out :out-file (first rest-args)))
          "--exclude-table"
          (recur (rest rest-args) (update out :exclude-tables conj (first rest-args)))
          "--help"
          (recur rest-args (assoc out :help? true))
          (do (binding [*out* *err*]
                (println (str "Unknown dump option: " a)))
              (System/exit 2)))))))

;; ----------------------------------------------------------------------------
;; Help / version
;; ----------------------------------------------------------------------------

(defn- read-version
  "Read the version string written into pg-datahike.version inside the
   uberjar's classpath. Falls back to 'dev' when running from source."
  []
  (or (some-> (Thread/currentThread)
              .getContextClassLoader
              (.getResourceAsStream "pg-datahike.version")
              slurp
              str/trim)
      "dev"))

(def ^:private serve-help-text
  "java -jar pg-datahike-VERSION-standalone.jar [serve] [OPTIONS]

  --port N             TCP port              (default 5432)
  --host ADDR          Bind address          (default 127.0.0.1)
  --data-dir DIR       Storage location for file-backed dbs
                       (default $PG_DATAHIKE_DATA_DIR or
                        ~/.local/share/pg-datahike)
  --memory             Use in-memory backend (no persistence; overrides
                       --data-dir for the template)
  --db NAME            Pre-create / pin a database named NAME.
                       Repeatable. Defaults to a single 'datahike' if
                       no --db is given.
  --no-create-database Disable SQL CREATE/DROP DATABASE (clients see
                       0A000 feature_not_supported). Default: enabled.
  --history            Keep transaction history in pre-created
                       databases. Default: off (5x storage hit).
  --help               Show this and exit
  --version            Print version and exit")

(def ^:private dump-help-text
  "java -jar pg-datahike-VERSION-standalone.jar dump [OPTIONS]

   Dump a pg-datahike database to portable PostgreSQL SQL on stdout
   (or to --out FILE). The output replays into either pg-datahike
   (via psql) or real PostgreSQL.

  --data-dir DIR       Storage location for file-backed stores
                       (default $PG_DATAHIKE_DATA_DIR or
                        ~/.local/share/pg-datahike). The store is
                       opened at DIR/{db}.
  --db NAME            Database to dump. Default: 'datahike'.
                       Used to construct the file path under --data-dir.
  --config FILE        EDN file with the full datahike config map.
                       Use this for non-file backends (:jdbc, :s3, :redis,
                       :lmdb, …) — anything konserve supports. Overrides
                       --data-dir/--db. The :store :id is auto-discovered
                       from the persisted :db config if absent.
  --inserts            Emit data as INSERT INTO ... VALUES (default).
                       Universally re-loadable, including by tier-1
                       of pg-datahike's pg_dump-import path.
  --copy               Emit data as COPY ... FROM stdin (more compact;
                       requires the loader to drive the COPY-IN
                       sub-protocol — psql does this transparently).
  --schema-only        Skip data; emit only DDL.
  --data-only          Skip DDL; emit only INSERT/COPY rows.
  --exclude-table NAME Skip a table by namespace. Repeatable.
  --out FILE           Write to FILE instead of stdout.
  --help               Show this and exit")

(defn- print-help []
  (println serve-help-text)
  (println)
  (println dump-help-text))

;; ----------------------------------------------------------------------------
;; Template + pre-create
;; ----------------------------------------------------------------------------

(defn- build-template [{:keys [memory? data-dir history?]}]
  (cond-> {:schema-flexibility :write
           ;; SQL migration order is CREATE TABLE -> COPY -> ADD PRIMARY KEY:
           ;; enabling uniqueness on populated attributes needs datahike's
           ;; index-backfill migration (opt-in upstream; ignored by datahike
           ;; versions without it).
           :allow-index-backfill? true
           :keep-history? history?}
    memory?
    (assoc :store {:backend :memory})

    (not memory?)
    (assoc :store {:backend :file
                   :path (str data-dir "/{{name}}")})))

(defn- pre-create-dbs!
  "Materialise the --db NAME entries by calling the template hook
   (same code path SQL `CREATE DATABASE` uses). Returns the
   {name → conn} registry seed."
  [hook db-names]
  (into {}
        (map (fn [name]
               [name (hook name [])])
             db-names)))

;; ----------------------------------------------------------------------------
;; Subcommand: serve
;; ----------------------------------------------------------------------------

(defn- run-serve [args]
  (let [opts (parse-serve-args args)]
    (cond
      (:help? opts)
      (do (println serve-help-text) (System/exit 0))

      (:version? opts)
      (do (println (read-version)) (System/exit 0))

      :else
      (let [{:keys [port host data-dir memory? create-database? dbs]} opts
            template (build-template opts)
            on-create (when create-database?
                        (database/db-from-template template))
            on-delete (when create-database?
                        (database/db-delete-from-template template))]
        (when-not memory?
          (.mkdirs (File. ^String data-dir)))
        (let [registry (pre-create-dbs! (database/db-from-template template) dbs)
              srv (pg/start-server registry
                                   (cond-> {:port port :host host}
                                     on-create (assoc :on-create-database on-create)
                                     on-delete (assoc :on-delete-database on-delete)))]
          (println)
          (println (str "pg-datahike " (read-version) " ready on " host ":" port))
          (println (str "  backend:  " (if memory? "memory (ephemeral)"
                                           (str "file (" data-dir ")"))))
          (println (str "  history:  " (if (:history? opts) "on" "off")))
          (println (str "  CREATE DATABASE:  " (if create-database? "enabled" "disabled")))
          (println (str "  databases: " (vec (keys registry))))
          (println)
          (println (str "Connect with: psql -h " (if (= host "0.0.0.0") "localhost" host)
                        " -p " port " -U datahike " (first dbs)))
          (println "Press Ctrl+C to stop.")
          (.addShutdownHook (Runtime/getRuntime)
                            (Thread. ^Runnable (fn [] (pg/stop-server srv))))
          @(promise))))))

;; ----------------------------------------------------------------------------
;; Subcommand: dump
;; ----------------------------------------------------------------------------

(defn- discover-store-id
  "Connect to a konserve store via the backend-agnostic
   `konserve.store/connect-store` and read the `:db` branch's
   persisted config to recover the datahike store-id. Returns nil
   if the store can't be opened or has no `:db` key.

   This sidesteps having to know the UUID up-front — datahike
   always persists its full config (including the store-id) under
   the `:db` key, so we can peek at any konserve-supported backend
   before `d/connect` (which demands the matching UUID).

   konserve currently validates `:id` is present in the config (it
   is intended as a global store identifier), so we inject a
   placeholder UUID for the discovery handshake. For backends that
   key on path/host/bucket (`:file`, `:jdbc`, `:s3`, `:redis`, …) the
   placeholder is metadata-only and doesn't affect the lookup. The
   `:memory` backend keys on `:id`, so it can't be discovered this
   way — but ephemeral stores don't survive across JVMs anyway.

   `store-config` is the `:store` map (`{:backend :file :path …}`,
   `{:backend :jdbc :host …}`, etc.) — anything `connect-store`
   dispatches on."
  [store-config]
  (let [probe-cfg (cond-> store-config
                    (nil? (:id store-config))
                    (assoc :id (java.util.UUID/randomUUID)))
        store (try (<!! (ks/connect-store probe-cfg))
                   (catch Throwable _ nil))]
    (when store
      (try
        (let [db (<!! (k/get store :db))]
          (get-in db [:config :store :id]))
        (finally
          (try (<!! (ks/release-store probe-cfg store)) (catch Throwable _)))))))

(defn- read-config-file
  "Read an EDN datahike config map from PATH."
  [^String path]
  (try
    (edn/read-string (slurp path))
    (catch Throwable e
      (binding [*out* *err*]
        (println (str "ERROR: cannot read config file " path ": " (.getMessage e))))
      (System/exit 1))))

(defn- run-dump [args]
  (let [opts (parse-dump-args args)]
    (when (:help? opts)
      (println dump-help-text)
      (System/exit 0))
    (let [{:keys [data-dir db config-file format sections out-file exclude-tables]} opts
          base-cfg (if config-file
                     (read-config-file config-file)
                     {:store {:backend :file :path (str data-dir "/" db)}
                      :schema-flexibility :write
                      :allow-index-backfill? true
                      :keep-history? false})
          store-id (or (get-in base-cfg [:store :id])
                       (discover-store-id (:store base-cfg)))
          _ (when (nil? store-id)
              (binding [*out* *err*]
                (println (str "ERROR: no datahike database found at "
                              (pr-str (:store base-cfg))
                              " (missing :db branch — wrong --data-dir/--db or --config?)")))
              (System/exit 1))
          cfg (assoc-in base-cfg [:store :id] store-id)
          conn (try
                 (d/connect cfg)
                 (catch Throwable e
                   (binding [*out* *err*]
                     (println (str "ERROR: cannot open database "
                                   (pr-str (:store cfg)) ": " (.getMessage e))))
                   (System/exit 1)))
          dump-opts (cond-> {:format format :sections sections}
                      (seq exclude-tables) (assoc :exclude-tables exclude-tables))
          writer (if out-file
                   (java.io.PrintWriter. (java.io.FileWriter. ^String out-file))
                   nil)]
      (try
        (if writer
          (do (doseq [stmt (dump/dump conn dump-opts)]
                (.println ^java.io.PrintWriter writer ^String stmt))
              (.close ^java.io.PrintWriter writer)
              (binding [*out* *err*]
                (println (str "[dump] wrote " out-file))))
          (doseq [stmt (dump/dump conn dump-opts)]
            (println stmt)))
        (finally
          (try (d/release conn) (catch Throwable _)))))))

;; ----------------------------------------------------------------------------
;; Top-level dispatch
;; ----------------------------------------------------------------------------

(defn -main
  "Subcommand dispatch. First positional arg is the subcommand
   (serve / dump). Everything else passes to the subcommand parser.
   For backward-compat, an unrecognised first arg is treated as a
   serve flag (so existing `--port 5432` invocations still work)."
  [& args]
  (let [[head & rest-args] args]
    (cond
      (or (nil? head) (= "--help" head))
      (do (print-help) (System/exit 0))

      (= "--version" head)
      (do (println (read-version)) (System/exit 0))

      (= "serve" head)
      (run-serve rest-args)

      (= "dump" head)
      (run-dump rest-args)

      ;; Unrecognised first arg — assume legacy `serve` invocation.
      :else
      (run-serve args))))
