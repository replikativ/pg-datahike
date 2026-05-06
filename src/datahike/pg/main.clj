(ns datahike.pg.main
  "Standalone CLI entrypoint for pg-datahike — bundled into the uberjar
   produced by `clojure -T:build uber` so a single
   `java -jar pg-datahike.jar` brings up a PostgreSQL-wire server backed
   by Datahike, with no Clojure setup or library dependencies on the
   client side.

   Usage:
     java -jar pg-datahike-VERSION-standalone.jar [OPTIONS]

   Options:
     --port N             TCP port              (default 5432)
     --host ADDR          Bind address          (default 127.0.0.1)
     --data-dir DIR       Storage location for file-backed dbs
                          (default \\$PG_DATAHIKE_DATA_DIR or
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
     --version            Print version and exit

   Examples:
     java -jar pg-datahike.jar
        # 5432 / 127.0.0.1 / file-backed at ~/.local/share/pg-datahike
        # one database 'datahike'; CREATE DATABASE works.

     java -jar pg-datahike.jar --memory --db prod --db staging
        # Two pre-created in-memory databases.

     java -jar pg-datahike.jar --port 15432 --data-dir /var/lib/dh \\
                               --db app
        # File-backed in /var/lib/dh, single db 'app'."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.database :as database])
  (:import [java.io File])
  (:gen-class))

(set! *warn-on-reflection* true)

;; ----------------------------------------------------------------------------
;; Argument parsing — handcrafted to keep the dep-tree minimal in the uberjar
;; ----------------------------------------------------------------------------

(def ^:private default-data-dir
  (or (System/getenv "PG_DATAHIKE_DATA_DIR")
      (str (System/getProperty "user.home") "/.local/share/pg-datahike")))

(defn- parse-args [args]
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

(def ^:private help-text
  "java -jar pg-datahike-VERSION-standalone.jar [OPTIONS]

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

(defn- print-help []
  (println help-text))

;; ----------------------------------------------------------------------------
;; Template + pre-create
;; ----------------------------------------------------------------------------

(defn- build-template [{:keys [memory? data-dir history?]}]
  (cond-> {:schema-flexibility :write
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
;; Entry point
;; ----------------------------------------------------------------------------

(defn -main
  "Boot a pg-datahike server from the command line. See ns docstring."
  [& args]
  (let [opts (parse-args args)]
    (cond
      (:help? opts)
      (do (print-help) (System/exit 0))

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
