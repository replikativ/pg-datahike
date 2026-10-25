(ns postgres-reference
  "Shared reference selection and provenance for the regression harness."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.security MessageDigest]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent TimeUnit]))

(def repo-root
  (.getCanonicalPath (io/file (.getParent (io/file *file*)) "../../..")))

(def input-paths
  ["src/test/regress/parallel_schedule" "src/test/regress/sql"
   "src/test/regress/expected" "src/test/regress/data"
   "src/test/regress/input" "src/test/regress/output"
   "src/test/regress/resultmap"])

(defn fail! [message]
  (throw (ex-info message {:exit 2})))

(defn canonical [path] (.getCanonicalPath (io/file path)))

(def ^:private client-timeout-ms 15000)
(def ^:private termination-grace-ms 2000)
(def ^:private client-statement-timeout-ms 10000)

(defn- owned-descendants [process]
  (with-open [stream (.descendants (.toHandle process))]
    (vec (iterator-seq (.iterator stream)))))

(defn wait-for-owned!
  "Wait finitely, then terminate only this process and its descendants."
  [process timeout-ms]
  (if (.waitFor process (long timeout-ms) TimeUnit/MILLISECONDS)
    (.exitValue process)
    (let [children (owned-descendants process)]
      (doseq [child children] (.destroy child))
      (.destroy process)
      (.waitFor process termination-grace-ms TimeUnit/MILLISECONDS)
      (doseq [child (distinct (concat children (owned-descendants process)))]
        (.destroyForcibly child))
      (.destroyForcibly process)
      (.waitFor process termination-grace-ms TimeUnit/MILLISECONDS)
      124)))

(defn- timed-command! [args extra-env]
  ;; Files avoid waiting indefinitely for a descendant to close an inherited pipe.
  (let [out (java.io.File/createTempFile "pgdh-reference-out-" ".log")
        err (java.io.File/createTempFile "pgdh-reference-err-" ".log")]
    (try
      (let [builder (ProcessBuilder. ^java.util.List (vec args))
            _ (.putAll (.environment builder) extra-env)
            _ (.redirectOutput builder out)
            _ (.redirectError builder err)
            exit (wait-for-owned! (.start builder) client-timeout-ms)]
        (when-not (zero? exit)
          (fail! (if (= 124 exit)
                   "reference client timed out and was terminated"
                   (str "reference client failed: " (str/trim (slurp err))))))
        (str/trim (slurp out)))
      (finally (.delete out) (.delete err)))))

(defn psql! [cfg args]
  (let [options (str (when-let [existing (not-empty (System/getenv "PGOPTIONS"))]
                       (str existing " "))
                     "-c statement_timeout=" client-statement-timeout-ms)]
    (timed-command!
     (into [(str (io/file (:bindir cfg) "psql"))] args)
     {"PGCONNECT_TIMEOUT" (or (System/getenv "PGCONNECT_TIMEOUT") "10")
      "PGOPTIONS" options})))

(defn command!
  ([args] (command! args nil))
  ([args extra-env]
   (let [args (cond-> (vec args)
                extra-env (into [:env (merge (into {} (System/getenv)) extra-env)]))
         {:keys [exit out err]} (apply sh/sh args)]
     (when-not (zero? exit)
       (fail! (str "command failed: " (first args) ": " (str/trim err))))
     (str/trim out))))

(defn config
  ([root env]
   (let [root (canonical root)
         here (io/file root "test/integration/postgres-regress")
         catalog (edn/read-string (slurp (io/file here "profiles.edn")))
         profile (or (not-empty (get env "PG_REGRESS_PROFILE")) (:default catalog))
         selected (get-in catalog [:profiles profile])
         _ (when-not selected (fail! (str "unknown PG_REGRESS_PROFILE: " profile)))
         source (some-> (not-empty (get env "POSTGRES_SOURCE")) canonical)
         regress (some-> (not-empty (get env "PG_REGRESS_SRC")) canonical)
         from-regress (when regress (canonical (io/file regress "../../..")))
         _ (when (and source from-regress (not= source from-regress))
             (fail! "POSTGRES_SOURCE and PG_REGRESS_SRC identify different checkouts"))
         source (or source from-regress
                    (canonical (io/file root ".internal" (str "postgres-" (:postgres-ref selected)))))
         _ (when (and regress (not= regress (canonical (io/file source "src/test/regress"))))
             (fail! "PG_REGRESS_SRC must name SOURCE/src/test/regress"))
         major (:major selected)
         _ (when-not (= (str major) (second (re-find #"^REL_(\d+)_" (:postgres-ref selected))))
             (fail! "reference profile major and release ref disagree"))
         _ (when (and (get env "PG_REGRESS_MAJOR")
                      (not= (str major) (get env "PG_REGRESS_MAJOR")))
             (fail! "PG_REGRESS_MAJOR contradicts PG_REGRESS_PROFILE"))
         bindir (or (not-empty (get env "PG_REGRESS_BINDIR"))
                    (str "/usr/lib/postgresql/" major "/bin"))
         target {:host (or (not-empty (get env "PG_REGRESS_HOST")) "127.0.0.1")
                 :port (or (not-empty (get env "PG_REGRESS_PORT")) "15432")
                 :user (or (not-empty (get env "PG_REGRESS_USER")) "datahike")
                 :database (or (not-empty (get env "PG_REGRESS_DB")) "datahike")}
         _ (doseq [value [(:host target) (:database target)]]
             (when (or (str/includes? value "://") (str/includes? value "="))
               (fail! (str "use a plain PG_REGRESS_HOST and database name; "
                           "connection URIs/conninfo are not accepted and credentials belong in libpq environment variables"))))]
     (merge selected
            {:profile profile :repo-root root :source source
             :regress-root (canonical (io/file source "src/test/regress"))
             :bindir (canonical bindir)
             :pg-regress (canonical (or (not-empty (get env "PG_REGRESS_BIN"))
                                        (str "/usr/lib/postgresql/" major
                                             "/lib/pgxs/src/test/regress/pg_regress")))
             :artifact-root (canonical (io/file root ".internal/pg-regress" (:postgres-ref selected)))
             :allow-unpinned? (= "1" (get env "PG_REGRESS_ALLOW_UNPINNED"))
             :target target}
            (into {} (for [k [:campaign :scope :ledger :agreement :inventory]
                           :when (get selected k)]
                       [k (canonical (io/file here (get selected k)))])))))
  ([] (config repo-root (System/getenv))))

(defn sha256-bytes [bytes]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (MessageDigest/getInstance "SHA-256") bytes))))

(defn file-sha256 [path]
  (with-open [in (io/input-stream path)]
    (let [digest (MessageDigest/getInstance "SHA-256")
          buf (byte-array 65536)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n) (.update digest buf 0 n) (recur))))
      (apply str (map #(format "%02x" (bit-and 255 %)) (.digest digest))))))

(defn runner-state [root]
  (let [files (->> (file-seq (io/file root "test/integration/postgres-regress"))
                   (filter #(.isFile %))
                   (map #(str (.relativize (.toPath (io/file root)) (.toPath %))))
                   (concat ["bb.edn" ".circleci/config.yml"]) sort)
        hashes (into (sorted-map) (for [path files] [path (file-sha256 (io/file root path))]))]
    {:commit (command! ["git" "-C" root "rev-parse" "HEAD"])
     :dirty? (not (str/blank? (command! ["git" "-C" root "status" "--porcelain"
                                         "--untracked-files=normal"])))
     :harness-sha256 (sha256-bytes (.getBytes (pr-str hashes) "UTF-8"))}))

(defn limits [env]
  {:pg-regress-timeout (not-empty (get env "PG_REGRESS_TIMEOUT"))
   :statement-timeout (or (not-empty (get env "PG_REGRESS_STATEMENT_TIMEOUT")) "10s")
   :agreement-file-timeout (get env "AGREEMENT_FILE_TIMEOUT" "900")
   :agreement-comparison-timeout (get env "AGREEMENT_COMPARE_TIMEOUT" "30m")
   :agreement-comparison-heap "512m"
   :client-process-timeout-ms client-timeout-ms
   :client-statement-timeout-ms client-statement-timeout-ms
   :termination-grace-ms termination-grace-ms})

(defn- options-hash [text]
  {:present? (some? text)
   :sha256 (when text (sha256-bytes (.getBytes text "UTF-8")))})

(defn options-evidence [env]
  (let [caller (get env "PGOPTIONS")
        statement (or (not-empty (get env "PG_REGRESS_STATEMENT_TIMEOUT")) "10s")
        effective (if (str/blank? statement) caller
                      (str (when (seq caller) (str caller " "))
                           "-c statement_timeout=" statement))]
    {:caller (options-hash caller) :pg-regress-startup (options-hash effective)}))

(defn validate-source! [{:keys [source postgres-ref allow-unpinned?] :as cfg}]
  (doseq [path ["src/test/regress/parallel_schedule" "src/test/regress/sql" "src/test/regress/expected"]]
    (when-not (.exists (io/file source path))
      (fail! (str "missing pinned PostgreSQL inputs at " source
                  "; run bb pg-regress-setup for profile " (:profile cfg)))))
  (let [head (command! ["git" "-C" source "rev-parse" "HEAD"])
        expected (sh/sh "git" "-C" source "rev-parse" (str "refs/tags/" postgres-ref "^{commit}"))
        dirty (command! (into ["git" "-C" source "status" "--porcelain"
                               "--untracked-files=all" "--ignored=matching" "--"] input-paths))
        authoritative? (and (zero? (:exit expected))
                            (= head (str/trim (:out expected))) (str/blank? dirty))
        _ (when (and (not authoritative?) (not allow-unpinned?))
            (fail! (str "reference must be clean " postgres-ref
                        " (including SQL, expected outputs and fixtures): " source)))
        files (->> input-paths
                   (map #(io/file source %))
                   (filter #(.exists %))
                   (mapcat file-seq)
                   (filter #(.isFile %))
                   (map #(str (.relativize (.toPath (io/file source)) (.toPath %))))
                   distinct sort)
        hashes (into (sorted-map) (for [p files] [p (file-sha256 (io/file source p))]))]
    {:postgres-ref postgres-ref :commit head :authoritative? authoritative?
     :inputs-sha256 (sha256-bytes (.getBytes (pr-str hashes) "UTF-8"))
     :input-files (count hashes)}))

(defn validate-tools! [{:keys [major bindir pg-regress]}]
  (into {}
        (for [[kind executable] [[:psql (str (io/file bindir "psql"))]
                                 [:pg-regress pg-regress]]]
          (do
            (when-not (.canExecute (io/file executable))
              (fail! (str "reference tool is not executable: " executable)))
            (let [version (timed-command! [executable "--version"] {})
                  actual (some-> (re-find #"PostgreSQL\)?\s+(\d+)" version) second parse-long)]
              (when-not (= major actual)
                (fail! (str "reference tool major contradicts profile: " executable " -> " version)))
              [kind {:path executable :version version :sha256 (file-sha256 executable)}])))))

(defn require-campaign! [cfg]
  (when-not (:campaign cfg)
    (fail! (str "profile " (:profile cfg) " supports named discovery runs only; "
                "its audited campaign/agreement inventory is not yet admitted")))
  (let [campaign (edn/read-string (slurp (:campaign cfg)))
        ledger (edn/read-string (slurp (:ledger cfg)))]
    (when-not (and (= (:major cfg) (:postgres-major campaign))
                   (= (:postgres-ref cfg) (:postgres-ref campaign))
                   (= (:postgres-ref cfg) (:postgres-ref ledger))
                   (or (nil? (:postgres-major ledger))
                       (= (:major cfg) (:postgres-major ledger))))
      (fail! "campaign/ledger identity disagrees with selected reference profile"))
    (when (:inventory cfg)
      (load-file (str (io/file (:repo-root cfg) "test/integration/postgres-regress/inventory.clj")))
      ((ns-resolve 'postgres-inventory 'validate!) cfg campaign
                                                   (edn/read-string (slurp (:scope cfg))) ledger))))

(defn validate-baseline! [cfg manifest]
  (when-not (and (= (:postgres-ref cfg) (:postgres-ref manifest))
                 (or (nil? (:profile manifest)) (= (:profile cfg) (:profile manifest))))
    (fail! (str "baseline reference " (:postgres-ref manifest)
                " does not match selected reference " (:postgres-ref cfg))))
  (when (or (:source-commit manifest) (:inputs-sha256 manifest))
    (let [source (validate-source! cfg)]
      (when-not (and (or (nil? (:source-commit manifest))
                         (= (:source-commit manifest) (:commit source)))
                     (or (nil? (:inputs-sha256 manifest))
                         (= (:inputs-sha256 manifest) (:inputs-sha256 source))))
        (fail! "baseline source provenance differs from selected reference inputs")))))

(defn require-authoritative! [cfg]
  (let [source (validate-source! cfg)]
    (when-not (:authoritative? source)
      (fail! "baseline and strict operations require clean pinned reference inputs"))
    source))

(defn new-output [cfg env]
  (or (not-empty (get env "PG_REGRESS_OUTPUT"))
      (str (io/file (:artifact-root cfg) (str (System/currentTimeMillis) "-" (UUID/randomUUID))))))

(defn observe-target! [{:keys [target] :as cfg}]
  (let [{:keys [host port user database]} target
        answer (psql! cfg ["-X" "-A" "-t" "-F" "\t"
                           "--host" host "--port" port "--username" user "--dbname" database
                           "--set=ON_ERROR_STOP=1" "--command"
                           (str "SELECT current_setting('server_version'), "
                                "current_setting('server_version_num'), current_setting('TimeZone'), "
                                "current_setting('server_encoding')")])
        parts (str/split answer #"\t" -1)]
    (when-not (= 4 (count parts))
      (fail! "target did not return server version, timezone and encoding"))
    (assoc (zipmap [:server-version :server-version-num :time-zone :server-encoding] parts)
           :observation :preflight-session)))

(defn write-edn! [path value]
  (spit path (str (pr-str value) "\n")))

(defn validate-tests! [cfg tests]
  (when-not (seq tests) (fail! "select at least one named regression test"))
  (doseq [name tests]
    (when-not (and (re-matches #"[a-zA-Z0-9_][a-zA-Z0-9_.-]*" name)
                   (.isFile (io/file (:regress-root cfg) "sql" (str name ".sql"))))
      (fail! (str "unknown or invalid named regression test: " name)))))

(defn begin-output! [cfg output tests]
  (validate-tests! cfg tests)
  (let [directory (io/file output)
        source (validate-source! cfg)
        tools (validate-tools! cfg)]
    (when (and (.exists directory)
               (or (not (.isDirectory directory)) (seq (.listFiles directory))))
      (fail! (str "output directory must be empty: " output)))
    (.mkdirs directory)
    (let [receipt {:format 1 :run-id (str (UUID/randomUUID))
                   :profile (:profile cfg) :reference source :tools tools
                   :output-directory (canonical output) :tests (vec tests)
                   :status :running :started-at (str (Instant/now))
                   :target (assoc (observe-target! cfg) :code-revision :unverified
                                  :endpoint (select-keys (:target cfg) [:host :port :database]))
                   :runner (runner-state (:repo-root cfg))
                   :limits (limits (System/getenv))
                   :pgoptions (options-evidence (System/getenv))
                   :caller-environment (select-keys (into {} (System/getenv))
                                                    ["LANG" "LC_ALL" "LC_CTYPE" "LC_COLLATE"
                                                     "LC_MESSAGES" "LC_MONETARY" "LC_NUMERIC"
                                                     "LC_TIME" "PGTZ" "PGDATESTYLE" "PGCLIENTENCODING"])}]
      (write-edn! (io/file directory "reference.edn") receipt)
      receipt)))

(defn- process-failure-output? [path]
  (when (.isFile (io/file path))
    (with-open [reader (io/reader path)]
      (boolean
       (some #(re-find #"(?i)test process (?:exited|was terminated)|server closed the connection|connection to server was lost" %)
             (line-seq reader))))))

(defn finish-output! [cfg output status]
  (let [path (io/file output "reference.edn")
        receipt (edn/read-string (slurp path))
        reference (validate-source! cfg)
        tests (:tests receipt)
        _ (when-not (= :running (:status receipt))
            (fail! "output receipt has already finished; start a fresh run"))
        _ (when-not (and (= (:profile cfg) (:profile receipt))
                         (= (:postgres-ref cfg) (get-in receipt [:reference :postgres-ref])))
            (fail! "output receipt uses a different reference profile"))
        present? (every? #(.isFile (io/file output "results" (str % ".out"))) tests)
        process-failed? (some process-failure-output?
                              (cons (io/file output "pg_regress.log")
                                    (map #(io/file output "results" (str % ".out")) tests)))
        complete? (boolean (and (#{0 1} status) present? (not process-failed?)
                                (= reference (:reference receipt))
                                (= (validate-tools! cfg) (:tools receipt))))]
    (when-not (= (canonical output) (:output-directory receipt))
      (fail! "output receipt belongs to another directory"))
    (write-edn! path
                (assoc receipt :status (if complete? :complete :incomplete)
                       :pg-regress-exit status :finished-at (str (Instant/now))
                       :outputs (when complete?
                                  (into {} (for [name tests]
                                             [name (file-sha256 (io/file output "results" (str name ".out")))])))))
    complete?))

(defn collect-result! [cfg run-directory name collected-directory]
  (let [receipt (edn/read-string (slurp (io/file run-directory "reference.edn")))]
    (when-not (and (= :complete (:status receipt))
                   (= (:profile cfg) (:profile receipt))
                   (= (validate-source! cfg) (:reference receipt))
                   (= (canonical run-directory) (:output-directory receipt))
                   (contains? (:outputs receipt) name))
      (fail! (str "incomplete or foreign output receipt: " run-directory)))
    (let [source (io/file run-directory "results" (str name ".out"))]
      (when-not (= (get-in receipt [:outputs name]) (file-sha256 source))
        (fail! (str "output changed after the run completed: " source)))
      (let [destination (io/file collected-directory (str name ".out"))]
        (when (.exists destination)
          (fail! (str "refusing to overwrite a collected result: " destination)))
        (io/copy source destination)))))

(defn finish-collection! [cfg directory names complete?]
  (write-edn! (io/file directory "collection.edn")
              {:format 1 :profile (:profile cfg) :reference (validate-source! cfg)
               :output-directory (canonical directory)
               :status (if complete? :complete :incomplete)
               :expected-comparison :canonical-expected-lines
               :outputs (into {} (for [name names
                                       :let [file (io/file directory (str name ".out"))]
                                       :when (.isFile file)]
                                   [name (file-sha256 file)]))}))

(defn validate-collection! [cfg directory]
  (let [receipt (edn/read-string (slurp (io/file directory "collection.edn")))]
    (when-not (and (= :complete (:status receipt))
                   (= (:profile cfg) (:profile receipt))
                   (= (validate-source! cfg) (:reference receipt))
                   (= (canonical directory) (:output-directory receipt))
                   (seq (:outputs receipt)))
      (fail! "results are incomplete or belong to another reference/run"))
    (doseq [[name hash] (:outputs receipt)]
      (when-not (and (re-matches #"[a-zA-Z0-9_][a-zA-Z0-9_.-]*" name)
                     (= hash (file-sha256 (io/file directory (str name ".out")))))
        (fail! (str "collected output changed: " name))))
    receipt))

(defn emit-env! [cfg]
  (if (some #(= "1" (System/getenv %)) ["PG_REGRESS_STRICT" "PG_REGRESS_API_STRICT"])
    (require-authoritative! cfg)
    (validate-source! cfg))
  (validate-tools! cfg)
  (doseq [[k v] {"POSTGRES_SOURCE" (:source cfg) "PG_REGRESS_SRC" (:regress-root cfg)
                 "PG_REGRESS_MAJOR" (str (:major cfg)) "PG_REGRESS_BINDIR" (:bindir cfg)
                 "PG_REGRESS_BIN" (:pg-regress cfg)}]
    (print (str k \u0000 v \u0000))))

(defn setup! [cfg target]
  (let [target (or target (:source cfg))]
    (if (.exists (io/file target))
      (require-authoritative! (assoc cfg :source (canonical target)))
      (do (io/make-parents (io/file target ".keep"))
          (command! ["git" "clone" "--depth" "1" "--branch" (:postgres-ref cfg)
                     "https://github.com/postgres/postgres.git" target])
          (require-authoritative! (assoc cfg :source (canonical target)))))
    (println "PostgreSQL source ready:" (canonical target) (:postgres-ref cfg))))

(defn -main [& [operation & args]]
  (try
    (let [cfg (config)]
      (case operation
        "env" (emit-env! cfg)
        "check-tests" (validate-tests! cfg args)
        "output" (println (new-output cfg (System/getenv)))
        "begin" (begin-output! cfg (first args) (rest args))
        "finish" (when-not (finish-output! cfg (first args) (parse-long (second args)))
                   (System/exit 2))
        "setup" (setup! cfg (first args))
        "psql" (println (psql! cfg args))
        (fail! (str "unknown reference command: " operation))))
    (catch Exception e
      (binding [*out* *err*] (println "PostgreSQL reference error:" (.getMessage e)))
      (System/exit 2))))

(when (and (System/getProperty "babashka.file")
           (= (canonical *file*) (canonical (System/getProperty "babashka.file"))))
  (apply -main *command-line-args*))
