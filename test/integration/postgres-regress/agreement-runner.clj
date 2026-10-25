#!/usr/bin/env bb
(ns postgres-agreement-runner
  "Collect one versioned measurement against an externally managed endpoint."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.lang ProcessBuilder]
           [java.util UUID]))

(load-file "test/integration/postgres-regress/reference.clj")
(alias 'reference 'postgres-reference)

(def prerequisites
  {"geometry" ["lseg" "line" "box" "path" "polygon" "circle"]
   "horology" ["date" "time" "timetz" "timestamp" "timestamptz" "interval"]
   "join" ["create_table" "create_misc"]
   "select_views" ["create_view"]
   "numeric_big" ["numeric"]})

(defn duration-ms
  ([text] (duration-ms "AGREEMENT_FILE_TIMEOUT" text))
  ([setting text]
   (let [[_ number unit] (re-matches #"([0-9]+(?:\.[0-9]+)?)([smhd]?)" text)
         value (when number (* (bigdec number) (get {"" 1000 "s" 1000 "m" 60000
                                                     "h" 3600000 "d" 86400000} unit)))]
     (when-not (and value (pos? value) (< value (- Long/MAX_VALUE 10000)))
       (reference/fail! (str setting " must be a positive finite duration, e.g. 900 or 10m")))
     (max 1 (long value)))))

(defn execute! [args env log timeout-ms]
  (let [builder (ProcessBuilder. ^java.util.List (vec args))]
    (.putAll (.environment builder) env)
    (.redirectErrorStream builder true)
    (.redirectOutput builder (io/file log))
    (reference/wait-for-owned! (.start builder) timeout-ms)))

(defn collect! [mode env]
  (when-not (#{"measure" "gate" "probe"} mode)
    (reference/fail! "usage: agreement.sh [measure|gate|probe]"))
  (let [_ (duration-ms "AGREEMENT_COMPARE_TIMEOUT" (get env "AGREEMENT_COMPARE_TIMEOUT" "30m"))
        cfg (reference/config reference/repo-root env)
        _ (reference/require-campaign! cfg)
        _ (reference/require-authoritative! cfg)
        _ (reference/validate-tools! cfg)]
    (load-file "test/integration/postgres-regress/agreement.clj")
    (let [in-scope ((ns-resolve 'agreement 'application-facing))
          asked (some-> (not-empty (get env "AGREEMENT_FILES")) str/trim
                        (str/split #"[,\s]+") (->> (remove str/blank?) distinct vec))
          _ (when (and asked (not= mode "probe"))
              (reference/fail! "AGREEMENT_FILES is only supported with probe"))
          names (vec (or asked in-scope))
          file-limit (get env "AGREEMENT_FILE_TIMEOUT" "900")
          grace (:termination-grace-ms (reference/limits env))
          deadline (+ (duration-ms file-limit) (* 2 grace) 2000)
          _ (when (or (empty? names) (seq (remove (set in-scope) names)))
              (reference/fail! "selection contains unknown or out-of-scope regression names"))
          manifest (edn/read-string (slurp (:agreement cfg)))
          _ (reference/validate-baseline! cfg manifest)
          _ (when (and (= mode "gate")
                       (or (= :unmeasured (:status manifest)) (empty? (:files manifest))))
              (reference/fail! "no measured agreement baseline; run measure before gate"))
          directory (or (not-empty (get env "AGREEMENT_KEEP"))
                        (str (io/file (:artifact-root cfg) (str "agreement-" (UUID/randomUUID)))))
          _ (when (and (.exists (io/file directory)) (seq (.listFiles (io/file directory))))
              (reference/fail! (str "collection directory must be empty: " directory)))
          _ (.mkdirs (io/file directory "runs"))
          _ (reference/observe-target! cfg)
          _ (println "Externally managed endpoint; server code revision is unverified."
                     "Start the intended build before measuring.")
          outcomes
          (mapv
           (fn [name]
             (let [output (str (io/file directory "runs" name))
                   log (str (io/file directory "runs" (str name ".log")))
                   args (into ["timeout" (str "--kill-after=" (/ (double grace) 1000) "s") file-limit
                               "bash" (str (io/file (:repo-root cfg)
                                                    "test/integration/postgres-regress/run-with-api-fixtures.sh"))]
                              (concat (get prerequisites name) [name]))
                   child-env (assoc (into {} env)
                                    "PG_REGRESS_PROFILE" (:profile cfg)
                                    "POSTGRES_SOURCE" (:source cfg)
                                    "PG_REGRESS_SRC" (:regress-root cfg)
                                    "PG_REGRESS_OUTPUT" output
                                    "PGCONNECT_TIMEOUT" (or (get env "PGCONNECT_TIMEOUT") "10"))]
               (println "Measuring" name "->" output)
               (let [status (execute! args child-env log deadline)]
                 (if (zero? status)
                   (try
                     (reference/collect-result! cfg (io/file output "tests") name directory)
                     {:test name :status :complete}
                     (catch Exception e
                       {:test name :status :incomplete :reason (.getMessage e)}))
                   {:test name :status :incomplete :exit status :log log}))))
           names)
          complete? (every? #(= :complete (:status %)) outcomes)]
      (reference/write-edn! (io/file directory "runs.edn") outcomes)
      (reference/finish-collection! cfg directory names complete?)
      (println "Artifacts:" (reference/canonical directory))
      (when-not complete?
        (doseq [outcome (remove #(= :complete (:status %)) outcomes)]
          (binding [*out* *err*] (println "Not measured:" (pr-str outcome))))
        (reference/fail! "incomplete measurement; no baseline was written or compared"))
      directory)))

(defn comparison-command [mode directory]
  ["clojure" "-J-Xmx512m" "-Sdeps" "{:paths [\"test/integration/postgres-regress\"]}"
   "-M" "-m" "agreement" (if (= mode "measure") "stage-measure" mode) directory])

(defn compare! [cfg mode directory env]
  (let [before (reference/validate-collection! cfg directory)
        timeout-ms (duration-ms "AGREEMENT_COMPARE_TIMEOUT"
                                (get env "AGREEMENT_COMPARE_TIMEOUT" "30m"))
        _ (when (= mode "measure") (.delete (io/file directory "baseline.edn")))
        process (doto (ProcessBuilder. ^java.util.List (comparison-command mode directory))
                  (.inheritIO))
        _ (.putAll (.environment process) env)
        exit (reference/wait-for-owned! (.start process) timeout-ms)]
    (cond
      (= 124 exit)
      (binding [*out* *err*]
        (println "comparison timed out; completed per-file checkpoints were retained; baseline unchanged"))

      (and (zero? exit) (= mode "measure"))
      (let [after (reference/validate-collection! cfg directory)
            staged (io/file directory "baseline.edn")
            staged-text (slurp staged)
            manifest (edn/read-string staged-text)
            names ((ns-resolve 'agreement 'application-facing))]
        (when-not (and (= before after)
                       (= (set names) (set (keys (:files manifest))))
                       (= (set names) (set (keys (:outputs after)))))
          (reference/fail! "collection or comparison scope changed; baseline unchanged"))
        (reference/validate-baseline! cfg manifest)
        ((ns-resolve 'postgres-comparison 'atomic-write!) (:agreement cfg) staged-text)
        (println "Wrote baseline:" (:agreement cfg))))
    exit))

(defn -main [& [operation comparison-mode results-dir]]
  (try
    (let [mode (if (= operation "compare") comparison-mode (or operation "gate"))
          _ (when-not (#{"measure" "probe" "gate"} mode)
              (reference/fail! "usage: agreement.sh [measure|gate|probe|compare MODE DIRECTORY]"))
          _ (when (and (= operation "compare") (nil? results-dir))
              (reference/fail! "compare requires a completed collection directory"))
          directory (if (= operation "compare")
                      (do (load-file "test/integration/postgres-regress/agreement.clj") results-dir)
                      (collect! mode (System/getenv)))
          cfg (reference/config)]
      (System/exit (compare! cfg mode directory (System/getenv))))
    (catch Exception e
      (binding [*out* *err*] (println "agreement error:" (.getMessage e)))
      (System/exit 2))))

(when (and (System/getProperty "babashka.file")
           (= (reference/canonical *file*)
              (reference/canonical (System/getProperty "babashka.file"))))
  (apply -main *command-line-args*))
