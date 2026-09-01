#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def here (fs/parent (fs/file *file*)))
(def root (-> here fs/parent fs/parent))
(def campaign (edn/read-string (slurp (fs/file here "beta-exit.edn"))))
(def circle-config (slurp (fs/file root ".circleci" "config.yml")))

(defn fail! [message]
  (binding [*out* *err*]
    (println "beta-exit campaign error:" message))
  (System/exit 2))

(defn duplicates [xs]
  (->> xs frequencies (keep (fn [[x n]] (when (> n 1) x))) sort))

(defn ci-job-defined? [job]
  (str/includes? circle-config (str "\n  " job ":\n")))

(defn deploy-requires? [job]
  (let [[_ requires]
        (re-find #"(?s)      - deploy:.*?          requires:\n(.*?)      - build-uber:"
                 circle-config)]
    (and requires (str/includes? requires (str "            - " job "\n")))))

(defn validate! []
  (let [gates (:gates campaign)
        blockers (:blockers campaign)
        duplicate-gates (duplicates (map :id gates))
        duplicate-blockers (duplicates (map :id blockers))]
    (when-not (and (string? (:target campaign)) (seq (:target campaign)))
      (fail! ":target must be a non-empty release string"))
    (when (seq duplicate-gates)
      (fail! (str "duplicate gate IDs: " (str/join ", " duplicate-gates))))
    (when (seq duplicate-blockers)
      (fail! (str "duplicate blocker IDs: " (str/join ", " duplicate-blockers))))
    (doseq [{:keys [id cadence status ci-job release-gate? evidence]} gates]
      (when-not (and (keyword? id)
                     (#{:commit :release} cadence)
                     (#{:gating :manual :planned} status)
                     (seq evidence))
        (fail! (str "malformed gate " id)))
      (when (and (= :gating status) (not (string? ci-job)))
        (fail! (str "gating gate has no :ci-job: " id)))
      (when (and ci-job (not (ci-job-defined? ci-job)))
        (fail! (str "CI job is not defined for " id ": " ci-job)))
      (when (and release-gate? (not (deploy-requires? ci-job)))
        (fail! (str "deploy does not require " ci-job " for " id)))
      (doseq [path evidence]
        (when-not (fs/exists? (fs/file root path))
          (fail! (str "missing evidence for " id ": " path)))))
    (doseq [{:keys [id wave status evidence]} blockers]
      (when-not (and (keyword? id) (pos-int? wave)
                     (#{:open :closed :deferred} status) (seq evidence))
        (fail! (str "malformed blocker " id)))
      (doseq [path evidence]
        (when-not (fs/exists? (fs/file root path))
          (fail! (str "missing blocker evidence for " id ": " path)))))))

(defn print-campaign! []
  (println (str "pg-datahike beta-exit target: " (:target campaign)))
  (println (:rule campaign))
  (println)
  (println "Coverage gates")
  (doseq [{:keys [id cadence status claim]} (:gates campaign)]
    (println (format "  %-22s %-7s %-7s %s"
                     (name id) (name cadence) (name status) claim)))
  (println)
  (println "Open blockers by wave")
  (doseq [[wave blockers] (sort-by key (group-by :wave
                                                 (filter #(= :open (:status %))
                                                         (:blockers campaign))))]
    (println (str "  wave " wave))
    (doseq [{:keys [id exit]} blockers]
      (println (str "    " (name id) " — " exit))))
  (println)
  (println (format "%d gates, %d open blockers, %d explicit non-goals"
                   (count (:gates campaign))
                   (count (filter #(= :open (:status %)) (:blockers campaign)))
                   (count (:non-goals campaign)))))

(validate!)
(print-campaign!)
