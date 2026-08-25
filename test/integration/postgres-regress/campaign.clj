#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[babashka.process :refer [shell]]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def here (fs/parent (fs/file *file*)))
(def repo-root (-> here fs/parent fs/parent fs/parent))
(def campaign (edn/read-string (slurp (fs/file here "campaign.edn"))))
(def postgres-source
  (fs/file (or (System/getenv "POSTGRES_SOURCE")
               (str (fs/file repo-root ".." "postgres")))))

(defn fail! [message]
  (binding [*out* *err*]
    (println "campaign error:" message))
  (System/exit 2))

(defn validate! []
  (let [tests (mapcat :tests (:waves campaign))
        slices (for [test tests, slice (:strict-slices test)] [test slice])
        names (map :name tests)
        prerequisites (mapcat :requires tests)
        duplicates (->> names frequencies (keep (fn [[n c]] (when (> c 1) n))) sort)
        bad-modes (remove (:modes campaign) (map :mode tests))
        missing (remove #(fs/regular-file?
                          (fs/file postgres-source "src" "test" "regress" "sql"
                                   (str % ".sql")))
                        (concat names prerequisites))]
    (when (seq duplicates) (fail! (str "duplicate tests: " (str/join ", " duplicates))))
    (when (seq bad-modes) (fail! (str "unknown modes: " (pr-str (set bad-modes)))))
    (when (seq missing) (fail! (str "missing upstream SQL: " (str/join ", " missing))))
    (doseq [[test {:keys [id source-lines gate test-var]}] slices]
      (let [source (fs/file postgres-source "src" "test" "regress" "sql"
                            (str (:name test) ".sql"))
            [start end] source-lines
            line-count (count (str/split-lines (slurp source)))
            gate-file (fs/file repo-root gate)]
        (when-not (and (integer? start) (integer? end)
                       (pos? start) (<= start end line-count))
          (fail! (str "invalid source range for " (:name test) "/" id ": " source-lines)))
        (when-not (fs/regular-file? gate-file)
          (fail! (str "missing strict-slice gate: " gate)))
        (when-not (str/includes? (slurp gate-file) (str "(deftest " test-var))
          (fail! (str "missing strict-slice test var: " test-var)))))))

(defn print-campaign! []
  (doseq [{:keys [id tests] wave-name :name} (:waves campaign)]
    (println (format "wave %d — %s" id wave-name))
    (doseq [[mode grouped] (sort-by key (group-by :mode tests))]
      (println (format "  %-10s %s" (clojure.core/name mode)
                       (str/join " " (map :name grouped)))))
    (doseq [{:keys [name requires]} tests
            :when (seq requires)]
      (println (format "  requires   %s <- %s" name (str/join " " requires))))
    (doseq [{:keys [name strict-slices]} tests
            {:keys [id source-lines]} strict-slices]
      (println (format "  admitted   %s/%s lines %d-%d"
                       name (clojure.core/name id) (first source-lines) (second source-lines))))))

(validate!)

(if (empty? *command-line-args*)
  (print-campaign!)
  (let [[wave-arg mode-arg] *command-line-args*
        wave-id (parse-long wave-arg)
        wave (some #(when (= wave-id (:id %)) %) (:waves campaign))
        mode (when mode-arg (keyword mode-arg))]
    (when-not wave (fail! (str "unknown wave: " wave-arg)))
    (when (and mode (not ((:modes campaign) mode)))
      (fail! (str "unknown mode: " mode-arg)))
    (let [tests (cond->> (:tests wave) mode (filter #(= mode (:mode %))))
          names (->> tests
                     (mapcat #(concat (:requires %) [(:name %)]))
                     distinct
                     vec)]
      (when (empty? names) (fail! "selection contains no tests"))
      (println (str "running wave " wave-id
                    (when mode (str " mode " (clojure.core/name mode)))
                    ": " (str/join " " names)))
      (let [runner ["bash" (str (fs/file here "run.sh"))]
            command (if (= mode :strict)
                      (into ["env" "PG_REGRESS_API_STRICT=1"] runner)
                      runner)]
        (apply shell {:dir (str repo-root)} (concat command names))))))
