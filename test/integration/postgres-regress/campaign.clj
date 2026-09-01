#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[babashka.process :refer [shell]]
         '[clojure.edn :as edn]
         '[clojure.java.shell :as sh]
         '[clojure.set :as set]
         '[clojure.string :as str])

(def here (fs/parent (fs/file *file*)))
(def repo-root (-> here fs/parent fs/parent fs/parent))
(def campaign (edn/read-string (slurp (fs/file here "campaign.edn"))))
(def scope (edn/read-string (slurp (fs/file here "scope.edn"))))
(def pinned-postgres-source
  (fs/file repo-root ".internal" (str "postgres-" (:postgres-ref campaign))))
(def postgres-source
  (fs/file (or (System/getenv "POSTGRES_SOURCE")
               (when (fs/directory? pinned-postgres-source)
                 (str pinned-postgres-source))
               (str (fs/file repo-root ".." "postgres")))))

(defn fail! [message]
  (binding [*out* *err*]
    (println "campaign error:" message))
  (System/exit 2))

(defn scheduled-test-list []
  (let [schedule (fs/file postgres-source "src" "test" "regress"
                          "parallel_schedule")]
    (when-not (fs/regular-file? schedule)
      (fail! (str "missing upstream schedule: " schedule)))
    (->> (str/split-lines (slurp schedule))
         (keep #(second (re-matches #"test:\s+(.+)" %)))
         (mapcat #(str/split % #"\s+")))))

(defn scheduled-tests []
  (set (scheduled-test-list)))

(defn scope-entries []
  (for [[status groups] scope
        [reason names] groups
        name names]
    {:name name :status status :reason reason}))

(defn validate-postgres-ref! []
  (let [expected (:postgres-ref campaign)
        allow-unpinned? (= "1" (System/getenv "PG_REGRESS_ALLOW_UNPINNED"))
        {:keys [exit out err]}
        (sh/sh "git" "-C" (str postgres-source)
               "describe" "--tags" "--exact-match" "HEAD")
        actual (str/trim out)
        relevant-diff (sh/sh "git" "-C" (str postgres-source)
                             "diff" "--quiet" "HEAD" "--"
                             "src/test/regress/parallel_schedule"
                             "src/test/regress/sql")
        clean-source? (zero? (:exit relevant-diff))]
    (cond
      (and (zero? exit) (= expected actual) clean-source?) nil
      allow-unpinned?
      (binding [*out* *err*]
        (println (str "campaign warning: expected PostgreSQL " expected
                      " with clean regression sources, using "
                      (if (seq actual) actual (str/trim err))
                      (when-not clean-source? " (modified regression sources)"))))
      (and (zero? exit) (= expected actual))
      (fail! (str "PostgreSQL checkout is at " expected
                  " but its regression schedule or SQL files are modified. "
                  "Use a clean pinned checkout for campaign validation, or set "
                  "PG_REGRESS_ALLOW_UNPINNED=1 for deliberate discovery."))
      :else
      (fail! (str "PostgreSQL source must be checked out at " expected
                  "; got " (if (seq actual) actual (str/trim err))
                  ". Set POSTGRES_SOURCE to a pinned checkout, or set "
                  "PG_REGRESS_ALLOW_UNPINNED=1 for deliberate discovery.")))))

(defn validate! []
  (validate-postgres-ref!)
  (let [_ (when-not (and (map? scope) (every? map? (vals scope)))
            (fail! "scope.edn must map statuses to reason maps"))
        scope-statuses (set (keys scope))
        expected-scope-statuses #{:backlog :out-of-scope}
        _ (when-not (= expected-scope-statuses scope-statuses)
            (fail! (str "scope.edn statuses must be exactly "
                        (pr-str expected-scope-statuses) "; got "
                        (pr-str scope-statuses))))
        malformed-groups (for [[status groups] scope
                               [reason names] groups
                               :when (or (not (keyword? reason))
                                         (not (set? names))
                                         (some #(not (string? %)) names))]
                           [status reason])
        _ (when (seq malformed-groups)
            (fail! (str "scope.edn groups must map keyword reasons to sets of "
                        "test-name strings: " (pr-str malformed-groups))))
        tests (mapcat :tests (:waves campaign))
        slices (for [test tests, slice (:strict-slices test)] [test slice])
        names (map :name tests)
        prerequisites (mapcat :requires tests)
        duplicates (->> names frequencies (keep (fn [[n c]] (when (> c 1) n))) sort)
        bad-modes (remove (:modes campaign) (map :mode tests))
        missing (remove #(fs/regular-file?
                          (fs/file postgres-source "src" "test" "regress" "sql"
                                   (str % ".sql")))
                        (concat names prerequisites))
        scheduled-list (scheduled-test-list)
        scheduled (set scheduled-list)
        duplicate-scheduled (->> scheduled-list frequencies
                                 (keep (fn [[n c]] (when (> c 1) n))) sort)
        entries (scope-entries)
        scoped-names (map :name entries)
        duplicate-scope (->> scoped-names frequencies
                             (keep (fn [[n c]] (when (> c 1) n))) sort)
        campaign-names (set names)
        inventoried (into campaign-names scoped-names)
        unclassified (sort (set/difference scheduled inventoried))
        unscheduled (sort (set/difference inventoried scheduled))
        doubly-classified (sort (set/intersection campaign-names
                                                  (set scoped-names)))]
    (when (seq duplicates) (fail! (str "duplicate tests: " (str/join ", " duplicates))))
    (when (seq bad-modes) (fail! (str "unknown modes: " (pr-str (set bad-modes)))))
    (when (seq missing) (fail! (str "missing upstream SQL: " (str/join ", " missing))))
    (when (seq duplicate-scheduled)
      (fail! (str "duplicate tests in PostgreSQL schedule: "
                  (str/join ", " duplicate-scheduled))))
    (when (seq duplicate-scope)
      (fail! (str "tests occur more than once in scope.edn: "
                  (str/join ", " duplicate-scope))))
    (when (seq doubly-classified)
      (fail! (str "campaign tests must not also occur in scope.edn: "
                  (str/join ", " doubly-classified))))
    (when (seq unclassified)
      (fail! (str "scheduled tests missing from the inventory: "
                  (str/join ", " unclassified))))
    (when (seq unscheduled)
      (fail! (str "inventory entries absent from the PostgreSQL schedule: "
                  (str/join ", " unscheduled))))
    (doseq [{:keys [name strict-slices]} tests]
      (let [ids (map :id strict-slices)
            duplicate-ids (->> ids frequencies
                               (keep (fn [[id c]] (when (> c 1) id))))]
        (when (seq duplicate-ids)
          (fail! (str "duplicate strict-slice IDs for " name ": "
                      (str/join ", " duplicate-ids))))))
    (doseq [[test {:keys [id source-lines gate test-var]}] slices]
      (when-not (keyword? id)
        (fail! (str "strict-slice ID must be a keyword for " (:name test))))
      (when-not (and (vector? source-lines) (= 2 (count source-lines)))
        (fail! (str "strict-slice source-lines must contain exactly two values for "
                    (:name test) "/" id)))
      (when-not (and (string? gate) (not (str/blank? gate))
                     (string? test-var) (not (str/blank? test-var)))
        (fail! (str "strict-slice gate and test-var must be nonblank strings for "
                    (:name test) "/" id)))
      (let [source (fs/file postgres-source "src" "test" "regress" "sql"
                            (str (:name test) ".sql"))
            [start end] source-lines
            line-count (count (str/split-lines (slurp source)))
            gate-file (fs/file repo-root gate)
            test-pattern (re-pattern
                          (str "(?m)^\\s*\\(deftest\\s+"
                               (java.util.regex.Pattern/quote test-var)
                               "(?=\\s|\\[)"))]
        (when-not (and (integer? start) (integer? end)
                       (pos? start) (<= start end line-count))
          (fail! (str "invalid source range for " (:name test) "/" id ": " source-lines)))
        (when-not (fs/regular-file? gate-file)
          (fail! (str "missing strict-slice gate: " gate)))
        (when-not (re-find test-pattern (slurp gate-file))
          (fail! (str "missing strict-slice test var: " test-var)))))))

(defn print-inventory! []
  (let [campaign-count (count (set (mapcat (comp (partial map :name) :tests)
                                           (:waves campaign))))
        entries (scope-entries)]
    (println (format "PostgreSQL %d schedule: %d tests"
                     (:postgres-major campaign) (count (scheduled-tests))))
    (println (format "  campaign     %d" campaign-count))
    (doseq [status [:backlog :out-of-scope]]
      (let [selected (filter #(= status (:status %)) entries)]
        (println (format "  %-12s %d" (name status) (count selected)))
        (doseq [[reason xs] (sort-by key (group-by :reason selected))]
          (println (format "    %-26s %d" (name reason) (count xs))))))))

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

(if (= ["inventory"] *command-line-args*)
  (print-inventory!)
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
            prerequisites (->> tests (mapcat :requires) distinct vec)
            api-fixtures? (some #{"test_setup"} prerequisites)
            names (->> (concat (remove #{"test_setup"} prerequisites)
                               (map :name tests))
                       distinct
                       vec)]
        (when (empty? names) (fail! "selection contains no tests"))
        (println (str "running wave " wave-id
                      (when mode (str " mode " (clojure.core/name mode)))
                      ": " (str/join " " names)))
        (let [runner ["bash" (str (fs/file here
                                           (if api-fixtures?
                                             "run-with-api-fixtures.sh"
                                             "run.sh")))]
              command (if (= mode :strict)
                        (into ["env" "PG_REGRESS_API_STRICT=1"] runner)
                        runner)]
          (apply shell {:dir (str repo-root)} (concat command names)))))))
