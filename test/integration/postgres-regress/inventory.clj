(ns postgres-inventory
  "Validate an audited release's SQL inventory and explicitly scoped admissions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(alias 'reference 'postgres-reference)

(defn scheduled-list [regress-root]
  (->> (str/split-lines (slurp (io/file regress-root "parallel_schedule")))
       (keep #(second (re-matches #"test:\s+(.+)" %)))
       (mapcat #(str/split % #"\s+"))))

(defn sql-files [regress-root]
  (into (sorted-map)
        (for [file (.listFiles (io/file regress-root "sql"))
              :when (and (.isFile file) (str/ends-with? (.getName file) ".sql"))]
          [(str/replace (.getName file) #"\.sql$" "") file])))

(defn dispositions [campaign scope]
  (concat (for [wave (:waves campaign) test (:tests wave)]
            [(:name test) {:status :campaign :wave (:id wave)}])
          (for [[status groups] scope [reason names] groups name names]
            [name {:status status :reason reason}])))

(defn check! [condition message]
  (when-not condition (reference/fail! (str "audited inventory: " message))))

(defn- validate-partition! [application-files groups categories label]
  (check! (and (map? groups) (= categories (set (keys groups)))
               (every? set? (vals groups)))
          (str "invalid " label " groups"))
  (let [names (mapcat val groups)]
    (check! (and (= application-files (set names))
                 (= (count names) (count application-files)))
            (str label " must account for every application SQL file exactly once"))))

(defn- validate-residuals! [cfg manifest files tests]
  (when-let [filename (:residual-map manifest)]
    (let [directory (.getParentFile (io/file (:inventory cfg)))
          {:keys [postgres-ref source-commit target-revision target-jar-sha256
                  outcomes fixture-readiness findings] :as residuals}
          (edn/read-string (slurp (io/file directory filename)))
          application-files (into #{} (comp (remove #(= :out-of-scope (:status %)))
                                            (map :name))
                                  (:dispositions manifest))
          remaining (set/union (:differs outcomes) (:incomplete outcomes))
          gates (into {} (for [{:keys [name strict-slices]} tests
                               {:keys [id source-lines]} strict-slices]
                           [[name id] source-lines]))]
      (check! (= [postgres-ref source-commit]
                 [(:postgres-ref cfg) (get-in manifest [:reference :commit])])
              "residual map reference disagrees with profile")
      (check! (and (string? target-revision) (re-matches #"[0-9a-f]{40}" target-revision)
                   (string? target-jar-sha256) (re-matches #"[0-9a-f]{64}" target-jar-sha256))
              "residual map needs a target revision and build checksum")
      (validate-partition! application-files outcomes #{:matches :differs :incomplete} "residual outcomes")
      (validate-partition! application-files fixture-readiness #{:ready :fixture-limited :unverified} "fixture readiness")
      (check! (set/subset? (:incomplete outcomes) (:unverified fixture-readiness))
              "an incomplete run cannot qualify its fixture")
      (check! (and (vector? findings) (= (count findings) (count (set (map :id findings)))))
              "residual findings must have unique identifiers")
      (doseq [{:keys [id file lines state impact detail]} findings]
        (check! (and (keyword? id) (contains? remaining file)
                     (#{:open :boundary :covered} state)
                     (#{:wrong-result :internal-error :fixture :unsupported} impact)
                     (string? detail) (not (str/blank? detail)))
                (str "invalid residual finding " id))
        (let [[start end] lines
              line-count (count (str/split-lines (slurp (get files file))))]
          (check! (and (vector? lines) (= 2 (count lines))
                       (integer? start) (integer? end) (pos? start) (<= start end line-count))
                  (str "invalid source range for residual " id)))
        (when (= :covered state)
          (check! (= lines (get gates [file id]))
                  (str "covered residual must name a matching strict slice: " id))))
      residuals)))

(defn validate! [cfg campaign scope ledger]
  (let [manifest (edn/read-string (slurp (:inventory cfg)))
        root (:regress-root cfg)
        files (sql-files root)
        sql-names (set (keys files))
        scheduled (scheduled-list root)
        scheduled-names (set scheduled)
        declared (dispositions campaign scope)
        declared-names (map first declared)
        entries (:dispositions manifest)
        entry-names (map :name entries)
        tests (mapcat :tests (:waves campaign))
        strict-tests (filter #(= :strict (:mode %)) tests)
        strict-fixtures (when-let [file (:fixtures cfg)]
                          (set (keys (:strict-fixtures (edn/read-string (slurp file))))))
        admitted? (= :partial (:admission manifest))
        expected-files (into (sorted-map)
                             (for [file (.listFiles (io/file root "expected"))
                                   :when (and (.isFile file) (str/ends-with? (.getName file) ".out"))]
                               [(.getName file) (reference/file-sha256 file)]))]
    (check! (= 1 (:format manifest)) "unsupported manifest format")
    (check! (= [(:major cfg) (:postgres-ref cfg)]
               [(:postgres-major manifest) (:postgres-ref manifest)])
            "manifest release identity disagrees with profile")
    (check! (= (reference/validate-source! cfg) (:reference manifest))
            "source provenance changed; re-audit the release inputs")
    (check! (= (reference/file-sha256 (io/file root "parallel_schedule"))
               (:schedule-sha256 manifest)) "schedule fingerprint changed")
    (check! (= expected-files (:expected-files manifest)) "expected variants changed or missing")
    (check! (every? #(contains? expected-files (str % ".out")) sql-names)
            "every SQL file needs its canonical expected output")
    (check! (= (:scheduled-count manifest) (count scheduled) (count scheduled-names))
            "schedule count or uniqueness disagrees")
    (check! (= (:sql-count manifest) (count files) (count entries) (count (set entry-names)))
            "SQL count or disposition uniqueness disagrees")
    (check! (= sql-names (set entry-names) (set declared-names))
            "every SQL file must have exactly one campaign/backlog/exclusion disposition")
    (check! (= (count declared-names) (count (set declared-names)))
            "SQL file classified more than once")
    (check! (set/subset? scheduled-names sql-names) "scheduled source file missing")
    (check! (set/subset? (set (map :name tests)) (into scheduled-names strict-fixtures))
            "campaign entries must belong to the schedule or have an admitted fixture")
    (check! (set/subset? (or strict-fixtures #{}) (set (map :name strict-tests)))
            "admitted fixtures must name strict whole-file gates")
    (check! (#{:unverified :partial} (:admission manifest)) "unsupported admission policy")
    (if admitted?
      (let [fixture-boundaries (into {} (map (juxt :name :fixture-boundaries)) entries)]
        (check! (seq strict-tests) "partial admission needs a strict whole-file gate")
        (check! (every? #(and (= #{} (get fixture-boundaries (:name %)))
                              (or (contains? strict-fixtures (:name %))
                                  (and (empty? (:requires %))
                                       (empty? (get-in manifest [:ordering-prerequisites (:name %)])))))
                        strict-tests)
                "strict whole-file gates need independence or an admitted fixture"))
      (check! (and (empty? strict-tests) (every? (comp empty? :strict-slices) tests))
              "unverified release cannot inherit strict modes or slices"))
    (check! (and (empty? (:evidence ledger))
                 (every? #(= :unverified (:status %)) (vals (:capabilities ledger))))
            "audited release cannot inherit routine/trigger capability or evidence admissions")
    (doseq [{:keys [name scheduled? sql-sha256 expected-sha256] :as entry} entries]
      (check! (= (select-keys entry [:status :reason :wave]) (get (into {} declared) name))
              (str "classification disagrees for " name))
      (check! (= scheduled? (contains? scheduled-names name)) (str "schedule membership disagrees for " name))
      (check! (= sql-sha256 (reference/file-sha256 (get files name))) (str "SQL fingerprint changed for " name))
      (check! (= expected-sha256 (get expected-files (str name ".out")))
              (str "canonical expected fingerprint changed for " name)))
    (doseq [[name prerequisites] (:ordering-prerequisites manifest)]
      (check! (and (contains? sql-names name) (vector? prerequisites)
                   (= (count prerequisites) (count (set prerequisites)))
                   (not (some #{name} prerequisites))
                   (every? sql-names prerequisites))
              (str "invalid source ordering prerequisites for " name)))
    (doseq [[name {:keys [scope source-lines basis]}] (:surface-decisions manifest)]
      (check! (= scope (:status (get (into {} declared) name)))
              (str "surface decision disagrees for " name))
      (check! (and (string? basis) (not (str/blank? basis)))
              (str "surface decision needs a concrete basis for " name))
      (when source-lines
        (let [[start end] source-lines
              line-count (count (str/split-lines (slurp (get files name))))]
          (check! (and (vector? source-lines) (= 2 (count source-lines))
                       (integer? start) (integer? end) (pos? start) (<= start end line-count))
                  (str "invalid surface decision source range for " name)))))
    {:sql-count (count files) :expected-count (count expected-files)
     :application-count (count (remove #(= :out-of-scope (:status %)) entries))
     :scheduled-count (count scheduled-names)
     :unscheduled (vec (sort (set/difference sql-names scheduled-names)))
     :strict-fixtures strict-fixtures
     :residuals (validate-residuals! cfg manifest files tests)
     :manifest manifest}))
