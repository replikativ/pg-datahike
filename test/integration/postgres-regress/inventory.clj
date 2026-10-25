(ns postgres-inventory
  "Validate an audited release's complete SQL inventory without admitting behavior."
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
    (check! (set/subset? (set (map :name (mapcat :tests (:waves campaign)))) scheduled-names)
            "campaign entries must belong to the schedule")
    (check! (= :unverified (:admission manifest)) "unsupported admission policy")
    (check! (every? #(and (not= :strict (:mode %)) (empty? (:strict-slices %)))
                    (mapcat :tests (:waves campaign)))
            "unverified release cannot inherit strict modes or slices")
    (check! (and (empty? (:evidence ledger))
                 (every? #(= :unverified (:status %)) (vals (:capabilities ledger))))
            "unverified release cannot inherit capability or evidence admissions")
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
     :manifest manifest}))
