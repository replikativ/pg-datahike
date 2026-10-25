(ns postgres-fixtures
  "Versioned fixture phases and their independently validated provenance."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(when-not (find-ns 'postgres-reference)
  (load-file (str (io/file (.getParent (io/file *file*)) "reference.clj"))))
(alias 'reference 'postgres-reference)

(defn read-edn [path] (edn/read-string (slurp path)))
(defn check! [condition message]
  (when-not condition (reference/fail! (str "fixture evidence: " message))))

(defn descriptor [cfg]
  (let [data (read-edn (:fixtures cfg))]
    (check! (= [1 (:postgres-ref cfg)] [(:format data) (:postgres-ref data)])
            "descriptor identity disagrees with reference")
    data))

(defn ordered-closure [cfg data targets]
  (reference/validate-tests! cfg targets)
  (check! (= (count targets) (count (distinct targets))) "duplicate requested target")
  (let [edges (:prerequisites data)
        setup (:setup data)
        _ (check! (and (map? edges) (not (contains? edges setup)))
                  "setup must be the first phase and cannot have prerequisites")
        visiting (atom #{})
        visited (atom #{})
        ordered (atom [])]
    (reference/validate-tests! cfg [setup])
    (doseq [[name dependencies] edges]
      (reference/validate-tests! cfg (cons name dependencies))
      (check! (and (vector? dependencies) (= (count dependencies) (count (distinct dependencies)))
                   (not (some #{name} dependencies)) (not (some #{setup} dependencies)))
              (str "invalid prerequisites for " name)))
    (letfn [(visit [name]
              (check! (not (@visiting name)) (str "prerequisite cycle at " name))
              (when-not (@visited name)
                (swap! visiting conj name)
                (doseq [dependency (get edges name)] (visit dependency))
                (swap! visiting disj name)
                (swap! visited conj name)
                (swap! ordered conj name)))]
      ;; Validate unused graph components too, then traverse only this request.
      (doseq [name (sort (keys edges))] (visit name))
      (reset! visited #{setup})
      (reset! ordered [setup])
      (doseq [target targets] (visit target))
      @ordered)))

(defn context [cfg env]
  (if-let [path (not-empty (get env "PG_REGRESS_FIXTURE_ENV"))]
    (let [data (read-edn path)
          {:keys [role endpoint native]} data
          _ (check! (= [1 (:postgres-ref cfg)] [(:format data) (:postgres-ref data)])
                    "environment identity disagrees with reference")
          _ (check! (#{:oracle :target} role) "environment role must be oracle or target")
          _ (check! (= (select-keys (:target cfg) [:host :port]) (select-keys endpoint [:host :port]))
                    "environment endpoint disagrees with configured endpoint")
          library (when native
                    (let [manifest (read-edn (:manifest native))
                          directory (reference/canonical (:directory native))
                          source (reference/validate-source! cfg)
                          _ (check! (= [1 (:postgres-ref cfg) (:commit source)]
                                       [(:format manifest) (:postgres-ref manifest) (:source-commit manifest)])
                                    "native build identity disagrees with reference")
                          source-file (:source-file manifest)
                          expected-source (io/file (:source cfg) "src/test/regress/regress.c")
                          _ (check! (and (map? source-file)
                                         (= (:sha256 source-file)
                                            (reference/file-sha256 (:path source-file))
                                            (reference/file-sha256 expected-source))
                                         (str/blank? (reference/command!
                                                      ["git" "-C" (:source cfg) "status" "--porcelain" "--"
                                                       "src/test/regress/regress.c"])))
                                    "native source file differs from the pinned build input")
                          _ (check! (and (map? (:libraries manifest))
                                         (contains? (:libraries manifest) "regress.so"))
                                    "native build must identify regress.so")]
                      (doseq [[name hash] (:libraries manifest)]
                        (check! (and (re-matches #"[a-zA-Z0-9_-]+\.so" name)
                                     (= hash (reference/file-sha256 (io/file directory name))))
                                (str "native library changed: " name)))
                      {:directory directory :manifest (reference/canonical (:manifest native))
                       :manifest-sha256 (reference/file-sha256 (:manifest native))
                       :libraries (:libraries manifest)}))]
      (check! (or (not= :oracle role) library) "oracle requires a verified native regression library")
      {:role role :endpoint (select-keys endpoint [:host :port])
       :exclusive? (true? (:exclusive? data))
       :context-file (reference/canonical path) :context-sha256 (reference/file-sha256 path)
       :native library})
    {:role :unverified}))

(def fixture-database "regression")

(defn discovery-flags! [env]
  (check! (not-any? #(= "1" (get env %)) ["PG_REGRESS_STRICT" "PG_REGRESS_API_STRICT"])
          "PostgreSQL 18 fixtures are raw discovery; use named run.sh for strict assertions"))

(defn exclusive-context! [cfg env]
  (discovery-flags! env)
  (check! (not= fixture-database (get-in cfg [:target :database]))
          "administrative database must not be the canonical fixture database regression")
  (let [ctx (context cfg env)]
    (check! (and (#{:oracle :target} (:role ctx)) (:exclusive? ctx))
            "PostgreSQL 18 fixture runs require an explicit endpoint-bound exclusive context")
    ctx))

(defn plan [cfg env targets]
  (let [data (descriptor cfg)
        names (ordered-closure cfg data targets)
        targets (vec targets)
        requested (set targets)
        ctx (exclusive-context! cfg env)
        default-timeout (or (not-empty (get env "PG_REGRESS_STATEMENT_TIMEOUT")) "10s")
        process-limit (not-empty (get env "PG_REGRESS_TIMEOUT"))
        statement-limit (fn [name]
                          (cond (= name (:setup data)) "120s"
                                (requested name) (get-in data [:target-statement-timeouts name] default-timeout)
                                :else default-timeout))
        phases (mapv (fn [name]
                       {:name name :target? (contains? requested name)
                        :kind (if (= name (:setup data)) :setup
                                  (if (requested name) :target :prerequisite))
                        :statement-timeout (statement-limit name)
                        :process-timeout (or process-limit (when (= "0" (statement-limit name)) "180s"))
                        :directory (cond (requested name) (if (= 1 (count targets)) "tests" (str "targets/" name))
                                         (= name (:setup data)) "setup"
                                         :else (str "prerequisites/" name))}) names)]
    {:format 1 :profile (:profile cfg) :reference (reference/validate-source! cfg)
     :database fixture-database :statement-timeout-default default-timeout
     :process-timeout-default process-limit
     :descriptor {:path (reference/canonical (:fixtures cfg))
                  :sha256 (reference/file-sha256 (:fixtures cfg))}
     :context ctx :targets targets :phases phases}))

(defn oracle-version! [cfg observation]
  (let [number (some-> (:server-version-num observation) str parse-long)]
    (check! (and number (= (:major cfg) (quot number 10000)))
            "oracle server major disagrees with reference")))

(defn start! [cfg env output targets]
  (let [directory (io/file output)
        data (plan cfg env targets)]
    (when (= :oracle (get-in data [:context :role]))
      (oracle-version! cfg (reference/observe-target! cfg)))
    (check! (or (not (.exists directory)) (and (.isDirectory directory) (empty? (.listFiles directory))))
            "output directory must be empty")
    (.mkdirs directory)
    (reference/write-edn! (io/file directory "fixture-plan.edn") data)
    data))

(defn phase-evidence [cfg output phase database]
  (let [directory (io/file output (:directory phase))
        receipt (reference/validate-output! cfg directory)]
    (check! (= [(:name phase)] (:tests receipt)) "phase contains unexpected SQL files")
    (check! (= (assoc (select-keys (:target cfg) [:host :port]) :database database)
               (get-in receipt [:target :endpoint])) "phase used another endpoint or database")
    (check! (= (:statement-timeout phase) (get-in receipt [:limits :statement-timeout]))
            "phase statement timeout disagrees with plan")
    (check! (= (:process-timeout phase) (get-in receipt [:limits :pg-regress-timeout]))
            "phase process timeout disagrees with plan")
    (merge phase {:receipt-sha256 (reference/file-sha256 (io/file directory "reference.edn"))
                  :run-id (:run-id receipt) :outputs (:outputs receipt)
                  :pg-regress-exit (:pg-regress-exit receipt)
                  :driver-options (:driver-options receipt)
                  :server-observation (select-keys (:target receipt) [:server-version :server-version-num])})))

(defn query! [cfg sql]
  (let [{:keys [host port user database]} (:target cfg)]
    (reference/psql! cfg ["-X" "-A" "-t" "--host" host "--port" port "--username" user
                          "--dbname" database "--set=ON_ERROR_STOP=1" "--command" sql])))

(defn global-state [cfg oracle?]
  {:tablespace (query! cfg "SELECT count(*) FROM pg_tablespace WHERE spcname = 'regress_tblspace'")
   :roles (query! cfg "SELECT rolname FROM pg_roles ORDER BY rolname")
   :prepared (query! cfg "SELECT gid FROM pg_prepared_xacts ORDER BY gid")
   :databases (query! cfg "SELECT datname FROM pg_database ORDER BY datname")
   :subscriptions (if oracle?
                    (query! cfg "SELECT oid::text || '|' || subname || '|' || subdbid::text FROM pg_subscription ORDER BY oid")
                    :unobserved-target)})

(defn action-plan! [cfg output]
  (let [run-plan (read-edn (io/file output "fixture-plan.edn"))]
    (check! (and (= (:profile cfg) (:profile run-plan))
                 (= fixture-database (:database run-plan))
                 (true? (get-in run-plan [:context :exclusive?]))
                 (= (select-keys (:target cfg) [:host :port]) (get-in run-plan [:context :endpoint])))
            "fixture action endpoint/profile does not match the owned run")
    run-plan))

(defn database-oid [cfg name]
  (let [answer (query! cfg (str "SELECT oid::text FROM pg_catalog.pg_database WHERE datname = '"
                                (str/replace name "'" "''") "'"))]
    (when (seq answer)
      (let [oid (parse-long answer)]
        (check! (and oid (pos? oid)) "database identity is not a single positive OID")
        oid))))

(defn preflight! [cfg output]
  (let [run-plan (action-plan! cfg output)
        oracle? (= :oracle (get-in run-plan [:context :role]))
        state (global-state cfg oracle?)
        admin (get-in cfg [:target :database])
        admin-oid (database-oid cfg admin)
        existing (database-oid cfg fixture-database)
        proof {:format 1 :database fixture-database
               :endpoint (select-keys (:target cfg) [:host :port]) :admin {:name admin :oid admin-oid}
               :absent-before? (nil? existing) :create-acknowledged? false
               :identity-kind (if oracle? :native-oid :synthetic-name-oid)
               :identity-limitation (when-not oracle?
                                      "Target OIDs derive from names; ownership relies on exclusive endpoint, absent-before and CREATE acknowledgement, not native generation identity.")}]
    (reference/write-edn! (io/file output "fixture-global-before.edn") state)
    (reference/write-edn! (io/file output "fixture-database.edn") proof)
    (check! admin-oid "administrative database identity is unavailable")
    (check! (nil? existing) "canonical fixture database regression already exists; it is not owned by this run")
    (when oracle?
      (check! (= "0" (:tablespace state))
              "regress_tblspace already exists; it is not owned by this run and will not be removed"))))

(defn database-created! [cfg output]
  (let [run-plan (action-plan! cfg output)
        file (io/file output "fixture-database.edn")
        proof (read-edn file)
        oid (database-oid cfg fixture-database)]
    (check! (and (= (:endpoint proof) (get-in run-plan [:context :endpoint]))
                 (:absent-before? proof) oid
                 (not= oid (get-in proof [:admin :oid]))
                 (not= fixture-database (get-in proof [:admin :name])))
            "created database has no distinct, confirmed owned identity")
    (reference/write-edn! file (assoc proof :oid oid :create-acknowledged? true))))

(defn database-cleanup! [cfg output]
  (let [result (try
                 (let [run-plan (action-plan! cfg output)
                       proof (read-edn (io/file output "fixture-database.edn"))
                       oid (:oid proof)]
                   (check! (and (= (:endpoint proof) (get-in run-plan [:context :endpoint]))
                                (= fixture-database (:database proof)) (:absent-before? proof)
                                (:create-acknowledged? proof) (integer? oid) (pos? oid)
                                (not= fixture-database (get-in cfg [:target :database]))
                                (= (get-in cfg [:target :database]) (get-in proof [:admin :name]))
                                (not= oid (get-in proof [:admin :oid])))
                           "refusing to drop an unproven or administrative database")
                   (let [current (database-oid cfg fixture-database)
                         resolved (query! cfg (str "SELECT datname FROM pg_catalog.pg_database WHERE oid = " oid))
                         admin-oid (database-oid cfg (get-in cfg [:target :database]))]
                     (check! (and (= oid current) (= fixture-database resolved)
                                  (= admin-oid (get-in proof [:admin :oid])) (not= oid admin-oid))
                             "database identity changed, disappeared or was renamed; refusing DROP")
                     (query! cfg "DROP DATABASE regression")
                     {:status :complete :database fixture-database :oid oid
                      :identity-kind (:identity-kind proof)}))
                 (catch Exception e {:status :failed :database fixture-database :reason (.getMessage e)}))]
    (reference/write-edn! (io/file output "fixture-database-cleanup.edn") result)
    (= :complete (:status result))))

(defn cleanup-global! [cfg output database-cleanup]
  (let [run-plan (action-plan! cfg output)
        before-file (io/file output "fixture-global-before.edn")
        before (when (.isFile before-file) (read-edn before-file))
        oracle? (= :oracle (get-in run-plan [:context :role]))
        result (try
                 (if before
                   (let [observed (global-state cfg oracle?)
                         owned? (and oracle? (= "0" (:tablespace before)) (= "1" (:tablespace observed)))
                         removed? (and owned? (= "complete" database-cleanup)
                                       (true? (get-in run-plan [:context :exclusive?])))
                         _ (when removed? (query! cfg "DROP TABLESPACE regress_tblspace"))
                         after (global-state cfg oracle?)]
                     {:status (if (= before after) :complete :unverified)
                      :tablespace-owned? owned? :tablespace-removed? (boolean removed?)
                      :before before :after after})
                   {:status :unverified})
                 (catch Exception e {:status :failed :reason (.getMessage e)}))]
    (reference/write-edn! (io/file output "fixture-global-cleanup.edn") result)
    result))

(defn readiness [data ctx phases bootstrap-status cleanup-status global-status]
  (cond
    (not= :complete bootstrap-status) :missing-environment
    (not= :complete cleanup-status) :unverified
    (= :oracle (:role ctx)) (if (not= :complete global-status) :unverified
                                (if (every? #(zero? (:pg-regress-exit %)) phases) :ready :unverified))
    (not= :target (:role ctx)) :unverified
    (some #(and (pos? (:pg-regress-exit %))
                (#{:native-helper-boundary :unsupported-sql-fixture}
                 (get-in data [:contexts (:name %)]))) phases)
    :fixture-limited
    (every? #(or (not (:target? %)) ((:core-only-targets data) (:name %))) phases) :ready
    :else :unverified))

(defn plan-content [run-plan]
  ;; Checkout locations are provenance; identical descriptor bytes can be replayed
  ;; from the merged checkout after an isolated worktree has been removed.
  (update run-plan :descriptor dissoc :path))

(defn finish! [cfg env output exit bootstrap cleanup database]
  (let [run-plan (read-edn (io/file output "fixture-plan.edn"))
        phases (atom [])
        problem (atom nil)
        _ (try
            (check! (= (plan-content run-plan) (plan-content (plan cfg env (:targets run-plan)))) "fixture plan changed during execution")
            (doseq [phase (:phases run-plan)]
              (swap! phases conj (phase-evidence cfg output phase database)))
            (catch Exception e (reset! problem (.getMessage e))))
        bootstrap-status (if (= "complete" bootstrap) :complete :incomplete)
        cleanup-status (keyword cleanup)
        complete? (and (zero? exit) (nil? @problem) (= :complete bootstrap-status))
        _ (when (some #(not= (get-in run-plan [:context :native :directory])
                             (get-in % [:driver-options :dlpath])) @phases)
            (reset! problem "phase used another native library directory"))
        _ (when (= :oracle (get-in run-plan [:context :role]))
            (try (doseq [phase @phases] (oracle-version! cfg (:server-observation phase)))
                 (catch Exception e (reset! problem (.getMessage e)))))
        complete? (and complete? (nil? @problem))
        ownership-file (io/file output "fixture-database.edn")
        ownership (when (.isFile ownership-file) (read-edn ownership-file))
        database-cleanup-file (io/file output "fixture-database-cleanup.edn")
        global-file (io/file output "fixture-global-cleanup.edn")
        global (when (.isFile global-file) (read-edn global-file))
        outcome-complete? (and complete? (= :complete cleanup-status) (= :complete (:status global)))
        status (if complete? (readiness (descriptor cfg) (:context run-plan) @phases bootstrap-status cleanup-status (:status global))
                   (if (= :complete bootstrap-status) :unverified :missing-environment))
        evidence {:format 1 :profile (:profile cfg) :reference (:reference run-plan)
                  :output-directory (reference/canonical output) :database database
                  :plan-sha256 (reference/file-sha256 (io/file output "fixture-plan.edn"))
                  :descriptor (:descriptor run-plan) :context (:context run-plan)
                  :targets (:targets run-plan) :phases @phases
                  :transport-status (if complete? :complete :incomplete)
                  :outcome-status (if outcome-complete? :complete :incomplete)
                  :readiness status :bootstrap {:status bootstrap-status
                                                :log-sha256 (when (.isFile (io/file output "bootstrap.log"))
                                                              (reference/file-sha256 (io/file output "bootstrap.log")))}
                  :cleanup {:status cleanup-status :database database
                            :log-sha256 (when (.isFile (io/file output "cleanup.log"))
                                          (reference/file-sha256 (io/file output "cleanup.log")))}
                  :global-cleanup {:status (:status global)
                                   :sha256 (when (.isFile global-file) (reference/file-sha256 global-file))
                                   :before-sha256 (when (.isFile (io/file output "fixture-global-before.edn"))
                                                    (reference/file-sha256 (io/file output "fixture-global-before.edn")))}
                  :database-ownership {:sha256 (when (.isFile ownership-file) (reference/file-sha256 ownership-file))
                                       :identity-kind (:identity-kind ownership) :oid (:oid ownership)
                                       :cleanup-sha256 (when (.isFile database-cleanup-file)
                                                         (reference/file-sha256 database-cleanup-file))}
                  :problem @problem :wrapper-exit exit}]
    (reference/write-edn! (io/file output "fixture.edn") evidence)
    outcome-complete?))

(defn validate! [cfg directory]
  (let [evidence (read-edn (io/file directory "fixture.edn"))
        plan (read-edn (io/file directory "fixture-plan.edn"))
        _ (check! (= 1 (:format evidence)) "unsupported fixture evidence format")
        _ (check! (= :complete (get-in evidence [:bootstrap :status])) "bootstrap was not completed")
        _ (check! (#{:complete :failed} (get-in evidence [:cleanup :status])) "cleanup has no final status")
        _ (check! (= (reference/canonical directory) (:output-directory evidence)) "foreign fixture directory")
        _ (check! (= :complete (:transport-status evidence)) "fixture transport incomplete")
        _ (check! (= (reference/validate-source! cfg) (:reference evidence) (:reference plan)) "fixture source changed")
        _ (check! (= (:profile cfg) (:profile evidence) (:profile plan)) "fixture profile changed")
        _ (check! (= (plan-content plan)
                     (plan-content (postgres-fixtures/plan cfg
                                                           (cond-> {"PG_REGRESS_STATEMENT_TIMEOUT" (:statement-timeout-default plan)
                                                                    "PG_REGRESS_TIMEOUT" (:process-timeout-default plan)}
                                                             (get-in plan [:context :context-file])
                                                             (assoc "PG_REGRESS_FIXTURE_ENV" (get-in plan [:context :context-file])))
                                                           (:targets plan))))
                  "fixture context or plan changed")
        _ (check! (and (= (:descriptor evidence) (:descriptor plan))
                       (= (get-in evidence [:descriptor :sha256])
                          (reference/file-sha256 (:fixtures cfg)))) "fixture descriptor changed")
        _ (check! (= (:plan-sha256 evidence) (reference/file-sha256 (io/file directory "fixture-plan.edn")))
                  "fixture plan changed")
        _ (check! (= (:phases evidence)
                     (mapv #(phase-evidence cfg directory % (:database evidence)) (:phases plan)))
                  "fixture phase evidence changed")
        _ (check! (= (:log-sha256 (:bootstrap evidence))
                     (reference/file-sha256 (io/file directory "bootstrap.log"))) "bootstrap evidence changed")
        _ (check! (= (:database evidence) (get-in evidence [:cleanup :database])) "cleanup database disagrees")
        _ (check! (= (get-in evidence [:cleanup :log-sha256])
                     (reference/file-sha256 (io/file directory "cleanup.log"))) "cleanup evidence changed")
        _ (check! (every? #(= (get-in plan [:context :native :directory])
                              (get-in % [:driver-options :dlpath])) (:phases evidence))
                  "phase native library directory disagrees")]
    (let [proof (read-edn (io/file directory "fixture-database.edn"))
          cleanup-proof (read-edn (io/file directory "fixture-database-cleanup.edn"))]
      (check! (and (= (:endpoint proof) (get-in plan [:context :endpoint]))
                   (= fixture-database (:database evidence) (:database proof))
                   (:absent-before? proof) (:create-acknowledged? proof) (pos-int? (:oid proof))
                   (pos-int? (get-in proof [:admin :oid]))
                   (= (:identity-kind proof) (if (= :oracle (get-in plan [:context :role]))
                                               :native-oid :synthetic-name-oid))
                   (or (= :native-oid (:identity-kind proof)) (seq (:identity-limitation proof)))
                   (not= (:oid proof) (get-in proof [:admin :oid]))
                   (not= fixture-database (get-in proof [:admin :name]))
                   (= (:oid proof) (get-in evidence [:database-ownership :oid]))
                   (= (:identity-kind proof) (get-in evidence [:database-ownership :identity-kind]))
                   (= (get-in evidence [:database-ownership :sha256])
                      (reference/file-sha256 (io/file directory "fixture-database.edn")))
                   (= (get-in evidence [:database-ownership :cleanup-sha256])
                      (reference/file-sha256 (io/file directory "fixture-database-cleanup.edn"))))
              "database ownership evidence changed")
      (when (= :complete (get-in evidence [:cleanup :status]))
        (check! (and (= :complete (:status cleanup-proof)) (= (:oid proof) (:oid cleanup-proof)))
                "database cleanup identity disagrees")))
    (check! (= (get-in evidence [:global-cleanup :status])
               (:status (read-edn (io/file directory "fixture-global-cleanup.edn"))))
            "global cleanup status disagrees with evidence")
    (check! (= (get-in evidence [:global-cleanup :sha256])
               (reference/file-sha256 (io/file directory "fixture-global-cleanup.edn")))
            "global cleanup evidence changed")
    (check! (= (get-in evidence [:global-cleanup :before-sha256])
               (reference/file-sha256 (io/file directory "fixture-global-before.edn")))
            "global preflight evidence changed")
    (when (= :oracle (get-in plan [:context :role]))
      (doseq [phase (:phases evidence)] (oracle-version! cfg (:server-observation phase))))
    (check! (= (:outcome-status evidence)
               (if (and (= :complete (get-in evidence [:cleanup :status]))
                        (= :complete (get-in evidence [:global-cleanup :status])))
                 :complete :incomplete)) "fixture overall outcome changed")
    (check! (= (:readiness evidence)
               (readiness (descriptor cfg) (:context plan) (:phases evidence)
                          (get-in evidence [:bootstrap :status]) (get-in evidence [:cleanup :status])
                          (get-in evidence [:global-cleanup :status])))
            "fixture readiness changed")
    evidence))

(defn collection-entry [cfg directory name]
  (let [relative (str "runs/" name)
        run-directory (io/file directory relative)
        evidence (validate! cfg run-directory)
        _ (check! (= :complete (:outcome-status evidence)) "fixture cleanup/environment outcome incomplete")
        phase (some #(when (and (:target? %) (= name (:name %))) %) (:phases evidence))]
    (check! phase (str "no fixture target phase for " name))
    (check! (= (get-in phase [:outputs name]) (reference/file-sha256 (io/file directory (str name ".out"))))
            (str "collected target is not the fixture target output: " name))
    {:directory relative :sha256 (reference/file-sha256 (io/file run-directory "fixture.edn"))
     :readiness (:readiness evidence) :role (get-in evidence [:context :role])
     :cleanup (get-in evidence [:cleanup :status])}))

(defn baseline-policy! [cfg mode]
  (when (not= mode "probe")
    (check! (true? (:baseline-qualified? (descriptor cfg)))
            "profile baseline qualification is pending fixture/oracle review")))

(defn assert-qualified! [cfg collection mode]
  (baseline-policy! cfg mode)
  (when (not= mode "probe")
    (check! (and (seq (:fixture-evidence collection))
                 (= (set (keys (:outputs collection))) (set (keys (:fixture-evidence collection))))
                 (every? #(and (= :ready (:readiness %)) (= :target (:role %)))
                         (vals (:fixture-evidence collection))))
            "baseline requires ready target fixture evidence for every target")))

(defn -main [& [operation output & args]]
  (try
    (let [cfg (reference/config)]
      (case operation
        "validate-descriptor" (let [data (descriptor cfg)]
                                (reference/validate-source! cfg)
                                (ordered-closure cfg data [(:setup data)])
                                (println "Fixture descriptor validated:" (:postgres-ref cfg)))
        "plan" (let [data (start! cfg (System/getenv) output args)]
                 (doseq [{:keys [name directory kind statement-timeout process-timeout]} (:phases data)]
                   (print (str name \u0000 directory \u0000 (clojure.core/name kind) \u0000 statement-timeout \u0000 process-timeout \u0000))))
        "preflight" (preflight! cfg output)
        "database-created" (database-created! cfg output)
        "cleanup-database" (when-not (database-cleanup! cfg output) (System/exit 2))
        "cleanup-global" (cleanup-global! cfg output (first args))
        "pending" (reference/write-edn! (io/file output "fixture.edn")
                                        {:format 1 :profile (:profile cfg) :database (first args)
                                         :transport-status :incomplete :readiness :unverified
                                         :cleanup {:status :pending :database (first args)}})
        "cleanup-start" (reference/write-edn! (io/file output "fixture.edn")
                                              (assoc-in (read-edn (io/file output "fixture.edn"))
                                                        [:cleanup :status] :running))
        "dlpath" (when-let [directory (get-in (read-edn (io/file output "fixture-plan.edn")) [:context :native :directory])]
                   (println directory))
        "finish" (let [[exit bootstrap cleanup database] args]
                   (when-not (finish! cfg (System/getenv) output (parse-long exit) bootstrap cleanup database)
                     (System/exit 2)))
        (reference/fail! "unknown fixture operation")))
    (catch Exception e
      (binding [*out* *err*] (println "fixture error:" (.getMessage e)))
      (System/exit 2))))

(when (and (System/getProperty "babashka.file")
           (= (reference/canonical *file*) (reference/canonical (System/getProperty "babashka.file"))))
  (apply -main *command-line-args*))
