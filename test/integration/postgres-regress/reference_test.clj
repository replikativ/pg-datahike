#!/usr/bin/env bb
(ns postgres-reference-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(load-file "test/integration/postgres-regress/reference.clj")
(alias 'reference 'postgres-reference)

(def ^:dynamic *temporary* nil)

(defn command! [& args]
  (let [r (apply sh/sh args)]
    (when-not (zero? (:exit r)) (throw (ex-info (:err r) r)))
    r))

(defn file! [path text]
  (io/make-parents path)
  (spit path text)
  (str path))

(defn git! [dir & args]
  (apply command! "git" "-C" (str dir) args))

(defn source! [label ref]
  (let [dir (io/file *temporary* label)]
    (.mkdirs dir)
    (git! dir "init" "-q")
    (git! dir "config" "user.name" "Reference Harness Test")
    (git! dir "config" "user.email" "harness@example.invalid")
    (file! (io/file dir "src/test/regress/parallel_schedule") "test: test_setup foo\n")
    (doseq [name ["foo" "test_setup"]]
      (file! (io/file dir "src/test/regress/sql" (str name ".sql")) "SELECT 1;\n")
      (file! (io/file dir "src/test/regress/expected" (str name ".out")) "checked output\n"))
    (file! (io/file dir "src/test/regress/data/value.data") "fixture\n")
    (file! (io/file dir "src/test/regress/resultmap") "# map\n")
    (git! dir "add" ".")
    (git! dir "commit" "-qm" "reference inputs")
    (git! dir "tag" ref)
    (str dir)))

(defn tools! [label major]
  (let [dir (io/file *temporary* label)
        psql (io/file dir "psql")
        regress (io/file dir "pg_regress")]
    (file! psql
           (str "#!/usr/bin/env bash\nset -eu\n"
                "if [[ -n \"${REFERENCE_TEST_TOOL_LOG:-}\" ]]; then echo psql >> \"$REFERENCE_TEST_TOOL_LOG\"; fi\n"
                "if [[ \"${1:-}\" == --version ]]; then echo 'psql (PostgreSQL) " major ".7'; exit 0; fi\n"
                "if [[ \"$*\" == *DROP*DATABASE* && -n \"${REFERENCE_TEST_HANG_CLEANUP:-}\" ]]; then echo $$ > \"$REFERENCE_TEST_HANG_CLEANUP\"; trap '' TERM; while :; do sleep 1; done; fi\n"
                "case \"$*\" in\n"
                " *current_setting*) printf '17.7\\t170007\\tUTC\\tUTF8\\n' ;;\n"
                " *count*onek*) echo '1000|1000|10000|10000|58|6|5|3' ;;\n"
                "esac\n"))
    (file! regress
           (str "#!/usr/bin/env bash\nset -eu\n"
                "if [[ \"${1:-}\" == --version ]]; then echo 'pg_regress (PostgreSQL) " major ".7'; exit 0; fi\n"
                "names=()\nfor arg in \"$@\"; do case \"$arg\" in\n"
                " --outputdir=*) out=\"${arg#*=}\" ;;\n"
                " --inputdir=*) input=\"${arg#*=}\" ;;\n"
                " --*) ;;\n *) names+=(\"$arg\") ;;\nesac; done\n"
                "mkdir -p \"$out/results\"\n"
                "for name in \"${names[@]}\"; do cp \"$input/expected/$name.out\" \"$out/results/$name.out\"; done\n"
                "if [[ \"${REFERENCE_TEST_PARTIAL:-0}\" == 1 ]]; then exit 124; fi\n"
                "if [[ \" ${names[*]} \" == *\" foo \"* && -n \"${REFERENCE_TEST_HANG_DRIVER:-}\" ]]; then echo $$ > \"$REFERENCE_TEST_HANG_DRIVER\"; trap '' TERM; while :; do sleep 1; done; fi\n"))
    (.setExecutable psql true)
    (.setExecutable regress true)
    (str dir)))

(defn env! [profile src tools]
  (merge
   (apply dissoc (into {} (System/getenv))
          ["PG_REGRESS_PROFILE" "POSTGRES_SOURCE" "PG_REGRESS_SRC" "PG_REGRESS_MAJOR"
           "PG_REGRESS_BIN" "PG_REGRESS_BINDIR" "PG_REGRESS_OUTPUT"
           "PG_REGRESS_ALLOW_UNPINNED" "AGREEMENT_KEEP" "AGREEMENT_FILES"])
   {"PG_REGRESS_PROFILE" profile "POSTGRES_SOURCE" src
    "PG_REGRESS_BINDIR" tools "PG_REGRESS_BIN" (str (io/file tools "pg_regress"))
    "PGPASSWORD" "reference-test-secret-must-not-be-recorded"}))

(defn cfg! [profile]
  (let [src (source! (str "source" profile) (str "REL_" profile "_" (if (= profile "17") "7" "6")))
        tools (tools! (str "tools" profile) profile)
        env (env! profile src tools)]
    {:config (reference/config reference/repo-root env) :env env :source src :tools tools}))

(use-fixtures :each
  (fn [f]
    (let [dir (.toFile (Files/createTempDirectory "pgdh-reference-test-" (make-array FileAttribute 0)))]
      (try (binding [*temporary* dir] (f))
           (finally (doseq [file (reverse (file-seq dir))] (.delete file)))))))

(deftest profiles-select-independent-inputs-and-artifacts
  (let [{a :config env :env} (cfg! "17")
        {b :config} (cfg! "18")]
    (is (= "17" (:profile (reference/config reference/repo-root {}))))
    (is (= (:target (reference/config reference/repo-root {}))
           (:target (reference/config reference/repo-root
                                      {"PG_REGRESS_HOST" "" "PG_REGRESS_PORT" ""
                                       "PG_REGRESS_USER" "" "PG_REGRESS_DB" ""}))))
    (is (= "REL_17_7" (:postgres-ref a)))
    (is (= "REL_18_6" (:postgres-ref b)))
    (is (not= (:artifact-root a) (:artifact-root b)))
    (is (:authoritative? (reference/validate-source! a)))
    (is (:authoritative? (reference/validate-source! b)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"different checkouts"
                          (reference/config reference/repo-root
                                            (assoc env "PG_REGRESS_SRC" (str *temporary* "/other/src/test/regress")))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"contradicts"
                          (reference/config reference/repo-root (assoc env "PG_REGRESS_MAJOR" "18"))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"discovery runs only"
                          (reference/require-campaign! b)))))

(deftest dirty-expected-fixtures-and-wrong-commit-cannot-be-authoritative
  (let [{:keys [config source env]} (cfg! "17")]
    (doseq [file ["src/test/regress/expected/foo.out" "src/test/regress/data/value.data"
                  "src/test/regress/resultmap"]]
      (file! (io/file source file) "changed\n")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"clean REL_17_7"
                            (reference/validate-source! config)))
      (let [untrusted (reference/config reference/repo-root (assoc env "PG_REGRESS_ALLOW_UNPINNED" "1"))]
        (is (false? (:authoritative? (reference/validate-source! untrusted))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"clean pinned"
                              (reference/require-authoritative! untrusted))))
      (git! source "checkout" "HEAD" "--" file))
    (file! (io/file source "src/test/regress/expected/new.out") "untracked\n")
    (is (thrown? clojure.lang.ExceptionInfo (reference/validate-source! config)))
    (.delete (io/file source "src/test/regress/expected/new.out"))
    (file! (io/file source "src/test/regress/sql/foo.sql") "SELECT 2;\n")
    (git! source "add" ".")
    (git! source "commit" "-qm" "different release contents")
    (is (thrown? clojure.lang.ExceptionInfo (reference/validate-source! config)))))

(deftest baseline-and-driver-version-must-match-the-profile
  (let [{:keys [config tools]} (cfg! "17")]
    (reference/validate-baseline! config {:postgres-ref "REL_17_7"})
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not match"
                          (reference/validate-baseline! config {:postgres-ref "REL_18_6"})))
    (is (seq (reference/validate-tools! config)))
    (file! (io/file tools "psql") "#!/usr/bin/env bash\necho 'psql (PostgreSQL) 18.6'\n")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"contradicts profile"
                          (reference/validate-tools! config)))))

(deftest receipt-ownership-rejects-partial-stale-and-foreign-results
  (let [{:keys [config]} (cfg! "17")
        partial (io/file *temporary* "partial")
        run (io/file *temporary* "run")
        collected (io/file *temporary* "collected")]
    (.mkdirs collected)
    (reference/begin-output! config partial ["foo"])
    (file! (io/file partial "results/foo.out") "partial\n")
    (is (false? (reference/finish-output! config partial 124)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (reference/collect-result! config partial "foo" collected)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be empty"
                          (reference/begin-output! config partial ["foo"])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already finished"
                          (reference/finish-output! config partial 0)))
    (reference/begin-output! config run ["foo"])
    (file! (io/file run "results/foo.out") "complete\n")
    (is (reference/finish-output! config run 0))
    (reference/collect-result! config run "foo" collected)
    (is (= "complete\n" (slurp (io/file collected "foo.out"))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"overwrite"
                          (reference/collect-result! config run "foo" collected)))
    (reference/finish-collection! config collected ["foo"] true)
    (is (= :complete (:status (reference/validate-collection! config collected))))
    (let [receipt (edn/read-string (slurp (io/file run "reference.edn")))
          foreign (io/file *temporary* "foreign")]
      (file! (io/file foreign "reference.edn") (pr-str receipt))
      (file! (io/file foreign "results/foo.out") "complete\n")
      (is (thrown? clojure.lang.ExceptionInfo
                   (reference/collect-result! config foreign "foo" (io/file *temporary* "other")))))
    (file! (io/file run "results/foo.out") "later tampering\n")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"changed after"
                          (reference/collect-result! config run "foo" (io/file *temporary* "other"))))
    (file! (io/file collected "foo.out") "stale replacement\n")
    (is (thrown? clojure.lang.ExceptionInfo (reference/validate-collection! config collected)))))

(defn collect-through-wrapper [env]
  (sh/sh "bb" "-e"
         (str "(load-file \"test/integration/postgres-regress/agreement-runner.clj\") "
              "(postgres-agreement-runner/collect! \"probe\" (System/getenv))")
         :env env :dir reference/repo-root))

(deftest wrappers-collect-only-their-own-complete-run-and-never-manage-the-server
  (let [{:keys [env]} (cfg! "17")
        collection (io/file *temporary* "agreement")
        env (assoc env "AGREEMENT_FILES" " , foo,  " "AGREEMENT_KEEP" (str collection))
        unrelated (.start (ProcessBuilder. ^java.util.List
                           ["bash" "-c" "exec -a start_pgwire_reference_test sleep 30"]))]
    (try
      (let [r (collect-through-wrapper env)]
        (is (zero? (:exit r)) (str (:out r) (:err r)))
        (is (.isAlive unrelated))
        (is (= "checked output\n" (slurp (io/file collection "foo.out"))))
        (is (.isFile (io/file collection "runs/foo/tests/reference.edn")))
        (let [receipt (slurp (io/file collection "runs/foo/tests/reference.edn"))]
          (is (str/includes? receipt ":code-revision :unverified"))
          (is (not (str/includes? receipt "reference-test-secret-must-not-be-recorded")))))
      (let [before (slurp (io/file collection "foo.out"))
            r (collect-through-wrapper env)]
        (is (not (zero? (:exit r))))
        (is (= before (slurp (io/file collection "foo.out")))))
      (finally (.destroy unrelated) (.waitFor unrelated)))))

(deftest interrupted-child-results-do-not-get-measured-from-old-artifacts
  (let [{:keys [env]} (cfg! "17")
        collection (io/file *temporary* "partial-agreement")
        stale (file! (io/file *temporary* "unrelated-old/tests/results/foo.out") "stale\n")
        r (collect-through-wrapper
           (assoc env "AGREEMENT_FILES" "foo" "AGREEMENT_KEEP" (str collection)
                  "REFERENCE_TEST_PARTIAL" "1"))]
    (is (not (zero? (:exit r))))
    (is (= "stale\n" (slurp stale)))
    (is (not (.exists (io/file collection "foo.out"))))
    (is (= :incomplete (:status (edn/read-string (slurp (io/file collection "collection.edn"))))))))

(deftest named-pg18-discovery-keeps-the-runtime-target-independent
  (let [{:keys [config env]} (cfg! "18")
        output (io/file *temporary* "pg18-run")
        r (sh/sh "bash" "test/integration/postgres-regress/run.sh" "foo"
                 :dir reference/repo-root :env (assoc env "PG_REGRESS_OUTPUT" (str output)))]
    (is (zero? (:exit r)) (str (:out r) (:err r)))
    (let [receipt (edn/read-string (slurp (io/file output "reference.edn")))]
      (is (= "18" (:profile receipt)))
      (is (= "REL_18_6" (get-in receipt [:reference :postgres-ref])))
      (is (= "17.7" (get-in receipt [:target :server-version])))
      (is (= :unverified (get-in receipt [:target :code-revision])))
      (is (= :complete (:status receipt))))))

(deftest a-dirty-source-fails-the-wrapper-before-running-reference-tools
  (let [{:keys [source env]} (cfg! "17")
        tool-log (io/file *temporary* "calls.log")
        output (io/file *temporary* "dirty-run")]
    (file! (io/file source "src/test/regress/expected/foo.out") "contaminated\n")
    (let [r (sh/sh "bash" "test/integration/postgres-regress/run.sh" "foo"
                   :dir reference/repo-root
                   :env (assoc env "PG_REGRESS_OUTPUT" (str output)
                               "REFERENCE_TEST_TOOL_LOG" (str tool-log)))]
      (is (= 2 (:exit r)))
      (is (str/includes? (:err r) "clean REL_17_7"))
      (is (not (.exists tool-log)))
      (is (not (.exists output))))))

(deftest connection-strings-are-rejected-without-recording-credentials
  (let [{:keys [env]} (cfg! "17")
        output (io/file *temporary* "unsafe-endpoint")
        secret "credential-value-must-not-leak"]
    (doseq [database [(str "postgresql://user:" secret "@host/db")
                      (str "dbname=db password=" secret)]]
      (let [r (sh/sh "bash" "test/integration/postgres-regress/run.sh" "foo"
                     :dir reference/repo-root
                     :env (assoc env "PG_REGRESS_DB" database "PG_REGRESS_OUTPUT" (str output)))]
        (is (= 2 (:exit r)))
        (is (not (str/includes? (str (:out r) (:err r)) secret)))
        (is (not (.exists output)))))))

(deftest unpinned-discovery-hashes-untracked-input-bytes
  (let [{:keys [env source]} (cfg! "17")
        cfg (reference/config reference/repo-root (assoc env "PG_REGRESS_ALLOW_UNPINNED" "1"))
        path (io/file source "src/test/regress/sql/untracked.sql")]
    (file! path "SELECT 1;\n")
    (let [before (reference/validate-source! cfg)]
      (is (false? (:authoritative? before)))
      (file! path "SELECT 2;\n")
      (is (not= (:inputs-sha256 before) (:inputs-sha256 (reference/validate-source! cfg)))))))

(deftest campaign-ledger-and-baseline-provenance-cannot-contradict-the-profile
  (let [{:keys [config]} (cfg! "17")
        campaign (io/file *temporary* "campaign.edn")
        ledger (io/file *temporary* "ledger.edn")
        cfg (assoc config :campaign (str campaign) :ledger (str ledger))]
    (file! campaign "{:postgres-major 17 :postgres-ref \"REL_17_7\"}")
    (file! ledger "{:postgres-ref \"REL_17_7\"}")
    (reference/require-campaign! cfg)
    (doseq [[file data] [[campaign "{:postgres-major 18 :postgres-ref \"REL_17_7\"}"]
                         [ledger "{:postgres-ref \"REL_18_6\"}"]]]
      (let [before (slurp file)]
        (file! file data)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"identity disagrees"
                              (reference/require-campaign! cfg)))
        (file! file before)))
    (let [source (reference/validate-source! cfg)
          baseline {:postgres-ref "REL_17_7" :profile "17"
                    :source-commit (:commit source) :inputs-sha256 (:inputs-sha256 source)}]
      (reference/validate-baseline! cfg baseline)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"provenance differs"
                            (reference/validate-baseline! cfg (assoc baseline :inputs-sha256 "wrong"))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not match"
                            (reference/validate-baseline! cfg (assoc baseline :profile "18")))))))

(deftest abnormal-connection-output-is-incomplete-even-with-a-result-file
  (let [{:keys [config]} (cfg! "17")
        output (io/file *temporary* "lost-connection")]
    (reference/begin-output! config output ["foo"])
    (file! (io/file output "results/foo.out") "psql: server closed the connection unexpectedly\n")
    (is (false? (reference/finish-output! config output 1)))
    (is (= :incomplete (:status (edn/read-string (slurp (io/file output "reference.edn"))))))))

(deftest comparator-writes-verified-provenance-and-rejects-a-foreign-baseline
  (let [{:keys [env]} (cfg! "17")
        project (io/file *temporary* "project")
        here (io/file project "test/integration/postgres-regress")
        collection (io/file project "results")]
    (doseq [name ["reference.clj" "profiles.edn" "agreement.clj" "comparison.clj"]]
      (file! (io/file here name)
             (slurp (io/file reference/repo-root "test/integration/postgres-regress" name))))
    (file! (io/file here "campaign.edn") "{:postgres-major 17 :postgres-ref \"REL_17_7\"}")
    (file! (io/file here "routine-trigger-capabilities.edn") "{:postgres-ref \"REL_17_7\"}")
    (file! (io/file here "scope.edn") "{:out-of-scope {:fixtures #{\"test_setup\"}}}")
    (file! (io/file here "agreement.edn") "{:postgres-ref \"REL_17_7\" :tolerance 3.0 :files {\"foo\" 0.0}}")
    (.mkdirs collection)
    (let [cfg (reference/config project env)
          source (reference/validate-source! cfg)
          baseline-file (io/file here "agreement.edn")
          invoke (fn [mode]
                   (sh/sh "bb" "-e"
                          (str "(load-file \"test/integration/postgres-regress/agreement.clj\") "
                               "(agreement/-main " (pr-str mode) " " (pr-str (str collection)) ")")
                          :dir (str project) :env env))]
      (file! (io/file collection "foo.out") "checked output\n")
      (reference/finish-collection! cfg collection ["foo"] true)
      (let [r (invoke "measure")
            manifest (edn/read-string (slurp baseline-file))]
        (is (zero? (:exit r)) (str (:out r) (:err r)))
        (is (= "17" (:profile manifest)))
        (is (= (:commit source) (:source-commit manifest)))
        (is (= (:inputs-sha256 source) (:inputs-sha256 manifest)))
        (is (= 100.0 (get-in manifest [:files "foo"]))))
      (let [before (slurp baseline-file)
            staged (invoke "stage-measure")]
        (is (zero? (:exit staged)) (str (:out staged) (:err staged)))
        (is (= before (slurp baseline-file)))
        (is (= :measured (:status (edn/read-string (slurp (io/file collection "baseline.edn")))))))
      (let [seed "{:postgres-ref \"REL_17_7\" :status :unmeasured :files {}}"]
        (spit baseline-file seed)
        (let [r (invoke "gate")]
          (is (not (zero? (:exit r))))
          (is (str/includes? (:err r) "no measured agreement baseline"))
          (is (= seed (slurp baseline-file)))))
      (let [foreign (pr-str {:postgres-ref "REL_18_6" :tolerance 3.0 :files {"foo" 100.0}})]
        (spit baseline-file foreign)
        (let [r (invoke "gate")]
          (is (not (zero? (:exit r))))
          (is (str/includes? (:err r) "does not match selected reference"))
          (is (= foreign (slurp baseline-file))))))))

(deftest provenance-labels-observations-and-hashes-private-options-and-harness-bytes
  (let [{:keys [config]} (cfg! "17")
        env {"PG_REGRESS_TIMEOUT" "30s" "PG_REGRESS_STATEMENT_TIMEOUT" "2s"
             "AGREEMENT_FILE_TIMEOUT" "1m" "PGOPTIONS" "-c private.secret=sensitive"}
        evidence (reference/options-evidence env)
        project (io/file *temporary* "runner")]
    (is (= :preflight-session (:observation (reference/observe-target! config))))
    (is (= "30s" (:pg-regress-timeout (reference/limits env))))
    (is (= "2s" (:statement-timeout (reference/limits env))))
    (is (= "10s" (:statement-timeout (reference/limits {"PG_REGRESS_STATEMENT_TIMEOUT" ""}))))
    (is (= "1m" (:agreement-file-timeout (reference/limits env))))
    (is (true? (get-in evidence [:caller :present?])))
    (is (not (str/includes? (pr-str evidence) "sensitive")))
    (is (not= (get-in evidence [:caller :sha256]) (get-in evidence [:pg-regress-startup :sha256])))
    (doseq [file ["bb.edn" ".circleci/config.yml" "test/integration/postgres-regress/helper.clj"]]
      (file! (io/file project file) "initial"))
    (git! project "init" "-q")
    (git! project "-c" "user.name=Test" "-c" "user.email=test@example.invalid" "add" ".")
    (git! project "-c" "user.name=Test" "-c" "user.email=test@example.invalid" "commit" "-qm" "runner")
    (let [before (reference/runner-state (str project))]
      (file! (io/file project "test/integration/postgres-regress/new-helper.clj") "untracked executed helper")
      (let [after (reference/runner-state (str project))]
        (is (= (:commit before) (:commit after)))
        (is (not= (:harness-sha256 before) (:harness-sha256 after)))
        (is (:dirty? after))))))

(deftest preflight-kills-only-owned-term-ignoring-client-with-a-finite-deadline
  (let [{:keys [config tools]} (cfg! "17")
        marker (io/file *temporary* "client.pid")
        unrelated (.start (ProcessBuilder. ^java.util.List ["sleep" "30"]))
        started (System/nanoTime)]
    (file! (io/file tools "psql")
           (str "#!/usr/bin/env bash\necho $$ > " marker "\ntrap '' TERM\nwhile :; do sleep 1; done\n"))
    (try
      (with-redefs-fn {(ns-resolve 'postgres-reference 'client-timeout-ms) 150
                       (ns-resolve 'postgres-reference 'termination-grace-ms) 100}
        #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"timed out"
                               (reference/observe-target! config))))
      (is (.exists marker))
      (is (< (/ (- (System/nanoTime) started) 1e9) 5))
      (is (.isAlive unrelated))
      (finally (.destroyForcibly unrelated) (.waitFor unrelated)))))

(deftest whole-file-timeout-bounds-term-ignoring-driver-and-cleanup
  (let [{:keys [env]} (cfg! "17")
        collection (io/file *temporary* "hung-agreement")
        driver (io/file *temporary* "driver.pid")
        cleanup (io/file *temporary* "cleanup.pid")
        unrelated (.start (ProcessBuilder. ^java.util.List ["sleep" "60"]))
        started (System/nanoTime)]
    (try
      (let [r (collect-through-wrapper
               (assoc env "AGREEMENT_FILES" "foo" "AGREEMENT_KEEP" (str collection)
                      "AGREEMENT_FILE_TIMEOUT" "6s"
                      "REFERENCE_TEST_HANG_DRIVER" (str driver)
                      "REFERENCE_TEST_HANG_CLEANUP" (str cleanup)))]
        (is (not (zero? (:exit r))))
        (is (.exists driver) (str (:out r) (:err r)))
        (is (.exists cleanup) (str (:out r) (:err r)))
        (is (< (/ (- (System/nanoTime) started) 1e9) 20))
        (is (.isAlive unrelated))
        (is (not (.exists (io/file collection "foo.out"))))
        (is (= :incomplete (:status (edn/read-string (slurp (io/file collection "collection.edn")))))))
      (finally (.destroyForcibly unrelated) (.waitFor unrelated)))))

(deftest comparison-deadline-keeps-baseline-and-unrelated-process
  (load-file "test/integration/postgres-regress/agreement-runner.clj")
  (let [baseline (io/file *temporary* "agreement.edn")
        collection (io/file *temporary* "comparison")
        staged (io/file collection "baseline.edn")
        unrelated (.start (ProcessBuilder. ^java.util.List ["sleep" "30"]))
        started (System/nanoTime)]
    (.mkdirs collection)
    (spit baseline "previous baseline")
    (try
      (with-redefs-fn
        {(ns-resolve 'postgres-reference 'validate-collection!) (fn [& _] {:status :complete})
         (ns-resolve 'postgres-reference 'termination-grace-ms) 100
         (ns-resolve 'postgres-agreement-runner 'comparison-command)
         (fn [& _] ["bash" "-c"
                    "printf 'uncommitted result' > \"$1\"; trap '' TERM; while :; do sleep 1; done"
                    "comparison-test" (str staged)])}
        #(is (= 124 ((ns-resolve 'postgres-agreement-runner 'compare!)
                     {:agreement (str baseline)} "measure" (str collection)
                     {"AGREEMENT_COMPARE_TIMEOUT" "0.2s"}))))
      (is (.exists staged))
      (is (= "previous baseline" (slurp baseline)))
      (is (< (/ (- (System/nanoTime) started) 1e9) 5))
      (is (.isAlive unrelated))
      (finally (.destroyForcibly unrelated) (.waitFor unrelated)))))

(deftest baseline-promotion-requires-success-and-unchanged-scope-and-collection
  (load-file "test/integration/postgres-regress/agreement-runner.clj")
  (load-file "test/integration/postgres-regress/comparison.clj")
  (let [{:keys [config]} (cfg! "17")
        source (reference/validate-source! config)
        baseline (io/file *temporary* "baseline.edn")
        collection (io/file *temporary* "completed")
        candidate (io/file *temporary* "candidate.edn")
        staged (io/file collection "baseline.edn")
        cfg (assoc config :agreement (str baseline))
        receipt {:status :complete :outputs {"foo" "hash"}}
        manifest {:postgres-ref "REL_17_7" :profile "17" :status :measured
                  :source-commit (:commit source) :inputs-sha256 (:inputs-sha256 source)
                  :files {"foo" 100.0}}
        scope-var (intern (create-ns 'agreement) 'application-facing (fn [] ["foo"]))
        run (fn [collection-fn names status]
              (with-redefs-fn
                {scope-var (fn [] names)
                 (ns-resolve 'postgres-reference 'validate-collection!) collection-fn
                 (ns-resolve 'postgres-agreement-runner 'comparison-command)
                 (fn [& _] ["bash" "-c" "cp -- \"$1\" \"$2\"; exit \"$3\""
                            "comparison-test" (str candidate) (str staged) (str status)])}
                #((ns-resolve 'postgres-agreement-runner 'compare!) cfg "measure" (str collection) {})))]
    (.mkdirs collection)
    (spit candidate (pr-str manifest))
    (spit baseline "old baseline")
    (is (zero? (run (fn [& _] receipt) ["foo"] 0)))
    (is (= manifest (edn/read-string (slurp baseline))))
    (doseq [[check names status] [[(fn [& _] receipt) ["foo"] 1]
                                  [(fn [& _] receipt) ["other"] 0]
                                  [(let [calls (atom 0)]
                                     (fn [& _] (if (= 1 (swap! calls inc)) receipt
                                                   (assoc receipt :changed true)))) ["foo"] 0]]]
      (spit baseline "old baseline")
      (try (run check names status) (catch clojure.lang.ExceptionInfo _))
      (is (= "old baseline" (slurp baseline))))
    (spit candidate (pr-str (assoc manifest :inputs-sha256 "wrong")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"provenance differs"
                          (run (fn [& _] receipt) ["foo"] 0)))
    (is (= "old baseline" (slurp baseline)))))

(deftest setup-file-is-measured-directly-without-duplicating-fixture-bootstrap
  (let [{:keys [env]} (cfg! "17")
        directory (io/file *temporary* "setup-collection")
        r (collect-through-wrapper (assoc env "AGREEMENT_FILES" "test_setup"
                                         "AGREEMENT_KEEP" (str directory)))
        receipt (edn/read-string (slurp (io/file directory "collection.edn")))
        run (edn/read-string (slurp (io/file directory "runs/test_setup/reference.edn")))]
    (is (zero? (:exit r)) (str (:out r) (:err r)))
    (is (= :complete (:status receipt)))
    (is (= #{"test_setup"} (set (keys (:outputs receipt)))))
    (is (= ["test_setup"] (:tests run)))
    (is (= "120s" (get-in run [:limits :statement-timeout])))
    (is (not (.exists (io/file directory "runs/test_setup/setup"))))))

(let [r (clojure.test/run-tests 'postgres-reference-test)]
  (System/exit (if (pos? (+ (:fail r) (:error r))) 1 0)))
