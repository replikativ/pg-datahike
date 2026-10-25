#!/usr/bin/env bb
(ns postgres-fixtures-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(load-file "test/integration/postgres-regress/fixtures.clj")
(alias 'fixtures 'postgres-fixtures)
(alias 'reference 'postgres-reference)
(def ^:dynamic *temporary* nil)
(defn file! [path text] (io/make-parents path) (spit path text) path)
(defn command! [& args]
  (let [result (apply sh/sh args)]
    (when-not (zero? (:exit result)) (throw (ex-info (:err result) result))) result))
(defn git! [dir & args] (apply command! "git" "-C" (str dir) args))
(defn commit! [dir]
  (git! dir "add" ".")
  (git! dir "-c" "user.name=Fixture Test" "-c" "user.email=fixture@example.invalid" "commit" "-qm" "fixture"))
(use-fixtures :each
  (fn [f]
    (let [dir (.toFile (Files/createTempDirectory "pgdh-fixture-test-" (make-array FileAttribute 0)))]
      (try (binding [*temporary* dir] (f))
           (finally (doseq [file (reverse (file-seq dir))] (.delete file)))))))

(defn fixture! []
  (let [root (.toFile (Files/createTempDirectory (.toPath *temporary*) "case-" (make-array FileAttribute 0)))
        source (io/file root "source")
        project (io/file root "project")
        here (io/file project "test/integration/postgres-regress")
        tools (io/file root "tools")
        output (io/file root "output")
        log (io/file root "calls.log")
        data {:format 1 :postgres-ref "REL_18_6" :setup "test_setup"
              :baseline-qualified? false :core-only-targets #{}
              :prerequisites {"foo" ["bar" "baz"] "baz" ["bar"]}
              :contexts {"bar" :native-helper-boundary}}]
    (doseq [name ["test_setup" "foo" "bar" "baz"]]
      (file! (io/file source "src/test/regress/sql" (str name ".sql")) "SELECT 1;\n")
      (file! (io/file source "src/test/regress/expected" (str name ".out")) "checked output\n"))
    (file! (io/file source "src/test/regress/regress.c") "/* native fixture source */\n")
    (file! (io/file source "src/test/regress/parallel_schedule") "test: test_setup bar baz foo\n")
    (git! source "init" "-q") (commit! source) (git! source "tag" "REL_18_6")
    (doseq [name ["reference.clj" "reference.sh" "fixtures.clj" "run.sh" "run-fixture-phases.sh"
                  "run-with-api-fixtures.sh" "bootstrap-api.sh"]]
      (file! (io/file here name) (slurp (io/file reference/repo-root "test/integration/postgres-regress" name))))
    (file! (io/file here "profiles.edn")
           "{:default \"18\" :profiles {\"18\" {:major 18 :postgres-ref \"REL_18_6\" :fixtures \"fixtures.edn\"}}}")
    (file! (io/file here "fixtures.edn") (pr-str data))
    (file! (io/file project "bb.edn") "{}")
    (file! (io/file project ".circleci/config.yml") "{}")
    (git! project "init" "-q") (commit! project)
    (file! (io/file tools "psql")
           (str "#!/usr/bin/env bash\nset -eu\n"
                "if [[ \"${1:-}\" == --version ]]; then echo 'psql (PostgreSQL) 18.6'; exit 0; fi\n"
                "printf 'psql %s\\n' \"$*\" >> \"$FIXTURE_TEST_LOG\"\n"
                "case \"$*\" in\n"
                " *pg_roles*) if [[ -f \"$FIXTURE_TEST_LOG.role\" ]]; then echo leaked_role; fi ;;\n"
                " *pg_tablespace*) if [[ -f \"$FIXTURE_TEST_LOG.tablespace\" ]]; then echo 1; else echo 0; fi ;;\n"
                " *DROP*TABLESPACE*) rm -f \"$FIXTURE_TEST_LOG.tablespace\" ;;\n"
                " *current_setting*) printf '%s.6\\t%s0006\\tUTC\\tUTF8\\n' \"${FIXTURE_TEST_SERVER_MAJOR:-18}\" \"${FIXTURE_TEST_SERVER_MAJOR:-18}\" ;;\n"
                " *CREATE*DATABASE*) if [[ \"${FIXTURE_TEST_CREATE_FAIL:-0}\" == 1 ]]; then exit 1; fi ;;\n"
                " *DROP*DATABASE*) if [[ \"${FIXTURE_TEST_HANG_CLEANUP:-0}\" == 1 ]]; then echo $$ > \"$FIXTURE_TEST_LOG.hung\"; trap '' TERM; while :; do sleep 1; done; fi; if [[ \"${FIXTURE_TEST_CLEANUP_FAIL:-0}\" == 1 ]]; then exit 1; fi ;;\n"
                " *count*onek*) if [[ \"${FIXTURE_TEST_CORE_FAIL:-0}\" == 1 ]]; then echo broken; else echo '1000|1000|10000|10000|58|6|5|3'; fi ;;\n"
                "esac\n"))
    (file! (io/file tools "pg_regress")
           (str "#!/usr/bin/env bash\nset -eu\n"
                "if [[ \"${1:-}\" == --version ]]; then echo 'pg_regress (PostgreSQL) 18.6'; exit 0; fi\n"
                "printf 'driver %s\\n' \"$*\" >> \"$FIXTURE_TEST_LOG\"\n"
                "names=()\nfor arg in \"$@\"; do case \"$arg\" in\n"
                " --outputdir=*) out=\"${arg#*=}\" ;;\n --inputdir=*) input=\"${arg#*=}\" ;;\n --*) ;;\n *) names+=(\"$arg\") ;;\nesac; done\n"
                "if [[ \"${FIXTURE_TEST_ORACLE:-0}\" == 1 && \" ${names[*]} \" == *\" test_setup \"* ]]; then touch \"$FIXTURE_TEST_LOG.tablespace\"; fi\n"
                "if [[ \"${FIXTURE_TEST_LEAK_ROLE:-0}\" == 1 ]]; then touch \"$FIXTURE_TEST_LOG.role\"; fi\n"
                "mkdir -p \"$out/results\"\nfor name in \"${names[@]}\"; do cp \"$input/expected/$name.out\" \"$out/results/$name.out\"; done\n"
                "if [[ \" ${names[*]} \" == *\" ${FIXTURE_TEST_DIFF:-never} \"* ]]; then exit 1; fi\n"))
    (.setExecutable (io/file tools "psql") true)
    (.setExecutable (io/file tools "pg_regress") true)
    (let [env (merge (apply dissoc (into {} (System/getenv))
                            ["PG_REGRESS_SRC" "PG_REGRESS_ALLOW_UNPINNED" "PG_REGRESS_FIXTURE_ENV"
                             "PG_REGRESS_TIMEOUT" "PG_REGRESS_STRICT" "PG_REGRESS_API_STRICT"])
                     {"PG_REGRESS_PROFILE" "18" "POSTGRES_SOURCE" (str source)
                      "PG_REGRESS_BINDIR" (str tools) "PG_REGRESS_BIN" (str (io/file tools "pg_regress"))
                      "PG_REGRESS_OUTPUT" (str output) "FIXTURE_TEST_LOG" (str log)
                      "PG_REGRESS_HOST" "127.0.0.1" "PG_REGRESS_PORT" "15472"})]
      (let [path (file! (io/file root "target-context.edn")
                        (pr-str {:format 1 :postgres-ref "REL_18_6" :role :target :exclusive? true
                                 :endpoint {:host "127.0.0.1" :port "15472"}}))
            env (assoc env "PG_REGRESS_FIXTURE_ENV" (str path))]
        {:cfg (reference/config project env) :env env :data data :project project
         :here here :output output :source source :log log}))))

(defn context! [cfg role]
  (let [directory (io/file *temporary* "native")
        library (file! (io/file directory "regress.so") "native fixture bytes")
        manifest (file! (io/file directory "build-manifest.edn")
                        (pr-str {:format 1 :postgres-ref "REL_18_6"
                                 :source-commit (:commit (reference/validate-source! cfg))
                                 :source-file {:path (str (io/file (:source cfg) "src/test/regress/regress.c"))
                                               :sha256 (reference/file-sha256 (io/file (:source cfg) "src/test/regress/regress.c"))}
                                 :libraries {"regress.so" (reference/file-sha256 library)}}))
        path (file! (io/file *temporary* "environment.edn")
                    (pr-str {:format 1 :postgres-ref "REL_18_6" :role role :exclusive? true
                             :endpoint (select-keys (:target cfg) [:host :port])
                             :native {:directory (str directory) :manifest (str manifest)}}))]
    (str path)))

(defn run-wrapper [env project & targets]
  (let [role (some-> (get env "PG_REGRESS_FIXTURE_ENV") fixtures/read-edn :role)
        env (assoc env "FIXTURE_TEST_ORACLE" (if (= :oracle role) "1" "0"))]
    (apply sh/sh "bash" "test/integration/postgres-regress/run-with-api-fixtures.sh"
           (concat targets [:env env :dir (str project)]))))

(deftest deterministic-prerequisite-closure-rejects-invalid-graphs
  (let [{:keys [cfg data]} (fixture!)]
    (is (= ["test_setup" "bar" "baz" "foo"] (fixtures/ordered-closure cfg data ["foo"])))
    (is (= ["test_setup" "bar" "baz" "foo"] (fixtures/ordered-closure cfg data ["baz" "foo"])))
    (doseq [bad [(assoc-in data [:prerequisites "bar"] ["foo"])
                 (assoc-in data [:prerequisites "foo"] ["foo"])
                 (assoc-in data [:prerequisites "foo"] ["bar" "bar"])
                 (assoc-in data [:prerequisites "foo"] ["missing"])
                 (assoc-in data [:prerequisites "foo"] ["test_setup"])]]
      (is (thrown? clojure.lang.ExceptionInfo (fixtures/ordered-closure cfg bad ["foo"]))))
    (is (thrown? clojure.lang.ExceptionInfo (fixtures/ordered-closure cfg data ["foo" "foo"])))))

(deftest native-context-is-endpoint-release-and-content-bound
  (let [{:keys [cfg]} (fixture!)
        path (context! cfg :oracle)
        env {"PG_REGRESS_FIXTURE_ENV" path}]
    (is (= :oracle (:role (fixtures/context cfg env))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (fixtures/context (assoc-in cfg [:target :port] "wrong") env)))
    (file! (io/file *temporary* "native/regress.so") "changed binary")
    (is (thrown? clojure.lang.ExceptionInfo (fixtures/context cfg env)))))

(deftest wrapper-retains-independent-phases-and-complete-hash-chain
  (let [{:keys [cfg env project output log]} (fixture!)
        env (assoc env "PG_REGRESS_FIXTURE_ENV" (context! cfg :oracle))
        result (run-wrapper env project "foo")]
    (is (zero? (:exit result)) (str (:out result) (:err result)))
    (let [evidence (fixtures/validate! cfg output)
          calls (slurp log)]
      (is (= :complete (:transport-status evidence)))
      (is (= :ready (:readiness evidence)))
      (is (= ["test_setup" "bar" "baz" "foo"] (mapv :name (:phases evidence))))
      (is (= 4 (count (set (map :run-id (:phases evidence))))))
      (is (= 1 (count (re-seq #"(?m)^driver .* bar(?: |$)" calls))))
      (is (= 4 (count (re-seq #"--dlpath=" calls))))
      (is (= #{(:database evidence)} (set (map second (re-seq #"--dbname=([^ ]+)" (str/join "\n" (filter #(str/starts-with? % "driver ") (str/split-lines calls))))))))
      (is (.isFile (io/file output "prerequisites/bar/reference.edn")))
      (is (.isFile (io/file output "tests/reference.edn")))
      (is (= :complete (get-in evidence [:cleanup :status])))
      (is (true? (:tablespace-removed? (fixtures/read-edn (io/file output "fixture-global-cleanup.edn")))))
      (file! (io/file output "prerequisites/bar/results/bar.out") "tampered prerequisite")
      (is (thrown? clojure.lang.ExceptionInfo (fixtures/validate! cfg output))))))

(deftest semantic-prerequisite-differences-remain-raw-with-explicit-limitations
  (let [{:keys [cfg env project output]} (fixture!)
        env (assoc env "PG_REGRESS_FIXTURE_ENV" (context! cfg :target)
                   "FIXTURE_TEST_SERVER_MAJOR" "17" "FIXTURE_TEST_DIFF" "bar")
        result (run-wrapper env project "foo")]
    (is (zero? (:exit result)) (str (:out result) (:err result)))
    (let [evidence (fixtures/validate! cfg output)]
      (is (= :complete (:transport-status evidence)))
      (is (= :fixture-limited (:readiness evidence)))
      (is (= 1 (:pg-regress-exit (second (:phases evidence)))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (fixtures/assert-qualified! cfg {:fixture-evidence {"foo" evidence}} "measure")))
      (is (nil? (fixtures/assert-qualified! cfg {} "probe"))))))

(deftest cleanup-and-bootstrap-failures-cannot-be-promoted-to-healthy-fixtures
  (let [{:keys [cfg env project output]} (fixture!)
        result (run-wrapper (assoc env "FIXTURE_TEST_CLEANUP_FAIL" "1") project "foo")]
    (is (not (zero? (:exit result))))
    (let [evidence (fixtures/validate! cfg output)]
      (is (= :complete (:transport-status evidence)))
      (is (= :failed (get-in evidence [:cleanup :status])))
      (is (= :incomplete (:outcome-status evidence)))
      (is (= :unverified (:readiness evidence)))))
  (let [{:keys [cfg env project output]} (fixture!)
        result (run-wrapper (assoc env "FIXTURE_TEST_CORE_FAIL" "1") project "foo")]
    (is (not (zero? (:exit result))))
    (let [evidence (fixtures/read-edn (io/file output "fixture.edn"))]
      (is (= :incomplete (:transport-status evidence)))
      (is (= :missing-environment (:readiness evidence)))
      (is (not (.exists (io/file output "tests/reference.edn")))))
    (is (thrown? clojure.lang.ExceptionInfo (fixtures/validate! cfg output)))))

(deftest oracle-version-and-preexisting-global-context-fail-before-sql
  (let [{:keys [cfg env project output log]} (fixture!)
        env (assoc env "PG_REGRESS_FIXTURE_ENV" (context! cfg :oracle)
                   "FIXTURE_TEST_SERVER_MAJOR" "17")
        result (run-wrapper env project "foo")]
    (is (not (zero? (:exit result))))
    (is (str/includes? (:err result) "oracle server major"))
    (is (not (str/includes? (slurp log) "driver "))))
  (let [{:keys [cfg env project output log]} (fixture!)
        env (assoc env "PG_REGRESS_FIXTURE_ENV" (context! cfg :oracle))]
    (file! (str log ".tablespace") "preexisting")
    (let [result (run-wrapper env project "foo")]
      (is (not (zero? (:exit result))))
      (is (.exists (io/file (str log ".tablespace"))))
      (is (not (str/includes? (slurp log) "DROP TABLESPACE")))
      (is (not (str/includes? (slurp log) "driver "))))))

(deftest collection-revalidates-every-phase-and-retains-raw-readiness
  (let [{:keys [cfg env project]} (fixture!)
        collection (io/file *temporary* "collection")
        run-dir (io/file collection "runs/foo")
        result (run-wrapper (assoc env "PG_REGRESS_OUTPUT" (str run-dir)) project "foo")]
    (is (zero? (:exit result)) (str (:out result) (:err result)))
    (reference/collect-result! cfg (io/file run-dir "tests") "foo" collection)
    (reference/finish-collection! cfg collection ["foo"] true)
    (let [receipt (reference/validate-collection! cfg collection)]
      (is (= :unverified (get-in receipt [:fixture-evidence "foo" :readiness])))
      (is (nil? (fixtures/assert-qualified! cfg receipt "probe")))
      (is (thrown? clojure.lang.ExceptionInfo (fixtures/assert-qualified! cfg receipt "measure"))))
    (file! (io/file run-dir "prerequisites/bar/results/bar.out") "tampered prerequisite")
    (is (thrown? clojure.lang.ExceptionInfo (reference/validate-collection! cfg collection)))))

(deftest setup-as-target-and-multiple-targets-are-executed-exactly-once
  (let [{:keys [cfg env project output]} (fixture!)
        result (run-wrapper env project "test_setup")]
    (is (zero? (:exit result)) (str (:out result) (:err result)))
    (let [evidence (fixtures/validate! cfg output)]
      (is (= ["test_setup"] (mapv :name (:phases evidence))))
      (is (:target? (first (:phases evidence))))
      (is (.isFile (io/file output "tests/reference.edn")))))
  (let [{:keys [cfg env project output]} (fixture!)
        result (run-wrapper env project "baz" "foo")]
    (is (zero? (:exit result)) (str (:out result) (:err result)))
    (let [evidence (fixtures/validate! cfg output)]
      (is (= ["test_setup" "bar" "baz" "foo"] (mapv :name (:phases evidence))))
      (is (= ["baz" "foo"] (:targets evidence)))
      (is (.isFile (io/file output "targets/baz/reference.edn")))
      (is (.isFile (io/file output "targets/foo/reference.edn"))))))

(deftest hung-cleanup-is-bounded-and-never-recorded-as-success
  (let [{:keys [env project output log]} (fixture!)
        unrelated (.start (ProcessBuilder. ^java.util.List ["sleep" "30"]))
        started (System/nanoTime)]
    (try
      (let [result (sh/sh "timeout" "--kill-after=0.2s" "8s" "bash"
                          "test/integration/postgres-regress/run-with-api-fixtures.sh" "foo"
                          :env (assoc env "FIXTURE_TEST_HANG_CLEANUP" "1") :dir (str project))
            evidence (fixtures/read-edn (io/file output "fixture.edn"))]
        (is (not (zero? (:exit result))))
        (is (.exists (io/file (str log ".hung"))))
        (is (< (/ (- (System/nanoTime) started) 1e9) 15))
        (is (.isAlive unrelated))
        (is (= :incomplete (:transport-status evidence)))
        (is (= :running (get-in evidence [:cleanup :status]))))
      (finally (.destroyForcibly unrelated) (.waitFor unrelated)))))

(deftest fixture-execution-refuses-missing-or-shared-context-before-any-sql
  (let [{:keys [env cfg project log]} (fixture!)]
    (doseq [bad [(dissoc env "PG_REGRESS_FIXTURE_ENV")
                 (let [path (file! (io/file *temporary* "shared.edn")
                                   (pr-str {:format 1 :postgres-ref "REL_18_6" :role :target
                                            :exclusive? false :endpoint (select-keys (:target cfg) [:host :port])}))]
                   (assoc env "PG_REGRESS_FIXTURE_ENV" (str path)))]]
      (let [result (run-wrapper bad project "foo")]
        (is (not (zero? (:exit result))))
        (is (str/includes? (:err result) "explicit endpoint-bound exclusive context"))
        (is (not (.exists log)))))))

(deftest phase-endpoint-cannot-be-rebound-to-another-server
  (let [{:keys [cfg env project output]} (fixture!)
        result (run-wrapper env project "foo")
        path (io/file output "prerequisites/bar/reference.edn")]
    (is (zero? (:exit result)) (str (:out result) (:err result)))
    (spit path (pr-str (assoc-in (fixtures/read-edn path) [:target :endpoint :port] "different")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"another endpoint"
                          (fixtures/validate! cfg output)))))

(deftest measured-baseline-requires-reviewed-fixture-provenance
  (let [{:keys [cfg]} (fixture!)
        measured {:postgres-ref "REL_18_6" :profile "18" :status :measured :files {"foo" 100.0}}
        qualified (assoc measured :fixture-qualification :ready
                         :fixture-descriptor-sha256 (reference/file-sha256 (:fixtures cfg)))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"fixture qualification/provenance"
                          (reference/validate-baseline! cfg measured)))
    (is (nil? (reference/validate-baseline! cfg qualified)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (reference/validate-baseline! cfg (assoc qualified :fixture-descriptor-sha256 "changed"))))))

(deftest leaked-global-state-keeps-raw-output-but-fails-the-overall-run
  (let [{:keys [cfg env project output]} (fixture!)
        result (run-wrapper (assoc env "FIXTURE_TEST_LEAK_ROLE" "1") project "foo")]
    (is (not (zero? (:exit result))))
    (let [evidence (fixtures/validate! cfg output)]
      (is (= :complete (:transport-status evidence)))
      (is (= :incomplete (:outcome-status evidence)))
      (is (= :unverified (get-in evidence [:global-cleanup :status])))
      (is (.isFile (io/file output "tests/results/foo.out"))))))

(deftest qualification-never-promotes-oracle-unverified-or-empty-evidence
  (with-redefs [fixtures/descriptor (constantly {:baseline-qualified? true})]
    (let [ready {:outputs {"foo" "hash"} :fixture-evidence {"foo" {:role :target :readiness :ready}}}]
      (is (nil? (fixtures/assert-qualified! {} ready "measure")))
      (doseq [bad [(assoc-in ready [:fixture-evidence "foo" :role] :oracle)
                   (assoc-in ready [:fixture-evidence "foo" :readiness] :unverified)
                   {:outputs {} :fixture-evidence {}}]]
        (is (thrown? clojure.lang.ExceptionInfo (fixtures/assert-qualified! {} bad "measure")))))))

(deftest identical-descriptor-content-can-be-replayed-after-checkout-moves
  (let [{:keys [cfg env project output]} (fixture!)
        result (run-wrapper env project "foo")
        moved (file! (io/file *temporary* "moved-descriptor.edn") (slurp (:fixtures cfg)))]
    (is (zero? (:exit result)) (str (:out result) (:err result)))
    (.delete (io/file (:fixtures cfg)))
    (is (= :complete (:outcome-status (fixtures/validate! (assoc cfg :fixtures (str moved)) output))))))

(deftest intake-stops-after-uncertain-resource-outcome-and-records-unattempted-files
  (let [collect-in-order (do (load-file "test/integration/postgres-regress/agreement-runner.clj")
                             (ns-resolve 'postgres-agreement-runner 'collect-in-order))
        calls (atom [])
        run-file (fn [name]
                   (swap! calls conj name)
                   {:test name :status (if (= name "bad") :incomplete :complete)
                    :fixture-readiness :fixture-limited})
        outcomes (collect-in-order ["good" "bad" "later"] run-file true)]
    (is (= ["good" "bad"] @calls))
    (is (= [:complete :incomplete :unattempted] (mapv :status outcomes)))
    (reset! calls [])
    (collect-in-order ["good" "bad" "later"] run-file false)
    (is (= ["good" "bad" "later"] @calls))))

(deftest unsuccessful-create-never-claims-or-drops-an-unproven-database
  (let [{:keys [env project output log]} (fixture!)
        result (run-wrapper (assoc env "FIXTURE_TEST_CREATE_FAIL" "1") project "foo")
        evidence (fixtures/read-edn (io/file output "fixture.edn"))]
    (is (not (zero? (:exit result))))
    (is (= :unproven (get-in evidence [:cleanup :status])))
    (is (= :incomplete (:outcome-status evidence)))
    (is (not (str/includes? (slurp log) "DROP DATABASE")))))

(let [result (clojure.test/run-tests 'postgres-fixtures-test)]
  (System/exit (if (pos? (+ (:fail result) (:error result))) 1 0)))
