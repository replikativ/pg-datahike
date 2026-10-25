#!/usr/bin/env bb
(ns postgres-inventory-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is use-fixtures]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(load-file "test/integration/postgres-regress/reference.clj")
(load-file "test/integration/postgres-regress/inventory.clj")
(alias 'reference 'postgres-reference)
(alias 'inventory 'postgres-inventory)

(def ^:dynamic *temporary* nil)
(defn file! [file text] (io/make-parents file) (spit file text) file)
(defn git! [dir & args]
  (let [result (apply sh/sh "git" "-C" (str dir) args)]
    (when-not (zero? (:exit result)) (throw (ex-info (:err result) result)))))

(use-fixtures :each
  (fn [f]
    (let [dir (.toFile (Files/createTempDirectory "pgdh-inventory-test-" (make-array FileAttribute 0)))]
      (try (binding [*temporary* dir] (f))
           (finally (doseq [file (reverse (file-seq dir))] (.delete file)))))))

(defn fixture! []
  (let [source (io/file *temporary* "source")
        regress (io/file source "src/test/regress")
        manifest-file (io/file *temporary* "inventory.edn")
        campaign {:waves [{:id 1 :tests [{:name "foo" :mode :unmeasured}]}]}
        scope {:backlog {:client #{"test_setup" "numeric_big"}}
               :out-of-scope {:physical #{"reindex_catalog"}}}
        ledger {:capabilities {:routines {:status :unverified}} :evidence []}]
    (file! (io/file regress "parallel_schedule") "test: test_setup foo\n")
    (doseq [name ["foo" "test_setup" "numeric_big" "reindex_catalog"]]
      (file! (io/file regress "sql" (str name ".sql")) "SELECT 1;\n")
      (file! (io/file regress "expected" (str name ".out")) "1\n"))
    (file! (io/file regress "expected/foo_1.out") "platform variant\n")
    (git! source "init" "-q")
    (git! source "add" ".")
    (git! source "-c" "user.name=Inventory Test" "-c" "user.email=inventory@example.invalid"
          "commit" "-qm" "reference")
    (git! source "tag" "REL_18_6")
    (let [cfg (assoc (reference/config reference/repo-root
                                       {"PG_REGRESS_PROFILE" "18" "POSTGRES_SOURCE" (str source)})
                     :inventory (str manifest-file))
          declared (into {} (inventory/dispositions campaign scope))
          expected (into (sorted-map) (for [file (.listFiles (io/file regress "expected"))]
                                        [(.getName file) (reference/file-sha256 file)]))
          manifest {:format 1 :postgres-major 18 :postgres-ref "REL_18_6"
                    :reference (reference/validate-source! cfg)
                    :scheduled-count 2 :sql-count 4 :admission :unverified
                    :schedule-sha256 (reference/file-sha256 (io/file regress "parallel_schedule"))
                    :expected-files expected
                    :ordering-prerequisites {"foo" ["test_setup"]}
                    :surface-decisions {"foo" {:scope :campaign :source-lines [1 1] :basis "Client SQL."}}
                    :dispositions (vec (for [[name file] (inventory/sql-files (str regress))]
                                         (merge {:name name :scheduled? (contains? #{"foo" "test_setup"} name)
                                                 :sql-sha256 (reference/file-sha256 file)
                                                 :expected-sha256 (get expected (str name ".out"))}
                                                (get declared name))))}]
      (file! manifest-file (pr-str manifest))
      {:cfg cfg :campaign campaign :scope scope :ledger ledger
       :manifest manifest :manifest-file manifest-file :regress regress})))

(deftest complete-inventory-distinguishes-scheduled-sql-and-expected-variants
  (let [{:keys [cfg campaign scope ledger]} (fixture!)
        result (inventory/validate! cfg campaign scope ledger)]
    (is (= 2 (:scheduled-count result)))
    (is (= 4 (:sql-count result)))
    (is (= 5 (:expected-count result)))
    (is (= 3 (:application-count result)))
    (is (= ["numeric_big" "reindex_catalog"] (:unscheduled result)))))

(deftest stale-ambiguous-and-cross-reference-inventories-fail-closed
  (let [{:keys [cfg campaign scope ledger manifest manifest-file]} (fixture!)]
    (doseq [bad [(assoc manifest :postgres-major 17)
                 (assoc manifest :postgres-ref "REL_17_7")
                 (assoc manifest :schedule-sha256 "wrong")
                 (assoc manifest :scheduled-count 3)
                 (assoc manifest :sql-count 3)
                 (update manifest :expected-files dissoc "foo_1.out")
                 (update manifest :dispositions pop)
                 (update manifest :dispositions conj (first (:dispositions manifest)))
                 (assoc-in manifest [:dispositions 0 :scheduled?] false)
                 (assoc-in manifest [:dispositions 0 :sql-sha256] "wrong")
                 (assoc-in manifest [:dispositions 0 :expected-sha256] "wrong")
                 (assoc-in manifest [:dispositions 0 :status] :backlog)
                 (assoc manifest :ordering-prerequisites {"foo" ["missing"]})
                 (assoc-in manifest [:surface-decisions "foo" :source-lines] [1 2])
                 (assoc-in manifest [:surface-decisions "foo" :scope] :out-of-scope)]]
      (spit manifest-file (pr-str bad))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"audited inventory"
                            (inventory/validate! cfg campaign scope ledger)) (pr-str bad)))))

(deftest inherited-claims-and-overlapping-dispositions-are-rejected
  (let [{:keys [cfg campaign scope ledger]} (fixture!)]
    (doseq [[c s l] [[(assoc-in campaign [:waves 0 :tests 0 :mode] :strict) scope ledger]
                     [(assoc-in campaign [:waves 0 :tests 0 :strict-slices] [{:id :old}]) scope ledger]
                     [campaign scope (assoc-in ledger [:capabilities :routines :status] :target)]
                     [campaign scope (assoc ledger :evidence [{:status :admitted}])]
                     [campaign (assoc-in scope [:backlog :overlap] #{"foo"}) ledger]
                     [campaign (assoc-in scope [:backlog :absent] #{"removed"}) ledger]]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"audited inventory"
                            (inventory/validate! cfg c s l))))))

(deftest actual-source-and-expected-input-mutations-invalidate-the-audit
  (let [{:keys [cfg campaign scope ledger regress]} (fixture!)]
    (doseq [[relative text] [["sql/foo.sql" "SELECT 2;\n"]
                             ["sql/new.sql" "SELECT 1;\n"]
                             ["expected/foo_1.out" "changed variant\n"]
                             ["parallel_schedule" "test: test_setup foo numeric_big\n"]]]
      (let [file (io/file regress relative)
            before (when (.exists file) (slurp file))]
        (file! file text)
        (is (thrown? clojure.lang.ExceptionInfo (inventory/validate! cfg campaign scope ledger)))
        (if before (spit file before) (.delete file))))))

(deftest shared-campaign-entrypoint-validates-audit-before-agreement-or-waves
  (let [{:keys [cfg campaign scope ledger manifest manifest-file]} (fixture!)
        paths (into {} (for [[kind data] [[:campaign (assoc campaign :postgres-major 18 :postgres-ref "REL_18_6")]
                                          [:scope scope]
                                          [:ledger (assoc ledger :postgres-major 18 :postgres-ref "REL_18_6")]]]
                         [kind (str (file! (io/file *temporary* (str (name kind) ".edn")) (pr-str data)))]))
        config (merge cfg paths)]
    (is (= 4 (:sql-count (reference/require-campaign! config))))
    (spit manifest-file (pr-str (assoc manifest :sql-count 3)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"audited inventory"
                          (reference/require-campaign! config)))))

(let [result (clojure.test/run-tests 'postgres-inventory-test)]
  (System/exit (if (pos? (+ (:fail result) (:error result))) 1 0)))
