(ns datahike.test.pg-probe-fixtures-test
  "Probe-fixture regression suite. For every captured client SQL probe
   (under test/fixtures/probe-fixtures/*.edn), assert that
   `catalog/system-query?` still returns the kind we recorded.

   When a client upgrade changes the SQL it emits, the fixture file
   needs updating — but the diff makes the change visible. When our
   matcher silently stops recognising a SQL we previously fast-pathed,
   the test fails immediately with a precise pointer.

   Each fixture entry has shape:
     {:sql str
      :expected-system-kind <kw or nil>
      :note <str or nil>}

   `expected-system-kind` records what `system-query?` returns *today*.
   `:note` documents intent — `\"missed catalog probe — could be
   fast-pathed\"` flags entries we'd like to promote later by adding a
   shape matcher; `\"real data probe\"` flags entries whose slow-path
   routing is correct.

   Layered with `pg_probe_dispatch_test.clj`, which uses the same
   fixtures to assert the **handler-level** dispatch counter sees
   the right fast/slow ratio under a real handler."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.catalog :as catalog]))

(def ^:private fixture-dir "test/fixtures/probe-fixtures")

(defn- load-fixtures []
  (->> (.listFiles (io/file fixture-dir))
       (filter #(.endsWith (.getName ^java.io.File %) ".edn"))
       (mapv (fn [f]
               (assoc (edn/read-string (slurp f))
                      :filename (.getName ^java.io.File f))))))

(defn- short-preview [^String sql]
  (let [s (str/replace sql #"\s+" " ")]
    (subs s 0 (min 80 (count s)))))

(deftest probe-fixtures-route-correctly
  (doseq [{:keys [client version probes filename]} (load-fixtures)]
    (testing (format "%s (%s, %s)" filename client version)
      (doseq [{:keys [sql expected-system-kind]} probes]
        (testing (str "probe: " (short-preview sql))
          (is (= expected-system-kind (catalog/system-query? sql))))))))

(deftest probe-fixture-coverage-summary
  ;; Informational: fails (with a useful diff) only when the fast-path
  ;; coverage ratio would change unexpectedly, e.g. you added a matcher
  ;; without bumping the fixture's expected kind.
  ;;
  ;; This is NOT a guard against having too few matchers — it's a
  ;; tripwire that flags drift between fixture intent and matcher
  ;; behaviour.
  (doseq [{:keys [client probes filename]} (load-fixtures)]
    (testing (format "%s (%s)" filename client)
      (let [actual-fast (count (filter #(some? (catalog/system-query? (:sql %))) probes))
            expected-fast (count (filter :expected-system-kind probes))]
        (is (= expected-fast actual-fast)
            (format "fast-path-count drifted: fixture says %d, matcher reports %d. Update fixture or matcher in lockstep."
                    expected-fast actual-fast))))))
