(ns datahike.test.cross-engine
  "Cross-engine differential runner.

   Runs the same sqllogictest `.test` file against two pgwire endpoints
   over JDBC and diffs the row-sets. Use to compare Datahike-pgwire's
   behavior with a real PostgreSQL's for the same SQL, isolating
   dialect-drift bugs from engine-execution bugs.

   Both endpoints connect via `org.postgresql.Driver`, so the test
   harness stays identical:

     REFERENCE_URL=\"jdbc:postgresql://localhost:5432/test?user=pg\"
     TARGET_URL=\"jdbc:postgresql://localhost:15432/datahike?user=datahike\"

     clojure -M:test -m datahike.test.cross-engine \\
       test/sqllogictest/test_select.test

   Exit 0 → every query agreed on result set (modulo `rowsort`/`nosort`
   mode). Exit 1 → at least one disagreement, printed to stdout.

   What's expected to diverge:
   - tie-order in `nosort` ORDER BY
   - error messages for syntax errors (worded differently)
   - data-type inference for untyped literals

   Bug-worthy divergences:
   - row sets disagree on a non-error query
   - target throws where reference succeeds (or vice versa)

   This runner is a dev + triage tool; not wired into CI (requires a
   running external PG). Use for feature-bug triage and to generate
   new sqllogictest files from real-PG behavior:
     REFERENCE_URL=... ./cross-engine --record new-case.sql"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.sql DriverManager Connection Statement ResultSet
            ResultSetMetaData]
           [java.util Properties]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; JDBC plumbing

(defn- open-conn ^Connection [^String url]
  (Class/forName "org.postgresql.Driver")
  (DriverManager/getConnection url (Properties.)))

(defn- row->cells
  "Convert one JDBC row to a vector of String cells, mapping NULL to
   the literal 'NULL' so output matches sqllogictest convention."
  [^ResultSet rs n-cols]
  (into []
        (for [i (range 1 (inc n-cols))]
          (if-let [v (.getObject rs (int i))]
            (str v)
            "NULL"))))

(defn- execute
  "Run one SQL statement. Returns
     {:rows  [[...] [...]]}    — for SELECT
     {:updated n}               — for DML
     {:error  \"msg\" :sqlstate \"25xxx\"}
   The same shape for both drivers so a straight (=) compare works."
  [^Connection c ^String sql]
  (try
    (with-open [^Statement st (.createStatement c)]
      (if (.execute st sql)
        (with-open [^ResultSet rs (.getResultSet st)]
          (let [md (.getMetaData rs)
                n  (.getColumnCount md)]
            {:rows (loop [out (transient [])]
                     (if (.next rs)
                       (recur (conj! out (row->cells rs n)))
                       (persistent! out)))}))
        {:updated (.getUpdateCount st)}))
    (catch java.sql.SQLException e
      {:error (.getMessage e) :sqlstate (.getSQLState e)})))

;; ---------------------------------------------------------------------------
;; Test-file driver

(defn- split-statements
  "Parse a sqllogictest-style file into a seq of maps:
     {:type :statement :expect :ok|:error :sql str}
     {:type :query     :types str :sort str :sql str :expected [str ...]}
   Re-uses the grammar from datahike.test.sqllogictest-runner but
   returns a flat, driver-agnostic spec — we don't execute here."
  [content]
  (let [lines (str/split-lines content)]
    (loop [i 0 out []]
      (if (>= i (count lines))
        out
        (let [ln (str/trim (nth lines i))]
          (cond
            (or (str/blank? ln) (str/starts-with? ln "#"))
            (recur (inc i) out)

            (str/starts-with? ln "statement")
            (let [expect (if (str/includes? ln "error") :error :ok)
                  [parts next-i]
                  (loop [j (inc i) acc []]
                    (if (or (>= j (count lines))
                            (str/blank? (nth lines j))
                            (str/starts-with? (str/trim (nth lines j)) "#")
                            (str/starts-with? (str/trim (nth lines j)) "statement")
                            (str/starts-with? (str/trim (nth lines j)) "query"))
                      [acc j]
                      (recur (inc j) (conj acc (nth lines j)))))]
              (recur next-i
                     (conj out {:type :statement :expect expect
                                :sql (str/trim (str/join " " parts))})))

            (str/starts-with? ln "query")
            (let [toks (str/split ln #"\s+")
                  types (if (> (count toks) 1) (nth toks 1) "T")
                  sort-mode (if (> (count toks) 2) (nth toks 2) "nosort")
                  [sql-parts sep-i]
                  (loop [j (inc i) acc []]
                    (if (or (>= j (count lines))
                            (= (str/trim (nth lines j)) "----"))
                      [acc (inc j)]
                      (recur (inc j) (conj acc (nth lines j)))))
                  [exp-parts next-i]
                  (loop [j sep-i acc []]
                    (if (or (>= j (count lines))
                            (str/blank? (nth lines j)))
                      [acc j]
                      (recur (inc j) (conj acc (nth lines j)))))]
              (recur next-i
                     (conj out {:type :query :types types :sort sort-mode
                                :sql (str/trim (str/join " " sql-parts))
                                :expected (vec exp-parts)})))

            :else (recur (inc i) out)))))))

;; ---------------------------------------------------------------------------
;; Row-set comparison

(defn- canonicalize
  "Sort rows per sqllogictest mode so we can compare row-sets that
   are only meaningfully equal up to the declared ordering."
  [rows mode]
  (case mode
    "rowsort"   (vec (sort-by pr-str rows))
    "valuesort" (vec (sort (mapcat identity rows)))
    ;; nosort or unknown — preserve order
    rows))

(defn- diff-result
  "Return nil when ref/target results match (accounting for sort mode),
   otherwise a map describing the first divergence."
  [ref target sort-mode]
  (cond
    (not= (boolean (:error ref)) (boolean (:error target)))
    {:kind :error-mismatch :ref ref :target target}

    (and (:error ref) (:error target))
    nil  ;; both erred — consider equivalent (SQLSTATE compare left to another tool)

    (:rows ref)
    (let [rr (canonicalize (:rows ref) sort-mode)
          tr (canonicalize (:rows target) sort-mode)]
      (when (not= rr tr)
        {:kind :rows-differ
         :ref-rows rr
         :target-rows tr
         :only-in-ref (vec (remove (set tr) rr))
         :only-in-target (vec (remove (set rr) tr))}))

    :else nil))

;; ---------------------------------------------------------------------------
;; Top-level

(defn run-file
  "Run one sqllogictest file against both endpoints. Returns a map:
     {:passed  n :failed n :diffs [...] :errored [...]}
   where each diff is `{:sql str :sort str :diff <result of diff-result>}`."
  [^String ref-url ^String target-url ^String file-path]
  (let [specs (split-statements (slurp file-path))]
    (with-open [ref  (open-conn ref-url)
                tgt  (open-conn target-url)]
      (reduce
       (fn [acc spec]
         (let [ref-r (execute ref (:sql spec))
               tgt-r (execute tgt (:sql spec))]
           (case (:type spec)
             :statement
             ;; We only flag statements where one side threw and the
             ;; other succeeded. Divergent error wording is fine.
             (if (not= (boolean (:error ref-r)) (boolean (:error tgt-r)))
               (update acc :diffs conj
                       {:sql (:sql spec) :kind :statement-disagree
                        :ref ref-r :target tgt-r})
               (update acc :passed inc))

             :query
             (if-let [d (diff-result ref-r tgt-r (:sort spec))]
               (-> acc
                   (update :failed inc)
                   (update :diffs conj (assoc d :sql (:sql spec) :sort (:sort spec))))
               (update acc :passed inc)))))
       {:passed 0 :failed 0 :diffs []}
       specs))))

(defn -main
  "CLI: clojure -M:test -m datahike.test.cross-engine <file.test>...

   Reads REFERENCE_URL + TARGET_URL from the environment. Each argument
   is a .test file path. Exits 0 iff every query agreed."
  [& args]
  (let [ref-url (or (System/getenv "REFERENCE_URL")
                    (do (println "REFERENCE_URL not set") (System/exit 2)))
        target-url (or (System/getenv "TARGET_URL")
                       (do (println "TARGET_URL not set") (System/exit 2)))
        totals (reduce (fn [acc f]
                         (println "==" f)
                         (let [r (run-file ref-url target-url f)]
                           (println "   passed=" (:passed r) "failed=" (:failed r))
                           (doseq [d (take 5 (:diffs r))]
                             (println "   SQL:" (:sql d))
                             (when-let [only-ref (:only-in-ref d)]
                               (println "     only in ref:   " only-ref))
                             (when-let [only-tgt (:only-in-target d)]
                               (println "     only in target:" only-tgt)))
                           (merge-with + (select-keys r [:passed :failed])
                                       (select-keys acc [:passed :failed]))))
                       {:passed 0 :failed 0}
                       args)]
    (println "TOTAL passed=" (:passed totals) "failed=" (:failed totals))
    (System/exit (if (pos? (:failed totals)) 1 0))))
