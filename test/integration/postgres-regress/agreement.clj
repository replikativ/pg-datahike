(ns agreement
  "Measure how much of PostgreSQL's expected output we reproduce, and
   fail when it drops.

   `pg_regress` fails a file on any difference, so a per-file pass rate
   reads zero and says nothing. This measures AGREEMENT instead: the
   share of PostgreSQL's own expected lines that appear, in order, in
   ours. A file at 80% answers four fifths of what PostgreSQL prints,
   which is what a partially-supported area looks like from a client.

   Two modes:

     measure  -- run the files and write agreement.edn
     gate     -- run them and compare against agreement.edn

   The gate fails on a file that dropped more than the tolerance, on a
   file at 100% that stopped being at 100%, and on a file that stopped
   being measurable at all.

   100% here means every EXPECTED line is present and in order, not
   that the output is byte-identical: extra lines of ours do not lower
   it. That is deliberate -- the number answers how much of what
   PostgreSQL prints we also print, and a stricter reading is what
   `pg_regress` itself already gives, namely zero for any difference.

   It does NOT fail on an improvement: rerun
   `measure` and commit the new numbers.

   The tolerance exists because a file whose output is mostly cascade
   realigns by a point or two whenever anything ahead of its first
   divergence changes -- in either direction. It is not slack for
   regressions; a real one shows up as several points, or as a file
   that stopped matching exactly."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(load-file "test/integration/postgres-regress/reference.clj")
(alias 'reference 'postgres-reference)
(load-file "test/integration/postgres-regress/comparison.clj")
(alias 'comparison 'postgres-comparison)

(def reference-config (reference/config))
(reference/require-campaign! reference-config)
(reference/validate-source! reference-config)
(def campaign-ref (:postgres-ref reference-config))
(def regress-root (:regress-root reference-config))
(def manifest-file (:agreement reference-config))
(def scope-file (:scope reference-config))

(defn agreement
  "[percentage matched total] for one file, or nil when it was not run.

   THE METRIC IS GAMEABLE, and knowing how is the point of reading it.
   It divides by the size of PostgreSQL's expected output and counts
   only what appears in order, so EMITTING MORE LINES CAN ONLY RAISE
   IT. A statement we answer with a wrong-but-plausible block of rows
   scores above one we refuse outright, and a parser that echoes junk
   before failing scores above one that fails cleanly.

   That is not hypothetical. Ten files appeared to LOSE ground in the
   2026-09-29 comparison; eight of them reproduced their old score
   exactly once the removed JSqlParser grammar dump was simulated --
   the earlier number had been inflated by lines that were never an
   answer to anything.

   So: a rise is evidence of nothing on its own, and a fall is a
   question rather than a regression. What the number is good for is
   WHICH FILE to open next, and the first divergence inside it. The
   gate exists to make a large drop visible, not to make the mean a
   goal."
  [test-name results-dir]
  (let [exp (io/file regress-root "expected" (str test-name ".out"))
        act (io/file results-dir (str test-name ".out"))]
    (when (and (.exists exp) (.exists act))
      (comparison/checkpoint-score!
       exp act (io/file results-dir "comparisons" (str test-name ".edn"))
       reference/file-sha256))))

(defn application-facing
  "Every regression file except the ones `scope.edn` puts out of scope --
   server internals, physical access methods, and the rest of what a
   client never sees."
  []
  (let [scope (edn/read-string (slurp scope-file))
        oos (set (mapcat val (:out-of-scope scope)))]
    (->> (.listFiles (io/file regress-root "sql"))
         (map #(.getName %))
         (filter #(str/ends-with? % ".sql"))
         (map #(subs % 0 (- (count %) 4)))
         (remove oos)
         sort
         vec)))

(defn report [measured manifest]
  (let [{:keys [tolerance files]} manifest
        problems
        (for [[name recorded] (sort files)
              :let [now (get measured name)]
              :when (or (nil? now)
                        (< now (- recorded (double tolerance)))
                        (and (>= recorded 99.95) (< now 99.95)))]
          (cond
            (nil? now) (format "  %-24s was %.1f%%, NOT MEASURED" name recorded)
            (and (>= recorded 99.95) (< now 99.95))
            (format "  %-24s was EXACT, now %.1f%%" name now)
            :else (format "  %-24s %.1f%% -> %.1f%%  (%.1f)" name recorded now (- now recorded))))
        gains (for [[name recorded] (sort files)
                    :let [now (get measured name)]
                    :when (and now (> now (+ recorded (double tolerance))))]
                (format "  %-24s %.1f%% -> %.1f%%  (+%.1f)" name recorded now (- now recorded)))]
    {:problems (vec problems) :gains (vec gains)}))

(defn- subset-names
  "`AGREEMENT_FILES`, space- or comma-separated, restricted to files that
   are actually in scope. Only `probe` honours it -- see -main."
  []
  (when-let [raw (System/getenv "AGREEMENT_FILES")]
    (let [asked (remove str/blank? (str/split (str/trim raw) #"[,\s]+"))
          in-scope (set (application-facing))
          unknown (remove in-scope asked)]
      (when (seq unknown)
        (println "not in scope (see scope.edn):" (str/join " " unknown))
        (System/exit 2))
      (vec asked))))

(defn -main
  "Usage: agreement <measure|gate|probe> <collected-results-dir>

   The caller has already run the regression files and gathered their
   `.out` files into one directory; this only measures and compares, so
   the slow part stays in the harness and the O(n*m) comparison stays
   on the JVM.

   `probe` reports per-file agreement against the recorded manifest and
   preserves the baseline. All modes retain hash-checked comparison checkpoints.
   `stage-measure` is the runner's internal mode: the parent promotes its staged
   baseline only after the bounded comparison process exits successfully. It exists so a campaign can iterate on a handful of
   files without paying for all 154 -- and it is a separate mode rather
   than a flag on `measure` on purpose: `measure` rewrites `:files` from
   what it measured, so measuring a SUBSET would silently drop every
   other file's baseline and leave the gate unable to see a regression
   in them. `AGREEMENT_FILES` is therefore honoured by `probe` alone."
  [& [mode results-dir]]
  (when-not (and mode results-dir)
    (println "usage: agreement <measure|gate|probe> <results-dir>")
    (System/exit 2))
  (when (and (System/getenv "AGREEMENT_FILES") (not= mode "probe"))
    (println (str "AGREEMENT_FILES is only honoured by `probe`. Using it with `"
                  mode "` would rewrite the manifest from a subset and drop"
                  " every other file's baseline."))
    (System/exit 2))
  (reference/require-authoritative! reference-config)
  (let [manifest (edn/read-string (slurp manifest-file))]
    (reference/validate-baseline! reference-config manifest))
  (let [manifest (edn/read-string (slurp manifest-file))
        _ (when (and (= mode "gate")
                     (or (= :unmeasured (:status manifest)) (empty? (:files manifest))))
            (reference/fail! "no measured agreement baseline; run measure before gate"))
        collection (reference/validate-collection! reference-config results-dir)
        names (or (subset-names) (application-facing))
        _ (doseq [name names]
            (when-not (contains? (:outputs collection) name)
              (reference/fail! (str "no current complete result for " name))))
        measured (into {} (map (fn [n]
                                 (let [[pct _ _] (or (agreement n results-dir)
                                                     (reference/fail! (str "missing comparison input for " n)))]
                                   [n (Double/parseDouble (format "%.1f" pct))])))
                       names)
        _ (when-not (= collection (reference/validate-collection! reference-config results-dir))
            (reference/fail! "collection changed during comparison"))
        _ (when-not (= (set names) (set (or (subset-names) (application-facing))))
            (reference/fail! "comparison scope changed during comparison"))]
    (case mode
      ("measure" "stage-measure")
      (let [destination (if (= mode "stage-measure")
                          (io/file results-dir "baseline.edn") manifest-file)
            manifest (try (edn/read-string (slurp manifest-file)) (catch Exception _ {}))]
        (comparison/atomic-write! destination
                                  (str "{:measured \"" (subs (str (java.time.LocalDate/now)) 0 10) "\"\n"
                                       " :status :measured\n"
                                       " :postgres-ref \"" campaign-ref "\"\n"
                                       " :profile " (pr-str (:profile reference-config)) "\n"
                                       " :source-commit " (pr-str (get-in collection [:reference :commit])) "\n"
                                       " :inputs-sha256 " (pr-str (get-in collection [:reference :inputs-sha256])) "\n"
                                       " ;; Per-file agreement: the share of PostgreSQL's expected output\n"
                                       " ;; lines that appear, in order, in ours. `bb pg-regress-gate`\n"
                                       " ;; fails when a file drops more than the tolerance below, when an\n"
                                       " ;; exact match is lost, or when a file stops being measurable.\n"
                                       " ;;\n"
                                       " ;; The tolerance is not slack for regressions: a file that is\n"
                                       " ;; mostly cascade realigns by a point or two whenever anything\n"
                                       " ;; ahead of its first divergence changes, in either direction.\n"
                                       " ;;\n"
                                       " ;; And the number is GAMEABLE: it counts expected lines we\n"
                                       " ;; reproduce, so emitting MORE output can only raise it. A\n"
                                       " ;; wrong-but-plausible block of rows scores above a clean\n"
                                       " ;; refusal. Read a file's score to decide what to open next,\n"
                                       " ;; never as a goal. See `agreement`.\n"
                                       " :tolerance " (or (:tolerance manifest) 3.0) "\n"
                                       " :files\n {\n"
                                       (str/join "\n" (for [[n p] (sort measured)]
                                                        (format "  \"%s\" %.1f" n p)))
                                       "\n }}\n"))
        (println (format "measured %d files, %s %s" (count measured)
                         (if (= mode "stage-measure") "staged" "wrote") destination)))

      "probe"
      (let [manifest (try (edn/read-string (slurp manifest-file)) (catch Exception _ {}))
            recorded (:files manifest)]
        (println (format "%-26s %7s %7s %s" "file" "now" "was" "delta"))
        (doseq [n (sort names)
                :let [now (get measured n)
                      was (get recorded n)]]
          (println (format "%-26s %7s %7s %s"
                           n
                           (if now (format "%.1f" now) "-")
                           (if was (format "%.1f" was) "-")
                           (cond (nil? now) "NOT MEASURABLE"
                                 (nil? was) "new"
                                 (>= now 99.95) (if (>= was 99.95) "exact" "EXACT (was not)")
                                 :else (format "%+.1f" (- now was))))))
        (println "\nprobe only -- baseline unchanged. Run `bb pg-regress-measure` to re-baseline."))

      "gate"
      (let [manifest (edn/read-string (slurp manifest-file))
            {:keys [problems gains]} (report measured manifest)
            total (reduce + 0.0 (vals measured))]
        (println (format "%d files measured, mean agreement %.1f%%"
                         (count measured) (/ total (max 1 (count measured)))))
        (when (seq gains)
          (println "\nImproved — rerun `bb pg-regress-measure` and commit the new numbers:")
          (run! println gains))
        (if (seq problems)
          (do (println "\nAGREEMENT DROPPED:")
              (run! println problems)
              (System/exit 1))
          (println "\nNo file lost ground.")))

      (do (println "unknown mode:" mode) (System/exit 2)))))
