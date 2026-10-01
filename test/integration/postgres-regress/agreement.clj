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

(def campaign-ref
  "The PostgreSQL the campaign measures against, from campaign.edn. One
   source of truth: the oracle, the regression sources, and the
   manifest's own `:postgres-ref` all have to be this, or the number
   asserts a reference it was not measured against."
  (:postgres-ref (edn/read-string
                  (slurp "test/integration/postgres-regress/campaign.edn"))))

(def regress-root
  "The regression sources. The PINNED checkout by default.

   This used to default to `~/Development/postgres`, a maintainer's own
   checkout, which moves. It had moved to REL_19_BETA1 while
   campaign.edn pinned REL_17_7 and the oracle was PostgreSQL 17 -- so
   the manifest recorded a reference nothing had been measured against,
   and 22 of the 174 measured files did not exist in 17 at all (SQL/PGQ,
   temporal keys, VIRTUAL generated columns), while `generated.sql` --
   17's file for a feature that IS in scope -- was not measured, because
   the file list came from 19 where it had been split in two.

   `PG_REGRESS_SRC` still overrides, for deliberately measuring against
   another version."
  (or (System/getenv "PG_REGRESS_SRC")
      (let [pinned (io/file ".internal" (str "postgres-" campaign-ref)
                            "src" "test" "regress")]
        (when (.isDirectory pinned) (str pinned)))
      (str (System/getProperty "user.home") "/Development/postgres/src/test/regress")))

(def manifest-file "test/integration/postgres-regress/agreement.edn")
(def scope-file "test/integration/postgres-regress/scope.edn")

(defn- presentation-line?
  "PostgreSQL's parser echoes the offending source and a caret under it.
   Both sides drop those: they are about the error's POSITION, which is
   a separate thing to get right and would otherwise dominate the
   comparison."
  [^String l]
  (or (str/starts-with? l "LINE ")
      (re-matches #"\s*\^\s*" l)))

(defn- lines [f]
  (with-open [r (io/reader f)]
    (vec (remove presentation-line? (line-seq r)))))

(defn- matched-lines
  "How many of `expected` appear, in order, in `actual` -- the total
   size of the longest common subsequence's matching blocks."
  [expected actual]
  (let [a (vec expected), b (vec actual)
        n (count a), m (count b)]
    ;; Hirschberg is unnecessary here; the files are thousands of lines,
    ;; not millions, and only the LENGTH is needed.
    (loop [i 0, prev (int-array (inc m))]
      (if (= i n)
        (aget prev m)
        (let [cur (int-array (inc m))]
          (dotimes [j m]
            (aset cur (inc j)
                  (if (= (nth a i) (nth b j))
                    (inc (aget prev j))
                    (max (aget cur j) (aget prev (inc j))))))
          (recur (inc i) cur))))))

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
      (let [e (lines exp), a (lines act)
            m (matched-lines e a)]
        [(if (seq e) (* 100.0 (/ (double m) (count e))) 0.0) m (count e)]))))

(defn application-facing
  "Every regression file except the ones `scope.edn` puts out of scope --
   server internals, physical access methods, and the rest of what a
   client never sees."
  []
  (let [s (slurp scope-file)
        ;; A test name may contain dots -- `collate.icu.utf8` -- so the
        ;; character class has to allow them, or the entry silently
        ;; fails to exclude anything.
        oos (set (re-seq #"(?<=\")[a-z0-9_.]+(?=\")"
                         (subs s (str/index-of s ":out-of-scope"))))]
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

(defn -main
  "Usage: agreement <measure|gate> <collected-results-dir>

   The caller has already run the regression files and gathered their
   `.out` files into one directory; this only measures and compares, so
   the slow part stays in the harness and the O(n*m) comparison stays
   on the JVM."
  [& [mode results-dir]]
  (when-not (and mode results-dir)
    (println "usage: agreement <measure|gate> <results-dir>")
    (System/exit 2))
  (let [names (application-facing)
        measured (into {} (keep (fn [n]
                                  (when-let [[pct _ _] (agreement n results-dir)]
                                    [n (Double/parseDouble (format "%.1f" pct))])))
                       names)]
    (case mode
      "measure"
      (let [manifest (try (edn/read-string (slurp manifest-file)) (catch Exception _ {}))]
        (spit manifest-file
              (str "{:measured \"" (subs (str (java.time.LocalDate/now)) 0 10) "\"\n"
                   " :postgres-ref \"" (or (:postgres-ref manifest) "REL_17_7") "\"\n"
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
        (println (format "measured %d files, wrote %s" (count measured) manifest-file)))

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
