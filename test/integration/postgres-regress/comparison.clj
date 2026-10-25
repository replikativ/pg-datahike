(ns postgres-comparison
  "Exact expected-line agreement with bounded LCS workspace and durable checkpoints."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files StandardCopyOption]
           [java.util UUID]))

;; Change this identity if normalization or the metric changes. Checkpoints
;; cache comparison work only; SQL collection is never skipped by this cache.
(def metric :canonical-expected-line-lcs-v1)

(defn matched-lines
  "Exact LCS length, with two reused arrays sized to the shorter input."
  [expected actual]
  (let [[a b] (if (< (count expected) (count actual))
                [actual expected] [expected actual])
        n (count a), m (count b)]
    (loop [i 0, prev (int-array (inc m)), cur (int-array (inc m))]
      (if (= i n)
        (aget prev m)
        (do
          (aset-int cur 0 0)
          (dotimes [j m]
            (aset-int cur (inc j)
                      (if (= (nth a i) (nth b j))
                        (inc (aget prev j))
                        (max (aget cur j) (aget prev (inc j))))))
          (recur (inc i) cur prev))))))

(defn- lines [file]
  (with-open [reader (io/reader file)]
    (into [] (remove #(or (str/starts-with? % "LINE ")
                          (re-matches #"\s*\^\s*" %)))
          (line-seq reader))))

(defn score-files [expected actual]
  (let [e (lines expected), a (lines actual)
        matched (matched-lines e a), total (count e)]
    [(if (pos? total) (* 100.0 (/ (double matched) total)) 0.0)
     matched total]))

(defn atomic-write!
  "Replace a completed artifact atomically, leaving the previous file on failure."
  [file text]
  (io/make-parents file)
  (let [file (io/file file)
        temporary (io/file (.getParentFile (.getAbsoluteFile file))
                           (str "." (.getName file) "." (UUID/randomUUID) ".tmp"))]
    (try
      (spit temporary text)
      (Files/move (.toPath temporary) (.toPath file)
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
      (finally (.delete temporary)))))

(defn- valid-score? [[percentage matched total :as score]]
  (and (= 3 (count score))
       (integer? matched) (integer? total) (<= 0 matched total)
       (= percentage (if (pos? total) (* 100.0 (/ (double matched) total)) 0.0))))

(defn checkpoint-score!
  "Resume a finished comparison only when both raw input hashes and metric match.
   A partial/corrupt checkpoint is recomputed. Changed inputs fail before writing."
  [expected actual checkpoint hash-file]
  (let [identity {:metric metric :expected-sha256 (hash-file expected)
                  :actual-sha256 (hash-file actual)}
        saved (try (edn/read-string (slurp checkpoint)) (catch Exception _ nil))
        score (if (and (map? saved) (= identity (dissoc saved :score))
                       (vector? (:score saved)) (valid-score? (:score saved)))
                (:score saved)
                (score-files expected actual))]
    (when-not (= identity {:metric metric :expected-sha256 (hash-file expected)
                           :actual-sha256 (hash-file actual)})
      (throw (ex-info "comparison inputs changed while scoring" {})))
    (when-not (= (assoc identity :score score) saved)
      (atomic-write! checkpoint (str (pr-str (assoc identity :score score)) "\n")))
    score))
