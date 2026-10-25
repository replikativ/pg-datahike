#!/usr/bin/env bb
(ns postgres-comparison-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]))

(load-file "test/integration/postgres-regress/comparison.clj")
(alias 'comparison 'postgres-comparison)

(defn reference-lcs [a b]
  ;; Deliberately independent, exhaustive subsequence oracle for small inputs.
  (let [subsequences (fn [xs] (reduce (fn [ss x] (into ss (map #(conj % x) ss))) [[]] xs))
        candidates (set (subsequences b))]
    (apply max (map count (filter candidates (subsequences a))))))

(defn words [n]
  (if (zero? n) [[]] (for [prefix (words (dec n)), x [:a :b]] (conj prefix x))))

(deftest exact-lcs-agrees-with-independent-exhaustive-oracle
  (let [inputs (mapcat words (range 5))]
    (doseq [a inputs, b inputs]
      (is (= (reference-lcs a b) (comparison/matched-lines a b)) (pr-str [a b]))))
  (is (= 2 (comparison/matched-lines [1 2] (vec (concat (repeat 1000 0) [1 2])))))
  (is (= 2 (comparison/matched-lines (vec (concat (repeat 1000 0) [1 2])) [1 2]))))

(defn hash-file [file]
  (vec (.digest (MessageDigest/getInstance "SHA-256") (Files/readAllBytes (.toPath (io/file file))))))

(deftest normalization-checkpoints-and-changed-inputs
  (let [dir (.toFile (Files/createTempDirectory "pgdh-comparison-test-" (make-array FileAttribute 0)))
        expected (io/file dir "expected")
        actual (io/file dir "actual")
        checkpoint (io/file dir "checkpoints/foo.edn")]
    (try
      (spit expected "answer\nLINE 1: wrong source\n  ^\nanswer\nend\n")
      (spit actual "extra\nanswer\nanswer\nend\nextra\n")
      (is (= [100.0 3 3] (comparison/score-files expected actual)))
      (is (= [100.0 3 3] (comparison/checkpoint-score! expected actual checkpoint hash-file)))
      (with-redefs [comparison/score-files (fn [& _] (throw (Exception. "should reuse checkpoint")))]
        (is (= [100.0 3 3] (comparison/checkpoint-score! expected actual checkpoint hash-file))))
      (spit actual "answer\n")
      (is (= [(* 100.0 (/ 1.0 3)) 1 3]
             (comparison/checkpoint-score! expected actual checkpoint hash-file)))
      (spit expected "answer\n")
      (is (= [100.0 1 1] (comparison/checkpoint-score! expected actual checkpoint hash-file)))
      (doseq [invalid ["partial{" "[]" "42" "nil" "{:score [100.0 10 1]}"
                       (pr-str (assoc (edn/read-string (slurp checkpoint)) :metric :old-metric))
                       (pr-str (assoc (edn/read-string (slurp checkpoint)) :score [100.0 2 1]))]]
        (spit checkpoint invalid)
        (is (= [100.0 1 1] (comparison/checkpoint-score! expected actual checkpoint hash-file))))
      (let [before (slurp checkpoint)]
        (with-redefs [comparison/score-files (fn [& _] (spit actual "changed\n") [0.0 0 1])]
          (spit expected "changed expected\n")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"inputs changed"
                                (comparison/checkpoint-score! expected actual checkpoint hash-file))))
        (is (= before (slurp checkpoint))))
      (is (empty? (filter #(.endsWith (.getName %) ".tmp") (file-seq dir))))
      (finally (doseq [file (reverse (file-seq dir))] (.delete file))))))

(let [result (clojure.test/run-tests 'postgres-comparison-test)]
  (when (pos? (+ (:fail result) (:error result))) (System/exit 1)))
