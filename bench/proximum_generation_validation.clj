(ns proximum-generation-validation
  "Layer benchmark for Proximum's immutable-generation integration.

   This deliberately bypasses SQL and Datahike. It separates initial HNSW
   construction, sealing, live-view transfer, cold reopen, descendant fork,
   update, and search so an end-to-end regression can be assigned to the
   correct layer.

   Run with the local secondary stack on JDK 22 or newer:

     PROXIMUM_BENCH_ROWS=10000 PROXIMUM_BENCH_DIMENSION=384
       clojure -J-Xmx4g -M:dev:local-secondary-stack
       bench/proximum_generation_validation.clj"
  (:require
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [proximum.core :as prox]
   [proximum.generations :as generation])
  (:import
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]
   [java.util Random]))

(defn- env-long [name default]
  (parse-long (or (System/getenv name) (str default))))

(defn- elapsed-ms [start]
  (/ (double (- (System/nanoTime) start)) 1e6))

(defn- timed [f]
  (let [start (System/nanoTime)
        value (f)]
    {:value value :ms (elapsed-ms start)}))

(defn- timing-summary [warmups iterations f]
  (dotimes [_ warmups] (f))
  (let [samples (vec (sort (repeatedly iterations
                                       #(let [start (System/nanoTime)]
                                          (f)
                                          (elapsed-ms start)))))
        at (fn [p]
             (nth samples
                  (min (dec (count samples))
                       (long (Math/floor (* p (count samples)))))))]
    {:p50-ms (at 0.50)
     :p95-ms (at 0.95)
     :min-ms (first samples)
     :max-ms (peek samples)}))

(defn- benchmark-vector [id dimension]
  (let [random (Random. (unchecked-add 7331 (long id)))
        result (float-array dimension)]
    (dotimes [i dimension]
      (aset-float result i (float (- (* 2.0 (.nextDouble random)) 1.0))))
    result))

(defn- query-vector [dimension]
  (doto (float-array dimension)
    (aset-float 0 1.0)))

(defn- delete-tree! [path]
  (when path
    (doseq [file (reverse (file-seq (io/file path)))]
      (io/delete-file file true))))

(defn- close-view! [view]
  (when view
    (async/<!! (generation/close-view! view))))

(defn- discard-builder! [builder]
  (when (and builder (#{:open :failed :sealed-unrooted} @(:status builder)))
    (async/<!! (generation/discard! builder))))

(defn- result-ids [results]
  (mapv :id results))

(defn- recall-at-k [expected actual]
  (/ (double (count (set/intersection (set expected) (set actual))))
     (max 1 (count expected))))

(defn run-benchmark
  ([] (run-benchmark {}))
  ([opts]
   (let [rows (long (or (:rows opts)
                        (env-long "PROXIMUM_BENCH_ROWS" 2000)))
        dimension (long (or (:dimension opts)
                            (env-long "PROXIMUM_BENCH_DIMENSION" 384)))
        batch-size (long (or (:batch-size opts)
                             (env-long "PROXIMUM_BENCH_BATCH_SIZE" 256)))
        parallelism
        (long (or (:parallelism opts)
                  (env-long "PROXIMUM_BENCH_PARALLELISM"
                            (.availableProcessors (Runtime/getRuntime)))))
        ef-construction
        (long (or (:ef-construction opts)
                  (env-long "PROXIMUM_BENCH_EF_CONSTRUCTION" 200)))
        ef-search (long (or (:ef-search opts)
                            (env-long "PROXIMUM_BENCH_EF_SEARCH" 1000)))
        mmap-dir (str (Files/createTempDirectory
                       "proximum-generation-bench-"
                       (make-array FileAttribute 0)))
        config {:type :hnsw
                :dim dimension
                :distance :cosine
                :capacity (max 32 (* 2 rows))
                :M 16
                :ef-construction ef-construction
                :ef-search ef-search
                :crypto-hash? true
                :store-config {:backend :memory :id (random-uuid)}
                :mmap-dir mmap-dir}
        builder* (atom nil)
        sealed* (atom nil)
        initial-view* (atom nil)
        reopened* (atom nil)
        child-builder* (atom nil)
        child-sealed* (atom nil)
        child-view* (atom nil)]
    (try
      (let [{builder :value begin-ms :ms}
            (timed #(generation/begin-generation-from-config config))
            _ (reset! builder* builder)
            ingest
            (timed
             (fn []
               (doseq [start (range 0 rows batch-size)]
                (let [end (min rows (+ start batch-size))
                      ids (vec (range start end))
                      vectors (mapv #(benchmark-vector % dimension) ids)]
                  (generation/put-batch! builder vectors ids
                                         {:parallelism parallelism})))))
            {sealed :value seal-ms :ms} (timed #(generation/seal! builder))
            _ (reset! sealed* sealed)
            {initial-view :value transfer-ms :ms}
            (timed #(generation/take-generation-view! sealed))
            _ (reset! initial-view* initial-view)
            generation-id (generation/generation-id sealed)
            publish-ms (:ms (timed #(generation/rooted! sealed)))
            close-sealed-ms (:ms (timed #(close-view! sealed)))
            query (query-vector dimension)
            initial-index (generation/generation-index initial-view)
            exact-result
            (result-ids
             (prox/search-filtered initial-index query 10 (range rows)
                                   {:filter-strategy :exact}))
            approximate-result
            (result-ids (prox/search initial-index query 10 {:ef ef-search}))
            search-before
            (timing-summary
             5 20
             #(prox/search initial-index query 10 {:ef ef-search}))
            exact-search-before
            (timing-summary
             2 5
             #(prox/search-filtered initial-index query 10 (range rows)
                                    {:filter-strategy :exact}))
            close-live-ms (:ms (timed #(close-view! initial-view)))
            _ (reset! initial-view* nil)
            {reopened :value reopen-ms :ms}
            (timed #(generation/open-generation config generation-id))
            _ (reset! reopened* reopened)
            search-after
            (timing-summary
             5 20
             #(prox/search (generation/generation-index reopened)
                           (query-vector dimension) 10 {:ef ef-search}))
            {child-builder :value fork-ms :ms}
            (timed #(generation/begin-generation
                     (generation/generation-index reopened)))
            _ (reset! child-builder* child-builder)
            replacement (query-vector dimension)
            update-ms
            (:ms (timed #(do (generation/delete! child-builder 0)
                              (generation/put! child-builder 0 replacement))))
            {child-sealed :value child-seal-ms :ms}
            (timed #(generation/seal! child-builder))
            _ (reset! child-sealed* child-sealed)
            {child-view :value child-transfer-ms :ms}
            (timed #(generation/take-generation-view! child-sealed))
            _ (reset! child-view* child-view)
            child-publish-ms (:ms (timed #(generation/rooted! child-sealed)))
            child-close-sealed-ms (:ms (timed #(close-view! child-sealed)))
            update-search
            (timing-summary
             5 20
             #(prox/search (generation/generation-index child-view)
                           replacement 10 {:ef ef-search}))]
        {:environment {:recorded-at (str (java.time.Instant/now))
                       :java-version (System/getProperty "java.version")
                       :available-processors (.availableProcessors
                                              (Runtime/getRuntime))}
         :shape {:rows rows
                 :dimension dimension
                 :batch-size batch-size
                 :parallelism parallelism
                 :m 16
                 :ef-construction ef-construction
                 :ef-search ef-search}
         :initial {:begin-ms begin-ms
                   :ingest-ms (:ms ingest)
                   :seal-ms seal-ms
                   :live-view-transfer-ms transfer-ms
                   :publish-guard-release-ms publish-ms
                   :sealed-handle-close-ms close-sealed-ms
                   :search search-before
                   :exact-search exact-search-before
                   :recall-at-10 (recall-at-k exact-result approximate-result)
                   :live-view-close-ms close-live-ms}
         :cold {:reopen-ms reopen-ms
                :search search-after}
         :update {:generation-fork-ms fork-ms
                  :delete-and-put-ms update-ms
                  :seal-ms child-seal-ms
                  :live-view-transfer-ms child-transfer-ms
                  :publish-guard-release-ms child-publish-ms
                  :sealed-handle-close-ms child-close-sealed-ms
                  :search update-search}})
      (finally
        (doseq [view [@child-view* @reopened* @initial-view*]]
          (try (close-view! view) (catch Throwable _)))
        (doseq [builder [@child-sealed* @child-builder* @sealed* @builder*]]
          (try (discard-builder! builder) (catch Throwable _)))
        (delete-tree! mmap-dir))))))

(when (= *file* (System/getProperty "babashka.file"))
  (prn (run-benchmark)))

;; `clojure file.clj` does not set babashka.file.
(when (and *file* (not (System/getProperty "babashka.file")))
  (prn (run-benchmark)))
