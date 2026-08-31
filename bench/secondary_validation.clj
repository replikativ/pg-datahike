(ns secondary-validation
  "Bounded semantic/performance probe for the PostgreSQL secondary vertical.

   Run on JDK 22+:
     clojure -J-Xmx3g -M:dev:local-secondary-stack bench/secondary_validation.clj

   Set SECONDARY_BENCH_ROWS to override the default 10,000 rows. Results are
   printed as one EDN map so runs can be compared without scraping prose. Set
   SECONDARY_BENCH_COMPACT=true for a growth-run summary that retains timings,
   cardinalities, and optimizer decisions but omits call sites and tails.
   SECONDARY_BENCH_DIMENSION raises the vector width from the cheap 16-float
   smoke shape to realistic embedding widths such as 384 or 768."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.index.entity-set :as es]
            [datahike.pg.server :as pg]
            [datahike.query :as q])
  (:import [datahike.pg PgWireServer$QueryResult]))

(defn- checked [handler sql]
  (let [^PgWireServer$QueryResult result
        (binding [q/*query-result-cache?* false]
          (.execute handler sql))]
    (when (.-error result)
      (throw (ex-info (.-error result)
                      {:sqlstate (.-sqlstate result) :sql sql})))
    result))

(defn- rows [handler sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (checked handler sql))))

(defn- now-nanos [] (System/nanoTime))

(defn- slurp-trim [path]
  (try (str/trim (slurp path))
       (catch Exception _ nil)))

(defn- benchmark-environment []
  {:epoch (or (System/getenv "SECONDARY_BENCH_EPOCH") "unspecified")
   :recorded-at (str (java.time.Instant/now))
   :java-version (System/getProperty "java.version")
   :available-processors (.availableProcessors (Runtime/getRuntime))
   ;; With the active intel_pstate driver, scaling_governor may still read
   ;; "powersave" while energy_performance_preference is "performance".
   ;; Preserve both so results from different power-policy epochs cannot be
   ;; combined accidentally.
   :cpu {:scaling-driver
         (slurp-trim "/sys/devices/system/cpu/cpu0/cpufreq/scaling_driver")
         :scaling-governor
         (slurp-trim "/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor")
         :energy-performance-preference
         (slurp-trim
          "/sys/devices/system/cpu/cpu0/cpufreq/energy_performance_preference")}})

(defn- elapsed-ms [start]
  (/ (double (- (now-nanos) start)) 1e6))

(defn- percentile [sorted-values p]
  (nth sorted-values
       (min (dec (count sorted-values))
            (long (Math/floor (* p (count sorted-values)))))))

(defn- timing-summary [values]
  (let [values (vec (sort values))]
    {:p50-ms (percentile values 0.50)
     :p95-ms (percentile values 0.95)
     :min-ms (first values)
     :max-ms (peek values)}))

(defn- timings [warmups iterations f]
  (dotimes [_ warmups] (f))
  (let [values (->> (repeatedly iterations
                                (fn []
                                  (let [start (now-nanos)]
                                    (f)
                                    (elapsed-ms start))))
                    sort vec)]
    (timing-summary values)))

(def ^:dynamic *stage-recorder*
  "Per-query stage accumulator used only by the maintainer benchmark."
  nil)

(defn- result-count [result]
  (cond
    (es/entity-bitset? result) (es/entity-bitset-cardinality result)
    (instance? java.util.Collection result) (.size ^java.util.Collection result)
    (counted? result) (count result)
    :else nil))

(defn- stage-observation
  "Small, non-realizing description of a production-path stage result.

   This deliberately avoids Datahike's `:stats?` mode: that mode selects the
   legacy relational executor and therefore reports a different access path.
   Only already-counted collections, native EntityBitSets, and explicit
   candidate envelopes are observed here."
  [stage result]
  (cond
    (and (map? result) (contains? result :candidates))
    (cond-> {:candidates (count (:candidates result))}
      (contains? result :exhausted?) (assoc :exhausted? (:exhausted? result))
      (:precision result) (assoc :precision (:precision result))
      (:recall result) (assoc :recall (:recall result))
      (:ordering result) (assoc :ordering (:ordering result))
      (:stop-reason result) (assoc :stop-reason (:stop-reason result))
      (:stats result) (assoc :stats (:stats result)))

    (and (map? result) (contains? result :rows))
    {:results (count (:rows result))}

    ;; Candidate restriction functions return [query args access]. Preserve
    ;; the optimizer's decision and declared candidate bound.
    (and (vector? result) (= 3 (count result)) (map? (nth result 2 nil)))
    (select-keys (nth result 2)
                 [:kind :candidate-count :candidate-limit :precision :recall
                  :ordering :underfill-fallback :filter-strategy])

    (es/entity-bitset? result)
    {:entities (es/entity-bitset-cardinality result)}

    (contains? #{:datahike-query :datahike-post-process
                 :datahike-bounded-order-by
                 :datahike-planned-direct :datahike-execute-prepared
                 :datahike-execute-direct :stratum-query
                 :scriptum-generation-search :scriptum-snapshot-search
                 :proximum-search :proximum-api-search
                 :proximum-filtered-search :proximum-api-filtered-search
                 :vector-prefiltered-query :vector-materialized-probe
                 :vector-iterative-query :primary-filtered-vector
                 :primary-candidate-vector-recheck}
               stage)
    (when-some [n (result-count result)] {:results n})

    :else nil))

(defn- stage-wrapper [stage f]
  (fn [& args]
    (let [start (now-nanos)]
      (try
        (let [result (apply f args)]
          (when (and *stage-recorder* result)
            (when-some [observation (stage-observation stage result)]
              (swap! *stage-recorder* update-in [stage :observations]
                     (fnil conj []) observation)))
          result)
        (finally
          (when *stage-recorder*
            (swap! *stage-recorder*
                   (fn [stages]
                     (-> stages
                         (update-in [stage :nanos] (fnil + 0)
                                    (- (now-nanos) start))
                         (update-in [stage :calls] (fnil inc 0)))))))))))

(defn- pg-call-site []
  (some (fn [^StackTraceElement frame]
          (when (str/starts-with? (.getClassName frame) "datahike.pg.")
            (str (.getClassName frame) "/" (.getMethodName frame)
                 ":" (.getLineNumber frame))))
        (.getStackTrace (Thread/currentThread))))

(defn- call-site-wrapper [stage call-sites f]
  (fn [& args]
    (swap! call-sites update-in [stage (or (pg-call-site) :unknown)]
           (fnil inc 0))
    (apply f args)))

(defn- profiled-timings
  "Uninstrumented end-to-end latency plus inclusive selected-call diagnostics.

   Stage times are diagnostic rather than a subtraction identity: the
   Datahike query stage contains the Scriptum/Proximum stage when an external
   engine runs inside d/q. `with-redefs` also prevents normal Var inlining and
   can materially perturb a hot query path, so headline timing is collected in
   a separate pass and the instrumented total is retained only as an explicit
   diagnostic. Recording starts after warmup so setup calls cannot pollute the
   per-statement distributions."
  [warmups iterations stages f]
  (let [end-to-end (timings warmups iterations f)
        samples (atom {})
        call-sites (atom {})
        replacements
        (into {}
              (map (fn [[stage v]] [v (stage-wrapper stage @v)]))
              stages)
        call-site-replacements
        (into {}
              (map (fn [[stage v]]
                     [v (call-site-wrapper stage call-sites @v)]))
              stages)
        _ (with-redefs-fn call-site-replacements f)
        instrumented-totals
        (with-redefs-fn
          replacements
          (fn []
            (vec
             (repeatedly
              iterations
              (fn []
                (let [per-query (atom {})
                      start (now-nanos)]
                  ;; Write lifecycles run in core.async workers, which do not
                  ;; inherit the caller's dynamic binding. This benchmark is
                  ;; deliberately single-threaded, so a temporary root binding
                  ;; lets those workers report into the current statement too.
                  (with-redefs [*stage-recorder* per-query]
                    (f))
                  (swap! samples
                         (fn [acc]
                           (reduce-kv
                            (fn [acc stage {:keys [nanos calls observations]}]
                              (-> acc
                                  (update-in [stage :elapsed-ms] (fnil conj [])
                                             (/ (double nanos) 1e6))
                                  (update-in [stage :calls] (fnil conj []) calls)
                                  (update-in [stage :observations] (fnil into [])
                                             observations)))
                            acc @per-query)))
                  (elapsed-ms start)))))))]
    (assoc end-to-end
           :instrumented-timing (timing-summary instrumented-totals)
           :stages
           (into {}
                 (map (fn [[stage {:keys [elapsed-ms calls observations]}]]
                        [stage (assoc (timing-summary elapsed-ms)
                                      :calls-per-query
                                      {:min (apply min calls)
                                       :max (apply max calls)}
                                      :observations
                                      (frequencies observations)
                                      :call-sites
                                      (get @call-sites stage {}))]))
                 @samples))))

(defn- profiling-stages []
  (cond->
   {:vector-candidate-restriction
    (requiring-resolve 'datahike.pg.server/restrict-to-vector-candidates)
   :vector-iterative-query
   (requiring-resolve 'datahike.pg.server/run-iterative-vector-query)
   :vector-prefiltered-query
   (requiring-resolve 'datahike.pg.server/run-prefiltered-vector-query)
   :vector-materialized-probe
   (requiring-resolve 'datahike.pg.server/run-materialized-vector-probe)
   :primary-exact-vector
   (requiring-resolve 'datahike.pg.server/run-primary-exact-vector-query)
   :primary-filtered-vector
   (requiring-resolve
    'datahike.pg.server/run-primary-filtered-exact-vector-query)
   :primary-candidate-vector-recheck
   (ns-resolve 'datahike.pg.server 'run-primary-filtered-vector-entities)
   :text-candidate-restriction
   (requiring-resolve 'datahike.pg.server/restrict-to-text-candidates)
   :simple-entity-projection
   (requiring-resolve 'datahike.pg.server/simple-entity-projection)
   :format-query-result
   (requiring-resolve 'datahike.pg.server/format-query-result)
   :execute-select
   (requiring-resolve 'datahike.pg.server/exec-select)
   :parse-sql
   (requiring-resolve 'datahike.pg.sql/parse-sql)
   :scalar-candidate-restriction
   (requiring-resolve 'datahike.pg.server/restrict-to-scalar-order-candidates)
   :datahike-query (requiring-resolve 'datahike.api/q)
   ;; Datahike query decomposition. These are deliberately inclusive stages:
   ;; `execute-planned-direct` contains prepared/direct execution, while `q`
   ;; contains all of them. The nesting is the useful evidence—we can tell
   ;; whether a selective secondary result spends its remaining time planning,
   ;; scanning/joining, or shaping the final result without sampling a whole
   ;; benchmark process whose index builds dominate the profile.
   :datahike-plan
   (requiring-resolve 'datahike.query/get-or-create-plan)
   :datahike-planned-direct
   (requiring-resolve 'datahike.query/execute-planned-direct)
   :datahike-post-process
   (requiring-resolve 'datahike.query/post-process-result)
   :datahike-execute-prepared
   (requiring-resolve 'datahike.query.execute/execute-plan-prepared)
   :datahike-execute-direct
   (requiring-resolve 'datahike.query.execute/execute-plan-direct)
   :exact-cosine-distance (requiring-resolve 'datahike.pg.vector/cosine-distance)
   :candidate-page (requiring-resolve 'datahike.index.secondary/candidate-page)
   :secondary-search (requiring-resolve 'datahike.index.secondary/search-with-vt)
   :stratum-query (requiring-resolve 'stratum.api/q)
   :scriptum-count (requiring-resolve 'scriptum.core/count-store-snapshot)
   :scriptum-candidate-page (requiring-resolve 'scriptum.core/candidate-page)
   :scriptum-generation-search (requiring-resolve 'scriptum.core/search)
   :scriptum-snapshot-search (requiring-resolve 'scriptum.core/search-store-snapshot)
   :ts-match-recheck (requiring-resolve 'datahike.pg.tsearch/ts-match?)
   :proximum-search (requiring-resolve 'proximum.core/search)
   :proximum-api-search (requiring-resolve 'proximum.api-impl/search)
   :proximum-filtered-search (requiring-resolve 'proximum.core/search-filtered)
   :proximum-api-filtered-search
   (requiring-resolve 'proximum.api-impl/search-filtered)
   ;; Write-path decomposition. These generation operations are synchronous at
   ;; their public boundary, so their inclusive wall time remains meaningful
   ;; even though the surrounding Datahike transaction is channel based.
   :proximum-generation-begin
   (requiring-resolve 'proximum.generations/begin-generation)
   :proximum-generation-seal
   (requiring-resolve 'proximum.generations/seal!)
   :proximum-generation-view-transfer
   (requiring-resolve 'proximum.generations/take-generation-view!)
   :proximum-generation-root
   (requiring-resolve 'proximum.generations/rooted!)}
    (ns-resolve 'datahike.query 'bounded-order-by)
    (assoc :datahike-bounded-order-by
           (ns-resolve 'datahike.query 'bounded-order-by))))

(defn- random-vector [^java.util.Random random dimension]
  (float-array
   (repeatedly dimension
               #(- (* 2.0 (.nextDouble random)) 1.0))))

(defn- query-vector-text [dimension]
  (str "[1" (apply str (repeat (dec dimension) ",0")) "]"))

(defn- opposite-query-vector-text [dimension]
  (str "[-1" (apply str (repeat (dec dimension) ",0")) "]"))

(defn- vector-text [vector]
  (str "[" (str/join "," (map #(Float/toString (float %)) vector)) "]"))

(defn- vector-query-sql [query-vector]
  (str "SELECT id FROM secondary_bench ORDER BY embedding <=> '"
       query-vector "'::vector LIMIT 10"))

(defn- ids [query-rows]
  (mapv (comp parse-long first) query-rows))

(defn- recall [expected actual]
  (if (empty? expected)
    1.0
    (/ (double (count (set/intersection (set expected) (set actual))))
       (count expected))))

(def ^:dynamic *benchmark-rows*
  "REPL override for the fixture cardinality; nil uses SECONDARY_BENCH_ROWS."
  nil)

(def ^:dynamic *benchmark-dimension*
  "REPL override for vector width; nil uses SECONDARY_BENCH_DIMENSION."
  nil)

(def ^:dynamic *benchmark-ef-construction*
  "REPL override for HNSW construction breadth."
  nil)

(defn- run-benchmark []
  (require 'datahike.index.secondary.scriptum)
  (require 'datahike.index.secondary.proximum)
  (let [n (long (or *benchmark-rows*
                    (parse-long (or (System/getenv "SECONDARY_BENCH_ROWS") "10000"))))
        hnsw-ef-construction
        (long (or *benchmark-ef-construction*
                  (parse-long
                   (or (System/getenv "SECONDARY_BENCH_EF_CONSTRUCTION") "64"))))
        dimension
        (long (or *benchmark-dimension*
                  (parse-long
                   (or (System/getenv "SECONDARY_BENCH_DIMENSION") "16"))))
        cfg {:store {:backend :memory :id (random-uuid)}
             :writer {:backend :self :writer-ownership :exclusive}
             :schema-flexibility :write
             :allow-index-backfill? true
             :max-string-length 0}
        vector-store-id (random-uuid)
        random (java.util.Random. 7331)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          handler
          (pg/make-query-handler
           conn
           {:secondary-index-build-timeout-ms 300000
            :secondary-index-config
            {:proximum {:store-config {:backend :memory
                                       :id vector-store-id}}
             :stratum {}}})
          query-vector (query-vector-text dimension)
          opposite-query-vector (opposite-query-vector-text dimension)
          quality-query-vectors
          (let [quality-random (java.util.Random. 991)]
            (mapv (comp vector-text
                        (fn [_] (random-vector quality-random dimension)))
                  (range 12)))
          fulltext-sql (str "SELECT id FROM secondary_bench "
                            "WHERE body @@ 'needle & database' ORDER BY id")
          fulltext-1-sql (str "SELECT id FROM secondary_bench "
                              "WHERE body @@ 'needle & uncommon' ORDER BY id")
          fulltext-01-sql (str "SELECT id FROM secondary_bench "
                               "WHERE body @@ 'needle & rare' ORDER BY id")
          scalar-order-sql
          "SELECT id FROM secondary_bench ORDER BY rank DESC LIMIT 10"
          vector-sql (vector-query-sql query-vector)
          vector-update-sqls
          [(str "UPDATE secondary_bench SET embedding = '" query-vector
                "'::vector WHERE id = 0")
           (str "UPDATE secondary_bench SET embedding = '" opposite-query-vector
                "'::vector WHERE id = 0")]
          vector-update-turn (atom -1)
          update-vector!
          #(checked handler
                    (nth vector-update-sqls
                         (mod (swap! vector-update-turn inc) 2)))
          filtered-10-sql
          (str "SELECT id FROM secondary_bench WHERE category < 10 "
               "ORDER BY embedding <=> '" query-vector "'::vector LIMIT 10")
          filtered-1-sql
          (str "SELECT id FROM secondary_bench WHERE category = 0 "
               "ORDER BY embedding <=> '" query-vector "'::vector LIMIT 10")
          filtered-01-sql
          (str "SELECT id FROM secondary_bench WHERE id < "
               (max 10 (quot n 1000)) " "
               "ORDER BY embedding <=> '" query-vector "'::vector LIMIT 10")]
      (try
        (checked handler
                 (str "CREATE TABLE secondary_bench ("
                      "id int PRIMARY KEY, category int, rank int NOT NULL, body tsvector, "
                      "embedding vector(" dimension "))"))
        (let [load-start (now-nanos)]
          (doseq [start (range 0 n 1000)]
            (d/transact
             conn
             (mapv (fn [i]
                     {:secondary_bench/db-row-exists true
                      :secondary_bench/id i
                      :secondary_bench/category (mod i 100)
                      :secondary_bench/rank i
                      :secondary_bench/body
                      (cond
                        (zero? (mod i 1000))
                        "'database':2 'needle':1 'uncommon':3 'rare':4"

                        (zero? (mod i 100))
                        "'database':2 'needle':1 'uncommon':3"

                        (zero? (mod i 10))
                        "'database':2 'needle':1"

                        :else "'ordinary':1")
                      :secondary_bench/embedding
                      (random-vector random dimension)})
                   (range start (min n (+ start 1000))))))
          (let [load-ms (elapsed-ms load-start)
                stages (profiling-stages)
                exact-fulltext (ids (rows handler fulltext-sql))
                exact-fulltext-1 (ids (rows handler fulltext-1-sql))
                exact-fulltext-01 (ids (rows handler fulltext-01-sql))
                exact-scalar-order (ids (rows handler scalar-order-sql))
                exact-vector (ids (rows handler vector-sql))
                exact-quality-sample
                (mapv #(ids (rows handler (vector-query-sql %)))
                      quality-query-vectors)
                exact-filtered-10 (ids (rows handler filtered-10-sql))
                exact-filtered-1 (ids (rows handler filtered-1-sql))
                exact-filtered-01 (ids (rows handler filtered-01-sql))
                exact-fulltext-timing (timings 3 10 #(rows handler fulltext-sql))
                exact-fulltext-1-timing
                (timings 3 10 #(rows handler fulltext-1-sql))
                exact-fulltext-01-timing
                (timings 3 10 #(rows handler fulltext-01-sql))
                exact-scalar-order-timing
                (profiled-timings
                 5 20
                 (select-keys stages
                              [:datahike-query :datahike-bounded-order-by
                               :simple-entity-projection])
                 #(rows handler scalar-order-sql))
                exact-vector-timing
                (profiled-timings 5 20
                                  (select-keys stages
                                               [:datahike-query
                                                :primary-exact-vector
                                                :exact-cosine-distance])
                                  #(rows handler vector-sql))
                exact-filtered-10-timing (timings 3 10 #(rows handler filtered-10-sql))
                exact-filtered-1-timing (timings 3 10 #(rows handler filtered-1-sql))
                exact-filtered-01-timing
                (timings 3 10 #(rows handler filtered-01-sql))
                scalar-build-start (now-nanos)
                _ (checked handler
                           (str "CREATE INDEX secondary_bench_rank_btree "
                                "ON secondary_bench (rank)"))
                scalar-build-ms (elapsed-ms scalar-build-start)
                indexed-scalar-order (ids (rows handler scalar-order-sql))
                indexed-scalar-order-timing
                (profiled-timings
                 5 20 (select-keys stages [:datahike-query :candidate-page
                                           :stratum-query
                                           :scalar-candidate-restriction
                                           :simple-entity-projection
                                           :format-query-result
                                           :execute-select
                                           :parse-sql])
                 #(rows handler scalar-order-sql))
                text-build-start (now-nanos)
                _ (checked handler
                           (str "CREATE INDEX secondary_bench_body_gin "
                                "ON secondary_bench USING gin (body)"))
                text-build-ms (elapsed-ms text-build-start)
                indexed-fulltext (ids (rows handler fulltext-sql))
                indexed-fulltext-1 (ids (rows handler fulltext-1-sql))
                indexed-fulltext-01 (ids (rows handler fulltext-01-sql))
                indexed-fulltext-timing
                (profiled-timings
                 3 10 (select-keys stages [:datahike-query :secondary-search
                                           :text-candidate-restriction
                                           :simple-entity-projection
                                           :scriptum-count
                                           :scriptum-candidate-page
                                           :scriptum-generation-search
                                           :scriptum-snapshot-search
                                           :ts-match-recheck])
                 #(rows handler fulltext-sql))
                indexed-fulltext-1-timing
                (profiled-timings
                 3 10 (select-keys stages [:datahike-query :secondary-search
                                           :text-candidate-restriction
                                           :simple-entity-projection
                                           :scriptum-count
                                           :scriptum-candidate-page
                                           :scriptum-generation-search
                                           :scriptum-snapshot-search
                                           :ts-match-recheck])
                 #(rows handler fulltext-1-sql))
                indexed-fulltext-01-timing
                (profiled-timings
                 3 10 (select-keys stages [:datahike-query :secondary-search
                                           :text-candidate-restriction
                                           :simple-entity-projection
                                           :scriptum-count
                                           :scriptum-candidate-page
                                           :scriptum-generation-search
                                           :scriptum-snapshot-search
                                           :ts-match-recheck])
                 #(rows handler fulltext-01-sql))
                vector-build-start (now-nanos)
                _ (checked handler
                           (str "CREATE INDEX secondary_bench_embedding_hnsw "
                                "ON secondary_bench USING hnsw "
                                "(embedding vector_cosine_ops) "
                                "WITH (m=16, ef_construction="
                                hnsw-ef-construction ")"))
                vector-build-ms (elapsed-ms vector-build-start)
                beam-sweep
                (into (sorted-map)
                      (map (fn [ef]
                             (checked handler (str "SET hnsw.ef_search = " ef))
                             (let [actual (ids (rows handler vector-sql))]
                               [ef {:returned (count actual)
                                    :recall-at-k (recall exact-vector actual)
                                    :timing
                                    (profiled-timings
                                     5 20
                                     (select-keys stages
                                                  [:vector-candidate-restriction
                                                   :candidate-page
                                                   :proximum-search
                                                   :proximum-api-search
                                                   :datahike-query
                                                   :exact-cosine-distance])
                                     #(rows handler vector-sql))}]))
                           [40 100 200 400 800 1000]))
                _ (checked handler "SET hnsw.ef_search = 1000")
                indexed-vector (ids (rows handler vector-sql))
                indexed-quality-sample
                (mapv #(ids (rows handler (vector-query-sql %)))
                      quality-query-vectors)
                quality-recalls
                (mapv recall exact-quality-sample indexed-quality-sample)
                indexed-filtered-10 (ids (rows handler filtered-10-sql))
                indexed-filtered-1 (ids (rows handler filtered-1-sql))
                indexed-filtered-01 (ids (rows handler filtered-01-sql))
                indexed-vector-timing
                (profiled-timings
                 5 20
                 (select-keys stages
                              [:vector-candidate-restriction
                               :candidate-page
                               :proximum-search
                               :proximum-api-search
                               :datahike-query
                               :exact-cosine-distance])
                 #(rows handler vector-sql))
                indexed-filtered-10-timing
                (profiled-timings
                 3 10
                 (select-keys stages
                              [:vector-candidate-restriction
                               :vector-materialized-probe
                               :primary-candidate-vector-recheck
                               :vector-prefiltered-query
                               :vector-iterative-query
                               :candidate-page
                               :secondary-search
                               :proximum-filtered-search
                               :proximum-api-filtered-search
                               :datahike-query
                               :exact-cosine-distance])
                 #(rows handler filtered-10-sql))
                indexed-filtered-1-timing
                (profiled-timings
                 3 10
                 (select-keys stages
                              [:vector-candidate-restriction
                               :vector-materialized-probe
                               :primary-candidate-vector-recheck
                               :vector-prefiltered-query
                               :vector-iterative-query
                               :candidate-page
                               :secondary-search
                               :proximum-filtered-search
                               :proximum-api-filtered-search
                               :datahike-query
                               :exact-cosine-distance])
                 #(rows handler filtered-1-sql))
                indexed-filtered-01-timing
                (profiled-timings
                 3 10
                 (select-keys stages
                              [:vector-candidate-restriction
                               :vector-materialized-probe
                               :primary-candidate-vector-recheck
                               :vector-prefiltered-query
                               :vector-iterative-query
                               :candidate-page
                               :secondary-search
                               :proximum-filtered-search
                               :proximum-api-filtered-search
                               :datahike-query
                               :exact-cosine-distance])
                 #(rows handler filtered-01-sql))
                indexed-vector-update-timing
                (profiled-timings
                 2 5
                 (select-keys stages
                              [:proximum-generation-begin
                               :proximum-generation-seal
                               :proximum-generation-view-transfer
                               :proximum-generation-root])
                 update-vector!)]
            {:environment (benchmark-environment)
             :rows n
             :dimension dimension
             :hnsw {:m 16 :ef-construction hnsw-ef-construction}
             :load-ms load-ms
             :build-ms {:stratum scalar-build-ms
                        :scriptum text-build-ms
                        :proximum vector-build-ms}
             :scalar-order
             {:same-results? (= exact-scalar-order indexed-scalar-order)
              :exact exact-scalar-order-timing
              :indexed indexed-scalar-order-timing}
             :fulltext
             {:filter-10-percent
              {:matches (count exact-fulltext)
               :same-results? (= exact-fulltext indexed-fulltext)
               :exact exact-fulltext-timing
               :indexed indexed-fulltext-timing}
              :filter-1-percent
              {:matches (count exact-fulltext-1)
               :same-results? (= exact-fulltext-1 indexed-fulltext-1)
               :exact exact-fulltext-1-timing
               :indexed indexed-fulltext-1-timing}
              :filter-0.1-percent
              {:matches (count exact-fulltext-01)
               :same-results? (= exact-fulltext-01 indexed-fulltext-01)
               :exact exact-fulltext-01-timing
               :indexed indexed-fulltext-01-timing}}
             :vector
             {:k 10
              :update indexed-vector-update-timing
              :unfiltered
              {:beam-sweep beam-sweep
               :quality-sample
               {:queries (count quality-recalls)
                :mean-recall-at-k (/ (reduce + quality-recalls)
                                     (double (count quality-recalls)))
                :min-recall-at-k (apply min quality-recalls)
                :max-recall-at-k (apply max quality-recalls)}
               :ef-1000-confirmation
               {:returned (count indexed-vector)
                :recall-at-k (recall exact-vector indexed-vector)
                :timing indexed-vector-timing}}
              :filter-10-percent
              {:returned (count indexed-filtered-10)
               :recall-at-k (recall exact-filtered-10 indexed-filtered-10)
               :exact exact-filtered-10-timing
               :indexed indexed-filtered-10-timing}
              :filter-1-percent
              {:returned (count indexed-filtered-1)
               :recall-at-k (recall exact-filtered-1 indexed-filtered-1)
               :exact exact-filtered-1-timing
               :indexed indexed-filtered-1-timing}
              :filter-0.1-percent
              {:returned (count indexed-filtered-01)
               :recall-at-k (recall exact-filtered-01 indexed-filtered-01)
               :exact exact-filtered-01-timing
               :indexed indexed-filtered-01-timing}
              :exact exact-vector-timing}}))
        (finally
          (.close handler)
          (d/release conn)
          (d/delete-database cfg))))))

(defn- compact-timing [timing]
  (cond-> (select-keys timing [:p50-ms :p95-ms])
    (:stages timing)
    (assoc :stages
           (into {}
                 (map (fn [[stage details]]
                        [stage (select-keys details
                                            [:p50-ms :p95-ms
                                             :calls-per-query :observations])]))
                 (:stages timing)))))

(defn- compact-case [benchmark-case]
  (cond-> (select-keys benchmark-case
                       [:matches :returned :same-results? :recall-at-k])
    (:exact benchmark-case) (assoc :exact (compact-timing (:exact benchmark-case)))
    (:indexed benchmark-case)
    (assoc :indexed (compact-timing (:indexed benchmark-case)))
    (:timing benchmark-case) (assoc :timing (compact-timing (:timing benchmark-case)))))

(defn- compact-result [result]
  {:environment (:environment result)
   :rows (:rows result)
   :dimension (:dimension result)
   :load-ms (:load-ms result)
   :build-ms (:build-ms result)
   :scalar-order (compact-case (:scalar-order result))
   :fulltext (update-vals (:fulltext result) compact-case)
   :vector
   (let [vector (:vector result)
         unfiltered (:unfiltered vector)]
     {:k (:k vector)
      :update (compact-timing (:update vector))
      :exact (compact-timing (:exact vector))
      :unfiltered
      {:beam-sweep
       (into (sorted-map)
             (map (fn [[ef benchmark-case]]
                    [ef (compact-case benchmark-case)]))
             (:beam-sweep unfiltered))
       :quality-sample (:quality-sample unfiltered)
       :ef-1000-confirmation
       (compact-case (:ef-1000-confirmation unfiltered))}
      :filter-10-percent (compact-case (:filter-10-percent vector))
      :filter-1-percent (compact-case (:filter-1-percent vector))
      :filter-0.1-percent (compact-case (:filter-0.1-percent vector))})})

(when-not (some #{"--no-run"} *command-line-args*)
  (let [result (run-benchmark)]
    (prn (if (= "true" (System/getenv "SECONDARY_BENCH_COMPACT"))
           (compact-result result)
           result))))
