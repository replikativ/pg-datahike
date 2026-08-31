(ns postgres-secondary-reference
  "Bounded PostgreSQL reference probe for the secondary-index benchmark.

   The repository's isolated PostgreSQL 17 server is the default target. Set
   POSTGRES_REFERENCE_URL, PGUSER, PGPASSWORD, and SECONDARY_BENCH_ROWS to
   override it. The script owns only the `secondary_bench_reference` table."
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.sql Connection DriverManager PreparedStatement ResultSet Statement]))

(defn- now-nanos [] (System/nanoTime))

(defn- slurp-trim [path]
  (try (str/trim (slurp path))
       (catch Exception _ nil)))

(defn- benchmark-environment []
  {:epoch (or (System/getenv "SECONDARY_BENCH_EPOCH") "unspecified")
   :recorded-at (str (java.time.Instant/now))
   :java-version (System/getProperty "java.version")
   :available-processors (.availableProcessors (Runtime/getRuntime))
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

(defn- timings [warmups iterations f]
  (dotimes [_ warmups] (f))
  (let [values (->> (repeatedly iterations
                                (fn []
                                  (let [start (now-nanos)]
                                    (f)
                                    (elapsed-ms start))))
                    sort
                    vec)]
    {:p50-ms (percentile values 0.50)
     :p95-ms (percentile values 0.95)
     :min-ms (first values)
     :max-ms (peek values)}))

(defn- execute! [^Connection conn sql]
  (with-open [^Statement statement (.createStatement conn)]
    (.execute statement sql)))

(defn- query-ids [^Connection conn sql]
  (with-open [^Statement statement (.createStatement conn)
              ^ResultSet results (.executeQuery statement sql)]
    (loop [ids []]
      (if (.next results)
        (recur (conj ids (.getLong results 1)))
        ids))))

(defn- explain [^Connection conn sql]
  (with-open [^Statement statement (.createStatement conn)
              ^ResultSet results
              (.executeQuery statement
                             (str "EXPLAIN (ANALYZE, BUFFERS, TIMING OFF) " sql))]
    (loop [lines []]
      (if (.next results)
        ;; A 384-dimensional literal otherwise dominates the EDN result and
        ;; makes matrix diffs unreadable. The plan node/operator is the useful
        ;; evidence here; the corpus and dimension are reported separately.
        (recur (conj lines
                     (str/replace (.getString results 1)
                                  #"'\[[^']+\]'::vector"
                                  "'<query-vector>'::vector")))
        lines))))

(defn- query-string [^Connection conn sql]
  (with-open [^Statement statement (.createStatement conn)
              ^ResultSet results (.executeQuery statement sql)]
    (when (.next results)
      (.getString results 1))))

(defn- random-vector [^java.util.Random random dimension]
  (float-array
   (repeatedly dimension
               #(- (* 2.0 (.nextDouble random)) 1.0))))

(defn- vector-text [vector]
  (str "[" (str/join "," (map #(Float/toString (float %)) vector)) "]"))

(defn- query-vector-text [dimension]
  (str "[1" (apply str (repeat (dec dimension) ",0")) "]"))

(defn- opposite-query-vector-text [dimension]
  (str "[-1" (apply str (repeat (dec dimension) ",0")) "]"))

(defn- vector-query-sql [query-vector]
  (str "SELECT id FROM secondary_bench_reference ORDER BY embedding <=> '"
       query-vector "'::vector LIMIT 10"))

(defn- recall [expected actual]
  (if (empty? expected)
    1.0
    (/ (double (count (set/intersection (set expected) (set actual))))
       (count expected))))

(defn- load-vectors!
  [^Connection conn n dimension]
  (let [random (java.util.Random. 7331)]
    (with-open [^PreparedStatement statement
                (.prepareStatement
                 conn
                 "UPDATE secondary_bench_reference SET embedding = ?::vector WHERE id = ?")]
      (doseq [i (range n)]
        (.setString statement 1 (vector-text (random-vector random dimension)))
        (.setInt statement 2 i)
        (.addBatch statement)
        (when (zero? (mod (inc i) 500))
          (.executeBatch statement)))
      (when (pos? (mod n 500))
        (.executeBatch statement)))))

(def ^:dynamic *benchmark-rows* nil)
(def ^:dynamic *benchmark-dimension* nil)
(def ^:dynamic *benchmark-ef-construction* nil)
(def ^:dynamic *postgres-reference-url* nil)

(defn- run-benchmark []
  (let [n (long (or *benchmark-rows*
                    (parse-long
                     (or (System/getenv "SECONDARY_BENCH_ROWS") "10000"))))
        dimension
        (long (or *benchmark-dimension*
                  (parse-long
                   (or (System/getenv "SECONDARY_BENCH_DIMENSION") "16"))))
        hnsw-ef-construction
        (long (or *benchmark-ef-construction*
                  (parse-long
                   (or (System/getenv "SECONDARY_BENCH_EF_CONSTRUCTION") "64"))))
        url (or *postgres-reference-url*
                (System/getenv "POSTGRES_REFERENCE_URL")
                "jdbc:postgresql://127.0.0.1:15499/datahike")
        user (or (System/getenv "PGUSER") "datahike")
        password (or (System/getenv "PGPASSWORD") "datahike")
        fulltext-sql (str "SELECT id FROM secondary_bench_reference "
                          "WHERE body @@ 'needle & database'::tsquery ORDER BY id")
        fulltext-1-sql (str "SELECT id FROM secondary_bench_reference "
                            "WHERE body @@ 'needle & uncommon'::tsquery ORDER BY id")
        fulltext-01-sql (str "SELECT id FROM secondary_bench_reference "
                             "WHERE body @@ 'needle & rare'::tsquery ORDER BY id")
        scalar-order-sql
        "SELECT id FROM secondary_bench_reference ORDER BY rank DESC LIMIT 10"
        query-vector (query-vector-text dimension)
        opposite-query-vector (opposite-query-vector-text dimension)
        vector-sql (vector-query-sql query-vector)
        vector-update-sqls
        [(str "UPDATE secondary_bench_reference SET embedding = '" query-vector
              "'::vector WHERE id = 0")
         (str "UPDATE secondary_bench_reference SET embedding = '"
              opposite-query-vector "'::vector WHERE id = 0")]
        vector-update-turn (atom -1)
        next-vector-update-sql
        #(nth vector-update-sqls
              (mod (swap! vector-update-turn inc) 2))
        filtered-10-sql
        (str "SELECT id FROM secondary_bench_reference WHERE category < 10 "
             "ORDER BY embedding <=> '" query-vector "'::vector LIMIT 10")
        filtered-1-sql
        (str "SELECT id FROM secondary_bench_reference WHERE category = 0 "
             "ORDER BY embedding <=> '" query-vector "'::vector LIMIT 10")
        filtered-01-sql
        (str "SELECT id FROM secondary_bench_reference WHERE id < "
             (max 10 (quot n 1000)) " "
             "ORDER BY embedding <=> '" query-vector "'::vector LIMIT 10")
        quality-query-vectors
        (let [quality-random (java.util.Random. 991)]
          (mapv (fn [_]
                  (vector-text (random-vector quality-random dimension)))
                (range 12)))]
    (with-open [^Connection conn (DriverManager/getConnection url user password)]
      (execute! conn "CREATE EXTENSION IF NOT EXISTS vector")
      (execute! conn "DROP TABLE IF EXISTS secondary_bench_reference")
      (execute! conn
                (str "CREATE TABLE secondary_bench_reference "
                     "(id integer PRIMARY KEY, category integer, "
                     "rank integer NOT NULL, body tsvector, "
                     "embedding vector(" dimension "))"))
      (let [load-start (now-nanos)]
        (execute! conn
                  (str "INSERT INTO secondary_bench_reference "
                       "SELECT i, i % 100, i, "
                       "CASE WHEN i % 1000 = 0 THEN "
                       "'''database'':2 ''needle'':1 ''uncommon'':3 ''rare'':4'::tsvector "
                       "WHEN i % 100 = 0 THEN "
                       "'''database'':2 ''needle'':1 ''uncommon'':3'::tsvector "
                       "WHEN i % 10 = 0 THEN "
                       "'''database'':2 ''needle'':1'::tsvector "
                       "ELSE '''ordinary'':1'::tsvector END "
                       "FROM generate_series(0, " (dec n) ") AS i"))
        (load-vectors! conn n dimension)
        (let [load-ms (elapsed-ms load-start)
              _ (execute! conn "SET enable_indexscan = off")
              _ (execute! conn "SET enable_bitmapscan = off")
              exact-results (query-ids conn fulltext-sql)
              exact-results-1 (query-ids conn fulltext-1-sql)
              exact-results-01 (query-ids conn fulltext-01-sql)
              exact-timing (timings 3 10 #(query-ids conn fulltext-sql))
              exact-timing-1 (timings 3 10 #(query-ids conn fulltext-1-sql))
              exact-timing-01 (timings 3 10 #(query-ids conn fulltext-01-sql))
              exact-scalar-order (query-ids conn scalar-order-sql)
              exact-scalar-order-timing
              (timings 5 20 #(query-ids conn scalar-order-sql))
              exact-vector (query-ids conn vector-sql)
              exact-quality-sample
              (mapv #(query-ids conn (vector-query-sql %))
                    quality-query-vectors)
              exact-filtered-10 (query-ids conn filtered-10-sql)
              exact-filtered-1 (query-ids conn filtered-1-sql)
              exact-filtered-01 (query-ids conn filtered-01-sql)
              exact-vector-timing (timings 5 20 #(query-ids conn vector-sql))
              exact-filtered-10-timing
              (timings 3 10 #(query-ids conn filtered-10-sql))
              exact-filtered-1-timing
              (timings 3 10 #(query-ids conn filtered-1-sql))
              exact-filtered-01-timing
              (timings 3 10 #(query-ids conn filtered-01-sql))
              scalar-build-start (now-nanos)
              _ (execute! conn
                          (str "CREATE INDEX secondary_bench_reference_rank_btree "
                               "ON secondary_bench_reference (rank)"))
              scalar-build-ms (elapsed-ms scalar-build-start)
              build-start (now-nanos)
              _ (execute! conn
                          (str "CREATE INDEX secondary_bench_reference_body_gin "
                               "ON secondary_bench_reference USING gin (body)"))
              build-ms (elapsed-ms build-start)
              vector-build-start (now-nanos)
              _ (execute! conn
                          (str "CREATE INDEX secondary_bench_reference_embedding_hnsw "
                               "ON secondary_bench_reference USING hnsw "
                               "(embedding vector_cosine_ops) "
                               "WITH (m=16, ef_construction="
                               hnsw-ef-construction ")"))
              vector-build-ms (elapsed-ms vector-build-start)
              ;; Keep estimates and actual-row plans meaningful. Bulk INSERT
              ;; plus immediate CREATE INDEX does not guarantee autovacuum has
              ;; populated statistics before this short-lived benchmark ends.
              _ (execute! conn "ANALYZE secondary_bench_reference")
              _ (execute! conn "SET enable_indexscan = on")
              _ (execute! conn "SET enable_bitmapscan = on")
              ;; The indexed/natural half represents what an application sees,
              ;; including PostgreSQL choosing a selective primary index plus
              ;; exact top-N instead of HNSW when that is cheaper.
              _ (execute! conn "SET enable_seqscan = on")
              indexed-results (query-ids conn fulltext-sql)
              indexed-results-1 (query-ids conn fulltext-1-sql)
              indexed-results-01 (query-ids conn fulltext-01-sql)
              indexed-timing (timings 3 10 #(query-ids conn fulltext-sql))
              indexed-timing-1 (timings 3 10 #(query-ids conn fulltext-1-sql))
              indexed-timing-01 (timings 3 10 #(query-ids conn fulltext-01-sql))
              indexed-scalar-order (query-ids conn scalar-order-sql)
              indexed-scalar-order-timing
              (timings 5 20 #(query-ids conn scalar-order-sql))
              beam-sweep
              (into
               (sorted-map)
               (map (fn [ef]
                      (execute! conn (str "SET hnsw.ef_search = " ef))
                      (let [actual (query-ids conn vector-sql)]
                        [ef {:returned (count actual)
                             :recall-at-k (recall exact-vector actual)
                             :timing (timings 5 20
                                              #(query-ids conn vector-sql))}]))
                    [40 100 200 400 800 1000]))
              _ (execute! conn "SET hnsw.ef_search = 1000")
              indexed-vector (query-ids conn vector-sql)
              indexed-quality-sample
              (mapv #(query-ids conn (vector-query-sql %))
                    quality-query-vectors)
              quality-recalls
              (mapv recall exact-quality-sample indexed-quality-sample)
              indexed-vector-timing (timings 5 20 #(query-ids conn vector-sql))
              indexed-filtered-10 (query-ids conn filtered-10-sql)
              indexed-filtered-1 (query-ids conn filtered-1-sql)
              indexed-filtered-01 (query-ids conn filtered-01-sql)
              indexed-filtered-10-timing
              (timings 3 10 #(query-ids conn filtered-10-sql))
              indexed-filtered-1-timing
              (timings 3 10 #(query-ids conn filtered-1-sql))
              indexed-filtered-01-timing
              (timings 3 10 #(query-ids conn filtered-01-sql))
              indexed-vector-update-timing
              (timings 2 5 #(execute! conn (next-vector-update-sql)))]
          {:environment (benchmark-environment)
           :postgres-version (query-string conn "SHOW server_version")
           :pgvector-version
           (query-string conn
                         "SELECT extversion FROM pg_extension WHERE extname = 'vector'")
           :rows n
           :dimension dimension
           :hnsw {:m 16 :ef-construction hnsw-ef-construction}
           :load-ms load-ms
           :build-ms {:btree scalar-build-ms
                      :gin build-ms
                      :pgvector-hnsw vector-build-ms}
           :scalar-order
           {:same-results? (= exact-scalar-order indexed-scalar-order)
            :exact exact-scalar-order-timing
            :indexed indexed-scalar-order-timing
            :plan (explain conn scalar-order-sql)}
           :fulltext
           {:filter-10-percent
            {:matches (count exact-results)
             :same-results? (= exact-results indexed-results)
             :exact exact-timing
             :indexed indexed-timing
             :plan (explain conn fulltext-sql)}
            :filter-1-percent
            {:matches (count exact-results-1)
             :same-results? (= exact-results-1 indexed-results-1)
             :exact exact-timing-1
             :indexed indexed-timing-1
             :plan (explain conn fulltext-1-sql)}
            :filter-0.1-percent
            {:matches (count exact-results-01)
             :same-results? (= exact-results-01 indexed-results-01)
             :exact exact-timing-01
             :indexed indexed-timing-01
             :plan (explain conn fulltext-01-sql)}}
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
              :timing indexed-vector-timing}
             :plan (explain conn vector-sql)}
            :filter-10-percent
            {:returned (count indexed-filtered-10)
             :recall-at-k (recall exact-filtered-10 indexed-filtered-10)
             :exact exact-filtered-10-timing
             :indexed indexed-filtered-10-timing
             :plan (explain conn filtered-10-sql)}
            :filter-1-percent
            {:returned (count indexed-filtered-1)
             :recall-at-k (recall exact-filtered-1 indexed-filtered-1)
             :exact exact-filtered-1-timing
             :indexed indexed-filtered-1-timing
             :plan (explain conn filtered-1-sql)}
            :filter-0.1-percent
            {:returned (count indexed-filtered-01)
             :recall-at-k (recall exact-filtered-01 indexed-filtered-01)
             :exact exact-filtered-01-timing
             :indexed indexed-filtered-01-timing
             :plan (explain conn filtered-01-sql)}
            :exact exact-vector-timing}})))))

(when-not (some #{"--no-run"} *command-line-args*)
  (prn (run-benchmark)))
