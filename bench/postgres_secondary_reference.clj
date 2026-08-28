(ns postgres-secondary-reference
  "Bounded PostgreSQL reference probe for the secondary-index benchmark.

   The repository's isolated PostgreSQL 17 server is the default target. Set
   POSTGRES_REFERENCE_URL, PGUSER, PGPASSWORD, and SECONDARY_BENCH_ROWS to
   override it. The script owns only the `secondary_bench_reference` table."
  (:import [java.sql Connection DriverManager ResultSet Statement]))

(defn- now-nanos [] (System/nanoTime))

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
              ^ResultSet results (.executeQuery statement (str "EXPLAIN " sql))]
    (loop [lines []]
      (if (.next results)
        (recur (conj lines (.getString results 1)))
        lines))))

(defn- run-benchmark []
  (let [n (parse-long (or (System/getenv "SECONDARY_BENCH_ROWS") "10000"))
        url (or (System/getenv "POSTGRES_REFERENCE_URL")
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
        "SELECT id FROM secondary_bench_reference ORDER BY rank DESC LIMIT 10"]
    (with-open [^Connection conn (DriverManager/getConnection url user password)]
      (execute! conn "DROP TABLE IF EXISTS secondary_bench_reference")
      (execute! conn
                (str "CREATE TABLE secondary_bench_reference "
                     "(id integer PRIMARY KEY, category integer, "
                     "rank integer NOT NULL, body tsvector)"))
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
              _ (execute! conn "SET enable_indexscan = on")
              _ (execute! conn "SET enable_bitmapscan = on")
              _ (execute! conn "SET enable_seqscan = off")
              indexed-results (query-ids conn fulltext-sql)
              indexed-results-1 (query-ids conn fulltext-1-sql)
              indexed-results-01 (query-ids conn fulltext-01-sql)
              indexed-timing (timings 3 10 #(query-ids conn fulltext-sql))
              indexed-timing-1 (timings 3 10 #(query-ids conn fulltext-1-sql))
              indexed-timing-01 (timings 3 10 #(query-ids conn fulltext-01-sql))
              indexed-scalar-order (query-ids conn scalar-order-sql)
              indexed-scalar-order-timing
              (timings 5 20 #(query-ids conn scalar-order-sql))]
          {:postgres-version
           (with-open [^Statement statement (.createStatement conn)
                       ^ResultSet results (.executeQuery statement
                                                        "SHOW server_version")]
             (.next results)
             (.getString results 1))
           :rows n
           :load-ms load-ms
           :build-ms {:btree scalar-build-ms :gin build-ms}
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
             :plan (explain conn fulltext-01-sql)}}})))))

(prn (run-benchmark))
