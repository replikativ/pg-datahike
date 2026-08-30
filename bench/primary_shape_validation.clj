(ns primary-shape-validation
  "Measure primary row-shape costs that secondary benchmarks intentionally hide.

   The same deterministic corpus is loaded into an in-memory pg-datahike
   handler and PostgreSQL. Point/range projection width isolates EAV attribute
   seeks; point/fanout joins isolate lookup and indexed reverse-fanout work.

   PRIMARY_BENCH_ROWS defaults to 100,000. PostgreSQL defaults to the local
   bench/realpg.sh instance and can be overridden with POSTGRES_REFERENCE_URL,
   PGUSER, and PGPASSWORD."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.server :as pg]
            [datahike.pg.sql :as sql]
            [datahike.query :as q])
  (:import [datahike.pg PgWireServer$QueryResult]
           [java.sql Connection DriverManager ResultSet Statement]))

(def widths [1 2 4 8 16])

(defn- now-nanos [] (System/nanoTime))

(defn- elapsed-ms [start]
  (/ (double (- (now-nanos) start)) 1e6))

(defn- percentile [sorted-values p]
  (nth sorted-values
       (min (dec (count sorted-values))
            (long (Math/floor (* p (count sorted-values)))))))

(defn- timings [warmups iterations f]
  (dotimes [_ warmups] (f))
  (let [values (vec (sort (repeatedly iterations
                                      (fn []
                                        (let [start (now-nanos)]
                                          (f)
                                          (elapsed-ms start))))))]
    {:p50-ms (percentile values 0.50)
     :p95-ms (percentile values 0.95)}))

(defn- columns [prefix n]
  (str/join ", " (map #(str prefix %) (range 1 (inc n)))))

(defn- qualified-columns [alias prefix n]
  (str/join ", " (map #(str alias "." prefix %) (range 1 (inc n)))))

(defn- table-ddl [table extra-column column-count]
  (str "CREATE TABLE " table " (id int PRIMARY KEY, "
       (when extra-column (str extra-column " int NOT NULL, "))
       (str/join ", " (map #(str "c" % " int NOT NULL")
                            (range 1 (inc column-count))))
       ")"))

(defn- point-sql [id width]
  (str "SELECT " (columns "c" width)
       " FROM shape_fact WHERE id = " id))

(defn- range-sql [start width]
  (str "SELECT id, " (columns "c" width)
       " FROM shape_fact WHERE id >= " start " AND id < " (+ start 100)
       " ORDER BY id"))

(defn- join-columns [width]
  (let [fact-width (quot width 2)
        parent-width (- width fact-width)]
    (str/join ", "
              (remove str/blank?
                      [(qualified-columns "f" "c" fact-width)
                       (qualified-columns "p" "c" parent-width)]))))

(defn- point-join-sql [id width]
  (str "SELECT " (join-columns width)
       " FROM shape_fact f JOIN shape_parent p ON f.parent_id = p.id "
       "WHERE f.id = " id))

(def fanout-sql
  (str "SELECT f.id, f.c1, p.c1 FROM shape_fact f "
       "JOIN shape_parent p ON f.parent_id = p.id "
       "WHERE p.id = 42 ORDER BY f.id LIMIT 100"))

(defn- measured-query [run-query sql warmups iterations expected-rows]
  (let [actual-rows (count (run-query sql))]
    (when-not (= expected-rows actual-rows)
      (throw (ex-info "Primary shape query returned the wrong row count"
                      {:sql sql :expected expected-rows :actual actual-rows})))
    (assoc (timings warmups iterations #(run-query sql)) :rows actual-rows)))

(defn- benchmark-queries [run-query n parent-count]
  (let [point-id (quot n 2)
        range-start (max 0 (- point-id 50))]
    {:point-projection
     (into (sorted-map)
           (map (fn [width]
                  [width (measured-query run-query (point-sql point-id width)
                                         10 50 1)]))
           widths)
     :range-100-projection
     (into (sorted-map)
           (map (fn [width]
                  [width (measured-query run-query (range-sql range-start width)
                                         5 20 100)]))
           widths)
     :point-join-projection
     (into (sorted-map)
           (map (fn [width]
                  [width (measured-query run-query
                                         (point-join-sql point-id width)
                                         10 50 1)]))
           [2 4 8 16])
     :indexed-fanout-100
     (measured-query run-query fanout-sql 5 20
                     (min 100 (quot n parent-count)))}))

(defn- checked [handler sql]
  (let [^PgWireServer$QueryResult result
        (binding [q/*query-result-cache?* false]
          (.execute handler sql))]
    (when (.-error result)
      (throw (ex-info (.-error result)
                      {:sqlstate (.-sqlstate result) :sql sql})))
    result))

(defn- datahike-rows [handler sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (checked handler sql))))

(defn- profile-datahike-query [handler sql]
  (let [original @#'d/q
        nanos (atom 0)
        calls (atom 0)
        timing
        (with-redefs [d/q
                      (fn [& args]
                        (let [start (now-nanos)]
                          (try
                            (apply original args)
                            (finally
                              (swap! nanos + (- (now-nanos) start))
                              (swap! calls inc)))))]
          (timings 5 20 #(datahike-rows handler sql)))]
    (assoc timing
           :datahike-q-calls @calls
           :datahike-q-ms-per-query
           (when (pos? @calls) (/ (double @nanos) 1e6 @calls)))))

(defn- fact-row [i parent-count]
  (into {:shape_fact/db-row-exists true
         :shape_fact/id i
         :shape_fact/parent_id (mod i parent-count)}
        (map (fn [column]
               [(keyword "shape_fact" (str "c" column)) (+ i column)]))
        (range 1 17)))

(defn- parent-row [i]
  (into {:shape_parent/db-row-exists true
         :shape_parent/id i}
        (map (fn [column]
               [(keyword "shape_parent" (str "c" column)) (+ i column)]))
        (range 1 9)))

(defn- run-datahike [n parent-count]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          handler (pg/make-query-handler conn)]
      (try
        (checked handler (table-ddl "shape_parent" nil 8))
        (checked handler (table-ddl "shape_fact" "parent_id" 16))
        ;; Establish AVET before loading so both engines have the same indexed
        ;; fanout access path without conflating this probe with online backfill.
        (d/transact conn [{:db/ident :shape_fact/parent_id :db/index true}])
        (let [load-start (now-nanos)]
          (doseq [start (range 0 parent-count 1000)]
            (d/transact conn (mapv parent-row
                                   (range start (min parent-count (+ start 1000))))))
          (doseq [start (range 0 n 1000)]
            (d/transact conn (mapv #(fact-row % parent-count)
                                   (range start (min n (+ start 1000))))))
          (let [db (d/db conn)
                parsed (sql/parse-sql fanout-sql (dbi/-schema db) db)]
            {:load-ms (elapsed-ms load-start)
             :queries (benchmark-queries #(datahike-rows handler %) n parent-count)
             :indexed-fanout-profile (profile-datahike-query handler fanout-sql)
             :indexed-fanout-plan
             (apply q/explain (:query parsed) db (:in-args parsed))}))
        (finally
          (.close handler)
          (d/release conn)
          (d/delete-database cfg))))))

(defn- execute! [^Connection conn sql]
  (with-open [^Statement statement (.createStatement conn)]
    (.execute statement sql)))

(defn- postgres-rows [^Connection conn sql]
  (with-open [^Statement statement (.createStatement conn)
              ^ResultSet result (.executeQuery statement sql)]
    (let [width (.getColumnCount (.getMetaData result))]
      (loop [rows (transient [])]
        (if (.next result)
          (recur (conj! rows
                        (mapv #(.getObject result (int %))
                              (range 1 (inc width)))))
          (persistent! rows))))))

(defn- run-postgres [n parent-count]
  (let [url (or (System/getenv "POSTGRES_REFERENCE_URL")
                "jdbc:postgresql://127.0.0.1:15499/datahike")
        user (or (System/getenv "PGUSER") "datahike")
        password (or (System/getenv "PGPASSWORD") "datahike")]
    (with-open [^Connection conn (DriverManager/getConnection url user password)]
      (execute! conn "DROP TABLE IF EXISTS shape_fact")
      (execute! conn "DROP TABLE IF EXISTS shape_parent")
      (execute! conn (table-ddl "shape_parent" nil 8))
      (execute! conn (table-ddl "shape_fact" "parent_id" 16))
      (let [load-start (now-nanos)]
        (execute!
         conn
         (str "INSERT INTO shape_parent SELECT i, "
              (str/join ", " (map #(str "i + " %) (range 1 9)))
              " FROM generate_series(0, " (dec parent-count) ") AS i"))
        (execute!
         conn
         (str "INSERT INTO shape_fact SELECT i, i % " parent-count ", "
              (str/join ", " (map #(str "i + " %) (range 1 17)))
              " FROM generate_series(0, " (dec n) ") AS i"))
        (execute! conn "CREATE INDEX shape_fact_parent_id ON shape_fact (parent_id)")
        (execute! conn "ANALYZE shape_parent")
        (execute! conn "ANALYZE shape_fact")
        {:load-ms (elapsed-ms load-start)
         :queries (benchmark-queries #(postgres-rows conn %) n parent-count)}))))

(let [n (parse-long (or (System/getenv "PRIMARY_BENCH_ROWS") "100000"))
      parent-count (max 100 (quot n 100))]
  (prn {:rows n
        :parent-rows parent-count
        :datahike (run-datahike n parent-count)
        :postgresql (run-postgres n parent-count)}))
