(ns scriptum-build-growth
  "Isolate PostgreSQL-facing Scriptum CREATE INDEX growth.

   This intentionally omits query timing and Proximum construction so 100k and
   1m-row build curves can run in a bounded JVM. Run on the local secondary
   stack; comma-separated sizes default to 10k and 100k:

     SECONDARY_BUILD_ROWS=10000,100000 clojure -J-Xmx4g -M:dev:local-secondary-stack bench/scriptum_build_growth.clj"
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(defn- elapsed-ms [start]
  (/ (double (- (System/nanoTime) start)) 1e6))

(defn- checked [handler sql]
  (let [^PgWireServer$QueryResult result (.execute handler sql)]
    (when (.-error result)
      (throw (ex-info (.-error result)
                      {:sqlstate (.-sqlstate result) :sql sql})))
    result))

(defn- body-value [i]
  (cond
    (zero? (mod i 1000))
    "'database':2 'needle':1 'uncommon':3 'rare':4"

    (zero? (mod i 100))
    "'database':2 'needle':1 'uncommon':3"

    (zero? (mod i 10))
    "'database':2 'needle':1"

    :else "'ordinary':1"))

(def ^:dynamic *build-wrapper*
  "REPL hook for profiling only the CREATE INDEX interval."
  (fn [build!] (build!)))

(defn- run-build [n]
  (require 'datahike.index.secondary.scriptum)
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :writer {:backend :self :writer-ownership :exclusive}
             :schema-flexibility :write
             :allow-index-backfill? true
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          handler (pg/make-query-handler
                   conn {:secondary-index-build-timeout-ms 300000})]
      (try
        (checked handler
                 "CREATE TABLE scriptum_build (id int PRIMARY KEY, body tsvector)")
        (let [load-start (System/nanoTime)]
          (doseq [batch (partition-all 1000 (range n))]
            (d/transact
             conn
             (mapv (fn [i]
                     {:scriptum_build/db-row-exists true
                      :scriptum_build/id i
                      :scriptum_build/body (body-value i)})
                   batch)))
          (let [load-ms (elapsed-ms load-start)
                scan-start (System/nanoTime)
                scanned (reduce (fn [n _] (unchecked-inc n))
                                0
                                (d/datoms (d/db conn)
                                          :aevt :scriptum_build/body))
                primary-scan-ms (elapsed-ms scan-start)
                build-start (System/nanoTime)]
            (*build-wrapper*
             #(checked handler
                       (str "CREATE INDEX scriptum_build_body_gin "
                            "ON scriptum_build USING gin (body)")))
            (let [build-ms (elapsed-ms build-start)]
              {:rows n
               :load-ms load-ms
               :primary-scan-ms primary-scan-ms
               :primary-scan-rows scanned
               :build-ms build-ms
               :build-rows-per-second (/ (* 1000.0 n) build-ms)})))
        (finally
          (.close handler)
          (d/release conn)
          (d/delete-database cfg))))))

(defn- row-counts []
  (mapv parse-long
        (str/split (or (System/getenv "SECONDARY_BUILD_ROWS")
                       "10000,100000")
                   #",")))

(doseq [n (row-counts)]
  (prn (run-build n)))
