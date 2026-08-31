(ns scriptum-layer-breakdown
  "Separate Lucene field representation from Scriptum generation overhead.

   Run on the local secondary stack. The default is three 100k-document runs:

     clojure -J-Xmx4g -M:dev:local-secondary-stack bench/scriptum_layer_breakdown.clj

   Override the size and repetitions with SCRIPTUM_LAYER_ROWS and
   SCRIPTUM_LAYER_RUNS. Compare runs under one CPU governor; the absolute
   timings are intentionally not a CI assertion."
  (:require [konserve.memory :as km]
            [scriptum.core :as sc])
  (:import [org.apache.lucene.analysis.standard StandardAnalyzer]
           [org.apache.lucene.document Document Field$Store LongField
            StringField TextField]
           [org.apache.lucene.index IndexWriter IndexWriterConfig]
           [org.apache.lucene.store ByteBuffersDirectory]))

(defn- body-value [i]
  (cond
    (zero? (mod i 1000))
    "'database':2 'needle':1 'uncommon':3 'rare':4"

    (zero? (mod i 100))
    "'database':2 'needle':1 'uncommon':3"

    (zero? (mod i 10))
    "'database':2 'needle':1"

    :else "'ordinary':1"))

(defn- candidate-document [id-layout i]
  (let [document (Document.)]
    (.add document
          (case id-layout
            :string (StringField. "_entity_id" (str i) Field$Store/YES)
            :numeric (LongField. "_entity_id" (long i) Field$Store/YES)))
    (.add document (TextField. "value" (body-value i) Field$Store/NO))
    document))

(defn- elapsed-ms [start]
  (/ (double (- (System/nanoTime) start)) 1e6))

(defn- raw-lucene-build [id-layout n]
  (let [directory (ByteBuffersDirectory.)
        writer (IndexWriter. directory (IndexWriterConfig. (StandardAnalyzer.)))]
    (try
      (let [ingest-start (System/nanoTime)]
        (dotimes [i n]
          (.addDocument writer (candidate-document id-layout i)))
        (let [ingest-ms (elapsed-ms ingest-start)
              seal-start (System/nanoTime)]
          (.commit writer)
          (let [seal-ms (elapsed-ms seal-start)]
            {:ingest-ms ingest-ms
             :seal-ms seal-ms
             :total-ms (+ ingest-ms seal-ms)})))
      (finally
        (.close writer)
        (.close directory)))))

(defn- scriptum-build [n]
  (let [store-id (random-uuid)
        store (km/new-mem-store (atom {}) {:sync? true :id store-id})
        generation (sc/begin-generation
                    store (str "/tmp/scriptum-layer-" (random-uuid)) nil
                    {:store-id store-id
                     :workspace-id (str (random-uuid))})]
    (try
      (let [ingest-start (System/nanoTime)]
        (dotimes [i n]
          (sc/add-document generation (candidate-document :numeric i)))
        (let [ingest-ms (elapsed-ms ingest-start)
              seal-start (System/nanoTime)]
          (sc/seal-generation! generation "scriptum-layer-breakdown")
          (let [seal-ms (elapsed-ms seal-start)]
            (sc/release-generation! generation)
            {:ingest-ms ingest-ms
             :seal-ms seal-ms
             :total-ms (+ ingest-ms seal-ms)})))
      (finally
        (try
          (sc/abort-generation! generation)
          (catch Throwable _))
        (sc/close! generation)))))

(defn- env-long [name default]
  (if-let [value (System/getenv name)] (parse-long value) default))

(let [rows (env-long "SCRIPTUM_LAYER_ROWS" 100000)
      runs (env-long "SCRIPTUM_LAYER_RUNS" 3)]
  (doseq [run (range runs)
          [variant build!] [[:raw-string #(raw-lucene-build :string %)]
                            [:raw-numeric #(raw-lucene-build :numeric %)]
                            [:scriptum-numeric scriptum-build]]]
    (System/gc)
    (Thread/sleep 100)
    (prn {:variant variant :run run :rows rows :timing (build! rows)})))
