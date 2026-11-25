(ns datahike.test.pg-mirror-protocol-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.pg.mirror :as mirror]
            [datahike.pg.server :as pg]
            [datahike.tools :as tools]
            [datahike.writer :as writer]
            [datahike.writing :as writing])
  (:import [java.nio ByteBuffer]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.sql SQLException]))

(deftest publication-cli-hides-publisher-error-detail
  (with-redefs [mirror/publication-report (fn [_] (throw (SQLException. "private details" "28P01")))]
    (let [output (with-out-str
                   (is (= 1 (mirror/run-cli! {} true))))]
      (is (= {:compatible? false
              :last-error {:reason :publisher-error :sqlstate "28P01"}}
             (read-string output))))))

(deftest ordinary-transaction-limit-precedes-change-decode
  (let [begin (doto (ByteBuffer/allocate 21)
                (.put (byte (int \B)))
                (.putLong 16)
                (.putLong 0)
                (.putInt 7)
                (.flip))
        change (doto (ByteBuffer/allocate 64)
                 (.put (byte (int \I)))
                 (.position 0))
        frames (atom [begin change])]
    ;; The oversized frame deliberately lacks a valid tuple. Reaching the
    ;; decoder would raise a tuple error before the transaction limit.
    (with-redefs-fn {#'mirror/read-packet (fn [_] {:data (ffirst (swap-vals! frames subvec 1))})
                     #'mirror/feedback! (fn [& _])}
      (fn []
        (let [error (try
                      (#'mirror/stream! nil "0/1" "0/1" {:max-transaction-bytes 16}
                                        {:tick! (constantly nil) :check-running! (constantly nil)
                                         :select-change! identity})
                      nil
                      (catch clojure.lang.ExceptionInfo error error))]
          (is (= "Transaction exceeds the configured byte limit" (ex-message error)))
          (is (= {:xid 7 :limit 16} (ex-data error))))))))

(defn- begin-frame [xid]
  (doto (ByteBuffer/allocate 21)
    (.put (byte (int \B)))
    (.putLong xid)
    (.putLong 0)
    (.putInt xid)
    (.flip)))

(defn- commit-frame [xid]
  (doto (ByteBuffer/allocate 26)
    (.put (byte (int \C)))
    (.put (byte 0))
    (.putLong xid)
    (.putLong (inc xid))
    (.putLong 0)
    (.flip)))

(deftest feedback-waits-for-the-ordered-durable-prefix
  (let [frames (atom (vec (mapcat #(vector (begin-frame %) (commit-frame %)) [16 17 18])))
        pending [(tools/throwable-promise) (tools/throwable-promise)]
        submitted (atom [])
        durable (atom [])
        feedback (atom [])]
    (with-redefs-fn {#'mirror/read-packet
                     (fn [_]
                       (let [[before _] (swap-vals! frames subvec 1)]
                         (case (count before)
                           3 (deliver (second pending) {:durable? true})
                           2 (do (is (empty? @durable))
                                 (is (= #{"0/F"} (set @feedback)))
                                 (deliver (first pending) {:durable? true}))
                           nil)
                         {:data (first before)}))
                     #'mirror/feedback! (fn [_ position _] (swap! feedback conj position))}
      (fn []
        (is (= "0/13"
               (#'mirror/stream! nil "0/F" "0/F" {:max-transaction-bytes 1024 :max-in-flight-commits 2}
                                 {:tick! (constantly nil)
                                  :check-running! (constantly nil)
                                  :select-change! identity
                                  :submit! (fn [_ previous tx]
                                             (when-some [completion (get pending (count @submitted))]
                                               (swap! submitted conj [previous (:end-lsn tx)])
                                               completion))
                                  :committed! #(swap! durable conj (:end-lsn %))
                                  :commit! (fn [_ previous tx]
                                             (is (= "0/12" previous))
                                             (swap! durable conj (:end-lsn tx))
                                             true)})))
        (is (= [["0/F" "0/11"] ["0/11" "0/12"]] @submitted))
        (is (= ["0/11" "0/12" "0/13"] @durable))
        (is (= ["0/F" "0/11" "0/12" "0/13"] (vec (dedupe @feedback))))))))

(defn- await-writer [completion]
  (let [result (deref completion 10000 ::timed-out)]
    (when (= ::timed-out result)
      (throw (ex-info "Writer timed out in replay test" {})))
    result))

(deftest queued-sql-replay-preserves-grouped-history
  (let [root (.toFile (Files/createTempDirectory "pgdh-queued-replay-" (make-array FileAttribute 0)))
        directory (.getPath (io/file root "mirror"))
        table {:schema "public" :name "replay_rows"
               :columns [{:name "id" :oid 20 :attnum 1 :key? true}
                         {:name "payload" :oid 25 :attnum 2}]}
        tx (fn [xid end-lsn change]
             {:end-lsn end-lsn :xid xid :commit-micros xid :changes [(assoc change :relation-id 4)]})
        begin (tx 1 "0/2" {:op :insert :new ["8" "inserted"]})
        update (tx 2 "0/3" {:op :update :new ["8" "updated"]})
        delete (tx 3 "0/4" {:op :delete :old-kind \K :old ["8" nil]})
        value (fn [db]
                (with-open [handler (pg/make-query-handler (delay db) {:datahike.pg.server/embedded-staging? true})]
                  (let [result (#'mirror/sink-execute! handler "SELECT payload FROM public.replay_rows WHERE id = 8")]
                    (ffirst (.-rows result)))))]
    (try
      (let [[aid bid cid]
            (let [conn (#'mirror/open-store directory 8)
                  at-storage (promise)
                  release-storage (promise)
                  applied (promise)
                  old-commit @#'writing/commit!]
              (try
                (with-open [handler (pg/make-query-handler conn)]
                  (#'mirror/sink-execute! handler "CREATE TABLE public.replay_rows (id bigint PRIMARY KEY, payload text)")
                  (#'mirror/init-checkpoint! handler {:system-id "fixture"} "slot" "publication")
                  (#'mirror/apply-commit! handler {} nil {:end-lsn "0/1" :xid 0 :commit-micros 0})
                  (with-redefs-fn {#'writing/commit!
                                   (fn [& args]
                                     (deliver at-storage true)
                                     (await-writer release-storage)
                                     (apply old-commit args))}
                    (fn []
                      (let [warm (d/transact! conn [])
                            submit! (fn [previous tx]
                                      (d/transact! conn (writer/prepare-tx-data
                                                         #(#'mirror/prepare-replay-tx % {4 table} previous tx))))]
                        (try
                          (await-writer at-storage)
                          (let [one (submit! "0/1" begin)
                                two (submit! "0/2" update)
                                three (submit! "0/3" delete)
                                witness (d/transact! conn [[:db.fn/call (fn [_] (deliver applied true) [])]])]
                            (await-writer applied)
                            (deliver release-storage true)
                            (await-writer warm)
                            (let [reports (mapv await-writer [one two three])]
                              (await-writer witness)
                              (is (apply = (map #(get-in % [:tx-meta :db/commitId]) reports)))
                              (let [failed (submit! "0/99" (assoc begin :end-lsn "0/5"))
                                    successor (submit! "0/5" (assoc update :end-lsn "0/6"))]
                                (doseq [completion [failed successor]]
                                  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                                        #"checkpoint changed"
                                                        (await-writer completion)))))
                              (mapv #(get-in % [:tempids :db/current-tx]) reports)))
                          (finally (deliver release-storage true)))))))
                (finally (d/release conn))))
            conn (#'mirror/open-store directory 8)]
        (try
          (is (< aid bid cid))
          (is (= "inserted" (value (d/as-of @conn aid))))
          (is (= "updated" (value (d/as-of @conn bid))))
          (is (nil? (value (d/as-of @conn cid))))
          (is (= "0/4" (:end-lsn (#'mirror/checkpoint @conn))))
          (finally (d/release conn))))
      (finally
        (doseq [file (reverse (file-seq root))]
          (io/delete-file file true))))))
