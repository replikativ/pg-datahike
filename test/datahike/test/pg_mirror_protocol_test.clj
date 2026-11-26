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

(deftest a-catch-up-that-advances-has-no-deadline-and-a-stalled-one-reconnects
  (with-redefs-fn {#'mirror/stall-timeout-nanos 1000000000}
    (fn []
      (is (nil? (#'mirror/require-progress! (System/nanoTime) "stalled"))
          "progress inside the window is not a failure, however long the catch-up has run")
      (let [error (try (#'mirror/require-progress! (- (System/nanoTime) 2000000000) "stalled")
                       (catch SQLException e e))]
        (is (instance? SQLException error))
        (is (#'mirror/retryable? error)
            "a stall reconnects from the durable checkpoint instead of pausing for good")
        (is (= {:reason :replication-stalled :sqlstate "08006"}
               (select-keys (#'mirror/mirror-error error) [:reason :sqlstate])))))))

(deftest a-worker-ended-by-an-error-reports-failure
  (let [root (.toFile (Files/createTempDirectory "pgdh-worker-error-" (make-array FileAttribute 0)))]
    (with-redefs-fn {#'mirror/open-store (fn [& _] (throw (OutOfMemoryError. "simulated")))}
      (fn []
        (let [subscriber (mirror/start! {:source-url "jdbc:postgresql://127.0.0.1:1/none?user=mirror"
                                         :publication "analytics"
                                         :store-path (str (io/file root "store"))})]
          (.join ^Thread (:worker subscriber) 10000)
          (is (not (.isAlive ^Thread (:worker subscriber))))
          (is (= :failed (:state (mirror/status subscriber))))
          (is (false? (:serving? (mirror/status subscriber)))))))))

(deftest publisher-commits-are-recorded-at-their-commit-time
  (let [last-millis (volatile! nil)
        stamp #(.getTime ^java.util.Date (#'mirror/source-instant! last-millis %))]
    (is (= 946684800001 (stamp 1500)) "microseconds since 2000-01-01 become Unix milliseconds")
    (is (= 946684800002 (stamp 2000)))
    (is (= 946684800002 (stamp 1000))
        "a commit timestamp that goes backwards takes the instant before it")
    (is (= 946684800002 (stamp 2999)) "commits within one millisecond share an instant")
    (is (nil? (#'mirror/source-instant! last-millis nil))))
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write :keep-history? true}
        instant #inst "2020-03-04T05:06:07.000-00:00"]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try
        (with-open [destination (pg/make-query-handler conn)]
          (#'mirror/sink-execute! destination "CREATE TABLE dated (id bigint PRIMARY KEY)")
          (#'mirror/transaction! destination instant
                                 #(#'mirror/sink-execute! destination "INSERT INTO dated VALUES (1)"))
          (is (= instant (:db/txInstant (d/entity (d/db conn) (:max-tx (d/db conn))))))
          (is (= [[1]] (vec (d/q '[:find ?id :where [?e :dated/id ?id]]
                                 (d/as-of (d/db conn) instant)))))
          (is (empty? (d/q '[:find ?id :where [?e :dated/id ?id]]
                           (d/as-of (d/db conn) #inst "2020-03-04T05:06:06.999-00:00")))
              "the row is absent one millisecond before the publisher committed it"))
        (finally (d/release conn) (d/delete-database config))))))

(deftest an-unfinished-snapshot-is-discarded-and-a-finished-store-is-kept
  (let [root (.toFile (Files/createTempDirectory "pgdh-unfinished-snapshot-" (make-array FileAttribute 0)))
        config {:directory (str (io/file root "store")) :max-in-flight-commits 1}
        origin {:system "1" :timeline 1 :database "source"}
        conn (#'mirror/open-store (:directory config) 1)]
    (with-open [destination (pg/make-query-handler conn)]
      (#'mirror/init-checkpoint! destination origin "pgdh_mirror_slot" "analytics")
      (#'mirror/sink-execute! destination "CREATE TABLE partial (id bigint PRIMARY KEY)")
      (#'mirror/sink-execute! destination "INSERT INTO partial VALUES (1)"))
    (let [fresh (#'mirror/discard-incomplete-snapshot! conn config)]
      (try
        (is (not (identical? conn fresh)))
        (is (empty? (d/q '[:find ?e :where [?e :partial/id _]] (d/db fresh)))
            "rows of the unfinished copy are gone")
        (is (= {:origin origin :slot "pgdh_mirror_slot" :publication "analytics"}
               (select-keys (#'mirror/checkpoint (d/db fresh)) [:origin :slot :publication]))
            "the slot name survives, so the next attempt can drop that slot")
        (with-open [destination (pg/make-query-handler fresh)]
          (#'mirror/transaction! destination
                                 #(#'mirror/update-checkpoint! destination {:end-lsn nil} {:end-lsn "0/10"})))
        (is (identical? fresh (#'mirror/discard-incomplete-snapshot! fresh config))
            "a store with a finished snapshot is left alone")
        (finally (d/release fresh))))))

(deftest the-instant-floor-is-the-last-publisher-dated-transaction
  (let [root (.toFile (Files/createTempDirectory "pgdh-instant-floor-" (make-array FileAttribute 0)))
        conn (#'mirror/open-store (str (io/file root "store")) 1)
        instant #inst "2020-03-04T05:06:07.000-00:00"]
    (try
      (with-open [destination (pg/make-query-handler conn)]
        (#'mirror/init-checkpoint! destination {:system "1" :timeline 1 :database "source"}
                                   "pgdh_mirror_slot" "analytics")
        (is (nil? (#'mirror/source-floor-millis (d/db conn)))
            "a checkpoint without an end LSN has no publisher-dated transaction")
        (#'mirror/transaction! destination instant
                               #(#'mirror/update-checkpoint! destination {:end-lsn nil} {:end-lsn "0/10"}))
        ;; A later local transaction at the mirror's clock, like the one a new
        ;; query handler commits after a restart.
        (#'mirror/sink-execute! destination "CREATE TABLE local_only (id bigint PRIMARY KEY)")
        (is (< (.getTime instant)
               (.getTime ^java.util.Date (:db/txInstant (d/entity (d/db conn) (:max-tx (d/db conn)))))))
        (is (= (.getTime instant) (#'mirror/source-floor-millis (d/db conn)))
            "a backlog replayed after the restart is not raised to the local transaction's time"))
      (finally (d/release conn)))))

(deftest an-interrupted-store-replacement-finishes-on-the-next-start
  (let [root (.toFile (Files/createTempDirectory "pgdh-store-reset-" (make-array FileAttribute 0)))
        directory (str (io/file root "store"))
        origin {:system "1" :timeline 1 :database "source"}]
    ;; The state after a kill between deleting the old store and recording the
    ;; slot in the new one: no store, only the reset file.
    (spit (str directory ".mirror-reset")
          (pr-str {:origin origin :slot "pgdh_mirror_slot" :publication "analytics"}))
    (#'mirror/finish-store-reset! directory 1)
    (is (not (.exists (io/file (str directory ".mirror-reset")))))
    (let [conn (#'mirror/open-store directory 1)]
      (try
        (is (= {:origin origin :slot "pgdh_mirror_slot" :publication "analytics"}
               (select-keys (#'mirror/checkpoint (d/db conn)) [:origin :slot :publication]))
            "the slot name is still known, so the next attempt drops that slot")
        (finally (d/release conn))))
    (is (nil? (#'mirror/finish-store-reset! directory 1)) "nothing to do without a reset file")))

(deftest a-reset-file-that-reappears-does-not-delete-a-finished-store
  (let [root (.toFile (Files/createTempDirectory "pgdh-stale-reset-" (make-array FileAttribute 0)))
        directory (str (io/file root "store"))
        conn (#'mirror/open-store directory 1)]
    (try
      (with-open [destination (pg/make-query-handler conn)]
        (#'mirror/init-checkpoint! destination {:system "1" :timeline 1 :database "source"}
                                   "pgdh_mirror_slot" "analytics")
        (#'mirror/transaction! destination
                               #(#'mirror/update-checkpoint! destination {:end-lsn nil} {:end-lsn "0/10"})))
      (finally (d/release conn)))
    ;; A power loss can bring back a reset file whose removal was not durable.
    (spit (str directory ".mirror-reset")
          (pr-str {:origin {:system "1" :timeline 1 :database "source"}
                   :slot "pgdh_mirror_slot" :publication "analytics"}))
    (#'mirror/finish-store-reset! directory 1)
    (is (not (.exists (io/file (str directory ".mirror-reset")))))
    (let [conn (#'mirror/open-store directory 1)]
      (try
        (is (= "0/10" (:end-lsn (#'mirror/checkpoint (d/db conn))))
            "the finished snapshot and its position are still there")
        (finally (d/release conn))))))

(deftest the-store-lock-survives-reading-its-marker
  (let [root (.toFile (Files/createTempDirectory "pgdh-store-lock-" (make-array FileAttribute 0)))
        directory (str (io/file root "store"))]
    ;; The first acquisition writes the marker; the second reads it back.
    (.close ^java.nio.channels.FileChannel (#'mirror/acquire-store-lock directory))
    (with-open [_ ^java.nio.channels.FileChannel (#'mirror/acquire-store-lock directory)]
      (let [java (str (System/getProperty "java.home") "/bin/java")
            probe (str "(let [c (java.nio.channels.FileChannel/open (.toPath (java.io.File. \"" directory ".mirror-lock\"))"
                       " (into-array java.nio.file.StandardOpenOption [java.nio.file.StandardOpenOption/WRITE]))]"
                       " (System/exit (if (.tryLock c) 3 0)))")
            process (.start (doto (ProcessBuilder. ^java.util.List [java "-cp" (System/getProperty "java.class.path")
                                                                    "clojure.main" "-e" probe])
                              (.redirectErrorStream true)))]
        (is (= 0 (.waitFor process))
            "another process cannot take the lock while this one holds it")))))
