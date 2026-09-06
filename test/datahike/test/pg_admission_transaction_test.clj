(ns datahike.test.pg-admission-transaction-test
  "Admission ordering experiments using only APIs present before Datahike #1074."
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.tx-preds :as predicates]
            [datahike.writer :as writer]
            [datahike.writing :as writing]))

(defn- await-result [channel]
  (let [[result selected] (async/alts!! [channel (async/timeout 10000)])]
    (if (= selected channel) result ::timeout)))

(defn- validate! [db]
  (when (some neg? (d/q '[:find [?v ...] :where [_ :admission/value ?v]] db))
    (throw (ex-info "invalid admission value" {:type ::invalid}))))

(defn- dispatch [conn tx-data]
  (writer/dispatch! (:writer @(:wrapped-atom conn))
                    {:op 'transact! :args [{:tx-data tx-data}]}))

(deftest admission-validation-is-reexecuted-after-head-conflict
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :read :keep-history? false
                :writer {:backend :self :writer-ownership :shared
                         :head-conflict-backoff-ms 0}}
        attempts (atom 0)
        scans (atom 0)]
    (d/create-database config)
    (let [conn (d/connect config)
          original writing/commit!]
      (try
        (with-redefs [writing/commit!
                      (fn [& args]
                        (if (= 1 (swap! attempts inc))
                          (async/to-chan!
                           [(ex-info "forced admission conflict"
                                     {:type :konserve/revision-mismatch})])
                          (apply original args)))]
          (is (map? (await-result
                     (dispatch conn [[:db.fn/call
                                      (fn [db]
                                        (swap! scans inc)
                                        (validate! db)
                                        [])]]))))
          (is (= 2 @attempts))
          (is (= 2 @scans)
              "retry must run validation, not reuse a prior scan result"))
        (finally (d/release conn) (d/delete-database config))))))

(deftest admission-validates-pre-registration-in-flight-writes
  (doseq [ownership [:exclusive :shared]
          conflict? [false true]
          :when (or (= ownership :shared) (not conflict?))]
    (testing (str ownership " conflict=" conflict?)
      (let [id (random-uuid)
            config {:store {:backend :memory :id id}
                    :schema-flexibility :read :keep-history? false
                    :writer {:backend :self :writer-ownership ownership
                             :head-conflict-backoff-ms 0}}
            entered (promise)
            release (promise)
            attempts (atom 0)
            scans (atom 0)]
        (d/create-database config)
        (let [conn (d/connect config)
              original writing/commit!]
          (try
            (with-redefs [writing/commit!
                          (fn [& args]
                            (if (= 1 (swap! attempts inc))
                              (async/go
                                (deliver entered true)
                                @release
                                (if conflict?
                                  (ex-info "forced head conflict"
                                           {:type :konserve/revision-mismatch})
                                  (async/<! (apply original args))))
                              (apply original args)))]
              ;; This report passed the writer's predicate hook before a guard
              ;; existed. Hold its publication, not transaction processing.
              (let [old-write (dispatch conn [{:admission/value -1}])]
                (is (= true (deref entered 10000 ::timeout)))
                (predicates/register-tx-pred! id #(validate! (:db-after %)))
                (let [admission
                      (dispatch conn
                                [[:db.fn/call
                                  (fn [db]
                                    (swap! scans inc)
                                    (validate! db)
                                    [])]])]
                  (deliver release true)
                  (let [old-result (await-result old-write)
                        result (await-result admission)]
                    (if conflict?
                      (do
                        (is (instance? Throwable old-result)
                            "a replay must encounter the newly registered guard")
                        (is (map? result))
                        (is (empty? (d/q '[:find ?v :where [_ :admission/value ?v]]
                                         (d/db conn)))))
                      (do
                        (is (map? old-result))
                        (is (instance? Throwable result)
                            "admission must reject the earlier invalid write")))
                    (is (pos? @scans))))))
            (finally
              (deliver release true)
              (predicates/unregister-tx-pred! id)
              (d/release conn)
              (d/delete-database config))))))))
