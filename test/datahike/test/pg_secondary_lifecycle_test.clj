(ns datahike.test.pg-secondary-lifecycle-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.gc-guard :as guard]
            [datahike.index.secondary :as secondary]
            [datahike.pg.server :as server]))

(defonce slow-build-control (atom nil))

(defrecord SlowIndex [attrs]
  secondary/ISecondaryIndex
  (-search [_ _ _] nil)
  (-estimate [_ _] 0)
  (-can-order? [_ _ _] false)
  (-slice-ordered [_ _ _ _ _ _] nil)
  (-indexed-attrs [_] attrs)
  (-transact [this _]
    (when-let [{:keys [entered release finished blocked?]} @slow-build-control]
      (when (compare-and-set! blocked? false true)
        (deliver entered true)
        @release
        (deliver finished true)))
    this))

(defonce _register-slow-index
  (secondary/register-index-type!
   :test/pg-slow-index
   (fn [config _db]
     (->SlowIndex (set (:attrs config))))))

(defrecord FailingIndex [attrs]
  secondary/ISecondaryIndex
  (-search [_ _ _] nil)
  (-estimate [_ _] 0)
  (-can-order? [_ _ _] false)
  (-slice-ordered [_ _ _ _ _ _] nil)
  (-indexed-attrs [_] attrs)
  (-transact [_ _]
    (throw (ex-info "pg lifecycle adapter failure"
                    {:type :test/pg-adapter-failure}))))

(defonce _register-failing-index
  (secondary/register-index-type!
   :test/pg-failing-index
   (fn [config _db]
     (->FailingIndex (set (:attrs config))))))

(def ^:private await-secondary-ready!
  (ns-resolve 'datahike.pg.server 'await-secondary-ready!))

(defn- config []
  {:store {:backend :memory :id (random-uuid)}
   :writer {:backend :self :writer-ownership :exclusive}
   :keep-history? false
   :schema-flexibility :write})

(defn- thrown [f]
  (try
    (f)
    nil
    (catch Throwable failure failure)))

(deftest timed-out-sql-wait-cancels-the-observed-generation
  (testing "timeout retracts the declaration and clears buffered deltas"
    (let [cfg (config)
          entered (promise)
          release (promise)
          finished (promise)]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (d/transact conn [{:db/ident :item/slow-value
                             :db/valueType :db.type/string
                             :db/cardinality :db.cardinality/one}])
          (d/transact conn [{:item/slow-value "before"}])
          (reset! slow-build-control
                  {:entered entered
                   :release release
                   :finished finished
                   :blocked? (atom false)})
          (d/transact conn [{:db/ident :idx/pg-slow
                             :db.secondary/type :test/pg-slow-index
                             :db.secondary/attrs [:item/slow-value]}])
          (is (= true (deref entered 5000 ::timeout)))
          ;; Exercise journal cleanup as well as declaration cleanup.
          (d/transact conn [{:item/slow-value "during"}])
          (let [failure (thrown #(await-secondary-ready!
                                  conn :idx/pg-slow 1))]
            (is (= :query-canceled (:error (ex-data failure))))
            (is (re-find #"asynchronous build was canceled"
                         (ex-message failure))))
          (is (nil? (get-in (d/db conn) [:schema :idx/pg-slow])))
          (is (nil? (get-in (d/db conn)
                            [:secondary-indices :idx/pg-slow])))
          (is (nil? (:secondary-index-build-deltas (d/db conn))))
          (deliver release true)
          (is (= true (deref finished 5000 ::timeout)))
          (let [deadline (+ (System/currentTimeMillis) 5000)]
            (loop []
              (when (and (guard/in-flight? (get-in cfg [:store :id]))
                         (< (System/currentTimeMillis) deadline))
                (Thread/sleep 10)
                (recur))))
          (is (not (guard/in-flight? (get-in cfg [:store :id]))))
          (is (nil? (get-in (d/db conn) [:schema :idx/pg-slow]))
              "the detached worker cannot publish after SQL cancellation")
          (finally
            (deliver release true)
            (reset! slow-build-control nil)
            (d/release conn)
            (d/delete-database cfg)))))))

(deftest asynchronous-adapter-failure-reaches-the-sql-waiter
  (testing "automatic cleanup is reported as prerequisite-state failure"
    (let [cfg (config)]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (d/transact conn [{:db/ident :item/failing-value
                             :db/valueType :db.type/string
                             :db/cardinality :db.cardinality/one}])
          (d/transact conn [{:item/failing-value "before"}])
          (d/transact conn [{:db/ident :idx/pg-failing
                             :db.secondary/type :test/pg-failing-index
                             :db.secondary/attrs [:item/failing-value]}])
          (let [failure (thrown #(await-secondary-ready!
                                  conn :idx/pg-failing 5000))]
            (is (= :object-not-in-prerequisite-state
                   (:error (ex-data failure))))
            (is (re-find #"pg lifecycle adapter failure"
                         (ex-message failure))))
          (is (nil? (get-in (d/db conn) [:schema :idx/pg-failing])))
          (is (nil? (:secondary-index-build-deltas (d/db conn))))
          (is (= :test/pg-adapter-failure
                 (get-in (d/db conn)
                         [:secondary-index-build-failures :idx/pg-failing
                          :type])))
          (finally
            (d/release conn)
            (d/delete-database cfg)))))))
