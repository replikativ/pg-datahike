(ns datahike.test.pg-cancel-context-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.expr :as expr]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.window :as window]))

(defn- state [f]
  (try (f) nil
       (catch Exception e (first (errors/classify-exception e)))))

(deftest engine-cancellation-and-timeouts-have-the-same-protocol-state
  (doseq [marker [{:datahike/canceled true} {:type :datahike/query-timeout}]]
    (is (= "57014" (state #(throw (ex-info "engine stopped" marker)))))
    (is (= "57014" (state #(throw (Exception. "wrapped" (ex-info "engine stopped" marker))))))))

(deftest cached-execution-uses-the-current-token
  (let [query {:find '[?x] :where '[[?x :x/id]]}
        run #(params/cancellable-query query)
        first-token (atom false)
        second-token (atom false)]
    (is (identical? first-token (:cancel (binding [params/*cancel* first-token] (run)))))
    (is (identical? second-token (:cancel (binding [params/*cancel* second-token] (run)))))
    (is (= query (run)))
    (reset! second-token true)
    (is (= "57014" (binding [params/*cancel* second-token] (state run))))
    (is (identical? first-token (:cancel (binding [params/*cancel* first-token] (run)))))
    (is (not (contains? query :cancel)))))

(deftest having-observes-cancellation-during-row-processing
  (let [cancel (atom false)
        calls (atom 0)]
    (binding [params/*cancel* cancel]
      (with-redefs [expr/interpret-form
                    (fn [_ _]
                      (when (= 10 (swap! calls inc)) (reset! cancel true))
                      true)]
        (is (= "57014"
               (state #(expr/apply-having (repeat 2000 [1]) {:form true :slots []}
                                          {:find '[?x]} []))))))
    (is (<= 10 @calls 522))
    (reset! cancel false)
    (binding [params/*cancel* cancel]
      (is (= [[1]] (expr/apply-having [[1]] {:form true :slots []}
                                      {:find '[?x]} []))))))

(deftest canceled-projection-does-not-evaluate-user-forms
  (let [calls (atom 0)]
    (binding [params/*cancel* (atom true)]
      (with-redefs [expr/interpret-form (fn [_ _] (swap! calls inc))]
        (is (= "57014"
               (state #(expr/apply-compound-projections
                        [[1]] ["x"] {:find '[?x]} []
                        [{:alias "computed" :form true :slots []}]))))))
    (is (zero? @calls))))

(deftest window-work-observes-cancellation-after-admission
  (let [polls (atom 0)
        token (reify clojure.lang.IDeref
                (deref [_] (>= (swap! polls inc) 3)))]
    (binding [params/*cancel* token]
      (is (= "57014"
             (state #(window/execute-window-functions
                      (mapv vector (range 2000))
                      [{:op :row_number :partition-by [] :order-by [[0 :asc]]}])))))
    (is (= 3 @polls))))

(deftest recursive-evaluator-cancellation-does-not-trigger-a-fallback
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}
        fallback-calls (atom 0)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (doseq [marker [{:datahike/canceled true} {:type :datahike/query-timeout}]]
          (with-redefs [stmt/materialize-recursive-cte!
                        (fn [& _] (throw (ex-info "engine stopped" marker)))
                        stmt/materialize-recursive-iterative!
                        (fn [& _] (swap! fallback-calls inc) nil)]
            (let [db (d/db conn)
                  plan (sql/parse-sql
                        (str "WITH RECURSIVE r(n) AS (SELECT 1 UNION ALL "
                             "SELECT n+1 FROM r WHERE n<3) SELECT n FROM r")
                        (:schema db) db)]
              (is (= "57014" (:sqlstate plan)))
              (is (zero? @fallback-calls)))))
        (finally (d/release conn) (d/delete-database cfg))))))
