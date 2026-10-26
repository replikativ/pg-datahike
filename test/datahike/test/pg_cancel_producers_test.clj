(ns datahike.test.pg-cancel-producers-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.window :as window])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]))

(def ^:dynamic *db* nil)

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [c (d/connect cfg)]
      (try
        (d/transact c [{:db/ident :cancel_source/id :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
                       {:db/ident :cancel_sink/id :db/valueType :db.type/long :db/cardinality :db.cardinality/one}])
        (d/transact c [{:cancel_source/id 1} {:cancel_source/id 2}])
        (binding [*db* (d/db c) params/*parse-sql* sql/parse-sql] (f))
        (finally (d/release c) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(def query '{:find [?v] :in [$] :where [[?e :cancel_source/id ?v]]})
(defn- body [] (CCJSqlParserUtil/parse "SELECT id FROM cancel_source"))
(defn- derived [] (stmt/materialize-set-op! (body) "__cancel_rows" *db* (:schema *db*)))
(defn- rule [] (stmt/run-recursive-rule *db* '[[(walk ?v) [?e :cancel_source/id ?v]]]
                                        'walk '[?v] [] []))
(defn- insert-source []
  (let [p (sql/parse-sql "INSERT INTO cancel_sink SELECT id FROM cancel_source" (:schema *db*) *db*)]
    (stmt/materialize-insert-select (:insert-source p) *db* identity)))
(defn- row-query? [q]
  (some #(or (and (vector? %) (= :cancel_source/id (second %)))
             (and (seq? %) (= 'walk (first %)))
             (and (seq? %) (= 'get-else (first %)) (= :cancel_source/id (nth % 3 nil))))
        (tree-seq coll? seq q)))
(defn- canceled? [f]
  (try (f) false (catch Throwable e (boolean (:datahike/canceled (ex-data e))))))

(deftest statement-producers-pass-the-current-token-to-datahike
  (let [q d/q token (atom false) seen (atom [])]
    (binding [params/*cancel* token]
      (with-redefs [d/q (fn [query & args]
                          (when (row-query? query) (swap! seen conj (:cancel query)))
                          (apply q query args))]
        (doseq [[name produce] [["derived/CTE" derived]
                                ["constant fallback" #(stmt/run-const-select-row {:query query} *db* 1)]
                                ["recursive rule" rule]
                                ["INSERT source" insert-source]
                                ["LATERAL" #((#'stmt/lateral-rows-fn "SELECT id FROM cancel_source" [] (:schema *db*) *db* 1))]]]
          (testing name
            (reset! seen [])
            (produce)
            (is (seq @seen))
            (is (every? #(identical? token %) @seen))))))))

(deftest preset-cancel-stops-tiny-producers-and-reset-recovers
  (let [token (atom true)]
    (binding [params/*cancel* token]
      (doseq [produce [derived rule insert-source
                       #(stmt/run-const-select-row {:literal-row [1]} *db* 1)]]
        (is (canceled? produce)))
      (reset! token false)
      (is (= "__cancel_rows" (:name (derived))))
      (is (= #{[1] [2]} (set (rule)))))))

(deftest cancel-after-query-is-observed-during-materialization
  (let [q d/q token (atom false)]
    (binding [params/*cancel* token]
      (with-redefs [d/q (fn [query & args]
                          (let [rows (apply q query args)]
                            (when (row-query? query) (reset! token true))
                            rows))]
        (is (canceled? derived))))))

(deftest reused-lateral-closure-reads-the-execution-token
  (let [first-token (atom false) second-token (atom false)
        producer (binding [params/*cancel* first-token]
                   (#'stmt/lateral-rows-fn "SELECT id FROM cancel_source" [] (:schema *db*) *db* 1))
        q d/q seen (atom [])]
    (binding [params/*cancel* second-token]
      (with-redefs [d/q (fn [query & args]
                          (when (row-query? query) (swap! seen conj (:cancel query)))
                          (apply q query args))]
        (is (= #{[1] [2]} (set (producer))))
        (is (seq @seen))
        (is (every? #(identical? second-token %) @seen))
        (reset! second-token true)
        (is (canceled? producer))))))

(deftest window-postprocessing-honors-preexisting-cancellation
  (binding [params/*cancel* (atom true)]
    (is (canceled? #(window/execute-window-functions [[1] [2]] [{:op :row_number :partition-by [] :order-by [[0 :asc]]}])))))

(deftest deferred-rule-cancellation-is-never-a-capability-fallback
  (let [spec {:rule '[[(walk ?v) [?e :cancel_source/id ?v]]]
              :rule-name 'walk :rule-vars '[?v] :in-params [] :in-args []
              :col-names ["id"] :col-types [:db.type/long]
              :target-name "__cancel_rule" :row-marker :__cancel_rule/row}]
    (with-redefs [stmt/run-recursive-rule
                  (fn [& _] (throw (ex-info "producer canceled" {:datahike/canceled true})))]
      (is (canceled? #(stmt/materialize-recursive-rows! spec *db*))))
    (binding [params/*cancel* (atom true)]
      (with-redefs [stmt/ground-rule-params (fn [& _] (throw (AssertionError. "grounded after cancellation")))]
        (is (canceled? #(stmt/materialize-recursive-rows! spec *db*)))))))
