;; Seed pgwire server for Metabase integration testing.
;;
;; Boots a Datahike pgwire on :15432 with a fixture DB that exercises
;; the catalog shapes Metabase probes during sync (PK / UNIQUE / FK /
;; CHECK + scalar / string / instant column types). Logs every SQL
;; statement Metabase sends to stdout so the inspector run.sh / dev
;; REPL can tail it. Also starts an nREPL on :15433 so a side-by-side
;; Datalog session can inspect the live conn while Metabase queries it.
;;
;; Usage:
;;   clojure -M:dev test/integration/metabase/seed.clj
;;
;; Stops on SIGINT.

(require '[datahike.api :as d]
         '[datahike.pg :as pg]
         '[nrepl.server :as nrepl])

(def cfg {:store              {:backend :memory :id (java.util.UUID/randomUUID)}
          :schema-flexibility :write
          :keep-history?      false})

(d/create-database cfg)
(def conn (d/connect cfg))

(println "[seed] populating fixture schema + data")
(d/transact conn [{:db/ident :customer/id
                   :db/valueType :db.type/long :db/cardinality :db.cardinality/one
                   :db/unique :db.unique/identity}
                  {:db/ident :customer/email
                   :db/valueType :db.type/string :db/cardinality :db.cardinality/one
                   :db/unique :db.unique/value}
                  {:db/ident :customer/name
                   :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                  {:db/ident :customer/age
                   :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
                  {:db/ident :customer/created_at
                   :db/valueType :db.type/instant :db/cardinality :db.cardinality/one}
                  {:db/ident :order/id
                   :db/valueType :db.type/long :db/cardinality :db.cardinality/one
                   :db/unique :db.unique/identity}
                  {:db/ident :order/customer
                   :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                  {:db/ident :order/total_cents
                   :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
                  {:db/ident :order/status
                   :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])

(d/transact conn
            (let [now #(java.util.Date.)]
              [{:customer/id 1 :customer/email "alice@example.com" :customer/name "Alice"
                :customer/age 30 :customer/created_at (now)}
               {:customer/id 2 :customer/email "bob@example.com" :customer/name "Bob"
                :customer/age 28 :customer/created_at (now)}
               {:customer/id 3 :customer/email "carol@example.com" :customer/name "Carol"
                :customer/age 41 :customer/created_at (now)}]))

(d/transact conn
            [{:order/id 100 :order/customer [:customer/id 1] :order/total_cents 4500 :order/status "paid"}
             {:order/id 101 :order/customer [:customer/id 1] :order/total_cents 1200 :order/status "paid"}
             {:order/id 102 :order/customer [:customer/id 2] :order/total_cents 9999 :order/status "pending"}
             {:order/id 103 :order/customer [:customer/id 3] :order/total_cents  500 :order/status "paid"}])

;; on-query hook — dump every SQL Metabase sends with a query counter.
;; Toggle with DATAHIKE_PG_TRACE=0 to silence; default is on for
;; Metabase debugging since the whole point of this seed is inspection.
(def query-counter (atom 0))
(def trace? (not= "0" (or (System/getenv "DATAHIKE_PG_TRACE") "1")))

(println "[seed] starting pgwire server on :15432 (trace =" trace? ")")
(def server
  (pg/start-server {"datahike" conn}
                   {:port 15432
                    :on-query (when trace?
                                (fn [sql]
                                  (let [n (swap! query-counter inc)
                                        snippet (-> (str sql)
                                                    (clojure.string/replace #"\s+" " ")
                                                    clojure.string/trim)]
                                    (println (format "[q%04d] %s" n snippet)))))}))

;; Expose conn + server as user-ns vars so the nREPL session can poke
;; at them without re-establishing.
(intern 'user 'conn conn)
(intern 'user 'server server)

(def nrepl-port (Integer/parseInt (or (System/getenv "DATAHIKE_NREPL_PORT") "15433")))
(def nrepl-server (nrepl/start-server :port nrepl-port :bind "0.0.0.0"))
(println (format "[seed] nREPL listening on :%d  (clj-nrepl-eval -p %d \"…\")" nrepl-port nrepl-port))

(println "[seed] ready — Metabase can now connect to localhost:15432")
;; Block until interrupted.
@(promise)
