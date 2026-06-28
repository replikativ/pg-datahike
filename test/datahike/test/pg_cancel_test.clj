(ns datahike.test.pg-cancel-test
  "End-to-end test that CancelRequest interrupts an in-flight query.

   Boots a PgWireServer over an in-memory DB and races a query against a
   Statement.cancel() — which issues a PostgreSQL CancelRequest on a
   parallel connection. The server flips its per-session cancel flag and
   interrupts the backend thread; the query engine's check-cancel!
   raises 57014 and pgJDBC surfaces it to the runner future as a
   SQLException.

   Timing is eliminated: the test handler hooks :on-query to (a) signal
   the test thread that dispatch has begun, then (b) park on
   LockSupport.parkNanos until its own thread is interrupted. The
   CancelRequest's safety-net interrupt is what wakes parkNanos, so the
   server thread is guaranteed to enter d/q *after* the cancel flag has
   been set — the first check-cancel! on the scan path raises 57014
   regardless of scan duration. No Thread/sleep coordination.

   Requires the query planner: legacy engine has no cancel points. Set
   DATAHIKE_QUERY_PLANNER=true when running the test suite."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.query :as q])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager SQLException Statement]
           [java.util.concurrent CountDownLatch TimeUnit]
           [java.util.concurrent.locks LockSupport]))

(def ^:private cancel-schema
  [{:db/ident :x/id  :db/valueType :db.type/long   :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :x/val :db/valueType :db.type/long   :db/cardinality :db.cardinality/one}])

(def ^:dynamic *port* nil)
(def ^:dynamic *cancel-landed* nil)

(defn- load-db [conn n]
  (d/transact conn cancel-schema)
  (d/transact conn (into [] (for [i (range n)] {:x/id i :x/val (mod i 100)}))))

;; One server, one DB, one CountDownLatch template — shared across the
;; three read-only tests in this ns. Per-test fresh coordination state
;; lives in `reset-latches!`. Saves ~50s of fixture transacting that
;; the per-test :each setup would repeat.
(def ^:dynamic ^:private *server-state* nil)

(defn- reset-latches! [state]
  (reset! (:dispatch-started state) (CountDownLatch. 1))
  (reset! (:cancel-landed state) (promise)))

(defn- cancel-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}
        ;; The pgwire connection runs on its own virtual thread, so a
        ;; thread-local `binding` from the test thread wouldn't reach
        ;; the handler. Flip the root so every thread sees the planner.
        prev-disable-planner q/*disable-planner*]
    (alter-var-root #'q/*disable-planner* (constantly false))
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          ;; Hold the mutable latches in atoms so each test resets them
          ;; without rebuilding the fixture.
          state {:dispatch-started (atom (CountDownLatch. 1))
                 :cancel-landed    (atom (promise))}]
      (load-db conn 1000)
      (let [factory (reify PgWireServer$QueryHandlerFactory
                      (create [_]
                        (pg/make-query-handler
                         conn
                         {:on-query
                          (fn [^String sql]
                            (when (and (re-find #"(?i)from\s+x\b" sql)
                                       (re-find #"(?i)order\s+by" sql))
                              ;; Signal dispatch-started, then park
                              ;; indefinitely. The CancelRequest
                              ;; safety-net interrupt wakes us.
                              ;; parkNanos doesn't throw on interrupt
                              ;; — it just returns — so we fall
                              ;; straight into d/q with the flag set.
                              (.countDown ^CountDownLatch @(:dispatch-started state))
                              (LockSupport/parkNanos (* 10 1000 1000 1000)) ;; 10s ceiling
                              (deliver @(:cancel-landed state) true)))})))
            server (PgWireServer. 0 "127.0.0.1" factory)]
        (.start server)
        (try
          (binding [*port* (.getPort server)
                    *server-state* state]
            (f))
          (finally
            (.stop server)
            (d/release conn)
            (d/delete-database cfg)
            (alter-var-root #'q/*disable-planner* (constantly prev-disable-planner))))))))

(use-fixtures :once cancel-fixture)

(defn- open ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/datahike?user=datahike&password=x&sslmode=disable&binaryTransfer=false")))

(deftest cancel-request-interrupts-long-scan
  (reset-latches! *server-state*)
  (testing "Statement.cancel() issues CancelRequest; query raises 57014"
    (with-open [c (open)]
      (with-open [st (.createStatement c)]
        (let [sql "SELECT id, val FROM x ORDER BY val DESC, id DESC"
              result (promise)
              runner (future
                       (try
                         (with-open [rs (.executeQuery st sql)]
                           (let [rows (loop [n 0]
                                        (if (.next rs) (recur (inc n)) n))]
                             (deliver result [:done rows])))
                         (catch SQLException e
                           (deliver result [:sqlstate (.getSQLState e) (.getMessage e)]))
                         (catch Throwable t
                           (deliver result [:threw (class t) (.getMessage t)]))))]
          (is (.await ^CountDownLatch @(:dispatch-started *server-state*) 10 TimeUnit/SECONDS)
              "on-query never fired — server did not begin dispatching the SELECT")
          (.cancel st)
          ;; Wait for the server thread to exit parkNanos, confirming
          ;; the cancel's safety-net interrupt landed. After this,
          ;; the flag is set and the first check-cancel! in d/q raises.
          (is (= true (deref @(:cancel-landed *server-state*) 10000 :timeout))
              "cancel's safety-net interrupt never woke on-query")
          (let [outcome (deref result 10000 [:timeout])]
            @runner
            (is (= :sqlstate (first outcome))
                (format "expected SQLException from cancel, got %s" outcome))
            (is (= "57014" (second outcome))
                (format "expected SQLSTATE 57014 (query_canceled), got %s" outcome))))))))

(deftest cancel-without-running-query-is-harmless
  (reset-latches! *server-state*)
  (testing "Statement.cancel() with no query in-flight is a no-op"
    (with-open [c (open)]
      (with-open [st (.createStatement c)]
        (.cancel st)
        (is (.isValid c 1)
            "connection remains usable after spurious cancel")
        (with-open [rs (.executeQuery st "SELECT 1")]
          (is (.next rs))
          (is (= 1 (.getInt rs 1))))))))

(deftest connection-survives-cancel
  (reset-latches! *server-state*)
  (testing "after cancel, the same connection runs the next query normally"
    (with-open [c (open)]
      (with-open [st (.createStatement c)]
        (let [sql "SELECT id, val FROM x ORDER BY val DESC, id DESC"
              runner (future
                       (try (with-open [rs (.executeQuery st sql)]
                              (loop [] (when (.next rs) (recur))))
                            :done
                            (catch SQLException e (.getSQLState e))))]
          (is (.await ^CountDownLatch @(:dispatch-started *server-state*) 10 TimeUnit/SECONDS))
          (.cancel st)
          (is (= true (deref @(:cancel-landed *server-state*) 10000 :timeout)))
          @runner)
        ;; Next query on the same connection must just work.
        (with-open [rs (.executeQuery st "SELECT COUNT(*) FROM x")]
          (is (.next rs))
          (is (= 1000 (.getLong rs 1))))))))
