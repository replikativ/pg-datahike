;; Start a PgWire server for integration testing.
;; Run with: clj -A:test -M test/integration/start_pgwire.clj
;; The server listens on port 15432 and blocks until killed.
;;
;; Also starts an nREPL on port 15433 for dual-access:
;;   SQL via pgwire on 15432 + Datalog via nREPL on 15433
;;
;; CREATE DATABASE / DROP DATABASE are provisioned from an in-memory
;; template so clients that create their own throwaway databases (e.g.
;; asyncpg's test harness, which creates a fresh database per test class)
;; work AND get an isolated store — no cross-test pollution of the
;; default `datahike` db.

(require '[datahike.api :as d]
         '[datahike.pg.server :as pg]
         '[datahike.pg.dev :as dev]
         '[nrepl.server :as nrepl])

(let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
           :schema-flexibility :write
           ;; DATAHIKE_KEEP_HISTORY=true to exercise history; default off for
           ;; integration tests where writes are the throughput bottleneck.
           :keep-history? (= "true" (System/getenv "DATAHIKE_KEEP_HISTORY"))}
      _ (d/create-database cfg)
      conn (d/connect cfg)
      port (Integer/parseInt (or (System/getenv "DATAHIKE_PG_PORT") "15432"))
      nrepl-port (Integer/parseInt (or (System/getenv "DATAHIKE_NREPL_PORT") "15433"))
      {:keys [^datahike.pg.PgWireServer server registry-atom] :as srv}
      (pg/start-server
       {"datahike" conn}
       {:port port
        :host "0.0.0.0"
        ;; SQL CREATE/DROP DATABASE → fresh in-memory Datahike db.
        :database-template {:store {:backend :memory}
                            :schema-flexibility :write
                            :keep-history? false}
        ;; dev/on-query records per-connection in-flight SQL (always on, cheap)
        ;; so a REPL session can see what each connection is running and which
        ;; query is stuck during a hang. DATAHIKE_SQL_DEBUG=true also prints.
        :on-query (fn [sql]
                    (dev/on-query sql)
                    (when (System/getenv "DATAHIKE_SQL_DEBUG")
                      (println "SQL:" sql)))})]
  (when (System/getenv "DATAHIKE_SQL_DEBUG") (dev/set-trace! true))
  ;; Expose conn / registry + dev helpers as global vars for REPL access
  (intern 'user 'conn conn)
  (intern 'user 'cfg cfg)
  (intern 'user 'registry-atom registry-atom)
  (intern 'user 'srv srv)
  (let [nrepl-server (nrepl/start-server :port nrepl-port :bind "0.0.0.0")]
    (println (str "nREPL server listening on port " nrepl-port))
    (println "  Connect: clj-nrepl-eval -p" nrepl-port "\"(d/q '{:find [?e ?a ?v] :where [[?e ?a ?v]]} (d/db conn))\"")
    (println "Press Ctrl+C to stop")
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (pg/stop-server srv)
                                 (nrepl/stop-server nrepl-server)
                                 (d/release conn)
                                 (d/delete-database cfg)
                                 (println "Server stopped."))))
    (Thread/sleep Long/MAX_VALUE)))
