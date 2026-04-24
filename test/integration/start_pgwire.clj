;; Start a PgWire server for integration testing.
;; Run with: clj -A:test -M test/integration/start_pgwire.clj
;; The server listens on port 15432 and blocks until killed.
;;
;; Also starts an nREPL on port 15433 for dual-access:
;;   SQL via pgwire on 15432 + Datalog via nREPL on 15433

(require '[datahike.api :as d]
         '[datahike.pg.server :as pg]
         '[nrepl.server :as nrepl])
(import '[datahike.pg PgWireServer])

(let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
           :schema-flexibility :write
           ;; DATAHIKE_KEEP_HISTORY=true to exercise history; default off for
           ;; integration tests where writes are the throughput bottleneck.
           :keep-history? (= "true" (System/getenv "DATAHIKE_KEEP_HISTORY"))}
      _ (d/create-database cfg)
      conn (d/connect cfg)
      handler-factory (reify datahike.pg.PgWireServer$QueryHandlerFactory
                        (create [_]
                          (pg/make-query-handler conn {:on-query (fn [sql]
                                                                   (when (System/getenv "DATAHIKE_SQL_DEBUG")
                                                                     (println "SQL:" sql)))})))
      port (Integer/parseInt (or (System/getenv "DATAHIKE_PG_PORT") "15432"))
      nrepl-port (Integer/parseInt (or (System/getenv "DATAHIKE_NREPL_PORT") "15433"))
      server (PgWireServer. port "0.0.0.0" handler-factory)]
  ;; Expose conn as a global var for REPL access
  (intern 'user 'conn conn)
  (intern 'user 'cfg cfg)
  (.start server)
  (let [nrepl-server (nrepl/start-server :port nrepl-port :bind "0.0.0.0")]
    (println (str "Datahike PgWire server listening on port " port))
    (println (str "nREPL server listening on port " nrepl-port))
    (println "  Connect: clj-nrepl-eval -p" nrepl-port "\"(d/q '{:find [?e ?a ?v] :where [[?e ?a ?v]]} (d/db conn))\"")
    (println "Press Ctrl+C to stop")
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (.stop server)
                                 (nrepl/stop-server nrepl-server)
                                 (d/release conn)
                                 (d/delete-database cfg)
                                 (println "Server stopped."))))
    (Thread/sleep Long/MAX_VALUE)))
