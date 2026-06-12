(ns datahike.pg.dev
  "REPL-driven diagnostics for the pgwire integration server.

   The integration server (test/integration/start_pgwire.clj) starts an
   nREPL in the SAME JVM as the pgwire server, so everything is live-
   inspectable from a REPL — no separate process, no restart churn:

     • run SQL straight through a handler          (run-sql)
     • see what each connection is running          (in-flight-sql)
     • dump the live thread stacks when it hangs     (pgwire-threads, thread-dump, dump-to-file)
     • watch databases for accumulation / leaks      (registry-info)
     • clear test databases between runs             (clear-test-dbs!)

   Connect:  clj-nrepl-eval -p 15433 \"(require '[datahike.pg.dev :as dev]) (dev/pgwire-threads)\"

   Hang playbook: when a client stalls, call (dev/pgwire-threads) — the
   thread whose :state is RUNNABLE/WAITING with datahike/query frames in
   :top is the culprit, and its :sql is the query that hung."
  (:require [datahike.api :as d]
            [datahike.pg.server :as pg]
            [clojure.string :as str])
  (:import [datahike.pg PgWireServer$QueryHandler]))

;; ===========================================================================
;; In-flight SQL tracking + trace toggle
;; ===========================================================================
;; start_pgwire.clj wires `on-query` as the server's :on-query hook, so the
;; last SQL each connection thread started is always recorded. On a hang the
;; stuck thread's entry is the offending query (it never gets overwritten by
;; a later one on that connection).

(defonce ^{:doc "thread-name -> {:sql :at-ms}"} in-flight (atom {}))
(defonce ^{:doc "when true, every SQL is printed to the server console"} trace? (atom false))

(defn on-query
  "Server :on-query hook — records per-thread in-flight SQL (+ optional trace)."
  [sql]
  (let [t (Thread/currentThread)]
    (swap! in-flight assoc (.getName t)
           {:sql sql :at-ms (quot (System/nanoTime) 1000000)})
    (when @trace? (println "SQL>" (.getName t) "|" sql))))

(defn set-trace! [on?] (reset! trace? (boolean on?)))

(defn in-flight-sql
  "Map of connection-thread -> last SQL it started. On a hang, the stuck
   thread's entry names the query."
  []
  @in-flight)

;; ===========================================================================
;; Thread X-ray — the live server runs in this JVM, so we can read every
;; thread's stack directly (a REPL-native jstack).
;; ===========================================================================

(defn thread-dump
  "All live threads as a sorted map name -> {:state :stack}. `:grep` keeps
   only threads whose name or any stack frame contains the substring
   (case-insensitive)."
  [& {:keys [grep]}]
  (let [g (some-> grep str/lower-case)]
    (into (sorted-map)
          (keep (fn [[^Thread t frames]]
                  (let [nm (.getName t)
                        stack (mapv str frames)]
                    (when (or (nil? g)
                              (str/includes? (str/lower-case nm) g)
                              (some #(str/includes? (str/lower-case %) g) stack))
                      [nm {:state (str (.getState t)) :stack stack}]))))
          (Thread/getAllStackTraces))))

(defn pgwire-threads
  "The go-to hang diagnostic: each pgwire connection worker (or any thread
   with a recorded in-flight SQL) with its :state, its current :sql, and the
   top of its :stack."
  [& {:keys [depth] :or {depth 14}}]
  (let [dump (thread-dump)
        flight @in-flight]
    (into (sorted-map)
          (keep (fn [[nm {:keys [state stack]}]]
                  (when (or (str/includes? (str/lower-case nm) "pgwire")
                            (contains? flight nm))
                    [nm {:state state
                         :sql (get-in flight [nm :sql])
                         :top (vec (take depth stack))}]))
                dump))))

(defn blocked-threads
  "Threads currently BLOCKED/WAITING/TIMED_WAITING that touch datahike or
   pgwire frames — i.e. likely participants in a deadlock or hang."
  []
  (into (sorted-map)
        (filter (fn [[_ {:keys [state stack]}]]
                  (and (#{"BLOCKED" "WAITING" "TIMED_WAITING"} state)
                       (some #(or (str/includes? % "datahike")
                                  (str/includes? % "datahike.pg")) stack)))
                (thread-dump))))

(defn dump-to-file
  "Full jstack-style dump to `path` (optionally `:grep`-filtered) for offline
   inspection. Returns the path."
  [path & {:keys [grep]}]
  (spit path
        (str/join "\n\n"
                  (for [[nm {:keys [state stack]}] (thread-dump :grep grep)]
                    (str nm "  [" state "]\n  " (str/join "\n  " stack)))))
  path)

;; ===========================================================================
;; Run SQL straight through a handler (no external client)
;; ===========================================================================

(defn run-sql
  "Execute one SQL string against `conn` via a throwaway handler. Returns
   {:tag :cols :rows :error :sqlstate}. Handy for reproducing a single
   statement's behaviour without spinning up psql/node/asyncpg."
  [conn sql]
  (let [h (pg/make-query-handler conn {})
        r (.execute ^PgWireServer$QueryHandler h sql)]
    {:tag (.-commandTag r)
     :cols (when (.-columnNames r) (vec (.-columnNames r)))
     :rows (when (.-rows r) (mapv vec (.-rows r)))
     :error (.-error r)
     :sqlstate (.-sqlstate r)}))

;; ===========================================================================
;; Database registry — watch for accumulation / leaks; reset between runs
;; ===========================================================================

(defn registry-info
  "databases -> datom count. A growing set of leftover names (or growing
   counts on the default db) flags an accumulation/pollution problem."
  [registry-atom]
  (into (sorted-map)
        (map (fn [[n c]]
               [n (try (count (seq (d/datoms (d/db c) :eavt)))
                       (catch Throwable _ :err))]))
        @registry-atom))

(defn clear-test-dbs!
  "Release and drop every registry database except those in `keep` (default
   just \"datahike\"). Clears the accumulation that builds up when a client
   (e.g. asyncpg) creates a database per test class — without restarting the
   JVM. Returns the remaining database names."
  [registry-atom & {:keys [keep] :or {keep #{"datahike"}}}]
  (doseq [[n c] @registry-atom :when (not (contains? keep n))]
    (try (d/release c) (catch Throwable _))
    (swap! registry-atom dissoc n))
  (vec (keys @registry-atom)))
