(ns datahike.pg.locks
  "Advisory locks, the registry and its operations.

   They live here rather than in the server because a TRANSLATED
   `pg_advisory_lock(…)` -- one written inside an expression rather than
   as a whole statement -- has to reach them from the SQL layer, which
   the server namespace requires rather than the other way round.")

;; Advisory locks (pg_advisory_lock / pg_try_advisory_lock / …).
;;
;; PG semantics we implement:
;;   - Session-level: held until pg_advisory_unlock / pg_advisory_unlock_all /
;;     connection close. Re-entrant for the same session — a second
;;     pg_advisory_lock by the same owner increments a refcount and requires
;;     an equal number of unlocks to release.
;;   - Transaction-level (pg_advisory_xact_lock): auto-released at COMMIT or
;;     ROLLBACK. Not reference-counted in PG (a second call in the same tx
;;     is a no-op); we match that.
;;   - Try variants are non-blocking: return false instead of waiting.
;;   - Blocking variants spin-wait (exponential backoff up to 100ms). Our
;;     entire pgwire server is a single JVM, so conflicts resolve in-process.
;;
;; Crucial: migration tools (Flyway, Alembic, Ecto.Migrator, Rails) take a
;; well-known key at startup and only proceed if pg_try_advisory_lock
;; returns true. Returning anything other than a real boolean (or a dummy
;; "looks truthy" string — our prior behavior) silently breaks
;; mutual-exclusion across concurrent migrate runs.
(defonce ^:private advisory-locks
  ;; {lock-key {:session-id str :count long :xact? bool}}
  (atom {}))

(defn reset-advisory-locks!
  "Clear the advisory-lock registry. Test-fixture helper; not for handler
   code. In production, locks release on unlock, COMMIT/ROLLBACK (xact-
   level), DISCARD ALL, or connection close."
  []
  (reset! advisory-locks {}))

(defn advisory-lock-try!
  "Acquire a single advisory lock, non-blocking. Returns true on success,
   false if another session holds it. Same-session re-lock increments the
   refcount (session-level only); xact-level re-lock is a no-op."
  [lock-key session-id xact?]
  (let [result (atom :unknown)]
    (swap! advisory-locks
           (fn [m]
             (let [existing (get m lock-key)]
               (cond
                 (nil? existing)
                 (do (reset! result :acquired)
                     (assoc m lock-key {:session-id session-id
                                        :count 1
                                        :xact? xact?}))
                 (not= (:session-id existing) session-id)
                 (do (reset! result :blocked) m)
                 xact?
                 (do (reset! result :acquired) m)
                 :else
                 (do (reset! result :acquired)
                     (update-in m [lock-key :count] inc))))))
    (= @result :acquired)))

(defn advisory-lock!
  "Blocking acquire. Spin-wait with exponential backoff up to 100ms."
  [lock-key session-id xact?]
  (loop [attempt 0]
    (if (advisory-lock-try! lock-key session-id xact?)
      true
      (do (Thread/sleep ^long (min 100 (bit-shift-left 1 (min 7 attempt))))
          (recur (inc attempt))))))

(defn advisory-unlock!
  "Decrement refcount on `lock-key` (session-level); remove when it
   reaches zero. Returns true if WE held it and released one, false
   otherwise. xact-level locks cannot be unlocked by hand in PG — they
   release at tx end — but we accept it as a no-op-false for symmetry."
  [lock-key session-id]
  (let [[before after]
        (swap-vals! advisory-locks
                    (fn [m]
                      (let [e (get m lock-key)]
                        (cond
                          (or (nil? e)
                              (not= (:session-id e) session-id)
                              (:xact? e))
                          m
                          (<= (:count e) 1)
                          (dissoc m lock-key)
                          :else
                          (update-in m [lock-key :count] dec)))))]
    (not= before after)))

(defn release-advisory-locks!
  "Release advisory locks owned by `session-id`. If xact-only? is true,
   only release the tx-level ones (called from COMMIT/ROLLBACK); otherwise
   release all (called from session close and DISCARD ALL)."
  ([session-id] (release-advisory-locks! session-id false))
  ([session-id xact-only?]
   ;; Hot path (end-tx! on every implicit-tx commit): skip the global
   ;; rebuild + write when this session holds no matching advisory lock.
   (let [match? (fn [v] (and (= (:session-id v) session-id)
                             (or (not xact-only?) (:xact? v))))]
     (when (some match? (vals @advisory-locks))
       (swap! advisory-locks
              (fn [m] (into {} (remove (fn [[_k v]] (match? v))) m)))))))
