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
(defonce advisory-locks
  ;; {lock-key {[session-id xact? mode] {:count long}}}
  ;;
  ;; A session can hold the SAME object at session level and at
  ;; transaction level at once -- PostgreSQL keeps those as separate
  ;; holds, and its own advisory_lock test takes both and then expects
  ;; the session one to survive the ROLLBACK -- so the holder key carries
  ;; the level and the mode, not just the session.
  ;;
  ;; Public because `pg_locks` reports the advisory locks this server
  ;; holds, and the catalog reads them from here.
  (atom {}))

(defn reset-advisory-locks!
  "Clear the advisory-lock registry. Test-fixture helper; not for handler
   code. In production, locks release on unlock, COMMIT/ROLLBACK (xact-
   level), DISCARD ALL, or connection close."
  []
  (reset! advisory-locks {}))

(defn advisory-lock-try!
  "Acquire a single advisory lock, non-blocking. Returns true on success,
   false while another session holds it in a conflicting mode.

   `mode` is `:exclusive` or `:shared`, PostgreSQL's
   `pg_advisory_lock` / `pg_advisory_lock_shared` pair: two SHARED
   holders coexist, an exclusive holder excludes every other session.
   A same-session, same-level, same-mode re-lock increments the refcount
   at session level and is a no-op at transaction level, as in
   PostgreSQL."
  ([lock-key session-id xact?] (advisory-lock-try! lock-key session-id xact? :exclusive))
  ([lock-key session-id xact? mode]
   (let [holder-key [session-id (boolean xact?) mode]
         result (atom :unknown)]
     (swap! advisory-locks
            (fn [m]
              (let [holders (get m lock-key)
                    others (into {} (remove (fn [[[sid _ _] _]] (= sid session-id))) holders)
                    conflict? (and (seq others)
                                   (or (= mode :exclusive)
                                       (some (fn [[[_ _ md] _]] (= md :exclusive)) others)))]
                (cond
                  conflict? (do (reset! result :blocked) m)

                  (contains? holders holder-key)
                  (do (reset! result :acquired)
                      (if xact? m (update-in m [lock-key holder-key :count] inc)))

                  :else
                  (do (reset! result :acquired)
                      (assoc-in m [lock-key holder-key] {:count 1}))))))
     (= @result :acquired))))

(defn advisory-lock!
  "Blocking acquire. Spin-wait with exponential backoff up to 100ms."
  ([lock-key session-id xact?] (advisory-lock! lock-key session-id xact? :exclusive))
  ([lock-key session-id xact? mode]
   (loop [attempt 0]
     (if (advisory-lock-try! lock-key session-id xact? mode)
       true
       (do (Thread/sleep ^long (min 100 (bit-shift-left 1 (min 7 attempt))))
           (recur (inc attempt)))))))

(defn advisory-unlock!
  "Release one SESSION-level hold of `lock-key` in `mode`: decrement the
   refcount, dropping the hold at zero. Returns true when we held one.
   A transaction-level lock cannot be unlocked by hand in PostgreSQL --
   it releases at transaction end -- and answers false here too (with a
   WARNING there, which we do not emit)."
  ([lock-key session-id] (advisory-unlock! lock-key session-id :exclusive))
  ([lock-key session-id mode]
   (let [holder-key [session-id false mode]
         [before after]
         (swap-vals! advisory-locks
                     (fn [m]
                       (let [e (get-in m [lock-key holder-key])]
                         (cond
                           (nil? e) m
                           (<= (:count e) 1)
                           (let [holders (dissoc (get m lock-key) holder-key)]
                             (if (seq holders) (assoc m lock-key holders) (dissoc m lock-key)))
                           :else (update-in m [lock-key holder-key :count] dec)))))]
     (not= before after))))

(defn release-advisory-locks!
  "Release advisory locks owned by `session-id`, within `scope`:

     :xact     only the transaction-level ones (COMMIT / ROLLBACK)
     :session  only the session-level ones -- what
               `pg_advisory_unlock_all()` does, which in PostgreSQL
               leaves a transaction-level lock held until the
               transaction ends
     :all      every one of them (connection close, DISCARD ALL)

   The legacy boolean arity means `:xact` when true, `:all` when false."
  ([session-id] (release-advisory-locks! session-id :all))
  ([session-id scope]
   (let [scope (case scope true :xact false :all scope)
         match? (fn [[sid xact? _mode]]
                  (and (= sid session-id)
                       (case scope :xact xact? :session (not xact?) true)))
         any? (fn [m] (some (fn [[_k holders]] (some match? (keys holders))) m))]
     (when (any? @advisory-locks)
       (swap! advisory-locks
              (fn [m]
                (persistent!
                 (reduce (fn [out [k holders]]
                           (let [kept (into {} (remove (fn [[hk _]] (match? hk))) holders)]
                             (if (seq kept) (assoc! out k kept) out)))
                         (transient {}) m))))))))

(defn held-advisory-locks
  "The advisory locks currently held, as `[[lock-key mode] …]`, one entry
   per object and mode -- which is how `pg_locks` reports them: a session
   holding the same object at both levels is ONE row there."
  []
  (sort-by (fn [[k mode]] [(if (vector? k) (first k) 0)
                           (if (vector? k) (second k) k)
                           (str mode)])
           (distinct (for [[lock-key holders] @advisory-locks
                           [[_sid _xact? mode] _] holders]
                       [lock-key mode]))))

