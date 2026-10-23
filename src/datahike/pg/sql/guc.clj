(ns datahike.pg.sql.guc
  "The ONE place a session GUC is written.

   A setting can be changed three ways, and PostgreSQL treats them as
   the same act: `SET timezone = 'Europe/Berlin'`, `SELECT
   set_config('timezone', 'Europe/Berlin', false)` as a statement of
   its own, and the same call inside a larger query. All three arrived
   here by separate routes, and two of them honoured `search_path` and
   silently dropped every other setting -- so `SET timezone` moved
   timestamptz output and `set_config('timezone', …)` did not, from one
   session, with `SHOW timezone` agreeing with whichever had run last.

   `current_setting` had the mirror-image bug on the READ side and its
   fix left a comment claiming `set_config` already wrote the session
   state. It did not. One writer is the only version of this that stays
   true, so the parse-and-store for every GUC that changes how we
   execute lives in `set-setting!` and nowhere else.

   A setting we do not model is NOT an error: asyncpg toggles `jit`
   around its type introspection and reads the result back, so an
   unrecognised name is accepted and echoed by the caller. This
   function reports whether it recognised the name; it never throws for
   an unknown one, only for a known one given a bad value."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]
            [datahike.pg.types :as types]))

(defn normalize-search-path
  "Split, trim and unquote a SET search_path value list."
  [values]
  (->> values
       (mapcat #(str/split (str %) #","))
       (map str/trim)
       (map #(if (and (>= (count %) 2)
                      (str/starts-with? % "\"")
                      (str/ends-with? % "\""))
               (subs % 1 (dec (count %)))
               %))
       (remove str/blank?)
       vec))

(defn- set-search-path! [session-state values]
  (if (and (= 1 (count values))
           (= "default" (str/lower-case (str (first values)))))
    (swap! session-state dissoc :search-path)
    (swap! session-state assoc :search-path (normalize-search-path values))))

(def setting-keys
  "Every GUC we model, by its folded name, to the session-state key it
   writes. One table so that writing a setting and resetting it cannot
   name different things -- `SET` and `RESET <var>` each carried their
   own copy of these names, which is the shape the `set_config` bug
   grew out of.

   `RESET ALL` is NOT driven from here: it clears a wider set
   (`server/session-guc-keys`), including the datahike.* temporal vars
   and statement_timeout, which are written on their own paths."
  {"search_path"            :search-path
   "timezone"               :time-zone
   "time zone"              :time-zone
   "time_zone"              :time-zone
   "datestyle"              :date-style
   "date_style"             :date-style
   "hnsw.ef_search"         :hnsw-ef-search
   "pg_datahike.float_sum"  :float-sum})

(def ^:private read-only-settings
  "GUCs PostgreSQL reports but never lets a session change."
  #{"server_version" "server_version_num" "server_encoding" "lc_collate"
    "lc_ctype" "is_superuser" "block_size" "max_identifier_length"
    "integer_datetimes" "segment_size" "wal_block_size"})

(defn reset-setting!
  "Clear one GUC, returning it to its compiled-in default. Returns true
   when the name is one we model.

   This is what `RESET <var>` does, and also what `set_config(name,
   NULL, …)` does: `guc.c`'s `set_config_by_name` passes a NULL value
   straight to `set_config_option`, where it means \"reset to default\"."
  [session-state name]
  (let [folded (some-> name str str/lower-case)
        ;; Back to the value the session STARTED with, which is not the
        ;; compiled-in default when the StartupMessage set it: under
        ;; PGTZ=Asia/Tokyo, `RESET timezone` answers Asia/Tokyo.
        put-back (fn [st path]
                   (let [start (get-in st (into [:guc-startup] path) ::absent)]
                     (cond
                       (not= ::absent start) (assoc-in st path start)
                       (= 1 (count path)) (dissoc st (first path))
                       :else (update-in st (pop path) dissoc (peek path)))))]
    (if-let [k (get setting-keys folded)]
      (do (when session-state
            (swap! session-state
                   (fn [st] (cond-> (put-back st [k])
                              (= :time-zone k) (put-back [:time-zone-text])))))
          true)
      (do (when session-state (swap! session-state put-back [:gucs folded]))
          false))))

(defn set-setting!
  "Apply one session GUC. `name` is the setting as written (folded
   here), `values` the raw value strings -- one for `set_config`, one or
   more for a `SET` with a value list.

   Returns true when the name is one we model and the value was stored,
   false otherwise. Throws 22023 for a recognised setting given a value
   it cannot take.

   A single NULL value RESETS the setting, which is what PostgreSQL's
   `set_config(name, NULL, …)` does.

   `set_config`'s third argument asks for the change to last only to
   the end of the transaction. We do not model transaction-local
   settings -- `SET LOCAL` is not distinguished either -- so the change
   is applied to the session and outlives the transaction. Stated once
   here rather than silently in two places."
  [session-state name values]
  (let [setting (some-> name str str/lower-case)
        values  (vec values)
        raw     (str/join "," values)]
    (cond
      (nil? session-state) false

      (and (= 1 (count values)) (nil? (first values)))
      (reset-setting! session-state setting)

      (= "search_path" setting)
      (do (set-search-path! session-state values) true)

      ;; `types/time-zone-setting` owns the conventions -- three of them,
      ;; disagreeing about the sign -- and what SHOW answers for each.
      ;; DEFAULT / LOCAL clear the setting.
      (contains? #{"timezone" "time zone" "time_zone"} setting)
      (do (if-let [{:keys [zone text]} (types/time-zone-setting raw)]
            (swap! session-state assoc :time-zone zone :time-zone-text text)
            (swap! session-state dissoc :time-zone :time-zone-text))
          true)

      (contains? #{"datestyle" "date_style"} setting)
      (do (swap! session-state assoc :date-style
                 (types/parse-date-style
                  raw (or (:date-style @session-state) [:iso :mdy])))
          true)

      ;; Compensated float summation, OFF by default. See
      ;; `fns/*compensated-float-sum?*` for why the default cannot
      ;; change: PostgreSQL sums float8 naively and our own
      ;; differential gate compares against it.
      (= "pg_datahike.float_sum" setting)
      (let [v (some-> (first values) str str/lower-case
                      (str/replace #"^'|'$" ""))]
        (case v
          "naive"       (swap! session-state dissoc :float-sum)
          "compensated" (swap! session-state assoc :float-sum :compensated)
          (throw (errors/pg-error
                  :invalid-parameter-value
                  {:message (str "pg_datahike.float_sum must be "
                                 "'naive' or 'compensated'")})))
        true)

      (= "hnsw.ef_search" setting)
      (let [value (try (parse-long raw) (catch Exception _ nil))]
        (when-not (and value (<= 1 value 1000))
          (throw (errors/pg-error
                  :invalid-parameter-value
                  {:message "hnsw.ef_search must be between 1 and 1000"})))
        (swap! session-state assoc :hnsw-ef-search value)
        true)

      ;; Read-only in PostgreSQL too: 55P02, not a silent no-op.
      (contains? read-only-settings setting)
      (throw (ex-info (str "parameter \"" setting "\" cannot be changed")
                      {:sqlstate "55P02"}))

      ;; One we do not model is still a setting: PostgreSQL stores it
      ;; and answers it back. `application_name` was answered with the
      ;; role name whatever had been set, and a custom dotted one --
      ;; `set_config('request.jwt.sub', …)`, how PostgREST hands a
      ;; request's identity to row-level security -- was not stored at
      ;; all, so the `current_setting` after it raised 42704.
      :else
      (do (swap! session-state assoc-in [:gucs setting] raw)
          false))))

(defn apply-startup-settings!
  "Apply a StartupMessage's settings, `[[name value] …]`, and remember
   the result as what RESET goes back to. One PostgreSQL would refuse is
   skipped rather than failing the connection."
  [session-state settings]
  (doseq [[k v] settings]
    (try (set-setting! session-state k [v])
         (catch Exception _ nil)))
  (swap! session-state
         (fn [st] (assoc st :guc-startup
                         (select-keys st (conj (vec (distinct (vals setting-keys)))
                                               :time-zone-text :gucs))))))

(def ^:private default-settings
  "The GUCs we report a value for, whether or not we model them. These
   are overwhelmingly connect-time probes from JDBC / psycopg2 /
   asyncpg / Metabase; a name that is neither here nor session-tracked
   is unknown, and the caller raises 42704.

   Moved here from `current_setting` because `set_config` has to read
   the SAME values: PostgreSQL's `set_config_by_name` ends with
   `GetConfigOptionByName`, so what it answers is the setting as it now
   stands, not the string the caller passed."
  {"server_version"              datahike.pg.PgWireServer/SERVER_VERSION
   "server_version_num"          datahike.pg.PgWireServer/SERVER_VERSION_NUM
   "client_encoding"             "UTF8"
   "server_encoding"             "UTF8"
   "TimeZone"                    "UTC"
   "DateStyle"                   "ISO, MDY"
   "IntervalStyle"               "postgres"
   "search_path"                 "\"$user\", public"
   "standard_conforming_strings" "on"
   "lc_messages"                 "C"
   "lc_collate"                  "C"
   "lc_ctype"                    "C"
   ;; `pg_roles.rolsuper` and `pg_authid.rolsuper` report TRUE for this
   ;; role, with the stated reason that the server enforces no
   ;; privileges -- one login role, everything permitted. This said
   ;; "off", contradicting them from the same server. `on` is what the
   ;; rest of it honours.
   "is_superuser"                "on"
   "session_authorization"       types/pg-role-name
   "application_name"            types/pg-role-name
   "transaction_isolation"       "read committed"
   "transaction_read_only"       "off"
   "default_transaction_isolation" "read committed"
   ;; asyncpg probes `jit` during type introspection -- `SELECT
   ;; current_setting('jit'), set_config('jit', 'off', false)` -- and a
   ;; 42704 there breaks its codec pipeline, not just the probe.
   ;; DELIBERATE DIVERGENCE: PostgreSQL defaults this to `on`; we have
   ;; no JIT at all, so `off` is the truthful answer. A client that
   ;; reads this to decide whether to apply a JIT workaround gets the
   ;; right answer from us, not a copied default.
   "jit"                         "off"
   ;; PostgreSQL's default. Above 0 it means "print shortest
   ;; round-trip", which is what our float renderer does -- so
   ;; reporting the real default is truthful, and SHOW returning empty
   ;; was not.
   "extra_float_digits"          "1"})

(def ^:private folded-defaults
  ;; GUC names are CASE-INSENSITIVE in PostgreSQL:
  ;; `current_setting('SEARCH_PATH')` answers, and a plain `get` on a
  ;; mixed-case map (TimeZone, DateStyle) raised 42704.
  (into {} (map (fn [[k v]] [(str/lower-case k) v])) default-settings))

(defn- session-value
  "What the SESSION has set this GUC to, or nil. Spelled to match
   `handle-show`'s own formatting for each setting, because the whole
   point is that the two agree -- `SET search_path = 'myschema'` once
   made `SHOW search_path` answer myschema while
   `current_setting('search_path')` still answered the compiled-in
   default."
  [session-state folded]
  (when session-state
    (let [st @session-state]
      (case folded
        "search_path" (when-let [p (:search-path st)]
                        (str/join ", " p))
        ("datestyle" "date_style")
        (when-let [d (:date-style st)] (types/date-style-text d))
        ("timezone" "time zone" "time_zone")
        (or (:time-zone-text st)
            (when-let [z (:time-zone st)] (types/time-zone-text z)))
        "hnsw.ef_search" (some-> (:hnsw-ef-search st) str)
        (get-in st [:gucs folded])))))

(defn read-setting
  "One GUC as text, exactly as `SHOW` reports it, or nil when we neither
   model nor default it. The session's own value wins over the default.

   This is the READ half of the pair: `set-setting!` writes, this reads,
   and `set_config` needs both because PostgreSQL answers it with the
   value the setting now has."
  [session-state name]
  (let [folded (str/lower-case (str name))]
    (or (session-value session-state folded)
        (get folded-defaults folded))))
