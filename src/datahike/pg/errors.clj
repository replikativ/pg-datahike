(ns datahike.pg.errors
  "Exception → PostgreSQL SQLSTATE classification for the pgwire layer.

   Postgres clients branch on SQLSTATE codes: Odoo retries 40001
   (serialization_failure) and 40P01 (deadlock_detected); ORMs map 23505
   to 'unique violation', 23502 to 'not-null violation', 22P02 to 'invalid
   input syntax', etc. Returning XX000 or 42000 for everything makes these
   signals invisible.

   Lookup order when classifying a Throwable:
     1. ex-data `:sqlstate` (explicit override wins).
     2. ex-data `:datahike/canceled` → \"57014\" (query_canceled). Datahike
        core stays postgres-agnostic — it raises a plain
        `{:datahike/canceled true}` ex-info; this layer translates to the
        wire code at the boundary.
     3. ex-data `:error` keyword, via `dh-error->sqlstate`.
     4. Regex pattern match on the exception message (for Datahike's
        common unstructured errors that don't set `:error`).
     5. Fallback: \"XX000\" (internal_error).

   Canonical code list: postgres/src/backend/utils/errcodes.txt.")

(def dh-error->sqlstate
  "Map Datahike :error keyword (from ex-data) to PostgreSQL SQLSTATE.

   The left column lists every `:error` key grep'd from Datahike's
   source (as of 0.6.1611). When a new Datahike release adds another
   one, add it here too — an unmapped key falls through to XX000
   internal_error, which is what ORMs treat as 'generic failure'."
  {;; Schema validation: value type mismatch, bad entity value
   :transact/schema            "22P02"   ;; invalid_text_representation
   :retract/schema             "22P02"
   :schema/validation          "22P02"
   :transact/upsert            "23505"   ;; unique_violation (upsert conflict)
   :transact/unique            "23505"   ;; unique_violation (hard conflict)
   :lookup-ref/unique          "23505"   ;; lookup ref hit non-unique attribute
   :transact/syntax            "42601"   ;; syntax_error
   :entity-id/syntax           "22P02"
   :lookup-ref/syntax          "22P02"

   ;; Entity resolution
   :entity-id/missing          "42704"   ;; undefined_object
   :db/invalid-attribute       "42703"   ;; undefined_column

   ;; Query-time errors
   :query/invalid-clause       "42601"
   :query/where                "42601"
   :query/where-conflict       "42703"
   :query/binding              "42601"

   ;; CAS / serialization — map to the retry code (40001) so clients
   ;; retry the whole tx rather than surface as internal error.
   :transact/cas               "40001"   ;; serialization_failure

   ;; Check-like / value constraints
   :transact/ensure            "23514"   ;; check_violation
   :transact/purge             "22023"   ;; invalid_parameter_value
   :search/pattern             "22023"

   ;; Feature not supported (sync / filtered db / merge)
   :merge/sync-not-supported   "0A000"   ;; feature_not_supported
   :transact/sync-not-supported "0A000"
   :transaction/filtered       "0A000"

   ;; Legacy cardinality / uniqueness keys
   :db.unique/identity         "23505"
   :db.unique/value            "23505"

   ;; Data import shape mismatch
   :import/mismatch            "22P02"

   ;; Connection / storage
   :db.error/connection        "08000"   ;; connection_exception
   :db.error/storage           "58000"   ;; system_error
   :db.error/serialization     "40001"}) ;; serialization_failure (ours)

(defn classify-message
  "Classify a bare error message string to a SQLSTATE code via regex
   pattern matching. Returns the code or nil when nothing matches.

   LAST-RESORT fallback. The right way to set a SQLSTATE is on the
   ex-info at throw site:
     (throw (ex-info \"…\" {:sqlstate \"23505\" :table …}))

   This fn exists for Datahike-internal exceptions we don't own
   (schema validation, value-type mismatch, CAS conflicts) — when
   Datahike throws without a :error key or with one we haven't mapped,
   the regex gets close enough that the client sees a useful SQLSTATE
   instead of XX000. Do not add new reliance on it from pgwire code:
   set :sqlstate at the throw site."
  [^String msg]
  (when msg
    (cond
      ;; Datahike schema validation + type cast failures
      (re-find #"(?i)Bad entity value .* does not match schema" msg) "22P02"
      (re-find #"(?i)cannot be cast to class" msg) "22P02"
      ;; Uniqueness and not-null
      (re-find #"(?i)unique constraint|unique value.*already" msg) "23505"
      (re-find #"(?i)not-null|NOT NULL constraint" msg) "23502"
      (re-find #"(?i)foreign key|ref.*not.*exist" msg) "23503"
      ;; Parser / query errors. Parser errors are 42601 (syntax_error);
      ;; a missing table or column ref that makes it past the parser but
      ;; fails at resolve time is 42P01 / 42703.
      (re-find #"(?i)SQL parse error|ParseException|Unsupported SQL" msg) "42601"
      (re-find #"(?i)column .* does not exist|unknown.*attribute" msg) "42703"
      (re-find #"(?i)relation .* does not exist|unknown.*table|table .* does not exist" msg) "42P01"
      ;; Serialization / optimistic-concurrency conflicts (for 40001 retry)
      (re-find #"(?i)transaction.*conflict|stale.*db|CAS.*failed" msg) "40001"
      :else nil)))

(defn- extract-error-fields
  "Pull ErrorResponse-style detail fields out of Datahike ex-data.
   Returns a Java Map<String,String> keyed by PG protocol field codes
   (\"n\" constraint, \"t\" table, \"c\" column, \"d\" data type,
   \"D\" detail, \"H\" hint), or nil when nothing maps.

   ORMs depend on these: psycopg2's `Diagnostics.constraint_name`,
   pgJDBC's `PSQLException.getServerErrorMessage().getConstraint()`.
   Without them, a unique-violation surfaces as an opaque message
   instead of 'email must be unique'."
  [data msg]
  (let [fields (java.util.HashMap.)
        attr (:attribute data)
        table (when (keyword? attr) (namespace attr))
        col   (when (keyword? attr) (name attr))
        ;; :value is set for :transact/schema & :transact/upsert, but
        ;; :transact/unique carries the offending fact as :datom — read
        ;; position 2 (value) from it.
        value (or (:value data)
                  (when-let [d (:datom data)]
                    (try
                      (if (vector? d) (nth d 2 nil) (.v d))
                      (catch Exception _ nil))))
        schema (:schema data)
        error (:error data)]
    (cond
      ;; Schema-mismatch ex-data from datahike.db.transaction/validate_val
      (or (= :transact/schema error)
          (= :retract/schema error)
          (= :schema/validation error))
      (do (when table  (.put fields "t" table))
          (when col    (.put fields "c" col))
          (when value  (.put fields "D" (str "value: " (pr-str value))))
          ;; Datahike ex-data uses :db/valueType (namespaced-map form
          ;; #:db{:valueType ...}), but some callers pass :valueType.
          (when-let [vt (or (:db/valueType schema) (:valueType schema))]
            (.put fields "d" (clojure.core/name vt))))

      ;; Unique-violation — the attribute becomes the constraint name
      ;; (mirroring PG's UNIQUE index naming: `<table>_<col>_key`).
      (or (= :db.unique/identity error)
          (= :db.unique/value error)
          (= :transact/upsert error)
          (= :transact/unique error)
          (= :lookup-ref/unique error))
      (do (when (and table col)
            (.put fields "n" (str table "_" col "_key"))
            (.put fields "t" table)
            (.put fields "c" col))
          (when value
            (.put fields "D" (str "duplicate value: " (pr-str value)))))

      ;; Lookup-ref / entity resolution failures — surface what was
      ;; being looked up when available.
      (or (= :entity-id/missing error)
          (= :entity-id/syntax error)
          (= :lookup-ref/syntax error))
      (do (when table (.put fields "t" table))
          (when col   (.put fields "c" col))
          (when-let [lr (:lookup-ref data)]
            (.put fields "D" (str "lookup: " (pr-str lr)))))

      ;; Query-time errors: show the clause that failed if we have it.
      (or (= :query/where error)
          (= :query/where-conflict error)
          (= :query/binding error)
          (= :query/invalid-clause error))
      (do (when table (.put fields "t" table))
          (when col   (.put fields "c" col))
          (when-let [clause (:clause data)]
            (.put fields "D" (str "clause: " (pr-str clause))))
          (when-let [v (:value data)]
            (.put fields "D" (str "value: " (pr-str v)))))

      ;; CAS failure — attr + expected/actual.
      (= :transact/cas error)
      (do (when table (.put fields "t" table))
          (when col   (.put fields "c" col))
          (when-let [ev (:expected data)]
            (.put fields "D" (str "expected: " (pr-str ev)
                                  ", got: " (pr-str (:actual data))))))

      ;; Check-like / ensure violations.
      (or (= :transact/ensure error)
          (= :transact/purge error)
          (= :search/pattern error))
      (do (when table (.put fields "t" table))
          (when col   (.put fields "c" col))
          (when value (.put fields "D" (str "value: " (pr-str value))))
          (when-let [h (:hint data)]
            (.put fields "H" h)))

      ;; Feature-not-supported — the hint is most useful here.
      (or (= :merge/sync-not-supported error)
          (= :transact/sync-not-supported error)
          (= :transaction/filtered error))
      (do (.put fields "H" "operation not supported on this database")
          (when-let [h (:hint data)] (.put fields "H" h)))

      ;; Best-effort: pull a table/column from a "Bad entity value … at
      ;; [:db/add _ :ns/col val]" message when no ex-data present.
      (and msg (re-find #"\[:db/add\s+[^\s]+\s+:([^/\s]+)/([^\s]+)\s" msg))
      (let [[_ tbl c] (re-find #"\[:db/add\s+[^\s]+\s+:([^/\s]+)/([^\s]+)\s" msg)]
        (.put fields "t" tbl)
        (.put fields "c" c))

      :else nil)
    (when (pos? (.size fields)) fields)))

(defn- find-ex-data
  "Walk the cause chain and return the first non-empty ex-data map.
   Datahike's async writer wraps the real cause in an ExecutionException
   plus a re-raised ExceptionInfo whose ex-data is `{}`, so looking only
   at the outer exception loses the `:error` key. We walk up to 8 levels."
  [^Throwable e]
  (loop [x e depth 0]
    (cond
      (nil? x) nil
      (> depth 8) nil
      :else
      (let [d (when (instance? clojure.lang.IExceptionInfo x) (ex-data x))]
        (if (and d (seq d)) d (recur (.getCause x) (inc depth)))))))

(defn classify-exception
  "Classify a Throwable to `[sqlstate message fields]`. Falls back to
   XX000 with nil fields.

   The regex pattern branch is deliberately last so that structured
   `:error` data in ex-data always wins — a Datahike commit that throws
   with both a recognizable message and explicit `:error :db.unique/*`
   must classify as 23505 regardless of the message wording.

   `fields` is a Java Map<String,String> of optional PG ErrorResponse
   fields (n/t/c/d/D/H), or nil. The wire layer passes these to
   sendError so ORMs get structured diagnostics."
  [^Throwable e]
  (let [msg   (or (.getMessage e) (.getSimpleName (class e)))
        data  (find-ex-data e)
        from-data (or (:sqlstate data)
                      (when (:datahike/canceled data) "57014")
                      (get dh-error->sqlstate (:error data)))
        from-msg (when (nil? from-data) (classify-message msg))
        code (or from-data from-msg "XX000")
        fields (extract-error-fields data msg)]
    [code msg fields]))
