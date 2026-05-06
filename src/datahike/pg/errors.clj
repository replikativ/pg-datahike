(ns datahike.pg.errors
  "Exception → PostgreSQL ErrorResponse classification for the pgwire
   layer. Owns three things:

     1. Mapping a Throwable → SQLSTATE code (the wire ABI clients
        branch on).
     2. Producing a PG-shaped user-facing message (don't leak
        Datahike vocabulary like `:foo/bar`, `{:db/id 47, …}`).
     3. Populating the ErrorResponse fields (n / t / c / d / D / H)
        that ORMs read via `getServerErrorMessage()` /
        `Diagnostics.constraint_name`.

   Throw sites within `datahike.pg.*` describe errors structurally:

     (throw (ex-info \"<short internal description>\"
                     {:error :undefined-column
                      :table \"employee\"
                      :column \"dept_id\"}))

   The wire boundary (`classify-exception`) then derives:
     - SQLSTATE  via `error-categories`
     - message   via the category's `:format` fn (falls back to the
                 throw site's own message when nil)
     - fields    via `extract-error-fields`

   ## Why centralised formatting

   PG's own backend works the same way: throw sites call
   `ereport(ERROR, errcode(ERRCODE_UNDEFINED_COLUMN), errmsg(...))`
   with structured args; the wire layer (`pqcomm.c`) emits the protocol
   message from the resulting `ErrorData` struct. There is no
   `report_undefined_column()` helper at every throw site. That's the
   pattern this namespace implements.

   ## Lookup order in classify-exception

     1. Explicit `:sqlstate` in ex-data (override; bypasses formatter).
     2. `:error` key in `error-categories` →
          - SQLSTATE from the registry
          - message from the entry's `:format` fn (or fallback)
     3. `:datahike/canceled` in ex-data → \"57014\" (cancelled).
     4. Datahike-emitted message regex (`classify-message`) — a
        last-resort safety net for unstructured errors thrown by
        Datahike core that we don't own. New pgwire throws should NOT
        rely on this.
     5. Fallback: \"XX000\" (internal_error).

   ## Categories

   Pgwire-side throw categories live in `error-categories`. Each entry
   has a `:sqlstate` and an optional `:format` fn. Datahike-internal
   `:error` keys map directly to SQLSTATEs (no formatter — Datahike's
   own message text comes through, with the SQLAlchemy-class
   missing-attribute case rewritten to PG vocabulary).

   Canonical PG code list: postgres/src/backend/utils/errcodes.txt."
  (:require [clojure.string :as str]))

;; ============================================================================
;; Error category registry
;; ============================================================================
;;
;; Each entry: {:sqlstate \"NNNNN\" :format (fn [data] msg-or-nil)}
;; Setting :format to nil means \"use the throw site's own message\".

(def error-categories
  "Pgwire-side error categories. Throw sites set `:error` in ex-data to
   one of these keys.

   The `:format` fn receives the full ex-data map and returns the
   PG-shaped user-facing message, or nil to fall through to the
   throw site's own message string."
  {;; --- catalog / object resolution -----------------------------------
   :undefined-column
   {:sqlstate "42703"
    :format (fn [{:keys [column table]}]
              (when column
                (if table
                  (str "column \"" column "\" of relation \"" table "\" does not exist")
                  (str "column \"" column "\" does not exist"))))}

   :undefined-table
   {:sqlstate "42P01"
    :format (fn [{:keys [table]}]
              (when table
                (str "relation \"" table "\" does not exist")))}

   :undefined-database
   {:sqlstate "3D000"
    :format (fn [{:keys [database]}]
              (when database
                (str "database \"" database "\" does not exist")))}

   :undefined-sequence
   {:sqlstate "42P01"
    :format (fn [{:keys [sequence]}]
              (when sequence
                (str "relation \"" sequence "\" does not exist")))}

   :undefined-object
   {:sqlstate "42704"
    :format (fn [{:keys [name kind]}]
              (when name
                (str "unrecognized " (or kind "object") " \"" name "\"")))}

   ;; --- constraint violations -----------------------------------------
   :unique-violation
   {:sqlstate "23505"
    :format (fn [{:keys [table column constraint value]}]
              (let [con (or constraint
                            (when (and table column) (str table "_" column "_key"))
                            (when table (str table "_pkey")))]
                (when con
                  (cond-> (str "duplicate key value violates unique constraint \"" con "\"")
                    (some? value) (str " (value: " (pr-str value) ")")))))}

   :not-null-violation
   {:sqlstate "23502"
    :format (fn [{:keys [column table]}]
              (when (and column table)
                (str "null value in column \"" column "\" of relation \""
                     table "\" violates not-null constraint")))}

   :foreign-key-violation
   {:sqlstate "23503"
    :format (fn [{:keys [table constraint detail]}]
              (when (and table constraint)
                (cond-> (str "insert or update on table \"" table
                             "\" violates foreign key constraint \"" constraint "\"")
                  detail (str ": " detail))))}

   :check-violation
   {:sqlstate "23514"
    :format (fn [{:keys [table constraint]}]
              (when (and table constraint)
                (str "new row for relation \"" table
                     "\" violates check constraint \"" constraint "\"")))}

   ;; --- input / data ---------------------------------------------------
   :invalid-text-representation
   {:sqlstate "22P02"
    :format (fn [{:keys [type value detail]}]
              (cond
                (and type value)
                (str "invalid input syntax for type " type ": " (pr-str value))
                detail detail
                :else nil))}

   :numeric-value-out-of-range
   {:sqlstate "22003"
    :format (fn [{:keys [type value]}]
              (when (and type value)
                (str "value " (pr-str value) " out of range for type " type)))}

   :division-by-zero
   {:sqlstate "22012"
    :format (fn [_] "division by zero")}

   :array-element-error
   {:sqlstate "2202E"
    :format (fn [{:keys [detail]}] (or detail "malformed array literal"))}

   ;; --- syntax / structural -------------------------------------------
   :syntax-error
   {:sqlstate "42601"
    :format (fn [{:keys [detail]}] detail)}

   :feature-not-supported
   {:sqlstate "0A000"
    :format (fn [{:keys [feature detail]}]
              (or detail (when feature (str feature " is not supported"))))}

   :grouping-error
   {:sqlstate "42803"
    :format (fn [{:keys [detail]}] detail)}

   :wrong-object-type
   {:sqlstate "42809"
    :format (fn [{:keys [detail]}] detail)}

   ;; --- transaction / concurrency -------------------------------------
   :serialization-failure
   {:sqlstate "40001" :format nil}

   :in-failed-sql-transaction
   {:sqlstate "25P02"
    :format (fn [_]
              "current transaction is aborted, commands ignored until end of transaction block")}

   :no-active-transaction
   {:sqlstate "25P01"
    :format (fn [_] "there is no transaction in progress")}

   :savepoint-does-not-exist
   {:sqlstate "3B001"
    :format (fn [{:keys [name]}]
              (when name
                (str "savepoint \"" name "\" does not exist")))}

   :query-canceled
   {:sqlstate "57014"
    :format (fn [_] "canceling statement due to user request")}

   :statement-timeout
   {:sqlstate "57014"
    :format (fn [_] "canceling statement due to statement timeout")}

   ;; --- generic fallbacks ---------------------------------------------
   :invalid-parameter-value
   {:sqlstate "22023"
    :format (fn [{:keys [detail]}] detail)}

   :connection-failure
   {:sqlstate "08006" :format nil}

   :internal-error
   {:sqlstate "XX000" :format nil}})

;; ============================================================================
;; Datahike-internal error mapping (legacy + extended)
;; ============================================================================

(def dh-error->sqlstate
  "Map Datahike's `:error` keyword (set by datahike core when it raises
   ex-info) to a PG SQLSTATE. Covers errors the wire layer doesn't
   throw itself but receives via the cause chain.

   Datahike's messages are unstructured; the wire layer uses them
   verbatim unless `rewrite-datahike-message` recognises a known
   pattern and produces a PG-shaped substitute."
  {;; Schema validation: value type mismatch, bad entity value
   :transact/schema             "22P02"
   :retract/schema              "22P02"
   :schema/validation           "22P02"
   :transact/upsert             "23505"
   :transact/unique             "23505"
   :lookup-ref/unique           "23505"
   :transact/syntax             "42601"
   :entity-id/syntax            "22P02"
   :lookup-ref/syntax           "22P02"

   ;; Entity resolution
   :entity-id/missing           "42704"
   :db/invalid-attribute        "42703"

   ;; Query-time errors
   :query/invalid-clause        "42601"
   :query/where                 "42601"
   :query/where-conflict        "42703"
   :query/binding               "42601"

   ;; CAS / serialization
   :transact/cas                "40001"

   ;; Check-like / value constraints
   :transact/ensure             "23514"
   :transact/purge              "22023"
   :search/pattern              "22023"

   ;; Feature not supported
   :merge/sync-not-supported    "0A000"
   :transact/sync-not-supported "0A000"
   :transaction/filtered        "0A000"

   ;; Legacy cardinality / uniqueness
   :db.unique/identity          "23505"
   :db.unique/value             "23505"

   ;; Data import shape mismatch
   :import/mismatch             "22P02"

   ;; Connection / storage
   :db.error/connection         "08000"
   :db.error/storage            "58000"
   :db.error/serialization      "40001"})

;; ============================================================================
;; Datahike-message rewriting (the SQLAlchemy-class fix)
;; ============================================================================

(defn rewrite-datahike-message
  "Translate a Datahike-emitted error message into PG vocabulary.
   Returns `[code message extra-fields]` if the message matches a
   known pattern, nil otherwise.

   Datahike's `:transact/schema` for an unknown attribute reads:
     `Bad entity attribute :employee/dept_id at {:db/id 47, …},
      not defined in current schema`
   PG would say:
     `column \"dept_id\" of relation \"employee\" does not exist`
   with SQLSTATE 42703 (UndefinedColumn). Without this rewrite,
   SQL clients see SQLSTATE 22P02 (InvalidTextRepresentation) and
   `:db/id 47` in the user-facing string."
  [^String msg]
  (when msg
    (cond
      ;; "Bad entity attribute :ns/col at {…}, not defined in current schema"
      (re-find #"Bad entity attribute :([^/\s]+)/([^\s]+).*not defined in current schema" msg)
      (let [[_ table column] (re-find #"Bad entity attribute :([^/\s]+)/([^\s]+)" msg)]
        ["42703"
         (str "column \"" column "\" of relation \"" table "\" does not exist")
         {:table table :column column}])

      ;; "Bad entity value <v> at <stmt>, value '<v>' is not match schema"
      ;; (kept as 22P02 — value-doesn't-match-type — but tightened message
      ;; if we can extract attribute and value)
      (re-find #"Bad entity value .* at \[:db/(?:add|retract) [^\s]+ :([^/\s]+)/([^\s]+) ([^\]]+)\]" msg)
      (let [[_ table column value] (re-find #"Bad entity value .* at \[:db/(?:add|retract) [^\s]+ :([^/\s]+)/([^\s]+) ([^\]]+)\]" msg)]
        ["22P02"
         (str "invalid input syntax for column \"" column "\" of relation \"" table "\": " value)
         {:table table :column column}])

      :else nil)))

(defn classify-message
  "Last-resort SQLSTATE-only classifier for Datahike messages we can't
   structurally rewrite. Returns the SQLSTATE code or nil. Used only
   when ex-data has no `:error` / `:sqlstate` and
   `rewrite-datahike-message` doesn't match.

   Do not add new reliance on this from pgwire code — set `:error` or
   `:sqlstate` at the throw site instead."
  [^String msg]
  (when msg
    (cond
      (re-find #"(?i)Bad entity value .* does not match schema" msg) "22P02"
      (re-find #"(?i)cannot be cast to class" msg) "22P02"
      (re-find #"(?i)unique constraint|unique value.*already" msg) "23505"
      (re-find #"(?i)not-null|NOT NULL constraint" msg) "23502"
      (re-find #"(?i)foreign key|ref.*not.*exist" msg) "23503"
      (re-find #"(?i)SQL parse error|ParseException|Unsupported SQL" msg) "42601"
      (re-find #"(?i)column .* does not exist|unknown.*attribute" msg) "42703"
      (re-find #"(?i)relation .* does not exist|unknown.*table|table .* does not exist" msg) "42P01"
      (re-find #"(?i)transaction.*conflict|stale.*db|CAS.*failed" msg) "40001"
      :else nil)))

;; ============================================================================
;; ErrorResponse field extraction
;; ============================================================================

(defn- extract-error-fields
  "Pull ErrorResponse-style detail fields out of ex-data. Returns a
   Java Map<String,String> keyed by PG protocol field codes (n
   constraint, t table, c column, d data type, D detail, H hint), or
   nil when nothing maps.

   ORMs depend on these — psycopg2's `Diagnostics.constraint_name`,
   pgJDBC's `PSQLException.getServerErrorMessage().getConstraint()`."
  [data msg]
  (let [fields (java.util.HashMap.)
        ;; Pgwire-side throws set keys directly; Datahike-side throws
        ;; carry an `:attribute` keyword we split into table+column.
        attr (:attribute data)
        attr-table (when (keyword? attr) (namespace attr))
        attr-col (when (keyword? attr) (name attr))
        table (or (:table data) attr-table)
        column (or (:column data) attr-col)
        constraint (:constraint data)
        type-name (:type data)
        ;; :value is set for :transact/schema & :transact/upsert; for
        ;; :transact/unique the offending fact is in :datom.
        value (or (:value data)
                  (when-let [d (:datom data)]
                    (try (if (vector? d) (nth d 2 nil) (.v d))
                         (catch Exception _ nil))))
        detail (:detail data)
        hint (:hint data)
        error (:error data)]
    (when constraint (.put fields "n" constraint))
    (when table      (.put fields "t" table))
    (when column     (.put fields "c" column))
    (when type-name  (.put fields "d" (str/replace (str type-name) #"^:" "")))
    (when detail     (.put fields "D" (str detail)))
    (when hint       (.put fields "H" (str hint)))
    ;; Per-Datahike-error opportunistic fields (legacy paths). These
    ;; only fire when none of the above have already populated the
    ;; relevant slot.
    (when (and value (not (.containsKey fields "D")))
      (cond
        (or (= :db.unique/identity error)
            (= :db.unique/value error)
            (= :transact/upsert error)
            (= :transact/unique error)
            (= :lookup-ref/unique error))
        (.put fields "D" (str "duplicate value: " (pr-str value)))

        (or (= :transact/schema error)
            (= :retract/schema error)
            (= :schema/validation error))
        (.put fields "D" (str "value: " (pr-str value)))

        (or (= :transact/ensure error)
            (= :transact/purge error)
            (= :search/pattern error))
        (.put fields "D" (str "value: " (pr-str value)))))
    ;; Synthesise unique-constraint name if we have table+column but no
    ;; explicit constraint (legacy paths that throw ex-info without
    ;; setting :constraint).
    (when (and (not (.containsKey fields "n"))
               table column
               (or (= :db.unique/identity error)
                   (= :db.unique/value error)
                   (= :transact/upsert error)
                   (= :transact/unique error)
                   (= :lookup-ref/unique error)))
      (.put fields "n" (str table "_" column "_key")))
    ;; Schema-level type info from a Datahike `:transact/schema` etc.
    (when-let [vt (or (:db/valueType (:schema data))
                      (:valueType (:schema data)))]
      (when-not (.containsKey fields "d")
        (.put fields "d" (clojure.core/name vt))))
    ;; Fallback: parse the message for `[:db/add _ :ns/col …]` to
    ;; populate t/c when no structured ex-data was provided.
    (when (and (not (.containsKey fields "t"))
               msg)
      (when-let [[_ tbl c] (re-find #"\[:db/add\s+[^\s]+\s+:([^/\s]+)/([^\s]+)\s" msg)]
        (.put fields "t" tbl)
        (.put fields "c" c)))
    (when (pos? (.size fields)) fields)))

;; ============================================================================
;; Cause-chain walk
;; ============================================================================

(defn- find-ex-data
  "Walk up to 8 levels of cause chain and return the first non-empty
   ex-data. Datahike's async writer wraps the real cause in an
   ExecutionException + a re-raised ExceptionInfo with empty ex-data,
   so looking only at the outer exception loses the `:error` key."
  [^Throwable e]
  (loop [x e depth 0]
    (cond
      (nil? x) nil
      (> depth 8) nil
      :else
      (let [d (when (instance? clojure.lang.IExceptionInfo x) (ex-data x))]
        (if (and d (seq d)) d (recur (.getCause x) (inc depth)))))))

(defn- find-cause-message
  "Walk the cause chain for the first non-blank message string, in
   case the outer exception has only a wrapper message."
  [^Throwable e]
  (loop [x e depth 0]
    (cond
      (nil? x) nil
      (> depth 8) nil
      :else
      (let [m (.getMessage x)]
        (if (and m (not (str/blank? m))) m (recur (.getCause x) (inc depth)))))))

;; ============================================================================
;; Public entry — classify-exception
;; ============================================================================

(defn classify-exception
  "Map a Throwable to `[sqlstate message fields]` for emission via the
   pgwire ErrorResponse. Falls back to `[\"XX000\" <some-msg> nil]`
   when no rule matches.

   Resolution order (first match wins):
     1. Explicit `:sqlstate` in ex-data — full override; uses the
        throw site's own message.
     2. `:error` key matched against `error-categories` — derives
        SQLSTATE; runs the category's `:format` fn over ex-data; if
        the formatter returns a string, that's the user-facing
        message, otherwise the throw site's message stands.
     3. `:datahike/canceled` flag → 57014.
     4. `:error` key matched against `dh-error->sqlstate` (Datahike
        internal). Tries `rewrite-datahike-message` to upgrade the
        message + extract extra fields.
     5. `rewrite-datahike-message` on a bare message (no error key).
     6. `classify-message` regex (last-resort SQLSTATE only).
     7. Fallback `XX000`.

   `fields` is a Java Map<String,String> of optional ErrorResponse
   field codes (n, t, c, d, D, H), or nil."
  [^Throwable e]
  (let [outer-msg (.getMessage e)
        msg (or (when-not (str/blank? outer-msg) outer-msg)
                (find-cause-message e)
                (.getSimpleName (class e)))
        data (or (find-ex-data e) {})
        explicit-sqlstate (:sqlstate data)
        category-key (:error data)
        category (when category-key (get error-categories category-key))
        ;; Explicit :sqlstate is a full override — skip the category
        ;; formatter, leaving the throw site's own message intact.
        formatted (when (and category (not explicit-sqlstate))
                    (when-let [f (:format category)] (f data)))
        canceled? (:datahike/canceled data)
        ;; Datahike's own error keys (`:transact/schema`, `:transact/upsert`,
        ;; …) live in dh-error->sqlstate. Try this whenever the key
        ;; isn't a registered pgwire category.
        dh-code (when-not category (get dh-error->sqlstate category-key))
        ;; Try to rewrite Datahike messages even when we got a code
        ;; from the dh table — the SQLSTATE may be wrong (e.g. 22P02
        ;; for `:transact/schema` is too coarse when the real cause is
        ;; an undefined attribute → 42703).
        dh-rewrite (rewrite-datahike-message msg)
        regex-code (when (and (nil? explicit-sqlstate)
                              (nil? category)
                              (nil? canceled?)
                              (nil? dh-code)
                              (nil? dh-rewrite))
                     (classify-message msg))
        code (or explicit-sqlstate
                 (when category (:sqlstate category))
                 (when canceled? "57014")
                 (when dh-rewrite (first dh-rewrite))
                 dh-code
                 regex-code
                 "XX000")
        ;; Message preference: category formatter > dh-rewrite > raw.
        final-msg (or formatted
                      (when dh-rewrite (second dh-rewrite))
                      msg)
        ;; Fields: union of data-driven extraction and any extras from
        ;; dh-rewrite (e.g. {:table … :column …} parsed from the msg).
        merged-data (if dh-rewrite
                      (merge (get dh-rewrite 2) data)
                      data)
        fields (extract-error-fields merged-data msg)]
    [code final-msg fields]))
