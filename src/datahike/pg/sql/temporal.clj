(ns datahike.pg.sql.temporal
  "SQL:2011 SELECT-side temporal clause preprocessor.

   Recognises `FOR <AXIS> <spec>` / `FOR ALL <AXIS>` clauses on SELECT
   statements, strips them from the SQL string, and returns a
   side-channel override map the handler threads into `apply-temporal`
   for THIS statement only — equivalent to a per-statement `SET
   datahike.{valid_at,system_at} = …` that auto-resets after the query.

   Two axes supported:

   VALID_TIME — the application-domain time axis. Routed through
   `d/valid-at` / `d/valid-between` (vt-aware secondary index when
   present; predicate fallback otherwise).
     - `FOR VALID_TIME AS OF <expr>`        → {:valid-at <Date>}
     - `FOR VALID_TIME BETWEEN <a> AND <b>` → {:valid-between [a b]}
     - `FOR VALID_TIME FROM <a> TO <b>`     → {:valid-between [a b]}
     - `FOR ALL VALID_TIME` (or `FOR VALID_TIME ALL`) → {:valid-at :all}

   SYSTEM_TIME — the transaction-time axis. Routed through `d/as-of`.
     - `FOR SYSTEM_TIME AS OF <expr>`       → {:as-of <Date>}
     - `FOR ALL SYSTEM_TIME` (or `FOR SYSTEM_TIME ALL`) → {:as-of :all}
   BETWEEN / FROM-TO on SYSTEM_TIME is rejected — datahike's
   `d/as-of` takes a single time-point; range tx-time reads would
   need a primitive that doesn't currently exist.

   The two axes compose: `SELECT … FROM t FOR SYSTEM_TIME AS OF '…'
   FOR VALID_TIME AS OF '…'` produces `{:as-of <Date> :valid-at
   <Date>}`. `apply-temporal` wraps `d/as-of` first, then tags
   `:datahike/valid-at` on the result — matching the composition
   order documented on `d/as-of`.

   `:{valid-at,as-of} :all` instructs `apply-temporal` to clear any
   session-scoped marker on that axis for the duration of this query
   — useful when the connection has `SET datahike.{valid_at,as_of}`
   pinned and a single query wants the unfiltered view.

   The preprocessor is char-based (not token-based) because it runs
   BEFORE the main SQL tokenization + rewrite pipeline; it operates
   on the raw SQL string so the downstream parser never sees the
   non-standard `FOR <AXIS>` keywords."
  (:require [clojure.string :as str]))

;; ===========================================================================
;; Low-level scanning helpers
;; ===========================================================================

(defn- skip-string
  "Position at a quote char; advance past the closing quote, handling
   doubled-quote escape (`''` inside `'...'`)."
  ^long [^String s ^long i ^Character q]
  (let [n (.length s)]
    (loop [j (inc i)]
      (cond
        (>= j n) j
        (= q (.charAt s j))
        (if (and (< (inc j) n) (= q (.charAt s (inc j))))
          (recur (+ j 2))
          (inc j))
        :else (recur (inc j))))))

(defn- skip-ws
  ^long [^String s ^long start]
  (let [n (.length s)]
    (loop [j start]
      (if (and (< j n) (Character/isWhitespace (.charAt s j)))
        (recur (inc j)) j))))

(defn- word-char? [^Character c]
  (or (Character/isLetterOrDigit c) (= \_ c)))

(defn- read-word
  "Read an unquoted identifier or keyword starting at i. Returns
   [end-index lowercase-word] or [i nil] if the char at i isn't a
   word start."
  [^String s ^long i]
  (let [n (.length s)
        c (when (< i n) (.charAt s i))]
    (if (or (nil? c) (not (or (Character/isLetter ^Character c) (= \_ c))))
      [i nil]
      (loop [j (inc i)]
        (if (and (< j n) (word-char? (.charAt s j)))
          (recur (inc j))
          [j (.toLowerCase (subs s i j))])))))

(defn- next-keyword
  "Skip whitespace then read the next word (lowercase). Returns
   [end-index word-or-nil]."
  [^String s ^long start]
  (let [j (skip-ws s start)]
    (read-word s j)))

(def ^:private temporal-tail-keywords
  "Keywords after which a temporal-literal expression ends — when we
   see any of these, the literal has terminated."
  #{"where" "group" "order" "limit" "having" "fetch" "offset"
    "union" "intersect" "except" "for" "join" "on" "using" ")"})

(defn- find-balanced-end
  "Starting at i, advance past a temporal-literal expression and stop
   when we hit any of `stop-words` (case-insensitive) at depth-0. The
   literal can include parens (balanced), string literals, and word
   tokens. Returns the end index (exclusive)."
  [^String s ^long i stop-words]
  (let [n (.length s)
        stop (set (map str/lower-case stop-words))]
    (loop [j i depth 0]
      (cond
        (>= j n) j
        (and (zero? depth)
             (or (Character/isLetter (.charAt s j))
                 (= \_ (.charAt s j))))
        (let [[wend* w] (read-word s j)
              wend (long wend*)]
          (if (contains? stop w)
            j
            (recur wend depth)))
        (= \' (.charAt s j))
        (recur (skip-string s j \') depth)
        (= \( (.charAt s j))
        (recur (inc j) (inc depth))
        (= \) (.charAt s j))
        (if (zero? depth)
          j ;; closing paren outside our scope
          (recur (inc j) (dec depth)))
        :else
        (recur (inc j) depth)))))

;; ===========================================================================
;; Literal parser
;; ===========================================================================

(defn parse-temporal-literal
  "Parse a temporal expression substring into a java.util.Date.

   Accepts:
     - ISO instants:  '2024-04-15T00:00:00Z', '2024-04-15T00:00:00.000Z'
     - ISO date-only: '2024-04-15' → 2024-04-15T00:00:00Z
     - millis since epoch: 1713139200000
     - the bare keyword `MAX_VALUE` → Long/MAX_VALUE sentinel
     - the bare keyword `MIN_VALUE` → Long/MIN_VALUE sentinel

   Returns either java.util.Date or one of the Long sentinels.
   Throws ex-info on unparseable input."
  [^String expr-str]
  (let [v (str/trim (or expr-str ""))
        ;; Strip surrounding quotes if present
        v (if (and (> (count v) 1)
                   (= \' (first v))
                   (= \' (last v)))
            (subs v 1 (dec (count v)))
            v)]
    (cond
      (= v "MAX_VALUE") Long/MAX_VALUE
      (= v "MIN_VALUE") Long/MIN_VALUE
      (re-matches #"\d+" v)
      (java.util.Date. (Long/parseLong v))
      :else
      (try
        ;; Date-only YYYY-MM-DD: pad to midnight UTC
        (let [iso (if (re-matches #"\d{4}-\d{2}-\d{2}" v)
                    (str v "T00:00:00Z")
                    v)]
          (java.util.Date/from (java.time.Instant/parse iso)))
        (catch Exception e
          (throw (ex-info (str "Cannot parse temporal literal: " (pr-str expr-str))
                          {:error :sql/bad-temporal-literal
                           :input expr-str}
                          e)))))))

;; ===========================================================================
;; FOR <AXIS> clause finder
;; ===========================================================================

(defn- skip-non-temporal
  "Walk past content the FOR-axis scanner shouldn't peek into: string
   literals, quoted identifiers, line comments, block comments.
   Returns next index, or `i` if nothing to skip."
  ^long [^String s ^long i]
  (let [n (.length s)
        c (when (< i n) (.charAt s i))]
    (cond
      (nil? c) i
      (= \' c) (skip-string s i \')
      (= \" c) (skip-string s i \")
      (and (= \- c) (< (inc i) n) (= \- (.charAt s (inc i))))
      (loop [j (+ i 2)]
        (cond (>= j n) j
              (= \newline (.charAt s j)) (inc j)
              :else (recur (inc j))))
      (and (= \/ c) (< (inc i) n) (= \* (.charAt s (inc i))))
      (loop [j (+ i 2)]
        (cond (>= j n) j
              (and (< (inc j) n) (= \* (.charAt s j)) (= \/ (.charAt s (inc j))))
              (+ j 2)
              :else (recur (inc j))))
      :else i)))

(defn- parse-for-axis-spec
  "At index `start` (positioned at `FOR`), try to recognise a `FOR
   <AXIS> <spec>` or `FOR ALL <AXIS>` clause for the given `axis`
   (`\"valid_time\"` or `\"system_time\"`). Returns `[end-index spec]`
   on success — `end-index` is just past the spec, `spec` is one of
   `{:kind :all}`, `{:kind :as-of :at <expr>}`,
   `{:kind :between :from <expr> :to <expr>}`, or
   `{:kind :from-to :from <expr> :to <expr>}` — or `nil` if the
   sequence at `start` is not a recognised temporal clause.

   The parser is grammar-only; the caller decides which kinds are
   semantically meaningful for the axis (SYSTEM_TIME accepts only
   `:all` and `:as-of`)."
  [^String sql ^long start ^String axis]
  (let [[wend* w1] (read-word sql start)]
    (when (= w1 "for")
      (let [[k1end* k1] (next-keyword sql (long wend*))
            k1end (long k1end*)]
        (cond
          ;; FOR ALL <AXIS>
          (= k1 "all")
          (let [[k2end* k2] (next-keyword sql k1end)]
            (when (= k2 axis)
              [(long k2end*) {:kind :all}]))

          ;; FOR <AXIS> <spec>
          (= k1 axis)
          (let [[k2end* k2] (next-keyword sql k1end)
                k2end (long k2end*)]
            (cond
              ;; FOR <AXIS> ALL
              (= k2 "all") [k2end {:kind :all}]

              ;; FOR <AXIS> AS OF <expr>
              (= k2 "as")
              (let [[ofend* of] (next-keyword sql k2end)]
                (when (= of "of")
                  (let [end (long (find-balanced-end sql (long ofend*) temporal-tail-keywords))
                        expr (str/trim (subs sql (long ofend*) end))]
                    [end {:kind :as-of :at (parse-temporal-literal expr)}])))

              ;; FOR <AXIS> BETWEEN <a> AND <b>
              (= k2 "between")
              (let [from-end (long (find-balanced-end sql k2end #{"and"}))
                    from-expr (subs sql k2end from-end)
                    [after-and* _] (next-keyword sql from-end)
                    after-and (long after-and*)
                    to-end (long (find-balanced-end sql after-and temporal-tail-keywords))
                    to-expr (subs sql after-and to-end)]
                [to-end {:kind :between
                         :from (parse-temporal-literal from-expr)
                         :to   (parse-temporal-literal to-expr)}])

              ;; FOR <AXIS> FROM <a> TO <b>
              (= k2 "from")
              (let [from-end (long (find-balanced-end sql k2end #{"to"}))
                    from-expr (subs sql k2end from-end)
                    [after-to* _] (next-keyword sql from-end)
                    after-to (long after-to*)
                    to-end (long (find-balanced-end sql after-to temporal-tail-keywords))
                    to-expr (subs sql after-to to-end)]
                [to-end {:kind :from-to
                         :from (parse-temporal-literal from-expr)
                         :to   (parse-temporal-literal to-expr)}])

              :else nil)))))))

(defn- parse-for-valid-time-spec [^String sql ^long start]
  (parse-for-axis-spec sql start "valid_time"))

(defn- parse-for-system-time-spec
  "SYSTEM_TIME axis: only `AS OF` and `ALL` are supported. Datahike's
   `d/as-of` takes a single time-point; range tx-time reads would
   need a primitive that doesn't currently exist. We reject
   `BETWEEN` / `FROM-TO` at preprocess time with a clear error so
   the failure points at the SQL clause rather than at a downstream
   adapter."
  [^String sql ^long start]
  (when-let [[end spec] (parse-for-axis-spec sql start "system_time")]
    (when (contains? #{:between :from-to} (:kind spec))
      (throw (ex-info (str "FOR SYSTEM_TIME "
                           (str/upper-case (str/replace (name (:kind spec)) #"-" " "))
                           " is not yet supported. Datahike's `d/as-of` "
                           "takes a single time-point; range tx-time reads "
                           "would need a range primitive that does not "
                           "currently exist. Use `FOR SYSTEM_TIME AS OF "
                           "<single-point>` or split into separate queries.")
                      {:error :sql/system-time-range-unsupported
                       :kind (:kind spec)})))
    [end spec]))

(defn- find-for-axis
  "Scan `sql` from the start for the next clause matched by `parse-fn`.
   `parse-fn` is one of `parse-for-valid-time-spec` /
   `parse-for-system-time-spec`. Returns `[start end spec]` on first
   match or `nil` if none found. `start` is the position of the `FOR`
   keyword; `end` is just past the spec; `spec` is the parsed-spec
   map."
  [^String sql parse-fn]
  (let [n (.length sql)]
    (loop [i 0]
      (cond
        (>= i n) nil
        :else
        (let [j (skip-non-temporal sql i)]
          (if (not= j i)
            (recur j)
            (cond
              (and (or (Character/isLetter (.charAt sql i)) (= \_ (.charAt sql i)))
                   ;; Word-boundary on the left
                   (or (zero? i) (not (word-char? (.charAt sql (dec i))))))
              (if-let [[end spec] (parse-fn sql i)]
                [i end spec]
                (let [[wend _] (read-word sql i)]
                  (recur (long wend))))
              :else (recur (inc i)))))))))

(defn- find-for-valid-time [^String sql]
  (find-for-axis sql parse-for-valid-time-spec))

(defn- find-for-system-time [^String sql]
  (find-for-axis sql parse-for-system-time-spec))

;; ===========================================================================
;; Public preprocessor
;; ===========================================================================

(defn- strip-axis
  "Strip every `FOR <AXIS> <spec>` clause matched by `find-fn` from
   `sql`. Returns `{:sql <stripped-sql> :spec <spec-or-nil>}`. At
   most one clause per axis is allowed (multi-table reads with one
   clause per joined table are ambiguous without qualifier-aware
   resolution); a second match throws `axis-err-data`."
  [^String sql find-fn axis-err-data]
  (loop [sql sql, specs []]
    (if-let [[start end spec] (find-fn sql)]
      (do
        (when (seq specs)
          (throw (ex-info (:msg axis-err-data) (dissoc axis-err-data :msg))))
        (recur (str (subs sql 0 start) " " (subs sql end))
               (conj specs spec)))
      {:sql sql :spec (first specs)})))

(defn- valid-time-override [spec]
  (case (:kind spec)
    nil      nil
    :all     {:valid-at :all}
    :as-of   {:valid-at (:at spec)}
    :between {:valid-between [(:from spec) (:to spec)]}
    :from-to {:valid-between [(:from spec) (:to spec)]}))

(defn- system-time-override [spec]
  (case (:kind spec)
    nil    nil
    :all   {:as-of :all}
    :as-of {:as-of (:at spec)}))

(defn preprocess
  "Strip every `FOR VALID_TIME` / `FOR SYSTEM_TIME` clause from `sql`.
   Returns `{:sql <stripped-sql> :override <map-or-nil>}`.

   The override map can carry any combination of:
     `:valid-at <Date>`                — VALID_TIME AS OF
     `:valid-between [<Date> <Date>]`  — VALID_TIME BETWEEN / FROM-TO
     `:valid-at :all`                  — FOR ALL VALID_TIME (clears any
                                         session-scoped pin for this stmt)
     `:as-of   <Date>`                 — SYSTEM_TIME AS OF
     `:as-of :all`                     — FOR ALL SYSTEM_TIME

   `nil` when no clause is present.

   Multiple FOR-clauses on the SAME axis (e.g. two `FOR VALID_TIME`s
   for two joined tables) is rejected — the per-statement override
   applies one pin to the whole query. Use explicit per-column
   predicates (`<table>._valid_from`/`._valid_to`) for the
   multi-table case, or split into separate queries.

   Mixing the two axes in one statement is supported: `FOR
   SYSTEM_TIME AS OF '…' FOR VALID_TIME AS OF '…'` produces
   `{:as-of <Date> :valid-at <Date>}`."
  [^String sql]
  (let [{vt-sql :sql vt-spec :spec}
        (strip-axis sql find-for-valid-time
                    {:msg (str "Multi-table SELECT with more than one "
                               "FOR VALID_TIME clause is not yet supported "
                               "— the per-statement override applies one "
                               "valid-time pin to the whole query. Use "
                               "explicit per-column predicates "
                               "(`<table>._valid_from`/`._valid_to`) for "
                               "the multi-table case, or split into "
                               "separate queries.")
                     :error :sql/multi-for-valid-time-unsupported})
        {st-sql :sql st-spec :spec}
        (strip-axis vt-sql find-for-system-time
                    {:msg (str "More than one FOR SYSTEM_TIME clause in "
                               "one statement is not supported — pin "
                               "tx-time once per query. Use a session-"
                               "level `SET datahike.as_of` or split into "
                               "separate queries.")
                     :error :sql/multi-for-system-time-unsupported})
        override (not-empty
                  (merge (valid-time-override vt-spec)
                         (system-time-override st-spec)))]
    {:sql st-sql :override override}))
