(ns datahike.pg.sql.copy.csv-format
  "PostgreSQL COPY-IN CSV-format decoder. Quote-aware state machine
   matching PG's `CopyReadAttributesCSV` semantics from
   `../postgres/src/backend/commands/copyfromparse.c:1827`.

   CSV is a *different beast* from text format:

     - Backslash is a literal char (no escape sequences).
     - End-of-data marker `\\.` is **not** recognised inside CSV
       streams (uses CopyDone / EOF instead).
     - NULL detection only fires on **unquoted** fields whose raw
       text matches the null marker. `\"\"` is empty-string, never
       null (unless FORCE_NULL is set for that column).
     - Embedded delimiters / line terminators are allowed inside
       quoted fields.
     - Embedded quote chars in quoted fields are escaped by either:
         - doubling them (default: ESCAPE = QUOTE = `\"`)
         - prefixing with the configured escape char

   The state machine per row:

     - Start in NOT_QUOTED. Walk bytes:
         delimiter → end of field
         line terminator → end of row
         quote char → enter QUOTED, set saw_quote=true
         else → append to field
     - In QUOTED, walk bytes:
         escape char (peek next):
           if next is escape or quote → consume, append literal
           else fall through (treat as literal)
         quote char → exit QUOTED (back to NOT_QUOTED)
         else → append to field

     - At end of row, for each field:
         if !saw_quote AND raw == null_marker → field is ::null
         else → field is the de-escaped string

     - FORCE_NOT_NULL columns: skip the null check (always treated
       as non-null even if raw matches null_marker).
     - FORCE_NULL columns: NULL check applies even if quoted (so a
       quoted `\"\"` matching null_marker becomes null instead of
       empty string).

   Streaming API mirrors `text-format`:

     (def d (make-decoder opts))
     [d' rows eod?] = (decode-step d chunk)
     [rows eod?]   = (decode-finalize d')

   `opts` keys: `:delimiter`, `:null-marker`, `:quote`, `:escape`,
   `:header` (`:true|:false|:match`), `:force-not-null`, `:force-null`,
   `:columns` (used when HEADER MATCH is on).

   Output is a vector of vectors-of-(String|`::null`)."
  (:require [clojure.string :as str]))

;; ----------------------------------------------------------------------------
;; Decoder state
;; ----------------------------------------------------------------------------

(defn make-decoder
  "Build a fresh CSV decoder state."
  [{:keys [delimiter null-marker quote escape header
           force-not-null force-null columns]
    :or {header :false}
    :as opts}]
  {:pre [(string? delimiter)  (= 1 (count delimiter))
         (string? null-marker)
         (string? quote)      (= 1 (count quote))
         (string? escape)     (= 1 (count escape))]}
  {:opts          opts
   :columns       columns        ;; for HEADER MATCH validation
   :line-buf      (StringBuilder.)
   :state         :not-quoted    ;; :not-quoted | :quoted
   :field-buf     (StringBuilder.)
   :row           []             ;; fields accumulated in current row
   :saw-quote?    false          ;; for current field
   :saw-quote-flags []            ;; per-field, parallel to :row
   :eol-type      nil            ;; :nl | :cr | :crnl
   :header-skipped? (= :false header) ;; if :false, never skip
   :eod?          false})

(defn- match-null?
  "True iff `raw-text` matches the null-marker AND the field is
   eligible for null detection given quoting and FORCE_* options."
  [raw-text saw-quote? field-idx columns null-marker force-not-null force-null]
  (let [matches? (= raw-text null-marker)
        col-name (when (and columns (< field-idx (count columns)))
                   (nth columns field-idx))
        force-not-null? (cond
                          (= :all force-not-null) true
                          (and col-name (set? force-not-null))
                          (contains? force-not-null col-name)
                          :else false)
        force-null? (cond
                      (= :all force-null) true
                      (and col-name (set? force-null))
                      (contains? force-null col-name)
                      :else false)]
    (cond
      (not matches?) false
      force-not-null? false
      saw-quote? force-null?
      :else true)))

(defn- finalize-row
  "Apply NULL-marker detection per-field and return the
   final row vector. `state` is the decoder state with row
   and saw-quote-flags accumulators."
  [{:keys [opts columns] :as state}]
  (let [{:keys [null-marker force-not-null force-null]} opts
        row (:row state)
        flags (:saw-quote-flags state)]
    (->> (map-indexed
          (fn [idx ^String raw]
            (let [saw? (nth flags idx)]
              (if (match-null? raw saw? idx columns null-marker
                               force-not-null force-null)
                ::null
                raw)))
          row)
         vec)))

;; ----------------------------------------------------------------------------
;; Per-character state machine
;; ----------------------------------------------------------------------------

(defn- step-char
  "Advance the decoder state by one character. Returns either:
     state'                       — char consumed, state updated
     {:row-complete row state'}   — char consumed, row finished
   `eof?` true permits standalone CR to terminate (for finalize)."
  [state ^Character c next-c eof?]
  (let [{:keys [opts state-mode]} state
        {:keys [^String delimiter ^String quote ^String escape]} opts
        delim-c   (.charAt delimiter 0)
        quote-c   (.charAt quote 0)
        escape-c  (.charAt escape 0)
        ^StringBuilder fb (:field-buf state)
        s-mode    (:state state)]
    (cond
      ;; --- IN QUOTED MODE ---
      (= s-mode :quoted)
      (cond
        ;; Escape char + known-next: peek and decide. If escape and
        ;; quote are the same (the common default), this also handles
        ;; the doubled-quote case.
        (and (= c escape-c)
             (some? next-c)
             (or (= next-c escape-c) (= next-c quote-c)))
        (do (.append fb next-c)
            (assoc state :skip-next? true))

        ;; Escape char with NO next available — and we're not at EOF
        ;; — defer. We can't decide yet whether this is an escape
        ;; pair (consume both) or a quote-end (consume one); the next
        ;; chunk's first char tells us.
        (and (= c escape-c)
             (nil? next-c)
             (not eof?))
        (assoc state :defer? true)

        ;; End of quoted run (after escape disambiguation)
        (= c quote-c)
        (assoc state :state :not-quoted)

        :else
        (do (.append fb c) state))

      ;; --- IN NOT-QUOTED MODE ---
      :else
      (cond
        ;; Field separator
        (= c delim-c)
        (let [field-text (.toString fb)
              new-row (conj (:row state) field-text)
              new-flags (conj (:saw-quote-flags state) (:saw-quote? state))]
          (.setLength fb 0)
          (assoc state
                 :row new-row
                 :saw-quote-flags new-flags
                 :saw-quote? false))

        ;; Line terminator (LF)
        (= c \newline)
        (let [field-text (.toString fb)
              new-row (conj (:row state) field-text)
              new-flags (conj (:saw-quote-flags state) (:saw-quote? state))
              row-state (assoc state
                               :row new-row
                               :saw-quote-flags new-flags)
              final-row (finalize-row row-state)]
          (.setLength fb 0)
          {:row-complete final-row
           :state (-> state
                      (assoc :row [] :saw-quote-flags []
                             :saw-quote? false :state :not-quoted)
                      ;; Lock in EOL type
                      (update :eol-type #(or % :nl)))})

        ;; CR (might be CRLF or bare CR)
        (= c \return)
        (cond
          (= next-c \newline)
          ;; CRLF — caller handles by skipping next char too
          (let [field-text (.toString fb)
                new-row (conj (:row state) field-text)
                new-flags (conj (:saw-quote-flags state) (:saw-quote? state))
                row-state (assoc state
                                 :row new-row
                                 :saw-quote-flags new-flags)
                final-row (finalize-row row-state)]
            (.setLength fb 0)
            {:row-complete final-row
             :state (-> state
                        (assoc :row [] :saw-quote-flags []
                               :saw-quote? false :state :not-quoted
                               :skip-next? true)
                        (update :eol-type #(or % :crnl)))})

          (or (some? next-c) eof?)
          ;; Bare CR
          (let [field-text (.toString fb)
                new-row (conj (:row state) field-text)
                new-flags (conj (:saw-quote-flags state) (:saw-quote? state))
                row-state (assoc state
                                 :row new-row
                                 :saw-quote-flags new-flags)
                final-row (finalize-row row-state)]
            (.setLength fb 0)
            {:row-complete final-row
             :state (-> state
                        (assoc :row [] :saw-quote-flags []
                               :saw-quote? false :state :not-quoted)
                        (update :eol-type #(or % :cr)))})

          :else
          ;; CR at end-of-buffer with more chunks expected — defer.
          ;; Caller signals via returning the same state and we'll
          ;; back up the buffer position.
          (assoc state :defer? true))

        ;; Quote char (start of quoted field/run)
        (= c quote-c)
        (assoc state :state :quoted :saw-quote? true)

        :else
        (do (.append fb c) state)))))

(defn- consume-rows
  "Drive the state machine over the buffered string. Returns
   [updated-state rows-emitted eod?]. `eof?` true means CR at end is
   a terminator (no defer)."
  [state ^String s eof?]
  (let [n (.length s)]
    (loop [i 0
           st state
           rows []]
      (cond
        (>= i n)
        ;; Consumed everything we could
        [(assoc st :line-buf (StringBuilder.)) rows false]

        ;; If state set :defer? — back up and stash.
        (:defer? st)
        (let [tail (subs s (dec i) n)
              st' (-> st (dissoc :defer?) (assoc :line-buf (StringBuilder. ^String tail)))]
          [st' rows false])

        :else
        (let [c (.charAt s i)
              next-c (when (< (unchecked-inc i) n) (.charAt s (unchecked-inc i)))
              result (step-char st c next-c eof?)
              ;; skip-next? can come either at the top level (escape
              ;; pairs in :quoted mode) or buried in result.state
              ;; (CRLF in :not-quoted mode emits row-complete with
              ;; the next-state set, including :skip-next?).
              skip-next? (or (:skip-next? result)
                             (:skip-next? (:state result)))
              advance (if skip-next? 2 1)]
          (cond
            (:row-complete result)
            (let [next-st (-> (:state result) (dissoc :skip-next?))]
              (recur (+ i advance) next-st (conj rows (:row-complete result))))

            (:defer? result)
            (let [tail (subs s i n)
                  st'' (-> result (dissoc :defer?)
                           (assoc :line-buf (StringBuilder. ^String tail)))]
              [st'' rows false])

            :else
            (recur (+ i advance) (dissoc result :skip-next?) rows)))))))

(defn decode-step
  "Process one chunk. Returns [decoder' rows eod?]."
  [decoder ^String chunk]
  (if (:eod? decoder)
    [decoder [] true]
    (let [^StringBuilder buf (:line-buf decoder)
          _ (.append buf chunk)
          s (.toString buf)
          ;; Reset the buffer (consume-rows builds a fresh one for any tail)
          _ (.setLength buf 0)
          [d' rows _eod?] (consume-rows decoder s false)]
      ;; HEADER skip: if header was :true/:match and we haven't skipped
      ;; yet, drop the first emitted row.
      (let [{:keys [header columns]} (:opts d')
            should-skip? (and (not (:header-skipped? d'))
                              (#{:true :match} header)
                              (seq rows))
            ;; HEADER MATCH validation — first row's field values must
            ;; equal :columns (case-insensitive).
            header-mismatch (when (and should-skip? (= :match header))
                              (let [hdr (mapv str/lower-case (first rows))
                                    cols (mapv str/lower-case columns)]
                                (when-not (= hdr cols)
                                  {:expected cols :got hdr})))
            _ (when header-mismatch
                (throw (ex-info "COPY HEADER MATCH: column mismatch"
                                {:error :bad-copy-format
                                 :expected (:expected header-mismatch)
                                 :got (:got header-mismatch)})))
            rows' (if should-skip? (rest rows) rows)
            d'' (cond-> d'
                  should-skip? (assoc :header-skipped? true))]
        [d'' (vec rows') false]))))

(defn decode-finalize
  "Emit any remaining rows. Returns [rows eod?]."
  [decoder]
  (cond
    (:eod? decoder)
    [[] true]

    :else
    (let [^StringBuilder buf (:line-buf decoder)
          tail (.toString buf)]
      (cond
        ;; Nothing left
        (zero? (.length buf))
        ;; But maybe there's an in-flight row buffered without a
        ;; trailing newline (file didn't end with NL)
        (let [^StringBuilder fb (:field-buf decoder)
              has-partial? (or (pos? (.length fb)) (seq (:row decoder)))]
          (if has-partial?
            (let [field-text (.toString fb)
                  new-row (conj (:row decoder) field-text)
                  new-flags (conj (:saw-quote-flags decoder) (:saw-quote? decoder))
                  row-state (assoc decoder :row new-row :saw-quote-flags new-flags)
                  final-row (finalize-row row-state)]
              [[final-row] false])
            [[] false]))

        :else
        (let [[d' rows _eod?] (consume-rows decoder tail true)
              ;; Then pick up any in-flight partial row
              ^StringBuilder fb (:field-buf d')
              has-partial? (or (pos? (.length fb)) (seq (:row d')))
              [final-rows]
              (if has-partial?
                (let [field-text (.toString fb)
                      new-row (conj (:row d') field-text)
                      new-flags (conj (:saw-quote-flags d') (:saw-quote? d'))
                      row-state (assoc d' :row new-row :saw-quote-flags new-flags)
                      final-row (finalize-row row-state)]
                  [(conj rows final-row)])
                [rows])]
          [final-rows false])))))

(defn decode-all
  "Convenience: decode an entire seq of chunks and return all rows."
  [opts chunks]
  (loop [d (make-decoder opts)
         remaining chunks
         all-rows []]
    (if-let [c (first remaining)]
      (let [[d' rows eod?] (decode-step d c)]
        (if eod?
          (into all-rows rows)
          (recur d' (rest remaining) (into all-rows rows))))
      (let [[rows _] (decode-finalize d)]
        (into all-rows rows)))))
