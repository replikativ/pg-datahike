(ns datahike.pg.sql.copy.text-format
  "PostgreSQL COPY-IN text-format decoder. Pure data transformation:
   bytes / string chunks in, vectors of fields out. Field values are
   either Strings or the sentinel `::null` (used in place of `nil` so
   downstream tx-data builders can distinguish a missing column from
   an explicit NULL).

   Spec source: `../postgres/doc/src/sgml/ref/copy.sgml` and
   `../postgres/src/backend/commands/copyfromparse.c`.

   Salient mechanics:

     - **Lines** are delimited by `\\n`, `\\r`, or `\\r\\n`. Once the
       first terminator is observed, the EOL type is locked for the
       rest of the stream — mid-stream switching is an error per PG.
     - **End-of-data** marker: a line containing exactly `\\.`.
       Bytes after the EOD line are silently discarded.
     - **NULL marker**: configurable (default `\\N`). Checked against
       the *raw* (pre-de-escape) field text before backslash
       processing.
     - **Field separator**: configurable single ASCII byte (default
       `\\t`). Must be backslash-escaped to appear as a data byte.
     - **Backslash escapes** (recognised after delimiter/EOD parsing):
       `\\b \\f \\n \\r \\t \\v` for control chars; `\\NNN` octal
       (1-3 digits); `\\xNN` hex (1-2 digits); `\\<any-other-char>`
       passes the char through literally.

   Streaming API:

     (def d (make-decoder opts))
     (let [[d' rows eod?] (decode-step d chunk)]    ;; called per CopyData
       ...)
     (let [[rows eod?] (decode-finalize d')]        ;; called on CopyDone
       ...)

   `opts` keys: `:delimiter` (1-char String, required),
                `:null-marker` (String, required).
   Output: rows are vectors of (String | `::null`)."
  (:require [clojure.string :as str]))

;; ----------------------------------------------------------------------------
;; Decoder state — a plain map. Operations return a new state.
;; ----------------------------------------------------------------------------

(defn make-decoder
  "Build a fresh decoder state. `opts` map keys:
     :delimiter   — single-character String
     :null-marker — String matched against the raw field text"
  [{:keys [delimiter null-marker] :as opts}]
  {:pre [(string? delimiter) (= 1 (count delimiter))
         (string? null-marker)]}
  {:opts        opts
   :line-buf    (StringBuilder.)
   :eol-type    nil      ;; :nl | :cr | :crnl | nil
   :eod?        false})

;; ----------------------------------------------------------------------------
;; Field-level decoding
;; ----------------------------------------------------------------------------

(defn- decode-octal
  "Read 1-3 octal digits starting at `idx` in `line`. Returns
   [byte-value next-idx]."
  ^"[Ljava.lang.Object;" [^String line ^long idx]
  (let [n (.length line)]
    (loop [i idx, val 0, count 0]
      (if (or (>= i n) (>= count 3))
        (object-array [val i])
        (let [c (.charAt line i)]
          (if (and (>= (int c) (int \0)) (<= (int c) (int \7)))
            (recur (unchecked-inc i) (+ (* val 8) (- (int c) (int \0))) (inc count))
            (object-array [val i])))))))

(defn- decode-hex
  "Read 1-2 hex digits starting at `idx` in `line`. Returns
   [byte-value next-idx]."
  ^"[Ljava.lang.Object;" [^String line ^long idx]
  (let [n (.length line)]
    (loop [i idx, val 0, count 0]
      (if (or (>= i n) (>= count 2))
        (object-array [val i])
        (let [c (.charAt line i)
              d (cond
                  (and (>= (int c) (int \0)) (<= (int c) (int \9)))
                  (- (int c) (int \0))
                  (and (>= (int c) (int \a)) (<= (int c) (int \f)))
                  (+ 10 (- (int c) (int \a)))
                  (and (>= (int c) (int \A)) (<= (int c) (int \F)))
                  (+ 10 (- (int c) (int \A)))
                  :else nil)]
          (if d
            (recur (unchecked-inc i) (+ (* val 16) d) (inc count))
            (object-array [val i])))))))

(defn- de-escape-field
  "Apply PG backslash-escape rules to a raw field string. Returns
   the decoded String. Pure function."
  [^String raw]
  (let [n (.length raw)
        sb (StringBuilder. n)]
    (loop [i 0]
      (if (>= i n)
        (.toString sb)
        (let [c (.charAt raw i)]
          (if (and (= c \\) (< (unchecked-inc i) n))
            (let [next-c (.charAt raw (unchecked-inc i))]
              (case next-c
                \b (do (.append sb \backspace) (recur (+ i 2)))
                \f (do (.append sb \formfeed)  (recur (+ i 2)))
                \n (do (.append sb \newline)   (recur (+ i 2)))
                \r (do (.append sb \return)    (recur (+ i 2)))
                \t (do (.append sb \tab)       (recur (+ i 2)))
                \v (do (.append sb (char 11))  (recur (+ i 2)))
                \x (let [^"[Ljava.lang.Object;" hr (decode-hex raw (+ i 2))
                         val (long (aget hr 0))
                         end (long (aget hr 1))]
                     (if (= end (+ i 2))
                       ;; No hex digits after \x — pass `\x` through literally
                       ;; per spec ("any other backslashed char that is not
                       ;; mentioned will be taken to represent itself").
                       (do (.append sb \x) (recur (+ i 2)))
                       (do (.append sb (char val)) (recur end))))
                ;; Octal — \0..\7 prefix
                (if (and (>= (int next-c) (int \0))
                         (<= (int next-c) (int \7)))
                  (let [^"[Ljava.lang.Object;" or* (decode-octal raw (unchecked-inc i))
                        val (long (aget or* 0))
                        end (long (aget or* 1))]
                    (.append sb (char val))
                    (recur end))
                  ;; \<other> — pass char through literally
                  (do (.append sb next-c) (recur (+ i 2))))))
            (do (.append sb c) (recur (unchecked-inc i)))))))))

(defn- split-fields
  "Split a single decoded line into raw field strings using
   `delimiter`. Backslash escapes the delimiter and any other byte —
   PG: `Backslash characters can be used in the COPY data to quote
   data characters that might otherwise be taken as row or column
   delimiters`.

   Returns a vector of raw (still-escaped) field strings."
  [^String line ^String delimiter]
  (let [n (.length line)
        delim-c (.charAt delimiter 0)
        out (java.util.ArrayList.)
        sb (StringBuilder.)]
    (loop [i 0]
      (cond
        (>= i n)
        (do (.add out (.toString sb)) (vec out))

        (= (.charAt line i) \\)
        ;; Backslash + one more char — pass both through to the field
        ;; verbatim. The field's de-escape pass will interpret them.
        (do (.append sb \\)
            (when (< (unchecked-inc i) n)
              (.append sb (.charAt line (unchecked-inc i))))
            (recur (+ i 2)))

        (= (.charAt line i) delim-c)
        (do (.add out (.toString sb))
            (.setLength sb 0)
            (recur (unchecked-inc i)))

        :else
        (do (.append sb (.charAt line i))
            (recur (unchecked-inc i)))))))

(defn- decode-line
  "Decode a single complete line (with terminator already stripped).
   Returns either:
     {:eod? true}                     — the line was the EOD marker `\\.`
     {:row [String | ::null ...]}     — a normal data row"
  [^String line {:keys [^String delimiter ^String null-marker]}]
  (cond
    (= line "\\.")
    {:eod? true}

    :else
    (let [raw-fields (split-fields line delimiter)
          row (mapv (fn [^String raw]
                      (if (= raw null-marker)
                        ::null
                        (de-escape-field raw)))
                    raw-fields)]
      {:row row})))

;; ----------------------------------------------------------------------------
;; Streaming chunk processor
;; ----------------------------------------------------------------------------

(defn- detect-eol [eol-type c next-c]
  "Given current eol-type, and the current/next chars, return the new
   eol-type and how many chars to consume for the terminator. Returns
   nil if the char is not a line terminator."
  (cond
    (and (= c \return) (= next-c \newline))
    [:crnl 2]

    (= c \newline)
    (case eol-type
      (nil :nl) [:nl 1]
      :cr (throw (ex-info "literal newline found in data — inconsistent EOL"
                          {:error :bad-copy-format :eol-type eol-type}))
      [:nl 1])

    (= c \return)
    (case eol-type
      (nil :cr) [:cr 1]
      :nl (throw (ex-info "literal carriage return found in data — inconsistent EOL"
                          {:error :bad-copy-format :eol-type eol-type}))
      [:cr 1])

    :else
    nil))

(defn- consume-lines
  "Drain complete lines from the line-buffer. Returns
   [updated-line-buf rows eod? new-eol-type].

   `eof?` true means no more chunks will follow — a trailing CR is
   then treated as a terminator (rather than deferred for
   CRLF-disambiguation). Always false during decode-step; true
   during decode-finalize."
  [^StringBuilder buf eol-type opts eof?]
  (let [s (.toString buf)
        n (.length s)]
    (loop [i 0
           start 0
           current-eol eol-type
           rows []
           eod? false]
      (cond
        eod?
        ;; Once we see EOD, drop everything else in the buffer and
        ;; stop scanning.
        [(StringBuilder.) rows true current-eol]

        (>= i n)
        ;; No more chars — copy any unconsumed tail back to a fresh buf
        (let [tail (subs s start n)
              new-buf (StringBuilder. ^String tail)]
          [new-buf rows false current-eol])

        :else
        (let [c (.charAt s i)
              next-c (when (< (unchecked-inc i) n) (.charAt s (unchecked-inc i)))]
          (cond
            ;; CR at the very end of the buffer, with no follower yet
            ;; available AND more chunks expected — defer this line
            ;; until we know whether the next chunk starts with LF
            ;; (making CRLF) or not (making bare CR). Don't consume;
            ;; carry the partial line to the next decode-step.
            (and (= c \return) (nil? next-c) (not eof?))
            (let [tail (subs s start n)
                  new-buf (StringBuilder. ^String tail)]
              [new-buf rows false current-eol])

            :else
            (if-let [[new-eol n-consumed] (detect-eol current-eol c next-c)]
              (let [line (subs s start i)
                    decoded (decode-line line opts)]
                (if (:eod? decoded)
                  ;; End-of-data marker — done
                  (recur (+ i n-consumed) (+ i n-consumed) new-eol rows true)
                  (recur (+ i n-consumed) (+ i n-consumed) new-eol
                         (conj rows (:row decoded)) false)))
              (recur (unchecked-inc i) start current-eol rows false))))))))

(defn decode-step
  "Process one chunk. Returns [decoder' rows eod?]. After eod? is
   true, further chunks are silently discarded."
  [decoder ^String chunk]
  (if (:eod? decoder)
    [decoder [] true]
    (let [^StringBuilder buf (:line-buf decoder)
          _ (.append buf chunk)
          [new-buf rows eod? new-eol-type]
          (consume-lines buf (:eol-type decoder) (:opts decoder) false)]
      [(assoc decoder
              :line-buf new-buf
              :eol-type new-eol-type
              :eod? eod?)
       rows
       eod?])))

(defn decode-finalize
  "Called after the wire layer signals CopyDone. Returns [rows eod?].
   Any unterminated trailing data is treated as a single final line
   (pg accepts files without a trailing newline). If eod? was already
   set, the trailing buffer is ignored."
  [decoder]
  (cond
    (:eod? decoder)
    [[] true]

    :else
    (let [^StringBuilder buf (:line-buf decoder)]
      (if (zero? (.length buf))
        [[] false]
        ;; Re-run consume-lines with eof?=true so any deferred CR at
        ;; end-of-buffer is treated as a terminator instead of
        ;; carried over.
        (let [[new-buf rows eod? _]
              (consume-lines buf (:eol-type decoder) (:opts decoder) true)
              tail (.toString ^StringBuilder new-buf)]
          (cond
            ;; EOD marker hit — discard tail
            eod? [rows true]
            ;; Trailing un-terminated final line
            (pos? (count tail))
            (let [decoded (decode-line tail (:opts decoder))]
              (cond
                (:eod? decoded) [rows true]
                :else           [(conj rows (:row decoded)) false]))
            :else [rows false]))))))

(defn decode-all
  "Convenience: decode an entire seq of chunks and return all rows.
   Used by tests; production wire-protocol path calls decode-step /
   decode-finalize directly."
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
