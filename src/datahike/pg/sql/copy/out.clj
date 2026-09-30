(ns datahike.pg.sql.copy.out
  "Field encoders for `COPY ... TO`, transcribed from PostgreSQL's
   `copyto.c` (`CopyAttributeOutText` / `CopyAttributeOutCSV`).

   The decoders in this package's sibling namespaces answer \"what did
   the client mean\"; these answer \"what does PostgreSQL write\", and
   the two are not mirror images. Text output escapes six control
   characters by C notation and passes the other ASCII control
   characters through literally, while the reader accepts far more than
   the writer ever emits. CSV output quotes only when it must, and what
   counts as must includes a value that merely *looks* like the NULL
   marker -- otherwise the file would not read back.

   A NULL is the null marker itself, written raw: in CSV it is
   deliberately NOT quoted, which is exactly how an unquoted empty
   field stays distinguishable from a quoted empty string."
  (:require [clojure.string :as str]))

(def ^:private text-escapes
  "The control characters `CopyAttributeOutText` spells in C notation.
   Every other character below 0x20 is written literally -- PostgreSQL
   escapes these six because they are the ones that would otherwise be
   mangled by line-ending translation, not because they are control
   characters."
  {\backspace "\\b"
   \formfeed  "\\f"
   \newline   "\\n"
   \return    "\\r"
   \tab       "\\t"
   \u000B     "\\v"})

(defn text-field
  "Encode one rendered field for COPY's text format. `nil` is NULL and
   becomes `null-marker` verbatim."
  [^String s ^String delimiter ^String null-marker]
  (if (nil? s)
    null-marker
    (let [delim-c (.charAt ^String delimiter 0)
          out (StringBuilder. (.length s))]
      (dotimes [i (.length s)]
        (let [c (.charAt s i)]
          (if-let [esc (get text-escapes c)]
            (.append out ^String esc)
            (cond
              ;; The delimiter must be escaped whatever it is, or the
              ;; field would split when read back.
              (= c delim-c) (doto out (.append \\) (.append c))
              (= c \\)      (.append out "\\\\")
              :else         (.append out c)))))
      (.toString out))))

(defn csv-needs-quote?
  "PostgreSQL's preliminary pass in `CopyAttributeOutCSV`.

   `\\.` alone in a single-column file is quoted so an older reader
   cannot mistake it for the end-of-data marker, and a value equal to
   the NULL marker is quoted so it reads back as data rather than NULL."
  [^String s ^String delimiter ^String quote ^String null-marker single-attr?]
  (let [delim-c (.charAt ^String delimiter 0)
        quote-c (.charAt ^String quote 0)]
    (or (= s null-marker)
        (and single-attr? (= s "\\."))
        (boolean (some (fn [c]
                         (or (= c delim-c) (= c quote-c)
                             (= c \newline) (= c \return)))
                       s)))))

(defn csv-field
  "Encode one rendered field for COPY's CSV format. `nil` is NULL and
   becomes `null-marker` verbatim -- unquoted, which is what keeps an
   empty unquoted field distinct from a quoted empty string."
  [^String s ^String delimiter ^String quote ^String escape
   ^String null-marker {:keys [force-quote? single-attr?]}]
  (if (nil? s)
    null-marker
    (if-not (or force-quote?
                (csv-needs-quote? s delimiter quote null-marker single-attr?))
      s
      (let [quote-c (.charAt ^String quote 0)
            escape-c (.charAt ^String escape 0)
            out (doto (StringBuilder. (+ 2 (.length s))) (.append quote-c))]
        (dotimes [i (.length s)]
          (let [c (.charAt s i)]
            (when (or (= c quote-c) (= c escape-c))
              (.append out escape-c))
            (.append out c)))
        (.toString (.append out quote-c))))))

(defn row-line
  "Encode one row -- a vector of rendered field strings, `nil` for NULL
   -- as the line COPY writes, without its terminating newline.

   `columns` names the fields positionally, which is what FORCE_QUOTE
   selects on; it is only consulted in CSV mode."
  [fields columns {:keys [format delimiter null-marker quote escape force-quote]}]
  (let [single? (= 1 (count fields))
        forced? (cond
                  (= :all force-quote) (constantly true)
                  (set? force-quote)   (fn [i] (contains? force-quote (nth columns i nil)))
                  :else                (constantly false))]
    (str/join delimiter
              (map-indexed
               (fn [i f]
                 (if (= :csv format)
                   (csv-field f delimiter quote escape null-marker
                              {:force-quote? (forced? i)
                               :single-attr? single?})
                   (text-field f delimiter null-marker)))
               fields))))
