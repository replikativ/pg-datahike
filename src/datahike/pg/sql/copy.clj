(ns datahike.pg.sql.copy
  "Token-driven hand-parser for `COPY ... FROM STDIN` (and `COPY ...
   TO STDOUT`, deferred). JSqlParser 5.x doesn't recognise COPY at
   all — UnsupportedStatement — so the wire-protocol layer needs
   structured access before it can drive the COPY-IN sub-protocol.

   Mirrors the structure of `datahike.pg.sql.database`: tokenise,
   parse the prefix (table + optional column list + FROM/TO target),
   then parse the option list in either:

     - **Modern paren form** — `WITH (key [=] value [, ...])`
     - **Legacy keyword form** — `[WITH] kw1 kw2 ...` (e.g.
       `WITH BINARY`, `WITH CSV HEADER`, `DELIMITER '|' NULL '\\N' CSV`)
       still in the wild from old pg_dump output.

   PG syntax (from `../postgres/doc/src/sgml/ref/copy.sgml`):

     COPY [schema.]table [ ( col [, ...] ) ]
         FROM { 'file' | PROGRAM 'cmd' | STDIN }
         [ [ WITH ] ( option [, ...] ) ]

   Options accepted:

     FORMAT          'text' | 'csv' | 'binary'
     DELIMITER       'X'                — single byte
     NULL            'X'                — null marker
     HEADER          BOOL | MATCH       — 1st row treatment
     QUOTE           'X'                — CSV quote char
     ESCAPE          'X'                — CSV escape char (defaults to QUOTE)
     FORCE_NOT_NULL  ( col, ... ) | *
     FORCE_NULL      ( col, ... ) | *
     FORCE_QUOTE     ( col, ... ) | *   — TO-only; we accept for COPY FROM as a no-op
     ENCODING        'X'                — accepted, ignored (UTF-8 internal)
     FREEZE          [ BOOL ]           — accepted, ignored
     DEFAULT         'X'                — defaults-marker (PG 16+)
     OIDS            [ BOOL ]           — legacy, removed in PG 12; rejected

   Output shape (for COPY FROM STDIN):
     {:type :copy-from-stdin
      :ns string                ;; lowercase table namespace
      :table string             ;; original-case table name
      :columns [string ...]     ;; lowercase, or nil if no col-list given
      :options {:format :text|:csv|:binary
                :delimiter String
                :null-marker String
                :quote String
                :escape String
                :header :true|:false|:match
                :force-not-null #{string ...} | :all
                :force-null #{string ...} | :all
                :encoding String
                :freeze? boolean
                :default-marker String}}

   Defaults (filled by `parse-copy-from`):
     text:  delimiter \"\\t\", null-marker \"\\N\"
     csv:   delimiter \",\",  null-marker \"\",
            quote \"\\\"\", escape = quote, header :false
     binary: rejected at this layer (returns :feature-not-supported)"
  (:require [clojure.string :as str]
            [datahike.pg.sql.database :as database]))

;; ----------------------------------------------------------------------------
;; Tokenizer — reuse the one from sql.database
;; ----------------------------------------------------------------------------

(def tokenize database/tokenize)

;; ----------------------------------------------------------------------------
;; Parser helpers
;; ----------------------------------------------------------------------------

(defn- ident-eq? [tok lower]
  (and (= :ident (first tok))
       (= lower (str/lower-case (second tok)))))

(defn- ident-token? [tok]
  (= :ident (first tok)))

(defn- consume-table-name
  "Parse `[schema.]name` from the front of `toks`. Returns
   [{:ns lower-namespace :table name} rest-toks]. The namespace lowers
   case-folded for use as a Datahike attribute namespace; the
   table-name keeps its original case for diagnostics."
  [toks]
  (let [t1 (first toks)]
    (when-not (ident-token? t1)
      (throw (ex-info (str "expected table name in COPY, got: " (pr-str t1))
                      {:error :syntax-error :got t1})))
    (let [first-name (second t1)]
      (cond
        ;; schema.name form
        (and (= :ident (first (second toks)) (first (nth toks 2 nil)))
             (= "." (second (second toks))))
        ;; Looks like table.column ($schema.$name)
        [{:ns (str/lower-case first-name)
          :table (second (nth toks 2 nil))}
         (drop 3 toks)]

        ;; schema . name with separated dot tokens
        ;; (Tokenizer treats `.` inside identifiers as part of the
        ;; ident if the surrounding chars are word chars. So
        ;; `public.users` lexes as a single ident "public.users".)
        (let [s first-name]
          (and (ident-token? t1) (str/includes? s ".")))
        (let [[ns nm] (str/split first-name #"\." 2)]
          [{:ns (str/lower-case ns) :table nm} (rest toks)])

        :else
        [{:ns nil :table first-name} (rest toks)]))))

(defn- consume-column-list
  "Parse the optional `(col, col, ...)` after the table name. Returns
   [columns-or-nil rest-toks]."
  [toks]
  (if-not (= :lparen (ffirst toks))
    [nil toks]
    (loop [t (rest toks) cols []]
      (let [t (if (= :comma (ffirst t)) (rest t) t)
            head (first t)]
        (cond
          (= :rparen (first head))
          [cols (rest t)]

          (ident-token? head)
          (recur (rest t) (conj cols (str/lower-case (second head))))

          :else
          (throw (ex-info (str "expected column name in COPY column list, got: "
                               (pr-str head))
                          {:error :syntax-error :got head})))))))

(defn- consume-from-target
  "Parse the FROM/TO clause. Returns [target rest-toks] where target
   is :stdin / :stdout / {:file path} / {:program cmd}. Throws
   :syntax-error if the keyword isn't FROM or TO."
  [toks expect-direction]
  (let [t1 (first toks)
        verb (cond
               (ident-eq? t1 "from") :from
               (ident-eq? t1 "to")   :to
               :else                 nil)]
    (when (or (nil? verb) (not= verb expect-direction))
      (throw (ex-info (str "expected " (name expect-direction)
                           " in COPY, got: " (pr-str t1))
                      {:error :syntax-error :got t1})))
    (let [[t2 & rest2] (rest toks)]
      (cond
        (and (= :from expect-direction) (ident-eq? t2 "stdin"))
        [:stdin rest2]

        (and (= :to expect-direction) (ident-eq? t2 "stdout"))
        [:stdout rest2]

        (= :string (first t2))
        [{:file (second t2)} rest2]

        (ident-eq? t2 "program")
        (let [t3 (first rest2)]
          (when-not (= :string (first t3))
            (throw (ex-info "expected string after PROGRAM in COPY"
                            {:error :syntax-error :got t3})))
          [{:program (second t3)} (rest rest2)])

        :else
        (throw (ex-info (str "expected stdin/stdout/'file'/PROGRAM in COPY, got: "
                             (pr-str t2))
                        {:error :syntax-error :got t2}))))))

;; ----------------------------------------------------------------------------
;; Option parser
;; ----------------------------------------------------------------------------

(defn- consume-paren-option-list
  "Parse the modern `(key [=] value [, key [=] value]*)` option list.
   Returns [options-vec rest-toks]. Each option is [name-string
   value-clj] (value can be string / number / boolean / keyword
   / vec for parenthesised column lists / :all for `*`)."
  [toks]
  (when-not (= :lparen (ffirst toks))
    (throw (ex-info "expected ( for COPY option list" {:error :syntax-error})))
  (loop [t (rest toks) out []]
    (let [t (if (= :comma (ffirst t)) (rest t) t)
          head (first t)]
      (cond
        (= :rparen (first head))
        [out (rest t)]

        (ident-token? head)
        (let [k (str/lower-case (second head))
              t1 (rest t)
              ;; optional =
              t2 (if (= :eq (ffirst t1)) (rest t1) t1)
              [vt vv] (first t2)
              [v t3]
              (cond
                ;; Bare keyword option — `FREEZE`, `BINARY`, `CSV`, `OIDS`
                ;; — next token is `,` or `)` so there's no value to
                ;; parse. Treat as `true`.
                (or (nil? vt) (= :rparen vt) (= :comma vt))
                [true t2]

                ;; (col [, col]*) — column list payload
                (= :lparen vt)
                (let [[cols rest-after] (consume-column-list t2)]
                  [(mapv #(str/lower-case %) cols) rest-after])

                ;; * (FORCE_NOT_NULL *, etc.)
                (and (= :ident vt) (= "*" vv))
                [:all (rest t2)]

                (= :string vt) [vv (rest t2)]
                (= :num vt)    [(Long/parseLong vv) (rest t2)]
                (= :bool vt)   [(Boolean/parseBoolean vv) (rest t2)]
                ;; HEADER MATCH — keep MATCH as a keyword for
                ;; normalize-header. All other ident values lowercase.
                (= :ident vt)
                (cond
                  (= "match" (str/lower-case vv)) [:match (rest t2)]
                  :else [(str/lower-case vv) (rest t2)])
                :else          [vv (rest t2)])]
          (recur t3 (conj out [k v])))

        :else
        (throw (ex-info (str "expected option name in COPY (...), got: "
                             (pr-str head))
                        {:error :syntax-error :got head}))))))

(defn- consume-legacy-option-list
  "Parse the pre-9.0 keyword option list (no parens, no commas):

     COPY t FROM stdin BINARY
     COPY t FROM stdin WITH CSV HEADER
     COPY t FROM stdin DELIMITER '|' NULL 'NIL' CSV HEADER

   Returns [options-vec rest-toks]. Order of options doesn't matter."
  [toks]
  (loop [t toks out []]
    (let [head (first t)]
      (cond
        ;; End of input or end of statement
        (or (nil? head) (= :semicolon (first head)))
        [out t]

        (ident-token? head)
        (let [k (str/lower-case (second head))]
          (case k
            ;; Boolean shortcuts (no value)
            ("binary" "csv" "freeze" "oids")
            (recur (rest t) (conj out [k true]))

            "header"
            ;; HEADER as a flag (no value) — treated as HEADER true.
            ;; If the next tok is a bool/MATCH, consume it.
            (let [t1 (rest t)
                  [vt vv] (first t1)]
              (cond
                (= :bool vt) (recur (rest t1) (conj out ["header" (Boolean/parseBoolean vv)]))
                (and (= :ident vt) (#{"match" "MATCH"} vv))
                (recur (rest t1) (conj out ["header" :match]))
                :else (recur t1 (conj out ["header" true]))))

            ;; String-valued options
            ("delimiter" "null" "quote" "escape" "encoding" "default")
            (let [t1 (rest t)
                  [vt vv] (first t1)]
              (when-not (= :string vt)
                (throw (ex-info (str "expected string after " (str/upper-case k)
                                     ", got: " (pr-str (first t1)))
                                {:error :syntax-error :option k :got (first t1)})))
              (recur (rest t1) (conj out [k vv])))

            ;; FORCE_NOT_NULL / FORCE_NULL / FORCE_QUOTE — column lists
            ("force_not_null" "force_null" "force_quote")
            (let [t1 (rest t)]
              (cond
                (= :lparen (ffirst t1))
                (let [[cols t-after] (consume-column-list t1)]
                  (recur t-after (conj out [k (mapv str/lower-case cols)])))
                ;; Bare * — applies to all columns
                (and (= :ident (ffirst t1)) (= "*" (second (first t1))))
                (recur (rest t1) (conj out [k :all]))
                :else
                (throw (ex-info (str (str/upper-case k) " expects ( col, ... ) or *")
                                {:error :syntax-error}))))

            ;; Unknown bare-ident — assume it's a FORMAT shortcut we don't recognise
            (throw (ex-info (str "unknown COPY option: " k)
                            {:error :syntax-error :option k}))))

        :else
        ;; Anything else (e.g. trailing content) — stop, let the caller
        ;; surface a clean error if there's something we missed.
        [out t]))))

;; ----------------------------------------------------------------------------
;; Option-vec → options-map (with defaults + validation)
;; ----------------------------------------------------------------------------

(def ^:private pg-encoding-aliases
  "Common pg_dump-emitted encoding names that we treat as UTF-8.
   Datahike is UTF-8 internal; an explicit ENCODING option is
   accepted for round-trip compatibility but doesn't change behavior."
  #{"utf8" "utf-8" "unicode"})

(defn- normalize-format
  "Resolve the format from collected options:
     FORMAT 'text'/'csv'/'binary' (modern)
     bare CSV / BINARY keywords (legacy)
     defaults to text"
  [opts-vec]
  (let [from-format (some (fn [[k v]] (when (= "format" k) v)) opts-vec)
        bare-csv?    (some (fn [[k v]] (and (= "csv" k) (true? v))) opts-vec)
        bare-binary? (some (fn [[k v]] (and (= "binary" k) (true? v))) opts-vec)]
    (cond
      bare-binary?  :binary
      bare-csv?     :csv
      (#{"text" "TEXT"} from-format)     :text
      (#{"csv" "CSV"} from-format)       :csv
      (#{"binary" "BINARY"} from-format) :binary
      :else                              :text)))

(defn- normalize-header [opts-vec]
  (let [v (some (fn [[k v]] (when (= "header" k) v)) opts-vec)]
    (cond
      (= :match v) :match
      (true? v)    :true
      (false? v)   :false
      ;; HEADER 1 / HEADER 0 — integer is "skip N rows" in PG 18+,
      ;; treat 0 as :false, anything else as :true (we don't support
      ;; the multi-row form at this layer; documented limitation).
      (and (number? v) (zero? v)) :false
      (number? v)  :true
      :else        :false)))

(defn- normalize-force-set [opts-vec key-name]
  (let [v (some (fn [[k v]] (when (= key-name k) v)) opts-vec)]
    (cond
      (= :all v)        :all
      (sequential? v)   (into #{} v)
      :else             nil)))

(defn options->map
  "Translate the raw [[\"key\" value] ...] option list (from either
   paren or legacy parser) into a normalised map with defaults
   applied. Throws :feature-not-supported for FORMAT 'binary' and
   :syntax-error for unknown options."
  [opts-vec]
  (let [format (normalize-format opts-vec)
        _ (when (= :binary format)
            (throw (ex-info "COPY in BINARY format is not supported"
                            {:error :feature-not-supported
                             :feature "COPY BINARY"})))
        ;; Validate every option key is recognised, even if its
        ;; effect is a no-op. Catches typos like NULL_MARKER.
        known? #{"format" "delimiter" "null" "header" "quote" "escape"
                 "force_not_null" "force_null" "force_quote"
                 "encoding" "freeze" "default" "oids"
                 "csv" "binary"  ;; legacy bare keywords
                 "on_error" "log_verbosity"}
        _ (doseq [[k _] opts-vec]
            (when-not (known? k)
              (throw (ex-info (str "unknown COPY option: " k)
                              {:error :syntax-error :option k}))))
        ;; OIDS — legacy, removed in PG 12. Reject explicitly so users
        ;; don't get a silent wrong-shape parse.
        _ (when (some (fn [[k _]] (= "oids" k)) opts-vec)
            (throw (ex-info "OIDS is not supported (removed in PostgreSQL 12)"
                            {:error :feature-not-supported :feature "COPY OIDS"})))
        delim (or (some (fn [[k v]] (when (= "delimiter" k) v)) opts-vec)
                  (case format :csv "," :text "\t"))
        null-marker (or (some (fn [[k v]] (when (= "null" k) v)) opts-vec)
                        (case format :csv "" :text "\\N"))
        quote-char (or (some (fn [[k v]] (when (= "quote" k) v)) opts-vec)
                       "\"")
        escape-char (or (some (fn [[k v]] (when (= "escape" k) v)) opts-vec)
                        ;; default = quote char
                        quote-char)
        encoding (some (fn [[k v]] (when (= "encoding" k) v)) opts-vec)
        freeze? (some (fn [[k v]] (when (= "freeze" k) (boolean v))) opts-vec)
        default-marker (some (fn [[k v]] (when (= "default" k) v)) opts-vec)]
    {:format         format
     :delimiter      delim
     :null-marker    null-marker
     :quote          quote-char
     :escape         escape-char
     :header         (normalize-header opts-vec)
     :force-not-null (normalize-force-set opts-vec "force_not_null")
     :force-null     (normalize-force-set opts-vec "force_null")
     :force-quote    (normalize-force-set opts-vec "force_quote")
     :encoding       encoding
     :freeze?        (boolean freeze?)
     :default-marker default-marker}))

;; ----------------------------------------------------------------------------
;; Top-level parser
;; ----------------------------------------------------------------------------

(defn parse-copy-from-stdin
  "Parse a tokenised `COPY ... FROM STDIN` statement.

   Returns:
     {:db-name nil       ;; for parity with database.clj parse shape
      :ns lowercase-string-or-nil
      :table original-case-string
      :columns [lowercase-string ...] | nil
      :options normalised-options-map}

   Throws ex-info with `:error :syntax-error` on malformed input,
   or `:feature-not-supported` for COPY BINARY / OIDS."
  [toks]
  (let [[c1 c2 & rest1] toks]
    (when-not (ident-eq? c1 "copy")
      (throw (ex-info "not a COPY statement"
                      {:error :syntax-error :got [c1 c2]})))
    ;; Reject COPY (query) TO stdout shape early — only FROM stdin in tier 2.
    (when (= :lparen (first c2))
      (throw (ex-info "COPY ( query ) TO/FROM not supported in this version"
                      {:error :feature-not-supported
                       :feature "COPY ( query ) ..."})))
    (when-not (ident-token? c2)
      (throw (ex-info "expected table name after COPY"
                      {:error :syntax-error :got c2})))
    (let [[table-info t-after-name] (consume-table-name (cons c2 rest1))
          [columns t-after-cols] (consume-column-list t-after-name)
          [target t-after-target] (consume-from-target t-after-cols :from)
          _ (when-not (= :stdin target)
              (throw (ex-info (str "only COPY FROM STDIN is supported; got: "
                                   (pr-str target))
                              {:error :feature-not-supported
                               :feature "COPY FROM file/PROGRAM"})))
          ;; Optional WITH (or no WITH at all)
          t-after-with (if (ident-eq? (first t-after-target) "with")
                         (rest t-after-target)
                         t-after-target)
          ;; Modern paren form vs legacy keyword form
          [opts-vec _t-rest]
          (cond
            ;; No options at all
            (or (empty? t-after-with)
                (= :semicolon (ffirst t-after-with)))
            [[] t-after-with]

            (= :lparen (ffirst t-after-with))
            (consume-paren-option-list t-after-with)

            :else
            (consume-legacy-option-list t-after-with))]
      {:db-name nil
       :ns (:ns table-info)
       :table (:table table-info)
       :columns columns
       :options (options->map opts-vec)})))
