(ns datahike.pg.sql.template
  "Lexical INSERT-VALUES templater for the parse-sql fast path.

   Bulk-load workloads (`pg_dump` replay, ETL pipelines, log
   ingestion) repeatedly issue INSERT statements with the same
   structural shape but varying literal values. The full parse-sql
   pipeline — JSqlParser AST + pg-datahike SQL→Datalog translation
   — costs ~1 ms/call dominated by JSqlParser, so a 46k-row Pagila
   replay paid ~46 s on parsing alone.

   The trick: every Pagila-style row of one table has the *same*
   shape, just different values. If we replace the literal values
   with `?` placeholders, the templated SQL string is shared across
   all rows of that table — and our parse-sql LRU cache turns
   every-row-but-the-first into a sub-µs cache hit. We only need a
   fast lexical scan to extract literals; we don't need a real
   parser.

   This namespace exports:

     - `template-insert-sql`  — pure string transform; returns
                                `{:templated <sql> :literals [<tok>...]}`
                                or nil if the SQL doesn't match the
                                simple INSERT VALUES shape we know
                                how to template.
     - `parse-literal-token`  — best-guess Java type for one captured
                                literal (Long, Double, String, nil,
                                Boolean, …).
     - `typed-substitute`     — replace ParamRefs in a parsed result
                                with values coerced through
                                `coerce-insert-value` so types match
                                each column's `:db/valueType`.

   Anything outside the simple INSERT VALUES shape returns nil from
   `template-insert-sql` so the caller falls through to full
   parse-sql. Conservative: false negatives (missed templating)
   degrade gracefully to the existing path; we never produce wrong
   tx-data."
  (:require [clojure.string :as str]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.types :as types])
  (:import [java.util ArrayList HashMap]))

(set! *warn-on-reflection* true)

;; ============================================================================
;; Lexical scan helpers
;; ============================================================================

(defn- skip-ws
  "Advance past whitespace from position i."
  ^long [^String s ^long i]
  (loop [i i]
    (if (and (< i (.length s))
             (Character/isWhitespace (.charAt s i)))
      (recur (inc i)) i)))

(defn- skip-line-comment
  "If position i is the start of a `-- … <newline>` comment, return the
   position past the newline (or end of string). Otherwise return i."
  ^long [^String s ^long i]
  (let [n (.length s)]
    (if (and (< (inc i) n)
             (= (.charAt s i) \-)
             (= (.charAt s (inc i)) \-))
      (loop [j (+ i 2)]
        (cond
          (>= j n) j
          (= (.charAt s j) \newline) (inc j)
          :else (recur (inc j))))
      i)))

(defn- skip-block-comment
  "If position i is the start of `/* … */`, return the position past the
   closing `*/`. Otherwise return i."
  ^long [^String s ^long i]
  (let [n (.length s)]
    (if (and (< (inc i) n)
             (= (.charAt s i) \/)
             (= (.charAt s (inc i)) \*))
      (loop [j (+ i 2)]
        (cond
          (>= j n) j
          (and (= (.charAt s j) \*)
               (< (inc j) n)
               (= (.charAt s (inc j)) \/))
          (+ j 2)
          :else (recur (inc j))))
      i)))

(defn- skip-string-literal
  "If position i is the start of a `'…'` (or `N'…'`) string literal,
   return the position just past the closing quote. Doubled `''` inside
   the string is an escaped single-quote."
  ^long [^String s ^long i]
  (let [n (.length s)
        ;; allow leading N for national-character literal
        i (if (and (< (inc i) n)
                   (or (= (.charAt s i) \N) (= (.charAt s i) \n))
                   (= (.charAt s (inc i)) \'))
            (inc i) i)]
    (if (or (>= i n) (not= (.charAt s i) \'))
      i
      (loop [j (inc i)]
        (cond
          (>= j n) j
          (and (= (.charAt s j) \')
               (< (inc j) n)
               (= (.charAt s (inc j)) \'))
          (recur (+ j 2))
          (= (.charAt s j) \') (inc j)
          :else (recur (inc j)))))))

(defn- starts-keyword-at?
  "Case-insensitive keyword match at position i with a word-boundary at
   the end. Used to find the VALUES keyword without a regex pass."
  [^String s ^long i ^String kw]
  (let [klen (.length kw)
        slen (.length s)]
    (and (<= (+ i klen) slen)
         (.regionMatches s true i kw 0 klen)
         (or (= (+ i klen) slen)
             (let [c (.charAt s (+ i klen))]
               (not (or (Character/isLetterOrDigit c) (= c \_))))))))

(defn- find-values-start
  "Locate the first VALUES keyword outside of strings and comments.
   Returns the index just past `VALUES` (pointing at whitespace
   before the open paren), or -1 if not found."
  ^long [^String s]
  (let [n (.length s)]
    (loop [i 0]
      (cond
        (>= i n) -1

        ;; Quoted identifier ("foo") or string literal
        (= (.charAt s i) \') (recur (long (skip-string-literal s i)))
        (= (.charAt s i) \") (recur (long (loop [j (inc i)]
                                            (cond
                                              (>= j n) j
                                              (= (.charAt s j) \") (inc j)
                                              :else (recur (inc j))))))

        ;; Comments
        (and (= (.charAt s i) \-) (< (inc i) n) (= (.charAt s (inc i)) \-))
        (recur (long (skip-line-comment s i)))
        (and (= (.charAt s i) \/) (< (inc i) n) (= (.charAt s (inc i)) \*))
        (recur (long (skip-block-comment s i)))

        (starts-keyword-at? s i "VALUES") (+ i 6)
        :else (recur (inc i))))))

;; ============================================================================
;; Token reading + classification
;; ============================================================================

(defn- read-token
  "From position i (just past whitespace, NOT past an opening paren),
   read one VALUES-list token ending at the next top-level `,` or `)`.
   Skips nested parens, square brackets (so ARRAY[1,2,3] stays one
   token), and string literals. Returns `[token-string end-index]`
   where end-index points at the terminating `,` or `)` (or end of
   string)."
  [^String s ^long i]
  (let [n (.length s)
        sb (StringBuilder.)]
    (loop [j i depth 0 in-str? false]
      (cond
        (>= j n) [(.toString sb) j]

        in-str?
        (let [c (.charAt s j)]
          (.append sb c)
          (cond
            ;; Doubled '' inside a string is an escaped single quote.
            (and (= c \') (< (inc j) n) (= (.charAt s (inc j)) \'))
            (do (.append sb \') (recur (+ j 2) depth true))
            (= c \') (recur (inc j) depth false)
            :else (recur (inc j) depth true)))

        (and (zero? depth) (or (= (.charAt s j) \,) (= (.charAt s j) \))))
        [(.toString sb) j]

        :else
        (let [c (.charAt s j)]
          (.append sb c)
          (cond
            (= c \') (recur (inc j) depth true)
            ;; Treat parens and square brackets uniformly. Bare `[…]`
            ;; appears in ARRAY[1,2,3] and subscript expressions —
            ;; their commas must not terminate the outer token.
            (or (= c \() (= c \[)) (recur (inc j) (inc depth) false)
            (or (= c \)) (= c \])) (recur (inc j) (dec depth) false)
            :else    (recur (inc j) depth false)))))))

;; A token qualifies as a templatable literal if it's purely a value
;; expression with no embedded function call or operator that the
;; downstream translator would interpret beyond a static value.

(def ^:private numeric-pattern
  ;; -?<digits>[.<digits>][e±<digits>] — covers ints, decimals, scientific.
  #"-?\d+(?:\.\d+)?(?:[eE][+\-]?\d+)?")

(def ^:private string-pattern
  ;; '<chars-or-doubled-quote>' optionally followed by an explicit cast
  ;;   — ::ident[.ident][(N[,M])]
  ;; National-char prefix N'…' is also accepted.
  #"[Nn]?'(?:''|[^'])*'(?:\s*::\s*[a-zA-Z_][\w\.]*(?:\s*\(\s*\d+(?:\s*,\s*\d+)?\s*\))?)?")

(defn- regclass-cast?
  "Detect `'…'::regclass` and the symmetric `::regtype/::regnamespace/…`
   shapes. The full parser routes these through `apply-sql-cast` to
   resolve a real OID; the lexical templater would just unwrap the
   string and lose the cast — diverging from the slow path. Bail
   on these so the slow path runs (rare in INSERT VALUES; common in
   pg_dump's DEFAULT clauses but those don't reach the templater)."
  [^String tok]
  (boolean (re-find #"::\s*reg[a-z]+" (str/lower-case tok))))

(defn- token-kind
  "Classify a trimmed token. `:literal` means we'll emit a `?` and
   capture the raw token. `:nonliteral` means we leave the token
   alone in the templated SQL — the parser handles function calls,
   bare identifiers like DEFAULT, ARRAY[...], subqueries, etc."
  [^String tok]
  (let [t (str/trim tok)
        lc (str/lower-case t)]
    (cond
      (empty? t) :nonliteral
      (regclass-cast? t) :nonliteral
      (re-matches numeric-pattern t) :literal
      (#{"null" "true" "false"} lc) :literal
      (re-matches string-pattern t) :literal
      :else :nonliteral)))

(defn- template-values-row
  "Walk the contents of a single VALUES (...) row. Caller positions
   `i` just after the opening paren. For each comma-separated token,
   if it's a templatable literal, emit `?` and capture the raw token;
   otherwise pass it through unchanged. Returns
   `[templated-row-content end-index]` where end-index points just
   past the closing `)`."
  [^String s ^long i ^ArrayList lits-out]
  (let [n (.length s)
        sb (StringBuilder.)]
    (loop [j i first? true]
      (let [j (long (skip-ws s j))]
        (cond
          (>= j n) [(.toString sb) j]
          (= (.charAt s j) \)) [(.toString sb) (inc j)]
          :else
          (let [[tok end] (read-token s j)
                end (long end)
                kind (token-kind tok)]
            (when-not first? (.append sb ", "))
            (case kind
              :literal (do (.append sb \?) (.add lits-out (str/trim tok)))
              :nonliteral (.append sb tok))
            (cond
              (>= end n) [(.toString sb) end]
              (= (.charAt s end) \,) (recur (inc end) false)
              (= (.charAt s end) \)) [(.toString sb) (inc end)]
              :else [(.toString sb) end])))))))

;; ============================================================================
;; Public API
;; ============================================================================

(defn- has-on-conflict?
  "True if the SQL contains an ON CONFLICT clause outside string
   literals or comments. ON CONFLICT routes through translate-insert's
   alternate `:db.fn/call`, which maintains a `:row-refs` atom that
   RETURNING reads back after the transact. That atom is per-parse
   mutable state the templated path — whose whole point is reusing one
   cached parsed map across calls — can't share safely (`cacheable-parse?`
   excludes `:row-refs` for the same reason). Bail and use full parse-sql
   for these. (The row maps themselves ARE reachable: they travel as
   `:db.fn/call` args, not closure captures.)"
  [^String s]
  (let [n (.length s)]
    (loop [i 0]
      (cond
        (>= i n) false
        (= (.charAt s i) \') (recur (long (skip-string-literal s i)))
        (and (= (.charAt s i) \-) (< (inc i) n) (= (.charAt s (inc i)) \-))
        (recur (long (skip-line-comment s i)))
        (and (= (.charAt s i) \/) (< (inc i) n) (= (.charAt s (inc i)) \*))
        (recur (long (skip-block-comment s i)))
        (and (starts-keyword-at? s i "ON")
             (let [j (skip-ws s (+ i 2))]
               (and (< j n) (starts-keyword-at? s j "CONFLICT"))))
        true
        :else (recur (inc i))))))

(defn template-insert-sql
  "Lexically transform an INSERT statement of the shape

     INSERT INTO t [( cols )] VALUES (lit, …) [, (lit, …)] [tail]

   into a templated form with `?` placeholders, capturing the
   original literals in declaration order. Returns

     {:templated <sql>  :literals [<token>, ...]}

   on success, or nil if the SQL doesn't match the simple shape (no
   VALUES keyword, ON CONFLICT clause, malformed paren structure,
   etc.). The caller should fall through to the full parser on nil.

   Tail content (e.g. `RETURNING …`) is preserved verbatim — we
   stop templating once we run out of `(…)` tuples and copy the
   rest of the string as-is."
  [^String sql]
  (try
    (when-not (has-on-conflict? sql)
      (let [v-end (find-values-start sql)]
        (when (>= v-end 0)
          (let [n (.length sql)
                sb (StringBuilder. ^String (subs sql 0 v-end))
                lits (ArrayList.)]
            (loop [i (long (skip-ws sql v-end)) first-row? true]
              (cond
                (>= i n)
                {:templated (.toString sb) :literals (vec lits)}

                (= (.charAt sql i) \()
                (let [_ (when-not first-row? (.append sb ", "))
                      _ (.append sb "(")
                      [row-content end] (template-values-row sql (inc i) lits)
                      _ (.append sb row-content)
                      _ (.append sb ")")
                      j (long (skip-ws sql end))]
                  (cond
                    (>= j n)
                    {:templated (.toString sb) :literals (vec lits)}

                    (= (.charAt sql j) \,)
                    (recur (long (skip-ws sql (inc j))) false)

                  ;; Not a comma — anything else (RETURNING, ON CONFLICT,
                  ;; trailing `;`, whitespace+EOL, …) is the tail. Emit
                  ;; verbatim and stop.
                    :else
                    (do (.append sb " ")
                        (.append sb (subs sql j))
                        {:templated (.toString sb) :literals (vec lits)})))

              ;; We expected a `(` to start the next row but found
              ;; something else (most often this is INSERT … SELECT …
              ;; without a VALUES clause that nonetheless matched the
              ;; literal "VALUES" keyword inside a comment or quoted
              ;; identifier — find-values-start already filters those,
              ;; but bail conservatively).
                :else nil))))))
    (catch Throwable _ nil)))

(def ^:private templater-fail (Object.))

(defn parse-literal-token
  "Convert a captured literal token to a 'best-guess' Java value:
     - numeric tokens → Long or Double
     - 'string'[::cast] → String (cast suffix discarded; the
       templated parse already encoded the cast structurally, and
       the typed-substitute pass below pipes the value through
       `coerce-insert-value` against the column's :db/valueType)
     - NULL / TRUE / FALSE → nil / true / false
     - anything else → a sentinel that signals the caller to fall
       back to full parse-sql.

   Returns the sentinel rather than throwing so a single odd token
   in a long INSERT cleanly bails to the slow path instead of
   crashing the whole parse."
  [^String tok]
  (try
    (let [t (str/trim tok)
          lc (str/lower-case t)]
      (cond
        (= lc "null") nil
        (= lc "true") true
        (= lc "false") false
        (re-matches #"-?\d+" t) (Long/parseLong t)
        ;; numeric, not float8 -- see types/decimal-literal. The
        ;; column's :db/valueType still governs what is finally stored;
        ;; coerce-insert-value narrows a numeric to a float8 column.
        (re-matches #"-?\d+\.\d+([eE][+\-]?\d+)?" t) (types/decimal-literal t (Double/parseDouble t))
        (re-matches #"-?\d+[eE][+\-]?\d+" t) (types/decimal-literal t (Double/parseDouble t))

        (or (.startsWith t "'") (.startsWith t "n'") (.startsWith t "N'"))
        ;; Strip the optional N prefix, then unwrap '...' (handling ''
        ;; doubled-quote escapes). Discard any ::cast suffix; coerce-
        ;; insert-value handles type alignment downstream from the
        ;; column's :db/valueType.
        ;;
        ;; PG quirk: `N'foo '` is typed `character` (CHAR) and the
        ;; CHAR→VARCHAR coerce strips trailing blanks. The full-parse
        ;; path mirrors this in `expr/string-value-text`; we replicate
        ;; here so the templated path produces the same value real-PG
        ;; would store. Plain `'foo '` keeps its trailing space.
        (let [n-prefixed? (or (.startsWith t "n'") (.startsWith t "N'"))
              t (if n-prefixed? (subs t 1) t)
              ;; find closing ' (skipping doubled '')
              close (loop [i 1 n (.length t)]
                      (cond
                        (>= i n) -1
                        (and (= (.charAt t i) \')
                             (< (inc i) n)
                             (= (.charAt t (inc i)) \'))
                        (recur (+ i 2) n)
                        (= (.charAt t i) \') i
                        :else (recur (inc i) n)))]
          (if (>= close 0)
            (let [unwrapped (-> (subs t 1 close) (str/replace "''" "'"))]
              (if n-prefixed?
                (str/replace unwrapped #" +$" "")
                unwrapped))
            templater-fail))

        :else templater-fail))
    (catch Throwable _ templater-fail)))

(defn templater-fail?
  "True if `parse-literal-token` returned its bail-out sentinel.
   The integration layer treats this as 'fall through to full
   parse-sql for this statement'."
  [v]
  (identical? v templater-fail))

(defn- collect-attr-by-idx
  "Walk parsed.tx-data: for each `{:idx N}` ParamRef found as a value
   under attribute key A inside an entity map, build `{N → A}`.
   Used by `typed-substitute` to coerce literals to the right
   `:db/valueType` per column.

   Walks into vectors, lists, and the `:db.fn/call` second-arg form.
   Records (defrecord ParamRef) satisfy `map?` but we explicitly
   stop at them via the param-ref? check."
  [parsed]
  (let [m (HashMap.)]
    (letfn [(walk [v]
              (cond
                (params/param-ref? v) nil
                (map? v)
                (doseq [[k val] v]
                  (cond
                    (params/param-ref? val) (.put m (:idx val) k)
                    (or (map? val) (sequential? val)) (walk val)))
                (sequential? v) (run! walk v)))]
      (walk (:tx-data parsed)))
    (into {} m)))

(defn- refresh-tempids
  "Walk a substituted tx-data and replace each entity-map's :db/id
   with a fresh gensym. The cached placeholder-shape parsed map has
   fixed tempid strings from translate-time; reusing them across
   Bind/Execute cycles (or across batched rows in one Sync group)
   makes Datahike collapse entity-maps that share a tempid. Mints
   fresh strings per substitute pass so every commit sees unique
   tempids."
  [tx-data]
  (let [renames (java.util.HashMap.)
        rename (fn [old]
                 (or (.get renames old)
                     (let [new (str (gensym "tem-"))]
                       (.put renames old new)
                       new)))
        walk (fn walk [v]
               (cond
                 (map? v)
                 (if-let [old (:db/id v)]
                   (assoc v :db/id (cond-> old (string? old) rename))
                   v)
                 (vector? v) (mapv walk v)
                 (seq? v) (map walk v)
                 :else v))]
    (walk tx-data)))

(defn typed-substitute
  "Replace ParamRefs in `parsed.tx-data` with bound values from
   `literals`, coercing each via `stmt/coerce-insert-value` against
   the column's `:db/valueType` so string→instant, string→long,
   etc. land at the same shape the literal-SQL parse would have
   produced.

   Returns nil if any literal can't be parsed cleanly — the caller
   uses that as the signal to fall through to full parse-sql.
   `literals` is a 0-indexed vector of captured token strings (as
   returned by `template-insert-sql`); ParamRef N pulls
   `(parse-literal-token (literals (dec N)))` and runs coercion.

   Also rewrites every entity-map's :db/id with a fresh gensym so
   the cached parsed map (shared across calls) doesn't hand
   Datahike colliding tempids when consecutive Bind/Execute cycles
   land in the same wire-layer batch."
  [parsed literals schema]
  (try
    (let [idx->attr (collect-attr-by-idx parsed)
          ;; First pass: parse each literal, bail out if any of them
          ;; needs the slow path. This avoids a half-substituted
          ;; tx-data leaking templater-fail sentinels into d/transact.
          raws (mapv parse-literal-token literals)]
      (when-not (some templater-fail? raws)
        (let [bound (mapv (fn [i raw]
                            (let [attr (get idx->attr (inc i))]
                              (if attr
                                (or (stmt/coerce-insert-value raw attr schema) raw)
                                raw)))
                          (range) raws)]
          (-> parsed
              (update :tx-data params/substitute-params bound)
              (update :tx-data refresh-tempids)))))
    (catch Throwable _ nil)))

;; ============================================================================
;; General number-literal parameterization (simple-protocol plan stability)
;; ============================================================================
;;
;; Simple-protocol clients interpolate literals, so every statement is a
;; novel string: JSqlParser, the parse-sql result cache, datalog's
;; memoized query parse AND the planner's clause-keyed plan cache all
;; miss on every call — a pgbench tpcb transaction re-ran the whole
;; front end per statement (~13% jsqlparser + ~11% replanning of server
;; CPU). Rewriting bare number literals to $N parameters makes one
;; SHAPE per statement family: every layer keys on the templated string
;; or the var-form clauses and hits.
;;
;; v1 scope — NUMBERS ONLY, conservatively:
;;   - bare integer/decimal literals, including a unary +/- folded into
;;     the captured value when the sign sits in operand position;
;;   - skipped after LIMIT / OFFSET / FETCH / TOP (translation needs
;;     compile-time numbers there) and before `::` casts (constant-fold
;;     branches expect literals);
;;   - strings stay inline: their translation is column-type-directed
;;     (coerce-unknown-literal), which a late-bound parameter can't see.
;; Callers MUST fall back to parsing the original SQL if the templated
;; parse errors.

(def ^:private no-param-after
  #{"limit" "offset" "fetch" "top" "interval"})

(def ^:private no-template-idents
  "Statement-disqualifying identifiers: constructs that consume literal
   values at PARSE/translate time, where a late-bound $N parameter
   either leaks as the literal string \"$N\" (set-returning functions
   materialized into the speculative db, table-free literal rows) or
   breaks clause scoping (BETWEEN/IN expansions, HAVING predicates).
   Conservative v1 — each of these can graduate off the list once its
   translation handles ?pN vars."
  #{"having" "between" "unnest" "generate_series" "array" "values"
    "in" "any" "all" "case" "coalesce" "nullif" "distinct" "union"
    "intersect" "except" "group"})

(defn parameterize-numbers
  "Rewrite bare number literals in `sql` to $N placeholders. Returns
   {:sql <templated> :params [v ...]} (params 0-indexed, Long/Double)
   or nil when nothing was parameterized or the statement kind is out
   of scope (only SELECT/UPDATE/DELETE; INSERT has its own templater)."
  [^String sql]
  (try
    (let [toks (vec (cls/tokenize-all sql))
          kind (some-> (first toks) :text str/lower-case)]
      (when (and (contains? #{"select" "update" "delete" "with"} kind)
                 (not-any? #(and (= :ident (:type %))
                                 (contains? no-template-idents
                                            (str/lower-case (:text %))))
                           toks))
        (let [sb (StringBuilder.)
              params (java.util.ArrayList.)
              n (count toks)]
          ;; A bare integer in ORDER BY / GROUP BY is a 1-based ORDINAL
          ;; into the select list, not a value. Templating it to $N
          ;; destroys that: `ORDER BY $1` sorts every row by the same
          ;; constant, which is what PostgreSQL does with a real
          ;; parameter there too — so `ORDER BY 2` silently returned
          ;; rows in scan order and `ORDER BY 3` never raised 42P10.
          ;; (GROUP BY escaped only because "group" is in
          ;; no-template-idents and disqualifies the whole statement.)
          ;;
          ;; `ordinal-pos?` marks the positions where a number would BE
          ;; the whole sort item: right after `BY`, and right after each
          ;; comma in the list. Anywhere else — `ORDER BY sal + 2` — the
          ;; number is an ordinary value and still templates.
          (loop [i 0 last-end 0 skip-next-number? false fn-depth 0 paren-stack ()
                 ordinal-pos? false in-sort-list? false]
            (if (= i n)
              (do (.append sb (subs sql last-end))
                  (when (pos? (.size params))
                    {:sql (.toString sb) :params (vec params)}))
              (let [{:keys [type text pos end] :as tok} (nth toks i)
                    ;; $N placeholders already present → mixing ours in
                    ;; would renumber theirs; bail entirely.
                    bail? (= :param type)
                    ;; Track whether we're inside a FUNCTION-CALL argument
                    ;; list: `ident (` opens one. Literals there feed
                    ;; translate-time machinery (pg_typeof's type answer,
                    ;; SUBSTR positions, casts) and must stay inline.
                    open? (and (= :punct type) (= "(" text))
                    close? (and (= :punct type) (= ")" text))
                    after-ident? (= :ident (:type (nth toks (dec i) nil)))
                    [fn-depth' paren-stack']
                    (cond
                      open? [(if after-ident? (inc fn-depth) fn-depth)
                             (cons (if after-ident? :fn :plain) paren-stack)]
                      close? [(if (= :fn (first paren-stack))
                                (dec fn-depth) fn-depth)
                              (rest paren-stack)]
                      :else [fn-depth paren-stack])]
                (cond
                  bail? nil
                  (and (= :number type) (not skip-next-number?)
                       (not ordinal-pos?)
                       (zero? fn-depth)
                       ;; A number after a json operator is an ARRAY INDEX
                       ;; or a path step, not a value: `d->'a'->>1`.
                       ;; Templating it to $1 turns the chain's ident into
                       ;; a ParamRef, and the extraction silently answers
                       ;; NULL — the query is well-formed, just wrong.
                       (not (contains? #{"->" "->>" "#>" "#>>"}
                                       (:text (nth toks (dec i) nil))))
                       ;; `1::bigint` — leave for the constant-fold cast
                       (not (= "::" (:text (nth toks (inc i) nil)))))
                  (let [prev (nth toks (dec i) nil)
                        prev2 (nth toks (- i 2) nil)
                        ;; unary sign: +/- whose own predecessor cannot
                        ;; end an expression (operator, '(', ',', or a
                        ;; keyword) — fold it into the value.
                        unary? (and prev (= :op (:type prev))
                                    (#{"-" "+"} (:text prev))
                                    (or (nil? prev2)
                                        (#{:op :punct} (:type prev2))
                                        (and (= :punct (:type prev2))
                                             (#{"(" ","} (:text prev2)))
                                        (= :ident (:type prev2))))
                        ;; ident before a sign only makes it unary after
                        ;; keywords like WHERE/AND/VALUES — after a column
                        ;; name it's binary. Conservative: treat ident as
                        ;; binary (leave the sign in place, param the bare
                        ;; number) EXCEPT we already required :op/:punct.
                        unary? (and unary?
                                    (not (= :ident (:type prev2))))
                        start (if unary? (:pos prev) pos)
                        raw (subs sql (if unary? (:pos prev) pos) end)
                        ;; A decimal literal is `numeric` in PostgreSQL,
                        ;; not float8. Parameterizing it as a Double
                        ;; discarded both the exactness and the scale
                        ;; before any translator could see the token --
                        ;; `SELECT 1.10` came back 1.1 and `0.1 + 0.2`
                        ;; came back 0.30000000000000004, no matter what
                        ;; the literal paths downstream did.
                        v (if (re-find #"[.eE]" text)
                            (types/decimal-literal raw (Double/parseDouble raw))
                            (Long/parseLong raw))]
                    (.append sb (subs sql last-end start))
                    (.add params v)
                    (.append sb (str "$" (.size params)))
                    (recur (inc i) (long end) false fn-depth' paren-stack'
                           false in-sort-list?))
                  :else
                  (let [low (when (= :ident type) (str/lower-case text))
                        prev-low (let [pt (nth toks (dec i) nil)]
                                   (when (= :ident (:type pt))
                                     (str/lower-case (:text pt))))
                        by? (and (= low "by") (contains? #{"order" "group"} prev-low))
                        ;; LIMIT / OFFSET / FETCH end the sort list; so
                        ;; does closing the parens of a subquery.
                        ends? (or (contains? #{"limit" "offset" "fetch"} low)
                                  (and (= :punct type) (= ")" text)))
                        in-sort-list?' (cond by? true ends? false
                                             :else in-sort-list?)]
                    (recur (inc i) (long last-end)
                           (and (= :ident type)
                                (contains? no-param-after low))
                           fn-depth' paren-stack'
                           (or by?
                               (and in-sort-list?'
                                    (= :punct type) (= "," text)))
                           in-sort-list?')))))))))
    (catch Throwable _ nil)))
