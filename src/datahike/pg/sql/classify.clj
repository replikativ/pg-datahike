(ns datahike.pg.sql.classify
  "Structural SQL classifier — routes statements to the right handler
   before JSqlParser sees them.

   Kills the regex sprawl across `system-query?`, `parse-sql`'s CT6
   guard, and the arg-extraction regexes (savepoint names, advisory-
   lock keys, temporal SET values) by feeding those sites a real
   tokenizer.

   The tokenizer handles enough of PG lexical syntax to classify
   correctly in the face of keyword-inside-a-string, keyword-inside-
   a-comment, dollar-quoted strings, and case mix. It does NOT parse
   expressions — it yields a flat token stream for a keyword-dispatch
   classifier.

   Classifier output:
     {:kind        <statement-kind keyword>
      :name        <savepoint/variable name if relevant>
      :args        <vector of literal args — advisory keys, pg_sleep dur>
      :var         <SET/RESET/SHOW variable name>
      :value       <SET value (string) when captured>
      :reject-kind <opt-out knob key>
      :tag         <synthetic command tag on silent-accept>}

   :kind :generic-sql means 'pass to JSqlParser unchanged'.

   Non-goals: full PG lexer, expression parsing. The token-driven
   preprocess-sql rewriter lives in datahike.pg.sql.rewrite and consumes
   this tokenizer's output."
  (:require [clojure.string :as str]))

;; ============================================================================
;; Tokenizer
;; ============================================================================
;;
;; Token shape: {:type :keyword :text "..." :pos N  (optional :idx :value)}
;;
;; Types:
;;   :ident   bare identifier (keywords included — classifier matches :text)
;;   :quoted  quoted "identifier"
;;   :string  'literal' or E'…' or $tag$…$tag$  (:value holds decoded body)
;;   :number  42, 3.14, 1e10
;;   :op      :: = <> != <= >= < > + - * / % || @> <@ ? ?& ?| && ~ ~* !~ !~* #
;;   :punct   ( ) , ; . [ ] { }
;;   :param   $N (with :idx) or ? (JDBC — :idx nil)

(def ^:private op-chars #{\+ \- \* \/ \% \< \> \= \! \| \& \@ \? \# \~ \:})

(def ^:private punct-chars #{\( \) \, \; \. \[ \] \{ \}})

(defn- identifier-start? [^Character c]
  (or (Character/isLetter c) (= c \_)))

(defn- identifier-part? [^Character c]
  (or (Character/isLetterOrDigit c) (= c \_) (= c \$)))

(defn- digit? [^Character c] (Character/isDigit c))

(defn- skip-line-comment ^long [^String sql ^long pos ^long len]
  ;; pos at the first `-` of `--`. Return position past the terminating
  ;; newline (or at end-of-input).
  (loop [q (+ pos 2)]
    (cond
      (>= q len) q
      (= \newline (.charAt sql q)) (unchecked-inc q)
      :else (recur (unchecked-inc q)))))

(defn- skip-block-comment ^long [^String sql ^long pos ^long len]
  ;; pos at the `/` of `/*`. Nested block comments are a PG extension.
  (loop [q (+ pos 2) depth 1]
    (cond
      (>= q len) q
      (and (< (+ q 1) len)
           (= \/ (.charAt sql q))
           (= \* (.charAt sql (+ q 1))))
      (recur (+ q 2) (inc depth))
      (and (< (+ q 1) len)
           (= \* (.charAt sql q))
           (= \/ (.charAt sql (+ q 1))))
      (if (= depth 1)
        (+ q 2)
        (recur (+ q 2) (dec depth)))
      :else (recur (unchecked-inc q) depth))))

(defn- skip-whitespace-and-comments ^long [^String sql ^long pos ^long len]
  (loop [p pos]
    (cond
      (>= p len) p
      (Character/isWhitespace (.charAt sql p))
      (recur (unchecked-inc p))
      ;; -- line comment — consume, then re-loop for following ws/comments.
      (and (< (+ p 1) len)
           (= \- (.charAt sql p))
           (= \- (.charAt sql (+ p 1))))
      (recur (skip-line-comment sql p len))
      ;; /* ... */ (nested)
      (and (< (+ p 1) len)
           (= \/ (.charAt sql p))
           (= \* (.charAt sql (+ p 1))))
      (recur (skip-block-comment sql p len))
      :else p)))

(defn- read-single-quoted
  "Read a single-quoted string starting at pos (which points to the
   opening '). Returns [token end-pos]. '' is an escaped ' inside."
  [^String sql ^long pos ^long len]
  (let [sb (StringBuilder.)]
    (loop [p (unchecked-inc pos)]
      (cond
        (>= p len)
        [{:type :string :text (subs sql pos p) :pos pos :value (.toString sb)} p]
        (= \' (.charAt sql p))
        (if (and (< (unchecked-inc p) len) (= \' (.charAt sql (unchecked-inc p))))
          (do (.append sb \') (recur (+ p 2)))
          [{:type :string :text (subs sql pos (unchecked-inc p))
            :pos pos :value (.toString sb)}
           (unchecked-inc p)])
        :else
        (do (.append sb (.charAt sql p))
            (recur (unchecked-inc p)))))))

(defn- read-e-quoted
  "E'...' style C-escape string. We don't fully decode the escapes; we
   just track the boundaries so classification isn't fooled by keywords
   inside."
  [^String sql ^long pos ^long len]
  ;; pos points to 'E'; the quote is at pos+1.
  (let [q-start (unchecked-inc pos)
        sb (StringBuilder.)]
    (loop [p (unchecked-inc q-start)]
      (cond
        (>= p len)
        [{:type :string :text (subs sql pos p) :pos pos :value (.toString sb)} p]
        (and (= \\ (.charAt sql p)) (< (unchecked-inc p) len))
        (do (.append sb (.charAt sql (unchecked-inc p)))
            (recur (+ p 2)))
        (= \' (.charAt sql p))
        (if (and (< (unchecked-inc p) len) (= \' (.charAt sql (unchecked-inc p))))
          (do (.append sb \') (recur (+ p 2)))
          [{:type :string :text (subs sql pos (unchecked-inc p))
            :pos pos :value (.toString sb)}
           (unchecked-inc p)])
        :else (do (.append sb (.charAt sql p)) (recur (unchecked-inc p)))))))

(defn- read-dollar-quoted
  "$tag$...$tag$ where tag is zero or more word chars. If tag doesn't
   close, treat as end-of-input (best effort — matches PG behavior of
   raising a lex error, which we don't need to propagate here)."
  [^String sql ^long pos ^long len]
  ;; pos at $.
  (let [tag-end (loop [p (unchecked-inc pos)]
                  (cond (>= p len) nil
                        (= \$ (.charAt sql p)) p
                        (or (Character/isLetterOrDigit (.charAt sql p))
                            (= \_ (.charAt sql p)))
                        (recur (unchecked-inc p))
                        :else nil))]
    (if (nil? tag-end)
      ;; Not a valid dollar-quote opener — fall back to treating as op.
      [{:type :op :text "$" :pos pos} (unchecked-inc pos)]
      (let [tag (subs sql pos (unchecked-inc tag-end))
            body-start (unchecked-inc tag-end)
            close-idx (str/index-of sql tag body-start)]
        (if close-idx
          [{:type :string
            :text (subs sql pos (+ close-idx (count tag)))
            :pos pos
            :value (subs sql body-start close-idx)}
           (+ close-idx (count tag))]
          [{:type :string :text (subs sql pos len) :pos pos
            :value (subs sql body-start len)}
           len])))))

(defn- read-quoted-ident
  "Read a \"quoted identifier\"."
  [^String sql ^long pos ^long len]
  (let [sb (StringBuilder.)]
    (loop [p (unchecked-inc pos)]
      (cond
        (>= p len)
        [{:type :quoted :text (subs sql pos p) :pos pos :value (.toString sb)} p]
        (= \" (.charAt sql p))
        (if (and (< (unchecked-inc p) len) (= \" (.charAt sql (unchecked-inc p))))
          (do (.append sb \") (recur (+ p 2)))
          [{:type :quoted :text (subs sql pos (unchecked-inc p))
            :pos pos :value (.toString sb)}
           (unchecked-inc p)])
        :else (do (.append sb (.charAt sql p)) (recur (unchecked-inc p)))))))

(defn- read-ident [^String sql ^long pos ^long len]
  (loop [p pos]
    (if (and (< p len) (identifier-part? (.charAt sql p)))
      (recur (unchecked-inc p))
      [{:type :ident :text (subs sql pos p) :pos pos} p])))

(defn- read-number [^String sql ^long pos ^long len]
  (loop [p pos
         seen-dot? false
         seen-e? false]
    (cond
      (>= p len) [{:type :number :text (subs sql pos p) :pos pos} p]
      (digit? (.charAt sql p))
      (recur (unchecked-inc p) seen-dot? seen-e?)
      (and (not seen-dot?) (= \. (.charAt sql p)))
      (recur (unchecked-inc p) true seen-e?)
      (and (not seen-e?) (or (= \e (.charAt sql p)) (= \E (.charAt sql p))))
      (let [nxt (unchecked-inc p)]
        (if (and (< nxt len)
                 (or (= \+ (.charAt sql nxt)) (= \- (.charAt sql nxt))))
          (recur (+ p 2) seen-dot? true)
          (recur nxt seen-dot? true)))
      :else [{:type :number :text (subs sql pos p) :pos pos} p])))

(defn- read-param [^String sql ^long pos ^long len]
  ;; pos at $
  (let [digits-end (loop [p (unchecked-inc pos)]
                     (if (and (< p len) (digit? (.charAt sql p)))
                       (recur (unchecked-inc p))
                       p))]
    (if (= digits-end (unchecked-inc pos))
      nil  ; $ not followed by digits — not a param
      [{:type :param
        :text (subs sql pos digits-end)
        :pos pos
        :idx (Long/parseLong (subs sql (unchecked-inc pos) digits-end))}
       digits-end])))

(defn- read-op
  "Read a sequence of operator chars as a single op token. Multi-char
   operators (::, <>, <=, >=, ||, @>, <@, ?|, ?&, !~, !~*, ~*) are
   captured by the greedy-consume-op-chars pattern; the classifier
   doesn't care about the exact op, only that something opaque is there."
  [^String sql ^long pos ^long len]
  (loop [p pos]
    (if (and (< p len) (contains? op-chars (.charAt sql p)))
      (recur (unchecked-inc p))
      [{:type :op :text (subs sql pos p) :pos pos} p])))

(defn- next-comment-token
  "Emit the next whitespace-bounded token that is a comment, or nil if
   the next non-whitespace char is not a comment opener. Position-only
   boundary recording; we don't bother capturing comment text since no
   caller inspects it."
  [^String sql ^long pos ^long len]
  (let [;; Advance over whitespace only.
        p (loop [q pos]
            (if (and (< q len) (Character/isWhitespace (.charAt sql q)))
              (recur (unchecked-inc q))
              q))]
    (cond
      (>= p len) nil
      (and (< (+ p 1) len) (= \- (.charAt sql p)) (= \- (.charAt sql (+ p 1))))
      (let [e (skip-line-comment sql p len)]
        [{:type :comment :text (subs sql p e) :pos p :end e} e])
      (and (< (+ p 1) len) (= \/ (.charAt sql p)) (= \* (.charAt sql (+ p 1))))
      (let [e (skip-block-comment sql p len)]
        [{:type :comment :text (subs sql p e) :pos p :end e} e])
      :else nil)))

(defn- with-end [[tok end]] [(assoc tok :end end) end])

(defn- next-token
  "Read the next meaningful token at pos. Returns [token end-pos] or
   nil at end of input. Skips whitespace and comments before reading.
   Every token carries both :pos (start) and :end (one past last) so
   callers can slice the original source for rewrites."
  [^String sql ^long pos ^long len]
  (let [p (skip-whitespace-and-comments sql pos len)]
    (when (< p len)
      (with-end
        (let [c (.charAt sql p)]
          (cond
          ;; Leading .digit → number (must come before punct check so `.5`
          ;; isn't consumed as the `.` punctuation).
            (and (= c \.)
                 (< (unchecked-inc p) len)
                 (digit? (.charAt sql (unchecked-inc p))))
            (read-number sql p len)

          ;; punctuation
            (contains? punct-chars c)
            [{:type :punct :text (str c) :pos p} (unchecked-inc p)]

          ;; ? JDBC param
            (= c \?)
          ;; disambiguate ?& ?| operators — if next char is & or |, treat as op
            (if (and (< (unchecked-inc p) len)
                     (let [n (.charAt sql (unchecked-inc p))]
                       (or (= n \&) (= n \|))))
              [{:type :op :text (subs sql p (+ p 2)) :pos p} (+ p 2)]
              [{:type :param :text "?" :pos p :idx nil} (unchecked-inc p)])

          ;; $-prefixed: could be $N param, $tag$ string, or bare $
            (= c \$)
            (cond
              (and (< (unchecked-inc p) len) (digit? (.charAt sql (unchecked-inc p))))
              (read-param sql p len)
              :else (read-dollar-quoted sql p len))

            (= c \')
            (read-single-quoted sql p len)

            (= c \")
            (read-quoted-ident sql p len)

          ;; E'...' C-escape string (only the leading E when followed by ')
            (and (or (= c \E) (= c \e))
                 (< (unchecked-inc p) len)
                 (= \' (.charAt sql (unchecked-inc p))))
            (read-e-quoted sql p len)

            (identifier-start? c)
            (read-ident sql p len)

            (digit? c)
            (read-number sql p len)

            (contains? op-chars c)
            (read-op sql p len)

          ;; Fallback: single-char punct-ish
            :else [{:type :op :text (str c) :pos p} (unchecked-inc p)]))))))

(defn tokenize-all
  "Lazy seq of tokens INCLUDING :comment tokens (line and block).
   Useful for source-rewriting callers that need to know comment
   spans so they don't excise into or across a commented region.
   Classification itself uses `tokenize`, which filters comments
   out for speed."
  [^String sql]
  (let [len (count sql)]
    (letfn [(step [pos]
              (lazy-seq
               (if-let [[tok end] (next-comment-token sql pos len)]
                 (cons tok (step end))
                 (when-let [[tok end] (next-token sql pos len)]
                   (cons tok (step end))))))]
      (step 0))))

(defn tokenize
  "Lazy seq of non-comment tokens. Skips whitespace and comments.
   Stops at end-of-input; does not attempt to recover from mid-string
   EOF (caller will see :generic-sql and JSqlParser will report the
   real syntax error)."
  [^String sql]
  (let [len (count sql)]
    (letfn [(step [pos]
              (lazy-seq
               (when-let [[tok end] (next-token sql pos len)]
                 (cons tok (step end)))))]
      (step 0))))

;; ============================================================================
;; Classifier
;; ============================================================================

(defn- ident-tok? [tok] (or (= :ident (:type tok)) (= :quoted (:type tok))))

(defn- kw=?
  "Case-insensitive keyword equality against a token's text. Only
   matches bare idents (quoted \"SELECT\" is an identifier, not the
   keyword SELECT — PG rule)."
  [tok ^String kw]
  (and tok
       (= :ident (:type tok))
       (= (str/lower-case (:text tok)) kw)))

(defn- fn-name=?
  "Like kw=? but also accepts PG-quoted identifiers, since
   `SELECT pg_notify(...)` and `SELECT \"pg_notify\"(...)` resolve to the
   same function in PG (function lookup case-folds regardless of
   quoting). Used inside `classify-system-call` because the entries
   there are PG function/schema names — clients like psycopg2's
   `SQL.identifier()` always emit the quoted form, and Odoo's bus relies
   on that path."
  [tok ^String fname]
  (and tok
       (case (:type tok)
         :ident  (= (str/lower-case (:text tok)) fname)
         :quoted (= (str/lower-case (or (:value tok) (:text tok))) fname)
         false)))

(defn- kw-in?
  "Matches a lowercase string against a set of kw names."
  [tok ^clojure.lang.IPersistentSet kws]
  (and tok
       (= :ident (:type tok))
       (contains? kws (str/lower-case (:text tok)))))

(defn- ident-text [tok]
  (when tok
    (case (:type tok)
      :ident (:text tok)
      :quoted (:value tok)
      nil)))

(defn- string-value [tok]
  (when (= :string (:type tok)) (:value tok)))

(defn- number-value [tok]
  (when (= :number (:type tok))
    (let [t (:text tok)]
      (if (or (str/includes? t ".")
              (str/includes? t "e")
              (str/includes? t "E"))
        (Double/parseDouble t)
        (Long/parseLong t)))))

(defn- skip-leading-paren
  "PG allows `(SELECT …)` as the top-level form. Classification should
   see the inner kind, so we strip a single leading `(` token."
  [toks]
  (if (and (seq toks) (= "(" (:text (first toks))))
    (rest toks)
    toks))

(defn- extract-fn-numeric-args
  "Walk inside the first parenthesized argument list and collect
   :number tokens as longs. Stops at the matching `)`. Handles a
   leading unary `-` before a number (tokenizer emits `-` and the
   number as separate tokens)."
  [toks]
  (loop [ts toks
         acc []
         depth 0
         neg? false]
    (cond
      (empty? ts) acc
      (= "(" (:text (first ts)))
      (recur (rest ts) acc (inc depth) false)
      (= ")" (:text (first ts)))
      (if (= 1 depth) acc
          (recur (rest ts) acc (dec depth) false))
      (and (pos? depth)
           (= :op (:type (first ts)))
           (= "-" (:text (first ts))))
      (recur (rest ts) acc depth (not neg?))
      (and (pos? depth) (= :number (:type (first ts))))
      (let [n (number-value (first ts))]
        (recur (rest ts) (conj acc (if neg? (- n) n)) depth false))
      :else (recur (rest ts) acc depth false))))

(defn- extract-fn-string-args
  "Same as extract-fn-numeric-args but returns string literal values
   (the :value of :string tokens, which has already been unescaped)."
  [toks]
  (loop [ts toks, acc [], depth 0]
    (cond
      (empty? ts) acc
      (= "(" (:text (first ts))) (recur (rest ts) acc (inc depth))
      (= ")" (:text (first ts))) (if (= 1 depth) acc (recur (rest ts) acc (dec depth)))
      (and (pos? depth) (= :string (:type (first ts))))
      (recur (rest ts) (conj acc (:value (first ts))) depth)
      :else (recur (rest ts) acc depth))))

(defn- sole-fn-projection?
  "Given the tokens that FOLLOW a leading function-name token in a SELECT
   projection (an optional balanced `(...)` arg list, an optional alias,
   then whatever else the statement contains), decide whether that
   function call is the SOLE select item with no further clauses. Returns
   false as soon as a paren-depth-0 comma or SQL clause keyword
   (FROM/WHERE/GROUP/…) appears — that means the SELECT has additional
   projection columns or a real query body and must flow through
   JSqlParser, not a single-value hijack. Guards e.g.
   `SELECT now() AS now, 1 AS num` from collapsing to a one-column
   `now()` result (the hijack would otherwise swallow `1 AS num`)."
  [toks]
  (let [clause #{"from" "where" "group" "having" "order"
                 "limit" "offset" "union" "except" "intersect"
                 "join" "window" "fetch" "for"}]
    (loop [ts toks, depth 0]
      (if (empty? ts)
        true
        (let [t (first ts), tx (:text t)]
          (cond
            (= "(" tx) (recur (rest ts) (inc depth))
            (= ")" tx) (recur (rest ts) (max 0 (dec depth)))
            (pos? depth) (recur (rest ts) depth)
            (= "," tx) false
            ;; A trailing cast (`now()::date`) changes the result type
            ;; and column name — the hijack handlers hardcode both, so
            ;; route through the translator (issue #13).
            (= "::" tx) false
            (kw-in? t clause) false
            :else (recur (rest ts) depth)))))))

(defn- classify-select
  "Classify a SELECT: either one of our hijack patterns, or
   {:kind :generic-sql} for anything that should flow through to
   JSqlParser. Called with the tokens AFTER the leading SELECT."
  [toks]
  ;; Function-name dispatch uses fn-name=?, which accepts both bare and
  ;; double-quoted identifiers — psycopg2's SQL.identifier() always
  ;; quotes the function name, so Odoo emits `SELECT "pg_notify"(...)`,
  ;; `SELECT "pg_advisory_lock"(...)`, etc. PG resolves these to the
  ;; same system functions as the unquoted form (function lookup
  ;; case-folds regardless of quoting), so we must too.
  (let [[t1 _t2 & _] (skip-leading-paren toks)
        ;; When SELECT is followed by `pg_catalog.` prefix, skip it so
        ;; `pg_catalog.current_database()` classifies the same as the
        ;; bare form. Same treatment for the `datahike.` prefix, which
        ;; namespaces our branching / versioning functions; below the
        ;; cond we route datahike.X specifically (so a bare `branches()`
        ;; or `current_branch()` without the prefix stays generic-SQL).
        [t1 rest-args dh?] (cond
                             (and (fn-name=? t1 "pg_catalog")
                                  (= "." (:text (second toks))))
                             [(nth toks 2 nil) (drop 3 toks) false]
                             (and (fn-name=? t1 "datahike")
                                  (= "." (:text (second toks))))
                             [(nth toks 2 nil) (drop 3 toks) true]
                             :else
                             [t1 (rest toks) false])
        ;; All branches below hijack a SELECT whose projection is a single
        ;; system/scalar function call (now(), version(), nextval(…), …).
        ;; They only stay correct when that call IS the whole projection —
        ;; otherwise extra columns / a FROM body get silently dropped. When
        ;; the function is not the sole select item, fall through to
        ;; JSqlParser, which handles arbitrary projections.
        sole? (sole-fn-projection? rest-args)]
    (cond
      (nil? t1) {:kind :generic-sql}
      (not sole?) {:kind :generic-sql}
      ;; datahike.* branching / versioning functions
      (and dh? (fn-name=? t1 "branches"))       {:kind :dh-branches}
      (and dh? (fn-name=? t1 "current_branch")) {:kind :dh-current-branch}
      (and dh? (fn-name=? t1 "commit_id"))      {:kind :dh-commit-id}
      (and dh? (fn-name=? t1 "parent_commits")) {:kind :dh-parent-commits}
      (and dh? (fn-name=? t1 "create_branch"))
      {:kind :dh-create-branch :args (extract-fn-string-args rest-args)}
      (and dh? (fn-name=? t1 "delete_branch"))
      {:kind :dh-delete-branch :args (extract-fn-string-args rest-args)}
      (fn-name=? t1 "version")       {:kind :version}
      (fn-name=? t1 "now")           {:kind :now}
      (fn-name=? t1 "current_schema") {:kind :current-schema}
      (fn-name=? t1 "current_database") {:kind :current-database}
      ;; SQL-spec equivalents — both are bare-keyword expressions, not
      ;; function calls. Tokeniser treats them as :ident, no parens.
      (fn-name=? t1 "current_catalog") {:kind :current-database}
      ;; pg_dump session-prelude function — sets a GUC. We don't honor
      ;; the side-effect (the GUC has no Datahike equivalent), but we
      ;; need to silently accept the call so the dump replays.
      ;; The 3-arg form is `set_config(name, value, is_local)`; we
      ;; accept any args.
      (fn-name=? t1 "set_config") {:kind :set-config}
      (fn-name=? t1 "pg_backend_pid") {:kind :pg-backend-pid}
      (fn-name=? t1 "txid_current")  {:kind :txid-current}
      (fn-name=? t1 "pg_sleep")
      {:kind :pg-sleep :args (extract-fn-numeric-args rest-args)}
      ;; pg_notify(channel, payload) — Odoo's bus uses this from a
      ;; post-commit hook (addons/bus/models/bus.py) on every model
      ;; write. We accept it as a void no-op: there is no LISTEN
      ;; delivery in pg-datahike, so the sender path is observably
      ;; equivalent to delivering to zero subscribers.
      (fn-name=? t1 "pg_notify")
      {:kind :pg-notify}
      (fn-name=? t1 "pg_advisory_lock")
      {:kind :advisory-lock :args (extract-fn-numeric-args rest-args)}
      (fn-name=? t1 "pg_try_advisory_lock")
      {:kind :try-advisory-lock :args (extract-fn-numeric-args rest-args)}
      (fn-name=? t1 "pg_advisory_xact_lock")
      {:kind :advisory-xact-lock :args (extract-fn-numeric-args rest-args)}
      (fn-name=? t1 "pg_try_advisory_xact_lock")
      {:kind :try-advisory-xact-lock :args (extract-fn-numeric-args rest-args)}
      (fn-name=? t1 "pg_advisory_unlock")
      {:kind :advisory-unlock :args (extract-fn-numeric-args rest-args)}
      (fn-name=? t1 "pg_advisory_unlock_all")
      {:kind :advisory-unlock-all}
      (fn-name=? t1 "nextval")
      {:kind :nextval :seq-name (first (extract-fn-string-args rest-args))}
      (fn-name=? t1 "currval")
      {:kind :currval :seq-name (first (extract-fn-string-args rest-args))}
      ;; lastval() takes no arguments — it reports the sequence most
      ;; recently advanced by nextval in THIS session.
      (fn-name=? t1 "lastval")
      {:kind :lastval}
      (fn-name=? t1 "setval")
      {:kind :setval
       :seq-name (first (extract-fn-string-args rest-args))
       :new-value (first (extract-fn-numeric-args rest-args))
       ;; The 3-arg form's is_called flag. It decides whether the NEXT
       ;; nextval returns `n` or `n + increment`, so dropping it makes
       ;; the sequence hand out the wrong value — `setval(s,10,false)`
       ;; then `nextval(s)` must be 10, not 11. Defaults to true, which
       ;; is what the 2-arg form means.
       :is-called (let [b (first (keep (fn [{:keys [type text]}]
                                         (when (= :ident type)
                                           (case (str/lower-case text)
                                             "false" false
                                             "true" true
                                             nil)))
                                       rest-args))]
                    (if (some? b) b true))}
      (fn-name=? t1 "pg_get_keywords") {:kind :pg-keywords}
      :else {:kind :generic-sql})))

(defn- next-ident-after
  "Skip a (possibly empty) run of non-ident tokens and return the next
   ident token or nil."
  [toks]
  (first (filter ident-tok? toks)))

(defn- alter-table-rls?
  "Match ALTER TABLE … (ENABLE|DISABLE|FORCE|NO FORCE) ROW LEVEL SECURITY.
   Token stream starts AFTER the `ALTER TABLE [IF EXISTS] name`."
  [toks]
  (let [ts (vec (take 5 toks))
        texts (mapv #(when (= :ident (:type %)) (str/lower-case (:text %))) ts)]
    (or (and (= "row" (nth texts 1 nil))
             (= "level" (nth texts 2 nil))
             (= "security" (nth texts 3 nil))
             (contains? #{"enable" "disable" "force"} (nth texts 0 nil)))
        (and (= "no" (nth texts 0 nil))
             (= "force" (nth texts 1 nil))
             (= "row" (nth texts 2 nil))
             (= "level" (nth texts 3 nil))
             (= "security" (nth texts 4 nil))))))

;; Sequence DDL is classified in full further down (it needs
;; read-relation-name, which is defined after this dispatch).
(declare classify-create-sequence classify-alter-sequence)

(defn- classify-create [toks]
  ;; toks starts after CREATE. Skip qualifiers (OR REPLACE, UNIQUE,
  ;; TEMPORARY, GLOBAL, LOCAL, UNLOGGED) that don't disambiguate kind.
  (let [skip? #{"or" "replace" "unique" "temporary" "temp"
                "global" "local" "unlogged"}
        toks (loop [ts toks]
               (if (kw-in? (first ts) skip?)
                 (recur (rest ts))
                 ts))
        t1 (first toks)]
    (cond
      (kw=? t1 "policy")    {:kind :create-policy :reject-kind :policy :tag "CREATE POLICY"}
      (kw=? t1 "extension") {:kind :create-extension :reject-kind :create-extension
                             :tag "CREATE EXTENSION"}
      (kw=? t1 "schema")    {:kind :schema-noop :tag "CREATE SCHEMA"}
      (kw=? t1 "database")  {:kind :create-database :tag "CREATE DATABASE"}
      (kw=? t1 "view")      {:kind :create-view}
      (kw=? t1 "index")     {:kind :create-index}
      (kw=? t1 "table")     {:kind :generic-sql}
      (kw=? t1 "sequence")  (classify-create-sequence (rest toks))

      ;; CREATE TYPE — only AS ENUM is supported as a first-class
      ;; type (lowered to string + check-in). Other forms (composite,
      ;; range, base) fall through to a silently-accepted reject so
      ;; pg_dump output keeps loading; the fields/values aren't used.
      (kw=? t1 "type")
      (let [;; toks already past CREATE; t1 is "type". Scan for AS ENUM.
            after-type (rest toks)
            ;; skip the type name (possibly schema-qualified, possibly
            ;; quoted) — we only need to know whether AS ENUM follows.
            scan (drop-while
                  (fn [t] (and t (not (or (kw=? t "as")
                                          (kw=? t "is")))))
                  after-type)
            after-as (rest scan)]
        (cond
          (kw=? (first after-as) "enum")
          {:kind :create-type-enum :system? true
           :tag "CREATE TYPE … AS ENUM"}
          ;; CREATE TYPE name AS (field type, …) — composite type.
          (= "(" (:text (first after-as)))
          {:kind :create-type-composite :system? true
           :tag "CREATE TYPE … AS (...)"}
          ;; other non-ENUM CREATE TYPE (range, base) — silently accept
          :else
          {:kind :create-type :reject-kind :type :tag "CREATE TYPE"}))

      (kw=? t1 "domain")
      {:kind :create-domain :system? true :tag "CREATE DOMAIN"}

      ;; pg_dump-emitted DDL we don't model. Each gets a dedicated
      ;; :reject-kind so operators can opt-in selectively (or via
      ;; the :pg-dump compat preset). Classified here — before
      ;; JSqlParser sees the SQL — so the function body's `$$…$$`
      ;; / TRIGGER body / etc. never reach the parser.
      (kw=? t1 "trigger")
      {:kind :create-trigger :reject-kind :trigger :tag "CREATE TRIGGER"}
      (kw=? t1 "function")
      {:kind :create-function :reject-kind :function :tag "CREATE FUNCTION"}
      (kw=? t1 "procedure")
      {:kind :create-procedure :reject-kind :procedure :tag "CREATE PROCEDURE"}
      (kw=? t1 "aggregate")
      {:kind :create-aggregate :reject-kind :aggregate :tag "CREATE AGGREGATE"}
      (and (kw=? t1 "materialized") (kw=? (second toks) "view"))
      {:kind :create-materialized-view :reject-kind :materialized-view
       :tag "CREATE MATERIALIZED VIEW"}
      (kw=? t1 "rule")
      {:kind :create-rule :reject-kind :rule :tag "CREATE RULE"}
      (kw=? t1 "operator")
      {:kind :create-operator :reject-kind :operator :tag "CREATE OPERATOR"}
      (kw=? t1 "cast")
      {:kind :create-cast :reject-kind :cast :tag "CREATE CAST"}
      (kw=? t1 "language")
      {:kind :create-language :reject-kind :language :tag "CREATE LANGUAGE"}

      :else                 {:kind :generic-sql})))

(defn- read-relation-name
  "Consume one `[ONLY] [schema.]name [*]` from the token stream.
   Returns [last-name-segment remaining-toks], or nil when the head is
   not a relation name. The schema qualifier is dropped (single-
   namespace store — mirrors the JSqlParser Drop branch's
   `(-> .getName .getName)` in parse-sql)."
  [toks]
  (let [toks (if (kw=? (first toks) "only") (rest toks) toks)]
    (when (ident-tok? (first toks))
      (loop [nm (ident-text (first toks))
             ts (rest toks)]
        (if (and (= "." (:text (first ts))) (ident-tok? (second ts)))
          (recur (ident-text (second ts)) (drop 2 ts))
          [nm (if (= "*" (:text (first ts))) (rest ts) ts)])))))

(defn- read-relation-list
  "Comma-separated relation names (each per read-relation-name).
   Returns [names remaining-toks], or nil on a malformed head — the
   caller falls back to :generic-sql and JSqlParser reports the real
   syntax error."
  [toks]
  (loop [names [] ts toks]
    (when-let [[nm ts'] (read-relation-name ts)]
      (if (= "," (:text (first ts')))
        (recur (conj names nm) (rest ts'))
        [(conj names nm) ts']))))

(defn- classify-drop-table
  "DROP TABLE with MORE THAN ONE name. JSqlParser 5.2's Drop grammar
   accepts a single name (ParseException at the comma — it expects
   CASCADE/ON/RESTRICT), so the list form is classified here and
   executed as one :ddl-drop with a :tables vector; pgbench -i sends
   `drop table if exists pgbench_accounts, pgbench_branches, …`.
   Single-name DROP TABLE keeps flowing through JSqlParser unchanged.
   A trailing CASCADE/RESTRICT is accepted and ignored, matching the
   JSqlParser branch (which discards Drop parameters)."
  [toks]
  (let [if-exists? (boolean (and (kw=? (first toks) "if")
                                 (kw=? (second toks) "exists")))
        ts (if if-exists? (drop 2 toks) toks)]
    (or (when-let [[names ts'] (read-relation-list ts)]
          (let [ts' (if (kw-in? (first ts') #{"cascade" "restrict"})
                      (rest ts') ts')
                ts' (drop-while #(= ";" (:text %)) ts')]
            (when (and (empty? ts') (> (count names) 1))
              {:kind :drop-table-multi :tables names :if-exists? if-exists?})))
        {:kind :generic-sql})))

;; ============================================================================
;; CREATE / ALTER SEQUENCE — token-classified in full.
;;
;; JSqlParser's CreateSequence grammar is a strict subset of PG's, and
;; every gap is a statement real clients send:
;;
;;   INCREMENT 20        BY is optional in PG      (issue #21, as reported)
;;   START 400           WITH is optional
;;   INCREMENT -1        signed values are legal
;;   AS bigint           no production at all
;;   IF NOT EXISTS       no production at all
;;   NO MINVALUE         no production at all
;;   ALTER SEQUENCE …    no AST branch downstream
;;
;; Patching that up with pre-parse rewrite rules only moves the problem:
;; the option VALUES were then recovered by regex over JSqlParser's
;; re-rendered SQL (`increment\s+by\s+(\d+)`), which cannot see a
;; negative increment and silently drops MINVALUE/MAXVALUE/CACHE/CYCLE.
;;
;; The option list is a flat, unordered, comma-free token sequence —
;; exactly what this classifier handles well and a fixed grammar we don't
;; control handles badly. Duplicates are PRESERVED in order here rather
;; than merged, because PG rejects a repeated option with 42601
;; ("conflicting or redundant options") and that check belongs with the
;; other validation, in ddl/sequence-params.
;; ============================================================================

(def ^:private seq-opt-keywords
  "Words that begin a sequence option. Used to decide whether a bare
   RESTART is followed by its optional value or by the next option."
  #{"as" "cache" "cycle" "increment" "logged" "maxvalue" "minvalue" "no"
    "owned" "restart" "sequence" "start" "unlogged"})

(defn- read-signed-number
  "Consume `[+|-] <number>`, returning [value remaining-toks] or nil.
   PG's NumericOnly accepts an explicit sign on every numeric sequence
   option, which is how `INCREMENT -1` and `MINVALUE -9223372036854775808`
   are written. The tokenizer emits the sign as a separate :op token."
  [toks]
  (let [t0 (first toks)
        [neg? ts] (cond
                    (= "-" (:text t0)) [true (rest toks)]
                    (= "+" (:text t0)) [false (rest toks)]
                    :else [false toks])]
    (when-let [v (number-value (first ts))]
      [(if neg? (- v) v) (rest ts)])))

(defn- read-sequence-option
  "Consume ONE sequence option. Returns [[opt-name value] remaining-toks],
   or nil when the head is not a recognised option.

   `value` is the parsed literal, `:none` for the NO / unspecified forms,
   and `:default` for a bare RESTART (which means \"restart at START\").
   Unrecognised-but-parseable options (LOGGED, SEQUENCE NAME) are
   returned too so validation can reject them the way PG does, rather
   than the statement dying with a syntax error."
  [toks]
  (let [t0 (first toks)]
    (cond
      ;; AS <type> — the type name may carry a typmod we ignore.
      (kw=? t0 "as")
      (when-let [nm (ident-text (second toks))]
        [[:as (str/lower-case nm)] (drop 2 toks)])

      ;; INCREMENT [BY] n
      (kw=? t0 "increment")
      (let [ts (if (kw=? (second toks) "by") (drop 2 toks) (rest toks))]
        (when-let [[v ts'] (read-signed-number ts)]
          [[:increment v] ts']))

      ;; START [WITH] n
      (kw=? t0 "start")
      (let [ts (if (kw=? (second toks) "with") (drop 2 toks) (rest toks))]
        (when-let [[v ts'] (read-signed-number ts)]
          [[:start v] ts']))

      ;; RESTART [[WITH] n] — the ONLY option whose value is optional, so
      ;; it needs lookahead: a WITH or a signed numeric is its argument;
      ;; another option keyword, `;`, or end-of-input means bare RESTART.
      (kw=? t0 "restart")
      (let [ts (if (kw=? (second toks) "with") (drop 2 toks) (rest toks))]
        (or (when-let [[v ts'] (read-signed-number ts)]
              [[:restart v] ts'])
            [[:restart :default] ts]))

      (kw=? t0 "cache")
      (when-let [[v ts'] (read-signed-number (rest toks))]
        [[:cache v] ts'])

      (kw=? t0 "minvalue")
      (when-let [[v ts'] (read-signed-number (rest toks))]
        [[:minvalue v] ts'])

      (kw=? t0 "maxvalue")
      (when-let [[v ts'] (read-signed-number (rest toks))]
        [[:maxvalue v] ts'])

      (kw=? t0 "cycle") [[:cycle true] (rest toks)]

      ;; NO {CYCLE | MINVALUE | MAXVALUE}. `NO MINVALUE` does NOT mean
      ;; unbounded — it means "use the default for this type and
      ;; increment sign", which is what :none signals downstream.
      (kw=? t0 "no")
      (let [t1 (second toks)]
        (cond
          (kw=? t1 "cycle")    [[:cycle false]    (drop 2 toks)]
          (kw=? t1 "minvalue") [[:minvalue :none] (drop 2 toks)]
          (kw=? t1 "maxvalue") [[:maxvalue :none] (drop 2 toks)]
          :else nil))

      ;; OWNED BY {table.column | NONE} — accepted and carried; we have
      ;; no dependency tracking, so it is a no-op semantically.
      (and (kw=? t0 "owned") (kw=? (second toks) "by"))
      (when-let [[nm ts'] (read-relation-name (drop 2 toks))]
        [[:owned-by nm] ts'])

      ;; Parse-but-reject forms, so the error is PG's rather than a
      ;; syntax error: SEQUENCE NAME is 42601 and LOGGED/UNLOGGED are
      ;; "option not recognized" when used as sequence options.
      (and (kw=? t0 "sequence") (kw=? (second toks) "name"))
      (when-let [[nm ts'] (read-relation-name (drop 2 toks))]
        [[:sequence-name nm] ts'])

      (kw=? t0 "logged")   [[:logged true] (rest toks)]
      (kw=? t0 "unlogged") [[:unlogged true] (rest toks)]

      :else nil)))

(defn- read-sequence-options
  "Consume the whole option list. Returns [opts remaining-toks] with
   `opts` an ordered vector of [name value] pairs — duplicates kept, so
   validation can raise 42601 for a repeated option."
  [toks]
  (loop [opts [], ts toks]
    (if-let [[opt ts'] (read-sequence-option ts)]
      (recur (conj opts opt) ts')
      [opts ts])))

(defn- classify-create-sequence
  "CREATE [TEMP|UNLOGGED] SEQUENCE [IF NOT EXISTS] name [options…].
   `toks` starts just after the SEQUENCE keyword (classify-create has
   already eaten the persistence qualifiers).

   Falls back to :generic-sql when the tail doesn't parse, so anything
   this doesn't model still reaches JSqlParser and reports its own
   syntax error rather than being silently accepted."
  [toks]
  (let [ine? (and (kw=? (first toks) "if")
                  (kw=? (second toks) "not")
                  (kw=? (nth toks 2 nil) "exists"))
        ts (if ine? (drop 3 toks) toks)]
    (or (when-let [[nm ts1] (read-relation-name ts)]
          (let [[opts ts2] (read-sequence-options ts1)
                ts3 (drop-while #(= ";" (:text %)) ts2)]
            (when (empty? ts3)
              {:kind :create-sequence
               :seq-name nm
               :if-not-exists? ine?
               :seq-opts opts})))
        {:kind :generic-sql})))

(defn- classify-alter-sequence
  "ALTER SEQUENCE [IF EXISTS] name options… — at least one option is
   MANDATORY here (PG's SeqOptList, not OptSeqOptList), which is the one
   grammatical difference from CREATE.

   RENAME TO / SET SCHEMA / OWNER TO are different statement shapes; they
   fall through to :generic-sql."
  [toks]
  (let [ie? (and (kw=? (first toks) "if") (kw=? (second toks) "exists"))
        ts (if ie? (drop 2 toks) toks)]
    (or (when-let [[nm ts1] (read-relation-name ts)]
          (let [[opts ts2] (read-sequence-options ts1)
                ts3 (drop-while #(= ";" (:text %)) ts2)]
            (when (and (seq opts) (empty? ts3))
              {:kind :alter-sequence
               :seq-name nm
               :if-exists? ie?
               :seq-opts opts})))
        {:kind :generic-sql})))

(defn- classify-truncate
  "TRUNCATE [TABLE] [ONLY] name [*] [, …] [RESTART|CONTINUE IDENTITY]
   [CASCADE|RESTRICT]. Token-classified in full: JSqlParser's Truncate
   grammar lacks RESTART/CONTINUE IDENTITY and parse-sql has no branch
   for its AST. ONLY/`*` are accepted and ignored (no inheritance
   children to include/exclude); CASCADE is carried so parse-sql can
   reject it with 0A000."
  [toks]
  (let [ts (if (kw=? (first toks) "table") (rest toks) toks)]
    (or (when-let [[names ts1] (read-relation-list ts)]
          (let [identity? (and (or (kw=? (first ts1) "restart")
                                   (kw=? (first ts1) "continue"))
                               (kw=? (second ts1) "identity"))
                restart? (boolean (and identity? (kw=? (first ts1) "restart")))
                ts2 (if identity? (drop 2 ts1) ts1)
                cascade? (kw=? (first ts2) "cascade")
                ts3 (if (or cascade? (kw=? (first ts2) "restrict"))
                      (rest ts2) ts2)
                ts4 (drop-while #(= ";" (:text %)) ts3)]
            (when (empty? ts4)
              {:kind :truncate :tables names
               :restart-identity? restart? :cascade? (boolean cascade?)})))
        {:kind :generic-sql})))

(defn- classify-drop [toks]
  (let [t1 (first toks)]
    (cond
      (kw=? t1 "table")     (classify-drop-table (rest toks))
      (kw=? t1 "policy")    {:kind :drop-policy :reject-kind :policy :tag "DROP POLICY"}
      (kw=? t1 "extension") {:kind :drop-extension :reject-kind :create-extension
                             :tag "DROP EXTENSION"}
      (kw=? t1 "schema")    {:kind :schema-noop :tag "DROP SCHEMA"}
      (kw=? t1 "database")  {:kind :drop-database :tag "DROP DATABASE"}

      ;; Symmetric with classify-create — reuse the same :reject-kind
      ;; so a single :silently-accept entry covers both ends.
      (kw=? t1 "trigger")    {:kind :drop-trigger :reject-kind :trigger :tag "DROP TRIGGER"}
      (kw=? t1 "function")   {:kind :drop-function :reject-kind :function :tag "DROP FUNCTION"}
      (kw=? t1 "procedure")  {:kind :drop-procedure :reject-kind :procedure :tag "DROP PROCEDURE"}
      (kw=? t1 "aggregate")  {:kind :drop-aggregate :reject-kind :aggregate :tag "DROP AGGREGATE"}
      (and (kw=? t1 "materialized") (kw=? (second toks) "view"))
      {:kind :drop-materialized-view :reject-kind :materialized-view
       :tag "DROP MATERIALIZED VIEW"}
      (kw=? t1 "rule")       {:kind :drop-rule :reject-kind :rule :tag "DROP RULE"}
      (kw=? t1 "operator")   {:kind :drop-operator :reject-kind :operator :tag "DROP OPERATOR"}
      (kw=? t1 "cast")       {:kind :drop-cast :reject-kind :cast :tag "DROP CAST"}
      (kw=? t1 "language")   {:kind :drop-language :reject-kind :language :tag "DROP LANGUAGE"}

      :else                 {:kind :generic-sql})))

(defn- contains-owner-to?
  "Scan the (already-consumed-prelude) token tail for a bare-ident
   `OWNER` followed immediately by a bare-ident `TO`. Used to detect
   `ALTER <object> ... OWNER TO <role>` — pg_dump emits this verb
   for every table/sequence/view/index/function/type it dumps. Real
   PG doesn't allow `OWNER` or `TO` as bare column / constraint names
   in DDL (they're reserved-ish in this context), so the false-positive
   risk is low."
  [toks]
  (loop [ts (seq toks)]
    (cond
      (nil? ts) false
      (and (kw=? (first ts) "owner")
           (kw=? (second ts) "to"))
      true
      :else (recur (next ts)))))

(defn- classify-alter [toks]
  (let [[t1 & rest-toks] toks]
    (cond
      (kw=? t1 "policy")  {:kind :alter-policy :reject-kind :policy :tag "ALTER POLICY"}
      (kw=? t1 "schema")  {:kind :schema-noop :tag "ALTER SCHEMA"}

      ;; ALTER <object> ... OWNER TO ... — pg_dump-emitted boilerplate.
      ;; We don't have a role system, so silently accept with the
      ;; matching command tag. Detected here (before the per-object-
      ;; type dispatch) so we cover ALTER TABLE / SEQUENCE / VIEW /
      ;; INDEX / FUNCTION / TYPE / DOMAIN / AGGREGATE / ... uniformly.
      (and (or (kw-in? t1 #{"table" "sequence" "view" "index"
                            "function" "procedure" "type" "domain"
                            "aggregate" "operator" "language"
                            "materialized" "publication" "subscription"
                            "foreign" "server" "trigger" "rule"
                            "collation" "conversion"})
               ;; ALTER LARGE OBJECT, ALTER GROUP, etc.
               (kw-in? t1 #{"large" "group" "role" "user"
                            "tablespace" "database"}))
           (contains-owner-to? rest-toks))
      {:kind :owner-noop
       :tag (str "ALTER " (str/upper-case (:text t1)))}

      ;; Must come after the OWNER TO check above, which claims
      ;; `ALTER SEQUENCE s OWNER TO r` as a no-op.
      (kw=? t1 "sequence") (classify-alter-sequence rest-toks)

      (kw=? t1 "table")
      ;; Consume optional [ONLY], [IF EXISTS] and the table name (possibly
      ;; schema.name), then inspect what follows.
      (let [ts (if (kw=? (first rest-toks) "only") (rest rest-toks) rest-toks)
            ts (if (and (kw=? (first ts) "if") (kw=? (second ts) "exists"))
                 (drop 2 ts) ts)
            ;; Consume exactly one (possibly schema-qualified) name.
            ts (if (ident-tok? (first ts)) (rest ts) ts)
            ts (if (and (= "." (:text (first ts)))
                        (ident-tok? (second ts)))
                 (drop 2 ts)
                 ts)]
        (cond
          (alter-table-rls? ts)
          {:kind :rls :reject-kind :rls :tag "ALTER TABLE ROW LEVEL SECURITY"}

          ;; ALTER TABLE [ONLY] x ATTACH PARTITION ... — partition
          ;; management. We don't model partitions; pg_dump emits these
          ;; per child table. The data is in the children either way.
          (and (kw=? (first ts) "attach") (kw=? (second ts) "partition"))
          {:kind :attach-partition :reject-kind :attach-partition
           :tag "ALTER TABLE ATTACH PARTITION"}

          (and (kw=? (first ts) "detach") (kw=? (second ts) "partition"))
          {:kind :detach-partition :reject-kind :attach-partition
           :tag "ALTER TABLE DETACH PARTITION"}

          :else {:kind :generic-sql}))

      ;; ALTER TYPE / ALTER DOMAIN — pg_dump emits these for setval
      ;; defaults and ownership; we silently accept under :pg-dump.
      ;; ALTER TYPE name OWNER TO ... is already covered by the
      ;; OWNER TO branch above.
      (kw=? t1 "type")
      {:kind :alter-type :reject-kind :alter-type :tag "ALTER TYPE"}
      (kw=? t1 "domain")
      {:kind :alter-domain :reject-kind :alter-domain :tag "ALTER DOMAIN"}

      :else {:kind :generic-sql})))

(defn- classify-rollback [toks]
  (let [[t1 t2 t3] toks]
    (cond
      (kw=? t1 "to")
      ;; ROLLBACK TO [SAVEPOINT] name
      (let [nm (if (kw=? t2 "savepoint") (ident-text t3) (ident-text t2))]
        {:kind :rollback-to-savepoint :name nm})
      (kw=? t1 "work")
      {:kind :rollback}
      :else
      {:kind :rollback})))

(defn- read-dotted-name
  "Consume a possibly-dotted, possibly-SET-TIME-ZONE-flavored variable
   name from the head of the token seq. Returns [var-name after-toks]
   or [nil toks] if no ident leads. var-name is lowercased."
  [toks]
  (let [[parts after]
        (loop [ts toks, acc []]
          (let [t (first ts)]
            (cond
              (and (empty? acc) (ident-tok? t))
              (recur (rest ts) [(str/lower-case (ident-text t))])
              (and (seq acc) (= "." (:text t)))
              (let [n (second ts)]
                (if (ident-tok? n)
                  (recur (drop 2 ts) (conj acc (str/lower-case (ident-text n))))
                  [acc ts]))
              ;; SET TIME ZONE is a special form — the variable is literally
              ;; "timezone". Collapse "time" followed by "zone" into it.
              (and (seq acc) (= (last acc) "time") (kw=? t "zone"))
              (recur (rest ts) (conj (vec (butlast acc)) "timezone"))
              :else [acc ts])))]
    [(when (seq parts) (str/join "." parts)) after]))

(defn- find-kw-idx
  "Index of the first bare-ident token whose lower-case text matches
   kw, or nil. Skips :comment tokens."
  [toks ^String kw]
  (loop [i 0, ts (seq toks)]
    (when-let [t (first ts)]
      (cond
        (= :comment (:type t)) (recur (inc i) (rest ts))
        (kw=? t kw) i
        :else (recur (inc i) (rest ts))))))

(defn- paren-group-end
  "Given idx pointing at a `(` punct, return the index of the matching
   `)`. Returns idx unchanged if there's no `(` there (caller has an
   easy test: `(= idx (paren-group-end toks idx))`)."
  [toks ^long idx]
  (if-not (= "(" (:text (nth toks idx nil)))
    idx
    (loop [i (inc idx), depth 1]
      (let [t (nth toks i nil)]
        (cond
          (nil? t) i
          (= "(" (:text t)) (recur (inc i) (inc depth))
          (= ")" (:text t)) (if (= 1 depth) i (recur (inc i) (dec depth)))
          :else (recur (inc i) depth))))))

(defn- text-tail
  "Source text from byte position `pos` to end of sql, trimmed of
   leading/trailing whitespace and a single trailing semicolon. Used
   for classify kinds (PREPARE, DECLARE) that need to carry a whole
   SQL suffix through to the handler."
  [^String sql ^long pos]
  (-> (subs sql pos)
      str/trim
      (str/replace #"\s*;\s*$" "")))

(defn- classify-prepare
  "PREPARE name [(arg-types)] AS <sql-template>"
  [^String sql toks]
  (let [name (ident-text (first toks))
        ;; Skip optional (arg-types)
        after-name-idx 1
        after-types-idx (if (= "(" (:text (nth toks after-name-idx nil)))
                          (inc (paren-group-end toks after-name-idx))
                          after-name-idx)
        as-tok (when (kw=? (nth toks after-types-idx nil) "as")
                 (nth toks after-types-idx))]
    {:kind :prepare
     :name (when name (str/lower-case name))
     :template (when as-tok (text-tail sql (:end as-tok)))}))

(defn- classify-execute
  "EXECUTE name [(args)]"
  [^String sql toks]
  (let [name (ident-text (first toks))
        lparen-idx (when (= "(" (:text (nth toks 1 nil))) 1)
        rparen-idx (when lparen-idx (paren-group-end toks lparen-idx))
        args-text (when (and lparen-idx rparen-idx
                             (< rparen-idx (count toks)))
                    (let [l (nth toks lparen-idx)
                          r (nth toks rparen-idx)]
                      (str/trim (subs sql (:end l) (:pos r)))))]
    {:kind :execute-prepared
     :name (when name (str/lower-case name))
     :args-text args-text}))

(defn- classify-deallocate
  "DEALLOCATE [PREPARE] (name | ALL). The optional PREPARE keyword is
   PG-compat noise; PG allows both forms."
  [toks]
  (let [t1 (first toks)
        t2 (second toks)
        prepare? (kw=? t1 "prepare")
        target (if prepare? t2 t1)
        all? (kw=? target "all")]
    {:kind :deallocate
     :all? (boolean all?)
     :name (when (and (not all?) (or (= :ident (:type target))
                                     (= :quoted (:type target))))
             (str/lower-case (ident-text target)))}))

(defn- classify-declare-cursor
  "DECLARE name [NO SCROLL|SCROLL] [BINARY] CURSOR [WITH HOLD|WITHOUT
   HOLD] FOR <select-sql>"
  [^String sql toks]
  (let [name (ident-text (first toks))
        for-idx (find-kw-idx toks "for")
        for-tok (when for-idx (nth toks for-idx nil))]
    (if for-idx
      {:kind :declare-cursor
       :name (when name (str/lower-case name))
       :inner-sql (text-tail sql (:end for-tok))}
      {:kind :generic-sql})))

(def ^:private cursor-directions
  #{"all" "next" "backward" "forward" "prior" "first" "last"
    "absolute" "relative"})

(defn- classify-fetch-move
  "FETCH/MOVE [direction] [count] [FROM|IN] cursor-name. Returns
   :direction (lowercased keyword string), :count (long or nil), and
   :name (lowercased cursor name). Each is optional except the name —
   we capture the LAST ident that isn't a direction/FROM/IN keyword."
  [kind toks]
  (let [direction (first (keep #(when (kw-in? % cursor-directions)
                                  (str/lower-case (:text %)))
                               toks))
        count-val (some #(when (= :number (:type %))
                           (try (Long/parseLong (:text %))
                                (catch Throwable _ nil)))
                        toks)
        name (last (keep (fn [t]
                           (when (and (or (= :ident (:type t))
                                          (= :quoted (:type t)))
                                      (not (kw-in? t cursor-directions))
                                      (not (kw-in? t #{"from" "in"})))
                             (ident-text t)))
                         toks))]
    {:kind kind
     :direction direction
     :count count-val
     :name (when name (str/lower-case name))}))

(defn- classify-close
  "CLOSE (name | ALL)"
  [toks]
  (let [t1 (first toks)
        all? (kw=? t1 "all")]
    {:kind :close-cursor
     :all? (boolean all?)
     :name (when (and (not all?) (or (= :ident (:type t1))
                                     (= :quoted (:type t1))))
             (str/lower-case (ident-text t1)))}))

(defn- isolation-level-after
  "Scan `toks` for `ISOLATION LEVEL <level>` and return the level as a
   PG-canonical lowercased, space-separated string (\"read committed\",
   \"read uncommitted\", \"repeatable read\", \"serializable\"), or nil if
   the phrase isn't present. Used by SET SESSION CHARACTERISTICS, SET
   TRANSACTION, and BEGIN/START TRANSACTION."
  [toks]
  (let [v (vec toks)
        n (count v)]
    (loop [i 0]
      (cond
        (>= i (dec n)) nil
        (and (kw=? (nth v i) "isolation") (kw=? (nth v (inc i) nil) "level"))
        (let [a (nth v (+ i 2) nil)
              b (nth v (+ i 3) nil)
              w1 (when (ident-tok? a) (str/lower-case (ident-text a)))
              w2 (when (ident-tok? b) (str/lower-case (ident-text b)))]
          (cond
            (nil? w1) nil
            (= w1 "serializable") "serializable"
            (and (= w1 "read") w2)       (str "read " w2)
            (and (= w1 "repeatable") w2) (str "repeatable " w2)
            :else w1))
        :else (recur (inc i))))))

(defn- classify-set*
  "Generic SET name = value branch (no isolation special-casing)."
  [toks]
  (let [[var-name after-var] (read-dotted-name toks)
        ;; Skip optional = or TO
        after-eq (if (or (= "=" (:text (first after-var)))
                         (kw=? (first after-var) "to"))
                   (rest after-var) after-var)
        first-val (first after-eq)
        value-str (cond
                    (nil? first-val) nil
                    (= :string (:type first-val)) (:value first-val)
                    (= :number (:type first-val)) (:text first-val)
                    (ident-tok? first-val) (ident-text first-val)
                    :else nil)]
    {:kind :set :var var-name :value value-str}))

(defn- classify-set
  "SET name = value / SET TIME ZONE '…' / SET SESSION AUTHORIZATION …
   / SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL …
   / SET [LOCAL] TRANSACTION ISOLATION LEVEL …

   Returns {:kind :set :var \"fullname\" :value \"raw text\"}. Known
   temporal vars (datahike.as_of/.since/.history) are pre-extracted so
   the server can look at :value without re-parsing. Isolation-level
   forms get their own kinds so the server can track the session/tx
   isolation that `SHOW transaction_isolation` must reflect."
  [toks]
  (let [;; Skip optional LOCAL/SESSION modifier
        toks (if (kw-in? (first toks) #{"local" "session"})
               (rest toks) toks)]
    (cond
      ;; SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL <lvl>
      ;; (and/or READ ONLY|WRITE — the READ-only part is a no-op for us).
      (kw=? (first toks) "characteristics")
      (if-let [lvl (isolation-level-after toks)]
        {:kind :set-session-isolation :value lvl}
        {:kind :set :var "session_characteristics"})

      ;; SET [LOCAL] TRANSACTION ISOLATION LEVEL <lvl> (per-tx override).
      (kw=? (first toks) "transaction")
      (if-let [lvl (isolation-level-after toks)]
        {:kind :set-transaction-isolation :value lvl}
        {:kind :set :var "transaction"})

      :else
      (classify-set* toks))))

(def ^:private psql-metacommand-names
  "Lower-case psql metacommand names that pg_dump emits unprefixed
   (i.e. as a leading `\\NAME` token). psql normally consumes these
   itself, but a script that pipes the dump directly through a JDBC
   `executeStatement` will leak them through to us.

   Limited to the metacommands we actually see in pg_dump output —
   not a full psql lexicon. False-positive risk: if someone genuinely
   wants to send a `\\connect` as application SQL, we'll accept-and-
   ignore it. That's the same outcome as if they'd sent it through
   real psql, so it's a wash."
  #{"restrict" "unrestrict"
    "connect" "c"
    "set" "i" "ir"
    "encoding"
    "echo" "qecho"})

(defn classify
  "Classify a SQL string. See namespace docstring for the output
   contract."
  [^String sql]
  (let [all-toks (tokenize sql)
        ;; Skip leading `;` (empty statement separators) — some clients
        ;; prefix with a stray semicolon or batch multiple statements.
        skipped (drop-while #(= ";" (:text %)) all-toks)
        ;; Kinds that carry a source-text suffix (PREPARE template,
        ;; DECLARE inner SELECT, EXECUTE args-paren-group) need the
        ;; full token stream to locate the end-of-prefix token; all
        ;; others are decided from the first handful. Realize more
        ;; than 12 so the cursor-walkers have enough context.
        toks (vec (take 64 skipped))
        t1 (first toks)
        rest-toks (subvec toks (if (seq toks) 1 0))]
    (cond
      (nil? t1)
      {:kind :empty}

      ;; psql metacommand at the head of the statement.
      ;; Tokenizer emits `\` as :op (it's not in op-chars but falls
      ;; through to the single-char-punct fallback). The next ident
      ;; carries the metacommand name. pg_dump 18+ emits `\restrict`
      ;; / `\unrestrict` markers; older versions emit `\connect`
      ;; before each database in a multi-database dump.
      (and (= "\\" (:text t1))
           (= :ident (:type (first rest-toks)))
           (contains? psql-metacommand-names
                      (str/lower-case (:text (first rest-toks)))))
      {:kind :psql-meta
       :tag (str "\\" (str/lower-case (:text (first rest-toks))))}

      :else
      (let [kw (when (= :ident (:type t1)) (str/lower-case (:text t1)))]
        (case kw
          ;; --- authorization DDL — rejected by default, opt-in silent-accept
          "grant"   {:kind :grant   :reject-kind :grant   :tag "GRANT"}
          "revoke"  {:kind :revoke  :reject-kind :revoke  :tag "REVOKE"}

          ;; --- DDL routed by second keyword
          "create"  (classify-create rest-toks)
          ;; classify-drop walks a comma-separated name list of
          ;; unbounded length (DROP TABLE a, b, …) — feed it the full
          ;; lazy token stream, not the 64-token prefix, so a long list
          ;; can't be silently cut short at the realization boundary.
          "drop"    (classify-drop (rest skipped))
          "alter"   (classify-alter rest-toks)

          ;; --- COPY — recognised so parse-sql can dispatch to the
          ;; hand-rolled parser in datahike.pg.sql.copy (JSqlParser
          ;; doesn't handle COPY). Routed as :copy-from-stdin which
          ;; lands in the system-type table.
          "copy"    {:kind :copy-from-stdin :tag "COPY"}

          ;; --- Transaction control
          "begin"   (cond-> {:kind :begin}
                      (isolation-level-after rest-toks)
                      (assoc :isolation (isolation-level-after rest-toks)))
          "start"   (if (kw=? (first rest-toks) "transaction")
                      (cond-> {:kind :begin}
                        (isolation-level-after rest-toks)
                        (assoc :isolation (isolation-level-after rest-toks)))
                      {:kind :generic-sql})
          "commit"  {:kind :commit}
          ;; Bare END / END WORK / END TRANSACTION = COMMIT. A trailing
          ;; `;` token must not disqualify it — pgbench -M prepared sends
          ;; "END;" as its own Parse message (jsqlparser can't parse END).
          "end"     (let [t (first rest-toks)]
                      (if (or (nil? t)
                              (= ";" (:text t))
                              (kw-in? t #{"work" "transaction"}))
                        {:kind :commit}
                        {:kind :generic-sql}))
          "rollback" (classify-rollback rest-toks)
          "savepoint" {:kind :savepoint :name (ident-text (first rest-toks))}
          "release"  (let [nm (if (kw=? (first rest-toks) "savepoint")
                                (ident-text (second rest-toks))
                                (ident-text (first rest-toks)))]
                       {:kind :release-savepoint :name nm})

          ;; --- Cursors
          "declare"    (if (some #(kw=? % "cursor") (take 5 rest-toks))
                         (classify-declare-cursor sql rest-toks)
                         {:kind :generic-sql})
          "fetch"      (classify-fetch-move :fetch-cursor rest-toks)
          "move"       (classify-fetch-move :move-cursor  rest-toks)
          "close"      (classify-close rest-toks)

          ;; --- Session / prepared statements
          "prepare"    (classify-prepare sql rest-toks)
          "execute"    (classify-execute sql rest-toks)
          "deallocate" (classify-deallocate rest-toks)
          "discard" (cond
                      (kw=? (first rest-toks) "all") {:kind :discard-all}
                      (kw-in? (first rest-toks)
                              #{"plans" "sequences" "temp" "temporary" "locks"})
                      {:kind :discard-scoped
                       :scope (str/lower-case (:text (first rest-toks)))}
                      :else {:kind :generic-sql})
          "set"     (classify-set rest-toks)
          "reset"   {:kind :reset :var (first (read-dotted-name rest-toks))}
          "show"    {:kind :show  :var (first (read-dotted-name rest-toks))}

          ;; --- LISTEN / UNLISTEN / NOTIFY (async notification channels).
          ;; pg-datahike has no notification delivery, so these are
          ;; observably no-ops (a NOTIFY with zero subscribers ==
          ;; delivered-and-ignored). JSqlParser can't parse UNLISTEN at
          ;; all, so they MUST be intercepted here. asyncpg's pool reset
          ;; query (`get_reset_query`) sends `UNLISTEN *` on every
          ;; connection release.
          "listen"   {:kind :listen-noop   :tag "LISTEN"}
          "unlisten" {:kind :unlisten-noop :tag "UNLISTEN"}
          "notify"   {:kind :notify-noop   :tag "NOTIFY"}

          ;; --- No-op DDL and maintenance
          "comment" {:kind :comment-on}
          "lock"    {:kind :lock-table}
          "vacuum"  {:kind :maintenance-noop :tag "VACUUM"}
          "reindex" {:kind :maintenance-noop :tag "REINDEX"}
          "cluster" {:kind :maintenance-noop :tag "CLUSTER"}
          "analyze" {:kind :maintenance-noop :tag "ANALYZE"}

          ;; --- SELECT may hijack common zero-arg fns + advisory locks
          "select"  (classify-select rest-toks)
          "with"    {:kind :generic-sql}  ; CTE: DML follows
          "table"   {:kind :generic-sql}  ; TABLE t shorthand
          "values"  {:kind :generic-sql}

          ;; --- DML
          "insert"  {:kind :generic-sql}
          "update"  {:kind :generic-sql}
          "delete"  {:kind :generic-sql}
          "merge"   {:kind :generic-sql}
          ;; Full stream for the same reason as "drop" — the table list
          ;; is unbounded.
          "truncate" (classify-truncate (rest skipped))

          ;; --- EXPLAIN, CALL (function)
          "explain" {:kind :generic-sql}
          "call"    {:kind :generic-sql}

          ;; default — let JSqlParser try
          {:kind :generic-sql})))))
