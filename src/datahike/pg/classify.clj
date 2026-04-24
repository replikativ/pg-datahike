(ns datahike.pg.classify
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
   preprocess-sql rewriter lives in datahike.pg.rewrite and consumes
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

(defn- classify-select
  "Classify a SELECT: either one of our hijack patterns, or
   {:kind :generic-sql} for anything that should flow through to
   JSqlParser. Called with the tokens AFTER the leading SELECT."
  [toks]
  (let [[t1 _t2 & _] (skip-leading-paren toks)
        ;; When SELECT is followed by `pg_catalog.` prefix, skip it.
        [t1 rest-args] (if (and (kw=? t1 "pg_catalog")
                                (= "." (:text (second toks))))
                         [(nth toks 2 nil) (drop 3 toks)]
                         [t1 (rest toks)])]
    (cond
      (nil? t1) {:kind :generic-sql}
      (kw=? t1 "version")       {:kind :version}
      (kw=? t1 "now")           {:kind :now}
      (kw=? t1 "current_schema") {:kind :current-schema}
      (kw=? t1 "current_database") {:kind :current-database}
      (kw=? t1 "pg_backend_pid") {:kind :pg-backend-pid}
      (kw=? t1 "txid_current")  {:kind :txid-current}
      (kw=? t1 "pg_sleep")
      {:kind :pg-sleep :args (extract-fn-numeric-args rest-args)}
      (kw=? t1 "pg_advisory_lock")
      {:kind :advisory-lock :args (extract-fn-numeric-args rest-args)}
      (kw=? t1 "pg_try_advisory_lock")
      {:kind :try-advisory-lock :args (extract-fn-numeric-args rest-args)}
      (kw=? t1 "pg_advisory_xact_lock")
      {:kind :advisory-xact-lock :args (extract-fn-numeric-args rest-args)}
      (kw=? t1 "pg_try_advisory_xact_lock")
      {:kind :try-advisory-xact-lock :args (extract-fn-numeric-args rest-args)}
      (kw=? t1 "pg_advisory_unlock")
      {:kind :advisory-unlock :args (extract-fn-numeric-args rest-args)}
      (kw=? t1 "pg_advisory_unlock_all")
      {:kind :advisory-unlock-all}
      (kw=? t1 "nextval")
      {:kind :nextval :seq-name (first (extract-fn-string-args rest-args))}
      (kw=? t1 "currval")
      {:kind :currval :seq-name (first (extract-fn-string-args rest-args))}
      (kw=? t1 "setval")
      {:kind :setval
       :seq-name (first (extract-fn-string-args rest-args))
       :new-value (first (extract-fn-numeric-args rest-args))}
      (kw=? t1 "pg_get_keywords") {:kind :pg-keywords}
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
      (kw=? t1 "view")      {:kind :create-view}
      (kw=? t1 "index")     {:kind :create-index}
      (kw=? t1 "table")     {:kind :generic-sql}
      (kw=? t1 "sequence")  {:kind :generic-sql}
      :else                 {:kind :generic-sql})))

(defn- classify-drop [toks]
  (let [t1 (first toks)]
    (cond
      (kw=? t1 "policy")    {:kind :drop-policy :reject-kind :policy :tag "DROP POLICY"}
      (kw=? t1 "extension") {:kind :drop-extension :reject-kind :create-extension
                             :tag "DROP EXTENSION"}
      (kw=? t1 "schema")    {:kind :schema-noop :tag "DROP SCHEMA"}
      :else                 {:kind :generic-sql})))

(defn- classify-alter [toks]
  (let [[t1 & rest-toks] toks]
    (cond
      (kw=? t1 "policy")  {:kind :alter-policy :reject-kind :policy :tag "ALTER POLICY"}
      (kw=? t1 "schema")  {:kind :schema-noop :tag "ALTER SCHEMA"}
      (kw=? t1 "table")
      ;; Consume optional [IF EXISTS] and the table name (possibly
      ;; schema.name), then inspect what follows. If it's ENABLE/
      ;; DISABLE/FORCE/NO FORCE … ROW LEVEL SECURITY, classify as
      ;; RLS; otherwise pass through.
      (let [ts (if (and (kw=? (first rest-toks) "if")
                        (kw=? (second rest-toks) "exists"))
                 (drop 2 rest-toks)
                 rest-toks)
            ;; Consume exactly one (possibly schema-qualified) name.
            ts (if (ident-tok? (first ts)) (rest ts) ts)
            ts (if (and (= "." (:text (first ts)))
                        (ident-tok? (second ts)))
                 (drop 2 ts)
                 ts)]
        (if (alter-table-rls? ts)
          {:kind :rls :reject-kind :rls :tag "ALTER TABLE ROW LEVEL SECURITY"}
          {:kind :generic-sql}))
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

(defn- classify-set
  "SET name = value / SET TIME ZONE '…' / SET SESSION AUTHORIZATION …

   Returns {:kind :set :var \"fullname\" :value \"raw text\"}. Known
   temporal vars (datahike.as_of/.since/.history) are pre-extracted so
   the server can look at :value without re-parsing."
  [toks]
  (let [;; Skip optional LOCAL/SESSION modifier
        toks (if (kw-in? (first toks) #{"local" "session"})
               (rest toks) toks)
        [var-name after-var] (read-dotted-name toks)
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
    (if (nil? t1)
      {:kind :empty}
      (let [kw (when (= :ident (:type t1)) (str/lower-case (:text t1)))]
        (case kw
          ;; --- authorization DDL — rejected by default, opt-in silent-accept
          "grant"   {:kind :grant   :reject-kind :grant   :tag "GRANT"}
          "revoke"  {:kind :revoke  :reject-kind :revoke  :tag "REVOKE"}

          ;; --- DDL routed by second keyword
          "create"  (classify-create rest-toks)
          "drop"    (classify-drop rest-toks)
          "alter"   (classify-alter rest-toks)

          ;; --- COPY — reject cleanly with 0A000
          "copy"    {:kind :copy :reject-kind :copy :tag "COPY"}

          ;; --- Transaction control
          "begin"   {:kind :begin}
          "start"   (if (kw=? (first rest-toks) "transaction")
                      {:kind :begin}
                      {:kind :generic-sql})
          "commit"  {:kind :commit}
          "end"     (if (or (nil? (first rest-toks))
                            (kw-in? (first rest-toks) #{"work" "transaction"}))
                      {:kind :commit}
                      {:kind :generic-sql})
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
          "truncate" {:kind :generic-sql}

          ;; --- EXPLAIN, CALL (function)
          "explain" {:kind :generic-sql}
          "call"    {:kind :generic-sql}

          ;; default — let JSqlParser try
          {:kind :generic-sql})))))
