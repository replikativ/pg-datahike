(ns datahike.pg.sql.rewrite
  "Token-driven SQL source rewrites. Normalize SQL before JSqlParser
   sees it by excising or injecting source-level spans — all based on
   positions captured by the datahike.pg.sql.classify tokenizer.

   Each rule is a pure function `(tokens) -> seq of spans`, where a
   span is `[start end replacement]`. The rewriter applies all non-
   overlapping spans right-to-left (so earlier offsets stay stable)
   and returns the new SQL string.

   Why this exists: the previous preprocess-sql was a pile of regex
   `str/replace` calls that could false-positive on keywords inside
   string literals, dollar-quotes, or comments (`SELECT 'REFERENCES'`
   was vulnerable to the inline-REFERENCES stripper). Token-based
   rules only match tokens of the right kind, so a keyword inside a
   :string or :comment is invisible to them.

   Callers: sql/preprocess-sql."
  (:require [clojure.string :as str]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.params :as params]))

;; ============================================================================
;; Core rewriter
;; ============================================================================

(defn- overlaps? [[a-start a-end] [b-start _b-end]]
  (< b-start a-end))

(defn- drop-overlaps
  "Given spans sorted by :start ascending, drop any span whose start
   falls inside a prior span."
  [spans]
  (reduce (fn [acc span]
            (if (and (seq acc) (overlaps? (peek acc) span))
              acc
              (conj acc span)))
          []
          spans))

(defn rewrite
  "Apply rules to sql and return the rewritten string.
   Each rule is a `(tokens) -> seq of [start end replacement]` fn.
   Throws exceptions from rules upward (callers rely on this for
   unsupported-feature detection — e.g. FK ON DELETE CASCADE)."
  [^String sql rules]
  (let [toks (vec (cls/tokenize-all sql))
        spans (->> rules
                   (mapcat (fn [rule] (rule toks)))
                   (sort-by first)
                   drop-overlaps
                   (sort-by first >))]
    (reduce (fn [s [start end replacement]]
              (str (subs s 0 start) replacement (subs s end)))
            sql
            spans)))

;; ============================================================================
;; Token helpers
;; ============================================================================

(defn- ident-tok? [tok]   (= :ident   (:type tok)))
(defn- punct? [tok text] (and (= :punct (:type tok)) (= text (:text tok))))

(defn- kw-text
  "lowercase :ident text, or nil for non-idents."
  [tok]
  (when (ident-tok? tok) (str/lower-case (:text tok))))

(defn- non-comment-before
  "The most recent non-comment token strictly before idx, or nil."
  [toks ^long idx]
  (loop [i (dec idx)]
    (cond
      (neg? i) nil
      (= :comment (:type (nth toks i))) (recur (dec i))
      :else (nth toks i))))

(defn- skip-non-comment
  "Advance from idx through a predicate over non-comment tokens. Returns
   index of the first token that doesn't satisfy pred (or count of toks
   at EOF). Comment tokens pass through invisibly."
  [toks ^long idx pred]
  (loop [i idx]
    (let [t (nth toks i nil)]
      (cond
        (nil? t) i
        (= :comment (:type t)) (recur (inc i))
        (pred t) (recur (inc i))
        :else i))))

(defn- match-paren-group
  "If toks[idx] is `(`, advance through matching parens and return the
   index one past the closing `)`. Otherwise return idx. Quoted strings
   inside parens are already distinct tokens so paren counting is safe."
  [toks ^long idx]
  (let [t (nth toks idx nil)]
    (if (punct? t "(")
      (loop [i (inc idx), depth 1]
        (let [x (nth toks i nil)]
          (cond
            (nil? x) i
            (punct? x "(") (recur (inc i) (inc depth))
            (punct? x ")") (if (= depth 1) (inc i) (recur (inc i) (dec depth)))
            :else (recur (inc i) depth))))
      idx)))

;; ============================================================================
;; Rules
;; ============================================================================

(defn- match-on-action
  "Given toks and an index pointing at what might be `ON DELETE/UPDATE
   action`, return {:end-idx … :verb … :action …} if matched, nil
   otherwise. :action is a lowercase string like \"cascade\" / \"set null\"."
  [toks ^long idx]
  (let [t1 (nth toks idx nil)
        t2 (nth toks (inc idx) nil)
        t3 (nth toks (+ idx 2) nil)
        t4 (nth toks (+ idx 3) nil)
        k1 (kw-text t1)
        k2 (kw-text t2)
        k3 (kw-text t3)
        k4 (kw-text t4)]
    (when (and (= "on" k1) (contains? #{"delete" "update"} k2))
      (cond
        (and (= "no" k3) (= "action" k4))
        {:end-idx (+ idx 4) :verb k2 :action "no action"}
        (and (= "set" k3) (contains? #{"null" "default"} k4))
        {:end-idx (+ idx 4) :verb k2 :action (str k3 " " k4)}
        (contains? #{"cascade" "restrict"} k3)
        {:end-idx (+ idx 3) :verb k2 :action k3}))))

(defn- find-inline-col-name
  "Walk back from `idx` (pointing at inline `references`) to find the
   column name — the first ident after the most recent `,` or opening
   `(` of the column list. Returns the source text of that ident, or
   nil if not found."
  [toks ^long idx]
  (let [start-idx (loop [i (dec idx)]
                    (cond
                      (neg? i) 0
                      (or (punct? (nth toks i) ",")
                          (punct? (nth toks i) "("))
                      (inc i)
                      :else (recur (dec i))))]
    (loop [i start-idx]
      (let [t (nth toks i nil)]
        (cond
          (nil? t) nil
          (= :comment (:type t)) (recur (inc i))
          (ident-tok? t) (:text t)
          :else (recur (inc i)))))))

(defn- find-enclosing-close-paren
  "From `idx` walk forward at the current paren depth until the matching
   close `)` of the outer paren. Returns its position or nil."
  [toks ^long idx]
  (loop [i idx, depth 0]
    (let [t (nth toks i nil)]
      (cond
        (nil? t) nil
        (punct? t "(") (recur (inc i) (inc depth))
        (and (punct? t ")") (zero? depth)) (:pos t)
        (punct? t ")") (recur (inc i) (dec depth))
        :else (recur (inc i) depth)))))

(defn- token-range-text
  "Reconstruct the source between tokens [from, to) by concatenating
   :text values, with single spaces between adjacent tokens. Cheap
   substitute for slicing the original SQL — accurate enough for
   identifiers + a parenthesised col list, which is what we need for
   `REFERENCES name [(col)]`."
  [toks ^long from ^long to]
  (str/join " "
            (keep (fn [t]
                    (when (not= :comment (:type t))
                      (:text t)))
                  (subvec toks from to))))

(defn inline-references-rule
  "Inline `col TYPE … REFERENCES name [(cols)] [ON (DELETE|UPDATE) action]`.
   JSqlParser doesn't accept the inline form, so we rewrite it. Two paths:

   1. **No action / RESTRICT / no action clause** — just strip the
      `REFERENCES …` span. Our existing FK plumbing only enforces
      table-level `FOREIGN KEY (col) REFERENCES …`, and NO ACTION /
      RESTRICT have no operational consequence beyond blocking the
      parent delete (which we then can't enforce, but Odoo's
      _auto_init flow doesn't depend on it).

   2. **CASCADE on DELETE** — lift to a table-level `FOREIGN KEY` so
      our FK plumbing tracks it and enforces cascade at runtime.
      We strip the inline span AND inject a synthetic
      `, FOREIGN KEY (col) REFERENCES name(cols) ON DELETE CASCADE`
      just before the closing `)` of the CREATE TABLE column list.

   3. **SET NULL / SET DEFAULT / ON UPDATE non-trivial** — raise
      0A000; not yet implemented at the runtime side.

   Distinguishes inline from table-level by checking the previous
   non-comment token: if it's `)` (from `FOREIGN KEY (col)`), we
   leave the whole REFERENCES alone so JSqlParser parses it natively
   as a ForeignKeyIndex."
  [toks]
  ;; Wrap so the outer caller can pass the SQL string for substr lookups.
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)]
          (if (not= "references" (kw-text t))
            (recur (inc i) acc)
            (let [prev (non-comment-before toks i)]
              (if (punct? prev ")")
                (recur (inc i) acc)
                (let [start (:pos t)
                      after-name-idx (skip-non-comment toks (inc i)
                                                       #(or (ident-tok? %)
                                                            (punct? % ".")))
                      after-cols-idx (match-paren-group toks after-name-idx)
                      on-match (match-on-action toks after-cols-idx)
                      end-idx (or (:end-idx on-match) after-cols-idx)
                      end-pos (:end (nth toks (dec end-idx)))
                      act (:action on-match)
                      verb (:verb on-match)]
                  (cond
                    ;; SET NULL / SET DEFAULT / non-DELETE CASCADE — not yet
                    ;; implemented at the runtime FK enforcement side.
                    (or (contains? #{"set null" "set default"} act)
                        (and (= "update" verb)
                             (contains? #{"cascade" "set null" "set default"} act)))
                    (throw (ex-info "unsupported foreign-key action"
                                    {:error :feature-not-supported
                                     :feature (str "foreign-key action ON "
                                                   (str/upper-case (or verb "DELETE")) " "
                                                   (str/upper-case act))}))

                    ;; ON DELETE CASCADE — lift to table-level FK so our
                    ;; runtime cascade machinery (server.clj
                    ;; collect-fk-cascade-retractions!) sees it.
                    (and (= "delete" verb) (= "cascade" act))
                    (let [col-name (find-inline-col-name toks i)
                          ;; `REFERENCES name [(col)]` — reconstruct from
                          ;; the tokens between `references` (inc i) and
                          ;; the action clause start (after-cols-idx).
                          ;; after-name-idx points one past the table name
                          ;; (at `(` of the col list), so we need (inc i).
                          ref-target (token-range-text toks (inc i) after-cols-idx)
                          close-paren-pos (find-enclosing-close-paren toks (inc i))
                          inject (when col-name
                                   (str ", FOREIGN KEY (" col-name ") "
                                        "REFERENCES " ref-target
                                        " ON DELETE CASCADE "))
                          acc' (conj acc [start end-pos " "])
                          acc'' (if (and inject close-paren-pos)
                                  (conj acc' [close-paren-pos close-paren-pos inject])
                                  acc')]
                      (recur end-idx acc''))

                    ;; NO ACTION / RESTRICT / no action — strip silently.
                    :else
                    (recur end-idx
                           (conj acc [start end-pos " "]))))))))))))

(defonce ^:private anon-index-counter (atom 0))

(defn create-index-anonymous-rule
  "`CREATE [UNIQUE] INDEX ON …` → inject `idx_auto_<N>` between INDEX
   and ON. PG allows unnamed indexes; JSqlParser doesn't. The counter
   is process-wide and monotonic; collisions across handler sessions
   are harmless because the name is thrown away by the :create-index
   no-op handler anyway."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)]
          (if-not (= "create" (kw-text t))
            (recur (inc i) acc)
            (let [i1 (inc i)
                  t1 (nth toks i1 nil)
                  i2 (if (= "unique" (kw-text t1)) (inc i1) i1)
                  t2 (nth toks i2 nil)
                  i3 (if (= "index" (kw-text t2)) (inc i2) i2)
                  t3 (nth toks i3 nil)]
              (if (and (= "index" (kw-text t2))
                       (= "on" (kw-text t3)))
                (let [insert-pos (:pos t3)
                      n-idx (swap! anon-index-counter inc)]
                  (recur (inc i3)
                         (conj acc [insert-pos insert-pos
                                    (str "idx_auto_" n-idx " ")])))
                (recur (inc i) acc)))))))))

(defn select-from-rule
  "`SELECT FROM …` (empty projection) → `SELECT 1 FROM …`. PG allows
   projection-less SELECT in EXISTS subqueries; JSqlParser doesn't."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)]
          (if (and (= "select" (kw-text t))
                   (= "from" (kw-text (nth toks (inc i) nil))))
            (let [insert-pos (:pos (nth toks (inc i)))]
              (recur (+ i 2)
                     (conj acc [insert-pos insert-pos "1 "])))
            (recur (inc i) acc)))))))

;; ---------------------------------------------------------------------------
;; AS <reserved-keyword> alias rewriter
;; ---------------------------------------------------------------------------
;; PostgreSQL accepts most reserved keywords as column aliases after
;; `AS` (e.g. `SELECT 1 AS update` is valid PG — `update` becomes the
;; column label). JSqlParser 5's grammar is stricter and errors out on
;; many of these. ORMs that emit privilege/metadata queries frequently
;; use reserved words as aliases (Metabase's `build_privilege_map`
;; uses `AS select, AS update, AS insert, ...`).
;;
;; Rewrite: when we see `AS <reserved-kw>` where `<reserved-kw>` is an
;; unquoted ident that JSqlParser would reject, wrap it in PG's
;; double-quote identifier syntax (`AS "<kw>"`). PG treats both forms
;; as the same alias so the downstream behaviour is unchanged.
;;
;; Must skip `CAST(<expr> AS <type>)` — the `AS` there introduces a
;; type-name, not an alias, and some type names (`int`, `text`, …)
;; overlap with the reserved-word set.

(def ^:private alias-reserved-kws
  "Reserved keywords that JSqlParser 5.x rejects as an unquoted alias
   after `AS`. Empirically determined with
   `SELECT 1 AS <kw>` against 5.2. PG accepts all of these; we just
   need to double-quote them before JSqlParser sees them."
  #{"select" "from" "where" "group" "order" "having"
    "and" "or" "not" "in" "like" "between"
    "when" "else" "join" "union" "intersect" "except"
    "as" "with" "all" "any" "some" "exists"
    "null" "true" "false" "is" "on"
    "asc" "desc" "cross" "inner" "outer" "left" "right" "full"
    "limit" "offset" "fetch" "for" "of"
    "by" "into" "values" "returning" "using" "sample"})

(defn- inside-cast-parens?
  "True if the token at idx sits inside an unmatched paren group opened
   by a `CAST` keyword (ignoring nested plain-paren groups). Used to
   distinguish `CAST(x AS int)` (type context) from `SELECT x AS int`
   (alias context)."
  [toks ^long idx]
  (loop [i (dec idx), depth 0]
    (if (neg? i)
      false
      (let [t (nth toks i)]
        (cond
          (punct? t ")") (recur (dec i) (inc depth))
          (punct? t "(")
          (if (pos? depth)
            (recur (dec i) (dec depth))
            ;; Found the opening paren at our nesting level. Look at
            ;; the previous non-comment token to decide.
            (let [prev (non-comment-before toks i)]
              (= "cast" (kw-text prev))))
          :else (recur (dec i) depth))))))

(defn- create-relation-as?
  "True for the statement-level AS in CREATE TABLE/VIEW name AS SELECT.
   SELECT/WITH/VALUES starts a query here, not a reserved-word alias."
  [toks ^long as-idx]
  (let [prior-kws (keep kw-text (take as-idx toks))
        next-kw (some-> (nth toks (inc as-idx) nil) kw-text)]
    (and (contains? #{"select" "with" "values"} next-kw)
         (= "create" (first prior-kws))
         (not-any? #{"select" "with" "values"} prior-kws)
         (some #{"table" "view"} prior-kws))))

(defn quote-reserved-alias-rule
  "Find `AS <reserved-kw>` outside of `CAST(... AS ...)` contexts and
   replace `<reserved-kw>` with `\"<reserved-kw>\"` so JSqlParser
   accepts it as an identifier. Also quote `sample.` references: JSqlParser
   reserves SAMPLE as query syntax even after PostgreSQL has accepted it as a
   relation alias. PG already treats the quoted lower-case forms equivalently."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)
              next-t (nth toks (inc i) nil)]
          (cond
            ;; `sample.col`: once SAMPLE was used as a relation alias,
            ;; JSqlParser still reads this as its sampling-clause keyword.
            (and (= "sample" (kw-text t)) (punct? next-t "."))
            (recur (inc i) (conj acc [(:pos t) (:end t) "\"sample\""]))

            (not= "as" (kw-text t))
            (recur (inc i) acc)

            :else
            (let [next-t (nth toks (inc i) nil)
                  nt-kw (kw-text next-t)]
              (if (and nt-kw
                       (contains? alias-reserved-kws nt-kw)
                       (not (inside-cast-parens? toks (inc i)))
                       (not (create-relation-as? toks i)))
                (let [start (:pos next-t)
                      end (:end next-t)
                      ;; FOLD before quoting. The source token is
                      ;; unquoted, so PostgreSQL would have lower-cased
                      ;; it; quoting it verbatim here would make the
                      ;; synthetic quotes preserve a case the user never
                      ;; asked to preserve, and `SELECT 1 AS Select`
                      ;; would be labelled `Select` where PG says
                      ;; `select`. The quotes exist only to get the
                      ;; reserved word past the parser.
                      quoted (str "\"" (params/fold-identifier (:text next-t)) "\"")]
                  (recur (+ i 2)
                         (conj acc [start end quoted])))
                (recur (inc i) acc)))))))))

;; ============================================================================
;; COLLATE — strip both qualified (`COLLATE pg_catalog.default`) and bare
;; (`COLLATE "C"` / `COLLATE default`) forms. We don't track collations.
;; ============================================================================

(defn collate-rule
  "Strip `COLLATE <name>`, `COLLATE <qual>.<name>`, `COLLATE \"<name>\"`.

   Token-driven: the matcher only fires on `:ident COLLATE` tokens, so
   the substring `COLLATE` inside a string literal or a comment is
   invisible to it (those are `:string` / `:comment` tokens)."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)]
          (if-not (= "collate" (kw-text t))
            (recur (inc i) acc)
            (let [t1 (nth toks (inc i) nil)
                  t2 (nth toks (+ i 2) nil)
                  t3 (nth toks (+ i 3) nil)]
              (cond
                ;; COLLATE <ident> . <ident>
                (and (ident-tok? t1) (punct? t2 ".")
                     (or (ident-tok? t3) (= :quoted (:type t3))))
                (recur (+ i 4) (conj acc [(:pos t) (:end t3) " "]))
                ;; COLLATE <ident> | COLLATE "<quoted>"
                (or (ident-tok? t1) (= :quoted (:type t1)))
                (recur (+ i 2) (conj acc [(:pos t) (:end t1) " "]))
                :else
                (recur (inc i) acc)))))))))

;; ============================================================================
;; OPERATOR(qual.op) — psql `\d` family emits operator-schema-qualified
;; references. JSqlParser doesn't accept the wrapper. Collapse to the
;; bare op symbol; we don't honour cross-schema operator scoping.
;; ============================================================================

(defn operator-paren-rule
  "Match `OPERATOR ( <ident> . <op> )` and replace the whole span with
   the bare operator symbol. The op token may be any `:op` — single or
   multi-char (`~`, `!~`, `~*`, `||`, etc.)."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)]
          (if-not (= "operator" (kw-text t))
            (recur (inc i) acc)
            (let [t1 (nth toks (inc i) nil)
                  t2 (nth toks (+ i 2) nil)
                  t3 (nth toks (+ i 3) nil)
                  t4 (nth toks (+ i 4) nil)
                  t5 (nth toks (+ i 5) nil)]
              (if (and (punct? t1 "(")
                       (ident-tok? t2)
                       (punct? t3 ".")
                       (= :op (:type t4))
                       (punct? t5 ")"))
                (recur (+ i 6)
                       (conj acc [(:pos t) (:end t5) (:text t4)]))
                (recur (inc i) acc)))))))))

;; ============================================================================
;; ALTER COLUMN <name> DROP DEFAULT — no-op clause. Strip the whole clause
;; (and the trailing comma if present) so the surrounding ALTER TABLE
;; survives.
;; ============================================================================

(defn alter-column-drop-default-rule
  "Match `ALTER COLUMN [<quote>]<name>[<quote>] DROP DEFAULT [,]` anywhere
   in the token stream and remove it. JSqlParser would parse the form
   but our schema can't honour it (we don't store DEFAULT values that
   could be dropped); the regex tail in preprocess-sql used to handle
   this. Token rule is immune to the substring appearing in literals or
   comments."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i (- n 4))
        acc
        (let [t (nth toks i)]
          (if-not (= "alter" (kw-text t))
            (recur (inc i) acc)
            (let [t1 (nth toks (inc i) nil)
                  t2 (nth toks (+ i 2) nil)
                  t3 (nth toks (+ i 3) nil)
                  t4 (nth toks (+ i 4) nil)
                  t5 (nth toks (+ i 5) nil)]
              (if (and (= "column" (kw-text t1))
                       (or (ident-tok? t2) (= :quoted (:type t2)))
                       (= "drop" (kw-text t3))
                       (= "default" (kw-text t4)))
                (let [end-pos (if (punct? t5 ",")
                                (:end t5)
                                (:end t4))
                      next-i (if (punct? t5 ",") (+ i 6) (+ i 5))]
                  (recur next-i (conj acc [(:pos t) end-pos " "])))
                (recur (inc i) acc)))))))))

;; ============================================================================
;; (PRIMARY KEY (<col>)) — INHERITS table body that contains only a PK.
;; Replace the outer parenthesised body with `(id serial)` so JSqlParser
;; sees a viable column list. We don't enforce inherited PKs on the child;
;; this rewrite is purely to make INHERITS bootstrap parse cleanly.
;; ============================================================================

(defn primary-key-only-body-rule
  "Match `( PRIMARY KEY ( <ident> [, <ident>]* ) )` as a complete
   parenthesised body — i.e. nothing else inside the outer `(...)` —
   and replace with `(id serial)`. Used by Odoo's bootstrap-DDL where
   tables declared with `INHERITS (parent)` carry only a PK."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)]
          (if-not (punct? t "(")
            (recur (inc i) acc)
            (let [t1 (nth toks (inc i) nil)
                  t2 (nth toks (+ i 2) nil)
                  t3 (nth toks (+ i 3) nil)]
              (if (and (= "primary" (kw-text t1))
                       (= "key" (kw-text t2))
                       (punct? t3 "("))
                (let [;; Walk to the inner `)` matching t3.
                      inner-end-idx (loop [k (inc (+ i 3)), depth 1]
                                      (let [x (nth toks k nil)]
                                        (cond
                                          (nil? x) nil
                                          (punct? x "(") (recur (inc k) (inc depth))
                                          (punct? x ")") (if (= depth 1)
                                                           k
                                                           (recur (inc k) (dec depth)))
                                          :else (recur (inc k) depth))))
                      ;; The token immediately after the inner `)` must be
                      ;; the outer `)`. Otherwise the body has more than
                      ;; just `PRIMARY KEY(...)` and we leave it alone.
                      outer-end (when inner-end-idx
                                  (let [x (nth toks (inc inner-end-idx) nil)]
                                    (when (punct? x ")") x)))]
                  (if outer-end
                    (recur (+ inner-end-idx 2)
                           (conj acc [(:pos t) (:end outer-end) "(id serial)"]))
                    (recur (inc i) acc)))
                (recur (inc i) acc)))))))))

;; ============================================================================
;; ALTER TABLE … TYPE <type> USING <expr> — strip the USING half.
;; PG accepts a USING clause to convert column data; we treat the rewrite
;; as a no-op so callers can issue the same DDL they'd issue against PG.
;; ============================================================================

(defn type-using-rule
  "Match `TYPE <ident>[(...)] [<more idents>]* USING <anything>` and strip
   from `USING` to the end of the statement (next `;` at depth 0 or
   end of input).

   The `TYPE`-prefixed gating is what makes this safe: a bare `USING`
   keyword (e.g. JOIN ... USING (col)) never has a preceding `TYPE`
   token in the same clause, so it isn't matched."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (let [t (nth toks i)]
          (if-not (= "type" (kw-text t))
            (recur (inc i) acc)
            ;; Walk forward looking for USING. Stop at `;` or EOF.
            (let [using-idx
                  (loop [k (inc i), depth 0]
                    (let [x (nth toks k nil)]
                      (cond
                        (nil? x) nil
                        (punct? x "(") (recur (inc k) (inc depth))
                        (punct? x ")") (recur (inc k) (max 0 (dec depth)))
                        (and (zero? depth) (punct? x ";")) nil
                        (and (zero? depth) (= "using" (kw-text x))) k
                        :else (recur (inc k) depth))))]
              (if (nil? using-idx)
                (recur (inc i) acc)
                ;; Span: from the USING token to either the next top-
                ;; level `;` or end of input.
                (let [end-idx (loop [k (inc using-idx), depth 0]
                                (let [x (nth toks k nil)]
                                  (cond
                                    (nil? x) k
                                    (punct? x "(") (recur (inc k) (inc depth))
                                    (punct? x ")") (recur (inc k) (max 0 (dec depth)))
                                    (and (zero? depth) (punct? x ";")) k
                                    :else (recur (inc k) depth))))
                      using-tok (nth toks using-idx)
                      end-pos (if (and (< end-idx n)
                                       (= ";" (:text (nth toks end-idx))))
                                (:pos (nth toks end-idx))
                                (:end (nth toks (dec end-idx))))]
                  (recur end-idx
                         (conj acc [(:pos using-tok) end-pos " "])))))))))))

;; ============================================================================
;; INDEX/KEY varchar — quote a reserved-word column name. PG accepts
;; `index varchar` as a column declaration; JSqlParser rejects the
;; reserved word in column position. Quote it so the AST builds.
;; ============================================================================

(defn reserved-column-name-rule
  "Match `<INDEX|KEY> varchar` and quote the first ident: `\"INDEX\" varchar`."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i (dec n))
        acc
        (let [t  (nth toks i)
              t1 (nth toks (inc i) nil)
              kw (kw-text t)]
          (if (and (or (= "index" kw) (= "key" kw))
                   (= "varchar" (kw-text t1)))
            (recur (+ i 2)
                   (conj acc [(:pos t) (:end t)
                              (str "\"" (:text t) "\"")]))
            (recur (inc i) acc)))))))

;; ============================================================================
;; <expr> IS [NOT] (TRUE|FALSE|UNKNOWN) — JSqlParser only accepts a
;; "boolean primary" on the left of IS. `IN (…)`, `EXISTS (…)`,
;; comparison expressions etc. parse fine standalone but trip the
;; grammar when followed by IS. Wrap the LHS in parens — this lifts
;; the arbitrary predicate to a boolean primary.
;;
;; Surfaced by Odoo's view-loading queries (`<col> IN (SELECT …) IS NOT TRUE`).
;; ============================================================================

(def ^:private is-bool-clause-boundaries
  "Tokens that delimit a boolean expression in WHERE / ON / HAVING /
   CASE-WHEN context. We walk back from `IS` over balanced parens until
   one of these (at depth 0) — that token sits one before the LHS."
  #{"where" "and" "or" "having" "on" "when" "then" "else" "by" "select"})

(defn- bool-is-tail?
  "True if `(IS [NOT] (TRUE|FALSE|UNKNOWN))` starts at index i. Returns
   the index of the boolean literal on match, nil otherwise."
  [toks ^long i]
  (when (= "is" (kw-text (nth toks i nil)))
    (let [t1 (nth toks (inc i) nil)
          not? (= "not" (kw-text t1))
          bl-idx (if not? (+ i 2) (inc i))
          bl (nth toks bl-idx nil)]
      (when (#{"true" "false" "unknown"} (kw-text bl))
        bl-idx))))

(defn boolean-is-rule
  "Wrap the LHS of `<expr> IS [NOT] (TRUE|FALSE|UNKNOWN)` in parens
   when it isn't already a single parenthesised group. JSqlParser's
   grammar requires a `boolean_primary` on the left; an `IN (…)` or
   `EXISTS (…)` parses standalone but trips when followed by IS.

   Walking back: balanced-paren walk until a clause-boundary keyword
   (WHERE / AND / OR / …), comma, semicolon, or unmatched `(` — the
   token immediately after that boundary is where the LHS starts."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i n)
        acc
        (if-not (bool-is-tail? toks i)
          (recur (inc i) acc)
          (let [boundary-idx
                (loop [k (dec i), depth 0]
                  (cond
                    (neg? k) -1
                    (punct? (nth toks k) ")") (recur (dec k) (inc depth))
                    (and (zero? depth) (punct? (nth toks k) "(")) k
                    (punct? (nth toks k) "(") (recur (dec k) (dec depth))
                    (and (zero? depth)
                         (or (is-bool-clause-boundaries (kw-text (nth toks k)))
                             (= "," (:text (nth toks k)))
                             (= ";" (:text (nth toks k)))))
                    k
                    :else (recur (dec k) depth)))
                lhs-start-idx (inc boundary-idx)
                lhs-start-tok (nth toks lhs-start-idx nil)
                lhs-end-tok   (nth toks (dec i) nil)]
            (if (or (nil? lhs-start-tok) (nil? lhs-end-tok)
                    ;; LHS is already exactly one parenthesised group.
                    (and (punct? lhs-start-tok "(")
                         (= (dec i)
                            ;; Find the matching `)` index from lhs-start-idx
                            (loop [k (inc lhs-start-idx), d 1]
                              (cond
                                (>= k i) -1
                                (punct? (nth toks k) "(") (recur (inc k) (inc d))
                                (punct? (nth toks k) ")") (if (= d 1) k (recur (inc k) (dec d)))
                                :else (recur (inc k) d))))))
              (recur (inc i) acc)
              (recur (inc i)
                     (-> acc
                         (conj [(:pos lhs-start-tok) (:pos lhs-start-tok) "("])
                         (conj [(:end lhs-end-tok)   (:end lhs-end-tok)   ")"]))))))))))

;; ============================================================================
;; CREATE / ALTER SEQUENCE used to need two rules here — one to strip
;; `NO MINVALUE`/`NO MAXVALUE`/`NO CYCLE`, one to strip `IF NOT EXISTS` —
;; because JSqlParser's CreateSequence grammar has no production for
;; either. Both are gone: sequence DDL is now token-classified in full
;; (classify/classify-create-sequence) and never reaches JSqlParser, so
;; the option list is parsed rather than deleted before parsing. See
;; issue #21.
;; ============================================================================

;; ============================================================================
;; CREATE TABLE … (cols) PARTITION BY <strategy> (<expr>) — JSqlParser
;; chokes on the `RANGE` / `LIST` / `HASH` keyword after the closing `)`
;; of the column definition list. We don't model partitioning; pg_dump
;; emits one CREATE TABLE per partition child anyway, and the data
;; lands in the children. Strip the trailing `PARTITION BY …` clause
;; so the parent table parses as a normal (empty) base table.
;;
;; Match shape (after the column-def `)`):
;;   PARTITION BY <ident>(RANGE|LIST|HASH) ( <balanced-paren-group> )
;; ============================================================================

(defn partition-by-rule
  "Strip `PARTITION BY <strategy> (<expr>)` after a top-level CREATE TABLE
   body. Replaces the matched span with a single space; the trailing
   `;` stays in place so statement boundaries are unaffected.

   Walks the token stream looking for `partition` `by` <ident>
   followed by a `(...)` group. The clause is paired with a CREATE
   TABLE — not a CREATE INDEX or other DDL — but the rule doesn't
   need that context: PARTITION BY only appears in CREATE TABLE in
   any well-formed PG SQL, and the pre-parse rewrite is conservative
   (we'd at worst delete a syntactically-similar but semantically-
   absurd substring elsewhere)."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i (- n 3))
        acc
        (let [t0 (nth toks i)
              t1 (nth toks (inc i) nil)
              t2 (nth toks (+ i 2) nil)
              t3 (nth toks (+ i 3) nil)]
          (if (and (= "partition" (kw-text t0))
                   (= "by" (kw-text t1))
                   (#{"range" "list" "hash"} (kw-text t2))
                   (punct? t3 "("))
            (let [close-idx (loop [k (+ i 4), depth 1]
                              (cond
                                (>= k n) -1
                                (punct? (nth toks k) "(") (recur (inc k) (inc depth))
                                (punct? (nth toks k) ")")
                                (if (= depth 1) k (recur (inc k) (dec depth)))
                                :else (recur (inc k) depth)))]
              (if (neg? close-idx)
                (recur (inc i) acc)
                (let [start-pos (:pos t0)
                      end-pos (:end (nth toks close-idx))]
                  (recur (inc close-idx)
                         (conj acc [start-pos end-pos " "])))))
            (recur (inc i) acc)))))))

;; ============================================================================
;; DEFAULT <fn>(<args>) — JSqlParser's grammar rejects a function call with
;; a string-literal argument in a DEFAULT clause (5.2 and 5.3 — the parser
;; expects `::` after the close paren, hinting at PG cast-syntax ambiguity
;; in the DEFAULT-expression production). The exact same call wrapped in
;; an extra paren — `DEFAULT (nextval('seq'))` — parses cleanly with
;; identical AST. So we rewrite source-level.
;;
;; This unblocks pg_dump output, which always emits `DEFAULT nextval(<seq>)`
;; for SERIAL/BIGSERIAL/IDENTITY columns.
;; ============================================================================

(def ^:private default-paren-fns
  "Sequence-fns that JSqlParser stumbles on in DEFAULT position."
  #{"nextval" "currval" "lastval"})

(defn default-fn-call-paren-rule
  "Match `DEFAULT <fn>(...)` for fn ∈ {nextval, currval, lastval} where
   the call isn't already wrapped in extra parens, and inject parens
   around the call. Same semantics, parser-friendly form.

   Skipped when the token after DEFAULT is already `(` — assume the
   user already wrapped, leave alone."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i (- n 3))
        acc
        (let [t0 (nth toks i)]
          (if-not (= "default" (kw-text t0))
            (recur (inc i) acc)
            (let [t1 (nth toks (inc i) nil)
                  t2 (nth toks (+ i 2) nil)]
              (cond
                ;; Already parenthesised — caller did the dance.
                (punct? t1 "(") (recur (inc i) acc)

                ;; <fn>(...)
                (and (some? t1)
                     (contains? default-paren-fns (kw-text t1))
                     (punct? t2 "("))
                (let [close-idx
                      (loop [k (+ i 3), depth 1]
                        (cond
                          (>= k n) -1
                          (punct? (nth toks k) "(") (recur (inc k) (inc depth))
                          (punct? (nth toks k) ")")
                          (if (= depth 1) k (recur (inc k) (dec depth)))
                          :else (recur (inc k) depth)))]
                  (if (neg? close-idx)
                    (recur (inc i) acc)
                    (let [open-pos  (:pos t1)
                          close-end (:end (nth toks close-idx))]
                      (recur (inc close-idx)
                             (-> acc
                                 (conj [open-pos open-pos "("])
                                 (conj [close-end close-end ")"]))))))

                :else (recur (inc i) acc)))))))))

(defn json-path-operator-spacing-rule
  "Put a lexical boundary around PostgreSQL's `#>` and `#>>` operators.

   JSqlParser parses the spaced forms as JsonExpression, but reads a tight
   `value::jsonb#>array[...]` as the ordinary `>` comparison between a cast
   whose type name absorbed `#` and the array. PostgreSQL's regression SQL
   uses that whitespace-free spelling. The tokenizer already recognizes the
   complete operator and excludes strings/comments, so this rewrite is both
   narrow and representation-independent."
  [toks]
  (keep (fn [{:keys [type text pos end]}]
          (when (and (= :op type) (#{"#>" "#>>"} text))
            [pos end (str " " text " ")]))
        toks))

(defn hash-xor-rule
  "Rewrite PostgreSQL's bare `#` XOR operator to JSqlParser's `XOR`
   keyword. The tokenizer keeps strings, quoted identifiers and comments
   opaque, and the exact-token check leaves JSON `#>` / `#>>` untouched.

   JSqlParser assigns XOR a lower precedence than `&` / `|` / shifts;
   expr/normalize-xor-tree corrects that AST grouping after parsing."
  [toks]
  (keep (fn [{:keys [type text pos end]}]
          (when (and (= :op type) (= "#" text))
            [pos end " XOR "]))
        toks))

(defn negative-numeric-scale-rule
  "Encode a negative NUMERIC/DECIMAL scale for JSqlParser.

   PostgreSQL stores scales as signed 11-bit values in atttypmod, while
   JSqlParser 5.2 rejects the minus token in `numeric(p,-s)`. Valid positive
   scales stop at 1000, so the packed range 1048..2047 is unambiguous and
   can be decoded immediately by the shared typmod readers. Strings and
  comments remain opaque because this matches classified tokens only."
  [toks]
  (let [significant (vec (remove #(= :comment (:type %)) toks))]
    (into []
          (keep (fn [i]
                  (let [type-tok (nth significant i nil)
                        open     (nth significant (inc i) nil)
                        precision (nth significant (+ i 2) nil)
                        comma    (nth significant (+ i 3) nil)
                        minus    (nth significant (+ i 4) nil)
                        scale    (nth significant (+ i 5) nil)
                        close    (nth significant (+ i 6) nil)]
                    (when (and (contains? #{"numeric" "decimal"} (kw-text type-tok))
                               (punct? open "(")
                               (= :number (:type precision))
                               (punct? comma ",")
                               (= :op (:type minus)) (= "-" (:text minus))
                               (= :number (:type scale))
                               (punct? close ")"))
                      (let [n (try (Long/parseLong (:text scale))
                                   (catch Exception _ nil))]
                        (when (and n (<= 0 n 1000))
                          [(:pos minus) (:end scale)
                           (str (bit-and (- n) 0x7ff))]))))))
          (range (count significant)))))

(defn wide-integer-literal-rule
  "Make integer literals wider than Java long parseable by JSqlParser.

   PostgreSQL assigns an integer constant to numeric when it does not fit
   int4 or int8. JSqlParser instead constructs LongValue and fails before
   we can apply PostgreSQL's type inference. Appending `e0` selects its
   DoubleValue AST without changing the number; our expression lowering
   rebuilds DoubleValue from its original token as BigDecimal, so no
   floating-point conversion or scale change occurs."
  [toks]
  (keep (fn [{:keys [type text end]}]
          (when (and (= :number type)
                     (not (re-find #"[.eE]" text))
                     (try
                       (Long/parseLong text)
                       false
                       (catch NumberFormatException _ true)))
            [end end "e0"]))
        toks))

;; ============================================================================
;; Canonical rule set for preprocess-sql
;; ============================================================================

(def default-rules
  "Token-driven rewrites applied in `datahike.pg.sql/preprocess-sql`
   before JSqlParser sees the SQL. Each rule operates on tokens, not
   on raw source, so a keyword the rule matches inside a string literal
   or comment is invisible to it.

   Order matters only for rules that target the same source span; all
   rules here are disjoint."
  [inline-references-rule
   create-index-anonymous-rule
   select-from-rule
   quote-reserved-alias-rule
   collate-rule
   operator-paren-rule
   alter-column-drop-default-rule
   primary-key-only-body-rule
   type-using-rule
   reserved-column-name-rule
   boolean-is-rule
   default-fn-call-paren-rule
   negative-numeric-scale-rule
   wide-integer-literal-rule
   hash-xor-rule
   json-path-operator-spacing-rule
   partition-by-rule])
