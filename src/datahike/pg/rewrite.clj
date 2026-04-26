(ns datahike.pg.rewrite
  "Token-driven SQL source rewrites. Normalize SQL before JSqlParser
   sees it by excising or injecting source-level spans — all based on
   positions captured by the datahike.pg.classify tokenizer.

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
            [datahike.pg.classify :as cls]))

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
                    (throw (ex-info
                            (str "foreign-key action ON "
                                 (str/upper-case (or verb "DELETE")) " "
                                 (str/upper-case act)
                                 " is not supported by datahike pgwire")
                            {:sqlstate "0A000"}))

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
    "by" "into" "values" "returning" "using"})

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

(defn quote-reserved-alias-rule
  "Find `AS <reserved-kw>` outside of `CAST(... AS ...)` contexts and
   replace `<reserved-kw>` with `\"<reserved-kw>\"` so JSqlParser
   accepts it as an identifier. PG already treats the two forms
   equivalently (both produce the same column label)."
  [toks]
  (let [n (count toks)]
    (loop [i 0, acc []]
      (if (>= i (dec n))
        acc
        (let [t (nth toks i)]
          (if-not (= "as" (kw-text t))
            (recur (inc i) acc)
            (let [next-t (nth toks (inc i) nil)
                  nt-kw (kw-text next-t)]
              (if (and nt-kw
                       (contains? alias-reserved-kws nt-kw)
                       ;; Preserve original capitalisation of the token
                       ;; so the quoted alias still equals the user's
                       ;; intent ("Select" vs "select" vs "SELECT").
                       (not (inside-cast-parens? toks (inc i))))
                (let [start (:pos next-t)
                      end (:end next-t)
                      original-text (:text next-t)
                      quoted (str "\"" original-text "\"")]
                  (recur (+ i 2)
                         (conj acc [start end quoted])))
                (recur (inc i) acc)))))))))

;; ============================================================================
;; Canonical rule set for preprocess-sql
;; ============================================================================

(def default-rules
  "Rules replacing the most-error-prone regex replacements in the old
   preprocess-sql. Others (reserved-word quoting, ALTER TABLE TYPE
   USING stripping, complex DEFAULT paren-peel, ALTER COLUMN DROP
   DEFAULT) remain as regex in sql.clj for now — they're narrow
   enough that the regex is low-risk. Migrate them incrementally as
   needed."
  [inline-references-rule
   create-index-anonymous-rule
   select-from-rule
   quote-reserved-alias-rule])
