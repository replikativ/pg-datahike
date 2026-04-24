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

(defn inline-references-rule
  "Inline `REFERENCES name [(cols)] [ON (DELETE|UPDATE) action]` —
   strip it (our EAV model doesn't enforce inline FKs; callers must
   use the table-level `FOREIGN KEY (col) REFERENCES …` form to get
   enforcement). Before stripping, if the action is CASCADE / SET
   NULL / SET DEFAULT, raise 0A000 — we can't silently drop an
   unsupported action.

   Distinguishes inline from table-level by checking the previous
   non-comment token: if it's `)` (from `FOREIGN KEY (col)`), we
   leave it alone so JSqlParser parses it as a ForeignKeyIndex."
  [toks]
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
                      end-idx (or (:end-idx on-match) after-cols-idx)]
                  (when-let [act (:action on-match)]
                    (when (contains? #{"cascade" "set null" "set default"} act)
                      (throw (ex-info
                              (str "foreign-key action ON "
                                   (str/upper-case (:verb on-match)) " "
                                   (str/upper-case act)
                                   " is not supported by datahike pgwire")
                              {:sqlstate "0A000"}))))
                  (let [end-pos (:end (nth toks (dec end-idx)))]
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
   select-from-rule])
