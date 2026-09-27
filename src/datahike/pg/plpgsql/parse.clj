(ns datahike.pg.plpgsql.parse
  "Parse a plpgsql function body into an AST.

   `pl_gram.y` is the specification. What that grammar actually does --
   and what this does -- is parse the *statement structure* and hand
   every embedded expression and query to the SQL parser untouched.
   plpgsql has no expression grammar of its own: `IF a > b THEN` finds
   the `THEN` and gives `a > b` to the SQL layer as `SELECT a > b`. So
   the job here is to find where each embedded fragment ends, and to
   keep its SOURCE, which the executor later runs through the ordinary
   statement path.

   Everything `pl_gram.y` accepts is parsed, including the constructs
   the executor does not implement. That is deliberate: `CREATE
   FUNCTION` walks the AST and refuses a body that uses one, naming it,
   so an unimplemented construct is a clean 0A000 at definition time
   rather than a wrong answer at call time.

   The tokenizer is the SQL classifier's, so a keyword inside a string,
   a comment, a dollar-quoted body or a quoted identifier is not a
   keyword."
  (:require [clojure.string :as str]
            [datahike.pg.sql.classify :as cls]))

;; ============================================================================
;; Token cursor
;; ============================================================================

(defn- toks-of [^String src]
  (vec (remove #(= :comment (:type %)) (cls/tokenize-all src))))

(defn- tok [ts i] (nth ts i nil))

(defn- kw?
  "Case-insensitive bare-keyword match. A quoted \"IF\" is an identifier."
  [t ^String w]
  (and t (= :ident (:type t)) (.equalsIgnoreCase ^String (:text t) w)))

(defn- kw-in? [t ws]
  (and t (= :ident (:type t)) (contains? ws (str/lower-case (:text t)))))

(defn- punct? [t ^String p] (and t (= :punct (:type t)) (= p (:text t))))

(defn- ident-text [t]
  (case (:type t) :ident (:text t) :quoted (:value t) nil))

(defn- fail! [msg]
  (throw (ex-info msg {:error :syntax-error :sqlstate "42601"})))

(defn- expect-kw [ts i w]
  (if (kw? (tok ts i) w) (inc i) (fail! (str "expected " w))))

(defn- expect-semi [ts i]
  (if (punct? (tok ts i) ";") (inc i) (fail! "expected ;")))

;; ============================================================================
;; Slicing an embedded SQL fragment
;; ============================================================================

(def ^:private block-openers
  "Words that open a nested construct, so a terminator inside one does
   not end the fragment we are slicing."
  #{"case" "if" "loop"})

(defn- fragment
  "The SQL source from token `i` up to the first `stop?` token at
   nesting depth zero. Returns [source-text index-of-stop-token].

   Parens and CASE/IF/LOOP nest: `CASE WHEN a THEN b END` inside an
   expression must not end at its own THEN."
  [^String src ts i stop?]
  (loop [j i, depth 0, case-depth 0]
    (let [t (tok ts j)]
      (cond
        (nil? t)
        (if (> j i)
          [(str/trim (subs src (:pos (tok ts i)) (:end (tok ts (dec j))))) j]
          (fail! "unexpected end of body"))

        (punct? t "(") (recur (inc j) (inc depth) case-depth)
        (punct? t ")") (recur (inc j) (dec depth) case-depth)

        ;; A CASE inside an expression owns its own WHEN/THEN/ELSE/END.
        (and (zero? depth) (kw? t "case")) (recur (inc j) depth (inc case-depth))
        (and (zero? depth) (pos? case-depth) (kw? t "end")) (recur (inc j) depth (dec case-depth))

        (and (zero? depth) (zero? case-depth) (stop? t))
        (if (> j i)
          [(str/trim (subs src (:pos (tok ts i)) (:end (tok ts (dec j))))) j]
          [nil j])

        :else (recur (inc j) depth case-depth)))))

(defn- fragment-to-semi [src ts i]
  (fragment src ts i #(punct? % ";")))

;; ============================================================================
;; Declarations
;; ============================================================================

(defn- parse-declaration
  "`name [CONSTANT] type [COLLATE c] [NOT NULL] [{DEFAULT|:=|=} expr] ;`

   A cursor declaration (`name CURSOR [(args)] FOR query`) and an ALIAS
   are parsed to their own node so CREATE FUNCTION can refuse them by
   name rather than mis-reading them as a variable."
  [src ts i]
  (let [nm (ident-text (tok ts i))]
    (when-not nm (fail! "expected a declaration name"))
    (let [i (inc i)]
      (cond
        (kw? (tok ts i) "alias")
        (let [[_ j] (fragment-to-semi src ts i)]
          [{:node :alias-declaration :name nm} (expect-semi ts j)])

        (or (kw? (tok ts i) "cursor")
            (and (kw? (tok ts i) "no") (kw? (tok ts (inc i)) "scroll"))
            (kw? (tok ts i) "scroll"))
        (let [[_ j] (fragment-to-semi src ts i)]
          [{:node :cursor-declaration :name nm} (expect-semi ts j)])

        :else
        (let [constant? (kw? (tok ts i) "constant")
              i (cond-> i constant? inc)
              ;; The type runs to DEFAULT / := / = / NOT NULL / ;
              [type-src j] (fragment src ts i
                                     #(or (punct? % ";")
                                          (kw? % "default")
                                          (kw? % "not")
                                          (and (= :op (:type %))
                                               (contains? #{":=" "="} (:text %)))))
              [not-null? j] (if (and (kw? (tok ts j) "not") (kw? (tok ts (inc j)) "null"))
                              [true (+ j 2)]
                              [false j])
              [default j] (if (or (kw? (tok ts j) "default")
                                  (and (= :op (:type (tok ts j)))
                                       (contains? #{":=" "="} (:text (tok ts j)))))
                            (fragment-to-semi src ts (inc j))
                            [nil j])]
          [{:node :declaration :name nm :type (some-> type-src str/trim)
            :constant? (boolean constant?) :not-null? not-null?
            :default default}
           (expect-semi ts j)])))))

;; ============================================================================
;; Statements
;; ============================================================================

(declare parse-block parse-statements parse-statement)

(def ^:private statement-enders
  #{"end" "else" "elsif" "elseif" "exception" "when"})

(defn- parse-statements
  "Statements until a word that ends the enclosing construct. Returns
   [stmts next-index]; the ender itself is left for the caller."
  [src ts i]
  (loop [i i, out []]
    (let [t (tok ts i)]
      (cond
        (nil? t) [out i]
        (kw-in? t statement-enders) [out i]
        (punct? t ";") (recur (inc i) out)              ; a stray separator
        :else (let [[stmt j] (parse-statement src ts i)]
                (recur j (cond-> out stmt (conj stmt))))))))

(defn- parse-raise-using
  "`USING opt = expr [, …]` — the option values are expressions, so each
   runs to the next top-level comma or the semicolon."
  [src ts i]
  (if-not (kw? (tok ts i) "using")
    [nil i]
    (loop [i (inc i), out {}]
      (let [k (some-> (ident-text (tok ts i)) str/lower-case)
            i (inc i)
            i (if (and (tok ts i)
                       (or (punct? (tok ts i) "=")
                           (and (= :op (:type (tok ts i)))
                                (contains? #{"=" ":="} (:text (tok ts i))))))
                (inc i)
                (fail! "expected = in a RAISE USING option"))
            [v j] (fragment src ts i #(or (punct? % ";") (punct? % ",")))
            out (cond-> out k (assoc (keyword k) v))]
        (if (punct? (tok ts j) ",")
          (recur (inc j) out)
          [out j])))))

(defn- parse-raise
  "PostgreSQL's four RAISE shapes, per `pl_gram.y`:

     RAISE [level] 'format' [, expr…] [USING …];
     RAISE [level] condition_name    [USING …];
     RAISE [level] SQLSTATE 'xxxxx'  [USING …];
     RAISE [level]                    USING …;
     RAISE;                                        -- re-raise"
  [src ts i]
  (let [i (inc i)                                        ; past RAISE
        levels #{"debug" "log" "info" "notice" "warning" "exception"}
        [level i] (if (kw-in? (tok ts i) levels)
                    [(str/upper-case (:text (tok ts i))) (inc i)]
                    ["EXCEPTION" i])]
    (cond
      ;; bare RAISE; — re-raise the current exception
      (punct? (tok ts i) ";")
      [{:node :raise :level level :reraise? true} (inc i)]

      ;; RAISE [level] USING …;
      (kw? (tok ts i) "using")
      (let [[opts j] (parse-raise-using src ts i)]
        [{:node :raise :level level :options opts} (expect-semi ts j)])

      ;; RAISE [level] SQLSTATE 'xxxxx' [USING …];
      (kw? (tok ts i) "sqlstate")
      (let [lit (tok ts (inc i))
            _ (when-not (= :string (:type lit)) (fail! "expected a SQLSTATE literal"))
            [opts j] (parse-raise-using src ts (+ i 2))]
        [{:node :raise :level level :sqlstate (:value lit) :options opts}
         (expect-semi ts j)])

      ;; RAISE [level] 'format' [, expr…] [USING …];
      (= :string (:type (tok ts i)))
      (let [fmt (:value (tok ts i))
            [args j] (loop [i (inc i), out []]
                       (if (punct? (tok ts i) ",")
                         (let [[e k] (fragment src ts (inc i)
                                               #(or (punct? % ";") (punct? % ",")
                                                    (kw? % "using")))]
                           (recur k (conj out e)))
                         [out i]))
            [opts k] (parse-raise-using src ts j)]
        [{:node :raise :level level :format fmt :args args :options opts}
         (expect-semi ts k)])

      ;; RAISE [level] condition_name [USING …];
      (ident-text (tok ts i))
      (let [cnd (str/lower-case (ident-text (tok ts i)))
            [opts j] (parse-raise-using src ts (inc i))]
        [{:node :raise :level level :condition cnd :options opts}
         (expect-semi ts j)])

      :else (fail! "could not read a RAISE statement"))))

(defn- parse-if [src ts i]
  (let [read-branch
        (fn [i]
          (let [[c j] (fragment src ts i #(kw? % "then"))
                j (expect-kw ts j "then")
                [body k] (parse-statements src ts j)]
            [[c body] k]))]
    (loop [i (inc i), branches []]
      (let [[br j] (read-branch i)
            branches (conj branches br)
            t (tok ts j)]
        (cond
          (or (kw? t "elsif") (kw? t "elseif")) (recur (inc j) branches)
          (kw? t "else")
          (let [[body k] (parse-statements src ts (inc j))
                k (expect-kw ts k "end")
                k (expect-kw ts k "if")]
            [{:node :if :branches branches :else body} (expect-semi ts k)])
          :else
          (let [k (expect-kw ts j "end")
                k (expect-kw ts k "if")]
            [{:node :if :branches branches} (expect-semi ts k)]))))))

(defn- parse-case
  "`CASE [expr] WHEN … THEN … [ELSE …] END CASE;` — the statement form."
  [src ts i]
  (let [i (inc i)
        [subject i] (if (kw? (tok ts i) "when") [nil i] (fragment src ts i #(kw? % "when")))]
    (loop [i i, branches []]
      (if (kw? (tok ts i) "when")
        (let [[c j] (fragment src ts (inc i) #(kw? % "then"))
              j (expect-kw ts j "then")
              [body k] (parse-statements src ts j)]
          (recur k (conj branches [c body])))
        (let [[els i] (if (kw? (tok ts i) "else")
                        (parse-statements src ts (inc i))
                        [nil i])
              i (expect-kw ts i "end")
              i (expect-kw ts i "case")]
          [{:node :case :subject subject :branches branches :else els}
           (expect-semi ts i)])))))

(defn- parse-loop-body
  "`LOOP stmts END LOOP [label];`"
  [src ts i label]
  (let [i (expect-kw ts i "loop")
        [body j] (parse-statements src ts i)
        j (expect-kw ts j "end")
        j (expect-kw ts j "loop")
        j (if (ident-text (tok ts j)) (inc j) j)]
    [body (expect-semi ts j)]))

(defn- parse-for
  "`FOR target IN … LOOP … END LOOP;` — three shapes share the header:
   an integer range, a query, and FOREACH over an array."
  [src ts i label]
  (let [i (inc i)                                          ; past FOR
        targets (loop [i i, out []]
                  (if-let [nm (ident-text (tok ts i))]
                    (if (punct? (tok ts (inc i)) ",")
                      (recur (+ i 2) (conj out nm))
                      [(conj out nm) (inc i)])
                    (fail! "expected a FOR target")))
        [targets i] targets
        i (expect-kw ts i "in")
        reverse? (kw? (tok ts i) "reverse")
        i (cond-> i reverse? inc)
        ;; An integer range is `a .. b [BY c]`; anything else is a query.
        [head j] (fragment src ts i #(kw? % "loop"))
        dotdot (when head
                 (re-find #"(?s)^(.*?)\.\.(.*)$" head))]
    (if (and dotdot (not (re-find #"(?i)^\s*select\b" (str head))))
      (let [[_ from rest-src] dotdot
            [to by] (if-let [m (re-find #"(?is)^(.*?)\bby\b(.*)$" rest-src)]
                      [(nth m 1) (nth m 2)]
                      [rest-src nil])
            [body k] (parse-loop-body src ts j label)]
        [{:node :for-range :var (first targets) :from (str/trim from)
          :to (str/trim to) :by (some-> by str/trim) :reverse? (boolean reverse?)
          :body body :label label}
         k])
      (let [[body k] (parse-loop-body src ts j label)]
        [{:node :for-query :vars targets :query head :body body :label label} k]))))

(defn- parse-foreach [src ts i label]
  (let [i (inc i)
        targets (loop [i i, out []]
                  (if-let [nm (ident-text (tok ts i))]
                    (if (punct? (tok ts (inc i)) ",")
                      (recur (+ i 2) (conj out nm))
                      [(conj out nm) (inc i)])
                    (fail! "expected a FOREACH target")))
        [targets i] targets
        [slice i] (if (kw? (tok ts i) "slice")
                    [(:text (tok ts (inc i))) (+ i 2)]
                    [nil i])
        i (expect-kw ts i "in")
        i (expect-kw ts i "array")
        [arr j] (fragment src ts i #(kw? % "loop"))
        [body k] (parse-loop-body src ts j label)]
    [{:node :foreach :vars targets :slice slice :array arr :body body :label label} k]))

(defn- parse-into
  "The `INTO [STRICT] a, b` a query statement may carry, lifted out so
   the rest is ordinary SQL.

   Only `SELECT … INTO` and `… RETURNING … INTO` are this clause.
   `INSERT INTO t VALUES (…)` is not, and treating it as one turned the
   statement into `INSERT  VALUES (…)` and a parse error at the comma."
  [^String q]
  (when q
    (let [select? (re-find #"(?is)^\s*select\b" q)
          returning? (re-find #"(?is)\breturning\b" q)
          pattern (if returning?
                    #"(?is)^(.*\breturning\b.*?)\binto\s+(strict\s+)?([a-zA-Z_][\w$]*(?:\s*,\s*[a-zA-Z_][\w$]*)*)\s*(.*)$"
                    #"(?is)^(.*?)\binto\s+(strict\s+)?([a-zA-Z_][\w$]*(?:\s*,\s*[a-zA-Z_][\w$]*)*)\s*(.*)$")]
      (if-let [m (and (or select? returning?) (re-find pattern q))]
        {:query (str/trim (str (nth m 1) " " (nth m 4)))
         :targets (mapv str/trim (str/split (nth m 3) #","))
         :strict? (boolean (nth m 2))}
        {:query q}))))

(defn- parse-statement [src ts i]
  (let [t (tok ts i)
        w (some-> (ident-text t) str/lower-case)
        ;; `<<label>>` prefixes a loop or a block.
        [label i w t]
        (if (and (= :op (:type t)) (= "<<" (:text t)))
          (let [nm (ident-text (tok ts (inc i)))
                i (+ i 3)                                  ; name and `>>`
                t (tok ts i)]
            [nm i (some-> (ident-text t) str/lower-case) t])
          [nil i w t])]
    (case w
      "declare" (parse-block src ts i label)
      "begin"   (parse-block src ts i label)
      "if"      (parse-if src ts i)
      "case"    (parse-case src ts i)
      "raise"   (parse-raise src ts i)

      "loop" (let [[body j] (parse-loop-body src ts i label)]
               [{:node :loop :body body :label label} j])

      "while" (let [[c j] (fragment src ts (inc i) #(kw? % "loop"))
                    [body k] (parse-loop-body src ts j label)]
                [{:node :while :cond c :body body :label label} k])

      "for"     (parse-for src ts i label)
      "foreach" (parse-foreach src ts i label)

      ("exit" "continue")
      (let [i (inc i)
            [lbl i] (if (and (ident-text (tok ts i)) (not (kw? (tok ts i) "when")))
                      [(ident-text (tok ts i)) (inc i)]
                      [nil i])
            [cnd i] (if (kw? (tok ts i) "when")
                      (fragment-to-semi src ts (inc i))
                      [nil i])]
        [{:node (if (= w "exit") :exit :continue) :label lbl :when cnd}
         (expect-semi ts i)])

      "return"
      (cond
        (kw? (tok ts (inc i)) "next")
        (let [[e j] (fragment-to-semi src ts (+ i 2))]
          [{:node :return-next :expr e} (expect-semi ts j)])

        (kw? (tok ts (inc i)) "query")
        (if (kw? (tok ts (+ i 2)) "execute")
          (let [[e j] (fragment-to-semi src ts (+ i 3))]
            [{:node :return-query-execute :expr e} (expect-semi ts j)])
          (let [[q j] (fragment-to-semi src ts (+ i 2))]
            [{:node :return-query :query q} (expect-semi ts j)]))

        :else
        (let [[e j] (fragment-to-semi src ts (inc i))]
          [{:node :return :expr e} (expect-semi ts j)]))

      "perform"
      (let [[q j] (fragment-to-semi src ts (inc i))]
        [{:node :perform :query (str "SELECT " q)} (expect-semi ts j)])

      "execute"
      (let [[e j] (fragment-to-semi src ts (inc i))]
        [{:node :execute :expr e} (expect-semi ts j)])

      "get"
      (let [[e j] (fragment-to-semi src ts i)]
        [{:node :get-diagnostics :source e} (expect-semi ts j)])

      "assert"
      (let [[e j] (fragment-to-semi src ts (inc i))]
        [{:node :assert :cond e} (expect-semi ts j)])

      "null"
      [{:node :null} (expect-semi ts (inc i))]

      ("open" "fetch" "close" "move")
      (let [[e j] (fragment-to-semi src ts i)]
        [{:node :cursor-statement :keyword w :source e} (expect-semi ts j)])

      ;; Assignment or a plain SQL statement. `a := e` and `a[1] := e`
      ;; and `a.b := e` are assignments; everything else is SQL that the
      ;; ordinary statement path runs.
      (let [[lhs j] (fragment src ts i
                              #(and (= :op (:type %)) (contains? #{":=" "="} (:text %))))]
        (if (and lhs
                 (= :op (:type (tok ts j)))
                 (contains? #{":=" "="} (:text (tok ts j)))
                 ;; only a bare target, not `WHERE a = b` mid-statement
                 (re-matches #"[A-Za-z_][\w$]*(\s*\.\s*[A-Za-z_][\w$]*)?(\s*\[[^\]]*\])?" lhs))
          (let [[e k] (fragment-to-semi src ts (inc j))]
            [{:node :assign :target (str/trim lhs) :expr e} (expect-semi ts k)])
          (let [[q k] (fragment-to-semi src ts i)]
            [(merge {:node :sql} (parse-into q)) (expect-semi ts k)]))))))

;; ============================================================================
;; Blocks
;; ============================================================================

(defn- parse-exception-handlers
  "`WHEN cond [OR cond …] THEN stmts` — a condition is a name
   (`division_by_zero`, `others`) or `SQLSTATE 'xxxxx'`."
  [src ts i]
  (loop [i i, out []]
    (if (kw? (tok ts i) "when")
      (let [[conds j] (loop [i (inc i), cs []]
                        (let [[c i] (if (kw? (tok ts i) "sqlstate")
                                      [{:sqlstate (:value (tok ts (inc i)))} (+ i 2)]
                                      [(some-> (ident-text (tok ts i)) str/lower-case) (inc i)])
                              cs (cond-> cs c (conj c))]
                          (if (kw? (tok ts i) "or")
                            (recur (inc i) cs)
                            [cs i])))
            j (expect-kw ts j "then")
            [body k] (parse-statements src ts j)]
        (recur k (conj out {:conditions conds :body body})))
      [out i])))

(defn- parse-block
  "`[DECLARE decls] BEGIN stmts [EXCEPTION handlers] END [label];`"
  [src ts i label]
  (let [[decls i] (if (kw? (tok ts i) "declare")
                    (loop [i (inc i), out []]
                      (if (or (kw? (tok ts i) "begin") (nil? (tok ts i)))
                        [out i]
                        (let [[d j] (parse-declaration src ts i)]
                          (recur j (conj out d)))))
                    [[] i])
        i (expect-kw ts i "begin")
        [body i] (parse-statements src ts i)
        [handlers i] (if (kw? (tok ts i) "exception")
                       (parse-exception-handlers src ts (inc i))
                       [nil i])
        i (expect-kw ts i "end")
        i (if (ident-text (tok ts i)) (inc i) i)
        i (if (punct? (tok ts i) ";") (inc i) i)]
    [{:node :block :declarations decls :body body
      :exception-handlers handlers :label label}
     i]))

(def positional-prefix
  "A parameter written `$n` in a body becomes a variable of this name.

   It cannot stay `$n`: the translator rewrites LITERALS to `$n`
   placeholders so a plan can be cached across values, so by the time a
   body's statement is translated its own `$1` is indistinguishable from
   a `0` the rewriter lifted. `SELECT 0` came back as the first
   argument."
  "__plpgsql_arg_")

(defn positional-name [n] (str positional-prefix n))

(defn- name-positional-params
  "Rewrite every `$n` in the body to `positional-name`, using the SQL
   tokenizer so a `$n` inside a string literal or a comment is left
   alone."
  [^String src]
  (let [spans (keep (fn [t]
                      (when (and (= :param (:type t)) (:idx t))
                        [(:pos t) (:end t) (positional-name (:idx t))]))
                    (cls/tokenize-all src))]
    (reduce (fn [acc [start end replacement]]
              (str (subs acc 0 start) replacement (subs acc end)))
            src
            (sort-by first > spans))))

(defn parse-body
  "Parse a plpgsql function body. Returns the top-level `:block` node.
   Raises 42601 with PostgreSQL's SQLSTATE on a body this cannot read."
  [^String src]
  (let [src (name-positional-params src)
        ts (toks-of src)
        ;; plpgsql compiler options -- `#variable_conflict use_column`,
        ;; `#print_strict_params on` -- precede the block and change how
        ;; the body is COMPILED, not what it does. Skipped: the ones
        ;; that matter are about name resolution, which we do not have a
        ;; conflict to resolve yet.
        ts (loop [ts ts]
             (if (and (= :op (:type (first ts))) (= "#" (:text (first ts))))
               (recur (drop-while #(not (or (kw? % "declare") (kw? % "begin"))) ts))
               (vec ts)))
        start (cond
                (kw? (tok ts 0) "declare") 0
                (kw? (tok ts 0) "begin") 0
                (and (= :op (:type (tok ts 0))) (= "<<" (:text (tok ts 0)))) 0
                :else (fail! "a plpgsql body must begin with DECLARE or BEGIN"))
        [block _] (if (and (= :op (:type (tok ts start))) (= "<<" (:text (tok ts start))))
                    (parse-statement src ts start)
                    (parse-block src ts start nil))]
    block))

;; ============================================================================
;; What the executor does not implement
;; ============================================================================

(def implemented-nodes
  "Node kinds the executor runs. `CREATE FUNCTION` refuses a body that
   uses anything else, naming it, so a gap is a 0A000 at definition time
   rather than a wrong answer at call time."
  #{:block :declaration :assign :if :case :loop :while :for-range :for-query
    :exit :continue :return :return-next :return-query :raise :perform :sql
    :null :foreach})

(defn- walk-nodes [node]
  (when (map? node)
    (cons node
          (mapcat walk-nodes
                  (concat (:body node) (:else node) (:declarations node)
                          (mapcat :body (:exception-handlers node))
                          (mapcat second (:branches node)))))))

(defn unsupported-constructs
  "The distinct node kinds in `ast` the executor cannot run, plus
   `:exception-handler` when the body catches. Empty when it can run."
  [ast]
  (let [nodes (walk-nodes ast)]
    (cond-> (into (sorted-set)
                  (comp (map :node) (remove implemented-nodes))
                  nodes)
      (some (comp seq :exception-handlers) nodes) (conj :exception-handler))))
