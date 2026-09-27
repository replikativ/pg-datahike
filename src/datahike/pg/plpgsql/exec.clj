(ns datahike.pg.plpgsql.exec
  "Run a parsed plpgsql body.

   The evaluator has two primitives and everything else is control flow:

     - evaluate an expression  -- run `SELECT <expr>` and take one value
     - run a statement         -- hand the source to the query handler

   Both go through the handler this function was called from, which is
   what SPI is for PostgreSQL: the nested statement sees the same
   transaction, the same temporary tables and the same session. That is
   also why `pl_exec.c` is 9,226 lines and this is not -- most of it is
   memory contexts, plan caching and TupleDesc conversion, none of which
   we have to reproduce.

   A variable reaches an embedded statement as a PARAMETER: the
   statement's references to it are rewritten to `$n` and the values are
   bound per call. That is how PostgreSQL does it, and it is what makes
   the plan cacheable -- resolving a variable to its VALUE during
   translation made the plan specific to that value, so every execution
   re-translated and a body reading one variable cost 11ms a call
   against 0.3ms for one reading none."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]
            [datahike.pg.sql.classify :as cls]
            [datahike.pg.sql.oid-infer :as oid]
            [datahike.pg.sql.cast :as sql-cast]
            [datahike.pg.types :as types]
            [datahike.pg.sql.params :as params])
  (:import [datahike.pg PgWireServer$QueryResult]))

;; ============================================================================
;; Control-flow signals
;; ============================================================================
;;
;; RETURN, EXIT and CONTINUE unwind an arbitrary depth of blocks and
;; loops. Exceptions carry them rather than threading a status through
;; every statement's return value: the body is a tree, and a `RETURN`
;; nested in three loops inside two blocks has to leave all five.

(defrecord Returned [value rows])
(defrecord Exited [label])
(defrecord Continued [label])

(defn- signal! [x] (throw (ex-info "plpgsql control flow" {::signal x})))
(defn- signal-of [e] (::signal (ex-data e)))

;; ============================================================================
;; State
;; ============================================================================

(defn- new-state [handler vars]
  {:handler handler
   ;; A stack of scopes, innermost first. A block pushes one; assignment
   ;; writes to the innermost scope that already holds the name.
   :scopes (atom (list (atom (into {} vars))))
   ;; The DECLARE type of each variable, by name. A statement gives its
   ;; result back as TEXT, and a plpgsql variable is typed, so an
   ;; assignment coerces: without it `t int := 0` held the string "0"
   ;; and `t + 1` was `'0' + 1`.
   :types (atom {})
   ;; Rows accumulated by RETURN NEXT / RETURN QUERY in a set-returning
   ;; function.
   :out (atom [])})

(defn- all-vars
  "Every variable in scope, innermost shadowing outermost."
  [st]
  (reduce (fn [acc scope] (merge @scope acc)) {} @(:scopes st)))

(defn- lookup [st nm]
  (let [nm (str/lower-case nm)]
    (some (fn [scope] (when (contains? @scope nm) [@scope nm])) @(:scopes st))))

(defn- coerce-to-declared
  "A value on its way into a variable, in the variable's declared type.
   PostgreSQL assigns through the type's input function; this is the
   same idea over the text a statement returns."
  [st nm v]
  (let [t (get @(:types st) nm)]
    (if (or (nil? v) (nil? t) (not (string? v)))
      v
      (try (sql-cast/cast-scalar v t {}) (catch Exception _ v)))))

(defn- assign! [st nm v]
  (let [nm (str/lower-case nm)
        v (coerce-to-declared st nm v)]
    (if-let [scope (some (fn [s] (when (contains? @s nm) s)) @(:scopes st))]
      (swap! scope assoc nm v)
      ;; No declaration: PostgreSQL rejects this at compile time. We see
      ;; it at run time, which is still an error rather than a silent
      ;; new variable.
      (throw (errors/pg-error :undefined-column
                              {:message (str "\"" nm "\" is not a known variable")})))))

(defn- push-scope! [st names]
  (swap! (:scopes st) conj (atom (into {} (map (fn [n] [(str/lower-case n) nil])) names))))

(defn- pop-scope! [st] (swap! (:scopes st) rest))

;; ============================================================================
;; The two primitives
;; ============================================================================

(defn- var-reference-spans
  "Where a statement's text refers to a variable in scope, as
   `[start end name]`, longest match first: `r.i` before `r`.

   Token-driven, so a name inside a string literal, a comment or a
   quoted identifier is not a reference, and `t.n` is only a variable
   when a RECORD called `t` is in scope -- otherwise it is a column of
   a table called `t`."
  [^String sql vars]
  (let [toks (vec (remove #(= :comment (:type %)) (cls/tokenize-all sql)))]
    (loop [i 0, out []]
      (if (>= i (count toks))
        out
        (let [t (nth toks i)
              nxt (nth toks (inc i) nil)
              nxt2 (nth toks (+ i 2) nil)
              ident? #(contains? #{:ident :quoted} (:type %))
              dotted (when (and (ident? t) (= "." (:text nxt)) (some-> nxt2 ident?))
                       (str/lower-case (str (:text t) "." (:text nxt2))))
              bare (when (ident? t) (str/lower-case (:text t)))
              prev (nth toks (dec i) nil)]
          (cond
            (and dotted (contains? vars dotted))
            (recur (+ i 3) (conj out [(:pos t) (:end nxt2) dotted]))

            (and bare (contains? vars bare)
                 (not= "." (:text prev))
                 (not= "." (:text nxt)))
            (recur (inc i) (conj out [(:pos t) (:end t) bare]))

            :else (recur (inc i) out)))))))

(def ^:private rewrite-cache
  "The rewrite of a body statement, by `[sql variable-names]`. It
   depends only on the NAMES in scope, never on their values, so one
   entry serves every call and every loop iteration -- and tokenising
   the statement each time was itself a measurable share of the cost."
  (atom {}))

(defn- rewrite-plan
  "`[rewritten-sql ordered-names]` for this statement under these
   variable names."
  [^String sql names]
  (let [k [sql names]]
    (or (get @rewrite-cache k)
        (let [spans (var-reference-spans sql names)
              plan (if (empty? spans)
                     [sql []]
                     (let [order (reduce (fn [acc [_ _ nm]]
                                           (if (contains? acc nm)
                                             acc
                                             (assoc acc nm (inc (count acc)))))
                                         {} spans)]
                       [(reduce (fn [acc [start end nm]]
                                  (str (subs acc 0 start) "$" (get order nm) (subs acc end)))
                                sql
                                (sort-by first > spans))
                        (mapv second (sort-by first (map (fn [[nm i]] [i nm]) order)))]))]
          ;; Bounded: a server that defines new function bodies forever
          ;; should not grow this without limit.
          (when (> (count @rewrite-cache) 4096) (reset! rewrite-cache {}))
          (swap! rewrite-cache assoc k plan)
          plan))))

(defn- parameterise
  "Rewrite a body statement's variable references to `$1 … $n` and
   return `[sql values oids]`.

   This is what makes a body statement CACHEABLE. Binding the variable's
   value during translation made the plan specific to that value, so it
   could not be shared and every execution re-translated: a body reading
   one variable cost 11ms a call against 0.3ms for one reading none, a
   35x difference with nothing else changed. As `$n` the plan is the
   same for every value, which is also how PostgreSQL does it.

   A variable used twice gets one placeholder, so it is passed once."
  [st ^String sql]
  (let [vars (all-vars st)
        [sql' names] (rewrite-plan sql (set (keys vars)))]
    (if (empty? names)
      [sql [] []]
      [sql'
       (mapv #(get vars %) names)
       (mapv #(or (some-> (get @(:types st) %) (oid/sql-type-name->oid nil)) 0) names)])))

(defn- run-sql!
  "Run one statement of the body. Returns the QueryResult; raises the
   statement's own error.

   The statement goes through the PREPARED path -- parse once, bind the
   values -- so its plan is cached across calls and across loop
   iterations. `run` is a function rather than the handler itself: it
   clears the caller's own prepared-statement bindings, which would
   otherwise make every nested statement re-run the outer plan."
  ^PgWireServer$QueryResult [st ^String sql]
  (let [run (:handler st)
        [sql' values oids] (parameterise st sql)
        result (run sql' values oids)]
    (when (.error result)
      (throw (ex-info (.error result)
                      {:sqlstate (or (.sqlstate result) "XX000")
                       :error :plpgsql-statement})))
    result))

(defn- eval-expr
  "The value of a plpgsql expression: `SELECT <expr>` and its one cell.
   nil for SQL NULL."
  [st ^String expr]
  (when (and expr (not (str/blank? expr)))
    (let [r (run-sql! st (str "SELECT " expr))
          rows (.rows r)]
      (when (pos? (alength rows))
        (aget ^"[Ljava.lang.String;" (aget rows 0) 0)))))

(defn- truthy?
  "PostgreSQL's boolean: NULL is not true."
  [v]
  (and (some? v) (contains? #{"t" "true" "TRUE" "T" true} v)))

(defn- ->long [v]
  (cond (number? v) (long v)
        (nil? v) nil
        :else (try (Long/parseLong (str/trim (str v)))
                   (catch Exception _
                     (throw (errors/pg-error
                             :invalid-text-representation
                             {:message (str "loop bound is not an integer: " v)}))))))

;; ============================================================================
;; RAISE
;; ============================================================================

(defn- format-raise
  "plpgsql's RAISE format: `%` takes the next argument, `%%` is a
   literal per cent. An argument that is NULL prints as `<NULL>`."
  [st fmt args]
  (let [vals (mapv #(eval-expr st %) args)]
    (loop [i 0, n 0, out (StringBuilder.)]
      (if (>= i (count fmt))
        (str out)
        (let [c (.charAt ^String fmt i)]
          (cond
            (and (= \% c) (< (inc i) (count fmt)) (= \% (.charAt ^String fmt (inc i))))
            (recur (+ i 2) n (.append out \%))

            (= \% c)
            (recur (inc i) (inc n)
                   (.append out (let [v (nth vals n ::missing)]
                                  (cond (= ::missing v)
                                        (throw (errors/pg-error
                                                :syntax-error
                                                {:message "too few parameters specified for RAISE"}))
                                        (nil? v) "<NULL>"
                                        :else (str v)))))

            :else (recur (inc i) n (.append out c))))))))

(def ^:private condition-sqlstates
  "The condition names plpgsql bodies in the corpus raise by name. The
   full table is PostgreSQL's `errcodes.txt`; these are the ones that
   appear, and an unknown name is its own error rather than a guess."
  {"division_by_zero" "22012"
   "unique_violation" "23505"
   "foreign_key_violation" "23503"
   "not_null_violation" "23502"
   "check_violation" "23514"
   "invalid_text_representation" "22P02"
   "numeric_value_out_of_range" "22003"
   "no_data_found" "P0002"
   "too_many_rows" "P0003"
   "raise_exception" "P0001"
   "others" "P0001"})

(defn- do-raise! [st {:keys [level format args options condition sqlstate reraise?]}]
  (when reraise?
    (throw (errors/pg-error :feature-not-supported
                            {:message "RAISE without arguments outside an exception handler"})))
  (let [msg (cond
              format (format-raise st format args)
              (:message options) (eval-expr st (:message options))
              condition condition
              :else "")
        state (or sqlstate
                  (when condition
                    (or (condition-sqlstates condition)
                        (throw (errors/pg-error
                                :syntax-error
                                {:message (str "unrecognized exception condition \"" condition "\"")}))))
                  (when (:errcode options) (eval-expr st (:errcode options)))
                  (if (= "EXCEPTION" level) "P0001" "00000"))
        fields (cond-> {}
                 (:detail options) (assoc :D (eval-expr st (:detail options)))
                 (:hint options) (assoc :H (eval-expr st (:hint options))))]
    (if (= "EXCEPTION" level)
      (throw (ex-info msg (cond-> {:sqlstate state :error :raise}
                            (seq fields) (assoc :detail (:D fields) :hint (:H fields)))))
      (params/notice! level msg state fields))))

;; ============================================================================
;; Statements
;; ============================================================================

(declare exec-statements! exec-statement! exec-block!)

(defn- into-targets!
  "Bind an `INTO a, b` from the first row of a result. PostgreSQL sets
   the targets to NULL when the query returned nothing, unless STRICT."
  [st {:keys [targets strict?]} ^PgWireServer$QueryResult r]
  (let [rows (.rows r)
        n (alength rows)]
    (when strict?
      (cond
        (zero? n) (throw (ex-info "query returned no rows"
                                  {:sqlstate "P0002" :error :no-data-found}))
        (> n 1) (throw (ex-info "query returned more than one row"
                                {:sqlstate "P0003" :error :too-many-rows}))))
    (let [row (when (pos? n) (aget ^"[Ljava.lang.String;" rows 0))]
      (doseq [[i t] (map-indexed vector targets)]
        (assign! st t (when row (aget ^"[Ljava.lang.String;" row i)))))))

(defn- exec-loop!
  "A loop body, with EXIT / CONTINUE for this label handled. `step`
   runs one iteration and returns false to stop."
  [st label step]
  (loop []
    (let [outcome
          (try (step) true
               (catch clojure.lang.ExceptionInfo e
                 (let [sig (signal-of e)]
                   (cond
                     (and (instance? Exited sig)
                          (or (nil? (:label sig)) (= (:label sig) label)))
                     :stop
                     (and (instance? Continued sig)
                          (or (nil? (:label sig)) (= (:label sig) label)))
                     true
                     :else (throw e)))))]
      (when (true? outcome) (recur)))))

(defn- exec-statement! [st {:keys [node] :as s}]
  (case node
    :null nil

    :block (exec-block! st s)

    :declaration nil                                       ; handled by the block

    :assign (assign! st (:target s) (eval-expr st (:expr s)))

    :sql (let [r (run-sql! st (:query s))]
           (when (seq (:targets s)) (into-targets! st s r)))

    :perform (run-sql! st (:query s))

    :raise (do-raise! st s)

    :return (signal! (->Returned (eval-expr st (:expr s)) nil))

    :return-next (do (swap! (:out st) conj [(eval-expr st (:expr s))]) nil)

    :return-query
    (let [r (run-sql! st (:query s))]
      (swap! (:out st) into (mapv vec (.rows r)))
      nil)

    :if (let [hit (some (fn [[c body]] (when (truthy? (eval-expr st c)) body))
                        (:branches s))]
          (exec-statements! st (or hit (:else s))))

    :case
    (let [subject (when (:subject s) (eval-expr st (:subject s)))
          hit (some (fn [[c body]]
                      (when (if (:subject s)
                              (= subject (eval-expr st c))
                              (truthy? (eval-expr st c)))
                        body))
                    (:branches s))]
      (if (or hit (:else s))
        (exec-statements! st (or hit (:else s)))
        (throw (ex-info "case not found" {:sqlstate "20000" :error :case-not-found}))))

    :loop (exec-loop! st (:label s) #(exec-statements! st (:body s)))

    :while (exec-loop! st (:label s)
                       #(if (truthy? (eval-expr st (:cond s)))
                          (exec-statements! st (:body s))
                          (signal! (->Exited (:label s)))))

    :for-range
    (let [from (->long (eval-expr st (:from s)))
          to (->long (eval-expr st (:to s)))
          by (or (->long (eval-expr st (:by s))) 1)
          _ (when-not (pos? by)
              (throw (errors/pg-error :invalid-parameter-value
                                      {:message "BY value of FOR loop must be greater than zero"})))
          step (if (:reverse? s) (- by) by)
          done? (if (:reverse? s) #(< % to) #(> % to))
          i (atom (if (:reverse? s) from from))]
      (push-scope! st [(:var s)])
      (swap! (:types st) assoc (str/lower-case (:var s)) "bigint")
      (try
        (exec-loop! st (:label s)
                    ;; The counter advances BEFORE the body runs. A
                    ;; CONTINUE leaves the body by throwing, so advancing
                    ;; afterwards meant a CONTINUE never moved the loop
                    ;; on -- `CONTINUE WHEN i % 2 = 1` looped forever.
                    #(if (done? @i)
                       (signal! (->Exited (:label s)))
                       (do (assign! st (:var s) @i)
                           (swap! i + step)
                           (exec-statements! st (:body s)))))
        (finally (pop-scope! st))))

    :for-query
    (let [r (run-sql! st (:query s))
          cols (vec (.columnNames r))
          ;; The row arrives as TEXT; the query says what each column
          ;; actually is, so the loop variables get those types and
          ;; `t := t + r.i` is arithmetic rather than a cast error.
          col-types (mapv #(types/oid->pg-name %) (vec (.columnOids r)))
          rows (vec (map vec (.rows r)))
          vars (:vars s)
          ;; `FOR r IN SELECT …` with ONE target over a multi-column
          ;; query binds a record; we have no record variable yet, so
          ;; each column also becomes a variable of its own name, which
          ;; is what `r.col` would have reached.
          names (if (= 1 (count vars))
                  (into (into vars cols) (map #(str (first vars) "." %)) cols)
                  vars)
          idx (atom 0)]
      (push-scope! st names)
      (doseq [[c t] (map vector cols col-types) :when t]
        (swap! (:types st) assoc (str/lower-case c) t)
        (when (= 1 (count vars))
          (swap! (:types st) assoc (str/lower-case (str (first vars) "." c)) t)))
      (try
        (exec-loop! st (:label s)
                    ;; As above: the cursor advances before the body, so
                    ;; a CONTINUE still moves to the next row.
                    #(if (>= @idx (count rows))
                       (signal! (->Exited (:label s)))
                       (let [row (nth rows @idx)]
                         (if (= 1 (count vars))
                           (do (assign! st (first vars) (first row))
                               ;; A single target over a multi-column
                               ;; query is a RECORD: its fields are
                               ;; reachable both bare and as `r.col`.
                               (doseq [[c v] (map vector cols row)]
                                 (assign! st c v)
                                 (assign! st (str (first vars) "." c) v)))
                           (doseq [[v x] (map vector vars row)] (assign! st v x)))
                         (swap! idx inc)
                         (exec-statements! st (:body s)))))
        (finally (pop-scope! st))))

    :foreach
    (let [arr (eval-expr st (:array s))
          ;; The array arrives as its canonical text; its elements are
          ;; what a one-dimensional unnest would give.
          r (run-sql! st (str "SELECT unnest(" (or (:array s) "NULL") ")"))
          vals (mapv #(aget ^"[Ljava.lang.String;" % 0) (.rows r))
          idx (atom 0)]
      (when (nil? arr) nil)
      (push-scope! st (:vars s))
      (try
        (exec-loop! st (:label s)
                    #(if (>= @idx (count vals))
                       (signal! (->Exited (:label s)))
                       (do (assign! st (first (:vars s)) (nth vals @idx))
                           (swap! idx inc)
                           (exec-statements! st (:body s)))))
        (finally (pop-scope! st))))

    :exit (when (or (nil? (:when s)) (truthy? (eval-expr st (:when s))))
            (signal! (->Exited (:label s))))

    :continue (when (or (nil? (:when s)) (truthy? (eval-expr st (:when s))))
                (signal! (->Continued (:label s))))

    (throw (errors/pg-error
            :feature-not-supported
            {:message (str "plpgsql " (name node) " is not supported")}))))

(defn- exec-statements! [st stmts]
  (doseq [s stmts] (exec-statement! st s))
  nil)

(defn- exec-block! [st {:keys [declarations body]}]
  (push-scope! st (mapv :name declarations))
  (try
    (doseq [{:keys [name type]} declarations]
      (swap! (:types st) assoc (str/lower-case name) type))
    (doseq [{:keys [name default]} declarations]
      (assign! st name (when default (eval-expr st default))))
    (exec-statements! st body)
    (finally (pop-scope! st))))

;; ============================================================================
;; Entry point
;; ============================================================================

(defn run-body
  "Run a parsed plpgsql body with `args` bound, and return
   `{:value v}` for a scalar function or `{:rows [[…] …]}` for a
   set-returning one.

   `args` is a seq of [name value] and `arg-types` the declared type of
   each by the same name. An unnamed parameter is reachable as `$n`,
   which is bound under that name too.

   The types matter: a statement gives its result back as TEXT, so
   without them `a := a - 1` turned an integer parameter into the
   string \"3\" and the next comparison against it was nonsense."
  [handler ast args setof? arg-types]
  (let [st (new-state handler args)
        _ (swap! (:types st) into arg-types)
        returned (try
                   (exec-block! st ast)
                   nil
                   (catch clojure.lang.ExceptionInfo e
                     (let [sig (signal-of e)]
                       (if (instance? Returned sig)
                         sig
                         (throw e)))))]
    (if setof?
      {:rows @(:out st)}
      {:value (:value returned)})))
