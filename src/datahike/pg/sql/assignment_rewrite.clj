(ns datahike.pg.sql.assignment-rewrite
  "Parser adaptation confined to top-level DML assignment targets.
   Original expression tokens remain intact, including parameter order.
   Omitted slice bounds are represented outside the shared parser AST."
  (:require [clojure.string :as str]
            [datahike.pg.sql.classify :as classify])
  (:import [java.util IdentityHashMap]
           [net.sf.jsqlparser.schema Column]
           [net.sf.jsqlparser.statement.update Update]
           [net.sf.jsqlparser.statement.insert Insert]))

(defn- keyword-token? [token word]
  (and (= :ident (:type token)) (= word (str/lower-case (:text token)))))

(defn- identifier? [token]
  (contains? #{:ident :quoted} (:type token)))

(defn- token-depths [tokens]
  (loop [remaining tokens, depth 0, result []]
    (if-let [token (first remaining)]
      (recur (next remaining)
             (+ depth (case (:text token) ("(" "[" "{") 1 (")" "]" "}") -1 0))
             (conj result depth))
      result)))

(defn- matching-close [tokens start]
  (let [opening (:text (get tokens start))
        closing ({"(" ")" "[" "]" "{" "}"} opening)]
    (when closing
      (loop [i (inc start), stack [closing]]
        (when-let [token (get tokens i)]
          (let [text (:text token)]
            (cond
              (and (= :punct (:type token)) ({"(" ")" "[" "]" "{" "}"} text))
              (recur (inc i) (conj stack ({"(" ")" "[" "]" "{" "}"} text)))
              (and (= :punct (:type token)) (= text (peek stack)))
              (if (= 1 (count stack)) i (recur (inc i) (pop stack)))
              :else (recur (inc i) stack))))))))

(defn- slice-info [tokens opening closing]
  (let [inside (subvec tokens (inc opening) closing)
        depths (token-depths inside)
        _ (when (some (fn [[token depth]]
                        (and (zero? depth) (= "," (:text token))))
                      (map vector inside depths))
            (throw (ex-info "array subscripts must use separate brackets"
                            {:sqlstate "42601"})))
        colons
        (for [[token depth] (map vector inside depths)
              :when (and (zero? depth) (= :op (:type token)))
              i (range (count (:text token)))
              :let [text (:text token)]
              :when (and (= \: (nth text i))
                         (not= \: (get text (dec i)))
                         (not= \: (get text (inc i))))]
          (+ (:pos token) i))]
    (when (next colons)
      (throw (ex-info "invalid array slice" {:sqlstate "42601"})))
    (if-let [colon (first colons)]
      (let [lower? (not-any? #(< (:pos %) colon) inside)
            upper? (not-any? #(> (:end %) (inc colon)) inside)]
        {:flags {:lower-omitted? lower? :upper-omitted? upper?}
         :spans (cond-> []
                  lower? (conj [colon colon "0"])
                  upper? (conj [(inc colon) (inc colon) "0"]))})
      {:flags {:lower-omitted? false :upper-omitted? false} :spans []})))

(defn- statement-start
  "Skip a WITH list structurally: UPDATE and INSERT are legal CTE names."
  [tokens]
  (if-not (keyword-token? (get tokens 0) "with")
    0
    (loop [i (if (keyword-token? (get tokens 1) "recursive") 2 1)]
      (when (identifier? (get tokens i))
        (let [i (inc i)
              i (if (= "(" (:text (get tokens i)))
                  (some-> (matching-close tokens i) inc) i)]
          (when (and i (keyword-token? (get tokens i) "as"))
            (let [i (inc i)
                  i (cond
                      (keyword-token? (get tokens i) "materialized") (inc i)
                      (and (keyword-token? (get tokens i) "not")
                           (keyword-token? (get tokens (inc i)) "materialized")) (+ i 2)
                      :else i)
                  closing (when (= "(" (:text (get tokens i)))
                            (matching-close tokens i))]
              (when closing
                (if (= "," (:text (get tokens (inc closing))))
                  (recur (+ closing 2))
                  (inc closing))))))))))

(defn- update-set-position [tokens statement]
  (let [start (inc statement)
        start (if (keyword-token? (get tokens start) "only") (inc start) start)
        after-table (if (= "(" (:text (get tokens start)))
                      (some-> (matching-close tokens start) inc)
                      (when (identifier? (get tokens start))
                        (loop [i (inc start)]
                          (if (and (= "." (:text (get tokens i)))
                                   (identifier? (get tokens (inc i))))
                            (recur (+ i 2)) i))))]
    (when after-table
      (let [i (if (= "*" (:text (get tokens after-table))) (inc after-table) after-table)
            i (cond
                (keyword-token? (get tokens i) "as") (+ i 2)
                (and (identifier? (get tokens i))
                     (not (keyword-token? (get tokens i) "set"))) (inc i)
                :else i)]
        (when (keyword-token? (get tokens i) "set") i)))))

(defn- target-list
  "Read only the name/indirection portion of a target list or SET LHS."
  [sql tokens start end]
  (loop [i start, result []]
    (if (>= i end)
      result
      (if-not (identifier? (get tokens i))
        (recur (inc i) result)
        (let [name-end (loop [j (inc i)]
                         (if (and (= "." (:text (get tokens j)))
                                  (identifier? (get tokens (inc j))))
                           (recur (+ j 2)) j))
              brackets
              (loop [j name-end, collected []]
                (if (= "[" (:text (get tokens j)))
                  (if-let [closing (matching-close tokens j)]
                    (if (< closing end)
                      (recur (inc closing) (conj collected [j closing]))
                      {:end j :brackets collected})
                    {:end j :brackets collected})
                  {:end j :brackets collected}))
              dimensions (mapv (fn [[opening closing]] (slice-info tokens opening closing))
                               (:brackets brackets))
              joins (mapv (fn [[[_ closing] [opening _]]]
                            (let [left (get tokens closing)
                                  right (get tokens opening)]
                              [(:pos left) (:end right)
                               (str (subs sql (:end left) (:pos right)) ",")]))
                          (partition 2 1 (:brackets brackets)))]
          (recur (max (inc i) (:end brackets))
                 (conj result {:flags (mapv :flags dimensions)
                               :spans (into joins (mapcat :spans dimensions))})))))))

(defn- set-targets [sql tokens depths start]
  (let [end (or (first (for [i (range start (count tokens))
                             :let [token (get tokens i)]
                             :when (and (zero? (get depths i))
                                        (or (= ";" (:text token))
                                            (some #(keyword-token? token %) ["where" "from" "returning"])))]
                         i))
                (count tokens))
        separators (filter #(and (zero? (get depths %))
                                 (= "," (:text (get tokens %))))
                           (range start end))
        boundaries (concat [start] (map inc separators))
        ends (concat separators [end])]
    (vec (mapcat (fn [[lo hi]]
                   (if-let [equal (first (filter #(and (zero? (get depths %))
                                                     ;; The classifier groups adjacent operator
                                                     ;; characters, including `=?` and `=-`.
                                                     ;; Only the first '=' delimits this LHS.
                                                       (str/starts-with? (:text (get tokens %)) "="))
                                                 (range lo hi)))]
                     (target-list sql tokens lo equal)
                     []))
                 (map vector boundaries ends)))))

(defn normalize
  "Return {:sql parser-sql :targets {[kind ordinal] dimension-flags}}.
   Chained brackets are combined only on write targets; omitted slice bounds
   become parser placeholders. Annotation restores omission by Column identity.
   Nested statements and every RHS remain untouched."
  [sql]
  (let [tokens (vec (remove #(= :comment (:type %)) (classify/tokenize-all sql)))
        depths (token-depths tokens)
        top? #(zero? (get depths % -1))
        find-keyword (fn [word start]
                       (first (filter #(and (top? %) (keyword-token? (get tokens %) word))
                                      (range start (count tokens)))))
        statement (statement-start tokens)
        update? (keyword-token? (get tokens statement) "update")
        insert? (keyword-token? (get tokens statement) "insert")
        groups
        (cond
          update?
          (when-let [set-position (update-set-position tokens statement)]
            {:update (set-targets sql tokens depths (inc set-position))})

          insert?
          (let [into (find-keyword "into" (inc statement))
                table-end (when into
                            (loop [j (+ into 2)]
                              (if (= "." (:text (get tokens j)))
                                (recur (+ j 2)) j)))
                table-end (if (and table-end (keyword-token? (get tokens table-end) "as"))
                            (+ table-end 2) table-end)
                opening (when (= "(" (:text (get tokens table-end))) table-end)
                closing (when opening (matching-close tokens opening))
                conflict (first (filter #(and (top? %)
                                              (keyword-token? (get tokens %) "conflict")
                                              (keyword-token? (get tokens (dec %)) "on"))
                                        (range (inc statement) (count tokens))))
                conflict-set (when conflict (find-keyword "set" (inc conflict)))]
            (cond-> {}
              closing (assoc :insert (target-list sql tokens (inc opening) closing))
              conflict-set (assoc :conflict (set-targets sql tokens depths (inc conflict-set)))))
          :else {})
        targets (into {} (for [[kind descriptors] groups
                               [ordinal descriptor] (map-indexed vector descriptors)
                               :when (seq (:flags descriptor))]
                           [[kind ordinal] (:flags descriptor)]))
        spans (sort-by first > (mapcat :spans (mapcat val groups)))]
    {:sql (reduce (fn [text [start end replacement]]
                    (str (subs text 0 start) replacement (subs text end)))
                  sql spans)
     :targets targets}))

(defn annotate
  "Attach omission metadata to actual cached-AST Column identities externally.
   The returned IdentityHashMap is execution-local; the AST is never mutated."
  [statement targets]
  (let [result (IdentityHashMap.)
        groups (cond
                 (instance? Update statement)
                 {:update (mapcat #(.getColumns %) (.getUpdateSets ^Update statement))}
                 (instance? Insert statement)
                 {:insert (.getColumns ^Insert statement)
                  :conflict (when-let [action (.getConflictAction ^Insert statement)]
                              (mapcat #(.getColumns %) (.getUpdateSets action)))}
                 :else {})]
    (doseq [[kind columns] groups
            [ordinal column] (map-indexed vector columns)
            :let [flags (get targets [kind ordinal])]
            :when flags]
      (when-not (= (count flags)
                   (some-> ^Column column .getArrayConstructor .getExpressions count))
        (throw (ex-info "invalid assignment target subscripts" {:sqlstate "42601"})))
      (.put result column flags))
    (when-not (= (count targets) (.size result))
      (throw (ex-info "invalid assignment target list" {:sqlstate "42601"})))
    result))
