(ns datahike.pg.tsearch
  "PostgreSQL-compatible full-text value semantics.

   This namespace owns the SQL-visible normalization and canonical text form.
   A secondary index (Scriptum or otherwise) may use these values to find
   candidates, but must not substitute its parser, analyzer, or scores for
   PostgreSQL semantics."
  (:require [clojure.string :as str]
            [datahike.pg.arrays :as pg-arr])
  (:import [org.tartarus.snowball.ext EnglishStemmer]))

(def ^:private english-stop-words
  ;; PostgreSQL 17 src/backend/snowball/stopwords/english.stop. Keep this
  ;; explicit and versioned with the compatibility implementation: relying on
  ;; Lucene's analyzer stop set would make a dependency update change stored
  ;; tsvectors silently.
  (set (re-seq #"\S+"
               "i me my myself we our ours ourselves you your yours yourself yourselves
         he him his himself she her hers herself it its itself they them their
         theirs themselves what which who whom this that these those am is are
         was were be been being have has had having do does did doing a an the
         and but if or because as until while of at by for with about against
         between into through during before after above below to from up down in
         out on off over under again further then once here there when where why
         how all any both each few more most other some such no nor not only own
         same so than too very s t can will just don should now")))

(def ^:private word-pattern
  ;; The complete PostgreSQL parser distinguishes URLs, emails, versions, and
  ;; many other token classes. This lexical baseline intentionally handles
  ;; Unicode words/numbers only; broader parser conformance remains separately
  ;; measured by the pinned tsearch campaign.
  #"(?U)[\p{L}\p{N}_]+")

(defn- catalog-name [value]
  (let [n (-> value str str/lower-case)]
    (if (str/starts-with? n "pg_catalog.")
      (subs n (count "pg_catalog."))
      n)))

(defn- undefined-text-search-object! [kind value]
  (let [n (str value)]
    ;; errors/pg-error's generic :undefined-object wording is "unrecognized";
    ;; PostgreSQL's regconfig/regdictionary lookup has a more specific public
    ;; diagnostic. Keep both the exact message and the shared SQLSTATE shape.
    (throw (ex-info (str "text search " kind " \"" n "\" does not exist")
                    {:error :undefined-object
                     :sqlstate "42704"
                     :kind (str "text search " kind)
                     :name n}))))

(defn- resolve-config [config]
  (let [n (catalog-name config)]
    (if (contains? #{"english" "simple"} n)
      n
      (undefined-text-search-object! "configuration" config))))

(defn- resolve-dictionary [dictionary]
  (let [n (catalog-name dictionary)]
    (if (contains? #{"english_stem" "simple"} n)
      n
      (undefined-text-search-object! "dictionary" dictionary))))

(defn- stem-english [word]
  (let [stemmer (EnglishStemmer.)]
    (.setCurrent stemmer word)
    (.stem stemmer)
    (.getCurrent stemmer)))

(defn- normalize-token [config token]
  (let [token (str/lower-case token)]
    (cond
      (contains? #{"english" "english_stem"} config)
      (when-not (contains? english-stop-words token)
        (stem-english token))

      (= "simple" config) token)))

(defn- tokens-with-positions [config text]
  (let [config (resolve-config config)]
    (->> (re-seq word-pattern (str text))
         (map-indexed (fn [i token]
                        {:position (inc i)
                         :lexeme (normalize-token config token)}))
         (keep #(when (:lexeme %) %)))))

(defn- quote-lexeme [lexeme]
  (str "'" (-> lexeme
               (str/replace "\\" "\\\\")
               (str/replace "'" "''")) "'"))

(defn- syntax-error! [kind input offset detail]
  (throw (ex-info (str "syntax error in " kind ": \"" input "\"")
                  {:error :syntax-error
                   :sqlstate "42601"
                   :kind kind
                   :input input
                   :offset offset
                   :detail detail})))

(defn- whitespace? [^Character c]
  (Character/isWhitespace c))

(defn- skip-space [^String s i]
  (loop [i i]
    (if (and (< i (.length s)) (whitespace? (.charAt s i)))
      (recur (inc i))
      i)))

(defn- read-quoted
  "Read PostgreSQL's single-quoted tsearch operand. Both doubled quotes and
   backslash escapes are accepted by the tsearch input grammar."
  [kind ^String input start]
  (loop [i (inc start), out (StringBuilder.)]
    (when (>= i (.length input))
      (syntax-error! kind input start "unterminated quoted lexeme"))
    (let [c (.charAt input i)]
      (cond
        (= c \\)
        (if (< (inc i) (.length input))
          (do (.append out (.charAt input (inc i)))
              (recur (+ i 2) out))
          (syntax-error! kind input i "trailing backslash"))

        (= c \')
        (if (and (< (inc i) (.length input))
                 (= \' (.charAt input (inc i))))
          (do (.append out \') (recur (+ i 2) out))
          [(str out) (inc i)])

        :else (do (.append out c) (recur (inc i) out))))))

(defn- read-bare [kind ^String input start delimiter?]
  (loop [i start, out (StringBuilder.)]
    (if (or (>= i (.length input)) (delimiter? (.charAt input i)))
      (if (zero? (.length out))
        (syntax-error! kind input start "expected lexeme")
        [(str out) i])
      (let [c (.charAt input i)]
        (if (= c \\)
          (if (< (inc i) (.length input))
            (do (.append out (.charAt input (inc i)))
                (recur (+ i 2) out))
            (syntax-error! kind input i "trailing backslash"))
          (do (.append out c) (recur (inc i) out)))))))

(defn- read-lexeme [kind ^String input start delimiter?]
  (if (= \' (.charAt input start))
    (read-quoted kind input start)
    (read-bare kind input start delimiter?)))

(def ^:private weight-rank {\D 0, \C 1, \B 2, \A 3})

(defn- merge-positions [positions]
  (->> positions
       (reduce (fn [by-pos {:keys [position weight] :as p}]
                 (let [old (get by-pos position)]
                   (if (or (nil? old)
                           (> (weight-rank weight) (weight-rank (:weight old))))
                     (assoc by-pos position p)
                     by-pos)))
               (sorted-map))
       vals vec))

(defn parse-tsvector
  "Parse a tsvector input value into its canonical semantic representation.

   The returned sorted map associates each lexeme with its ordered, deduplicated
   position/weight vector. It deliberately remains independent of Scriptum:
   secondary indexes may derive candidates from this value, while @@ rechecks
   against this exact representation."
  [value]
  (let [input (str value), n (.length ^String input)]
    (loop [i 0, entries (sorted-map)]
      (let [i (skip-space input i)]
        (if (= i n)
          entries
          (let [[lexeme after-lexeme]
                (read-lexeme "tsvector" input i
                             #(or (whitespace? %) (= % \:)))
                [positions next-i]
                (if (and (< after-lexeme n) (= \: (.charAt input after-lexeme)))
                  (loop [j (inc after-lexeme), result []]
                    (let [digit-start j
                          j (loop [k j]
                              (if (and (< k n) (Character/isDigit (.charAt input k)))
                                (recur (inc k)) k))]
                      (when (= digit-start j)
                        (syntax-error! "tsvector" input j "expected position"))
                      (let [position (Long/parseLong (subs input digit-start j))
                            _ (when (or (< position 1) (> position 16383))
                                (syntax-error! "tsvector" input digit-start
                                               "position must be between 1 and 16383"))
                            weight (if (and (< j n)
                                            (contains? weight-rank (.charAt input j)))
                                     (.charAt input j) \D)
                            j (if (= weight \D)
                                ;; An explicit D still consumes a character.
                                (if (and (< j n) (= \D (.charAt input j))) (inc j) j)
                                (inc j))
                            result (conj result {:position position :weight weight})]
                        (if (and (< j n) (= \, (.charAt input j)))
                          (recur (inc j) result)
                          [result j]))))
                  [[] after-lexeme])
                raw-next-i next-i
                next-i (skip-space input raw-next-i)]
            (when (and (< raw-next-i n)
                       (not (whitespace? (.charAt input raw-next-i))))
              (syntax-error! "tsvector" input next-i "unexpected character"))
            (recur next-i
                   (update entries lexeme
                           (fn [old] (merge-positions (into (or old []) positions)))))))))))

(defn canonical-tsvector
  "Validate and return PostgreSQL's canonical textual tsvector form."
  [value]
  (->> (parse-tsvector value)
       (map (fn [[lexeme positions]]
              (str (quote-lexeme lexeme)
                   (when (seq positions)
                     (str ":"
                          (str/join ","
                                    (map (fn [{:keys [position weight]}]
                                           (str position (when (not= \D weight) weight)))
                                         positions)))))))
       (str/join " ")))

(defn to-tsvector
  "Build a canonical positioned tsvector using the supported configuration."
  ([text] (to-tsvector "english" text))
  ([config text]
   (->> (tokens-with-positions config text)
        (group-by :lexeme)
        (map (fn [[lexeme tokens]]
               [lexeme (mapv #(select-keys % [:position]) tokens)]))
        (into (sorted-map))
        (map (fn [[lexeme positions]]
               (str (quote-lexeme lexeme) ":"
                    (str/join "," (map :position positions)))))
        (str/join " "))))

(defn- query-token [^String input start]
  (let [n (.length input), i (skip-space input start)]
    (if (= i n)
      [{:type :eof} i]
      (let [c (.charAt input i)]
        (case c
          \! [{:type :not} (inc i)]
          \& [{:type :and} (inc i)]
          \| [{:type :or} (inc i)]
          \( [{:type :lparen} (inc i)]
          \) [{:type :rparen} (inc i)]
          \< (let [end (.indexOf input ">" (inc i))]
               (when (neg? end)
                 (syntax-error! "tsquery" input i "unterminated phrase operator"))
               (let [body (subs input (inc i) end)
                     distance (if (= body "-") 1
                                  (try (Long/parseLong body)
                                       (catch NumberFormatException _
                                         (syntax-error! "tsquery" input i
                                                        "invalid phrase distance"))))]
                 (when (or (< distance 0) (> distance 16384))
                   (syntax-error! "tsquery" input i "invalid phrase distance"))
                 [{:type :phrase :distance distance} (inc end)]))
          (let [[lexeme j]
                (read-lexeme "tsquery" input i
                             #(or (whitespace? %) (contains? #{\: \! \& \| \( \) \< \>} %)))
                [weights prefix? j]
                (if (and (< j n) (= \: (.charAt input j)))
                  (loop [k (inc j), weights #{}, prefix? false]
                    (if (and (< k n) (contains? #{\A \B \C \D \*} (.charAt input k)))
                      (let [m (.charAt input k)]
                        (recur (inc k) (if (= m \*) weights (conj weights m))
                               (or prefix? (= m \*))))
                      (if (= k (inc j))
                        (syntax-error! "tsquery" input j "empty modifier")
                        [weights prefix? k])))
                  [#{} false j])]
            [{:type :term :lexeme lexeme :weights weights :prefix? prefix?} j]))))))

(declare parse-query-precedence)

(defn- parse-query-prefix [^String input start]
  (let [[token i] (query-token input start)]
    (case (:type token)
      :term [token i]
      :not (let [[arg j] (parse-query-prefix input i)]
             [{:op :not :arg arg} j])
      :lparen (let [[expr j] (parse-query-precedence input i 1)
                    [close k] (query-token input j)]
                (when-not (= :rparen (:type close))
                  (syntax-error! "tsquery" input j "expected closing parenthesis"))
                [expr k])
      (syntax-error! "tsquery" input start "expected lexeme, !, or ("))))

(def ^:private query-precedence {:or 1, :and 2, :phrase 3})

(defn- parse-query-precedence [^String input start min-precedence]
  (let [[left i] (parse-query-prefix input start)]
    (loop [left left, i i]
      (let [[token j] (query-token input i)
            precedence (query-precedence (:type token))]
        (if (and precedence (>= precedence min-precedence))
          (let [[right k] (parse-query-precedence input j (inc precedence))]
            (recur {:op (:type token) :left left :right right
                    :distance (:distance token)} k))
          [left i])))))

(defn parse-tsquery
  "Parse PostgreSQL's boolean/prefix/weight/phrase tsquery grammar to an AST."
  [value]
  (let [input (str value), i (skip-space input 0)]
    (if (= i (.length ^String input))
      nil
      (let [[ast j] (parse-query-precedence input i 1)
            [tail _] (query-token input j)]
        (when-not (= :eof (:type tail))
          (syntax-error! "tsquery" input j "unexpected token"))
        ast))))

(defn- query-text [ast parent-precedence]
  (if (= :term (:type ast))
    (str (quote-lexeme (:lexeme ast))
         (when (or (seq (:weights ast)) (:prefix? ast))
           (str ":" (when (:prefix? ast) "*")
                (apply str (filter (:weights ast) [\A \B \C \D])))))
    (let [op (:op ast), precedence (if (= op :not) 4 (query-precedence op))
          text (case op
                 :not (str "!" (query-text (:arg ast) precedence))
                 :and (str (query-text (:left ast) precedence) " & "
                           (query-text (:right ast) (inc precedence)))
                 :or (str (query-text (:left ast) precedence) " | "
                          (query-text (:right ast) (inc precedence)))
                 :phrase (str (query-text (:left ast) precedence) " "
                              (if (= 1 (:distance ast)) "<->"
                                  (str "<" (:distance ast) ">")) " "
                              (query-text (:right ast) (inc precedence))))]
      (if (< precedence parent-precedence) (str "(" text ")") text))))

(defn canonical-tsquery [value]
  (some-> (parse-tsquery value) (query-text 0) (or "")))

(defn- candidate-query [ast]
  (cond
    (nil? ast) :none

    (= :term (:type ast))
    {:op (if (:prefix? ast) :prefix :term)
     :lexeme (:lexeme ast)}

    ;; A negative term does not identify a posting list containing all of its
    ;; matches. PostgreSQL's GIN path has the same boundary: a negative-only
    ;; query must consider every indexed row and recheck the primary value.
    (= :not (:op ast)) :all

    (= :and (:op ast))
    (let [left (candidate-query (:left ast))
          right (candidate-query (:right ast))]
      (cond
        (= :none left) :none
        (= :none right) :none
        (= :all left) right
        (= :all right) left
        :else {:op :and :args [left right]}))

    (= :or (:op ast))
    (let [left (candidate-query (:left ast))
          right (candidate-query (:right ast))]
      (cond
        (= :all left) :all
        (= :all right) :all
        (= :none left) right
        (= :none right) left
        :else {:op :or :args [left right]}))

    ;; A phrase probe can intersect its positive lexemes, but positions and
    ;; weights live in the authoritative tsvector and are never delegated to
    ;; Scriptum/Lucene.
    (= :phrase (:op ast))
    (let [left (candidate-query (:left ast))
          right (candidate-query (:right ast))]
      (cond
        (= :none left) :none
        (= :none right) :none
        (= :all left) right
        (= :all right) left
        :else {:op :and :args [left right]}))))

(defn tsquery-candidate-plan
  "Return an analyzer-free posting-list plan for a secondary index.

   The plan is deliberately less selective than @@ whenever necessary, but it
   has no false negatives. `:all` means a complete indexed-document scan and
   `:none` means the empty tsquery. Every non-empty plan requires authoritative
   primary tsvector recheck; Lucene scores and analyzers are never SQL-visible."
  [query]
  (let [canonical (canonical-tsquery query)]
    {:query (candidate-query (parse-tsquery canonical))
     :query-id canonical
     :precision :recheck
     :recall :complete
     :ordering :none}))

(defn- term-result [vector {:keys [lexeme weights prefix?]}]
  (let [entries (if prefix?
                  (filter (fn [[candidate]] (str/starts-with? candidate lexeme)) vector)
                  (when-let [positions (get vector lexeme)] [[lexeme positions]]))
        positions (->> entries (mapcat second)
                       (filter #(or (empty? weights) (contains? weights (:weight %))))
                       (map :position) set)]
    {:match? (boolean
              (some (fn [[_ ps]]
                      (if (empty? weights) true
                          (some #(contains? weights (:weight %)) ps)))
                    entries))
     :positions positions
     :spans (set (map (fn [p] [p p]) positions))}))

(defn- eval-query [vector ast]
  (cond
    (nil? ast) {:match? false :positions #{}}
    (= :term (:type ast)) (term-result vector ast)
    (= :not (:op ast)) (let [v (eval-query vector (:arg ast))]
                         {:match? (not (:match? v)) :positions #{}})
    (= :and (:op ast)) (let [l (eval-query vector (:left ast))
                             r (eval-query vector (:right ast))]
                         {:match? (and (:match? l) (:match? r)) :positions #{}})
    (= :or (:op ast)) (let [l (eval-query vector (:left ast))
                            r (eval-query vector (:right ast))]
                        {:match? (or (:match? l) (:match? r)) :positions #{}})
    (= :phrase (:op ast))
    (let [l (eval-query vector (:left ast)), r (eval-query vector (:right ast))
          ;; PostgreSQL's phrase executor has richer NOT/OR positional
          ;; semantics. Do not guess: this first exact slice supports terms and
          ;; nested positive phrases, and fails explicitly for other operands.
          positional? (fn positional? [node]
                        (or (= :term (:type node))
                            (and (= :phrase (:op node))
                                 (positional? (:left node))
                                 (positional? (:right node)))))]
      (when-not (and (positional? (:left ast)) (positional? (:right ast)))
        (throw (ex-info "phrase operands containing boolean operators are not supported"
                        {:error :feature-not-supported :sqlstate "0A000"})))
      (let [spans (set (for [[left-start left-end] (:spans l)
                             [right-start right-end] (:spans r)
                             :when (= right-start (+ left-end (:distance ast)))]
                         [left-start right-end]))]
        {:match? (boolean (seq spans))
         :positions (set (map second spans))
         :spans spans}))))

(defn ts-match?
  "Exact PostgreSQL @@ recheck for the supported semantic slice. Nulls are
   false in predicate position; callers projecting the operator preserve SQL
   NULL separately."
  [vector query]
  (and (some? vector) (not= :__null__ vector)
       (some? query) (not= :__null__ query)
       (:match? (eval-query (parse-tsvector vector) (parse-tsquery query)))))

(defn ts-match3
  "Value-position @@ with PostgreSQL's strict NULL propagation."
  [vector query]
  (if (or (nil? vector) (= :__null__ vector)
          (nil? query) (= :__null__ query))
    :__null__
    (boolean (ts-match? vector query))))

(defn ts-lexize
  "Apply a PostgreSQL text-search dictionary and return text[].

   The initial compatibility boundary covers english_stem and simple. Other
   dictionaries fail explicitly until catalog-backed resolution exists."
  [dictionary token]
  (if-let [lexeme (normalize-token (resolve-dictionary dictionary) (str token))]
    (pg-arr/array :text [lexeme])
    (pg-arr/array :text [])))

(defn plainto-tsquery
  "Build canonical tsquery text, treating punctuation as separators and
   joining retained lexemes with AND."
  [config text]
  (->> (tokens-with-positions config text)
       (map (comp quote-lexeme :lexeme))
       (str/join " & ")))

(defn phraseto-tsquery
  "Build canonical phrase tsquery text. Distances use original token
   positions, so removing stop words widens <N> exactly as PostgreSQL does."
  [config text]
  (let [tokens (tokens-with-positions config text)]
    (if (empty? tokens)
      ""
      (reduce (fn [query [{prev-pos :position} {pos :position lexeme :lexeme}]]
                (str query " " (if (= 1 (- pos prev-pos))
                                 "<->"
                                 (str "<" (- pos prev-pos) ">"))
                     " " (quote-lexeme lexeme)))
              (quote-lexeme (:lexeme (first tokens)))
              (partition 2 1 tokens)))))
