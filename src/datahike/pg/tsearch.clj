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
