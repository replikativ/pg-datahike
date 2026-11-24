(ns datahike.pg.sql.types
  "Parsers for `CREATE TYPE … AS ENUM (…)` and `CREATE DOMAIN`. Both
   bypass JSqlParser (which can't / doesn't parse them) and produce
   parsed maps the server's exec-* dispatch picks up.

   Mirrors the shape of `datahike.pg.sql.database` (CREATE DATABASE):
   classify routes the statement to a system-type bypass, this ns
   tokenises and extracts the structured form, the server transacts
   it as a registry entity (`:datahike.pg.enum/*` or
   `:datahike.pg.domain/*`).

   Why registry entities and not native Datahike types: enums are
   semantically `string + check-in <values>`; domains are
   `<base-type> + optional CHECK`. Both can be lowered transparently
   when a column references them in CREATE TABLE — column-type
   resolution looks the name up in the registry and expands. The
   dump tool reads the registry back and emits CREATE TYPE / CREATE
   DOMAIN headers before tables, so the round trip preserves the
   full schema."
  (:require [clojure.string :as str]
            [datahike.pg.catalog.regoid :as regoid]
            [datahike.pg.sql.classify :as classify]
            [datahike.pg.sql.database :as database]))

;; ============================================================================
;; Token helpers (built on database/tokenize — same lightweight tokeniser
;; the CREATE DATABASE parser uses)
;; ============================================================================

(defn- ident-eq?
  "True if tok is an :ident matching `lower` (case-insensitively)."
  [tok ^String lower]
  (and (= :ident (first tok))
       (= lower (str/lower-case (second tok)))))

(defn- skip-kw
  "If toks starts with the given lower-case keyword, drop it. Otherwise
   throw a syntax error. Used to consume required keywords."
  [toks ^String lower]
  (if (ident-eq? (first toks) lower)
    (rest toks)
    (throw (ex-info (str "expected " (str/upper-case lower))
                    {:error :syntax-error :got (first toks)}))))

(defn- consume-name
  "Consume a possibly-schema-qualified identifier. Returns
   [name remaining-toks]. Accepts:
     name                   → name
     schema.name            → name           (one glued ident)
     schema.\"name\"        → name           (ident-with-trailing-dot + quoted ident)
     \"schema\".\"name\"    → name           (quoted ident + dot + quoted ident)
     \"name\"               → name           (single quoted ident)
   Drops any schema prefix — pg-datahike has one schema namespace
   so qualifying is informational only."
  [toks]
  (let [t1 (first toks)]
    (when-not (= :ident (first t1))
      (throw (ex-info "expected identifier"
                      {:error :syntax-error :got t1})))
    (let [first-text (second t1)
          rest1 (rest toks)]
      (cond
        ;; `schema.name` glued into one token by the tokenizer (both
        ;; sides are bare alphanumeric identifiers). Strip the schema.
        (and (not (:quoted? (meta t1)))
             (str/includes? first-text ".")
             (re-matches #"[A-Za-z_][A-Za-z0-9_]*\.[A-Za-z_][A-Za-z0-9_]*"
                         first-text))
        [(second (str/split first-text #"\." 2)) rest1]

        ;; `schema."name"` — the unquoted schema part absorbs the
        ;; trailing dot, the quoted name follows as a separate
        ;; `:ident` token.
        (and (not (:quoted? (meta t1)))
             (str/ends-with? first-text ".")
             (= :ident (first (first rest1))))
        [(second (first rest1)) (rest rest1)]

        ;; `"schema"."name"` — three tokens: quoted ident, lone `.`,
        ;; quoted ident. Tokenizer drops `.` as a single-char ident.
        (and (= [:ident "."] (first rest1))
             (= :ident (first (second rest1))))
        [(second (second rest1)) (drop 2 rest1)]

        :else
        [first-text rest1]))))

;; ============================================================================
;; CREATE TYPE … AS ENUM
;; ============================================================================

(defn parse-create-type-enum
  "Parse a tokenised `CREATE TYPE [schema.]name AS ENUM ('v1', 'v2', …)`
   statement. Returns:

     {:type-name string                      ; unqualified
      :values    [string …]                  ; in declaration order
      :original-sql string}                  ; for re-emission

   Throws ex-info with `:error :syntax-error` on malformed input."
  [toks original-sql]
  ;; Skip 'CREATE TYPE' (caller passes toks AFTER 'CREATE')
  (let [toks (skip-kw toks "type")
        [type-name toks] (consume-name toks)
        toks (skip-kw toks "as")
        toks (skip-kw toks "enum")
        ;; consume `(`
        _ (when-not (= :lparen (first (first toks)))
            (throw (ex-info "expected `(`"
                            {:error :syntax-error :got (first toks)})))
        toks (rest toks)
        ;; parse string values separated by commas, terminated by `)`
        values (loop [ts toks acc []]
                 (let [t (first ts)]
                   (cond
                     (= :rparen (first t)) [acc (rest ts)]
                     (= :string (first t))
                     (let [acc (conj acc (second t))
                           rest-ts (rest ts)
                           next-t (first rest-ts)]
                       (cond
                         (= :comma (first next-t)) (recur (rest rest-ts) acc)
                         (= :rparen (first next-t)) [acc (rest rest-ts)]
                         :else (throw (ex-info "expected `,` or `)`"
                                               {:error :syntax-error
                                                :got next-t}))))
                     :else (throw (ex-info "expected string literal"
                                           {:error :syntax-error :got t})))))
        [values _] values]
    (when (empty? values)
      (throw (ex-info "ENUM must have at least one value"
                      {:error :syntax-error})))
    {:type-name type-name
     :values values
     :original-sql original-sql}))

(defn parse-alter-type-enum
  "Parse ALTER TYPE enum label mutations. Supports PostgreSQL's
   `ADD VALUE [IF NOT EXISTS] label [BEFORE|AFTER neighbor]` and
   `RENAME VALUE old TO new` forms."
  [original-sql]
  (let [toks (database/tokenize original-sql)
        toks (skip-kw toks "alter")
        toks (skip-kw toks "type")
        [type-name toks] (consume-name toks)]
    (cond
      (and (ident-eq? (first toks) "add")
           (ident-eq? (second toks) "value"))
      (let [toks (drop 2 toks)
            if-not-exists? (and (ident-eq? (first toks) "if")
                                (ident-eq? (second toks) "not")
                                (ident-eq? (nth toks 2 nil) "exists"))
            toks (if if-not-exists? (drop 3 toks) toks)
            label-token (first toks)
            _ (when-not (= :string (first label-token))
                (throw (ex-info "expected enum label" {:error :syntax-error})))
            toks (rest toks)
            placement (cond
                        (ident-eq? (first toks) "before") :before
                        (ident-eq? (first toks) "after") :after
                        :else nil)
            neighbor-token (when placement (second toks))
            _ (when (and placement (not= :string (first neighbor-token)))
                (throw (ex-info "expected neighboring enum label"
                                {:error :syntax-error})))]
        {:op :add-value :type-name type-name :label (second label-token)
         :if-not-exists? if-not-exists? :placement placement
         :neighbor (when neighbor-token (second neighbor-token))})

      (and (ident-eq? (first toks) "rename")
           (ident-eq? (second toks) "value"))
      (let [old-token (nth toks 2 nil)
            to-token (nth toks 3 nil)
            new-token (nth toks 4 nil)]
        (when-not (and (= :string (first old-token))
                       (ident-eq? to-token "to")
                       (= :string (first new-token)))
          (throw (ex-info "malformed ALTER TYPE ... RENAME VALUE"
                          {:error :syntax-error})))
        {:op :rename-value :type-name type-name
         :old-label (second old-token) :new-label (second new-token)})

      :else
      (throw (ex-info "unsupported ALTER TYPE form"
                      {:error :unsupported-feature :sqlstate "0A000"})))))

(defn parse-rename-type
  "Parse `ALTER TYPE old_name RENAME TO new_name`."
  [original-sql]
  (let [toks (database/tokenize original-sql)
        toks (skip-kw toks "alter")
        toks (skip-kw toks "type")
        [type-name toks] (consume-name toks)
        toks (skip-kw toks "rename")
        toks (skip-kw toks "to")
        [new-name _] (consume-name toks)]
    {:type-name type-name :new-name new-name}))

(defn- parse-type-drop [original-sql]
  (let [tokens (drop 2 (classify/tokenize original-sql))
        if-exists? (= ["if" "exists"] (mapv (comp str/lower-case :text) (take 2 tokens)))
        tokens (if if-exists? (drop 2 tokens) tokens)
        [name-tokens rest-tokens]
        (loop [parts [(first tokens)], remaining (next tokens)]
          (if (= "." (:text (first remaining)))
            (recur (into parts (take 2 remaining)) (nnext remaining))
            [parts remaining]))
        _ (when-not (every? #(contains? #{:ident :quoted} (:type %)) (take-nth 2 name-tokens))
            (throw (ex-info "expected type name" {:sqlstate "42601"})))
        names (regoid/parse-qualified-name (apply str (map :text name-tokens)))
        _ (when (> (count names) 2)
            (throw (ex-info "cross-database type names are not supported" {:sqlstate "0A000"})))
        suffix (cond-> (vec rest-tokens) (= ";" (:text (last rest-tokens))) pop)
        modifier (some-> (first suffix) :text str/lower-case)]
    (when-not (and (<= (count suffix) 1) (contains? #{nil "cascade" "restrict"} modifier))
      (throw (ex-info "expected CASCADE or RESTRICT at the end of DROP" {:sqlstate "42601"})))
    {:type-name (peek names) :type-schema (when (= 2 (count names)) (first names))
     :if-exists? if-exists? :cascade? (= "cascade" modifier)}))

(defn parse-drop-type
  "Parse a single-name DROP TYPE statement."
  [original-sql]
  (parse-type-drop original-sql))

(defn parse-drop-domain
  "Parse a single-name DROP DOMAIN statement."
  [original-sql]
  (parse-type-drop original-sql))

;; ============================================================================
;; CREATE TYPE … AS (composite)
;; ============================================================================

(defn- split-top-level-commas
  "Split `s` on commas that are not nested inside parentheses, so a field
   type like `numeric(10,2)` stays intact."
  [s]
  (loop [cs (seq s), depth 0, ^StringBuilder cur (StringBuilder.), out []]
    (if-let [c (first cs)]
      (cond
        (= c \()            (recur (rest cs) (inc depth) (.append cur c) out)
        (= c \))            (recur (rest cs) (max 0 (dec depth)) (.append cur c) out)
        (and (= c \,) (zero? depth)) (recur (rest cs) depth (StringBuilder.) (conj out (.toString cur)))
        :else               (recur (rest cs) depth (.append cur c) out))
      (conj out (.toString cur)))))

(defn- strip-quotes [s]
  (if (and (> (count s) 1) (str/starts-with? s "\"") (str/ends-with? s "\""))
    (subs s 1 (dec (count s)))
    s))

(defn- unqualify [s]
  (if-let [d (str/last-index-of s ".")] (subs s (inc d)) s))

(defn parse-create-type-composite
  "Parse `CREATE TYPE [schema.]name AS (field type, …)` from the raw SQL
   (the tokenizer doesn't model `[]`, so we work on the text). Returns:

     {:type-name string                       ; unqualified, unquoted
      :fields    [{:field-name string :pg-type string} …]  ; declaration order
      :original-sql string}

   Throws ex-info `:error :syntax-error` on malformed input."
  [sql]
  (let [m (re-find #"(?is)\bcreate\s+type\s+(.+?)\s+as\s*\((.*)\)\s*;?\s*$" sql)]
    (when-not m
      (throw (ex-info "malformed CREATE TYPE … AS (...)" {:error :syntax-error})))
    (let [type-name (-> (nth m 1) str/trim strip-quotes unqualify strip-quotes)
          fields (->> (split-top-level-commas (nth m 2))
                      (map str/trim)
                      (remove str/blank?)
                      (mapv (fn [fs]
                              (let [fm (re-find #"(?s)^(\"[^\"]+\"|[^\s]+)\s+(.+)$" fs)]
                                (when-not fm
                                  (throw (ex-info (str "bad composite field: " fs)
                                                  {:error :syntax-error})))
                                {:field-name (strip-quotes (nth fm 1))
                                 :pg-type (str/trim (nth fm 2))}))))]
      (when (empty? fields)
        (throw (ex-info "composite type must have at least one field"
                        {:error :syntax-error})))
      {:type-name type-name :fields fields :original-sql sql})))

;; ============================================================================
;; CREATE DOMAIN
;; ============================================================================

(defn- collect-balanced-parens
  "Starting from the opening `(` token, walk forward and return
   [tokens-inside outer-toks-after-close]. Errors if the parens are
   unbalanced (or absent)."
  [toks]
  (when-not (= :lparen (first (first toks)))
    (throw (ex-info "expected `(`"
                    {:error :syntax-error :got (first toks)})))
  (loop [ts (rest toks), depth 1, acc []]
    (let [t (first ts)]
      (cond
        (nil? t)
        (throw (ex-info "unterminated parenthesis group"
                        {:error :syntax-error}))
        (= :lparen (first t))
        (recur (rest ts) (inc depth) (conj acc t))
        (and (= :rparen (first t)) (= 1 depth))
        [acc (rest ts)]
        (= :rparen (first t))
        (recur (rest ts) (dec depth) (conj acc t))
        :else
        (recur (rest ts) depth (conj acc t))))))

(defn- domain-token [{:keys [type text value pos end]}]
  (with-meta
    (case type
      :ident [:ident text]
      :quoted [:ident value]
      (case text
        "(" [:lparen text]
        ")" [:rparen text]
        "[" [:lbracket text]
        "]" [:rbracket text]
        "," [:comma text]
        ";" [:semicolon text]
        "." [:ident text]
        [type text]))
    {:pos pos :end end :quoted? (= :quoted type)}))

(defn- token-source [sql tokens]
  (when (seq tokens)
    (subs sql (:pos (meta (first tokens))) (:end (meta (peek tokens))))))

(def ^:private invalid-domain-clauses
  {"references" ["42601" "foreign key constraints not possible for domains"]
   "unique" ["42601" "unique constraints not possible for domains"]
   "primary" ["42601" "primary key constraints not possible for domains"]
   "exclude" ["42601" "exclusion constraints not possible for domains"]
   "generated" ["0A000" "specifying GENERATED not supported for domains"]
   "deferrable" ["0A000" "specifying constraint deferrability not supported for domains"]
   "initially" ["0A000" "specifying constraint deferrability not supported for domains"]
   "enforced" ["42P17" "specifying constraint enforceability not supported for domains"]})

(defn- domain-keyword [tok]
  (when (and (= :ident (first tok)) (not (:quoted? (meta tok))))
    (str/lower-case (second tok))))

(defn- domain-definition-error! [sqlstate message]
  (throw (ex-info message {:sqlstate sqlstate})))

(defn- reject-domain-no-inherit! [toks constraint]
  (when (= ["no" "inherit"] (mapv domain-keyword (take 2 toks)))
    (domain-definition-error! "42P17" (str constraint " constraints for domains cannot be marked NO INHERIT"))))

(defn- domain-default-tokens [toks]
  (loop [remaining toks, depth 0, cases 0, expression []]
    (let [[tok next-tok] remaining
          kind (first tok)
          kw (domain-keyword tok)
          next-kw (domain-keyword next-tok)]
      (if (or (nil? tok) (= :semicolon kind)
              (and (zero? depth) (zero? cases)
                   (or (contains? #{"constraint" "check" "default" "collate"} kw)
                       (contains? invalid-domain-clauses kw)
                       (and (= "not" kw) (contains? #{"null" "deferrable" "enforced"} next-kw))
                       (and (= "null" kw) (seq expression)))))
        [expression remaining]
        (recur (rest remaining)
               (case kind (:lparen :lbracket) (inc depth) (:rparen :rbracket) (dec depth) depth)
               (case kw "case" (inc cases) "end" (dec cases) cases)
               (conj expression tok))))))

(defn- domain-base-sql [sql tokens]
  (let [after-name (loop [tokens (drop 3 tokens)]
                     (if (= "." (:text (first tokens)))
                       (recur (nnext tokens))
                       tokens))
        tokens (if (= "as" (some-> (first after-name) :text str/lower-case))
                 (next after-name)
                 after-name)
        start (:pos (first tokens))]
    (when start
      (loop [[token & more] tokens, depth 0]
        (let [word (when (= :ident (:type token)) (str/lower-case (:text token)))
              text (:text token)]
          (if (or (nil? token)
                  (and (zero? depth)
                       (or (= ";" text)
                           (contains? #{"constraint" "check" "not" "null" "default" "collate"} word)
                           (contains? invalid-domain-clauses word))))
            (str/trim (subs sql start (or (:pos token) (count sql))))
            (recur more (case text "(" (inc depth) ")" (dec depth) depth))))))))

(defn parse-create-domain
  "Parse `CREATE DOMAIN [schema.]name AS <base-type> [<modifiers>...]`.

   Modifiers we capture:
     - `CONSTRAINT <name> CHECK (<expr>)`  → :check-name + :check-expr
     - `CHECK (<expr>)`                    → :check-expr (anonymous)
     - `NOT NULL` / `NULL`                 → :not-null
     - `DEFAULT <expr>`                    → :default-raw

   Returns:
     {:domain-name str
      :base-type   str         ; lower-case base-type name as written
      :base-args   [str ...]   ; type args if any (e.g. NUMERIC(10,2))
      :check-name  str | nil
      :check-expr  str | nil   ; SQL fragment between parens
      :not-null    bool
      :default-raw str | nil
      :original-sql str}

   Multiple constraints are not supported — first wins."
  [original-sql]
  (let [source-tokens (classify/tokenize original-sql)
        tokens (map domain-token source-tokens)
        toks (skip-kw (skip-kw tokens "create") "domain")
        [domain-name toks] (consume-name toks)
        ;; optional AS
        toks (cond-> toks (ident-eq? (first toks) "as") rest)
        ;; base type — single ident, optionally followed by `(args)`
        [base-type-tok & toks] toks
        _ (when-not (= :ident (first base-type-tok))
            (throw (ex-info "expected base type"
                            {:error :syntax-error :got base-type-tok})))
        base-type (str/lower-case (second base-type-tok))
        ;; optional `(args)` after base type
        [base-args toks]
        (if (= :lparen (first (first toks)))
          (let [[inside after] (collect-balanced-parens toks)]
            [(mapv second (filter #(not= :comma (first %)) inside))
             after])
          [[] toks])]
    ;; Walk modifiers: CONSTRAINT <name> CHECK (...), CHECK (...), NOT NULL, DEFAULT ...
    (loop [ts toks
           seen-nullability? false
           seen-default? false
           out {:domain-name domain-name
                :base-type   base-type
                :base-sql    (domain-base-sql original-sql source-tokens)
                :base-args   base-args
                :original-sql original-sql
                :not-null    false}]
      (let [[t & rest-ts] ts
            kw (domain-keyword t)
            next-kw (domain-keyword (first rest-ts))
            disallowed (get invalid-domain-clauses
                            (if (and (= "not" kw) (contains? #{"deferrable" "enforced"} next-kw))
                              next-kw kw))]
        (cond
          (or (nil? t) (= :semicolon (first t)))
          out

          disallowed
          (apply domain-definition-error! disallowed)

          (= "constraint" kw)
          (let [[name-tok & rest-ts] rest-ts
                check-name (when (= :ident (first name-tok)) (second name-tok))]
            (recur rest-ts seen-nullability? seen-default? (assoc out :check-name check-name)))

          (= "collate" kw)
          (let [[collation after] (consume-name rest-ts)]
            (recur after seen-nullability? seen-default? (assoc out :collation collation)))

          (= "check" kw)
          (let [[inside after] (collect-balanced-parens rest-ts)]
            (reject-domain-no-inherit! after "check")
            (recur after seen-nullability? seen-default? (assoc out :check-expr (token-source original-sql inside))))

          (and (= "not" kw) (= "null" next-kw))
          (do
            (when seen-nullability?
              (if (:not-null out)
                (domain-definition-error! "42P17" "redundant NOT NULL constraint definition")
                (domain-definition-error! "42601" "conflicting NULL/NOT NULL constraints")))
            (reject-domain-no-inherit! (rest rest-ts) "not-null")
            (recur (rest rest-ts) true seen-default? (assoc out :not-null true)))

          (= "null" kw)
          (do
            (when (:not-null out)
              (domain-definition-error! "42601" "conflicting NULL/NOT NULL constraints"))
            (recur rest-ts true seen-default? out))

          (= "default" kw)
          (let [[expression left] (domain-default-tokens rest-ts)]
            (when seen-default?
              (domain-definition-error! "42601" "multiple default expressions"))
            (when (empty? expression)
              (domain-definition-error! "42601" "expected a domain default expression"))
            (recur left seen-nullability? true
                   (assoc out :default-raw (token-source original-sql expression))))

          :else
          (recur rest-ts seen-nullability? seen-default? out))))))
