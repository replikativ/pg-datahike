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
        (and (str/includes? first-text ".")
             (re-matches #"[A-Za-z_][A-Za-z0-9_]*\.[A-Za-z_][A-Za-z0-9_]*"
                         first-text))
        [(second (str/split first-text #"\." 2)) rest1]

        ;; `schema."name"` — the unquoted schema part absorbs the
        ;; trailing dot, the quoted name follows as a separate
        ;; `:ident` token.
        (and (str/ends-with? first-text ".")
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

(defn- toks->raw-text
  "Reassemble a token sub-sequence back to a SQL fragment for storage.
   We don't try to reproduce exact whitespace — just enough that
   re-parsing on dump emit yields an equivalent expression."
  [toks]
  (->> toks
       (map (fn [[k t]]
              (case k
                :string (str "'" (str/replace t "'" "''") "'")
                :ident  t
                :num    t
                :bool   t
                :comma  ","
                :lparen "("
                :rparen ")"
                :eq     "="
                t)))
       (interpose " ")
       (apply str)))

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
  [toks original-sql]
  (let [toks (skip-kw toks "domain")
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
           out {:domain-name domain-name
                :base-type   base-type
                :base-args   base-args
                :original-sql original-sql
                :not-null    false}]
      (let [[t & rest-ts] ts]
        (cond
          (or (nil? t) (= :semicolon (first t)))
          out

          (ident-eq? t "constraint")
          (let [[name-tok & rest-ts] rest-ts
                check-name (when (= :ident (first name-tok)) (second name-tok))]
            (recur rest-ts (assoc out :check-name check-name)))

          (ident-eq? t "check")
          (let [[inside after] (collect-balanced-parens rest-ts)]
            (recur after (assoc out :check-expr (toks->raw-text inside))))

          (and (ident-eq? t "not") (ident-eq? (first rest-ts) "null"))
          (recur (rest rest-ts) (assoc out :not-null true))

          (ident-eq? t "null")
          (recur rest-ts (assoc out :not-null false))

          (ident-eq? t "default")
          ;; consume tokens until next top-level keyword or end
          (let [stop? #{"constraint" "check" "not" "null"}
                [taken left] (split-with
                              (fn [tk]
                                (not (and (= :ident (first tk))
                                          (stop? (str/lower-case (second tk))))))
                              rest-ts)]
            (recur left (assoc out :default-raw (toks->raw-text taken))))

          :else
          ;; unknown token — skip silently rather than error so future
          ;; PG additions don't block ingestion
          (recur rest-ts out))))))
