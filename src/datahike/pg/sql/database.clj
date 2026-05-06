(ns datahike.pg.sql.database
  "CREATE DATABASE / DROP DATABASE token-driven parser plus the
   `db-from-template` helper used to wire a server-level config
   template to a per-database config.

   JSqlParser 5.x has no AST class for either statement (returns
   `UnsupportedStatement`), so we hand-parse before JSqlParser sees
   the SQL. The grammar accepted is:

     CREATE DATABASE name [WITH] [(] [k [=] v [,]]* [)]
     DROP DATABASE [IF EXISTS] name [WITH (FORCE)]

   `name` is either a bare identifier or a quoted identifier
   (\"...\"). Option values are 'string', \"ident\", number,
   true|false, or a bare identifier.

   Datahike-aware option keys are translated into datahike config:

     BACKEND               -> [:store :backend] keyword
     STORE_ID              -> [:store :id]      string
     PATH                  -> [:store :path]    string (file backend)
     HOST / PORT / USER /  -> [:store :*]       string|long
     PASSWORD / DBNAME       (pg backend)
     SCHEMA_FLEXIBILITY    -> :schema-flexibility keyword
     KEEP_HISTORY          -> :keep-history?    boolean
     INDEX                 -> :index            :datahike.index/<value>

   PostgreSQL-only option keys (OWNER, TEMPLATE, ENCODING, LC_COLLATE,
   LC_CTYPE, LOCALE, LOCALE_PROVIDER, ICU_LOCALE, ICU_RULES,
   COLLATION_VERSION, TABLESPACE, ALLOW_CONNECTIONS, CONNECTION_LIMIT,
   IS_TEMPLATE, OID, STRATEGY, REFRESH_COLLATION_VERSION) are silently
   accepted with a NOTICE so pg_dump output round-trips.

   Unknown option names raise `:syntax-error`."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [datahike.api :as d]))

;; ----------------------------------------------------------------------------
;; Tokenizer
;; ----------------------------------------------------------------------------

(defn- skip-ws ^long [^String sql ^long i ^long n]
  (loop [i i]
    (cond
      (>= i n) i
      (Character/isWhitespace (.charAt sql i)) (recur (unchecked-inc i))
      :else i)))

(defn- skip-line-comment ^long [^String sql ^long i ^long n]
  (loop [i i]
    (cond
      (>= i n) i
      (= \newline (.charAt sql i)) (unchecked-inc i)
      :else (recur (unchecked-inc i)))))

(defn- skip-block-comment ^long [^String sql ^long i ^long n]
  (loop [i (+ i 2) depth 1]
    (cond
      (>= i n) i
      (and (< (+ i 1) n) (= \/ (.charAt sql i)) (= \* (.charAt sql (inc i))))
      (recur (+ i 2) (inc depth))
      (and (< (+ i 1) n) (= \* (.charAt sql i)) (= \/ (.charAt sql (inc i))))
      (let [d (dec depth)]
        (if (zero? d) (+ i 2) (recur (+ i 2) d)))
      :else (recur (unchecked-inc i) depth))))

(defn tokenize
  "Tokenize a SQL string into a vector of [kind text-or-value] pairs.
   Tokens: :ident :string :num :bool :eq :comma :lparen :rparen :semicolon."
  [^String sql]
  (let [n (.length sql)]
    (loop [i 0 toks []]
      (let [i (long (skip-ws sql i n))]
        (cond
          (>= i n) toks

          ;; -- line comment
          (and (< (+ i 1) n) (= \- (.charAt sql i)) (= \- (.charAt sql (inc i))))
          (recur (long (skip-line-comment sql (+ i 2) n)) toks)

          ;; /* block comment */
          (and (< (+ i 1) n) (= \/ (.charAt sql i)) (= \* (.charAt sql (inc i))))
          (recur (long (skip-block-comment sql i n)) toks)

          (= \, (.charAt sql i)) (recur (unchecked-inc i) (conj toks [:comma ","]))
          (= \( (.charAt sql i)) (recur (unchecked-inc i) (conj toks [:lparen "("]))
          (= \) (.charAt sql i)) (recur (unchecked-inc i) (conj toks [:rparen ")"]))
          (= \= (.charAt sql i)) (recur (unchecked-inc i) (conj toks [:eq "="]))
          (= \; (.charAt sql i)) (recur (unchecked-inc i) (conj toks [:semicolon ";"]))

          ;; 'string'  — ANSI-style: '' inside is an escaped quote
          (= \' (.charAt sql i))
          (let [sb (StringBuilder.)
                end (loop [j (unchecked-inc i)]
                      (cond
                        (>= j n) j
                        (= \' (.charAt sql j))
                        (if (and (< (unchecked-inc j) n)
                                 (= \' (.charAt sql (unchecked-inc j))))
                          (do (.append sb \') (recur (+ j 2)))
                          j)
                        :else (do (.append sb (.charAt sql j)) (recur (unchecked-inc j)))))]
            (recur (unchecked-inc end) (conj toks [:string (.toString sb)])))

          ;; "ident"   — quoted identifier; preserves case
          (= \" (.charAt sql i))
          (let [end (loop [j (unchecked-inc i)]
                      (cond
                        (>= j n) j
                        (= \" (.charAt sql j)) j
                        :else (recur (unchecked-inc j))))]
            (recur (unchecked-inc end) (conj toks [:ident (subs sql (unchecked-inc i) end)])))

          ;; Single-char operators / wildcards. We don't need a real
          ;; expression parser at this layer — just need to surface
          ;; these characters as their own tokens so downstream
          ;; consumers (notably the COPY parser's FORCE_NULL * /
          ;; FORCE_NOT_NULL * forms) can recognise them. Without this
          ;; the unrecognised-char branch silently drops them.
          (#{\* \+ \/ \%} (.charAt sql i))
          (recur (unchecked-inc i)
                 (conj toks [:ident (str (.charAt sql i))]))

          :else
          (let [end (loop [j i]
                      (cond
                        (>= j n) j
                        (let [c (.charAt sql j)]
                          (or (Character/isLetterOrDigit c) (= \_ c) (= \. c) (= \- c)))
                        (recur (unchecked-inc j))
                        :else j))
                t (subs sql i end)]
            (if (= end i)
              ;; an unrecognised char — just skip it, treats it as a separator
              (recur (unchecked-inc i) toks)
              (recur end (conj toks
                               [(cond
                                  (re-matches #"[+-]?\d+" t) :num
                                  (#{"true" "TRUE"} t) :bool
                                  (#{"false" "FALSE"} t) :bool
                                  :else :ident)
                                t])))))))))

;; ----------------------------------------------------------------------------
;; CREATE DATABASE / DROP DATABASE parsers
;; ----------------------------------------------------------------------------

(defn- ident-eq? [tok lower]
  (and (= :ident (first tok))
       (= lower (str/lower-case (second tok)))))

(defn parse-create-database
  "Parse a tokenised CREATE DATABASE statement. Returns:
     {:db-name str
      :if-not-exists? bool
      :options [[lower-case-key value] ...]}

   The grammar is intentionally lenient — `WITH` is optional, `(...)`
   is optional, `=` between key and value is optional, `,` between
   pairs is optional. Each option value is decoded as the right
   Clojure type (string for 'literal', long for digits, boolean for
   true/false, raw string otherwise)."
  [toks]
  (let [[c d & rest1] toks]
    (when-not (and (ident-eq? c "create") (ident-eq? d "database"))
      (throw (ex-info "not a CREATE DATABASE statement"
                      {:error :syntax-error :got [c d]})))
    (let [[if-ne? rest2] (if (and (ident-eq? (first rest1) "if")
                                  (ident-eq? (second rest1) "not")
                                  (ident-eq? (nth rest1 2 nil) "exists"))
                           [true (drop 3 rest1)]
                           [false rest1])
          name-tok (first rest2)
          db-name (when name-tok (second name-tok))
          rest3 (rest rest2)
          rest4 (if (ident-eq? (first rest3) "with") (rest rest3) rest3)
          [rest5 paren?] (if (= :lparen (ffirst rest4))
                           [(rest rest4) true]
                           [rest4 false])]
      (loop [t rest5 opts []]
        (let [t (if (= :comma (ffirst t)) (rest t) t)
              t (if (= :semicolon (ffirst t)) (rest t) t)]
          (cond
            (empty? t)
            {:db-name db-name :if-not-exists? if-ne? :options opts}

            (and paren? (= :rparen (ffirst t)))
            {:db-name db-name :if-not-exists? if-ne? :options opts}

            (= :ident (ffirst t))
            (let [k (str/lower-case (second (first t)))
                  t1 (rest t)
                  t2 (if (= :eq (ffirst t1)) (rest t1) t1)
                  [vt vv] (first t2)
                  v (case vt
                      :string vv
                      :num (Long/parseLong vv)
                      :bool (Boolean/parseBoolean vv)
                      :ident (cond
                               (#{"true" "TRUE"} vv) true
                               (#{"false" "FALSE"} vv) false
                               :else vv)
                      vv)]
              (recur (rest t2) (conj opts [k v])))

            :else
            {:db-name db-name :if-not-exists? if-ne? :options opts}))))))

(defn parse-drop-database
  "Parse a tokenised DROP DATABASE statement. Returns:
     {:db-name str :if-exists? bool}

   `WITH (FORCE)` is silently accepted but ignored — Datahike
   connections are released as part of the drop regardless."
  [toks]
  (let [[d1 d2 & rest1] toks]
    (when-not (and (ident-eq? d1 "drop") (ident-eq? d2 "database"))
      (throw (ex-info "not a DROP DATABASE statement"
                      {:error :syntax-error :got [d1 d2]})))
    (let [[if-exists? rest2] (if (and (ident-eq? (first rest1) "if")
                                      (ident-eq? (second rest1) "exists"))
                               [true (drop 2 rest1)]
                               [false rest1])
          db-name (some-> (first rest2) second)]
      {:db-name db-name :if-exists? if-exists?})))

;; ----------------------------------------------------------------------------
;; Option → datahike config translator
;; ----------------------------------------------------------------------------

(def ^:private pg-only-options
  "PostgreSQL-only options that we silently accept (with NOTICE) so
   pg_dump output and ORM-emitted CREATE DATABASE statements round-trip."
  #{"owner" "template" "encoding" "lc_collate" "lc_ctype" "locale"
    "locale_provider" "icu_locale" "icu_rules" "collation_version"
    "tablespace" "allow_connections" "connection_limit" "is_template"
    "oid" "strategy" "refresh_collation_version"})

(def ^:private datahike-options
  "Map of option-name (lowercase) → fn [config value] → config."
  {"backend"            (fn [c v] (assoc-in c [:store :backend] (keyword v)))
   "store_id"           (fn [c v] (assoc-in c [:store :id] v))
   "path"               (fn [c v] (assoc-in c [:store :path] v))
   "host"               (fn [c v] (assoc-in c [:store :host] v))
   "port"               (fn [c v] (assoc-in c [:store :port]
                                            (if (number? v) v (Long/parseLong (str v)))))
   "user"               (fn [c v] (assoc-in c [:store :user] v))
   "password"           (fn [c v] (assoc-in c [:store :password] v))
   "dbname"             (fn [c v] (assoc-in c [:store :dbname] v))
   "schema_flexibility" (fn [c v] (assoc c :schema-flexibility (keyword v)))
   "keep_history"       (fn [c v] (assoc c :keep-history? (boolean v)))
   "index"              (fn [c v] (assoc c :index (keyword "datahike.index" v)))})

(defn options->config
  "Apply parsed options on top of `server-template`. Returns
     {:config datahike-config :notices [str ...]}.
   Throws `:syntax-error` on an unknown non-pg option."
  [server-template options]
  (let [notices (volatile! [])
        cfg (reduce
             (fn [c [k v]]
               (cond
                 (contains? datahike-options k)
                 ((get datahike-options k) c v)

                 (contains? pg-only-options k)
                 (do (vswap! notices conj
                             (str "option \"" k "\" accepted for PostgreSQL "
                                  "compatibility but ignored"))
                     c)

                 :else
                 (throw (ex-info (str "unknown option \"" k
                                      "\" in CREATE DATABASE")
                                 {:error :syntax-error :option k}))))
             server-template
             options)]
    {:config cfg :notices (vec @notices)}))

;; ----------------------------------------------------------------------------
;; Template → hook helper
;; ----------------------------------------------------------------------------

(defn- interpolate-name
  "Replace `{{name}}` in any string value within `tmpl` with `db-name`."
  [tmpl db-name]
  (walk/postwalk
   (fn [x]
     (if (and (string? x) (.contains ^String x "{{name}}"))
       (str/replace x "{{name}}" db-name)
       x))
   tmpl))

(defn- ensure-store-id
  "All konserve backends require a UUID `:id` in `:store`. If the
   merged config lacks one, generate a fresh UUID. (Operators who
   explicitly want a deterministic id can set `STORE_ID` in the
   CREATE DATABASE WITH clause or supply `:id` in the template.)"
  [cfg]
  (if (get-in cfg [:store :id])
    cfg
    (assoc-in cfg [:store :id] (java.util.UUID/randomUUID))))

(defn db-from-template
  "Build an `:on-create-database` hook from a server-level config
   template. The returned fn receives `[db-name parsed-options]`,
   merges options over the template, interpolates `{{name}}`,
   ensures a UUID `:id`, then runs `d/create-database` and
   `d/connect` and returns the connection.

   Example template:
     {:store {:backend :memory}
      :schema-flexibility :write
      :keep-history? false}

   With `{{name}}` interpolation:
     {:store {:backend :file :path \"/var/lib/dh/{{name}}\"}
      :schema-flexibility :write}"
  [server-template]
  (fn db-create-hook [db-name parsed-options]
    (let [{:keys [config]} (options->config server-template parsed-options)
          resolved (-> config
                       (interpolate-name db-name)
                       ensure-store-id)]
      (d/create-database resolved)
      (d/connect resolved))))

(defn db-delete-from-template
  "Build an `:on-delete-database` hook that mirrors `db-from-template`.
   Releases the conn and best-effort deletes the backing store; the
   template is consulted for the storage backend so file/pg/etc.
   stores get the right delete call."
  [server-template]
  (fn db-delete-hook [db-name conn _parsed-options]
    (let [{:keys [config]} (options->config server-template [])
          resolved (-> config
                       (interpolate-name db-name)
                       ensure-store-id)]
      (try (d/release conn) (catch Throwable _))
      (try (d/delete-database resolved) (catch Throwable _)))))
