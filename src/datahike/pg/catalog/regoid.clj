(ns datahike.pg.catalog.regoid
  "OID aliases with catalog-dependent input/output, following regproc.c.

   Values remain unsigned OIDs. Lookup uses the statement's database and
   search path; no parsed plan retains a database snapshot. Catalog projection
   and name resolution consume the same routine descriptors."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.catalog.objects :as objects]
            [datahike.pg.keywords :as keywords]
            [datahike.pg.pg-catalog :as catalog]
            [datahike.pg.sql.classify :as classify]))

(def ^:dynamic *db* nil)
(def ^:dynamic *search-path* ["$user" "public"])

(def routine-oids #{24 2202})
(def routine-types #{"regproc" "regprocedure"})

(defn- fail! [state message]
  (throw (ex-info message {:sqlstate state})))

(defn- fold-ident [s]
  (str/replace s #"[A-Z]" #(str (char (+ 32 (int (first %)))))))

(defn- truncate-ident [^String s]
  ;; downcase_truncate_identifier / truncate_identifier: NAMEDATALEN - 1
  ;; bytes, preserving complete UTF-8 characters for quoted names too.
  (loop [end (let [end (min 63 (.length s))]
               (if (and (< end (.length s)) (pos? end)
                        (Character/isHighSurrogate (.charAt s (dec end))))
                 (dec end) end))]
    (let [part (subs s 0 end)]
      (if (<= (alength (.getBytes ^String part java.nio.charset.StandardCharsets/UTF_8)) 63)
        part
        (recur (.offsetByCodePoints s end -1))))))

(defn- space? [c]
  (contains? #{\space \tab \newline \return \formfeed \u000b} c))

(defn- skip-space [s i]
  (loop [i i]
    (if (and (< i (count s)) (space? (nth s i)))
      (recur (inc i)) i)))

(defn- identifier
  "SplitIdentifierString's quoted/unquoted component, without SQL comments."
  [s start]
  (let [start (skip-space s start)]
    (when (>= start (count s)) (fail! "42602" "invalid name syntax"))
    (if (= \" (nth s start))
      (loop [i (inc start) out (StringBuilder.)]
        (when (>= i (count s)) (fail! "42602" "invalid name syntax"))
        (let [c (nth s i)]
          (if (= c \")
            (if (and (< (inc i) (count s)) (= \" (nth s (inc i))))
              (recur (+ i 2) (.append out c))
              (if (zero? (.length out))
                (fail! "42602" "invalid name syntax")
                [(truncate-ident (.toString out)) (inc i)]))
            (recur (inc i) (.append out c)))))
      (let [end (loop [i start]
                  (if (and (< i (count s))
                           (not (space? (nth s i)))
                           (not (contains? #{\. \"} (nth s i))))
                    (recur (inc i)) i))]
        (when (= start end) (fail! "42602" "invalid name syntax"))
        [(truncate-ident (fold-ident (subs s start end))) end]))))

(defn- qualified-name [s]
  (loop [i 0 names []]
    (let [[part end] (identifier s i)
          end (skip-space s end)
          names (conj names part)]
      (cond
        (= end (count s)) names
        (= \. (nth s end)) (recur (inc end) names)
        :else (fail! "42602" "invalid name syntax")))))

(defn- numeric-oid [s]
  ;; parseDashOrOid: numeric input is recognized BEFORE name parsing and
  ;; does not trim spaces. An unknown numeric OID remains a valid value.
  (cond
    (= s "-") 0
    (re-matches #"[0-9]+" s)
    (let [n (bigint s)]
      (when (> n 4294967295)
        (fail! "22003" (str "value \"" s "\" is out of range for type oid")))
      (long n))))

(def type-aliases
  {"boolean" "bool" "smallint" "int2" "integer" "int4" "int" "int4"
   "bigint" "int8" "real" "float4" "double precision" "float8"
   "decimal" "numeric" "dec" "numeric" "character varying" "varchar"
   "character" "bpchar" "char" "bpchar" "bit varying" "varbit"
   "char varying" "varchar" "national character" "bpchar"
   "national char" "bpchar" "nchar" "bpchar"
   "national character varying" "varchar" "national char varying" "varchar"
   "nchar varying" "varchar"
   "timestamp without time zone" "timestamp" "timestamp with time zone" "timestamptz"
   "time without time zone" "time" "time with time zone" "timetz"})

(defn- namespace-oid [db n]
  (or (case n "pg_catalog" 11 "public" 2200 nil)
      (when db (:datahike.pg.object/oid (objects/namespace-by-name db n)))))

(defn- search-path [db]
  (let [explicit (vec (distinct (keep #(namespace-oid db %) *search-path*)))]
    (if (some #{11} explicit) explicit (into [11] explicit))))

(defn- namespace-name [db oid]
  (or (case oid 11 "pg_catalog" 2200 "public" nil)
      (when db (:datahike.pg.object/name (objects/namespace-by-oid db oid)))))

(def ^:private integer-input
  ;; input -> types -> this namespace is a load-time dependency chain. Resolve
  ;; at use time so ArrayGetIntegerTypmods shares the real int4 input routine
  ;; rather than gaining a second, almost-compatible integer parser.
  (delay (requiring-resolve 'datahike.pg.input/parse-int4)))

(def ^:private keyword-categories
  (into {} (map (fn [[word category]] [word category]) keywords/keyword-rows)))

(defn- token-name [t]
  (when (contains? #{:ident :quoted} (:type t))
    (truncate-ident (if (= :quoted (:type t)) (:value t) (fold-ident (:text t))))))

(defn- word? [t word]
  (and (= :ident (:type t)) (= word (fold-ident (:text t)))))

(defn- type-syntax! []
  (fail! "42601" "invalid type name syntax"))

(defn- iconst?
  "A positive grammar Iconst, as distinct from signed/decimal expr_list items."
  [tokens]
  (and (= 1 (count tokens))
       (= :number (:type (first tokens)))
       (re-matches #"[0-9]+" (:text (first tokens)))
       (<= (bigint (:text (first tokens))) Integer/MAX_VALUE)))

(defn- array-decoration [tokens]
  (let [start (loop [i 0 depth 0]
                (if (= i (count tokens)) i
                    (let [t (nth tokens i) s (:text t)]
                      (cond
                        (and (zero? depth) (or (= s "[") (word? t "array"))) i
                        (= s "(") (recur (inc i) (inc depth))
                        (= s ")") (recur (inc i) (dec depth))
                        :else (recur (inc i) depth)))))
        suffix (subvec tokens start)
        standard? (word? (first suffix) "array")
        suffix (if standard? (subvec suffix 1) suffix)]
    (loop [ts suffix dimensions 0]
      (when (seq ts)
        (when-not (= "[" (:text (first ts))) (type-syntax!))
        (let [end (first (keep-indexed #(when (= "]" (:text %2)) %1) ts))]
          (when-not end (type-syntax!))
          (let [bound (subvec ts 1 end)]
            (when-not (or (and (empty? bound) (not standard?)) (iconst? bound))
              (type-syntax!)))
          (when (and standard? (pos? dimensions)) (type-syntax!))
          (recur (subvec ts (inc end)) (inc dimensions)))))
    [(subvec tokens 0 start) (or standard? (seq suffix))]))

(defn- modifier-decoration [tokens]
  (if-let [start (first (keep-indexed #(when (= "(" (:text %2)) %1) tokens))]
    (let [end (loop [i (inc start) depth 1]
                (when (= i (count tokens)) (type-syntax!))
                (let [s (:text (nth tokens i))]
                  (cond
                    (= s "(") (recur (inc i) (inc depth))
                    (= s ")") (if (= depth 1) i (recur (inc i) (dec depth)))
                    :else (recur (inc i) depth))))
          mods (subvec tokens (inc start) end)
          groups (loop [ts mods group [] groups []]
                   (if (empty? ts)
                     (conj groups group)
                     (if (= "," (:text (first ts)))
                       (recur (rest ts) [] (conj groups group))
                       (recur (rest ts) (conj group (first ts)) groups))))]
      [(into (subvec tokens 0 start) (subvec tokens (inc end))) groups start])
    [tokens nil nil]))

(defn- modifier-integer [tokens]
  ;; typenameTypeMod permits simple numbers, strings and identifiers. Compound
  ;; expressions are syntax errors; the selected typmodin parses each cstring.
  (let [t (first tokens)
        s (cond
            (and (= 1 (count tokens)) (= :string (:type t))) (:value t)
            (and (= 1 (count tokens)) (contains? #{:number :ident :quoted} (:type t)))
            (if (= :quoted (:type t)) (:value t) (:text t))
            (and (= 2 (count tokens))
                 (contains? #{"+" "-"} (:text t))
                 (= :number (:type (second tokens))))
            (str (:text t) (:text (second tokens)))
            :else (type-syntax!))]
    (@integer-input s)))

(defn- check-modifiers! [type mods]
  (when mods
    (when (= "-" (:typmodin type))
      (fail! "42601" (str "type modifier is not allowed for type \"" (:typname type) "\"")))
    (let [ns (mapv modifier-integer mods)
          n (:typname type)]
      (case n
        "numeric"
        (do
          (when-not (contains? #{1 2} (count ns)) (fail! "22023" "invalid NUMERIC type modifier"))
          (when-not (<= 1 (first ns) 1000) (fail! "22023" "NUMERIC precision must be between 1 and 1000"))
          (when (and (= 2 (count ns)) (not (<= -1000 (second ns) 1000)))
            (fail! "22023" "NUMERIC scale must be between -1000 and 1000")))
        ("bpchar" "varchar" "bit" "varbit")
        (when-not (and (= 1 (count ns))
                       (<= 1 (first ns) (if (contains? #{"bit" "varbit"} n) 83886080 10485760)))
          (fail! "22023" "invalid type modifier"))
        ("timestamp" "timestamptz" "time" "timetz")
        (when-not (and (= 1 (count ns)) (not (neg? (first ns))))
          (fail! "22023" "invalid type modifier"))
        "interval"
        (when-not (and (contains? #{1 2} (count ns))
                       (contains? #{4 2 8 1024 2048 4096 6 1032 3080 7176 3072 7168 6144 32767} (first ns))
                       (or (= 1 (count ns)) (not (neg? (second ns)))))
          (fail! "22023" "invalid INTERVAL type modifier"))
        "vector"
        (when-not (and (= 1 (count ns)) (<= 1 (first ns) 16000))
          (fail! "22023" "invalid vector type modifier"))
        (fail! "0A000" (str "type modifiers for \"" n "\" are not implemented"))))))

(def ^:private interval-fields
  #{["year"] ["month"] ["day"] ["hour"] ["minute"] ["second"]
    ["year" "to" "month"] ["day" "to" "hour"] ["day" "to" "minute"]
    ["day" "to" "second"] ["hour" "to" "minute"] ["hour" "to" "second"]
    ["minute" "to" "second"]})

(defn- closed-quote? [^String text quote start backslash?]
  (loop [i (inc start)]
    (if (>= i (.length text))
      false
      (let [c (.charAt text i)]
        (cond
          (and backslash? (= c \\)) (recur (+ i 2))
          (= c quote)
          (if (and (< (inc i) (.length text)) (= quote (.charAt text (inc i))))
            (recur (+ i 2))
            (= (inc i) (.length text)))
          :else (recur (inc i)))))))

(defn- closed-comment? [^String text]
  (loop [i 2 depth 1]
    (if (>= (inc i) (.length text))
      false
      (let [pair (subs text i (+ i 2))]
        (case pair
          "/*" (recur (+ i 2) (inc depth))
          "*/" (if (= depth 1) (= (+ i 2) (.length text)) (recur (+ i 2) (dec depth)))
          (recur (inc i) depth))))))

(defn- type-tokens [spelling]
  ;; The classifier intentionally tolerates EOF inside opaque tokens. A
  ;; TypeName is an input boundary, so validate closure before interpreting it.
  (let [tokens (vec (classify/tokenize-all spelling))]
    (doseq [{:keys [type text dollar-quoted? closed?]} tokens]
      (when-not
       (case type
         :quoted (and (not= text "\"\"") (closed-quote? text \" 0 false))
         :string (if dollar-quoted? closed?
                     (let [escaped? (boolean (re-find #"^[eE]'" text))]
                       (closed-quote? text \' (if escaped? 1 0) escaped?)))
         :comment (or (not (str/starts-with? text "/*")) (closed-comment? text))
         true)
        (type-syntax!)))
    (vec (remove #(= :comment (:type %)) tokens))))

(defn- parsed-type [spelling]
  (let [tokens (type-tokens spelling)
        [tokens array?] (array-decoration tokens)
        [name-tokens mods mod-pos] (modifier-decoration tokens)
        all-words? (every? #(= :ident (:type %)) name-tokens)
        words (when all-words? (mapv #(fold-ident (:text %)) name-tokens))
        phrase (when words (str/join " " words))
        interval? (= "interval" (first words))
        special? (or (contains? type-aliases phrase)
                     (contains? #{"float" "numeric" "varchar" "timestamp" "time" "interval" "bit"} phrase)
                     (and interval? (contains? interval-fields (subvec words 1))))
        type-name (if special?
                    (cond
                      (= phrase "float")
                      (let [p (when mods
                                (when-not (and (= 1 (count mods)) (iconst? (first mods))) (type-syntax!))
                                (modifier-integer (first mods)))]
                        (when (and p (not (<= 1 p 53))) (fail! "22023" "invalid FLOAT precision"))
                        (if (and p (<= p 24)) "float4" "float8"))
                      interval? "interval"
                      :else (get type-aliases phrase phrase))
                    nil)
        _ (when (and special? mods)
            (cond
              (= phrase "float") nil
              (contains? #{"varchar" "character" "char" "character varying" "char varying"
                           "national character" "national char" "nchar"
                           "national character varying" "national char varying" "nchar varying"
                           "timestamp" "time" "timestamp with time zone" "timestamp without time zone"
                           "time with time zone" "time without time zone"} phrase)
              (when-not (and (= 1 (count mods)) (iconst? (first mods))
                             (if (contains? #{"time" "timestamp"} (first words))
                               (= mod-pos 1) (= mod-pos (count name-tokens))))
                (type-syntax!))
              interval?
              (when-not (and (= 1 (count mods)) (iconst? (first mods))
                             (or (= phrase "interval")
                                 (and (= "second" (last words)) (= mod-pos (count name-tokens)))))
                (type-syntax!))
              (contains? #{"integer" "int" "smallint" "bigint" "real" "double precision" "boolean"} phrase)
              (type-syntax!)))
        names (if type-name
                ["pg_catalog" type-name]
                (loop [ts name-tokens names []]
                  (let [t (first ts) n (token-name t)]
                    (when-not (and n
                                   (or (seq names) (= :quoted (:type t))
                                       (not (contains? #{"C" "R"} (keyword-categories n)))))
                      (type-syntax!))
                    (cond
                      (= 1 (count ts)) (conj names n)
                      (= "." (:text (second ts))) (recur (subvec ts 2) (conj names n))
                      :else (type-syntax!)))))
        mods (cond
               (= phrase "float") nil
               (and interval? mods) [[{:type :number :text "32767"}] (first mods)]
               :else mods)]
    {:names names :mods mods :array? array?}))

(defn- type-oid*
  "parseTypeString for declared routine argument types. Type modifiers are
   validated before being discarded: they do not participate in identity."
  [db spelling]
  (let [{:keys [names mods array?]} (parsed-type spelling)
        n (last names)
        ns-name (when (= 2 (count names)) (first names))
        builtin (when (and (<= (count names) 2)
                           (or (nil? ns-name) (= "pg_catalog" ns-name)))
                  (get catalog/type-by-name n))
        extension (when (and (= n "vector") (<= (count names) 2)
                             (or (nil? ns-name) (= ns-name "public")))
                    {:oid 16383 :typname "vector" :typarray 0 :typmodin ::vector})
        path (if ns-name [(namespace-oid db ns-name)] (search-path db))
        user-oid (when (and db (<= (count names) 2) (some #{2200} path))
                   (or (:datahike.pg.object/oid
                        (objects/object-by-identity db objects/pg-type-oid 2200 n))
                       (some (fn [[name-attr oid-attr]]
                               (d/q {:find '[?oid .] :in '[$ ?n]
                                     :where [['?e name-attr '?n] ['?e oid-attr '?oid]]}
                                    db n))
                             [[:datahike.pg.enum/name :datahike.pg.enum/oid]
                              [:datahike.pg.domain/name :datahike.pg.domain/oid]
                              [:datahike.pg.composite/name :datahike.pg.composite/oid]])))
        candidates (cond-> []
                     builtin (conj [11 builtin])
                     extension (conj [2200 extension])
                     user-oid (conj [2200 {:oid user-oid :user? true}]))
        type (some (fn [ns-oid] (some #(when (= ns-oid (first %)) (second %)) candidates)) path)
        oid (:oid type)]
    (when-not oid (fail! "42704" (str "type \"" spelling "\" does not exist")))
    (when-not (:user? type) (check-modifiers! type mods))
    (when (and (:user? type) mods) (fail! "42601" "type modifier is not allowed"))
    (let [oid (if array? (:typarray type) oid)]
      (or (when (and oid (pos? oid)) oid)
          (fail! "42704" (str "type \"" spelling "\" does not exist"))))))

(defn type-oid
  "Resolve and validate a TypeName. PostgreSQL's type grammar and typmodin
   errors are hard errors even within to_regprocedure; missing types are soft."
  [db spelling]
  (try
    (type-oid* db spelling)
    (catch clojure.lang.ExceptionInfo e
      (if (= "42704" (:sqlstate (ex-data e)))
        (throw e)
        (throw (ex-info (.getMessage e) (assoc (ex-data e) :regoid-hard-error? true) e))))))

(def ^:private extensions
  ;; Preserve the existing extension catalog identities when replacing the
  ;; old hand-written pg_proc projection. These are executable functions;
  ;; PostgreSQL's core catalog does not describe their extension-assigned OIDs.
  (mapv (fn [[name args ret]]
          {:oid (long (Math/abs (.hashCode ^String name))) :proname name
           :args args :ret ret :namespace-oid 2200 :kind "f"
           :strict true :volatile "i" :retset false})
        [["vector_dims" [16383] 23]
         ["vector_norm" [16383] 701]
         ["l2_distance" [16383 16383] 701]
         ["vector_l2_squared_distance" [16383 16383] 701]
         ["l1_distance" [16383 16383] 701]
         ["inner_product" [16383 16383] 701]
         ["cosine_distance" [16383 16383] 701]]))

(def ^:private builtins
  (into (mapv #(assoc % :namespace-oid 11) catalog/procs) extensions))

(def ^:private builtins-by-oid (into {} (map (juxt :oid identity)) builtins))
(def ^:private builtins-by-name (group-by :proname builtins))

(defn- stored-routine [db e]
  (let [ent (d/entity db e)
        full-name (:datahike.pg.function/name ent)
        ;; CREATE FUNCTION currently persists public routines by bare name.
        returns (edn/read-string (:datahike.pg.function/return-type ent))]
    {:oid (:datahike.pg.function/oid ent) :proname full-name
     :namespace-oid 2200
     ;; Stored definitions currently belong to public and keep type spellings.
     ;; Decode them in that defining context, independently of the reader's path.
     :args (binding [*search-path* ["public"]]
             (mapv #(type-oid db %) (edn/read-string (:datahike.pg.function/arg-types ent))))
     :ret (binding [*search-path* ["public"]]
            (type-oid db (or (:type returns) (when (:columns returns) "record") "void")))
     :kind "f" :strict (boolean (:datahike.pg.function/strict? ent))
     :volatile "v" :retset (boolean (:datahike.pg.function/setof? ent))}))

(defn- user-routines
  ([db] (user-routines db nil nil))
  ([db attr value]
   (when db
     (mapv #(stored-routine db (first %))
           (if attr
             (d/q {:find '[?e] :in '[$ ?v]
                   :where [['?e attr '?v]]} db value)
             (d/q '[:find ?e :where [?e :datahike.pg.function/oid]] db))))))

(defn routines
  "Descriptors shared by pg_proc projection and regproc-family lookup.
   Builtin metadata describes PostgreSQL identities, not an assertion that
   every catalog entry has an executable implementation."
  [db]
  (into builtins (user-routines db)))

(defn- candidates [db names]
  (when (> (count names) 2)
    (fail! "42601" "improper qualified name (too many dotted names)"))
  (let [name (last names)
        ns-name (when (= 2 (count names)) (first names))
        nsp (when ns-name (namespace-oid db ns-name))
        ;; FuncnameGetCandidates is called with missing_ok=true by
        ;; regprocin/regprocedurein, including for a nonexistent schema.
        path (if ns-name (if nsp [nsp] []) (search-path db))
        ranks (zipmap path (range))
        all (concat (get builtins-by-name name)
                    (when (contains? ranks 2200)
                      (user-routines db :datahike.pg.function/name name)))]
    ;; Mask only an identical input signature in a later namespace. Different
    ;; overloads remain candidates even when they live in different schemas.
    (->> all
         (filter #(contains? ranks (:namespace-oid %)))
         (sort-by #(get ranks (:namespace-oid %)))
         (reduce (fn [m r] (if (contains? m (:args r)) m (assoc m (:args r) r))) {})
         vals)))

(defn- signature [s]
  ;; Find and split the argument list while respecting quoted identifiers
  ;; and nested typmod parentheses, as parseNameAndArgTypes does.
  (let [n (count s)
        open (loop [i 0 quoted? false]
               (when (= i n) (fail! "22P02" "expected a left parenthesis"))
               (let [c (nth s i)]
                 (cond
                   (and quoted? (= c \") (< (inc i) n) (= \" (nth s (inc i))))
                   (recur (+ i 2) quoted?)
                   (= c \") (recur (inc i) (not quoted?))
                   (and (not quoted?) (= c \()) i
                   :else (recur (inc i) quoted?))))]
    (loop [i (inc open) start (inc open) depth 0 quoted? false args []]
      (when (= i n) (fail! "22P02" "expected a right parenthesis"))
      (let [c (nth s i)]
        (cond
          (and quoted? (= c \") (< (inc i) n) (= \" (nth s (inc i))))
          (recur (+ i 2) start depth quoted? args)
          (= c \") (recur (inc i) start depth (not quoted?) args)
          quoted? (recur (inc i) start depth quoted? args)
          (contains? #{\( \[} c) (recur (inc i) start (inc depth) false args)
          (and (contains? #{\) \]} c) (pos? depth))
          (recur (inc i) start (dec depth) false args)
          (= c \]) (fail! "22P02" "improper type name")
          (and (= c \,) (zero? depth))
          (recur (inc i) (inc i) depth false (conj args (subs s start i)))
          (= c \))
          (do
            (when-not (str/blank? (subs s (inc i)))
              (fail! "22P02" "too much junk after right parenthesis"))
            (let [last-arg (subs s start i)
                  _ (when (and (seq args) (str/blank? last-arg))
                      (fail! "22P02" "expected a type name"))
                  args (if (and (empty? args) (str/blank? last-arg))
                         [] (conj args last-arg))]
              (when (> (count args) 100)
                (fail! "54023" "functions cannot have more than 100 arguments"))
              [(qualified-name (subs s 0 open)) args]))
          :else (recur (inc i) start depth false args))))))

(defn input
  "regprocin/regprocedurein. A numeric value preserves OID identity; text
   resolves names. `to_reg*` callers use input-or-nil for soft errors."
  [kind value]
  (when-not (or (nil? value) (= :__null__ value))
    (let [s (str value)]
      (or (when (integer? value)
            (if (<= -2147483648 value 4294967295)
              (bit-and (long value) 0xffffffff)
              (fail! "22003" "OID out of range")))
          (numeric-oid s)
          (let [[names args] (if (= kind "regprocedure")
                               (let [[names types] (signature s)]
                                 [names (mapv #(type-oid *db* %) types)])
                               [(qualified-name s) nil])
                matches (cond->> (candidates *db* names)
                          (some? args) (filter #(= args (:args %))))]
            (case (count matches)
              0 (fail! "42883" (str "function \"" s "\" does not exist"))
              1 (:oid (first matches))
              (fail! "42725" (str "more than one function named \"" s "\""))))))))

(defn input-or-nil [kind value]
  (try (input kind value)
       (catch clojure.lang.ExceptionInfo e
         (if (and (:sqlstate (ex-data e)) (not (:regoid-hard-error? (ex-data e))))
           nil (throw e)))))

(def ^:private needs-quotes
  (set (for [[word category] keywords/keyword-rows :when (not= category "U")] word)))

(defn- quote-ident [s]
  (if (and (re-matches #"[a-z_][a-z0-9_$]*" s) (not (contains? needs-quotes s)))
    s (str "\"" (str/replace s "\"" "\"\"") "\"")))

(defn- argument-type-name [arg-oid format-type]
  (if-let [t (when *db* (objects/object-by-address *db* objects/pg-type-oid arg-oid))]
    (let [ns-oid (:datahike.pg.object/oid
                  (d/entity *db* (get-in t [:datahike.pg.object/namespace :db/id])))
          name (:datahike.pg.object/name t)
          builtin-oid (:oid (catalog/type-by-name name))
          path (search-path *db*)
          shadowed? (and builtin-oid (< (.indexOf path 11) (.indexOf path ns-oid)))]
      (str (when (or shadowed? (not (some #{ns-oid} path)))
             (str (quote-ident (namespace-name *db* ns-oid)) "."))
           (quote-ident name)))
    (format-type arg-oid)))

(defn output
  "regprocout/regprocedureout. `format-type` supplies PostgreSQL's type
   spelling without coupling this catalog layer to the value-type registry."
  [oid value format-type]
  (let [id (long value)]
    (if (zero? id)
      "-"
      (if-let [r (or (get builtins-by-oid id)
                     (first (user-routines *db* :datahike.pg.function/oid id)))]
        (let [matches (candidates *db* [(:proname r)])
              matches (if (= oid 2202) (filter #(= (:args r) (:args %)) matches) matches)
              visible? (and (= 1 (count matches)) (= id (:oid (first matches))))
              name (str (when-not visible? (str (quote-ident (namespace-name *db* (:namespace-oid r))) "."))
                        (quote-ident (:proname r)))]
          (if (= oid 2202)
            (str name "(" (str/join "," (map #(argument-type-name % format-type) (:args r))) ")") name))
        (str id)))))
