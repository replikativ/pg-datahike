(ns datahike.pg.sql.ddl
  "DDL translation: CREATE TABLE, CREATE SEQUENCE, constraint extraction.

   Produces Datahike tx-data that installs schema attributes (`:db/ident`,
   `:db/valueType`, `:db/cardinality`, `:db/unique`, `:pg/type`, and the
   pgwire-specific `:pg/*` constraint metadata).

   The predicate helpers (`identity-column?`, `column-is-primary-key?`,
   `column-is-unique?`, `column-is-not-null?`, `specs-contain-seq?`,
   `column-check-expr-text`, `column-default-spec`) walk JSqlParser's
   `ColumnDefinition.getColumnSpecs` lists, which are raw strings — so
   each predicate is a case-insensitive token matcher.

   Entry points:
     - translate-create-table    — main; returns {:type :ddl :tx-data [...]}
     - translate-create-sequence — CREATE SEQUENCE name [...]

   Helpers:
     - extract-ddl-constraints — CHECK / NOT NULL / UNIQUE / FK clauses
     - extract-inherits        — PostgreSQL INHERITS"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.constraints.unique :as unique]
            [datahike.pg.errors :as errors]
            [datahike.pg.catalog.regoid :as regoid]
            [datahike.pg.jsonb :as jb]
            [datahike.pg.pg-catalog :as pg-catalog]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sequence :as sequence]
            [datahike.pg.sql.params :as params]
            [datahike.pg.types :as types]
            [datahike.pg.vector :as pg-vector])
  (:import [net.sf.jsqlparser.schema Column Table]
           [net.sf.jsqlparser.expression CastExpression Expression Function StringValue]
           [net.sf.jsqlparser.statement.create.table
            CreateTable ColumnDefinition ColDataType Index
            CheckConstraint ForeignKeyIndex]
           [net.sf.jsqlparser.statement.create.sequence CreateSequence]
           [net.sf.jsqlparser.parser CCJSqlParserUtil]
           [java.nio.charset StandardCharsets]
           [java.util.function Consumer]
           [datahike.pg ColumnSubstitutingDeParser ColumnSubstitutingDeParser$Unsupported
            DefaultExpressionVisitor]))

(set! *warn-on-reflection* true)

;; DDL type mapping delegates to centralized registry
(def ^:private pg-type->dh-type types/sql-name->dh-type)

(def ^:private serial-types
  "SQL type names that imply auto-increment (like IDENTITY)."
  #{"serial" "serial2" "serial4" "serial8" "bigserial" "smallserial"})

(defn primary-key-cols
  "Return the declared primary-key columns of `table-name` in key order.

   PostgreSQL permits `REFERENCES parent` without an explicit column list;
   that shorthand targets the parent's primary key.  Datahike represents a
   single-column key directly and a composite key through our hidden
   `pg$pk_tuple` attribute, so recover both shapes here while translating the
   child DDL."
  [db table-name]
  (when db
    (let [schema (:schema db)
          tuple-props (get schema (keyword table-name "pg$pk_tuple"))]
      (if (seq (:db/tupleAttrs tuple-props))
        (mapv name (:db/tupleAttrs tuple-props))
        (when-let [attr
                   (some (fn [[a props]]
                           (when (and (keyword? a)
                                      (= table-name (namespace a))
                                      (= :db.unique/identity (:db/unique props))
                                      (not= "pg$pk_tuple" (name a)))
                             a))
                         schema)]
          [(name attr)])))))

(defn- schema-unique-keys [schema table columns]
  (let [column-names (set/map-invert columns)]
    (into #{}
          (keep (fn [[attr props]]
                  (when (and (keyword? attr) (= table (namespace attr)) (:db/unique props))
                    (if-let [members (seq (:db/tupleAttrs props))]
                      (when (every? column-names members)
                        (into #{} (map column-names) members))
                      (when-let [column (get column-names attr)]
                        #{column})))))
          schema)))

(defn- attr-enum-name
  "Return the enum type attached to a persisted column attribute."
  [db attr]
  (when db
    (ffirst
     (d/q '{:find [?name]
            :in [$ ?ident]
            :where [[?e :db/ident ?ident]
                    [?e :datahike.pg/enum-of ?name]]}
          db attr))))

(defn- fk-column-attrs [db table]
  (into {} (keep (fn [{:keys [name attr]}]
                   (when-not (= :db/id attr) [name attr])))
        (pgs/column-info (:schema db) table db)))

(def ^:private system-column-names
  #{"tableoid" "ctid" "xmin" "xmax" "cmin" "cmax"})

(defn validate-table-column-name! [column]
  (when (system-column-names column)
    (throw (ex-info (str "column name \"" column "\" conflicts with a system column name")
                    {:error :duplicate-column :sqlstate "42701" :column column}))))

(defn- validate-fk-columns! [columns names]
  (doseq [column names]
    (when-not (contains? columns column)
      (if (system-column-names column)
        (throw (errors/pg-error :feature-not-supported
                                {:message "system columns cannot be used in foreign keys"}))
        (throw (ex-info (str "column \"" column "\" referenced in foreign key constraint does not exist")
                        {:sqlstate "42703" :column column}))))))

(defn resolve-foreign-key!
  "Validate and resolve a foreign key against the catalog snapshot used by DDL.
   The optional local table describes columns and schema still being created."
  [db child {:keys [name cols parent-table parent-cols added-columns on-delete]} local]
  (let [child-columns (if local (:columns local) (into (fk-column-attrs db child) added-columns))
        local-parent? (and local (= child parent-table))
        parent-columns (if local-parent? (:columns local) (fk-column-attrs db parent-table))
        _ (when-not (or local-parent? (seq parent-columns)
                        (get (:schema db) (pgs/row-marker-attr parent-table)))
            (throw (errors/pg-error :undefined-table {:table parent-table})))
        _ (validate-fk-columns! child-columns cols)
        _ (when (and (= :set-null on-delete)
                     (some (fn [attr]
                             (or (get-in local [:schema attr :pg/generated])
                                 (some->> attr (d/entity db) :pg/generated)))
                           (map child-columns cols)))
            (throw (ex-info "invalid ON DELETE action for foreign key constraint containing generated column"
                            {:sqlstate "42601" :constraint name})))
        explicit? (seq parent-cols)
        parent-cols (if explicit? parent-cols
                        (if local-parent? (:primary-key local) (primary-key-cols db parent-table)))]
    (when-not (seq parent-cols)
      (throw (ex-info (str "there is no primary key for referenced table \"" parent-table "\"")
                      {:sqlstate "42830" :constraint name})))
    (validate-fk-columns! parent-columns parent-cols)
    (when (not= (count parent-cols) (count (set parent-cols)))
      (throw (ex-info "foreign key referenced-columns list must not contain duplicates"
                      {:sqlstate "42830" :constraint name})))
    (when (and explicit?
               (not (or (contains? (schema-unique-keys (if local-parent? (:schema local) (:schema db))
                                                       parent-table
                                                       parent-columns)
                                   (set parent-cols))
                        (and (not local-parent?)
                             (some (fn [index]
                                     (and (= parent-table (:table index))
                                          (= (set parent-cols) (into #{} (map :name) (:keys index)))))
                                   (unique/index-descriptors db))))))
      (throw (ex-info (str "there is no unique constraint matching given keys for referenced table \""
                           parent-table "\"")
                      {:sqlstate "42830" :constraint name})))
    (when-not (= (count cols) (count parent-cols))
      (throw (ex-info "number of referencing and referenced columns for foreign key disagree"
                      {:sqlstate "42830" :constraint name})))
    ;; Equal storage types do not make two distinct enum types compatible.
    (let [enum-name (fn [attr]
                      (or (get-in local [:schema attr :datahike.pg/enum-of])
                          (attr-enum-name db attr)))]
      (doseq [[child-col parent-col] (map vector cols parent-cols)
              :let [child-enum (enum-name (get child-columns child-col))
                    parent-enum (enum-name (get parent-columns parent-col))]
              :when (and (or child-enum parent-enum) (not= child-enum parent-enum))]
        (throw (ex-info (str "foreign key constraint \"" name "\" cannot be implemented")
                        {:sqlstate "42804" :constraint name :table child
                         :detail (str "Key columns \"" child-col "\" of the referencing table and \""
                                      parent-col "\" of the referenced table are of incompatible types: "
                                      (or child-enum "non-enum") " and " (or parent-enum "non-enum") ".")}))))
    parent-cols))

(defn identity-column?
  "Check if a JSqlParser ColumnDefinition has a GENERATED AS IDENTITY
   clause or uses a SERIAL type (which implies auto-increment)."
  [^ColumnDefinition col]
  (or
   ;; SERIAL/BIGSERIAL type
   (let [^ColDataType cdt (.getColDataType col)]
     (when cdt
       (contains? serial-types (str/lower-case (str (.getDataType cdt))))))
   ;; JSqlParser emits IDENTITY as a separate token and preserves quotes
   ;; on string defaults.
   (let [specs (set (map #(str/lower-case (str %)) (.getColumnSpecs col)))]
     (and (contains? specs "generated") (contains? specs "identity")))))

(defn stored-generated-column?
  "True for PostgreSQL's `GENERATED ALWAYS AS (...) STORED` columns.
   Identity columns also begin with GENERATED, so exclude those explicitly."
  [^ColumnDefinition col]
  (let [specs (.getColumnSpecs col)]
    (and (seq specs)
         (some #(= "generated" (str/lower-case (str %))) specs)
         (not (identity-column? col)))))

(defn generated-column-spec
  "For `GENERATED ALWAYS AS (expr) STORED`, the expression's text and its
   storage mode; nil for any other column.

   The specs arrive tokenized -- GENERATED, ALWAYS, AS, `(a * 2)`,
   STORED -- so the expression is the token after AS --
   already parenthesised by JSqlParser, and already normalised, which is
   the form `pg_get_expr` should show.

   `VIRTUAL` is PostgreSQL 18's and is REFUSED rather than quietly
   treated as STORED: the two differ in when the expression runs and
   what a later ALTER may do to it, and silently picking one would make
   a column that disagrees with the statement that created it."
  [^ColumnDefinition col]
  (let [specs (mapv str (or (.getColumnSpecs col) []))
        lower (mapv str/lower-case specs)
        as-idx (first (keep-indexed (fn [i w] (when (= "as" w) i)) lower))]
    (when (and (some #{"generated"} lower)
               (not (identity-column? col))
               as-idx
               (< (inc as-idx) (count specs)))
      {:expr (nth specs (inc as-idx))
       :mode (cond (some #{"stored"} lower)  :stored
                   (some #{"virtual"} lower) :virtual
                   ;; PostgreSQL requires one of the two words; without
                   ;; it this is not a generated column it would accept.
                   :else nil)})))

(defn identity-generation
  "Return PostgreSQL's generation mode for an identity column."
  [^ColumnDefinition col]
  (when (and (identity-column? col)
             (not (contains? serial-types
                             (str/lower-case (str (.getDataType (.getColDataType col)))))))
    (let [spec (->> (.getColumnSpecs col)
                    (map str)
                    (str/join " ")
                    str/lower-case)]
      (if (re-find #"\bgenerated\s+always\b" spec)
        "ALWAYS"
        "BY DEFAULT"))))

(defn specs-contain-seq?
  "True when the whitespace-split column-spec list contains the given token
   sequence contiguously, case-insensitive. Used to detect PRIMARY KEY
   (two tokens) and UNIQUE (one token) inline on a ColumnDefinition."
  [specs tokens]
  (let [lowered (mapv #(str/lower-case (str %)) specs)
        needle  (mapv str/lower-case tokens)
        n       (count needle)]
    (when (and (pos? n) (<= n (count lowered)))
      (boolean
       (some (fn [i] (= needle (subvec lowered i (+ i n))))
             (range (inc (- (count lowered) n))))))))

(defn column-is-primary-key?
  "True when a column has inline PRIMARY KEY in its column specs."
  [^ColumnDefinition col]
  (when-let [specs (.getColumnSpecs col)]
    (specs-contain-seq? specs ["primary" "key"])))

(defn column-is-unique?
  "True when a column has inline UNIQUE in its column specs (not part of
   PRIMARY KEY, which we handle separately)."
  [^ColumnDefinition col]
  (when-let [specs (.getColumnSpecs col)]
    (and (specs-contain-seq? specs ["unique"])
         (not (column-is-primary-key? col)))))

(defn column-is-not-null?
  "True when the column-spec list carries `NOT NULL`. PK columns are
   implicitly NOT NULL in PG; callers should combine this with
   column-is-primary-key? to avoid double-flagging."
  [^ColumnDefinition col]
  (when-let [specs (.getColumnSpecs col)]
    (specs-contain-seq? specs ["not" "null"])))

(defn constraint-key
  "The identity of constraint `name` on `table`. PostgreSQL constraint
   names are unique per table, not per database: two tables can each have
   a CHECK called `positive`."
  [table name]
  (str table "\u001f" name))

(defn check-columns
  "The distinct columns a CHECK expression references, or nil when it
   cannot be read. Public because ALTER TABLE ADD CHECK names its
   constraint by the same rule CREATE TABLE does."
  [^String expr]
  (when-let [ast (try (CCJSqlParserUtil/parseCondExpression expr)
                      (catch Exception _ nil))]
    (try
      (let [cols (java.util.LinkedHashSet.)]
        (ColumnSubstitutingDeParser/deparse
         ast (reify java.util.function.Function
               (apply [_ c]
                 (.add cols (params/unquote-ident (.getColumnName ^Column c)))
                 "")))
        (vec cols))
      (catch ColumnSubstitutingDeParser$Unsupported _ nil))))

(defn- utf8-prefix [^bytes encoded length]
  (let [end (loop [end (long length)]
              (if (and (< end (alength encoded))
                       ;; Continuation bytes are 10xxxxxx. Back up to the
                       ;; current character's start before decoding.
                       (= 0x80 (bit-and 0xc0 (aget encoded end))))
                (recur (dec end))
                end))]
    (String. encoded 0 (int end) StandardCharsets/UTF_8)))

(defn- make-object-name
  "Allocate PostgreSQL's byte budget, then clip each part at a UTF-8 boundary."
  [^String name1 ^String name2 ^String label]
  (let [part1 (.getBytes name1 StandardCharsets/UTF_8)
        part2 (when name2 (.getBytes name2 StandardCharsets/UTF_8))
        avail (- params/max-identifier-bytes
                 (inc (alength (.getBytes label StandardCharsets/UTF_8)))
                 (if part2 1 0))
        [n1 n2] (loop [n1 (alength part1) n2 (if part2 (alength part2) 0)]
                  (if (<= (+ n1 n2) avail)
                    [n1 n2]
                    (if (> n1 n2) (recur (dec n1) n2) (recur n1 (dec n2)))))]
    (str (utf8-prefix part1 n1)
         (when part2 (str "_" (utf8-prefix part2 n2)))
         "_" label)))

(defn available-object-name
  "Use PostgreSQL's name1_name2_label, adding a number and shortening
   the components again if a candidate is occupied."
  [occupied? name1 name2 label]
  (some #(let [n (make-object-name name1 name2 (str label %))]
           (when-not (occupied? n) n))
        (cons "" (iterate inc 1))))

(defn- choose-constraint-name [taken name1 name2 label]
  (available-object-name #(contains? taken %) name1 name2 label))

(defn generated-constraint-name
  "PostgreSQL's name for a constraint the statement did not name:
   `t_col_check`, `t_col_fkey`, numbered if taken. Exposed for ALTER
   TABLE ADD, which names its constraints the same way CREATE TABLE does."
  [taken table-name col label]
  (choose-constraint-name (set taken) table-name col label))

(defn- reject-duplicate-constraint-names!
  "42710 for two explicitly named constraints of one table sharing a name,
   worded as PostgreSQL words it for a CHECK."
  [table-name check-names fk-names]
  (let [counts (frequencies (remove nil? (concat check-names fk-names)))]
    (when-let [dup (some (fn [[n c]] (when (> c 1) n)) counts)]
      (throw (ex-info (if (some #{dup} check-names)
                        (str "check constraint \"" dup "\" already exists")
                        (str "constraint \"" dup "\" for relation \""
                             table-name "\" already exists"))
                      {:error :duplicate-object :sqlstate "42710"})))))

(defn reject-check-subquery!
  "PostgreSQL refuses a subquery in a CHECK constraint -- of a table or a
   domain -- when it is created (0A000), rather than on every write. Row-level evaluation
   cannot run one either, so a table accepting it could take no rows."
  [^String expr]
  (when-let [ast (try (CCJSqlParserUtil/parseCondExpression expr)
                      (catch Exception _ nil))]
    (try
      (ColumnSubstitutingDeParser/deparse
       ast (reify java.util.function.Function
             (apply [_ c] (str c))))
      (catch ColumnSubstitutingDeParser$Unsupported e
        (when (= "a subquery" (ex-message e))
          (throw (errors/pg-error :feature-not-supported
                                  {:message "cannot use subquery in check constraint"})))))))

(defn- column-check-name
  "The name of an inline `CONSTRAINT <name> CHECK (…)`, or nil."
  [^ColumnDefinition col]
  (when-let [specs (.getColumnSpecs col)]
    (let [lowered (mapv #(str/lower-case (str %)) specs)
          idx (.indexOf ^java.util.List lowered "check")]
      (when (and (>= idx 2) (= "constraint" (nth lowered (- idx 2))))
        (params/unquote-ident (str (nth specs (dec idx))))))))

(defn column-check-expr-text
  "Extract the text of an inline `CHECK (…)` constraint from a
   ColumnDefinition's ColumnSpecs, or nil if none.

   JSqlParser emits inline CHECK as two tokens: `CHECK` and a second
   token that's the entire parenthesized expression, e.g.
   `[\"CHECK\" \"(x > 0)\"]`. We keep the inner text.

   Also tolerate the tokenized form PG dialect dumps sometimes
   produce — CHECK followed by `(`, individual tokens, `)` — by
   paren-matching across the remaining specs."
  [^ColumnDefinition col]
  (when-let [specs (.getColumnSpecs col)]
    (let [lowered (mapv #(str/lower-case (str %)) specs)
          originals (mapv str specs)
          idx (.indexOf ^java.util.List lowered "check")]
      (when (and (>= idx 0) (< (inc idx) (count lowered)))
        (let [nxt (nth originals (inc idx))]
          (cond
            ;; Single-token case: `(x > 0)` or `(x > 0) NOT NULL` etc.
            (and (str/starts-with? nxt "(")
                 (str/ends-with? nxt ")"))
            (str/trim (subs nxt 1 (dec (count nxt))))
            ;; Single token but partial paren (very rare) — walk forward.
            (str/starts-with? nxt "(")
            (let [concat-rest (str/join " " (subvec originals (inc idx)))
                  idx2 (.indexOf ^String concat-rest (int \())]
              (when (>= idx2 0)
                (let [after (subs concat-rest (inc idx2))
                      ;; paren-matched substring up to matching ).
                      matched
                      (loop [i 0 depth 1 sb (StringBuilder.)]
                        (cond
                          (or (>= i (count after)) (zero? depth)) (str sb)
                          (= \( (.charAt ^String after i))
                          (do (.append sb (.charAt ^String after i))
                              (recur (inc i) (inc depth) sb))
                          (= \) (.charAt ^String after i))
                          (if (= 1 depth)
                            (str sb)
                            (do (.append sb (.charAt ^String after i))
                                (recur (inc i) (dec depth) sb)))
                          :else
                          (do (.append sb (.charAt ^String after i))
                              (recur (inc i) depth sb))))]
                  (str/trim matched))))))))))

(defn parse-default-expression
  "Parse a DEFAULT expression's text, or nil if JSqlParser cannot read it."
  [raw]
  (when-not (str/blank? raw)
    (try
      (CCJSqlParserUtil/parseExpression raw)
      (catch Exception _ nil))))

(defn- static-nextval-argument [^Function call]
  (when (#{"nextval" "pg_catalog.nextval"} (str/lower-case (.getName call)))
    (let [arg (params/unwrap-scalar-parens (first (.getParameters call)))
          arg (if (and (instance? CastExpression arg)
                       (#{"regclass" "pg_catalog.regclass"}
                        (str/lower-case (str (.getColDataType ^CastExpression arg)))))
                (params/unwrap-scalar-parens (.getLeftExpression ^CastExpression arg))
                arg)]
      (when (instance? StringValue arg)
        arg))))

(defn- visit-default-sequence-arguments [value visit]
  (when (and value (str/includes? (str/lower-case value) "nextval"))
    (when-let [^Expression ast (parse-default-expression value)]
      (.accept ast
               (DefaultExpressionVisitor.
                (reify Consumer (accept [_ _]))
                (reify Consumer
                  (accept [_ call]
                    (when-let [arg (static-nextval-argument call)]
                      (visit arg)))))
               nil)
      ast)))

(defn rewrite-default-sequence-references
  "Rewrite literal nextval arguments in a stored expression."
  [value replace-reference]
  (let [changed? (volatile! false)
        rewritten
        (visit-default-sequence-arguments
         value (fn [^StringValue arg]
                 (let [reference (.getNotExcapedValue arg)]
                   (when-let [replacement (replace-reference reference)]
                     (when-not (= reference replacement)
                       (.setValue arg (str/replace replacement "'" "''"))
                       (vreset! changed? true))))))]
    (if @changed? (str rewritten) value)))

(defn default-sequence-references
  "Static sequence names referenced by a stored column default."
  [kind value argument]
  (case kind
    :nextval [(or argument value)]
    :expr (let [references (volatile! [])]
            (visit-default-sequence-arguments
             value (fn [^StringValue arg]
                     (vswap! references conj (.getNotExcapedValue arg))))
            (distinct @references))
    nil))

(defn stored-column-default [db kind value argument]
  (when kind
    (let [resolve-reference (partial sequence/stored-counter-reference db)]
      (case kind
        :nextval [kind value (resolve-reference (or argument value))]
        :expr [kind (if (and (string? value) (re-find #"(?i)\bnextval\s*\(" value))
                      (rewrite-default-sequence-references value resolve-reference)
                      value) argument]
        [kind value argument]))))

(defn- reject-default-expression! [kind]
  (let [[error message]
        (case kind
          "column" [:feature-not-supported "cannot use column reference in DEFAULT expression"]
          "subquery" [:feature-not-supported "cannot use subquery in DEFAULT expression"]
          "aggregate" [:grouping-error "aggregate functions are not allowed in DEFAULT expressions"]
          "window" [:windowing-error "window functions are not allowed in DEFAULT expressions"]
          "set" [:feature-not-supported "set-returning functions are not allowed in DEFAULT expressions"])]
    (throw (errors/pg-error error {:message message}))))

(defn- check-default-function! [^Function call]
  (let [fname (-> (.getName call) str/lower-case (str/replace #"^pg_catalog\." ""))
        procs (get pg-catalog/procs-by-name fname)]
    ;; Avoid inferring the nature of a call by name when it also has
    ;; scalar overloads. This check never evaluates the DEFAULT.
    (when (seq procs)
      (cond
        (every? #(= "a" (:kind %)) procs) (reject-default-expression! "aggregate")
        (every? :retset procs) (reject-default-expression! "set")))))

(defn validate-default-expression! [^Expression expr]
  (.accept expr
           (DefaultExpressionVisitor.
            (reify Consumer (accept [_ kind] (reject-default-expression! kind)))
            (reify Consumer (accept [_ call] (check-default-function! call))))
           nil))

(defn column-default-spec
  "If the column spec list carries `DEFAULT <expr>`, return a map
   describing the default so translate-create-table can attach it to
   the schema entity. Shape:

     {:kind :literal  :value <long|double|string|boolean|nil>}
     {:kind :bit      :value <canonical bit input string>}
     {:kind :bit-coerced :value <quoted input coerced to a bit column>}
     {:kind :fn       :value <canonical fn name string>}
     {:kind :nextval  :value <sequence name string>}
     {:kind :unsupported :raw <original SQL text>}

   The literal branch handles numbers, single-quoted strings, boolean
   keywords, and NULL. The :bit branch prevents a digit-only bit default
   from being reinterpreted as a number by delayed materialization. The
   :fn branch covers the stateless current-*
   functions PG users rely on. :nextval is the only stateful default
   we support and it ties into our existing sequence infrastructure.
   Unknown expressions land in :unsupported so CREATE TABLE can
   raise loudly instead of silently dropping the constraint.

   Walks the ColumnSpecs as whitespace-separated tokens; tolerant of
   parens (e.g. `DEFAULT (now())`)."
  [^ColumnDefinition col]
  (when-let [specs (.getColumnSpecs col)]
    (let [lowered (mapv #(str/lower-case (str %)) specs)
          originals (mapv str specs)
          idx (.indexOf ^java.util.List lowered "default")]
      ;; In `GENERATED BY DEFAULT AS IDENTITY`, DEFAULT is part of the
      ;; identity clause rather than the start of a column default.
      (when (and (not (identity-column? col))
                 (>= idx 0)
                 (< (inc idx) (count lowered)))
        (let [;; Grab tokens up to the next reserved word — NOT,
              ;; CHECK, UNIQUE, PRIMARY, REFERENCES — or end.
              stop? #{"not" "check" "unique" "primary" "references"
                      "generated" "collate"}
              toks (loop [i (inc idx) acc []]
                     (if (or (>= i (count lowered)) (stop? (nth lowered i)))
                       acc
                       (recur (inc i) (conj acc (nth originals i)))))
              raw (str/trim (str/join " " toks))
              ;; Peel outer parens so `DEFAULT (0)` works identically
              ;; to `DEFAULT 0`.
              peeled (if (and (str/starts-with? raw "(")
                              (str/ends-with? raw ")"))
                       (str/trim (subs raw 1 (dec (count raw))))
                       raw)
              low (str/lower-case peeled)
              strip-cast (fn [s]
                           ;; Strip a trailing `::typename` so
                           ;; `0::int`, `'x'::text`, and pg_dump's
                           ;; `'G'::public.mpaa_rating` (cast to a
                           ;; schema-qualified custom type) all
                           ;; parse as the inner literal. We record
                           ;; the value itself; coerce-insert-value
                           ;; adapts it to the column type at INSERT
                           ;; time.
                           (str/replace s #"\s*::\s*[a-zA-Z_][\w. ]*(?:\([^)]*\))?\s*$" ""))
              ;; JSqlParser tokenizes `now()` as ["now" "()"] which
              ;; joins to "now ()" — collapse whitespace between the
              ;; identifier and the parens so the fn-name regex
              ;; below matches regardless of tokenization.
              collapse-fn-parens #(str/replace %
                                               #"([A-Za-z_][A-Za-z0-9_]*)\s*\(\s*\)"
                                               "$1()")
              base (-> peeled strip-cast collapse-fn-parens)
              nextval-match (re-matches #"(?i)\(?\s*(?:pg_catalog\s*\.\s*)?nextval\s*\(\s*'((?:[^']|'')*)'\s*(?:::\s*(?:pg_catalog\s*\.\s*)?regclass)?\s*\)\s*\)?" base)]
          (cond
            (= low "null")  {:kind :literal :value nil}
            (= low "true")  {:kind :literal :value true}
            (= low "false") {:kind :literal :value false}
            ;; Number literal (possibly signed, decimal), kept as its text:
            ;; the column type's input function reads it at write time, so
            ;; `numeric DEFAULT 1.50` keeps its scale (a double lost it).
            (re-matches #"-?\d+(?:\.\d+)?" base)
            {:kind :literal :value base}
            ;; Single-quoted string. PG doubles embedded ' as ''; keep
            ;; that for parity with StringValue literals elsewhere.
            (re-matches #"'(?:[^']|'')*'" base)
            {:kind :literal
             :value (-> base (subs 1 (dec (count base)))
                        (str/replace "''" "'"))}
            ;; SQL-standard bit literal.  Defaults are materialized after
            ;; INSERT expression coercion, so persist the canonical digit
            ;; run used by bit/varbit columns. Hex keeps its input prefix so
            ;; the column's bit coercion can expand each digit to four bits.
            (re-matches #"(?i)b'[01]*'" base)
            {:kind :bit :value (subs base 2 (dec (count base)))}
            (re-matches #"(?i)x'[0-9a-f]*'" base)
            {:kind :bit
             :value (str "x" (subs base 2 (dec (count base))))}
            ;; Known zero-arg functions. Match either form: `now`,
            ;; `now()`, `current_timestamp`, `CURRENT_TIMESTAMP`. Also
            ;; accept the AT TIME ZONE wrapper Odoo uses, for UTC only:
            ;; `now() AT TIME ZONE 'UTC'` is the UTC wall-clock time, which
            ;; is how a timestamp is stored, so it IS `now`. Any other zone
            ;; shifts the time; folding it into `now` stored a wrong value,
            ;; so it is refused below until DEFAULT expressions are
            ;; evaluated by the translator.
            (re-matches #"(?i)(?:now|current_timestamp|statement_timestamp|transaction_timestamp|clock_timestamp)(?:\(\))?(?:\s+at\s+time\s+zone\s+'(?:utc|etc/utc|gmt|z|zulu|\+00|\+00:00)')?"
                        base)
            {:kind :fn :value "now"}
            (re-matches #"(?i)current_date(?:\(\))?" base)
            {:kind :fn :value "current_date"}
            (re-matches #"(?i)current_time(?:\(\))?" base)
            {:kind :fn :value "current_time"}
            (re-matches #"(?i)current_user(?:\(\))?|session_user(?:\(\))?|user"
                        base)
            {:kind :fn :value "current_user"}
            ;; Keep the complete regclass spelling for namespace resolution.
            nextval-match
            {:kind :nextval :value (str/replace (second nextval-match) "''" "'")}
            ;; Anything else is kept as its own text and evaluated per
            ;; write, through the same expression evaluator a CHECK
            ;; constraint uses. Deferred rather than folded here because
            ;; the schema entity is written once and the expression may
            ;; be volatile -- `gen_random_uuid()` folded at CREATE TABLE
            ;; would give every row the same uuid.
            :else
            ;; Anything else is kept as its own text and evaluated per
            ;; write, through the same expression evaluator a CHECK
            ;; constraint uses. Deferred rather than folded here because
            ;; the schema entity is written once and the expression may
            ;; be volatile -- `gen_random_uuid()` folded at CREATE TABLE
            ;; would give every row the same uuid.
            ;;
            ;; The PARSED form is what is stored, not the token join:
            ;; the specs arrive tokenized, so `raw` reads `100 +1` and
            ;; `upper ('ab')`, and that text is what `pg_get_expr` shows
            ;; and what pg_dump writes back out.
            (if-let [expr (or (parse-default-expression peeled)
                              (when (not= peeled raw) (parse-default-expression raw)))]
              (do (validate-default-expression! expr)
                  {:kind :expr :value (str expr)})
              {:kind :unsupported :raw raw})))))))

(defn- referenced-columns [columns]
  (let [names (mapv (comp params/unquote-ident str) columns)]
    (if (= ["__pg_default_pk__"] names) [] names)))

(defn- foreign-key-index->spec
  "The FK shape `extract-ddl-constraints` produces, from a
   `ForeignKeyIndex` -- the node CREATE TABLE and ALTER TABLE both carry."
  [^ForeignKeyIndex fk]
  {:name (some-> (.getName fk) params/unquote-ident)
   :cols (mapv (comp params/unquote-ident str) (.getColumnsNames fk))
   :parent-table (some-> (.getTable fk) .getName params/unquote-ident)
   :parent-schema (some-> (.getTable fk) .getSchemaName params/unquote-ident)
   :parent-cols (referenced-columns (.getReferencedColumnNames fk))
   :on-delete (some-> (.getOnDeleteReferenceOption fk) str/lower-case)
   :on-update (some-> (.getOnUpdateReferenceOption fk) str/lower-case)})

(defn foreign-key-action [action]
  (when action
    (case (str/lower-case action)
      "cascade" :cascade
      "set null" :set-null
      "set default" :set-default
      "restrict" :restrict
      :no-action)))

(defn validate-foreign-key-actions! [constraint on-delete on-update]
  (doseq [[event action] [["DELETE" on-delete] ["UPDATE" on-update]]
          :when (or (= :set-default action)
                    (and (= "UPDATE" event) (contains? #{:cascade :set-null} action)))]
    (throw (ex-info "foreign-key action is not supported"
                    {:error :feature-not-supported
                     :feature (str "FOREIGN KEY ON " event " "
                                   (-> action name (str/replace "-" " ") str/upper-case))
                     :constraint constraint}))))

(defn alter-added-constraint
  "The constraint an `ALTER TABLE ... ADD ...` expression adds, as
   {:op :add-check ...} or {:op :add-foreign-key ...}, or nil when it is
   neither.

   PRIMARY KEY and UNIQUE are read by the caller, which maps them onto
   Datahike's own uniqueness; these two are stored as their own
   constraint entities, the same ones CREATE TABLE writes, so the
   spellings cannot diverge."
  [^net.sf.jsqlparser.statement.alter.AlterExpression exp]
  (let [idx (.getIndex exp)]
    (cond
      (instance? CheckConstraint idx)
      {:op :add-check
       :name (let [n (some-> (.getName ^CheckConstraint idx) params/unquote-ident)]
               ;; The sentinel `alter-add-check-rule` injects so
               ;; JSqlParser accepts an unnamed `ADD CHECK (...)`.
               (when-not (= "__pg_unnamed_check__" n) n))
       :expr (str (.getExpression ^CheckConstraint idx))}

      (instance? ForeignKeyIndex idx)
      (assoc (foreign-key-index->spec idx) :op :add-foreign-key)

      ;; `ADD FOREIGN KEY (a) REFERENCES p (b)` with no CONSTRAINT name
      ;; arrives in these flat accessors instead of an Index node.
      (seq (.getFkColumns exp))
      (let [action (fn [^net.sf.jsqlparser.statement.ReferentialAction$Type t]
                     ;; Not `str` on the ReferentialAction, which renders
                     ;; as " ON DELETE CASCADE" -- the action alone.
                     (some-> (.getReferentialAction exp t)
                             .getAction str str/lower-case
                             (str/replace "_" " ")))
            parent (some-> (.getFkSourceTable exp) regoid/parse-qualified-name)]
        {:op :add-foreign-key
         :name nil
         :cols (mapv params/unquote-ident (.getFkColumns exp))
         :parent-table (peek parent)
         :parent-schema (when (> (count parent) 1) (nth parent (- (count parent) 2)))
         :parent-cols (referenced-columns (.getFkSourceColumns exp))
         :on-delete (action net.sf.jsqlparser.statement.ReferentialAction$Type/DELETE)
         :on-update (action net.sf.jsqlparser.statement.ReferentialAction$Type/UPDATE)})

      :else nil)))

(defn extract-ddl-constraints
  "Collect PRIMARY KEY and UNIQUE constraints from a CreateTable into a
   normalized shape. Merges column-level specs (`id INT PRIMARY KEY`) with
   table-level indexes (`PRIMARY KEY (a,b), UNIQUE (c)`).

   Returns a map:
     :pk-cols       — vector of column names in the PK (empty if none)
     :pk-name       — explicit constraint name for the PK, or nil
     :uniques       — vector of {:cols [str...] :name str-or-nil}
                      each an independent UNIQUE constraint
   Column names are the raw SQL idents (unquoted), not namespaced keywords.
   The caller owns the :t/col namespace mapping."
  [^CreateTable ct]
  (let [columns (or (.getColumnDefinitions ct) [])
        ;; Column-level PRIMARY KEY / UNIQUE specs
        col-pk-cols (vec (keep (fn [^ColumnDefinition col]
                                 (when (column-is-primary-key? col)
                                   (params/unquote-ident (.getColumnName col))))
                               columns))
        col-unique-cols (vec (keep (fn [^ColumnDefinition col]
                                     (when (column-is-unique? col)
                                       (params/unquote-ident (.getColumnName col))))
                                   columns))
        ;; Table-level indexes: PRIMARY KEY (…), UNIQUE (…), CONSTRAINT n UNIQUE (…)
        indexes (or (.getIndexes ct) [])
        ;; `.getType` returns the keyword AS WRITTEN -- JSqlParser does
        ;; not normalise it -- and SQL keywords are case-insensitive. A
        ;; case-SENSITIVE comparison here meant `primary key (x,y)` in
        ;; lower case produced no PK at all, so the composite tuple attr
        ;; that enforces it was never built and two identical rows went
        ;; in where PostgreSQL raises 23505. The ALTER path next door
        ;; already upper-cases; only this one did not.
        index-type (fn [^Index idx] (some-> (.getType idx) str/upper-case))
        index-pk (first (filter (fn [idx] (= "PRIMARY KEY" (index-type idx)))
                                indexes))
        index-pk-cols (when index-pk
                        (mapv (comp params/unquote-ident str)
                              (.getColumns ^Index index-pk)))
        index-pk-name (when index-pk
                        (params/unquote-ident (.getName ^Index index-pk)))
        index-uniques (vec (keep (fn [^Index idx]
                                   (when (#{"UNIQUE" "UNIQUE KEY"} (index-type idx))
                                     {:cols (mapv (comp params/unquote-ident str)
                                                  (.getColumns idx))
                                      :name (some-> (.getName idx) params/unquote-ident)}))
                                 indexes))
        ;; Table-level CHECK constraints — `CheckConstraint` extends
        ;; Index, so they ride along in .getIndexes. Keep the original
        ;; expression as a string; enforcement re-parses it.
        table-checks (vec (keep (fn [^Index idx]
                                  (when (instance? CheckConstraint idx)
                                    {:name (some-> (.getName ^CheckConstraint idx)
                                                   params/unquote-ident)
                                     :expr (str (.getExpression ^CheckConstraint idx))}))
                                indexes))
        ;; Column-level CHECK: `col INT CHECK (…)` — extract from the
        ;; ColumnSpecs text.
        col-checks (vec (keep (fn [^ColumnDefinition col]
                                (when-let [et (column-check-expr-text col)]
                                  {:name (column-check-name col)
                                   :col (params/unquote-ident (.getColumnName col))
                                   :expr et}))
                              columns))
        ;; Table-level FOREIGN KEY — ForeignKeyIndex is also an Index.
        table-fks (into []
                        (comp (filter #(instance? ForeignKeyIndex %))
                              (map foreign-key-index->spec))
                        indexes)]
    {:pk-cols  (or (not-empty index-pk-cols) col-pk-cols)
     :pk-name  index-pk-name
     :uniques  (into (mapv (fn [c] {:cols [c] :name nil}) col-unique-cols)
                     index-uniques)
     :checks   (into col-checks table-checks)
     :fks      table-fks}))

(defn extract-inherits
  "The parent table names of a CREATE TABLE's INHERITS clause, in order,
   or nil when there is none.

   `INHERITS (b, c, a)` names three parents, and JSqlParser hands the
   whole list back as one option string. Reading it as a single
   relation named \"b,c,a\" is what made a multiply-inheriting table
   come out with none of its inherited columns at all."
  [^CreateTable ct]
  (when-let [opts (.getTableOptionsStrings ct)]
    (let [opts-vec (vec opts)
          idx (first (keep-indexed
                      (fn [i option]
                        (when (= "inherits" (str/lower-case (str option))) i))
                      opts-vec))]
      (when (and (some? idx) (< (inc idx) (count opts-vec)))
        (let [raw (str (nth opts-vec (inc idx)))]
          (->> (-> raw
                   (str/replace #"^\(" "")
                   (str/replace #"\)$" "")
                   (str/split #","))
               (map #(params/unquote-ident (str/trim %)))
               (remove str/blank?)
               vec
               seq))))))

(defn pg-type-hint
  "The `:pg/type` a column of SQL type `base-type` should record, or nil
   when the storage valueType already implies the right OID.

   Datahike collapses whole families onto one carrier — every integer is
   :db.type/long, every temporal is :db.type/instant, jsonb and bit are
   :db.type/string — so this hint is the ONLY thing that lets the
   catalog report `date` rather than `timestamp`, or `int4` rather than
   `int8`. Drivers pick codecs off those OIDs.

   Extracted so `ALTER TABLE ... ADD COLUMN` records the same hints
   CREATE TABLE does. It previously recorded them only for json/jsonb,
   so a column added by ALTER as `date` or `smallint` reported the
   storage type forever after.

   `array?` suppresses the scalar hints: an `int[]` column has base-type
   \"int\" too and its array `:pg/type` (\"_int4\") is set elsewhere."
  [^String base-type array?]
  ;; `base-type-name-of` rather than a bare lower-case: a type name can
  ;; arrive quoted (`path` is rewritten to its quoted spelling because
  ;; JSqlParser reserves the word), and an unquoted comparison missed it.
  (let [bt (some-> base-type types/base-type-name-of not-empty)]
    (cond
      (nil? bt) nil

      array? nil

      (#{"jsonb" "json" "bytea" "money" "interval" "tsvector" "tsquery" "vector"
         "regproc" "regprocedure" "regclass" "regtype" "regnamespace"} bt) bt

      ;; The geometric family is carried as canonical text like jsonb, so
      ;; the hint is the only thing that lets the catalog report the
      ;; column's own type. Without it `format_type` answered `text` for
      ;; every one of them but `point`.
      (#{"point" "lseg" "path" "box" "polygon" "line" "circle"} bt) bt

      ;; Same reasoning for the MAC family: the hint is the only thing
      ;; that lets format_type and pg_attribute.atttypid report
      ;; `macaddr` rather than the storage type.
      (#{"macaddr" "macaddr8"} bt) bt

      (#{"date" "time" "timetz" "timestamp" "timestamptz"
         "timestamp without time zone" "timestamp with time zone"
         "time without time zone" "time with time zone"} bt)
      (cond
        (= "timestamp without time zone" bt) "timestamp"
        (= "timestamp with time zone" bt)    "timestamptz"
        (= "time without time zone" bt) "time"
        (= "time with time zone" bt)    "timetz"
        :else bt)

      (#{:bit :varbit} (types/cast-category bt))
      (if (= :varbit (types/cast-category bt)) "varbit" "bit")

      (contains? #{"char" "character" "bpchar"} bt) "bpchar"
      (contains? #{"varchar" "character varying"} bt) "varchar"
      (= "name" bt) "name"

      (#{"smallint" "int2" "smallserial" "serial2"} bt) "int2"
      (#{"integer" "int" "int4" "serial" "serial4"} bt) "int4"
      (= "oid" bt) "oid"
      :else nil)))

(defn translate-create-table
  "Translate a CREATE TABLE statement to Datahike schema transaction data.
   Also returns :table-name, :column-order, :identity-cols, and :inherits."
  [^CreateTable ct db]
  (let [table (.getTable ct)
        table-name (params/unquote-ident (.getName ^Table table))
        ns table-name
        ;; CREATE [GLOBAL|LOCAL] TEMP[ORARY] TABLE — JSqlParser keeps the
        ;; leading keywords in getCreateOptionsStrings. We track temp
        ;; tables in session-specific physical namespaces and drop them when
        ;; the connection closes (see make-query-handler's :temp-tables).
        temp? (boolean (some #(#{"temp" "temporary"} (str/lower-case %))
                             (.getCreateOptionsStrings ct)))
        table-options (->> (.getTableOptionsStrings ct)
                           (map str/lower-case)
                           (str/join " "))
        on-commit-action (some-> (re-find #"\bon\s+commit\s+(drop|delete\s+rows|preserve\s+rows)\b"
                                          table-options)
                                 second)
        _ (when (and on-commit-action (not temp?))
            (throw (errors/pg-error
                    :invalid-table-definition
                    {:message "ON COMMIT can only be used on temporary tables"})))
        _ (when (#{"drop" "delete rows"} on-commit-action)
            (throw (errors/pg-error
                    :feature-not-supported
                    {:feature (str "ON COMMIT " (str/upper-case on-commit-action))})))
        columns (.getColumnDefinitions ct)
        ;; A VIRTUAL generated column (PostgreSQL 18) keeps the explicit
        ;; boundary the STORED ones used to have. Silently creating an
        ;; ordinary writable column also lets a later COPY name it, at
        ;; which point psql enters COPY mode and consumes following SQL
        ;; as row data -- which is why this is loud.
        _ (when-let [col (some (fn [c]
                                 (let [g (generated-column-spec c)]
                                   (when (and g (not= :stored (:mode g))) c)))
                               columns)]
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message (str "virtual generated column \""
                                   (params/unquote-ident (.getColumnName ^ColumnDefinition col))
                                   "\" is not supported")})))
        ;; Detect INHERITS (parent_table) — either from this CREATE TABLE
        ;; directly, or from prior registration (some clients issue CREATE
        ;; TABLE twice: once with INHERITS, then again with the full column
        ;; list without INHERITS — both refer to the same child table).
        parent-from-sql (extract-inherits ct)
        db-table-name (get params/*temp-table-map* table-name table-name)
        parent-from-db (when db
                         (seq (mapv first
                                    (d/q
                                     '{:find [?p]
                                       :where [[?e :__inherit__/child ?c]
                                               [?e :__inherit__/parent ?p]]
                                       :in [$ ?c]}
                                     db db-table-name))))
        parent-tables (or parent-from-sql parent-from-db)
        missing-parent (when (and parent-from-sql db)
                         (some (fn [parent]
                                 (let [db-parent (get params/*temp-table-map*
                                                      parent parent)]
                                   (when (nil? (pgs/column-info
                                                (:schema db) db-parent db))
                                     parent)))
                               parent-from-sql))
        _ (when missing-parent
            (throw (errors/pg-error :undefined-table {:table missing-parent})))
        vector-columns
        (into #{}
              (keep (fn [^ColumnDefinition col]
                      (let [^ColDataType cdt (.getColDataType col)
                            base (types/normalize-sql-type-name
                                  (str (.getDataType cdt)))]
                        (when (= :vector (types/cast-category base))
                          (params/unquote-ident (.getColumnName col))))))
              columns)
        _vector-arrays
        (doseq [^ColumnDefinition col columns
                :let [^ColDataType cdt (.getColDataType col)
                      raw-type (str (.getDataType cdt))
                      base (types/normalize-sql-type-name
                            raw-type)
                      array-data (try (.getArrayData cdt)
                                      (catch Throwable _ nil))]]
          (when (and (types/vector-type-spelling? raw-type)
                     (not= :vector (types/cast-category base)))
            (throw (errors/pg-error
                    :undefined-object
                    {:kind "type" :name raw-type})))
          (when (and (seq array-data)
                     (= :vector (types/cast-category base)))
            (throw (errors/pg-error
                    :feature-not-supported
                    {:message "vector arrays are not supported"}))))
        ;; For INHERITS children, skip `id` — it's shared with parent. Adding
        ;; a child-namespaced id attr would cause auto-increment to assign
        ;; DIFFERENT values for child vs parent, breaking SELECT FROM parent
        ;; WHERE id = N lookups.
        skip-col? (fn [cn]
                    (or (= cn "db_id")
                        (and (seq parent-tables) (= cn "id"))))
        col-names (vec (keep (fn [^ColumnDefinition col]
                               (let [cn (params/unquote-ident (.getColumnName col))]
                                 (when-not (skip-col? cn) cn)))
                             columns))
        ;; PostgreSQL rejects a repeated column name outright. Without
        ;; this the duplicate reached the schema transaction and came
        ;; back as datahike's own unique-constraint message, naming an
        ;; internal attribute and a datom -- an error the caller cannot
        ;; act on, for a mistake in their own statement. It matters
        ;; more now that `LIKE` can introduce the duplicate.
        _ (when-let [dup (->> col-names frequencies
                              (keep (fn [[n c]] (when (> c 1) n)))
                              first)]
            (throw (ex-info (str "column \"" dup "\" specified more than once")
                            {:error :duplicate-column :sqlstate "42701"
                             :column dup})))
        column-types
        (into {}
              (keep (fn [^ColumnDefinition col]
                      (let [cn (params/unquote-ident (.getColumnName col))]
                        (when-not (skip-col? cn)
                          [cn (str (.getColDataType col))]))))
              columns)
        ;; Detect IDENTITY columns and create implicit sequences
        identity-cols (vec (keep (fn [^ColumnDefinition col]
                                   (let [cn (params/unquote-ident (.getColumnName col))]
                                     (when (and (identity-column? col)
                                                (not (skip-col? cn)))
                                       cn)))
                                 columns))
        identity-generations
        (into {}
              (keep (fn [^ColumnDefinition col]
                      (let [cn (params/unquote-ident (.getColumnName col))]
                        (when-let [generation (identity-generation col)]
                          [cn generation]))))
              columns)
        identity-types
        (into {}
              (keep (fn [^ColumnDefinition col]
                      (let [cn (params/unquote-ident (.getColumnName col))
                            raw (some-> col .getColDataType .getDataType str str/lower-case)]
                        (when (identity-column? col)
                          [cn (cond
                                (#{"smallint" "int2" "smallserial" "serial2"} raw) "smallint"
                                (#{"integer" "int" "int4" "serial" "serial4"} raw) "integer"
                                :else "bigint")]))))
              columns)
        ;; Extract PRIMARY KEY + UNIQUE from DDL. The Datahike schema
        ;; mapping is deliberately asymmetric to respect both worlds:
        ;;   single-col PRIMARY KEY → :db.unique/identity
        ;;     (Datomic upsert semantics for Datalog callers; the SQL
        ;;      layer adds a pre-check in translate-insert to raise
        ;;      23505 for bare INSERT collisions.)
        ;;   single-col UNIQUE     → :db.unique/value
        ;;     (Datahike enforces strict uniqueness natively; 23505
        ;;      falls out via :db.error/unique → errors.clj mapping.)
        ;;   multi-col  PRIMARY KEY / UNIQUE → additional :db.type/tuple
        ;;     attr with :db/tupleAttrs pointing at the components, same
        ;;     uniqueness as the single-col case.
        constraints (extract-ddl-constraints ct)
        pk-cols (:pk-cols constraints)
        constrained-vector-cols
        (set (concat pk-cols (mapcat :cols (:uniques constraints))))
        _vector-uniqueness
        (when (seq (set/intersection vector-columns constrained-vector-cols))
          (throw (errors/pg-error
                  :feature-not-supported
                  {:message (str "UNIQUE and PRIMARY KEY constraints on vector "
                                 "columns are not supported")})))
        single-pk? (= 1 (count pk-cols))
        single-pk-col (when single-pk? (first pk-cols))
        pk-cols-set (set pk-cols)
        ;; UNIQUE columns that aren't ALSO the single-col PK (avoid
        ;; double-constraining a single column — PG would reject the
        ;; redundant declaration, we silently dedupe).
        single-unique-cols (into #{}
                                 (comp (filter (comp #(= 1 (count %)) :cols))
                                       (map (comp first :cols))
                                       (remove pk-cols-set))
                                 (:uniques constraints))
        ;; Multi-col PK / UNIQUE — emit a derived tuple attr per
        ;; constraint. The ident name MUST NOT begin with '_' —
        ;; Datahike treats any name starting with underscore as a
        ;; reverse-ref shorthand (Datomic convention) and strips the
        ;; leading _ during attr validation, which then fails to
        ;; resolve. Using `pg$` as a prefix keeps the attrs
        ;; distinguishable from user columns without tripping that
        ;; detection.
        multi-pk-tuple (when (and (seq pk-cols) (> (count pk-cols) 1))
                         {:ident     (keyword ns "pg$pk_tuple")
                          :tuple-cols pk-cols
                          :unique    :db.unique/identity
                          :name      (or (:pk-name constraints)
                                         (str table-name "_pkey"))})
        multi-uniques (vec
                       (keep-indexed
                        (fn [i {:keys [cols name]}]
                          (when (> (count cols) 1)
                            {:ident     (keyword ns (str "pg$unique_tuple_" i))
                             :tuple-cols cols
                             :unique    :db.unique/value
                             :name      (or name
                                            (str table-name "_"
                                                 (str/join "_" cols) "_key"))}))
                        (:uniques constraints)))
        tuple-attrs (into (if multi-pk-tuple [multi-pk-tuple] [])
                          multi-uniques)
        ;; Schema tx-data for columns
        schema-tx (into
                   [{:db/ident       (pgs/row-marker-attr table-name)
                     :db/valueType   :db.type/boolean
                     :db/cardinality :db.cardinality/one}]
                   (for [^ColumnDefinition col columns
                         :let [col-name (params/unquote-ident (.getColumnName col))
                               ^ColDataType cdt (.getColDataType col)
                               raw-type-orig (types/normalize-sql-type-name
                                              (str (.getDataType cdt)))
                               ;; Strip schema prefix from type names. pg_dump
                               ;; emits `public.year`, `public.mpaa_rating` —
                               ;; we have a flat type namespace.
                               type-no-schema (last (str/split raw-type-orig #"\." 2))
                               ;; Check the enum registry first, then the
                               ;; domain registry. Either lowers to a
                               ;; concrete base type for the column.
                               q-fn d/q
                               enum-match (when db
                                            (ffirst (q-fn '{:find [?vs-ord]
                                                            :where [[?e :datahike.pg.enum/name ?n]
                                                                    [?e :datahike.pg.enum/values-ordered ?vs-ord]]
                                                            :in [$ ?n]}
                                                          db type-no-schema)))
                               domain-match (when (and (not enum-match) type-no-schema)
                                              (params/domain-spec-by-name db type-no-schema))
                               ;; Resolve every domain in the chain to the physical base type.
                               raw-type (cond
                                          enum-match "text"
                                          (:array-type domain-match)
                                          (get-in domain-match [:array-type :element-sql])
                                          domain-match (params/domain-storage-type domain-match)
                                          :else raw-type-orig)
                               base-type (str/replace raw-type #"\s*\([^)]*\)" "")
                               _ (when (types/unsupported-input-type? base-type)
                                   (throw (ex-info
                                           (str "type \"" base-type
                                                "\" is not supported until its PostgreSQL input parser is implemented")
                                           {:error :feature-not-supported
                                            :sqlstate "0A000"
                                            :type base-type})))
                               ;; Detect array column. JSqlParser exposes
                               ;; the array dimensions via getArrayData()
                               ;; — a list whose size = ndim (entries are
                               ;; null for unsized `[]`, integers for
                               ;; sized `[3]` which PG ignores anyway).
                               ;; We look up the element-keyword from
                               ;; `base-type` (typmod stripped) — which
                               ;; for `numeric(10,2)[]` resolves to
                               ;; `:numeric`, matching PG's array OIDs.
                               array-data (try (.getArrayData cdt)
                                               (catch Throwable _ nil))
                               ndim       (when (seq array-data) (count array-data))
                               array-spec (or (:array-type domain-match)
                                              (when ndim
                                                (when-let [elem (get types/sql-name->elem-kw base-type)]
                                                  {:elem    elem
                                                   :pg-name (str "_" (name elem))
                                                   :ndim    ndim})))
                               ;; Array columns: store as serialized
                               ;; canonical text in :db.type/string.
                               ;; Round-trip via to-pg-text/from-pg-text.
                               dh-type (cond
                                         array-spec :db.type/string
                                         :else (or (get pg-type->dh-type raw-type)
                                                   (get pg-type->dh-type base-type)
                                                   :db.type/string))
                               ;; NUMERIC(p, s): preserve precision +
                               ;; scale via PG's atttypmod encoding so
                               ;; clients see the declared type. Real
                               ;; PG keeps these in pg_attribute; we
                               ;; tag the schema attr's ident entity
                               ;; with `:pg/typmod` and surface it in
                               ;; the catalog + describeResult. An array's
                               ;; typmod constrains each element; it does
                               ;; not constrain its dimensions or bounds.
                               numeric-typmod
                               (when (or (= dh-type :db.type/bigdec)
                                         (= :numeric (:elem array-spec)))
                                 (let [[p s] (types/parse-numeric-args raw-type)]
                                   (when (or p s)
                                     (types/encode-numeric-typmod p s))))
                               temporal-typmod (or (types/parse-timestamp-precision raw-type)
                                                   (types/parse-time-precision raw-type))
                               ;; varchar(n) / char(n): the declared
                               ;; length was dropped entirely, so the
                               ;; column reported as plain `text` with no
                               ;; character_maximum_length and nothing
                               ;; enforced it on write. PostgreSQL's
                               ;; typmod for these is n + VARHDRSZ.
                               char-typmod
                               (when (= dh-type :db.type/string)
                                 (when-let [n (or (types/parse-char-length raw-type)
                                                  (when (and array-spec
                                                             (contains? #{"char" "character"} base-type))
                                                    1))]
                                   (+ n 4)))
                               bit-typmod
                               (when (= dh-type :db.type/string)
                                 ;; JSqlParser inconsistently includes the
                                 ;; modifier in getDataType for `bit(4)`, but
                                 ;; exposes `varbit(4)` as dataType="varbit"
                                 ;; plus a separate argument list. `str` on
                                 ;; ColDataType faithfully includes both.
                                 (some-> (or (types/parse-bit-length
                                              (if domain-match
                                                raw-type
                                                (str/replace (str cdt) #"(?:\s*\[[^\]]*\])+$" "")))
                                             (when (and array-spec (= "bit" base-type)) 1))
                                         long))
                               vector-typmod
                               (when (= dh-type :db.type/float-array)
                                 (pg-vector/parse-typmod
                                  (types/normalize-sql-type-name (str cdt))))
                               pk-here? (or (and single-pk-col (= col-name single-pk-col))
                                            (contains? pk-cols-set col-name))
                               ;; PRIMARY KEY is implicitly NOT NULL.  The SQL
                               ;; enforcement path reads only :pg/not-null;
                               ;; :db.unique/identity cannot represent that
                               ;; half of the PostgreSQL constraint by itself.
                               not-null-here? (or pk-here?
                                                  (identity-column? col)
                                                  (column-is-not-null? col))
                               default-spec (let [spec (or (column-default-spec col)
                                                           (when (contains? serial-types base-type)
                                                             {:kind :nextval
                                                              :value (str table-name "_" col-name "_seq")}))
                                                  bit-type? (contains?
                                                             #{"bit" "varbit" "bit varying"}
                                                             (types/base-type-name-of raw-type))]
                                              ;; A quoted unknown literal is
                                              ;; coerced to the declared bit
                                              ;; type at CREATE time in PG.
                                              ;; Preserve that intent: the
                                              ;; delayed default evaluator's
                                              ;; legacy :literal path parses
                                              ;; digit-only strings as longs.
                                              (if (and bit-type?
                                                       (= :literal (:kind spec))
                                                       (string? (:value spec)))
                                                (assoc spec :kind :bit-coerced)
                                                spec))
                               _ (when (= :unsupported (:kind default-spec))
                                   (throw (ex-info "DEFAULT expression not supported"
                                                   {:error :feature-not-supported
                                                    :feature (str "DEFAULT expression " (pr-str (:raw default-spec))
                                                                  " on column \"" col-name "\"")
                                                    :detail (str "DEFAULT expression " (pr-str (:raw default-spec))
                                                                 " on column \"" col-name "\" is not supported"
                                                                 " — only literals, now(), current_date,"
                                                                 " current_time, current_user, and"
                                                                 " nextval('seq') are implemented")
                                                    :column col-name})))]
                         :when (not (skip-col? col-name))]
                     (cond-> {:db/ident       (keyword ns col-name)
                              :db/valueType   dh-type
                              :db/cardinality :db.cardinality/one}
                       ;; Array column: tag with element-keyword and ndim
                       ;; so coerce-insert-value / value->string /
                       ;; coerce-pg-array can reconstruct the typed
                       ;; PgArray from the stored canonical text. The
                       ;; `:pg/type "_T"` slot drives column-OID
                       ;; resolution via pg-name->oid in oid_infer.
                       array-spec (assoc :pg/type        (:pg-name array-spec)
                                         :pg/array-elem  (:elem array-spec)
                                         ;; Long-cast for Datahike's
                                         ;; :db.type/long validation
                                         ;; (count returns int).
                                         :pg/array-ndim  (long (:ndim array-spec)))
                       numeric-typmod (assoc :pg/typmod numeric-typmod)
                       temporal-typmod (assoc :pg/typmod temporal-typmod)
                       char-typmod (assoc :pg/typmod char-typmod)
                       bit-typmod (assoc :pg/typmod bit-typmod)
                       vector-typmod (assoc :pg/typmod vector-typmod)
                       (= dh-type :db.type/float-array) (assoc :pg/type "vector")
                       not-null-here? (assoc :pg/not-null true)
                       ;; A STORED generated column: `attgenerated = 's'`
                       ;; plus the expression. It is NOT a default --
                       ;; a default fills an omitted column and this
                       ;; replaces whatever was written -- so it gets
                       ;; its own attributes rather than reusing
                       ;; :pg/default-*, which INSERT consults only when
                       ;; the column was left out.
                       (generated-column-spec col)
                       (as-> m
                             (assoc m :pg/generated "s"
                                    :pg/generated-expr
                                    (:expr (generated-column-spec col))))
                       (and default-spec
                            (not= :unsupported (:kind default-spec)))
                       (as-> m
                             (cond-> m
                               true (assoc :pg/default-kind (:kind default-spec))
                               (some? (:value default-spec))
                               (assoc :pg/default-value (str (:value default-spec)))
                               (= :nextval (:kind default-spec))
                               (assoc :pg/default-arg (:value default-spec))))
                       ;; The declared SQL type when the storage valueType
                       ;; does not imply it (dates/times on instants, json/
                       ;; bit/money on strings, narrow ints on longs). One
                       ;; table, shared with ALTER TABLE ADD COLUMN: this
                       ;; branch used to carry its own copy, and the copies
                       ;; drifted -- neither knew the short name `timetz`,
                       ;; so such columns reported (and rendered) as text.
                       (and (not array-spec) (pg-type-hint base-type false))
                       (assoc :pg/type (pg-type-hint base-type false))

                       ;; ENUM-typed column: remember the enum's name so
                       ;; the dump can re-emit the column as `<enum>`
                       ;; (not `text`). Membership constraints could be
                       ;; bolted on via `:pg/check-in` once the runtime
                       ;; CHECK enforcement lands; for now we only need
                       ;; to round-trip the type identity.
                       enum-match
                       (assoc :datahike.pg/enum-of type-no-schema)

                       ;; DOMAIN-typed column: remember the domain so the
                       ;; dump can emit `<domain>` instead of the lowered
                       ;; base type. The domain's CHECK clause lives on
                       ;; the registry entity and gets re-emitted as part
                       ;; of the CREATE DOMAIN block in the dump header.
                       domain-match
                       (assoc :datahike.pg/domain-of type-no-schema)
                       ;; Single-col PRIMARY KEY → upsert identity.
                       (and single-pk-col (= col-name single-pk-col))
                       (assoc :db/unique :db.unique/identity)
                       ;; Single-col UNIQUE (non-PK) → strict value.
                       (contains? single-unique-cols col-name)
                       (assoc :db/unique :db.unique/value))))
        ;; Derived tuple attrs for multi-col PK / UNIQUE constraints.
        tuple-schema-tx (mapv (fn [{:keys [ident tuple-cols unique]}]
                                {:db/ident       ident
                                 :db/valueType   :db.type/tuple
                                 :db/tupleAttrs  (mapv #(keyword ns %) tuple-cols)
                                 :db/cardinality :db.cardinality/one
                                 :db/unique      unique})
                              tuple-attrs)
        ;; Datahike tuple attributes implement composite SQL uniqueness,
        ;; but they are engine indexes rather than user columns. Hide them
        ;; from SELECT *, pg_attribute and information_schema while keeping
        ;; the underlying attribute available to Datahike.
        tuple-hint-tx (mapv (fn [{:keys [ident]}]
                              {:datahike.pg/for-ident ident
                               :datahike.pg/hidden true})
                            tuple-attrs)
        schema-tx (into schema-tx (into tuple-schema-tx tuple-hint-tx))
        ;; CHECK-constraint entities, one per CHECK clause, identified by
        ;; (table, name) so CREATE TABLE IF NOT EXISTS is idempotent and
        ;; two tables may use the same constraint name. An unnamed CHECK
        ;; is named as PostgreSQL names it: <table>_<column>_check when it
        ;; references exactly one column, else <table>_check, numbered
        ;; when taken.
        _ (doseq [{:keys [expr]} (:checks constraints)]
            (reject-check-subquery! expr))
        _ (reject-duplicate-constraint-names!
           table-name (map :name (:checks constraints)) (map :name (:fks constraints)))
        ;; Names chosen so far, across CHECK and FOREIGN KEY: PostgreSQL
        ;; numbers a generated name that any constraint of the table has.
        constraint-names
        (atom (set (remove nil? (concat (map :name (:checks constraints))
                                        (map :name (:fks constraints))))))
        choose! (fn [explicit name2 label]
                  (let [n (or explicit (choose-constraint-name
                                        @constraint-names table-name name2 label))]
                    (swap! constraint-names conj n)
                    n))
        check-entities
        (vec (for [{:keys [name expr]} (:checks constraints)
                   :let [cols (check-columns expr)
                         cname (choose! name (when (= 1 (count cols)) (first cols)) "check")]]
               {:pg/constraint-key (constraint-key table-name cname)
                :pg/check-conname  cname
                :pg/check-table    table-name
                :pg/check-expr     expr}))
        ;; Store the referenced column list as written. Omitted columns
        ;; resolve against the current catalog when CREATE commits.
        fk-entities
        (vec (for [{:keys [name cols parent-table parent-schema parent-cols on-delete on-update]} (:fks constraints)
                   :let [cname (choose! name (str/join "_" cols) "fkey")
                         od (foreign-key-action on-delete)
                         ou (foreign-key-action on-update)
                         _ (validate-foreign-key-actions! cname od ou)]
                   :when parent-table]
               (cond-> {:pg/constraint-key  (constraint-key table-name cname)
                        :pg/fk-conname      cname
                        :pg/fk-child-table  table-name
                        :pg/fk-child-cols   (jb/serialize-jsonb cols)
                        :pg/fk-parent-table parent-table
                        :pg/fk-parent-cols  (jb/serialize-jsonb parent-cols)}
                 parent-schema (assoc :pg/fk-parent-schema parent-schema)
                 od (assoc :pg/fk-on-delete od)
                 ou (assoc :pg/fk-on-update ou))))
        schema-tx (into schema-tx (into check-entities fk-entities))
        ;; Sequence schema + initial entity for each IDENTITY column
        seq-tx (vec (mapcat (fn [col-name]
                              (let [seq-name (str table-name "_" col-name "_seq")
                                    seq-type (get identity-types col-name "bigint")
                                    max-value (case seq-type
                                                "smallint" 32767
                                                "integer" 2147483647
                                                Long/MAX_VALUE)]
                                [{:db/ident :__seq__/name :db/valueType :db.type/string
                                  :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                                 {:db/ident :__seq__/value :db/valueType :db.type/long
                                  :db/cardinality :db.cardinality/one}
                                 sequence/called-attribute
                                 {:db/ident :__seq__/increment :db/valueType :db.type/long
                                  :db/cardinality :db.cardinality/one}
                                 {:db/ident :__seq__/minvalue :db/valueType :db.type/long
                                  :db/cardinality :db.cardinality/one}
                                 {:db/ident :__seq__/maxvalue :db/valueType :db.type/long
                                  :db/cardinality :db.cardinality/one}
                                 {:db/ident :__seq__/cache :db/valueType :db.type/long
                                  :db/cardinality :db.cardinality/one}
                                 {:db/ident :__seq__/cycle :db/valueType :db.type/boolean
                                  :db/cardinality :db.cardinality/one}
                                 {:db/ident :__seq__/start :db/valueType :db.type/long
                                  :db/cardinality :db.cardinality/one}
                                 {:db/ident :__seq__/type :db/valueType :db.type/string
                                  :db/cardinality :db.cardinality/one}
                                 sequence/identity-generation-attribute
                                 {:db/ident :__seq__/owned-by-table :db/valueType :db.type/string
                                  :db/cardinality :db.cardinality/one}
                                 (cond-> {:__seq__/name seq-name
                                          :__seq__/value 1
                                          :__seq__/is-called false
                                          :__seq__/increment 1
                                          :__seq__/minvalue 1
                                          :__seq__/maxvalue max-value
                                          :__seq__/cache 1
                                          :__seq__/cycle false
                                          :__seq__/start 1
                                          :__seq__/type seq-type
                                          :__seq__/owned-by-table table-name}
                                   (get identity-generations col-name)
                                   (assoc :__seq__/identity-generation
                                          (get identity-generations col-name)))]))
                            identity-cols))]
    (cond-> {:type :ddl-create
             :table-name table-name
             :table-schema (some-> (.getSchemaName ^Table table) params/unquote-ident)
             :temp? temp?
             :if-not-exists? (.isIfNotExists ct)
             :column-order col-names
             :column-types column-types
             :identity-cols identity-cols
             :pk-cols pk-cols
             :pk-name (or (:pk-name constraints)
                          (when (seq pk-cols) (str table-name "_pkey")))
             :unique-cols (vec single-unique-cols)
             :tx-data (into schema-tx seq-tx)}
      (seq parent-tables) (assoc :inherits (vec parent-tables)))))

;; ============================================================================
;; Sequence DDL translation
;; ============================================================================

(def ^:private seq-type-bounds
  "PG's per-type sequence bounds (sequence.c:1454, :1486). The default
   type is bigint."
  {"smallint" [-32768 32767]
   "int2"     [-32768 32767]
   "integer"  [-2147483648 2147483647]
   "int"      [-2147483648 2147483647]
   "int4"     [-2147483648 2147483647]
   "bigint"   [-9223372036854775808 9223372036854775807]
   "int8"     [-9223372036854775808 9223372036854775807]})

(defn- seq-invalid [detail]
  (throw (errors/pg-error :invalid-parameter-value {:detail detail})))

(defn- seq-syntax [detail]
  (throw (errors/pg-error :syntax-error {:detail detail})))

(defn validate-sequence-position! [position minvalue maxvalue]
  (when (some? position)
    (when (< position minvalue)
      (seq-invalid (str "RESTART value (" position ") cannot be less than MINVALUE (" minvalue ")")))
    (when (> position maxvalue)
      (seq-invalid (str "RESTART value (" position ") cannot be greater than MAXVALUE (" maxvalue ")")))))

(defn sequence-params
  "Resolve a classifier option list into concrete sequence parameters,
   applying PG's defaults and raising PG's errors.

   Mirrors `init_params` (postgres src/backend/commands/sequence.c:1260)
   including its ORDER, because a statement with more than one problem
   must report the same one PG reports: duplicate detection, then AS,
   INCREMENT, CYCLE, MAXVALUE, MINVALUE, the min/max crosscheck, START,
   RESTART, CACHE.

   `existing` is the current parameter map for ALTER (nil for CREATE);
   options left unspecified keep their existing values on ALTER and take
   the defaults on CREATE.

   Returns {:type :increment :minvalue :maxvalue :start :cache :cycle?
            :restart :owned-by}."
  [opts {:keys [existing]}]
  ;; Duplicate/conflicting options — 42601, before anything is read.
  ;; NO MINVALUE and MINVALUE n are the same option, so `MINVALUE 5 NO
  ;; MINVALUE` collides here, as does CYCLE with NO CYCLE.
  (let [names (map first opts)]
    (when-let [dup (some (fn [[k n]] (when (> n 1) k)) (frequencies names))]
      (when (contains? #{:as :cache :cycle :increment :maxvalue :minvalue
                         :owned-by :restart :start}
                       dup)
        (seq-syntax "conflicting or redundant options"))))

  (let [o (into {} opts)
        init? (nil? existing)]

    ;; Options PG parses but refuses in this position.
    (when (contains? o :sequence-name)
      (seq-syntax "invalid sequence option SEQUENCE NAME"))
    (when (or (contains? o :logged) (contains? o :unlogged))
      ;; PG reaches this through a bare elog, so it really is XX000
      ;; upstream rather than a user-facing error class.
      (throw (ex-info (str "option \""
                           (if (contains? o :logged) "logged" "unlogged")
                           "\" not recognized")
                      {:sqlstate "XX000"})))

    (let [;; --- AS <type>
          type-name (or (:as o) (:type existing) "bigint")
          [type-min type-max]
          (or (seq-type-bounds type-name)
              (seq-invalid "sequence type must be smallint, integer, or bigint"))
          [old-min old-max] (when (and existing (:as o))
                              (seq-type-bounds (:type existing)))
          reset-max? (and (some? old-max) (= old-max (:maxvalue existing)))
          reset-min? (and (some? old-min) (= old-min (:minvalue existing)))

          ;; --- INCREMENT
          increment (cond
                      (contains? o :increment) (:increment o)
                      existing (:increment existing)
                      :else 1)
          _ (when (zero? increment) (seq-invalid "INCREMENT must not be zero"))
          ascending? (pos? increment)

          ;; --- CYCLE
          cycle? (cond
                   (contains? o :cycle) (:cycle o)
                   existing (:cycle? existing)
                   :else false)

          ;; --- MAXVALUE. `NO MAXVALUE` (:none) means "the default for
          ;; this type and increment sign", NOT unbounded.
          maxvalue (let [v (:maxvalue o)]
                     (cond
                       (and (some? v) (not= v :none)) v
                       (or (= v :none) init? reset-max?) (if (or ascending? reset-max?) type-max -1)
                       :else (:maxvalue existing)))
          _ (when (or (< maxvalue type-min) (> maxvalue type-max))
              (seq-invalid (str "MAXVALUE (" maxvalue ") is out of range for "
                                "sequence data type " type-name)))

          ;; --- MINVALUE
          minvalue (let [v (:minvalue o)]
                     (cond
                       (and (some? v) (not= v :none)) v
                       (or (= v :none) init? reset-min?) (if (and ascending? (not reset-min?)) 1 type-min)
                       :else (:minvalue existing)))
          _ (when (or (< minvalue type-min) (> minvalue type-max))
              (seq-invalid (str "MINVALUE (" minvalue ") is out of range for "
                                "sequence data type " type-name)))

          ;; PG compares with >=, so min == max is rejected even though
          ;; the message says "must be less than".
          _ (when (>= minvalue maxvalue)
              (seq-invalid (str "MINVALUE (" minvalue ") must be less than "
                                "MAXVALUE (" maxvalue ")")))

          ;; --- START
          start (cond
                  (contains? o :start) (:start o)
                  existing (:start existing)
                  :else (if ascending? minvalue maxvalue))
          _ (when (< start minvalue)
              (seq-invalid (str "START value (" start ") cannot be less than "
                                "MINVALUE (" minvalue ")")))
          _ (when (> start maxvalue)
              (seq-invalid (str "START value (" start ") cannot be greater than "
                                "MAXVALUE (" maxvalue ")")))

          ;; --- RESTART. A bare RESTART (:default) restarts at START —
          ;; the START of this same statement, since START is applied
          ;; first. RESTART never changes START itself.
          restart (when (contains? o :restart)
                    (let [v (:restart o)]
                      (if (= v :default) start v)))
          _ (validate-sequence-position! (or restart (:position existing)) minvalue maxvalue)

          ;; --- CACHE
          cache (cond
                  (contains? o :cache) (:cache o)
                  existing (:cache existing)
                  :else 1)
          _ (when (<= cache 0)
              (seq-invalid (str "CACHE (" cache ") must be greater than zero")))]
      {:type type-name
       :increment increment
       :minvalue minvalue
       :maxvalue maxvalue
       :start start
       :cache cache
       :cycle? cycle?
       :restart restart
       :owned-by (:owned-by o)})))

(def sequence-schema
  "Schema attributes backing a sequence entity. Idempotent — re-transacted
   on every CREATE SEQUENCE."
  [{:db/ident :__seq__/name :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :__seq__/value :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   sequence/called-attribute
   {:db/ident :__seq__/increment :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :__seq__/minvalue :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :__seq__/maxvalue :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :__seq__/cache :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :__seq__/cycle :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident :__seq__/start :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :__seq__/type :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :__seq__/owned-by-table :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(defn sequence-entity
  "A fresh sequence stores START and returns it on its first nextval."
  [seq-name {:keys [increment minvalue maxvalue start cache cycle? type]}]
  {:__seq__/name seq-name
   :__seq__/value start
   :__seq__/is-called false
   :__seq__/increment increment
   :__seq__/minvalue minvalue
   :__seq__/maxvalue maxvalue
   :__seq__/cache cache
   :__seq__/cycle cycle?
   :__seq__/start start
   :__seq__/type type})

(defn translate-create-sequence
  "Translate a classified CREATE SEQUENCE into schema + initial entity.

   Consumes the token classifier's `:seq-opts` rather than JSqlParser's
   AST: the grammar there covers only a subset of PG's option list, and
   the values used to be recovered by regex over the re-rendered SQL
   (`increment\\s+by\\s+(\\d+)`), which could not see a negative increment
   and silently dropped MINVALUE/MAXVALUE/CACHE/CYCLE. See issue #21."
  [{:keys [seq-name seq-opts if-not-exists?]}]
  (let [seq-name (params/unquote-ident seq-name)
        params (sequence-params seq-opts {})]
    {:type :ddl-create-sequence
     :seq-name seq-name
     :if-not-exists? if-not-exists?
     :seq-params params
     :tx-data (conj (vec sequence-schema) (sequence-entity seq-name params))}))
