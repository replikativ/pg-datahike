(ns datahike.test.pg-classify-test
  "Tests for the structural SQL classifier. Covers:
   - tokenizer correctness on tricky PG lexical syntax
   - classifier equivalence with the current regex-based system-query?
     (for the kinds that ship)
   - hostile cases: keyword inside a string / comment / dollar-quote"
  (:require [clojure.test :refer [deftest testing is]]
            [datahike.pg.sql.classify :as c]))

;; ============================================================================
;; Tokenizer
;; ============================================================================

(defn- types [sql] (mapv :type (c/tokenize sql)))
(defn- texts [sql] (mapv :text (c/tokenize sql)))

(deftest tokenize-simple
  (is (= [:ident] (types "SELECT")))
  (is (= [:ident :number] (types "x 42")))
  (is (= [:ident :punct :number :punct :number :punct :punct]
         (types "foo(1, 2);"))))

(deftest tokenize-strings
  (testing "single-quoted with '' escape"
    (let [[t & _] (c/tokenize "'hello ''world'''")]
      (is (= :string (:type t)))
      (is (= "hello 'world'" (:value t)))))
  (testing "E'…' with backslash escape — boundaries respected"
    (let [[t & _] (c/tokenize "E'a\\'b'")]
      (is (= :string (:type t)))
      (is (= "a'b" (:value t)))))
  (testing "dollar-quoted empty tag"
    (let [[t & _] (c/tokenize "$$SELECT 1$$")]
      (is (= :string (:type t)))
      (is (= "SELECT 1" (:value t)))))
  (testing "dollar-quoted with named tag"
    (let [[t & _] (c/tokenize "$body$ x $body$tail$body$ y $body$")]
      ;; greedy: first $body$ opens, the NEXT $body$ closes → value is " x "
      (is (= :string (:type t)))
      (is (= " x " (:value t))))))

(deftest tokenize-comments
  (testing "line comment skipped"
    (is (= [:ident :number] (types "SELECT -- ignore me\n 1"))))
  (testing "block comment skipped"
    (is (= [:ident :number] (types "/* hi */ SELECT 1"))))
  (testing "nested block comment"
    (is (= [:ident :number] (types "SELECT /* /* nested */ still-in */ 1")))))

(deftest tokenize-params
  (testing "$N params"
    (let [toks (vec (c/tokenize "$1 + $42"))]
      (is (= :param (:type (nth toks 0))))
      (is (= 1 (:idx (nth toks 0))))
      (is (= 42 (:idx (nth toks 2))))))
  (testing "? JDBC param"
    (let [[t & _] (c/tokenize "?")]
      (is (= :param (:type t)))
      (is (nil? (:idx t)))))
  (testing "?| and ?& are operators, not params"
    (is (= :op (:type (first (c/tokenize "?|")))))
    (is (= :op (:type (first (c/tokenize "?&")))))))

(deftest tokenize-numbers
  (is (= :number (:type (first (c/tokenize "42")))))
  (is (= :number (:type (first (c/tokenize "3.14")))))
  (is (= :number (:type (first (c/tokenize ".5")))))
  (is (= :number (:type (first (c/tokenize "1e10")))))
  (is (= :number (:type (first (c/tokenize "1.5E-3"))))))

(deftest tokenize-ops
  (testing "multi-char operators are one token"
    (is (= "::" (:text (first (c/tokenize "::text")))))
    (is (= "<>" (:text (first (c/tokenize "<>0")))))
    (is (= "||" (:text (first (c/tokenize "||b")))))
    (is (= "@>" (:text (first (c/tokenize "@>x")))))))

(deftest tokenize-quoted-ident
  (let [[t & _] (c/tokenize "\"My Table\"")]
    (is (= :quoted (:type t)))
    (is (= "My Table" (:value t))))
  (testing "\"\" escape inside quoted ident"
    (let [[t & _] (c/tokenize "\"a\"\"b\"")]
      (is (= "a\"b" (:value t))))))

;; ============================================================================
;; Classifier — statement kinds we care about
;; ============================================================================

(defn- kind [sql] (:kind (c/classify sql)))

(deftest classify-authorization-ddl
  (is (= :grant  (kind "GRANT SELECT ON t TO bob")))
  (is (= :grant  (kind "  grant  select  on t to bob")))
  (is (= :revoke (kind "REVOKE ALL ON t FROM bob")))
  (is (= :create-policy (kind "CREATE POLICY p ON t USING (true)")))
  (is (= :alter-policy  (kind "ALTER POLICY p ON t USING (false)")))
  (is (= :drop-policy   (kind "DROP POLICY p ON t")))
  (is (= :create-extension (kind "CREATE EXTENSION pg_trgm")))
  (is (= :drop-extension   (kind "DROP EXTENSION pg_trgm"))))

(deftest classify-rls-on-alter-table
  (is (= :rls (kind "ALTER TABLE t ENABLE ROW LEVEL SECURITY")))
  (is (= :rls (kind "alter table t disable row level security")))
  (is (= :rls (kind "ALTER TABLE t FORCE ROW LEVEL SECURITY")))
  (is (= :rls (kind "ALTER TABLE t NO FORCE ROW LEVEL SECURITY")))
  (is (= :rls (kind "ALTER TABLE IF EXISTS t ENABLE ROW LEVEL SECURITY")))
  ;; ALTER TABLE without RLS → generic pass-through
  (is (= :generic-sql (kind "ALTER TABLE t ADD COLUMN x INT"))))

(deftest classify-transaction-control
  (is (= :begin    (kind "BEGIN")))
  (is (= :begin    (kind "BEGIN;")))
  (is (= :begin    (kind "START TRANSACTION")))
  (is (= :commit   (kind "COMMIT")))
  (is (= :commit   (kind "COMMIT WORK")))
  (is (= :commit   (kind "END")))
  (is (= :rollback (kind "ROLLBACK")))
  (is (= :rollback (kind "ROLLBACK WORK"))))

(deftest classify-savepoint
  (is (= {:kind :savepoint :name "sp1"}
         (c/classify "SAVEPOINT sp1")))
  (is (= {:kind :savepoint :name "sp1"}
         (c/classify "savepoint \"sp1\"")))
  (testing "RELEASE [SAVEPOINT] name"
    (is (= {:kind :release-savepoint :name "sp1"}
           (c/classify "RELEASE sp1")))
    (is (= {:kind :release-savepoint :name "sp1"}
           (c/classify "RELEASE SAVEPOINT sp1"))))
  (testing "ROLLBACK TO [SAVEPOINT] name"
    (is (= {:kind :rollback-to-savepoint :name "sp1"}
           (c/classify "ROLLBACK TO sp1")))
    (is (= {:kind :rollback-to-savepoint :name "sp1"}
           (c/classify "ROLLBACK TO SAVEPOINT sp1"))))
  (testing "quoted savepoint name with dashes (pgjdbc emits UUIDs)"
    (is (= "3e8d481a-3964-11f1"
           (:name (c/classify "SAVEPOINT \"3e8d481a-3964-11f1\""))))))

(deftest classify-select-hijack
  (is (= :version          (kind "SELECT version()")))
  (is (= :now              (kind "SELECT now()")))
  (is (= :now              (kind "SELECT (now() AT TIME ZONE 'UTC')")))
  ;; A trailing cast changes result type + column name, so the
  ;; single-value hijack must NOT fire — the translator handles it
  ;; (issue #13: `SELECT now()::date` rendered as timestamp).
  (is (= :generic-sql      (kind "SELECT now()::date")))
  (is (= :generic-sql      (kind "SELECT version()::text")))
  (is (= :current-schema   (kind "SELECT current_schema()")))
  (is (= :current-database (kind "SELECT current_database()")))
  (is (= :pg-backend-pid   (kind "SELECT pg_backend_pid()")))
  (is (= :txid-current     (kind "SELECT txid_current()")))
  (is (= :nextval          (kind "SELECT nextval('foo')")))
  (is (= :currval          (kind "SELECT currval('foo')")))
  (is (= :setval           (kind "SELECT setval('foo', 10)"))))

(deftest classify-advisory-lock-args
  (is (= {:kind :advisory-lock       :args [42]}
         (c/classify "SELECT pg_advisory_lock(42)")))
  (is (= {:kind :try-advisory-lock   :args [42]}
         (c/classify "SELECT pg_try_advisory_lock(42)")))
  (is (= {:kind :advisory-xact-lock  :args [1 2]}
         (c/classify "SELECT pg_advisory_xact_lock(1, 2)")))
  (is (= {:kind :try-advisory-xact-lock :args [-99]}
         (c/classify "SELECT pg_try_advisory_xact_lock(-99)")))
  (is (= {:kind :advisory-unlock     :args [42]}
         (c/classify "SELECT pg_advisory_unlock(42)")))
  (is (= {:kind :advisory-unlock-all}
         (c/classify "SELECT pg_advisory_unlock_all()"))))

(deftest classify-pg-sleep-duration
  (is (= [0] (:args (c/classify "SELECT pg_sleep(0)"))))
  (is (= [3] (:args (c/classify "SELECT pg_sleep(3)"))))
  ;; fractional not numerically captured (we only take longs); pg_sleep's
  ;; integer path is what migrations use. Fractional still classifies as
  ;; :pg-sleep so the handler runs.
  (is (= :pg-sleep (kind "SELECT pg_sleep(0.5)"))))

(deftest classify-pg-notify
  ;; Odoo's bus issues `SELECT pg_notify(channel, payload)` from a
  ;; post-commit hook on every model write. We classify it as a void
  ;; no-op so it doesn't fall through to the JSqlParser path (which
  ;; would route it to datalog and fail on the unknown function).
  ;;
  ;; psycopg2's SQL.identifier() always double-quotes the function
  ;; name, so the load-bearing form is the quoted variant; the bare
  ;; form is included for parity.
  (is (= :pg-notify (kind "SELECT pg_notify('imbus', '{}')")))
  (is (= :pg-notify (kind "SELECT \"pg_notify\"('imbus', '{}')"))))

(deftest classify-quoted-system-fns
  ;; Regression: psycopg2 quotes EVERY system function it composes via
  ;; SQL.identifier (Odoo's bus uses pg_notify; future modules could
  ;; quote pg_advisory_lock etc.). Make sure the function-name
  ;; dispatch in classify-select accepts both bare and quoted forms.
  (is (= :advisory-lock (kind "SELECT \"pg_advisory_lock\"(42)")))
  (is (= :pg-sleep      (kind "SELECT \"pg_sleep\"(0)")))
  (is (= :now           (kind "SELECT \"now\"()")))
  (is (= :current-database (kind "SELECT \"current_database\"()"))))

(deftest classify-cursors
  (is (= :declare-cursor (kind "DECLARE c1 CURSOR FOR SELECT 1")))
  (is (= :declare-cursor (kind "DECLARE c1 NO SCROLL CURSOR FOR SELECT 1")))
  (is (= "c1" (:name (c/classify "DECLARE c1 CURSOR FOR SELECT 1"))))
  (is (= :fetch-cursor   (kind "FETCH 100 FROM c1")))
  (is (= :close-cursor   (kind "CLOSE c1")))
  (is (= :move-cursor    (kind "MOVE FORWARD 10 FROM c1"))))

(deftest classify-discard
  (is (= :discard-all    (kind "DISCARD ALL")))
  (is (= :discard-scoped (kind "DISCARD PLANS")))
  (is (= "plans"   (:scope (c/classify "DISCARD PLANS"))))
  (is (= "temp"    (:scope (c/classify "DISCARD TEMP")))))

(deftest classify-set-variants
  (testing "datahike temporal vars extract :var and :value"
    (is (= {:kind :set :var "datahike.as_of" :value "2024-01-15T00:00:00Z"}
           (c/classify "SET datahike.as_of = '2024-01-15T00:00:00Z'")))
    (is (= {:kind :set :var "datahike.history" :value "true"}
           (c/classify "SET datahike.history = 'true'"))))
  (testing "SET TIME ZONE captures canonical name 'timezone'"
    (is (= "timezone" (:var (c/classify "SET TIME ZONE 'UTC'"))))
    (is (= "UTC"      (:value (c/classify "SET TIME ZONE 'UTC'")))))
  (testing "SET LOCAL and SESSION modifiers"
    (is (= "search_path" (:var (c/classify "SET LOCAL search_path TO public"))))
    (is (= "search_path" (:var (c/classify "SET SESSION search_path = public"))))))

(deftest classify-maintenance-noops
  (is (= :maintenance-noop (kind "VACUUM")))
  (is (= :maintenance-noop (kind "VACUUM ANALYZE")))
  (is (= :maintenance-noop (kind "REINDEX TABLE foo")))
  (is (= :maintenance-noop (kind "CLUSTER")))
  (is (= :maintenance-noop (kind "ANALYZE"))))

(deftest classify-schema-noops
  (is (= :schema-noop (kind "CREATE SCHEMA foo")))
  (is (= :schema-noop (kind "ALTER SCHEMA foo OWNER TO bar")))
  (is (= :schema-noop (kind "DROP SCHEMA foo"))))

(deftest classify-copy-rejected
  ;; Tier 2 of pgdump-import wired COPY through to a real parser; the
  ;; classifier now routes to :copy-from-stdin (a system-type) instead
  ;; of rejecting at the wire boundary. See pg-copy-parse-test for the
  ;; structured parse coverage.
  (is (= {:kind :copy-from-stdin :tag "COPY"}
         (c/classify "COPY t FROM STDIN")))
  (is (= {:kind :copy-from-stdin :tag "COPY"}
         (c/classify "COPY t TO STDOUT"))))

(deftest classify-passthrough-dml
  (is (= :generic-sql (kind "SELECT * FROM t")))
  (is (= :generic-sql (kind "WITH x AS (SELECT 1) SELECT * FROM x")))
  (is (= :generic-sql (kind "INSERT INTO t VALUES (1)")))
  (is (= :generic-sql (kind "UPDATE t SET x = 1")))
  (is (= :generic-sql (kind "DELETE FROM t")))
  (is (= :generic-sql (kind "CREATE TABLE t (id INT)")))
  (is (= :generic-sql (kind "CREATE TABLE t (id INT REFERENCES p(id))")))
  (is (= :generic-sql (kind "ALTER TABLE t ADD COLUMN y INT")))
  (is (= :generic-sql (kind "MERGE INTO t USING s ON …")))
  (is (= :generic-sql (kind "EXPLAIN SELECT 1")))
  (is (= :generic-sql (kind "TRUNCATE TABLE t"))))

(deftest classify-empty-and-whitespace
  (is (= :empty (kind "")))
  (is (= :empty (kind "   \t\n")))
  (is (= :empty (kind "-- just a comment")))
  (is (= :empty (kind "/* nothing */"))))

;; ============================================================================
;; Hostile cases — keyword inside string / comment / quoted ident
;; ============================================================================

(deftest classify-keyword-inside-string-not-classified
  (testing "GRANT inside a string literal should not trigger rejection"
    (is (= :generic-sql (kind "SELECT 'GRANT' AS action")))
    (is (= :generic-sql (kind "SELECT $$GRANT$$"))))
  (testing "literal 'SAVEPOINT' in projection is not a savepoint"
    (is (= :generic-sql (kind "SELECT 'SAVEPOINT' AS foo")))))

(deftest classify-keyword-inside-comment-not-classified
  (is (= :generic-sql (kind "/* GRANT SELECT ON … */ SELECT 1")))
  (is (= :generic-sql (kind "-- GRANT something\nSELECT 1"))))

(deftest classify-quoted-identifier-not-keyword
  (testing "quoted \"GRANT\" is an identifier, not the GRANT keyword"
    ;; PG: SELECT "GRANT" FROM t  — references a column named GRANT
    (is (= :generic-sql (kind "SELECT \"GRANT\" FROM t")))))

(deftest classify-semicolon-and-batching
  (testing "leading ; is ignored"
    (is (= :begin (kind ";BEGIN"))))
  (testing "classification looks at first statement only"
    (is (= :begin (kind "BEGIN; SELECT 1;")))))

;; ============================================================================
;; Token :end field — required for source-span rewriting
;; ============================================================================

(deftest tokens-have-pos-and-end
  (testing "every token carries :pos and :end"
    (doseq [tok (c/tokenize "SELECT 42 FROM t WHERE x = 'hi' /*c*/ AND y > 0.5")]
      (is (integer? (:pos tok))  (str "no :pos on " tok))
      (is (integer? (:end tok))  (str "no :end on " tok))
      (is (< (:pos tok) (:end tok)) (str "bad span on " tok))))
  (testing ":end is one-past-last — subs sql :pos :end recovers :text"
    (let [sql "SELECT 'abc' + 42"]
      (doseq [tok (c/tokenize sql)]
        (is (= (subs sql (:pos tok) (:end tok)) (:text tok))
            (str "span mismatch for " tok))))))

;; ============================================================================
;; tokenize-all — emits comments (datahike.pg.sql.rewrite span-rewriter needs this)
;; ============================================================================

(deftest tokenize-all-emits-comments
  (testing "line comment becomes a :comment token"
    (let [toks (vec (c/tokenize-all "-- hi\nSELECT 1"))]
      (is (= :comment (:type (first toks))))
      (is (= :ident   (:type (second toks))))))
  (testing "block comment becomes a :comment token"
    (let [toks (vec (c/tokenize-all "/* x */ SELECT 1"))]
      (is (= :comment (:type (first toks))))
      (is (= "/* x */" (:text (first toks))))))
  (testing "consecutive comments emitted as separate tokens"
    (let [toks (vec (c/tokenize-all "/* a */ -- b\n/* c */"))]
      (is (= 3 (count toks)))
      (is (every? #(= :comment (:type %)) toks))))
  (testing "tokenize (the default) still filters comments"
    (is (every? #(not= :comment (:type %))
                (c/tokenize "/* hi */ SELECT -- also\n 1")))))

(deftest classify-select-always-returns-map
  (testing "no more nil — bare SELECT is :generic-sql"
    (is (= {:kind :generic-sql} (c/classify "SELECT 1")))
    (is (= {:kind :generic-sql} (c/classify "SELECT * FROM t")))))

;; ============================================================================
;; Arg extraction for dispatch — PREPARE / EXECUTE / DEALLOCATE, cursors,
;; sequences. These used to be re-regex'd in the server dispatch; classify
;; now carries the structural data directly.
;; ============================================================================

(deftest classify-prepare-carries-name-and-template
  (is (= {:kind :prepare :name "foo" :template "SELECT $1 + 1"}
         (c/classify "PREPARE foo AS SELECT $1 + 1")))
  (testing "trailing semicolon is stripped"
    (is (= "SELECT $1 + 1" (:template (c/classify "PREPARE foo AS SELECT $1 + 1;")))))
  (testing "(arg-types) list is skipped"
    (is (= "SELECT $1 + length($2)"
           (:template (c/classify "PREPARE foo (int, text) AS SELECT $1 + length($2)")))))
  (testing "quoted prepared-statement name is lowercased on capture"
    (is (= "s_1" (:name (c/classify "PREPARE \"S_1\" AS SELECT 1")))))
  (testing "template is nil when AS is missing"
    (is (nil? (:template (c/classify "PREPARE foo"))))))

(deftest classify-execute-carries-name-and-args
  (is (= {:kind :execute-prepared :name "foo" :args-text "1, 'hi'"}
         (c/classify "EXECUTE foo(1, 'hi')")))
  (testing "no args — args-text nil"
    (is (nil? (:args-text (c/classify "EXECUTE foo")))))
  (testing "args with strings containing parens"
    (is (= "'a(b)c'" (:args-text (c/classify "EXECUTE foo('a(b)c')"))))))

(deftest classify-deallocate-all-vs-name
  (is (= {:kind :deallocate :all? false :name "foo"} (c/classify "DEALLOCATE foo")))
  (is (= {:kind :deallocate :all? false :name "foo"} (c/classify "DEALLOCATE PREPARE foo")))
  (is (= {:kind :deallocate :all? true  :name nil}   (c/classify "DEALLOCATE ALL")))
  (is (= {:kind :deallocate :all? true  :name nil}   (c/classify "DEALLOCATE PREPARE ALL"))))

(deftest classify-declare-cursor-carries-name-and-inner-sql
  (is (= {:kind :declare-cursor :name "c1" :inner-sql "SELECT 1"}
         (c/classify "DECLARE c1 CURSOR FOR SELECT 1")))
  (testing "inner SELECT may contain AS, FOR-like words, and other keywords"
    (is (= "SELECT x, y AS renamed FROM t FOR UPDATE"
           (:inner-sql (c/classify "DECLARE c1 CURSOR FOR SELECT x, y AS renamed FROM t FOR UPDATE")))))
  (testing "NO SCROLL / SCROLL / BINARY / WITH HOLD / WITHOUT HOLD qualifiers"
    (is (= "SELECT 1"
           (:inner-sql (c/classify "DECLARE c1 NO SCROLL CURSOR WITH HOLD FOR SELECT 1"))))))

(deftest classify-fetch-move-carries-direction-count-name
  (is (= {:kind :fetch-cursor :direction nil :count 100 :name "c1"}
         (c/classify "FETCH 100 FROM c1")))
  (is (= {:kind :fetch-cursor :direction "forward" :count nil :name "c1"}
         (c/classify "FETCH FORWARD c1")))
  (is (= {:kind :fetch-cursor :direction "forward" :count 10 :name "c1"}
         (c/classify "FETCH FORWARD 10 FROM c1")))
  (is (= {:kind :move-cursor :direction "backward" :count nil :name "c1"}
         (c/classify "MOVE BACKWARD FROM c1")))
  (testing "IN instead of FROM"
    (is (= "c1" (:name (c/classify "FETCH NEXT IN c1"))))))

(deftest classify-close-all-vs-name
  (is (= {:kind :close-cursor :all? false :name "c1"} (c/classify "CLOSE c1")))
  (is (= {:kind :close-cursor :all? true  :name nil}  (c/classify "CLOSE ALL"))))

(deftest classify-sequence-functions-carry-args
  (is (= {:kind :nextval :seq-name "s1"} (c/classify "SELECT nextval('s1')")))
  (is (= {:kind :currval :seq-name "s1"} (c/classify "SELECT currval('s1')")))
  (is (= {:kind :setval :seq-name "s1" :new-value 100}
         (c/classify "SELECT setval('s1', 100)")))
  (testing "setval 3-arg form (is_called flag) — still extracts name + new-value"
    (is (= "s1" (:seq-name (c/classify "SELECT setval('s1', 100, true)"))))))

;; ============================================================================
;; pg_dump utility-statement support (tier 1)
;; ============================================================================

(deftest classify-owner-noop
  (testing "ALTER <object> ... OWNER TO <role> — silently accept by object kind"
    (is (= :owner-noop (kind "ALTER TABLE public.users OWNER TO postgres")))
    (is (= :owner-noop (kind "ALTER SEQUENCE public.users_id_seq OWNER TO postgres")))
    (is (= :owner-noop (kind "ALTER VIEW v OWNER TO bob")))
    (is (= :owner-noop (kind "ALTER INDEX i OWNER TO bob")))
    (is (= :owner-noop (kind "ALTER FUNCTION f() OWNER TO bob")))
    (is (= :owner-noop (kind "ALTER TYPE t OWNER TO bob")))
    (is (= :owner-noop (kind "ALTER MATERIALIZED VIEW mv OWNER TO bob")))
    (is (= :owner-noop (kind "ALTER FOREIGN TABLE ft OWNER TO bob")))
    (is (= :owner-noop (kind "ALTER LARGE OBJECT 12345 OWNER TO bob"))))

  (testing "tag matches the object kind"
    (is (= "ALTER TABLE"    (:tag (c/classify "ALTER TABLE t OWNER TO bob"))))
    (is (= "ALTER SEQUENCE" (:tag (c/classify "ALTER SEQUENCE s OWNER TO bob")))))

  (testing "ALTER without OWNER TO falls through to other handlers"
    (is (= :generic-sql (kind "ALTER TABLE t ADD COLUMN x INT")))
    (is (= :rls         (kind "ALTER TABLE t ENABLE ROW LEVEL SECURITY")))))

(deftest classify-psql-meta
  (testing "psql metacommands at the head of a statement"
    (is (= :psql-meta (kind "\\restrict abc123")))
    (is (= :psql-meta (kind "\\unrestrict abc123")))
    (is (= :psql-meta (kind "\\connect mydb")))
    (is (= :psql-meta (kind "\\c otherdb")))
    (is (= :psql-meta (kind "\\set var value"))))

  (testing "tag preserves the original metacommand name"
    (is (= "\\restrict" (:tag (c/classify "\\restrict abc"))))
    (is (= "\\connect"  (:tag (c/classify "\\connect mydb")))))

  (testing "unknown \\foo metacommands fall through (don't accidentally swallow)"
    (is (not= :psql-meta (kind "\\foobar this is unknown"))))

  (testing "non-meta statements aren't accidentally classified as meta"
    (is (= :begin     (kind "BEGIN")))
    (is (= :generic-sql (kind "SELECT 1")))))

(deftest classify-set-config
  (testing "SELECT pg_catalog.set_config(...) — pg_dump session prelude"
    (is (= :set-config (kind "SELECT pg_catalog.set_config('search_path', '', false)")))
    (is (= :set-config (kind "SELECT set_config('client_encoding', 'UTF8', false)"))))

  (testing "other set_* functions don't accidentally classify as set-config"
    (is (= :setval (kind "SELECT setval('s', 100)")))))
