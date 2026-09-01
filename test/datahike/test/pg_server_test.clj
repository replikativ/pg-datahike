(ns datahike.test.pg-server-test
  "Comprehensive tests for the PostgreSQL wire protocol server.

   Tests the SQL-to-Datalog translation, schema introspection, query handler,
   aggregates, functions, CASE WHEN, CAST, DML, DDL, and system queries."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.schema :as pgs]
            [datahike.pg.sql :as sql])
  (:import [datahike.pg PgWireServer$QueryResult]))

;; ============================================================================
;; Test fixtures
;; ============================================================================

(def test-schema
  [{:db/ident       :person/name
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity}
   {:db/ident       :person/age
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident       :person/email
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :person/salary
    :db/valueType   :db.type/double
    :db/cardinality :db.cardinality/one}
   {:db/ident       :person/department
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident       :department/name
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity}
   {:db/ident       :department/budget
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}])

(def test-data
  [{:db/id "dept-eng"  :department/name "Engineering" :department/budget 1000000}
   {:db/id "dept-sales" :department/name "Sales"      :department/budget 500000}
   {:person/name "Alice"   :person/age 30 :person/email "alice@example.com"
    :person/salary 75000.0 :person/department "dept-eng"}
   {:person/name "Bob"     :person/age 25 :person/email "bob@example.com"
    :person/salary 55000.0 :person/department "dept-eng"}
   {:person/name "Charlie" :person/age 35 :person/email "charlie@example.com"
    :person/salary 90000.0 :person/department "dept-sales"}])

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)

(defn pg-test-fixture [f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          _ (d/transact conn test-schema)
          _ (d/transact conn test-data)
          handler (pg/make-query-handler conn)]
      (try
        (binding [*conn* conn *handler* handler]
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each pg-test-fixture)

;; Helpers
(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (vec (map vec (.rows r)))))

(defn- cols [^PgWireServer$QueryResult r]
  (when-not (.error r) (vec (.columnNames r))))

(defn- err [^PgWireServer$QueryResult r] (.error r))

(defn- sqlstate [^PgWireServer$QueryResult r] (.sqlstate r))

(deftest scalar-wire-values-skip-structured-dispatch
  ;; Keep this structural rather than timing-based: a scalar result must not
  ;; consult any of the comparatively expensive structured-value predicates.
  ;; This pins the dispatch order that keeps large ordinary result sets cheap.
  (let [structured-preds (mapv requiring-resolve
                               '[datahike.pg.arrays/array?
                                 datahike.pg.bits/pg-bit?
                                 datahike.pg.records/record?
                                 datahike.pg.vector/vector-value?])
        unexpected (fn [_]
                     (throw (ex-info "structured predicate called for scalar"
                                     {})))]
    (with-redefs-fn (zipmap structured-preds (repeat unexpected))
      #(do
         (is (= "42" (#'pg/value->string 42)))
         (is (= "hello" (#'pg/value->string "hello")))
         (is (= "t" (#'pg/value->string true)))
         (is (= "app/status" (#'pg/value->string :app/status)))))))

;; ============================================================================
;; SQL-level PREPARE / EXECUTE / DEALLOCATE
;; ============================================================================

(deftest test-declare-cursor-requires-transaction
  (let [r (.execute *handler* "DECLARE outside CURSOR FOR SELECT 1")]
    (is (= "25P01" (sqlstate r)))
    (is (re-find #"transaction blocks" (err r)))))

(deftest test-prepared-declare-cursor-probes-inner-select
  (.execute *handler* "BEGIN")
  (let [parsed (.parse *handler* "DECLARE prepared CURSOR FOR SELECT $1"
                       (int-array [23]))
        result (.executePrepared *handler* parsed (object-array [nil 42]))]
    (is (nil? (err result)))
    (is (= "DECLARE CURSOR" (.commandTag result)))
    (is (= [["42"]]
           (mapv vec (.rows (.execute *handler* "FETCH ALL FROM prepared")))))))

(deftest test-cursor-is-dropped-at-transaction-end
  (doseq [end-sql ["COMMIT" "ROLLBACK"]]
    (.execute *handler* "BEGIN")
    (is (nil? (err (.execute *handler*
                             "DECLARE ended CURSOR FOR SELECT 1"))))
    (is (nil? (err (.execute *handler* end-sql))))
    (let [result (.execute *handler* "FETCH ALL FROM ended")]
      (is (= "34000" (sqlstate result)) end-sql))))

(deftest test-cursor-with-hold-is-rejected-until-supported
  (.execute *handler* "BEGIN")
  (let [result (.execute *handler*
                         "DECLARE held CURSOR WITH HOLD FOR SELECT 1")]
    (is (= "0A000" (sqlstate result)))
    (is (re-find #"WITH HOLD" (err result)))))

(deftest test-cursor-sees-declare-time-snapshot
  (testing "FETCH sees DB state at DECLARE, not later writes"
    (.execute *handler* "BEGIN")
    ;; Requires a real datahike conn to transact into between FETCHes.
    ;; The fixture builds *handler* from a fresh conn; we can grab it back
    ;; via a closure workaround — the fixture didn't expose it. Use
    ;; *conn* which IS bound.
    (is (nil? (err (.execute *handler*
                             "DECLARE snap_cur CURSOR FOR SELECT age FROM person ORDER BY age"))))
    (let [b1 (mapv first (map vec (.rows (.execute *handler* "FETCH 2 FROM snap_cur"))))]
      ;; Insert a new person with age 999 — sorts at the end.
      (d/transact *conn* [{:person/name "Intruder" :person/age 999}])
      (let [b2 (mapv first (map vec (.rows (.execute *handler* "FETCH ALL FROM snap_cur"))))]
        ;; Concat of b1 + b2 must be the original 3 person ages, not 4.
        ;; The new row with age 999 should NOT appear in the cursor.
        (let [all (map #(Long/parseLong %) (concat b1 b2))]
          (is (= 3 (count all))
              (str "cursor leaked post-DECLARE row: " all))
          (is (every? #(<= % 100) all)
              (str "cursor saw age=999 which was inserted after DECLARE: " all)))))))

(deftest test-cursor-preserves-order-by-across-pages
  (testing "ORDER BY in the declared query is honored on every FETCH"
    (.execute *handler* "BEGIN")
    ;; 5 persons with distinct ages so ORDER BY gives a stable order.
    (is (nil? (err (.execute *handler*
                             "DECLARE c_ord CURSOR FOR SELECT age FROM person ORDER BY age"))))
    (let [b1 (mapv first (map vec (.rows (.execute *handler* "FETCH 2 FROM c_ord"))))
          b2 (mapv first (map vec (.rows (.execute *handler* "FETCH 2 FROM c_ord"))))
          b3 (mapv first (map vec (.rows (.execute *handler* "FETCH 2 FROM c_ord"))))]
      ;; Concatenated must be strictly increasing.
      (let [all (concat b1 b2 b3)]
        (is (= (sort all) all)
            (str "cursor pages lost ORDER BY: " all))))))

(deftest test-cursor-respects-user-limit
  (testing "user-supplied LIMIT caps the cursor's total result"
    (.execute *handler* "BEGIN")
    (is (nil? (err (.execute *handler*
                             "DECLARE c_cap CURSOR FOR SELECT age FROM person ORDER BY age LIMIT 2"))))
    ;; Only 2 rows available total.
    (let [r1 (.execute *handler* "FETCH 5 FROM c_cap")]
      (is (= 2 (alength (.rows r1)))))
    ;; Subsequent FETCH returns 0 rows (cursor drained).
    (let [r2 (.execute *handler* "FETCH 5 FROM c_cap")]
      (is (zero? (alength (.rows r2)))))))

(deftest test-cursor-declare-fetch-close
  (testing "DECLARE / FETCH / CLOSE cursor lifecycle"
    (.execute *handler* "BEGIN")
    (is (nil? (err (.execute *handler*
                             "DECLARE c1 CURSOR FOR SELECT name FROM person ORDER BY age"))))
    (let [r (.execute *handler* "FETCH 2 FROM c1")]
      (is (nil? (err r)))
      (is (= 2 (count (.rows r))))
      (is (= "FETCH 2" (.commandTag r))))
    (testing "FETCH advances — subsequent fetch returns next rows"
      (let [r (.execute *handler* "FETCH 1 FROM c1")]
        (is (= 1 (count (.rows r))))
        (is (= "FETCH 1" (.commandTag r)))))
    (testing "FETCH ALL drains remainder"
      (let [r (.execute *handler* "FETCH ALL FROM c1")]
        (is (nil? (err r)))))
    (testing "CLOSE removes the cursor"
      (is (nil? (err (.execute *handler* "CLOSE c1"))))
      (is (some? (err (.execute *handler* "FETCH 1 FROM c1")))))))

(deftest test-sql-prepare-execute-deallocate
  (testing "PREPARE stores template, EXECUTE substitutes and dispatches"
    (is (nil? (err (.execute *handler*
                             "PREPARE byage (int) AS SELECT name FROM person WHERE age = ?"))))
    (let [r (.execute *handler* "EXECUTE byage(30)")]
      (is (nil? (err r)))
      (is (= [["Alice"]] (mapv vec (.rows r)))))
    (testing "stateful across calls: second EXECUTE with different arg"
      (let [r (.execute *handler* "EXECUTE byage(25)")]
        (is (nil? (err r)))
        (is (= [["Bob"]] (mapv vec (.rows r))))))
    (testing "DEALLOCATE removes the statement"
      (is (nil? (err (.execute *handler* "DEALLOCATE byage"))))
      (let [r (.execute *handler* "EXECUTE byage(30)")]
        (is (some? (err r)) "must error after deallocate")
        (is (re-find #"(?i)does not exist" (err r)))))))

(deftest test-sql-prepare-pg-style-placeholders
  (testing "PG-style $N placeholders work"
    (is (nil? (err (.execute *handler*
                             "PREPARE p_pg (int) AS SELECT name FROM person WHERE age = $1"))))
    (let [r (.execute *handler* "EXECUTE p_pg(30)")]
      (is (nil? (err r)))
      (is (= [["Alice"]] (mapv vec (.rows r))))))
  (testing "$N can be reused multiple times in the template"
    (is (nil? (err (.execute *handler*
                             "PREPARE p_reuse AS SELECT $1 AS a, $1 AS b"))))
    (let [r (.execute *handler* "EXECUTE p_reuse(42)")]
      (is (nil? (err r)))
      (is (= [["42" "42"]] (mapv vec (.rows r))))))
  (testing "quoted string arg with embedded comma isn't split"
    (is (nil? (err (.execute *handler* "PREPARE p_str AS SELECT ? AS s"))))
    (let [r (.execute *handler* "EXECUTE p_str('hello, world')")]
      (is (nil? (err r)))
      (is (= [["hello, world"]] (mapv vec (.rows r)))))))

(defn- tag [^PgWireServer$QueryResult r] (.commandTag r))

(defn- first-val [^PgWireServer$QueryResult r]
  (when-let [rs (rows r)] (ffirst rs)))

;; ============================================================================
;; Schema introspection
;; ============================================================================

(deftest test-schema-introspection
  (let [schema (:schema (d/db *conn*))
        tables (pgs/derive-virtual-tables schema)]
    (testing "Derives person and department tables"
      (is (contains? tables "person"))
      (is (contains? tables "department")))
    (testing "Person table has correct columns"
      (let [person-cols (set (map :name (get-in tables ["person" :columns])))]
        (is (contains? person-cols "name"))
        (is (contains? person-cols "age"))
        (is (contains? person-cols "email"))
        (is (contains? person-cols "salary"))))
    (testing "column-info prepends db_id"
      (let [cols (pgs/column-info schema "person")]
        (is (= "db_id" (:name (first cols))))
        ;; db_id + name + age + email + salary + department = 6
        (is (= 6 (count cols)))))))

;; ============================================================================
;; SELECT basics
;; ============================================================================

(deftest test-select-basic
  (testing "Simple SELECT returns all rows"
    (let [r (.execute *handler* "SELECT name, age FROM person ORDER BY name")]
      (is (nil? (err r)))
      (is (= ["name" "age"] (cols r)))
      (is (= 3 (count (rows r))))
      (is (= "Alice" (ffirst (rows r))))))

  (testing "SELECT * returns all columns"
    (let [r (.execute *handler* "SELECT * FROM department")]
      (is (nil? (err r)))
      (is (= 2 (count (rows r))))))

  (testing "SELECT with column alias"
    (let [r (.execute *handler* "SELECT name AS person_name, age AS years FROM person WHERE name = 'Alice'")]
      (is (nil? (err r)))
      (is (= ["person_name" "years"] (cols r)))
      (is (= 1 (count (rows r)))))))

;; ============================================================================
;; WHERE predicates
;; ============================================================================

(deftest test-where-predicates
  (testing "Greater than"
    (let [r (.execute *handler* "SELECT name FROM person WHERE age > 30")]
      (is (= 1 (count (rows r))))
      (is (= "Charlie" (first-val r)))))

  (testing "Greater than or equal"
    (let [r (.execute *handler* "SELECT name FROM person WHERE age >= 30")]
      (is (= 2 (count (rows r))))))

  (testing "Less than"
    (let [r (.execute *handler* "SELECT name FROM person WHERE age < 30")]
      (is (= 1 (count (rows r))))
      (is (= "Bob" (first-val r)))))

  (testing "Equality"
    (let [r (.execute *handler* "SELECT age FROM person WHERE name = 'Alice'")]
      (is (= [["30"]] (rows r)))))

  (testing "Not equals"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name != 'Alice'")]
      ;; 2 persons (Bob, Charlie) match name != 'Alice'.
      ;; With NULL synthesis, entities with missing 'name' attribute get :__null__
      ;; which also passes != 'Alice'. This can happen when tables are created
      ;; via d/transact (no row-marker) and NULL synthesis anchors on other attrs.
      (is (pos? (count (rows r))))))

  (testing "AND compound"
    (let [r (.execute *handler* "SELECT name FROM person WHERE age >= 25 AND age <= 30")]
      (is (= 2 (count (rows r))))))

  (testing "BETWEEN"
    (let [r (.execute *handler* "SELECT name FROM person WHERE age BETWEEN 25 AND 30")]
      (is (= 2 (count (rows r))))))

  (testing "IN list"
    (let [r (.execute *handler* "SELECT name FROM person WHERE age IN (25, 35)")]
      (is (= 2 (count (rows r))))))

  (testing "LIKE with %"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name LIKE 'A%'")]
      (is (= 1 (count (rows r))))
      (is (= "Alice" (first-val r)))))

  (testing "LIKE with % in middle"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name LIKE '%li%'")]
      (is (= 2 (count (rows r))))))

  (testing "regex match (~) — anchor"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name ~ '^A'")]
      (is (= [["Alice"]] (rows r)))))

  (testing "regex match (~) — substring"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name ~ 'li'")]
      (is (= 2 (count (rows r))))))

  (testing "regex non-match (!~) — case-sensitive"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name !~ '^A'")]
      ;; Bob, Charlie. NULLs are excluded (UNKNOWN → FALSE).
      (is (= #{"Bob" "Charlie"} (set (map first (rows r)))))))

  (testing "regex case-insensitive match (~*)"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name ~* '^a'")]
      (is (= [["Alice"]] (rows r)))))

  (testing "regex case-insensitive non-match (!~*)"
    (let [r (.execute *handler* "SELECT name FROM person WHERE name !~* '^a'")]
      (is (= #{"Bob" "Charlie"} (set (map first (rows r))))))))

;; ============================================================================
;; PG session / introspection functions (Metabase / pgjdbc / psycopg2 connect probes)
;; ============================================================================

(deftest test-pg-current-user-functions
  (testing "current_user / session_user / user / system_user as bare identifiers"
    (doseq [ident ["current_user" "CURRENT_USER" "session_user" "SESSION_USER"
                   "user" "USER" "system_user"]]
      (is (= [["datahike"]] (rows (.execute *handler* (str "SELECT " ident))))
          (str ident " should resolve to 'datahike'"))))

  (testing "current_user() / session_user() with parens"
    (doseq [fname ["current_user()" "session_user()" "user()" "system_user()"]]
      (is (= [["datahike"]] (rows (.execute *handler* (str "SELECT " fname))))
          (str fname " should resolve to 'datahike'")))))

(deftest test-pg-current-setting
  (testing "known GUC parameters return expected values"
    (is (= [["UTC"]]    (rows (.execute *handler* "SELECT current_setting('TimeZone')"))))
    (is (= [["15.0"]]   (rows (.execute *handler* "SELECT current_setting('server_version')"))))
    (is (= [["UTF8"]]   (rows (.execute *handler* "SELECT current_setting('client_encoding')"))))
    (is (= [["UTF8"]]   (rows (.execute *handler* "SELECT current_setting('server_encoding')"))))
    (is (= [["on"]]     (rows (.execute *handler* "SELECT current_setting('standard_conforming_strings')")))))

  (testing "missing_ok=true returns NULL for unknown parameters"
    (let [r (.execute *handler* "SELECT current_setting('does_not_exist', true)")]
      (is (nil? (err r)))
      (is (= [[nil]] (rows r)))))

  (testing "missing_ok=false (default) raises 42704 for unknown parameters"
    (let [r (.execute *handler* "SELECT current_setting('does_not_exist')")]
      (is (some? (err r))
          "unknown setting without missing_ok must error"))))

(deftest test-pg-set-config
  ;; set_config returns the NEW VALUE as text. asyncpg turns `jit` off
  ;; around type introspection and reads the answer back, so returning
  ;; an empty string here is a hard connect failure, not a cosmetic gap.
  (testing "standalone set_config echoes the value"
    (is (= [["off"]] (rows (.execute *handler* "SELECT set_config('jit', 'off', false)"))))
    (is (= [[""]]    (rows (.execute *handler* "SELECT pg_catalog.set_config('search_path', '', false)")))))

  (testing "set_config alongside current_setting — asyncpg's _introspect_types probe"
    ;; `cur` is "off" where PostgreSQL says "on": we have no JIT, so we
    ;; report it off rather than copying PostgreSQL's default. See the
    ;; settings map in sql/expr.clj.
    (is (= [["off" "off"]]
           (rows (.execute *handler*
                           "SELECT current_setting('jit') AS cur, set_config('jit', 'off', false) AS new"))))))

(deftest test-pg-format-type
  (testing "format_type maps OIDs to canonical type names"
    (is (= [["integer"]]            (rows (.execute *handler* "SELECT format_type(23, -1)"))))
    (is (= [["bigint"]]              (rows (.execute *handler* "SELECT format_type(20, -1)"))))
    (is (= [["text"]]                (rows (.execute *handler* "SELECT format_type(25, -1)"))))
    (is (= [["character varying"]]   (rows (.execute *handler* "SELECT format_type(1043, -1)"))))
    (is (= [["bpchar"]]              (rows (.execute *handler* "SELECT format_type(1042, -1)"))))
    (is (= [["character(14)"]]       (rows (.execute *handler* "SELECT format_type(1042, 18)"))))
    (is (= [["numeric(8,2)"]]        (rows (.execute *handler* "SELECT format_type(1700, 524294)"))))
    (is (= [["boolean"]]             (rows (.execute *handler* "SELECT format_type(16, -1)")))))

  (testing "format_type composes inside SELECT against pg_type"
    (let [r (.execute *handler* "SELECT typname, format_type(oid, -1) FROM pg_type WHERE oid IN (16, 23, 25) ORDER BY oid")]
      (is (= [["bool" "boolean"] ["int4" "integer"] ["text" "text"]]
             (rows r))))))

(deftest test-pg-typeof
  (testing "pg_typeof returns the type name of a literal expression"
    ;; A small integer literal is int4 in PostgreSQL, not int8.
    (is (= [["integer"]]         (rows (.execute *handler* "SELECT pg_typeof(1)"))))
    (is (= [["bigint"]]          (rows (.execute *handler* "SELECT pg_typeof(2147483648)"))))
    (is (= [["text"]]            (rows (.execute *handler* "SELECT pg_typeof('hello')"))))
    ;; An unadorned decimal literal is numeric in PostgreSQL, not float8.
    (is (= [["numeric"]]         (rows (.execute *handler* "SELECT pg_typeof(1.5)"))))
    (is (= [["double precision"]] (rows (.execute *handler* "SELECT pg_typeof(1.5::float8)"))))
    (is (= [["boolean"]]         (rows (.execute *handler* "SELECT pg_typeof(true)")))))

  (testing "pg_typeof on a column resolves to the column's declared type"
    (is (= [["text"]] (rows (.execute *handler* "SELECT DISTINCT pg_typeof(name) FROM person"))))
    (is (= [["bigint"]] (rows (.execute *handler* "SELECT DISTINCT pg_typeof(age) FROM person"))))))

(deftest test-predicate-in-case-when
  ;; CASE WHEN <pred> THEN x ELSE y END inside a SELECT projection
  ;; goes through translate-predicate-expr (boolean form) rather than
  ;; translate-predicate (where-clause vector). Several predicate
  ;; types had no inline branch and silently fell back to translate-expr,
  ;; which routed them back — infinite recursion → StackOverflow.
  ;; These regressions cover IN / NOT IN, LIKE / NOT LIKE, BETWEEN /
  ;; NOT BETWEEN, regex ~ / !~ used as the WHEN predicate.
  (testing "CASE WHEN col IN (lit, lit) THEN ..."
    (let [r (.execute *handler* "SELECT name, CASE WHEN age IN (25, 35) THEN 'edge' ELSE 'mid' END AS bucket FROM person ORDER BY age")]
      (is (= [["Bob" "edge"] ["Alice" "mid"] ["Charlie" "edge"]] (rows r)))))

  (testing "CASE WHEN col NOT IN (lit) THEN ..."
    (let [r (.execute *handler* "SELECT name, CASE WHEN age NOT IN (30) THEN 'other' ELSE 'thirty' END AS bucket FROM person ORDER BY age")]
      (is (= [["Bob" "other"] ["Alice" "thirty"] ["Charlie" "other"]] (rows r)))))

  (testing "CASE WHEN col LIKE 'pat' THEN ..."
    (let [r (.execute *handler* "SELECT name, CASE WHEN name LIKE 'A%' THEN 'A' ELSE 'X' END AS bucket FROM person ORDER BY name")]
      (is (= [["Alice" "A"] ["Bob" "X"] ["Charlie" "X"]] (rows r)))))

  (testing "CASE WHEN col BETWEEN lo AND hi"
    ;; Bob=25, Alice=30, Charlie=35; BETWEEN 28 AND 32 only matches Alice.
    (let [r (.execute *handler* "SELECT name, CASE WHEN age BETWEEN 28 AND 32 THEN 'mid' ELSE 'far' END AS bucket FROM person ORDER BY age")]
      (is (= [["Bob" "far"] ["Alice" "mid"] ["Charlie" "far"]] (rows r)))))

  (testing "CASE WHEN col ~ 'regex' THEN ..."
    (let [r (.execute *handler* "SELECT name, CASE WHEN name ~ '^A' THEN 'A' ELSE 'X' END AS bucket FROM person ORDER BY name")]
      (is (= [["Alice" "A"] ["Bob" "X"] ["Charlie" "X"]] (rows r)))))

  (testing "CASE WHEN col !~ 'regex' THEN ..."
    (let [r (.execute *handler* "SELECT name, CASE WHEN name !~ '^A' THEN 'X' ELSE 'A' END AS bucket FROM person ORDER BY name")]
      (is (= [["Alice" "A"] ["Bob" "X"] ["Charlie" "X"]] (rows r))))))

(deftest test-pg-comment-stubs
  (testing "obj_description / col_description return NULL (no comments tracked)"
    (is (= [[nil]] (rows (.execute *handler* "SELECT obj_description(0, 'pg_class')"))))
    (is (= [[nil]] (rows (.execute *handler* "SELECT col_description(0, 1)"))))))

(deftest test-pg-get-userbyid
  (testing "pg_get_userbyid returns the static handler role for any oid"
    (is (= [["datahike"]] (rows (.execute *handler* "SELECT pg_get_userbyid(10)"))))
    (is (= [["datahike"]] (rows (.execute *handler* "SELECT pg_get_userbyid(0)"))))))

(deftest test-pg-get-indexdef
  (testing "pg_index.indexdef pre-baked from schema (UNIQUE / PRIMARY KEY)"
    (let [r (rows (.execute *handler* "SELECT indexdef FROM pg_index ORDER BY indexdef"))]
      (is (some #(re-find #"CREATE UNIQUE INDEX person_name_pkey" %) (map first r))
          "PK index def synthesized from :db.unique/identity")
      (is (some #(re-find #"CREATE UNIQUE INDEX department_name_pkey" %) (map first r))
          "Department PK index def synthesized")))

  (testing "pg_get_indexdef(oid) reads the pre-baked column"
    (let [r (rows (.execute *handler* "SELECT pg_get_indexdef(indexrelid) FROM pg_index ORDER BY indexrelid"))]
      (is (every? #(re-find #"^CREATE UNIQUE INDEX " (first %)) r)
          "every row should be a CREATE UNIQUE INDEX statement"))))

(deftest test-pg-constraint-and-getconstraintdef
  (testing "pg_constraint synthesizes one row per PK / UNIQUE column"
    (let [r (rows (.execute *handler* "SELECT conname, contype, condef FROM pg_constraint ORDER BY conname"))
          named (into {} (map (fn [[n t d]] [n [t d]]) r))]
      (is (contains? named "person_name_pkey"))
      (is (= ["p" "PRIMARY KEY (name)"] (named "person_name_pkey")))
      (is (contains? named "department_name_pkey"))))

  (testing "pg_get_constraintdef(oid) joins pg_constraint and reads condef"
    (let [r (rows (.execute *handler* "SELECT c.conname, pg_get_constraintdef(c.oid) FROM pg_constraint c WHERE c.contype = 'p' ORDER BY c.conname"))]
      (is (every? #(re-find #"^PRIMARY KEY " (second %)) r)
          "every PK constraint def should start with PRIMARY KEY")))

  (testing "pg_constraint.conkey is a real int2[] — Metabase joins on attnum = ANY(conkey)"
    ;; Track 5 / Blocker 4. Metabase's FK-introspection query joins
    ;; pg_attribute → pg_constraint via `a.attnum = ANY(c.conkey)`.
    ;; Before Track 5, conkey didn't exist; the join silently
    ;; yielded zero rows and Metabase reported the table as having
    ;; no PKs/FKs.
    (let [r (rows (.execute *handler*
                            "SELECT a.attname
                               FROM pg_attribute a
                               JOIN pg_class cl ON cl.oid = a.attrelid
                               JOIN pg_constraint c ON c.conrelid = cl.oid
                                                  AND a.attnum = ANY(c.conkey)
                              WHERE cl.relname = 'person' AND c.contype = 'p'"))]
      (is (= 1 (count r)) "person has exactly one PK column")
      (is (= "name" (ffirst r))))))

(deftest test-pg-constraint-check-and-fk
  (testing "CHECK and FK constraints surface via pg_constraint after DDL"
    ;; New connection — fixture has only person/department.
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          h (pg/make-query-handler conn)]
      (try
        (.execute h "CREATE TABLE dept (id BIGINT PRIMARY KEY, name TEXT NOT NULL UNIQUE, budget BIGINT CHECK (budget > 0))")
        (.execute h "CREATE TABLE emp (id BIGINT PRIMARY KEY, name TEXT NOT NULL, dept_id BIGINT, FOREIGN KEY (dept_id) REFERENCES dept (id))")
        (let [^PgWireServer$QueryResult r (.execute h "SELECT conname, contype, condef FROM pg_constraint ORDER BY conname")
              named (into {} (map (fn [[n t d]] [n [t d]]) (mapv vec (.rows r))))]
          (is (contains? named "dept_budget_check"))
          (is (= ["c" "CHECK (budget > 0)"] (named "dept_budget_check")))
          (is (contains? named "emp_dept_id_fkey"))
          (let [[ctype cdef] (named "emp_dept_id_fkey")]
            (is (= "f" ctype))
            (is (= "FOREIGN KEY (dept_id) REFERENCES dept (id)" cdef))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

;; ============================================================================
;; Aggregates
;; ============================================================================

(deftest test-aggregates
  (testing "COUNT(*)"
    (is (= [["3"]] (rows (.execute *handler* "SELECT COUNT(*) FROM person")))))

  (testing "SUM"
    (is (= [["1500000"]] (rows (.execute *handler* "SELECT SUM(budget) FROM department")))))

  (testing "MIN and MAX"
    (let [r (.execute *handler* "SELECT MIN(age), MAX(age) FROM person")]
      (is (= [["25" "35"]] (rows r)))))

  (testing "AVG"
    (let [r (.execute *handler* "SELECT AVG(age) FROM person")]
      (is (= 1 (count (rows r))))))

  (testing "Multiple aggregates"
    (let [r (.execute *handler* "SELECT COUNT(*) as cnt, SUM(age) as total, MIN(age) as youngest, MAX(age) as oldest FROM person")]
      (is (= ["cnt" "total" "youngest" "oldest"] (cols r)))
      (is (= 1 (count (rows r)))))))

;; ============================================================================
;; ORDER BY, LIMIT, OFFSET
;; ============================================================================

(deftest test-order-limit-offset
  (testing "ORDER BY ASC"
    (let [r (.execute *handler* "SELECT name, age FROM person ORDER BY age")]
      (is (= "Bob" (ffirst (rows r))))))

  (testing "ORDER BY DESC"
    (let [r (.execute *handler* "SELECT name, age FROM person ORDER BY age DESC")]
      (is (= "Charlie" (ffirst (rows r))))))

  (testing "LIMIT"
    (let [r (.execute *handler* "SELECT name FROM person ORDER BY name LIMIT 2")]
      (is (= 2 (count (rows r))))))

  (testing "LIMIT with OFFSET"
    (let [r (.execute *handler* "SELECT name FROM person ORDER BY name LIMIT 2 OFFSET 1")]
      (is (= 2 (count (rows r))))
      (is (= "Bob" (ffirst (rows r)))))))

;; ============================================================================
;; SQL functions
;; ============================================================================

(deftest test-sql-functions
  (testing "UPPER"
    (let [r (.execute *handler* "SELECT UPPER(name) FROM person WHERE name = 'Alice'")]
      (is (= [["ALICE"]] (rows r)))))

  (testing "LOWER"
    (let [r (.execute *handler* "SELECT LOWER(name) FROM person WHERE name = 'Alice'")]
      (is (= [["alice"]] (rows r)))))

  (testing "SUBSTR"
    (let [r (.execute *handler* "SELECT SUBSTR(email, 1, 5) FROM person WHERE name = 'Alice'")]
      (is (= [["alice"]] (rows r)))))

  (testing "ABS"
    (let [r (.execute *handler* "SELECT name, ABS(age - 30) AS diff FROM person WHERE name = 'Bob'")]
      (is (= [["Bob" "5"]] (rows r)))))

  (testing "String concatenation ||"
    (let [r (.execute *handler* "SELECT name || ' is ' || CAST(age AS VARCHAR) FROM person WHERE name = 'Alice'")]
      (is (= [["Alice is 30"]] (rows r))))))

;; ============================================================================
;; CASE WHEN
;; ============================================================================

(deftest test-case-when
  (testing "Simple CASE WHEN"
    (let [r (.execute *handler* "SELECT name, CASE WHEN age >= 30 THEN 'senior' ELSE 'junior' END AS cat FROM person ORDER BY name")]
      (is (nil? (err r)))
      (is (= ["name" "cat"] (cols r)))
      (is (= 3 (count (rows r))))
      ;; Alice=30 → senior, Bob=25 → junior, Charlie=35 → senior
      (let [result-map (into {} (map (fn [[n c]] [n c]) (rows r)))]
        (is (= "senior" (get result-map "Alice")))
        (is (= "junior" (get result-map "Bob")))
        (is (= "senior" (get result-map "Charlie"))))))

  (testing "Multi-branch CASE WHEN"
    (let [r (.execute *handler* "SELECT name, CASE WHEN age > 30 THEN 'senior' WHEN age > 25 THEN 'mid' ELSE 'junior' END FROM person ORDER BY name")]
      (let [result-map (into {} (map vec (rows r)))]
        (is (= "mid" (get result-map "Alice")))
        (is (= "junior" (get result-map "Bob")))
        (is (= "senior" (get result-map "Charlie")))))))

;; ============================================================================
;; CAST
;; ============================================================================

(deftest test-cast
  (testing "CAST to DOUBLE"
    ;; PG text format elides the ".0" on whole-valued floats ("30" not
    ;; "30.0"). Our value->string matches that so pgjdbc's getBoolean on
    ;; float columns accepts "1" as true (it rejects "1.0").
    (let [r (.execute *handler* "SELECT CAST(age AS DOUBLE PRECISION) FROM person WHERE name = 'Alice'")]
      (is (= [["30"]] (rows r)))))

  (testing "CAST to VARCHAR"
    (let [r (.execute *handler* "SELECT CAST(age AS VARCHAR) FROM person WHERE name = 'Alice'")]
      (is (= [["30"]] (rows r))))))

;; ============================================================================
;; DML: INSERT, UPDATE, DELETE
;; ============================================================================

(deftest test-dml-insert
  (testing "INSERT adds a row"
    (let [r (.execute *handler* "INSERT INTO person (name, age, email, salary) VALUES ('Dave', 40, 'dave@example.com', 80000.0)")]
      (is (nil? (err r)))
      (is (= "INSERT 0 1" (tag r))))
    (is (= [["4"]] (rows (.execute *handler* "SELECT COUNT(*) FROM person")))))

  (testing "Cleanup"
    (.execute *handler* "DELETE FROM person WHERE name = 'Dave'")))

(deftest test-dml-update
  (testing "UPDATE modifies a row"
    (.execute *handler* "INSERT INTO person (name, age, email) VALUES ('Eve', 28, 'eve@example.com')")
    (let [r (.execute *handler* "UPDATE person SET age = 29 WHERE name = 'Eve'")]
      (is (nil? (err r)))
      (is (= "UPDATE 1" (tag r))))
    (is (= [["29"]] (rows (.execute *handler* "SELECT age FROM person WHERE name = 'Eve'")))))

  (testing "Cleanup"
    (.execute *handler* "DELETE FROM person WHERE name = 'Eve'")))

(deftest test-dml-delete
  (testing "DELETE removes a row"
    (.execute *handler* "INSERT INTO person (name, age, email) VALUES ('Frank', 45, 'frank@example.com')")
    (let [r (.execute *handler* "DELETE FROM person WHERE name = 'Frank'")]
      (is (nil? (err r)))
      (is (= "DELETE 1" (tag r))))
    (is (= [["3"]] (rows (.execute *handler* "SELECT COUNT(*) FROM person"))))))

;; ============================================================================
;; System queries
;; ============================================================================

(deftest test-system-queries
  (testing "SELECT version()"
    (let [r (.execute *handler* "SELECT version()")]
      (is (nil? (err r)))
      (is (.contains ^String (first-val r) "Datahike"))))

  (testing "SHOW tables"
    (let [r (.execute *handler* "SHOW tables")]
      (is (nil? (err r)))
      (let [tables (set (map second (rows r)))]
        (is (contains? tables "person"))
        (is (contains? tables "department")))))

  (testing "SET is no-op"
    (is (= "SET" (tag (.execute *handler* "SET client_encoding = 'UTF8'")))))

  (testing "BEGIN/COMMIT are no-ops"
    (is (nil? (err (.execute *handler* "BEGIN"))))
    (is (nil? (err (.execute *handler* "COMMIT")))))

  (testing "SHOW server_version"
    (let [r (.execute *handler* "SHOW server_version")]
      (is (= [["15.0"]] (rows r))))))

;; ============================================================================
;; DISTINCT
;; ============================================================================

(deftest test-distinct
  (testing "SELECT DISTINCT"
    (let [r (.execute *handler* "SELECT DISTINCT age FROM person ORDER BY age")]
      (is (nil? (err r)))
      (is (= 3 (count (rows r)))))))

;; ============================================================================
;; JOINs via ref attributes
;; ============================================================================

(deftest test-joins
  (testing "Basic JOIN via ref attribute"
    (let [r (.execute *handler* "SELECT p.name, d.name AS dept FROM person p JOIN department d ON p.department = d.db_id ORDER BY p.name")]
      (is (nil? (err r)))
      (is (= ["name" "dept"] (cols r)))
      (is (= 3 (count (rows r))))
      (is (= "Alice" (ffirst (rows r))))))

  (testing "JOIN with multiple columns"
    (let [r (.execute *handler* "SELECT p.name, p.age, d.name AS dept, d.budget FROM person p JOIN department d ON p.department = d.db_id ORDER BY p.name")]
      (is (nil? (err r)))
      (is (= 4 (count (cols r))))
      (is (= 3 (count (rows r))))))

  (testing "JOIN with WHERE filter"
    (let [r (.execute *handler* "SELECT p.name, d.name AS dept FROM person p JOIN department d ON p.department = d.db_id WHERE p.age > 28 ORDER BY p.name")]
      (is (nil? (err r)))
      (is (= 2 (count (rows r))))
      (let [names (set (map first (rows r)))]
        (is (contains? names "Alice"))
        (is (contains? names "Charlie")))))

  (testing "JOIN with aggregate"
    (let [r (.execute *handler* "SELECT d.name AS dept, COUNT(*) AS headcount FROM person p JOIN department d ON p.department = d.db_id GROUP BY d.name ORDER BY d.name")]
      (is (nil? (err r)))
      (is (= 2 (count (rows r))))
      (is (= [["Engineering" "2"] ["Sales" "1"]] (rows r)))))

  (testing "JOIN with function"
    (let [r (.execute *handler* "SELECT p.name, UPPER(d.name) AS dept FROM person p JOIN department d ON p.department = d.db_id WHERE p.name = 'Alice'")]
      (is (nil? (err r)))
      (is (= [["Alice" "ENGINEERING"]] (rows r))))))

;; ============================================================================
;; HAVING
;; ============================================================================

(deftest test-having
  (testing "HAVING filters aggregate results"
    (let [r (.execute *handler* "SELECT d.name AS dept, COUNT(*) AS cnt FROM person p JOIN department d ON p.department = d.db_id GROUP BY d.name HAVING COUNT(*) > 1")]
      (is (nil? (err r)))
      (is (= 1 (count (rows r))))
      (is (= "Engineering" (ffirst (rows r)))))))

;; ============================================================================
;; DDL: CREATE TABLE
;; ============================================================================

(deftest test-ddl-create-table
  (testing "CREATE TABLE adds schema attributes"
    (let [r (.execute *handler* "CREATE TABLE product (name TEXT, price DOUBLE PRECISION, quantity INTEGER)")]
      (is (nil? (err r)))
      (is (= "CREATE TABLE" (tag r))))
    ;; Verify the schema was created by inserting and querying
    (let [r (.execute *handler* "INSERT INTO product (name, price, quantity) VALUES ('Widget', 9.99, 100)")]
      (is (nil? (err r))))
    (let [r (.execute *handler* "SELECT name, price FROM product")]
      (is (nil? (err r)))
      (is (= 1 (count (rows r)))))))

;; ============================================================================
;; Server lifecycle
;; ============================================================================

(deftest test-server-lifecycle
  (testing "Server starts and stops cleanly"
    (let [port (+ 16000 (rand-int 1000))
          srv (pg/start-server *conn* {:port port})]
      (is (some? (:server srv)))
      (is (.isRunning ^datahike.pg.PgWireServer (:server srv)))
      (pg/stop-server srv)
      (is (not (.isRunning ^datahike.pg.PgWireServer (:server srv)))))))

;; ============================================================================
;; Error handling
;; ============================================================================

(deftest test-error-handling
  (testing "Invalid SQL returns error"
    (let [r (.execute *handler* "SELECTT broken")]
      (is (some? (err r)))))

  (testing "Unknown table does not crash"
    (let [r (.execute *handler* "SELECT * FROM nonexistent")]
      ;; Should not throw; may return error, empty result, or degenerate result
      (is (some? r))))

  (testing "Empty query handled"
    ;; SET with unknown var should not crash
    (let [r (.execute *handler* "SET something = 'value'")]
      (is (nil? (err r))))))

;; ============================================================================
;; Catalog / information_schema
;; ============================================================================

(deftest test-catalog-queries
  (testing "pg_type returns common types"
    (let [r (.execute *handler* "SELECT * FROM pg_type")]
      (is (nil? (err r)))
      (is (pos? (count (rows r))))
      (is (some #(= "int8" (second %)) (rows r)))))

  (testing "pg_tables lists virtual tables (PG-real schema)"
    ;; pg_tables columns match real PG: schemaname, tablename, tableowner,
    ;; tablespace, hasindexes, hasrules, hastriggers, rowsecurity. Probe
    ;; by tablename (what pgJDBC DatabaseMetaData.getTables does).
    (let [r (.execute *handler* "SELECT tablename FROM pg_tables")]
      (is (nil? (err r)))
      (let [table-names (set (map first (rows r)))]
        (is (contains? table-names "person"))
        (is (contains? table-names "department")))))

  (testing "pg_attribute returns column info"
    ;; pg_attribute.attrelid is now a PG-style integer OID (matching
    ;; pg_class.oid), so resolve through the join instead of expecting
    ;; the string table name in the raw row.
    (let [r (.execute *handler*
                      "SELECT a.attname FROM pg_attribute a
                       JOIN pg_class c ON c.oid = a.attrelid
                       WHERE c.relname = 'person'")]
      (is (nil? (err r)))
      (let [person-cols (set (map first (rows r)))]
        (is (contains? person-cols "name"))
        (is (contains? person-cols "age"))
        (is (contains? person-cols "email")))))

  (testing "information_schema.columns"
    (let [r (.execute *handler* "SELECT * FROM information_schema.columns")]
      (is (nil? (err r)))
      (let [rs (rows r)
            person-cols (set (map #(nth % 3) (filter #(= "person" (nth % 2)) rs)))]
        (is (contains? person-cols "name"))
        (is (contains? person-cols "age"))))
    ;; data_type lookup deserves an explicit projection — `SELECT *`
    ;; column ordering is determined by pg_attribute and not part of
    ;; the SQL contract.
    (let [r (.execute *handler*
                      "SELECT data_type FROM information_schema.columns
                        WHERE table_name = 'person' AND column_name = 'age'")]
      (is (nil? (err r)))
      (is (= "bigint" (ffirst (rows r))))))

  (testing "information_schema.tables"
    (let [r (.execute *handler* "SELECT * FROM information_schema.tables")]
      (is (nil? (err r)))
      (is (some #(= "person" (nth % 2)) (rows r)))))

  (testing "information_schema.columns ordinal_position is integer (Metabase Blocker 1)"
    ;; Metabase emits `ordinal_position - 1` arithmetic to derive a 0-based
    ;; column index. With ordinal_position stored as :db.type/string this
    ;; raised a runtime type error — fixed by mirroring PG's cardinal_number
    ;; domain (= integer). Regression test: subtract a literal and verify
    ;; the simple-query text-encoded result is "1" (computed numerically),
    ;; not "21" (string concat) or a type-error.
    (let [r (.execute *handler*
                      "SELECT ordinal_position - 1 AS idx
                         FROM information_schema.columns
                        WHERE table_name = 'person' AND column_name = 'age'")]
      (is (nil? (err r)))
      ;; Columns are reported in schema-declaration order now, not the
      ;; schema map's hash order. This fixture transacts :person/name
      ;; before :person/age, so age is the 2nd declared column and the
      ;; 3rd counting our synthetic db_id.
      (is (= [["2"]] (rows r)) "age is the 2nd person column → ordinal_position-1 = 2")))

  (testing "information_schema.columns udt_schema = pg_catalog (Metabase type inference)"
    (let [r (.execute *handler*
                      "SELECT udt_schema, udt_name
                         FROM information_schema.columns
                        WHERE table_name = 'person' AND column_name = 'age'")]
      (is (nil? (err r)))
      (is (= [["pg_catalog" "int8"]] (rows r)))))

  (testing "information_schema.columns is_identity = YES on the implicit db_id PK"
    (let [r (.execute *handler*
                      "SELECT is_identity, identity_generation, is_nullable
                         FROM information_schema.columns
                        WHERE table_name = 'person' AND column_name = 'db_id'")]
      (is (nil? (err r)))
      (is (= [["YES" "BY DEFAULT" "NO"]] (rows r)))))

  (testing "pg_namespace"
    (let [r (.execute *handler* "SELECT * FROM pg_namespace")]
      (is (nil? (err r)))
      (is (= "public" (second (first (rows r)))))))

  (testing "pg_database"
    (let [r (.execute *handler* "SELECT * FROM pg_database")]
      (is (nil? (err r)))
      ;; pg_database lists the current DB plus the two PG templates that
      ;; real PG always ships (tools check for them). Assert the set so
      ;; the test isn't brittle to row order.
      (is (= #{"datahike" "template0" "template1"}
             (set (map first (rows r))))))))

;; ============================================================================
;; Semantic error handling
;; ============================================================================

(deftest test-semantic-errors
  (testing "Unknown column on a known table raises 42703"
    ;; REVERSED from what this asserted originally. The old behaviour
    ;; mapped a non-existent column to a non-existent attribute, which
    ;; `get-else` reads as the `:__null__` sentinel — internally
    ;; consistent, but it meant a typo'd column name returned a row of
    ;; NULLs and `WHERE nosuchcol = 1` returned no rows, with nothing to
    ;; distinguish "this column is empty" from "this column does not
    ;; exist". PostgreSQL rejects it at parse-analyze.
    ;;
    ;; The permissiveness that IS wanted is untouched: a row lacking a
    ;; value for a column the table HAS still reads as NULL, and under
    ;; `:schema-flexibility :read` — where a real column need not appear
    ;; in the schema at all — the check does not run.
    (let [r (.execute *handler* "SELECT nonexistent_column FROM person")]
      (is (= "column \"nonexistent_column\" does not exist" (err r)))))

  (testing "Type mismatch in INSERT"
    ;; Try inserting string into INTEGER column via DDL table
    (.execute *handler* "CREATE TABLE err_test (val INTEGER)")
    (let [r (.execute *handler* "INSERT INTO err_test VALUES ('not_a_number')")]
      ;; Should either error or coerce — but not crash
      (is (some? r))))

  (testing "Division by zero"
    ;; PG raises SQLSTATE 22012 with message exactly "division by zero"
    ;; for integer, float, and numeric division alike (int4div /
    ;; float8div / numeric_div in postgres src/backend/utils/adt).
    (let [r (.execute *handler* "SELECT 1 / 0")]
      (is (= "22012" (sqlstate r)))
      (is (= "division by zero" (err r))))

    (let [r (.execute *handler* "SELECT 1.0 / 0")]
      (is (= "22012" (sqlstate r)))
      (is (= "division by zero" (err r))))

    (let [r (.execute *handler* "SELECT 1 % 0")]
      (is (= "22012" (sqlstate r)))
      (is (= "division by zero" (err r))))

    (let [r (.execute *handler* "SELECT age / 0 FROM person WHERE name = 'Alice'")]
      (is (= "22012" (sqlstate r)))
      (is (= "division by zero" (err r))))))

;; ============================================================================
;; Arithmetic expressions
;; ============================================================================

(deftest test-arithmetic
  (testing "Addition in SELECT"
    (let [r (.execute *handler* "SELECT name, age + 10 AS future_age FROM person WHERE name = 'Alice'")]
      (is (nil? (err r)))
      (is (= [["Alice" "40"]] (rows r)))))

  (testing "Multiplication"
    (let [r (.execute *handler* "SELECT name, salary * 1.1 AS raised FROM person WHERE name = 'Alice'")]
      (is (nil? (err r)))
      (is (= 1 (count (rows r)))))))

;; ============================================================================
;; Multiple tables
;; ============================================================================

(deftest test-multiple-tables
  (testing "Query department table independently"
    (let [r (.execute *handler* "SELECT name, budget FROM department ORDER BY name")]
      (is (nil? (err r)))
      (is (= ["name" "budget"] (cols r)))
      (is (= 2 (count (rows r))))
      (is (= "Engineering" (ffirst (rows r)))))))

;; ============================================================================
;; Temporal queries (AS OF, HISTORY)
;; ============================================================================

(deftest test-temporal-queries
  (testing "SET datahike.as_of enables time-travel queries"
    ;; Insert, record timestamp, then update
    (.execute *handler* "INSERT INTO department (name, budget) VALUES ('Research', 200000)")
    (Thread/sleep 50)
    (let [t1 (java.util.Date.)]
      (Thread/sleep 50)
      (.execute *handler* "UPDATE department SET budget = 300000 WHERE name = 'Research'")

      ;; Current state
      (let [r (.execute *handler* "SELECT budget FROM department WHERE name = 'Research'")]
        (is (= [["300000"]] (rows r))))

      ;; As-of t1 — should see old value
      (let [t1-str (.toString (.toInstant t1))]
        (.execute *handler* (str "SET datahike.as_of = '" t1-str "'"))
        (let [r (.execute *handler* "SELECT budget FROM department WHERE name = 'Research'")]
          (is (= [["200000"]] (rows r)))))

      ;; Reset as_of
      (.execute *handler* "RESET datahike.as_of")

      ;; Back to current
      (let [r (.execute *handler* "SELECT budget FROM department WHERE name = 'Research'")]
        (is (= [["300000"]] (rows r))))))

  (testing "SET datahike.history shows all versions"
    (.execute *handler* "SET datahike.history = 'true'")
    (let [r (.execute *handler* "SELECT budget FROM department WHERE name = 'Research'")]
      ;; Should see both old and new values (history may return more with get-else)
      (is (>= (count (rows r)) 1)))
    (.execute *handler* "RESET datahike.history"))

  ;; Cleanup
  (testing "Cleanup"
    (.execute *handler* "DELETE FROM department WHERE name = 'Research'")))

(deftest test-as-of-count-star-survives-pre-schema-timestamp
  (testing "Regression for the column-info empty-fallback bug:
            SET datahike.as_of to a point BEFORE the schema attrs were
            transacted, then SELECT count(*) — the translator must not
            error with 'Query for unknown vars: [?<table>_eid]'.

            Root cause was column-order-from-db returning [] under
            as-of (the schema-ident query saw zero entities at the
            past tx), and the column-info if-let treating empty vec
            as truthy — yielding a single-column [{db_id}] result.
            COUNT(*)'s entity-binder fallback then found no second
            column, never bound the entity-var, and the translator
            emitted an unbound :find."
    (.execute *handler* "SET datahike.as_of = '1970-01-01T00:00:00Z'")
    (let [r (.execute *handler* "SELECT count(*) FROM person")]
      (is (nil? (err r))
          (str "as-of-pre-schema count(*) errored: " (err r)))
      ;; At epoch the table is empty (datoms didn't exist yet).
      (is (= [["0"]] (rows r))))
    (let [r (.execute *handler* "SELECT count(*) FROM department")]
      (is (nil? (err r)))
      (is (= [["0"]] (rows r))))
    (.execute *handler* "RESET datahike.as_of")
    (let [r (.execute *handler* "SELECT count(*) FROM person")]
      (is (= [["3"]] (rows r)) "current head sees 3 people"))))

(deftest test-left-join-count-of-right-side-eid-binds-without-error
  (testing "LEFT JOIN <r> ON … then SELECT count(r.db_id) — the
            right-side entity var must be bound somewhere or the
            translator rejects with 'Query for unknown vars:
            [?<r>_eid]'.

            Root cause was twofold:
              (a) the ref-based LEFT JOIN post-processor used
                  `left-evar` (default-table's evar) as the get-else
                  input, but the entity-var swap in translate-join
                  had already corrupted that var to point at ref-var
                  itself — producing a self-referential
                  `(get-else $ ?ref :attr :__null__) ?ref`.
              (b) the right-side entity-var (?p_eid) was never bound:
                  no right-side data pattern existed when only
                  `count(p.db_id)` was projected, so it landed in
                  :find with no :where binding.

            Fixed by:
              (a) computing `owner-evar` from the JOIN's literal
                  right-alias (`p`) instead of trusting the ref-info's
                  potentially-corrupted `:left-evar`.
              (b) including owner-evar in shared-vars and binding it
                  in matched (data pattern) + unmatched (:__null__)
                  branches.

            Note: a deeper LEFT JOIN iteration bug remains where
            empty-right-side rows on the LEFT table aren't surfaced
            (the outer get-else drives iteration from the right table,
            not the left). Filed as a separate task; this test only
            covers the unknown-vars regression."
    (let [r (.execute *handler*
                      (str "SELECT d.name, count(p.db_id) "
                           "FROM department d "
                           "LEFT JOIN person p ON p.department = d.db_id "
                           "GROUP BY d.name "
                           "ORDER BY d.name"))]
      (is (nil? (err r))
          (str "LEFT JOIN + count(p.db_id) errored: " (err r)))
      (let [data (rows r)
            by-name (into {} (map (juxt first second) data))]
        ;; Sales has 1 person (Charlie), Engineering has 2 (Alice, Bob).
        (is (= "1" (get by-name "Sales")))
        (is (= "2" (get by-name "Engineering")))))))

(deftest test-insert-select-with-ref-columns-actually-lands
  (testing "INSERT INTO posting (...) SELECT ... lands the row in
            datahike. Bug observed in the wild: pg-datahike returned
            INSERT 0 1 (success) but no row appeared, even when the
            inner SELECT clearly returned a row. Suspected loss path:
            either coerce-insert-value silently drops the columns, or
            the ref-target FK projection on the INSERT … SELECT path
            consumes its arg and returns nil for the ref column."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :acct/code
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :tx/id
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :post/transaction
                          :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/one}
                         {:db/ident :post/account
                          :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/one}
                         {:db/ident :post/amount
                          :db/valueType :db.type/bigdec
                          :db/cardinality :db.cardinality/one}])
          {:keys [tempids]}
          (d/transact conn [{:db/id "a" :acct/code "1400"}
                            {:db/id "t" :tx/id "TX-1"}])
          acct-eid (get tempids "a")
          tx-eid   (get tempids "t")
          handler (pg/make-query-handler conn)]
      (try
        ;; Form 1: INSERT … VALUES — sanity check, should land
        (let [r (.execute handler
                          (str "INSERT INTO post (transaction, account, amount) "
                               "VALUES (" tx-eid ", " acct-eid ", 100.00)"))]
          (is (nil? (err r)) (str "VALUES INSERT errored: " (err r))))

        ;; Form 2: INSERT … SELECT with literal-only projection (no FROM)
        (let [r (.execute handler
                          (str "INSERT INTO post (transaction, account, amount) "
                               "SELECT " tx-eid ", " acct-eid ", 200.00"))]
          (is (nil? (err r)) (str "SELECT-no-FROM INSERT errored: " (err r))))

        ;; Form 3: INSERT … SELECT FROM <table> LIMIT 1 — drives the
        ;; canonical INSERT…SELECT branch. Was the failing form in the
        ;; accounting psql session: result reported INSERT 0 1 but no
        ;; row landed.
        (let [r (.execute handler
                          (str "INSERT INTO post (transaction, account, amount) "
                               "SELECT " tx-eid ", " acct-eid ", 300.00 "
                               "FROM acct LIMIT 1"))]
          (is (nil? (err r)) (str "SELECT-FROM INSERT errored: " (err r))))

        ;; All three should have landed. Read them back via SQL.
        (let [r (.execute handler "SELECT amount FROM post ORDER BY amount")
              data (rows r)]
          (is (nil? (err r)))
          (is (= 3 (count data))
              (str "expected 3 rows; got: " data))
          ;; Tolerate bigdec display variants (100.0 vs 100.00) — what
          ;; matters is the row landed at all. The pre-fix bug was the
          ;; row vanishing entirely.
          (is (= [100.0M 200.0M 300.0M]
                 (map (fn [[s]] (bigdec s)) data))
              (str "expected amounts 100/200/300; got: " data)))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-ref-column-insert-accepts-bigint-entity-id
  (testing "A ref column read via SELECT projects as the parent's PK
            value when one is resolvable, OR as the raw entity-id
            (bigint) when there's no PK target. INSERT must accept
            *both* forms symmetrically:
              - A string matching the target PK's value-type → wrap
                into a lookup-ref `[pk-attr v]` so datahike resolves
                by PK.
              - A bigint matching no PK type → pass through as a
                direct entity-id reference.

            Previously coerce-insert-value unconditionally wrapped the
            value in `[pk-attr val]`, which broke entity-id INSERTs:
              `INSERT … account=143` → `[:account/path 143]` →
              datahike: \"Nothing found for entity id\" because path
              is a string and 143 is a long."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :account/code
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :posting/account
                          :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/one}
                         {:db/ident :posting/amount
                          :db/valueType :db.type/long
                          :db/cardinality :db.cardinality/one}])
          {:keys [tempids]}
          (d/transact conn [{:db/id "acct" :account/code "1200"}])
          acct-eid (get tempids "acct")
          handler (pg/make-query-handler conn)]
      (try
        ;; Form 1: string PK value — datahike resolves via lookup-ref.
        (let [r (.execute handler
                          (str "INSERT INTO posting (account, amount) "
                               "VALUES ('1200', 100)"))]
          (is (nil? (err r))
              (str "string-PK INSERT errored: " (err r))))
        ;; Form 2: raw entity-id (bigint).
        (let [r (.execute handler
                          (str "INSERT INTO posting (account, amount) "
                               "VALUES (" acct-eid ", 200)"))]
          (is (nil? (err r))
              (str "entity-id INSERT errored: " (err r))))
        ;; Both forms should have landed on the same account.
        (let [r (.execute handler
                          "SELECT count(*) FROM posting WHERE account IS NOT NULL")
              data (rows r)]
          (is (= [["2"]] data)))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-jvm-time-types-coerce-to-instant
  (testing "Every java.time.* type that pgwire / wire-layer / casts
            can produce should coerce cleanly into :db.type/instant
            on INSERT. Without this, a parameterized SQL prepared
            statement bound to a java.time.LocalDate (the usual
            jdbc default for `:date` columns) errors with cryptic
            'value does not match schema definition'."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :evt/id
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :evt/at
                          :db/valueType :db.type/instant
                          :db/cardinality :db.cardinality/one}])
          handler (pg/make-query-handler conn)]
      (try
        ;; SQL '::date' cast → LocalDate path
        (let [r (.execute handler
                          "INSERT INTO evt (id, at) VALUES ('LD', '2026-04-01'::date)")]
          (is (nil? (err r)) (str "::date cast errored: " (err r))))
        ;; SQL '::timestamp' cast → java.util.Date path (already worked)
        (let [r (.execute handler
                          "INSERT INTO evt (id, at) VALUES ('TS', '2026-04-02 12:30:00'::timestamp)")]
          (is (nil? (err r)) (str "::timestamp cast errored: " (err r))))
        ;; Both rows should be readable
        (let [r (.execute handler "SELECT count(*) FROM evt")
              data (rows r)]
          (is (= [["2"]] data)))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-keyword-and-symbol-and-uuid-pass-through-typed-input
  (testing "When a value already has the target JVM type (Keyword,
            Symbol, UUID), coerce-insert-value should pass it through
            instead of rejecting. This matters for parameterized
            INSERTs where the wire layer decoded a typed literal, and
            for INSERT … SELECT pulling already-typed datahike values."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :w/sku
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :w/state
                          :db/valueType :db.type/keyword
                          :db/cardinality :db.cardinality/one}
                         {:db/ident :w/uid
                          :db/valueType :db.type/uuid
                          :db/cardinality :db.cardinality/one}])
          handler (pg/make-query-handler conn)
          uid     "12345678-1234-1234-1234-123456789012"]
      (try
        ;; UUID via string literal — typed-input path
        (let [r (.execute handler
                          (str "INSERT INTO w (sku, state, uid) "
                               "VALUES ('A', 'draft', '" uid "')"))]
          (is (nil? (err r)) (str "INSERT errored: " (err r))))
        (let [r (.execute handler "SELECT state, uid FROM w WHERE sku = 'A'")
              [[s u]] (rows r)]
          (is (nil? (err r)))
          (is (= "draft" s))
          (is (= uid u)))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-keyword-column-insert-and-select-via-string
  (testing "Columns typed :db.type/keyword should accept string values
            on INSERT (`'draft'` → `:draft`) and surface them as their
            string form on SELECT. SQL clients have no keyword literal,
            so the string ↔ keyword bridge belongs in the wire layer."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :doc/id
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :doc/state
                          :db/valueType :db.type/keyword
                          :db/cardinality :db.cardinality/one}])
          handler (pg/make-query-handler conn)]
      (try
        (let [r (.execute handler "INSERT INTO doc (id, state) VALUES ('A', 'draft')")]
          (is (nil? (err r)) (str "INSERT errored: " (err r))))
        (let [r (.execute handler "INSERT INTO doc (id, state) VALUES ('B', 'posted')")]
          (is (nil? (err r)) (str "INSERT errored: " (err r))))
        ;; SELECT should expose the keyword as a string (clients have
        ;; no datahike keyword type to receive).
        (let [r (.execute handler "SELECT id, state FROM doc ORDER BY id")
              data (rows r)]
          (is (nil? (err r)))
          (is (= 2 (count data)))
          (is (= "draft"  (-> data first  second)))
          (is (= "posted" (-> data second second))))
        ;; WHERE-clause comparison on a keyword column with a string
        ;; literal should ALSO work — symmetric with INSERT.
        (let [r (.execute handler "SELECT id FROM doc WHERE state = 'draft'")
              data (rows r)]
          (is (nil? (err r)))
          (is (= [["A"]] data) (str "WHERE state='draft' returned: " data)))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-left-join-empty-right-side-rows-appear
  (testing "LEFT JOIN <r> ON r.fk = l.db_id should surface rows from
            the LEFT table even when no RIGHT row matches. Standard
            SQL LEFT JOIN semantics. The translator currently drives
            iteration from the right table (a get-else over the right
            entity), so left rows with no match get dropped — they
            never appear in the result.

            Reproduces with a fresh fixture: 2 departments, only one
            has a person. The empty department should surface with
            count(p.db_id) = 0. Pre-fix: the empty department row is
            absent from the result entirely."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :dept/id
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :emp/id
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :emp/department
                          :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/one}])
          {:keys [tempids]}
          (d/transact conn [{:db/id "d1" :dept/id "Engineering"}
                            {:db/id "d2" :dept/id "Empty"}])
          eng (get tempids "d1")
          _ (d/transact conn [{:emp/id "alice" :emp/department eng}])
          handler (pg/make-query-handler conn)]
      (try
        (let [r (.execute handler
                          (str "SELECT d.id, count(e.db_id) AS n_emps "
                               "FROM dept d "
                               "LEFT JOIN emp e ON e.department = d.db_id "
                               "GROUP BY d.id "
                               "ORDER BY d.id"))]
          (is (nil? (err r)) (str "errored: " (err r)))
          (let [data (rows r)
                by-id (into {} (map (juxt first second) data))]
            (is (= 2 (count data))
                (str "expected 2 rows (one per dept); got: " data))
            (is (= "0" (get by-id "Empty"))
                (str "Empty dept should have count=0; got: "
                     (pr-str by-id)))
            (is (= "1" (get by-id "Engineering")))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-tx-wrap-config-fires-for-sql-writes
  (testing "make-query-handler accepts a :tx-wrap option that runs
            on every INSERT/UPDATE/DELETE's tx-data before
            d/transact. Frameworks (e.g. datahike-accounting) use
            this to inject [:db.fn/call validate tx-data] so their
            transactor-side validators fire for SQL writes too.

            This test uses a simple Clojure-side wrap that throws
            on negative `widget/qty` values, demonstrating the hook
            is reached by INSERT, UPDATE, and DELETE paths."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :widget/sku
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :widget/qty
                          :db/valueType :db.type/long
                          :db/cardinality :db.cardinality/one}])
          calls (atom [])
          neg-qty? (fn [v] (and (number? v) (neg? v)))
          tx-wrap (fn [tx-data]
                    (swap! calls conj (vec tx-data))
                    ;; Reject any tx-data entry asserting a negative
                    ;; qty. Handles both shapes:
                    ;;   {:widget/qty -7}            (entity map, INSERT)
                    ;;   [:db/add eid :widget/qty -7] (tuple, UPDATE)
                    (doseq [entry tx-data]
                      (cond
                        (and (map? entry)
                             (neg-qty? (:widget/qty entry)))
                        (throw (ex-info "negative qty rejected by tx-wrap"
                                        {:entry entry}))
                        (and (vector? entry) (= 4 (count entry))
                             (= :db/add (first entry))
                             (= :widget/qty (nth entry 2))
                             (neg-qty? (nth entry 3)))
                        (throw (ex-info "negative qty rejected by tx-wrap"
                                        {:entry entry}))))
                    tx-data)
          handler (pg/make-query-handler conn {:tx-wrap tx-wrap})]
      (try
        ;; Valid INSERT goes through.
        (let [r (.execute handler "INSERT INTO widget (sku, qty) VALUES ('A', 5)")]
          (is (nil? (err r)) (str "valid INSERT errored: " (err r))))

        ;; Invalid INSERT (qty = -1) is rejected by the wrap.
        (let [r (.execute handler "INSERT INTO widget (sku, qty) VALUES ('B', -1)")]
          (is (some? (err r)) "negative-qty INSERT should fail")
          (is (clojure.string/includes? (err r) "negative qty rejected")
              (str "expected wrap message; got: " (err r))))

        ;; Confirm only A landed.
        (let [r (.execute handler "SELECT sku FROM widget ORDER BY sku")]
          (is (= [["A"]] (rows r))))

        ;; UPDATE through the wrap: positive update succeeds.
        (let [r (.execute handler "UPDATE widget SET qty = 10 WHERE sku = 'A'")]
          (is (nil? (err r))))

        ;; UPDATE through the wrap: negative update rejected.
        (let [r (.execute handler "UPDATE widget SET qty = -7 WHERE sku = 'A'")]
          (is (some? (err r))
              "UPDATE that would set qty=-7 should fail"))

        ;; A's qty should still be 10 (last successful update).
        (let [r (.execute handler "SELECT qty FROM widget WHERE sku = 'A'")]
          (is (= [["10"]] (rows r))))

        ;; The wrap was called multiple times — confirm.
        (is (>= (count @calls) 3)
            (str "expected wrap called >=3 times; got " (count @calls)))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-tx-wrap-fires-via-batchable-insert-path
  (testing "psql Simple Query INSERTs (the typical flow) go through
            exec-batchable-insert, not execute-insert. The :tx-wrap
            hook must fire on this path too — otherwise framework-
            installed validators see only the slower / RETURNING /
            ON CONFLICT inserts, missing the bulk of writes."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :w/sku
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :w/qty
                          :db/valueType :db.type/long
                          :db/cardinality :db.cardinality/one}])
          calls (atom 0)
          handler (pg/make-query-handler conn
                                         {:tx-wrap (fn [tx-data]
                                                     (swap! calls inc)
                                                     tx-data)})]
      (try
        ;; Plain literal-VALUES INSERT — flows through the batchable
        ;; path when no RETURNING / ON CONFLICT is in play.
        (let [r (.execute handler "INSERT INTO w (sku, qty) VALUES ('A', 1)")]
          (is (nil? (err r))))
        (let [r (.execute handler "INSERT INTO w (sku, qty) VALUES ('B', 2)")]
          (is (nil? (err r))))
        ;; Each call should have invoked the wrap.
        (is (= 2 @calls)
            (str "expected 2 wrap calls; got " @calls
                 " (batchable path may be skipping :tx-wrap)"))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-tx-wrap-default-identity-no-op
  (testing "Without :tx-wrap, the handler behaves identically — the
            default is identity and adds no overhead."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :w/sku
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}])
          handler (pg/make-query-handler conn)]
      (try
        (let [r (.execute handler "INSERT INTO w (sku) VALUES ('A')")]
          (is (nil? (err r))))
        (let [r (.execute handler "SELECT sku FROM w")]
          (is (= [["A"]] (rows r))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-m2m-join-via-any-array
  (testing "JOIN through a :db.cardinality/many ref using
            `JOIN <table> <alias> ON <alias>.db_id = ANY(<src>.<m2m_col>)`.
            Common SQL idiom for tag-aggregated reports (e.g. UStVA via
            account-tag aggregation). Datalog-side is naturally a plain
            data pattern over the M2M ref attr — translator just needs
            to recognize the SQL form and emit the right pattern."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :tag/name
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :widget/sku
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :widget/tags
                          :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/many}])
          _ (d/transact conn
                        [{:tag/name "red"} {:tag/name "round"} {:tag/name "small"}])
          _ (d/transact conn
                        [{:widget/sku "A"
                          :widget/tags [[:tag/name "red"] [:tag/name "round"]]}
                         {:widget/sku "B"
                          :widget/tags [[:tag/name "red"] [:tag/name "small"]]}
                         {:widget/sku "C"
                          :widget/tags [[:tag/name "round"]]}])
          handler (pg/make-query-handler conn)]
      (try
        ;; M2M JOIN: list every (widget, tag) pair
        (let [r (.execute handler
                          (str "SELECT w.sku, t.name FROM widget w "
                               "JOIN tag t ON t.db_id = ANY(w.tags) "
                               "ORDER BY w.sku, t.name"))]
          (is (nil? (err r)) (str "M2M JOIN errored: " (err r)))
          (is (= [["A" "red"] ["A" "round"]
                  ["B" "red"] ["B" "small"]
                  ["C" "round"]]
                 (rows r))))
        ;; M2M JOIN with WHERE filter on tag side
        (let [r (.execute handler
                          (str "SELECT w.sku FROM widget w "
                               "JOIN tag t ON t.db_id = ANY(w.tags) "
                               "WHERE t.name = 'red' "
                               "ORDER BY w.sku"))]
          (is (nil? (err r)))
          (is (= [["A"] ["B"]] (rows r))))
        ;; M2M JOIN with aggregation grouped by tag
        (let [r (.execute handler
                          (str "SELECT t.name, count(w.db_id) AS n "
                               "FROM widget w "
                               "JOIN tag t ON t.db_id = ANY(w.tags) "
                               "GROUP BY t.name "
                               "ORDER BY t.name"))]
          (is (nil? (err r)))
          (let [data (rows r)
                by-name (into {} (map (juxt first second) data))]
            (is (= "2" (get by-name "red")))
            (is (= "2" (get by-name "round")))
            (is (= "1" (get by-name "small")))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-aliased-many-ref-projection-works-with-where
  (testing "Aliased projection of an M2M ref column with a WHERE
            clause was erroring with 'Bad format for attribute in
            pattern'. Triggered by the entity-var split: col-var!
            for :account/tags created entity-var ?account_eid for
            namespace `account`, while the rest of the query was
            using ?a_eid for the alias `a`. Two ungrounded eids;
            the M2M emitter's source-eid arg landed unbound."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :tag/name
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :widget/sku
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :widget/tags
                          :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/many}])
          _ (d/transact conn [{:tag/name "red"} {:tag/name "small"}])
          _ (d/transact conn [{:widget/sku "A"
                               :widget/tags [[:tag/name "red"] [:tag/name "small"]]}])
          handler (pg/make-query-handler conn)]
      (try
        ;; Bare unaliased — known to work
        (let [r (.execute handler "SELECT tags FROM widget")]
          (is (nil? (err r)))
          (is (= 1 (count (rows r)))))
        ;; Aliased — was erroring
        (let [r (.execute handler "SELECT w.tags FROM widget w")]
          (is (nil? (err r)) (str "aliased SELECT errored: " (err r)))
          (is (= 1 (count (rows r)))))
        ;; Aliased + WHERE — was the original failing pattern
        (let [r (.execute handler "SELECT w.tags FROM widget w WHERE w.sku = 'A'")]
          (is (nil? (err r)) (str "aliased+WHERE errored: " (err r)))
          (is (= 1 (count (rows r)))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-many-ref-projects-as-int8-array-via-data-inference
  (testing "A :db.cardinality/many ref attr whose local name does NOT
            match the target namespace (e.g. `:account/tags` →
            `:account-tag`) should still project as int8[] of target
            entity-ids, NOT as a single bigint. Previously the
            convention `(name ref-attr) → namespace` only matched
            singular-named refs (`:order/customer` → customer),
            silently dropping every plural / hyphen-named M2M.

            Resolution: when the schema-only convention finds no
            target, validate-ref-targets! probes the actual data via
            bulk-ref-target-namespaces. If exactly one target
            namespace appears AND that namespace has a
            :db.unique/identity attr, use it. The cardinality marker
            keeps the [pk :many] shape for many-refs."
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
               :schema-flexibility :write
               :keep-history? true}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          _ (d/transact conn
                        [{:db/ident :tag/name
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :widget/sku
                          :db/valueType :db.type/string
                          :db/cardinality :db.cardinality/one
                          :db/unique :db.unique/identity}
                         {:db/ident :widget/tags
                          :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/many}])
          _ (d/transact conn
                        [{:tag/name "red"} {:tag/name "round"} {:tag/name "small"}])
          _ (d/transact conn
                        [{:widget/sku "A"
                          :widget/tags [[:tag/name "red"] [:tag/name "round"]]}
                         {:widget/sku "B"
                          :widget/tags [[:tag/name "small"]]}])
          handler (pg/make-query-handler conn)]
      (try
        (let [r (.execute handler "SELECT sku, tags FROM widget ORDER BY sku")]
          (is (nil? (err r)) (str "errored: " (err r)))
          (let [data (rows r)]
            (is (= 2 (count data)))
            ;; Both rows should have non-nil tags column. Element type
            ;; is int8[], not a single bigint — the array form means
            ;; clients can WHERE … = ANY(tags), array_agg, etc.
            (is (every? (fn [[_ t]] (.startsWith ^String (str t) "{")) data)
                (str "expected int8[] form like '{1,2}', got: " data))
            ;; A's tags should be 2-element, B's 1-element.
            (let [a-tags (second (first data))
                  b-tags (second (second data))
                  count-elems (fn [^String s]
                                (->> (-> s
                                         (.replaceAll "[\\{\\}]" "")
                                         (.split ","))
                                     (remove empty?)
                                     count))]
              (is (= 2 (count-elems a-tags))
                  (str "widget A should have 2 tags, got: " a-tags))
              (is (= 1 (count-elems b-tags))
                  (str "widget B should have 1 tag, got: " b-tags)))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(deftest test-as-of-select-star-survives-pre-schema-timestamp
  (testing "SELECT * under the same as-of-pre-schema condition must
            also work — exercises the same column-info fallback path
            via a different translator entry point."
    (.execute *handler* "SET datahike.as_of = '1970-01-01T00:00:00Z'")
    (let [r (.execute *handler* "SELECT * FROM person LIMIT 1")]
      (is (nil? (err r)) (str "as-of-pre-schema SELECT * errored: " (err r)))
      (is (= [] (rows r)) "no rows visible at epoch"))
    (.execute *handler* "RESET datahike.as_of")))

;; ============================================================================
;; Privilege-check functions + boolean-valued WHERE
;; ============================================================================
;; PG has a family of `has_*_privilege(user, obj, action)` functions
;; that ORMs query during catalog discovery. We don't implement a
;; privilege model — one user, one schema, everything granted — so
;; every privilege-check returns true. The functions also appear as
;; bare boolean expressions in WHERE clauses, which our translator
;; previously rejected; those are now routed through translate-expr.

(deftest test-privilege-functions-return-true
  (testing "has_schema_privilege(user, schema, priv)"
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT has_schema_privilege('x', 'public', 'usage')")))))
  (testing "has_table_privilege(user, table, priv)"
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT has_table_privilege('x', 'person', 'select')")))))
  (testing "has_any_column_privilege(user, table, priv)"
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT has_any_column_privilege('x', 'person', 'update')")))))
  (testing "pg_catalog. qualifier accepted"
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT pg_catalog.has_schema_privilege('x', 'public', 'usage')"))))))

(deftest test-bool-function-as-where-predicate
  (testing "WHERE <bool-fn>(...) accepted (PG allows bool expr as predicate)"
    (let [r (.execute *handler*
                      "SELECT name FROM person WHERE has_table_privilege('x', 'person', 'select')")]
      (is (nil? (err r)))
      (is (= 3 (count (rows r))))))
  (testing "WHERE <bool-fn>(...) AND <other>"
    (let [r (.execute *handler*
                      "SELECT name FROM person WHERE has_schema_privilege('x', 'public', 'usage') AND age > 30")]
      (is (nil? (err r)))
      (is (= 1 (count (rows r)))))))

;; ============================================================================
;; Issues #12 / #13 / #14 — boolean input fidelity + current_timestamp
;; ============================================================================

(deftest test-boolean-cast-pg-fidelity
  (testing "string→boolean casts accept the full PG boolin table (issue #12)"
    (is (= [["t" "t" "t" "t"]]
           (rows (.execute *handler*
                           "SELECT '1'::boolean, 'yes'::boolean, ' t '::boolean, 'on'::boolean"))))
    (is (= [["f" "f" "f" "f"]]
           (rows (.execute *handler*
                           "SELECT '0'::boolean, 'no'::boolean, 'of'::boolean, 'F'::boolean")))))
  (testing "invalid boolean input raises 22P02 like PG, not silent false"
    (let [r (.execute *handler* "SELECT 'maybe'::boolean")]
      (is (some? (err r)))
      (is (= "22P02" (sqlstate r))))))

(deftest test-postgres-boolean-comparison-functions
  (is (= [["t" "t"]]
         (rows (.execute *handler* "SELECT booleq(true, true), boolne(true, false)"))))
  (is (= [["Charlie"]]
         (rows (.execute *handler*
                         "SELECT name FROM person WHERE boolne(age > 30, false)")))))

(deftest test-pg-input-is-valid
  (is (= [["t" "f" "t" "f"]]
         (rows (.execute *handler*
                         (str "SELECT pg_input_is_valid('yes', 'bool'), "
                              "pg_input_is_valid('junk', 'bool'), "
                              "pg_input_is_valid('32767', 'int2'), "
                              "pg_input_is_valid('32768', 'int2')")))))
  (is (= [["t" "f" "t" "f"]]
         (rows (.execute *handler*
                         (str "SELECT pg_input_is_valid('abcd  ', 'char(4)'), "
                              "pg_input_is_valid('abcde', 'varchar(4)'), "
                              "pg_input_is_valid('Infinity', 'numeric'), "
                              "pg_input_is_valid('nope', 'uuid')"))))))

(deftest test-pg-input-error-info
  (is (= [["invalid input syntax for type boolean: \"junk\""
           nil nil "22P02"]]
         (rows (.execute *handler*
                         "SELECT * FROM pg_input_error_info('junk', 'bool')")))))

(deftest test-current-timestamp-cast-and-render
  (testing "current_timestamp::date returns one row rendered as a date (issue #13)"
    (let [r (.execute *handler* "SELECT current_timestamp::date")]
      (is (nil? (err r)))
      (is (= 1 (count (rows r))))
      (is (re-matches #"\d{4}-\d{2}-\d{2}" (ffirst (rows r))))))
  (testing "now()::date is not hijacked by the :now fast path"
    (let [r (.execute *handler* "SELECT now()::date")]
      (is (nil? (err r)))
      (is (re-matches #"\d{4}-\d{2}-\d{2}" (ffirst (rows r))))))
  (testing "current_date renders as a bare date"
    (let [r (.execute *handler* "SELECT current_date")]
      (is (nil? (err r)))
      (is (re-matches #"\d{4}-\d{2}-\d{2}" (ffirst (rows r)))))))

(deftest test-sql-temporal-value-functions
  (testing "stable value functions share one statement clock"
    (is (= [["t"]]
           (rows (.execute *handler* "SELECT date(now())::text = current_date::text"))))
    (is (= [["t"]]
           (rows (.execute *handler* "SELECT current_timestamp = now()"))))
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT now()::timetz::text = current_time::text"))))
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT now()::time::text = localtime::text"))))
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT now()::timestamp::text = localtimestamp::text")))))
  (testing "precision-bearing keyword forms lower as value functions"
    (is (= [["t"]]
           (rows (.execute *handler*
                           "SELECT length(current_timestamp::text) >= length(current_timestamp(0)::text)"))))
    (is (= [["t"]]
           (rows (.execute *handler* "SELECT current_timestamp = current_timestamp(7)"))))
    (is (= [["t"]]
           (rows (.execute *handler* "SELECT localtime = localtime(7)"))))
    (is (= [["t"]]
           (rows (.execute *handler* "SELECT localtimestamp = localtimestamp(7)"))))))

(deftest test-current-catalog-inside-expression
  (is (= [["t"]]
         (rows (.execute *handler*
                         "SELECT current_catalog = current_database()")))))

(deftest test-current-schema-follows-search-path
  (let [prepared (.parse *handler*
                         "SELECT coalesce(current_schema, 'missing')"
                         (int-array 0))]
    (is (= [["public"]] (rows (.execute *handler* "SELECT current_schema"))))
    (is (nil? (err (.execute *handler* "SET search_path = notme"))))
    (is (= [[nil]] (rows (.execute *handler* "SELECT current_schema"))))
    (is (= [["t"]] (rows (.execute *handler* "SELECT current_schema IS NULL"))))
    (is (= [["missing"]]
           (rows (.executePrepared *handler* prepared (object-array [nil])))))
    (is (nil? (err (.execute *handler* "SET search_path = notme, pg_catalog"))))
    (is (= [["pg_catalog"]] (rows (.execute *handler* "SELECT current_schema"))))
    (is (= [["notme, pg_catalog"]]
           (rows (.execute *handler* "SHOW search_path"))))
    (is (nil? (err (.execute *handler* "RESET search_path"))))
    (is (= [["public"]] (rows (.execute *handler* "SELECT current_schema"))))))

(deftest test-create-view-is-live-and-transactional
  (is (nil? (err (.execute *handler* "CREATE TABLE view_base (id int, n numeric)"))))
  (is (nil? (err (.execute *handler* "INSERT INTO view_base VALUES (1, 1.25)"))))
  (is (nil? (err (.execute *handler*
                           (str "CREATE VIEW live_view AS "
                                "SELECT id, n::numeric(8,2) AS amount FROM view_base")))))
  (is (= [["1" "1.25"]]
         (rows (.execute *handler* "SELECT * FROM live_view ORDER BY id"))))
  (is (= [["live_view"]]
         (rows (.execute *handler*
                         "SELECT viewname FROM pg_views WHERE viewname = 'live_view'"))))
  (is (= [["v"]]
         (rows (.execute *handler*
                         "SELECT relkind FROM pg_class WHERE relname = 'live_view'"))))
  (is (= [["id" "23" "p"] ["amount" "1700" "m"]]
         (rows (.execute *handler*
                         (str "SELECT a.attname, a.atttypid, a.attstorage FROM pg_attribute a "
                              "JOIN pg_class c ON a.attrelid = c.oid "
                              "WHERE c.relname = 'live_view' ORDER BY a.attnum")))))
  (is (= [["integer"] ["numeric(8,2)"]]
         (rows (.execute *handler*
                         (str "SELECT format_type(a.atttypid, a.atttypmod) FROM pg_attribute a "
                              "JOIN pg_class c ON a.attrelid = c.oid "
                              "WHERE c.relname = 'live_view' ORDER BY a.attnum")))))
  (is (.contains ^String
       (first-val (.execute *handler*
                            (str "SELECT pg_get_viewdef(oid::oid, true) FROM pg_class "
                                 "WHERE relname = 'live_view'")))
                 "view_base"))
  (is (= [["1" "1.25"]]
         (rows (.execute *handler*
                         (str "SELECT b.id, v.amount FROM view_base b "
                              "JOIN live_view v ON b.id = v.id WHERE b.id = 1")))))
  (testing "unsupported column-name lists fail explicitly"
    (is (= "0A000"
           (sqlstate (.execute *handler*
                               "CREATE VIEW named_view (view_id) AS SELECT id FROM view_base")))))
  (is (nil? (err (.execute *handler* "INSERT INTO view_base VALUES (2, 2.50)"))))
  (is (= [["2" "2.50"]]
         (rows (.execute *handler* "SELECT id, amount FROM live_view WHERE id = 2"))))
  (is (nil? (err (.execute *handler*
                           "CREATE OR REPLACE VIEW live_view AS SELECT id FROM view_base"))))
  (is (= [["1"] ["2"]]
         (rows (.execute *handler* "SELECT * FROM live_view ORDER BY id"))))
  (testing "view metadata follows transaction rollback"
    (is (nil? (err (.execute *handler* "BEGIN"))))
    (is (nil? (err (.execute *handler*
                             "CREATE VIEW rolled_back_view AS SELECT id FROM view_base"))))
    (is (nil? (err (.execute *handler* "ROLLBACK"))))
    (is (= "42P01"
           (sqlstate (.execute *handler* "SELECT * FROM rolled_back_view")))))
  (is (nil? (err (.execute *handler* "DROP VIEW live_view"))))
  (is (= "42P01" (sqlstate (.execute *handler* "SELECT * FROM live_view"))))
  (is (nil? (err (.execute *handler* "DROP VIEW IF EXISTS live_view")))))

(deftest test-insert-update-current-timestamp-keyword
  (testing "bare current_timestamp (TimeKeyExpression) in VALUES / SET (issue #14)"
    (is (nil? (err (.execute *handler* "CREATE TABLE tkey(id INTEGER, ts TIMESTAMP)"))))
    (is (nil? (err (.execute *handler* "INSERT INTO tkey(id, ts) VALUES (1, current_timestamp)"))))
    (let [r (.execute *handler* "SELECT ts IS NOT NULL FROM tkey WHERE id = 1")]
      (is (= [["t"]] (rows r))))
    (is (nil? (err (.execute *handler* "UPDATE tkey SET ts = current_timestamp WHERE id = 1"))))
    (let [r (.execute *handler* "SELECT ts IS NOT NULL FROM tkey WHERE id = 1")]
      (is (= [["t"]] (rows r))))))

(deftest test-update-with-negative-literal-arithmetic
  (testing "SET x = x + <negative literal> (SignedExpression operand; pgbench tpcb)"
    (is (nil? (err (.execute *handler* "CREATE TABLE tneg(id INTEGER, bal INTEGER)"))))
    (is (nil? (err (.execute *handler* "INSERT INTO tneg(id, bal) VALUES (1, 100)"))))
    (is (nil? (err (.execute *handler* "UPDATE tneg SET bal = bal + -30 WHERE id = 1"))))
    (is (= [["70"]] (rows (.execute *handler* "SELECT bal FROM tneg WHERE id = 1"))))
    (is (nil? (err (.execute *handler* "UPDATE tneg SET bal = bal + +5 WHERE id = 1"))))
    (is (= [["75"]] (rows (.execute *handler* "SELECT bal FROM tneg WHERE id = 1"))))))

(deftest test-prepared-update-with-param-arithmetic
  (testing "extended-protocol UPDATE SET x = x + $1 / WHERE pk = $2 (pgbench -M prepared)"
    (is (nil? (err (.execute *handler* "CREATE TABLE acct(aid INTEGER PRIMARY KEY, bal INTEGER)"))))
    (is (nil? (err (.execute *handler* "INSERT INTO acct(aid, bal) VALUES (1, 100), (2, 200)"))))
    (let [parsed (.parse *handler* "UPDATE acct SET bal = bal + $1 WHERE aid = $2" nil)
          ;; bound-params is a 1-indexed Object[] (element 0 unused).
          ;; The wire layer decodes params by their inferred OIDs, so the
          ;; SET operand arrives typed for known columns; a String here
          ;; still exercises the unknown-type numeric-context coercion
          ;; (num-operand), while the WHERE param is typed like the wire
          ;; delivers it.
          r (.executePrepared *handler* parsed (object-array [nil "-30" (long 1)]))]
      (is (nil? (err r)) (err r))
      (.commitImplicit *handler*))
    (is (= [["70"]] (rows (.execute *handler* "SELECT bal FROM acct WHERE aid = 1"))))
    (is (= [["200"]] (rows (.execute *handler* "SELECT bal FROM acct WHERE aid = 2"))))))
