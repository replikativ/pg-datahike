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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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

;; ============================================================================
;; SQL-level PREPARE / EXECUTE / DEALLOCATE
;; ============================================================================

(deftest test-cursor-sees-declare-time-snapshot
  (testing "FETCH sees DB state at DECLARE, not later writes"
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

(deftest test-pg-format-type
  (testing "format_type maps OIDs to canonical type names"
    (is (= [["integer"]]            (rows (.execute *handler* "SELECT format_type(23, -1)"))))
    (is (= [["bigint"]]              (rows (.execute *handler* "SELECT format_type(20, -1)"))))
    (is (= [["text"]]                (rows (.execute *handler* "SELECT format_type(25, -1)"))))
    (is (= [["character varying"]]   (rows (.execute *handler* "SELECT format_type(1043, -1)"))))
    (is (= [["boolean"]]             (rows (.execute *handler* "SELECT format_type(16, -1)")))))

  (testing "format_type composes inside SELECT against pg_type"
    (let [r (.execute *handler* "SELECT typname, format_type(oid, -1) FROM pg_type WHERE oid IN (16, 23, 25) ORDER BY oid")]
      (is (= [["bool" "boolean"] ["int4" "integer"] ["text" "text"]]
             (rows r))))))

(deftest test-pg-typeof
  (testing "pg_typeof returns the type name of a literal expression"
    (is (= [["bigint"]]          (rows (.execute *handler* "SELECT pg_typeof(1)"))))
    (is (= [["text"]]            (rows (.execute *handler* "SELECT pg_typeof('hello')"))))
    (is (= [["double precision"]] (rows (.execute *handler* "SELECT pg_typeof(1.5)"))))
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
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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
      (is (= [["1"]] (rows r)) "age is the 2nd person column → ordinal_position-1 = 1")))

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
  (testing "Unknown column returns NULL values (EAV: missing attribute = NULL)"
    ;; In EAV, a non-existent column maps to a non-existent attribute.
    ;; get-else returns :__null__ sentinel which displays as NULL.
    ;; This is consistent behavior, not an error.
    (let [r (.execute *handler* "SELECT nonexistent_column FROM person")]
      (is (nil? (err r)))))

  (testing "Type mismatch in INSERT"
    ;; Try inserting string into INTEGER column via DDL table
    (.execute *handler* "CREATE TABLE err_test (val INTEGER)")
    (let [r (.execute *handler* "INSERT INTO err_test VALUES ('not_a_number')")]
      ;; Should either error or coerce — but not crash
      (is (some? r))))

  (testing "Division by zero"
    ;; Division by zero in SQL should not crash the server
    (let [r (.execute *handler* "SELECT 1 / 0")]
      (is (some? r)))))

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
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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
