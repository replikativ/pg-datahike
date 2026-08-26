(ns datahike.test.pg-extended-query-oid-test
  "Regression tests for RowDescription OID inference via the Extended
   Query protocol (Parse/Describe/Execute).

   Background: PG's `Describe` emits the RowDescription *before* Execute
   runs, so type OIDs must be known at parse time — not inferred from
   result values. Our `describeResult` previously fell back to TEXT for
   every non-schema-attribute column, which broke:

     - Metabase's `can-connect?` (asserts `(= 1 (first (vals row)))` on
       `SELECT 1` — Long 1 vs String \"1\" fails)
     - Any ORM introspecting `ResultSetMetaData.getColumnType()` for
       aggregates, arithmetic, function results, CAST expressions
     - pgjdbc's cached per-statement RowDescription (wrong client-side
       typing: `rs.getObject` returns String when Long was expected)

   Fix: PG-style per-node type inference (`datahike.pg.sql.oid-infer`)
   walks the JSqlParser AST at translate-select time and attaches
   `:select-item-oids` to the parsed map. `describeResult` consults
   these before the TEXT fallback.

   This file covers each rule in the inference registry + the pgjdbc
   Extended Query path end-to-end."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryResult]
           [java.sql Connection DriverManager PreparedStatement ResultSet
            ResultSetMetaData Types]))

(def oid-bool      16)
(def oid-int4      23)
(def oid-int8      20)
(def oid-text      25)
(def oid-name      19)
(def oid-float4   700)
(def oid-float8   701)
(def oid-numeric 1700)
(def oid-json      114)
(def oid-jsonb    3802)
(def oid-bytea      17)
(def oid-bit      1560)
(def oid-varbit   1562)
(def oid-int4-array 1007)
(def oid-int8-array 1016)
(def oid-time     1083)
(def oid-timestamp 1114)
(def oid-timetz   1266)
(def oid-timetz-array 1270)
(def oid-timestamptz 1184)
(def oid-date     1082)

(def test-schema
  [{:db/ident :employee/id    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :employee/name  :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :employee/salary :db/valueType :db.type/double
    :db/cardinality :db.cardinality/one}])

(def test-data
  [{:employee/id 1 :employee/name "Alice"   :employee/salary 90000.0}
   {:employee/id 2 :employee/name "Bob"     :employee/salary 80000.0}
   {:employee/id 3 :employee/name "Charlie" :employee/salary 70000.0}])

(def ^:dynamic *conn* nil)
(def ^:dynamic *handler* nil)
(def ^:dynamic *port* nil)
(def ^:dynamic *jdbc-url* nil)

(defn- with-server [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          _ (d/transact conn test-schema)
          _ (d/transact conn test-data)
          handler (pg/make-query-handler conn)
          {:keys [server]} (pg/start-server {"demo" conn} {:port 0})
          port (.getPort server)]
      (try
        (binding [*conn* conn
                  *handler* handler
                  *port* port
                  *jdbc-url* (str "jdbc:postgresql://127.0.0.1:" port
                                  "/demo?user=x&password=x&sslmode=disable")]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each with-server)

;; ---------------------------------------------------------------------------
;; Unit tests — describeResult OID inference
;; ---------------------------------------------------------------------------

(defn- describe-oids [sql]
  (let [parsed (.parse *handler* sql (int-array 0))
        ^PgWireServer$QueryResult r (.describeResult *handler* parsed)]
    (when (and r (.-columnOids r))
      (vec (.-columnOids r)))))

(defn- exec-oids [sql]
  (let [^PgWireServer$QueryResult r (.execute *handler* sql)]
    (when (.-columnOids r)
      (vec (.-columnOids r)))))

(defn- exec-rows [sql]
  (let [^PgWireServer$QueryResult r (.execute *handler* sql)]
    (mapv vec (.-rows r))))

(defn- assert-describe-and-execute-oids [expected sql]
  (is (= expected (describe-oids sql)) (str "Describe: " sql))
  (is (= expected (exec-oids sql)) (str "Execute: " sql)))

;; Table-free literal SELECTs — Metabase `can-connect?` path

(deftest describe-int-literal
  (testing "SELECT 1 -> INT4. PostgreSQL types an integer literal as the
            narrowest of int4/int8 that holds it, so a small literal is
            int4, not int8. (It was TEXT once, which broke Metabase's
            can-connect? probe -- hence this test.)"
    (is (= [oid-int4] (describe-oids "SELECT 1"))))
  (testing "and int8 only when it does not fit int4"
    (is (= [oid-int8] (describe-oids "SELECT 2147483648")))))

(deftest describe-int-literal-with-alias
  (testing "SELECT 1 AS n -> INT4"
    (is (= [oid-int4] (describe-oids "SELECT 1 AS n")))))

(deftest describe-decimal-literal
  (testing "SELECT 1.5 -> NUMERIC, not FLOAT8: PostgreSQL types an
            unadorned decimal literal as numeric"
    (is (= [oid-numeric] (describe-oids "SELECT 1.5"))))
  (testing "SELECT 1.5::float8 -> FLOAT8"
    (is (= [oid-float8] (describe-oids "SELECT 1.5::float8")))))

(deftest describe-string-literal
  (testing "SELECT 'hello' -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT 'hello'")))))

(deftest describe-mixed-literals
  (testing "SELECT 'x' AS s, 2 AS m -> [TEXT, INT4]"
    (is (= [oid-text oid-int4] (describe-oids "SELECT 'x' AS s, 2 AS m")))))

(deftest describe-null-literal
  (testing "SELECT NULL -> TEXT (PG default for untyped NULL)"
    (is (= [oid-text] (describe-oids "SELECT NULL")))))

(deftest describe-boolean-literal-true
  (testing "SELECT TRUE -> BOOL"
    (is (= [oid-bool] (describe-oids "SELECT TRUE")))))

(deftest describe-boolean-literal-false
  (testing "SELECT FALSE -> BOOL"
    (is (= [oid-bool] (describe-oids "SELECT FALSE")))))

;; Schema column refs — existing path should still work

(deftest describe-schema-string-column
  (testing "SELECT name FROM employee -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT name FROM employee")))))

(deftest describe-schema-long-column
  (testing "SELECT id FROM employee -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT id FROM employee")))))

(deftest describe-schema-double-column
  (testing "SELECT salary FROM employee -> FLOAT8"
    (is (= [oid-float8] (describe-oids "SELECT salary FROM employee")))))

;; Aggregates — COUNT→INT8, SUM/AVG/MIN/MAX→ type of arg

(deftest describe-count-star
  (testing "SELECT COUNT(*) FROM employee -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT COUNT(*) FROM employee")))))

(deftest describe-count-col
  (testing "SELECT COUNT(name) FROM employee -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT COUNT(name) FROM employee")))))

(deftest describe-sum-double-col
  (testing "SELECT SUM(salary) FROM employee -> FLOAT8"
    (is (= [oid-float8] (describe-oids "SELECT SUM(salary) FROM employee")))))

(deftest describe-avg-double-col
  (testing "SELECT AVG(salary) FROM employee -> FLOAT8"
    (is (= [oid-float8] (describe-oids "SELECT AVG(salary) FROM employee")))))

(deftest describe-max-double-col
  (testing "SELECT MAX(salary) FROM employee -> FLOAT8"
    (is (= [oid-float8] (describe-oids "SELECT MAX(salary) FROM employee")))))

(deftest describe-min-long-col
  (testing "SELECT MIN(id) FROM employee -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT MIN(id) FROM employee")))))

;; Scalar functions

(deftest describe-length-text-col
  (testing "SELECT LENGTH(name) FROM employee -> INT4"
    (is (= [oid-int4] (describe-oids "SELECT LENGTH(name) FROM employee")))))

(deftest describe-upper-text-col
  (testing "SELECT UPPER(name) FROM employee -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT UPPER(name) FROM employee")))))

(deftest describe-lower-text-col
  (testing "SELECT LOWER(name) FROM employee -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT LOWER(name) FROM employee")))))

(deftest describe-abs-long-col
  (testing "SELECT ABS(id) FROM employee -> INT8 (preserves arg type)"
    (is (= [oid-int8] (describe-oids "SELECT ABS(id) FROM employee")))))

(deftest describe-coalesce-text
  (testing "SELECT COALESCE(name, 'N/A') FROM employee -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT COALESCE(name, 'N/A') FROM employee")))))

(deftest postgres-function-signature-oids
  (testing "polymorphic numeric functions follow PostgreSQL overload resolution"
    (assert-describe-and-execute-oids
     [oid-int4] "SELECT nullif(1::int4, 2::int8)")
    (assert-describe-and-execute-oids
     [oid-int8] "SELECT mod(1::int4, 2::int8)")
    (assert-describe-and-execute-oids
     [oid-numeric] "SELECT pow(2::numeric, 3::numeric)"))
  (testing "varchar overlay resolves through PostgreSQL's text overload"
    (assert-describe-and-execute-oids
     [oid-text] "SELECT overlay('abc'::varchar placing 'x' from 2)"))
  (testing "real resolves to the float8 ceil overload"
    (assert-describe-and-execute-oids
     [oid-float8] "SELECT ceil(1.25::real)"))
  (testing "EXTRACT syntax returns numeric while date_part returns float8"
    (assert-describe-and-execute-oids
     [oid-numeric oid-float8]
     "SELECT extract(year FROM DATE '2020-01-01'), date_part('year', DATE '2020-01-01')")))

(deftest postgres-array-result-oids
  (testing "constructors select a common element type and preserve dimensions"
    (assert-describe-and-execute-oids
     [oid-int8-array] "SELECT ARRAY[1, 2147483648]")
    (assert-describe-and-execute-oids
     [oid-int4-array] "SELECT ARRAY[ARRAY[1,2], ARRAY[3,4]]"))
  (testing "array_prepend returns its second argument's array family"
    (assert-describe-and-execute-oids
     [oid-int4-array] "SELECT array_prepend(0, ARRAY[1,2])")))

(deftest postgres-json-function-oids
  (assert-describe-and-execute-oids
   [oid-json oid-jsonb oid-int4 oid-text]
   (str "SELECT json_build_object('a',1), jsonb_build_object('a',1), "
        "jsonb_array_length('[1]'::jsonb), "
        "jsonb_extract_path_text('{\"a\":1}'::jsonb, 'a')")))

(deftest postgres-json-access-operator-oids
  (assert-describe-and-execute-oids
   [oid-jsonb oid-text oid-json oid-text]
   (str "SELECT '{\"a\":1}'::jsonb->'a', '{\"a\":1}'::jsonb->>'a', "
        "'{\"a\":1}'::json->'a', '{\"a\":1}'::json->>'a'")))

(deftest declared-json-and-jsonb-column-oids-stay-distinct
  (exec-rows "CREATE TABLE oid_json_probe (j json, b jsonb)")
  (assert-describe-and-execute-oids
   [oid-json oid-jsonb] "SELECT j, b FROM oid_json_probe"))

(deftest derived-columns-preserve-collapsed-storage-oids
  (assert-describe-and-execute-oids
   [oid-int4 oid-float4 oid-json oid-timestamptz]
   (str "SELECT i, r, j, z FROM (SELECT 1::int4 AS i, 1::real AS r, "
        "'{\"a\":1}'::json AS j, now() AS z) AS q")))

(deftest postgres-window-function-oids
  (assert-describe-and-execute-oids
   [oid-int8 oid-int8 oid-int4 oid-float8 oid-int8]
   (str "SELECT row_number() OVER (), rank() OVER (ORDER BY id), "
        "ntile(2) OVER (ORDER BY id), percent_rank() OVER (ORDER BY id), "
        "lag(id) OVER (ORDER BY id) FROM employee")))

(deftest window-output-preserves-select-list-position
  (is (= [["1" "1" "11"] ["2" "2" "12"] ["3" "3" "13"]]
         (exec-rows
          (str "SELECT id, row_number() OVER (ORDER BY id), id + 10 "
               "FROM employee ORDER BY id")))))

(deftest window-output-position-accounts-for-wildcard-expansion
  (assert-describe-and-execute-oids
   [oid-int8 oid-text oid-float8 oid-int8]
   "SELECT *, row_number() OVER (ORDER BY id) FROM employee")
  (assert-describe-and-execute-oids
   [oid-int8 oid-int8 oid-text oid-float8]
   "SELECT row_number() OVER (ORDER BY id), * FROM employee"))

(deftest set-operation-common-oids-match-describe-and-execute
  (assert-describe-and-execute-oids
   [oid-int4] "SELECT 1::int4 UNION ALL SELECT 2")
  (assert-describe-and-execute-oids
   [oid-int8] "SELECT 1::int4 UNION ALL SELECT 2147483648::int8")
  (assert-describe-and-execute-oids
   [oid-float8] "SELECT 1::numeric UNION ALL SELECT 2::float8")
  (assert-describe-and-execute-oids
   [oid-int4] "SELECT NULL UNION ALL SELECT 2::int4")
  (assert-describe-and-execute-oids
   [oid-int4] "SELECT '1' UNION ALL SELECT 2::int4"))

(deftest set-operation-values-are-coerced-to-the-common-carrier
  (let [sql (str "SELECT DATE '2020-01-01' "
                 "UNION ALL SELECT TIMESTAMP '2020-01-02 03:04:05'")]
    (assert-describe-and-execute-oids [oid-timestamp] sql)
    (is (= [["2020-01-01 00:00"] ["2020-01-02 03:04:05"]]
           (exec-rows sql)))))

(deftest postgres-system-function-oids
  (assert-describe-and-execute-oids
   [oid-name] "SELECT current_database()")
  (assert-describe-and-execute-oids
   [oid-name] "SELECT current_schema()")
  (assert-describe-and-execute-oids
   [oid-timestamptz] "SELECT now()")
  (assert-describe-and-execute-oids
   [oid-timetz] "SELECT current_time"))

(deftest postgres-local-time-oids
  (assert-describe-and-execute-oids
   [oid-time oid-timestamp] "SELECT localtime, localtimestamp"))

(deftest declared-time-zone-column-oids-stay-distinct
  (exec-rows
   (str "CREATE TABLE oid_time_probe "
        "(t time without time zone, z time with time zone, za time with time zone[])"))
  (assert-describe-and-execute-oids
   [oid-time oid-timetz oid-timetz-array] "SELECT t, z, za FROM oid_time_probe"))

;; Binary arithmetic — numeric promotion

(deftest describe-add-long-long
  (testing "SELECT 1 + 2 -> INT4: integer arithmetic keeps the wider
            operand's width, and both operands here are int4"
    (is (= [oid-int4] (describe-oids "SELECT 1 + 2"))))
  (testing "and widens when an operand is int8"
    (is (= [oid-int8] (describe-oids "SELECT 1 + 2147483648")))))

(deftest describe-mul-schema-double
  (testing "SELECT salary * 2 FROM employee -> FLOAT8 (any-float promotes)"
    (is (= [oid-float8] (describe-oids "SELECT salary * 2 FROM employee")))))

(deftest describe-add-schema-long
  (testing "SELECT id + 1 FROM employee -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT id + 1 FROM employee")))))

(deftest describe-concat-strings
  (testing "SELECT name || ' (emp)' FROM employee -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT name || ' (emp)' FROM employee")))))

(deftest overloaded-concat-preserves-postgres-result-family
  (assert-describe-and-execute-oids
   [oid-jsonb] "SELECT '{\"a\":1}'::jsonb || '{\"b\":2}'::jsonb")
  (assert-describe-and-execute-oids
   [oid-jsonb] "SELECT '{\"a\":1}'::jsonb || '{}'")
  (assert-describe-and-execute-oids
   [oid-bytea] "SELECT '\\x01'::bytea || '\\x02'::bytea")
  (assert-describe-and-execute-oids
   [oid-varbit] "SELECT B'10' || B'01'")
  (assert-describe-and-execute-oids
   [oid-int8-array] "SELECT ARRAY[1] || ARRAY[2147483648]")
  (assert-describe-and-execute-oids
   [oid-int4-array] "SELECT 0 || ARRAY[1,2]")
  (assert-describe-and-execute-oids
   [oid-int4-array] "SELECT ARRAY[1,2] || 3"))

(deftest overloaded-scalar-functions-report-declared-return-type
  (assert-describe-and-execute-oids
   [oid-float8 oid-numeric oid-text oid-bit]
   (str "SELECT floor(1::real), round(1::numeric, 0), "
        "substring('abc'::varchar FROM 1), substring(B'101' FROM 1)")))

(deftest lag-default-participates-in-common-type-resolution
  (exec-rows "CREATE TABLE oid_lag_probe (v int4)")
  (exec-rows "INSERT INTO oid_lag_probe VALUES (1), (2)")
  (assert-describe-and-execute-oids
   [oid-int8]
   (str "SELECT lag(v, 1, 2147483648::int8) OVER (ORDER BY v) "
        "FROM oid_lag_probe")))

(deftest timetz-binary-codec-round-trips-scalar-and-array
  (let [value (java.time.OffsetTime/parse "12:34:56.123456-07:30")
        bytes (datahike.pg.PgParamCodec/encodeBinary oid-timetz value)]
    (is (= 12 (alength bytes)))
    (is (= value (datahike.pg.PgParamCodec/decodeBinary oid-timetz bytes))))
  (doseq [text ["12:34:56+00" "12:34:56+02"]]
    (is (some? (datahike.pg.PgParamCodec/encodeBinary oid-timetz text))))
  (let [text "{12:34:56+02:00,01:02:03-07:30}"
        bytes (datahike.pg.PgParamCodec/encodeArrayBinary oid-timetz-array text)]
    (is (= text (datahike.pg.PgParamCodec/decodeArrayBinary oid-timetz-array bytes)))))

;; Comparisons / logical → BOOL
;;
;; Historically these threw `Unsupported SQL expression` from
;; translate-expr when placed in a projection — translate-predicate-expr
;; handled them only in WHERE/HAVING. Fixed in the same PR as OID
;; inference: translate-expr now delegates boolean operators to
;; translate-predicate-expr so `SELECT col > 5` materializes to a
;; boolean-valued fresh var. Keep these regression tests adjacent to
;; the OID rule so the two stay in sync.

(deftest describe-comparison
  (testing "SELECT salary > 75000 FROM employee -> BOOL"
    (is (= [oid-bool] (describe-oids "SELECT salary > 75000 FROM employee")))))

(deftest describe-not-expr
  (testing "SELECT NOT (id = 1) FROM employee -> BOOL"
    (is (= [oid-bool] (describe-oids "SELECT NOT (id = 1) FROM employee")))))

(deftest describe-is-null
  (testing "SELECT name IS NULL FROM employee -> BOOL"
    (is (= [oid-bool] (describe-oids "SELECT name IS NULL FROM employee")))))

;; CAST — target type

(deftest describe-cast-to-bigint
  ;; SELECT CAST('1' AS BIGINT) used to crash the table-free CAST
  ;; branch with "String cannot be cast to Number": raw was a Java
  ;; String but (long raw) expected Number. Fixed to Long/parseLong
  ;; / Double/parseDouble the string form before coercing.
  (testing "SELECT CAST('1' AS BIGINT) -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT CAST('1' AS BIGINT)")))))

(deftest describe-cast-to-text
  (testing "SELECT CAST(1 AS TEXT) -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT CAST(1 AS TEXT)")))))

(deftest describe-cast-schema-col
  (testing "SELECT CAST(salary AS BIGINT) FROM employee -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT CAST(salary AS BIGINT) FROM employee")))))

;; CASE → type of first WHEN branch

(deftest describe-case-text
  (testing "SELECT CASE WHEN id > 1 THEN 'many' ELSE 'one' END FROM employee -> TEXT"
    (is (= [oid-text]
           (describe-oids
            "SELECT CASE WHEN id > 1 THEN 'many' ELSE 'one' END FROM employee")))))

(deftest describe-case-int
  (testing "SELECT CASE WHEN id > 1 THEN 100 ELSE 200 END FROM employee -> INT4"
    (is (= [oid-int4]
           (describe-oids
            "SELECT CASE WHEN id > 1 THEN 100 ELSE 200 END FROM employee")))))

;; Backwards compatibility — simple query should keep emitting same OIDs

(deftest simple-query-literal-ok
  (testing "Simple-Query SELECT 1 reports INT4, the same as the extended
            path. It used to report INT8 from value-based inference on the
            runtime Long, because the plan-cache rewrite turned the literal
            into a $N before the translator could type it. The literal's
            type now travels with its value."
    (is (= [oid-int4] (exec-oids "SELECT 1"))))
  (testing "and INT8 when the literal does not fit int4"
    (is (= [oid-int8] (exec-oids "SELECT 2147483648")))))

(deftest simple-query-bool-literal
  (testing "Simple-Query SELECT TRUE now returns BOOL (was TEXT)"
    (is (= [oid-bool] (exec-oids "SELECT TRUE")))))

(deftest select-true-returns-boolean-text-form
  (testing "SELECT TRUE emits the PG text form 't' (not String 'true') —
            value-level regression alongside the OID fix. Before: table-free
            parser left TRUE as a Column name-string, value->string passed
            through as 'true'. After: JSqlParser 5's BooleanValue is
            extracted to a real Boolean, so value->string returns 't'."
    (is (= [["t"]] (exec-rows "SELECT TRUE")))
    (is (= [["f"]] (exec-rows "SELECT FALSE")))))

;; ---------------------------------------------------------------------------
;; Integration — pgjdbc Extended Query
;; ---------------------------------------------------------------------------
;;
;; pgjdbc-in-extended-mode uses server-side prepared statements; the
;; RowDescription reported here is exactly what Metabase / Hibernate /
;; any ORM sees when it introspects result metadata. `preferQueryMode=
;; extended` forces the same path the real Metabase uses when an
;; application_name is set.

(defn- ^Connection jdbc-conn []
  (DriverManager/getConnection
   (str *jdbc-url* "&preferQueryMode=extended&prepareThreshold=1")))

(defn- meta-types [sql]
  (with-open [^Connection c (jdbc-conn)
              ^PreparedStatement ps (.prepareStatement c sql)
              ^ResultSet rs (.executeQuery ps)]
    (let [^ResultSetMetaData md (.getMetaData rs)]
      (vec (for [i (range 1 (inc (.getColumnCount md)))]
             (.getColumnType md i))))))

(defn- meta-type-names [sql]
  (with-open [^Connection c (jdbc-conn)
              ^PreparedStatement ps (.prepareStatement c sql)
              ^ResultSet rs (.executeQuery ps)]
    (let [^ResultSetMetaData md (.getMetaData rs)]
      (vec (for [i (range 1 (inc (.getColumnCount md)))]
             (.getColumnTypeName md i))))))

(deftest pgjdbc-extended-select-one
  (testing "Metabase's exact check: PreparedStatement rs.getObject(1) is Long 1"
    (with-open [^Connection c (jdbc-conn)
                ^PreparedStatement ps (.prepareStatement c "SELECT 1")
                ^ResultSet rs (.executeQuery ps)]
      (is (.next rs))
      (let [obj (.getObject rs 1)]
        ;; int4 now, so pgjdbc hands back an Integer rather than a Long.
        (is (instance? Integer obj)
            (str "expected Integer, got " (some-> obj class .getName)
                 " with value " obj))
        (is (= 1 (long obj)))))))

(deftest pgjdbc-extended-metadata-select-one
  (testing "ResultSetMetaData reports INTEGER for SELECT 1 -- PostgreSQL
            types a small integer literal as int4"
    (is (= [Types/INTEGER] (meta-types "SELECT 1")))))

(deftest pgjdbc-extended-metadata-aggregate
  (testing "COUNT(*) reports BIGINT; SUM(salary) reports DOUBLE"
    (is (= [Types/BIGINT]
           (meta-types "SELECT COUNT(*) FROM employee")))
    (is (= [Types/DOUBLE]
           (meta-types "SELECT SUM(salary) FROM employee")))))

(deftest pgjdbc-extended-metadata-cast
  (testing "CAST(... AS BIGINT) reports BIGINT"
    (is (= [Types/BIGINT]
           (meta-types "SELECT CAST(salary AS BIGINT) FROM employee")))))

(deftest pgjdbc-extended-metadata-semantic-types
  (is (= ["jsonb" "numeric" "int4" "timetz"]
         (meta-type-names
          (str "SELECT jsonb_build_object('a',1), "
               "extract(year FROM DATE '2020-01-01'), "
               "ntile(2) OVER (ORDER BY id), current_time FROM employee")))))

(deftest pgjdbc-current-time-decodes-as-offset-time
  (with-open [^Connection c (jdbc-conn)
              ^PreparedStatement ps (.prepareStatement c "SELECT current_time")
              ^ResultSet rs (.executeQuery ps)]
    (is (.next rs))
    (let [v (.getObject rs 1 java.time.OffsetTime)]
      (is (instance? java.time.OffsetTime v))
      (is (= java.time.ZoneOffset/UTC (.getOffset ^java.time.OffsetTime v))))))

(deftest pgjdbc-declared-parameter-types-drive-lowering
  (testing "the Parse message's int4 OID types unary operators and pg_typeof"
    (with-open [^Connection c (jdbc-conn)]
      (with-open [^PreparedStatement ps (.prepareStatement c "SELECT -(?)")]
        (.setInt ps 1 42)
        (with-open [^ResultSet rs (.executeQuery ps)]
          (is (.next rs))
          (is (= Types/INTEGER (.getColumnType (.getMetaData rs) 1)))
          (is (= -42 (.getInt rs 1)))))
      (with-open [^PreparedStatement ps (.prepareStatement c "SELECT pg_typeof(?)::text")]
        (.setInt ps 1 42)
        (with-open [^ResultSet rs (.executeQuery ps)]
          (is (.next rs))
          (is (= "integer" (.getString rs 1)))))))
  (testing "a parameterized target-list SRF materializes after Bind"
    (with-open [^Connection c (jdbc-conn)
                ^PreparedStatement ps (.prepareStatement c "SELECT generate_series(1, ?)")]
      (.setInt ps 1 5)
      (with-open [^ResultSet rs (.executeQuery ps)]
        (is (= Types/INTEGER (.getColumnType (.getMetaData rs) 1)))
        (is (= [1 2 3 4 5]
               (loop [values []]
                 (if (.next rs)
                   (recur (conj values (.getInt rs 1)))
                   values))))))))
