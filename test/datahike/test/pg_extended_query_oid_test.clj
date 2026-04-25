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
(def oid-float8   701)
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
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
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

;; Table-free literal SELECTs — Metabase `can-connect?` path

(deftest describe-int-literal
  (testing "SELECT 1 -> INT8 (was TEXT; broke Metabase can-connect?)"
    (is (= [oid-int8] (describe-oids "SELECT 1")))))

(deftest describe-int-literal-with-alias
  (testing "SELECT 1 AS n -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT 1 AS n")))))

(deftest describe-float-literal
  (testing "SELECT 1.5 -> FLOAT8"
    (is (= [oid-float8] (describe-oids "SELECT 1.5")))))

(deftest describe-string-literal
  (testing "SELECT 'hello' -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT 'hello'")))))

(deftest describe-mixed-literals
  (testing "SELECT 'x' AS s, 2 AS m -> [TEXT, INT8]"
    (is (= [oid-text oid-int8] (describe-oids "SELECT 'x' AS s, 2 AS m")))))

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

;; Binary arithmetic — numeric promotion

(deftest describe-add-long-long
  (testing "SELECT 1 + 2 -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT 1 + 2")))))

(deftest describe-mul-schema-double
  (testing "SELECT salary * 2 FROM employee -> FLOAT8 (any-float promotes)"
    (is (= [oid-float8] (describe-oids "SELECT salary * 2 FROM employee")))))

(deftest describe-add-schema-long
  (testing "SELECT id + 1 FROM employee -> INT8"
    (is (= [oid-int8] (describe-oids "SELECT id + 1 FROM employee")))))

(deftest describe-concat-strings
  (testing "SELECT name || ' (emp)' FROM employee -> TEXT"
    (is (= [oid-text] (describe-oids "SELECT name || ' (emp)' FROM employee")))))

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
  (testing "SELECT CASE WHEN id > 1 THEN 100 ELSE 200 END FROM employee -> INT8"
    (is (= [oid-int8]
           (describe-oids
            "SELECT CASE WHEN id > 1 THEN 100 ELSE 200 END FROM employee")))))

;; Backwards compatibility — simple query should keep emitting same OIDs

(deftest simple-query-literal-ok
  (testing "Simple-Query SELECT 1 already returned INT8 via value inference"
    (is (= [oid-int8] (exec-oids "SELECT 1")))))

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

(deftest pgjdbc-extended-select-one
  (testing "Metabase's exact check: PreparedStatement rs.getObject(1) is Long 1"
    (with-open [^Connection c (jdbc-conn)
                ^PreparedStatement ps (.prepareStatement c "SELECT 1")
                ^ResultSet rs (.executeQuery ps)]
      (is (.next rs))
      (let [obj (.getObject rs 1)]
        (is (instance? Long obj)
            (str "expected Long, got " (some-> obj class .getName)
                 " with value " obj))
        (is (= 1 (long obj)))))))

(deftest pgjdbc-extended-metadata-select-one
  (testing "ResultSetMetaData reports BIGINT (Types/BIGINT) for SELECT 1"
    (is (= [Types/BIGINT] (meta-types "SELECT 1")))))

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
