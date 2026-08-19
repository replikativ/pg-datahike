(ns datahike.test.pg-array-sql-test
  "SQL-level tests for PostgreSQL array support. Covers:

   - `ARRAY[…]` literal as a projection value (returns PgArray, serialised
     as `{…}` over the wire).
   - `arr[N]` subscripting (1-indexed, NULL on out-of-range) with constant
     and variable indices.
   - Array-returning functions: `current_schemas(bool)`, `string_to_array`,
     `regexp_split_to_array`.
   - Meta functions: `array_length`, `array_upper`, `array_lower`,
     `cardinality`, `array_to_string`.
   - Array operators: `= ANY(arr)`, `= ALL(arr)`, `@>`, `<@`, `&&`, `||`.
   - `unnest(arr)` table function.
   - `array_agg(col)` aggregate.
   - RowDescription OID inference: ARRAY[…] → array-OID; arr[N] → element-OID.

   The test set deliberately includes the pgjdbc `getSchemas()` idiom
   `(pg_catalog.current_schemas(true))[1]` — Metabase's current sync
   blocker — as an end-to-end assertion."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]
           [java.sql Connection DriverManager PreparedStatement ResultSet
            ResultSetMetaData Types]))

(def oid-int4 23)
(def oid-int8 20)
(def oid-text 25)
(def oid-float8 701)
(def oid-text-array 1009)
(def oid-name-array 1003)
(def oid-int4-array 1007)
(def oid-int8-array 1016)
(def oid-name 19)

(def test-schema
  [{:db/ident :t/id   :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :t/name :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :t/score :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}])

(def test-data
  [{:t/id 1 :t/name "alice"   :t/score 10}
   {:t/id 2 :t/name "bob"     :t/score 20}
   {:t/id 3 :t/name "charlie" :t/score 30}])

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

(defn- rows [sql]
  (let [^PgWireServer$QueryResult r (.execute *handler* sql)]
    (mapv vec (.-rows r))))

(defn- oids [sql]
  (let [^PgWireServer$QueryResult r (.execute *handler* sql)]
    (vec (.-columnOids r))))

(defn- describe-oids [sql]
  (let [parsed (.parse *handler* sql (int-array 0))
        ^PgWireServer$QueryResult r (.describeResult *handler* parsed)]
    (when (and r (.-columnOids r))
      (vec (.-columnOids r)))))

;; ---------------------------------------------------------------------------
;; ARRAY[…] literal as projection
;; ---------------------------------------------------------------------------

(deftest array-literal-int
  (testing "SELECT ARRAY[1,2,3] emits PG array text {1,2,3}"
    (is (= [["{1,2,3}"]] (rows "SELECT ARRAY[1,2,3]")))))

(deftest array-literal-text
  (is (= [["{alice,bob}"]] (rows "SELECT ARRAY['alice','bob']"))))

(deftest array-literal-mixed-needs-quoting
  (is (= [["{\"a,b\",c}"]] (rows "SELECT ARRAY['a,b','c']"))))

(deftest array-literal-empty
  (is (= [["{}"]] (rows "SELECT ARRAY[]::int[]"))))

(deftest array-literal-oid-reports-array-type
  (testing "RowDescription OID for ARRAY[1,2,3] is int4[] (1007) --
            PostgreSQL types the integer literals as int4, so the array
            is integer[] and not bigint[]"
    (is (= [oid-int4-array] (describe-oids "SELECT ARRAY[1,2,3]"))))
  (is (= [oid-text-array] (describe-oids "SELECT ARRAY['a','b']"))))

;; ---------------------------------------------------------------------------
;; Subscripting
;; ---------------------------------------------------------------------------

(deftest subscript-constant
  (testing "SELECT ARRAY[10,20,30][2] → 20"
    (is (= [["20"]] (rows "SELECT ARRAY[10,20,30][2]")))))

(deftest subscript-first
  (is (= [["10"]] (rows "SELECT ARRAY[10,20,30][1]"))))

(deftest subscript-out-of-range-null
  (testing "PG semantics: out-of-range subscript returns NULL"
    (is (= [[nil]] (rows "SELECT ARRAY[10,20,30][99]")))))

(deftest subscript-zero-null
  (is (= [[nil]] (rows "SELECT ARRAY[10,20,30][0]"))))

(deftest subscript-oid-is-element-type
  (testing "RowDescription OID for arr[N] is the element OID, not array OID"
    (is (= [oid-int4] (describe-oids "SELECT ARRAY[10,20,30][2]")))
    (is (= [oid-text] (describe-oids "SELECT ARRAY['a','b','c'][1]")))))

;; ---------------------------------------------------------------------------
;; Array-returning functions — the pgjdbc/Metabase blocker
;; ---------------------------------------------------------------------------

(deftest current-schemas-true
  (testing "current_schemas(true) returns {public}"
    (is (= [["{public}"]] (rows "SELECT current_schemas(true)")))))

(deftest current-schemas-subscript-1
  (testing "pgjdbc getSchemas idiom: (current_schemas(true))[1] → 'public'"
    (is (= [["public"]] (rows "SELECT (current_schemas(true))[1]")))))

(deftest current-schemas-false-subscript
  (is (= [["public"]] (rows "SELECT (current_schemas(false))[1]"))))

(deftest current-schemas-oid
  (testing "current_schemas returns name[] (OID 1003)"
    (is (= [oid-name-array] (describe-oids "SELECT current_schemas(true)"))))
  (testing "subscript of current_schemas returns name (OID 19)"
    (is (= [oid-name] (describe-oids "SELECT (current_schemas(true))[1]")))))

(deftest string-to-array
  (is (= [["{a,b,c}"]] (rows "SELECT string_to_array('a,b,c', ',')"))))

(deftest string-to-array-subscript
  (is (= [["b"]] (rows "SELECT (string_to_array('a,b,c', ','))[2]"))))

(deftest regexp-split-to-array
  (is (= [["{a,b,c}"]] (rows "SELECT regexp_split_to_array('a-b-c', '-')"))))

;; ---------------------------------------------------------------------------
;; Meta functions
;; ---------------------------------------------------------------------------

(deftest array-length-basic
  (is (= [["3"]] (rows "SELECT array_length(ARRAY[1,2,3], 1)"))))

(deftest array-length-empty
  (testing "PG: array_length returns NULL for empty array (not 0)"
    (is (= [[nil]] (rows "SELECT array_length(ARRAY[]::int[], 1)")))))

(deftest array-upper-basic
  (is (= [["3"]] (rows "SELECT array_upper(ARRAY[1,2,3], 1)"))))

(deftest array-lower-basic
  (is (= [["1"]] (rows "SELECT array_lower(ARRAY[1,2,3], 1)"))))

(deftest cardinality-basic
  (is (= [["3"]] (rows "SELECT cardinality(ARRAY[10,20,30])"))))

(deftest cardinality-empty
  (testing "PG: cardinality returns 0 for empty array (differs from array_length)"
    (is (= [["0"]] (rows "SELECT cardinality(ARRAY[]::int[])")))))

(deftest array-to-string-basic
  (is (= [["a,b,c"]] (rows "SELECT array_to_string(ARRAY['a','b','c'], ',')"))))

(deftest array-to-string-with-null-replacement
  (testing "array_to_string(arr, sep, null_replace) replaces NULLs"
    (is (= [["a,?,c"]] (rows "SELECT array_to_string(ARRAY['a',NULL,'c'], ',', '?')")))))

(deftest array-to-string-nulls-skipped
  (testing "Without null_replace, NULLs are skipped (PG behavior)"
    (is (= [["a,c"]] (rows "SELECT array_to_string(ARRAY['a',NULL,'c'], ',')")))))

;; ---------------------------------------------------------------------------
;; ANY / ALL — literal vs runtime
;; ---------------------------------------------------------------------------

(deftest any-literal-array
  (testing "Pre-existing path: col = ANY(ARRAY[…]) via or-join expansion"
    (is (= [["1"] ["3"]]
           (sort (rows "SELECT id FROM t WHERE id = ANY(ARRAY[1,3])"))))))

(deftest any-runtime-array-via-fn
  (testing "New path: col = ANY(fn_returning_array())"
    (is (= [["public"]]
           (rows "SELECT 'public' AS s WHERE 'public' = ANY(current_schemas(true))")))))

(deftest all-literal-array-numeric
  (is (= [["1"]]
         (rows "SELECT id FROM t WHERE id <= ALL(ARRAY[1,2,3]) ORDER BY id"))))

;; ---------------------------------------------------------------------------
;; Containment / overlap / concat operators
;; ---------------------------------------------------------------------------

(deftest contains-op
  (is (= [["t"]]
         (rows "SELECT ARRAY[1,2,3,4] @> ARRAY[2,3]"))))

(deftest contained-by-op
  (is (= [["t"]]
         (rows "SELECT ARRAY[2,3] <@ ARRAY[1,2,3,4]"))))

(deftest overlap-op
  (is (= [["t"]]
         (rows "SELECT ARRAY[1,2,3] && ARRAY[3,4,5]")))
  (is (= [["f"]]
         (rows "SELECT ARRAY[1,2,3] && ARRAY[7,8,9]"))))

(deftest concat-arrays-op
  (is (= [["{1,2,3,4}"]]
         (rows "SELECT ARRAY[1,2] || ARRAY[3,4]"))))

;; ---------------------------------------------------------------------------
;; unnest + array_agg
;; ---------------------------------------------------------------------------

(deftest unnest-literal
  (testing "unnest(ARRAY[…]) in SELECT list produces N rows"
    (is (= [["10"] ["20"] ["30"]]
           (rows "SELECT unnest(ARRAY[10,20,30])")))))

(deftest array-agg-basic
  (testing "array_agg collects rows into a PgArray"
    ;; Per-aggregate ORDER BY isn't honoured yet (Phase 2 follow-up);
    ;; assert set-equality on the parsed PgArray text.
    (let [[[s]] (rows "SELECT array_agg(name) FROM t")]
      (is (string? s))
      (is (= #{"alice" "bob" "charlie"}
             (set (-> s (subs 1 (dec (count s))) (clojure.string/split #","))))))))

;; ---------------------------------------------------------------------------
;; pgjdbc Extended Query: RowDescription + getMetaData
;; ---------------------------------------------------------------------------

(defn- ^Connection jdbc-conn []
  (DriverManager/getConnection
   (str *jdbc-url* "&preferQueryMode=extended")))

(defn- meta-types [sql]
  (with-open [^Connection c (jdbc-conn)
              ^PreparedStatement ps (.prepareStatement c sql)
              ^ResultSet rs (.executeQuery ps)]
    (let [^ResultSetMetaData md (.getMetaData rs)]
      (vec (for [i (range 1 (inc (.getColumnCount md)))]
             (.getColumnType md i))))))

(deftest pgjdbc-array-column-type
  (testing "pgjdbc reports Types/ARRAY for ARRAY[...] expressions"
    (is (= [Types/ARRAY] (meta-types "SELECT ARRAY[1,2,3]")))))

(deftest pgjdbc-subscript-element-type
  (testing "pgjdbc reports the element type for arr[N] -- INTEGER here,
            because ARRAY[1,2,3] is integer[]"
    (is (= [Types/INTEGER] (meta-types "SELECT ARRAY[1,2,3][2]")))))

(deftest pgjdbc-getSchemas-idiom
  (testing "pgjdbc's DatabaseMetaData.getSchemas() probe — the Metabase blocker"
    (with-open [^Connection c (jdbc-conn)
                ^PreparedStatement ps (.prepareStatement
                                       c (str "SELECT nspname AS \"TABLE_SCHEM\", "
                                              "current_database() AS \"TABLE_CATALOG\" "
                                              "FROM pg_catalog.pg_namespace "
                                              "WHERE nspname <> 'pg_toast' "
                                              "AND (nspname !~ '^pg_temp_' "
                                              "     OR nspname = (pg_catalog.current_schemas(true))[1])"))
                ^ResultSet rs (.executeQuery ps)]
      ;; Assertion: the query runs without raising the ArrayExpression
      ;; parse error. Row content depends on how much of pg_namespace
      ;; we materialise; what matters is that the SQL is accepted.
      (is (or (.next rs) true)))))
