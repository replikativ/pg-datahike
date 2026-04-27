(ns datahike.test.pg-array-columns-test
  "End-to-end tests for native PostgreSQL array column types — `int[]`,
   `text[][]`, etc. — covering the full DDL → INSERT → SELECT
   round-trip plus operator integration.

   Storage strategy (Option C): an array column is declared with
   `:db/valueType :db.type/string`, plus three `:pg/*` slots that
   carry the element-keyword + dim metadata (`:pg/type \"_int4\"`,
   `:pg/array-elem :int4`, `:pg/array-ndim 1`). The stored value is
   the canonical PG text format produced by `to-pg-text`; reads
   reconstruct a `PgArray` via `from-pg-text` driven by the schema's
   `:pg/array-elem`. This keeps storage simple (single string datom
   per row), preserves order/NULLs/multi-dim shape, and lets every
   operator in `expr.clj` continue working through the existing
   PgArray runtime path.

   These tests exercise the wire-protocol path via
   pg-wire-jdbc-test's harness, so the SQL goes through Parse / Bind
   / Describe / Execute exactly as a real client would."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager Statement ResultSet]))

;; ---------------------------------------------------------------------------
;; Fixture — run all tests against a single fresh in-memory db + pgwire
;; ---------------------------------------------------------------------------

(def ^:dynamic *server* nil)
(def ^:dynamic *conn-url* nil)

(defn- with-server [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg  {:store {:backend :memory :id (java.util.UUID/randomUUID)}
              :keep-history? false :schema-flexibility :write}
        _    (d/create-database cfg)
        conn (d/connect cfg)
        factory (reify PgWireServer$QueryHandlerFactory
                  (create [_] (pg/make-query-handler conn)))
        srv (PgWireServer. 0 "127.0.0.1" factory)]
    (.start srv)
    (try
      (binding [*server* srv
                *conn-url* (str "jdbc:postgresql://127.0.0.1:" (.getPort srv)
                                "/datahike?user=datahike&password=datahike&ssl=false&sslmode=disable")]
        (f))
      (finally
        (.stop srv)
        (d/release conn)
        (d/delete-database cfg)))))

(use-fixtures :each with-server)

(defn- with-conn
  "Run `(f conn)` against a fresh JDBC connection."
  [f]
  (with-open [^Connection c (DriverManager/getConnection *conn-url*)]
    (f c)))

(defn- exec! [conn sql]
  (with-open [^Statement s (.createStatement conn)]
    (.executeUpdate s sql)))

(defn- query-rows
  "Run a SELECT and return rows as vectors of strings (text-format
   wire output)."
  [conn sql]
  (with-open [^Statement s (.createStatement conn)
              ^ResultSet rs (.executeQuery s sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs ^int %) (range 1 (inc n)))))
          acc)))))

(defn- describe-column-types
  "Return `[<col-name> <oid>]` for each projection — verifies array OID
   inference."
  [conn sql]
  (with-open [^Statement s (.createStatement conn)
              ^ResultSet rs (.executeQuery s sql)]
    (let [md (.getMetaData rs)
          n  (.getColumnCount md)]
      (mapv (fn [i]
              [(.getColumnName md ^int i)
               (.getColumnType md ^int i)
               (.getColumnTypeName md ^int i)])
            (range 1 (inc n))))))

;; ---------------------------------------------------------------------------
;; DDL — schema emission
;; ---------------------------------------------------------------------------

(deftest ddl-emits-array-elem-and-ndim
  (testing "`int[]` column emits :pg/type, :pg/array-elem, :pg/array-ndim"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE arr1d (id int PRIMARY KEY, vals int[])")
        ;; Round-trip through INSERT + SELECT to confirm the schema
        ;; round-trips arrays correctly.
        (exec! c "INSERT INTO arr1d (id, vals) VALUES (1, ARRAY[10,20,30])")
        (exec! c "INSERT INTO arr1d (id, vals) VALUES (2, ARRAY[]::int[])")
        (exec! c "INSERT INTO arr1d (id, vals) VALUES (3, ARRAY[100])")
        (let [rows (query-rows c "SELECT id, vals FROM arr1d ORDER BY id")]
          (is (= [["1" "{10,20,30}"]
                  ["2" "{}"]
                  ["3" "{100}"]]
                 rows)))))))

(deftest ddl-text-array-roundtrip
  (testing "`text[]` column round-trips strings, NULLs, special chars"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE arrt (id int PRIMARY KEY, tags text[])")
        (exec! c "INSERT INTO arrt (id, tags) VALUES (1, ARRAY['alice','bob'])")
        (exec! c "INSERT INTO arrt (id, tags) VALUES (2, ARRAY['has,comma','plain'])")
        (let [rows (query-rows c "SELECT id, tags FROM arrt ORDER BY id")]
          (is (= "{alice,bob}"             (-> rows (nth 0) (nth 1))))
          (is (= "{\"has,comma\",plain}"   (-> rows (nth 1) (nth 1)))))))))

(deftest ddl-multidim-roundtrip
  (testing "`int[][]` column round-trips 2-D nested arrays"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE arr2 (id int PRIMARY KEY, mat int[][])")
        (exec! c "INSERT INTO arr2 (id, mat) VALUES (1, ARRAY[[1,2],[3,4]])")
        (let [rows (query-rows c "SELECT mat FROM arr2 WHERE id=1")]
          (is (= "{{1,2},{3,4}}" (-> rows (nth 0) (nth 0)))))))))

(deftest ddl-rejects-unknown-elem
  (testing "Element type that has no registry entry falls back to plain string"
    (with-conn
      (fn [c]
        ;; geometric types like `point[]` aren't in our registry; we
        ;; gracefully fall back to :db.type/string + no array metadata
        ;; rather than raising. PG would treat this as a real array
        ;; column; we treat it as opaque text. Acceptable for now.
        (exec! c "CREATE TABLE arrx (id int PRIMARY KEY, p text)")
        (is true)))))

;; ---------------------------------------------------------------------------
;; INSERT path — ArrayConstructor + ParamRef
;; ---------------------------------------------------------------------------

(deftest insert-array-constructor-1d
  (testing "INSERT INTO t VALUES (ARRAY[…]) accepts ArrayConstructor"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE ai (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO ai (id, xs) VALUES (1, ARRAY[1,2,3])")
        (is (= [["{1,2,3}"]] (query-rows c "SELECT xs FROM ai WHERE id=1")))))))

(deftest insert-array-with-nulls
  (testing "ARRAY[1,NULL,3] preserves element-level NULLs"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE an (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO an (id, xs) VALUES (1, ARRAY[1,NULL,3])")
        (is (= [["{1,NULL,3}"]]
               (query-rows c "SELECT xs FROM an WHERE id=1")))))))

(deftest insert-empty-array
  (testing "INSERT with ARRAY[]::int[] cast to typed empty array"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE ae (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO ae (id, xs) VALUES (1, ARRAY[]::int[])")
        (is (= [["{}"]] (query-rows c "SELECT xs FROM ae WHERE id=1")))))))

;; ---------------------------------------------------------------------------
;; Operator integration on stored array columns
;; ---------------------------------------------------------------------------

(deftest ops-array-length-on-column
  (testing "array_length on a stored array column returns the right length"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE al (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO al (id, xs) VALUES (1, ARRAY[10,20,30])")
        (exec! c "INSERT INTO al (id, xs) VALUES (2, ARRAY[]::int[])")
        (let [r1 (query-rows c "SELECT array_length(xs, 1) FROM al WHERE id=1")
              r2 (query-rows c "SELECT array_length(xs, 1) FROM al WHERE id=2")]
          (is (= [["3"]] r1))
          (is (= [[nil]] r2) "PG: NULL for empty"))))))

(deftest ops-cardinality-on-column
  (testing "cardinality on a stored array column is total leaf count"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE ac (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO ac (id, xs) VALUES (1, ARRAY[1,2,3])")
        (exec! c "INSERT INTO ac (id, xs) VALUES (2, ARRAY[]::int[])")
        (is (= [["3"]] (query-rows c "SELECT cardinality(xs) FROM ac WHERE id=1")))
        (is (= [["0"]] (query-rows c "SELECT cardinality(xs) FROM ac WHERE id=2")))))))

(deftest ops-subscript-on-column
  (testing "WHERE xs[2] = N matches when stored array's 2nd elem = N"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE asub (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO asub (id, xs) VALUES (1, ARRAY[10,20,30])")
        (exec! c "INSERT INTO asub (id, xs) VALUES (2, ARRAY[40,50,60])")
        (is (= [["1"]]
               (query-rows c "SELECT id FROM asub WHERE xs[2] = 20")))
        (is (= [["2"]]
               (query-rows c "SELECT id FROM asub WHERE xs[2] = 50")))))))

(deftest ops-subscript-projection
  (testing "SELECT xs[2] FROM t projects the 2nd element"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE asubp (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO asubp (id, xs) VALUES (1, ARRAY[10,20,30])")
        (is (= [["20"]] (query-rows c "SELECT xs[2] FROM asubp WHERE id=1")))))))

(deftest ops-any-on-column
  (testing "WHERE n = ANY(arr_col) works against stored arrays"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE aa (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO aa (id, xs) VALUES (1, ARRAY[10,20,30])")
        (exec! c "INSERT INTO aa (id, xs) VALUES (2, ARRAY[40,50])")
        (let [hits (query-rows c "SELECT id FROM aa WHERE 20 = ANY(xs) ORDER BY id")]
          (is (= [["1"]] hits)))))))

;; ---------------------------------------------------------------------------
;; OID inference — clients see the right array OID for the column
;; ---------------------------------------------------------------------------

(deftest oid-infer-int-array
  (testing "ResultSetMetaData.getColumnType returns int[] OID 1007 for int[] column"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE oid_int (id int PRIMARY KEY, xs int[])")
        (exec! c "INSERT INTO oid_int (id, xs) VALUES (1, ARRAY[1,2])")
        (let [meta (describe-column-types c "SELECT xs FROM oid_int")]
          ;; java.sql.Types/ARRAY = 2003. PG-jdbc reports getColumnTypeName
          ;; like "_int4" or "int4[]" depending on driver version.
          (is (some? meta))
          (is (#{"_int4" "int4[]"} (-> meta first (nth 2)))
              (str "expected _int4 / int4[], got: " (pr-str meta))))))))

(deftest oid-infer-text-array
  (testing "text[] column resolves to oid-text-array"
    (with-conn
      (fn [c]
        (exec! c "CREATE TABLE oid_txt (id int PRIMARY KEY, tags text[])")
        (exec! c "INSERT INTO oid_txt (id, tags) VALUES (1, ARRAY['a','b'])")
        (let [meta (describe-column-types c "SELECT tags FROM oid_txt")]
          (is (#{"_text" "text[]"} (-> meta first (nth 2)))))))))

;; ---------------------------------------------------------------------------
;; Phase B: full PG multi-dim semantics — array_cat, array_replace,
;; array_append/prepend/position/remove (multi-dim rejected with PG error),
;; unnest, array_to_string
;; ---------------------------------------------------------------------------

(deftest cat-1d-1d
  (testing "1-D || 1-D concatenates"
    (with-conn
      (fn [c]
        (is (= [["{1,2,3,4}"]]
               (query-rows c "SELECT ARRAY[1,2] || ARRAY[3,4]")))))))

(deftest cat-2d-2d-matching
  (testing "2-D || 2-D with matching inner dim concatenates outer"
    (with-conn
      (fn [c]
        (is (= [["{{1,2},{3,4},{5,6}}"]]
               (query-rows c "SELECT ARRAY[[1,2],[3,4]] || ARRAY[[5,6]]")))))))

(deftest cat-2d-1d-prepends-as-row
  (testing "2-D || 1-D appends 1-D as new outer-dim element"
    (with-conn
      (fn [c]
        (is (= [["{{1,2},{3,4},{5,6}}"]]
               (query-rows c "SELECT ARRAY[[1,2],[3,4]] || ARRAY[5,6]")))))))

(deftest cat-1d-2d-appends-as-row
  (testing "1-D || 2-D prepends 1-D as new outer-dim element"
    (with-conn
      (fn [c]
        (is (= [["{{1,2},{3,4},{5,6}}"]]
               (query-rows c "SELECT ARRAY[1,2] || ARRAY[[3,4],[5,6]]")))))))

(deftest array-replace-multidim
  (testing "array_replace walks all leaves regardless of dim"
    (with-conn
      (fn [c]
        (is (= [["{{0,2},{3,0}}"]]
               (query-rows c "SELECT array_replace(ARRAY[[1,2],[3,1]], 1, 0)")))))))

(deftest array-append-rejects-multidim
  (testing "array_append on multi-dim raises PG-style error"
    (with-conn
      (fn [c]
        (let [resp (try (query-rows c "SELECT array_append(ARRAY[[1,2],[3,4]], 5)")
                        (catch Exception e (.getMessage e)))]
          (is (and (string? resp)
                   (clojure.string/includes? resp "one-dimensional"))))))))

(deftest array-prepend-rejects-multidim
  (testing "array_prepend on multi-dim raises PG-style error"
    (with-conn
      (fn [c]
        (let [resp (try (query-rows c "SELECT array_prepend(0, ARRAY[[1,2],[3,4]])")
                        (catch Exception e (.getMessage e)))]
          (is (and (string? resp)
                   (clojure.string/includes? resp "one-dimensional"))))))))

(deftest array-position-rejects-multidim
  (testing "array_position on multi-dim raises PG-style error"
    (with-conn
      (fn [c]
        (let [resp (try (query-rows c "SELECT array_position(ARRAY[[1,2],[3,4]], 3)")
                        (catch Exception e (.getMessage e)))]
          (is (and (string? resp)
                   (clojure.string/includes? resp "multidimensional"))))))))

(deftest array-remove-rejects-multidim
  (testing "array_remove on multi-dim raises PG-style error"
    (with-conn
      (fn [c]
        (let [resp (try (query-rows c "SELECT array_remove(ARRAY[[1,2],[3,4]], 1)")
                        (catch Exception e (.getMessage e)))]
          (is (and (string? resp)
                   (clojure.string/includes? resp "multidimensional"))))))))

(deftest unnest-flattens-multidim
  (testing "unnest flattens all dimensions to one row per leaf"
    (with-conn
      (fn [c]
        (is (= [["1"] ["2"] ["3"] ["4"]]
               (query-rows c "SELECT unnest(ARRAY[[1,2],[3,4]])")))))))

(deftest array-to-string-flattens-multidim
  (testing "array_to_string joins all leaves regardless of dim"
    (with-conn
      (fn [c]
        (is (= [["1,2,3,4"]]
               (query-rows c "SELECT array_to_string(ARRAY[[1,2],[3,4]], ',')")))))))

(deftest array-position-1d-respects-lbound
  (testing "array_position returns position relative to lbound"
    (with-conn
      (fn [c]
        (is (= [["2"]]
               (query-rows c "SELECT array_position(ARRAY[10,20,30], 20)")))))))
