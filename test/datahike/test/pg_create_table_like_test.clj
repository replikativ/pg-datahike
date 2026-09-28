(ns datahike.test.pg-create-table-like-test
  "`CREATE TABLE t (LIKE source …)` — a table that starts as a copy of
   another's column definitions.

   It copies DEFINITIONS, not rows, and not the source's later changes:
   after the copy the two tables are unrelated, which is the whole
   difference from INHERITS.

   The copied definitions are read back out of the catalog and spliced
   into the statement, which then goes through the ordinary CREATE
   TABLE path. So the test that matters is that a copied column is
   indistinguishable from a written-out one -- same type spelling,
   same NOT NULL, same default -- because anything the catalog cannot
   spell would come back as something else.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0
             :schema-flexibility :write :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec ^PgWireServer$QueryResult [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (mapv vec (.rows r))))

(defn- err [^PgWireServer$QueryResult r] (some-> (.error r) str))

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (err r)))
    r))

(defn- release! [{:keys [conn cfg]}] (d/release conn) (d/delete-database cfg))

(defmacro with-h [[sym init] & body]
  `(let [~sym ~init] (try ~@body (finally (release! ~sym)))))

(defn- described
  "[[column, type, not-null] …] for a table, as the catalog reports it."
  [h table]
  (rows (ok! h (str "SELECT a.attname, format_type(a.atttypid, a.atttypmod),"
                    " a.attnotnull"
                    " FROM pg_attribute a JOIN pg_class c ON c.oid = a.attrelid"
                    " WHERE c.relname = '" table "'"
                    "   AND a.attnum > 0 AND NOT a.attisdropped"
                    " ORDER BY a.attnum"))))

(deftest a-copy-is-indistinguishable-from-the-original
  (with-h [h (fresh-handler)]
    (ok! h (str "CREATE TABLE src (a int NOT NULL, b varchar(10), c numeric(5,2),"
                " d text[], e timestamp with time zone, f date, g boolean)"))
    (ok! h "CREATE TABLE dst (LIKE src)")
    (is (= (described h "src") (described h "dst")))))

(deftest it-copies-definitions-not-rows
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE src (a int, b text)")
    (ok! h "INSERT INTO src VALUES (1,'x')")
    (ok! h "CREATE TABLE dst (LIKE src)")
    (is (= [["0"]] (rows (ok! h "SELECT count(*) FROM dst"))))
    (testing "and the two are unrelated afterwards"
      (ok! h "INSERT INTO dst VALUES (9,'z')")
      (is (= [["1"]] (rows (ok! h "SELECT count(*) FROM src"))))
      (is (= [["1"]] (rows (ok! h "SELECT count(*) FROM dst")))))))

(deftest defaults-come-only-with-including-defaults
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE src (a int DEFAULT 4, b text DEFAULT 'hi')")
    (ok! h "CREATE TABLE plain (LIKE src)")
    (ok! h "CREATE TABLE withdef (LIKE src INCLUDING DEFAULTS)")
    (ok! h "INSERT INTO plain (b) VALUES ('q')")
    (ok! h "INSERT INTO withdef (b) VALUES ('q')")
    (is (= [[nil "q"]] (rows (ok! h "SELECT a, b FROM plain"))))
    (is (= [["4" "q"]] (rows (ok! h "SELECT a, b FROM withdef"))))
    (testing "EXCLUDING DEFAULTS restates the default behaviour"
      (ok! h "CREATE TABLE excl (LIKE src EXCLUDING DEFAULTS)")
      (ok! h "INSERT INTO excl (b) VALUES ('q')")
      (is (= [[nil "q"]] (rows (ok! h "SELECT a, b FROM excl")))))))

(deftest it-sits-among-ordinary-column-definitions
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE src (a int, b text)")
    (ok! h "CREATE TABLE dst (x int, LIKE src, y text)")
    (is (= ["x" "a" "b" "y"] (mapv first (described h "dst"))))))

(deftest a-missing-source-is-an-error-not-an-empty-table
  (with-h [h (fresh-handler)]
    (let [r (exec h "CREATE TABLE dst (LIKE nosuch)")]
      (is (= "relation \"nosuch\" does not exist" (err r)))
      (is (= "42P01" (.sqlstate r))))
    (is (= [["0"]] (rows (ok! h "SELECT count(*) FROM pg_class WHERE relname = 'dst'"))))))

(deftest an-option-that-cannot-be-honoured-is-refused
  ;; INCLUDING CONSTRAINTS / INDEXES / ALL name things that are not
  ;; copied here. Creating the table anyway would leave it silently
  ;; missing what the statement asked for.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE src (a int PRIMARY KEY)")
    (is (some? (err (exec h "CREATE TABLE d1 (LIKE src INCLUDING ALL)"))))
    (is (some? (err (exec h "CREATE TABLE d2 (LIKE src INCLUDING CONSTRAINTS)"))))
    (is (some? (err (exec h "CREATE TABLE d3 (LIKE src INCLUDING INDEXES)"))))))

(deftest the-word-like-elsewhere-is-still-the-operator
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE src (a int, b text)")
    (ok! h "INSERT INTO src VALUES (1,'xy'),(2,'zz')")
    (ok! h "CREATE TABLE dst AS SELECT * FROM src WHERE b LIKE 'x%'")
    (is (= [["1" "xy"]] (rows (ok! h "SELECT * FROM dst"))))))

(deftest the-sql-standard-type-spellings-survive
  ;; `timestamptz` resolved and `timestamp with time zone` did not, so
  ;; the second spelling silently stored a text column -- which only
  ;; showed once LIKE started copying types back out of the catalog.
  (with-h [h (fresh-handler)]
    (ok! h (str "CREATE TABLE t (a timestamp with time zone,"
                " b timestamp without time zone, c time with time zone,"
                " d time without time zone, e interval, f money)"))
    (is (= [["a" "timestamp with time zone" "f"]
            ["b" "timestamp without time zone" "f"]
            ["c" "time with time zone" "f"]
            ["d" "time without time zone" "f"]
            ["e" "interval" "f"]
            ["f" "money" "f"]]
           (described h "t")))))
