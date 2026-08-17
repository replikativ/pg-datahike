(ns datahike.test.pg-jsonb-write-paths-test
  "jsonb canonicalization has to happen on EVERY write path.

   PostgreSQL's `jsonb` is a parsed tree: key order is not preserved,
   duplicate keys collapse last-wins, whitespace is gone. We reproduce
   that by canonicalizing the text on the way in — but only where the
   column is known to be jsonb, and that knowledge lives in `:pg/type`
   on the ident entity rather than in Datahike's `:schema` map.

   `coerce-insert-value` read it with `(get-in schema [attr :pg/type])`,
   which answers nil unless the caller happened to enrich the schema
   first. Exactly one caller did. So canonicalization fired for a
   simple-protocol INSERT with an inline literal, and silently did not
   fire for:

     - a prepared INSERT (the value arrives as a ParamRef and is
       re-coerced at Execute against the raw schema),
     - any UPDATE.

   Which is to say: it fired for `psql`, and not for JDBC `setString`,
   asyncpg, psycopg, node-postgres or any ORM. The existing
   canonicalization test used a `Statement`, which is why this went
   unnoticed.

   A missing `:pg/type` now falls back to the ident-entity query rather
   than reading as \"not jsonb\".

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer PgWireServer$QueryResult
            PgWireServer$QueryHandlerFactory]
           [java.sql Connection DriverManager]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (binding [*handler* (pg/make-query-handler conn)] (f))
           (finally (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- run [sql] (.execute *handler* sql))
(defn- rows [sql] (mapv vec (.-rows ^PgWireServer$QueryResult (run sql))))
(defn- v [sql] (ffirst (rows sql)))
(defn- state [sql]
  (try (.-sqlstate ^PgWireServer$QueryResult (run sql))
       (catch Exception e (:sqlstate (ex-data e)))))
(defn- err [sql]
  (try (.-error ^PgWireServer$QueryResult (run sql))
       (catch Exception e (ex-message e))))

;; The canonical form asserted here is PostgreSQL's own: keys ordered
;; length-first then bytewise, `": "` after a key and `", "` between
;; pairs. What this suite is really pinning is that EVERY write path
;; produces it, whatever it is.

(deftest canonicalizes-on-the-literal-insert-path
  (run "CREATE TABLE j1 (id int PRIMARY KEY, p jsonb)")
  (run "INSERT INTO j1 VALUES (1, '{\"b\":2,\"a\":1}')")
  (is (= "{\"a\": 1, \"b\": 2}" (v "SELECT p FROM j1 WHERE id = 1"))))

(deftest canonicalizes-on-the-update-path
  (testing "UPDATE stored the text verbatim — the tx builder coerced
            against the raw schema, where :pg/type is invisible"
    (run "CREATE TABLE j2 (id int PRIMARY KEY, p jsonb)")
    (run "INSERT INTO j2 VALUES (1, '{\"a\":1}')")
    (run "UPDATE j2 SET p = '{\"d\":4,\"c\":3}' WHERE id = 1")
    (is (= "{\"c\": 3, \"d\": 4}" (v "SELECT p FROM j2 WHERE id = 1"))))
  (testing "and duplicate keys still collapse last-wins on UPDATE"
    (run "UPDATE j2 SET p = '{\"a\":1,\"a\":9}' WHERE id = 1")
    (is (= "{\"a\": 9}" (v "SELECT p FROM j2 WHERE id = 1")))))

(deftest canonicalizes-on-the-prepared-insert-path
  (testing "a parameterised INSERT defers coercion to Execute, which ran
            against the raw schema"
    (run "CREATE TABLE j3 (id int PRIMARY KEY, p jsonb)")
    (run "PREPARE ins AS INSERT INTO j3 VALUES ($1, $2)")
    (run "EXECUTE ins (1, '{\"b\":2,\"a\":1}')")
    (is (= "{\"a\": 1, \"b\": 2}" (v "SELECT p FROM j3 WHERE id = 1")))))

(deftest json-is-never-canonicalized-on-any-path
  (testing "PG `json` is the text-faithful type: whitespace, key order
            and duplicate keys all survive, on every path"
    (run "CREATE TABLE j4 (id int PRIMARY KEY, p json)")
    (run "INSERT INTO j4 VALUES (1, '{ \"b\":1,  \"a\":2, \"a\":3 }')")
    (is (= "{ \"b\":1,  \"a\":2, \"a\":3 }" (v "SELECT p FROM j4 WHERE id = 1")))
    (run "UPDATE j4 SET p = '{ \"z\" : 9 }' WHERE id = 1")
    (is (= "{ \"z\" : 9 }" (v "SELECT p FROM j4 WHERE id = 1"))
        "UPDATE must not canonicalize a json column either")))

(deftest non-json-columns-are-unaffected
  (run "CREATE TABLE j5 (id int PRIMARY KEY, t text, n int)")
  (run "INSERT INTO j5 VALUES (1, '{\"b\":2,\"a\":1}', 5)")
  (is (= "{\"b\":2,\"a\":1}" (v "SELECT t FROM j5 WHERE id = 1"))
      "a text column holding JSON-looking text is left alone")
  (run "UPDATE j5 SET t = '{\"z\":1,\"y\":2}' WHERE id = 1")
  (is (= "{\"z\":1,\"y\":2}" (v "SELECT t FROM j5 WHERE id = 1"))))

;; ---------------------------------------------------------------------------
;; Over the wire. `PREPARE`/`EXECUTE` above is the SIMPLE protocol and takes a
;; different route, so it does not reach the Execute-time re-coercion where the
;; hole was — only a real driver's extended-protocol Bind does.
;; ---------------------------------------------------------------------------

(deftest canonicalizes-through-a-real-driver
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          factory (reify PgWireServer$QueryHandlerFactory
                    (create [_] (pg/make-query-handler conn)))
          server (PgWireServer. 0 "127.0.0.1" factory)]
      (.start server)
      (try
        (with-open [c (DriverManager/getConnection
                       (str "jdbc:postgresql://127.0.0.1:" (.getPort server)
                            "/datahike?user=datahike&password=x"
                            "&sslmode=disable&binaryTransfer=false"))]
          (with-open [st (.createStatement c)]
            (.execute st "CREATE TABLE jw (id int PRIMARY KEY, p jsonb)"))
          (testing "parameterised INSERT — the value arrives as a ParamRef and
                    is re-coerced at Execute, which used to see no :pg/type"
            (with-open [ps (.prepareStatement c "INSERT INTO jw VALUES (?, ?)")]
              (.setInt ps 1 1)
              (.setString ps 2 "{\"b\":2,\"a\":1}")
              (.execute ps)))
          (testing "UPDATE through the driver"
            (with-open [st (.createStatement c)]
              (.execute st "UPDATE jw SET p = '{\"d\":4,\"c\":3}' WHERE id = 1")))
          (with-open [st (.createStatement c)
                      rs (.executeQuery st "SELECT p FROM jw WHERE id = 1")]
            (.next rs)
            (is (= "{\"c\": 3, \"d\": 4}" (.getString rs 1))))
          (testing "and a parameterised INSERT that is never updated"
            (with-open [ps (.prepareStatement c "INSERT INTO jw VALUES (?, ?)")]
              (.setInt ps 1 2)
              (.setString ps 2 "{\"z\":1,\"y\":2}")
              (.execute ps))
            (with-open [st (.createStatement c)
                        rs (.executeQuery st "SELECT p FROM jw WHERE id = 2")]
              (.next rs)
              (is (= "{\"y\": 2, \"z\": 1}" (.getString rs 1))))))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

;; ---------------------------------------------------------------------------
;; Operators that were silently wrong. Each returned a plausible answer of the
;; wrong kind rather than failing, which is why none of them had a test.
;; ---------------------------------------------------------------------------

(deftest operators-that-were-wrong-answers
  (run "CREATE TABLE op (id int PRIMARY KEY, d jsonb, n int)")
  (run "INSERT INTO op VALUES (1, '{\"a\":1,\"b\":2,\"kind\":\"x\"}', 10)")

  (testing "?| and ?& against array[...] — the RHS arrives as a PgArray
            record, and iterating a record yields its MAP ENTRIES, so
            every key test compared a pair against a string and matched
            nothing"
    (is (= [["1"]] (rows "SELECT count(*) FROM op WHERE d ?| array['kind','zz']")))
    (is (= [["1"]] (rows "SELECT count(*) FROM op WHERE d ?& array['a','b']")))
    (is (= [["0"]] (rows "SELECT count(*) FROM op WHERE d ?& array['a','zz']")))
    (is (= [["1"]] (rows "SELECT count(*) FROM op WHERE d ? 'a'"))))

  (testing "|| was string concatenation outside UPDATE SET — a stored
            jsonb value IS a string, so runtime dispatch cannot tell it
            from text; the column type has to decide"
    (is (= "{\"a\": 1, \"b\": 2, \"z\": 9, \"kind\": \"x\"}"
           (v "SELECT d || '{\"z\":9}' FROM op"))))

  (testing "- threw a raw ClassCastException in SELECT, because every
            subtraction routed to numeric arithmetic"
    (is (= "{\"a\": 1, \"kind\": \"x\"}" (v "SELECT d - 'b' FROM op")))
    (is (= "[1, 3]" (v "SELECT '[1,2,3]'::jsonb - 1"))
        "an explicit ::jsonb cast is a jsonb operand too, not just a column")
    (is (= "7" (v "SELECT n - 3 FROM op")) "numeric subtraction is untouched"))

  (testing "jsonb_agg was a per-row fn, so it returned one row per input
            row instead of one array per group"
    (run "CREATE TABLE ag (id int PRIMARY KEY, g text, v int)")
    (run "INSERT INTO ag VALUES (1,'a',10),(2,'a',20),(3,'b',30)")
    (is (= 1 (count (rows "SELECT jsonb_agg(v) FROM ag"))))
    (is (= 2 (count (rows "SELECT g, jsonb_agg(v) FROM ag GROUP BY g"))))
    (is (= "[30]" (v "SELECT jsonb_agg(v) FROM ag WHERE g = 'b'")))))

(deftest is-null-sees-the-sql-null-sentinel
  (testing "SQL NULL travels as :__null__, not nil, so `nil?` answered
            false for a value that IS NULL"
    (run "CREATE TABLE nn (id int PRIMARY KEY, d jsonb)")
    (run "INSERT INTO nn VALUES (1, '{\"jn\":null,\"s\":\"x\"}')")
    (is (= [["t"]] (rows "SELECT (d->'nope') IS NULL FROM nn")))
    (is (= [["f"]] (rows "SELECT (d->'jn') IS NULL FROM nn"))
        "a PRESENT JSON null is a value, not SQL NULL")
    (is (= [["t"]] (rows "SELECT (d->>'jn') IS NULL FROM nn"))
        "but ->> collapses it to SQL NULL")
    (is (= [["1"]] (rows "SELECT count(*) FROM nn WHERE d->'nope' IS NULL")))))

;; ---------------------------------------------------------------------------
;; Error surface and path operators
;; ---------------------------------------------------------------------------

(deftest unknown-functions-raise-42883
  (testing "an unresolvable function used to emit a datalog clause and
            fail at execute time, so the client saw our internals:
            `Unknown function 'json_build_object in [(json_build_object
            \"a\" 1) ?v1]` under XX000"
    (is (= "42883" (state "SELECT row_to_json(1)")))
    (is (re-find #"function row_to_json does not exist"
                 (or (err "SELECT row_to_json(1)") "")))
    (is (= "42883" (state "SELECT jsonb_path_query('{}'::jsonb, '$.a')"))))
  (testing "implemented functions are unaffected"
    (is (= "3" (v "SELECT abs(-3)")))
    (is (= "{\"a\": 1}" (v "SELECT jsonb_build_object('a',1)")))))

(deftest the-json-family-is-text-faithful
  (testing "json_* is NOT jsonb_* under another name: `json` preserves
            argument order and KEEPS duplicate keys, and PostgreSQL's
            separator there is \" : \" with spaces on both sides"
    (is (= "{\"b\" : 1, \"a\" : 2, \"a\" : 3}"
           (v "SELECT json_build_object('b',1,'a',2,'a',3)")))
    (is (= "{\"a\": 3, \"b\": 1}"
           (v "SELECT jsonb_build_object('b',1,'a',2,'a',3)"))
        "the jsonb form sorts and takes the last"))
  (testing "arrays render alike in both families"
    (is (= "[1, \"x\", true]" (v "SELECT json_build_array(1,'x',true)"))))
  (testing "to_json / to_jsonb do NOT parse their argument — a text value
            becomes a json STRING, not a document"
    (run "CREATE TABLE tj (id int PRIMARY KEY, t text, b jsonb)")
    (run "INSERT INTO tj VALUES (1, '{\"a\":1}', '{\"a\":1}')")
    (is (= "\"x\"" (v "SELECT to_json('x'::text)")))
    (is (= "\"{\\\"a\\\":1}\"" (v "SELECT to_jsonb(t) FROM tj"))
        "a text column holding JSON is wrapped, not read")
    (is (= "{\"a\": 1}" (v "SELECT to_jsonb(b) FROM tj"))
        "an argument that already IS jsonb passes through"))
  (testing "the aliases that genuinely are aliases"
    (run "CREATE TABLE ja (id int PRIMARY KEY, v int)")
    (run "INSERT INTO ja VALUES (1,10),(2,20)")
    (is (= 1 (count (rows "SELECT json_agg(v) FROM ja"))))
    (is (= "3" (v "SELECT json_array_length('[1,2,3]'::json)"))))
  (testing "a json result is COMPACT where a jsonb one is spaced — the
            families differ in object punctuation, not just in what they
            keep"
    (is (= "{\"a\":1}" (v "SELECT json_strip_nulls('{\"a\":1,\"z\":null}'::json)")))
    (is (= "{\"a\": 1}" (v "SELECT jsonb_strip_nulls('{\"a\":1,\"z\":null}'::jsonb)")))
    (is (= "{\"a\": 1, \"n\": {\"r\": 2}}"
           (v "SELECT jsonb_strip_nulls('{\"a\":1,\"z\":null,\"n\":{\"q\":null,\"r\":2}}'::jsonb)"))
        "recursive, and JSON null is the sentinel now — `some?` was true
         for it, so nothing was stripped")))

(deftest path-operators
  (run "CREATE TABLE pj (id int PRIMARY KEY, d jsonb)")
  (run "INSERT INTO pj VALUES (1, '{\"a\":{\"b\":[10,20]},\"z\":1}')")
  (testing "#> and #>> were rejected at the parser front door by the `#`
            operator-character check, even though jsqlparser parses them
            as the same JsonExpression node `->`/`->>` produce"
    (is (= "20" (v "SELECT d #> '{a,b,1}' FROM pj")))
    (is (= "20" (v "SELECT d #>> '{a,b,1}' FROM pj")))
    (is (= "{\"b\": [10, 20]}" (v "SELECT d #> '{a}' FROM pj"))))
  (testing "a missing path is SQL NULL, not a dropped row"
    (is (= [[nil]] (rows "SELECT d #> '{nope}' FROM pj"))))
  (testing "bare # is still PostgreSQL's XOR and still unsupported"
    (is (= "42601" (state "SELECT 42#")))))

(deftest json-has-no-comparison-or-containment-operators
  (testing "PostgreSQL gives `json` exactly six operators — ->, ->>, #>,
            #>> and their int variants — and no btree or hash operator
            class at all, so @> and ? are 42883 there. We accepted them
            silently, comparing the stored text."
    (run "CREATE TABLE jc (id int PRIMARY KEY, j json, b jsonb)")
    (run "INSERT INTO jc VALUES (1, '{\"a\":1}', '{\"a\":1}')")
    (is (= "42883" (state "SELECT count(*) FROM jc WHERE j @> '{\"a\":1}'")))
    (is (re-find #"operator does not exist: json @> json"
                 (or (err "SELECT count(*) FROM jc WHERE j @> '{\"a\":1}'") "")))
    (is (= "42883" (state "SELECT count(*) FROM jc WHERE j ? 'a'"))))
  (testing "the same operators on jsonb are unaffected"
    (is (= [["1"]] (rows "SELECT count(*) FROM jc WHERE b @> '{\"a\":1}'")))
    (is (= [["1"]] (rows "SELECT count(*) FROM jc WHERE b ? 'a'"))))
  (testing "and json keeps the six it does have"
    (is (= "1" (v "SELECT j->>'a' FROM jc")))))

(deftest object-aggregates
  (run "CREATE TABLE oa (id int PRIMARY KEY, k text, v int, g text)")
  (run "INSERT INTO oa VALUES (1,'b',1,'x'),(2,'a',2,'x'),(3,'c',3,'y')")
  (testing "jsonb_object_agg was a per-row fn producing a one-key object
            per row; it now folds over the group through the two-argument
            aggregate path CORR already used"
    (is (= "{\"a\": 2, \"b\": 1, \"c\": 3}" (v "SELECT jsonb_object_agg(k,v) FROM oa")))
    (is (= 2 (count (rows "SELECT g, jsonb_object_agg(k,v) FROM oa GROUP BY g")))))
  (testing "json_object_agg keeps insertion order and pads its braces,
            which is PostgreSQL's own punctuation for that function"
    (is (= "{ \"b\" : 1, \"a\" : 2, \"c\" : 3 }" (v "SELECT json_object_agg(k,v) FROM oa")))))

(deftest arrow-returns-a-json-value
  (run "CREATE TABLE ar (id int PRIMARY KEY, d jsonb)")
  (run "INSERT INTO ar VALUES (1, '{\"s\":\"x\",\"n\":1.00,\"arr\":[1,2],\"obj\":{\"k\":9},\"jn\":null}')")
  (testing "-> yields a json VALUE, so a string renders QUOTED — the
            output path sees a Clojure string either way and cannot tell
            it from SQL text, so the chain serialises its own result"
    (is (= "\"x\"" (v "SELECT d->'s' FROM ar")))
    (is (= "x" (v "SELECT d->>'s' FROM ar")) "->> is text, unquoted")
    (is (= "1.00" (v "SELECT d->'n' FROM ar")) "and numeric scale survives")
    (is (= "[1, 2]" (v "SELECT d->'arr' FROM ar")))
    (is (= "{\"k\": 9}" (v "SELECT d->'obj' FROM ar")))
    (is (= "null" (v "SELECT d->'jn' FROM ar"))))
  (testing "chaining is unaffected — only the FINAL result of a
            value-returning chain is serialised"
    (is (= "9" (v "SELECT d->'obj'->>'k' FROM ar")))
    (is (= "9" (v "SELECT d->'obj'->'k' FROM ar")))
    (is (= "2" (v "SELECT d->'arr'->>1 FROM ar")))))

(deftest equality-is-structural-not-textual
  (run "CREATE TABLE sc (id int PRIMARY KEY, d jsonb)")
  (run "INSERT INTO sc VALUES (1,'{\"a\":1.00}'),(2,'{\"a\":1}'),(3,'{\"a\":1.0}')")
  (testing "PostgreSQL keeps display scale but IGNORES it when comparing,
            so its stored form is deliberately not canonical for its own
            equality. Comparing our canonical text was therefore too
            strict — the three rows below are one value in PostgreSQL."
    (is (= [["{\"a\": 1.00}"] ["{\"a\": 1}"] ["{\"a\": 1.0}"]]
           (rows "SELECT d FROM sc ORDER BY id"))
        "display is unchanged — scale still survives")
    (is (= [["3"]] (rows "SELECT count(*) FROM sc WHERE d = '{\"a\":1}'"))
        "all three match, whatever scale the literal was written with")
    (is (= [["3"]] (rows "SELECT count(*) FROM sc WHERE d = '{\"a\":1.000}'"))))
  (testing "genuinely different values still differ"
    (is (= [["0"]] (rows "SELECT count(*) FROM sc WHERE d = '{\"a\":2}'")))
    (is (= [["0"]] (rows "SELECT count(*) FROM sc WHERE d = '{\"b\":1}'"))))
  (testing "and the text fast path still handles the ordinary case —
            equal canonical text implies equal values, so only differing
            text is ever parsed"
    (run "CREATE TABLE sc2 (id int PRIMARY KEY, d jsonb)")
    (run "INSERT INTO sc2 VALUES (1,'{\"b\":2,\"a\":1}')")
    (is (= [["1"]] (rows "SELECT count(*) FROM sc2 WHERE d = '{ \"a\":1, \"b\":2 }'")))))

(deftest to-jsonb-dispatches-on-type-not-clojure-class
  (testing "PostgreSQL dispatches to_json/to_jsonb on the argument's
            DECLARED TYPE (json_categorize_type). We dispatched on the
            runtime Clojure class, and PgArray / PgRecord are defrecords
            — so `map?` was true for them and they leaked their internals
            as object keys."
    (is (= "[1, 2, 3]" (v "SELECT to_jsonb(ARRAY[1,2,3])")))
    (is (= "[\"a\", \"b\"]" (v "SELECT to_jsonb(ARRAY['a','b'])")))
    (is (= "{\"f1\": 1, \"f2\": \"x\"}" (v "SELECT to_jsonb(ROW(1,'x'))"))))
  (testing "a temporal value is ISO-8601, not java.util.Date's .toString,
            and must NOT be shifted — a `timestamp` carries no zone"
    (run "CREATE TABLE ts (id int PRIMARY KEY, t timestamp)")
    (run "INSERT INTO ts VALUES (1, '2026-08-16 19:02:08')")
    (is (= "\"2026-08-16T19:02:08\"" (v "SELECT to_jsonb(t) FROM ts"))))
  (testing "numeric scale survives to_jsonb when the value HAS its scale
            — a numeric column restores it even though a bare literal
            currently does not (a separate, pre-existing gap)"
    (run "CREATE TABLE nm (id int PRIMARY KEY, n numeric(10,2))")
    (run "INSERT INTO nm VALUES (1, 1.10)")
    (is (= "1.10" (v "SELECT n FROM nm")))
    (is (= "1.10" (v "SELECT to_jsonb(n) FROM nm")))))
