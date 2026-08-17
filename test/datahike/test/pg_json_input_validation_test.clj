(ns datahike.test.pg-json-input-validation-test
  "PostgreSQL VALIDATES json and jsonb on input.

   `json_in` does a full RFC-8259 parse and only then stores the original
   bytes; `jsonb_in` parses and stores the tree. Both raise 22P02 on
   anything malformed.

   We validated neither. `parse-jsonb` catches its own parse error and
   falls back to treating the text as a JSON string scalar — which is
   right for `to_jsonb('some text')` and wrong for `'…'::jsonb` — so
   `'\"abc'` (unclosed quote) silently became the string `\"abc` and
   reached storage. Running a slice of PostgreSQL's own
   `src/test/regress/sql/jsonb.sql` against both servers found 27 such
   statements.

   Jackson is already strict about invalid escapes, raw control bytes,
   leading zeros, NaN, unquoted keys, trailing commas and uppercase
   literals, so this is mostly about not SWALLOWING its verdict. The one
   thing it does not do by default is reject TRAILING content, which
   `FAIL_ON_TRAILING_TOKENS` restores.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

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

(deftest malformed-json-is-rejected-on-cast
  (testing "each of these returned a VALUE before"
    (is (= "22P02" (state "SELECT '\"abc'::jsonb")) "unclosed quote")
    (is (= "22P02" (state "SELECT '01'::jsonb")) "leading zero")
    (is (= "22P02" (state "SELECT '1 2'::jsonb")) "trailing content")
    (is (= "22P02" (state "SELECT '{} {}'::jsonb")) "two documents")
    (is (= "22P02" (state "SELECT 'NaN'::jsonb")))
    (is (= "22P02" (state "SELECT '{a:1}'::jsonb")) "unquoted key")
    (is (= "22P02" (state "SELECT '[1,]'::jsonb")) "trailing comma")
    (is (= "22P02" (state "SELECT 'TRUE'::jsonb")) "literals are lowercase"))
  (testing "json validates too — it stores the bytes verbatim, but only
            after a full parse"
    (is (= "22P02" (state "SELECT '\"abc'::json")))
    (is (= "22P02" (state "SELECT '01'::json"))))
  (testing "well-formed input is unaffected"
    (is (= "\"abc\"" (v "SELECT '\"abc\"'::jsonb")))
    (is (= "[1, 2]" (v "SELECT '[1,2]'::jsonb")))
    (is (= "null" (v "SELECT 'null'::jsonb")))))

(deftest the-cast-also-normalizes
  (testing "`::jsonb` was a no-op that only set the wire OID, so a
            well-formed literal was not canonicalised either"
    (is (= "{\"a\": 2, \"b\": 1}" (v "SELECT '{\"b\":1,\"a\":2}'::jsonb"))))
  (testing "`::json` is text-faithful and must NOT be normalised"
    (is (= "{ \"b\":1,  \"a\":2 }" (v "SELECT '{ \"b\":1,  \"a\":2 }'::json")))))

(deftest malformed-json-is-rejected-on-write
  (run "CREATE TABLE vj (id int PRIMARY KEY, b jsonb, j json)")
  (is (= "INSERT 0 1"
         (.-commandTag ^PgWireServer$QueryResult
          (run "INSERT INTO vj VALUES (1, '{\"a\":1}', '{\"a\":1}')"))))
  (testing "a malformed value must not reach storage through INSERT"
    (is (= "22P02" (state "INSERT INTO vj VALUES (2, '\"abc', '{}')")))
    (is (= "22P02" (state "INSERT INTO vj VALUES (3, '{}', '\"abc')"))
        "the json column validates as well"))
  (testing "or through UPDATE"
    (is (= "22P02" (state "UPDATE vj SET b = '01' WHERE id = 1"))))
  (testing "and only the well-formed row is there"
    (is (= [["1"]] (rows "SELECT count(*) FROM vj")))
    (is (= "{\"a\": 1}" (v "SELECT b FROM vj WHERE id = 1")))))
