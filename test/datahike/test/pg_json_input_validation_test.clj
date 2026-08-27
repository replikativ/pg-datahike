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
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.jsonb :as jb]
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

(defn- sql-string [s]
  (str "'" (str/replace s "'" "''") "'"))

(deftest postgres-json-input-regression-slices
  ;; Complete statement accounting for PostgreSQL 17.7 json.sql:1-48,55-87
  ;; and jsonb.sql:11-52,60-93, except the separately classified recursion/GUC
  ;; block. Error wording is parser-specific; success/failure, SQLSTATE, row
  ;; preservation, jsonb numeric range, and input-inspection behavior are the
  ;; user-visible contract gated here.
  (let [valid-docs ["\"\"" "\"abc\"" "\"\\n\\\"\\\\\""
                    "1" "0" "0.1" "9223372036854775808" "1e100" "1.3e100"
                    "[]" (str (apply str (repeat 100 "["))
                              (apply str (repeat 100 "]")))
                    "[1,2]" "{}" "{\"abc\":1}"
                    "{\"abc\":1,\"def\":2,\"ghi\":[3,4],\"hij\":{\"klm\":5,\"nop\":[6]}}"
                    "true" "false" "null" " true "
                    "{\n\t\"one\": 1,\n\t\"two\":\"two\",\n\t\"three\": true}"]
        invalid-docs ["\"abc" "\"abc\ndef\"" "\"\\v\"" "01" "1f2" "0.x1" "1.3ex100"
                      "[1,2,]" "[1,2" "[1,[2]" "{\"abc\"}" "{1:\"abc\"}"
                      "{\"abc\",1}" "{\"abc\"=1}" "{\"abc\"::1}"
                      "{\"abc\":1:2}" "{\"abc\":1,3}" "true false"
                      "true, false" "truf" "trues" "" "    "
                      "{\n\t\"one\":1,\n\t\"two\":,\"two\",\n\t\"three\":true}"
                      "{\n\t\"one\":1,\n\t\"two\":\"two\",\n\t\"longfield\":}"]]
    (doseq [type-name ["json" "jsonb"]]
      (testing (str type-name " accepts every valid document in the slices")
        (doseq [doc valid-docs]
          (is (seq (rows (str "SELECT " (sql-string doc) "::" type-name))) doc))
        (is (seq (rows (str "SELECT ('\"' || repeat('.', 12) || 'abc\"')::"
                            type-name))))
        (is (seq (rows (str "SELECT ('\"' || repeat('.', 12) || 'abc\\n\"')::"
                            type-name)))))
      (testing (str type-name " rejects every malformed document with 22P02")
        (is (= "22P02" (state (str "SELECT $$''$$::" type-name))))
        (doseq [doc invalid-docs]
          (is (= "22P02" (state (str "SELECT " (sql-string doc) "::" type-name))) doc))))
    (is (= "t" (v "SELECT pg_input_is_valid('{\"a\":true}', 'json')")))
    (is (= "f" (v "SELECT pg_input_is_valid('{\"a\":true', 'json')")))
    (is (= "t" (v "SELECT pg_input_is_valid('{\"a\":true}', 'jsonb')")))
    (is (= "f" (v "SELECT pg_input_is_valid('{\"a\":true', 'jsonb')")))
    (is (= "f" (v "SELECT pg_input_is_valid('{\"a\":1e1000000}', 'jsonb')")))
    (is (= "22P02"
           (v (str "SELECT sql_error_code FROM "
                   "pg_input_error_info('{\"a\":true', 'jsonb')"))))
    (is (= "22003"
           (v (str "SELECT sql_error_code FROM "
                   "pg_input_error_info('{\"a\":1e1000000}', 'jsonb')"))))))

(deftest postgres-json-number-length-and-range
  (let [n1001 (apply str (repeat 1001 "7"))
        too-wide (apply str (repeat 131073 "7"))]
    (testing "Jackson's dependency default is not our PostgreSQL input limit"
      (is (= "t" (v (str "SELECT pg_input_is_valid(" (sql-string n1001) ", 'json')"))))
      (is (= "t" (v (str "SELECT pg_input_is_valid(" (sql-string n1001) ", 'jsonb')")))))
    (testing "json remains text-like while jsonb enforces numeric's range"
      (is (= too-wide (jb/serialize-json too-wide)))
      (is (= "22003"
             (try (jb/serialize-jsonb too-wide) nil
                  (catch Exception e (:sqlstate (ex-data e))))))
      (is (= "f" (v (str "SELECT pg_input_is_valid("
                         (sql-string too-wide) ", 'jsonb')"))))
      (is (= "22003"
             (v (str "SELECT sql_error_code FROM pg_input_error_info("
                     (sql-string too-wide) ", 'jsonb')")))))
    (testing "a large exponent on numeric zero has no integer weight"
      (is (= "0" (v "SELECT '0e1000000'::jsonb")))
      (is (= "t" (v "SELECT pg_input_is_valid('0e1000000', 'jsonb')"))))))

(deftest malformed-json-is-rejected-on-cast
  (testing "each of these returned a VALUE before"
    ;; PostgreSQL json.sql:3 / jsonb.sql:13 use a dollar-quoted invalid
    ;; literal. JSqlParser surfaces it as a Column unless preprocessing
    ;; restores its string-literal meaning.
    (is (= "22P02" (state "SELECT $$''$$::json")) "dollar-quoted json")
    (is (= "22P02" (state "SELECT $$''$$::jsonb")) "dollar-quoted jsonb")
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

(deftest json-input-inspection-uses-the-json-input-function
  ;; PostgreSQL json.sql:85-87 and jsonb.sql:90-93. These functions used to
  ;; recognize neither JSON type and therefore reported every value invalid.
  (is (= "t" (v "SELECT pg_input_is_valid('{\"a\":true}', 'json')")))
  (is (= "f" (v "SELECT pg_input_is_valid('{\"a\":true', 'json')")))
  (is (= "t" (v "SELECT pg_input_is_valid('{\"a\":true}', 'jsonb')")))
  (is (= "f" (v "SELECT pg_input_is_valid('{\"a\":true', 'jsonb')")))
  (is (= "f" (v "SELECT pg_input_is_valid('{\"a\":1e1000000}', 'jsonb')")))
  (is (= "22P02"
         (v (str "SELECT sql_error_code FROM "
                 "pg_input_error_info('{\"a\":true', 'jsonb')"))))
  (is (= "22003"
         (v (str "SELECT sql_error_code FROM "
                 "pg_input_error_info('{\"a\":1e1000000}', 'jsonb')")))))

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
