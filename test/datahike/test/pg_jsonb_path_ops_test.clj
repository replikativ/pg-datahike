(ns datahike.test.pg-jsonb-path-ops-test
  "jsonb_set / jsonb_insert take a `text[]` path, and nothing between the
   parser and the function casts it — so the literal `'{a,b}'` arrived as
   the single 5-character string \"{a,b}\". Every call therefore addressed
   one key named `{a,b}`: `jsonb_set('{\"a\":1}','{a}','9')` answered
   `{\"a\": 1, \"{a}\": 9}` instead of `{\"a\": 9}`.

   The path is also how you address an ARRAY, so with it unparsed neither
   function could reach an array element at all, and `jsonb_insert` had
   been written as a delegate to `jsonb_set` that dropped `insert_after`
   outright. All three now share one traversal ported from setPath in
   PostgreSQL's jsonfuncs.c; the expectations below are that oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn jsonb-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"jsonb" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)]
          (f))
        (finally
          (.stop server)
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each jsonb-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/jsonb?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest jsonb-set-parses-the-text-array-path
  (with-open [c (jdbc)]
    (testing "a single-element path names a key, it is not a key"
      (is (= "{\"a\": 9}" (one c "SELECT jsonb_set('{\"a\":1}'::jsonb,'{a}','9'::jsonb)"))))
    (testing "a multi-element path descends"
      (is (= "{\"a\": {\"b\": 9}}"
             (one c "SELECT jsonb_set('{\"a\":{\"b\":1}}'::jsonb,'{a,b}','9'::jsonb)"))))
    (testing "an integer element addresses an array slot"
      (is (= "[1, 9, 3]" (one c "SELECT jsonb_set('[1,2,3]'::jsonb,'{1}','9'::jsonb)")))
      (is (= "[1, 2, 9]" (one c "SELECT jsonb_set('[1,2,3]'::jsonb,'{-1}','9'::jsonb)"))
          "negatives count back from the end")
      (is (= "{\"a\": [9, 2]}"
             (one c "SELECT jsonb_set('{\"a\":[1,2]}'::jsonb,'{a,0}','9'::jsonb)"))
          "object then array in one path"))))

(deftest jsonb-set-create-missing
  (with-open [c (jdbc)]
    (is (= "{\"a\": 1, \"b\": 9}" (one c "SELECT jsonb_set('{\"a\":1}'::jsonb,'{b}','9'::jsonb)"))
        "create_missing defaults to true")
    (is (= "{\"a\": 1}" (one c "SELECT jsonb_set('{\"a\":1}'::jsonb,'{b}','9'::jsonb,false)"))
        "the fourth argument has to reach the function to suppress the key")
    (is (= "{\"a\": 1}" (one c "SELECT jsonb_set('{\"a\":1}'::jsonb,'{x,y}','9'::jsonb)"))
        "PostgreSQL does not build out missing INTERMEDIATE levels — only
         jsonb_set_lax's FILL_GAPS does, and create_missing is not that")))

(deftest jsonb-insert-honours-insert-after
  (with-open [c (jdbc)]
    (is (= "[1, 2, 3]" (one c "SELECT jsonb_insert('[1,3]'::jsonb,'{1}','2'::jsonb)"))
        "default inserts BEFORE the addressed element")
    (is (= "[1, 3, 2]" (one c "SELECT jsonb_insert('[1,3]'::jsonb,'{1}','2'::jsonb,true)"))
        "insert_after was dropped at the call site, so this used to equal the
         insert-before answer")
    (is (= "{\"a\": {\"b\": [1, 2, 3]}}"
           (one c "SELECT jsonb_insert('{\"a\":{\"b\":[1,3]}}'::jsonb,'{a,b,1}','2'::jsonb)")))
    (is (= "{\"a\": [1, 9, 2]}"
           (one c "SELECT jsonb_insert('{\"a\":[1,2]}'::jsonb,'{a,0}','9'::jsonb,true)")))))

(deftest jsonb-insert-out-of-range-index
  (with-open [c (jdbc)]
    (is (= "[1, 2, 3]" (one c "SELECT jsonb_insert('[1,3]'::jsonb,'{-1}','2'::jsonb)"))
        "-1 is the last element; insert-before puts the value ahead of it")
    (is (= "[2, 1, 3]" (one c "SELECT jsonb_insert('[1,3]'::jsonb,'{-9}','2'::jsonb)"))
        "a negative that overshoots prepends (setPathArray's INT_MIN sentinel)")
    (is (= "[1, 3, 9]" (one c "SELECT jsonb_insert('[1,3]'::jsonb,'{5}','9'::jsonb)"))
        "a positive past the end clamps and appends")
    (is (= "[9]" (one c "SELECT jsonb_insert('[]'::jsonb,'{0}','9'::jsonb)"))
        "an empty array takes the value whatever the index")))

(deftest jsonb-insert-refuses-to-replace-an-existing-key
  (with-open [c (jdbc)]
    (is (thrown-with-msg? SQLException #"cannot replace existing key"
                          (one c "SELECT jsonb_insert('{\"a\":1}'::jsonb,'{a}','2'::jsonb)"))
        "jsonb_insert is insert-only on objects; PostgreSQL points you at
         jsonb_set instead of silently overwriting")))

(deftest jsonb-set-non-integer-index-into-array
  (with-open [c (jdbc)]
    (is (thrown-with-msg? SQLException #"is not an integer"
                          (one c "SELECT jsonb_set('[1,2,3]'::jsonb,'{x}','9'::jsonb)"))
        "an array can only be addressed by an integer, and the message
         carries the 1-based position of the offending element")))

(defn- error-state [c sql]
  (try
    (one c sql)
    nil
    (catch SQLException e (.getSQLState e))))

(deftest postgres-jsonpath-first-accessor-results
  (with-open [c (jdbc)]
    ;; PostgreSQL 18.6 jsonb_jsonpath.sql lines 1106–1108.
    (is (= "2203A" (error-state c "SELECT jsonb_path_query_first('[{\"a\": 1}, {\"a\": 2}, {}]', 'strict $[*].a')")))
    (is (= "1" (one c "SELECT jsonb_path_query_first('[{\"a\": 1}, {\"a\": 2}, {}]', 'strict $[*].a', silent => true)")))
    (is (= "1" (one c "SELECT jsonb_path_query_first('[{\"a\": 1}, {\"a\": 2}]', '$[*].a')")))
    (testing "strict silent stops where the first error occurs"
      (is (nil? (one c "SELECT jsonb_path_query_first('[{}, {\"a\":2}]', 'strict $[*].a', '{}', true)"))))
    (testing "member order follows the JSONB binary representation"
      (is (= "false" (one c "SELECT jsonb_path_query_first('{\"aa\":true,\"z\":false}', '$.*')")))
      (is (= "false" (one c "SELECT jsonb_path_query_first('{\"a.b\":false}', '$.\"a.b\"')"))))
    (testing "bare Unicode members and the lexical limits of the accessor subset"
      (is (= "42" (one c "SELECT jsonb_path_query_first('{\"漢字\":42}', 'strict$.漢字')")))
      (is (= "3" (one c "SELECT jsonb_path_query_first('{\"café\":3}', '$.\"caf\\u00e9\"')")))
      (is (= "0A000" (error-state c "SELECT jsonb_path_query_first('{\"a\":1}', '$./*c*/a')")))
      (is (= "0A000" (error-state c "SELECT jsonb_path_query_first('{\"a\":1}', '$.\"\\x61\"')")))
      (is (= "0A000" (error-state c "SELECT jsonb_path_query_first('{\"é\":1}', '$.\\u00e9')"))))
    (testing "Unicode input and long quoted names"
      (is (= "22P05" (error-state c "SELECT jsonb_path_query_first('{}', '$.\"\\u0000\"')")))
      (is (= "22P02" (error-state c "SELECT jsonb_path_query_first('{}', '$.\"\\uD800\"')")))
      (let [key (apply str (repeat 10000 "a"))]
        (is (nil? (one c (str "SELECT jsonb_path_query_first('{}', '$.\"" key "\"')"))))))
    (testing "lax member access unwraps one array level"
      (is (= "\"shallow\"" (one c "SELECT jsonb_path_query_first('[[{\"x\":\"deep\"}],{\"x\":\"shallow\"}]', '$.x')")))
      (is (= "\"deep\"" (one c "SELECT jsonb_path_query_first('[[{\"x\":\"deep\"}],{\"x\":\"shallow\"}]', '$[*].x')"))))
    (testing "JSON null, a string containing null, missing results and SQL NULL"
      (is (= "null" (one c "SELECT jsonb_path_query_first('{\"a\":null}', '$.*')")))
      (is (= "\"null\"" (one c "SELECT jsonb_path_query_first('{\"a\":\"null\"}', '$.*')")))
      (is (nil? (one c "SELECT jsonb_path_query_first('{}', '$.*')")))
      (is (nil? (one c "SELECT jsonb_path_query_first(NULL::jsonb, '$.*')"))))
    (testing "input errors precede strict NULL propagation"
      (is (= "22P02" (error-state c "SELECT jsonb_path_query_first('bad', '$', '{}', NULL)")))
      (is (= "42601" (error-state c "SELECT jsonb_path_query_first('{}', 'garbage', '{}', NULL)")))
      (is (= "42601" (error-state c "SELECT jsonb_path_query_first('{}', '$[', '{}', true)")))
      (is (= "22P02" (error-state c "SELECT jsonb_path_query_first(NULL, '$', 'bad', true)")))
      (is (= "22P02" (error-state c "SELECT jsonb_path_query_first(NULL, '$', '{}', 'bad')")))
      (is (nil? (one c "SELECT jsonb_path_query_first(NULL, '$[*] ? (@ > 1)')"))))
    (testing "type resolution, SQLSTATEs and unsupported SQL/JSON expressions"
      (is (= "2203C" (error-state c "SELECT jsonb_path_query_first('0', 'strict $.*', '{}', 'false')")))
      (is (= "42883" (error-state c "SELECT jsonb_path_query_first('0', '$', path => '$.*')")))
      (is (= "42601" (error-state c "SELECT jsonb_path_query_first('0', path => '$', path => '$.*')")))
      (is (= "22039" (error-state c "SELECT jsonb_path_query_first('0', 'strict $[*]')")))
      (is (= "2203C" (error-state c "SELECT jsonb_path_query_first('0', 'strict $.*')")))
      (is (= "22023" (error-state c "SELECT jsonb_path_query_first('0', '$', 'false'::jsonb)")))
      (is (= "42883" (error-state c "SELECT jsonb_path_query_first('0'::text, '$')")))
      (is (= "0A000" (error-state c "SELECT jsonb_path_query_first('[1,2]', '$[*] ? (@ > 1)')"))))))

(deftest jsonpath-composes-with-jsonb-constructors-and-operators
  (with-open [c (jdbc)]
    (is (= "null" (one c "SELECT jsonb_path_query_first(jsonb_build_array(NULL::jsonb), '$[*]')")))
    (is (= "1" (one c "SELECT jsonb_path_query_first(jsonb_build_array(ARRAY[1,2]), 'strict $[*][*]')")))
    (is (= "7" (one c "SELECT jsonb_path_query_first(jsonb_build_object('row',row(7,8)), '$.row.f1')")))
    (is (= "{\"b\": 2, \"x\": 1}"
           (one c "SELECT jsonb_path_query_first('{\"a\":{\"x\":1}}', '$.a') || '{\"b\":2}'")))
    (is (= "\"null\"suffix" (one c "SELECT to_jsonb('null'::text) || 'suffix'::text")))
    (is (= "22P02" (error-state c "SELECT jsonb_path_query_first('0', '$') || 'bad'")))
    (is (= "22P02" (error-state c "SELECT jsonb_path_query_first('0', '$') = 'bad'")))
    (is (= "{}" (one c "SELECT jsonb_path_query_first('{\"a\":{\"x\":1}}', '$.a') - 'x'")))
    (is (= "t" (one c "SELECT jsonb_path_query_first('[1]', '$[*]') = '1.00'")))
    (is (= "\"null\"" (one c "SELECT to_jsonb(jsonb_path_query_first('[\"null\"]', '$[*]'))")))))

(deftest jsonb-constructors-embed-json-values-by-type
  (with-open [c (jdbc) stmt (.createStatement c)]
    (.execute stmt "CREATE TABLE json_inputs (j json, jb jsonb)")
    (.execute stmt "INSERT INTO json_inputs VALUES ('\"string\"','null')")
    (testing "object keys are SQL scalars and SQL NULL has its own diagnostic"
      (doseq [key ["'null'::jsonb" "'1'::json" "jb" "j" "ARRAY[1]"]]
        (is (thrown-with-msg? SQLException #"key value must be scalar"
                              (one c (str "SELECT jsonb_build_object(" key ", 1) FROM json_inputs"))))
        (is (= "22023" (error-state c (str "SELECT jsonb_build_object(" key ", 1) FROM json_inputs")))))
      (is (thrown-with-msg? SQLException #"argument 3: key must not be null"
                            (one c "SELECT jsonb_build_object('ok', 1, NULL::jsonb, 2)")))
      (is (= "22023" (error-state c "SELECT jsonb_build_object(NULL, 2)")))
      (is (= "22023" (error-state c "SELECT jsonb_build_object('orphan')")))
      (is (= "{\"1\": 2, \"true\": 3}" (one c "SELECT jsonb_build_object(1, 2, true, 3)"))))
    (is (= "{\"v\": null}" (one c "SELECT jsonb_build_object('v', 'null'::jsonb)")))
    (is (= "{\"v\": null}" (one c "SELECT jsonb_build_object('v', NULL::jsonb)")))
    (is (= "[\"string\", null, {\"a\": 2}, \"null\"]"
           (one c "SELECT jsonb_build_array(j, jb, '{\"a\":2}'::json, 'null'::text) FROM json_inputs")))))

(deftest odoo-updates-translations-from-values
  (with-open [c (jdbc) stmt (.createStatement c)]
    (.execute stmt "CREATE TABLE currency_labels (id int PRIMARY KEY, label jsonb)")
    (.execute stmt "INSERT INTO currency_labels VALUES (1, NULL), (2, '{\"en_US\":\"old\"}'), (3, '{}')")
    (is (= 3 (.executeUpdate stmt
                             "UPDATE currency_labels SET label =
                               CASE WHEN tmp.label::jsonb IS NULL THEN NULL ELSE
                                 COALESCE(currency_labels.label, jsonb_build_object(
                                   'en_US', jsonb_path_query_first(tmp.label::jsonb, '$.*')
                                 )) || tmp.label::jsonb END
                               FROM (VALUES (1,'{\"fr\":\"Dollars\"}'), (2,'{\"fr\":\"Euros\"}'), (3,NULL)) AS tmp(id,label)
                               WHERE currency_labels.id=tmp.id")))
    (is (= "{\"fr\": \"Dollars\", \"en_US\": \"Dollars\"}" (one c "SELECT label FROM currency_labels WHERE id=1")))
    (is (= "{\"fr\": \"Euros\", \"en_US\": \"old\"}" (one c "SELECT label FROM currency_labels WHERE id=2")))
    (is (nil? (one c "SELECT label FROM currency_labels WHERE id=3")))))
