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
