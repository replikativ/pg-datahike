(ns datahike.test.pg-jsonb-containment-test
  "Fixture-independent containment and existence cases ported from
   PostgreSQL's jsonb regression suite and `JsonbDeepContains`."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false
             :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- result [sql] (.execute *handler* sql))

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (result sql))))

(defn- value [sql] (ffirst (rows sql)))

(defn- state [sql]
  (try
    (.-sqlstate ^PgWireServer$QueryResult (result sql))
    (catch Exception e (:sqlstate (ex-data e)))))

(defn- contains-value [left right]
  (value (str "SELECT '" left "'::jsonb @> '" right "'::jsonb")))

(deftest recursive-container-containment
  (testing "object edges line up while nested arrays and objects recurse"
    (doseq [[left right expected]
            [["{\"a\":[1,2],\"c\":\"b\"}" "{\"a\":[1]}" "t"]
             ["{\"a\":[2,1],\"c\":\"b\"}" "{\"a\":[1,2]}" "t"]
             ["{\"a\":{\"c\":3,\"x\":4}}" "{\"a\":{\"c\":3}}" "t"]
             ["{\"a\":[1,2,{\"c\":3,\"x\":4}]}" "{\"a\":[{\"c\":3}]}" "t"]
             ["{\"a\":[1,2,{\"c\":3,\"x\":4}]}" "{\"a\":[{\"x\":4},3]}" "f"]
             ["{\"name\":\"Bob\",\"tags\":[\"enim\",\"qui\"]}" "{\"tags\":[\"qu\"]}" "f"]]]
      (is (= expected (contains-value left right))
          (str left " @> " right)))))

(deftest array-and-raw-scalar-containment
  (testing "array multiplicity does not matter, including recursively"
    (is (= "t" (contains-value "[1,2]" "[1,2,2]")))
    (is (= "t" (contains-value "[[1,2]]" "[[1,2,2]]")))
    (is (= "t" (contains-value "[1.00]" "[1]"))
        "numeric scale is immaterial to jsonb containment")
    (is (= "t" (value "SELECT '[1,2,2]'::jsonb <@ '[1,2]'::jsonb"))
        "<@ is the exact inverse relation"))
  (testing "an array may contain a raw scalar, but not conversely"
    (is (= "t" (contains-value "[5]" "5")))
    (is (= "f" (contains-value "5" "[5]"))))
  (testing "a scalar match does not recurse into a nested array"
    (is (= "f" (contains-value "[[5]]" "5")))))

(deftest existence-array-null-elements-are-ignored
  (testing "jsonb_op.c skips NULL key datums for both any and all"
    (is (= "f" (value "SELECT '{\"a\":1}'::jsonb ?| ARRAY[NULL]")))
    (is (= "t" (value "SELECT '{\"a\":1}'::jsonb ?| ARRAY[NULL,'a']")))
    (is (= "t" (value "SELECT '{\"a\":1}'::jsonb ?& ARRAY[NULL]")))
    (is (= "f" (value "SELECT '{\"a\":1}'::jsonb ?& ARRAY[NULL,'x']"))))
  (testing "a NULL array is SQL NULL, unlike an empty or all-NULL array"
    (is (= [[nil]]
           (rows "SELECT '{\"a\":1}'::jsonb ?| NULL::text[]")))
    (is (= [[nil]]
           (rows "SELECT '{\"a\":1}'::jsonb ?& NULL::text[]")))))

(deftest named-predicate-functions
  (testing "the pg_proc spellings share their implementations with operators"
    (is (= "t" (value "SELECT jsonb_contains('{\"a\":1,\"b\":2}', '{\"a\":1}')")))
    (is (= "t" (value "SELECT jsonb_contained('{\"a\":1}', '{\"a\":1,\"b\":2}')")))
    (is (= "t" (value "SELECT jsonb_exists('{\"a\":null}', 'a')")))
    (is (= "t" (value "SELECT jsonb_exists_any('{\"a\":null}', ARRAY['x','a'])")))
    (is (= "f" (value "SELECT jsonb_exists_any('{\"a\":null}', ARRAY[]::text[])")))
    (is (= "t" (value "SELECT jsonb_exists_all('{\"a\":null}', ARRAY[]::text[])"))))
  (testing "the shared spec makes them strict, typed boolean, and arity checked"
    (is (= [[nil]] (rows "SELECT jsonb_contains(NULL, '{\"a\":1}')")))
    (is (= [16]
           (vec (.-columnOids ^PgWireServer$QueryResult
                 (result "SELECT jsonb_exists('{}', 'a')")))))
    (is (= "42883" (state "SELECT jsonb_exists('{}')")))))

(deftest named-predicate-in-update-expression
  (result "CREATE TABLE predicate_update (id int PRIMARY KEY, doc jsonb, matched bool)")
  (result "INSERT INTO predicate_update VALUES (1, '{\"a\":1}', false)")
  (result "UPDATE predicate_update SET matched = jsonb_contains(doc, '{\"a\":1}') WHERE id = 1")
  (is (= "t" (value "SELECT matched FROM predicate_update WHERE id = 1"))))
