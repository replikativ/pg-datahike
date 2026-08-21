(ns datahike.test.pg-distinct-having-array-test
  "DISTINCT ON, HAVING over aggregates, and the array / jsonb operators --
   the fourth tranche of differential-fuzzing findings.

   The fuzzer's generator was widened here to arrays, jsonb, CTEs, type
   coercion and numeric edges. CTEs, coercion and the numeric edges came
   back clean; these did not, and almost every one produced WRONG ROWS
   rather than an error.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn dh-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"dh" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each dh-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/dh?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.. rs getMetaData getColumnCount)]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv (fn [^long ix] (.getString rs ix)) (range 1 (inc n)))))
          acc)))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ft (id int, i int, j int, b boolean, arr int[], js jsonb)")
  (exec! c (str "INSERT INTO ft VALUES "
                "(1,10,20,true,'{1,2,3}','{\"a\":1}'),"
                "(2,NULL,20,false,NULL,NULL),"
                "(3,10,NULL,NULL,'{}','{}'),"
                "(4,-3,0,true,'{4,NULL}','{\"a\":null}'),"
                "(5,0,7,false,'{5}','[1,2]')")))

(deftest distinct-on-keeps-the-first-row-per-key
  (with-open [c (jdbc)]
    (seed! c)
    ;; `.getDistinct` was read as a BOOLEAN, so DISTINCT ON became plain
    ;; DISTINCT -- which dedupes the WHOLE row and therefore kept every
    ;; projection that happened to differ. Rows 1 and 3 both have i = 10.
    (is (= [["4" "0"] ["5" "7"] ["1" "20"]]
           (rows c "SELECT DISTINCT ON (i) id, j FROM ft WHERE i IS NOT NULL ORDER BY i, id")))
    (is (= ["2" "1" "3"] (col c 1 "SELECT DISTINCT ON (b) id FROM ft ORDER BY b, id")))
    (testing "the ON key may be several expressions, and need not be projected"
      (is (= ["4" "5" "1" "3" "2"]
             (col c 1 "SELECT DISTINCT ON (i, j) id FROM ft ORDER BY i, j, id"))))
    (testing "DESC picks the other end of each run"
      (is (= ["2" "1" "5" "4"] (col c 1 "SELECT DISTINCT ON (i) id FROM ft ORDER BY i DESC, id"))))
    (testing "plain DISTINCT is unaffected"
      (is (= ["-3" "0" "10" nil] (col c 1 "SELECT DISTINCT i FROM ft ORDER BY i"))))))

(deftest having-over-aggregates
  (with-open [c (jdbc)]
    (seed! c)
    (testing "IS NULL / IS NOT NULL on an aggregate"
      ;; The HAVING translation resolved a column index only for a bare
      ;; Column. An aggregate left it nil, and a nil index makes the
      ;; predicate nil, which apply-having reads as "no predicate" -- so
      ;; EVERY group passed, in both directions.
      (is (= [["-3" "0"] ["0" "7"] ["10" "20"] [nil "20"]]
             (rows c (str "SELECT i AS g, sum(j) AS a FROM ft GROUP BY 1 "
                          "HAVING sum(j) IS NOT NULL ORDER BY 1"))))
      (is (= [] (rows c (str "SELECT i AS g, sum(j) AS a FROM ft GROUP BY 1 "
                             "HAVING sum(j) IS NULL ORDER BY 1")))))
    (testing "an aggregate that is NOT the one in the SELECT list"
      ;; match-aggregate-index compared only the aggregate OPERATOR, so with
      ;; `min(1)` projected and `min(i)` in HAVING both resolved to the same
      ;; column and the filter ran against the wrong aggregate entirely.
      (is (= ["1" "3" "4" "5"]
             (col c 1 (str "SELECT id AS g, min(1) AS a FROM ft GROUP BY 1 "
                           "HAVING min(i) IS NOT NULL ORDER BY 1"))))
      (is (= ["1" "3"]
             (col c 1 (str "SELECT id AS g, min(1) AS a FROM ft GROUP BY 1 "
                           "HAVING min(i) > 0 ORDER BY 1")))))
    (testing "an aggregate over a constant, which the projection path already handled"
      (is (= ["-3" "0" "10" nil]
             (col c 1 (str "SELECT i AS g, sum(j) AS a FROM ft GROUP BY 1 "
                           "HAVING sum(1) IS NOT NULL ORDER BY 1")))))))

(deftest array-comparison-against-a-stored-column
  (with-open [c (jdbc)]
    (seed! c)
    ;; An array COLUMN comes back as canonical PG text ("{1,2,3}") while an
    ;; ARRAY[...] literal is a PgArray record, so every comparison against a
    ;; column compared a String to a record and matched nothing.
    (is (= ["1"] (col c 1 "SELECT id FROM ft WHERE arr = ARRAY[1,2,3] ORDER BY id")))
    (is (= ["1"] (col c 1 "SELECT id FROM ft WHERE arr @> ARRAY[1] ORDER BY id")))
    (is (= ["1"] (col c 1 "SELECT id FROM ft WHERE arr && ARRAY[1,9] ORDER BY id"))
        "&& is a different AST node (DoubleAnd) and was not handled in WHERE at all")
    (testing "a NULL element is not 'trivially contained'"
      ;; ARRAY[4,NULL] <@ ARRAY[1,2,3,4] is FALSE in PostgreSQL, and a NULL
      ;; does not even match a NULL on the other side.
      (is (= ["1" "3"] (col c 1 "SELECT id FROM ft WHERE arr <@ ARRAY[1,2,3,4] ORDER BY id"))))
    (testing "in value position too"
      (is (= ["t" nil "f" "f" "f"]
             (col c 2 "SELECT id, arr = ARRAY[1,2,3] AS c FROM ft ORDER BY id"))))))

(deftest array-functions-and-null
  (with-open [c (jdbc)]
    (seed! c)
    ;; array_append / array_prepend are NOT strict in the array argument:
    ;; PostgreSQL treats a NULL array as empty.
    (is (= "{9}" (one c "SELECT array_append(NULL::int[], 9)")))
    (is (= "{0}" (one c "SELECT array_prepend(0, NULL::int[])")))
    (is (= ["{1,2,3,9}" "{9}" "{9}" "{4,NULL,9}" "{5,9}"]
           (col c 2 "SELECT id, array_append(arr, 9) AS c FROM ft ORDER BY id")))
    (testing "cardinality of an unknown array is NULL, not 0"
      (is (nil? (one c "SELECT cardinality(NULL::int[])")))
      (is (= ["3" nil "0" "2" "1"] (col c 2 "SELECT id, cardinality(arr) AS c FROM ft ORDER BY id"))))))

(deftest not-equal-any-and-all
  (with-open [c (jdbc)]
    (seed! c)
    ;; Only the `=` forms were recognised, so `x <> ALL(arr)` reached the
    ;; function table as a call to a function named "all". `<> ALL` is the
    ;; array spelling of NOT IN, so this is a common idiom.
    (is (= "f" (one c "SELECT 2 <> ALL(ARRAY[1,2])")))
    (is (= "t" (one c "SELECT 2 <> ALL(ARRAY[3,4])")))
    (is (= "t" (one c "SELECT 2 <> ANY(ARRAY[1,2])")))
    (is (= "f" (one c "SELECT 2 <> ANY(ARRAY[2,2])")))
    (testing "Kleene over the elements"
      (is (= "f" (one c "SELECT 2 <> ALL(ARRAY[2,NULL])"))
          "an element that EQUALS settles it, NULL or no NULL")
      (is (nil? (one c "SELECT 2 <> ALL(ARRAY[3,NULL])")) "otherwise a NULL element is UNKNOWN"))
    (testing "against a stored column, in WHERE"
      (is (= ["3" "5"] (col c 1 "SELECT id FROM ft WHERE 2 <> ALL(arr) ORDER BY id")))
      (is (= ["1" "4" "5"] (col c 1 "SELECT id FROM ft WHERE 2 <> ANY(arr) ORDER BY id"))))))

(deftest jsonb-operators-propagate-null
  (with-open [c (jdbc)]
    (seed! c)
    ;; Every one of these answered FALSE for a NULL document, which is
    ;; indistinguishable from a genuine non-match.
    (is (= ["t" nil "f" "t" "f"] (col c 2 "SELECT id, js ? 'a' AS c FROM ft ORDER BY id"))
        "and the array case returned nil on a miss, which DROPPED the row")
    (is (= ["t" nil "f" "f" "f"] (col c 2 "SELECT id, js @> '{\"a\":1}' AS c FROM ft ORDER BY id")))
    (is (= ["t" nil "t" "f" "f"]
           (col c 2 "SELECT id, js <@ '{\"a\":1,\"b\":2}' AS c FROM ft ORDER BY id")))
    (is (= ["t" nil "f" "t" "f"] (col c 2 "SELECT id, js ?| ARRAY['a'] AS c FROM ft ORDER BY id")))
    (is (= ["t" nil "f" "t" "f"] (col c 2 "SELECT id, js ?& ARRAY['a'] AS c FROM ft ORDER BY id")))
    (testing "jsonb_typeof distinguishes a SQL NULL from the JSON null literal"
      (is (nil? (one c "SELECT jsonb_typeof(NULL::jsonb)")))
      (is (= "null" (one c "SELECT jsonb_typeof('null'::jsonb)")))
      (is (= ["object" nil "object" "object" "array"]
             (col c 2 "SELECT id, jsonb_typeof(js) AS c FROM ft ORDER BY id"))))
    (testing "and the WHERE forms still collapse UNKNOWN to reject"
      (is (= ["1" "4"] (col c 1 "SELECT id FROM ft WHERE js ? 'a' ORDER BY id")))
      (is (= ["1"] (col c 1 "SELECT id FROM ft WHERE js @> '{\"a\":1}' ORDER BY id"))))))
