(ns datahike.test.pg-aggregate-expressions-test
  "Expressions OVER aggregates, in the select list and in HAVING.

   An aggregate's value does not exist until the grouping step, so
   nothing computed from one can be a Datalog clause in the same query.
   PostgreSQL draws the same line -- scan and aggregate below, projection
   above -- and this is that split: each aggregate is hoisted into a
   hidden column and what surrounds it becomes a form evaluated per
   group.

   Before, only binary ARITHMETIC over aggregates worked, through a
   bespoke tree with a four-operator evaluator. Everything else was
   broken, mostly silently:

     round(avg(x), 2)        No matching ctor found for class BigDecimal
     coalesce(sum(x), 0)     answered {\":fn\": \"sum\", …} -- the internal
                             marker map, as DATA
     upper(max(s))           the same
     abs(sum(x))             PersistentArrayMap cannot be cast to Number
     avg(x)::int             the cast was DROPPED (15.0000000000000000)
     sum(x) FILTER (…) + 1   \"expression of type AnalyticExpression is
                             not supported\"
     CASE WHEN count(*) …    refused outright
     HAVING sum(x) + 1 > 11  the predicate was DROPPED -- every group

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn ae-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"ae" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each ae-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/ae?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [acc []]
        (if (.next rs)
          (recur (conj acc (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          acc)))))

(defn- seed! [^Connection c]
  (exec! c "CREATE TABLE ae (id int, dept text, n int, m numeric(8,2), s text)")
  (exec! c (str "INSERT INTO ae VALUES "
                "(1,'a',10,1.50,'x'),(2,'a',20,2.25,'y'),"
                "(3,'b',30,NULL,NULL),(4,'b',NULL,4.00,'w')")))

(deftest scalar-functions-over-aggregates
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["20.00"] (col c 1 "SELECT round(avg(n),2) FROM ae")))
    (is (= ["40"]    (col c 1 "SELECT abs(sum(n)-100) FROM ae")))
    (is (= ["Y"]     (col c 1 "SELECT upper(max(s)) FROM ae")))
    (is (= ["1"]     (col c 1 "SELECT length(min(s)) FROM ae")))
    (is (= ["60"]    (col c 1 "SELECT greatest(sum(n),5) FROM ae")))
    (is (= ["121"]   (col c 1 "SELECT sum(n)*2+1 FROM ae"))
        "arithmetic over aggregates, which is all that used to work")
    (is (= ["many"]  (col c 1 (str "SELECT case when count(*)>2 then 'many' "
                                   "else 'few' end FROM ae")))
        "an aggregate in a CASE condition was refused outright")))

(deftest cast-over-an-aggregate-converts
  (with-open [c (jdbc)]
    (seed! c)
    ;; A cast over an aggregate used to be PEELED and dispatched as the
    ;; inner aggregate, on the grounds that it "only re-types the
    ;; result". It does not: `avg(n)::int` rounds.
    (is (= ["20"] (col c 1 "SELECT avg(n)::int FROM ae")))
    (is (= ["20"] (col c 1 "SELECT cast(avg(n) as int) FROM ae")))
    (is (= ["2.6"] (col c 1 "SELECT avg(m)::numeric(6,1) FROM ae")))
    (is (= ["4"] (col c 1 "SELECT count(*)::text FROM ae"))
        "and the shapes that DID work through the peel still do")
    (is (= ["integer"] (col c 1 "SELECT pg_typeof(count(*)::int4) FROM ae")))
    (is (= ["numeric"] (col c 1 "SELECT pg_typeof(stddev(n)) FROM ae"))
        "an aggregate inside a scalar call now collapses the group")))

(deftest filter-aggregates-nest-too
  (with-open [c (jdbc)]
    (seed! c)
    ;; JSqlParser surfaces `agg(x) FILTER (…)` as an AnalyticExpression,
    ;; which never reached the aggregate branch at all.
    (is (= ["51"] (col c 1 "SELECT sum(n) FILTER (WHERE n>10)+1 FROM ae")))
    (is (= ["0"]  (col c 1 (str "SELECT coalesce(sum(n) FILTER (WHERE n>100),0) "
                                "FROM ae"))))))

(deftest grouped-projections
  (with-open [c (jdbc)]
    (seed! c)
    (is (= [["a" "15.0"] ["b" "30.0"]]
           (rows c "SELECT dept, round(avg(n),1) FROM ae GROUP BY dept ORDER BY dept")))
    (is (= [["a" "32"] ["b" "32"]]
           (rows c "SELECT dept, sum(n)+count(*) FROM ae GROUP BY dept ORDER BY dept")))))

(deftest compound-projections-preserve-select-list-order
  (with-open [c (jdbc)]
    (seed! c)
    (with-open [st (.createStatement c)
                rs (.executeQuery
                    st
                    (str "SELECT sum(n)+1 AS after_sum, dept, max(n)-1 AS before_max "
                         "FROM ae GROUP BY dept ORDER BY dept"))]
      (let [md (.getMetaData rs)]
        (is (= ["after_sum" "dept" "before_max"]
               (mapv #(.getColumnLabel md %) (range 1 4)))))
      (is (= [["31" "a" "19"] ["31" "b" "29"]]
             (loop [acc []]
               (if (.next rs)
                 (recur (conj acc (mapv #(.getString rs %) (range 1 4))))
                 acc)))))))

(deftest having-over-an-expression
  (with-open [c (jdbc)]
    (seed! c)
    ;; HAVING was a shape matcher -- aggregate on the left, literal on
    ;; the right -- and silently dropped everything else, which returns
    ;; EVERY group rather than erroring.
    (is (= ["a"] (col c 1 (str "SELECT dept FROM ae GROUP BY dept "
                               "HAVING max(n) > min(n) ORDER BY dept"))))
    (is (= [] (col c 1 (str "SELECT dept FROM ae GROUP BY dept "
                            "HAVING NOT (count(*) > 1) ORDER BY dept"))))
    (is (= ["b"] (col c 1 (str "SELECT dept FROM ae GROUP BY dept "
                               "HAVING round(avg(n),0) > 20 ORDER BY dept"))))
    (is (= ["b"] (col c 1 (str "SELECT dept FROM ae GROUP BY dept "
                               "HAVING dept <> 'a' ORDER BY dept")))
        "a plain grouping column in HAVING, not an aggregate at all")
    (is (= ["a" "b"] (col c 1 (str "SELECT dept FROM ae GROUP BY dept "
                                   "HAVING count(*) BETWEEN 2 AND 2 ORDER BY dept"))))
    (testing "the shapes the matcher did handle still work"
      (is (= [["a" "2"] ["b" "2"]]
             (rows c (str "SELECT dept, count(*) FROM ae GROUP BY dept "
                          "HAVING count(*) > 1 ORDER BY dept"))))
      (is (= ["b"] (col c 1 (str "SELECT dept FROM ae GROUP BY dept "
                                 "HAVING sum(m) > 3.9 ORDER BY dept")))))))

(deftest having-does-not-see-select-aliases
  (with-open [c (jdbc)]
    (seed! c)
    ;; PostgreSQL resolves names in HAVING against the FROM relations
    ;; only -- GROUP BY and ORDER BY are the two clauses that also see
    ;; output names. It answers `column "total" does not exist`, and so
    ;; do we now; the old matcher accepted it.
    (is (thrown-with-msg?
         org.postgresql.util.PSQLException #"(?i)total"
         (col c 1 (str "SELECT dept, sum(n) AS total FROM ae GROUP BY dept "
                       "HAVING total > 1"))))))
