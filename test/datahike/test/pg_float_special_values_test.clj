(ns datahike.test.pg-float-special-values-test
  "NaN and +-Infinity for float4 / float8.

   None of them could enter the system: `'NaN'::float8` raised 22P02,
   which meant the NaN-handling already written in sql/fns.clj was
   unreachable code. Three things had to land together:

   ACCEPTANCE. float8in takes NaN / Infinity / inf, case-insensitively
   and with surrounding whitespace (float.c float8in_internal).

   COMPARISON. PostgreSQL gives floats a TOTAL order: NaN equals itself
   and sorts above everything (float.c float8_cmp_internal). IEEE-754 --
   and so Clojure -- disagrees on every one of those.

   ORDERING, in the same change and not after. Clojure's `compare`
   routes numbers through an lt-based comparison, so NaN compares EQUAL
   to everything and `(sort [1.0 ##NaN 0.5 2.0])` returns its input
   unsorted. Letting NaN in without fixing that would have turned
   ORDER BY into a silent wrong answer.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn sp-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"s" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each sp-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/s?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest special-values-parse-and-render
  (with-open [c (jdbc)]
    (is (= "NaN" (one c "SELECT 'NaN'::float8")))
    (is (= "Infinity" (one c "SELECT 'Infinity'::float8")))
    (is (= "-Infinity" (one c "SELECT '-Infinity'::float8")))
    (testing "the short spellings and case-insensitivity PostgreSQL accepts"
      (is (= "Infinity" (one c "SELECT 'inf'::float8")))
      (is (= "-Infinity" (one c "SELECT '-inf'::float8")))
      (is (= "NaN" (one c "SELECT 'nan'::float8")))
      (is (= "NaN" (one c "SELECT '  NaN  '::float8"))))
    (testing "and for real too"
      (is (= "NaN" (one c "SELECT 'NaN'::real")))
      (is (= "Infinity" (one c "SELECT 'Infinity'::real"))))))

(deftest nan-compares-as-postgres-orders-it
  (with-open [c (jdbc)]
    (is (= "t" (one c "SELECT 'NaN'::float8 = 'NaN'::float8"))
        "equal to itself, unlike IEEE-754")
    (is (= "t" (one c "SELECT 'NaN'::float8 > 'Infinity'::float8"))
        "and greater than everything, including Infinity")
    (is (= "f" (one c "SELECT 'NaN'::float8 < 1")))
    (is (= "t" (one c "SELECT 'Infinity'::float8 > 1e308")))))

(deftest ordering-over-nan-is-a-total-order
  (with-open [c (jdbc)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE o (id int, v float8)")
      (.execute st (str "INSERT INTO o VALUES (1,'NaN'),(2,1),(3,'-Infinity'),"
                        "(4,0.5),(5,'Infinity')")))
    (testing "sorted through the server-side ORDER BY path. Clojure's
              compare answers 0 for every pair involving NaN, so this
              came back in insertion order."
      (is (= ["-Infinity" "0.5" "1" "Infinity" "NaN"]
             (with-open [st (.createStatement c)
                         rs (.executeQuery st "SELECT v FROM o ORDER BY v")]
               (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc))))))
    (testing "and descending"
      (is (= ["NaN" "Infinity" "1" "0.5" "-Infinity"]
             (with-open [st (.createStatement c)
                         rs (.executeQuery st "SELECT v FROM o ORDER BY v DESC")]
               (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs 1))) acc))))))
    (testing "min/max use the same order"
      (is (= "NaN" (one c "SELECT max(v) FROM o")))
      (is (= "-Infinity" (one c "SELECT min(v) FROM o"))))))

(deftest infinities-propagate-through-arithmetic
  (with-open [c (jdbc)]
    (testing "an operand that is ALREADY infinite is not an overflow"
      (is (= "Infinity" (one c "SELECT 'Infinity'::float8 * 2")))
      (is (= "Infinity" (one c "SELECT 'Infinity'::float8 + 1"))))
    (is (= "NaN" (one c "SELECT 'Infinity'::float8 - 'Infinity'::float8")))
    (is (= "NaN" (one c "SELECT abs('NaN'::float8)")))))

(deftest out-of-range-literals-raise
  (with-open [c (jdbc)]
    (testing "a literal too large or too small for the target is an error,
              not a silent Infinity or zero"
      (is (thrown-with-msg? SQLException #"out of range for type double precision"
                            (one c "SELECT 1e400::float8")))
      (is (thrown-with-msg? SQLException #"out of range for type double precision"
                            (one c "SELECT 1e-400::float8"))))
    (testing "and a special value cannot become an integer"
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (one c "SELECT 'NaN'::float8::int")))
      (is (thrown-with-msg? SQLException #"integer out of range"
                            (one c "SELECT 'Infinity'::float8::int"))))))

(deftest stored-specials-filter-correctly
  (with-open [c (jdbc)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE nx (id int, v float8)")
      (.execute st (str "INSERT INTO nx VALUES (1,'NaN'),(2,1),(3,'-Infinity'),"
                        "(4,0.5),(5,'Infinity'),(6,2),(7,3)")))
    (testing "worth pinning explicitly: Datahike's own AVET index does not
              order a stored NaN correctly (its comparator has the same
              lt-based flaw), so the question was whether a range filter
              could read through that index and silently miss rows. It
              cannot -- float comparisons lower to predicates, not to
              index seeks -- and these agree with PostgreSQL."
      (is (= "1,2,4,5,6,7"
             (one c "SELECT string_agg(id::text, ',' ORDER BY id) FROM nx WHERE v > 0"))
          "NaN is greater than 0, so row 1 belongs here")
      (is (= "2,3,4"
             (one c "SELECT string_agg(id::text, ',' ORDER BY id) FROM nx WHERE v < 2"))
          "and it is NOT less than 2 -- NaN is above everything, not below")
      (is (= "1" (one c "SELECT string_agg(id::text, ',' ORDER BY id) FROM nx WHERE v = 'NaN'::float8")))
      (is (= "1" (one c "SELECT count(*)::text FROM nx WHERE v > 'Infinity'::float8"))
          "only NaN is above Infinity"))))

(deftest comparison-predicates-still-filter-nulls
  (with-open [c (jdbc)]
    (with-open [st (.createStatement c)]
      (.execute st "CREATE TABLE cs (i int, f float8)")
      (.execute st "INSERT INTO cs VALUES (1,10.1),(2,20.2),(3,0),(4,NULL)"))
    (testing "the NaN-aware comparisons must NOT be null-safe-wrapped: that
              wrapper yields the :__null__ sentinel, which is TRUTHY in a
              datalog predicate position, so a NULL operand let the row
              through instead of filtering it. SQL says UNKNOWN and WHERE
              treats UNKNOWN as FALSE."
      (is (= "2" (one c "SELECT string_agg(i::text, ',' ORDER BY i) FROM cs WHERE NULLIF(f, 10.1) > 0"))
          "row 1 nullifies to NULL and row 4 is already NULL; neither passes")
      (is (= "1,2" (one c "SELECT string_agg(i::text, ',' ORDER BY i) FROM cs WHERE f > 0")))
      (is (nil? (one c "SELECT string_agg(i::text, ',' ORDER BY i) FROM cs WHERE f > NULL"))))))
