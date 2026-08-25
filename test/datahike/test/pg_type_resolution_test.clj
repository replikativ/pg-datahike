(ns datahike.test.pg-type-resolution-test
  "PostgreSQL resolves the type of CASE / COALESCE / GREATEST / LEAST and
   of an operator's arguments from three catalog facts: a type's
   CATEGORY, whether it is its category's PREFERRED type, and which
   coercions are IMPLICIT (pg_type.dat, pg_cast.dat). We guessed
   instead, and guessed two different things:

     coalesce(numeric, float8)   took the FIRST argument's type, so it
                                 printed 1.50 where PostgreSQL prints 1.5
     WHERE bool_col = 10         compared anything against anything and
                                 answered, where PostgreSQL raises 42883

   The second is the one that matters for a driver: `operator does not
   exist: boolean = integer` is part of the contract, and a probe that
   tests for it got a row count.

   Expectations are a PostgreSQL 17 oracle's, including the messages."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.types :as types])
  (:import [java.sql Connection DriverManager]))

(def ^:dynamic *port* nil)

(defn tr-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"tr" conn} {:port 0})]
      (try
        (binding [*port* (.getPort server)] (f))
        (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each tr-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/tr?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- col [^Connection c n sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (loop [acc []] (if (.next rs) (recur (conj acc (.getString rs (int n)))) acc))))

(defn- seed! [^Connection c]
  (exec! c (str "CREATE TABLE tr (id int, n numeric(8,2), f float8, i int, "
                "b boolean, s text, d date, ts timestamp)"))
  (exec! c (str "INSERT INTO tr VALUES "
                "(1,1.50,1.5,10,true,'a','2020-01-01','2020-01-01 10:00:00'),"
                "(2,2.25,NULL,NULL,false,NULL,NULL,NULL)")))

(deftest the-tables-say-what-the-catalog-says
  ;; The algorithm is only as good as the two tables it reads, so check
  ;; them directly as well as through SQL.
  (testing "numeric loses to float8, not the other way round"
    ;; numeric -> float8 is implicit; float8 -> numeric is ASSIGNMENT.
    (is (= types/oid-float8 (types/select-common-type [types/oid-numeric types/oid-float8])))
    (is (= types/oid-float4 (types/select-common-type [types/oid-numeric types/oid-float4]))))
  (testing "the preferred type of a category is never given up"
    ;; oid is category N's preferred type, so int4 gives way to it.
    (is (= types/oid-oid (types/select-common-type [types/oid-int4 types/oid-oid]))))
  (testing "widening within the integers"
    (is (= types/oid-int8 (types/select-common-type [types/oid-int2 types/oid-int8])))
    (is (= types/oid-numeric (types/select-common-type [types/oid-int4 types/oid-numeric]))))
  (testing "date widens to timestamp"
    (is (= types/oid-timestamp
           (types/select-common-type [types/oid-date types/oid-timestamp]))))
  (testing "an operator needs a candidate BOTH arguments reach"
    (is (false? (types/comparison-compatible? types/oid-bool types/oid-int4)))
    (is (false? (types/comparison-compatible? types/oid-text types/oid-int4)))
    (is (false? (types/comparison-compatible? types/oid-jsonb types/oid-text)))
    ;; Category alone is not enough: date and time are both category D
    ;; and neither coerces to the other, so PostgreSQL has no candidate.
    (is (false? (types/comparison-compatible? types/oid-date types/oid-time)))
    (is (true? (types/comparison-compatible? types/oid-date types/oid-timestamp)))
    (is (true? (types/comparison-compatible? types/oid-int2 types/oid-numeric))))
  (testing "an untyped literal takes the other side's type"
    (is (true? (types/comparison-compatible? nil types/oid-bool))))
  (testing "declared character types round-trip through schema metadata"
    (is (= types/oid-bpchar (get types/pg-name->oid "bpchar"))))
  (testing "coercibility is not a substitute for an operator catalog"
    (is (false? (types/comparison-compatible? '= types/oid-json types/oid-json)))
    (is (false? (types/comparison-compatible? '= types/oid-json-array types/oid-json-array)))
    (is (true? (types/comparison-compatible? '= types/oid-jsonb types/oid-jsonb)))))

(deftest common-type-of-a-construct
  (with-open [c (jdbc)]
    (seed! c)
    (is (= ["1.5" "2.25"] (col c 2 "SELECT id, coalesce(n,f) FROM tr ORDER BY id"))
        "float8 wins, so the numeric's scale goes with it")
    (is (= ["10" "2.25"] (col c 2 "SELECT id, coalesce(i,n) FROM tr ORDER BY id")))
    (is (= ["1.5" "2.25"] (col c 2 "SELECT id, greatest(n,f) FROM tr ORDER BY id")))
    (is (= ["1.50" "2.25"] (col c 2 "SELECT id, least(n,i) FROM tr ORDER BY id")))
    (is (= ["1.5" nil] (col c 2 "SELECT id, case when id=1 then n else f end FROM tr ORDER BY id")))
    (is (= ["2020-01-01 00:00:00" nil] (col c 2 "SELECT id, coalesce(d,ts) FROM tr ORDER BY id"))
        "date widens to timestamp and renders as one")
    (testing "and the reported type follows the same rule"
      (is (= ["double precision"] (col c 1 "SELECT pg_typeof(coalesce(n,f)) FROM tr LIMIT 1")))
      (is (= ["numeric"] (col c 1 "SELECT pg_typeof(coalesce(i,n)) FROM tr LIMIT 1"))))
    (testing "CASE gives ELSE the most significant type position"
      ;; varchar and bpchar coerce implicitly in both directions and neither
      ;; is preferred. PostgreSQL therefore keeps whichever one it sees
      ;; first -- and parse_expr.c deliberately prepends ELSE.
      (exec! c "CREATE TABLE tr_chars (v varchar(4), c char(4))")
      (exec! c "INSERT INTO tr_chars VALUES ('v', 'c')")
      (is (= ["character"]
             (col c 1 "SELECT pg_typeof(CASE WHEN true THEN v ELSE c END) FROM tr_chars")))
      (is (= ["character varying"]
             (col c 1 "SELECT pg_typeof(CASE WHEN true THEN c ELSE v END) FROM tr_chars"))))
    (testing "a parameterized table-free CASE is not constant-folded at Parse"
      (with-open [st (.prepareStatement
                      c
                      "SELECT CASE WHEN CAST(? AS numeric) IS NULL THEN 0 ELSE CAST(? AS numeric) END")]
        (.setBigDecimal st 1 (bigdec 123))
        (.setBigDecimal st 2 (bigdec 123))
        (with-open [rs (.executeQuery st)]
          (is (.next rs))
          (is (= "123" (.getString rs 1))))))
    (testing "an untyped literal is UNKNOWN, not text"
      ;; `expr-oid` answers text for a quoted literal because that is
      ;; right for a projection; using it here would make every mixed
      ;; construct a category mismatch.
      (is (= ["a" "z"] (col c 2 "SELECT id, coalesce(s,'z') FROM tr ORDER BY id")))
      (is (= [nil "2"] (col c 2 "SELECT id, case when id=1 then null else 2 end FROM tr ORDER BY id"))))
    (testing "a genuine category mismatch is an error"
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException
           #"COALESCE types text and integer cannot be matched"
           (col c 1 "SELECT coalesce(s, i) FROM tr"))))))

(deftest cast-target-must-exist
  (with-open [c (jdbc)]
    (doseq [sql ["SELECT 'ignore'::typedoesnotexist"
                 "SELECT NULL::typedoesnotexist"]]
      (let [e (try (col c 1 sql) nil
                   (catch java.sql.SQLException e e))]
        (is (some? e) sql)
        (is (= "42704" (.getSQLState e)) sql)
        (is (re-find #"type .* does not exist" (.getMessage e)) sql)))))

(deftest money-cast-keeps-its-postgresql-type
  (with-open [c (jdbc)
              st (.createStatement c)
              rs (.executeQuery st "SELECT 66::money")]
    (is (.next rs))
    (is (= "money" (.getColumnTypeName (.getMetaData rs) 1)))
    (is (= "66.00" (.getString rs 1))))
  (with-open [c (jdbc)]
    (is (= ["money"] (col c 1 "SELECT pg_typeof(66::money)::text")))
    (is (= ["123.46" "-123456.78"]
           (col c 1 (str "SELECT '$123.455'::money "
                         "UNION ALL SELECT '($123,456.78)'::money"))))))

(deftest postgres-money-comparison-slice
  ;; PostgreSQL 17 src/test/regress/sql/money.sql lines 32-45. This is an
  ;; admitted strict semantic slice; locale rendering and cash_words are
  ;; separate compatibility boundaries.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE money_data (m money)")
    (exec! c "INSERT INTO money_data VALUES ('123')")
    (doseq [sql ["SELECT m = '$123.00' FROM money_data"
                 "SELECT m != '$124.00' FROM money_data"
                 "SELECT m <= '$123.00' FROM money_data"
                 "SELECT m >= '$123.00' FROM money_data"
                 "SELECT m < '$124.00' FROM money_data"
                 "SELECT m > '$122.00' FROM money_data"]]
      (is (= ["t"] (col c 1 sql)) sql))
    (doseq [sql ["SELECT m = '$123.01' FROM money_data"
                 "SELECT m != '$123.00' FROM money_data"
                 "SELECT m <= '$122.99' FROM money_data"
                 "SELECT m >= '$123.01' FROM money_data"
                 "SELECT m > '$124.00' FROM money_data"
                 "SELECT m < '$122.00' FROM money_data"]]
      (is (= ["f"] (col c 1 sql)) sql))))

(deftest postgres-money-assignment-slice
  ;; PostgreSQL 17 src/test/regress/sql/money.sql lines 53-73.
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE money_input (m money)")
    (doseq [[input expected] [["$123.45" "123.45"]
                              ["$123.451" "123.45"]
                              ["$123.454" "123.45"]
                              ["$123.455" "123.46"]
                              ["$123.456" "123.46"]
                              ["$123.459" "123.46"]]]
      (exec! c "DELETE FROM money_input")
      (with-open [ps (.prepareStatement c "INSERT INTO money_input VALUES (?)")]
        (.setString ps 1 input)
        (.executeUpdate ps))
      (is (= [expected] (col c 1 "SELECT m FROM money_input")) input))))

(deftest postgres-money-arithmetic-slice
  ;; PostgreSQL 17 src/test/regress/sql/money.sql lines 108-118 and 139-148.
  (with-open [c (jdbc)]
    (with-open [st (.createStatement c)
                rs (.executeQuery st (str "SELECT '1'::money + '2'::money, "
                                          "'1'::money / '2'::money"))]
      (is (.next rs))
      (is (= "money" (.getColumnTypeName (.getMetaData rs) 1)))
      (is (= "float8" (.getColumnTypeName (.getMetaData rs) 2)))
      (is (= "3.00" (.getString rs 1)))
      (is (= "0.5" (.getString rs 2))))
    (doseq [[sql expected] [["SELECT '878.08'::money / 11::float8" "79.83"]
                            ["SELECT '878.08'::money / 11::float4" "79.83"]
                            ["SELECT '878.08'::money / 11::bigint" "79.82"]
                            ["SELECT '878.08'::money / 11::int" "79.82"]
                            ["SELECT '878.08'::money / 11::smallint" "79.82"]
                            ["SELECT '90000000000000099.00'::money / 10::bigint"
                             "9000000000000009.90"]]]
      (is (= [expected] (col c 1 sql)) sql))
    (doseq [sql ["SELECT '92233720368547758.07'::money + '0.01'::money"
                 "SELECT '-92233720368547758.08'::money - '0.01'::money"
                 "SELECT '92233720368547758.07'::money * 2::float8"
                 "SELECT '-1'::money / 1.175494e-38::float4"
                 "SELECT '92233720368547758.07'::money * 2::int4"
                 "SELECT '42'::money * 'inf'::float8"
                 "SELECT '42'::money * '-inf'::float8"
                 "SELECT '42'::money * 'nan'::float4"]]
      (let [e (try (col c 1 sql) nil (catch java.sql.SQLException e e))]
        (is (= "22003" (.getSQLState e)) sql)
        (is (= "ERROR: money out of range" (.getMessage e)) sql)))
    (let [sql "SELECT '1'::money / 0::int2"
          e (try (col c 1 sql) nil (catch java.sql.SQLException e e))]
      (is (= "22012" (.getSQLState e)) sql))))

(deftest operator-resolution
  (with-open [c (jdbc)]
    (seed! c)
    (testing "no candidate operator is an error, not an answer"
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException
           #"operator does not exist: json = json"
           (col c 1 "SELECT '{}'::json = '{}'::json")))
      (doseq [sql ["SELECT true IN (1,2)"
                   "SELECT 1 WHERE true IN (1,2)"
                   "SELECT CASE true WHEN 1 THEN 'x' ELSE 'y' END"
                   "SELECT true IS DISTINCT FROM 1"
                   "SELECT 1 WHERE true IS DISTINCT FROM 1"]]
        (is (thrown-with-msg?
             org.postgresql.util.PSQLException
             #"operator does not exist: boolean = integer"
             (col c 1 sql))
            sql))
      (doseq [sql ["SELECT true BETWEEN 0 AND 2"
                   "SELECT 1 WHERE true BETWEEN 0 AND 2"]]
        (is (thrown-with-msg?
             org.postgresql.util.PSQLException
             #"operator does not exist: boolean >= integer"
             (col c 1 sql))
            sql))
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException
           #"operator does not exist: boolean = integer"
           (col c 1 "SELECT id FROM tr WHERE b = 10")))
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException
           #"operator does not exist: text = integer"
           (col c 1 "SELECT id FROM tr WHERE s = 1")))
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException
           #"operator does not exist: boolean <> integer"
           (col c 1 "SELECT id FROM tr WHERE b <> i"))))
    (testing "including inside a correlated subquery, which PostgreSQL rejects too"
      ;; The deferred evaluator answers NULL for anything that fails to
      ;; translate -- right for the catalog probes it was written for,
      ;; wrong for an error PostgreSQL itself raises.
      (is (thrown-with-msg?
           org.postgresql.util.PSQLException
           #"operator does not exist: boolean = integer"
           (col c 1 (str "SELECT id, (SELECT count(*) FROM tr t2 WHERE t2.b = tr.i) "
                         "FROM tr")))))
    (testing "and the comparisons PostgreSQL does have still work"
      (is (= ["1"] (col c 1 "SELECT id FROM tr WHERE b = true")))
      (is (= ["1"] (col c 1 "SELECT id FROM tr WHERE s = 'a'")))
      (is (= ["1"] (col c 1 "SELECT id FROM tr WHERE i = 10")))
      (is (= [] (col c 1 "SELECT id FROM tr WHERE d = ts"))
          "date = timestamp resolves through timestamp")
      (is (= ["1"] (col c 1 "SELECT id FROM tr WHERE n = 1.50")))
      (is (= ["1"] (col c 1 "SELECT id FROM tr WHERE f > 1"))))))
