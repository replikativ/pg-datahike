(ns datahike.fuzz.differential
  "Differential fuzzer: generate SQL, run it on a real PostgreSQL (the
   ORACLE) and on pg-datahike (the TARGET), and diff the answers.

   Hand-written tests check what their author already suspected; this checks
   what nobody thought of. Its first run found 58 disagreements, two of them
   regressions the hand-written batteries of the same PRs had missed (#75),
   and it drove the correctness work through #96.

   Three surfaces, one fixed schema (NULL-heavy, every common type):

     :select    ~50 query classes -- projections, predicates, joins,
                aggregates, windows, CASE, casts, arrays, jsonb, CTEs,
                correlation, LATERAL, recursion -- over the simple protocol.
     :prepared  parameterised shapes over the EXTENDED protocol
                (Parse/Bind/Describe/Execute), which psql never exercises.
     :dml       INSERT/UPDATE/DELETE: both sides are re-seeded, run the
                statement, and the reported row count and resulting table
                are compared.

   What counts as agreement: identical rows (as text, in order -- every
   generated query orders fully), or BOTH sides failing with the same
   SQLSTATE. Error wording is not compared. A failure where PostgreSQL
   answers, or a different SQLSTATE -- above all our XX000 where PostgreSQL
   raises something specific -- is a disagreement: the beta rule is 'no
   silent wrong answers or internal failures'.

   Generation is seeded (java.util.Random), so a seed reproduces its
   queries exactly. `run` compares the disagreements against the manifest
   in test/integration/fuzz/expected-divergences.edn: an unlisted one is a
   regression, a listed one that now agrees is baseline drift. Both fail.

   Entry points:
     clojure -M:dev -m datahike.fuzz.differential [select|prepared|dml|all] [n] [seed]
     bb fuzz ...                      (same arguments)
     (report (run-surface :select 1500 42))   ; from a REPL
   Endpoints come from REFERENCE_URL / TARGET_URL (the cross-engine
   convention); the defaults are the local oracle and dev server."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.sql Connection DriverManager SQLException]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Endpoints
;; ---------------------------------------------------------------------------

(def ^:private default-reference
  "jdbc:postgresql://127.0.0.1:15998/postgres?user=pgtest&password=pgtest")

(def ^:private default-target
  "jdbc:postgresql://127.0.0.1:15432/datahike?user=datahike&password=datahike")

(defn- connect ^Connection [url]
  ;; Text results on both sides: the comparison is of rendered values, and
  ;; binary transfer would compare driver decoding instead.
  (DriverManager/getConnection
   (str url (if (str/includes? url "?") "&" "?")
        "sslmode=disable&binaryTransfer=false")))

(defn reference-conn ^Connection []
  (connect (or (System/getenv "REFERENCE_URL") default-reference)))

(defn target-conn ^Connection []
  (connect (or (System/getenv "TARGET_URL") default-target)))

;; ---------------------------------------------------------------------------
;; Execution
;; ---------------------------------------------------------------------------

(defn- sqlstate [^Exception e]
  (or (when (instance? SQLException e) (.getSQLState ^SQLException e)) "?????"))

(defn- read-rows [^java.sql.ResultSet rs]
  (let [n (.. rs getMetaData getColumnCount)]
    (loop [acc []]
      (if (.next rs)
        (recur (conj acc (mapv (fn [^long ix] (.getString rs ix)) (range 1 (inc n)))))
        acc))))

(defn q
  "Run `sql`; [:rows rows] or [:error sqlstate]."
  [^Connection c sql]
  (try
    (with-open [st (.createStatement c)
                rs (.executeQuery st sql)]
      [:rows (read-rows rs)])
    (catch Exception e [:error (sqlstate e)])))

(defn exec!
  "Run a statement; [:ok update-count] or [:error sqlstate]."
  [^Connection c sql]
  (try
    (with-open [st (.createStatement c)]
      (.execute st sql)
      [:ok (.getUpdateCount st)])
    (catch Exception e [:error (sqlstate e)])))

(defn- prep-q
  "Run `sql` as a prepared statement with `params` bound (extended protocol)."
  [^Connection c sql params]
  (try
    (with-open [st (.prepareStatement c sql)]
      (dotimes [i (count params)]
        (let [v (nth params i)]
          (if (nil? v)
            (.setNull st (int (inc i)) java.sql.Types/INTEGER)
            (.setObject st (int (inc i)) v))))
      (with-open [rs (.executeQuery st)]
        [:rows (read-rows rs)]))
    (catch Exception e [:error (sqlstate e)])))

;; ---------------------------------------------------------------------------
;; Fixture
;; ---------------------------------------------------------------------------

(def setup
  ["DROP TABLE IF EXISTS ft" "DROP TABLE IF EXISTS fu"
   (str "CREATE TABLE ft (id int, i int, j int, s text, b boolean, f float8, n numeric, "
        "d date, ts timestamp, arr int[], js jsonb, sm smallint, bg bigint)")
   (str "INSERT INTO ft VALUES "
        "(1,10,20,'aa',true,1.5,1.50,'2020-01-01','2020-01-01 10:20:30','{1,2,3}','{\"a\":1}',1,100),"
        "(2,NULL,20,'bb',false,NULL,2.25,NULL,NULL,NULL,NULL,NULL,NULL),"
        "(3,10,NULL,NULL,NULL,-0.5,NULL,'2021-06-15','2021-06-15 00:00:00','{}','{}',-1,-100),"
        "(4,-3,0,'dd',true,0.0,0.00,'2019-12-31','2019-12-31 23:59:59','{4,NULL}','{\"a\":null}',32767,9223372036854775807),"
        "(5,0,7,'',false,2.5,10,'2022-02-28','2022-02-28 12:00:00','{5}','[1,2]',-32768,-9223372036854775808)")
   "CREATE TABLE fu (id int, k int, v text)"
   "INSERT INTO fu VALUES (1,10,'ten'),(2,NULL,'null-k'),(3,99,NULL)"])

(def value-setup
  ;; Types whose rendering depends on the TYPE, not the JVM value: they
  ;; share carriers (money/numeric, time as text, instants for every
  ;; datetime), so a renderer that dispatches on the value's class gets
  ;; them wrong. Fractions, offsets and negatives are deliberate. Read-only,
  ;; so seeded once per run, not per DML sample.
  ["DROP TABLE IF EXISTS fv"
   "CREATE TABLE fv (id int, t time, ttz timetz, tz timestamptz, m money, iv interval, d date, ts timestamp)"
   (str "INSERT INTO fv VALUES "
        "(1,'10:00','10:00+02','2020-01-01 10:00:00+00:00',1234.5,'1 day','2020-01-01','2020-01-01 10:00:00'),"
        "(2,'23:59:59.5','00:00:00-03:30','2021-06-15 23:30:00.25+00:00',-0.75,'02:03:04','2021-06-15','2021-06-15 00:00:00.125'),"
        "(3,NULL,NULL,NULL,NULL,NULL,NULL,NULL)")])

(defn- seed!
  "Run the fixture statements (default: the mutable `ft`/`fu` tables, which
   the DML surface re-seeds before every sample)."
  [^Connection c & [statements]]
  (doseq [s (or statements setup)]
    (let [[status state :as r] (exec! c s)]
      (when (= :error status)
        (throw (ex-info (str "fixture statement failed (" state "): " s) {:result r}))))))

(defn- table-state [^Connection c]
  (q c "SELECT id, i, j, s, b, f, n, d FROM ft ORDER BY id"))

;; ---------------------------------------------------------------------------
;; Generators
;; ---------------------------------------------------------------------------

(def ^:private cols-num ["i" "j" "f" "n" "id"])
(def ^:private cols-any ["i" "j" "s" "b" "f" "n" "d" "id"])
(def ^:private cmp ["=" "<>" "<" ">" "<=" ">="])
(def ^:private arith ["+" "-" "*" "/"])

(defn gen-select
  "One [class sql] SELECT sample."
  [^java.util.Random r]
  (let [pick (fn [v] (nth v (.nextInt r (count v))))
        num  #(pick (into cols-num ["1" "0" "-1" "2.5" "10"]))
        any  #(pick cols-any)
        txt  #(pick ["s" "'aa'" "'%a%'" "''"])]
    (case (.nextInt r 58)
      0  [:cmp-proj  (format "SELECT id, %s %s %s AS c FROM ft ORDER BY id" (num) (pick cmp) (num))]
      1  [:cmp-where (format "SELECT id FROM ft WHERE %s %s %s ORDER BY id" (num) (pick cmp) (num))]
      2  [:arith     (format "SELECT id, %s %s %s AS c FROM ft ORDER BY id" (num) (pick arith) (num))]
      3  [:bool-conn (format "SELECT id, (%s %s %s) %s (%s %s %s) AS c FROM ft ORDER BY id"
                             (num) (pick cmp) (num) (pick ["AND" "OR"]) (num) (pick cmp) (num))]
      4  [:not-where (format "SELECT id FROM ft WHERE NOT (%s %s %s %s %s %s %s) ORDER BY id"
                             (num) (pick cmp) (num) (pick ["AND" "OR"]) (num) (pick cmp) (num))]
      5  [:agg       (format "SELECT %s(%s) FROM ft" (pick ["count" "sum" "min" "max" "avg"]) (num))]
      6  [:group     (format "SELECT %s, %s(%s) FROM ft GROUP BY 1 ORDER BY 1"
                             (any) (pick ["count" "sum" "min" "max"]) (num))]
      7  [:case      (format "SELECT id, CASE WHEN %s %s %s THEN %s ELSE %s END AS c FROM ft ORDER BY id"
                             (num) (pick cmp) (num) (pick ["1" "NULL" "0"]) (pick ["2" "NULL"]))]
      8  (let [fname (pick ["abs" "coalesce" "greatest" "least" "nullif"])]
           [:func (if (= fname "abs")
                    (format "SELECT id, abs(%s) AS c FROM ft ORDER BY id" (num))
                    (format "SELECT id, %s(%s, %s) AS c FROM ft ORDER BY id" fname (num) (num)))])
      9  [:join      (format "SELECT ft.id, fu.v FROM ft %s fu ON ft.i = fu.k ORDER BY ft.id, fu.v"
                             (pick ["JOIN" "LEFT JOIN"]))]
      10 [:in        (format "SELECT id FROM ft WHERE %s %s (10, 20, NULL) ORDER BY id"
                             (num) (pick ["IN" "NOT IN"]))]
      11 [:cast      (format "SELECT id, %s::%s AS c FROM ft ORDER BY id" (any)
                             (pick ["int" "text" "float8" "numeric" "bool"]))]
      12 [:order     (format "SELECT id, %s AS c FROM ft ORDER BY 2 %s %s, id" (any)
                             (pick ["ASC" "DESC"]) (pick ["" "NULLS FIRST" "NULLS LAST"]))]
      13 [:sub       (format "SELECT id FROM ft WHERE %s %s (SELECT %s(%s) FROM ft) ORDER BY id"
                             (num) (pick cmp) (pick ["max" "min" "avg"]) (num))]
      14 [:strfn     (format "SELECT id, %s AS c FROM ft ORDER BY id"
                             (pick [(format "upper(%s)" (txt)) (format "lower(%s)" (txt))
                                    (format "length(%s)" (txt)) (format "trim(%s)" (txt))
                                    (format "substring(%s from 1 for 2)" (txt))
                                    (format "%s || %s" (txt) (txt))
                                    (format "replace(%s, 'a', 'z')" (txt))
                                    (format "position('a' in %s)" (txt))]))]
      15 [:like      (format "SELECT id FROM ft WHERE %s %s %s ORDER BY id"
                             (txt) (pick ["LIKE" "NOT LIKE" "ILIKE"]) (pick ["'a%'" "'%a%'" "'aa'" "'_a'"]))]
      16 [:isnull    (format "SELECT id FROM ft WHERE %s %s ORDER BY id" (any)
                             (pick ["IS NULL" "IS NOT NULL"]))]
      17 [:distinct  (format "SELECT DISTINCT %s AS c FROM ft ORDER BY 1" (any))]
      18 [:having    (format "SELECT %s AS g, count(*) AS n FROM ft GROUP BY 1 HAVING count(*) %s 1 ORDER BY 1"
                             (any) (pick cmp))]
      19 [:limit     (format "SELECT id FROM ft ORDER BY id %s"
                             (pick ["LIMIT 2" "LIMIT 2 OFFSET 1" "OFFSET 3" "LIMIT 0" "LIMIT 100"]))]
      20 [:setop     (format "SELECT id FROM ft %s SELECT k FROM fu ORDER BY 1"
                             (pick ["UNION" "UNION ALL" "INTERSECT" "EXCEPT"]))]
      21 [:window    (format "SELECT id, %s(%s) OVER (%s%s%s) AS c FROM ft ORDER BY id"
                             (pick ["sum" "avg" "count" "min" "max" "array_agg"
                                    "first_value" "last_value" "stddev"])
                             (pick ["i" "j" "n" "f" "bg" "sm" "s"])
                             (pick ["" "PARTITION BY b " "PARTITION BY j "])
                             (pick ["" "ORDER BY i" "ORDER BY i DESC"
                                    "ORDER BY i NULLS FIRST" "ORDER BY j, id"])
                             (pick ["" " ROWS BETWEEN 1 PRECEDING AND CURRENT ROW"
                                    " ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING"
                                    " ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING"
                                    " ROWS UNBOUNDED PRECEDING"
                                    " RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW"
                                    " ROWS BETWEEN 1 FOLLOWING AND 2 FOLLOWING"]))]
      22 [:exists    (format "SELECT id FROM ft WHERE %sEXISTS (SELECT 1 FROM fu WHERE fu.k = ft.i) ORDER BY id"
                             (pick ["" "NOT "]))]
      23 [:insubq    (format "SELECT id FROM ft WHERE i %s (SELECT k FROM fu) ORDER BY id"
                             (pick ["IN" "NOT IN"]))]
      24 [:between   (format "SELECT id FROM ft WHERE %s %s %s AND %s ORDER BY id"
                             (num) (pick ["BETWEEN" "NOT BETWEEN"]) (num) (num))]
      25 [:date      (format "SELECT id, %s AS c FROM ft ORDER BY id"
                             (pick ["extract(year from d)" "extract(month from d)"
                                    "date_trunc('month', d)" "d + 1" "d - 1"
                                    "d::text" "to_char(d, 'YYYY-MM-DD')"]))]
      26 [:groupmulti (format "SELECT %s AS g1, %s AS g2, count(*) AS n FROM ft GROUP BY 1,2 ORDER BY 1,2"
                              (any) (any))]
      27 [:selfjoin  (format "SELECT a.id, b.id FROM ft a JOIN ft b ON a.i = b.i %s ORDER BY 1, 2"
                             (pick ["" "AND a.id <> b.id"]))]
      28 [:array     (format "SELECT id, %s AS c FROM ft ORDER BY id"
                             (pick ["arr" "array_length(arr,1)" "cardinality(arr)"
                                    "arr[1]" "array_upper(arr,1)" "array_lower(arr,1)"
                                    "array_append(arr, 9)" "array_prepend(0, arr)"
                                    "array_cat(arr, arr)" "array_to_string(arr, '-')"
                                    "array_position(arr, 2)" "array_remove(arr, 1)"]))]
      29 [:arraypred (format "SELECT id FROM ft WHERE %s ORDER BY id"
                             (pick ["2 = ANY(arr)" "2 <> ALL(arr)" "arr @> ARRAY[1]"
                                    "arr && ARRAY[1,9]" "arr = ARRAY[1,2,3]"
                                    "array_length(arr,1) > 1" "arr IS NULL"]))]
      30 [:jsonb     (format "SELECT id, %s AS c FROM ft ORDER BY id"
                             (pick ["js" "js->'a'" "js->>'a'" "jsonb_typeof(js)"
                                    "js ? 'a'" "jsonb_array_length(js)"
                                    "js || '{\"z\":9}'" "js - 'a'"]))]
      31 [:cte       (format "WITH w AS (SELECT id, %s AS v FROM ft) SELECT id, v FROM w ORDER BY id"
                             (num))]
      32 [:coerce    (format "SELECT id, %s AS c FROM ft ORDER BY id"
                             (pick ["i + n" "i + f" "n + f" "sm + i" "bg + i" "sm * sm"
                                    "i / n" "n / f" "i::numeric / 3" "f + 1"
                                    "greatest(i, n)" "coalesce(n, f)" "least(sm, bg)"]))]
      33 [:numedge   (format "SELECT %s" (pick ["2147483647 + 1" "(-2147483648) - 1"
                                                "9223372036854775807 + 1" "1/0" "1.0/0"
                                                "'NaN'::float8 + 1" "'Infinity'::float8 * 0"
                                                "0.1 + 0.2" "round(2.5)" "round(-2.5)"
                                                "trunc(-2.5)" "mod(-7, 3)" "(-7) % 3"]))]
      ;; PostgreSQL requires the DISTINCT ON expressions to lead ORDER BY.
      34 (let [k (any)]
           [:distincton (format "SELECT DISTINCT ON (%s) id, %s FROM ft ORDER BY %s, id"
                                k (any) k)])
      35 [:stragg    (format "SELECT string_agg(%s, ',' ORDER BY id) FROM ft"
                             (pick ["s" "id::text" "coalesce(s,'x')"]))]
      36 [:nullif    (format "SELECT id, %s AS c FROM ft ORDER BY id"
                             (pick ["nullif(i, 10)" "coalesce(i, j, 0)" "nullif(s, '')"
                                    "coalesce(s, 'z')" "nullif(n, 0)"]))]
      37 [:tsfn      (format "SELECT id, %s AS c FROM ft ORDER BY id"
                             (pick ["ts" "ts::date" "ts::text" "extract(hour from ts)"
                                    "date_trunc('day', ts)" "ts = d" "ts > d"]))]
      38 [:multijoin (format "SELECT ft.id, fu.v, f2.s FROM ft %s fu ON ft.i = fu.k %s ft f2 ON f2.id = ft.id ORDER BY ft.id, fu.v"
                             (pick ["JOIN" "LEFT JOIN"]) (pick ["JOIN" "LEFT JOIN"]))]
      39 [:havingagg (format "SELECT %s AS g, %s(%s) AS a FROM ft GROUP BY 1 HAVING %s(%s) IS NOT NULL ORDER BY 1"
                             (any) (pick ["sum" "min" "max"]) (num)
                             (pick ["sum" "min" "max"]) (num))]
      40 [:corrsel   (format "SELECT id, (SELECT %s(%s) FROM ft t2 WHERE t2.id %s ft.id) AS c FROM ft ORDER BY id"
                             (pick ["max" "min" "count" "sum"]) (num) (pick ["<=" "<" "=" "<>"]))]
      41 [:corrwhere (format "SELECT id FROM ft WHERE %s %s (SELECT %s(%s) FROM ft t2 WHERE t2.id %s ft.id) ORDER BY id"
                             (num) (pick cmp) (pick ["max" "min" "sum"]) (num) (pick ["<=" "<" "<>"]))]
      42 [:lateral   (format "SELECT ft.id, x.v FROM ft %s LATERAL (SELECT %s AS v FROM ft t2 WHERE t2.id = ft.id) x ON true ORDER BY ft.id, x.v"
                             (pick ["JOIN" "LEFT JOIN"]) (num))]
      43 [:latsrf    (format "SELECT ft.id, g FROM ft, LATERAL generate_series(1, %s) g ORDER BY ft.id, g"
                             (pick ["2" "id" "3"]))]
      44 [:recursive (format (str "WITH RECURSIVE r(n) AS (SELECT 1 UNION ALL SELECT n+1 FROM r WHERE n < %s) "
                                  "SELECT n FROM r ORDER BY n") (pick ["3" "5" "1"]))]
      45 [:ctechain  (format (str "WITH a AS (SELECT id, %s AS v FROM ft), b AS (SELECT id, v*2 AS w FROM a) "
                                  "SELECT a.id, a.v, b.w FROM a JOIN b ON a.id = b.id ORDER BY a.id") (num))]
      46 [:exists2   (format "SELECT id FROM ft WHERE %sEXISTS (SELECT 1 FROM ft t2 WHERE t2.i = ft.i AND t2.id %s ft.id) ORDER BY id"
                             (pick ["" "NOT "]) (pick ["<>" "<" ">"]))]
      47 [:scalarcmp (format "SELECT id, (SELECT count(*) FROM ft t2 WHERE t2.%s = ft.%s) AS c FROM ft ORDER BY id"
                             (pick ["i" "j" "b"]) (pick ["i" "j" "b"]))]
      48 [:winrank   (format "SELECT id, %s OVER (%s%s) AS c FROM ft ORDER BY id"
                             (pick ["row_number()" "rank()" "dense_rank()" "ntile(3)"
                                    "percent_rank()" "cume_dist()" "lag(i)" "lead(i,2,-1)"
                                    "nth_value(i,2)"])
                             (pick ["" "PARTITION BY b " "PARTITION BY j "])
                             (pick ["ORDER BY i" "ORDER BY i DESC" "ORDER BY id"
                                    "ORDER BY j NULLS FIRST"]))]
      49 [:winsub    (format "SELECT * FROM (SELECT id, %s OVER (%sORDER BY %s) AS r FROM ft) t WHERE %s ORDER BY id"
                             (pick ["row_number()" "rank()" "sum(i)" "count(*)"])
                             (pick ["" "PARTITION BY b "])
                             (pick ["id" "i" "j"])
                             (pick ["r = 1" "r <= 2" "r IS NOT NULL"]))]
      ;; ---- type-directed values: output, containers, zones, input ------
      51 [:render    (let [c (pick ["t" "ttz" "tz" "m" "iv" "d" "ts"])]
                       (format "SELECT id, %s, %s::text FROM fv ORDER BY id" c c))]
      52 [:container (let [c (pick ["t" "ttz" "tz" "m" "d" "ts"])]
                       (format "SELECT id, %s FROM fv ORDER BY id"
                               (pick [(format "ARRAY[%s]" c) (format "ROW(%s, id)" c)
                                      (format "(%s, id)::text" c) (format "(id, %s)" c)])))]
      53 [:atz       (format "SELECT id, %s AT TIME ZONE '%s' FROM fv ORDER BY id"
                             (pick ["ts" "tz" "ttz" "d"])
                             (pick ["UTC" "America/New_York" "+05" "-03:30" "Asia/Kolkata" "EST"]))]
      54 [:typein    (format "SELECT '%s'::%s"
                             (pick ["10:00" "1:2:3 PM" "10:00:00.1234567" "25:00" "garbage"
                                    "10:00+02" "2020-01-01 10:00:00" "2020-01-01" "12.5"])
                             (pick ["time" "timetz" "money"]))]
      55 [:concatv   (let [c (pick ["t" "tz" "m" "d" "ts"])]
                       (format "SELECT id, 'x' || %s, concat(%s, '|') FROM fv ORDER BY id" c c))]
      ;; An untyped literal takes the other operand's type through that
      ;; type's input function -- or raises its error. Valid, edge and
      ;; invalid spellings per type; a literal that failed to parse used
      ;; to compare as text and match nothing.
      56 [:litcmp    (let [c (pick ["i" "sm" "bg" "b" "f" "n" "id"])
                           lit (pick (case c
                                       ("i" "sm" "bg" "id") ["10" " 10 " "0x0A" "1_0" "-3" "1.5" "abc" "" "99999"
                                                             "9223372036854775807" "9223372036854775808"]
                                       "b" ["t" "false" "no" "of" "o" "yes" " on " "1" "maybe" ""]
                                       "f" ["1.5" "-0.5" "1e3" "inf" "NaN" "1d" "0x1p1" "1e400" "abc"]
                                       "n" ["1.50" "2.25" "NaN" "1_0" "0x0A" "1e" "abc"]))]
                       (format "SELECT id FROM ft WHERE %s %s '%s' ORDER BY id"
                               c (pick ["=" "<>" "<" ">="]) lit))]
      57 [:scalarin  (format "SELECT '%s'::%s"
                             (pick ["0" " 12 " "-1" "1.5" "0x1F" "0o17" "0b101" "1_000" "_1" "1e3" "99999"
                                    "4294967296" "t" "ye" "of" "o" "NaN" "-inf" "1e39" "1e-50" "0x1p-3"
                                    "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11" "{a0eebc999c0b4ef8bb6d6bb9bd380a11}"])
                             (pick ["int2" "int4" "int8" "oid" "bool" "float4" "float8" "numeric" "uuid"]))]
      50 [:winfilter (format "SELECT id, %s(%s) FILTER (WHERE %s) OVER (%s) AS c FROM ft ORDER BY id"
                             (pick ["sum" "count" "avg" "min" "array_agg"])
                             (pick ["i" "j" "n"])
                             (pick ["i > 0" "j IS NOT NULL" "b" "i IS NULL"])
                             (pick ["" "PARTITION BY b" "ORDER BY id"]))])))

(defn gen-prepared
  "A parameterised [class sql params]; the point is the extended protocol,
   so the shapes stay simple and the parameter positions vary."
  [^java.util.Random r]
  (let [pick (fn [v] (nth v (.nextInt r (count v))))
        v1 (pick [10 0 -3 nil 2])
        v2 (pick [20 7 nil 0])]
    (case (.nextInt r 12)
      0  [:p-eq    "SELECT id FROM ft WHERE i = ? ORDER BY id" [v1]]
      1  [:p-ne    "SELECT id FROM ft WHERE i <> ? ORDER BY id" [v1]]
      2  [:p-cmp   "SELECT id FROM ft WHERE i > ? ORDER BY id" [v1]]
      3  [:p-two   "SELECT id FROM ft WHERE i = ? OR j = ? ORDER BY id" [v1 v2]]
      4  [:p-not   "SELECT id FROM ft WHERE NOT (i = ? AND j = ?) ORDER BY id" [v1 v2]]
      5  [:p-proj  "SELECT id, i = ? AS c FROM ft ORDER BY id" [v1]]
      6  [:p-arith "SELECT id, i + ? AS c FROM ft ORDER BY id" [v1]]
      7  [:p-case  "SELECT id, CASE WHEN i = ? THEN 'y' ELSE 'n' END AS c FROM ft ORDER BY id" [v1]]
      8  [:p-in    "SELECT id FROM ft WHERE i IN (?, ?) ORDER BY id" [v1 v2]]
      9  [:p-agg   "SELECT count(*) FROM ft WHERE i = ?" [v1]]
      10 [:p-null  "SELECT id FROM ft WHERE (i = ?) IS NULL ORDER BY id" [v1]]
      11 [:p-betw  "SELECT id FROM ft WHERE i BETWEEN ? AND ? ORDER BY id" [v1 v2]])))

(defn gen-dml
  "One [class sql] mutation."
  [^java.util.Random r]
  (let [pick (fn [v] (nth v (.nextInt r (count v))))
        num  #(pick (into cols-num ["1" "0" "-1" "2.5" "10"]))]
    (case (.nextInt r 10)
      0 [:upd (format "UPDATE ft SET i = %s WHERE %s %s %s" (num) (num) (pick cmp) (num))]
      1 [:upd (format "UPDATE ft SET s = 'z' WHERE NOT (%s %s %s)" (num) (pick cmp) (num))]
      2 [:upd (format "UPDATE ft SET i = i + 1 WHERE id IN (SELECT id FROM ft WHERE %s %s %s)"
                      (num) (pick cmp) (num))]
      3 [:upd (format "UPDATE ft SET j = NULL WHERE %s IS NULL" (pick cols-any))]
      4 [:del (format "DELETE FROM ft WHERE %s %s %s" (num) (pick cmp) (num))]
      5 [:del (format "DELETE FROM ft WHERE NOT (%s %s %s AND %s %s %s)"
                      (num) (pick cmp) (num) (num) (pick cmp) (num))]
      6 [:del (format "DELETE FROM ft WHERE %s IS NULL" (pick cols-any))]
      7 [:ins (format "INSERT INTO ft (id, i, j) VALUES (%d, %s, %s)"
                      (+ 10 (.nextInt r 5)) (num) (num))]
      8 [:ins (format "INSERT INTO ft (id, i) SELECT id + 100, %s FROM ft WHERE %s %s %s"
                      (num) (num) (pick cmp) (num))]
      9 [:upd (format "UPDATE ft SET i = CASE WHEN %s %s %s THEN 1 ELSE 2 END"
                      (num) (pick cmp) (num))])))

;; ---------------------------------------------------------------------------
;; Runners
;; ---------------------------------------------------------------------------

(defn- sample
  "Draw the next sample for `surface`: {:class :key :run (fn [conn])}. The
   key identifies a distinct sample (the SQL, plus parameters when bound)."
  [surface ^java.util.Random r]
  (case surface
    :select   (let [[cls sql] (gen-select r)]
                {:class cls :key sql :run #(q % sql)})
    :prepared (let [[cls sql params] (gen-prepared r)]
                {:class cls :key (str sql " " (pr-str params))
                 :run #(prep-q % sql params)})
    ;; A mutation is only comparable from the same start state: re-seed,
    ;; run, then compare the outcome (row count or SQLSTATE) and the table.
    :dml      (let [[cls sql] (gen-dml r)]
                {:class cls :key sql
                 :run (fn [c] (seed! c) [(exec! c sql) (table-state c)])})))

(defn run-surface
  "Draw `n` samples of `surface` from `seed`, run each DISTINCT one on both
   endpoints, and collect the disagreements."
  [surface n seed]
  (with-open [o (reference-conn) t (target-conn)]
    ;; pgjdbc sends the client's TimeZone at startup, overriding the
    ;; server's; pin both sessions or date_trunc differs by the host zone.
    (doseq [c [o t]] (exec! c "SET TimeZone='UTC'"))
    (when (not= surface :dml)
      (doseq [c [o t]] (seed! c) (seed! c value-setup)))
    (let [r (java.util.Random. (long seed))]
      (loop [i 0 seen #{} ran {} diffs []]
        (if (>= i n)
          {:surface surface :seed seed :drawn seen :ran ran :diffs diffs}
          (let [{:keys [class key run]} (sample surface r)]
            (if (seen key)
              (recur (inc i) seen ran diffs)
              (let [a (run o) b (run t)]
                (recur (inc i) (conj seen key)
                       (update ran class (fnil inc 0))
                       (if (= a b)
                         diffs
                         (conj diffs {:surface surface :class class :key key
                                      :reference a :target b})))))))))))

;; ---------------------------------------------------------------------------
;; Manifest and reporting
;; ---------------------------------------------------------------------------

(def manifest-path "test/integration/fuzz/expected-divergences.edn")

(defn expected-divergences
  "{[surface key] reason} of known, explained disagreements."
  []
  (let [f (io/file manifest-path)]
    (if (.exists f)
      (into {} (map (fn [{:keys [surface key reason]}] [[surface key] reason]))
            (edn/read-string (slurp f)))
      {})))

(defn- clip [x] (let [s (pr-str x)] (subs s 0 (min 160 (count s)))))

(defn report
  "Print a run's per-class table and disagreements; return the unexpected
   disagreements and the drifted manifest entries."
  ([result] (report result (expected-divergences)))
  ([{:keys [surface seed drawn ran diffs]} expected]
   (let [bad (frequencies (map :class diffs))
         unexpected (remove #(contains? expected [(:surface %) (:key %)]) diffs)
         diff-keys (set (map :key diffs))
         ;; Only an entry this run actually DREW can drift.
         drifted (for [[[s k] _] expected
                       :when (and (= s surface) (drawn k) (not (diff-keys k)))]
                   k)]
     (println (format "== %s (seed %s): %d distinct samples, %d disagreements (%d unexpected) =="
                      (name surface) seed (count drawn) (count diffs) (count unexpected)))
     (println (format "   %-12s%6s%6s" "class" "ran" "diff"))
     (doseq [c (sort-by (juxt #(- (get bad % 0)) name) (keys ran))]
       (println (format "   %-12s%6d%6d" (name c) (ran c) (get bad c 0))))
     (doseq [{:keys [class key reference target]} diffs]
       (println (format "\n  [%s]%s %s" (name class)
                        (if (contains? expected [surface key]) " (expected)" "") key))
       (println "    PG  " (clip reference))
       (println "    we  " (clip target)))
     {:unexpected (vec unexpected) :drifted (vec drifted)})))

(defn -main
  "Run the differential fuzzer; exit 1 on an unexpected disagreement or a
   manifest entry the run drew that now agrees."
  [& [surface n seed]]
  (let [surfaces (if (or (nil? surface) (= "all" surface))
                   [:select :prepared :dml]
                   [(keyword surface)])
        n (if n (Long/parseLong n) nil)
        seed (if seed (Long/parseLong seed) 20260918)
        default-n {:select 1500 :prepared 600 :dml 300}
        outcomes (doall (for [s surfaces]
                          (report (run-surface s (or n (default-n s)) seed))))
        unexpected (mapcat :unexpected outcomes)
        drifted (mapcat :drifted outcomes)]
    (when (seq drifted)
      (println "\nBASELINE DRIFT: these expected divergences now agree -- prune"
               manifest-path ":")
      (doseq [k drifted] (println "  " k)))
    (when (seq unexpected)
      (println "\nREGRESSION:" (count unexpected)
               "disagreement(s) not in" manifest-path))
    (shutdown-agents)
    (System/exit (if (or (seq unexpected) (seq drifted)) 1 0))))
