(ns fuzz
  "Differential SQL fuzzer, run INSIDE the server JVM over two persistent
   JDBC connections -- the PG 17 oracle on 15998 and pg-datahike on 15432.

   Both speak the same wire protocol, so there is no reason to spawn a psql
   process per query: that was two processes and two connection handshakes
   per sample, which dominated the runtime.

   Biased toward the shapes real clients emit -- projections, predicates,
   joins, aggregates, CASE, arithmetic over NULLable columns -- not
   adversarial parser strings. The question it answers is \"how much silent
   wrong answer is left, and in which classes?\""
  (:require [clojure.string :as str])
  (:import [java.sql Connection DriverManager]))

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

(defn conn ^Connection [port user db]
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" port "/" db
        "?user=" user "&password=x&sslmode=disable&binaryTransfer=false")))

(defn oracle-conn [] (conn 15998 "pgtest" "postgres"))
(defn target-conn [] (conn 15432 "datahike" "datahike"))

(defn exec! [^Connection c sql]
  (try (with-open [st (.createStatement c)] (.execute st sql) nil)
       (catch Exception e (.getMessage e))))

(defn q
  "Run sql, return either [:rows ...] or [:error <sqlstate-ish first line>].
   Errors compare at CLASS level -- that both sides error, not the wording,
   since our messages often name a Java class."
  [^Connection c sql]
  (try
    (with-open [st (.createStatement c)
                rs (.executeQuery st sql)]
      (let [n (.. rs getMetaData getColumnCount)]
        [:rows (loop [acc []]
                 (if (.next rs)
                   (recur (conj acc (mapv (fn [^long ix] (.getString rs ix)) (range 1 (inc n)))))
                   acc))]))
    (catch Exception _ [:error])))

;; ---- generators -----------------------------------------------------------
(def cols-num ["i" "j" "f" "n" "id"])
(def cols-any ["i" "j" "s" "b" "f" "n" "d" "id"])
(def cmp ["=" "<>" "<" ">" "<=" ">="])
(def arith ["+" "-" "*" "/"])

(defn gen [^java.util.Random r]
  (let [pick (fn [v] (nth v (.nextInt r (count v))))
        num  #(pick (into cols-num ["1" "0" "-1" "2.5" "10"]))
        any  #(pick cols-any)
        txt  #(pick ["s" "'aa'" "'%a%'" "''"])]
    (case (.nextInt r 51)
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
      ;; ---- widened surface ------------------------------------------------
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
      ;; Ranking / navigation, which take no aggregate argument, and the
      ;; derived-table shape every top-N-per-group query uses.
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
      50 [:winfilter (format "SELECT id, %s(%s) FILTER (WHERE %s) OVER (%s) AS c FROM ft ORDER BY id"
                             (pick ["sum" "count" "avg" "min" "array_agg"])
                             (pick ["i" "j" "n"])
                             (pick ["i > 0" "j IS NOT NULL" "b" "i IS NULL"])
                             (pick ["" "PARTITION BY b" "ORDER BY id"]))]
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
      ;; ---- second widening: arrays, jsonb, CTEs, coercion, numeric edges ----
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
                                                "trunc(-2.5)" "mod(-7, 3)" "(-7) %% 3"]))]
      ;; PostgreSQL REQUIRES the DISTINCT ON expressions to be the leading
      ;; ORDER BY ones; anything else is a syntax error, not a query.
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
      ;; ---- third widening: correlation, laterals, recursion ----------------
      40 [:corrsel  (format "SELECT id, (SELECT %s(%s) FROM ft t2 WHERE t2.id %s ft.id) AS c FROM ft ORDER BY id"
                            (pick ["max" "min" "count" "sum"]) (num) (pick ["<=" "<" "=" "<>"]))]
      41 [:corrwhere (format "SELECT id FROM ft WHERE %s %s (SELECT %s(%s) FROM ft t2 WHERE t2.id %s ft.id) ORDER BY id"
                             (num) (pick cmp) (pick ["max" "min" "sum"]) (num) (pick ["<=" "<" "<>"]))]
      42 [:lateral  (format "SELECT ft.id, x.v FROM ft %s LATERAL (SELECT %s AS v FROM ft t2 WHERE t2.id = ft.id) x ON true ORDER BY ft.id, x.v"
                            (pick ["JOIN" "LEFT JOIN"]) (num))]
      43 [:latsrf   (format "SELECT ft.id, g FROM ft, LATERAL generate_series(1, %s) g ORDER BY ft.id, g"
                            (pick ["2" "id" "3"]))]
      44 [:recursive (format (str "WITH RECURSIVE r(n) AS (SELECT 1 UNION ALL SELECT n+1 FROM r WHERE n < %s) "
                                  "SELECT n FROM r ORDER BY n") (pick ["3" "5" "1"]))]
      45 [:ctechain (format (str "WITH a AS (SELECT id, %s AS v FROM ft), b AS (SELECT id, v*2 AS w FROM a) "
                                 "SELECT a.id, a.v, b.w FROM a JOIN b ON a.id = b.id ORDER BY a.id") (num))]
      46 [:exists2  (format "SELECT id FROM ft WHERE %sEXISTS (SELECT 1 FROM ft t2 WHERE t2.i = ft.i AND t2.id %s ft.id) ORDER BY id"
                            (pick ["" "NOT "]) (pick ["<>" "<" ">"]))]
      47 [:scalarcmp (format "SELECT id, (SELECT count(*) FROM ft t2 WHERE t2.%s = ft.%s) AS c FROM ft ORDER BY id"
                             (pick ["i" "j" "b"]) (pick ["i" "j" "b"]))])))

(defn run-fuzz [n seed]
  (with-open [o (oracle-conn) t (target-conn)]
    ;; pgjdbc sends the CLIENT's TimeZone in its startup packet, which
    ;; overrides the oracle's server setting -- so date_trunc came back in
    ;; the laptop's zone on one side and UTC on the other, a pure harness
    ;; artifact. Pin both sessions.
    (doseq [c [o t]] (exec! c "SET TimeZone='UTC'"))
    (doseq [s setup] (exec! o s) (exec! t s))
    (let [r (java.util.Random. seed)]
      (loop [i 0 seen #{} ran {} bad {} diffs []]
        (if (>= i n)
          {:distinct (count seen) :ran ran :bad bad :diffs diffs}
          (let [[cls sql] (gen r)]
            (if (seen sql)
              (recur (inc i) seen ran bad diffs)
              (let [a (q o sql) b (q t sql)
                    ok? (= a b)]
                (recur (inc i) (conj seen sql)
                       (update ran cls (fnil inc 0))
                       (if ok? bad (update bad cls (fnil inc 0)))
                       (if ok? diffs (conj diffs [cls sql a b])))))))))))

(defn report [{:keys [distinct ran bad diffs]}]
  (println (format "== %d distinct queries, %d disagreements ==" distinct (count diffs)))
  (println (format "%-12s%6s%6s" "class" "ran" "diff"))
  (doseq [c (sort-by #(- (get bad % 0)) (keys ran))]
    (println (format "%-12s%6d%6d" (name c) (ran c) (get bad c 0))))
  (println "\n== samples ==")
  (doseq [[cls items] (sort-by #(- (count (val %))) (group-by first diffs))]
    (println (format "\n--- %s (%d) ---" (name cls) (count items)))
    (doseq [[_ sql a b] (take 3 items)]
      (println "  Q  " sql)
      (println "  PG " (subs (pr-str a) 0 (min 130 (count (pr-str a)))))
      (println "  we " (subs (pr-str b) 0 (min 130 (count (pr-str b))))))))

;; ===========================================================================
;; DML and prepared-statement fuzzing
;;
;; Two surfaces the SELECT generator above cannot reach:
;;
;;   * A MUTATION is only comparable if both servers start from the same
;;     state, so each sample re-seeds both sides, runs the statement, and
;;     compares the resulting TABLE plus the reported row count.
;;   * A PREPARED statement goes through Parse/Bind/Describe/Execute -- the
;;     EXTENDED protocol every real client uses. psql's simple query never
;;     exercises it, and a Describe/Execute disagreement is invisible there.
;; ===========================================================================

(defn- reseed! [^Connection c]
  (doseq [s setup] (exec! c s)))

(defn- table-state [^Connection c]
  (q c "SELECT id, i, j, s, b, f, n, d FROM ft ORDER BY id"))

(defn gen-dml [^java.util.Random r]
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

(defn run-dml-fuzz [n seed]
  (with-open [o (oracle-conn) t (target-conn)]
    (doseq [c [o t]] (exec! c "SET TimeZone='UTC'"))
    (let [r (java.util.Random. seed)]
      (loop [i 0 seen #{} ran {} bad {} diffs []]
        (if (>= i n)
          {:distinct (count seen) :ran ran :bad bad :diffs diffs}
          (let [[cls sql] (gen-dml r)]
            (if (seen sql)
              (recur (inc i) seen ran bad diffs)
              (do
                (reseed! o) (reseed! t)
                (let [ao (exec! o sql) at (exec! t sql)
                      ;; exec! returns nil on success, the message on error;
                      ;; compare at class level like the query path does.
                      eo (if ao :error :ok) et (if at :error :ok)
                      so (table-state o) st (table-state t)
                      ok? (and (= eo et) (= so st))]
                  (recur (inc i) (conj seen sql)
                         (update ran cls (fnil inc 0))
                         (if ok? bad (update bad cls (fnil inc 0)))
                         (if ok? diffs (conj diffs [cls sql [eo so] [et st]]))))))))))))

(defn- prep-q
  "Run `sql` as a PREPARED statement with `params` bound, over the extended
   protocol. Returns the same [:rows …] / [:error] shape as `q`."
  [^Connection c sql params]
  (try
    (with-open [st (.prepareStatement c sql)]
      (dotimes [i (count params)]
        (let [v (nth params i)]
          (if (nil? v)
            (.setNull st (int (inc i)) java.sql.Types/INTEGER)
            (.setObject st (int (inc i)) v))))
      (with-open [rs (.executeQuery st)]
        (let [n (.. rs getMetaData getColumnCount)]
          [:rows (loop [acc []]
                   (if (.next rs)
                     (recur (conj acc (mapv (fn [^long ix] (.getString rs ix)) (range 1 (inc n)))))
                     acc))])))
    (catch Exception _ [:error])))

(defn gen-prepared
  "A parameterised query plus the values to bind. The point is the extended
   protocol, so the shapes stay simple and the PARAMETER positions vary."
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

(defn run-prepared-fuzz [n seed]
  (with-open [o (oracle-conn) t (target-conn)]
    (doseq [c [o t]] (exec! c "SET TimeZone='UTC'"))
    (doseq [s setup] (exec! o s) (exec! t s))
    (let [r (java.util.Random. seed)]
      (loop [i 0 seen #{} ran {} bad {} diffs []]
        (if (>= i n)
          {:distinct (count seen) :ran ran :bad bad :diffs diffs}
          (let [[cls sql params] (gen-prepared r)
                k [sql params]]
            (if (seen k)
              (recur (inc i) seen ran bad diffs)
              (let [a (prep-q o sql params) b (prep-q t sql params)
                    ok? (= a b)]
                (recur (inc i) (conj seen k)
                       (update ran cls (fnil inc 0))
                       (if ok? bad (update bad cls (fnil inc 0)))
                       (if ok? diffs (conj diffs [cls (str sql " " (pr-str params)) a b])))))))))))
