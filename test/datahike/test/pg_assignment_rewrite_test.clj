(ns datahike.test.pg-assignment-rewrite-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.pg.sql.assignment-rewrite :as rewrite])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]))

(deftest only-assignment-target-brackets-are-adapted
  (let [sql "UPDATE t SET a[1][2]=ARRAY[[1,2],[3,4]][1:2],b=a[1][2] WHERE a[1][2]=3"
        result (rewrite/normalize sql)]
    (is (= "UPDATE t SET a[1,2]=ARRAY[[1,2],[3,4]][1:2],b=a[1][2] WHERE a[1][2]=3"
           (:sql result)))
    (is (= {[:update 0] [{:lower-omitted? false :upper-omitted? false}
                         {:lower-omitted? false :upper-omitted? false}]}
           (:targets result)))
    (is (some? (CCJSqlParserUtil/parse (:sql result)))))
  (doseq [sql ["SELECT a[1][2], 'UPDATE t SET a[:]=3' FROM t"
               "DELETE FROM t WHERE a[1][2]=1"
               "SELECT $$UPDATE t SET a[:]=ARRAY[1]$$"]]
    (is (= {:sql sql :targets {}} (rewrite/normalize sql)))))

(deftest omitted-bounds-preserve-original-parameter-order
  (let [sql "UPDATE t SET a[:$1][$2:][:]=ARRAY[$3],b[$4]=5"
        result (rewrite/normalize sql)]
    (is (= "UPDATE t SET a[0:$1,$2:0,0:0]=ARRAY[$3],b[$4]=5" (:sql result)))
    (is (= [{:lower-omitted? true :upper-omitted? false}
            {:lower-omitted? false :upper-omitted? true}
            {:lower-omitted? true :upper-omitted? true}]
           (get-in result [:targets [:update 0]])))
    (let [statement (CCJSqlParserUtil/parse (:sql result))
          annotations (rewrite/annotate statement (:targets result))
          column (-> statement .getUpdateSets first .getColumns first)]
      (is (= (get-in result [:targets [:update 0]]) (.get annotations column)))
      (is (= 2 (.size annotations)))
      (is (= "a[0:$1, $2:0, 0:0]" (str column))))))

(deftest strings-comments-casts-and-nested-subscripts-are-preserved
  (let [sql "UPDATE t SET \"a\"[coalesce(a[1],1)::int][: /* upper */ ] /* join */ [2]=E'[:] [2]' RETURNING a[1][2]"
        result (rewrite/normalize sql)]
    (is (= "UPDATE t SET \"a\"[coalesce(a[1],1)::int,0:0 /* upper */  /* join */ ,2]=E'[:] [2]' RETURNING a[1][2]"
           (:sql result)))
    (is (= 3 (count (get-in result [:targets [:update 0]])))))
  (let [result (rewrite/normalize "UPDATE t SET a[-2:-1][?::int]=?,a[NULL:]=NULL")]
    (is (= "UPDATE t SET a[-2:-1,?::int]=?,a[NULL:0]=NULL" (:sql result)))
    (is (= {:lower-omitted? false :upper-omitted? true}
           (first (get-in result [:targets [:update 1]]))))))

(deftest insert-and-conflict-have-independent-target-ordinals
  (let [result (rewrite/normalize
                "INSERT INTO public.t AS x (id,a[:][2],a[3]) VALUES (1,ARRAY[2],3) ON CONFLICT(id) DO UPDATE SET id=excluded.id,a[2][:]=excluded.a[1][2]")
        statement (CCJSqlParserUtil/parse (:sql result))
        annotations (rewrite/annotate statement (:targets result))
        insert-column (second (.getColumns statement))
        conflict-column (-> statement .getConflictAction .getUpdateSets second .getColumns first)]
    (is (= "INSERT INTO public.t AS x (id,a[0:0,2],a[3]) VALUES (1,ARRAY[2],3) ON CONFLICT(id) DO UPDATE SET id=excluded.id,a[2,0:0]=excluded.a[1][2]"
           (:sql result)))
    (is (= 3 (.size annotations)))
    (is (= [{:lower-omitted? true :upper-omitted? true}
            {:lower-omitted? false :upper-omitted? false}]
           (.get annotations insert-column)))
    (is (= [{:lower-omitted? false :upper-omitted? false}
            {:lower-omitted? true :upper-omitted? true}]
           (.get annotations conflict-column)))))

(deftest tuple-and-with-outer-update-targets-keep-their-ordinal
  (let [result (rewrite/normalize
                "WITH c AS (SELECT a[1][2] FROM t) UPDATE t SET (id,a[:])=(1,ARRAY[2]),a[2][3]=4 FROM c")]
    (is (= "WITH c AS (SELECT a[1][2] FROM t) UPDATE t SET (id,a[0:0])=(1,ARRAY[2]),a[2,3]=4 FROM c"
           (:sql result)))
    (is (= #{[:update 1] [:update 2]} (set (keys (:targets result)))))))

(deftest private-parser-syntax-is-not-admitted-as-postgresql-input
  (doseq [sql ["UPDATE t SET a[1,2]=3"
               "INSERT INTO t(a[1:2,3:4]) VALUES(ARRAY[1])"
               "INSERT INTO t VALUES(1) ON CONFLICT(id) DO UPDATE SET a[1,2]=3"]]
    (is (= "42601" (try (rewrite/normalize sql) nil
                        (catch clojure.lang.ExceptionInfo e (:sqlstate (ex-data e)))))))
  (testing "commas nested within an index expression are retained"
    (is (= "UPDATE t SET a[coalesce(1,2),3]=4"
           (:sql (rewrite/normalize "UPDATE t SET a[coalesce(1,2)][3]=4"))))))

(deftest annotations-reject-mismatched-parser-shapes-without-mutating-them
  (let [statement (CCJSqlParserUtil/parse "UPDATE t SET a[1]=2")
        before (str statement)]
    (doseq [targets [{[:update 0] [{:lower-omitted? true} {}]}
                     {[:update 1] [{}]}
                     {[:insert 0] [{}]}]]
      (is (= "42601" (try (rewrite/annotate statement targets) nil
                          (catch clojure.lang.ExceptionInfo e (:sqlstate (ex-data e)))))))
    (is (= before (str statement)))))

(deftest keyword-identifiers-do-not-change-target-ordinals-or-statement-kind
  (doseq [[sql expected]
          [["UPDATE set SET a[:][2]=3" "UPDATE set SET a[0:0,2]=3"]
           ["UPDATE public.set AS set SET a[1][2]=3" "UPDATE public.set AS set SET a[1,2]=3"]
           ["WITH update AS (SELECT 1) INSERT INTO t(a[:][2]) VALUES(ARRAY[1])"
            "WITH update AS (SELECT 1) INSERT INTO t(a[0:0,2]) VALUES(ARRAY[1])"]
           ["WITH insert(x) AS NOT MATERIALIZED (SELECT 1), c AS (SELECT 2) UPDATE t SET a[:][2]=3"
            "WITH insert(x) AS NOT MATERIALIZED (SELECT 1), c AS (SELECT 2) UPDATE t SET a[0:0,2]=3"]
           ["WITH update AS (SELECT 1) SELECT a[1][2] FROM t"
            "WITH update AS (SELECT 1) SELECT a[1][2] FROM t"]]]
    (is (= expected (:sql (rewrite/normalize sql))))))
