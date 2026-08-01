(ns datahike.test.pg-join-scaling-test
  "Equi-join translation shape + scaling guard.

   Inner equi-joins must compile to shared-variable data patterns
   (hash-joinable, O(n)) — not get-else bindings plus an equality
   predicate, which the engine executes as a cross product (O(n²) time
   and heap; an 8k-row self-join OOMed a 2 GB heap before the fix).
   The shape assertions are deterministic; the timing check is a
   coarse backstop with a ~50× margin over the fixed implementation so
   CI noise can't flake it, while the quadratic regression (~2.5 s at
   this size, growing 4× per doubling) trips it reliably."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.stmt :as stmt])
  (:import [net.sf.jsqlparser.parser CCJSqlParserUtil]))

(defn- with-n-rows [n f]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)
        h (pg/make-query-handler conn {})]
    (try
      (.execute h "CREATE TABLE sj(id INTEGER, grp INTEGER)")
      (doseq [batch (partition-all 2000 (range n))]
        (.execute h (str "INSERT INTO sj(id,grp) VALUES "
                         (clojure.string/join ","
                                              (map #(str "(" % "," (mod % 100) ")") batch)))))
      (f conn h)
      (finally
        (d/release conn)
        (d/delete-database cfg)))))

(defn- data-pattern? [c]
  (and (vector? c) (= 3 (count c)) (symbol? (first c)) (keyword? (second c))))

(deftest equi-join-compiles-to-shared-var-patterns
  (with-n-rows 10
    (fn [conn _]
      (let [q (-> (CCJSqlParserUtil/parse
                   "SELECT count(*) FROM sj a JOIN sj b ON a.id = b.id")
                  (stmt/translate-select (dbi/-schema (d/db conn)) (d/db conn))
                  :query)
            patterns (filter data-pattern? (:where q))
            id-patterns (filter #(= :sj/id (second %)) patterns)]
        (testing "both sides bind :sj/id via plain patterns sharing one var"
          (is (= 2 (count id-patterns)) (pr-str (:where q)))
          (is (= 1 (count (distinct (map #(nth % 2) id-patterns))))
              (pr-str id-patterns)))
        (testing "no equality predicate between two get-else vars remains"
          (is (not-any? (fn [c]
                          (and (vector? c) (= 1 (count c))
                               (seq? (first c)) (= '= (ffirst c))))
                        (:where q))
              (pr-str (:where q))))))))

(deftest where-constant-compiles-to-value-bound-pattern
  (with-n-rows 10
    (fn [conn _]
      (let [q (-> (CCJSqlParserUtil/parse "SELECT grp FROM sj WHERE id = 7")
                  (stmt/translate-select (dbi/-schema (d/db conn)) (d/db conn))
                  :query)]
        (is (some #(and (data-pattern? %) (= :sj/id (second %)) (= 7 (nth % 2)))
                  (:where q))
            (pr-str (:where q)))))))

(deftest self-join-scales-linearly
  (with-n-rows 4000
    (fn [_ h]
      (let [t0 (System/nanoTime)
            r (.execute h "SELECT count(*) FROM sj a JOIN sj b ON a.id = b.id")
            ms (/ (- (System/nanoTime) t0) 1e6)]
        (is (nil? (.error r)))
        (is (= [["4000"]] (mapv vec (.rows r))))
        (is (< ms 1500)
            (format "4k self-join took %.0f ms — quadratic regression?" ms))))))
