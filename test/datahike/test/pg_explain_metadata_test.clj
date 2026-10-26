(ns datahike.test.pg-explain-metadata-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql :as sql]
            [datahike.pg.sql.params :as params]
            [datahike.pg.sql.stmt :as stmt]
            [datahike.pg.types :as types])
  (:import [java.sql Connection DriverManager SQLException]
           [org.postgresql PGStatement]))

(def ^:dynamic *port* nil)
(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :max-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"explain_metadata" conn} {:port 0})]
      (try (binding [*port* (.getPort server) *conn* conn] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/explain_metadata?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c text]
  (with-open [s (.createStatement c)] (.execute s text)))

(defn- rows [^Connection c text]
  (with-open [s (.createStatement c) r (.executeQuery s text)]
    (let [n (.getColumnCount (.getMetaData r))]
      (loop [out []]
        (if (.next r)
          (recur (conj out (mapv #(.getString r (int %)) (range 1 (inc n))))) out)))))

(defn- state [^Connection c text]
  (try (exec! c text) nil (catch SQLException e (.getSQLState e))))

(defn- source-read? [query]
  (some #(or (and (vector? %) (= :explain_source/id (second %)))
             (and (seq? %) (contains? #{'get-else 'get-some} (first %))
                  (= :explain_source/id (nth % 3 nil))))
        (tree-seq coll? seq query)))

(deftest explain-never-enters-the-derived-row-runner-or-writes-result-rows
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_source(id int)")
    (exec! c "INSERT INTO explain_source VALUES(1),(2)")
    (let [q d/q db-with d/db-with reads (atom 0) produced (atom 0)
          text (str "SELECT x,y FROM (SELECT a.id x,b.id y FROM explain_source a,"
                    "explain_source b) t WHERE x=1 GROUP BY x,y")]
      (with-redefs [d/q (fn [query & args]
                          (when (source-read? query)
                            (swap! reads inc)
                            (throw (ex-info "represented row runner invoked" {})))
                          (apply q query args))
                    d/db-with (fn [db tx & args]
                                (doseq [entry tx
                                        :when (and (map? entry) (not (:db/ident entry))
                                                   (some #(and (keyword? %)
                                                               (some-> (namespace %) (.startsWith "__sub__")))
                                                         (keys entry)))]
                                  (swap! produced inc)
                                  (throw (ex-info "represented result row written" {})))
                                (apply db-with db tx args))]
        (is (re-find #"^Datahike Query" (ffirst (rows c (str "EXPLAIN " text)))))
        (is (re-find #"^Datahike Query"
                     (ffirst (rows c (str "EXPLAIN WITH a AS(SELECT id FROM explain_source),"
                                          "b AS(SELECT id FROM a UNION ALL SELECT id FROM a) "
                                          "SELECT sum(id) FROM b")))))
        (is (zero? @reads))
        (is (zero? @produced))
        ;; The same sentinel observes ordinary execution of this producer.
        (is (some? (state c text)))
        (is (pos? @reads))))))

(deftest analysis-retains-star-window-compound-and-typmod-metadata
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_source(id int,amount numeric(5,2))")
    (let [db (d/db *conn*)]
      (binding [params/*analysis-only?* true]
        (let [p (sql/parse-sql
                 (str "SELECT * FROM (SELECT id+1 AS n,"
                      "row_number() OVER(ORDER BY id) AS rn,amount FROM explain_source) t")
                 (:schema db) db)
              columns (stmt/visible-output-columns p nil)]
          (is (= ["n" "rn" "amount"] (mapv :name columns)))
          (is (= [types/oid-int4 types/oid-int8 types/oid-numeric] (mapv :oid columns)))
          (is (= (types/encode-numeric-typmod 5 2) (:typmod (last columns)))))))))

(deftest explain-preserves-static-errors-without-execution
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_source(id int)")
    (is (= "42703" (state c "EXPLAIN SELECT no_such_column FROM explain_source")))
    (is (= "42P01" (state c "EXPLAIN SELECT * FROM absent_relation")))
    (is (= "42P10" (state c "EXPLAIN SELECT * FROM (SELECT 1) t(a,b)")))
    (is (= "42601" (state c "EXPLAIN SELECT * FROM (SELECT 1 UNION ALL SELECT 1,2) t")))
    (is (= "42804" (state c "EXPLAIN SELECT * FROM (SELECT 1 UNION ALL SELECT false) t")))))

(deftest named-explain-cache-does-not-poison-ordinary-materialization
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_source(id int)")
    (exec! c "INSERT INTO explain_source VALUES(1)")
    (let [body "SELECT sum(id) AS total FROM (SELECT id FROM explain_source) t"]
      (with-open [ps (.prepareStatement c (str "EXPLAIN " body))]
        (.setPrepareThreshold ^PGStatement ps 1)
        (is (= "QUERY PLAN" (.getColumnLabel (.getMetaData ps) 1)))
        (with-open [r (.executeQuery ps)] (is (.next r)))
        (exec! c "INSERT INTO explain_source VALUES(2)")
        (with-open [r (.executeQuery ps)] (is (.next r))))
      (is (= [["3"]] (rows c body)))
      (exec! c "INSERT INTO explain_source VALUES(3)")
      (is (= [["6"]] (rows c body))))))

(deftest explain-does-not-invoke-a-routine-with-dml-effects
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_effects(n int)")
    (exec! c (str "CREATE FUNCTION explain_write() RETURNS int LANGUAGE plpgsql AS $$ "
                  "BEGIN INSERT INTO explain_effects VALUES(1); RETURN 7; END $$"))
    (let [result (try {:plan (ffirst (rows c "EXPLAIN SELECT * FROM (SELECT explain_write() AS n) t"))}
                      (catch SQLException e {:sqlstate (.getSQLState e)}))]
      ;; Routine output metadata may be explicitly unsupported, but it must
      ;; never be discovered by invoking the routine to sample its result.
      (is (or (= "0A000" (:sqlstate result))
              (some->> (:plan result) (re-find #"^Datahike Query")))))
    (is (= [["0"]] (rows c "SELECT count(*) FROM explain_effects")))
    (is (= [["7"]] (rows c "SELECT explain_write()")))
    (is (= [["1"]] (rows c "SELECT count(*) FROM explain_effects")))))

(deftest set-operation-unknown-literals-resolve-before-virtual-schema
  (with-open [c (jdbc)]
    (let [db (d/db *conn*)]
      (binding [params/*analysis-only?* true]
        (let [p (sql/parse-sql "SELECT * FROM (SELECT NULL AS n UNION ALL SELECT 1) t"
                               (:schema db) db)]
          (is (= [types/oid-int4] (mapv :oid (stmt/visible-output-columns p nil)))))))))

(deftest explain-sequence-relations-use-only-identity-metadata
  (with-open [c (jdbc)]
    (exec! c "CREATE SEQUENCE explain_sequence")
    (let [q d/q state-reads (atom 0)]
      (with-redefs [d/q (fn [query & args]
                          (when (some #(and (vector? %) (= :__seq__/value (second %)))
                                      (tree-seq coll? seq query))
                            (swap! state-reads inc)
                            (throw (ex-info "sequence state rows were read" {})))
                          (apply q query args))]
        (is (re-find #"^Datahike Query"
                     (ffirst (rows c "EXPLAIN SELECT last_value,is_called FROM explain_sequence"))))
        (is (zero? @state-reads))
        (is (some? (state c "SELECT last_value FROM explain_sequence")))
        (is (pos? @state-reads))))))

(deftest correlated-lateral-metadata-is-an-explicit-boundary
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_source(id int)")
    (is (= "0A000"
           (state c (str "EXPLAIN SELECT s.n FROM explain_source t,"
                         "LATERAL(SELECT t.id AS n) s"))))))

(deftest joined-stars-use-all-relations-and-using-suppression
  (with-open [c (jdbc)]
    (exec! c "CREATE TABLE explain_left(id int,x numeric(5,2))")
    (exec! c "CREATE TABLE explain_right(id int,y varchar(4))")
    (exec! c "CREATE TABLE explain_unique(y varchar(4))")
    (let [db (d/db *conn*)]
      (binding [params/*analysis-only?* true]
        (let [p (sql/parse-sql
                 (str "SELECT t.y FROM(SELECT * FROM explain_left l "
                      "CROSS JOIN explain_unique r) t") (:schema db) db)]
          (is (= [{:name "y" :oid types/oid-varchar :unknown? false :typmod 8}]
                 (stmt/visible-output-columns p nil))))
        (let [p (sql/parse-sql
                 (str "SELECT * FROM(SELECT * FROM explain_left l "
                      "JOIN explain_right r USING(id)) t") (:schema db) db)
              columns (stmt/visible-output-columns p nil)]
          (is (= ["id" "x" "y"] (mapv :name columns)))
          (is (= [types/oid-int4 types/oid-numeric types/oid-varchar] (mapv :oid columns))))))
    (is (= "42601"
           (state c (str "EXPLAIN SELECT * FROM(SELECT l.id,l.x,r.y FROM explain_left l "
                         "CROSS JOIN explain_right r UNION ALL SELECT 1) t"))))))

(deftest cast-and-set-operation-modifiers-are-static-metadata
  (with-open [c (jdbc)]
    (let [db (d/db *conn*)
          columns (fn [text]
                    (binding [params/*analysis-only?* true]
                      (stmt/visible-output-columns (sql/parse-sql text (:schema db) db) nil)))]
      (is (= [9 (types/encode-numeric-typmod 5 2)]
             (mapv :typmod (columns "SELECT 'a'::varchar(5) AS v,1::numeric(5,2) AS n"))))
      (is (= [9] (mapv :typmod
                       (columns (str "SELECT * FROM(SELECT 'a'::varchar(5) AS v "
                                     "UNION ALL SELECT 'b'::varchar(5)) t")))))
      (is (= [nil] (mapv :typmod
                         (columns (str "SELECT * FROM(SELECT 'a'::varchar(3) AS v "
                                       "UNION ALL SELECT 'b'::varchar(5)) t"))))))))

(deftest unsupported-nominal-array-identity-is-not-laundered-to-text
  (with-open [c (jdbc)]
    (exec! c "CREATE TYPE explain_pair AS(x int)")
    (is (= "0A000" (state c "EXPLAIN SELECT NULL::explain_pair[]")))))

(deftest parenthesized-unknowns-and-set-carriers-are-analyzed
  (with-open [c (jdbc)]
    (let [db (d/db *conn*)]
      (binding [params/*analysis-only?* true]
        (doseq [text ["SELECT * FROM(SELECT ('1') AS n UNION ALL SELECT 1) t"
                      "SELECT * FROM(SELECT (NULL) AS n UNION ALL SELECT 1) t"]]
          (is (= [types/oid-int4]
                 (mapv :oid (stmt/visible-output-columns
                             (sql/parse-sql text (:schema db) db) nil)))))))
    (let [artifact (ffirst (rows c "EXPLAIN SELECT 1 UNION ALL SELECT 2"))]
      (is (re-find #":type :set-operation" artifact))
      (is (re-find #":sub-results" artifact))
      (is (not= "Datahike Query nil" artifact)))))
