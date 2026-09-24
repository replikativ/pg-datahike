(ns datahike.test.pg-query-resolver-test
  "pg-datahike resolves the symbols of the queries it runs through its own
   table, bound around each call into a query handler, and leaves the
   process alone.

   It used to install Datahike's safe resolver for the whole process and
   register three of its namespaces with Datahike's function registry. The
   first broke an embedding application's own queries (its functions,
   reflection and most of `clojure.core` stopped resolving, for good). The
   second handed every public function of those namespaces -- cache
   invalidation, the catalog registry, the translation cache -- to anyone
   Datahike's own server lets query, since that registry is what its safe
   resolver serves to clients."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.resolve :as pg-resolve]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.expr :as expr]
            [datahike.query.resolve :as dqr])
  (:import [datahike.pg PgWireServer$QueryHandler]
           [java.sql Connection DriverManager]
           [org.postgresql PGConnection]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *port* nil)

(defn- server-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          srv (pg/start-server {"qr" conn} {:port 0})]
      (try
        (binding [*conn* conn *port* (.getPort ^datahike.pg.PgWireServer (:server srv))] (f))
        (finally (pg/stop-server srv) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each server-fixture)

(defn- jdbc ^Connection []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/qr?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- rows [^Connection c sql]
  (with-open [st (.createStatement c)
              rs (.executeQuery st sql)]
    (let [n (.getColumnCount (.getMetaData rs))]
      (loop [out []]
        (if (.next rs)
          (recur (conj out (mapv #(.getString rs (int %)) (range 1 (inc n)))))
          out)))))

(defn- pg-symbol? [sym]
  (and (symbol? sym) (some-> (namespace sym) (.startsWith "datahike.pg"))))

(defn- with-strict-process-resolver
  "Run `f` with the process's own resolver refusing every pg-datahike
   symbol: a query pg-datahike runs outside its binding fails."
  [f]
  (let [root dqr/*symbol-resolver*]
    (alter-var-root #'dqr/*symbol-resolver*
                    (constantly (fn [sym]
                                  (when (pg-symbol? sym)
                                    (throw (ex-info (str "resolved outside pg-datahike's binding: " sym)
                                                    {:symbol sym})))
                                  (root sym))))
    (try (f)
         (finally (alter-var-root #'dqr/*symbol-resolver* (constantly root))))))

(deftest the-process-resolver-and-registry-are-left-alone
  (let [root dqr/*symbol-resolver*
        registry (dqr/registered)]
    (with-open [c (jdbc)]
      (exec! c "CREATE TABLE t (a int)")
      (exec! c "INSERT INTO t VALUES (1), (2)")
      (is (= [["2"]] (rows c "SELECT a FROM t WHERE a > 1"))))
    (is (identical? root dqr/*symbol-resolver*)
        "starting a server and running statements does not replace the process's resolver")
    (is (= registry (dqr/registered))
        "nothing is registered with Datahike's function registry")))

(deftest a-datahike-client-cannot-call-pg-internals
  (testing "Datahike's server resolves client queries with safe-symbol-resolver"
    (doseq [sym '[datahike.pg.sql/invalidate-parse-cache!
                  datahike.pg.sql/register-catalog-table!
                  datahike.pg.sql/cached-result
                  datahike.pg.sql.params/resolve-nextvals!
                  datahike.pg.query-fns/sql-eq?]]
      (is (nil? (dqr/safe-symbol-resolver sym)) (str sym)))
    (is (thrown-with-msg? Exception #"Unknown (function|predicate)"
                          (binding [dqr/*symbol-resolver* dqr/safe-symbol-resolver]
                            (d/q '[:find ?x . :where [(datahike.pg.sql/invalidate-parse-cache!)]
                                   [(ground 1) ?x]]
                                 @*conn*))))))

(deftest every-handler-method-is-scoped
  (let [handler (pg/make-query-handler *conn* {:db-name "qr"})
        declared (into #{} (map #(.getName ^java.lang.reflect.Method %))
                       (.getDeclaredMethods (class handler)))
        interface (into #{} (map #(.getName ^java.lang.reflect.Method %))
                        (.getMethods PgWireServer$QueryHandler))]
    (try
      (is (empty? (remove declared interface))
          "a method left to the interface's default would run unscoped on the wrapper")
      (finally (.close ^PgWireServer$QueryHandler handler)))))

(deftest statements-resolve-only-through-the-binding
  (with-strict-process-resolver
    (fn []
      (with-open [c (jdbc)]
        (exec! c "CREATE TABLE r (id int PRIMARY KEY, a int CHECK (a >= 0), b text, v numeric)")
        (exec! c "INSERT INTO r VALUES (1, 1, 'x', 1.5), (2, 2, 'yy', 2.5), (3, 3, 'zzz', 3.5)")
        (testing "simple queries: predicates, arithmetic, text, aggregates, windows"
          (is (= [["2" "4"] ["3" "6"]] (rows c "SELECT id, a * 2 FROM r WHERE a > 1 AND b <> 'x' ORDER BY id")))
          (is (= [["6" "2.5000000000000000"]] (rows c "SELECT sum(a), avg(v) FROM r")))
          (is (= [["1" "1"] ["2" "3"] ["3" "6"]] (rows c "SELECT id, sum(a) OVER (ORDER BY id) FROM r ORDER BY id")))
          (is (= [["zzz"]] (rows c "SELECT b FROM r WHERE length(b) = 3")))
          (is (= [["3"]] (rows c "SELECT count(*) FROM r WHERE a BETWEEN 1 AND 3 AND a IN (1, 2, 3)"))))
        (testing "writes: CHECK runs on the writer thread with the caller's binding"
          (exec! c "UPDATE r SET a = a + 1 WHERE id = 1")
          (is (thrown? java.sql.SQLException (exec! c "INSERT INTO r VALUES (4, -1, 'n', 0)")))
          (exec! c "DELETE FROM r WHERE id = 3"))
        (testing "prepared statements and batches"
          (with-open [ps (.prepareStatement c "SELECT b FROM r WHERE a > ? ORDER BY id")]
            (.setInt ps 1 1)
            (with-open [rs (.executeQuery ps)]
              (is (= ["x" "yy"] (loop [out []] (if (.next rs) (recur (conj out (.getString rs 1))) out))))))
          (with-open [ps (.prepareStatement c "INSERT INTO r VALUES (?, ?, ?, ?)")]
            (doseq [i [10 11]]
              (.setInt ps 1 i) (.setInt ps 2 i) (.setString ps 3 "b") (.setBigDecimal ps 4 1M)
              (.addBatch ps))
            (.executeBatch ps)))
        (testing "COPY FROM STDIN"
          (let [cm (.getCopyAPI (.unwrap c PGConnection))]
            (.copyIn cm "COPY r FROM STDIN" (io/input-stream (.getBytes "20\t5\tc\t2.0\n")))))
        (testing "a cursor fetched across calls inside a transaction"
          (.setAutoCommit c false)
          (with-open [st (.createStatement c)]
            (.setFetchSize st 1)
            (with-open [rs (.executeQuery st "SELECT id FROM r WHERE a > 0 ORDER BY id")]
              (is (= 5 (count (loop [out []] (if (.next rs) (recur (conj out (.getString rs 1))) out)))))))
          (.commit c))))))

(deftest every-emitted-symbol-is-in-the-table
  (let [emitted (->> (file-seq (io/file "src"))
                     (filter #(re-find #"\.cljc?$" (.getName ^java.io.File %)))
                     (mapcat #(re-seq #"(?<!`)datahike\.pg\.(?:query-fns|secondary|tsearch)/[^\s\)\]\}\"',`]+"
                                      (slurp %)))
                     (map symbol)
                     ;; the secondary/tsearch namespaces also have functions
                     ;; the translator calls directly rather than emits
                     (filter #(or (= "datahike.pg.query-fns" (namespace %))
                                  (#{'datahike.pg.secondary/candidates
                                     'datahike.pg.secondary/filtered-candidates
                                     'datahike.pg.tsearch/ts-match?
                                     'datahike.pg.tsearch/ts-match3} %)))
                     set)]
    (is (< 90 (count emitted)) "the scan found the translator's symbols")
    (doseq [sym emitted]
      (is (fn? (pg-resolve/emitted-fn sym)) (str sym)))))

(deftest an-unresolvable-projection-head-is-an-error-not-null
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No function for projection head"
                        (expr/interpret-form '(datahike.pg.query-fns/no-such-fn 1) {}))))
