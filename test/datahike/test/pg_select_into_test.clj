(ns datahike.test.pg-select-into-test
  "`SELECT … INTO t` and the command tag of `CREATE TABLE … AS`.

   PostgreSQL has two spellings for the same statement:

     CREATE TABLE t AS SELECT …
     SELECT … INTO t          -- and INTO TABLE / INTO TEMP t

   JSqlParser has no INTO clause in a SELECT, so the second spelling
   never parsed. A token rewrite lifts the clause to the front, which
   turns it into the CREATE TABLE AS it means.

   Both spellings report the row count they stored (`SELECT 3`), not
   `CREATE TABLE` -- we reported the latter, and therefore a row count
   of zero, for every CTAS. When IF NOT EXISTS skips the work the tag is
   `CREATE TABLE AS`.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg]
            [datahike.pg.sql.rewrite :as rw])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec ^PgWireServer$QueryResult [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (mapv vec (.rows r))))

(defn- release! [{:keys [conn cfg]}]
  (d/release conn)
  (d/delete-database cfg))

;; ============================================================================
;; The rewrite itself
;; ============================================================================

(defn- pre [sql] (rw/rewrite sql rw/default-rules))

(deftest select-into-becomes-create-table-as
  (is (= "CREATE TABLE t AS SELECT x  FROM s" (pre "SELECT x INTO t FROM s")))
  (is (= "CREATE TABLE t AS SELECT x  FROM s" (pre "SELECT x INTO TABLE t FROM s")))
  (testing "the persistence words are kept -- INTO TEMP t is a temporary table"
    (is (= "CREATE TEMP TABLE t AS SELECT x  FROM s" (pre "SELECT x INTO TEMP t FROM s")))
    (is (= "CREATE UNLOGGED TABLE t AS SELECT x  FROM s"
           (pre "SELECT x INTO UNLOGGED t FROM s"))))
  (testing "a SELECT with no INTO, and an INTO that is not the outer one"
    (is (= "SELECT x FROM s" (pre "SELECT x FROM s")))
    (is (= "SELECT x FROM (SELECT y INTO_LIKE FROM s) q"
           (pre "SELECT x FROM (SELECT y INTO_LIKE FROM s) q")))))

;; ============================================================================
;; End to end
;; ============================================================================

(deftest select-into-creates-and-fills-the-table
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE src (i int, s text)"))))
      (is (nil? (.error (exec h "INSERT INTO src VALUES (1,'a'),(2,'b'),(3,'c')"))))
      (testing "SELECT … INTO t"
        (let [r (exec h "SELECT i, s INTO dst FROM src WHERE i < 3")]
          (is (nil? (.error r)))
          ;; PostgreSQL tags this with the row count, like the SELECT.
          (is (= "SELECT 2" (.commandTag r))))
        (is (= [["1" "a"] ["2" "b"]] (rows (exec h "SELECT i, s FROM dst ORDER BY i")))))
      (testing "SELECT … INTO TABLE t"
        (is (nil? (.error (exec h "SELECT i INTO TABLE dst2 FROM src"))))
        (is (= [["1"] ["2"] ["3"]] (rows (exec h "SELECT i FROM dst2 ORDER BY i")))))
      (testing "a constant SELECT with no FROM"
        (let [r (exec h "SELECT 9 AS a INTO dst3")]
          (is (nil? (.error r)))
          (is (= "SELECT 1" (.commandTag r))))
        (is (= [["9"]] (rows (exec h "SELECT a FROM dst3")))))
      (finally (release! h)))))

(deftest create-table-as-reports-the-row-count
  (let [h (fresh-handler)]
    (try
      (is (nil? (.error (exec h "CREATE TABLE src (i int)"))))
      (is (nil? (.error (exec h "INSERT INTO src VALUES (1),(2),(3)"))))
      (let [r (exec h "CREATE TABLE c1 AS SELECT i FROM src")]
        (is (= "SELECT 3" (.commandTag r))))
      (let [r (exec h "CREATE TABLE c2 AS SELECT i FROM src WHERE i > 9")]
        (is (= "SELECT 0" (.commandTag r))))
      (testing "IF NOT EXISTS on an existing table says CREATE TABLE AS"
        (let [r (exec h "CREATE TABLE IF NOT EXISTS c1 AS SELECT i FROM src")]
          (is (nil? (.error r)))
          (is (= "CREATE TABLE AS" (.commandTag r)))))
      (testing "a plain CREATE TABLE is unchanged"
        (is (= "CREATE TABLE" (.commandTag (exec h "CREATE TABLE c3 (i int)")))))
      (finally (release! h)))))
