(ns datahike.test.pg-matview-test
  "Materialized views.

   A materialized view is a query's result held as a table: it is
   filled once, it does NOT follow the tables it was built from, and
   REFRESH is what makes it current again. That is the whole of the
   feature, and it is the difference from an ordinary view that these
   tests are about -- a matview that tracked its sources would be a
   view, and every one of PostgreSQL's expectations for it would be
   wrong.

   It is implemented by running the stored query through CREATE TABLE
   AS and INSERT … SELECT, so its columns, types and contents are
   whatever the same query written out by hand would produce. What
   remains to be tested here is the part that is NOT the query: when
   the contents change, what relkind reports, and which statements may
   name it.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryHandler PgWireServer$QueryResult]))

(defn- fresh-handler []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0
             :schema-flexibility :write :keep-history? false}
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    {:conn conn :cfg cfg :handler (pg/make-query-handler conn {})}))

(defn- exec ^PgWireServer$QueryResult [{:keys [^PgWireServer$QueryHandler handler]} sql]
  (.execute handler sql))

(defn- rows [^PgWireServer$QueryResult r]
  (when-not (.error r) (mapv vec (.rows r))))

(defn- err [^PgWireServer$QueryResult r] (some-> (.error r) str))

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (err r)))
    r))

(defn- release! [{:keys [conn cfg]}] (d/release conn) (d/delete-database cfg))

(defmacro with-h [[sym init] & body]
  `(let [~sym ~init] (try ~@body (finally (release! ~sym)))))

(defn- base! [h]
  (ok! h "CREATE TABLE base (a int, b text)")
  (ok! h "INSERT INTO base VALUES (1,'x'),(2,'y')"))

(deftest a-matview-is-a-snapshot
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT a, b FROM base WHERE a > 0")
    (is (= [["1" "x"] ["2" "y"]]
           (rows (ok! h "SELECT * FROM mv ORDER BY a"))))
    (testing "the base table moving on does not move the view"
      (ok! h "INSERT INTO base VALUES (3,'z')")
      (ok! h "DELETE FROM base WHERE a = 1")
      (is (= [["1" "x"] ["2" "y"]]
             (rows (ok! h "SELECT * FROM mv ORDER BY a")))))
    (testing "REFRESH is what makes it current"
      (ok! h "REFRESH MATERIALIZED VIEW mv")
      (is (= [["2" "y"] ["3" "z"]]
             (rows (ok! h "SELECT * FROM mv ORDER BY a")))))))

(deftest the-command-tag-is-the-query-s-row-count
  ;; PostgreSQL tags a populated materialized view the way it tags
  ;; CREATE TABLE AS -- with the number of rows the query produced.
  ;; Only WITH NO DATA, which runs no query to count, says CREATE
  ;; MATERIALIZED VIEW.
  (with-h [h (fresh-handler)]
    (base! h)
    (is (= "SELECT 2" (.commandTag (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base"))))
    (is (= "CREATE MATERIALIZED VIEW"
           (.commandTag (ok! h "CREATE MATERIALIZED VIEW mv2 AS SELECT 1 AS x WITH NO DATA"))))
    (is (= "REFRESH MATERIALIZED VIEW"
           (.commandTag (ok! h "REFRESH MATERIALIZED VIEW mv"))))
    (is (= "DROP MATERIALIZED VIEW"
           (.commandTag (ok! h "DROP MATERIALIZED VIEW mv, mv2"))))))

(deftest with-no-data-is-not-empty-it-is-unreadable
  ;; The distinction matters: an empty answer would be indistinguishable
  ;; from a view that was refreshed and found nothing.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT 1 AS x WITH NO DATA")
    (let [r (exec h "SELECT * FROM mv")]
      (is (= "materialized view \"mv\" has not been populated" (err r)))
      (is (= "55000" (.sqlstate r))))
    (testing "and REFRESH populates it"
      (ok! h "REFRESH MATERIALIZED VIEW mv")
      (is (= [["1"]] (rows (ok! h "SELECT * FROM mv")))))
    (testing "REFRESH … WITH NO DATA puts it back"
      (ok! h "REFRESH MATERIALIZED VIEW mv WITH NO DATA")
      (is (= "55000" (.sqlstate (exec h "SELECT * FROM mv")))))))

(deftest an-explicit-column-list-renames-the-output
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "CREATE MATERIALIZED VIEW mv (p, q) AS SELECT a, a * 2 FROM base")
    (is (= [["1" "2"] ["2" "4"]] (rows (ok! h "SELECT p, q FROM mv ORDER BY p"))))
    (testing "and REFRESH keeps the names"
      (ok! h "INSERT INTO base VALUES (5,'w')")
      (ok! h "REFRESH MATERIALIZED VIEW mv")
      (is (= [["1" "2"] ["2" "4"] ["5" "10"]]
             (rows (ok! h "SELECT p, q FROM mv ORDER BY p")))))))

(deftest concurrently-is-accepted
  ;; CONCURRENTLY is about who can read the view WHILE it rebuilds. The
  ;; rebuild here is one transaction either way, so the word changes
  ;; nothing and refusing it would block files that only ever say it.
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base")
    (ok! h "INSERT INTO base VALUES (3,'z')")
    (ok! h "REFRESH MATERIALIZED VIEW CONCURRENTLY mv")
    (is (= [["1"]] (rows (ok! h "SELECT count(*) FROM mv WHERE a = 3"))))))

(deftest it-is-relkind-m
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base")
    (is (= [["base" "r"] ["mv" "m"]]
           (rows (ok! h (str "SELECT relname, relkind FROM pg_class "
                             "WHERE relname IN ('base','mv') ORDER BY relname")))))))

(deftest only-drop-materialized-view-drops-one
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base")
    (testing "DROP TABLE says so and leaves it alone"
      (let [r (exec h "DROP TABLE mv")]
        (is (= "\"mv\" is not a table" (err r)))
        (is (= "42809" (.sqlstate r))))
      (is (= [["2"]] (rows (ok! h "SELECT count(*) FROM mv")))))
    (testing "so does DROP VIEW"
      (is (= "\"mv\" is not a view" (err (exec h "DROP VIEW mv")))))
    (testing "and the base table is not a materialized view"
      (let [r (exec h "DROP MATERIALIZED VIEW base")]
        (is (= "\"base\" is not a materialized view" (err r)))
        (is (= "42809" (.sqlstate r)))))
    (ok! h "DROP MATERIALIZED VIEW mv")
    (is (= [["0"]] (rows (ok! h "SELECT count(*) FROM pg_class WHERE relname = 'mv'"))))))

(deftest names-are-resolved-before-anything-is-dropped
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base")
    (is (= "relation \"nosuch\" does not exist" (err (exec h "DROP MATERIALIZED VIEW nosuch"))))
    (ok! h "DROP MATERIALIZED VIEW IF EXISTS nosuch")
    (is (= [["2"]] (rows (ok! h "SELECT count(*) FROM mv"))))
    (is (= "relation \"nosuch\" does not exist"
           (err (exec h "REFRESH MATERIALIZED VIEW nosuch"))))
    (is (= "\"base\" is not a materialized view"
           (err (exec h "REFRESH MATERIALIZED VIEW base"))))))

(deftest if-not-exists-skips-rather-than-fails
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base")
    (let [r (exec h "CREATE MATERIALIZED VIEW mv AS SELECT 1")]
      (is (= "relation \"mv\" already exists" (err r)))
      (is (= "42P07" (.sqlstate r))))
    (ok! h "CREATE MATERIALIZED VIEW IF NOT EXISTS mv AS SELECT 1")
    (testing "and the ORIGINAL definition survives the skip"
      (is (= [["1" "x"] ["2" "y"]] (rows (ok! h "SELECT a, b FROM mv ORDER BY a")))))))

(deftest it-rolls-back-with-its-transaction
  (with-h [h (fresh-handler)]
    (base! h)
    (ok! h "BEGIN")
    (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base")
    (is (= [["2"]] (rows (ok! h "SELECT count(*) FROM mv"))))
    (ok! h "ROLLBACK")
    (is (= [["0"]] (rows (ok! h "SELECT count(*) FROM pg_class WHERE relname = 'mv'"))))
    (testing "and commits with it"
      (ok! h "BEGIN")
      (ok! h "CREATE MATERIALIZED VIEW mv AS SELECT * FROM base")
      (ok! h "COMMIT")
      (is (= [["m"]] (rows (ok! h "SELECT relkind FROM pg_class WHERE relname = 'mv'")))))
    (testing "a REFRESH rolls back too"
      (ok! h "INSERT INTO base VALUES (3,'z')")
      (ok! h "BEGIN")
      (ok! h "REFRESH MATERIALIZED VIEW mv")
      (ok! h "ROLLBACK")
      (is (= [["2"]] (rows (ok! h "SELECT count(*) FROM mv")))))))

(deftest storage-options-are-still-refused
  ;; USING and TABLESPACE describe where the rows physically go. There
  ;; is nothing here to store them against, so accepting them would be
  ;; accepting a statement whose meaning was dropped.
  (with-h [h (fresh-handler)]
    (is (= "CREATE MATERIALIZED VIEW is not supported by datahike pgwire"
           (err (exec h "CREATE MATERIALIZED VIEW mv USING heap2 AS SELECT 1"))))))
