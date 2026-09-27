(ns datahike.test.pg-inheritance-only-test
  "`ONLY t` and `t*`, the two ways SQL says whether a query includes the
   tables that inherit from one.

   `t*` means \"and its descendants\", which has been the default since
   PostgreSQL 7.1; the marker is kept for compatibility and JSqlParser
   has no syntax for it, so it is dropped before parsing.

   `ONLY t` is the interesting one. JSqlParser accepts it in a SELECT
   and silently DISCARDS it -- the Table it hands back is just `t` --
   and rejects it outright in a DELETE. So the statement was answered as
   though ONLY had not been written, which is a wrong answer rather than
   a missing feature: `DELETE FROM ONLY parent` deleted the children's
   rows too.

   It works because a child's row carries every ancestor's row marker as
   well as its own -- that is how the parent sees it at all -- so
   excluding the descendants' markers is exactly what ONLY means.

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.pg.sql.rewrite :as rw]
            [datahike.pg.server :as pg])
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

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (some-> (.error r) str)))
    r))

(defn- release! [{:keys [conn cfg]}] (d/release conn) (d/delete-database cfg))

(defmacro with-h [[sym init] & body]
  `(let [~sym ~init] (try ~@body (finally (release! ~sym)))))

(defn- seed! [h]
  (ok! h "CREATE TABLE par (i int, s text)")
  (ok! h "CREATE TABLE chi () INHERITS (par)")
  (ok! h "INSERT INTO par VALUES (1,'a'),(3,'c')")
  (ok! h "INSERT INTO chi VALUES (2,'b')"))

(deftest the-star-marker-is-the-default
  (let [pre #(rw/rewrite % [rw/inheritance-star-rule])]
    (is (= "SELECT i FROM t" (pre "SELECT i FROM t*")))
    (is (= "SELECT i FROM ONLY t" (pre "SELECT i FROM ONLY t*")))
    (testing "multiplication and count(*) are untouched"
      (is (= "SELECT a * b FROM t" (pre "SELECT a * b FROM t")))
      (is (= "SELECT count(*) FROM t" (pre "SELECT count(*) FROM t")))
      (is (= "SELECT t.* FROM t" (pre "SELECT t.* FROM t"))))))

(deftest select-includes-descendants-unless-only
  (with-h [h (fresh-handler)]
    (seed! h)
    (is (= [["1"] ["2"] ["3"]] (rows (ok! h "SELECT i FROM par ORDER BY i"))))
    (is (= [["1"] ["2"] ["3"]] (rows (ok! h "SELECT i FROM par* ORDER BY i"))))
    (is (= [["1"] ["3"]] (rows (ok! h "SELECT i FROM ONLY par ORDER BY i"))))
    (testing "and an aggregate over it counts the same rows"
      (is (= [["2"]] (rows (ok! h "SELECT count(*) FROM ONLY par"))))
      (is (= [["3"]] (rows (ok! h "SELECT count(*) FROM par")))))))

(deftest delete-only-leaves-the-children
  (with-h [h (fresh-handler)]
    (seed! h)
    (let [r (ok! h "DELETE FROM ONLY par WHERE i = 2")]
      ;; Row 2 lives in the child, so ONLY matches nothing.
      (is (= "DELETE 0" (.commandTag r))))
    (is (= [["1"] ["2"] ["3"]] (rows (ok! h "SELECT i FROM par ORDER BY i"))))
    (testing "without ONLY it is deleted"
      (is (= "DELETE 1" (.commandTag (ok! h "DELETE FROM par WHERE i = 2"))))
      (is (= [["1"] ["3"]] (rows (ok! h "SELECT i FROM par ORDER BY i")))))))

(deftest update-only-leaves-the-children
  (with-h [h (fresh-handler)]
    (seed! h)
    (let [r (ok! h "UPDATE ONLY par SET s = 'z' WHERE i >= 1")]
      (is (= "UPDATE 2" (.commandTag r))))
    (is (= [["1" "z"] ["2" "b"] ["3" "z"]]
           (rows (ok! h "SELECT i, s FROM par ORDER BY i"))))))

(deftest only-and-not-only-do-not-share-a-plan
  ;; `ONLY` is stripped from the SQL before the parser sees it, so two
  ;; statements that differ only by it parse identically -- and would
  ;; share the cached row-matching plan if it were not in the key.
  (with-h [h (fresh-handler)]
    (seed! h)
    (is (= [["1"] ["3"]] (rows (ok! h "SELECT i FROM ONLY par ORDER BY i"))))
    (is (= [["1"] ["2"] ["3"]] (rows (ok! h "SELECT i FROM par ORDER BY i"))))
    (is (= "DELETE 0" (.commandTag (ok! h "DELETE FROM ONLY par WHERE i = 2"))))
    (is (= "DELETE 1" (.commandTag (ok! h "DELETE FROM par WHERE i = 2"))))))
