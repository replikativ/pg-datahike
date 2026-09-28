(ns datahike.test.pg-inheritance-star-test
  "`SELECT *` on a table that inherits from another.

   A child's inherited columns are real columns of the child: they come
   first in its column order, `\\d` lists them, `INSERT` without a
   target list fills them, and naming one in a SELECT returns it. They
   were missing from `SELECT *` alone -- the star expanded to the
   child's OWN attributes, so a query asking for every column got a
   row that silently lacked the inherited ones.

   The values live under the ANCESTOR's attribute, which is what made
   the second half of this: once the star expanded to that attribute,
   binding it by its own namespace put it on a separate, unjoined
   entity, and a child with two rows answered with four.

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

(defn- cols [^PgWireServer$QueryResult r]
  (when-not (.error r) (vec (.columnNames r))))

(defn- err [^PgWireServer$QueryResult r] (some-> (.error r) str))

(defn- notices [^PgWireServer$QueryResult r]
  (mapv #(.message ^datahike.pg.PgWireServer$Notice %) (or (.notices r) [])))

(defn- ok! [h sql]
  (let [r (exec h sql)]
    (is (nil? (.error r)) (str sql " => " (err r)))
    r))

(defn- release! [{:keys [conn cfg]}] (d/release conn) (d/delete-database cfg))

(defmacro with-h [[sym init] & body]
  `(let [~sym ~init] (try ~@body (finally (release! ~sym)))))

(defn- family! [h]
  (ok! h "CREATE TABLE ctla (aa text)")
  (ok! h "CREATE TABLE ctlb (bb text) INHERITS (ctla)")
  (ok! h "INSERT INTO ctlb VALUES ('a1','b1')")
  (ok! h "INSERT INTO ctlb VALUES ('a2','b2')"))

(deftest the-star-includes-inherited-columns
  (with-h [h (fresh-handler)]
    (family! h)
    (let [r (ok! h "SELECT * FROM ctlb ORDER BY aa")]
      (testing "inherited first, then the child's own -- PostgreSQL's order"
        (is (= ["aa" "bb"] (cols r))))
      (is (= [["a1" "b1"] ["a2" "b2"]] (rows r))))
    (testing "and one row per row, not the cross product of two entities"
      (is (= [["2"]] (rows (ok! h "SELECT count(*) FROM ctlb")))))
    (testing "the parent still sees only its own columns"
      (is (= ["aa"] (cols (ok! h "SELECT * FROM ctla")))))))

(deftest naming-the-columns-agrees-with-the-star
  ;; This is the check that would have caught it: the two spellings of
  ;; the same query have to answer the same.
  (with-h [h (fresh-handler)]
    (family! h)
    (is (= (rows (ok! h "SELECT aa, bb FROM ctlb ORDER BY aa"))
           (rows (ok! h "SELECT * FROM ctlb ORDER BY aa"))
           (rows (ok! h "SELECT ctlb.* FROM ctlb ORDER BY aa"))))))

(deftest a-qualified-star-and-an-alias-agree-too
  (with-h [h (fresh-handler)]
    (family! h)
    (is (= [["a1" "b1"] ["a2" "b2"]]
           (rows (ok! h "SELECT x.* FROM ctlb x ORDER BY x.aa"))))
    (is (= [["a1" "b1"] ["a2" "b2"]]
           (rows (ok! h "SELECT * FROM ctlb x ORDER BY x.aa"))))))

(deftest a-row-value-carries-the-inherited-fields
  ;; row_to_json expands the row the same way the star does, through the
  ;; same column list, so it would have gone wrong in step with it.
  (with-h [h (fresh-handler)]
    (family! h)
    (is (= [["{\"aa\":\"a1\",\"bb\":\"b1\"}"] ["{\"aa\":\"a2\",\"bb\":\"b2\"}"]]
           (rows (ok! h "SELECT row_to_json(t) FROM ctlb t ORDER BY t.aa"))))))

(deftest three-generations
  (with-h [h (fresh-handler)]
    (family! h)
    (ok! h "CREATE TABLE ctlc (cc text) INHERITS (ctlb)")
    (ok! h "INSERT INTO ctlc VALUES ('a3','b3','c3')")
    (is (= ["aa" "bb" "cc"] (cols (ok! h "SELECT * FROM ctlc"))))
    (is (= [["a3" "b3" "c3"]] (rows (ok! h "SELECT * FROM ctlc"))))
    (testing "and the grandparent sees the grandchild's row through its own columns"
      (is (= [["a1"] ["a2"] ["a3"]] (rows (ok! h "SELECT * FROM ctla ORDER BY aa")))))))

(deftest a-repeated-column-name-is-refused
  ;; It used to reach the schema transaction and come back as datahike's
  ;; unique-constraint message, naming an internal attribute and a
  ;; datom -- nothing the caller could act on.
  (with-h [h (fresh-handler)]
    (let [r (exec h "CREATE TABLE dup (a int, a int)")]
      (is (= "column \"a\" specified more than once" (err r)))
      (is (= "42701" (.sqlstate r))))
    (testing "including a duplicate LIKE brings in"
      (ok! h "CREATE TABLE src (xx text)")
      (is (= "column \"xx\" specified more than once"
             (err (exec h "CREATE TABLE d2 (x text, LIKE src, xx int)"))))
      (is (= "column \"xx\" specified more than once"
             (err (exec h "CREATE TABLE d3 (LIKE src, LIKE src)")))))))

(deftest like-and-inherits-in-one-statement
  ;; PostgreSQL's own create_table_like opens with this: a table that
  ;; both copies a definition and inherits one.
  (with-h [h (fresh-handler)]
    (family! h)
    (ok! h "CREATE TABLE inhx (xx text DEFAULT 'text')")
    (ok! h "CREATE TABLE inhe (ee text, LIKE inhx) INHERITS (ctlb)")
    (ok! h "INSERT INTO inhe VALUES ('ee-col1','ee-col2',DEFAULT,'ee-col4')")
    (is (= ["aa" "bb" "ee" "xx"] (cols (ok! h "SELECT * FROM inhe"))))
    (is (= [["ee-col1" "ee-col2" nil "ee-col4"]] (rows (ok! h "SELECT * FROM inhe"))))
    (testing "LIKE copies structure only -- the source table stays empty"
      (is (= [] (rows (ok! h "SELECT * FROM inhx")))))
    (testing "and the ancestors see the new row"
      (is (= [["a1"] ["a2"] ["ee-col1"]]
             (rows (ok! h "SELECT * FROM ctla ORDER BY aa")))))))

(deftest a-table-may-inherit-from-several
  ;; `INHERITS (b, c, a)`. JSqlParser hands the whole list back as one
  ;; option string, and reading it as a relation named "b,c,a" left the
  ;; child with NO inherited columns -- so `INSERT INTO d(aa)` said the
  ;; column did not exist. It is PostgreSQL's own inherit test's fourth
  ;; statement.
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE a (aa text)")
    (ok! h "CREATE TABLE b (bb text) INHERITS (a)")
    (ok! h "CREATE TABLE c (cc text) INHERITS (a)")
    (let [r (ok! h "CREATE TABLE d (dd text) INHERITS (b,c,a)")]
      (testing "`aa` arrives three times and is merged once, with a notice each"
        (is (= ["merging multiple inherited definitions of column \"aa\""
                "merging multiple inherited definitions of column \"aa\""]
               (notices r)))))
    (is (= ["aa" "bb" "cc" "dd"] (cols (ok! h "SELECT * FROM d"))))
    (ok! h "INSERT INTO a(aa) VALUES ('aaa')")
    (ok! h "INSERT INTO b(aa) VALUES ('bbb')")
    (ok! h "INSERT INTO c(aa) VALUES ('ccc')")
    (ok! h "INSERT INTO d(aa) VALUES ('ddd')")
    (is (= [["ddd" nil nil nil]] (rows (ok! h "SELECT * FROM d"))))
    (testing "every ancestor sees the descendant's row"
      (is (= [["aaa"] ["bbb"] ["ccc"] ["ddd"]]
             (rows (ok! h "SELECT aa FROM a ORDER BY aa"))))
      (is (= [["bbb"] ["ddd"]] (rows (ok! h "SELECT aa FROM b ORDER BY aa"))))
      (is (= [["ccc"] ["ddd"]] (rows (ok! h "SELECT aa FROM c ORDER BY aa")))))
    (testing "and ONLY still means the table itself"
      (is (= [["aaa"]] (rows (ok! h "SELECT * FROM ONLY a")))))))

(deftest a-parent-that-does-not-exist-is-named
  (with-h [h (fresh-handler)]
    (ok! h "CREATE TABLE a (aa text)")
    (let [r (exec h "CREATE TABLE d (dd text) INHERITS (a, nosuch)")]
      (is (= "relation \"nosuch\" does not exist" (err r)))
      (is (= "42P01" (.sqlstate r))))))
