(ns datahike.test.pg-inherits-resolution-test
  "INHERITS column resolution through a table ALIAS.

   An INHERITS child stores its parent's columns under the PARENT
   namespace on the same entity — a `chi` row carries `:par/pname`, not
   `:chi/pname` — so a reference has to be remapped before it can bind.
   `resolve-inherited-attr` does that, and every consumer called it…
   for only one of the two shapes `resolve-column` returns:

       pname     -> :chi/pname               remapped  ✓
       c.pname   -> [:aliased \"c\" :chi/pname]  NOT remapped  ✗

   So every aliased reference to an inherited column bound nothing and
   read as NULL. Aliases are what ORMs emit, so this was most of the
   real-world surface. `ctx/attr-of` is now the single normaliser both
   shapes go through.

   The IS NULL path was worse: it did no resolution at all, so
   `WHERE inherited_col IS NULL` was INVERTED — the child-namespace
   attribute is absent from every row, making the test true for all of
   them.

   Expectations captured from PostgreSQL 17."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer$QueryResult]))

(def ^:dynamic *handler* nil)

(defn- fixture [f]
  (pg/reset-lock-registry!)
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*handler* (pg/make-query-handler conn)]
          (.execute *handler* "CREATE TABLE par (id int PRIMARY KEY, pname text, pnum int)")
          (.execute *handler* "CREATE TABLE chi (cname text) INHERITS (par)")
          (.execute *handler* "INSERT INTO chi (id,pname,pnum,cname) VALUES (1,'p',10,'c')")
          (.execute *handler* "INSERT INTO chi (id,pname,pnum,cname) VALUES (2,'q',20,'d')")
          (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- rows [sql]
  (mapv vec (.-rows ^PgWireServer$QueryResult (.execute *handler* sql))))

(defn- v [sql] (ffirst (rows sql)))

;; ---------------------------------------------------------------------------
;; The regression: aliased access
;; ---------------------------------------------------------------------------

(deftest inherited-column-through-an-alias
  (testing "projection — returned NULL"
    (is (= "p" (v "SELECT c.pname FROM chi c")))
    (is (= "p" (v "SELECT c.pname FROM chi AS c WHERE c.id = 1"))))
  (testing "predicate — matched nothing"
    (is (= [["c"]] (rows "SELECT c.cname FROM chi c WHERE c.pname = 'p'")))
    (is (= #{["p"] ["q"]}
           (set (rows "SELECT c.pname FROM chi c WHERE c.pnum > 15 OR c.pnum = 10")))))
  (testing "arithmetic and aggregates over an aliased inherited column"
    (is (= "11" (v "SELECT c.pnum + 1 FROM chi c WHERE c.id = 1")))
    (is (= "30" (v "SELECT sum(c.pnum) FROM chi c"))))
  (testing "ORDER BY — the sort key was NULL for every row, so ordering
            silently did nothing"
    (is (= [["d"] ["c"]] (rows "SELECT c.cname FROM chi c ORDER BY c.pname DESC")))
    (is (= [["c"] ["d"]] (rows "SELECT c.cname FROM chi c ORDER BY c.pname ASC"))))
  (testing "JOIN on an aliased child"
    (is (= [["c"]] (rows "SELECT c.cname FROM chi c JOIN par p ON p.id = c.id WHERE c.id = 1")))))

;; ---------------------------------------------------------------------------
;; The IS NULL inversion — present even WITHOUT an alias
;; ---------------------------------------------------------------------------

(deftest is-null-on-an-inherited-column
  (testing "IS NULL was true for every row, because the child-namespace
            attribute is absent from all of them"
    (is (= [] (rows "SELECT cname FROM chi WHERE pname IS NULL")))
    (is (= [] (rows "SELECT cname FROM chi c WHERE c.pname IS NULL"))))
  (testing "IS NOT NULL, the converse"
    (is (= [["c"] ["d"]] (rows "SELECT cname FROM chi ORDER BY cname")))
    (is (= 2 (count (rows "SELECT cname FROM chi WHERE pname IS NOT NULL")))))
  (testing "a genuinely absent inherited value is still NULL"
    (.execute *handler* "INSERT INTO chi (id, cname) VALUES (3, 'e')")
    (is (= [["e"]] (rows "SELECT cname FROM chi WHERE pname IS NULL")))))

;; ---------------------------------------------------------------------------
;; Unaliased access kept working throughout — guard against a fix that
;; trades one shape for the other
;; ---------------------------------------------------------------------------

(deftest unaliased-access-still-works
  (is (= "p" (v "SELECT pname FROM chi WHERE id = 1")))
  (is (= "p" (v "SELECT chi.pname FROM chi WHERE id = 1")))
  (is (= "20" (v "SELECT max(pnum) FROM chi")))
  (is (= [["c"]] (rows "SELECT cname FROM chi WHERE pname = 'p'"))))

(deftest the-childs-own-columns-are-unaffected
  (is (= "c" (v "SELECT cname FROM chi WHERE id = 1")))
  (is (= "c" (v "SELECT c.cname FROM chi c WHERE c.id = 1"))))

(deftest the-parent-table-is-unaffected
  (.execute *handler* "INSERT INTO par (id, pname, pnum) VALUES (9, 'z', 90)")
  (is (= "z" (v "SELECT p.pname FROM par p WHERE p.id = 9")))
  (is (= "z" (v "SELECT pname FROM par WHERE id = 9"))))
